from __future__ import annotations

import logging
import sys

from src.common.cmds import Command
from src.emulator.fun import hello_ussr

machine_word_size: int = 32
argument_size: int = 16
port_number: int = 8
instr_limit: int = 65000
int_output: bool = False

log_lvl: dict = {
    "debug": logging.DEBUG,
    "info": logging.INFO,
    "warning": logging.WARNING,
    "error": logging.ERROR,
    "critical": logging.CRITICAL
}

group_no_signal_cmds: list[Command.value] = [
    Command.LDM.value, Command.ADD.value, Command.SUB.value, Command.INCR.value, Command.DECR.value,
    Command.LSL.value, Command.LSR.value, Command.AND.value, Command.OR.value, Command.NOT.value,
    Command.XOR.value, Command.MULT.value, Command.DIV.value, Command.MOD.value, Command.POW.value,
    Command.SQRT.value
]


def big_endian_split(value: int) -> tuple[int, int]:
    """Раскладывает число на его младшую и старшую составляющую"""
    high = value // (2 ** argument_size)
    low = value % (2 ** argument_size)
    return high, low


class Signals:
    def __init__(self):
        self.arg: int = 0
        self.latch_ar: bool = False
        self.latch_rar: bool = False
        self.latch_acc: bool = False
        self.referenced: bool = False
        self.io_out: bool = False
        self.io_in: bool = False
        self.write: bool = False
        self.inst_arg: bool = False
        self.alu_op: int = Command.NOP.value

    def reset(self) -> None:
        self.latch_ar = False
        self.latch_rar = False
        self.latch_acc = False
        self.referenced = False
        self.io_out = False
        self.io_in = False
        self.write = False
        self.inst_arg = False

    def dp_pause(self) -> None:  # Чтобы застопорить data_pass выставляем флаги latch_acc, latch_rar, latch_ar
        self.reset()  # предварительно сбрасываем остальные
        self.latch_ar = True
        self.latch_rar = True
        self.latch_acc = True


class DataPass:
    def __init__(self, inputs: list[int]):
        # память
        self.io_ports: list[int] = [0] * port_number
        self.data_mem: list[int] = [0] * 2 ** argument_size
        self.input_buffer: list[int] = inputs
        self.output_buffer: list[int] = []
        # регистры
        self.ar: int = 0  # address register 16 bit
        self.rar: int = 0  # relative address register 16 bit
        self.acc: int = 0  # accumulator 32 bit
        # флаги
        self.z: bool = False
        self.c: bool = False

    def output_buffer_to_str(self) -> str:
        line: str = ""
        for val in self.output_buffer:
            line += val if int_output else chr(val)
        return line

    def run(self, signals: Signals) -> None:
        pass


class ControlUnit:
    def __init__(self, instructions: list[int], data_pass: DataPass):
        self.inst_mem: list[int] = [0] * 2 ** argument_size
        self.pc: int = 0
        self.data_pass: DataPass = data_pass
        self._tick = 0
        for i in range(len(instructions)):
            self.inst_mem[i] = instructions[i]
        self.signals: Signals = Signals()

    def tick(self) -> None:
        self.data_pass.run(self.signals)
        self.signals.reset()
        self._tick += 1

    def current_tick(self) -> int:
        """Текущее модельное время процессора (в тактах)."""
        return self._tick

    def signal_sel_next(self, sel_next: bool) -> None:
        """Защёлкнуть новое значение счётчика команд.

        Если `sel_next` равен `True`, то счётчик будет увеличен на единицу,
        иначе -- будет установлен в значение аргумента текущей инструкции.
        """
        if sel_next:
            self.pc += 1
        else:
            instr = self.inst_mem[self.pc]
            arg = big_endian_split(instr)[1]
            assert str(arg) in str(instr), "internal error"
            self.pc = arg

    def decode_and_execute_control_flow_instruction(self, opcode: int, arg: int) -> bool:
        if opcode is Command.HALT.value:
            raise StopIteration()
        if opcode is Command.NOP.value:
            self.signals.dp_pause()
            return True
        if opcode in {Command.JMP.value, Command.JZ.value, Command.JZC.value}:  # latch_pc не требуется, т.к.
            # jump-ы 1 такт
            self.signals.dp_pause()

            if opcode is Command.JMP.value or (opcode is Command.JZ.value and self.data_pass.z) or (
                    opcode is Command.JZC.value and (self.data_pass.z or self.data_pass.c)):
                self.signal_sel_next(False)
            else:
                self.signal_sel_next(True)

            self.tick()
            return True
        return False

    def decode_and_execute_instruction(self) -> None:  # noqa: C901
        """Основной цикл процессора. Эмулирует Instruction Decoder.
                Обработка инструкции:
                0. Получаем инструкцию из памяти инструкций
                1. Проверить `Opcode`.
                2. Вызвать методы, имитирующие необходимые управляющие сигналы.
                3. Продвинуть модельное время вперёд на один такт (`tick`).
                   В этот момент прокручивается data_path
                4. (если необходимо) повторить шаги 2-3.
                5. Перейти к следующей инструкции.
                Обработка функций управления потоком исполнения вынесена в
                `decode_and_execute_control_flow_instruction`.
        """
        instr: int = self.inst_mem[self.pc]
        opcode: int
        arg: int
        opcode, arg = big_endian_split(instr)
        self.signals.arg = arg
        self.signals.alu_op = opcode

        if self.decode_and_execute_control_flow_instruction(opcode, instr):  # группа 0, операции управления pc
            return
        self.signals.reset()  # сброс значений сигналов (кроме arg и alu_op)

        if opcode in group_no_signal_cmds:  # большинство операций работы с аккумулятором не требуют ни 1 сигнала
            pass
        elif opcode == Command.LDI.value:  # однотактные
            self.signals.inst_arg = True
        elif opcode == Command.SAVE.value:
            self.signals.latch_acc = True
            self.signals.write = True
        elif opcode == Command.CMP.value:
            self.signals.latch_acc = True
        elif opcode == Command.IN.value:
            self.signals.io_in = True
        elif opcode == Command.OUT.value:
            self.signals.io_out = True

        elif opcode == Command.LDREF.value:  # двухтактные
            self.tick()  # +1 такт (на первом такте все сигналы нулевые)
            self.signals.latch_rar = True
            self.signals.referenced = True
        elif opcode == Command.SAVEREF.value:
            self.signals.latch_acc = True
            self.tick()
            self.signals.latch_rar = True
            self.signals.latch_acc = True
            self.signals.referenced = True
        else:
            logging.error("Invalid control flow instruction opcode!")
            raise IndentationError
        self.tick()
        self.signal_sel_next(True)  # pc++

    def __repr__(self):
        return "Control unit debug not realized yet"


# Эта функция эмулирует работу процессора
def emulation(code: list[int], inputs: list[int]) -> tuple[list[int], int, int]:  # (output, instr_counter, ticks)
    """Функция запускает эмуляцию процессора"""
    data_path: DataPass = DataPass(inputs)
    control_unit: ControlUnit = ControlUnit(code, data_path)
    instr_counter: int = 0

    logging.info("Starting emulation")
    logging.debug("%s", control_unit)
    try:
        while instr_counter < instr_limit:
            control_unit.decode_and_execute_instruction()
            instr_counter += 1
            logging.debug("%s", control_unit)
    except EOFError:
        logging.warning("Input buffer is empty!")
    except ZeroDivisionError:
        logging.warning("Zero division! Process stopped!")
    except IndentationError:
        logging.warning("Failed decoding instruction!")
    except StopIteration:
        logging.info("Emulation complete")

    if instr_counter >= instr_limit:
        logging.warning("Limit exceeded!")
    logging.info("output_buffer: %s", data_path.output_buffer_to_str())
    return data_path.output_buffer, instr_counter, control_unit.current_tick()


def read_instructions(binary_file: str) -> list[int]:
    # Открываем бинарный файл на чтение
    with open(binary_file, "rb") as file:
        instructions: list[int] = []
        four_bytes_cell: list = list(file.read(4))  # Читаем первые 4 байта из файла (32бит = 4байт)
        i: int = 0  # Проходим в цикле, чтобы считать оставшуюся часть файла
        while four_bytes_cell:
            assert len(four_bytes_cell) == 4, ("Wrong inst " + str(i) + " byte size " +
                                               str(len(four_bytes_cell)) + ", while 4 expected")
            instruction: int = 2 ** 16 * (2 ** 8 * four_bytes_cell[0] + four_bytes_cell[1]) + (
                    2 ** 8 * four_bytes_cell[2] + four_bytes_cell[3])
            instructions.append(instruction)
            i += 1
            four_bytes_cell: list = list(file.read(4))  # чтение (байт)
        assert len(instructions) != 0, ("Empty file " + binary_file)
        return instructions


def main(binary_file: str, inputs_file: str):
    hello_ussr()
    print("Исполняемый файл:", binary_file)
    code: list[int] = read_instructions(binary_file)
    with open(inputs_file, encoding="utf-8") as file:
        input_text = file.read()
        inputs = []
        for char in input_text:
            inputs.append(ord(char))
    logging.info("Got instructions and user input")

    output: list[int]
    instr_counter: int
    ticks: int
    output, instr_counter, ticks = emulation(code=code, inputs=inputs)
    print("output:", ", ".join(map(str, output)))
    print("instr_counter:", instr_counter, ", ticks:", ticks)


if __name__ == "__main__":
    logging_lvl: int = logging.DEBUG
    assert (len(sys.argv) == 3 or
            (len(sys.argv) == 4 and sys.argv[3].lower() in log_lvl)), ("Wrong arguments: python3 sovcode.py <code_file>"
                                                                       " <output_file> <debug_lvl>")
    if len(sys.argv) == 4:  # set logging lvl
        _, code_file, input_file, log_lvl_arg = sys.argv
        logging_lvl = log_lvl[str(log_lvl_arg).lower()]
    else:
        _, code_file, input_file = sys.argv

    logging.basicConfig(level=logging_lvl, filename="run_log.log", filemode="w",
                        format="%(levelname)s [%(funcName)s]: %(message)s")
    main(code_file, input_file)

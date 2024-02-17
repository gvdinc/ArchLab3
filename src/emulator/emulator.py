from __future__ import annotations

import logging
import math
import sys

from src.common.cmds import Command
from src.emulator.fun import hello_ussr

machine_word_size: int = 32
argument_size: int = 16
port_number: int = 8
in_port: int = 0
out_port: int = 1
instr_limit: int = 100000
int_output: bool = True

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

    def check_runnable(self) -> bool:
        check_arg: bool = 0 <= self.arg <= 2 ** argument_size
        check_cmd: bool = 2 <= self.alu_op <= Command.SAVEREF.value
        assert check_cmd, "Wrong alu_op code"
        if not check_arg:
            logging.warning("Running cmd " + str(self.alu_op) + " with invalid argument")
            return False
        return True


def from_twos_complement(value2c: int) -> int:
    assert 0 <= value2c < 2 ** machine_word_size, ValueError
    val = value2c
    if val & (1 << (machine_word_size - 1)):
        # Если старший бит установлен (число отрицательное)
        return val - (1 << machine_word_size)
    return val


class DataPass:
    def __init__(self, inputs: list[int]):
        # буфер вывода (сюда складываем значения из порта вывода) это порт ввода
        # память
        self.io_ports: list[list[int]] = [[] * 1 for i in range(port_number)]
        self.data_mem: list[int] = [0] * 2 ** argument_size
        self.io_ports[in_port] = inputs
        # регистры
        self.ar: int = 0  # address register 16 bit
        self.rar: int = 0  # relative address register 16 bit
        self.acc: int = 0  # accumulator 32 bit
        # флаги
        self.z: bool = False
        self.c: bool = False

    def output_buffer_to_str(self) -> str:
        line: str = ""
        for val in self.io_ports[out_port]:
            line += str(from_twos_complement(val)) if int_output else chr(val)
        return line

    def _io_read(self, port: int) -> int:
        assert port == in_port, "Wrong in port"
        if len(self.io_ports[port]) == 0:
            raise EOFError
        return self.io_ports[port].pop(0)

    def _set_flags(self, result: int):
        self.z = False
        self.c = False
        if result == 0:
            self.z = True
        if result < 0 or result >= 2 ** (machine_word_size - 1):
            self.c = True

    def _bounce_res(self, result: int) -> int:
        if result > 2 ** machine_word_size:
            logging.warning("overflow up")
            self.c = True
            return result % (2 ** machine_word_size)
        if result < -(2 ** machine_word_size):
            logging.warning("overflow down")
            self.c = True
            return (result + 2 ** machine_word_size) % (2 ** machine_word_size)
        return result

    def _to_twos_complement(self, int_value: int) -> int:
        value = self._bounce_res(int_value)
        if 0 <= value < 2 ** (machine_word_size - 1):
            return value
        if value > 2 ** (machine_word_size - 1):
            return value  # in two's complement already
        # переводим в дополнительный код (val < 0)
        d_positive_value: int = abs(value) - 1
        flipped_bits: str = bin(d_positive_value ^ (2 ** machine_word_size - 1))[2:]
        string_complement: str = flipped_bits.zfill(machine_word_size)
        assert string_complement[0] == "1", ValueError
        return int(string_complement, 2)

    def alu(self, data: int, mux_res: int, alu_op: int) -> int:  # returns value to write to acc  # noqa: C901
        val_mux = from_twos_complement(mux_res)
        val_data = from_twos_complement(data)
        if alu_op == Command.LDM.value:
            return data
        if alu_op == Command.LDI.value:
            return big_endian_split(self.acc)[0] * (2 ** argument_size) + mux_res  # works great if latch_rar
        if alu_op == Command.SAVE.value:
            return data
        if alu_op == Command.ADD.value:
            return self._to_twos_complement(val_mux + val_data)
        if alu_op == Command.SUB.value:
            return self._to_twos_complement(val_mux - val_data)
        if alu_op == Command.CMP.value:
            self._set_flags(val_mux)
            return mux_res
        if alu_op == Command.INCR.value:
            return mux_res + 1
        if alu_op == Command.DECR.value:
            return mux_res - 1
        if alu_op == Command.LSL.value:
            return mux_res << 16
        if alu_op == Command.LSR.value:
            return mux_res >> 16
        if alu_op == Command.CLRZ or alu_op == Command.CLRC:
            self.z = False if alu_op == Command.CLRZ else self.z
            self.c = False if alu_op == Command.CLRC else self.c
            return mux_res
        if alu_op == Command.AND.value:
            return 1 if (mux_res and data) else 0
        if alu_op == Command.OR.value:
            return 1 if (mux_res or data) else 0
        if alu_op == Command.NOT.value:
            return 0 if mux_res else 1
        if alu_op == Command.XOR.value:
            return mux_res ^ data
        if alu_op == Command.JMP.value:
            return self.acc  # В jump-ах он всегда защёлкнут, т.ч. условность
        if alu_op == Command.JZ.value:
            return self.acc
        if alu_op == Command.JZC.value:
            return self.acc
        if alu_op == Command.MULT.value:
            return val_mux * val_data
        if alu_op == Command.DIV.value:
            assert data != 0, "Division by zero"
            return val_mux // val_data
        if alu_op == Command.MOD.value:
            return val_mux % val_data
        if alu_op == Command.POW.value:
            return val_mux ** val_data
        if alu_op == Command.SQRT.value:
            return int(math.sqrt(val_mux))
        if alu_op == Command.IN.value:
            return data
        if alu_op == Command.OUT.value:
            return data
        if alu_op == Command.LDREF.value or alu_op == Command.SAVEREF.value:
            return data
        logging.error("ALU OP NOT SUPPORTED. ALU returned what came from data")
        return data

    def run(self, signals: Signals) -> None:
        assert signals.check_runnable(), "Can not run - incorrect signals"
        self.ar = signals.arg
        addr: int = self.rar if signals.referenced else self.ar  # выбираем адрес(порт) из ar или rar
        data: int = self._io_read(addr) if signals.io_in else self.data_mem[addr]
        mux_res: int = addr if signals.inst_arg else self.acc
        alu_res = self._bounce_res(self.alu(data, mux_res, signals.alu_op))
        self.acc = self.acc if signals.latch_acc else alu_res
        self.rar = self.rar if signals.latch_rar else alu_res
        if signals.io_out:
            assert addr == out_port, "wrong out port"
            self.io_ports[addr].append(self.acc)
        elif signals.write:
            self.data_mem[addr] = self.acc


class ControlUnit:
    def __init__(self, instructions: list[int], data_pass: DataPass):
        self.inst_mem: list[int] = [0] * 2 ** argument_size
        self.pc: int = 0
        self.data_pass: DataPass = data_pass
        self._tick = 0
        for i in range(len(instructions)):
            self.inst_mem[i] = instructions[i]
        self.signals: Signals = Signals()

    def run_dp(self) -> None:
        self.data_pass.run(self.signals)
        self.signals.reset()
        self.tick()

    def tick(self) -> None:
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
        global int_output
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
            self.signals.latch_rar = True
        elif opcode == Command.SAVE.value:
            self.signals.latch_acc = True
            self.signals.write = True
        elif opcode == Command.CMP.value:
            self.signals.latch_acc = True
        elif opcode == Command.IN.value:
            int_output = False
            self.signals.io_in = True
        elif opcode == Command.OUT.value:
            self.signals.latch_acc = True
            self.signals.io_out = True

        elif opcode == Command.LDREF.value:  # двухтактные
            int_output = False
            self.run_dp()  # +1 такт (на первом такте все сигналы нулевые)
            self.signals.latch_rar = True
            self.signals.referenced = True
        elif opcode == Command.SAVEREF.value:
            int_output = False
            self.signals.latch_acc = True
            self.run_dp()
            self.signals.latch_rar = True
            self.signals.latch_acc = True
            self.signals.referenced = True
            self.signals.write = True
        else:
            logging.error("Invalid control flow instruction opcode!")
            raise IndentationError
        self.run_dp()
        self.signal_sel_next(True)  # pc++

    def __repr__(self):
        return "Control unit debug not realized yet"


# Эта функция эмулирует работу процессора
def emulation(code: list[int], inputs: list[int]) -> tuple[str, int, int]:  # (output, instr_counter, ticks)
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
    except IndentationError:
        logging.warning("Failed decoding instruction!")
    except OverflowError or ValueError:
        logging.warning("Value error caught!")
    except StopIteration:
        logging.info("Emulation complete")

    if instr_counter >= instr_limit:
        logging.warning("Limit exceeded!")
    logging.info("output_buffer: %s", data_path.output_buffer_to_str())
    return data_path.output_buffer_to_str(), instr_counter, control_unit.current_tick()


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

    output: str
    instr_counter: int
    ticks: int
    output, instr_counter, ticks = emulation(code=code, inputs=inputs)
    print("int" if int_output else "str", "output:", "".join(map(str, output)))
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

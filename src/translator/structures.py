import typing
from enum import Enum


class Cmd(Enum):
    NOP: str = "nop"
    HALT: str = "halt"
    LDM: str = "ldm"
    LDI: str = "ldi"
    SAVE: str = "save"
    ADD: str = "add"
    SUB: str = "sub"
    CMP: str = "cmp"
    INCR: str = "incr"
    DECR: str = "decr"
    LSL: str = "lsl"
    LSR: str = "lsr"
    CLRC: str = "clrc"
    CLRZ: str = "clrz"
    AND: str = "and"
    OR: str = "or"
    NOT: str = "not"
    BTN: str = "btn"
    XOR: str = "xor"
    JMP: str = "jmp"
    JZ: str = "jz"
    JNZ: str = "jnz"
    MULT: str = "mult"
    DEV: str = "dev"
    MOD: str = "mod"
    POW: str = "pow"
    SQRT: str = "sqrt"
    IN: str = "in"
    OUT: str = "out"


class VarType(Enum):
    INT: int = 1
    FLOAT: float = 1
    CHAR: str = 1
    STR: str = 256


class ExprType(Enum):
    # Высшие операции (обрабатываются в translate)
    ASSIGNMENT: str = "assignment"
    IF: str = "if"
    IF_ELSE: str = "if-else"
    WHILE: str = "while"
    READ: str = "read"
    WRITE: str = "write"
    # типы
    VAR_CEL: str = "цел"
    VAR_VES: str = "вещ"
    VAR_SYM: str = "симв"
    VAR_STR: str = "текст"
    # унарные
    LITERAL: str = "literal"
    IDENTIFIER: str = "identifier"
    MINUS: str = "-"
    SQRT: str = "//"
    INCR: str = "++"
    DECR: str = "--"
    LOGICAL_NOT: str = "!"
    BITWISE_NOT: str = "~"
    LOGICAL_XOR: str = "^"
    TO_STR: str = "@"
    # операции с 2мя аргументами (3ий порядок)
    PLUS: str = "+"
    TIMES: str = "*"
    DIVIDE: str = "/"
    MOD: str = "%"
    POWER: str = "**"
    LOGICAL_AND: str = "&"
    LOGICAL_OR: str = "|"
    LESS_THAN: str = "<"
    GREATER_THAN: str = ">"
    EQUALS: str = "=="
    NOT_EQUALS: str = "!="


def to_twos_complement(num: int):
    bit_length = 16
    if num >= 0:
        # Положительное число
        return bin(num)[2:].zfill(bit_length).replace("0b", "")
    # Отрицательное число
    positive_value = abs(num)
    flipped_bits = bin(positive_value ^ (2 ** bit_length - 1))[2:]
    return flipped_bits.zfill(bit_length).replace("0b", "")


def is_bounced_16(num: int) -> bool:
    return 0 <= num < 65536


def is_bounced_32(num: int) -> bool:
    return 0 <= num < 4294967296


class MemStat:
    data_addr_total: int = 65536  # ячеек памяти всего
    io_addr_total: int = 8  # портов ввода/вывода всего
    buffer_initial: int  # адрес начала буфера
    variables_count: typing.ClassVar = {
        ExprType.VAR_CEL.value: 0,  # 1 ячейка (4 байта)
        ExprType.VAR_VES.value: 0,  # 1 ячейка (4 байта)
        ExprType.VAR_SYM.value: 0,  # 1 ячейка (4 байта)
        ExprType.VAR_STR.value: 0  # 256 ячеек (1060 байт)
    }

    def __init__(self):
        self._var_iter = 0
        self._vars: typing.ClassVar = {
            # var = [addr, type] type = (int, float, char, str)
        }
        self.io_ports: typing.ClassVar = {
            "in": 0,
            "out": 1
        }
        self._buff = 0

    def allocate_var(self, var: str, var_type: VarType):
        assert len(var) > 0, "Invalid variable name"
        assert (self._var_iter + var_type.value < self.buffer_initial or
                self._var_iter + var_type.value < self.data_addr_total), "memory access out of bounds"
        var_iter = self._var_iter
        self._var_iter += var_type.value
        try:
            self._vars[var] = [var_iter, var_type]
        except KeyError:
            assert KeyError
        return var_iter

    def allocate_tmp(self, value: int, var_type: VarType):
        assert is_bounced_32(value), "Invalid argument"
        assert self._buff + self.buffer_initial + 1 < self.data_addr_total - self.buffer_initial, MemoryError
        tmp_addr = self._buff + self.buffer_initial
        tmp_name: str = str(tmp_addr)
        self._buff += 1
        try:
            self._vars[tmp_name] = [tmp_addr, var_type]
        except KeyError:
            assert KeyError
        return tmp_addr

    def get_var(self, var: str):
        assert var in self._vars, "variable not initialized"
        return self._vars[var][0]

    def get_var_type(self, var: str):
        assert var in self._vars, "variable not initialized"
        return self._vars[var][1]

    def check_vars_type(self, var1: str, var2: str):
        if self._vars[var1][1] == self._vars[var2][1]:
            return True
        return False


class Coder:
    def __init__(self):
        self.instr_buf: typing.ClassVar = []
        self.cmd_codes: typing.ClassVar = {
            Cmd.NOP: bin(0).zfill(16).replace("0b", ""),  # Операция простоя
            Cmd.HALT: bin(1).zfill(16).replace("0b", ""),  # Сигнал остановки
            Cmd.LDM: bin(2).zfill(16).replace("0b", ""),  # Загрузить знач из памяти данных в acc
            Cmd.LDI: bin(3).zfill(16).replace("0b", ""),  # Загрузить знач из памяти команд в acc
            Cmd.SAVE: bin(4).zfill(16).replace("0b", ""),  # Выгрузить значение аккумулятора в ячейку памяти
            Cmd.ADD: bin(5).zfill(16).replace("0b", ""),  # Добавить значение из ячейки к аккумулятору
            Cmd.SUB: bin(6).zfill(16).replace("0b", ""),  # Вычесть значение ячейки из аккумулятора
            Cmd.CMP: bin(7).zfill(16).replace("0b", ""),  # Сравнить код из аккумулятора c ячейкой памяти
            Cmd.INCR: bin(8).zfill(16).replace("0b", ""),  # Инкрементирует значение аккумулятора
            Cmd.DECR: bin(9).zfill(16).replace("0b", ""),  # Декрементирует значение аккумулятора
            Cmd.LSL: bin(10).zfill(16).replace("0b", ""),  # Бит. Сдвиг влево, устанавливается флаг c
            Cmd.LSR: bin(11).zfill(16).replace("0b", ""),  # Бит. Сдвиг вправо, устанавливается флаг c
            Cmd.CLRC: bin(12).zfill(16).replace("0b", ""),  # Сбрасывает значение флага переполнения
            Cmd.CLRZ: bin(13).zfill(16).replace("0b", ""),  # Сбрасывает значение нулевого флага
            Cmd.AND: bin(14).zfill(16).replace("0b", ""),  # Логическое и acc
            Cmd.OR: bin(15).zfill(16).replace("0b", ""),  # Логическое или
            Cmd.NOT: bin(16).zfill(16).replace("0b", ""),  # Логическое не acc
            Cmd.BTN: bin(17).zfill(16).replace("0b", ""),  # Побитовое не acc
            Cmd.XOR: bin(18).zfill(16).replace("0b", ""),  # Логический xor acc
            Cmd.JMP: bin(19).zfill(16).replace("0b", ""),  # Совершить переход на pmem
            Cmd.JZ: bin(20).zfill(16).replace("0b", ""),  # Переход на pmem при установленном zero флаге 1
            Cmd.JNZ: bin(21).zfill(16).replace("0b", ""),  # Переход на pmem при неустановленном zero флаге
            Cmd.MULT: bin(22).zfill(16).replace("0b", ""),  # Умножить аккумулятор на знач. Из ячейки пам.
            Cmd.DEV: bin(23).zfill(16).replace("0b", ""),  # Разделить аккумулятор на знач. Из ячейки пам.
            Cmd.MOD: bin(24).zfill(16).replace("0b", ""),  # Остаток от деления аккумулятора на знач. Ячейки
            Cmd.POW: bin(25).zfill(16).replace("0b", ""),  # Возвести аккумулятор в степень из ячейки пам.
            Cmd.SQRT: bin(26).zfill(16).replace("0b", ""),  # Подсчитать корень знач. Аккумулятора
            Cmd.IN: bin(27).zfill(16).replace("0b", ""),  # Ввод символьного значения с внешнего устройства
            Cmd.OUT: bin(28).zfill(16).replace("0b", "")  # Вывод символьного значения на внешнее устройство
        }

    def gen(self, cmd: Cmd, arg: int):
        instruction = ""
        assert is_bounced_16(arg), "Argument out of bounced"
        instruction += self.cmd_codes[cmd]
        instruction += to_twos_complement(arg)
        self.instr_buf.append(instruction)
        print(instruction, "#", cmd.name, hex(arg))
        return instruction

    def get_instr_buf(self):
        return self.instr_buf

    def get_binary_code(self) -> list:
        return self.instr_buf

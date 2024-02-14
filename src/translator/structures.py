import typing
from enum import Enum

import pytest
import s_lexer as lexer


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
    XOR: str = "xor"
    JMP: str = "jmp"
    JZ: str = "jz"
    JZC: str = "jzc"
    MULT: str = "mult"
    DIV: str = "dev"
    MOD: str = "mod"
    POW: str = "pow"
    SQRT: str = "sqrt"
    IN: str = "in"
    OUT: str = "out"
    LDREF: str = "ldref"
    SAVEREF: str = "saveref"


class VarType(Enum):
    INT = 1
    CHAR = 2
    STR = 3

    def size(self) -> int:
        if self.value != VarType.STR.value:
            return 1
        return 256


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
    LOGICAL_XOR: str = "^"
    # бинарные (2 аргумента)
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


# методы конвертации
def sym_to_unicode_code(sym: str) -> int:
    return ord(sym)


def binary_32_16_split(value: int):
    # Преобразуем целое число в 32-битное двоичное значение, убирая префикс '0b'
    binary_value = bin(value & 0xFFFFFFFF)[2:].zfill(32)
    # Разделим 32-битное двоичное значение на старшую и младшую части по 16 бит
    upper_half = binary_value[:16]
    lower_half = binary_value[16:]
    return int(upper_half, 2), int(lower_half, 2)


def liter_to_assembly(liter: str, variable_type: VarType):
    if variable_type == VarType.INT:
        return int(liter)
    if variable_type == VarType.CHAR:
        return sym_to_unicode_code(liter)
    if variable_type == VarType.STR:
        pytest.fail("STR represented as literal")
    pytest.fail("unknown type expression")


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
        ExprType.VAR_SYM.value: 0,  # 1 ячейка (4 байта)
        ExprType.VAR_STR.value: 0  # 256 ячеек (1060 байт)
    }

    def __init__(self, sovcode_file: str = ""):
        self._var_it = 0
        self._buff_it = 0
        self._vars: typing.ClassVar = {
            # var = [addr, type] type = (int, char, str)
        }
        self._buffer: typing.ClassVar = {
            # val = [addr, type] type = (int, char, str)
        }
        self.io_ports: typing.ClassVar = {
            "in": 0,
            "out": 1
        }

        if sovcode_file != "":  # собираем статистику
            tokens = lexer.tokenize(sovcode_file)
            for token in tokens:
                if token.type == "VAR_CEL":
                    self.variables_count[ExprType.VAR_CEL.value] += 1
                    continue
                if token.type == "VAR_SYM":
                    self.variables_count[ExprType.VAR_SYM.value] += 1
                    continue
                if token.type == "VAR_STR":
                    self.variables_count[ExprType.VAR_STR.value] += 1
                    continue
            addr_count_sum = (self.variables_count[ExprType.VAR_CEL.value] * VarType.INT.size() +
                              self.variables_count[ExprType.VAR_SYM.value] * VarType.CHAR.size() +
                              self.variables_count[ExprType.VAR_STR.value] * VarType.STR.size())
            self.buffer_initial = addr_count_sum + 1
            print("Кол-во переменных: ", self.variables_count)
            print("Адресов Занято: ", addr_count_sum, "; Свободно: ", self.data_addr_total - addr_count_sum,
                  "; Начало буфера: ", self.buffer_initial, sep="")

    def is_initialized(self, var: str):
        return var in self._vars or var in self._buffer

    def allocate_var(self, var: str, var_type: VarType) -> int:
        assert len(var) > 0, "Invalid variable name"
        assert (self._var_it + var_type.value < self.buffer_initial or
                self._var_it + var_type.value < self.data_addr_total), "memory access out of bounds"
        var_iter = self._var_it
        self._var_it += var_type.size()
        try:
            self._vars[var] = [var_iter, var_type]
        except KeyError:
            assert KeyError
        return var_iter

    def allocate_tmp(self, var_type: VarType) -> int:
        assert self._buff_it + self.buffer_initial + 1 < self.data_addr_total - self.buffer_initial, MemoryError
        tmp_addr = self._buff_it + self.buffer_initial
        tmp_name: str = str(tmp_addr)
        self._buff_it += var_type.size()
        try:
            self._buffer[tmp_name] = [tmp_addr, var_type]
        except KeyError:
            assert KeyError
        return tmp_addr

    def get_var(self, var: str) -> int:
        assert self.is_initialized(var), "variable not initialized"
        if var in self._vars:
            return self._vars[var][0]
        if var in self._buffer:
            return self._buffer[var][0]
        pytest.fail(KeyError)

    def get_var_type(self, var: str) -> VarType:
        var = str(var)  # just in case
        assert self.is_initialized(var), "variable not initialized"
        if var in self._buffer:
            return self._buffer[var][1]
        if var in self._vars:
            self._vars.values()
            return self._vars[var][1]
        pytest.fail(KeyError)

    def clear_buffer(self):
        self._buff_it = self.buffer_initial
        self._buffer = {}

    def check_vars_type(self, var1: str, var2: str) -> bool:
        type1: VarType = self.get_var_type(var1)
        type2: VarType = self.get_var_type(var2)
        if type1 == type2:
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
            Cmd.XOR: bin(17).zfill(16).replace("0b", ""),  # Логический xor acc
            Cmd.JMP: bin(18).zfill(16).replace("0b", ""),  # Совершить переход на pmem
            Cmd.JZ: bin(19).zfill(16).replace("0b", ""),  # Переход на pmem при установленном zero флаге 1
            Cmd.JZC: bin(20).zfill(16).replace("0b", ""),  # Переход на pmem при неустановленном zero флаге
            Cmd.MULT: bin(21).zfill(16).replace("0b", ""),  # Умножить аккумулятор на знач. Из ячейки пам.
            Cmd.DIV: bin(22).zfill(16).replace("0b", ""),  # Разделить аккумулятор на знач. Из ячейки пам.
            Cmd.MOD: bin(23).zfill(16).replace("0b", ""),  # Остаток от деления аккумулятора на знач. Ячейки
            Cmd.POW: bin(24).zfill(16).replace("0b", ""),  # Возвести аккумулятор в степень из ячейки пам.
            Cmd.SQRT: bin(25).zfill(16).replace("0b", ""),  # Подсчитать корень знач. Аккумулятора
            Cmd.IN: bin(26).zfill(16).replace("0b", ""),  # Ввод символьного значения с внешнего устройства
            Cmd.OUT: bin(27).zfill(16).replace("0b", ""),  # Вывод символьного значения на внешнее устройство
            Cmd.LDREF: bin(28).zfill(16).replace("0b", ""),  # Относительная загрузка из памяти
            Cmd.SAVEREF: bin(29).zfill(16).replace("0b", ""),  # Относительное сохранение в память
        }

    def gen(self, cmd: Cmd, arg: int = 0) -> int:
        instruction = ""
        assert is_bounced_16(arg), "Argument out of bounced"
        instruction += self.cmd_codes[cmd]
        instruction += to_twos_complement(arg)
        self.instr_buf.append(instruction)
        index = len(self.instr_buf) - 1
        print(index, instruction, "#", cmd.name, arg)
        return index  # возвращаем индекс инструкции в буфере

    def change_instruction(self, index: int, cmd: Cmd, arg: int = 0) -> int:
        assert 0 <= index < len(self.instr_buf), IndexError
        assert is_bounced_16(arg), "Argument out of bounced"
        if cmd == cmd.HALT or cmd == cmd.NOP:
            arg = 0  # чтобы можно было посчитать точки выхода на этапе компиляции
        instruction = self.cmd_codes[cmd] + to_twos_complement(arg)
        print(index, "change instr", index, "to", instruction, "#", cmd.name, arg)
        self.instr_buf[index] = instruction
        return len(self.instr_buf) - 1  # возвращаем индекс инструкции в буфере

    def get_instr_buf(self) -> list:
        return self.instr_buf

    def get_instr_buf_size(self) -> int:
        return len(self.instr_buf)

    def get_binary_code(self) -> list:
        return self.instr_buf

    def check_inst_buf(self) -> bool:
        inst_halt = self.cmd_codes[Cmd.NOP] + to_twos_complement(0)
        if self.instr_buf.count(inst_halt) == 0:
            return True
        return False

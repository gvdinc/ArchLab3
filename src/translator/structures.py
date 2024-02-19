from __future__ import annotations

import typing
from enum import Enum

import pytest

import src.translator.s_lexer as lexer
from src.transit.cmds import Cmd, cmd_codes


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
    d_positive_value = abs(num) - 1
    flipped_bits = bin(d_positive_value ^ (2**bit_length - 1))[2:]
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
    def __init__(self, sovcode_file: str = ""):
        self._var_it = 0
        self._buff_it = 0
        self.data_addr_total: int = 65536  # ячеек памяти всего
        self.io_addr_total: int = 8  # портов ввода/вывода всего
        self.buffer_initial: int  # адрес начала буфера
        self.variables_count: typing.ClassVar = {
            ExprType.VAR_CEL.value: 0,  # 1 ячейка (4 байта)
            ExprType.VAR_SYM.value: 0,  # 1 ячейка (4 байта)
            ExprType.VAR_STR.value: 0,  # 256 ячеек (1060 байт)
        }
        self._vars: typing.ClassVar = {
            # var = [addr, type] type = (int, char, str)
        }
        self._buffer: typing.ClassVar = {
            # val = [addr, type] type = (int, char, str)
        }
        self.io_ports: typing.ClassVar = {"in": 0, "out": 1}

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
            addr_count_sum = (
                self.variables_count[ExprType.VAR_CEL.value] * VarType.INT.size()
                + self.variables_count[ExprType.VAR_SYM.value] * VarType.CHAR.size()
                + self.variables_count[ExprType.VAR_STR.value] * VarType.STR.size()
            )
            self.buffer_initial = addr_count_sum + 1
            print("Кол-во переменных: ", self.variables_count)
            print(
                "Адресов Занято: ",
                addr_count_sum,
                "; Свободно: ",
                self.data_addr_total - addr_count_sum,
                "; Начало буфера: ",
                self.buffer_initial,
                sep="",
            )

    def is_initialized(self, var: str):
        return var in self._vars or var in self._buffer

    def allocate_var(self, var: str, var_type: VarType) -> int:
        assert len(var) > 0, "Invalid variable name"
        assert (
            self._var_it + var_type.value < self.buffer_initial or self._var_it + var_type.value < self.data_addr_total
        ), "memory access out of bounds"
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
        self._buff_it = 0
        self._buffer = {}
        print("sys op: buffer cleared")

    def check_vars_type(self, var1: str, var2: str) -> bool:
        type1: VarType = self.get_var_type(var1)
        type2: VarType = self.get_var_type(var2)
        if type1 == type2:
            return True
        if type1 == VarType.CHAR and type2 == VarType.INT:
            return True
        return False


class Coder:
    def __init__(self):
        self.instr_buf: typing.ClassVar = []
        self.logs: str = ""

    def gen(self, cmd: Cmd, arg: int = 0) -> int:
        instruction = ""
        assert is_bounced_16(arg), "Argument out of bounced"
        instruction += cmd_codes[cmd]
        instruction += to_twos_complement(arg)
        self.instr_buf.append(instruction)
        index = len(self.instr_buf) - 1
        log: str = str(index) + " " + str(instruction) + " #" + str(cmd.name) + " " + str(arg)
        print(log)
        self.logs += log + ", "
        return index  # возвращаем индекс инструкции в буфере

    def get_log(self) -> str:
        return self.logs

    def change_instruction(self, index: int, cmd: Cmd, arg: int = 0) -> int:
        assert 0 <= index < len(self.instr_buf), IndexError
        assert is_bounced_16(arg), "Argument out of bounced"
        if cmd == cmd.HALT or cmd == cmd.NOP:
            arg = 0  # чтобы можно было посчитать точки выхода на этапе компиляции
        instruction = cmd_codes[cmd] + to_twos_complement(arg)
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
        inst_halt = cmd_codes[Cmd.NOP] + to_twos_complement(0)
        if self.instr_buf.count(inst_halt) == 0:
            return True
        return False

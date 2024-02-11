"""Транслятор sovcode в машинный код.
"""
import re
import struct

import pytest
import s_lexer as lexer
import s_parser as parser
from structures import Cmd, Coder, ExprType, MemStat, VarType

mem_stat = MemStat()
coder = Coder()


def float_to_binary(float_number):
    # Преобразовываем число с плавающей запятой в битовое представление по стандарту IEEE 754
    binary_representation = bin(struct.unpack("!I", struct.pack("!f", float(float_number)))[0])[2:]
    # Дополнить нулями до 32 бит (для чисел float)
    return binary_representation.zfill(32)


def char_to_utf8(char):
    # Преобразуем символ в его код UTF-8
    utf8_bytes = char.encode("utf-8")
    # Преобразуем байты UTF-8 в строку шестнадцатеричных чисел
    utf8_hex_string = utf8_bytes.hex()
    # Преобразуем шестнадцатеричную строку в десятичное число
    return int(utf8_hex_string, 16)


def binary_32_16_split(value: int):
    # Преобразуем целое число в 32-битное двоичное значение, убирая префикс '0b'
    binary_value = bin(value & 0xFFFFFFFF)[2:].zfill(32)
    # Разделим 32-битное двоичное значение на старшую и младшую части по 16 бит
    upper_half = binary_value[:16]
    lower_half = binary_value[16:]
    return int(upper_half, 2), int(lower_half, 2)


def is_nested(expression):
    # Проверка, является ли текущий элемент кортежем (элементом выражения)
    # Возвращает индекс вложенного элемента, если тот найден
    if isinstance(expression, tuple):
        nested_i = 1
        # Если текущий элемент - выражение, проверяем его элементы
        for elem in expression[1:]:
            # Если хотя бы один из элементов также является кортежем, то вложен
            if isinstance(elem, tuple):
                return nested_i
            # Если текущий элемент - идентификатор переменной, то нет
            if expression[0] == ExprType.IDENTIFIER.value or expression[0] == ExprType.LITERAL.value:
                return 0
            nested_i += 1
    # Если текущий элемент не является кортежем (не выражение), то операция не вложенная
    return 0


def classify_liter_type(expression: str):
    float_pattern = r"([[1-9][0-9]+|0)\.[0-9]+"
    int_pattern = "([1-9][0-9]+|0)"
    char_pattern = "."
    str_pattern = ".*"
    if re.fullmatch(int_pattern, expression):
        return VarType.FLOAT
    if re.fullmatch(float_pattern, expression):
        return VarType.INT
    if re.fullmatch(char_pattern, expression):
        return VarType.CHAR
    if re.fullmatch(str_pattern, expression):
        return VarType.STR
    pytest.fail("unknown type expression")


def classify_type(type_str: str):
    if type_str == ExprType.VAR_CEL.value:
        return VarType.INT
    if type_str == ExprType.VAR_VES.value:
        return VarType.FLOAT
    if type_str == ExprType.VAR_SYM.value:
        return VarType.CHAR
    if type_str == ExprType.VAR_STR.value:
        return VarType.STR
    pytest.fail(TypeError)


def liter_to_assembly(liter: str, variable_type: VarType):
    if variable_type == VarType.INT:
        return int(liter)
    if variable_type == VarType.FLOAT:
        binary_representation = float_to_binary(liter)
        return int(binary_representation, 2)
    if variable_type == VarType.CHAR:
        return char_to_utf8(liter)
    pytest.fail("unknown type expression")


def generate_ldm_instruction(self, var: str):
    # Загрузка переменной в память. Выполняется за 2 такта.
    addr = mem_stat.get_var(var)
    coder.gen(Cmd.ADD.value, addr)


def inst_save_var(expression: tuple) -> int:
    if expression[0] == ExprType.ASSIGNMENT.value:
        var_type: VarType = VarType.INT
        var_name: str = ""
        var_addr: int = -1
        val_addr: int = -1  # если не назначит далее, генератор выбросит ошибку
        if len(expression) == 3:  # вид ('assignment', 'x', $215)
            var_type = mem_stat.get_var_type(expression[1])
            var_name = expression[1]
            val_addr = expression[2]
            var_addr = mem_stat.get_var(var_name)  # меняем значение переменной
        elif len(expression) == 4:  # вид инициализации ('assignment', 'цел', 'север', $80)
            var_type = classify_type(expression[1])
            var_name = expression[2]
            val_addr = expression[3]
            var_addr = mem_stat.allocate_var(var_name, var_type)  # первично инициализируем
        coder.gen(Cmd.LDM, val_addr)  # загружаем значение из буфера в аккумулятор
        coder.gen(Cmd.SAVE, var_addr)  # сохраняем переменную
        print("saved var", var_name, "= buf", val_addr, "to addr", hex(var_addr), "; expr: ", expression)
        return var_addr
    pytest.fail(SyntaxError)


def inst_save_liter(expression: tuple):
    assert is_nested(expression) <= 0, "invalid expression"
    # Загрузка переменной в память. Выполняется за 2 такта.
    tmp_var_type = classify_liter_type(expression[1])  # определяем тип литера
    tmp_var_val = liter_to_assembly(expression[1], tmp_var_type)  # приводим значение к значению к хранимому
    tmp_var_addr = mem_stat.allocate_tmp(tmp_var_val, tmp_var_type)  # выбор ячейки хранения
    upper, lower = binary_32_16_split(tmp_var_val)
    coder.gen(Cmd.LDI, upper)
    coder.gen(Cmd.LSL, 8)
    coder.gen(Cmd.LDI, lower)
    coder.gen(Cmd.SAVE, tmp_var_addr)
    print("saved tmp val", tmp_var_addr, "=", tmp_var_val, "to buffer addr", hex(tmp_var_addr), "; expr: ", expression)
    return tmp_var_addr


def instr_handler_unary(expression: tuple):  # noqa: C901
    expr_list = list(expression)
    if expr_list[0] == ExprType.LITERAL.value:  # литерал
        return inst_save_liter(expression)
    if expr_list[0] == ExprType.IDENTIFIER.value:  # переменная
        return mem_stat.get_var(expression[1])  # возвращаем адрес переменной в dmem
    if expr_list[0] == ExprType.MINUS.value:  # унарный минус
        return expression[1]
    if expr_list[0] == ExprType.SQRT.value:
        instr = coder.gen(Cmd.SQRT, expression[1])
    if expr_list[0] == ExprType.INCR.value:
        pass
    if expr_list[0] == ExprType.DECR.value:
        pass
    if expr_list[0] == ExprType.LOGICAL_NOT.value:
        pass
    if expr_list[0] == ExprType.BITWISE_NOT.value:
        pass
    if expr_list[0] == ExprType.LOGICAL_XOR.value:
        pass
    if expr_list[0] == ExprType.TO_STR.value:
        pass
    pytest.fail(SyntaxError)


def instr_handler_quadro(expression: tuple):
    expr_list = list(expression)
    if expr_list[0] == ExprType.ASSIGNMENT.value:  # первичная инициализация
        return inst_save_var(expression)
    pytest.fail(SyntaxError)


def simplify_expression(expression: tuple) -> int:
    expr_list = list(expression)
    nested_i = is_nested(expression)
    supervisor = 0
    while nested_i > 0:  # 1, 2, 3
        expr_list[nested_i] = simplify_expression(expression[nested_i])  # список с упрощённым nested_i
        # если это не литерал, то все выражения уже лежат в instr_buf
        expression = tuple(expr_list)
        nested_i = is_nested(expression)  # поиск новой вложенности
        supervisor += 1
        assert supervisor < len(expression), RecursionError  # защита от бесконечной рекурсии
    # expression выражение не вложено - упрощаем
    assert 1 <= len(expr_list) <= 4, RecursionError  # полностью приведенное выражение до 4 вхождений
    if len(expr_list) == 1:  # адрес ячейки или значение (214)
        return expression[0]
    if len(expr_list) == 2:  # Унарные операторы и литералы ('literal', '18')
        return instr_handler_unary(expression)
    if len(expr_list) == 3:
        pass
    if len(expr_list) == 4:
        return instr_handler_quadro(expression)
    pytest.fail(SyntaxError)


def translate(ops):
    res: int
    for op in ops:
        if op[0] == ExprType.ASSIGNMENT.value:
            if is_nested(op):
                res = simplify_expression(op)
            else:
                pytest.fail(SyntaxError)
            print("$", res, "--", op)
            continue
    # Обработка операции цикла
    # Генерация машинного кода для условного перехода и проверки условия цикла
    # Рекурсивный вызов translate для тела цикла
    # Другие операции


def init_mem_stat(sovcode_file: str):
    tokens = lexer.tokenize(sovcode_file)
    for token in tokens:
        if token.type == "VAR_CEL":
            mem_stat.variables_count[ExprType.VAR_CEL.value] += 1
            continue
        if token.type == "VAR_VES":
            mem_stat.variables_count[ExprType.VAR_VES.value] += 1
            continue
        if token.type == "VAR_SYM":
            mem_stat.variables_count[ExprType.VAR_SYM.value] += 1
            continue
        if token.type == "VAR_STR":
            mem_stat.variables_count[ExprType.VAR_STR] += 1
            continue
    addr_count_sum = (mem_stat.variables_count[ExprType.VAR_CEL.value] * VarType.INT.value +
                      mem_stat.variables_count[ExprType.VAR_VES.value] * VarType.FLOAT.value +
                      mem_stat.variables_count[ExprType.VAR_SYM.value] * VarType.CHAR.value +
                      mem_stat.variables_count[ExprType.VAR_STR.value] * VarType.STR.value)
    mem_stat.buffer_initial = addr_count_sum + 1
    print("Кол-во переменных: ", mem_stat.variables_count)
    print("Адресов Занято: ", addr_count_sum, "; Свободно: ", mem_stat.data_addr_total - addr_count_sum,
          "; Начало буфера: ", hex(mem_stat.buffer_initial), sep="")


def main(sovcode_file: str, binary_out_file: str):
    init_mem_stat(sovcode_file)
    ops_parsed = parser.parse_sovcode(sovcode_file)
    for ops in ops_parsed:
        print(ops)
    print("")
    translate(ops_parsed)
    # Запись сгенерированного машинного кода в файл
    binary_code = coder.get_binary_code()
    print(binary_code)
    with open(binary_out_file, "wb") as f:
        for instruction in binary_code:
            instr_int = int(instruction, 2)
            f.write(instr_int.to_bytes(4, "big"))  # Примерный формат записи машинного кода
        f.flush()
        f.close()


if __name__ == "__main__":
    ussr_file = "/home/prox/projects/ArchLab3/ArchLab3/src/examples/debug.ussr"
    bin_file = "/home/prox/projects/ArchLab3/ArchLab3/out/binary"
    main(ussr_file, bin_file)

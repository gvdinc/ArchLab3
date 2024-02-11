"""Транслятор sovcode в машинный код.
"""
import re

import pytest
import s_parser as parser
from structures import Cmd, Coder, ExprType, MemStat, VarType, binary_32_16_split, liter_to_assembly

mem_stat: MemStat
coder = Coder()


# classification
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
    float_pattern = r"(?:[1-9][0-9]*|0)\.[0-9]+"
    int_pattern = "([1-9][0-9]+|0)"
    char_pattern = "."
    str_pattern = ".*"
    if re.fullmatch(float_pattern, expression):
        return VarType.FLOAT
    if re.fullmatch(int_pattern, expression):
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
    pytest.fail(TypeError)  # end classification operations


# unary operations
def inst_op_decr(addr: int):  # ('--', 911)
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.DECR)
    coder.gen(Cmd.SAVE, addr)


def inst_op_incr(addr: int):  # ('++', 911)
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.INCR)
    coder.gen(Cmd.SAVE, addr)


def inst_op_not(addr: int):
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.NOT)
    coder.gen(Cmd.SAVE, addr)


def inst_op_uminus(addr: int):
    coder.gen(Cmd.LSL, 32)  # обнулим аккумулятор
    coder.gen(Cmd.SUB, addr)  # вычтем положительное знач
    coder.gen(Cmd.SAVE, addr)  # сохраним


def inst_op_sqrt(addr: int):
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.SQRT)
    coder.gen(Cmd.SAVE, addr)  # end unary operations


# binary operations
def inst_op_divide(addr1: int, addr2: int, res_addr: int):  # ('/', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.DIV, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_equals(addr1: int, addr2: int, res_addr: int):  # ('==', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.NOT)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_greater_than(addr1: int, addr2: int, res_addr: int):  # ('>', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.DECR)
    coder.gen(Cmd.CMP)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_less_than(addr1: int, addr2: int, res_addr: int):  # ('<', 12, 15)
    coder.gen(Cmd.LDM, addr2)
    coder.gen(Cmd.SUB, addr1)
    coder.gen(Cmd.DECR)
    coder.gen(Cmd.CMP)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_and(addr1: int, addr2: int, res_addr: int):  # ('&', 12, 15)

    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.AND, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_or(addr1: int, addr2: int, res_addr: int):  # ('|', 12, 15)

    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.OR, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_minus(addr1: int, addr2: int, res_addr: int):  # ('-', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_mod(addr1: int, addr2: int, res_addr: int):  # ('%', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.MOD, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_not_equals(addr1: int, addr2: int, res_addr: int):  # ('!=', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_plus(addr1: int, addr2: int, res_addr: int):  # ('+', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.ADD, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_power(addr1, addr2, res_addr):  # ('**', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.POW, addr2)
    coder.gen(Cmd.SAVE, res_addr)


def inst_op_times(addr1, addr2, res_addr):  # ('*', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.MULT, addr2)
    coder.gen(Cmd.SAVE, res_addr)  # end binary operations


# memory var/buff operations
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
        print("var [", var_name, "= buf", val_addr, "] saved to addr", var_addr, "; expr: ", expression)
        return var_addr
    pytest.fail(SyntaxError)


def inst_save_liter(expression: tuple):
    assert is_nested(expression) <= 0, "invalid expression"
    # Загрузка переменной в память. Выполняется за 2 такта.
    tmp_var_type = classify_liter_type(expression[1])  # определяем тип литера
    tmp_var_val = liter_to_assembly(expression[1], tmp_var_type)  # приводим значение к значению к хранимому
    tmp_var_addr = mem_stat.allocate_tmp(tmp_var_type, tmp_var_val)  # выбор ячейки хранения
    upper, lower = binary_32_16_split(tmp_var_val)
    coder.gen(Cmd.LDI, upper)
    coder.gen(Cmd.LSL, 16)  # сдвиг старших бит в старшие разряды
    coder.gen(Cmd.LDI, lower)
    coder.gen(Cmd.SAVE, tmp_var_addr)
    print("tmp [ val", tmp_var_addr, "=", tmp_var_val, "] saved to buffer addr", hex(tmp_var_addr), "; expr: ",
          expression)
    return tmp_var_addr


# handlers
def instr_handler_unary(expression: tuple):  # noqa: C901
    expr_list = list(expression)
    if expr_list[0] == ExprType.LITERAL.value:  # литерал
        return inst_save_liter(expression)
    if expr_list[0] == ExprType.IDENTIFIER.value:  # переменная
        return mem_stat.get_var(expression[1])  # возвращаем адрес переменной в dmem
    addr = expression[1]
    if expr_list[0] == ExprType.DECR.value:
        inst_op_decr(addr)
    elif expr_list[0] == ExprType.INCR.value:
        inst_op_incr(addr)
    elif expr_list[0] == ExprType.LOGICAL_NOT.value:
        inst_op_not(addr)
    elif expr_list[0] == ExprType.MINUS.value:  # унарный минус
        inst_op_uminus(addr)
    elif expr_list[0] == ExprType.SQRT.value:
        inst_op_sqrt(addr)
    else:
        pytest.fail(SyntaxError)
    return addr


def instr_handler_binary(expression: tuple):  # noqa: C901
    expr_list = list(expression)
    addr1: int = expression[1]
    addr2: int = expression[2]
    assert mem_stat.check_vars_type(str(addr1), str(addr2)), TypeError
    var_type = mem_stat.get_var_type(str(addr1))
    res_addr: int = mem_stat.allocate_tmp(var_type)

    if expr_list[0] == ExprType.DIVIDE.value:
        inst_op_divide(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.EQUALS.value:
        inst_op_equals(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.GREATER_THAN.value:
        inst_op_greater_than(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.LESS_THAN.value:
        inst_op_less_than(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.LOGICAL_AND.value:
        inst_op_and(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.LOGICAL_OR.value:
        inst_op_and(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.MINUS.value:
        inst_op_minus(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.MOD.value:
        inst_op_mod(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.NOT_EQUALS.value:
        inst_op_not_equals(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.PLUS.value:
        inst_op_plus(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.POWER.value:
        inst_op_power(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.TIMES.value:
        inst_op_times(addr1, addr2, res_addr)
    else:
        pytest.fail(SyntaxError)
    return res_addr


def instr_handler_ternary(expression: tuple):
    expr_list = list(expression)
    if expr_list[0] == ExprType.ASSIGNMENT.value:  # первичная инициализация
        return inst_save_var(expression)
    pytest.fail(SyntaxError)


# simplification operations
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
    if len(expr_list) == 3:  # Бинарные операции (a + b)
        return instr_handler_binary(expression)
    if len(expr_list) == 4:  # Тернарные (высшие) операции - (assign, if, not_if, while)
        return instr_handler_ternary(expression)
    pytest.fail(SyntaxError)


def translate(ops):
    for op in ops:  # операции верхнего уровня
        if op[0] == ExprType.ASSIGNMENT.value:  # операция присвоения
            if is_nested(op):
                var_addr: int = simplify_expression(op)
            else:
                pytest.fail(SyntaxError)
            print("\n$", var_addr, "--", op)
            continue
        # elif op[0] == ExprType.
    # Обработка операции цикла
    # Генерация машинного кода для условного перехода и проверки условия цикла
    # Рекурсивный вызов translate для тела цикла
    # Другие операции


def main(sovcode_file: str, binary_out_file: str):
    global mem_stat
    mem_stat = MemStat(sovcode_file)
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
    ussr_file = "/home/prox/projects/ArchLab3/ArchLab3/src/examples/math.ussr"
    bin_file = "/home/prox/projects/ArchLab3/ArchLab3/out/binary"
    main(ussr_file, bin_file)

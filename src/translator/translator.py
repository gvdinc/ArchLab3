"""Транслятор sovcode в машинный код.
"""
import re
import sys

import pytest
import s_parser as parser
from structures import Cmd, Coder, ExprType, MemStat, VarType, binary_32_16_split, liter_to_assembly

mem_stat: MemStat
coder = Coder()
default_in_port = 0
default_out_port = 1


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
    int_pattern = r"(?:[1-9][0-9]*|0)"
    char_pattern = r"^.$"
    str_pattern = r"^.*$"
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
    print("op: ", "--", addr, sep="")


def inst_op_incr(addr: int):  # ('++', 911)
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.INCR)
    coder.gen(Cmd.SAVE, addr)
    print("op: ", "++", addr, sep="")


def inst_op_not(addr: int):
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.NOT)
    coder.gen(Cmd.SAVE, addr)
    print("op: ", "!", addr, sep="")


def inst_op_uminus(addr: int):
    coder.gen(Cmd.LSL, 32)  # обнулим аккумулятор
    coder.gen(Cmd.SUB, addr)  # вычтем положительное знач
    coder.gen(Cmd.SAVE, addr)  # сохраним
    print("op: ", "-", addr, sep="")


def inst_op_sqrt(addr: int):
    coder.gen(Cmd.LDM, addr)
    coder.gen(Cmd.SQRT)
    coder.gen(Cmd.SAVE, addr)  # end unary operations
    print("op: ", "//", addr, sep="")


# binary operations
def inst_op_divide(addr1: int, addr2: int, res_addr: int):  # ('/', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.DIV, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "/", addr2, "save to buffer", res_addr)


def inst_op_equals(addr1: int, addr2: int, res_addr: int):  # ('==', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.NOT)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "==", addr2, "save to buffer", res_addr)


def inst_op_greater_than(addr1: int, addr2: int, res_addr: int):  # ('>', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.DECR)
    coder.gen(Cmd.CMP)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, ">", addr2, "save to buffer", res_addr)


def inst_op_less_than(addr1: int, addr2: int, res_addr: int):  # ('<', 12, 15)
    coder.gen(Cmd.LDM, addr2)
    coder.gen(Cmd.SUB, addr1)
    coder.gen(Cmd.DECR)
    coder.gen(Cmd.CMP)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "<", addr2, "save to buffer", res_addr)


def inst_op_and(addr1: int, addr2: int, res_addr: int):  # ('&', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.AND, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "&", addr2, "save to buffer", res_addr)


def inst_op_or(addr1: int, addr2: int, res_addr: int):  # ('|', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.OR, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "|", addr2, "save to buffer", res_addr)


def inst_op_xor(addr1: int, addr2: int, res_addr: int):  # ('^', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.XOR, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "^", addr2, "save to buffer", res_addr)


def inst_op_minus(addr1: int, addr2: int, res_addr: int):  # ('-', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "-", addr2, "save to buffer", res_addr)


def inst_op_mod(addr1: int, addr2: int, res_addr: int):  # ('%', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.MOD, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "%", addr2, "save to buffer", res_addr)


def inst_op_not_equals(addr1: int, addr2: int, res_addr: int):  # ('!=', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.SUB, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "!=", addr2, "save to buffer", res_addr)


def inst_op_plus(addr1: int, addr2: int, res_addr: int):  # ('+', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.ADD, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "+", addr2, "save to buffer", res_addr)


def inst_op_power(addr1, addr2, res_addr):  # ('**', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.POW, addr2)
    coder.gen(Cmd.SAVE, res_addr)
    print("op:", addr1, "**", addr2, "save to buffer", res_addr)


def inst_op_times(addr1, addr2, res_addr):  # ('*', 12, 15)
    coder.gen(Cmd.LDM, addr1)
    coder.gen(Cmd.MULT, addr2)
    coder.gen(Cmd.SAVE, res_addr)  # end binary operations
    print("op:", addr1, "*", addr2, "save to buffer", res_addr)


# memory var/buff operations
def inst_save_var(expression: tuple) -> int:
    if expression[0] == ExprType.ASSIGNMENT.value:
        var_name: str = ""
        var_addr: int = -1
        val_addr: int = -1  # если не назначит далее, генератор выбросит ошибку
        if len(expression) == 3:  # вид ('assignment', 'x', $215)
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


def inst_save_liter(expression: tuple) -> int:
    assert is_nested(expression) <= 0, "invalid expression"
    # Загрузка переменной в память. Выполняется за 2 такта.
    tmp_var_type = classify_liter_type(expression[1])  # определяем тип литера
    tmp_var_val = liter_to_assembly(expression[1], tmp_var_type)  # приводим значение к значению к хранимому
    tmp_var_addr = mem_stat.allocate_tmp(tmp_var_type)  # выбор ячейки хранения
    upper, lower = binary_32_16_split(tmp_var_val)
    coder.gen(Cmd.LDI, upper)
    coder.gen(Cmd.LSL, 16)  # сдвиг старших бит в старшие разряды
    coder.gen(Cmd.LDI, lower)
    coder.gen(Cmd.SAVE, tmp_var_addr)
    print("tmp [ val", tmp_var_addr, "=", tmp_var_val, "] saved to buffer addr", tmp_var_addr, "; expr: ",
          expression)
    return tmp_var_addr


def inst_save_var_to_buffer(var_name: str) -> int:
    var_addr = mem_stat.get_var(var_name)
    var_type = mem_stat.get_var_type(var_name)
    tmp_addr = mem_stat.allocate_tmp(var_type)
    coder.gen(Cmd.LDM, var_addr)
    coder.gen(Cmd.LDI, tmp_addr)
    print("op: var", var_addr, " to buffer:", tmp_addr)
    return tmp_addr


def inst_save_symbol(sym: str, addr: int):
    assert len(sym) == 1, ValueError
    code: int = ord(sym)
    code_lower: int = code % (2 ** 16)
    code_upper: int = code // (2 ** 16)

    coder.gen(Cmd.LDI, code_upper)
    coder.gen(Cmd.LSL, 16)  # верхнюю часть в старшие разряды, заодно затерли мусор
    coder.gen(Cmd.LDI, code_lower)
    coder.gen(Cmd.SAVE, addr)
    print("\033[3mop: save sym \'", sym, "\' to addr: ", addr, "\033[0m", sep="")


def inst_save_string(expression: tuple) -> int:
    assert 3 <= len(expression) <= 4, SyntaxError
    if len(expression) == 4:  # инициализировать ('assignment', 'текст', 'а', ('literal', '☭СССР☭'))
        assert expression[3][0] == ExprType.LITERAL.value, SyntaxError
        var_name = expression[2]
        line: str = expression[3][1]
        assert isinstance(line, str), SyntaxError
        length: int = len(line)
        assert length <= 255, "Line is too big"
        var_addr: int = mem_stat.allocate_var(var_name, VarType.STR)
    else:  # переопределить ('assignment', 'а', ('literal', '☭СССР☭'))
        assert expression[2][0] == ExprType.LITERAL.value, SyntaxError
        var_name = expression[1]
        line: str = expression[2][1]
        assert isinstance(line, str), SyntaxError
        length: int = len(line)
        assert length <= 255, "Line is too big"
        var_addr: int = mem_stat.get_var(var_name)

    coder.gen(Cmd.LSL, 32)  # acc = 0
    coder.gen(Cmd.LDI, length)  # acc(lower) = line length
    coder.gen(Cmd.SAVE, var_addr)  # сохраняем первую (служебную) ячейку длины строки
    print("\033[3mop: save str length to addr", var_addr, "\033[0m")
    for i in range(1, length + 1):
        inst_save_symbol(line[i - 1], var_addr + i)
    print("\033[34m", "$(str)", var_addr, "--", "save result", line, "\033[0m", "\n")
    return -1


def inst_read(expression: tuple, port: int = default_in_port) -> int:  # ('read', 'a')
    assert not isinstance(expression[1], tuple), SyntaxError
    assert 2 <= len(expression) <= 3, SyntaxError
    var = expression[1]
    addr = mem_stat.get_var(var) if mem_stat.is_initialized(var) else mem_stat.allocate_var(var, VarType.CHAR)
    coder.gen(Cmd.IN, port)
    coder.gen(Cmd.SAVE, addr)
    print("\033[34m", "op: read from port (", port, ") to var ", var, "(", addr, ")",
          " instructions complete! \033[0m\n", sep="")
    return -1


def inst_write_line(addr: int, port: int = default_out_port):
    # подразумевается что уже проверено, что по адресу расположена str переменная (начало)
    coder.gen(Cmd.LDM, addr)  # длина строки N
    addr_it = mem_stat.allocate_tmp(VarType.INT)
    addr_end = mem_stat.allocate_tmp(VarType.INT)

    coder.gen(Cmd.LDI, addr)  # acc = addr начала строки (ячейка хранит размер строки)
    coder.gen(Cmd.ADD, addr)  # прибавим длину к началу строки
    coder.gen(Cmd.INCR)  # addr + len + 1 = адрес конца строки (не включительно)
    coder.gen(Cmd.SAVE, addr_end)  # сохраняем в addr_end
    coder.gen(Cmd.LSL, 32)  # acc = 0
    coder.gen(Cmd.SAVE, addr_it)  # addr_it = 0
    # Цикл - N раз напечатаем символ в N + iter в строку
    repeat_index = coder.get_instr_buf_size()
    coder.gen(Cmd.LDM, addr_it)  # acc = addr_it
    coder.gen(Cmd.INCR)  # acc++
    coder.gen(Cmd.SAVE, addr_it)  # addr_it += 1
    coder.gen(Cmd.SUB, addr_end)  # acc = addr_it - addr_end
    promise_out_index = coder.gen(Cmd.NOP)  # promised JZ out_index
    coder.gen(Cmd.LDM, addr_it)
    coder.gen(Cmd.REF)  # reference flag
    coder.gen(Cmd.LDM, addr)  # acc = dmem[addr + addr_it] (char) referenced
    coder.gen(Cmd.OUT, port)  # вывод addr_it(ого) символа строки
    coder.gen(Cmd.JMP, repeat_index)

    out_index = coder.get_instr_buf_size()  # получаем индекс следующей инструкции
    coder.change_instruction(promise_out_index, Cmd.JZ, out_index)  # выставляем условный переход на выход из цикла
    return -1


def inst_write(expression: tuple, port: int = default_out_port):  # ('write', ('identifier', 'а'))
    assert isinstance(expression[1], tuple), SyntaxError
    assert 2 <= len(expression) <= 3, SyntaxError
    var: str = expression[1][1] if expression[1][0] == ExprType.IDENTIFIER.value else ""
    assert mem_stat.is_initialized(var), MemoryError
    var_type = mem_stat.get_var_type(var)
    addr = mem_stat.get_var(var)
    assert var_type == VarType.CHAR or var_type == VarType.STR, TypeError

    if var_type == VarType.CHAR:  # CHAR
        coder.gen(Cmd.LDM, addr)
        coder.gen(Cmd.OUT, port)
    else:  # STR
        inst_write_line(addr, port)
    print("\033[34mop: write var ", var, "(", addr, ") to port (", port, ") instructions complete! \033[0m\n", sep="")
    return -1


# handlers
def instr_handler_unary(expression: tuple):  # noqa: C901
    expr_list = list(expression)
    if expr_list[0] == ExprType.LITERAL.value:  # литерал
        return inst_save_liter(expression)
    if expr_list[0] == ExprType.IDENTIFIER.value:  # переменная
        return inst_save_var_to_buffer(expression[1])  # копируем переменную в буфер
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

    if expr_list[0] == ExprType.ASSIGNMENT.value:  # присваивание
        return inst_save_var(expression)
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
        inst_op_or(addr1, addr2, res_addr)
    elif expr_list[0] == ExprType.LOGICAL_XOR.value:
        inst_op_xor(addr1, addr2, res_addr)
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
        return int(expression[0])
    if len(expr_list) == 2:  # Унарные операторы и литералы ('literal', '18') или ('identifier', 'var_name')
        return instr_handler_unary(expression)
    if len(expr_list) == 3:  # Бинарные операции (a + b)
        return instr_handler_binary(expression)
    if len(expr_list) == 4:  # Тернарные (высшие) операции - (assign, if, not_if, while)
        return instr_handler_ternary(expression)
    pytest.fail(SyntaxError)


# translators
def translate_assignment(expression: tuple) -> int:
    if len(expression) == 4:  # операция первичного присвоения
        if is_nested(expression):
            var_addr: int = simplify_expression(expression)
        else:
            pytest.fail(SyntaxError)
        print("\033[34m", "$", var_addr, "--", "save result", expression[2], "\033[0m", "\n")
        return -1

    if len(expression) == 3:  # операция переопределения
        if is_nested(expression):
            var_addr: int = simplify_expression(expression)
        else:
            pytest.fail(SyntaxError)
        print("\033[34m", "$", var_addr, "--", "save result", expression[1], "\033[0m", "\n")
        return -1
    pytest.fail(SyntaxError)


def translate_if_expression(expression: tuple, rec_depth: int) -> int:
    assert len(expression) == 3, SyntaxError
    condition: tuple = expression[1]
    condition_res_addr: int = simplify_expression(condition)
    coder.gen(Cmd.LDM, condition_res_addr)
    coder.gen(Cmd.CMP)
    promise_out_index = coder.gen(Cmd.NOP)  # promised Cmd.JZ promise, потом заменим на jump
    translate(expression[2][0], rec_depth + 1)  # (do smth)
    out_index = coder.get_instr_buf_size()  # получаем индекс следующей инструкции
    coder.change_instruction(promise_out_index, Cmd.JZ, out_index)  # выставляем условный переход (если ложь)
    print("\033[34m", "if_else (depth =", rec_depth, ") instructions complete -- ",
          condition, "| out", out_index, "\033[0m", "\n", sep="")
    return -1


def translate_if_else_expression(expression: tuple, rec_depth: int) -> int:
    assert len(expression) == 4, SyntaxError
    condition: tuple = expression[1]
    condition_res_addr: int = simplify_expression(condition)
    coder.gen(Cmd.LDM, condition_res_addr)
    coder.gen(Cmd.CMP)
    promise_else_index = coder.gen(Cmd.NOP)  # promised Cmd.JZ promise, потом заменим на jump
    translate(expression[2][0], rec_depth + 1)  # (do smth1)
    promise_out_index = coder.gen(Cmd.NOP)  # promised Cmd.JUM promise, потом заменим на jump
    else_index = coder.get_instr_buf_size()  # получаем индекс следующей инструкции
    coder.change_instruction(promise_else_index, Cmd.JZ, else_index)  # выставляем условный переход (на else)
    translate(expression[3][0], rec_depth + 1)  # (do smth2)
    out_index = coder.get_instr_buf_size()  # получаем индекс следующей инструкции
    coder.change_instruction(promise_out_index, Cmd.JMP, out_index)
    print("\033[34m", "if_else (depth =", rec_depth, ") instructions complete -- ",
          condition, "| else", else_index, " out", out_index, "\033[0m", "\n", sep="")
    return -1


def translate_while_expression(expression: tuple, rec_depth: int) -> int:
    assert len(expression) == 3, SyntaxError

    condition: tuple = expression[1]
    condition_index: int = coder.get_instr_buf_size()
    condition_res_addr: int = simplify_expression(condition)  # начало проверки условия выхода
    coder.gen(Cmd.LDM, condition_res_addr)
    coder.gen(Cmd.CMP)  # условие проверено, флаги выставлены
    promise_out_index = coder.gen(Cmd.NOP)  # promised CMD.JZ out_index

    translate(expression[2][0], rec_depth + 1)  # тело цикла
    coder.gen(Cmd.JMP, condition_index)  # Cmd.JMP condition_index, переход на условие
    out_index = coder.get_instr_buf_size()  # получаем индекс следующей инструкции
    coder.change_instruction(promise_out_index, Cmd.JZ, out_index)  # выставляем условный переход на выход из цикла
    print("\033[34m", "while (depth =", rec_depth, ") instructions complete -- ",
          condition, "| condition", condition_index, " out", out_index, "\033[0m", "\n", sep="")
    return -1


def translate(op: tuple, rec_depth: int = 0) -> int:  # noqa: C901
    print("\033[32m{}\033[0m".format(str(op)))
    if rec_depth == 0:
        mem_stat.clear_buffer()

    if op[0] == ExprType.ASSIGNMENT.value:  # операция присвоения
        if op[1] != ExprType.VAR_STR.value:
            assert translate_assignment(op) == -1, SyntaxError
        else:
            assert inst_save_string(op) == -1, SyntaxError
        return -1
    if op[0] == ExprType.IF.value:  # условие ('if', ('>', (val1), (val2), [(do smth)])
        assert translate_if_expression(op, rec_depth) == -1, SyntaxError  # построение инструкций ветвления
        return -1
    if op[0] == ExprType.IF_ELSE.value:  # ('if-else', ('>', (val1), (val2), [(do_smth1))], [(do_smth2)])
        assert translate_if_else_expression(op, rec_depth) == -1, SyntaxError  # построение инструкций ветвления
        return -1
    if op[0] == ExprType.WHILE.value:  # ('while', ('<', ('identifier', 'a'), ('literal', '100')), [(do_smth))])
        assert translate_while_expression(op, rec_depth) == -1, SyntaxError  # построение инструкций ветвления
        return -1
    if op[0] == ExprType.READ.value:  # ('read', 'a')
        assert inst_read(op) == -1, SyntaxError
        return -1
    if op[0] == ExprType.WRITE.value:  # ('write', ('identifier', 'а'))
        assert inst_write(op) == -1, SyntaxError
        return -1
    simplify_expression(op)  # не высший порядок
    return -1


def main():
    sovcode_file: str = sys.argv[1]
    binary_out_file: str = sys.argv[2]
    global mem_stat
    mem_stat = MemStat(sovcode_file)
    ops_parsed = parser.parse_sovcode(sovcode_file)
    for op in ops_parsed:
        print(op)
    print("")

    for op in ops_parsed:  # обработаем операции верхнего уровня (if, if_else, while, read, write, assign, identify)
        # TODO: read and write
        assert translate(op) < 0, RuntimeError
    coder.gen(Cmd.HALT)  # конец программы
    print("\033[35m", "Compilation complete!", "\033[0m", sep="")

    # Запись сгенерированного машинного кода в файл
    binary_code = coder.get_binary_code()
    print(binary_code)
    with open(binary_out_file, "wb") as f:
        for instruction in binary_code:
            instr_int = int(instruction, 2)
            f.write(instr_int.to_bytes(4, "big"))  # Примерный формат записи машинного кода
        f.flush()
        f.close()


if __name__ == "__main__":  # TODO: разобраться с долбанным str
    main()

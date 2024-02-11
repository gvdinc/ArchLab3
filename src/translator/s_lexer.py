import re

import ply.lex as lex

states = (
    ("string", "exclusive"),
)

# Список токенов
tokens = [
    "IDENTIFIER",
    "INTEGER",
    "FLOAT",
    "CHAR",
    "STRING",
    "STR",
    "IF",
    "ELSE",
    "WHILE",
    "LPAREN",
    "RPAREN",
    "LBRACE",
    "RBRACE",
    "SEMICOLON",
    "COMMA",
    "ASSIGN",
    "READ",
    "WRITE",
    "TO_STR",
    "PLUS",
    "MINUS",
    "TIMES",
    "DIVIDE",
    "SQRT",
    "INCR",
    "DECR",
    "MOD",
    "POWER",
    "BITWISE_NOT",
    "LOGICAL_AND",
    "LOGICAL_OR",
    "LOGICAL_XOR",
    "LOGICAL_NOT",
    "LESS_THAN",
    "GREATER_THAN",
    "EQUALS",
    "NOT_EQUALS",
    "VAR_CEL",
    "VAR_VES",
    "VAR_SYM",
    "VAR_STR"
]

# определим регулярку для абстрактного идентификатора
ident = r"[a-zA-Zа-яА-Я_]\w*"

# Регулярные выражения для токенов
t_INTEGER = r"\d+"  # noqa: N816
t_FLOAT = r"\d+\.\d+"  # noqa: N816
t_string_STR = r'(\\.|[^$"])+'  # noqa: N816
t_string_ignore = ""
t_LPAREN = r"\("  # noqa: N816
t_RPAREN = r"\)"  # noqa: N816
t_LBRACE = r"\{"  # noqa: N816
t_RBRACE = r"\}"  # noqa: N816
t_SEMICOLON = r";"  # noqa: N816
t_COMMA = r","  # noqa: N816
t_ASSIGN = r"="  # noqa: N816
t_READ = r">>"  # noqa: N816
t_WRITE = r"<<"  # noqa: N816
t_TO_STR = r"@"  # noqa: N816
t_PLUS = r"\+"  # noqa: N816
t_MINUS = r"-"  # noqa: N816
t_TIMES = r"\*"  # noqa: N816
t_DIVIDE = r"/"  # noqa: N816
t_SQRT = r"//"  # noqa: N816
t_INCR = r"\+\+"  # noqa: N816
t_DECR = r"\-\-"  # noqa: N816
t_MOD = r"%"  # noqa: N816
t_POWER = r"\*\*"  # noqa: N816
t_BITWISE_NOT = r"~"  # noqa: N816
t_LOGICAL_AND = r"\&"  # noqa: N816
t_LOGICAL_OR = r"\|"  # noqa: N816
t_LOGICAL_XOR = r"\^"  # noqa: N816
t_LOGICAL_NOT = r"!"  # noqa: N816
t_LESS_THAN = r"<"  # noqa: N816
t_GREATER_THAN = r">"  # noqa: N816
t_EQUALS = r"=="  # noqa: N816
t_NOT_EQUALS = r"\!="  # noqa: N816
# Токены для переменных
t_VAR_CEL = r"""цел"""  # noqa: N816
t_VAR_VES = r"""вещ"""  # noqa: N816
t_VAR_SYM = r"""симв"""  # noqa: N816
t_VAR_STR = r"""текст"""  # noqa: N816


# Правила для ключевых слов
def t_IF(t):  # noqa: N802
    r"""если"""
    return t


def t_ELSE(t):  # noqa: N802
    r"""иначе"""
    return t


def t_WHILE(t):  # noqa: N802
    r"""пока"""
    return t


def t_CHAR(t):  # noqa: N802
    r"""'[a-zA-Zа-яА-Я0-9\s☭★]'"""
    t.value = t.value[1]  # Извлекаем имя переменной
    return t


# нужен в обоих состояниях, потому что двойные кавычки матчатся и там и там.
def t_ANY_STRING(t):  # noqa: N802
    r"""\""""
    if t.lexer.current_state() == "string":
        t.lexer.begin("INITIAL")  # переходим в начальное состояние
    else:
        t.lexer.begin("string")  # парсим строку
    # return t верну, если понадобятся кавычки


# ну и куда же мы без обработки ошибок
def t_string_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Игнорируем пробелы и табуляцию
t_ignore = " \r\t\f"


# Токен для идентификатора (переменной)
def t_IDENTIFIER(t):  # noqa: N802
    r"""[a-zA-Zа-яА-Я_][a-zA-Z0-9а-яА-Я_]*"""
    if t.value.lower() == "цел":
        t.type = "VAR_CEL"
    elif t.value.lower() == "вещ":
        t.type = "VAR_VES"
    elif t.value.lower() == "симв":
        t.type = "VAR_SYM"
    elif t.value.lower() == "текст":
        t.type = "VAR_STR"
    else:
        t.type = "IDENTIFIER"
    return t


# Обработка символов новой строки
def t_newline(t):
    r"""\n+"""
    t.lexer.lineno += len(t.value)


# Функция для токена комментария
def t_comment(t):
    r"""(/\*(.|\n)*?\*/)"""
    pass


# Обработка ошибок
def t_error(t):
    print(f"Неправильный символ: '{t.value[0]}'", t.lineno)
    t.lexer.skip(1)


lexer = lex.lex(reflags=re.UNICODE | re.DOTALL)


def tokenize(src: str):
    data = ""
    token_buffer = []
    try:
        with open(src) as file:
            data = file.read()
    except FileNotFoundError:
        print("File not found: {}".format(src))
        raise
    except Exception as e:
        print("An error occurred: {}".format(e))
        raise
    lexer.input(data)
    while True:
        tok = lexer.token()  # читаем следующий токен
        if not tok:
            break  # закончились печеньки
        token_buffer.append(tok)
    return token_buffer


if __name__ == "__main__":
    buf = tokenize("/home/prox/projects/ArchLab3/ArchLab3/src/examples/debug.ussr")
    for b in buf:
        print(b)

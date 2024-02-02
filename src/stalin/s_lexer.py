import ply.lex as lex
import re

states = (
    ('string', 'exclusive'),
)

# Список токенов
tokens = [
    'IDENTIFIER',
    'INTEGER',
    'FLOAT',
    'BOOLEAN',
    'CHAR',
    'STRING',
    'STR',
    'IF',
    'ELSE',
    'WHILE',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'SEMICOLON',
    'COMMA',
    'ASSIGN',
    'READ',
    'WRITE',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'MOD',
    'POWER',
    'BITWISE_NOT',
    'LOGICAL_AND',
    'LOGICAL_OR',
    'LOGICAL_NOT',
    'LESS_THAN',
    'GREATER_THAN',
    'EQUALS',
    'VAR_CEL',
    'VAR_NAT',
    'VAR_VES',
    'VAR_SYM'
]

# определим регулярку для абстрактного идетификатора
ident = r'[a-zA-Zа-яА-Я_]\w*'

# Регулярные выражения для токенов
t_INTEGER = r'\d+'
t_FLOAT = r'\d+\.\d+'
t_BOOLEAN = r'true|false'
t_string_STR = r'(\\.|[^$"])+'
t_string_ignore = ''
t_IF = r'если'
t_ELSE = r'иначе'
t_WHILE = r'пока'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_SEMICOLON = r';'
t_COMMA = r','
t_ASSIGN = r'='
t_READ = r'>>'
t_WRITE = r'<<'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_MOD = r'%'
t_POWER = r'\^'
t_BITWISE_NOT = r'~'
t_LOGICAL_AND = r'\&'
t_LOGICAL_OR = r'\|'
t_LOGICAL_NOT = r'!'
t_LESS_THAN = r'<'
t_GREATER_THAN = r'>'
t_EQUALS = r'=='


def t_CHAR(t):
    r"""'[a-zA-Zа-яА-Я0-9\s☭★]'"""
    t.value = t.value[1]  # Извлекаем имя переменной
    return t


def t_ANY_STRING(t):  # нужен в обоих состояниях, потому что двойные кавычки матчатся и там и там.
    r"""\""""
    if t.lexer.current_state() == 'string':
        t.lexer.begin('INITIAL')  # переходим в начальное состояние
    else:
        t.lexer.begin('string')  # парсим строку
    # return t верну, если понадобятся кавычки


# ну и куда же мы без обработки ошибок
def t_string_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)


# Токены для переменных
def t_VAR_CEL(t):
    r"""цел [a-zA-Zа-яА-Я_]\w*"""
    t.type = 'VAR_CEL'
    t.value = t.value.split()[1]  # Извлекаем имя переменной
    return t


def t_VAR_NAT(t):
    r"""нат [a-zA-Zа-яА-Я_]\w*"""
    t.type = 'VAR_NAT'
    t.value = t.value.split()[1]  # Извлекаем имя переменной
    return t


def t_VAR_VES(t):
    r"""вещ [a-zA-Zа-яА-Я_]\w*"""
    t.type = 'VAR_VES'
    t.value = t.value.split()[1]  # Извлекаем имя переменной
    return t


def t_VAR_SYM(t):
    r"""симв [a-zA-Zа-яА-Я_]\w*"""
    t.type = 'VAR_SYM'
    t.value = t.value.split()[1]  # Извлекаем имя переменной
    return t


# Игнорируем пробелы и табуляцию
t_ignore = ' \r\t\f'


# Токен для идентификатора (переменной)
def t_IDENTIFIER(t):
    r"""[a-zA-Zа-яА-Я_][a-zA-Z0-9а-яА-Я_]*"""
    if t.value.lower() == 'цел':
        t.type = 'VAR_CEL'
    elif t.value.lower() == 'нат':
        t.type = 'VAR_NAT'
    elif t.value.lower() == 'вещ':
        t.type = 'VAR_VES'
    elif t.value.lower() == 'логик':
        t.type = 'VAR_LOG'
    elif t.value.lower() == 'симв':
        t.type = 'VAR_SYM'
    else:
        t.type = 'IDENTIFIER'
    return t


# Обработка символов новой строки
def t_newline(t):
    r"""\n+"""
    t.lexer.lineno += len(t.value)


# Функция для токена комментария
def t_comment(t):
    r"""(/\*(.|\n)*?\*/)|(//.*)"""
    pass


# Обработка ошибок
def t_error(t):
    print(f"Неправильный символ: '{t.value[0]}'", t.lineno)
    t.lexer.skip(1)


def tokenize(src: str):
    data = ''
    try:
        with open(src, mode='r') as file:
            data = file.read()
    except FileNotFoundError:
        print("File not found: {}".format(src))
        raise
    except Exception as e:
        print("An error occurred: {}".format(e))
        raise

    lexer = lex.lex(reflags=re.UNICODE | re.DOTALL)

    lexer.input(data)
    while True:
        tok = lexer.token()  # читаем следующий токен
        if not tok:
            break  # закончились печеньки
        print(tok)

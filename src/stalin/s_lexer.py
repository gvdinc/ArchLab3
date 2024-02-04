import ply.lex as lex
from ply.lex import TOKEN
import re

states = (
    ('string', 'exclusive'),
)

# Список токенов
tokens = [
    'IDENTIFIER',
    'INTEGER',
    'FLOAT',
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
    'INCR',
    'DECR',
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
    'VAR_VES',
    'VAR_SYM',
    'VAR_STR'
]

# определим регулярку для абстрактного идентификатора
ident = r'[a-zA-Zа-яА-Я_]\w*'

# Регулярные выражения для токенов
t_INTEGER = r'\d+'
t_FLOAT = r'\d+\.\d+'
t_string_STR = r'(\\.|[^$"])+'
t_string_ignore = ''
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
t_INCR = r'\+\+'
t_DECR = r'\-\-'
t_MOD = r'%'
t_POWER = r'\^'
t_BITWISE_NOT = r'~'
t_LOGICAL_AND = r'\&'
t_LOGICAL_OR = r'\|'
t_LOGICAL_NOT = r'!'
t_LESS_THAN = r'<'
t_GREATER_THAN = r'>'
t_EQUALS = r'=='
# Токены для переменных
t_VAR_CEL = r"""цел"""
t_VAR_VES = r"""вещ"""
t_VAR_SYM = r"""симв"""
t_VAR_STR = r"""текст"""


# Правила для ключевых слов
def t_IF(t):
    r"""если"""
    return t


def t_ELSE(t):
    r"""иначе"""
    return t


def t_WHILE(t):
    r"""пока"""
    return t


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


# Игнорируем пробелы и табуляцию
t_ignore = ' \r\t\f'


# Токен для идентификатора (переменной)
def t_IDENTIFIER(t):
    r"""[a-zA-Zа-яА-Я_][a-zA-Z0-9а-яА-Я_]*"""
    if t.value.lower() == 'цел':
        t.type = 'VAR_CEL'
    elif t.value.lower() == 'вещ':
        t.type = 'VAR_VES'
    elif t.value.lower() == 'симв':
        t.type = 'VAR_SYM'
    elif t.value.lower() == 'текст':
        t.type = 'VAR_STR'
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


lexer = lex.lex(reflags=re.UNICODE | re.DOTALL)


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
    lexer.input(data)
    while True:
        tok = lexer.token()  # читаем следующий токен
        if not tok:
            break  # закончились печеньки
        print(tok)


if __name__ == '__main__':
    tokenize("/home/prox/projects/ArchLab3/ArchLab3/src/examples/test.ussr")

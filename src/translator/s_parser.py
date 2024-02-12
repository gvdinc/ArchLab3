import ply.yacc as yacc
from s_lexer import tokens  # noqa: F401

# Грамматика
start = "program"

precedence = (
    ("nonassoc", "LESS_THAN", "GREATER_THAN", "EQUALS", "NOT_EQUALS"),  # Non associative operators
    ("left", "PLUS", "MINUS"),
    ("left", "TIMES", "DIVIDE"),
    ("left", "POWER", "MOD"),
    ("left", "SQRT")
)


def p_program(p):
    """program : statements"""
    p[0] = p[1]


def p_statements(p):
    """statements : statements statement
                  | statement"""
    if len(p) == 3:
        p[0] = p[1] + [p[2]]
    else:
        p[0] = [p[1]]


def p_statement(p):
    """statement : assignment
                 | if_statement
                 | while_statement
                 | read_statement
                 | write_statement
                 | expression SEMICOLON"""
    p[0] = p[1]


def p_assignment(p):
    """assignment : VAR_CEL IDENTIFIER ASSIGN expression SEMICOLON
                  | VAR_VES IDENTIFIER ASSIGN expression SEMICOLON
                  | VAR_SYM IDENTIFIER ASSIGN expression SEMICOLON
                  | VAR_STR IDENTIFIER ASSIGN expression SEMICOLON
                  | IDENTIFIER ASSIGN expression SEMICOLON"""
    if len(p) == 5:  # reassign
        p[0] = ("assignment", p[1], p[3])
    elif len(p) == 6:   # assign new variable
        p[0] = ("assignment", p[1], p[2], p[4])


def p_if_statement(p):
    """if_statement : IF LPAREN expression RPAREN LBRACE statements RBRACE
                    | IF LPAREN expression RPAREN LBRACE statements RBRACE ELSE LBRACE statements RBRACE"""
    if len(p) == 8:
        p[0] = ("if", p[3], p[6])
    else:
        p[0] = ("if-else", p[3], p[6], p[10])


def p_while_statement(p):
    """while_statement : WHILE LPAREN expression RPAREN LBRACE statements RBRACE"""
    p[0] = ("while", p[3], p[6])


def p_read_statement(p):
    """read_statement : READ IDENTIFIER SEMICOLON"""
    p[0] = ("read", p[2])


def p_write_statement(p):
    """write_statement : WRITE expression SEMICOLON"""
    p[0] = ("write", p[2])


def p_expression_binary_op(p):
    """expression : expression TIMES expression
                  | expression DIVIDE expression
                  | expression MOD expression
                  | expression POWER expression
                  | expression PLUS expression
                  | expression MINUS expression
                  | expression LESS_THAN expression
                  | expression GREATER_THAN expression
                  | expression EQUALS expression
                  | expression NOT_EQUALS expression
                  | expression LOGICAL_AND expression
                  | expression LOGICAL_OR expression
                  | expression LOGICAL_XOR expression"""
    p[0] = (p[2], p[1], p[3])


def p_expression_unary_op(p):
    """expression : SQRT expression
                  | LOGICAL_NOT expression
                  | BITWISE_NOT expression
                  | MINUS expression
                  | INCR expression
                  | DECR expression"""
    p[0] = (p[1], p[2])


def p_expression_group(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]


def p_expression_identifier(p):
    """expression : IDENTIFIER"""
    p[0] = ("identifier", p[1])


def p_expression_literal(p):
    """expression : INTEGER
                  | FLOAT
                  | CHAR
                  | STR"""
    p[0] = ("literal", p[1])


def p_error(p):
    print(f"Syntax error at line {p.lineno}: Unexpected token '{p.value}'")


# Строим парсер
parser = yacc.yacc()


def parse_sovcode(src):
    try:
        with open(src) as file:
            data = file.read()
            return parser.parse(data)
    except FileNotFoundError:
        print("File not found: {}".format(src))
        raise
    except Exception as e:
        print("An error occurred: {}".format(e))
        raise


# # Пример использования
if __name__ == "__main__":
    result = parse_sovcode("/home/prox/projects/ArchLab3/ArchLab3/src/examples/debug.ussr")
    for line in result:
        print(line)

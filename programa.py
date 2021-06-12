import ply.lex as lex
import ply.yacc as yacc

#################################### LEX ####################################

palReservado = {
    'float':'FLOAT',
    'int':'INT',
    'string':'STRING',
    'bool':'BOOL',
    'true':'TRUE',
    'false':'FALSE',
    'if':'IF',
    'elseif':'ELSEIF',
    'else':'ELSE',
    'while':'WHILE',
    'do':'DO',
    'for':'FOR',
    'and':'AND',
    'or':'OR',
    'log':'LOG'
}

tokens = [
    'VALORFLOAT',
    'VALORINT',
    'VALORSUMA',
    'VALORRESTA',
    'VALORMULT',
    'VALORDIV',
    'VALOREXP',
    'VALORIGUAL',
    'VALOREQUIV',
    'VALORNOEQUIV',
    'VALORMENOR',
    'VALORMEOI',
    'VALORMAYOR',
    'VALORMAOI',
    'VALORCADENA',
    'PARIZ',
    'PARDER',
    'LLAVIZ',
    'LLAVDER',
    'VALORID',
    'FINAL'
] + list(palReservado.values())

t_VALORSUMA = r'\+'
t_VALORRESTA = r'\-'
t_VALORMULT = r'\*'
t_VALORDIV = r'/'
t_VALOREXP = r'\^'
t_VALORIGUAL = r'\='
t_VALOREQUIV = r'\=='
t_VALORNOEQUIV = r'\!='
t_VALORMENOR = r'\<'
t_VALORMEOI = r'\<='
t_VALORMAYOR = r'\>'
t_VALORMAOI = r'>='
t_PARIZ = r'\('
t_PARDER = r'\)'
t_LLAVIZ = r'\{'
t_LLAVDER = r'\}'
t_FINAL = r';'

t_ignore  = ' \t'

def t_VALORFLOAT(t):
    r'\d.\d+'
    t.value = float(t.value)
    return t

def t_VALORINT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_VALORCADENA(t):
    r'\"[^\"\n]+\"'
    return t

def t_VALORID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = palReservado.get(t.value, 'VALORID')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

#################################### YACC ####################################

precedence = (
    ('left', 'AND', 'OR'),
    ('left', 'VALOREQUIV', 'VALORNOEQUIV'),
    ('nonassoc','VALORMENOR', 'VALORMEOI', 'VALORMAYOR', 'VALORMAOI'),
    ('left', 'VALORSUMA', 'VALORRESTA'),
    ('left', 'VALORMULT', 'VALORDIV'),
    ('left', 'VALOREXP'),
    ('right', 'UMINUS')
)

arbol = ()

def p_inicio(p):
    '''
    inicio : statement
    '''
    global arbol
    arbol = p[1]

def p_null(p):
    '''
    null :
    '''
    pass

def p_statement(p):
    '''
    statement : condicional statement
              | loop_while statement
              | loop_for statement
              | asignacion FINAL statement
              | log FINAL statement
              | null
    '''
    if len(p) > 2:
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0] = ()

def p_condicional(p):
    '''
    condicional : if_def elseif_def else_def
    '''
    p[0] = ('condicional', p[1], p[2], p[3])

def p_if(p):
    '''
    if_def : IF PARIZ expression PARDER LLAVIZ statement LLAVDER
    '''
    p[0] = ('if', p[3], p[6])

def p_elseif(p):
    '''
    elseif_def : ELSEIF PARIZ expression PARDER LLAVIZ statement LLAVDER elseif_def
               | null
    '''
    if len(p) > 2:
        p[0] = (('elseif', p[3], p[6]),) + p[8]
    else:
        p[0] = ()

def p_else(p):
    '''
    else_def : ELSE LLAVIZ statement LLAVDER
             | null
    '''
    if len(p) > 2:
        p[0] = ('else', p[3])

def p_while(p):
    '''
    loop_while : WHILE PARIZ expression PARDER LLAVIZ statement LLAVDER
               | DO LLAVIZ statement LLAVDER WHILE PARIZ expression PARDER FINAL
    '''
    if p[1] == "while":
        p[0] = ('while', p[3], p[6])
    else:
        p[0] = ('do-while', p[7], p[3])

def p_for(p):
    '''
    loop_for : FOR PARIZ declarar-asignar FINAL expression FINAL guardar PARDER LLAVIZ statement LLAVDER
    '''
    p[0] = ('for', p[3], p[5], p[7], p[10])

def p_tipo(p):
    '''
    tipo : INT
         | FLOAT
         | STRING
         | BOOL
    '''
    p[0] = p[1]

def p_log(p):
    '''
    log : LOG expression
    '''
    p[0] = ('log', p[2])

def p_asignacion(p):
    '''
    asignacion : declarar
               | declarar-asignar
               | guardar
    '''
    p[0] = p[1]

def p_declarar(p):
    '''
    declarar : tipo VALORID
    '''
    p[0] = ('declarar', p[1], p[2])

def p_guardar(p):
    '''
    guardar : VALORID VALORIGUAL expression
    '''
    p[0] = ('guardar', p[1], p[3])

def p_declararAsignar(p):
    '''
    declarar-asignar : tipo VALORID VALORIGUAL expression
    '''
    p[0] = ('declarar-guardar', p[2], p[4])

def p_expressionNumero(p):
    '''
    expression : VALORINT
         | VALORFLOAT
         | VALORCADENA
         | valorbool
    '''
    p[0] = p[1]

def p_expression_group(p):
    '''
    expression : PARIZ expression PARDER
    '''
    p[0] = p[2]

def p_expression_name(p):
    '''
    expression : VALORID
    '''
    p[0] = p[1]

def p_valorbool(p):
    '''
    valorbool : TRUE
            | FALSE
    '''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False

def p_expression_uminus(p):
    '''
    expression : VALORRESTA expression %prec UMINUS
    '''
    p[0] = -p[2]

# Error rule for syntax errors
def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

def p_expression(p):
    '''
    expression : expression VALORSUMA expression
               | expression VALORRESTA expression
               | expression VALORMULT expression
               | expression VALORDIV expression
               | expression VALOREXP expression
               | expression VALOREQUIV expression
               | expression VALORNOEQUIV expression
               | expression VALORMENOR expression
               | expression VALORMEOI expression
               | expression VALORMAYOR expression
               | expression VALORMAOI expression
               | expression AND expression
               | expression OR expression
    '''
    p[0] = ('operacion', p[1], p[2], p[3])

# Build the parser
parser = yacc.yacc()

file = open("file.txt", "r")
s = file.read()
yacc.parse(s)

print("++++++++++++++++ Arbol ++++++++++++++++\n")
print(arbol)
print("\n++++++++++++++++++++++++++++++++++++++\n")
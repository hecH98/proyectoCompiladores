import ply.lex as lex

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
    t.value = t.value.replace("\"","")
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
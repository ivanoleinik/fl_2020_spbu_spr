from collections import defaultdict
from ply import lex, yacc

tokens = (
    'TERM',
    'NONTERM',
    'EPS',
    'ARROW',
    'LINEBREAK',
    'OR'
)

t_TERM = r':[a-z]:'
t_NONTERM = r'![A-Z]+'
t_EPS = r':!eps:'
t_ARROW = r'>=>'
t_LINEBREAK = r'\n'
t_OR = r'\|'
t_ignore = ' \t'

terms, nonterms, rules, log = set(), set(), defaultdict(list), ""

def t_error(t):
    global log
    log = f'Illegal character {t.value[0]!r}'
    t.lexer.skip(1)

lex.lex()

def p_gr_rule(p):
    """gr : rule"""
    p[0] = [p[1]]

def p_gr_rules(p):
    """gr : rule gr"""
    p[0] = [p[1]] + p[2]

def p_rule(p):
    """rule : NONTERM ARROW seq LINEBREAK"""
    rules[p[1][1:]] += p[3]
    nonterms.add(p[1][1:])

def p_rules(p):
    """rule : NONTERM ARROW seq LINEBREAK rule"""
    rules[p[1][1:]] += p[3]
    nonterms.add(p[1][1:])

def p_seq(p):
    """seq : rhs"""
    p[0] = [p[1]]

def p_seqs(p):
    """seq : rhs OR seq"""
    p[0] = [p[1].copy()] + p[3]

def p_rhs_term(p):
    """rhs : rhs TERM"""
    p[0] = p[1].copy() + [p[2][1:-1]]
    terms.add(p[2][1:-1])

def p_rhs_nonterm(p):
    """rhs : rhs NONTERM"""
    p[0] = p[1].copy() + [p[2][1:]]
    nonterms.add(p[2][1:])

def p_term(p):
    """rhs : TERM"""
    p[0] = [p[1][1:-1]]
    terms.add(p[1][1:-1])

def p_nonterm(p):
    """rhs : NONTERM"""
    p[0] = [p[1][1:]]
    nonterms.add(p[1][1:])

def p_eps(p):
    """rhs : EPS"""
    p[0] = ['']

def p_error(p):
    global log
    log = 'Syntax error at {}'.format(p)

yacc.yacc()

def parse(grammar):
    global terms, nonterms, rules, log
    terms, nonterms, rules, log = set(), set(), defaultdict(list), ""
    yacc.parse(grammar)
    if log == "" and 'S' not in nonterms:
        log = "No starting non-terminal S"
    return terms, nonterms, rules, log
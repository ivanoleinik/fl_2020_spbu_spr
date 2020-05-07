from parser import parse

def test_illegal_character():
    str = "\n!S -> :a:"
    _, _, _, log = parse(str)
    assert log.startswith("Illegal character")

def test_syntax_error():
    str = "abracadabra"
    _, _, _, log = parse(str)
    assert log.startswith("Syntax error")

def test_no_starting_non_terminal_S():
    str = "!R >=> :a:\n"
    _, _, _, log = parse(str)
    assert log.startswith("No starting non-terminal S")

def test_eps():
    str = "!S >=> :!eps:\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == set()
    assert nonterms == {'S'}
    assert rules == {'S': [['']]}

def test_term():
    str = "!S >=> :a:\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == {'a'}
    assert nonterms == {'S'}
    assert rules == {'S': [['a']]}

def test_nonterm():
    str = "!S >=> !U\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == set()
    assert nonterms == {'S', 'U'}
    assert rules == {'S': [['U']]}

def test_rhs_term():
    str = "!S >=> :a: :b:\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == {'a', 'b'}
    assert nonterms == {'S'}
    assert rules == {'S': [['a', 'b']]}

def test_rhs_nonterm():
    str = "!S >=> !U !V\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == set()
    assert nonterms == {'S', 'U', 'V'}
    assert rules == {'S': [['U', 'V']]}

def test_rhs():
    str = "!S >=> :a: !S :b:\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == {'a', 'b'}
    assert nonterms == {'S'}
    assert rules == {'S': [['a', 'S', 'b']]}

def test_seq():
    str = "!S >=> :a: :a: !S | !S :b: :b: | :a: !S :b: | :a: :a: | :b: :b: | :a: :b:\n"
    terms, nonterms, rules, log = parse(str)
    assert log is ""
    assert terms == {'a', 'b'}
    assert nonterms == {'S'}
    assert rules == {'S':
                        [ ['a', 'a', 'S']
                        , ['S', 'b', 'b']
                        , ['a', 'S', 'b']
                        , ['a', 'a']
                        , ['b', 'b']
                        , ['a', 'b']
                        ]
                    }

def test_parse():
    str = """!S >=> !R !S | !R
             !R >=> :a: !S :b: | :c: !R :d: | :a: :b: | :c: :d: | :!eps:
          """
    terms, nonterms, rules, log = parse(str)
    assert log == ""
    assert terms == {'a', 'b', 'c', 'd'}
    assert nonterms == {'S', 'R'}
    assert rules == {'R':
                        [ ['a', 'S', 'b']
                        , ['c', 'R', 'd']
                        , ['a', 'b']
                        , ['c', 'd']
                        , ['']
                        ]
                    ,'S':
                        [ ['R', 'S']
                        , ['R']
                        ]
                    }

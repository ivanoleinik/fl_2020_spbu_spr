import sys

RED = "\033[1;31m"
GREEN = "\033[0;32m"
CYAN = "\033[1;36m"
RESET = "\033[0;0m"

def show(grammar):
    _, _, _, log = grammar
    if log != "":
        sys.stdout.write(RED)
        print("Parsing failed:", end="")
        sys.stdout.write(RESET)
        print(log)
    else:
        terms, nonterms, rules, _ = grammar

        sys.stdout.write(GREEN)
        print("Parsing succeeded!")

        sys.stdout.write(CYAN)
        print("Start non-terminal:", end=" ")
        sys.stdout.write(RESET)
        print("S")

        sys.stdout.write(CYAN)
        print("Non-terminals:", end=" ")
        sys.stdout.write(RESET)
        print(" ".join(nonterms))

        sys.stdout.write(CYAN)
        print("Terminals:", end=" ")
        sys.stdout.write(RESET)
        print(" ".join(terms))

        sys.stdout.write(CYAN)
        print("Rules:")
        sys.stdout.write(RESET)
        for nonterm, rhss in rules.items():
            for rhs in rhss:
                print(nonterm, '->', *rhs)

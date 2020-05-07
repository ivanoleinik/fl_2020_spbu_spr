from parser import parse
from show import show
import sys

if __name__ == "__main__":
    with open(sys.argv[1], "r") as fin:
        grammar = parse(fin.read())
    show(grammar)

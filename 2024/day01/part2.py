import collections
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
pairs = [l.split() for l in lines]
lefts = [int(p[0]) for p in pairs]
rights = collections.Counter(int(p[1]) for p in pairs)

total = sum(l * rights[l] for l in lefts)
print(total)

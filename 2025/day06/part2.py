import itertools
import math
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.rstrip() for line in f]


lines = get_lines()
h = len(lines)
w = max(map(len, lines))
lines = [line.ljust(w) for line in lines]

blanks = [-1] + [c for c in range(w) if all(lines[r][c] == ' ' for r in range(h - 1))] + [w]

total = 0

for start, end in itertools.pairwise(blanks):
    start += 1

    op = lines[-1][start]
    vals = [(''.join(lines[r][c] for r in range(h - 1))) for c in range(start, end)]

    if op == '+':
        total += sum(map(int, vals))
    else:
        total += math.prod(map(int, vals))

print(total)

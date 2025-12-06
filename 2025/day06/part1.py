import math
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
lines = [line.split() for line in lines]
h = len(lines)
w = len(lines[0])
lines = [[lines[r][c] for r in range(h - 1, -1, -1)] for c in range(w)]  # transpose

total = 0

for line in lines:
    op = line[0]
    if op == '+':
        total += sum(map(int, line[1:]))
    else:
        total += math.prod(map(int, line[1:]))

print(total)

import sys
from bisect import *


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
blank = lines.index('')

ranges = []

for line in lines[:blank]:
    a, b = line.split('-')
    a = int(a)
    b = int(b)
    ranges.append([a, b])

total = 0

for line in lines[blank + 1:]:
    n = int(line)
    for lo, hi in ranges:
        if lo <= n <= hi:
            total += 1
            break

print(total)

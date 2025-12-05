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

ranges.sort()

i = 1
while i < len(ranges):
    l, r = ranges[i - 1]
    a, b = ranges[i]
    if l <= a <= r:
        ranges[i - 1] = [l, max(r, b)]
        ranges.pop(i)
    else:
        i += 1

s = sum(r - l + 1 for l, r in ranges)
print(s)

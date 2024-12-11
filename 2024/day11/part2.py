import functools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


line = list(map(int, get_lines()[0].split()))


@functools.cache
def blink(s):
    if s == 0:
        return [1]

    if len(str(s)) % 2 == 0:
        st = str(s)
        return [int(st[:len(st) // 2]), int(st[len(st) // 2:])]

    return [s * 2024]


@functools.cache
def count(s, n):
    if n == 1:
        return len(blink(s))

    return sum(count(t, n - 1) for t in blink(s))


total = 0

for s in line:
    t = count(s, 75)
    print(t)
    total += t

print(total)

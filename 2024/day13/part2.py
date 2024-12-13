import fractions
import itertools
import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
lines.append('')

spend = 0

for la, lb, lp, _ in itertools.batched(lines, 4):
    ax, ay, bx, by, px, py = list(map(int, re.findall(r'\d+', la + lb + lp)))
    px += 10000000000000
    py += 10000000000000

    # ax * i + bx * j = px
    # ay * i + by * j = py
    # do the algebra on paper and you get this
    i = fractions.Fraction(bx * py - by * px, bx * ay - by * ax)
    if i.is_integer():
        j = (px - ax * i) / bx
        if j.is_integer():
            spend += 3 * int(i) + int(j)

print(spend)

import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

out = 0

for l in lines:
    vals = re.findall(r'\d+', l)
    test = int(vals[0])
    vals = list(map(int, vals[1:]))

    bank = set()
    bank.add(vals[0])

    for v in vals[1:]:
        bn = set()

        for x in bank:
            bn.add(x + v)
            bn.add(x * v)

        bank = bn

    if test in bank:
        out += test

print(out)

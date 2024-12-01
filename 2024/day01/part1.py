import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
pairs = [l.split() for l in lines]
lefts = [int(p[0]) for p in pairs]
rights = [int(p[1]) for p in pairs]

lefts.sort()
rights.sort()

total = sum(abs(l - r) for (l, r) in zip(lefts, rights))
print(total)

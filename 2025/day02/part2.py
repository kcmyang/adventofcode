import math
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


line = get_lines()[0]
parts = line.split(',')

total = 0

for part in parts:
    print('**', part)
    a, b = part.split('-')
    a = int(a)
    b = int(b)
    l1 = len(str(a))
    l2 = len(str(b))

    for l in range(l1, l2 + 1):
        targets = set()
        # ends of range with this length
        lo = max(a, 10**(l - 1))
        hi = min(b, 10**l - 1)
        for repetitions in range(2, l + 1):
            if l % repetitions != 0:
                continue
            # targets must be divisible by factor
            k = l // repetitions
            # print('*', l, repetitions, k)
            factor = 1
            for _ in range(repetitions - 1):
                factor = factor * 10**k + 1
            # arithmetic sum of targets in range
            qlo = math.ceil(lo / factor)
            qhi = math.floor(hi / factor)
            for i in range(qlo, qhi + 1):
                targets.add(factor * i)
        val = sum(targets)
        print(val)
        total += val

print(total)

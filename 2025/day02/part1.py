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
    a, b = part.split('-')
    a = int(a)
    b = int(b)
    l1 = len(str(a))
    l2 = len(str(b))
    lo = a

    for l in range(l1, l2 + 1):
        if l % 2 == 1:
            continue
        # ends of range with this length
        lo = max(a, 10**(l - 1))
        hi = min(b, 10**l - 1)
        # targets must be divisible by factor
        half = l // 2
        factor = 10**half + 1
        # arithmetic sum of targets in range
        qlo = math.ceil(lo / factor)
        qhi = math.floor(hi / factor)
        n = qhi - qlo + 1
        a1 = qlo * factor
        d = factor
        subtotal = (2 * a1 + (n - 1) * d) * n // 2
        print(subtotal)
        total += subtotal

print(total)

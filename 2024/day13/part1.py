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
    best = 500

    for i in range(100):
        x = ax * i
        y = ay * i
        dx, rx = divmod(px - x, bx)
        dy, ry = divmod(py - y, by)
        if rx == 0 and ry == 0 and dx == dy:
            best = min(best, 3 * i + dx)

    if best < 500:
        spend += int(best)

print(spend)

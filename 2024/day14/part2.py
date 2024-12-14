import itertools
import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
h = 103
w = 101

# hard-coded answer: t = 6620
for t in itertools.count(762, step=101):
    robots = [[' '] * w for _ in range(h)]

    for line in lines:
        px, py, vx, vy = list(map(int, re.findall(r'(-?\d+)', line)))
        x = (px + t * vx) % w
        y = (py + t * vy) % h
        robots[y][x] = 'O'

    for row in robots:
        print(''.join(row))

    print(f't={t}')
    input()

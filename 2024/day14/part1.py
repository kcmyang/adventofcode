import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
h = 7 if len(sys.argv) > 1 else 103
w = 11 if len(sys.argv) > 1 else 101

q1 = 0
q2 = 0
q3 = 0
q4 = 0

for line in lines:
    px, py, vx, vy = list(map(int, re.findall(r'(-?\d+)', line)))
    x = (px + 100 * vx) % w
    y = (py + 100 * vy) % h

    if x < w // 2 and y < h // 2:
        q1 += 1
    elif x < w // 2 and y > h // 2:
        q2 += 1
    elif x > w // 2 and y < h // 2:
        q3 += 1
    elif x > w // 2 and y > h // 2:
        q4 += 1

print(q1 * q2 * q3 * q4)

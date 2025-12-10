import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
points = [tuple(map(int, line.split(','))) for line in lines]
print(points)

a = max((abs(x2 - x1) + 1) * (abs(y2 - y1) + 1) for (x1, y1), (x2, y2) in itertools.product(points, repeat=2))
print(a)

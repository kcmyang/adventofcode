import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = [list(l) for l in get_lines()]
w = len(grid[0])
h = len(grid)

dirs = list(set(itertools.product([-1, 0, 1], [-1, 0, 1])) - set([(0, 0)]))

total = 0

while True:
    targets = []

    for r, row in enumerate(grid):
        for c, cell in enumerate(row):
            if cell != '@':
                continue
            adj = 0
            for dr, dc in dirs:
                rr = r + dr
                cc = c + dc
                if 0 <= rr < h and 0 <= cc < w:
                    if grid[rr][cc] == '@':
                        adj += 1
            if adj < 4:
                targets.append((r, c))

    if not targets:
        break

    for r, c in targets:
        grid[r][c] = '.'
        total += 1

print(total)

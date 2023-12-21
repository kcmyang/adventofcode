import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
m = len(grid)
n = len(grid[0])

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

start = (0, 0)

for r, c in itertools.product(range(m), range(n)):
    if grid[r][c] == 'S':
        start = (r, c)
        break

q = [start]
STEPS = 64

for _ in range(STEPS):
    q_next = set()

    for r, c in q:
        for dr, dc in dirs:
            rr = r + dr
            cc = c + dc
            if 0 <= rr < m and 0 <= cc < n and grid[rr][cc] != '#':
                q_next.add((rr, cc))

    q = list(q_next)

print(len(q))

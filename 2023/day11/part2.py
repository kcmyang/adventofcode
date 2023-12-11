import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


grid = get_lines()
m = len(grid)
n = len(grid[0])

rows = set()
cols = set()

for i in range(m):
    if grid[i].count('#') == 0:
        rows.add(i)

for j in range(n):
    if all(grid[i][j] == '.' for i in range(m)):
        cols.add(j)

# get points of all galaxies
points = list(filter(lambda p: grid[p[0]][p[1]] == '#', itertools.product(range(m), range(n))))
p = len(points)

expand = 10**6

total = 0

for i in range(p - 1):
    for j in range(i + 1, p):
        r1, c1 = points[i]
        r2, c2 = points[j]

        for r in range(min(r1, r2), max(r1, r2)):
            if r in rows:
                total += expand
            else:
                total += 1

        for c in range(min(c1, c2), max(c1, c2)):
            if c in cols:
                total += expand
            else:
                total += 1

print(total)

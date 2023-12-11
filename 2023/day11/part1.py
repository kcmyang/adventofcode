import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


grid = get_lines()
i = 0

# expand rows
while i < len(grid):
    if grid[i].count('#') == 0:
        grid.insert(i, '.' * len(grid[i]))
        i += 1

    i += 1

m = len(grid)

# expand columns
j = 0

while j < len(grid[0]):
    if all(grid[i][j] == '.' for i in range(m)):
        for i in range(m):
            grid[i] = grid[i][:j] + '.' + grid[i][j:]
        j += 1

    j += 1

n = len(grid[0])

# get points of all galaxies
points = list(filter(lambda p: grid[p[0]][p[1]] == '#', itertools.product(range(m), range(n))))
p = len(points)

total = 0

for i in range(p - 1):
    for j in range(i + 1, p):
        r1, c1 = points[i]
        r2, c2 = points[j]

        total += abs(r1 - r2) + abs(c1 - c2)

print(total)

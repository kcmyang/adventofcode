import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


grid = get_lines()
m = len(grid)
n = len(grid[0])

seen = [[False for _ in range(n)] for _ in range(m)]
sr, sc = 0, 0

for r, c in itertools.product(range(m), range(n)):
    if grid[r][c] == 'S':
        sr, sc = r, c
        break


north = (-1, 0)
south = (1, 0)
east = (0, 1)
west = (0, -1)

dirs = {
    '|': [north, south],
    '-': [east, west],
    'L': [north, east],
    'J': [north, west],
    '7': [south, west],
    'F': [south, east],
    '.': [],
}

dist = 1
frontier = []

# north
if sr > 0 and south in dirs[grid[sr - 1][sc]]:
    frontier.append((sr - 1, sc))
# south
if sr < m - 1 and north in dirs[grid[sr + 1][sc]]:
    frontier.append((sr + 1, sc))
# east
if sc < n - 1 and west in dirs[grid[sr][sc + 1]]:
    frontier.append((sr, sc + 1))
# west
if sc > 0 and east in dirs[grid[sr][sc - 1]]:
    frontier.append((sr, sc - 1))

seen[sr][sc] = True

for r, c in frontier:
    seen[r][c] = True

while True:
    f = []

    for r, c in frontier:
        for dr, dc in dirs[grid[r][c]]:
            rr = r + dr
            cc = c + dc

            if 0 <= rr < m and 0 <= cc < n and not seen[rr][cc]:
                seen[rr][cc] = True
                f.append((rr, cc))

    frontier = f

    if not frontier:
        break

    dist += 1

print(dist)

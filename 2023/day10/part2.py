import collections
import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


grid = get_lines()

# for ease of computation, extend the grid by one in all directions
for r in range(len(grid)):
    grid[r] = '.' + grid[r] + '.'

grid.append('.' * len(grid[0]))
grid.insert(0, '.' * len(grid[0]))

m = len(grid)
n = len(grid[0])
inf = m * n

# 0 -> unseen, 1 -> pipe, 2 -> outside
flag = [[0 for _ in range(n)] for _ in range(m)]

# mark entire border as outside
for c in range(n):
    flag[0][c] = 2
    flag[-1][c] = 2

for r in range(m):
    flag[r][0] = 2
    flag[r][-1] = 2

# find S
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

# figure out which pipe S really is
start_dirs = []

# north
if south in dirs[grid[sr - 1][sc]]:
    start_dirs.append(north)
# south
if north in dirs[grid[sr + 1][sc]]:
    start_dirs.append(south)
# east
if west in dirs[grid[sr][sc + 1]]:
    start_dirs.append(east)
# west
if east in dirs[grid[sr][sc - 1]]:
    start_dirs.append(west)

for k, v in dirs.items():
    if start_dirs == v:
        row = list(grid[sr])
        row[sc] = k
        grid[sr] = row
        break

# find the loop
frontier = [(sr, sc)]
flag[sr][sc] = 1

while frontier:
    f = []

    for r, c in frontier:
        for dr, dc in dirs[grid[r][c]]:
            rr = r + dr
            cc = c + dc

            if flag[rr][cc] == 0:
                flag[rr][cc] = 1
                f.append((rr, cc))

    frontier = f

# find all outside tiles: work along the spaces between indices rather than on indices
space_seen = [[False for _ in range(n + 1)] for _ in range(m + 1)]

for c in range(n + 1):
    space_seen[0][c] = True
    space_seen[-1][c] = True

for r in range(m + 1):
    space_seen[r][0] = True
    space_seen[r][-1] = True

def can_squeeze_north(r: int, c: int) -> bool:
    return not (east in dirs[grid[r - 1][c - 1]] and west in dirs[grid[r - 1][c]])

def can_squeeze_south(r: int, c: int) -> bool:
    return not (east in dirs[grid[r][c - 1]] and west in dirs[grid[r][c]])

def can_squeeze_east(r: int, c: int) -> bool:
    return not (north in dirs[grid[r][c]] and south in dirs[grid[r - 1][c]])

def can_squeeze_west(r: int, c: int) -> bool:
    return not (north in dirs[grid[r][c - 1]] and south in dirs[grid[r - 1][c - 1]])

# an element (r, c) in the queue is the "space" to the top-left of the tile grid[r][c]
# when we visit each element, also check on grid[r][c]
q = collections.deque()

# start at top-left of original input grid
q.append((1, 1))

if flag[1][1] == 0:
    flag[1][1] = 2

space_seen[1][1] = True

while q:
    r, c = q.popleft()

    # north
    if not space_seen[r - 1][c] and can_squeeze_north(r, c):
        space_seen[r - 1][c] = True

        if flag[r - 1][c] == 0:
            flag[r - 1][c] = 2

        q.append((r - 1, c))

    # south
    if not space_seen[r + 1][c] and can_squeeze_south(r, c):
        space_seen[r + 1][c] = True

        if flag[r + 1][c] == 0:
            flag[r + 1][c] = 2

        q.append((r + 1, c))

    # east
    if not space_seen[r][c + 1] and can_squeeze_east(r, c):
        space_seen[r][c + 1] = True

        if flag[r][c + 1] == 0:
            flag[r][c + 1] = 2

        q.append((r, c + 1))

    # west
    if not space_seen[r][c - 1] and can_squeeze_west(r, c):
        space_seen[r][c - 1] = True

        if flag[r][c - 1] == 0:
            flag[r][c - 1] = 2

        q.append((r, c - 1))

# Finally, there may be other loops outside of the main loop which we cannot "squeeze" into; fill these in
for r, c in itertools.product(range(1, m - 1), range(1, n - 1)):
    if flag[r][c] == 0 and any(flag[r + dr][c + dc] == 2 for dr, dc in [north, south, east, west]):
        flag[r][c] = 2

inside = len(list(filter(lambda p: flag[p[0]][p[1]] == 0, itertools.product(range(m), range(n)))))
print(inside)

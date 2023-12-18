import collections
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
dirs = {'U': (-1, 0), 'R': (0, 1), 'D': (1, 0), 'L': (0, -1)}

holes = set()

r, c = 0, 0

holes.add((r, c))

for line in lines:
    [d, t, _] = line.split(' ')
    dr, dc = dirs[d]
    t = int(t)

    for _ in range(t):
        r, c = r + dr, c + dc
        holes.add((r, c))

cmin = min(c for _, c in holes)
cmax = max(c for _, c in holes)
rmin = min(r for r, _ in holes)
rmax = max(r for r, _ in holes)

grid = [[1 for _ in range(cmin - 1, cmax + 2)] for _ in range(rmin - 1, rmax + 2)]

q = collections.deque()
q.append((rmin - 1, cmin - 1))

while q:
    r, c = q.popleft()
    if not (rmin - 1 <= r <= rmax + 1 and cmin - 1 <= c <= cmax + 1) or (r, c) in holes or grid[r - rmin + 1][c - cmin + 1] == 0:
        continue
    grid[r - rmin + 1][c - cmin + 1] = 0

    for dr, dc in dirs.values():
        q.append((r + dr, c + dc))

print(sum(sum(row) for row in grid))

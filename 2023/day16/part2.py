import collections
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


grid = get_lines()
m = len(grid)
n = len(grid[0])

up = (-1, 0)
right = (0, 1)
down = (1, 0)
left = (0, -1)
dirs = [up, right, down, left]

def config(pos, dirn):
    energy = [[False for _ in range(n)] for _ in range(m)]
    cache = set()
    q = collections.deque()
    q.append((pos, dirn))

    while q:
        (r, c), d = q.popleft()
        if not (0 <= r < m and 0 <= c < n) or ((r, c), d) in cache:
            continue
        cell = grid[r][c]
        energy[r][c] = True
        cache.add(((r, c), d))

        if cell == '/':
            d = ((d - 1) % 4) if d % 2 == 1 else ((d + 1) % 4)
            dr, dc = dirs[d]
            q.append(((r + dr, c + dc), d))
        elif cell == '\\':
            d = ((d - 1) % 4) if d % 2 == 0 else ((d + 1) % 4)
            dr, dc = dirs[d]
            q.append(((r + dr, c + dc), d))
        elif cell == '|' and d % 2 == 1:
            for d in [0, 2]:
                dr, dc = dirs[d]
                q.append(((r + dr, c + dc), d))
        elif cell == '-' and d % 2 == 0:
            for d in [1, 3]:
                dr, dc = dirs[d]
                q.append(((r + dr, c + dc), d))
        else:
            dr, dc = dirs[d]
            q.append(((r + dr, c + dc), d))

    total = sum(row.count(True) for row in energy)
    return total

best = 0

for c in range(n):
    # down
    best = max(best, config((0, c), 2))
    # up
    best = max(best, config((m - 1, c), 0))

for r in range(m):
    # right
    best = max(best, config((r, 0), 1))
    # left
    best = max(best, config((r, n - 1), 3))

print(best)

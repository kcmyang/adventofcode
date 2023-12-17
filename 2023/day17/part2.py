import collections
import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
grid = [list(map(int, line)) for line in lines]
m = len(grid)
n = len(grid[0])

# rdlu
dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

min_steps = 4
max_steps = 10

# cost[i][j][d][t] = minimum heat loss required to visit cell (i, j), at the t-th consecutive step in direction d
cost = [[[[9 * m * n for i in range(max_steps + 1)] for _ in range(len(dirs))] for _ in range(n)] for _ in range(m)]
cost[0][0][0][0] = 0

q = collections.deque()
q.append((0, 0, 0, 0))

while q:
    r, c, d, t = q.popleft()

    # d, t
    deltas = []

    if t < max_steps:
        deltas.append((0, 1))

    if t >= min_steps:
        deltas.append((-1, -t + 1))
        deltas.append((1, -t + 1))

    for x1, x2 in deltas:
        dd = (d + x1) % 4
        dr, dc = dirs[dd]
        rr, cc = r + dr, c + dc

        if not (0 <= rr < m and 0 <= cc < n):
            continue

        tt = t + x2

        if cost[r][c][d][t] + grid[rr][cc] < cost[rr][cc][dd][tt]:
            cost[rr][cc][dd][tt] = cost[r][c][d][t] + grid[rr][cc]
            q.append((rr, cc, dd, tt))

best = min(cost[-1][-1][d][t] for d, t in itertools.product(range(len(dirs)), range(min_steps, max_steps + 1)))
print(best)

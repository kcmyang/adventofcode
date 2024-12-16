import heapq
import collections
import math
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

sr, sc = 0, 0

for r in range(h):
    for c in range(w):
        if g[r][c] == 'S':
            sr, sc = r, c

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

dist = collections.defaultdict(lambda: math.inf)
dist[(sr, sc, 0)] = 0
# (dist, r, c, dir)
pq = [(0, sr, sc, 0)]

# dijkstra for shortest dist
while pq:
    t0, r, c, d = heapq.heappop(pq)

    if g[r][c] == 'E':
        # print(t0)
        break

    # forward
    rr, cc = r + dirs[d][0], c + dirs[d][1]
    if 0 <= rr < h and 0 <= cc < w and g[rr][cc] != '#' and t0 + 1 < dist[(rr, cc, d)]:
        dist[(rr, cc, d)] = t0 + 1
        heapq.heappush(pq, (t0 + 1, rr, cc, d))

    # turn left/right
    for i in (-1, 1):
        d2 = (d + i) % 4
        if t0 + 1000 < dist[(r, c, d2)]:
            dist[(r, c, d2)] = t0 + 1000
            heapq.heappush(pq, (t0 + 1000, r, c, d2))


# dfs to find all shortest paths
def dfs(r, c, d, t, path, points):
    if g[r][c] == 'E':
        points |= set(path)
        return

    # forward
    rr, cc = r + dirs[d][0], c + dirs[d][1]
    if 0 <= rr < h and 0 <= cc < w and g[rr][cc] != '#' and t + 1 == dist[(rr, cc, d)]:
        path.append((rr, cc))
        dfs(rr, cc, d, t + 1, path, points)
        path.pop()

    # turn left/right
    for i in (-1, 1):
        d2 = (d + i) % 4
        if t + 1000 == dist[(r, c, d2)]:
            dfs(r, c, d2, t + 1000, path, points)


points = set()
dfs(sr, sc, 0, 0, [(sr, sc)], points)
print(len(points))

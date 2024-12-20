import heapq
import collections
import math
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [list(line.strip()) for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
r0, c0 = 0, 0
cheats = set()

for r in range(h):
    for c in range(w):
        if g[r][c] == 'S':
            r0, c0 = r, c
        for dr, dc in dirs:
            r1, c1 = r + dr, c + dc
            r2, c2 = r1 + dr, c1 + dc
            if 0 <= r2 < h and 0 <= c2 < w and g[r1][c1] == '#' and g[r2][c2] != '#':
                cheats.add((r, c, r2, c2))


def search(cheat=(h, h, h, h)) -> int:
    dist = collections.defaultdict(lambda: math.inf)
    dist[(r0, c0)] = 0
    pq = [(0, r0, c0)]
    r1, c1, r2, c2 = cheat

    while pq:
        d0, r, c = heapq.heappop(pq)

        if g[r][c] == 'E':
            return d0

        if r == r1 and c == c1:
            if d0 + 2 < dist[(r2, c2)]:
                dist[(r2, c2)] = d0 + 2
                heapq.heappush(pq, (d0 + 2, r2, c2))

        for dr, dc in dirs:
            rr, cc = r + dr, c + dc
            if 0 <= rr < h and 0 <= cc < w and g[rr][cc] != '#':
                if d0 + 1 < dist[(rr, cc)]:
                    dist[(rr, cc)] = d0 + 1
                    heapq.heappush(pq, (d0 + 1, rr, cc))


base = search()
print(base)
count = 0

# beware, this runs super slowly (>90s on my machine, lol)
# see part 2 for (more) efficient algorithm that only searches the full graph twice
for cheat in cheats:
    new = search(cheat)

    if base - new >= 100:
        count += 1

print(count)

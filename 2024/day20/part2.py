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
rs, cs = 0, 0
re, ce = 0, 0

for r in range(h):
    for c in range(w):
        if g[r][c] == 'S':
            rs, cs = r, c
        elif g[r][c] == 'E':
            re, ce = r, c


def dist_from(r0, c0, target) -> dict:
    dist = collections.defaultdict(lambda: math.inf)
    dist[(r0, c0)] = 0
    pq = [(0, r0, c0)]

    while pq:
        d0, r, c = heapq.heappop(pq)

        if g[r][c] == target:
            continue

        for dr, dc in dirs:
            rr, cc = r + dr, c + dc
            if 0 <= rr < h and 0 <= cc < w and g[rr][cc] != '#':
                if d0 + 1 < dist[(rr, cc)]:
                    dist[(rr, cc)] = d0 + 1
                    heapq.heappush(pq, (d0 + 1, rr, cc))

    return dist


dist_s = dist_from(rs, cs, 'E')
dist_e = dist_from(re, ce, 'S')

base = dist_s[(re, ce)]
assert base == dist_e[(rs, cs)]
print(base)
count = 0

rad = 20
# group = collections.Counter()

for r in range(h):
    for c in range(w):
        if g[r][c] != '#':
            for rr in range(r - rad, r + rad + 1):
                for cc in range(c - rad, c + rad + 1):
                    d = abs(r - rr) + abs(c - cc)
                    if d <= rad and 0 <= rr < h and 0 <= cc < w and g[rr][cc] != '#':
                        new = dist_s[(r, c)] + dist_e[(rr, cc)] + d

                        if base - new >= 100:
                            # group[base - new] += 1
                            count += 1

# print(sorted(group.items(), key=lambda x: x[0]))
print(count)

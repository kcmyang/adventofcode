import collections
import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def search(r0, c0, seen):
    t = g[r0][c0]
    q = collections.deque()
    q.append((r0, c0))
    seen.add((r0, c0))
    area = 0
    # (x, dir) -> set of y vals
    xmap = collections.defaultdict(set)
    # (y, dir) -> set of x vals
    ymap = collections.defaultdict(set)

    while q:
        r, c = q.popleft()
        area += 1
        for d, (dr, dc) in enumerate(dirs):
            add = True
            rr, cc = r + dr, c + dc
            if 0 <= rr < w and 0 <= cc < h:
                if g[rr][cc] == t:
                    add = False
                    if (rr, cc) not in seen:
                        seen.add((rr, cc))
                        q.append((rr, cc))
            if add:
                if dr == 0:
                    ymap[(c, d)].add(r)
                else:
                    xmap[(r, d)].add(c)

    # line sweep-ish algorithm: count sides left/right of each x coord or up/down of each y coord
    sides = 0

    for vals in itertools.chain(xmap.values(), ymap.values()):
        vals = sorted(vals)
        prev = vals[0]
        sides += 1
        for v in vals[1:]:
            if v != prev + 1:
                sides += 1
            prev = v

    return area, sides


seen = set()
price = 0

for r0 in range(h):
    for c0 in range(w):
        if (r0, c0) not in seen:
            area, sides = search(r0, c0, seen)
            print(area, sides, g[r0][c0])
            price += area * sides

print(price)

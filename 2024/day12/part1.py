import collections
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
    perim = 0

    while q:
        r, c = q.popleft()
        area += 1
        p = 4
        for dr, dc in dirs:
            rr, cc = r + dr, c + dc
            if 0 <= rr < w and 0 <= cc < h:
                if g[rr][cc] == t:
                    p -= 1
                    if (rr, cc) not in seen:
                        seen.add((rr, cc))
                        q.append((rr, cc))
        perim += p

    return area, perim


seen = set()
price = 0

for r0 in range(h):
    for c0 in range(w):
        if (r0, c0) not in seen:
            area, perim = search(r0, c0, seen)
            print(area, perim, g[r0][c0])
            price += area * perim

print(price)

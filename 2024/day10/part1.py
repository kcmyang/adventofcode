import collections
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [list(map(int, line.strip())) for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]


def search(r, c) -> int:
    seen = set()
    seen.add((r, c))
    ends = 0

    q = collections.deque()
    q.append((r, c))

    while q:
        x, y = q.popleft()

        for dx, dy in dirs:
            xx, yy = x + dx, y + dy

            if 0 <= xx < h and 0 <= yy < w and (xx, yy) not in seen and g[xx][yy] - g[x][y] == 1:
                seen.add((xx, yy))

                if g[xx][yy] == 9:
                    ends += 1
                else:
                    q.append((xx, yy))

    return ends


count = 0

for r in range(h):
    for c in range(w):
        if g[r][c] == 0:
            count += search(r, c)

print(count)

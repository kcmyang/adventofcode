import collections
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

freq = collections.defaultdict(list)

for r, row in enumerate(g):
    for c, cell in enumerate(row):
        if cell != '.':
            freq[cell].append((r, c))

out = set()

for f, p in freq.items():
    for i, (r1, c1) in enumerate(p):
        for r2, c2 in p[i + 1:]:
            dr = r2 - r1
            dc = c2 - c1

            x, y = r1 - dr, c1 - dc
            if 0 <= x < h and 0 <= y < w:
                out.add((x, y))

            x, y = r2 + dr, c2 + dc
            if 0 <= x < h and 0 <= y < w:
                out.add((x, y))

print(len(out))

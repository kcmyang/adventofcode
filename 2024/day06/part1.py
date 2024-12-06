import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [list(line.strip()) for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

seen = set()
pos = 0, 0
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]  # NESW
d = 0

for r, row in enumerate(g):
    for c, cell in enumerate(row):
        if cell == '^':
            pos = (r, c)
            seen.add(pos)
            g[r][c] = '.'
            break

r, c = pos
while True:
    rr = r + dirs[d][0]
    cc = c + dirs[d][1]

    if not (0 <= rr < h and 0 <= cc < w):
        break

    if g[rr][cc] == '#':
        d = (d + 1) % 4
        rr = r + dirs[d][0]
        cc = c + dirs[d][1]

    if not (0 <= rr < h and 0 <= cc < w):
        break

    seen.add((rr, cc))
    r, c = rr, cc

print(len(seen))

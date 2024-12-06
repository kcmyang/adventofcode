import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [list(line.strip()) for line in f]


g = get_lines()
h = len(g)
w = len(g[0])

pos = 0, 0
dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]  # NESW

for r, row in enumerate(g):
    for c, cell in enumerate(row):
        if cell == '^':
            pos = (r, c)
            g[r][c] = '.'
            break


def test_loop():
    r, c = pos
    d = 0
    seen = set()  # (r, c, dir)

    while True:
        if (r, c, d) in seen:
            return True

        seen.add((r, c, d))

        rr = r + dirs[d][0]
        cc = c + dirs[d][1]

        if not (0 <= rr < h and 0 <= cc < w):
            break

        # if next space is a wall, rotate in place
        if g[rr][cc] == '#':
            d = (d + 1) % 4
            continue

        r, c = rr, cc

    return False


spots = set()

# the "smart" way of blocking each next block while traversing the graph was bugged when I tried,
# so just brute force lol (130x130 is small enough)
for x, row in enumerate(g):
    for y, cell in enumerate(row):
        if cell == '.':
            g[x][y] = '#'
            if test_loop():
                spots.add((x, y))
            g[x][y] = '.'

print(len(spots))

import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
i = lines.index('')

g = [list(s) for s in lines[:i]]
h = len(g)
w = len(g[0])

r0, c0 = 0, 0
for r in range(h):
    for c in range(w):
        if g[r][c] == '@':
            r0, c0 = r, c
            break


def can_move(r, c, dr, dc):
    while g[r][c] != '#':
        if g[r][c] == '.':
            return True
        r += dr
        c += dc
    return False


moves = ''.join(lines[i:])
dirmap = {'<': (0, -1), '>': (0, 1), 'v': (1, 0), '^': (-1, 0)}

r, c = r0, c0

for m in moves:
    dr, dc = dirmap[m]

    if can_move(r, c, dr, dc):
        swap = g[r][c]
        g[r][c] = '.'
        rr, cc = r + dr, c + dc
        while g[rr][cc] != '#' and swap != '.':
            swap, g[rr][cc] = g[rr][cc], swap
            rr += dr
            cc += dc
        r += dr
        c += dc

for row in g:
    print(''.join(row))

total = 0

for r in range(h):
    for c in range(w):
        if g[r][c] == 'O':
            total += 100 * r + c

print(total)

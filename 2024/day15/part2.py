import copy
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
i = lines.index('')

gmap = {'#': '##', '.': '..', 'O': '[]', '@': '@.'}
g = [list(''.join(gmap[ch] for ch in s)) for s in lines[:i]]
h = len(g)
w = len(g[0])

r0, c0 = 0, 0
for r in range(h):
    for c in range(w):
        if g[r][c] == '@':
            r0, c0 = r, c
            break


class NoMove(Exception):
    pass


def can_move(g, r, c, dc):
    '''
    Checks if a horizontal move is possible.
    '''
    while g[r][c] != '#':
        if g[r][c] == '.':
            return True
        c += dc
    return False


def do_move(g, r, c, dr, dc) -> list[list[str]]:
    '''
    Does a move and returns the new grid, or throws NoMove if the move is impossible.

    For horizontal moves, we do it like part 1.
    For vertical moves, we need to try to move each tile recursively.
    If there is no move, throw an exception so we can quickly break the stack.
    '''
    # Horizontal moves
    if dr == 0:
        if not can_move(g, r, c, dc):
            raise NoMove()

        swap = g[r][c]
        g[r][c] = '.'
        rr, cc = r + dr, c + dc
        while g[rr][cc] != '#' and swap != '.':
            swap, g[rr][cc] = g[rr][cc], swap
            rr += dr
            cc += dc

        return g

    # Vertical moves
    if g[r + dr][c] == '#':
        raise NoMove()

    if g[r + dr][c] == '.':
        g = copy.deepcopy(g)
        g[r + dr][c], g[r][c] = g[r][c], g[r + dr][c]  # do a single swap
        return g

    # Here we have to move a box, so move both halves.
    c2 = c + 1 if g[r + dr][c] == '[' else c - 1
    g = do_move(g, r + dr, c, dr, dc)
    g = do_move(g, r + dr, c2, dr, dc)
    g = do_move(g, r, c, dr, dc)
    return g


moves = ''.join(lines[i:])
dirmap = {'<': (0, -1), '>': (0, 1), 'v': (1, 0), '^': (-1, 0)}

r, c = r0, c0

for m in moves:
    dr, dc = dirmap[m]

    try:
        g = do_move(g, r, c, dr, dc)
        r += dr
        c += dc
    except NoMove:
        pass

for row in g:
    print(''.join(row))

total = 0

for r in range(h):
    for c in range(w):
        if g[r][c] == '[':
            total += 100 * r + c

print(total)

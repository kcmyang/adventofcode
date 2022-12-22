import re
import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip('\n') for line in list(f)]


lines = get_lines()

SIDE_LENGTH = {'sample.txt': 4, 'input.txt': 50}[sys.argv[1]]
END = SIDE_LENGTH - 1

# Some good old hard-coding happens in this file.
# There are only 11 nets of a cube (up to rotation and reflection), so a more general solution
# might have a big lookup table of all possible nets and their neighbour maps.

# right, down, left, up
dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]

# coordinates (y, x) of each face on a square grid with side length SIDE_LENGTH
coords = {
    # ..0.
    # 123.
    # ..45
    'sample.txt': [(0, 2), (1, 0), (1, 1), (1, 2), (2, 2), (2, 3)],
    # .01
    # .2.
    # 34.
    # 5..
    'input.txt': [(0, 1), (0, 2), (1, 1), (2, 0), (2, 1), (3, 0)],
}[sys.argv[1]]

# neighbours[i][f] = (neighbour of face i along facing f, new facing after moving to the neighbour)
# all 24 edges of the cube should be represented in the array
neighbours = {
    'sample.txt': [[(5, 2), (3, 1), (2, 1), (1, 1)],
                   [(2, 0), (4, 3), (5, 3), (0, 1)],
                   [(3, 0), (4, 0), (1, 2), (0, 0)],
                   [(5, 1), (4, 1), (2, 2), (0, 3)],
                   [(5, 0), (1, 3), (2, 3), (3, 3)],
                   [(0, 2), (1, 0), (4, 2), (3, 2)]],
    'input.txt': [[(1, 0), (2, 1), (3, 0), (5, 0)],
                  [(4, 2), (2, 2), (0, 2), (5, 3)],
                  [(1, 3), (4, 1), (3, 1), (0, 3)],
                  [(4, 0), (5, 1), (0, 0), (2, 0)],
                  [(1, 2), (5, 2), (3, 2), (2, 3)],
                  [(4, 3), (1, 1), (0, 1), (3, 3)]],
}[sys.argv[1]]

assert len(set(e for arr in neighbours for e in arr)) == 24

board = [list(s) for s in lines[:-2]]

faces = [[
    board[yy][(x * SIDE_LENGTH):((x + 1) * SIDE_LENGTH)]
    for yy in range(y * SIDE_LENGTH, (y + 1) * SIDE_LENGTH)
] for y, x in coords]

print(faces)
print(coords)
print(neighbours)

# always one more move than rotation
moves = [int(m) for m in re.compile(r'\d+').findall(lines[-1])]
rotations = re.compile(r'[RL]').findall(lines[-1])
commands = [x for pair in zip(moves, rotations) for x in pair] + [moves[-1]]

# coords
face = 0
x, y = 0, 0
facing = 0


def change_face(new_facing: int):
    x_new, y_new = x, y

    if new_facing == 0:
        x_new = 0
        if facing == 1:
            y_new = END - x
        elif facing == 2:
            y_new = END - y
        elif facing == 3:
            y_new = x
    elif new_facing == 1:
        y_new = 0
        if facing == 0:
            x_new = END - y
        elif facing == 2:
            x_new = y
        elif facing == 3:
            x_new = END - x
    elif new_facing == 2:
        x_new = END
        if facing == 0:
            y_new = END - y
        elif facing == 1:
            y_new = x
        elif facing == 3:
            y_new = END - x
    elif new_facing == 3:
        y_new = END
        if facing == 0:
            x_new = y
        elif facing == 1:
            x_new = END - x
        elif facing == 2:
            x_new = END - y

    return x_new, y_new


def debug_update():
    fy, fx = coords[face]
    board[fy * SIDE_LENGTH + y][fx * SIDE_LENGTH + x] = '>v<^'[facing]
    print(f'face = {face}, y = {y}, x = {x}, facing = {facing}')


def print_board():
    for row in board:
        print(''.join(row))


def move(dist: int):
    global face
    global x
    global y
    global facing

    for _ in range(dist):
        dx, dy = dirs[facing]
        x_new, y_new = x + dx, y + dy

        if 0 <= x_new < SIDE_LENGTH and 0 <= y_new < SIDE_LENGTH:
            if faces[face][y_new][x_new] == '#':
                break

            x, y = x_new, y_new
            # debug_update()
            continue

        new_face, new_facing = neighbours[face][facing]
        x_new, y_new = change_face(new_facing)

        if faces[new_face][y_new][x_new] == '#':
            break

        face = new_face
        x, y = x_new, y_new
        facing = new_facing
        # debug_update()


def rotate(direction: str):
    global facing
    facing = (facing + (1 if direction == 'R' else -1)) % 4


# print_board()
# debug_update()

# do the stuff
for i in range(len(rotations)):
    move(moves[i])
    rotate(rotations[i])
    # debug_update()

move(moves[-1])

# output
print_board()
fy, fx = coords[face]
row = fy * SIDE_LENGTH + y
col = fx * SIDE_LENGTH + x
print(row, col, facing)

password = 1000 * (row + 1) + 4 * (col + 1) + facing
print(password)

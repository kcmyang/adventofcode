import re
import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip('\n') for line in list(f)]


lines = get_lines()

board = lines[:-2]
width = max(map(len, board))
height = len(board)
# fill out the empty rows
for y in range(height):
    w = len(board[y])
    board[y] += ' ' * (width - w)


def get_x_bounds(row: int):
    i = 0
    while board[row][i] == ' ':
        i += 1
    low = i

    i = width - 1
    while board[row][i] == ' ':
        i -= 1
    high = i

    return low, high


def get_y_bounds(col: int):
    i = 0
    while board[i][col] == ' ':
        i += 1
    low = i

    i = height - 1
    while board[i][col] == ' ':
        i -= 1
    high = i

    return low, high


x_bounds = [get_x_bounds(row) for row in range(height)]
y_bounds = [get_y_bounds(col) for col in range(width)]

# always one more move than rotation
moves = [int(m) for m in re.compile(r'\d+').findall(lines[-1])]
rotations = re.compile(r'[RL]').findall(lines[-1])
commands = [x for pair in zip(moves, rotations) for x in pair] + [moves[-1]]

# right, down, left, up
dirs = [(1, 0), (0, 1), (-1, 0), (0, -1)]

# coords
x, y = x_bounds[0][0], 0
facing = 0
i = 0


def move(dist: int):
    global x
    global y
    dx, dy = dirs[facing]

    for _ in range(dist):
        x_new = x + dx
        y_new = y + dy

        if x_new < x_bounds[y][0]:
            x_new = x_bounds[y][1]
        elif x_new > x_bounds[y][1]:
            x_new = x_bounds[y][0]

        if y_new < y_bounds[x][0]:
            y_new = y_bounds[x][1]
        elif y_new > y_bounds[x][1]:
            y_new = y_bounds[x][0]

        if board[y_new][x_new] == '#':
            break

        x = x_new
        y = y_new


def rotate(direction: str):
    global facing
    facing = (facing + (1 if direction == 'R' else -1)) % 4


# do the stuff
for i in range(len(rotations)):
    move(moves[i])
    rotate(rotations[i])

move(moves[-1])

# output
password = 1000 * (y + 1) + 4 * (x + 1) + facing
print(password)

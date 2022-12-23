import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

dirs = 'NSWE'

vecs = {
    'NW': (-1, -1),
    'N': (-1, 0),
    'NE': (-1, 1),
    'E': (0, 1),
    'SE': (1, 1),
    'S': (1, 0),
    'SW': (1, -1),
    'W': (0, -1),
}

# (row, col)
elves = set()
# (row, col) -> list of elf coords
proposals = dict()

for row in range(len(lines)):
    for col in range(len(lines[row])):
        if lines[row][col] == '#':
            elves.add((row, col))

NUM_ROUNDS = 10


def can_stay(row: int, col: int) -> bool:
    return all((row + dr, col + dc) not in elves for dr, dc in vecs.values())


def can_move(row: int, col: int, d: str) -> bool:
    dirs_to_try = []

    if d == 'N':
        dirs_to_try = ['N', 'NE', 'NW']
    elif d == 'S':
        dirs_to_try = ['S', 'SE', 'SW']
    elif d == 'W':
        dirs_to_try = ['W', 'NW', 'SW']
    elif d == 'E':
        dirs_to_try = ['E', 'NE', 'SE']

    return all(
        (row + vecs[k][0], col + vecs[k][1]) not in elves for k in dirs_to_try)


for i in range(NUM_ROUNDS):
    # make proposals
    for row, col in elves:
        if can_stay(row, col):
            continue

        for j in range(4):
            d = dirs[(i + j) % 4]
            if can_move(row, col, d):
                dr, dc = vecs[d]
                destination = (row + dr, col + dc)
                proposals.setdefault(destination, []).append((row, col))
                break

    # process all proposals
    for destination, sources in proposals.items():
        if len(sources) == 1:
            source = sources[0]
            elves.discard(source)
            elves.add(destination)

    proposals = dict()

# output
row_min = min(map(lambda elf: elf[0], elves))
row_max = max(map(lambda elf: elf[0], elves))
col_min = min(map(lambda elf: elf[1], elves))
col_max = max(map(lambda elf: elf[1], elves))

empty_spaces = (row_max - row_min + 1) * (col_max - col_min + 1) - len(elves)
print(empty_spaces)

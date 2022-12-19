import sys
from typing import List
"""
Each row of the chamber is 7 units wide.
We represent each row as a bitstring, so each row is in the range [0, 2^7 - 1].
A 0 bit is air and a 1 bit is rock.
Moving left in a row is a left bit shift, moving right is a right bit shift.

The chamber is represented by a list of rows, where earlier indices are lower down in the chamber.
"""

# Each rock is a list of rows, bottom-up.
# Rocks appear two units away from the left wall.
# yapf: disable
rocks = [
    # - shape
    [0b0011110],
    # + shape
    [0b0001000,
     0b0011100,
     0b0001000],
    # L shape
    [0b0011100,
     0b0000100,
     0b0000100],
    # | shape
    [0b0010000,
     0b0010000,
     0b0010000,
     0b0010000],
    # square shape
    [0b0011000,
     0b0011000]
]
# yapf: enable
NUM_ROCKS = len(rocks)
BUFFER_HEIGHT = 3

jet = list(open(sys.argv[1]))[0].strip()
JET_LENGTH = len(jet)

# initialize the chamber with a full row for ease of computation
chamber = [0b1111111]
chamber_width = 7
# index of row of highest piece of rock
chamber_height = 0
# vertical space between the chamber height and the highest piece of rock in each column
vspace = [0 for _ in range(chamber_width)]

# memo table for cycle detection:
#   (index in jet, index in rocks, vspace as tuple) -> loop index
# [actually, the cycle should have period divisible by len(rocks) for the cycle to make any sense,
# so the rock index is not necessary, but this feels more correct]
memo = dict()
# history of chamber heights:
# chamber_height_history[k] = chamber height after iteration with loop index k
chamber_height_history = []

iterations = 1000000000000
if len(sys.argv) > 2:
    iterations = int(sys.argv[2])

# index in jet
j = -1
# index in rocks
r = 0
# loop index
k = 0


def shift_rock(rock: List[int], bottom: int, direction: str):
    if direction == "<":
        hits_wall = any(map(lambda row: row & 0b1000000 != 0, rock))
        if hits_wall:
            return

        hits_tower = any(
            map(lambda t: (t[1] << 1) & chamber[bottom + t[0]] != 0,
                enumerate(rock)))
        if hits_tower:
            return

        for i in range(len(rock)):
            rock[i] <<= 1

    elif direction == ">":
        hits_wall = any(map(lambda row: row & 0b0000001 != 0, rock))
        if hits_wall:
            return

        hits_tower = any(
            map(lambda t: (t[1] >> 1) & chamber[bottom + t[0]] != 0,
                enumerate(rock)))
        if hits_tower:
            return

        for i in range(len(rock)):
            rock[i] >>= 1

    else:
        raise RuntimeError(f'illegal direction {direction}')


def can_drop_rock(rock: List[int], bottom: int) -> bool:
    return all(
        map(lambda t: t[1] & chamber[bottom + t[0] - 1] == 0, enumerate(rock)))


def update_vspace() -> None:
    for i in range(chamber_width - 1, -1, -1):
        v = 0
        while chamber[chamber_height - v] & 2**i == 0:
            v += 1
        vspace[i] = v


def print_row(row: int):
    s = ''
    for i in range(chamber_width - 1, -1, -1):
        s += '#' if row & 2**i != 0 else '.'
    print(s)


# drop the rocks until a cycle is detected or until we run out of rocks to drop
while k < iterations:
    rock = rocks[r].copy()
    rock_height = len(rock)

    # set up empty rows at the top of the chamber
    chamber.extend(0 for _ in range(rock_height + BUFFER_HEIGHT))

    # index of row of bottom of rock
    bottom = chamber_height + BUFFER_HEIGHT + 1

    # move rock until it settles
    while True:
        j = (j + 1) % JET_LENGTH
        shift_rock(rock, bottom, jet[j])

        if not can_drop_rock(rock, bottom):
            break
        bottom -= 1

    # copy the rock into the chamber
    for i in range(rock_height):
        chamber[bottom + i] |= rock[i]

    chamber_height = max(chamber_height, bottom + rock_height - 1)
    update_vspace()

    # check for a cycle
    key = (j, r, tuple(vspace))

    if key in memo:
        base_k = memo[key]
        base_height = chamber_height_history[base_k]
        cycle_period = k - base_k
        print(
            f'cycle from base_k = {base_k} to k = {k} (period = {cycle_period})'
        )
        cycle_height = chamber_height - base_height

        # we've already performed the update for iteration k, so subtract 1 from iterations - k
        # for the number of rocks that still need to be dropped
        num_full_cycles, remaining_iters = divmod(iterations - k - 1,
                                                  cycle_period)

        chamber_height += num_full_cycles * cycle_height
        chamber_height += chamber_height_history[base_k +
                                                 remaining_iters] - base_height
        break

    memo[key] = k
    chamber_height_history.append(chamber_height)

    # clear empty rows at the top of the chamber
    while chamber[-1] == 0:
        chamber.pop()

    # update indices
    r = (r + 1) % NUM_ROCKS
    k += 1

print(chamber_height)

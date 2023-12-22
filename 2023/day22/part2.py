import collections
import itertools
import sys
from copy import deepcopy


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
bricks = []

for line in lines:
    [a, b] = line.split('~')
    bricks.append([eval(f'[{a}]'), eval(f'[{b}]')])

bricks.sort(key=lambda b: (b[0][2], b[1][2], b[0][0], b[0][1]))
n = len(bricks)

xmax = max(max(b[0][0], b[1][0]) for b in bricks)
ymax = max(max(b[0][1], b[1][1]) for b in bricks)
zmax = max(max(b[0][2], b[1][2]) for b in bricks)

# entries are indices into bricks
world = [[[None for _ in range(zmax + 1)] for _ in range(ymax + 1)]
         for _ in range(xmax + 1)]
# the bricks below this brick
below = [set() for _ in range(n)]


def xy(brick):
    return itertools.product(*(range(brick[0][i], brick[1][i] + 1)
                               for i in range(2)))


def xyz(brick):
    return itertools.product(*(range(brick[0][i], brick[1][i] + 1)
                               for i in range(3)))


def place_brick(brick, i):
    for x, y, z in xyz(brick):
        world[x][y][z] = i


def height(brick):
    return brick[1][2] - brick[0][2] + 1


# settle bricks and find support of each
for i, brick in enumerate(bricks):
    [[x1, y1, z1], [x2, y2, z2]] = brick
    z = z1

    while z > 1 and all(world[x][y][z - 1] is None for x, y in xy(brick)):
        z -= 1

    h = height(brick)
    brick[0][2] = z
    brick[1][2] = z + h - 1

    place_brick(brick, i)
    below[i] = set(
        filter(lambda x: x is not None,
               (world[x][y][z - 1] for x, y in xy(brick))))

# bricks above this brick
above = [set() for _ in range(n)]

for i, brick in enumerate(bricks):
    for j in below[i]:
        above[j].add(i)

# check how many bricks are removed with each brick
total = 0

for i in range(n):
    ab = deepcopy(above)
    be = deepcopy(below)
    be[i].clear()
    q = collections.deque([i])
    output = set()

    while q:
        j = q.popleft()

        if not be[j]:
            output.add(j)

            for k in ab[j]:
                be[k].remove(j)
                q.append(k)

            ab[j].clear()

    total += len(output) - 1

print(total)

import sys
from collections import Counter
from queue import PriorityQueue
from typing import List, Tuple


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


board = get_lines()
WALL = '#'
# internal dimensions (excludes walls)
height = len(board) - 2
width = len(board[0]) - 2

# (x, y)
start = (1, 0)
end = (width, height + 1)

# (dx, dy) - right, down, left, up, stay
dirs = [(1, 0), (0, 1), (-1, 0), (0, -1), (0, 0)]
dir_names = '>v<^.'

# (x, y) -> list of blizzard directions as indices into dirs (assumed 0-3)
blizzards_initial = dict()
for y in range(1, height + 1):
    for x in range(1, width + 1):
        if board[y][x] == '.':
            continue
        blizzards_initial.setdefault((x, y),
                                     []).append(dir_names.index(board[y][x]))


def update_blizzards(blizzards: dict) -> dict:
    blizzards_new = dict()

    for (x, y), vals in blizzards.items():
        for d in vals:
            dx, dy = dirs[d]
            x_new = ((x + dx) - 1) % width + 1
            y_new = ((y + dy) - 1) % height + 1
            blizzards_new.setdefault((x_new, y_new), []).append(d)

    return blizzards_new


def distance_to_end(point: Tuple[int, int]) -> int:
    return end[0] - point[0] + end[1] - point[1]


def get_possible_moves(blizzards: dict,
                       point: Tuple[int, int]) -> List[Tuple[int, int]]:
    x, y = point
    return [
        i for i, (dx, dy) in enumerate(dirs)
        if 0 <= x + dx <= width + 1 and 0 <= y + dy <= height + 1
        and board[y + dy][x + dx] != WALL and (x + dx, y + dy) not in blizzards
    ]


def search(slack: int = 0, limit: int = 1000) -> int:
    '''
    BFS (with heuristics?).
    '''
    t = 0
    t_max = distance_to_end(start) + slack
    print(f't_max = {t_max}')

    blizzards = blizzards_initial
    frontier = PriorityQueue()
    frontier.put((0, start))

    while t < t_max and not frontier.empty():
        blizzards = update_blizzards(blizzards)
        frontier_next = set()
        counter = Counter()

        i = 0
        while i < limit and not frontier.empty():
            i += 1
            _, (x, y) = frontier.get()

            for d in get_possible_moves(blizzards, (x, y)):
                counter[dir_names[d]] += 1
                dx, dy = dirs[d]
                point_new = (x + dx, y + dy)

                if point_new == end:
                    return t + 1

                frontier_next.add(point_new)

        # if making progress is harder than backtracking, adjust t_max
        if counter['>'] + counter['v'] < counter['<'] + counter['^']:
            t_max += 1
            print(f'  update t_max = {t_max}')

        frontier = PriorityQueue()
        for point in frontier_next:
            dist = distance_to_end(point)
            if t + dist <= t_max:
                frontier.put((dist, point))

        t += 1

    return None


slack = 1
t = None
while t is None:
    t = search(slack=slack)
    slack *= 2

print(f't = {t} (slack = {slack})')

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
START = (1, 0)
END = (width, height + 1)

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


def distance(p1: Tuple[int, int], p2: Tuple[int, int]) -> int:
    return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])


def get_possible_moves(blizzards: dict,
                       point: Tuple[int, int]) -> List[Tuple[int, int]]:
    x, y = point
    return [
        i for i, (dx, dy) in enumerate(dirs)
        if 0 <= x + dx <= width + 1 and 0 <= y + dy <= height + 1
        and board[y + dy][x + dx] != WALL and (x + dx, y + dy) not in blizzards
    ]


def search(blizzards: dict,
           forward: bool = True,
           slack: int = 0,
           limit: int = 1000) -> Tuple[int, dict]:
    '''
    BFS (with heuristics?).
    '''
    start = START if forward else END
    end = END if forward else START

    t = 0
    t_max = distance(start, end) + slack
    print(f't_max = {t_max}')

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
                    return t + 1, blizzards

                frontier_next.add(point_new)

        # if making progress is harder than backtracking, adjust t_max
        total_forward = counter['>'] + counter['v']
        total_backward = counter['<'] + counter['^']
        should_adjust = (total_forward < total_backward) if forward else (
            total_backward < total_forward)
        if should_adjust:
            t_max += 1
            print(f'  update t_max = {t_max}')

        frontier = PriorityQueue()
        for point in frontier_next:
            dist = distance(point, end)
            if t + dist <= t_max:
                frontier.put((dist, point))

        t += 1

    return None, None


blizzards = blizzards_initial
totals = []

for forward in [True, False, True]:
    slack = 1
    t = None
    blizzards_new = None
    while t is None or blizzards_new is None:
        t, blizzards_new = search(blizzards, forward=forward, slack=slack)
        slack *= 2

    blizzards = blizzards_new
    totals.append(t)

print(totals)
print(sum(totals))

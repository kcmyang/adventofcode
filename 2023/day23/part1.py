import collections
import sys

sys.setrecursionlimit(1000000)


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
m = len(grid)
n = len(grid[0])

start = (0, grid[0].index('.'))

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
slopes = '>v<^'

dist = collections.defaultdict(int)


class Search:

    def __init__(self) -> None:
        self.seen = set()
        self.best = 0

    def dfs(self, pos, depth):
        r, c = pos
        cell = grid[r][c]

        self.seen.add(pos)
        self.best = max(self.best, depth)

        dir_indices = range(4) if cell not in slopes else [slopes.index(cell)]

        for i in dir_indices:
            dr, dc = dirs[i]
            rr, cc = r + dr, c + dc
            if 0 <= rr < m and 0 <= cc < n and grid[rr][cc] in [
                    '.', slopes[i]
            ] and (rr, cc) not in self.seen:
                self.dfs((rr, cc), depth + 1)

        self.seen.remove(pos)


s = Search()
s.dfs(start, 0)
print(s.best)

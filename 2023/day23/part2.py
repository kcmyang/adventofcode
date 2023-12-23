import itertools
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
end = (m - 1, grid[-1].index('.'))

dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

# build a graph out of the map
adj = dict()
adj[start] = dict()
adj[end] = dict()

# find nodes
for r, c in itertools.product(range(m), range(n)):
    if grid[r][c] == '#':
        continue

    count = 0

    for dr, dc in dirs:
        rr, cc = r + dr, c + dc

        if 0 <= rr < m and 0 <= cc < n and grid[rr][cc] != '#':
            count += 1

    if count > 2:
        adj[(r, c)] = dict()

# find distances
for node in adj.keys():
    q = [node]
    seen = set(q)
    d = 0

    while q:
        q_next = set()

        for pos in q:
            if pos in adj and pos != node:
                adj[node][pos] = d
                adj[pos][node] = d
                continue

            r, c = pos

            for dr, dc in dirs:
                rr, cc = r + dr, c + dc

                if 0 <= rr < m and 0 <= cc < n and grid[rr][cc] != '#' and (
                        rr, cc) not in seen:
                    q_next.add((rr, cc))
                    seen.add((rr, cc))

        q = list(q_next)
        d += 1


class Search:

    def __init__(self) -> None:
        self.seen = set()
        self.best = 0

    def dfs(self, node, depth):
        self.seen.add(node)

        if node == end:
            self.best = max(self.best, depth)

        for node2, d in adj[node].items():
            if node2 not in self.seen:
                self.dfs(node2, depth + d)

        self.seen.remove(node)


s = Search()
s.dfs(start, 0)
print(s.best)

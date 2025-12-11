import itertools
import math
import sys
from collections import Counter, defaultdict


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

nodes = []
adj = defaultdict(list)

for line in lines:
    node, rest = line.split(': ')
    nodes.append(node)
    adj[node] = rest.split()


class Search:

    def __init__(self, adj):
        self.adj = adj

    # bfs
    def search(self, start, goal):
        total = 0
        counter = Counter({start: 1})
        while counter:
            new = Counter()
            for u, c in counter.items():
                if u == goal:
                    total += c
                    continue
                for v in self.adj[u]:
                    new[v] += c
            counter = new
        return total


s = Search(adj)
paths = [
    ['svr', 'fft', 'dac', 'out'],
    ['svr', 'dac', 'fft', 'out'],
]
total = 0
for p in paths:
    vals = [s.search(*t) for t in itertools.pairwise(p)]
    print(vals)
    total += math.prod(vals)
print(total)

import math
import sys

import networkx as nx


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

G = nx.Graph()

for line in lines:
    [k, rest] = line.split(': ')
    rest = rest.split(' ')

    for r in rest:
        G.add_edge(k, r)

# cheating? maybe...
cut_value, partition = nx.stoer_wagner(G)
print(cut_value)
print(math.prod(len(p) for p in partition))

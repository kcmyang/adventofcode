import collections
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
adj = collections.defaultdict(set)
nodes = set()

for l in lines:
    u, v = l.split('-')
    adj[u].add(v)
    adj[v].add(u)
    nodes.add(u)
    nodes.add(v)

threes = set()

for u in nodes:
    for v1 in adj[u]:
        for v2 in adj[u]:
            if v1 != v2 and v2 in adj[v1]:
                threes.add(tuple(sorted([u, v1, v2])))

s = [x for x in threes if any(t[0] == 't' for t in x)]
print(len(s))

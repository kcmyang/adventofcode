import functools
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


def add_comp(comps, c):
    comps[','.join(sorted(c))] = c


comps = dict()  # key each comp by sorting and joining it into a string

# make all K_3's
for u in nodes:
    for v1 in adj[u]:
        for v2 in adj[u]:
            if v1 != v2 and v2 in adj[v1]:
                add_comp(comps, [u, v1, v2])

# repeatedly expand to next K_n's
while True:
    new = dict()
    for c in comps.values():
        neighbours = functools.reduce(lambda acc, u: acc | adj[u], c, set())
        for v in neighbours:
            if all(v in adj[u] for u in c):
                add_comp(new, list(c) + [v])

    if not new:
        break

    comps = new

print(comps)
k, _ = comps.popitem()
print(k)

import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

nodes = []
adj = dict()

for line in lines:
    node, rest = line.split(': ')
    nodes.append(node)
    adj[node] = rest.split()


# dfs
def search(adj, path, goal, ref):
    if path[-1] == goal:
        ref[0] += 1
        return
    for node in adj[path[-1]]:
        path.append(node)
        search(adj, path, goal, ref)
        path.pop()


goal = 'out'
path = ['you']
ref = [0]
search(adj, path, goal, ref)
print(ref[0])

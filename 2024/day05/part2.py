import collections
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
s = lines.index('')
rules = lines[:s]
updates = lines[s + 1:]

adj = collections.defaultdict(set)
for r in rules:
    [x, y] = r.split('|')
    adj[x].add(y)


def check(nums):
    seen = set()

    while nums:
        x = nums.pop(0)

        for v in adj[x]:
            if v in seen:
                return False

        seen.add(x)

    return True


incorrect = []
for u in updates:
    nums = u.split(',')
    if not check(list(nums)):
        incorrect.append(nums)


def fix(nums):
    # toposort
    nodes = set(nums)
    adj_ = collections.defaultdict(set)
    indegree = collections.Counter()

    for u in nodes:
        for v in adj[u]:
            if v in nodes:
                adj_[u].add(v)
                indegree[v] += 1

    s = set(filter(lambda u: indegree[u] == 0, nodes))
    topo = []

    while s:
        u = s.pop()
        topo.append(u)

        for v in adj_[u]:
            indegree[v] -= 1

            if indegree[v] == 0:
                s.add(v)

        adj_[u].clear()

    return topo


fixed = [fix(nums) for nums in incorrect]
print(sum(int(a[len(a) // 2]) for a in fixed))

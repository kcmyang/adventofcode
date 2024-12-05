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


correct = []
for u in updates:
    nums = u.split(',')
    if check(list(nums)):
        correct.append(nums)

print(sum(int(a[len(a) // 2]) for a in correct))

import collections
import math
import re
import sys
from copy import deepcopy


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()

flows = dict()

RMIN = 1
RMAX = 4000

for i, line in enumerate(lines):
    if not line:
        break

    key, rest = re.findall(r'(.+)\{(.+)\}', line)[0]
    parts = rest.split(',')
    p = []

    for part in parts:
        if ':' in part:
            t, op, num, k = re.findall(r'(.)(.)(\d+):(.+)', part)[0]
            p.append([t, op, int(num), k])
        else:
            p.append([part])

    flows[key] = p

q = collections.deque()
q.append((['in'], {'x': (RMIN, RMAX), 'm': (RMIN, RMAX), 'a': (RMIN, RMAX), 's': (RMIN, RMAX)}))

total = 0

# Trace all possible paths through the workflows
while q:
    path, nums = q.popleft()
    k = path[-1]

    if any(lo > hi for lo, hi in nums.values()):
        continue

    if k == 'A':
        total += math.prod(hi - lo + 1 for lo, hi in nums.values())
        print(f'accept: {path} {nums}')
        continue

    if k == 'R':
        continue

    for part in flows[k]:
        if len(part) == 1:
            path2 = path.copy() + [part[0]]
            nums2 = deepcopy(nums)
            q.append((path2, nums2))
            break

        [t, op, num, k2] = part
        path2 = path.copy() + [k2]

        if op == '<':
            lo, hi = nums[t]

            # Follow this jump
            nums2 = deepcopy(nums)
            nums2[t] = (lo, num - 1)
            q.append((path2, nums2))

            # Skip this jump
            nums[t] = (num, hi)

        if op == '>':
            lo, hi = nums[t]

            # Follow this jump
            nums2 = deepcopy(nums)
            nums2[t] = (num + 1, hi)
            q.append((path2, nums2))

            # Skip this jump
            nums[t] = (lo, num)

print(total)

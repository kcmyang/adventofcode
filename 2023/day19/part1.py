import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()

flows = dict()
s = 0

for i, line in enumerate(lines):
    if not line:
        s = i + 1
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

total = 0

for line in lines[s:]:
    k = 'in'
    nums = eval('dict(' + line[1:-1] + ')')

    while True:
        if k == 'A':
            total += sum(nums.values())
            break

        if k == 'R':
            break

        parts = flows[k]

        for part in parts:
            if len(part) == 1:
                k = part[0]
                break

            [t, op, num, k2] = part

            if op == '<' and nums[t] < num:
                k = k2
                break

            if op == '>' and nums[t] > num:
                k = k2
                break

print(total)

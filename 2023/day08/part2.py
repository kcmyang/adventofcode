import math
import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

moves = lines[0]

mapping = dict()

for line in lines[2:]:
    start, left, right = re.findall(r'(.+) = \((.+), (.+)\)', line)[0]
    mapping[start] = {'L': left, 'R': right}

ps = list(filter(lambda x: x.endswith('A'), mapping.keys()))
dists = []

for p in ps:
    dist = 0

    while not p.endswith('Z'):
        p = mapping[p][moves[dist % len(moves)]]
        dist += 1

    dists.append(dist)

print(math.lcm(*dists))

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

i = 0
p = 'AAA'

while p != 'ZZZ':
    p = mapping[p][moves[i % len(moves)]]
    i += 1

print(i)

import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

total = 0

for line in lines:
    [s1, s2] = line.split('| ')
    s1 = s1.split(': ')[1]

    winners = set(re.findall(r'\d+', s1))
    hand = set(re.findall(r'\d+', s2))

    k = len(winners.intersection(hand))

    if k > 0:
        total += 2**(k - 1)

print(total)

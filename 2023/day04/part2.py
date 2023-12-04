import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
n = len(lines)

copies = [1] * n

for i in range(n):
    line = lines[i]
    [s1, s2] = line.split('|')
    s1 = s1.split(':')[1]

    winners = set(re.findall(r'\d+', s1))
    hand = set(re.findall(r'\d+', s2))

    k = len(winners.intersection(hand))

    for j in range(i + 1, min(n, i + k + 1)):
        copies[j] += copies[i]

print(sum(copies))

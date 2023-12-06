import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
times = [int(x) for x in re.findall(r'\d+', lines[0])]
dists = [int(x) for x in re.findall(r'\d+', lines[1])]
ways = 1

for time, dist in zip(times, dists):
    count = 0

    for t in range(1, time):
        d = (time - t) * t
        if d > dist:
            count += 1

    ways *= count

print(ways)

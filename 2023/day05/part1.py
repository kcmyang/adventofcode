import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
n = len(lines)

seeds = [int(x) for x in re.findall('\d+', lines[0])]

maps = []

i = 2
while i < n:
    if re.match(r'.* map:', lines[i]):
        ranges = []
        i += 1

        while i < n and lines[i]:
            ranges.append([int(x) for x in lines[i].split(' ')])
            i += 1

        maps.append(ranges)

    i += 1


locations = []

for seed in seeds:
    x = seed

    for m in maps:
        for [dest, source, d] in m:
            if source <= x < source + d:
                x = dest + (x - source)
                break

    locations.append(x)

print(min(locations))

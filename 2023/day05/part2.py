import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
n = len(lines)

seeds = [(int(x), int(y)) for x, y in re.findall('(\d+) (\d+)', lines[0])]

maps = []

i = 2
while i < n:
    if re.match(r'.* map:', lines[i]):
        ranges = []
        i += 1

        while i < n and lines[i]:
            ranges.append([int(x) for x in lines[i].split(' ')])
            i += 1

        ranges.sort()

        # if this map does not start at 0, add that range
        if ranges[0][0] != 0:
            ranges.insert(0, [0, 0, ranges[0][0]])

        maps.append(ranges)

    i += 1

maps.reverse()


def get_ranges(ranges: list[tuple[int, int, int]], maps: list[list[int]]) -> list[tuple[int, int, int]]:
    if not maps:
        return ranges

    new_ranges = []

    for (loc_start, start, length) in ranges:
        while length > 0:
            # the input range is either mapped to multiple output ranges, or comes after all
            # the mappings and is passed through unchanged
            new_start = start
            new_length = length

            for dest, source, d in maps[0]:
                k = start - dest

                if 0 <= k < d:
                    new_start = source + k
                    new_length = min(length, d - k)
                    break

            new_ranges.append((loc_start, new_start, new_length))
            loc_start += new_length
            start += new_length
            length -= new_length

    return get_ranges(new_ranges, maps[1:])


# map each location range to a seed range
best = float('inf')
seed_ranges = []

for dest, _, d in maps[0]:
    seed_ranges.extend(get_ranges([(dest, dest, d)], maps))

# for each seed range, obtain the lowest corresponding location which maps to a starting seed
locations = []

for loc_start, start, length in seed_ranges:
    for seed_start, seed_length in seeds:
        if seed_start <= start < seed_start + seed_length:
            locations.append(loc_start)
        elif start <= seed_start < start + length:
            locations.append(loc_start + (seed_start - start))

print(min(locations))

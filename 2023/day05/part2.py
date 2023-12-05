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


def get_ranges(ranges: list[tuple[int, int]], maps: list[list[int]]) -> list[tuple[int, int]]:
    if not maps:
        return ranges

    new_ranges = []

    for start, length in ranges:
        while length > 0:
            # the input range is either mapped to multiple output ranges, or comes after all
            # the mappings and is passed through unchanged
            new_start = start
            new_length = length

            for dest, source, d in maps[0]:
                k = start - source

                if 0 <= k < d:
                    new_start = dest + k
                    new_length = min(length, d - k)
                    break

            new_ranges.append((new_start, new_length))
            start += new_length
            length -= new_length

    return get_ranges(new_ranges, maps[1:])


# map each seed range to its corresponding location range(s)
location_ranges = get_ranges(seeds, maps)
print(min(start for start, _ in location_ranges))

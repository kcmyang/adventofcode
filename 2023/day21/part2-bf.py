import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
n = len(grid)
s = n // 2
print(f'n = {n}, s = {s}')

dirs = [(-1, 0), (0, 1), (1, 0), (0, -1)]

q = [(s, s)]
seen = set(q)
odd = set()
even = set(q)

# first s steps
for i in range(1, s + 1):
    q_next = set()

    for r, c in q:
        for dr, dc in dirs:
            rr = (r + dr) % n
            cc = (c + dc) % n
            if grid[rr][cc] != '#' and (r + dr, c + dc) not in seen:
                q_next.add((r + dr, c + dc))
                seen.add((r + dr, c + dc))
                (odd if i % 2 == 1 else even).add((r + dr, c + dc))

    q = list(q_next)

MAX_RADIUS = 15
lengths = []

for radius in range(1, MAX_RADIUS + 1):
    begin = (radius - 1) * n + s + 1
    end = radius * n + s

    for i in range(begin, end + 1):
        q_next = set()

        for r, c in q:
            for dr, dc in dirs:
                rr = (r + dr) % n
                cc = (c + dc) % n
                if grid[rr][cc] != '#' and (r + dr, c + dc) not in seen:
                    q_next.add((r + dr, c + dc))
                    seen.add((r + dr, c + dc))
                    (odd if i % 2 == 1 else even).add((r + dr, c + dc))

        q = list(q_next)

    lengths.append(len(odd if end % 2 == 1 else even))
    print(f'{end} steps (radius {radius}): {lengths[-1]}')

print('lengths', lengths)
diffs = [lengths[i + 1] - lengths[i] for i in range(len(lengths) - 1)]
diffs2 = [diffs[i + 1] - diffs[i] for i in range(len(diffs) - 1)]
print('diffs', diffs)
print('diffs2', diffs2)

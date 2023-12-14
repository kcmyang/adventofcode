import itertools
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
grid = [list(line) for line in lines]
m = len(grid)
n = len(grid[0])

iterations = 1000000000

# map of (tuple of round rock coordinates) -> iteration number
memo = dict()
# iteration number -> list of round rock coordinates
rocks_list = []

final_rocks = []

for i in range(iterations):
    # north
    for c in range(n):
        target = 0

        while target < m and grid[target][c] != '.':
            target += 1

        for r in range(target, m):
            if grid[r][c] == '#':
                target = r + 1
            elif grid[r][c] == 'O':
                grid[r][c] = '.'
                grid[target][c] = 'O'

                while target <= r and grid[target][c] != '.':
                    target += 1

    # west
    for r in range(m):
        target = 0

        while target < n and grid[r][target] != '.':
            target += 1

        for c in range(target, n):
            if grid[r][c] == '#':
                target = c + 1
            elif grid[r][c] == 'O':
                grid[r][c] = '.'
                grid[r][target] = 'O'

                while target <= c and grid[r][target] != '.':
                    target += 1

    # south
    for c in range(n):
        target = m - 1

        while target >= 0 and grid[target][c] != '.':
            target -= 1

        for r in range(target, -1, -1):
            if grid[r][c] == '#':
                target = r - 1
            elif grid[r][c] == 'O':
                grid[r][c] = '.'
                grid[target][c] = 'O'

                while target >= r and grid[target][c] != '.':
                    target -= 1

    # east
    for r in range(m):
        target = n - 1

        while target >= 0 and grid[r][target] != '.':
            target -= 1

        for c in range(target, -1, -1):
            if grid[r][c] == '#':
                target = c - 1
            elif grid[r][c] == 'O':
                grid[r][c] = '.'
                grid[r][target] = 'O'

                while target >= c and grid[r][target] != '.':
                    target -= 1

    rocks = tuple(filter(lambda p: grid[p[0]][p[1]] == 'O', itertools.product(range(m), range(n))))

    if rocks not in memo:
        memo[rocks] = i
        rocks_list.append(list(rocks))
        continue

    # cycle found: find the final list of rock positions directly
    cycle_start = memo[rocks]
    cycle_length = i - cycle_start

    i += (iterations - i) // cycle_length * cycle_length
    remaining = iterations - i - 1
    final_rocks = rocks_list[cycle_start + remaining]
    break

total = sum(map(lambda p: m - p[0], final_rocks))
print(total)

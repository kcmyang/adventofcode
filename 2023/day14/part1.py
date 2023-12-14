import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
grid = [list(line) for line in lines]
m = len(grid)
n = len(grid[0])

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

total = sum(map(lambda p: (m - p[0]) * p[1].count('O'), enumerate(grid)))
print(total)

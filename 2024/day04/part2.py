import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
h = len(grid)
w = len(grid[0])

count = 0

bank = set(['MSMS', 'SMSM', 'MMSS', 'SSMM'])

for r in range(1, h - 1):
    for c in range(1, w - 1):
        if grid[r][c] == 'A' and (grid[r - 1][c - 1] + grid[r - 1][c + 1] + grid[r + 1][c - 1] +
                                  grid[r + 1][c + 1]) in bank:
            count += 1

print(count)

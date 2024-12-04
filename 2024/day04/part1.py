import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
h = len(grid)
w = len(grid[0])

count = 0

vecs = [(1, 1), (-1, -1), (1, -1), (-1, 1), (1, 0), (-1, 0), (0, 1), (0, -1)]

for r in range(h):
    for c in range(w):
        for dr, dc in vecs:
            if 0 <= r + 3 * dr < h and 0 <= c + 3 * dc < w and all(
                    grid[r + i * dr][c + i * dc] == ch for i, ch in zip(range(4), 'XMAS')):
                count += 1

print(count)

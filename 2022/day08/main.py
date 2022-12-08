import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


def make_grid(lines) -> List[List[int]]:
    return [[int(ch) for ch in row] for row in lines]


def part1(grid):
    n = len(grid)

    visible_grid = [[False for _ in range(n)] for _ in range(n)]

    # fill rows
    for i in range(n):
        curr_max = -1

        for j in range(n):
            if grid[i][j] > curr_max:
                curr_max = grid[i][j]
                visible_grid[i][j] = True

        curr_max = -1

        for j in range(n - 1, -1, -1):
            if grid[i][j] > curr_max:
                curr_max = grid[i][j]
                visible_grid[i][j] = True

    # fill columns
    for j in range(n):
        curr_max = -1

        for i in range(n):
            if grid[i][j] > curr_max:
                curr_max = grid[i][j]
                visible_grid[i][j] = True

        curr_max = -1

        for i in range(n - 1, -1, -1):
            if grid[i][j] > curr_max:
                curr_max = grid[i][j]
                visible_grid[i][j] = True

    # count
    total = 0

    for i in range(n):
        for j in range(n):
            if visible_grid[i][j]:
                total += 1

    return total


def part2(grid):
    n = len(grid)

    def viewing_distance(x, y, dx, dy):
        dist = 0
        x2, y2 = x, y

        while True:
            x2 += dx
            y2 += dy

            if not (0 <= x2 < n and 0 <= y2 < n):
                break

            dist += 1

            if grid[y2][x2] >= grid[y][x]:
                break

        return dist

    def scenic_score(x, y):
        return (viewing_distance(x, y, 0, 1) * viewing_distance(x, y, 0, -1) *
                viewing_distance(x, y, 1, 0) * viewing_distance(x, y, -1, 0))

    best = 0

    for y in range(n):
        for x in range(n):
            best = max(best, scenic_score(x, y))

    return best


def main() -> None:
    lines = get_lines()
    # square grid
    grid = make_grid(lines)
    print(part1(grid))
    print(part2(grid))


if __name__ == '__main__':
    main()

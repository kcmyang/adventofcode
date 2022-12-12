import sys
from queue import Queue
from typing import List, Tuple


def process_lines(
    lines
) -> Tuple[List[List[int]], int, int, Tuple[int, int], Tuple[int, int]]:
    grid = []
    height = len(lines)
    width = len(lines[0])
    start = (None, None)
    end = (None, None)

    for r in range(height):
        row = []

        for c in range(width):
            if lines[r][c] == 'S':
                row.append(ord('a'))
                start = (r, c)
            elif lines[r][c] == 'E':
                row.append(ord('z'))
                end = (r, c)
            else:
                row.append(ord(lines[r][c]))

        grid.append(row)

    return (grid, height, width, start, end)


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


def part1(grid, height, width, start, end):
    dist_grid = [[float('inf') for _ in range(width)] for _ in range(height)]
    dist = 0

    def neighbours(r: int, c: int) -> List[Tuple[int, int]]:
        result = []

        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            r2, c2 = r + dr, c + dc
            if (0 <= r2 < height and 0 <= c2 < width
                    and grid[r2][c2] + 1 >= grid[r][c]
                    and dist_grid[r2][c2] > dist):
                result.append((r2, c2))

        return result

    queue = Queue()
    queue.put(set({end}))

    while not queue.empty():
        frontier = queue.get()
        frontier_next = set()

        for r, c in frontier:
            if (r, c) == start:
                return dist

            dist_grid[r][c] = dist

            for neighbour in neighbours(r, c):
                frontier_next.add(neighbour)

        if frontier_next:
            queue.put(frontier_next)

        dist += 1

    return -1


def part2(grid, height, width, _, end):
    dist_grid = [[float('inf') for _ in range(width)] for _ in range(height)]
    dist = 0

    def neighbours(r: int, c: int) -> List[Tuple[int, int]]:
        result = []

        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            r2, c2 = r + dr, c + dc
            if (0 <= r2 < height and 0 <= c2 < width
                    and grid[r2][c2] + 1 >= grid[r][c]
                    and dist_grid[r2][c2] > dist_grid[r][c]):
                result.append((r2, c2))

        return result

    queue = Queue()
    queue.put(set({end}))
    best = float('inf')

    while not queue.empty():
        frontier = queue.get()
        frontier_next = set()

        for r, c in frontier:
            dist_grid[r][c] = dist

            if grid[r][c] == ord('a') and dist < best:
                best = dist

            for neighbour in neighbours(r, c):
                frontier_next.add(neighbour)

        if frontier_next:
            queue.put(frontier_next)

        dist += 1

    return best


def main() -> None:
    lines = get_lines()
    args = process_lines(lines)
    print(part1(*args))
    print(part2(*args))


if __name__ == '__main__':
    main()

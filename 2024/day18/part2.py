import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


s = 71 if len(sys.argv) == 1 else 7


def reachable(walls):
    seen = set((0, 0))
    frontier = [(0, 0)]
    steps = 0
    dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    while frontier:
        steps += 1
        f2 = []

        for r, c in frontier:
            for dr, dc in dirs:
                rr, cc = r + dr, c + dc

                if 0 <= rr < s and 0 <= cc < s and (rr, cc) not in seen and (rr, cc) not in walls:
                    if rr == s - 1 and cc == s - 1:
                        return True

                    seen.add((rr, cc))
                    f2.append((rr, cc))

        frontier = f2

    return False


lines = get_lines()

walls = set()

for l in lines:
    x, y = list(map(int, l.split(',')))
    walls.add((x, y))

    if not reachable(walls):
        print(f'{x},{y}')
        break

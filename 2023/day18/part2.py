import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
dirs = {'L': (-1, 0), 'U': (0, 1), 'R': (1, 0), 'D': (0, -1)}
dirmap = 'RDLU'
opposite_dir = {'R': 'L', 'L': 'R', 'U': 'D', 'D': 'U'}

x, y = 0, 0

# list of points in order
points = []
# list of directions of segments
directions = []

for line in lines:
    [d, t, h] = line.split(' ')
    h = h[2:-1]
    d = dirmap[int(h[-1])]
    dx, dy = dirs[d]
    t = int(t)
    t = int(h[:-1], base=16)

    points.append((x, y))
    directions.append(d)
    x += dx * t
    y += dy * t

n = len(points)

# Find the orientation of the polygon.
# https://en.wikipedia.org/wiki/Curve_orientation
# Tricky caveat: the internal angle of the three points chosen here should be convex (< pi).
# We can ensure this happens by taking the lowest, leftmost point as the middle point.
m, (x2, y2) = min(enumerate(points), key=lambda x: x[1])
x1, y1 = points[(m - 1) % n]
x3, y3 = points[(m + 1) % n]
det = (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

# Force positive orientation.
if det < 0:
    points.reverse()
    points.insert(0, points.pop())
    directions = [opposite_dir[d] for d in directions[::-1]]

# Use shoelace formula to find the enclosed area.
# https://en.wikipedia.org/wiki/Shoelace_formula
# Note: since the blocks are 1 metre on-centre at each point in points, we need to adjust the points so that
# they describe the corners of the polygon.
# Convention: a block "at (x, y)" is taken to have its centre at Cartesian coordinates (x + 0.5, y + 0.5).

cpoints = []

for i, (x, y) in enumerate(points):
    d_in = directions[(i - 1) % n]
    d_out = directions[i]

    match d_in, d_out:
        case ('R', 'D') | ('D', 'R'):
            cpoints.append((x, y))
        case ('R', 'U') | ('U', 'R'):
            cpoints.append((x + 1, y))
        case ('L', 'D') | ('D', 'L'):
            cpoints.append((x, y + 1))
        case ('L', 'U') | ('U', 'L'):
            cpoints.append((x + 1, y + 1))

for i in range(n):
    x1, y1 = cpoints[i]
    x2, y2 = cpoints[(i + 1) % n]
    assert (x1 == x2 and y1 != y2) or (x1 != x2 and y1 == y2)

area = 0

for i in range(len(cpoints)):
    x1, y1 = cpoints[i]
    x2, y2 = cpoints[(i + 1) % n]
    area += (y1 + y2) * (x1 - x2)

area = abs(area) // 2

print(area)

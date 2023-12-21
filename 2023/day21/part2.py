import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


grid = get_lines()
n = len(grid)
s = n // 2

# Observations/assumptions:
# - m, n are equal and odd
# - the start is in the centre of the grid
# - the borders are always empty
# - there is a diamond of empty spaces in the grid, with each corner at the midpoint of an edge
# (!) there are no walls in the middle row and column

# Observation: this is actually RADIUS * n + s
STEPS = 26501365
RADIUS = (STEPS - s) // n
print(RADIUS)

# Get these from the brute force solution and wolframalpha
a = 14909
b = 15021
c = 3802

# I love hard coding
print(a * RADIUS * RADIUS + b * RADIUS + c)

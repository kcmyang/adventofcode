import heapq
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

points = []
for line in lines:
    point = tuple(map(int, line.split(',')))
    points.append(point)

dist = lambda x, y: sum(abs(i - j)**2 for i, j in zip(x, y))

edges = []
for i, p1 in enumerate(points):
    for p2 in points[i + 1:]:
        edges.append((dist(p1, p2), p1, p2))
edges.sort(reverse=True)


def find(p, comps):
    for i, c in enumerate(comps):
        if p in c:
            return i


comps = [set([p]) for p in points]
p1 = None
p2 = None
while len(comps) > 1:
    _, p1, p2 = edges.pop()
    i1 = find(p1, comps)
    i2 = find(p2, comps)
    if i1 == i2:
        continue
    if i2 < i1:
        i1, i2 = i2, i1
    c2 = comps.pop(i2)
    comps[i1].update(c2)

x1 = p1[0]
x2 = p2[0]
print(x1, x2)
print(x1 * x2)

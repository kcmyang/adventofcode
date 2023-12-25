import collections
import itertools
import math
import re
import sys

import numpy as np


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

stones = []

for line in lines:
    [p, v] = line.split(' @ ')
    stones.append((eval(f'({p})'), eval(f'({v})')))


def get_intersect(a1, a2, b1, b2):
    """
    Returns the point of intersection of the lines passing through a2,a1 and b2,b1.
    a1: [x, y] a point on the first line
    a2: [x, y] another point on the first line
    b1: [x, y] a point on the second line
    b2: [x, y] another point on the second line
    """
    s = np.vstack([a1, a2, b1, b2])  # s for stacked
    h = np.hstack((s, np.ones((4, 1))))  # h for homogeneous
    l1 = np.cross(h[0], h[1])  # get first line
    l2 = np.cross(h[2], h[3])  # get second line
    x, y, z = np.cross(l1, l2)  # point of intersection
    if z == 0:  # lines are parallel
        return None
    return (x / z, y / z)


MIN, MAX = 7, 27
MIN, MAX = 200000000000000, 400000000000000
count = 0


def forward(p, v, t):
    return all(np.sign(t[i] - p[i]) == np.sign(v[i]) for i in range(2))


for i, (pp1, vv1) in enumerate(stones):
    for pp2, vv2 in stones[i + 1:]:
        p1 = np.array(pp1[:2])
        p2 = np.array(pp2[:2])
        v1 = np.array(vv1[:2])
        v2 = np.array(vv2[:2])

        t = get_intersect(p1, p1 + 1000 * v1, p2, p2 + 1000 * v2)
        if t is not None:
            x, y = t
            if forward(p1, v1, t) and forward(
                    p2, v2, t) and MIN <= x <= MAX and MIN <= y <= MAX:
                count += 1

print(count)

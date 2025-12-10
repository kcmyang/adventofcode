import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


def is_point_on_segment(point, segment):
    (x1, y1), (x2, y2) = segment
    x, y = point
    return x == x1 == x2 and min(y1, y2) <= y <= max(y1, y2) or y == y1 == y2 and min(x1, x2) <= x <= max(x1, x2)


def is_point_in_polygon(point, segments):
    # to be inside, the ray extending to the right from the point must cross an odd number of
    # (vertical) segments
    x, y = point
    inside = False
    for segment in segments:
        if is_point_on_segment(point, segment):
            return True
        # check if this segment crosses the horizontal line at y
        (x1, y1), (x2, y2) = segment
        if min(y1, y2) < y < max(y1, y2):
            assert x1 == x2
            # if the intersection is to the right, negate the answer
            if x1 > x:
                inside = not inside
    return inside


def does_segment_intersect_rectangle(segment, rect_edges: tuple[int, int, int, int]):
    (x1, y1), (x2, y2) = segment
    xmin = min(x1, x2)
    xmax = max(x1, x2)
    ymin = min(y1, y2)
    ymax = max(y1, y2)
    left, right, bottom, top = rect_edges
    # vertical lines
    if x1 == x2:
        # if above or below, then outside
        if ymax <= top or ymin >= bottom:
            return False
        # if left or right, then outside
        if x1 < left or x1 > right:
            return False
        # if strictly inside, then inside
        if left < x1 < right:
            return True
        # if segment pointing up, inside is to the right
        if y1 > y2:
            return x1 != left
        # if segment pointing down, inside is to the left
        if y1 < y2:
            return x1 != right
        raise Exception('unhandled vertical line')
    # horizontal lines
    if y1 == y2:
        # if left or right, then outside
        if xmax <= left or xmin >= right:
            return False
        # if above or below, then outside
        if y1 < top or y1 > bottom:
            return False
        # if strictly inside, then inside
        if top < y1 < bottom:
            return True
        # if segment pointing left, inside is above
        if x1 > x2:
            return y1 != bottom
        # if segment pointing right, inside is below
        if x1 < x2:
            return y1 != top
        raise Exception('unhandled horizontal line')
    raise Exception('invalid segment')


def main():
    lines = get_lines()
    points = list(tuple(map(int, line.split(','))) for line in lines)
    n = len(points)
    segments = list(itertools.pairwise(points)) + [(points[-1], points[0])]

    # check each possible rectangle
    valid = []
    for i in range(n):
        x1, y1 = pi = points[i]
        for j in range(i + 1, n):
            x2, y2 = pj = points[j]
            left = min(x1, x2)
            right = max(x1, x2)
            top = min(y1, y2)  # y points down
            bottom = max(y1, y2)  # y points down
            corners = [(left, top), (left, bottom), (right, top), (right, bottom)]
            rect = (pi, pj)
            if not all(is_point_in_polygon(c, segments) for c in corners):
                continue
            ok = True
            for s in segments:
                if does_segment_intersect_rectangle(s, (left, right, bottom, top)):
                    ok = False
                    break
            if not ok:
                continue
            valid.append(rect)

    a = max((abs(x2 - x1) + 1) * (abs(y2 - y1) + 1) for (x1, y1), (x2, y2) in valid)
    print(a)


main()

import itertools
import sys
import functools


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

numpad = ['789', '456', '123', ' 0A']
numpad_map = dict(
    (numpad[r][c], (r, c)) for r in range(len(numpad)) for c in range(len(numpad[0])) if numpad[r][c] != ' ')
dirpad = [' ^A', '<v>']
dirpad_map = dict(
    (dirpad[r][c], (r, c)) for r in range(len(dirpad)) for c in range(len(dirpad[0])) if dirpad[r][c] != ' ')

dirs = [(0, 1, '>'), (0, -1, '<'), (1, 0, 'v'), (-1, 0, '^')]


def make_paths(pad, padmap):
    h = len(pad)
    w = len(pad[0])
    out = dict()

    def dfs(r, c, target, path, paths, seen, maxdepth):
        if len(path) > maxdepth:
            return

        if pad[r][c] == target:
            paths.append(''.join(path) + 'A')
            return

        for dr, dc, t in dirs:
            rr, cc = r + dr, c + dc
            if 0 <= rr < h and 0 <= cc < w and pad[rr][cc] in padmap and (rr, cc) not in seen:
                path.append(t)
                seen.add((rr, cc))
                dfs(rr, cc, target, path, paths, seen, maxdepth)
                seen.remove((rr, cc))
                path.pop()

    for ch1, (r1, c1) in padmap.items():
        for ch2, (r2, c2) in padmap.items():
            paths = []
            dfs(r1, c1, ch2, [], paths, set([(r1, c1)]), abs(r2 - r1) + abs(c2 - c1))
            out[(ch1, ch2)] = paths

    return out


numpad_paths = make_paths(numpad, numpad_map)
dirpad_paths = make_paths(dirpad, dirpad_map)


def shortest_paths(paths, seq):
    out = list(paths[('A', seq[0])])

    for ch1, ch2 in itertools.pairwise(seq):
        out = [x + p for p in paths[(ch1, ch2)] for x in out]

    return out


@functools.cache
def dist(ch1, ch2, d: int):
    if d == 1:
        return len(dirpad_paths[(ch1, ch2)][0])

    return dist_multi(dirpad_paths[(ch1, ch2)], d - 1)


def dist_multi(paths, d: int):
    return min(dist_path(p, d) for p in paths)


def dist_path(path, d: int):
    return dist('A', path[0], d) + sum(dist(x, y, d) for x, y in itertools.pairwise(path))


ans = 0
depth = 25

for line in lines:
    paths = shortest_paths(numpad_paths, line)
    best = dist_multi(paths, depth)
    num = int(line[:-1])
    print('=====', line, best, num)
    ans += best * num

print(ans)

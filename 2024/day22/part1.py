import functools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()


@functools.cache
def mix(x, y):
    return x ^ y


@functools.cache
def prune(x):
    return x % 16777216


@functools.cache
def secret(x):
    x = prune(mix(x, x * 64))
    x = prune(mix(x, x // 32))
    x = prune(mix(x, x * 2048))
    return x


ans = 0

for l in lines:
    x = int(l)
    s = x
    for _ in range(2000):
        s = secret(s)
    # print(x, s)
    ans += s

print(ans)

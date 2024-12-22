import functools
import collections
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


prices = collections.Counter()

for l in lines:
    x = int(l)
    s = x
    seq = [s]
    price = dict()  # map of (4-tuple -> first sale price)

    for _ in range(2000):
        s2 = secret(s)
        p = s2 % 10
        diff = p - (s % 10)
        seq.append(diff)

        if len(seq) > 4:
            seq.pop(0)
            if tuple(seq) not in price:
                price[tuple(seq)] = p

        s = s2

    for k, v in price.items():
        prices[k] += v

print(max(prices.values()))

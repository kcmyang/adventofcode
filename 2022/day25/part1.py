import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


map_sd = {
    '2': 2,
    '1': 1,
    '0': 0,
    '-': -1,
    '=': -2,
}

map_ds = {
    2: '2',
    1: '1',
    0: '0',
    -1: '-',
    -2: '=',
}


def snafu_to_decimal(s: str) -> int:
    return sum(map_sd[ch] * 5**i for i, ch in enumerate(s[::-1]))


def decimal_to_snafu(d: int) -> str:
    # decimal value in each column of the result, right to left
    vals = []

    while d >= 5:
        vals.append(d % 5)
        d //= 5

    vals.append(d)

    # move things around so every digit fits in 2, 1, 0, -1, -2
    n = len(vals)

    for i in range(n):
        if vals[i] > 2:
            vals[i] -= 5
            if i == n - 1:
                vals.append(0)
            vals[i + 1] += 1

    # convert
    return ''.join(map_ds[digit] for digit in vals[::-1])


lines = get_lines()
dec_sum = sum(map(snafu_to_decimal, lines))
print(dec_sum)
snafu_sum = decimal_to_snafu(dec_sum)
print(snafu_sum)

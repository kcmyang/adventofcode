import itertools
import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()

def groups(s: str) -> list[int]:
    return list(map(len, re.findall(r'#+', s)))

def f(row: str) -> int:
    [s, nums] = row.split(' ')
    nums = list(map(int, nums.split(',')))
    blanks = list(filter(lambda i: s[i] == '?', range(len(s))))
    count = 0

    for prod in itertools.product('.#', repeat=len(blanks)):
        t = list(s)

        for ch, i in zip(prod, blanks):
            t[i] = ch

        if groups(''.join(t)) == nums:
            count += 1

    return count

total = sum(map(f, lines))
print(total)

import sys
import re


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

total = 0

words = dict(zip(['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'], range(10)))

def match(s: str, t: str) -> bool:
    i = 0

    while i < len(s) and i < len(t):
        if s[i] != t[i]:
            return False

        i += 1

    return i == len(t)


for line in lines:
    digits = []

    for i in range(len(line)):
        if line[i].isdigit():
            digits.append(int(line[i]))
            continue

        for key, val in words.items():
            if match(line[i:], key):
                digits.append(val)
                break

    total += digits[0] * 10 + digits[-1]

print(total)

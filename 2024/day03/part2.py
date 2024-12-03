import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
enable = True
count = 0

for l in lines:
    matches = re.finditer(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)", l)

    for m in matches:
        if m.group(0) == 'do()':
            enable = True
        elif m.group(0) == "don't()":
            enable = False
        elif enable:
            x, y = m.groups()
            count += int(x) * int(y)

print(count)

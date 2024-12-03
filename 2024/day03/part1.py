import re
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
count = 0

for l in lines:
    matches = re.findall(r'mul\((\d+),(\d+)\)', l)

    for x, y in matches:
        count += int(x) * int(y)

print(count)

import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

dial = 50
count = 0
mod = 100

for line in lines:
    d = line[0]
    v = int(line[1:])

    if d == 'L':
        dial = (dial - v) % mod
    else:
        dial = (dial + v) % mod

    if dial == 0:
        count += 1

print(count)

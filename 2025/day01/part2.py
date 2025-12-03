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

    q, v = divmod(v, mod)
    count += q

    if v > 0:
        if d == 'L':
            dial2 = dial - v
        else:
            dial2 = dial + v

        if dial2 <= 0 < dial or dial2 >= 100:
            count += 1

        dial = dial2 % mod

print(count)

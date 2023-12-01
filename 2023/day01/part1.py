import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

total = 0

for line in lines:
    digits = list(filter(str.isdigit, line))
    total += int(digits[0] + digits[-1])

print(total)

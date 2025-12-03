import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

total = 0

for line in lines:
    digits = list(map(int, line))
    n = len(digits)
    total += max(digits[l] * 10 + digits[r] for l in range(n) for r in range(l + 1, n))

print(total)

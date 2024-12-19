import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

pieces = set(lines[0].split(', '))


def check(pattern: str) -> int:
    n = len(pattern)
    dp = [0] * (n + 1)
    dp[0] = 1

    for i in range(n):
        for p in pieces:
            if pattern.startswith(p, i):
                dp[i + len(p)] += dp[i]

    return dp[n]


count = sum(check(line) for line in lines[2:])

print(count)

import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

pieces = set(lines[0].split(', '))


def check(pattern: str) -> bool:
    n = len(pattern)
    dp = [False] * (n + 1)
    dp[0] = True

    for i in range(n):
        if dp[i]:
            for p in pieces:
                if pattern[i:i + len(p)] == p:
                    dp[i + len(p)] = True

    return dp[n]


count = sum(int(check(line)) for line in lines[2:])

print(count)

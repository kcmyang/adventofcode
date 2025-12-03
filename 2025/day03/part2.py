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
    m = 12

    # dp[i][j] = best length-i joltage in digits[j:]
    dp = [[0 for _ in range(n)] for _ in range(m + 1)]
    dp[1] = list(digits)
    for j in range(n - 2, -1, -1):
        dp[1][j] = max(dp[1][j], dp[1][j + 1])
    for i in range(2, m + 1):
        dp[i][n - i] = int(line[n - i:])
        for j in range(n - i - 1, -1, -1):
            dp[i][j] = max(digits[j] * 10**(i - 1) + dp[i - 1][j + 1], dp[i][j + 1])

    total += dp[m][0]

print(total)

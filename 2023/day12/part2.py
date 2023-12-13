import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()

def run_lengths(s: str) -> list[int]:
    lengths = [0] * len(s)

    if s[0] != '.':
        lengths[0] = 1

    for i in range(1, len(s)):
        if s[i] == '.':
            lengths[i] = 0
        else:
            lengths[i] = lengths[i - 1] + 1

    return lengths

repeat = 5

def f(row: str) -> int:
    [s, nums] = row.split(' ')
    s = '?'.join([s] * repeat)
    n = len(s)
    nums = list(map(int, nums.split(','))) * repeat
    m = len(nums)

    # lengths[i] = largest group of consecutive '#' and '?' ending at s[i]
    lengths = run_lengths(s)

    # dp[i][j][ch] = number of ways to use s[:(i + 1)] such that (1) the groups of '#' in the substring
    # match exactly nums[:j] and (2) s[i] == ch
    dp = [[{'#': 0, '.': 0} for _ in range(m + 1)] for _ in range(n + 1)]

    # fill i = 0 case
    dp[0][0]['.'] = 1

    # fill j = 0 case - the only way to match 0 groups is by setting all '?' to '.' and having no '#'
    for i, ch in enumerate(s, start=1):
        if ch in '.?':
            dp[i][0]['.'] = 1
        else:
            break

    for i, ch in enumerate(s, start=1):
        for j, group in enumerate(nums, start=1):
            if ch in '.?':
                dp[i][j]['.'] = dp[i - 1][j]['#'] + dp[i - 1][j]['.']

            if ch in '#?' and lengths[i - 1] >= group:
                dp[i][j]['#'] = dp[i - group][j - 1]['.']

    return sum(dp[-1][-1].values())

total = sum(map(f, lines))
print(total)

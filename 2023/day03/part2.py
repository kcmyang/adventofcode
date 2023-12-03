import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


board = get_lines()
m = len(board)
n = len(board[0])

seen = [[False for _ in range(n)] for _ in range(m)]
dirs = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

total = 0

for r in range(m):
    for c in range(n):
        if board[r][c] == '*':
            # find adjacent numbers
            nums = []

            for dr, dc in dirs:
                rr, cc = r + dr, c + dc

                if 0 <= rr < m and 0 <= cc < n and board[rr][cc].isdigit() and not seen[rr][cc]:
                    left = cc
                    right = cc

                    while left - 1 >= 0 and board[rr][left - 1].isdigit():
                        left -= 1

                    while right + 1 < n and board[rr][right + 1].isdigit():
                        right += 1

                    nums.append(int(board[rr][left:right + 1]))

                    for i in range(left, right + 1):
                        seen[rr][i] = True

            if len(nums) == 2:
                total += nums[0] * nums[1]

print(total)

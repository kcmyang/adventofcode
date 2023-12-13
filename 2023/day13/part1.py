import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
lines.append('')

cols = []
rows = []

block = []

for line in lines:
    if line:
        block.append(line)
        continue

    m = len(block)
    n = len(block[0])
    flag = True

    # try columns
    # reflect across line to the right of this column
    for i in range(n - 1):
        flag = True
        j = 0

        while True:
            if i - j < 0 or i + j + 1 >= n:
                break

            col1 = [block[r][i - j] for r in range(m)]
            col2 = [block[r][i + j + 1] for r in range(m)]

            if col1 != col2:
                flag = False
                break

            j += 1

        if flag:
            break

    if flag:
        cols.append(i + 1)
        block = []
        continue

    # try rows
    # reflect across line to the bottom of this row
    for i in range(m - 1):
        flag = True
        j = 0

        while True:
            if i - j < 0 or i + j + 1 >= m:
                break

            row1 = block[i - j]
            row2 = block[i + j + 1]

            if row1 != row2:
                flag = False
                break

            j += 1

        if flag:
            break

    if flag:
        rows.append(100 * (i + 1))

    block = []

print(sum(cols) + sum(rows))

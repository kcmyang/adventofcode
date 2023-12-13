import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
lines.append('')

cols = []
rows = []

block = []

def diff(l1, l2):
    return len(l1) - len(set(enumerate(l1)).intersection(set(enumerate(l2))))

for line in lines:
    if line:
        block.append(line)
        continue

    m = len(block)
    n = len(block[0])
    total_diff = 0

    # try columns
    # reflect across line to the right of this column
    for i in range(n - 1):
        total_diff = 0
        j = 0

        while True:
            if i - j < 0 or i + j + 1 >= n:
                break

            col1 = [block[r][i - j] for r in range(m)]
            col2 = [block[r][i + j + 1] for r in range(m)]

            total_diff += diff(col1, col2)

            if total_diff > 1:
                break

            j += 1

        if total_diff == 1:
            break

    if total_diff == 1:
        cols.append(i + 1)
        block = []
        continue

    # try rows
    # reflect across line to the bottom of this row
    for i in range(m - 1):
        total_diff = 0
        j = 0

        while True:
            if i - j < 0 or i + j + 1 >= m:
                break

            row1 = block[i - j]
            row2 = block[i + j + 1]

            total_diff += diff(row1, row2)

            if total_diff > 1:
                break

            j += 1

        if total_diff == 1:
            break

    if total_diff == 1:
        rows.append(100 * (i + 1))

    block = []

print(sum(cols) + sum(rows))

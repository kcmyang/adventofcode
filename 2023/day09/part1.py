import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()

total = 0

for line in lines:
    nums = [int(x) for x in line.split(' ')]

    rows = [nums]

    while not all(x == 0 for x in rows[-1]):
        row = rows[-1]
        rows.append([row[i] - row[i - 1] for i in range(1, len(row))])

    for r in range(len(rows) - 1, 0, -1):
        rows[r - 1].append(rows[r - 1][-1] + rows[r][-1])

    total += rows[0][-1]

print(total)

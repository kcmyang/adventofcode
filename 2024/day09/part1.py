import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


s = list(map(int, get_lines()[0]))
n = len(s)

# build disk - use -1 for a blank
disk = [0] * s[0]
k = 0

for i in range(1, n, 2):
    k += 1
    disk.extend([-1] * s[i])
    disk.extend([k] * s[i + 1])

# swap blocks to compact disk
j = len(disk) - 1
i = disk.index(-1)

while i < j:
    disk[i] = disk[j]
    j -= 1

    while disk[j] == -1:
        j -= 1

    i = disk.index(-1, i + 1)

disk = disk[:j + 1]

check = sum(i * x for (i, x) in enumerate(disk))
print(check)

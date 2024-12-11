import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


line = list(map(int, get_lines()[0].split()))

for _ in range(25):
    new = []

    for s in line:
        if s == 0:
            new.append(1)
        elif len(str(s)) % 2 == 0:
            st = str(s)
            new.append(int(st[:len(st) // 2]))
            new.append(int(st[len(st) // 2:]))
        else:
            new.append(s * 2024)

    line = new

print(len(line))

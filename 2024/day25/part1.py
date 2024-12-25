import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
g = []

for i in range(0, len(lines), 8):
    k = lines[i:i + 7]
    g.append(k)

count = 0
for i, k1 in enumerate(g):
    for k2 in g[:i]:
        if all(all('.#'.index(k1[r][c]) + '.#'.index(k2[r][c]) <= 1 for r in range(7)) for c in range(5)):
            count += 1

print(count)

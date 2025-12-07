import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

splits = 0
beams = set([lines[0].index('S')])

for line in lines[1:]:
    new = set()
    for b in beams:
        if line[b] == '.':
            new.add(b)
        else:
            new.add(b - 1)
            new.add(b + 1)
            splits += 1
    beams = new

print(splits)

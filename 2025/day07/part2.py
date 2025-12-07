import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

beams = {lines[0].index('S'): 1}  # pairs (index, multiplicity)

for line in lines[1:]:
    new = dict()
    for b, m in beams.items():
        if line[b] == '.':
            new.setdefault(b, 0)
            new[b] += m
        else:
            new.setdefault(b - 1, 0)
            new.setdefault(b + 1, 0)
            new[b - 1] += m
            new[b + 1] += m
    beams = new

print(sum(beams.values()))

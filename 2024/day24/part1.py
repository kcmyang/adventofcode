import sys
from collections import defaultdict


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
i = lines.index('')
regs = dict()

for l in lines[:i]:
    r, v = l.split(': ')
    regs[r] = int(v)

gates = defaultdict(list)  # r_1 -> op, r_2, r_out

for l in lines[i + 1:]:
    r1, op, r2, _, r3 = l.split()
    gates[r1].append((op, r2, r3))
    gates[r2].append((op, r1, r3))

level = set(regs.keys())
seen = set(level)

while level:
    new = set()

    for r1 in level:
        for op, r2, r3 in gates[r1]:
            if r1 in regs and r2 in regs:
                if r3 not in seen:
                    match op:
                        case 'OR':
                            regs[r3] = regs[r1] | regs[r2]
                        case 'AND':
                            regs[r3] = regs[r1] & regs[r2]
                        case 'XOR':
                            regs[r3] = regs[r1] ^ regs[r2]
                    seen.add(r3)
                    new.add(r3)
            else:
                new.add(r2)

    level = new

x = [v for _, v in sorted(((int(k[1:]), v) for k, v in regs.items() if k[0] == 'z'), reverse=True)]
bits = ''.join(map(str, x))
print(bits)
print(int(bits, 2))

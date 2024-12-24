import itertools
import sys
from collections import defaultdict


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()
index = lines.index('')

gates = defaultdict(list)  # r_1 -> op, r_2, r_out

# sort gates for easier searching
with open('input-transform.txt', 'w') as f:
    buf = []
    for l in lines[index + 1:]:
        r1, op, r2, _, r3 = l.split()
        gates[r1].append([op, r2, r3])
        gates[r2].append([op, r1, r3])
        buf.append(f'{min(r1,r2)} {op} {max(r1,r2)} -> {r3}\n')
    buf.sort()
    f.writelines(buf)


def get_val(regs, ch):
    return int(''.join(map(str, (v for _, v in sorted(((k, v) for k, v in regs.items() if k[0] == ch), reverse=True)))),
               2)


def display(x, y, z):
    print(f'x       {x:>20} {x:>47b}')
    print(f'y       {y:>20} {y:>47b}')
    print(f'x+y     {x+y:>20} {x+y:>47b}')
    print(f'z       {z:>20} {z:>47b}')
    print(f'(x+y)^z {(x+y)^z:>20} {(x+y)^z:>47b}')
    wrong = [i for i in range(45) if ((x + y) ^ z) & (1 << i) != 0]
    print('wrong bits', wrong)


def compute(regs, x, y):
    regs = dict()
    for i in range(45):
        regs[f'x{i:02}'] = int((1 << i) & x > 0)
        regs[f'y{i:02}'] = int((1 << i) & y > 0)

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

    assert get_val(regs, 'x') == x
    assert get_val(regs, 'y') == y
    z = get_val(regs, 'z')
    display(x, y, z)
    return z


# Manual process.
# Initially this array is empty.
# The 45-iter loop below will stop at the first bit where the truth table of the circuit does not
# match the expected logic.
# At this point from the debug output we know roughly where to look in the gates to find the bug.
# After fixing it, the loop should proceed for quite a few more iters; otherwise the bug is likely
# not correctly fixed.
# Not much to say about the intuition except that the input models a full adder, so we know
# what to expect in the logic.
# Drawing some pictures is helpful here.
swaps = [
    ('svm', 'nbc'),  # 05
    ('z15', 'kqk'),  # 14
    ('z23', 'cgq'),  # 22
    ('z39', 'fnr'),  # 38
]
for r1, vs in gates.items():
    for i, (_, r2, r3) in enumerate(vs):
        for s in swaps:
            if r3 == s[0]:
                gates[r1][i][2] = s[1]
            elif r3 == s[1]:
                gates[r1][i][2] = s[0]

for i in range(45):
    print(f'=== try i = {i} ===')
    regs = dict()
    bit = (1 << i)
    vecs = [(0, 0), (bit, 0), (0, bit), (bit, bit)]
    expected = [x + y for x, y in vecs]
    table = [compute(regs, x, y) for x, y in vecs]
    res = [a == b for a, b in zip(expected, table)]
    if not all(res):
        print('fix', i)
        break

print(','.join(sorted(list(itertools.chain.from_iterable(swaps)))))

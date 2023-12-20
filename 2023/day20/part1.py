import itertools
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

# (on/off, outputs)
ffs = dict()
# (input memory, outputs)
cons = dict()
broadcast = []

for line in lines:
    [k, rest] = line.split(' -> ')
    rest = rest.split(', ')

    if k == 'broadcaster':
        broadcast = rest
    elif k[0] == '%':
        ffs[k[1:]] = [False, rest]
    else:
        cons[k[1:]] = [dict(), rest]

for ff, [_, outputs] in ffs.items():
    for output in outputs:
        if output in cons:
            cons[output][0][ff] = False

TIMES = 1000
low_count = 0
high_count = 0

for _ in range(TIMES):
    # button
    low_count += 1

    # queue of (input, output, pulse)
    q = list(zip(itertools.repeat(''), broadcast, itertools.repeat(False)))
    # broadcast
    low_count += len(broadcast)

    while q:
        q_next = []

        for i, x, pulse in q:
            if x in ffs and not pulse:
                p = ffs[x][0] = not ffs[x][0]
                if p:
                    high_count += len(ffs[x][1])
                else:
                    low_count += len(ffs[x][1])
                q_next.extend(zip(itertools.repeat(x), ffs[x][1], itertools.repeat(p)))
            elif x in cons:
                cons[x][0][i] = pulse
                # send low pulse if all inputs are high, otherwise send high pulse
                p = not all(cons[x][0].values())
                if p:
                    high_count += len(cons[x][1])
                else:
                    low_count += len(cons[x][1])
                q_next.extend(zip(itertools.repeat(x), cons[x][1], itertools.repeat(p)))

        q = q_next

print(low_count * high_count)

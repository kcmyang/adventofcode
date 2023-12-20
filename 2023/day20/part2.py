import itertools
import math
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

# Need a bit of hard coding for this one.
# The target module is fed by exactly one input, a conjuction module.
# The assumption is that each input to this conjuction module will send a high pulse cyclically,
# so we want to find the cycle length for each such input.
# The final answer is the LCM of all these lengths.
# (No, I don't know why the math for this works, but the answer worked!)
TARGET = 'rx'
TARGET_CON = 'kz'
TARGET_INPUTS = ['sj', 'qq', 'ls', 'bg']
cycle_lengths = dict.fromkeys(TARGET_INPUTS, 0)
count = 0
found = False

while not found:
    count += 1

    # queue of (input, output, pulse)
    q = list(zip(itertools.repeat(''), broadcast, itertools.repeat(False)))

    while q and not found:
        q_next = []

        for i, x, pulse in q:
            if i in TARGET_INPUTS and x == TARGET_CON and pulse:
                if cycle_lengths[i] == 0:
                    cycle_lengths[i] = count
                if all(v > 0 for v in cycle_lengths.values()):
                    found = True
            if x in ffs and not pulse:
                p = ffs[x][0] = not ffs[x][0]
                q_next.extend(zip(itertools.repeat(x), ffs[x][1], itertools.repeat(p)))
            elif x in cons:
                cons[x][0][i] = pulse
                # send low pulse if all inputs are high, otherwise send high pulse
                p = not all(cons[x][0].values())
                q_next.extend(zip(itertools.repeat(x), cons[x][1], itertools.repeat(p)))

        q = q_next

print(math.lcm(*cycle_lengths.values()))

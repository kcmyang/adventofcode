import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


KEY = 811589153
ITERATIONS = 10

lines = get_lines()
decrypted = [int(n) * KEY for n in lines]
N = len(decrypted)
# each element is a tuple of (original index in decrypted, number)
mixed = list(enumerate(decrypted))
# pointers[p] = index of p-th element of decrypted in mixed
# (so mixed[pointers[p]][1] == decrypted[p])
pointers = list(range(N))

for _ in range(ITERATIONS):
    for p, n in enumerate(decrypted):
        shift = 1 if n >= 0 else -1
        i = pointers[p]
        # assert n * KEY == mixed[i][1]

        for _ in range(abs(n) % (N - 1)):
            j = (i + shift) % N
            mixed[i], mixed[j] = mixed[j], mixed[i]
            pointers[mixed[i][0]] = i
            pointers[mixed[j][0]] = j
            i = j

z = pointers[decrypted.index(0)]
coords = [mixed[(z + k) % N][1] for k in [1000, 2000, 3000]]
print(sum(coords))

import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
encrypted = [int(n) for n in lines]
N = len(encrypted)
# each element is a tuple of (original index in encrypted, number)
mixed = list(enumerate(encrypted))
# pointers[p] = index of p-th element of encrypted in mixed
# (so mixed[pointers[p]][1] == encrypted[p])
pointers = list(range(N))

for p, n in enumerate(encrypted):
    shift = 1 if n >= 0 else -1
    i = pointers[p]
    # assert n == mixed[i][1]

    for _ in range(abs(n) % (N - 1)):
        j = (i + shift) % N
        mixed[i], mixed[j] = mixed[j], mixed[i]
        pointers[mixed[i][0]] = i
        pointers[mixed[j][0]] = j
        i = j

z = pointers[encrypted.index(0)]
coords = [mixed[(z + k) % N][1] for k in [1000, 2000, 3000]]
print(sum(coords))

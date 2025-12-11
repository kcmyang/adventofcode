from itertools import zip_longest as z
from functools import reduce as r
from operator import add as f, mul as g

print(r(lambda a, c: (a[0] + a[1], 0, a[2]) if c.isspace() else (a[0], int(c[:-1]), (f, g)['+*'.index(c[-1])]) if not c[-1].isspace() else (a[0], a[2](a[1], int(c[:-1])), a[2]), list(''.join(x) for x in z(*(open('t').readlines()), fillvalue=' ')), (0, 0, None))[0])

import sys

import z3


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

stones = []

for line in lines:
    [p, v] = line.split(' @ ')
    stones.append((eval(f'({p})'), eval(f'({v})')))

# For each stone i, we need to find some ki such that pi + ki * vi = p0 + ki * v0.
s = z3.Solver()

x = z3.Int('x')
y = z3.Int('y')
z = z3.Int('z')
vx = z3.Int('vx')
vy = z3.Int('vy')
vz = z3.Int('vz')

for i, (p, v) in enumerate(stones):
    pxi = z3.Int(f'px{i}')
    pyi = z3.Int(f'py{i}')
    pzi = z3.Int(f'pz{i}')
    vxi = z3.Int(f'vx{i}')
    vyi = z3.Int(f'vy{i}')
    vzi = z3.Int(f'vz{i}')
    ki = z3.Int(f'k{i}')

    s.add(pxi == p[0])
    s.add(pyi == p[1])
    s.add(pzi == p[2])
    s.add(vxi == v[0])
    s.add(vyi == v[1])
    s.add(vzi == v[2])
    s.add(ki > 0)

    s.add(pxi + ki * vxi == x + ki * vx)
    s.add(pyi + ki * vyi == y + ki * vy)
    s.add(pzi + ki * vzi == z + ki * vz)

s.check()
m = s.model()
print(m[x], m[y], m[z])
print(m[x].as_long() + m[y].as_long() + m[z].as_long())

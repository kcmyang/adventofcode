import sys
from collections import deque
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

# name -> value
constants = dict()
# name -> [arg1, operation, arg2]
operations = dict()

for line in lines:
    [name, rest] = line.split(': ')
    rest = rest.split()

    if len(rest) == 1:
        constants[name] = int(rest[0])
    elif len(rest) == 3:
        operations[name] = rest

queue = deque(operations.keys())

while queue:
    name = queue.popleft()
    [arg1, op, arg2] = operations[name]

    c1 = constants.get(arg1)
    c2 = constants.get(arg2)

    if c1 is not None and c2 is not None:
        if op == '/':
            op = '//'
        value = eval(f'{c1} {op} {c2}')
        constants[name] = value
    else:
        queue.append(name)

print(constants['root'])

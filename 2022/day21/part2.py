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

# The monkeys form a tree where each monkey yells to its children.

# a node has either no children, or two children
# name -> pair of names
children = dict()
# a node has at most one parent
# name -> name
parent = dict()

for line in lines:
    [name, rest] = line.split(': ')
    rest = rest.split()

    if len(rest) == 1:
        constants[name] = int(rest[0])
    elif len(rest) == 3:
        operations[name] = rest
        children[name] = (rest[0], rest[2])
        parent[rest[0]] = name
        parent[rest[2]] = name


def get_path(start: str, end: str) -> List[str]:
    '''
    DFS. Returns an empty list if there is no such path.
    '''
    path = []

    def dfs(curr):
        path.append(curr)

        if path[-1] == end:
            return True

        if curr not in children:
            path.pop()
            return False

        c1, c2 = children[curr]
        if dfs(c1) or dfs(c2):
            return True

        path.pop()
        return False

    dfs(start)

    return path


# Get the two subtrees of root
human_root, monkey_root = children['root']
path = get_path(human_root, 'humn')
if not path:
    human_root, monkey_root = monkey_root, human_root
    path = get_path(human_root, 'humn')
assert path


def get_tree(root: str) -> List[str]:
    tree = [root]
    queue = deque()
    queue.append(root)

    while queue:
        curr = queue.popleft()

        if curr not in children:
            continue

        c1, c2 = children[curr]
        tree.append(c1)
        tree.append(c2)
        queue.append(c1)
        queue.append(c2)

    return tree


def collapse(root: str) -> int:
    tree = get_tree(root)

    queue = deque()
    for node in tree:
        if node in operations:
            queue.append(node)

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

    return constants[root]


# Collapse the monkey subtree
monkey_value = collapse(monkey_root)

# Go down the human subtree and apply the inverse of each operation
human_value = monkey_value
curr = human_root

for depth, node in enumerate(path[:-1]):
    p = path[depth + 1]
    c1, c2 = children[node]
    value = collapse(c2 if c1 == p else c1)
    op = operations[node][1]

    if op == '+':
        # x + k1 = k2 => x = k2 - k1
        human_value -= value
    elif op == '*':
        # x * k1 = k2 => x = k2 / k1
        human_value //= value
    elif op == '-':
        if c1 == p:
            # x - k1 = k2 => x = k2 + k1
            human_value += value
        else:
            # k1 - x = k2 => x = k1 - k2
            human_value = value - human_value
    elif op == '/':
        if c1 == p:
            # x / k1 = k2 => x = k2 * k1
            human_value *= value
        else:
            # k1 / x = k2 => x = k1 / k2
            human_value = value // human_value

constants['humn'] = human_value
collapse(human_root)
assert constants[human_root] == constants[monkey_root]

print(human_value)

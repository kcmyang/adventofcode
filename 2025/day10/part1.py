import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


def apply_button(state: tuple[int], button: list[int]) -> tuple[int]:
    out = list(state)
    for i in button:
        out[i] = 1 - out[i]
    return tuple(out)


def solve(goal: tuple[int], buttons: list[list[int]]) -> int:
    start = tuple([0] * len(goal))
    cost = 0
    queue = [start]
    seen = set(queue)
    while queue:
        new = []
        for state in queue:
            if state == goal:
                return cost
            for button in buttons:
                state2 = apply_button(state, button)
                if state2 not in seen:
                    seen.add(state2)
                    new.append(state2)
        queue = new
        cost += 1
    raise Exception('no solution!')


def main():
    lines = get_lines()
    nums = []
    for line in lines:
        parts = line.split()
        goal = tuple(int(x == '#') for x in parts[0].strip('[]'))
        buttons = [list(map(int, part.strip('()').split(','))) for part in parts[1:-1]]
        presses = solve(goal, buttons)
        nums.append(presses)
    print(sum(nums))


main()

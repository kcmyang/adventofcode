import sys
import scipy.optimize
import numpy as np


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


def main():
    lines = get_lines()
    nums = []
    for line in lines:
        parts = line.split()
        buttons = [list(map(int, part.strip('()').split(','))) for part in parts[1:-1]]
        goal = list(map(int, parts[-1].strip('{}').split(',')))
        n = len(goal)

        # solve LP
        b = goal
        A = np.transpose([[1 if i in button else 0 for i in range(n)] for button in buttons])
        c = np.ones_like(A[0])
        res = scipy.optimize.linprog(c, A_eq=A, b_eq=b, integrality=1)
        sol = list(map(round, res.x))
        nums.append(sum(sol))
    print(sum(nums))


main()

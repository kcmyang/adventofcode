import re
import sys
from ortools.linear_solver import pywraplp
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


def get_blueprint(line) -> List[int]:
    pattern = re.compile(r'\d+')
    nums = pattern.findall(line)
    return [int(n) for n in nums]


def geodes(blueprint):
    '''
    Shorthand for variables:
      e -> ore
      c -> clay
      b -> obsidian
      g -> geode
      zr -> robot for resource z
      xz -> decision for building robot for resource z
    '''
    [index, E_e, E_c, E_b, C_b, E_g, B_g] = blueprint

    solver = pywraplp.Solver.CreateSolver('SCIP')
    if not solver:
        sys.exit(1)

    t_max = 32
    infinity = solver.infinity()

    # variables
    vars = dict()

    # resource count at end of minute t
    vars['e'] = [solver.IntVar(0.0, infinity, f'e_{i}') for i in range(t_max)]
    vars['c'] = [solver.IntVar(0.0, infinity, f'c_{i}') for i in range(t_max)]
    vars['b'] = [solver.IntVar(0.0, infinity, f'b_{i}') for i in range(t_max)]
    vars['g'] = [
        solver.IntVar(0.0, infinity, f'g_{i}') for i in range(t_max + 1)
    ]
    # robot count at end of minute t
    vars['er'] = [
        solver.IntVar(0.0, infinity, f'er_{i}') for i in range(t_max)
    ]
    vars['cr'] = [
        solver.IntVar(0.0, infinity, f'cr_{i}') for i in range(t_max)
    ]
    vars['br'] = [
        solver.IntVar(0.0, infinity, f'br_{i}') for i in range(t_max)
    ]
    vars['gr'] = [
        solver.IntVar(0.0, infinity, f'gr_{i}') for i in range(t_max)
    ]
    # decision to build robot at start of minute t
    vars['xe'] = [None] + [
        solver.IntVar(0.0, 1.0, f'xe_{i}') for i in range(1, t_max)
    ]
    vars['xc'] = [None] + [
        solver.IntVar(0.0, 1.0, f'xc_{i}') for i in range(1, t_max)
    ]
    vars['xb'] = [None] + [
        solver.IntVar(0.0, 1.0, f'xb_{i}') for i in range(1, t_max)
    ]
    vars['xg'] = [None] + [
        solver.IntVar(0.0, 1.0, f'xg_{i}') for i in range(1, t_max)
    ]

    # constraints

    # starting conditions
    solver.Add(vars['er'][0] == 1)
    for name in ['e', 'c', 'b', 'g', 'cr', 'br', 'gr']:
        solver.Add(vars[name][0] == 0)

    # resource counts
    for t in range(1, t_max):
        solver.Add(vars['e'][t] == vars['e'][t - 1] + vars['er'][t - 1] -
                   E_e * vars['xe'][t] - E_c * vars['xc'][t] -
                   E_b * vars['xb'][t] - E_g * vars['xg'][t])
        solver.Add(vars['c'][t] == vars['c'][t - 1] + vars['cr'][t - 1] -
                   C_b * vars['xb'][t])
        solver.Add(vars['b'][t] == vars['b'][t - 1] + vars['br'][t - 1] -
                   B_g * vars['xg'][t])
        solver.Add(vars['g'][t] == vars['g'][t - 1] + vars['gr'][t - 1])

        solver.Add(
            vars['e'][t - 1] >= E_e * vars['xe'][t] + E_c * vars['xc'][t] +
            E_b * vars['xb'][t] + E_g * vars['xg'][t])
        solver.Add(vars['c'][t - 1] >= C_b * vars['xb'][t])
        solver.Add(vars['b'][t - 1] >= B_g * vars['xg'][t])

    solver.Add(vars['g'][t_max] == vars['g'][t_max - 1] +
               vars['gr'][t_max - 1])

    # robot counts
    for t in range(1, t_max):
        solver.Add(vars['er'][t] == vars['er'][t - 1] + vars['xe'][t])
        solver.Add(vars['cr'][t] == vars['cr'][t - 1] + vars['xc'][t])
        solver.Add(vars['br'][t] == vars['br'][t - 1] + vars['xb'][t])
        solver.Add(vars['gr'][t] == vars['gr'][t - 1] + vars['xg'][t])

    # decisions
    for t in range(1, t_max):
        solver.Add(
            vars['xe'][t] + vars['xc'][t] + vars['xb'][t] + vars['xg'][t] <= 1)

    # objective
    solver.Maximize(vars['g'][t_max])

    # solve
    status = solver.Solve()

    if status != pywraplp.Solver.OPTIMAL:
        print("No solution!")
        sys.exit(1)

    print(f'Blueprint {index}')

    print('e  = ' + ' '.join(f"{round(vars['e'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('c  = ' + ' '.join(f"{round(vars['c'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('b  = ' + ' '.join(f"{round(vars['b'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('g  = ' + ' '.join(f"{round(vars['g'][t].solution_value()):3}"
                             for t in range(t_max + 1)))
    print('er = ' + ' '.join(f"{round(vars['er'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('cr = ' + ' '.join(f"{round(vars['cr'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('br = ' + ' '.join(f"{round(vars['br'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('gr = ' + ' '.join(f"{round(vars['gr'][t].solution_value()):3}"
                             for t in range(t_max)))
    print('xe =     ' + ' '.join(f"{round(vars['xe'][t].solution_value()):3}"
                                 for t in range(1, t_max)))
    print('xc =     ' + ' '.join(f"{round(vars['xc'][t].solution_value()):3}"
                                 for t in range(1, t_max)))
    print('xb =     ' + ' '.join(f"{round(vars['xb'][t].solution_value()):3}"
                                 for t in range(1, t_max)))
    print('xg =     ' + ' '.join(f"{round(vars['xg'][t].solution_value()):3}"
                                 for t in range(1, t_max)))

    obj_val = round(solver.Objective().Value())
    print(f'Solution: g_{t_max} = {obj_val}')
    print()

    return obj_val


blueprints = [get_blueprint(line) for line in get_lines()][:3]

num_geodes = [geodes(bp) for bp in blueprints]
print(num_geodes)

result = 1
for n in num_geodes:
    result *= n
print(result)

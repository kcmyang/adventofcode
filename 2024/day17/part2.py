import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()


def run_prog(ra0, prog) -> list[int]:
    ra = ra0
    rb = 0
    rc = 0
    output = []

    def combo(op):
        nonlocal ra, rb, rc
        if op <= 3:
            return op
        if op == 4:
            return ra
        if op == 5:
            return rb
        if op == 6:
            return rc
        raise Exception()

    def run(ip, op, arg):
        nonlocal ra, rb, rc, output
        match op:
            case 0:
                ra = ra // 2**combo(arg)
                return ip + 2
            case 1:
                rb = rb ^ arg
                return ip + 2
            case 2:
                rb = combo(arg) % 8
                return ip + 2
            case 3:
                return arg if ra != 0 else ip + 2
            case 4:
                rb = rb ^ rc
                return ip + 2
            case 5:
                output.append(combo(arg) % 8)
                return ip + 2
            case 6:
                rb = ra // 2**combo(arg)
                return ip + 2
            case 7:
                rc = ra // 2**combo(arg)
                return ip + 2

    # print(ra, rb, rc, prog)
    ip = 0
    while ip < len(prog) - 1:
        ip = run(ip, prog[ip], prog[ip + 1])
        # if prog[:len(output)] != output:
        #     return False
        # print(ip, 'regs', ra, rb, rc, output)

    return output


prog = list(map(int, lines[-1].split()[-1].split(',')))

# trace the program by hand and discover:
# - the program loops until ra == 0
# - each loop ends with a print from rb
# - what ends up being printed is only dependent on the value of ra at the start of the iteration
# - ra is updated by shifts after each iteration
# - key observation: we can find the correct ra for each suffix of the program, then
#   continue on to the next suffix, until finally we cover the entire program
ra = 0

for i, x in enumerate(prog[::-1]):
    # find ra0 (value of ra at the start of the loop).
    # note ra0 >> 3 == ra.
    ra0 = ra << 3
    # find a set of bottom bits that gives the right output
    while True:
        output = run_prog(ra0, prog)
        if prog[-(i + 1):] == output:
            break
        ra0 += 1

    ra = ra0

    # note rb and rc are overwritten on every loop, so we don't need to compute
    # the intermediate values.

# sanity check
print('ra =', ra)
print('works?', run_prog(ra, prog) == prog)

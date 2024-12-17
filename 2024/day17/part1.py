import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()

ra = 0
rb = 0
rc = 0
output = []


def combo(op):
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
    global ra, rb, rc, output

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


for line in lines:
    if 'Register A' in line:
        ra = int(line.split()[-1])
    elif 'Register B' in line:
        rb = int(line.split()[-1])
    elif 'Register C' in line:
        rc = int(line.split()[-1])
    elif 'Program' in line:
        prog = list(map(int, line.split()[-1].split(',')))
        ip = 0
        while ip < len(prog) - 1:
            ip = run(ip, prog[ip], prog[ip + 1])

print(','.join(map(str, output)))

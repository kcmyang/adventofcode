import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
words = lines[0].split(',')

def hash(s):
    val = 0

    for ch in s:
        val += ord(ch)
        val *= 17
        val %= 256

    return val

print(sum(map(hash, words)))

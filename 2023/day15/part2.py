import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in f]


lines = get_lines()
words = lines[0].split(',')

boxes = [list() for _ in range(256)]

def hash(s):
    val = 0

    for ch in s:
        val += ord(ch)
        val *= 17
        val %= 256

    return val

for word in words:
    if word[-1] == '-':
        l = word[:-1]
        k = hash(l)

        for i, (key, _) in enumerate(boxes[k]):
            if key == l:
                boxes[k].pop(i)
                break
    else:
        [l, v] = word.split('=')
        k = hash(l)
        found = False

        for i, (key, _) in enumerate(boxes[k]):
            if key == l:
                boxes[k][i] = (l, int(v))
                found = True
                break

        if not found:
            boxes[k].append((l, int(v)))

total = 0

for i, box in enumerate(boxes):
    for j, (_, v) in enumerate(box):
        total += (i + 1) * (j + 1) * v

print(total)

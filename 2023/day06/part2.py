import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()
time = int(re.findall(r'\d+', re.sub(r' +', '', lines[0]))[0])
dist = int(re.findall(r'\d+', re.sub(r' +', '', lines[1]))[0])

count = 0

for t in range(1, time):
    d = (time - t) * t
    if d > dist:
        count += 1

print(count)

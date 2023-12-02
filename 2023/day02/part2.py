import sys
from collections import Counter


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

powers = []

for line in lines:
    _, s2 = line.split(': ')

    hands = s2.split('; ')
    bag = Counter()

    for hand in hands:
        counts = hand.split(', ')

        for count in counts:
            num, colour = count.split(' ')
            bag[colour] = max(bag[colour], int(num))

    colours = ['red', 'green', 'blue']
    power = 1

    for colour in colours:
        power *= bag[colour]

    powers.append(power)

total = sum(powers)
print(total)

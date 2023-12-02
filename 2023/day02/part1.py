import sys
from collections import Counter


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()

bank = Counter(red=12, green=13, blue=14)
ids = []

for line in lines:
    s1, s2 = line.split(': ')
    game_id = int(s1.removeprefix('Game '))

    hands = s2.split('; ')
    valid = True

    for hand in hands:
        counts = hand.split(', ')
        counter = Counter()

        for count in counts:
            num, colour = count.split(' ')
            counter[colour] += int(num)

        if not counter <= bank:
            valid = False
            break

    if valid:
        ids.append(game_id)

total = sum(ids)
print(total)

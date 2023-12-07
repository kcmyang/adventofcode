import collections
import re
import sys


def get_lines() -> list[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


lines = get_lines()


def get_rank(hand):
    jokers = 0
    counter = collections.Counter()

    for ch in hand:
        if ch == 'J':
            jokers += 1
        else:
            counter[ch] += 1

    if counter:
        counter[counter.most_common(1)[0][0]] += jokers
    else:
        counter['A'] = jokers

    counts = sorted(counter.values())

    rank = 0

    if counts == [5]:
        rank = 6
    elif counts == [1, 4]:
        rank = 5
    elif counts == [2, 3]:
        rank = 4
    elif counts == [1, 1, 3]:
        rank = 3
    elif counts == [1, 2, 2]:
        rank = 2
    elif counts == [1, 1, 1, 2]:
        rank = 1

    return rank


pairs = []
# later strings are more valuable
face_card_map = {'T': 'V', 'J': '0', 'Q': 'X', 'K': 'Y', 'A': 'Z'}

for line in lines:
    [hand, bid] = line.split(' ')
    rank = get_rank(hand)

    for k, v in face_card_map.items():
        hand = re.sub(k, v, hand)

    bid = int(bid)
    pairs.append((rank, hand, bid))

pairs.sort()

total = 0

for i in range(len(pairs)):
    _, _, bid = pairs[i]
    total += (i + 1) * bid

print(total)

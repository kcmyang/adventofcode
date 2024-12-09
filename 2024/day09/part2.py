import heapq
import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


s = list(map(int, get_lines()[0]))
s.append(0)  # use dummy value for easier implementation
n = len(s)

# build disk - use -1 for a blank
# elems are pairs [list of [file id, file length], gap size]
disk = [[[[0, s[0]]], s[1]]]
k = 0
# gaps[size] = minheap of indices of disk with a gap of this size
gaps = [list() for _ in range(10)]
gaps[s[1]].append(0)

for i in range(2, n, 2):
    k += 1
    disk.append([[[k, s[i]]], s[i + 1]])
    gaps[s[i + 1]].append(len(disk) - 1)

for i in range(10):
    heapq.heapify(gaps[i])

# try to move each file
for j in range(k, -1, -1):
    size = disk[j][0][0][1]

    heap_index = -1

    # clean up heaps and find any that fit this block.
    # if a gap fits, use the earliest one.
    for i in range(10):
        while gaps[i] and gaps[i][0] >= j:
            heapq.heappop(gaps[i])

        if gaps[i] and i >= size:
            if heap_index == -1:
                heap_index = i
            elif gaps[i][0] < gaps[heap_index][0]:
                heap_index = i

    if heap_index >= 0:
        pos = heapq.heappop(gaps[heap_index])

        # move block to new spot
        disk[pos][0].append(disk[j][0][0].copy())
        # blank out this file in the current element of disk
        # (note the element may have other files, so we need to keep this file in its position)
        disk[j][0][0][0] = -1

        # update gap
        disk[pos][1] -= size
        heapq.heappush(gaps[disk[pos][1]], pos)

# flatten the disk
flat = []

for parts, gap in disk:
    for f, size in parts:
        flat.extend([f] * size)
    flat.extend([-1] * gap)

# get real length of disk
m = len(flat)

while flat[m - 1] == -1:
    m -= 1

flat = flat[:m]

check = sum(i * x for (i, x) in enumerate(flat) if x != -1)
print(check)

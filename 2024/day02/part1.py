import sys


def get_lines() -> list[str]:
    filename = sys.argv[1] if len(sys.argv) > 1 else 'input.txt'
    with open(filename) as f:
        return [line.strip() for line in f]


lines = get_lines()


def test(nums):
    prev = nums[0]
    incr = nums[1] > nums[0]

    for num in nums[1:]:
        d = abs(num - prev)
        if not (1 <= d <= 3):
            return False

        if d == 0:
            return False

        if (num > prev) != incr:
            return False

        prev = num

    return True


count = 0

for line in lines:
    nums = list(map(int, line.split()))

    if test(nums):
        count += 1

print(count)

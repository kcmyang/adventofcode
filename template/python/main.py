import sys
from typing import List


def get_lines() -> List[str]:
    with open(sys.argv[1]) as f:
        return [line.strip() for line in list(f)]


def part1(lines):
    return 0


def part2(lines):
    return 0


def main() -> None:
    lines = get_lines()
    print(part1(lines))
    print(part2(lines))


if __name__ == '__main__':
    main()

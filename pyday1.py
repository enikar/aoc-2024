#! /usr/bin/python3

# a solution for AoC 2024, day 1

def read_datas():
    lefts = []
    rights = []
    with open('day1.txt') as datas:
        for line in datas:
            words = line.split()
            lefts.append(int(words[0]))
            rights.append(int(words[1]))

    lefts.sort()
    rights.sort()
    return [lefts, rights]


def part1(lefts, rights):
    s = 0
    for [l, r] in zip(lefts, rights):
        s += abs(l - r)
    return s


def part2(lefts, rights):
    s = 0
    for n in lefts:
        s += n * rights.count(n)
    return s


def show_solution(part, sol):
    print(f"{part}: {sol}")


def main():
    lefts, rights = read_datas()
    show_solution('Part1', part1(lefts, rights))
    show_solution('Part2', part2(lefts, rights))


main()

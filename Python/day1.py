"""Advent Of Code Day 1 in Python"""


def find_pair(puzzle_input):
    """Find the two numbers summing to 2020 and return their product."""
    for x in puzzle_input:
        if 2020 - x in puzzle_input:
            return x * (2020 - x)
    raise Exception("Invalid Input!")


def read_data(path):
    """Read the input data."""
    with open(path) as f:
        return [int(line) for line in f.read().split("\n") if line.strip()]


def find_triplet(puzzle_input):
    """Find the three numbers with sum 2020 and return their product."""
    for i in range(len(puzzle_input)):
        for j in range(len(puzzle_input)):
            if i != j and 2020 - puzzle_input[i] - puzzle_input[j] in puzzle_input:
                return (
                    puzzle_input[i]
                    * puzzle_input[j]
                    * (2020 - puzzle_input[i] - puzzle_input[j])
                )
    raise Exception("Invalid Input!")


if __name__ == "__main__":
    solve_part_1 = False
    example = [1721, 979, 366, 299, 675, 1456]

    input = read_data("day1.txt")
    if solve_part_1:
        assert find_pair(example) == 514579, "Your algorithm has an error."
        solution = find_pair(input)

    else:
        assert find_triplet(example) == 241861950, "Your algorithm has an error."
        solution = find_triplet(input)
    print(f"The solution is {solution}")

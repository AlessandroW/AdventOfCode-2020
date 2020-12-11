from typing import List


def is_valid_number(previous: List[int], number: int) -> bool:
    """A number is valid if it is the sum of two distinct previous numbers."""
    for previous_number in previous:
        if number - previous_number in previous and previous_number != number - previous_number:
            return True
    return False


def solve_part_one(puzzle_input, preamble=25):
    for i in range(preamble, len(puzzle_input)):
        if not is_valid_number(puzzle_input[i - preamble: i], puzzle_input[i]):
            return puzzle_input[i]
    raise ValueError


def check_number(puzzle_input, position, invalid_number):
    running_sum = puzzle_input[position]
    highest_number = puzzle_input[position]
    while running_sum < invalid_number:
        position += 1
        next_number = puzzle_input[position]
        highest_number = max(highest_number, next_number)
        running_sum += next_number

    if running_sum == invalid_number:
        return True, highest_number
    return False, highest_number


def solve_part_two(puzzle_input, preamble=25):
    invalid_number = solve_part_one(puzzle_input, preamble)
    for position, number in enumerate(puzzle_input):
        solves_puzzle, highest_number = check_number(puzzle_input, position, invalid_number)
        if solves_puzzle:
            return number + highest_number
    raise ValueError


with open("../day9.txt") as file:
    puzzle_input = [int(line) for line in file.read().split("\n") if line]

example_numbers = [20, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 23, 24, 25]
assert is_valid_number(example_numbers, 26)
assert is_valid_number(example_numbers, 49)
assert not is_valid_number(example_numbers, 100)
assert not is_valid_number(example_numbers, 50)
assert is_valid_number(example_numbers[1:] + [45], 26)
assert not is_valid_number(example_numbers[1:] + [45], 65)
assert is_valid_number(example_numbers[1:] + [45], 64)
assert is_valid_number(example_numbers[1:] + [45], 66)

example_xmas = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
assert solve_part_one(example_xmas, 5) == 127
assert solve_part_two(example_xmas, 5) == 62

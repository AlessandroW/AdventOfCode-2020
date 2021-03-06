#!/usr/bin/env python3


def parse_policy(policy):
    """Parse the policy into count and letter."""
    count_and_letter = policy.split(" ")
    counts = count_and_letter[0].split("-")
    min_count = int(counts[0])
    max_count = int(counts[1])
    return {"min": min_count, "max": max_count, "letter": count_and_letter[1]}


def parse_input(puzzle_input):
    """Parse the password and policy."""
    policy_and_passwords = [element.split(": ") for element in puzzle_input]
    policies = [parse_policy(row[0]) for row in policy_and_passwords]
    passwords = [row[1] for row in policy_and_passwords]
    return policies, passwords


def count_letter(target_letter, password):
    """Get the number of occurrences of the target_letter in the password."""
    return sum(1 for letter in password if letter == target_letter)


def verify_passwords_part_one(policies, passwords):
    """Count the passwords that comply with their policy."""
    correct = 0
    for policy, password in zip(policies, passwords):
        count = count_letter(policy["letter"], password)
        if policy["min"] <= count and policy["max"] >= count:
            correct += 1
    return correct


def verify_passwords_part_two(policies, passwords):
    """Count the passwords that comply with the policy of part two."""
    correct = 0
    for policy, password in zip(policies, passwords):
        position_1 = policy["min"] - 1
        position_2 = policy["max"] - 1
        letter = policy["letter"]

        if (password[position_1] == letter and password[position_2] != letter) or (
            password[position_1] != letter and password[position_2] == letter
        ):
            correct += 1
    return correct


def solve_part_one(puzzle_input):
    """Solve Part One."""
    policies, passwords = parse_input(puzzle_input)
    number_of_correct_passwords = verify_passwords_part_one(policies, passwords)
    return number_of_correct_passwords


def solve_part_two(puzzle_input):
    """Solve Part Two."""
    policies, passwords = parse_input(puzzle_input)
    number_of_correct_passwords = verify_passwords_part_two(policies, passwords)
    return number_of_correct_passwords


def read_input(filename):
    """Read the input file."""
    with open(filename) as f:
        return [row for row in f.read().split("\n") if row]


if __name__ == "__main__":
    example = ["1-3 a: abcde", "1-3 b: cdefg", "2-9 c: ccccccccc"]
    puzzle_input = read_input("day2.txt")
    do_solve_part_one = False

    if do_solve_part_one:
        assert (
            solve_part_one(example) == 2
        ), "There is an error in your algorithm for part one."

        solution_part_one = solve_part_one(puzzle_input)
        print(
            f"Of all passwords {solution_part_one} passwords are correct according to their policy."
        )

    else:
        assert (
            solve_part_two(example) == 1
        ), "There is an error in your algorithm for part two."

        solution_part_two = solve_part_two(puzzle_input)
        print(
            f"Of all passwords {solution_part_two} passwords are correct according to their policy."
        )

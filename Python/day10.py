def solve_part_one(puzzle_input):
    sorted_bag = sorted(puzzle_input)
    sorted_bag.append(sorted_bag[-1] + 3)

    current_joltage = 0
    one_jolt_differences = 0
    three_jolt_differences = 0
    for adapter in sorted_bag:
        current_difference = adapter - current_joltage
        if current_difference == 1:
            one_jolt_differences += 1
        elif current_difference == 3:
            three_jolt_differences += 1
        current_joltage = adapter
    return one_jolt_differences * three_jolt_differences


def prepare_bag(bag):
    sorted_bag = sorted(bag, reverse=True)
    sorted_bag.append(0)
    result = [sorted_bag[0] + 3]
    result.extend(sorted_bag)
    return result


def solve_part_two(puzzle_input):
    prepared_input = prepare_bag(puzzle_input)
    result = {}
    for adapter in prepared_input:
        if not result:
            result[adapter] = 1
        else:
            result[adapter] = sum(result[joltage] for joltage in range(adapter + 1, adapter + 4)
                                if joltage in result)
    return result[0]


with open("../day10.txt") as f:
    puzzle_input = [int(line) for line in f.read().split("\n") if line]


example_input = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
example_input_2 = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
assert solve_part_one(example_input) == 7 * 5
assert solve_part_one(example_input_2) == 22 * 10
assert solve_part_two(example_input) == 8
assert solve_part_two(example_input_2) == 19208


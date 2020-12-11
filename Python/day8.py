def parse_instruction(instruction):
    parts = instruction.split(" ")
    operation = parts[0]
    argument = int(parts[1])
    return operation, argument


def parse_instructions(puzzle_input):
    return [parse_instruction(instruction) for instruction in puzzle_input]


def run_program(instructions):
    accumulator = 0
    visited_instructions = []
    instruction_index = 0
    while instruction_index not in visited_instructions:
        visited_instructions.append(instruction_index)
        operation, argument = instructions[instruction_index]
        if operation == "acc":
            accumulator += argument
            instruction_index += 1
        elif operation == "jmp":
            instruction_index += argument
        elif operation == "nop":
            instruction_index += 1
        else:
            raise ValueError("Invalid Operation", operation)
    return accumulator


def solve_part_one(puzzle_input):
    instructions = parse_instructions(puzzle_input)
    accumulator = run_program(instructions)
    return accumulator


def find_nop_jmp_positions(instructions):
    return [
        index
        for index, content in enumerate(instructions)
        if content[0] == "jmp" or content[0] == "nop"
    ]


def run_modified_program(instructions, position):
    accumulator = 0
    visited_instructions = []
    instruction_index = 0
    while instruction_index not in visited_instructions and instruction_index < len(
        instructions
    ):
        visited_instructions.append(instruction_index)
        operation, argument = instructions[instruction_index]
        if operation == "acc":
            accumulator += argument
            instruction_index += 1
        elif operation == "jmp":
            if instruction_index == position:
                instruction_index += 1
            else:
                instruction_index += argument
        elif operation == "nop":
            if instruction_index == position:
                instruction_index += argument
            else:
                instruction_index += 1
        else:
            raise ValueError("Invalid Operation", operation)

    # The last processed instruction is the last instruction in the file.
    if visited_instructions[-1] == len(instructions) - 1:
        return True, accumulator
    return False, accumulator


def solve_part_two(puzzle_input):
    instructions = parse_instructions(puzzle_input)
    nop_jmp_positions = find_nop_jmp_positions(instructions)
    for position in nop_jmp_positions:
        terminated, accumulator = run_modified_program(instructions, position)
        if terminated:
            return accumulator
    raise ValueError("Invalid Program")


with open("../day8.txt") as file:
    puzzle_input = [line for line in file.read().split("\n") if line]


example_input = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"""
example_lines = example_input.split("\n")
assert solve_part_one(example_lines) == 5
assert solve_part_two(example_lines) == 8

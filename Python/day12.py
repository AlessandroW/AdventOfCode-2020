example_input = """F10
N3
F7
R90
F11""".split("\n")


def parse_instruction(line):
    return (line[0], int(line[1:]))


def parse_instructions(puzzle_input):
    return [parse_instruction(line) for line in puzzle_input]


def change_direction(direction, value):
    directions = "NWSE"
    change_in_direction = (value // 90) % 4
    current_position = directions.index(direction)
    if current_position + change_in_direction < len(directions):
        return directions[current_position + change_in_direction]
    else:
        return directions[current_position + change_in_direction - len(directions)]


def calculate_manhattan_distance(position):
    return sum(abs(value) for value in position.values())


def execture_instruction(instruction, direction, position):
    action, value = instruction
    if action == "F":
        action = direction
    if action == "N":
        position["N"] = position["N"] + value
    elif action == "S":
        position["N"] = position["N"] - value
    elif action == "E":
        position["E"] = position["E"] + value
    elif action == "W":
        position["E"] = position["E"] - value
    elif action == "L":
        direction = change_direction(direction, value)
    elif action == "R":
        direction = change_direction(direction, - value)
    return direction, position


def solve_part_one(puzzle_input):
    parsed_instructions = parse_instructions(puzzle_input)
    position = {"E": 0, "N": 0}
    direction = "E"
    for instruction in parsed_instructions:
        direction, position = execture_instruction(instruction, direction, position)
    return calculate_manhattan_distance(position)


def move_waypoint(instruction, waypoint):
    action, value = instruction
    opposite = {
        "N": "S",
        "E": "W",
        "S": "N",
        "W": "E"
    }
    if waypoint[0][0] == action:
        waypoint[0][1] += value
    elif waypoint[1][0] == action:
        waypoint[1][1] += value
    elif opposite[waypoint[0][0]] == action:
        waypoint[0][1] -= value
    elif opposite[waypoint[1][0]] == action:
        waypoint[1][1] -= value
    elif action == "L":
        waypoint[0][0] = change_direction(waypoint[0][0], value)
        waypoint[1][0] = change_direction(waypoint[1][0], value)
    elif action == "R":
        waypoint[0][0] = change_direction(waypoint[0][0], - value)
        waypoint[1][0] = change_direction(waypoint[1][0], - value)
    else:
        raise Exception("Wrong action!", instruction, waypoint)
    return waypoint


def move_ship(instruction, waypoint, position):
    action, value = instruction
    opposite = {
        "N": "S",
        "E": "W",
        "S": "N",
        "W": "E"
    }
    if waypoint[0][0] in position:
        position[waypoint[0][0]] = position[waypoint[0][0]] + value * waypoint[0][1]
    else:
        position[opposite[waypoint[0][0]]] = position[opposite[waypoint[0][0]]] - value * waypoint[0][1]

    if waypoint[1][0] in position:
        position[waypoint[1][0]] = position[waypoint[1][0]] + value * waypoint[1][1]
    else:
       position[opposite[waypoint[1][0]]] = position[opposite[waypoint[1][0]]] - value * waypoint[1][1]
    return position


def solve_part_two(puzzle_input):
    parsed_instructions = parse_instructions(puzzle_input)
    position = {"E": 0, "N": 0}
    waypoint = [["E", 10], ["N", 1]]
    for instruction in parsed_instructions:
        if instruction[0] == "F":
            position = move_ship(instruction, waypoint, position)
        else:
            waypoint = move_waypoint(instruction, waypoint)
    return calculate_manhattan_distance(position)


def read_input(filename):
    """Read the input file."""
    with open(filename) as f:
        return [row for row in f.read().split("\n") if row]

assert change_direction("N", 90) == "W"
assert change_direction("N", 180) == "S"
assert change_direction("N", 270) == "E"
assert change_direction("N", -90) == "E"
assert change_direction("N", 360) == "N"
assert change_direction("N", 450) == "W"

assert solve_part_one(example_input) == 25
assert solve_part_two(example_input) == 286

puzzle_input = read_input("../day12.txt")

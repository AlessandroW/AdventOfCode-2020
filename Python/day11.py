example_input = """L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL""".split("\n")


def get_adjacent_positions(row, column, depth=1):
    upper_left = (row - depth, column - depth)
    upper_middle = (row - depth, column)
    upper_right = (row - depth, column + depth)
    left = (row, column - depth)
    right = (row, column + depth)
    lower_left = (row + depth, column - depth)
    lower_middle = (row + depth, column)
    lower_right = (row + depth, column + depth)
    positions = {"upper_left": upper_left,
                 "upper_middle": upper_middle,
                 "upper_right": upper_right,
                 "left": left,
                 "right": right,
                 "lower_left": lower_left,
                 "lower_middle": lower_middle,
                 "lower_right": lower_right}

    return positions


def is_valid_position(position, puzzle_input):
    row, column = position
    return row > -1 and column > -1 and row < len(puzzle_input) and column < len(puzzle_input[0])


def get_adjacent_seats(puzzle_input, row, column, extended=False):
    positions = get_adjacent_positions(row, column)
    if not extended:
        valid_positions = filter(lambda position: is_valid_position(position, puzzle_input), positions.values())
        return [puzzle_input[position[0]][position[1]] for position in valid_positions]
    final_positions = {}
    depth = 1
    while len(final_positions) < 8:
        for position, coordinates in positions.items():
            if position not in final_positions:
                if not is_valid_position(coordinates, puzzle_input):
                    final_positions[position] = ""
                elif puzzle_input[coordinates[0]][coordinates[1]] != ".":
                    final_positions[position] = puzzle_input[coordinates[0]][coordinates[1]]

        depth += 1
        positions = get_adjacent_positions(row, column, depth)
    return list(final_positions.values())


def sit(seating_plan, extended=False):
    new_seating_plan = []
    for row in range(len(seating_plan)):
        new_seating_plan.append([])
        for column in range(len(seating_plan[0])):
            adjacent_seats = get_adjacent_seats(seating_plan, row, column, extended)
            if seating_plan[row][column] == "L" and "#" not in adjacent_seats:
                new_seating_plan[row].append("#")
            elif not extended and seating_plan[row][column] == "#" and sum(1 for seat in adjacent_seats if seat == "#") > 3:
                new_seating_plan[row].append("L")
            elif extended and seating_plan[row][column] == "#" and sum(1 for seat in adjacent_seats if seat == "#") > 4:
                new_seating_plan[row].append("L")
            else:
                new_seating_plan[row].append(seating_plan[row][column])
    return new_seating_plan

def solve_part_one(seating_plan, extended=False):
    changing = True
    while changing:
        new_seating_plan = sit(seating_plan, extended)
        if seating_plan == new_seating_plan:
            changing = False
        seating_plan = new_seating_plan
    return sum(1 for row in range(len(seating_plan)) for column in range(len(seating_plan[0])) if seating_plan[row][column] == "#")

def solve_part_two(seating_plan):
    return solve_part_one(seating_plan, True)



with open("../day11.txt") as f:
    puzzle_input = [line for line in f.read().split("\n") if line]

assert solve_part_one(example_input) == 37
assert solve_part_two(example_input) == 26

#!/usr/bin/env python3
example_input = """light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."""

example_input_2 = """shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."""

example_sentences = example_input.split("\n")
example_sentences_2 = example_input_2.split("\n")
target_bag = "shiny gold bag"

with open("../day7.txt") as f:
    puzzle_input = [line for line in f.read().split("\n") if line]


def parse_rule(rule):
    """Split a rule into bag name, bag content and quantity."""
    rule = rule.replace(".", "")
    rule = rule.replace("bags", "bag")
    temp = rule.split(" contain ")
    name = temp[0]
    if "no other bag" in rule:
        return name, []

    content = []
    bags = temp[1].split(", ")

    for bag in bags:
        count = int(bag[0])
        content_name = bag[2:].replace("bags", "bag")
        content.append((count, content_name))
    return name, content


def contains_bag(rule, bag="shiny gold bag"):
    name, contents = parse_rule(rule)
    for content in contents:
        if bag == content[1]:
            return name, content[0]
    return None


def get_super_bags(rules, bag="shiny gold bag"):
    bags = {}
    for rule in rules:
        result = contains_bag(rule, bag)
        if result is not None:
            bags[result[0]] = result[1]
    return bags


def count_bags(rules, target_bag, collected_bags=[]):
    bags = get_super_bags(rules, target_bag)
    collected_bags.extend(list(bags.keys()))
    for bag in bags:
        collected_bags.extend(count_bags(rules, bag, collected_bags))
    return set(collected_bags)


def solve_part_one():
    return len(count_bags(puzzle_input, target_bag))


def parse_all_rules(rules):
    return {x[0]: x[1] for x in (parse_rule(rule) for rule in rules)}


def count_content(bag, bags):
    content = bags[bag]
    if not content:
        result = 1
    else:
        result = 1 + sum(x[0] * count_content(x[1], bags) for x in content)
    return result


def solve_part_two():
    bags = parse_all_rules(puzzle_input)
    return count_content(target_bag, bags) - 1  # Do not count the shiny gold bag

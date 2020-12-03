"""Unsere Lösung für Tag 3."""

EXAMPLE = """..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""


def baumfäller(buchstabe):
    if buchstabe == "#":
        return 1
    else:
        return 0


def positionierer(
    feld_unten, feld_rechts, zeilenlänge, steigung_unten=1, steigung_rechts=3
):
    if feld_rechts + steigung_rechts < zeilenlänge:
        return feld_unten + steigung_unten, feld_rechts + steigung_rechts
    else:
        zeilenumbruch = -(zeilenlänge - (feld_rechts + steigung_rechts))
        return feld_unten + steigung_unten, zeilenumbruch


def baumzähler(feld, steigung_unten=1, steigung_rechts=3):
    feld_unten = 0
    feld_rechts = 0
    zeilenlänge = len(feld[0])
    bäume = 0

    while feld_unten < len(feld):
        bäume += baumfäller(feld[feld_unten][feld_rechts])
        feld_unten, feld_rechts = positionierer(
            feld_unten, feld_rechts, zeilenlänge, steigung_unten, steigung_rechts
        )

    return bäume


def datenlader(dateiname):
    with open(dateiname) as file:
        return [line for line in file.read().split("\n") if line]


def löse_part_1(feld):
    return baumzähler(feld)


def löse_part_2(feld, steigungen_unten, steigungen_rechts):
    bäume = 1
    for steigung_unten, steigung_rechts in zip(steigungen_unten, steigungen_rechts):
        bäume *= baumzähler(feld, steigung_unten, steigung_rechts)
    return bäume


if __name__ == "__main__":
    assert baumfäller("#") == 1, "baum hat dich gefällt"
    assert baumfäller(".") == 0, "baum hat dich gefällt"
    assert positionierer(0, 0, 5) == (1, 3), "Verzählt"
    assert positionierer(2, 6, 8) == (3, 1), "Verzählt"
    assert positionierer(2, 7, 8) == (3, 2), "Verzählt"
    assert positionierer(2, 8, 8) == (3, 3), "Verzählt"
    assert positionierer(2, 5, 8) == (3, 0), "Verzählt"
    assert baumzähler(EXAMPLE.split("\n")) == 7, "Verzählt! 😝"
    assert baumzähler(EXAMPLE.split("\n"), steigung_rechts=1) == 2, "Verzählt! 😝"
    assert baumzähler(EXAMPLE.split("\n"), steigung_rechts=5) == 3, "Verzählt! 😝"
    assert baumzähler(EXAMPLE.split("\n"), steigung_rechts=7) == 4, "Verzählt! 😝"
    assert (
        baumzähler(EXAMPLE.split("\n"), steigung_unten=2, steigung_rechts=1) == 2
    ), "Verzählt! 😝"

    feld = datenlader("day3.txt")

    assert löse_part_1(EXAMPLE.split("\n")) == 7

    steigungen_rechts = [1, 3, 5, 7, 1]
    steigungen_unten = [1, 1, 1, 1, 2]
    assert löse_part_2(EXAMPLE.split("\n"), steigungen_unten, steigungen_rechts) == 336

    bäume = löse_part_2(feld, steigungen_unten, steigungen_rechts)
    print(f"Die Anzahl der Bäume ist {bäume}.")

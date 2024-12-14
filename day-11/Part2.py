def blink(ns):
    rs = []

    for n in ns:
        if n == 0:
            rs.append(1)
        elif len(str(n)) % 2 == 0:
            l = len(str(n))
            (xs, ys) = (str(n)[0:(l//2)], str(n)[(l//2):])
            x = int("".join(xs))
            y = int("".join(ys))
            rs.append(x)
            rs.append(y)
        else:
            rs.append(2024 * n)

    return rs


def stone_count(k, n):
    if k == 0:
        return 1
    else:
        ms = blink([n])
        return sum([ stone_count(k-1, m) for m in ms ])


counts = {}

def stone_countm(k, n):
    if k == 0:
        return 1

    if (k, n) not in counts:
        ms = blink([n])
        counts[(k, n)] = sum([ stone_countm(k-1, m) for m in ms ])

    return counts[(k, n)]


def main():
    file_name = "day-11/input.txt"
    lines     = []

    with open(file_name, "r") as file:
        lines  = list(file)
        init   = lines[0]
        stones = [ int(s) for s in init.split(" ") ]

        total = 0
        for s in stones:
            total += stone_countm(75, s)
        print(total)
        print(len(counts))

if __name__ == "__main__":
    main()

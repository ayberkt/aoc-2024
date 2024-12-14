def cut(ns):
    xs = []
    ys = []

    for i in range(0, len(ns)):
        if i < len(ns) / 2:
            xs.append(ns[i])
        else:
            ys.append(ns[i])

    return (xs, ys)

def blink(ns):

    rs = []

    for n in ns:
        if n == 0:
            rs.append(1)
        elif len(str(n)) % 2 == 0:
            (xs, ys) = cut(str(n))
            x = int("".join(xs))
            y = int("".join(ys))
            rs.append(x)
            rs.append(y)
        else:
            rs.append(2024 * n)

    return rs

def iterated_blink(k, ns):

    ms = ns

    for i in range(0, k):
        # print(ms)
        ms = blink(ms)

    return ms


def stone_count(k, ns):
    if k == 0:
        return len(ns)
    else:
        ms = blink(ns)

        return sum([ stone_count(k-1, [ m ]) for m in ms ])

counts = []

for _ in range(0, 76):
    counts.append({})

def stone_countm(k, ns):
    if k <= 3 and len(ns) <= 2:
        s = frozenset(ns)
        counts[k][s] = stone_count(k, ns)
        return counts[k][s]
    else:
        ms = blink(ns)
        s  = frozenset(ms)
        if len(ns) <= 30 and s in counts[k]:
            # print("Cache hit.")
            return counts[k][s]
        else:
            xs, ys = cut(ms)
            result = stone_countm(k-1, xs) + stone_countm(k-1, ys)

            if len(ns) <= 20:
                counts[k][s] = result

            return result

init   = "3 386358 86195 85 1267 3752457 0 741"
stones = [ int(s) for s in init.split(" ") ]

print(stone_countm(75, stones))

# for c in counts:
#     print(len(c))

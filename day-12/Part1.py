from collections import deque

lines  = []
width  = 0
height = 0

def neighbours(coord):
    (i, j) = coord
    return [(i-1, j), (i, j+1), (i+1, j), (i, j-1)]

def get_region(coord):
    q = deque([coord])

    species = lines[coord[0]][coord[1]]
    region  = [coord]

    while q:
        curr = q.pop()
        for n in neighbours(curr):
            (i, j) = n
            if i >= 0 and j >= 0 and i < height and j < width:
                if lines[i][j] == species and n not in region:
                    region.append(n)
                    q.append(n)

    return region

def is_out_of_bounds(coord):
    return coord[0] < 0 or coord[1] < 0 or coord[0] >= height or coord[1] >= width

def is_in_bounds(coord):
    return not is_out_of_bounds(coord)

def have_the_same_species(coord1, coord2):
    (x1, y1), (x2, y2) = coord1, coord2

    return lines[x1][y1] == lines[x2][y2]

def perimeter(region):
    total = 0

    for cell in region:
        total += len([n for n in neighbours(cell) if is_out_of_bounds(n)])
        total += len([ n for n in neighbours(cell) if is_in_bounds(n) and not have_the_same_species(n, cell) ])

    return total

def main():
    global lines, width, height

    file_name = "day-12/input.txt"
    file      = open(file_name, "r")
    lines     = [ line.rstrip() for line in file ]
    height    = len(lines)
    width     = len(lines[0])

    partitions = []
    cells      = []

    for i, line in enumerate(lines):
        for j, _ in enumerate(line):
            cells.insert(0, (i, j))

    while len(cells) > 0:
        curr   = cells.pop()
        region = get_region(curr)

        partitions.append(region)

        for n in region:
            if n in cells:
                cells.remove(n)

    totalPrice = 0
    for part in partitions:
        totalPrice += perimeter(part) * len(part)

    print(totalPrice)

if __name__ == "__main__":
    main()
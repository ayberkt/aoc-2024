from collections import deque

file_name = "/Users/ayberkt/Code/aoc-2024/day-10/input.txt"
lines     = []

def discover(i, j):
    neighbours = [(i-1, j), (i, j+1), (i+1, j), (i, j-1)]
    result     = []

    for (x, y) in neighbours:
        if 0 <= x < height and 0 <= y < width:
            if int(lines[x][y]) == int(lines[i][j])+1:
                result.append((x, y))

    return result

def score(i, j):
    q       = deque([(i, j)])
    result  = 0
    visited = []

    while q:
        (x, y) = q.popleft()
        visited.append((x, y))

        if lines[x][y]  == '9':
            result += 1
        else:
            for (xn, yn) in discover(x, y):
                if (xn, yn) not in visited:
                    q.appendleft((xn, yn))

    return result

with open(file_name, "r") as f:

    for line in f:
        lines.append(line.rstrip())

    height = len(lines)
    width  = len(lines[0])

def main():
    total = 0

    for i in range(height):
        for j in range(width):
            if lines[i][j] == '0':
                total += score(i, j)

    print(total)

if __name__ == "__main__":
    main()

import pulp
# creates a nested list of tuples with each [row][col] index representing
# the coordinate location of each box in the sudoku puzzle
sudoku_list = []
for row in range(1, 10):
    mylist = []
    for col in range(1,10):
        index = (row, col)
        mylist.append(index)
    sudoku_list.append(mylist)

# creates 9 lists of "boxes"
# Each list containing the coordinates of all squares in each of the 9 boxes
box_list = []
for row in range(0,3):
    for col in range(0,3):
        box = [sudoku_list[3 * row + i][3 * col + j] for i in range(0,3) for j in range(0,3)]
        box_list.append(box)

# initialize lp
sudoku_lp = pulp.LpProblem("Sudoku LP", pulp.LpMinimize)

# initialize # of rows and columns in puzzle, and # of "colors"
rows = range(1,10)
columns = range(1,10)

# upper limit is 81
numbers = range(1,10)

# initialize the x and y variables
# x is 3 dimensional with row and column (representing location in puzzle), and numbers (1-9 "colors")
# y is 1 dimensional array of "colors" (numbers 1-9)

x_rck = pulp.LpVariable.dicts("x",(rows,columns,numbers),0,1,pulp.LpInteger)
yk = pulp.LpVariable.dicts("yk",numbers,0,1,pulp.LpInteger)

# objective: sum of y over all k
obj = pulp.lpSum(yk[k] for k in numbers)
sudoku_lp += obj, "Objective Function"

# constaint s.t. if node i is assigned color k, color k is used
for row in rows:
    for col in columns:
        for k in numbers:
            sudoku_lp += x_rck[row][col][k]<= yk[k],""

# constraint s.t. each node uses exactly one color
for row in rows:
    for col in columns:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for k in numbers]) == 1,""

# constraint s.t. no nodes in the same box have the same number
for k in numbers:
    for box in box_list:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for (row, col) in box]) <= 1,""

# constraint s.t. no nodes in the same column have the same number
for k in numbers:
    for row in rows:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for col in columns]) <= 1,""

# constraint s.t. no nodes in the same column have the same number
for k in numbers:
    for col in columns:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for row in rows]) <= 1,""

#constraint for upper bound on # of colors used
#upper_bound = 81
sudoku_lp += pulp.lpSum(yk[k] for k in numbers)<= 9,""

#constraints for pre-filled boxes given in puzzle
sudoku_lp += x_rck[1][1][6] == 1,""
sudoku_lp += x_rck[2][1][9] == 1,""
sudoku_lp += x_rck[4][1][2] == 1,""
sudoku_lp += x_rck[9][1][8] == 1,""

sudoku_lp += x_rck[3][2][7] == 1,""
sudoku_lp += x_rck[6][2][8] == 1,""

sudoku_lp += x_rck[6][3][9] == 1,""

sudoku_lp += x_rck[4][4][6] == 1,""

sudoku_lp += x_rck[3][5][4] == 1,""
sudoku_lp += x_rck[4][5][1] == 1,""
sudoku_lp += x_rck[7][5][6] == 1,""

sudoku_lp += x_rck[1][6][8] == 1,""
sudoku_lp += x_rck[2][6][6] == 1,""
sudoku_lp += x_rck[6][6][2] == 1,""
sudoku_lp += x_rck[9][6][1] == 1,""

sudoku_lp += x_rck[1][7][9] == 1,""
sudoku_lp += x_rck[2][7][1] == 1,""
sudoku_lp += x_rck[5][7][2] == 1,""
sudoku_lp += x_rck[9][7][6] == 1,""

sudoku_lp += x_rck[1][8][4] == 1,""
sudoku_lp += x_rck[8][8][3] == 1,""

sudoku_lp += x_rck[7][9][5] == 1,""

'''
# solves lp
sudoku_lp.solve()
status = str(pulp.LpStatus[sudoku_lp.status])
print("Solution: "+ status)


#prints out optimal solution if one is found
if status == "Optimal":
    for row in rows:
        line = ""
        if row in [1,4,7]: print("+=======+=======+=======+")
        for col in columns:
            for k in numbers:
                if x_rck[row][col][k].value() == 1:
                    if col in [1,4,7]: line += "| "
                    line += str(k) + " "
                    if col == 9: line += "| "
        print(line)
    print("+=======+=======+=======+")
else:
    print("Optimal Solution could not be found.")

print("Chromatic Number: ", sudoku_lp.objective.value())

'''
optimal = True
counter = 0
while optimal:
    sudoku_lp.solve()
    status = str(pulp.LpStatus[sudoku_lp.status])
    counter += 1
    print("Solution: "+ status, 'Counter : {}'.format(counter))

    print(" ")

    if status == "Optimal":
        for row in rows:
            line = ""
            if row in [1,4,7]: print("+=======+=======+=======+")
            for col in columns:
                for k in numbers:
                    if x_rck[row][col][k].value() == 1:
                        if col in [1,4,7]: line += "| "
                        line += str(k) + " "
                        if col == 9: line += "| "
            print(line)
        print("+=======+=======+=======+")



    if status == "Optimal":
        sudoku_lp += pulp.lpSum([x_rck[r][c][v] for v in numbers for r in rows for c in columns if x_rck[r][c][v].value()==1]) <= 80
    # If a new optimal solution cannot be found, we end the program
    else:
        optimal = False
        break 
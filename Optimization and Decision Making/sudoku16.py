import pulp
# creates a nested list of tuples with each [row][col] index representing
# the coordinate location of each box in the sudoku puzzle
sudoku_list = []
for row in range(1, 17):
    mylist = []
    for col in range(1,17):
        index = (row, col)
        mylist.append(index)
    sudoku_list.append(mylist)

# creates 16 lists of "boxes"
# Each list containing the coordinates of all squares in each of the 9 boxes
box_list = []
for row in range(0,4):
    for col in range(0,4):
        box = [sudoku_list[4 * row + i][4 * col + j] for i in range(0,4) for j in range(0,4)]
        box_list.append(box)

# initialize lp
sudoku_lp = pulp.LpProblem("Sudoku LP", pulp.LpMinimize)

# initialize # of rows and columns in puzzle, and # of "colors"
rows = range(1,17)
columns = range(1,17)
#numbers = range(1,17)
numbers = ['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']

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

# constraint s.t. no nodes in the same row have the same number
for k in numbers:
    for row in rows:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for col in columns]) <= 1,""

# constraint s.t. no nodes in the same column have the same number
for k in numbers:
    for col in columns:
        sudoku_lp += pulp.lpSum([x_rck[row][col][k] for row in rows]) <= 1,""

# constraint s.t. ALL 16 numbers are used
sudoku_lp += pulp.lpSum(yk[k] for k in numbers) == 16,""

#constraints for pre-filled boxes given in puzzle
sudoku_lp += x_rck[1][1]["A"] == 1,""
sudoku_lp += x_rck[1][5]["9"] == 1,""
sudoku_lp += x_rck[1][11]["2"] == 1,""
sudoku_lp += x_rck[1][16]["3"] == 1,""
sudoku_lp += x_rck[2][1]["2"] == 1,""
sudoku_lp += x_rck[2][5]["0"] == 1,""
sudoku_lp += x_rck[2][7]["F"] == 1,""
sudoku_lp += x_rck[2][12]["5"] == 1,""
sudoku_lp += x_rck[3][1]["8"] == 1,""
sudoku_lp += x_rck[3][2]["B"] == 1,""
sudoku_lp += x_rck[3][6]["7"] == 1,""
sudoku_lp += x_rck[3][12]["3"] == 1,""
sudoku_lp += x_rck[3][13]["6"] == 1,""
sudoku_lp += x_rck[3][14]["C"] == 1,""
sudoku_lp += x_rck[3][15]["0"] == 1,""
sudoku_lp += x_rck[4][2]["F"] == 1,""
sudoku_lp += x_rck[4][3]["9"] == 1,""
sudoku_lp += x_rck[4][4]["3"] == 1,""
sudoku_lp += x_rck[4][5]["1"] == 1,""
sudoku_lp += x_rck[4][9]["C"] == 1,""
sudoku_lp += x_rck[4][11]["4"] == 1,""
sudoku_lp += x_rck[4][12]["B"] == 1,""
sudoku_lp += x_rck[4][13]["A"] == 1,""
sudoku_lp += x_rck[4][14]["D"] == 1,""
sudoku_lp += x_rck[5][6]["6"] == 1,""
sudoku_lp += x_rck[5][8]["D"] == 1,""
sudoku_lp += x_rck[5][14]["5"] == 1,""
sudoku_lp += x_rck[5][16]["2"] == 1,""
sudoku_lp += x_rck[6][2]["A"] == 1,""
sudoku_lp += x_rck[6][3]["6"] == 1,""
sudoku_lp += x_rck[6][4]["F"] == 1,""
sudoku_lp += x_rck[6][8]["3"] == 1,""
sudoku_lp += x_rck[6][11]["9"] == 1,""
sudoku_lp += x_rck[6][12]["1"] == 1,""
sudoku_lp += x_rck[6][14]["B"] == 1,""
sudoku_lp += x_rck[7][1]["D"] == 1,""
sudoku_lp += x_rck[7][4]["C"] == 1,""
sudoku_lp += x_rck[7][7]["B"] == 1,""
sudoku_lp += x_rck[7][8]["4"] == 1,""
sudoku_lp += x_rck[7][9]["3"] == 1,""
sudoku_lp += x_rck[7][13]["1"] == 1,""
sudoku_lp += x_rck[7][14]["E"] == 1,""
sudoku_lp += x_rck[8][5]["8"] == 1,""
sudoku_lp += x_rck[8][6]["C"] == 1,""
sudoku_lp += x_rck[8][10]["5"] == 1,""
sudoku_lp += x_rck[8][11]["E"] == 1,""
sudoku_lp += x_rck[8][13]["0"] == 1,""
sudoku_lp += x_rck[9][3]["7"] == 1,""
sudoku_lp += x_rck[9][5]["F"] == 1,""
sudoku_lp += x_rck[9][6]["1"] == 1,""
sudoku_lp += x_rck[9][7]["8"] == 1,""
sudoku_lp += x_rck[9][9]["E"] == 1,""
sudoku_lp += x_rck[9][14]["6"] == 1,""
sudoku_lp += x_rck[9][16]["D"] == 1,""
sudoku_lp += x_rck[10][2]["9"] == 1,""
sudoku_lp += x_rck[10][3]["2"] == 1,""
sudoku_lp += x_rck[10][5]["6"] == 1,""
sudoku_lp += x_rck[10][8]["C"] == 1,""
sudoku_lp += x_rck[10][12]["4"] == 1,""
sudoku_lp += x_rck[10][13]["8"] == 1,""
sudoku_lp += x_rck[10][15]["F"] == 1,""
sudoku_lp += x_rck[11][3]["4"] == 1,""
sudoku_lp += x_rck[11][4]["8"] == 1,""
sudoku_lp += x_rck[11][8]["5"] == 1,""
sudoku_lp += x_rck[11][9]["B"] == 1,""
sudoku_lp += x_rck[11][12]["9"] == 1,""
sudoku_lp += x_rck[11][13]["2"] == 1,""
sudoku_lp += x_rck[11][14]["1"] == 1,""
sudoku_lp += x_rck[12][1]["0"] == 1,""
sudoku_lp += x_rck[12][2]["6"] == 1,""
sudoku_lp += x_rck[12][3]["E"] == 1,""
sudoku_lp += x_rck[12][11]["D"] == 1,""
sudoku_lp += x_rck[12][12]["C"] == 1,""
sudoku_lp += x_rck[12][16]["B"] == 1,""
sudoku_lp += x_rck[13][3]["A"] == 1,""
sudoku_lp += x_rck[13][7]["D"] == 1,""
sudoku_lp += x_rck[13][10]["8"] == 1,""
sudoku_lp += x_rck[13][12]["F"] == 1,""
sudoku_lp += x_rck[13][14]["9"] == 1,""
sudoku_lp += x_rck[14][7]["0"] == 1,""
sudoku_lp += x_rck[14][11]["7"] == 1,""
sudoku_lp += x_rck[14][13]["D"] == 1,""
sudoku_lp += x_rck[14][14]["3"] == 1,""
sudoku_lp += x_rck[14][15]["4"] == 1,""
sudoku_lp += x_rck[15][2]["2"] == 1,""
sudoku_lp += x_rck[15][4]["E"] == 1,""
sudoku_lp += x_rck[15][7]["4"] == 1,""
sudoku_lp += x_rck[15][8]["6"] == 1,""
sudoku_lp += x_rck[15][9]["9"] == 1,""
sudoku_lp += x_rck[15][14]["0"] == 1,""
sudoku_lp += x_rck[15][16]["7"] == 1,""
sudoku_lp += x_rck[16][1]["C"] == 1,""
sudoku_lp += x_rck[16][6]["8"] == 1,""
sudoku_lp += x_rck[16][12]["A"] == 1,""

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
            if row in [1,5,9,13]: print("+=========+=========+=========+=========+")
            for col in columns:
                for k in numbers:
                    if x_rck[row][col][k].value() == 1:
                        if col in [1,5,9,13]: line += "| "
                        line += str(k) + " "
                        if col == 16: line += "| "
            print(line)
        print("+=========+=========+=========+=========+")



    if status == "Optimal":
        sudoku_lp += pulp.lpSum([x_rck[r][c][v] for v in numbers for r in rows for c in columns if x_rck[r][c][v].value()==1]) <= 80
    # If a new optimal solution cannot be found, we end the program
    else:
        optimal = False
        break 
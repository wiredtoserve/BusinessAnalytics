import matplotlib.pyplot as plt

from matplotlib.patches import Circle, Wedge, Polygon
from matplotlib.collections import PatchCollection

with open('cstviccd_l.mif', 'r') as f:
    r = f.readlines()

    content = [x.strip() for x in r]

    datalist = []
    start = False

    for row in content:
        # print(row)
        if row == 'DATA':
            # print(row)
            start = True
        if start:
            datalist.append(row)

state = 0
vertices = 0

figure = plt.figure()
ax = figure.gca()

for row in datalist:
    # print(row[0:5])
    if row[0:] == 'DATA':
        state = 1

    elif state == 1:
        if row[0:4] == 'LINE':
            x_str = row[5:]
            x = x_str.split()
            x = [float(i) for i in x]

            xx = x[::2]
            yy = x[1::2]

            ax.plot(xx, yy)
            # plt.show()

            state = 1
            # break

        elif row[0:5] == 'PLINE':

            vertices = int(row[6:8])
            state = 2

            points = []
            counter = 0

    elif state == 2:

        counter += 1

        if counter <= vertices:
            num = row.split()
            x = float(num[0])
            y = float(num[1])

            points.append((x, y))

        else:
            p = plt.Polygon(points, closed=False, alpha=0.5, fill=False, edgecolor=None)
            ax.add_line(p)
            state = 1
            vertices = 0

    elif row == '':
        ax.show()
        break

plt.interactive(False)
plt.show()
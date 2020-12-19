import copy as cp

with open("../input") as f:
    encoded_data = f.read()


# 0 = NoSeat; 1 = FreeSeat; 2 = OccSeat

def decode(char):
    if char == '.':
        return 0
    elif char == 'L':
        return 1
    else:
        return -99

block = list(map( lambda xs: list(map(decode,xs)), encoded_data.split()))

change = True


width = len(block[0])
height = len(block)

def print_block(b):
    print("BLOCK")
    for row in b:
        print(row)



def seeOcc(b,x0,y0,dx,dy):
    width = len(b[0]) - 1
    height = len(b) - 1
    if x0 + dx >= 0 and x0 + dx <= width:
        if y0 + dy >= 0 and y0 + dy <= height:
            spotted_seat = b[y0+dy][x0+dx]
            if spotted_seat == 2:
                return 1
            elif spotted_seat == 1:
                return 0
            else:
                return seeOcc(b,x0+dx,y0+dy,dx,dy)

    return 0

while change:
    change = False
    new_block = cp.deepcopy(block)
    for y in range(height):
        for x in range(width):
            occupied = sum([seeOcc(block,x,y,dx,dy) for dx in [-1,0,1] for dy in [-1,0,1] if dx != 0 or dy != 0])

            if block[y][x] == 1 and occupied == 0:
                change = True 
                new_block[y][x] = 2

            elif block[y][x] == 2 and occupied >= 5:
                change = True
                new_block[y][x] = 1
    block = new_block
    print_block(block) 


acc_occupied = 0
for row in block:
    for i in row:
        if i == 2:
            acc_occupied += 1

print(acc_occupied)

               

# the idea is to read all the characters in and then index into the memory to
# read and process them

file_path = "inputs/day6_in.txt"

char_arr = []

with open(file_path) as f:
    for ch in f.read():
        char_arr.append(ch)

row_len = 0
for ch in char_arr:
    if ch == '\n':
        break
    row_len += 1

# print(char_arr[0], char_arr[row_len+1], char_arr[2*row_len+2], char_arr[3*row_len+3])
# print(f"{row_len=}")

row_offsets = [0, row_len+1, 2*row_len+2, 3*row_len+3, 4*row_len+4]

tot = 0
for col in range(row_len-1):
    if char_arr[row_offsets[4] + col] == '+':
        # print("+")
        curr_off = 0
        curr_num = 0
        while True:
            num = 0
            for row_off in range(4):
                curr_dig = char_arr[row_offsets[row_off] + col + curr_off]
                if curr_dig >= '0' and curr_dig <= '9':
                    num = (num * 10) + int(curr_dig)
            if num == 0:
                tot += curr_num
                break
            # print(f"{num=}")
            curr_num += num
            curr_off += 1

    elif char_arr[row_offsets[4] + col] == '*':
        # print("*")
        curr_off = 0
        curr_num = 1
        while True:
            num = 0
            for row_off in range(4):
                curr_dig = char_arr[row_offsets[row_off] + col + curr_off]
                if curr_dig >= '0' and curr_dig <= '9':
                    num = (num * 10) + int(curr_dig)
            if num == 0:
                tot += curr_num
                break
            # print(f"{num=}")
            curr_num *= num
            curr_off += 1

print(f"{tot=}")
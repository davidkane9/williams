import re
with open('faculty-2003-2004.txt') as f:
	lines = f.readlines()


for line in lines:
	count = 0
	for letter in line:
		if letter == '#':
			count = count + 1
	if count != 2:
		print(line)



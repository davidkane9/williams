import re
with open('faculty-2000-2001.txt') as f:
	lines = f.readlines()


for line in lines:
	count = 0 
	for letter in line: 
		if letter == '#':
			count = count + 1
	if count != 2:
		print(line)
	


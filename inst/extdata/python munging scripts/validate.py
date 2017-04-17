import re
with open('faculty-2014-2015.txt') as f:
	lines = f.readlines()


for line in lines:
	count = 0 
	for letter in line: 
		if letter == '#':
			count = count + 1
	if count != 1:
		print(line)
	


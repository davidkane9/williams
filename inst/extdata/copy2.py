import re
with open('faculty-2012-2013.txt') as f:
	lines = f.readlines()

with open('faculty-2013-2014.txt') as f:
	lines2 = f.readlines()


newList = []
for line in lines: 
	index = line.index('#')
	name = line[0:index].replace("*", "")
	newList.append(name.strip())


for i in range(len(lines2)):
	line = lines2[i]
	index = line.index('#') if '#' in line else -1
	if  index != -1:
		name = line[0:index].replace("*", "")
		pos = newList.index(name.strip()) if name.strip() in newList else -1
		if pos != -1:
			lines2[i] = lines[pos]
		else:
			print("nope")

print("here")
target = open("correctedFac.txt", 'w')
target.write("".join(lines2))
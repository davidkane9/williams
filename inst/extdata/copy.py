import re
with open('faculty-2005-2006.txt') as f:
	lines = f.readlines()

newList = []
for line in lines: 
	index = line.index('#')
	name = line[0:index].replace("*", "")
	newList.append(name.strip())

str = open('faculty-2006-2007.txt', 'r').read()
str = str.replace("and\n", "and ")
str = str.replace("the\n", "the ")
str = str.replace(" B.A.", "\nB.A.")
str = str.replace(" A.B.", "\nA.B.")



for name in newList:
	if name in str:
		index = str.index(name)
		str = str[0: index + len(name)] + " # " + str[index + len(name):]

target = open("facultyCopied.txt", 'w')
target.write(str)
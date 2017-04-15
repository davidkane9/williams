import re
with open('faculty-2012-2013.txt') as f:
	lines = f.readlines()

newList = []
for line in lines: 
	index = line.index('#')
	name = line[0:index].replace("*", "")
	newList.append(name.strip())

str = open('faculty-2013-2014.txt', 'r').read()
# str = str.replace("and\n", "and ")
# str = str.replace("the\n", "the ")
# str = str.replace(" B.A.", "\nB.A.")
# str = str.replace(" A.B.", "\nA.B.")
# str = str.replace(" B.S.", "\nB.S.")

newStr = ''
i = 0
while i < len(str):
	if i + 10 < len(str) and str[i] == ',' and str[i+1] == ' ' and str[i+2:i+6].isdigit() and \
	str[i+6] == ',' and str[i+7] == ' ' and str[i+8] == 'M' and str[i+9] == 'S' and str[i+10] == ',':
		newStr = newStr + " ; M.S. (" + str[i+2:i+6] + ")" 
		i = i + 11
	else:
		newStr = newStr + str[i]
		i = i + 1


# for name in newList:
# 	if name in str:
# 		index = str.index(name)
# 		str = str[0: index + len(name)] + " # " + str[index + len(name):]

target = open("correctedFac.txt", 'w')
target.write(newStr)
import re
with open('faculty-2008-2009.txt') as f:
	lines = f.readlines()

thefile = open('correctedFac.txt', 'w')
newlist = [] 

for i in range(len(lines)):
	if i % 2 == 1:
		continue
	if i + 1 < len(lines):
		print("\n" in lines[i])
		text = lines[i][0:len(lines[i])-1] + " # " + lines[i+1]  
	newlist.append(text)
	


for line in newlist:
	thefile.write(line)

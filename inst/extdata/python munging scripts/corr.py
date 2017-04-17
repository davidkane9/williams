import re
with open('faculty-2001-2002.txt') as f:
	lines = f.readlines()

thefile = open('correctedFac.txt', 'w')
newlist = [] 

for line in lines:
	text = line
	text = text.replace(" B.A. ", "# B.A. ")
	text = text.replace(" B.S. ", "# B.S. ")
	text = text.replace(" A.B. ", "# A.B. ")
	text = text.replace(" B.Mus. ", "# B.Mus. ")
	text = text.replace(" B.F.A. ", "# B.F.A. ")

	if not "B.A." in text and not "B.S." in text and not "A.B." in text and not "B.Mus." in text and not "B.F.A" in text:
		print(text)
	newlist.append(text)
	


for line in newlist:
	thefile.write(line)

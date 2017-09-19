import sys

def find_main(f):
	in_main = False
	for line in f:
		if len(line) > 3 and "00000000" not in line and '>:' not in line:
			get_ins_bytes(line)
		#elif in_main: break
		elif "<main>" in line:
			in_main = True

def get_ins_bytes(line):
	i = 0
	while line[i] != '\t': i += 1
	i += 1
	for j in range(8,0,-2):
		printHexByte(line[i+j-2:i+j])

def printHexByte(hex_byte):
	global code
	hb = ""
	hb += chr(int(hex_byte,16))
	code += hb
	

f = open('out.dis', 'r')
global code
code = ""
find_main(f)
sys.stdout.write(code)
		.data
y_:		.space 4				# size(int) = 4
x_:		.space 616				# size(R) = size(int)*2 = 8
								# size(S) = 11*size(R) = 11*8 = 88
								# size(T) = (9-3+1)*size(S) = 7*88 = 616
								# offset(T[3][1].f) = 0
								# offset(T[3][1].g) = 4
								# offset(T[3][2].f) = 8
								# offset(T[4][1].f) = 88
		.text
		.globl main
		.ent main
main:	
		lw $t6, y_				# $t6 = y (first index)
		sub $t6, $t6, 3			# $t6 = y - 3 (adjust for index start at 3)
		mul $t6, $t6, 88		# $t6 = y*size(S) = y*88 (x[y])
		sw $0, x_+32+4($t6)		# adr(x) + offset([5]) + offset(g) + offset([y]) = 0 | x[y][5].g = false
		lw $t0, y_				# $t0 = y (first index)
		sub $t0, $t0, 3			# $t0 = y - 3 (adjust for index start at 3)
		mul $t0, $t0, 88		# $t0 = y*size(S) = y*88 (x[y])
		lw $t7, y_				# $t7 = y
		add $t7, $t7, 1			# $t7 = y + 1 (element at y + 1)
		sub $t7, $t7, 1			# $t7 = y - 1 (adjust for index starting at 1)
		mul $t7, $t7, 8			# $t7 = $t7*size(R) offset(y+1) [y + 1]
		add $t7, $t0, $t7		# $t7 = $t7 + adr(x[y]) x[y][y+1]
		addi $t4, $0, 1			# $t4 = 1 (true)
		sw $t4, x_+0($t7)		# adr(x) + offset([y][y+1]) = 1  x[y][y+1] = true
		li $v0, 10				# $v0 = 10 (return 10)
		syscall
		.end main


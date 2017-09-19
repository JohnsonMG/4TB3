	.data
z_:	.space 48
x_:	.space 48
	.text
	.global main
main:	
	mov w11, wzr
	mov w14, #12
.L0:	
	adrp x10, x_
	mov w15, #4
	madd w10, w15, w11, w10
	add w10, w10, :lo12:x_
	ld1 {v26.4S}, [x10]
	mov w13, #1
	dup v6.4S, w13
	add v28.4S, v26.4S, v6.4S
	adrp x12, x_
	mov w9, #4
	madd w12, w9, w11, w12
	add w12, w12, :lo12:x_
	st1 {v28.4S}, [x12]
	add w11, w11, #4
	cmp w11, w14
	blt .L0
	mov w11, wzr
	mov w14, #12
.L1:	
	adrp x10, z_
	mov w15, #4
	madd w10, w15, w11, w10
	add w10, w10, :lo12:z_
	ld1 {v8.4S}, [x10]
	mov w13, #3
	dup v9.4S, w13
	add v13.4S, v8.4S, v9.4S
	adrp x12, z_
	mov w9, #4
	madd w12, w9, w11, w12
	add w12, w12, :lo12:z_
	st1 {v13.4S}, [x12]
	add w11, w11, #4
	cmp w11, w14
	blt .L1
	adrp x11, z_
	add w11, w11, :lo12:z_
	add w11, w11, #0
	mov w14, #9
	str w14, [x11]
	mov w10, wzr
	mov w15, #12
.L2:	
	adrp x13, z_
	mov w12, #4
	madd w13, w12, w10, w13
	add w13, w13, :lo12:z_
	ld1 {v21.4S}, [x13]
	adrp x9, x_
	mov w11, #4
	madd w9, w11, w10, w9
	add w9, w9, :lo12:x_
	ld1 {v5.4S}, [x9]
	add v21.4S, v21.4S, v5.4S
	adrp x14, x_
	mov w13, #4
	madd w14, w13, w10, w14
	add w14, w14, :lo12:x_
	st1 {v21.4S}, [x14]
	add w10, w10, #4
	cmp w10, w15
	blt .L2
	adrp x12, x_
	add w12, w12, :lo12:x_
	add w12, w12, #0
	mov w8, #64
	mov w0, #1
	mov x1, x12
	mov w2, #4
	svc #0
	mov w8, #64
	mov w0, #1
	sub sp, sp, #16
	mov w9, #10
	sturb w9, [sp]
	mov x1, sp
	mov w2, #1
	svc #0
	add sp, sp, #16
	adrp x11, x_
	add w11, w11, :lo12:x_
	add w11, w11, #20
	mov w8, #64
	mov w0, #1
	mov x1, x11
	mov w2, #4
	svc #0
	mov w8, #64
	mov w0, #1
	sub sp, sp, #16
	mov w14, #10
	sturb w14, [sp]
	mov x1, sp
	mov w2, #1
	svc #0
	add sp, sp, #16
	adrp x10, x_
	add w10, w10, :lo12:x_
	add w10, w10, #44
	mov w8, #64
	mov w0, #1
	mov x1, x10
	mov w2, #4
	svc #0
	mov w8, #64
	mov w0, #1
	sub sp, sp, #16
	mov w15, #10
	sturb w15, [sp]
	mov x1, sp
	mov w2, #1
	svc #0
	add sp, sp, #16
	mov w8, #93
	mov w0, wzr
	svc #0
	nop
			section .bss
fd_in: 		resb	1
results:    resb	994
			section		.data
			db			0			
fileName:	db 			'/home/j/Projects/Advent/Advent20/01/input'

			section 	.text

			global foo
foo:		mov		eax,	5		; Open File
			mov		ebx,	fileName
			mov		ecx,	0
			mov		edx,	0777
			int 	0x80

			mov		[fd_in],	eax
			
			;read from file

			mov		eax,	3
			mov 	ebx,	[fd_in]
			mov		ecx,	results
			mov 	edx,	994
			int		0x80

			; close file

			mov 	eax,	6
			mov 	ebx,	[fd_in]
			int		0x80

			; add $ on line end
			mov		eax,	0
loopHead:	cmp		eax,	993
			je		exitProg
loopBody:	cmp		byte [results + eax],	0x0a
			jne		loopEnd
onSpace:	mov		byte [results + eax],	64
loopEnd:   	add		eax,	1
			jmp		loopHead

			; return

exitProg:	mov		eax,	results
			ret



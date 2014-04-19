data segment
; Input
  args            db 255 dup('$')   ; stores command line arguments
  argPtr          db 128 dup(0)   	; array of argument offsets
  argNum          db 0            	; stores number of arguments

; RLE
	input						db 0							; input file name
	output					db 0							; output file name
	decopress				db 0 							; 0 if compressing input 1 if decompressig input

; Error messages
	e01h						db "Invalid function number.","$"
	e02h						db "Input file not found","$"
	e03h						db "Path not found.","$"
	e04h						db "Too many open files!","$"
	e05h						db "Access denied.","$"
	e0Ch						db "Invalid access mode.","$"
	e56h						db "Invalid password.","$"
data ends

.286
assume ds:data, cs:code

code segment

start:
	call init
	call parseArgs
	call printArgs
	call exit

	putChar proc
	
	putChar endp

	getChar proc

	getChar endp

	open proc
	; entry: DL = file name offset, AL = access mode 0 read only 1 write only
	; return: AX = file handle
		push ax
		push dx

		xor dh, dh
		mov ah, 3Dh
		int 21h
		jnc endOpenFile

		openFileError:
			mov dx, ax
			call printError
			call exit

		endOpenFile:

		pop dx
		pop ax
		ret
	open endp

	parseArgs proc
    push ax
    push bx
    push cx       
    
    mov si, 82h ; command line arguments offset
    mov di, offset args
    mov cl, byte ptr es:[80h] ; number of characters entered
    
    ; check if command line should be truncated 
    mov ax, ds
    mov bx, es
    ; calculate offset between data segment argument segment
    sub ax, bx
    shl ax, 4d
    ; add offset of arguments
    sub ax, 82h
    ; compare with number of characters entered
    cmp cl, al
    jle validLength
    
    ; read only characters that are available
    mov cl, al
    
    validLength:
    
    ; clear argument counter
    xor bx, bx
    
    call removeWhitespace
    ; check for empty command line
    cmp cx, 0
    je endParseArgs
    
    readLoop:
      call readArg
      call removeWhitespace
      
      cmp cx, 0
      jg readLoop
    
    ; save number of arguments
    endParseArgs:
    mov argNum, bl
    
    pop cx
    pop bx
    pop ax    
    ret
	parseArgs endp

	removeWhitespace proc
    rwLoop:
      ; read next character
      mov al, byte ptr es:[si]
      ; check if whitespace
      cmp al, 20h
      jg endRemoveWhitespace

      inc si
      dec cx
      cmp cx, 0
      jg rwLoop
        
    endRemoveWhitespace:
      ret
	removeWhitespace endp

	readArg proc
    
    call putAddress
    raLoop:
      ; read next character
      mov al, byte ptr es:[si]
      ; check if whitespace
      cmp al, 20h
      jle endReadArg
      
      cmp al, '$'
      je illegalChar
      ; save character
      mov ds:[di], al

      inc di
        
    illegalChar:    
      inc si
      
      dec cx
      cmp cx, 0
      jg raLoop
        
    endReadArg:
      ; end string with $
      mov byte ptr ds:[di], '$'
      inc di
      ; increment argument counter
      inc bl
      
      ret
	readArg endp        

	putAddress proc
	    push bx
	    
	    ; bl contains number of arguments
	    xor bh, bh        
	    mov word ptr ds:[argPtr + bx], di
	    
	    pop bx
	    ret
	putAddress endp

	getArg proc
	; entry: DL = argument index 
	; return: AL = argument offset
 
    push bx
    xor bx, bx
    
    mov bl, dl 
    mov al, [argPtr + bx] 
    
    pop bx
    ret
	getArg endp

	getArgLen proc 
	; entry : DL = argument index
	; return: AL = argument length

    push bx   
    
    ; clear address storage
    xor bx, bx
    ; dl stores argument index
    call getArg
    mov bl, al
    mov al, 1h
    getChar:
      ; go to next character 
      inc bx
      ; check for end of argument
      cmp byte ptr ds:[bx], '$'
      
      ; return if encountered end of string
      je endGetArgLen
      ; else increment character counter and read next
      inc al
      jmp getChar
    
    endGetArgLen:
    
    pop bx
    ret
	getArgLen endp

	getArgNum proc
	; return: AL = number of arguments

    mov al, argNum
    ret
	getArgNum endp

	printArgs proc ; prints command line arguments
    push ax
    push cx
    push dx
    
    xor cx, cx
    ; prints one argument
    printLoop:
      ; check for next argument
      cmp cl, argNum
      ; if no arguments left return
      je endPrintLoop
      
      ; get argument address
      mov dl, cl
      call getArg
      ; print argument to console
      mov dl, al
      call println
      ; print line break
      call crlf
      
      inc cl
      jmp printLoop
        
    endPrintLoop:
    
    pop dx
    pop cx
    pop ax
    ret
	printArgs endp

	printError proc
	; entry: DX = error number
		call println
		call exit
		ret
	printError endp

	println proc
    push ax
    mov ah, 9h
    int 21h
    pop ax
    ret
  println endp
  
  printChar proc
    push ax
    mov ah, 2h
    int 21h
    pop ax
    ret
  printChar endp
    
  crlf proc ; prints line break 
    push ax
    push dx
    
    ; prints new line
    mov dl, 0Ah
    mov ah, 2h
    int 21h
    ; prints carriage return
    mov dl, 0Dh
    int 21h 
    
    pop dx
    pop ax
    ret
  crlf endp

	init proc
    ; save data segment
    mov ax, seg args
    mov ds, ax
    
    ; initialize stack
    mov ax, seg top
    mov ss, ax
    mov sp, offset top
    
    ; clear arithmetic registers
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    ret
  init endp
  
  exit proc ; returns control to system
    mov ax, 4C00h
    int 21h
  exit endp

code ends

stack1 segment stack
        dw 200 dup(?)
    top dw ?
stack1 ends

end start
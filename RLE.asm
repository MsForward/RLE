data segment
; Input
  args            db 255 dup("$")   ; stores command line arguments
  argPtr          dw 128 dup(0)   	; array of argument offsets
  argNum          db 0            	; stores number of arguments

; RLE
	input						db 255 dup(0)			; input file name
	output					db 255 dup(0)			; output file name
	optiond					db 0 							; 0 if compressing input 1 if decompressing input
	bufferSize			dw 2048
	inputBufferPtr	dw 0
	inputBuffer			db 2048 dup("$")
	inputBufferEndPtr		dw 0
	buffer					db 2048 dup("$")

; Error messages 
	errorPtr				dw 255 dup(0)
	e01h						db "Invalid function number.","$"
	e02h						db "Input file not found","$"
	e03h						db "Path not found.","$"
	e04h						db "Too many open files!","$"
	e05h						db "Access denied.","$"
	e06h						db "File read failed.","$"
	e0Ch						db "Invalid access mode.","$"
	e56h						db "Invalid password.","$"
	argNumError			db "Invalid number of arguments. Usage: RLE [-d] input output","$"
	optionError			db "Invalid option. Usage: RLE [-d] input output","$"
data ends

.286
assume ds:data, cs:code

code segment

start:
	call init
	call parseArgs
	call checkArgs
	call printArgs
	mov dx, offset input
	xor al, al
	call open
	mov dx, ax
	call read
	mov dx, offset buffer
	call println

	mov dx, offset output
	call create
	mov dx, ax
	call write

	call exit
	
	compress proc
	compress endp
	
	decompress proc
	decompress endp

	compress proc
		push ax
		push bx
		push cx
		push dx

		mov dx, offset input
		; open input file in read mode
		xor al, al
		call open
		; AX = file handle
		mov dx, ax

		call getChar
		cmp ah, 0
		je endCompressLoop
		mov bl, al

		compressLoop:
			call getChar
			cmp ah, 0
			je endCompressLoop
			cmp al, bl
			je addChar
			jne 

			addChar:
				inc cx

		endCompressLoop:

		pop dx
		pop cx
		pop bx
		pop ax
		ret
	compress endp

	putChar proc
	; entry: DX = file handle, AL = character to write
		push bx
		
		; check if space left in buffer
		mov bx, bufferPtr
		cmp bx, bufferEndPtr
		je writeToFile
	
	putChar endp

	getChar proc
	; entry: DX = file handle
	; return: AL = next character from file, AH = 0 if no characters left
		push bx

		xor ah, ah
		; check if there are characters left in buffer
		mov bx, bufferPtr
		cmp bx, bufferEndPtr
		je readFromFile
		jmp returnChar

		readFromFile:
			call read
			cmp bx, bufferEndPtr
			je endGetChar

		returnChar:
			mov al, [buffer + bx]
			inc bufferPtr
			mov ah, 1
			
		endGetChar:	
		pop bx
		ret
	getChar endp

	read proc
	; entry: DX = file handle
		push ax
		push bx
		push cx
		push dx

		; CX = number of bytes to read
		mov cx, bufferSize
		; BX = file handle
		mov bx, dx
		; DS:[DX] = data save location
		mov dx, offset buffer
		mov ah, 3Fh
		int 21h
		; check for errors
		jnc endRead

		readFileError:
			; DX = error number
			mov dx, ax
			call printError
			
		; AX = number of bytes actually read
		mov bx, offset buffer
		mov bufferPtr, bx
		add bx, ax
		mov bufferEndPtr, bx

		endRead:
		pop dx
		pop cx
		pop bx
		pop ax
		ret
	read endp

	write proc
	; entry: DX = file handle
		push ax
		push bx
		push cx
		push dx

		; BX = file handle
		mov bx, dx
		; CX = number of bytes to write
		mov cx, bufferSize
		; DS:[DX] = location of bytes to write
		mov dx, offset buffer
		mov ah, 40h
		int 21h
		; check for errors
		jnc endWrite

		writeToFileError:
			; DX = error number
			mov dx, ax
			call printError

		endWrite:
		mov bx, offset buffer
		mov bufferPtr, bx
		
		pop dx
		pop cx
		pop bx
		pop ax
		ret
	write endp

	create proc
	; entry: DX = file name offset
	; return: AX = file handle
		push cx
		push dx

		xor cx, cx
		mov ah, 3Ch
		int 21h
		jnc endcreateFile

		createFileError:
			mov dx, ax
			call printError

		endcreateFile:

		pop dx
		pop cx
		ret
	create endp

	open proc
	; entry: DX = file name offset, AL = access mode 0 read only 1 write only
	; return: AX = file handle
		push dx

		mov ah, 3Dh
		int 21h
		jnc endOpenFile

		openFileError:
			mov dx, ax
			call printError

		endOpenFile:

		pop dx
		ret
	open endp

	close proc
	; entry: DX = file handle
		push ax
		push bx

		mov bx, dx
		mov ah, 3Eh
		int 21h
		jnc endCloseFile

		closeFileError:
			mov dx, ax
			call printError

		endCloseFile:

		pop ax
		pop bx
		ret
	close endp

	checkArgs proc
		push ax
		push bx
		push dx

		xor bx, bx

		cmp argNum, 3
		je addOption 
		cmp argNum, 2
		jne wrongArgNum
		jmp endCheckArgs

		addOption:
			xor dl, dl
			call getArgLen
			cmp al, 2h
			jne wrongOption

			call getArg
			mov bl, al
			mov ax, word ptr ds:[bx]
			mov bl, "-"
			mov bh, "d"
			cmp ax, bx
			jne wrongOption
			mov optiond, 1
			jmp endCheckArgs

		wrongArgNum:
			mov dx, 0FFh
			call printError
		wrongOption:
			mov dx, 0FEh
			call printError

		endCheckArgs:
			call saveInput

		pop dx
		pop bx
		pop ax	
		ret
	checkArgs endp

	saveInput proc
		push ax
		push dx

		xor ax, ax

		mov dl, argNum
		dec dl

		call getArg
		mov si, ax
		mov di, offset output

		call copy

		dec dl
		call getArg
		mov si, ax
		mov di, offset input
		call copy

		pop dx
		pop ax
		ret
	saveInput endp

	copy proc
	; entry: SI = source index, DI = destination index
		push ax
		push si
		push di

		copyLoop:
			mov al, ds:[si]
			mov ds:[di], al
			inc si
			inc di
			cmp al, "$"
			jne copyLoop

		dec di
		mov al, 0
		mov ds:[di], al

		pop di
		pop si
		pop ax
		ret
	copy endp

	parseArgs proc
    push ax
    push bx
    push cx       
    
    mov si, 82h ; command line arguments offset
    mov di, offset args
    mov cl, byte ptr es:[80h] ; number of characters entered
    
    ; clear argument counter
    xor bx, bx
    
    call removeWhitespace
    ; check for empty command line
    cmp cx, 0
    je endParseArgs
    
    parseArgsLoop:
      call readArg
      call removeWhitespace
      
      cmp cx, 0
      jg parseArgsLoop
    
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

	readArg proc ; saves one command line argument
	; entry: 
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

	putAddress proc ; saves pointer to argument
	; entry: DL = argument index, DI = argument offset
	    push bx
	    
	    mov bl, dl
	    mov word ptr [argPtr + bl], di
	    
	    pop bx
	    ret
	putAddress endp

	getArg proc ; returns argument offset
	; entry: DL = argument index 
	; return: AX = argument offset
    push bx
    
    mov bl, dl 
    mov ax, word ptr [argPtr + bl] 
    
    pop bx
    ret
	getArg endp

	getArgLen proc ; return length of argument with given index
	; entry : DL = argument index
	; return: AL = argument length
    push bx
    xor bx, bx

    push ax
    ; get argument offset
    call getArg
    mov bx, ax
    pop ax

    xor al, al
    countCharLoop:
    	; increment counter
    	inc al
 			; read next character
    	inc bx
      ; check if argument ends
      cmp byte ptr ds:[bx], "$"
      jne countCharLoop
    
    pop bx
    ret
	getArgLen endp

	getArgNum proc ; returns number of program arguments
	; return: AL = number of arguments
    mov al, argNum
    ret
	getArgNum endp

	printArgs proc ; prints command line arguments
    push ax
    push cx
    push dx
    
    ; start with argument 0
    xor cl, cl

    printArgLoop:
      ; check if argument exists
      cmp cl, argNum
      ; if no arguments left return
      je endPrintArgs
      
      ; get argument address
      mov dl, cl
      call getArg
      ; print argument to console
      mov dl, al
      call println
      
      ; go to next argument
      inc cl
      jmp printArgLoop
        
    endPrintArgs:
    
    pop dx
    pop cx
    pop ax
    ret
	printArgs endp

	printError proc ; writes error to console and exits program
	; entry: DX = error number
		mov bx, dx
		shl bx, 1
		mov dx, [errorPtr + bx]
		call println
		call exit
		ret
	printError endp

	println proc ; writes single line to console
	; entry: DX = string offset
    push ax
    mov ah, 9h
    int 21h
    call crlf
    pop ax
    ret
  println endp

  print proc ; writes string in DS:DX to console
  ; entry: DX = string offset
  	push ax
  	mov ah, 9h
  	call 21h
  	pop ax
  	ret
  print endp
  
  printChar proc ; writes single character to console
  ; entry: DL = character to write
    push ax
    mov ah, 2h
    int 21h
    pop ax
    ret
  printChar endp
    
  crlf proc ; prints line break 
    push ax
    push dx
    
    ; print new line
    mov dl, 0Ah
    mov ah, 2h
    int 21h
    ; print carriage return
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

    call errorTabInit
    call bufferInit
    
    ; clear arithmetic registers
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    ret
  init endp

  bufferInit proc ; moves buffer pointer to beginning of buffer
  	push ax
  	mov ax, offset buffer
  	mov bufferPtr, ax
  	pop ax
  	ret
  bufferInit endp

  errorTabInit proc ; creates array of pointers to error messages 
  	push ax
  	push bx
        
    xor ax, ax
    mov bx, 2    
  	mov ax, offset e01h
  	mov [errorPtr + bx], ax
  	mov ax, offset e02h
  	add bx, 2
   	mov [errorPtr + bx], ax
   	mov ax, offset e03h
   	add bx, 2
   	mov [errorPtr + bx], ax
   	mov ax, offset e04h
   	add bx, 2
   	mov [errorPtr + bx], ax
   	mov ax, offset e05h
   	add bx, 2
   	mov [errorPtr + bx], ax
   	mov ax, offset e06h
   	add bx, 2
   	mov [errorPtr + bx], ax
   	mov ax, offset e0Ch
   	mov bx, 0Ch
   	shl bx, 1
   	mov [errorPtr + bx], ax
   	mov ax, offset e56h
   	mov bx, 56h
   	shl bx, 1
   	mov [errorPtr + bx], ax
   	mov ax, offset argNumError
   	mov bx, 0FFh
   	shl bx, 1
   	mov [errorPtr + bx], ax
   	mov ax, offset optionError
   	mov bx, 0FEh
   	shl bx, 1
   	mov [errorPtr + bx], ax

   	pop bx
  	pop ax
  	ret
  errorTabInit endp

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
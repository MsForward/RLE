data segment
; Input
  args            		db 255 dup("$")   ; stores command line arguments
  argPtr         			dw 128 dup(0)   	; array of argument offsets
  argNum          		db 0            	; stores number of arguments

; File data
	inputName						db 255 dup(0)			; input file name
	inputFile						dw 0							; input file handle

	outputName					db 255 dup(0)			; output file name
	outputFile					dw 0							; output file handle

	optiond							db 0 							; 0 if compressing input 1 if decompressing input

; Buffer data
	bufferSize					dw 2048

	inputBuffer					db 2048 dup(?)
											db "$"
	inputBufferPtr			dw 0
	inputBufferEndPtr		dw 0

	outputBuffer				db 2048 dup(?)
											db "$"
	outputBufferPtr			dw 0
	outputBufferEndPtr	dw 0

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

;.286
assume ds:data, cs:code

code segment

start:
	call init
	call parseArgs
	call checkArgs
	call printArgs
	call fileInit

	cmp optiond, 0
	je runC
	jne runD

	runC: 
		call compress
		call exit
	runD: 
		call decompress
		call exit

	compress proc
		push ax
		push bx
		push cx
		push dx

		; get first character
		call getChar
		cmp ah, 0
		je endCompressLoop
		mov bl, al
		xor cx, cx

		compressLoop:
			; check for previous character repeated
			cmp al, bl
			je addChar
			
			; if new char, compress previous char
			compressChar:
				mov dl, bl
				call charToSeq
				mov cx, 1
				mov bl, al
				jmp cmpLoop

			addChar:
				; increment char occurence counter
				inc cx

			cmpLoop:
				; get character
				call getChar
				; if file empty return
				cmp ah, 0
				jne compressLoop
				mov dl, bl
				call charToSeq

		endCompressLoop:
		call forceWrite

		pop dx
		pop cx
		pop bx
		pop ax
		ret
	compress endp

	decompress proc
		push ax
		push bx
		push cx
		push dx

		; get first character
		call getChar
		cmp ah, 0
		je endDecompressLoop
		mov bl, al
		mov cx, 1

		decompressLoop:
			cmp al, 0
			je decompressChar
			call seqToChar
			jmp dcmpLoop

			decompressChar:
				call readSeq
				mov dl, al
				call seqToChar
				mov cx, 1
				jmp decompressLoop
				
	        dcmpLoop:
	        ; get character
    			call getChar
    			; if file empty return
    			cmp ah, 0
    			jne decompressLoop
	            

		endDecompressLoop:
		call forceWrite

		pop dx
		pop cx
		pop bx
		pop ax
		ret
	decompress endp

	charToSeq proc
	; entry: DL = character, CL = number of occurences
		push cx

		cmp dl, 0
		je RLE
		cmp cl, 3
		jle saveByte

		RLE:
			push dx
			mov dl, 0
			call putChar
			mov dl, cl
			call putChar
			xor cl, cl
			pop dx

		saveByte:
			call putChar
			dec cl
			cmp cl, 0
			jg saveByte

		pop cx
		ret
	charToSeq endp
	
	readSeq proc
	; return: AL = character, CL = number of occurences
		call getChar
		mov cl, al
		call getChar
		ret
	readSeq endp

	seqToChar proc
	; entry: DL = character, CL = number of occurences
		stcLoop:
			call putChar
			dec cl
			cmp cl, 1
			jg stcLoop
		ret
	seqToChar endp

	putChar proc
	; DL = character to write
		push bx
		
		; check if space left in buffer
		mov bx, outputBufferPtr
		cmp bx, outputBufferEndPtr
		je writeToFile
		jne writeToBuffer

		writeToFile:
			call write

		writeToBuffer:
			mov bx, outputBufferPtr
			; save character to buffer
			mov byte ptr [bx], dl
			inc outputBufferPtr

		endPutChar:
		pop bx
		ret
	putChar endp

	forceWrite proc
		call write
		ret
	forceWrite endp

	getChar proc
	; return: AL = next character from file, AH = 0 if no characters left
		push bx
		push dx

		xor ah, ah
		; check if there are characters left in buffer
		mov bx, inputBufferPtr
		cmp bx, inputBufferEndPtr
		je readFromFile
		jne returnChar

		readFromFile:
			call read
			mov bx, inputBufferPtr
			cmp bx, inputBufferEndPtr
			je endGetChar

		returnChar:
			mov bx, inputBufferPtr
			mov al, byte ptr [bx]
			inc inputBufferPtr
			mov ah, 1
			mov dl, al
			
		endGetChar:
		pop dx	
		pop bx
		ret
	getChar endp

	read proc
		push ax
		push bx
		push cx
		push dx

		; CX = number of bytes to read
		mov cx, bufferSize
		; BX = file handle
		mov bx, inputFile
		; DS:[DX] = data save location
		mov dx, offset inputBuffer
		mov ah, 3Fh
		int 21h
		; check for errors
		jnc endRead

		readFileError:
			; DX = error number
			mov dx, ax
			call printError
			
		endRead:
		; AX = number of bytes actually read
		mov bx, offset inputBuffer
		mov inputBufferPtr, bx
		add bx, ax
		mov inputBufferEndPtr, bx

		pop dx
		pop cx
		pop bx
		pop ax
		ret
	read endp

	write proc
		push ax
		push bx
		push cx
		push dx

		; BX = file handle
		mov bx, outputFile
		; DS:[DX] = location of bytes to write
		mov dx, offset outputBuffer
		; CX = number of bytes to write
		mov cx, outputBufferPtr
		sub cx, dx
		mov ah, 40h
		int 21h
		; check for errors
		jnc endWrite

		writeToFileError:
			; DX = error number
			mov dx, ax
			call printError

		endWrite:
		mov bx, offset outputBuffer
		mov outputBufferPtr, bx

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

		cmp dx, 0
		je endCloseFile

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
			mov bx, ax
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
		mov di, offset outputName
		call copy

		dec dl
		call getArg
		mov si, ax
		mov di, offset inputName
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
		push dx
		mov dl, bl
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
      
      pop dx
      ret
	readArg endp        

	putAddress proc ; saves pointer to argument
	; entry: DL = argument index, DI = argument offset
	    push bx
	    xor bh, bh
	    mov bl, dl
	    shl bl, 1
	    mov [argPtr + bx], di
	    
	    pop bx
	    ret
	putAddress endp

	getArg proc ; returns argument offset
	; entry: DL = argument index 
	; return: AX = argument offset
    push bx
    xor bx, bx
    mov bl, dl
    shl bl, 1 
    mov ax, [argPtr + bx] 
    
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
  	int 21h
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

  fileInit proc
  	push ax
  	push dx

  	mov dx, offset inputName
		; open input file in read mode
		xor al, al
		call open
		mov inputFile, ax

		mov dx, offset outputName
		; create output file
		call create
		; open output file in write mode
		mov al, 1
		call open
		mov outputFile, ax

		pop dx
		pop ax
		ret
  fileInit endp

  bufferInit proc ; moves buffer pointer to beginning of buffer
  	push ax
  	mov ax, offset inputBuffer
  	mov inputBufferPtr, ax
  	mov inputBufferEndPtr, ax

  	mov ax, offset outputBuffer
  	mov outputBufferPtr, ax
  	add ax, bufferSize
  	mov outputBufferEndPtr, ax

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
  	mov dx, inputFile
  	call close
  	mov dx, outputFile
  	call close
    mov ax, 4C00h
    int 21h
  exit endp

code ends

stack1 segment stack
        dw 200 dup(?)
    top dw ?
stack1 ends

end start
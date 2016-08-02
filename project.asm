;this file is for microwave oven project

.include "m2560def.inc"

.def row = r16 ; current row number
.def col = r17 ; current column number
.def rmask = r18 ; mask for current row during scan
.def cmask = r19 ; mask for current column during scan
.def temp1 = r20
.def temp2 = r21
.equ PORTLDIR = 0xF0 ; PD7-4: output, PD3-0, input
.equ INITCOLMASK = 0xEF ; scan from the rightmost column,
.equ INITROWMASK = 0x01 ; scan from the top row
.equ ROWMASK = 0x0F ; for obtaining input from Port D

.def mode=r22				; 0 for entry mode
					; 1 for running mode 
					; 2 for paused mode
					; 3 for finishing mode 
.def debounceFlag=r23
.def inputCounter=r25

; The macro clears a word (2 bytes) in a memory
; the parameter @0 is the memory address for that word
.macro clear
	ldi YL, low(@0) 			; load the memory address to Y
	ldi YH, high(@0)
	clr temp2
	st Y+, temp2 				; clear the two bytes at @0 in SRAM
	st Y, temp2
.endmacro

.macro do_lcd_command
	ldi temp2, @0
	 
	rcall lcd_command
	rcall lcd_wait
.endmacro

.macro do_lcd_data
	mov temp2, @0
	;ori temp2, 0x30
	rcall lcd_data
	rcall lcd_wait
.endmacro

.macro do_lcd_char
	ldi temp2, @0
	rcall lcd_data
	rcall lcd_wait
.endmacro


.dseg
SecondCounter:
	.byte 2 					; Two-byte counter for counting seconds.
TempCounter:
	.byte 2 					; Temporary counter. Used to determine
								; if one second has passed
Minutes:
	.byte 1						; One-byte of storage for minutes
Seconds:						
	.byte 1						; One-byte of storage for seconds
CurrentRotationSymbol:			; One-byte of storage for current rotation symbol
	.byte 1
OpenCloseStatus:				; One-byte of storage for status of the door (opened or closed)
	.byte 1
CurrentPowerLevel:				; One-byte of storage for current power level 
	.byte 1


.cseg
.org 0x0000
	jmp RESET
.org INT0addr
	jmp EXT_INT0_CLOSE_DOOR
.org INT1addr
	jmp EXT_INT1_OPEN_DOOR
.org INT2addr
	jmp DEFAULT 				; No handling for IRQ2.
.org OVF0addr
	jmp Timer0OVF 				; Jump to the interrupt handler for
								; Timer0 overflow.
	jmp DEFAULT 				; default service for all other interrupts.

DEFAULT: reti 					; no service


RESET:
	clr r23
	clr r24
	clr mode
	clr inputCounter

	ldi temp1, low(RAMEND)		; initialize stack pointer
	out SPL, temp1
	ldi temp1, high(RAMEND)
	out SPH, temp1

	ser temp1					; set PORTA (LCD CONTROL) and PORTF (LCD DATA) as output
	out DDRF, temp1
	out DDRA, temp1
	clr temp1
	out PORTF, temp1
	out PORTA, temp1


	ldi temp1, PORTLDIR 		; PA7:4/PA3:0, out/in
	sts DDRL, temp1
	ser temp1 					; Set PORTC as output 
	out DDRC, temp1

	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_5ms
	do_lcd_command 0b00111000 ; 2x5x7
	rcall sleep_1ms
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00111000 ; 2x5x7
	do_lcd_command 0b00001000 ; display off?
	do_lcd_command 0b00000001 ; clear display
	do_lcd_command 0b00000110 ; increment, no display shift
	do_lcd_command 0b00001110 ; Cursor on, bar, no blink

	;Enable individual interupts for button 0 & 1
	ldi temp1, (0b10<<ISC20)|(0b10<<ISC10)|(0b10<<ISC00)
	sts EICRA, temp1
	ldi temp1, (1<<INT2)|(1<<INT1)|(1<<INT0)
	out EIMSK, temp1

	ldi temp1, '<'						; Set default position of turntable and store
	sts CurrentRotationSymbol, temp1

	ldi temp1, 'C'
	sts OpenCloseStatus, temp1

	ldi temp1, 255
	sts CurrentPowerLevel, temp1
	out PORTC, temp1

	clr temp1							; Initialize timer to 00:00
	sts Minutes, temp1
	sts Seconds, temp1
	rcall Print_Screen					; Print out information to screen


	clear TempCounter 					; Initialize the temporary counter to 0
	clear SecondCounter 				; Initialize the second counter to 0
	ldi temp2, 0b00000000				; Enable Timer0
	out TCCR0A, temp2				
	ldi temp2, 0b00000010
	out TCCR0B, temp2 					; Prescaling value=8
	ldi temp2, 1<<TOIE0 				; = 128 microseconds
	sts TIMSK0, temp2 					; T/C0 interrupt enable

	ser temp1
	out DDRE, temp1 			; bit 3 is the OC3B
	clr temp1
	sts PORTE, temp1

	ldi temp1, (1<<WGM32)|(1<<CS30)		; Enable Timer3 for fast PWM
	sts TCCR3B, temp1
	ldi temp1, (1<<WGM30)|(1<<COM3B1)
	sts TCCR3A, temp1

	clr temp1				;128 in decimal to set the cycle to half on half off
	sts OCR3BL, temp1
	clr temp1
	sts OCR3BH, temp1

	sei 					; Enable global interrupt
	
	rjmp main



main:
	clr debounceFlag

Create_Mask:
	ldi	cmask, INITCOLMASK 	; initial column mask
	clr col 				; initial column
	
colloop:
	cpi col, 4
	breq main 				; If all keys are scanned, repeat.
	sts PORTL, cmask 		; Otherwise, scan a column.

	ldi temp1, 0xFF 		; Slow down the scan operation.
delay:
	dec temp1
	brne delay

	lds temp1, PINL 		; Read PORTL
	andi temp1, ROWMASK 	; Get the keypad output value
	cpi temp1, 0xF 			; Check if any row is low
	breq nextcol			; If yes, find which row is low

	ldi rmask, INITROWMASK  ; Initialize for row check
	clr row
 
rowloop:
	cpi row, 4
	breq nextcol 			; the row scan is over.
	mov temp2, temp1
	and temp2, rmask 		; check un - masked bit
	breq convert 			; if bit is clear, the key is pressed
	inc row 				; else move to the next row
	lsl rmask
	jmp rowloop

nextcol:    				; if row scan is over
	lsl cmask
	inc col 				; increase column value
	jmp colloop				; go to the next column



convert:
;	cpi debounceFlag, 1
;	breq Create_Mask
;	ldi debounceFlag, 1

	rcall sleep_100ms
	rcall sleep_100ms
	cpi col, 3 				; If the pressed key is in col.3
	breq letters 			; we have a letter

	; If the key is not in col.3 and
	cpi row, 3 				; If the key is in row3,
	breq symbols 			; we have a symbol or 0

	mov temp1, row 			; Otherwise we have a number in 1-9
	lsl temp1
	add temp1, row
	add temp1, col 			; temp1 = row*3 + col
	subi temp1, -1 			; Add the value of character ??
	
	cpi mode, 4
	brne Dont_Set_Power_Level
	rcall Set_Power_Level
	rjmp Create_Mask

	Dont_Set_Power_Level:
	rcall Read_Timer_Input
	rcall Print_Screen
	jmp Create_Mask

letters:
	ldi temp1, 'A'
	add temp1, row 				; Get the ASCII value for the key
	cpi mode, 4
	breq Create_Mask
	cpi temp1, 'D'				; Subtract 30 seconds from timer
	breq Less_30_Seconds	
	cpi temp1, 'C'				; Add 30 seconds to timer
	breq More_30_Seconds
	cpi temp1, 'B'
	breq Create_Mask
	cpi mode, 0
	brne Do_Nothing
	rcall Enter_Power_Selection ; If we get here, must be here, enter power select
	Do_Nothing:
	jmp Create_Mask

symbols:
	cpi mode, 4
	breq Do_Nothing
	cpi col, 0 				; Check if we have a star
	breq star
	cpi col, 1 				; or if we have zero
	breq zero
	ldi temp1, '#' 			; if not we have hash
	jmp hash

zero:
	ldi temp1, 0 			; Set to zero
	rcall Read_Timer_Input	; Read 0 into timer
	rcall Print_Screen		; Print information to screen
	jmp Create_Mask

hash:
	cpi mode, 0				; Stop button pressed in entry mode
	breq Stop_In_Entry_Mode
	cpi mode, 1				; Stop button pressed in running mode
	breq Stop_In_Running_Mode
	cpi mode, 2				; Stop button pressed in stop mode
	breq Stop_In_Stopped_Mode
	cpi mode, 3				; Stop button pressed in finished mode
	breq Stop_In_Finished_Mode	
	jmp Create_Mask

More_30_Seconds:			; If C is pressed, add 30 seconds
	cpi mode, 1
	brne convert_end
	ldi temp2, 30
	rcall add_to_seconds
	rcall Print_Screen
	jmp Create_Mask
	
Less_30_Seconds:			; If D is pressed, subtract 30 seconds
	cpi mode, 1
	brne convert_end
	ldi temp2, -30
	rcall add_to_seconds
	rcall Print_Screen
	jmp Create_Mask

Stop_In_Entry_Mode:			; If hash is pressed in entry mode, clear input
	clr temp2
	sts Seconds, temp2		; Clear seconds 
	sts Minutes, temp2		; Clear minutes
	clr inputCounter		; Reset input counter
	rcall Print_Screen		; Print information to screen
	jmp Create_Mask


Stop_In_Running_Mode:		; If hash is pressed in running mode, pause microwave
	ldi mode, 2
	clr temp2
	sts OCR3BL, temp2
	jmp Create_Mask

Stop_In_Stopped_Mode:		; If hash is pressed in paused mode, return to entry
	clr mode
	clr inputCounter
	sts Seconds, inputCounter
	sts Minutes, inputCounter
	rcall Print_Screen
	jmp Create_Mask

Stop_In_Finished_Mode:		; If hash is pressed in paused mode, return to entry
	jmp Stop_In_Stopped_Mode

star:
	lds temp2, OpenCloseStatus
	cpi temp2, 'O'
	breq convert_end
	cpi mode, 1				; Start button pressed in running mode
	breq Star_In_Running_Mode
	cpi mode, 2
	breq Star_In_Paused_Mode
	ldi mode, 1
	cpi inputCounter, 0		; Start button pressed with no entry
	breq Star_With_No_Input
	jmp Create_Mask

Star_In_Paused_Mode:
	ldi mode, 1
	jmp Create_Mask

Star_In_Running_Mode:		; If * is pressed in running mode, add 1 minute to timer
	ldi temp2, 60
	rcall add_to_seconds
	jmp Create_Mask

Star_With_No_Input:			; If * is pressed in entry mode with no input, add 1 minute to timer
	ldi temp2, 60
	rcall add_to_seconds
	ldi inputCounter, 4
	jmp Create_Mask

convert_end:			
	jmp Create_Mask 		; Restart main loop


Set_Power_Level:
	cpi temp1, 4
	brge Dont_Set_Power
	cpi temp1, 0
	breq Dont_Set_Power
	cpi temp1, 1
	breq Set_Power_Level_1
	cpi temp1, 2
	breq Set_Power_Level_2
	cpi temp1, 3
	breq Set_Power_Level_3
	Dont_Set_Power:
	ret

Set_Power_Level_1:
	ldi temp1, 0b11111111
	out PORTC, temp1
	ldi temp1, 255
	sts CurrentPowerLevel, temp1
	ldi mode, 0
	rcall Print_Screen
	ret

Set_Power_Level_2:
	ldi temp1, 0b00001111
	out PORTC, temp1
	ldi temp1, 255/2
	sts CurrentPowerLevel, temp1
	rcall Print_Screen
	ldi mode, 0
	ret

Set_Power_Level_3:
	ldi temp1, 0b00000011
	out PORTC, temp1
	ldi temp1, 255/4
	sts CurrentPowerLevel, temp1
	ldi mode, 0
	rcall Print_Screen
	ret



Read_Timer_Input:				; When an a number is pressed in entry mode, read it in to Minute/Seconds memory
	push r23
	push r24
	lds r23, Minutes			; Read in current Minutes/Seconds
	lds r24, Seconds
	cpi inputCounter, 0			; Check whether current input is 1st
	breq Read_First_Input
	cpi inputCounter, 1			; Check whether current input is 2nd
	breq Read_Second_Input
	cpi inputCounter, 2			; Check whether current input is 3rd
	breq Read_Third_Input
	cpi inputCounter, 3			; Check whether current input is 4th
	breq Read_Fourth_Input
	rjmp Finish_Reading_Input	; If inputCounter > 3 (more than 4 inputs given) ignore further input


Read_First_Input:				; If first input, store in seconds and finish
	mov r24, temp1
	inc inputCounter
	rjmp Finish_Reading_Input


Read_Second_Input:				; If second input, multiply seconds by 10 and add
	ldi temp2, 10
	mul r24, temp2
	mov r24, r0
	add r24, temp1
	inc inputCounter
	rjmp Finish_Reading_Input


Read_Third_Input:				; If third input, shift first digit of second to minutes,
	clr temp2					; multiply by 10 and add input to seconds

	Get_10s:					; Keep subtracting 10 until r24 < 10
	cpi r24,10
	brlt Less_Than_10			; Jump when r24 < 10
	subi r24,10					
	inc temp2
	rjmp Get_10s

	Less_Than_10:				; When r24 < 10, move the quotient into minutes
	mov r23, temp2
	ldi temp2, 10
	mul r24, temp2				; Multiply remainder by 10
	mov r24, r0
	add r24, temp1				; Add input to remainder
	inc inputCounter
	rjmp Finish_Reading_Input


Read_Fourth_Input:				; If fourth input, multiply minutes by 10 and shift first digit of 
	ldi temp2, 10				; Seconds to Minutes, then multiply seconds by 10 and add
	mul r23, temp2
	mov r23, r0					; Multiply Minutes by 10
	clr temp2

	Get_10s_2:					; Keep subtracting 10 until r24 < 10
	cpi r24,10
	brlt Less_Than_10_2
	subi r24,10
	inc temp2
	rjmp Get_10s_2

	Less_Than_10_2:				; Jump when r24 < 10
	add r23, temp2
	ldi temp2, 10
	mul r24, temp2				; Multiply remainder by 10
	mov r24, r0
	add r24, temp1				; Add input to remainder
	inc inputCounter
	rjmp Finish_Reading_Input

Finish_Reading_Input:
	sts Minutes, r23			; Store results into Minutes and Seconds
	sts Seconds, r24
	pop r24
	pop r23
	ret


Print_2_Digit_Number:			; Prints out a 2 digit number that is passed in via r23
	clr temp1

	Get_First_Digit:
	cpi r23,10
	brlt PrintOut
	subi r23,10
	inc temp1
	rjmp Get_First_Digit

	PrintOut:
	subi temp1, -48
	subi r23, -48
	do_lcd_data temp1
	do_lcd_data r23

	ret



Print_Screen:						; Print Timer, Rotation Symbol and Open/Closed Status to screen
	do_lcd_command 0b00000001		; Clear Screen
	push r23
	push temp1

	lds r23, Minutes				; Print out Minutes 
	rcall Print_2_Digit_Number

	ldi temp1, ':'					; Print dividing ':'
	do_lcd_data temp1				; ASCII FOR ':' is 58

	lds r23, Seconds				
	rcall Print_2_Digit_Number		; Print out Seconds

	; Move cursor to top right hand corner
	do_lcd_char ' '
	do_lcd_char ' '
	do_lcd_char ' '
	do_lcd_char ' '


	lds temp1, currentRotationSymbol
	do_lcd_data temp1				; Print out current rotation symbol

	do_lcd_char ' '
	do_lcd_char ' '
	do_lcd_char ' '
	do_lcd_char ' '
	do_lcd_char ' '
	//do_lcd_command 0b11000000
	
	lds temp1, OpenCloseStatus
	do_lcd_data temp1

	pop temp1
	pop r23
	ret

EXT_INT0_CLOSE_DOOR:
	ldi temp1, 'C'
	sts OpenCloseStatus, temp1

	cpi mode, 3
	brne Close_Door_In_Other_Mode
	clr inputCounter
	ldi mode, 0
	reti

	Close_Door_In_Other_Mode:
	;ldi mode, 1				; if there is still time left, unpause microwave
	rcall Print_screen
	
	reti

EXT_INT1_OPEN_DOOR:
	clr temp2
	sts OCR3BL, temp2
	ldi temp1, 'O'
	sts OpenCloseStatus, temp1
 	rcall Print_screen

	cpi mode, 3
	brne Close_Door_In_Other_Mode2
	reti 

	Close_Door_In_Other_Mode2:
	ldi mode, 2
	reti



Timer0OVF: 						; Interrupt subroutine for Timer0
	in temp2, SREG
	push temp2 					; Prologue starts.
	push YH 					; Save all conflict registers in the prologue.
	push YL
	push XH
	push XL
	push r25
	push r24 					; Prologue ends.
								; Load the value of the temporary counter.
	lds r24, TempCounter
	lds r25, TempCounter+1
	adiw r25:r24, 1 			; Increase the temporary counter by one.
	cpi r24, low(7812) 			; Check if (r25:r24) = 7812
	ldi temp2, high(7812) 		; 7812 = 106/128
	cpc r25, temp2
	brne NotSecond

	lds XL, SecondCounter		
	cpi XL, 5					; Check how many seconds have passed

	brne Not_5_Seconds
		
	rcall getNewRotationSymbol	; If 5 seconds has passed, change the rotation symbol (if in entry mode)

	clear SecondCounter

Not_5_Seconds:
	lds r23, Minutes
	lds r24, Seconds
	
	cpi mode, 1					; If in running mode, keep counting
	brne Keep_Counting
	cpi mode, 0
	breq Keep_Counting
	cpi r24, 0					; If seconds has gone to 0, check if minutes has gone to 0
	brne Keep_Counting
	cpi r23, 0					; If seconds and minutes have counted down to 0, enter finished mode
	breq Enter_Finished_Mode

	
Keep_Counting:
	cpi mode, 1					; If not in running mode, increase tempCounter and exit
	brne IncreaseCounter
	lds temp2, CurrentPowerLevel
	sts OCR3BL, temp2

	mov temp2, r23
	add temp2, r24
	cpi temp2, 0
	breq IncreaseCounter		; If both Minutes and Seconds are 0, do nothing
	cpi r24, 0 
	breq decMinute				; If seconds has counted down to 0, decrement Minutes and set Seconds to 59
	dec r24						; If not, just decrement Seconds
	sts Seconds, r24			; Store Seconds
	rcall Print_Screen			; Print information to screen

	rjmp IncreaseCounter		; Increase tempCounter and exit

decMinute:
	dec r23						; Decrement Minutes
	ldi r24, 59					; Load 59 into Seconds
	sts Minutes, r23			; Store Minutes and Seconds
	sts Seconds, r24
	rcall Print_Screen			; Print information to screen

IncreaseCounter:
	clear TempCounter 			; Reset the temporary counter.
	lds r24, SecondCounter
	lds r25, SecondCounter+1
	adiw r25:r24, 1 			; Increase the second counter by one.

	sts SecondCounter, r24
	sts SecondCounter+1, r25
	rjmp EndIF

NotSecond:
								; Store the new value of the temporary counter.
	sts TempCounter, r24
	sts TempCounter+1, r25
EndIF:

	pop r24 					; Epilogue starts;
	pop r25 					; Restore all conflict registers from the stack.
	pop XL
	pop XH
	pop YL
	pop YH
	pop temp2
	out SREG, temp2
	reti 


Enter_Finished_Mode:
	ldi mode, 3					; Put mode into Finished Mode
	do_lcd_command 0b00000001	; Clear Screen
	clr temp1
	sts OCR3BL, temp1

	do_lcd_char 'D' 			; Print finished message to screen
	do_lcd_char 'O' 
	do_lcd_char 'N' 
	do_lcd_char 'E' 

	
	do_lcd_char ' '
	do_lcd_char 'R' 
	do_lcd_char 'E' 
	do_lcd_char 'M' 
	do_lcd_char 'O' 
	do_lcd_char 'V' 
	do_lcd_char 'E' 
	do_lcd_char ' ' 
	do_lcd_char 'F' 
	do_lcd_char 'O' 
	do_lcd_char 'O' 
	do_lcd_char 'D' 
	
	//do_lcd_command 0b11000000	; Print new line
	
	rjmp IncreaseCounter


getNewRotationSymbol:			; Get new rotation symbol, depending on current symbol and direction
	cpi mode, 1
	brne dontChangeRotationSymbol
	lds temp1, CurrentRotationSymbol
	cpi temp1, '<'
	breq Up
	cpi temp1, '^'
	breq Right
	cpi temp1, '>'
	breq Down
	cpi temp1, 'V'
	breq Left


	Up:
	ldi temp1, '^'
	rjmp Store_New_Rotation_Symbol
	Right:
	ldi temp1, '>'
	rjmp Store_New_Rotation_Symbol
	Down:
	ldi temp1, 'V'
	rjmp Store_New_Rotation_Symbol
	Left:
	ldi temp1, '<'

	Store_New_Rotation_Symbol:
	sts CurrentRotationSymbol, temp1
	dontChangeRotationSymbol:
	ret


Enter_Power_Selection:
	ldi mode, 4
	do_lcd_command 0b00000001	; Clear Screen
	
	do_lcd_char 'S' 			
	do_lcd_char 'E' 
	do_lcd_char 'T' 
	do_lcd_char ' '
	do_lcd_char 'P' 
	do_lcd_char 'O' 
	do_lcd_char 'W'
	do_lcd_char 'E' 
	do_lcd_char 'R' 
	do_lcd_char ' '
	do_lcd_char '1' 
	do_lcd_char '/' 
	do_lcd_char '2'
	do_lcd_char '/' 
	do_lcd_char '3' 

	ret



.equ LCD_RS = 7
.equ LCD_E = 6
.equ LCD_RW = 5
.equ LCD_BE = 4

.macro lcd_set
	sbi PORTA, @0
.endmacro
.macro lcd_clr
	cbi PORTA, @0
.endmacro

;
; Send a command to the LCD (r16)
;

lcd_command:
	out PORTF, temp2
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	ret

lcd_data:
	out PORTF, temp2
	lcd_set LCD_RS
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	lcd_clr LCD_E
	rcall sleep_1ms
	lcd_clr LCD_RS
	ret

lcd_wait:
	push temp2
	clr r16
	out DDRF, temp2
	out PORTF, temp2
	lcd_set LCD_RW
lcd_wait_loop:
	rcall sleep_1ms
	lcd_set LCD_E
	rcall sleep_1ms
	in temp2, PINF
	lcd_clr LCD_E
	sbrc temp2, 7
	rjmp lcd_wait_loop
	lcd_clr LCD_RW
	ser temp2
	out DDRF, temp2
	pop temp2
	ret



.equ F_CPU = 16000000
.equ DELAY_1MS = F_CPU / 4 / 1000 - 4
; 4 cycles per iteration - setup/call-return overhead

sleep_1ms:
	push r24
	push r25
	ldi r25, high(DELAY_1MS)
	ldi r24, low(DELAY_1MS)
delayloop_1ms:
	sbiw r25:r24, 1
	brne delayloop_1ms
	pop r25
	pop r24
	ret

sleep_5ms:
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	rcall sleep_1ms
	ret

sleep_100ms:
	push r19
	clr r19

	loop:
	inc r19
	rcall sleep_5ms
	cpi r19, 20
	brlt loop

	pop r19
	ret




add_to_seconds:
	push r23
	push r24
	lds r23, Minutes
	lds r24, Seconds
	add r24, temp2
	cpi r24, 60
	brge greaterThan
	cpi r24, 0
	brlt negative
	sts Seconds, r24
	jmp finish
greaterThan:
	ldi temp1, 60
	sub r24, temp1
	inc r23
	cpi r23, 100
	brlt finish
	ldi r23, 99
	ldi r24, 99
	jmp finish
negative:
	cpi r23, 1
	brlt cantSubtract
	ldi temp1, 59
	add r24, temp1
	sub temp1, r23
	mov r24, temp1
	dec r23
	jmp finish

cantSubtract:
	;ldi r23, 0
	ldi r24, 1

finish:
	sts Minutes, r23
	sts Seconds, r24
	pop r24
	pop r23
	ret

halt:
	rjmp halt







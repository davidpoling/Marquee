;David Poling
;Program 2
;Program to draw a box on the console window and display several messages in a marquee style.

StackSegment SEGMENT STACK

        DW 256 DUP(?)                    ;Allocate a sufficient stack size for the program to use with data.
        
StackSegment ENDS

;--------------------------------------------------------------------------------------

Data SEGMENT

        upperLeft       DW 160 * 4 + 24 ;The offset in screen memory of where the upper-left corner of the box is located.
        upperRight      DW 0            ;Variable for the upper-right corner of the box to be used. 
        lineBorder      DB 1            ;1 uses double-line border.
        foreGround      DB 00001100b    ;Background and foreground colors to be used for the box.
        backGround      DB 00000000b
        boxWidth        DB 20           ;Box width of 20.
        prevTicks       DW 0            ;Variable for the previous tick count.
        ticksPerScroll  DW 18           ;The amount of ticks per scroll.
        prevTicksPerScroll DW (?)
        elapsedTicks    DW 0            ;Elapsed ticks.
        isPaused        DB 0            ;Check whether the F1 key is struck. If 1, pause scrolling. Otherwise, resume.
            
        leftShiftDown   DB 0            ;Two flags for testing the right and left shift status.
        rightShiftDown  DB 0            ;These are used so that the width of the box changes by 1 unit at a time.
        
        sentence        DB      'The travelling salesman will never find the shortest path.     ', 0    ;Sentence used to scroll through the box.
        sentenceOffset  DW (?)          ;Offset constant used for the very end of the sentence. Also is the value of the location counter.
        
        currSentPos     DW 160 * 6 + 26 ;Location of the beginning of the sentence. Starts initially at left edge of the inside of the box.
        sentenceColor   DB 00001100b    ;Color for the sentence.
        
        scrMem          EQU 0B800h      ;Screen memory.

Data ENDS

;--------------------------------------------------------------------------------------

Code SEGMENT
Assume DS:Data, CS:Code

main PROC

        MOV AX, Data            ;Set up the data segment.
        MOV DS, AX
        
        MOV AX, scrMem          ;Set up the screen in the ES register.
        MOV ES, AX
        
        MOV sentenceOffset, OFFSET sentence     ;sentenceOffset has the offset of the sentence.
        CALL clearScreen        ;Clear the screen before drawing.
        CALL drawBox
        CALL displayMsg
        
        MOV AH, 00h             ;Get the ticks and store it in CX:DX
        INT 1Ah
        MOV prevTicks, DX       ;Store the ticks into prevTicks to calculate in isTimeToScroll PROC
     
scrollText:
       
        CALL isTimeToScroll
        
        CMP AL, 1               ;Is it time to scroll?
        JNE getStatus           ;If not, get the next key command. 

        CMP isPaused, 1			;Is the text currently paused?
        JE getStatus			;If so, we don't want to update. Just get the next key command.

        CALL displayMsg
        INC sentenceOffset      
        CMP sentenceOffset, OFFSET sentenceOffset
        JNE getStatus
        MOV sentenceOffset, OFFSET sentence     ;sentenceOffset has the offset of the sentence.

getStatus:

        CALL testForLeftShift   ;PROC to decrease the size of the box if the left shift was pressed.
        CALL testForRightShift  ;PROC to increase the size of the box when the right shift is pressed.
        
        MOV AH, 11h             ;Is there a key ready in the buffer?            
        INT 16h
        JZ scrollText           ;If no key ready, loop again.
        
        MOV AH, 10h             ;Get the key
        INT 16h 
        
        CMP AL, 'd'             ;Is the key pressed a 'd'?
        JE goRight              ;If so, jump to the goRight label to call moveRight PROC.
        
        CMP AL, 'a'             ;Is the key pressed an 'a'?
        JE goLeft               ;If so, jump to the goLeft label to call moveLeft PROC.
        
        CMP AL, 'w'             ;Is the key pressed a 'w'?
        JE goUp                 ;If so, move the box up.
        
        CMP AL, 's'             ;Is the key pressed an 's'?
        JE goDown               ;If so, move the box down.
        
        CMP AL, 'f'             ;Is the key pressed an 'f'?
        JE changeForeground     ;If so, change the foreground color of the box.
        
        CMP AL, 'b'             ;Is the key pressed a 'b'?
        JE changeBackground     ;If so, change the background color of the box.
        
        CMP AH, 3Bh             ;Is the F1 key struck?
        JE pauseText            ;If so, pause the text. 

        CMP AH, 8Dh             ;Is the CRTL-Up key struck?
        JE speedUpText          ;If so, speed the text up.

        CMP AH, 91h             ;Is the CRTL-Down key struck
        JE slowDownText         ;If so, slow the text down.

        CMP AL, 27              ;Is the key escape?
        JE done            		;If so, exit the program.
        
        JMP scrollText          ;Loop back to get the scrolling status.

goRight:
        
        CALL moveRight          ;Call the PROC to move the box to the right 1 unit.     
        JMP scrollText          ;Loop back to get the scrolling status.

goLeft:
        
        CALL moveLeft           ;Call the PROC to move the box to the left 1 unit.
        JMP scrollText          ;Loop back to get the scrolling status.
        
goUp:

        CALL moveUp             ;Same as previous code for moving left and right.
        JMP scrollText
        
goDown:

        CALL moveDown           ;Same as previous code for other box manipulation.
        JMP scrollText
        
changeForeground:
        
        CALL changeForegroundColor	;Call the PROC to change the foreground color of the box.
        JMP scrollText

changeBackground:

        CALL changeBackgroundColor	;Call the PROC to change the background color of the box.
        JMP scrollText

pauseText:

        CMP isPaused, 0			;Is the text already paused?
        JE setPausedTrue		;If not, set the paused status to being currently paused.
        MOV isPaused, 0        	;Else, it must be already paused. In that case, set the paused status to currently not paused.
        JMP scrollText

setPausedTrue:
        
        MOV isPaused, 1       
        JMP scrollText

speedUpText:

        CALL speedUP			;PROC to speed the text up by 80%
        JMP scrollText

slowDownText:

        CALL slowDown			;PROC to slow the text down by 120%
        JMP scrollText
        
done:

        MOV AH, 4Ch             ;Return execution to DOS.
        INT 21h
        
main ENDP
;=======================================================================
clearScreen PROC
        ;Clears the screen for visual purposes.
        PUSH DI

        MOV DI, 4000

screenLoop:

        CMP DI, 0
        JL endScreenLoop
        MOV ES:[DI], WORD PTR 0720h
        SUB DI, 2
        JMP screenLoop

endScreenLoop:
        
        POP DI
        RET
        clearScreen ENDP

;=======================================================================
drawBox PROC
        ;Draws the upper-left corner of the box initially. 
        ;Each label draws different portions of the box.       
        PUSH SI AX CX
        
        MOV SI, upperLeft       ;SI is used as the starting position to draw the box.
        MOV CL, 0               ;Accumulator to ensure we draw the width of the box.
        
upperLeftCorner:
        
        MOV AL, 0C9h            ;AX contains line drawing ASCII code for an upper-left corner.
        MOV AH, foreGround
        ADD AH, backGround
        MOV ES:[SI], AX
        ADD SI, 2               ;Move to next position to begin drawing the top of the box.
        
topLines:

        CMP CL, boxWidth        ;Are we at the edge of the box?
        JE upperRightCorner     ;If so, done drawing the top of the box.
        
        MOV AL, 0CDh            ;AX contains line drawing ASCII code for a horizontal straight line.
        MOV ES:[SI], AX
        ADD SI, 2
        INC CL
        JMP topLines
        
upperRightCorner:
        
        MOV AL, 0BBh            ;AX contains the upper-right corner. This needs to be created before the sides.
        MOV ES:[SI], AX
        MOV SI, upperLeft       ;Move SI back to the starting position of the upper-left corner.
        ADD SI, 160             ;Move to the next row to begin drawing the left side.
        MOV CL, 0               ;Reset the counter.
        
leftSide:

        CMP CL, 3               ;Counter reached 3 for 3-line tall box?
        JE bottomLeftCorner     ;If so, begin drawing the bottom-left corner.
        
        MOV AL, 0BAh            ;AX contains line drawing ASCII code for vertical straight line.
        MOV ES:[SI], AX
        ADD SI, 160
        INC CL
        JMP leftSide

bottomLeftCorner:

        MOV AL, 0C8h            ;AX contains the bottom-left corner.
        MOV ES:[SI], AX
        ADD SI, 2
        MOV CL, 0               ;Reset the counter.

bottomLines:

        CMP CL, boxWidth        ;Are we at the edge of the box?
        JE bottomRightCorner    ;If so, done drawing the bottom.
        
        MOV AL, 0CDh            ;AX contains line drawing ASCII code for a horizontal straight line.
        MOV ES:[SI], AX
        ADD SI, 2
        INC CL
        JMP bottomLines

bottomRightCorner:

        MOV AL, 0BCh            ;AX contains bottom-right corner.
        MOV ES:[SI], AX
        SUB SI, 160             ;Move back a row to begin drawing the right side.
        MOV CL, 0
        
rightSide:

        CMP CL, 3               ;Counter reached 3 for 3-line tall box?
        JE doneDrawing          ;If so, completely done drawing!
        
        MOV AL, 0BAh            ;AX contains line drawing ASCII code for a vertical straight line.
        MOV ES:[SI], AX
        SUB SI, 160
        INC CL
        JMP rightSide
        
doneDrawing:
        
        CALL colorMiddle
        POP CX AX SI
        RET
        drawBox ENDP
;=======================================================================
colorMiddle PROC
        ;PROC to color the interior of the box since the coloring of the box has already been taken care of.       
        PUSH SI AX
        
        MOV SI, upperLeft		;SI points to the upper-left corner of the interior of the box.
        ADD SI, 162				;Hence the reason why 162 is added to the upper-left corner of the total box.
        MOV AH, backGround
        MOV AL, 0

        MOV CL, boxWidth		;Counter used for coloring each row by a width of boxWidth.
        
firstRow:

        MOV ES:[SI], AX			;Continue to color the first row until the edge of the box has been reached.	
        ADD SI, 2
        LOOP firstRow
        
        MOV SI, upperLeft		;Reposition SI to where we started.
        ADD SI, 162
        ADD SI, 160				;Add 160 to SI to move to the next row to begin coloring it.
        MOV CL, boxWidth		;Reset the counter.
        
secondRow:

        MOV ES:[SI], AX			;Same logic as coloring the first row.
        ADD SI, 2
        LOOP secondRow
        
        MOV SI, upperLeft
        ADD SI, 162
        ADD SI, 160 * 2			;This time, move SI to the third row.
        MOV CL, boxWidth
        
thirdRow:

        MOV ES:[SI], AX			;Repeat.
        ADD SI, 2
        LOOP thirdRow
        
        POP AX SI
        RET
        colorMiddle ENDP
;=======================================================================
moveRight PROC
        ;Shifts the box to the right after the right arrow key has been struck.
        ;On exit, box will be 1 unit shifted right.
        
        PUSH AX SI BX DX
        
        MOV SI, upperLeft
        
        CALL rightBoundaryTest  ;PROC to test if the box is going out of bounds.
        
        CMP DX, 0               ;Is (upperRight + 2) % 160 = 0?
        JE doneMovingRight      ;If so, we are at the right border of the screen.
        
        ADD SI, 2               ;Move the upper left corner over 1
        ADD currSentPos, 2      ;Also, move the sentence position to the right 1 unit so that the sentence stays in the box.
        MOV upperLeft, SI       ;Re-arrange the upper left corner.
        CALL clearScreen        ;Re-call clearScreen and drawBox to get the new instance of the box.
        CALL drawBox
        CALL displayMsg
        
        
doneMovingRight:

        POP DX BX SI AX
        RET
        moveRight ENDP
;=======================================================================
moveLeft PROC
        ;Shifts the box to the left after the left arrow key has been struck.
        ;On exit, box will be 1 unit shifted left.
        
        PUSH AX SI BX DX
        
        MOV SI, upperLeft
        
        MOV BX, 160             ;160 used for the divisor to ensure the left boundary isn't exceeded.
        MOV AX, SI
        MOV DX, 0               ;Going to divide the upper-left corner by 160 to test whether upper-left divides evenly. If so,
                                ;we know we are at the left-edge of the screen.
        DIV BX                  ;AX = DX:AX / BX
                                ;DX = DX:AX % BX
        
        CMP SI, 0               ;At the left edge of the screen?
        JL doneMovingLeft       ;If so, don't move to the left at all for boundary issues.
        
        CMP DX, 0               ;If the remainder is 0, then we are at the left edge of the screen (multiple of 160).
        JE doneMovingLeft
        
        SUB SI, 2               ;Move the upper left corner to the left by 1.
        SUB currSentPos, 2      ;Also, move the sentence position to the left 1 unit so that the sentence stays in the box.
        MOV upperLeft, SI       ;Re-arrange the upper left corner.
        CALL clearScreen        ;Re-call clearScreen and drawBox to get the new instance of the box.
        CALL drawBox
        CALL displayMsg
        
doneMovingLeft:
        
        POP DX BX SI AX
        RET
        moveLeft ENDP
;=======================================================================
moveUp PROC
        ;Shifts the box up 1 unit after the "up" arrow key has been struck.
        ;On exit, box will be 1 unit shifted up.
        PUSH SI DI
        
        MOV SI, upperLeft
        MOV DI, currSentPos
        SUB SI, 160             ;Move the upper left corner up by 1 unit.
        SUB DI, 160             ;Also, move the sentence position up 1 unit so that the sentence stays in the box.
        CMP SI, 0               ;At the top edge of the screen?
        JL doneMovingUp         ;If so, done moving the box up.
        
        MOV upperLeft, SI
        MOV currSentPos, DI
        CALL clearScreen
        CALL drawBox
        CALL displayMsg
        
doneMovingUp:

        POP DI SI
        RET
        moveUp ENDP
;=======================================================================
moveDown PROC
        ;Shifts the box down 1 unit after the "down" arrow key has been struck.
        ;On exit, box will be 1 unit shifted down.
        PUSH SI DI
              
        MOV SI, upperLeft
        MOV DI, currSentPos
        ADD SI, 160             ;Move the upper left corner down by 1 unit.
        ADD DI, 160             ;Also, move the sentence position down 1 unit so that the sentence stays in the box.
        CMP SI, 160 * 21        ;At the bottom edge of the screen?
        JGE doneMovingDown      ;If so, done moving the box down.
        
        MOV upperLeft, SI
        MOV currSentPos, DI
        CALL clearScreen
        CALL drawBox
        CALL displayMsg
        
doneMovingDown:
        
        POP DI SI
        RET
        moveDown ENDP
;=======================================================================
testForLeftShift PROC
        ;Test for left shift being pressed. If so, decrease the size of the box by 1 and re-draw.       
        PUSH AX
        
        MOV AH, 12h             ;Get the shift status.
        INT 16h
        
        TEST AL, 00000010b      ;Test for left shift down.
        JZ clearLeftShiftDown   ;Left shift is not pressed, set the flag for left shift being not down.
        
        CMP leftShiftDown, 1    ;Was the left shift key previously down?
        JGE leftShiftDone       ;If so, no resizing.
        
        MOV AL, boxWidth        ;If the left shift is presssed, decrease size of box.
        CMP AL, 2               ;Is the boxwidth less than or equal to 2?
        JLE leftShiftDone       ;If so, done processing shift.
        DEC boxWidth            ;Decrease the size of the box.
        MOV leftShiftDown, 1    ;Set the flag for left shift being down to true.
        
        CALL clearScreen        ;Clear the screen and re-draw the box.
        CALL drawBox
        CALL displayMsg
        JMP leftShiftDone
        
clearLeftShiftDown:

        MOV leftShiftDown, 0    ;If the key is not down, set the flag.
        
leftShiftDone:
        
        POP AX
        RET
        testForLeftShift ENDP
;=======================================================================
testForRightShift PROC
        ;Test for the right shift being pressed. If so, increase the size of the box by 1 and re-draw.
        PUSH AX SI DX

        MOV AH, 12h             ;Get the shift status.
        INT 16h
        
        MOV SI, upperLeft       ;Going to need to make sure boundary issues aren't happening.
        CALL rightBoundaryTest  ;See if the right boundary has been reached.
        
        CMP DX, 0               ;Are we at the right edge of the screen?
        JE rightShiftDone       ;If so, no resizing.
        
        TEST AL, 00000001b      ;Test for right shift down.
        JZ clearRightShiftDown  ;Right shift not pressed, set the flag for the right shift to not down.
        
        CMP rightShiftDown, 1   ;Was the right shift key previously down?
        JGE rightShiftDone      ;If so, no resizing needed.
        
        MOV AL, boxWidth        ;If the right shift is pressed, increase size of box.
        CMP AL, 40              ;Is the boxwidth greater than or equal to 40?
        JGE rightShiftDone      ;If so, done processing shift.
        INC boxWidth            ;Increment the width of the box.
        MOV rightShiftDown, 1   ;Set the flag for right shift being down to true.
        
        CALL clearScreen
        CALL drawBox
        CALL displayMsg
        
        JMP rightShiftDone
        
clearRightShiftDown:

        MOV rightShiftDown, 0   ;If the key is not down, set the flag.

rightShiftDone:
        
        POP DX SI AX
        RET
        testForRightShift ENDP
;=======================================================================
rightBoundaryTest PROC
        ;PROC to test whether the right corner of the box reaches the edge of the screen.
        ;One exit, DX will contain the remainder of the position on the screen.
        PUSH DI AX BX

        MOV DI, SI
        MOV Al, boxWidth        ;Since we're dealing with words, upper-right corner must be 2 * boxWidth.
        MOV BL, 2
        MUL BL                  ;AX = AL * BL
        
        ADD AX, 2               ;ADD 2 to AX due to the next cell not reaching the next row correctly.
        ADD DI, AX              ;DI points to the top-right corner of the box.
        MOV upperRight, DI      ;Upper-right now contains the position of the upper-right corner of the box.
        
        MOV BX, 160             ;BX will be used as a divisor for checking the border of the screen.
        MOV AX, upperRight
        ADD AX, 2               ;Test for next screen position being a multiple of 160.
        MOV DX, 0
        DIV BX                  ;AX = DX:AX / BX
                                ;DX = DX:AX % BX
        
        POP BX AX DI
        RET
        rightBoundaryTest ENDP
;=======================================================================
changeForegroundColor PROC
        ;PROC to change the foreground color when the 'f' key is pressed.
        PUSH AX

        MOV AL, foreGround      ;AL is used to manipulate the foreground.

        CMP AL, 15              ;Does the color = 15?
        JE dontChangeForeGround ;If so, revert back to 0 so as to not affect the background.
        
        INC AL                  ;Increment to cycle through all possible colors.
        MOV sentenceColor, AL
        JMP doneChangingForeGround

dontChangeForeGround:

        MOV AL, 0
        
doneChangingForeGround:

        MOV foreGround, AL
        CALL clearScreen
        CALL drawBox
        CALL displayMsg

        POP AX
        RET
        changeForegroundColor ENDP
;=======================================================================
changeBackgroundColor PROC
        ;PROC to change the background color when the 'b' key is pressed.
        PUSH AX

        MOV AL, backGround
        
        CMP AL, BYTE PTR 01110000b
        JGE dontChangeBackGround
        
        ADD AL, 16
        JMP doneChangingBackGround
        
dontChangeBackGround:
        
        MOV AL, 0

doneChangingBackGround:
        
        MOV backGround, AL
        CALL clearScreen
        CALL drawBox
        CALL displayMsg

        POP AX
        RET
        changeBackgroundColor ENDP
;=======================================================================
displayMsg PROC
        ;PROC to display the sentence inside of the box.               
        PUSH SI DI AX

        MOV SI, sentenceOffset  ;SI will be used as the offset for the sentence.
        MOV DI, currSentPos     ;Set DI to the beginning position to begin displaying the sentence.
        MOV CL, boxWidth        ;Set a boundary, in this case the width of the box, for which the sentence is displayed.
        MOV CH, 0
        MOV AH, sentenceColor   ;Load a color.
        ADD AH, backGround
        
dispLoop:

        MOV AL, [SI]            ;Move the character into AX.
        MOV ES:[DI], AX         ;Which is then moved into the position on the screen where the character is displayed.
        ADD DI, 2               ;Move to the next cell.
        INC SI                  ;Increment the offset to be able to grab the next character.
        CMP SI, OFFSET sentenceOffset;Are we at the end of the sentence?
        JNE dml                 ;If not, continue looping until the sentence has filled the interior of the box.
        LEA SI, sentence

dml:

        LOOP dispLoop

        POP AX DI SI
        RET
        displayMsg ENDP
;=======================================================================
isTimeToScroll PROC
        ;On exit
        ;AL = 1 if ready to scroll
        ;AL = 0 otherwise      
        PUSH DX CX BX
        
        MOV AH, 00h             ;Get the ticks and store them into CX:DX
        INT 1Ah
        
        SUB DX, prevTicks       ;DX will contain the elapsed ticks....DX = currTicks - prevTicks
        CMP DX, ticksPerScroll  ;Is the elapesd ticks less than or equal to ticks per scroll?
        JL notReadyToScroll     ;If so, we're not ready to scroll.
        
        MOV BX, prevTicks		;Add the rate of the scrolling to the previous ticks to move to the next time interval.
        ADD BX, ticksPerScroll
        MOV prevTicks, BX
        
        MOV AL, 1				;Move 1 into AL for the condition in MAIN.
        MOV elapsedTicks, DX    ;Re-store the new elapsed ticks value.
        JMP doneScrolling
        
notReadyToScroll:

        MOV AL, 0				;Return 0 in AL for the condition in MAIN.
        
doneScrolling:
        
        POP BX CX DX
        RET
        isTimeToScroll ENDP
;=======================================================================
speedUp PROC
		;PROC to speed up the scrolling of the text by 80% of its previous value.
		;Multiply the scrolling rate of the text by 80, and then divide by 100.
		;On exit, ticksPerScroll will be changed. 		
		PUSH AX BX DX
		
		MOV BX, 80
        MOV AX, ticksPerScroll

        MUL BX   			;DX:AX = AX * BX      
        MOV BX, 100
        DIV BX   			;AX = DX:AX / BX
							;DX = DX:AX % BX
        CMP AX, 0			;Is AX <= 0?
        JLE doneSpeedingUp	;If so, we don't want to speed up anymore.
        MOV ticksPerScroll, AX

doneSpeedingUp:

		POP DX BX AX
		RET
		speedUp ENDP
		
;=======================================================================
slowDown PROC
		;PROC to slow down the scrolling of the text by 120% of its previous value.
		;Multiply the scrolling rate of the text by 120, and then divide by 100.
		;On exit, ticksPerScroll will be changed.		
		PUSH AX BX DX
		
		CMP ticksPerScroll, 5
        JG dontAddToTicks
        INC ticksPerScroll

dontAddToTicks:
        
        MOV BX, 120
        MOV AX, ticksPerScroll

        MUL BX  		;DX:AX = AX * BX     
        MOV BX, 100
        DIV BX  		;AX = DX:AX / BX
						;DX = DX:AX % BX
        CMP AX, 3 * 18	;Is AX >= 3 seconds?
        JGE doneSlowingDown	;If so, we don't want to delay it longer.
        MOV ticksPerScroll, AX

doneSlowingDown:

		POP DX BX AX
		RET
		slowDown ENDP
;=======================================================================		
Code ENDS
;--------------------------------------------------------------------------------------

END main

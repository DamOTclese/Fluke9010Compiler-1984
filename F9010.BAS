
REM	*****************************************************************
REM	* This is the Fluke 9010 code compiler, written by Fredric Rice *
REM	* Date started: 27/Mar/84,   Date last update: 26/Aug/85.	*
REM	*								*
REM	*****************************************************************

REM	*****************************************************************
REM	* Updated: 11/Sep/84 for the inclusion of macros.		*
REM	* a possible 40 macros, a maximum of 100 lines long.		*
REM	*								*
REM     * Put in a ascii number to hex conversion ie "A": 23/Oct/84     *
REM	* Added binary number conversion routine: 02/Oct/84		*
REM	* Included sashings "|" to delimit program lines: 17/Sep/84	*
REM	* Symbolic program names added: 15/Sep/84			*
REM	* Includes added: 15/Sep/84					*
REM	* Fixed display commands too long error problem: 15/Sep/84	*
REM	* Removed original set-up parameter insertion: 14/Sep/84.	*
REM	* Added :NEXT to the end of program :equates: 17/Dec/84.	*
REM	* Added PRINT [ADDRESS] "STATEMENT" operation: 21/Dec/84.	*
REM	* Added :VARIABLE operations: 26/Dec/84.			*
REM	* Added :NOMACRO statement: 26/Dec/84.				*
REM	* Added: :FOR/:NEXT statements: 29/Dec/84.			*
REM	* Added :SETUP statements: 03/Jan/85.				*
REM	*    Setups that are allowed are:				*
REM	*    :SETUP DATA ERROR YES/NO					*
REM	*    :SETUP ADDRESS ERROR YES/NO				*
REM	*    :SETUP CONTROL ERROR YES/NO				*
REM	*    :SETUP ACTIVE FORCE YES/NO					*
REM	*    :SETUP ACTIVE INTERRUPT YES/NO				*
REM	*    :SETUP ILLEGAL ADDRESS YES/NO				*
REM	*    :SETUP BAD POWER YES/NO					*
REM	*    :SETUP BEEP ON/OFF						*
REM	*    :SETUP EXERCISE ERRORS YES/NO				*
REM	*								*
REM	* Removed :NOABORT, :CORRECT, :COMMENT statements: 17/Jan/85.	*
REM	* Removed [P] switch for printer: 17/Jan/85.			*
REM	* Added assembly code insertions: 21/Feb/85.			*
REM	* Removed DATA statements and put them into a file: 23/feb/85	*
REM	*								*
REM	* Correct the INCRIMENT to INCREMENT, DECRIMENT to DECREMENT,   *
REM	* SYNCRONIZE to SYNCHRONIZE, and removed RPEAT in favour of	*
REM	* REPEAT. 11/Mar/85.						*
REM	*								*
REM	* Added DEFM statements 17/Mar/85.				*
REM 	* Removed assembly language 22/may/85. 				*
REM 	*								*
REM	* Added block remark statements June 1985.			*
REM	* to be delimited by */ and /*.					*
REM	*								*
REM	* Includes removed 26/Aug/85. Will rely on MACRO'S.		*
REM	*								*
REM	*****************************************************************

	TRUE% = -1: FALSE% = 0: REVISION = 1.00
	CURRENT.PROG$ = "  ": CODE.COUNT% = 0: LOOP.ONE% =   0
	LOOP.TWO% =   0: NEXT.COUNT% = 0: VAL.COUNT% = 0
	SETUP% = 255: BEEP% = 1: EXERCISE% = 1

	REG.FLAG% =		FALSE%: SET.BINARY.END.FLAG% =  TRUE%
	TEST.FLAG% = 		FALSE%: SHOOT.FLAG% = 		FALSE%
	ABOVE.FLAG% = 		FALSE%: DATA.FLAG% = 		FALSE%
	END.REG.FLAG% = 	FALSE%
	APPEND.FLAG% = 		FALSE%: IF.FLAG% = 		FALSE%
	NOREG.FLAG% = 		FALSE%: RUN.FLAG% = 		FALSE%
	EQUATE.FLAG% = 		FALSE%: LEARN.FLAG% = 		FALSE%
	I.O.FLAG% = 		FALSE%: ATOG.FLAG% = 		FALSE%
	BUS.FLAG% = 		FALSE%: START.FLAG% = 		FALSE%
	NO.1C.HERE.FLAG% = 	FALSE%: CHECK.RESULT% = 	FALSE%

	NUMBER.OF.STATEMENTS% = 60
	AT.VALUE$ = "C000"

REM	*****************************************************************
REM	* Read the keywords into the matrix so that they can be accessed*
REM	* The keyword goes into KEYWORDS$(01, XX), the code that the    *
REM	* statement word creates goes into KEYWORDS$(02, XX).		*
REM	*								*
REM	*****************************************************************

	DIM WORDS$(20), LABEL.FLAG%(17), GOTO.FLAG%(17), TOTAL.CALLS%(40)
	DIM LABEL.LOCATION%(16), MACROS$(40), MACRO.TEXT$(100)
	DIM KEYWORDS$(02, NUMBER.OF.STATEMENTS%), MAC.REC%(40), MAC.LEN%(40)
	DIM SYMBOLIC.NAME$(100), VARIABLE$(2, 100), ASSEMBLY.LABEL$(2, 40)

REM	*****************************************************************
REM	* MACROS$ - MACRO NAMES						*
REM	* MACRO.TEXT$ - 100 LINES  OF THE MACRO				*
REM	* MAC.REC% - STARTING RECORD NUMBER				*
REM	* MAC.LEN% - NUMBER OF RECORDS FOR MACRO			*
REM	*								*
REM	*****************************************************************

	OPEN "F9010.001" AS 20 UNLOCKED
	FOR LOOP% = 1 TO NUMBER.OF.STATEMENTS%
	   READ #20; KEYWORDS$(01, LOOP%), KEYWORDS$(02, LOOP%)
	NEXT LOOP%
	CLOSE 20

REM	*****************************************************************
REM	* Here we store the coded information to the destination disk	*
REM	* We remove the : * and checksum from the count (checksum is 2)	*
REM	*								*
REM	*****************************************************************

DEF	STORE.THIS.DATA
	IF LEN(CODE$) = 1 THEN RETURN
	print using "&"; #2; code$

	IF LEFT$(CODE$, 3) <> ":1A" THEN \
	   CODE.COUNT% = CODE.COUNT% + ((LEN(CODE$) - 4) / 2)

	IF LEFT$(CODE$, 3) = ":53" THEN CODE.COUNT% = CODE.COUNT% - 1
FEND

REM	*****************************************************************
REM	* This procedure will remove the delimiters from the source code*
REM	* and redelimit it with commas. It returns before the addition	*
REM	* of commas if AUX or DISPLAY are found.			*
REM	*								*
REM	*****************************************************************

DEF	DELIMIT.RECORD
	RESULT$ = "": RECORD$ = UCASE$(RECORD$)
	
	FOR LOOP% = 1 TO LEN(RECORD$)
	IF LEFT$(RECORD$, 1) = " " OR LEFT$(RECORD$, 1) = CHR$(9) THEN \
	   RECORD$ = RIGHT$(RECORD$, LEN(RECORD$) - 1) ELSE \
	   GOTO NO.LEADING.DELIMITS
	NEXT LOOP%

NO.LEADING.DELIMITS:
	IF LEFT$(RECORD$, 3) = "AUX" OR LEFT$(RECORD$, 5) = "DISPL" OR \
	   LEFT$(RECORD$, 4) = ":REM" THEN RESULT$ = RECORD$: RETURN

	FOR LOOP% = 1 TO LEN(RECORD$)
	IF MID$(RECORD$, LOOP%, 1) = " " OR \
	   MID$(RECORD$, LOOP%, 1) = CHR$(9) THEN \
	   RESULT$ = RESULT$ + ",": GOTO BYPASS.THIS.CHARACTER

	RESULT$ = RESULT$ + MID$(RECORD$, LOOP%, 1)

BYPASS.THIS.CHARACTER:
	NEXT LOOP%

REMOVE.MULTIPLE.DELIMITERS:
	LOOP% = MATCH(",,", RESULT$, 1): IF LOOP% = 0 THEN RETURN
	RESULT$ = LEFT$(RESULT$, LOOP% - 1) + \
	         RIGHT$(RESULT$, LEN(RESULT$) - LOOP%)
	GOTO REMOVE.MULTIPLE.DELIMITERS
FEND

REM	*****************************************************************
REM	* If there is no code after the macro name, set the length to	*
REM	* the length of the code line.					*
REM	*								*
REM	*****************************************************************

DEF	TEST.THE.NAME
	IF MACRO.NAME.END% = 0 THEN MACRO.NAME.END% = LEN(RESULT$)
FEND

REM	*****************************************************************
REM	* Make a pass through the data file and locate any :MACRO or	*
REM	* :ENDMACRO statements. When 1 has been found, create a sym	*
REM	* file offering the macro name, number of lines, and the lines	*
REM	* that make up the macro.					*
REM	* Then close the files.						*
REM	*								*
REM	* Macro names are checked for duplication.			*
REM	* Macro text can only be 100 lines in length.			*
REM	*								*
REM	* We will also  need to look for :EQUATE statements and keep	*
REM	* track of the numbers for each PROGRAM.			*
REM	*								*
REM	*****************************************************************

DEF	MAKE.A.PASS.FOR.MACROS
	OPEN DR$ + SOURCE$ AS 12: MACRO.STATUS% = 0
	MACRO.OUTPUT.COUNT% = 1

READ.PASS.ONE.LOOP:
	IF END #12 THEN END.OF.PASS.HERE
	READ #12; LINE RECORD$

	IF 0 <> MATCH(":SETUP ", UCASE$(RECORD$), 1) THEN \
	   CALL DELIMIT.RECORD: GOSUB PLUG.A.WORD: GOSUB PLACE.SETUP.PARAMETER

	IF 0 <> MATCH(":NOMACRO ", UCASE$(RECORD$), 1) THEN \
	   GOTO END.OF.PASS.HERE

	IF 0 <> MATCH(":EQUATE ", UCASE$(RECORD$), 1) THEN \
	   CALL DELIMIT.RECORD: GOSUB PLACE.SYMBOLIC.PROGRAM

	IF 0 <> MATCH(":MACRO ", UCASE$(RECORD$), 1) THEN \
	   CALL DELIMIT.RECORD: GOSUB APPEND.MACRO

	IF 0 <> MATCH(":VARIABLE ", UCASE$(RECORD$), 1) THEN \
	   CALL DELIMIT.RECORD: GOSUB VARIABLE.REQUESTED

	GOTO READ.PASS.ONE.LOOP

END.OF.PASS.HERE:
	CLOSE 12: RETURN

APPEND.MACRO:
	MACRO.NAME.END% = MATCH(",", RESULT$, 8)

	IF MACRO.NAME.END% = 0 THEN MACRO.NAME.END% = LEN(RESULT$)

	MACRO.NAME.LENGTH% = MACRO.NAME.END% - 7
	MACRO.NAME$ = MID$(RESULT$, 8, MACRO.NAME.LENGTH%)

	IF LEN(MACRO.NAME$) = 0 OR \
	   LEFT$(MACRO.NAME$, 4) = "CODE" OR \
	   LEFT$(MACRO.NAME$, 4) = "PROG" OR \
	   LEFT$(MACRO.NAME$, 8) = "VARIABLE" OR \
	   LEFT$(MACRO.NAME$, 3) = "ROM" OR \
	   LEFT$(MACRO.NAME$, 3) = "I/O" OR \
	   LEFT$(MACRO.NAME$, 4) = "NEXT" OR \
           LEFT$(MACRO.NAME$, 3) = "RAM" OR \
	   LEFT$(MACRO.NAME$, 7) = "NOMACRO" OR \
	   LEFT$(MACRO.NAME$, 3) = "REM" THEN \
	   PRINT "Invalid MACRO name: "; MACRO.NAME$: STOP

	MACRO.STATUS% = MACRO.STATUS% + 1
	PRINT "MACRO: "; MACRO.NAME$;" ";

	FOR MACRO.TEST% = 1 TO MACRO.STATUS%

	   IF MACROS$(MACRO.TEST%) = MACRO.NAME$ THEN \
	      PRINT "Duplicated MACRO name: ";MACRO.NAME$: STOP

	NEXT MACRO.TEST%

	MACROS$(MACRO.STATUS%) = MACRO.NAME$
	MACRO.COUNT% = 0

READ.PASS.TWO:
	IF END #12 THEN MACRO.WITHOUT.ENDMACRO
	READ #12; LINE RECORD$: CALL DELIMIT.RECORD

	IF LEN(RESULT$) = 0 THEN GOTO READ.PASS.TWO
	IF LEFT$(RESULT$, 4) = ":REM" THEN GOTO READ.PASS.TWO

	IF LEFT$(RESULT$, 9) = ":VARIABLE" THEN GOSUB VARIABLE.REQUESTED:  \
	   GOTO READ.PASS.TWO

	IF LEFT$(RESULT$, 7) = ":EQUATE" THEN GOSUB PLACE.SYMBOLIC.PROGRAM: \
	   GOTO READ.PASS.TWO

	IF LEFT$(RESULT$, 5) = "LABEL" THEN \
	   PRINT "Macro ";MACRO.NAME$;" contains a label": \
	   PRINT "This is not allowed.": STOP

	IF LEFT$(RESULT$, 6) = ":MACRO" THEN \
	   PRINT "MACROS are nested: ";MACRO.NAME$;" and ";: \
	   MACRO.NAME.END% = MATCH(",", RESULT$, 8): \
	   CALL TEST.THE.NAME: \
	   MACRO.NAME$ = MID$(RESULT$, 8, MACRO.NAME.END% - 7): \
	   PRINT MACRO.NAME$: STOP

	IF LEFT$(RESULT$, 9) = ":ENDMACRO" THEN GOSUB WRITE.MACRO.INFO: RETURN

	   MACRO.COUNT% = MACRO.COUNT% + 1

	IF MACRO.COUNT% = 101 THEN \
	   PRINT "MACRO exceeds 100 lines, MACRO: "; MACRO.NAME$: STOP

	MACRO.TEXT$(MACRO.COUNT%) = RECORD$: GOTO READ.PASS.TWO

WRITE.MACRO.INFO:
	IF MAC.FILE% = 0 THEN GOSUB CREATE.SYM.FILE: MAC.FILE% = 1

	MAC.REC%(MACRO.STATUS%) = MACRO.OUTPUT.COUNT%
	MAC.LEN%(MACRO.STATUS%) = MACRO.COUNT%

	FOR MACRO.OUT% = 1 TO MACRO.COUNT%
	   PRINT USING "&"; #14, MACRO.OUTPUT.COUNT%; MACRO.TEXT$(MACRO.OUT%)
	   MACRO.OUTPUT.COUNT% = MACRO.OUTPUT.COUNT% + 1
	NEXT MACRO.OUT%: PRINT MACRO.COUNT%: RETURN

MACRO.WITHOUT.ENDMACRO:
	PRINT "Unable to located :ENDMACRO for MACRO: "; MACRO.NAME$: STOP

CREATE.SYM.FILE:
	IF END #14 THEN CREATE.THE.FILE.FOR.SYM
	OPEN DR$ + "MACRO.SYM" RECL 80 AS 14: DELETE 14

CREATE.THE.FILE.FOR.SYM:
	CREATE DR$ + "MACRO.SYM" RECL 80 AS 14: RETURN

REM	*****************************************************************
REM	* This is where symbolic name get appended to the programs.	*
REM	*								*
REM	*****************************************************************

PLACE.SYMBOLIC.PROGRAM:
	IF RIGHT$(RESULT$, 5) = ":NEXT" THEN GOTO THIS.IS.A.NEXT

	FOR SYM.LOOP% = LEN(RESULT$) TO 1 STEP - 1
	   IF MID$(RESULT$, SYM.LOOP%, 1) = "," THEN GOTO HAVE.THE.SYM.NUMBER
	NEXT SYM.LOOP%
	PRINT "Invalid program number equate: ";RECORD$: STOP

HAVE.THE.SYM.NUMBER:
	SYM.LEN% = LEN(RESULT$)
	SYM.NUMBER% = VAL(MID$(RESULT$, SYM.LOOP% +1, SYM.LEN% - SYM.LOOP%))

	IF SYM.NUMBER% < 0 OR SYM.NUMBER% > 99 THEN \
	   PRINT "Invalid program number equate: ";SYM.NUMBER%: STOP

TIE.UP.SYMBOLIC.NEXT:
	SYM.ENDING% = MATCH(",", RESULT$, 9)
	SYM.NAME$ = MID$(RESULT$, 9, SYM.ENDING% - 9)

	FOR SYM.LOOP% = 1 TO 100

	   IF SYMBOLIC.NAME$(SYM.LOOP%) = SYM.NAME$ THEN \
	      PRINT "Duplicated symbolic program name: "; SYM.NAME$: STOP

	NEXT SYM.LOOP%
	NEXT.COUNT% = NEXT.COUNT% +  1
	SYMBOLIC.NAME$(SYM.NUMBER% + 1) = SYM.NAME$: RETURN

THIS.IS.A.NEXT:
	SYM.NUMBER% = NEXT.COUNT%: GOTO TIE.UP.SYMBOLIC.NEXT

REM	*****************************************************************
REM	* When a :VARIABLE NAME AS NUMERIC has been requested, take the *
REM	* symbolic variable NAME and equate it as NUMERIC. Whenever the *
REM     * NAME is located in the Search-For-Number subroutine, place    *
REM	* NUMERIC value into the value.					*
REM	*								*
REM	*****************************************************************

VARIABLE.REQUESTED:
	FOR VAL.LOOP% = LEN(RESULT$) TO 1 STEP - 1
	   IF MID$(RESULT$, VAL.LOOP%, 1) = "," THEN GOTO HAVE.THE.VALUE
	NEXT VAL.LOOP%
	PRINT "Invalid variable equate: ";RECORD$
	PRINT "Error is on line: ";LINE.COUNT%: STOP

HAVE.THE.VALUE:
	VAL.LEN% = LEN(RESULT$)
	VAL.NUMBER$ = MID$(RESULT$, VAL.LOOP% +1, VAL.LEN% - VAL.LOOP%)
	VAL.ENDING% = MATCH(",", RESULT$, 11)
	VAL.NAME$ = MID$(RESULT$, 11, VAL.ENDING% - 11)

	FOR VAL.LOOP% = 1 TO 100

	   IF VARIABLE$(1, VAL.LOOP%) = VAL.NAME$ THEN \
	      PRINT "Duplicated symbolic variable: "; VAL.NAME$: \
	      PRINT "Error is on line: ";LINE.COUNT%: STOP

	NEXT VAL.LOOP%: VAL.COUNT% = VAL.COUNT% +  1

	IF VAL.COUNT% = 101 THEN \
	   PRINT "Exceeded 100 variables at line: ";LINE.COUNT%: STOP

	VARIABLE$(1, VAL.COUNT%) = VAL.NAME$
	VARIABLE$(2, VAL.COUNT%) = VAL.NUMBER$: RETURN

PLACE.SETUP.PARAMETER:
	IF WORDS$(2) = "DATA" AND WORDS$(3) = "ERROR" THEN \
	   GOTO SETUP.DATA.ERROR

	IF WORDS$(2) = "ADDRESS" AND WORDS$(3) = "ERROR" THEN \
	   GOTO SETUP.ADDRESS.ERROR

	IF WORDS$(2) = "CONTROL" AND WORDS$(3) = "ERROR" THEN \
	   GOTO SETUP.CONTROL.ERROR

	IF WORDS$(2) = "ACTIVE" AND WORDS$(3) = "FORCE" THEN \
	   GOTO SETUP.ACTIVE.FORCE

	IF WORDS$(2) = "ACTIVE" AND WORDS$(3) = "INTERRUPT" THEN \
	   GOTO SETUP.ACTIVE.INTERRUPT

	IF WORDS$(2) = "ILLEGAL" AND WORDS$(3) = "ADDRESS" THEN \
	   GOTO SETUP.ILLEGAL.ADDRESS

	IF WORDS$(2) = "BAD" AND WORDS$(3) = "POWER" THEN \
	   GOTO SETUP.BAD.POWER

	IF WORDS$(2) = "EXERCISE" AND WORDS$(3) = "ERRORS" THEN \
	   GOTO SETUP.EXERCISE.ERRORS

	IF WORDS$(2) = "BEEP" THEN GOTO SETUP.BEEP.ON.ERROR

	PRINT "Unknown :SETUP parameter on line: ";LINE.COUNT%
	PRINT "Line: ";RECORD$: PRINT "Unknown words: ";
	PRINT WORDS$(02);" AND "; WORDS$(03): STOP

SETUP.DATA.ERROR:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 1: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 1: RETURN

NOT.GOOD.SETUP:
	PRINT "Invalid :SETUP request on line: ";LINE.COUNT%
	PRINT "--->";WORDS$(2);" AND ";WORDS$(3)
	PRINT "YES, NO, ON, or OFF after the second keyword.": STOP

SETUP.ADDRESS.ERROR:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 2: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 2: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.CONTROL.ERROR:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 4: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 4: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.ACTIVE.FORCE:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 8: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 8: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.ACTIVE.INTERRUPT:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 16: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 16: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.ILLEGAL.ADDRESS:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 32 : RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 32: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.BAD.POWER:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN \
	   SETUP% = SETUP% - 128: RETURN

	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN \
	   SETUP% = SETUP% AND 128: RETURN

	GOTO NOT.GOOD.SETUP

SETUP.EXERCISE.ERRORS:
	IF WORDS$(4) = "NO" OR WORDS$(4) = "OFF" THEN EXERCISE% = 0: RETURN
	IF WORDS$(4) = "YES" OR WORDS$(4) = "ON" THEN EXERCISE% = 1: RETURN
	GOTO NOT.GOOD.SETUP

SETUP.BEEP.ON.ERROR:
	IF WORDS$(3) = "NO" OR WORDS$(3) = "OFF" THEN BEEP% = 0: RETURN
	IF WORDS$(3) = "YES" OR WORDS$(3) = "ON" THEN BEEP% = 1: RETURN
	GOTO NOT.GOOD.SETUP

PLUG.A.WORD:
	FOR LOOP% = 1 TO WORD.COUNT%: WORDS$(LOOP%) = "": NEXT LOOP%
	WORD.COUNT% = 1
	FOR LOOP% = 1 TO LEN(RESULT$)
	IF MID$(RESULT$, LOOP%, 1) <> "," THEN \
	   WORDS$(WORD.COUNT%) = WORDS$(WORD.COUNT%) + MID$(RESULT$, LOOP% ,1)\
	   ELSE WORD.COUNT% = WORD.COUNT% + 1
	NEXT LOOP%: RETURN
FEND

REM	*****************************************************************
REM	* Get a record from the macro library, keeping count of the	*
REM	* number of records under that macro as you do so. If the end	*
REM	* of the macro is encountered, set the source back to the main	*
REM	* data file by setting source.flag% to zero.			*
REM	*								*
REM	*****************************************************************

DEF	GET.MACRO.RECORD
	READ #14, MAC.REC%(PLUG.MAC%) + (MACRO.LINE.COUNT% - 1); LINE RECORD$

	FOR SPACE.OUT.LOOP% = LEN(RECORD$) TO 1 STEP -1
	   IF MID$(RECORD$, SPACE.OUT.LOOP%, 1) <> " " THEN GOTO OK.THATS.IT
	NEXT SPACE.OUT.LOOP%: SPACE.OUT.LOOP% = LEN(RECORD$)


OK.THATS.IT:
	RECORD$ = LEFT$(RECORD$, SPACE.OUT.LOOP%)
	MACRO.LINE.COUNT% = MACRO.LINE.COUNT% + 1

	IF MACRO.LINE.COUNT% > MAC.LEN%(PLUG.MAC%) THEN SOURCE.FLAG% = 0
FEND

REM	*****************************************************************
REM	* This routint will take the number in NUMBER% and place the 	*
REM	* result of a hex conversion into HEX.NUMBER$			*
REM	*								*
REM	*****************************************************************

DEF	HEX.IT.UP
	HEX.NUMBER$ = MID$(INDEX$, INT(NUMBER% / 16) + 1, 1) + MID$(INDEX$, \
	   (NUMBER% - (INT(NUMBER% / 16) * 16)) + 1, 1)
FEND

REM	*****************************************************************
REM	* Here a checksum is done. 					*
REM	*								*
REM	*****************************************************************

DEF	CREATE.CHECKSUM(TEST.STRING$)
	TOTAL.CHECKSUM% = 0
	FOR CHECK.LOOP% = 2 TO LEN(TEST.STRING$)
	   BYTE$ = MID$(TEST.STRING$, CHECK.LOOP%, 2)
	   GOSUB ADD.THIS
	   CHECK.LOOP% = CHECK.LOOP% + 1
	NEXT CHECK.LOOP%

	NUMBER% = TOTAL.CHECKSUM%: CALL HEX.IT.UP
	CHECKSUM$ = HEX.NUMBER$: RETURN

ADD.THIS:
	TOTAL.CHECKSUM% = TOTAL.CHECKSUM% + \ 
	   (MATCH(LEFT$(BYTE$, 1), INDEX$, 1) - 1) * 16 + \
 	   MATCH(RIGHT$(BYTE$, 1), INDEX$, 1) - 1

	IF TOTAL.CHECKSUM% > 255 THEN \
	   TOTAL.CHECKSUM% = TOTAL.CHECKSUM% - 256
	RETURN
FEND

REM	*****************************************************************
REM	* Here is where we write  the setup data out to the disk.	*
REM	*								*
REM	*****************************************************************

DEF	WRITE.SETUP.DATA.TO.DISK
	NUMBER% = SETUP%: CALL HEX.IT.UP
	CODE$ = ":01" + HEX.NUMBER$: CALL CREATE.CHECKSUM(CODE$)
	CODE$ = CODE$ + CHECKSUM$
	print using "&"; #2; code$
	NUMBER% = BEEP%: CALL HEX.IT.UP
	CODE$ = ":03" + HEX.NUMBER$: CALL CREATE.CHECKSUM(CODE$)
	CODE$ = CODE$ + CHECKSUM$
	print using "&"; #2; code$
	NUMBER% = EXERCISE%: CALL HEX.IT.UP
	CODE$ = ":04" + HEX.NUMBER$: CALL CREATE.CHECKSUM(CODE$)
	CODE$ = CODE$ + CHECKSUM$
	print using "&"; #2; code$
FEND

REM	*****************************************************************
REM	* The encode message will take the message in the source code	*
REM	* and encode it according to Fluke 9010 format. It will set the *
REM	* 7'th data bit high.						*
REM	*								*
REM	*****************************************************************

DEF	ENCODE.MESSAGE
	IF 0 <> SLASH% THEN RESULT$ = LEFT$(RESULT$, SLASH% - 1)
	CHARACTER.COUNT% = 0

	IF START.FLAG% THEN CODE$ = ":53": START.FLAG% = FALSE% ELSE \
	   CODE$ = ":"

	IF LEFT$(RESULT$, 3) = "AUX" THEN \
	   CODE$ = CODE$ + "3F": MESS.TYPE% = 1 ELSE \
	   CODE$ = CODE$ + "3E": MESS.TYPE% = 0

	MESSAGE.START% = MATCH(" ", RECORD$, 1)

	IF MESSAGE.START% = 0 THEN CODE$ = CODE$ + "74": \
	   CALL CREATE.CHECKSUM(CODE$): \
	   CODE$ = CODE$ + "*" + CHECKSUM$: RETURN

message.spaces:
	if right$(record$, 1) = " " then \
	   record$ = left$(record$, len(record$) - 1): \
	   goto message.spaces

	FOR LOOP% = MESSAGE.START% + 1 TO LEN(RECORD$)
	BYTE% = 128 + (ASC(MID$(RECORD$, LOOP%, 1)))

	CODE$ = CODE$ + MID$(INDEX$, INT(BYTE% / 16) + 1, 1) + MID$(INDEX$, \
	   (BYTE% - (INT(BYTE% / 16) * 16)) + 1, 1)

	CHARACTER.COUNT% = CHARACTER.COUNT% + 1

	IF CHARACTER.COUNT% = 27 AND mess.type% = 2 THEN \
	   PRINT "Length error located on line: ";LINE.COUNT%: \
	   PRINT "Line TOO LONG for display:": PRINT RECORD$: STOP

	IF CHARACTER.COUNT% = 27 THEN GOSUB STORE.CREATE.LARGE.MESSAGE
	NEXT LOOP%
	CODE$ = CODE$ + "74": CALL CREATE.CHECKSUM(CODE$):
	CODE$ = CODE$ + "*" + CHECKSUM$: RETURN

STORE.CREATE.LARGE.MESSAGE:
	IF mess.type% = 1 THEN CODE$ = CODE$ + "AB74" ELSE CODE$ = CODE$ + "74"
	CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + "*" + CHECKSUM$
	IF NOT CODE.FLAG% THEN PRINT CODE$
	CALL STORE.THIS.DATA
	IF MESS.TYPE% = 1 THEN CODE$ = ":3F" ELSE CODE$ = ":3E"
	CHARACTER.COUNT% = 0: RETURN
FEND

REM	*****************************************************************
REM	* Here an address is created.					*
REM	* Zeros area appended to all digits of the number.		*
REM	*								*
REM	*****************************************************************

DEF	MAKE.ADDRESS(TEST.STRING$)
	TEST.STRING$ = RIGHT$("00000000" + TEST.STRING$, 8)
	ADDRESS.CODE$ = MID$(TEST.STRING$, 3, 2) + \
			MID$(TEST.STRING$, 1, 2) + \
			MID$(TEST.STRING$, 7, 2) + \
			MID$(TEST.STRING$, 5, 2)
FEND

REM	*****************************************************************
REM	* This is the encode of the attribues section. When :I/O, :RAM,	*
REM	* or :ROM is encountered. These attributes can be changed on the*
REM	* Fluke 9010 tester with little effort, or included in SOURCE	*
REM	*								*
REM	*****************************************************************

DEF	ENCODE.ATTRIBUTE
	IF LEFT$(RESULT$, 4) = ":I/O" THEN TYPE$ = "01": NEED% = 3
	IF LEFT$(RESULT$, 4) = ":RAM" THEN TYPE$ = "02": NEED% = 2
	IF LEFT$(RESULT$, 4) = ":ROM" THEN TYPE$ = "03": NEED% = 3

TRY.ONE1:
	FIRST% = 1 + MATCH(",", RESULT$, 1)
	IF FIRST% <> 1 THEN GOT% = 1

	SECOND% = 1 + MATCH(",", RESULT$, FIRST%)
	IF SECOND% <> 1 THEN GOT% = 2

	THIRD% = 1 + MATCH(",", RESULT$, SECOND%)
	IF THIRD% <> 1 THEN GOT% = 3

	IF GOT% < NEED% THEN \
	   PRINT "Not enough address descriptions in line ";LINE.NUMBER%: \
	   PRINT "Needed ";NEED%;", got ";GOT%: STOP

	IF GOT% > NEED% THEN \
	   PRINT "Too many address descriptions in line ";LINE.NUMBER%: \
	   PRINT "Needed ";NEED%;", got ";GOT%: STOP

	IF THIRD% = 1 THEN THIRD% = LEN(RESULT$) + 2

	CODE$ = ":19"

	LENGTH% = (SECOND% - 1) - FIRST%
	ADDRESS.STRING$ = MID$(RESULT$, FIRST%, LENGTH%)
	CALL MAKE.ADDRESS(ADDRESS.STRING$): CODE$ = CODE$ + ADDRESS.CODE$

	LENGTH% = (THIRD% - 1) - SECOND%
	ADDRESS.STRING$ = MID$(RESULT$, SECOND%, LENGTH%)
	CALL MAKE.ADDRESS(ADDRESS.STRING$): CODE$ = CODE$ + ADDRESS.CODE$

	CODE$ = CODE$ + TYPE$ + "000000"

	IF TYPE$ = "02" THEN CODE$ = CODE$ + "000000000000": \
	   CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + CHECKSUM$: RETURN

	IF TYPE$ = "03" THEN \
	   LENGTH% = 1 + (LEN(RESULT$) - THIRD%) + 1: \
	   ADDRESS.STRING$ = MID$(RESULT$, THIRD%, LENGTH%): \
	   CALL MAKE.ADDRESS(ADDRESS.STRING$): \
	   CODE$ = CODE$ + ADDRESS.CODE$ + "0000": \
	   CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + CHECKSUM$: RETURN

	LENGTH% = (LEN(RESULT$) - THIRD%) + 1
	ADDRESS.STRING$ = MID$(RESULT$, THIRD%, LENGTH%)
	CALL MAKE.ADDRESS(ADDRESS.STRING$)
	CODE$ = CODE$ + "0000" + ADDRESS.CODE$
	CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + CHECKSUM$
FEND

REM	*****************************************************************
REM	* Here we have encountered an EXECUTE statement. See if the	*
REM	* program being executed is called with a symblic name. If it	*
REM	* is, then replace the symbolic name with the number.		*
REM	* If not, return as is for numeric.				*
REM	*								*
REM	*****************************************************************

DEF	CHECK.ON.ITS.SYMBOLIC
	IF 0 <> MATCH(LEFT$(WORDS$(02), 1), PROG.INDEX$, 1) THEN RETURN
	IF LEFT$(WORDS$(02), 3) = "REG" THEN RETURN

	FOR SYM.LOOP% = 1 TO 100
	   IF SYMBOLIC.NAME$(SYM.LOOP%) = WORDS$(02) THEN GOTO TRY.THIS.NAME.1
	NEXT SYM.LOOP%
	PRINT "Unable to execute symbol: "; RECORD$: STOP

TRY.THIS.NAME.1:
	RECORD$ = "EXECUTE " + STR$(SYM.LOOP% - 1)
	RESULT$ = "EXECUTE," + STR$(SYM.LOOP% - 1)
	WORDS$(02) = STR$(SYM.LOOP% - 1)
FEND

REM	*****************************************************************
REM	* Check for number will locate a number in the code if it is	*
REM	* THERE If not, it returns an error flag.			*
REM	*								*
REM	*****************************************************************

DEF	CHECK.FOR.NUMBER(TEST.STRING$)
	IF LEFT$(TEST.STRING$, 1) = CHR$(34) AND \
	   RIGHT$(TEST.STRING$, 1) = CHR$(34) THEN GOTO THIS.IS.AN.ASCII.NUMBER

	IF RIGHT$(TEST.STRING$, 2) = "B)" AND LEFT$(TEST.STRING$, 1) = "(" \
	   THEN GOTO THIS.IS.A.BINARY.NUMBER

	SET.BINARY.END.FLAG% = TRUE%

PROCESS.CONVERTED.NUMBER:
	FOR CHECK.LOOP% = 1 TO LEN(TEST.STRING$)
	   IF 0 = MATCH(MID$(TEST.STRING$, CHECK.LOOP%, 1), INDEX$, 1) THEN \
	      CHECK.RESULT% = FALSE%: RETURN
	NEXT CHECK.LOOP%

	NUMERIC$ = ""
	FOR CHECK.LOOP% = 1 TO LEN(TEST.STRING$)
	   NUMERIC$ = NUMERIC$ + "0" + MID$(TEST.STRING$, CHECK.LOOP%, 1)
	NEXT CHECK.LOOP%

	IF IF.FLAG% AND REG.FLAG% THEN CHECK.RESULT% = TRUE%: RETURN
	IF NO.1C.HERE.FLAG% THEN CHECK.RESULT% = TRUE%: RETURN
	IF RIGHT$(NUMERIC$, 2) = "1C" THEN CHECK.RESULT% = TRUE%: RETURN

	IF NOT SET.BINARY.END.FLAG% THEN CHECK.RESULT% = TRUE%: RETURN

	IF TEST.FLAG% OR REG.FLAG% OR SHOOT.FLAG% OR \
	   DATA.FLAG% OR LEARN.FLAG% OR I.O.FLAG% OR \
	   ATOG.FLAG% OR BUS.FLAG% THEN NUMERIC$ = NUMERIC$ + "1C"

	CHECK.RESULT% = TRUE%: RETURN

THIS.IS.A.BINARY.NUMBER:
	TEST.STRING$ = RIGHT$(TEST.STRING$, LEN(TEST.STRING$) - 1)
	TEST.STRING$ = LEFT$(TEST.STRING$, LEN(TEST.STRING$) - 2)
	IF LEN(TEST.STRING$) = 8 THEN GOTO OF.BINARY.IS.VALID
	PRINT "Your binary number on line: ";LINE.COUNT%;" needs to be";
	PRINT " in groups of 8 bits only."
	PRINT "You have a length of: "; LEN(TEST.STRING$): STOP

OF.BINARY.IS.VALID:
	NUMBER% = 0: B$ = ""
	FOR L% = 8 TO 1 STEP -1
	   B$ = B$ + MID$(TEST.STRING$, L%, 1)
	NEXT L%: TEST.STRING$ = B$

	FOR L% = 8 TO 1 STEP -1
	IF MID$(TEST.STRING$, L%, 1) = "1" THEN NUMBER% = NUMBER% + 2 ^ (L% -1)
	NEXT L%

	CALL HEX.IT.UP: TEST.STRING$ = HEX.NUMBER$

	IF RIGHT$(WORDS$(LOOP.ONE% + 1), 2) = "B)" AND \
	   LEFT$(WORDS$(LOOP.ONE% + 1), 1) = "(" THEN \
	   SET.BINARY.END.FLAG% = FALSE% ELSE SET.BINARY.END.FLAG% = TRUE%

	GOTO PROCESS.CONVERTED.NUMBER

THIS.IS.AN.ASCII.NUMBER:
	IF LEN(TEST.STRING$) <> 3 THEN CHECK.RESULT% = FALSE%: RETURN
	NUMBER% = ASC(MID$(TEST.STRING$, 2, 1)): CALL HEX.IT.UP
	TEST.STRING$ = HEX.NUMBER$: GOTO PROCESS.CONVERTED.NUMBER
FEND

REM	*****************************************************************
REM	* If the check for number does not result in a value  being	*
REM	* located, we enter into this routine which will check to see	*
REM	* if a variable is being used.					*
REM	*								*
REM	*****************************************************************

DEF	CHECK.FOR.VARIABLE(TEST.STRING$)
	FOR CHECK.LOOP% = 1 TO VAL.COUNT%

	   IF VARIABLE$(01, CHECK.LOOP%) = TEST.STRING$ THEN \
	      WORDS$(LOOP.ONE%) = VARIABLE$(02, CHECK.LOOP%): \
	      CHECK.RESULT% = TRUE%: RETURN

	NEXT CHECK.LOOP%: CHECK.RESULT% = FALSE%: RETURN
FEND

REM	*****************************************************************
REM	* Here we place the words into the proper variables.		*
REM	*								*
REM	*****************************************************************

DEF	PLUG.WORDS
	FOR LOOP% = 1 TO WORD.COUNT%: WORDS$(LOOP%) = "": NEXT LOOP%
	WORD.COUNT% = 1
	FOR LOOP% = 1 TO LEN(RESULT$)
	IF MID$(RESULT$, LOOP%, 1) <> "," THEN \
	   WORDS$(WORD.COUNT%) = WORDS$(WORD.COUNT%) + MID$(RESULT$, LOOP% ,1)\
	   ELSE WORD.COUNT% = WORD.COUNT% + 1
	NEXT LOOP%
FEND

REM	*****************************************************************
REM	* Here the program statements are encoded. If a bad keyword is	*
REM	* LOCATED an error message if offered and execution is 		*
REM	* terminated.							*
REM	*								*
REM	*****************************************************************

DEF	ENCODE.STATEMENTS
	IF XXXX = 0 THEN XXXX = MFRE
	XXX = MFRE
	IF XXX < XXXX THEN XXXX = XXX
	IF CURRENT.PROG$ = "  " THEN PRINT "No program number": STOP

TRY.TWO2:
	CODE$ = ":": CALL STORE.THIS.DATA
	IF START.FLAG% THEN CODE$ = CODE$ + "53": START.FLAG% = FALSE%
	CALL PLUG.WORDS

	IF LEFT$(RESULT$, 2) = "IF" AND \
	   0 = MATCH("GOTO", RESULT$, 3) THEN \
	   PRINT "IF missing goto. Line ";LINE.COUNT%: STOP

	IF LEFT$(RESULT$, 6) = "IF,REG" THEN \
	   IF.REG.FLAG% = TRUE% ELSE IF.REG.FLAG% = FALSE%

	IF WORDS$(01) = "EXECUTE" THEN CALL CHECK.ON.ITS.SYMBOLIC

	IF LEFT$(WORDS$(01), 3) = "BUS" THEN \
	   BUS.FLAG% = TRUE% ELSE BUS.FLAG% = FALSE%

	IF WORDS$(01) = "LEARN" THEN \
	   LEARN.FLAG% = TRUE% ELSE LEARN.FLAG% = FALSE%

	IF LEFT$(WORDS$(01), 3) = "I/O" THEN \
	   I.O.FLAG% = TRUE% ELSE I.O.FLAG% = FALSE%

	IF WORDS$(01) = "ATOG" OR WORDS$(01) = "TOGGLE-ADDRESS" THEN \
	   ATOG.FLAG% = TRUE% ELSE ATOG.FLAG% = FALSE%

	IF LEFT$(WORDS$(01), 3) = "REG" AND WORDS$(02) = "=" AND \
	   LEFT$(WORDS$(03), 3) = "REG" THEN \
	   EQUATE.FLAG% = TRUE% ELSE EQUATE.FLAG% = FALSE%

	IF WORDS$(01) = "RUN" THEN RUN.FLAG% = TRUE% ELSE RUN.FLAG% = FALSE%

	IF WORDS$(01) = "IF" THEN IF.FLAG% = TRUE% ELSE IF.FLAG% = FALSE%

	IF LEFT$(WORDS$(WORD.COUNT%), 3) = "REG" THEN \
	   END.REG.FLAG% = TRUE% ELSE END.REG.FLAG% = FALSE%

	IF LEFT$(WORDS$(01), 3) = "REG" THEN \
	   REG.FLAG% = TRUE% ELSE REG.FLAG% = FALSE%

	IF WORDS$(01) = "I/O-TEST" OR WORDS$(01) = "ROM-TEST" OR \
	   WORDS$(01) = "RAM-LONG" OR WORDS$(01) = "RAM-SHORT" THEN \
	   TEST.FLAG% = TRUE% ELSE TEST.FLAG% = FALSE%

	IF WORDS$(01) = "READ" OR WORDS$(01) = "WRITE" OR \
	   WORDS$(01) = "RAMP" OR WORDS$(01) = "WALK" OR \
	   LEFT$(WORDS$(01), 4) = "TOGG" OR \
	   LEFT$(WORDS$(01), 4) = "ATOG" OR \
	   LEFT$(WORDS$(01), 4) = "DTOG" THEN \
	   SHOOT.FLAG% = TRUE% ELSE SHOOT.FLAG% = FALSE%

	IF WORDS$(01) = "DTOG" OR \
	   WORDS$(01) = "TOGGEL-DATA" OR WORDS$(01) = "WALK" OR \
	   WORDS$(01) = "WRITE" OR WORDS$(01) = "EXECUTE" OR \
	   WORDS$(01) = "EXEC" THEN \
	   DATA.FLAG% = TRUE% ELSE DATA.FLAG% = FALSE%

	IF WORDS$(01) = "READ" OR WORDS$(01) = "WRITE" OR \
	   WORDS$(01) = "RAMP" OR WORDS$(01) = "WALK" OR \
	   WORDS$(01) = "TOGGEL-DATA" OR WORDS$(01) = "DTOG" OR  \
	   WORDS$(01) = "TOGGEL-ADDRESS" OR WORDS$(01) = "ATOG" THEN \
	   APPEND.FLAG% = TRUE% ELSE APPEND.FLAG% = FALSE%

	IF WORDS$(01) = "SHIFT-LEFT" OR WORDS$(01) = "SHIFT-RIGHT" OR \
	   WORDS$(01) = "INCREMENT" OR  WORDS$(01) = "DECREMENT" OR \
	   WORDS$(01) = "COMPLIMENT" THEN \
	   NOREG.FLAG% = TRUE% ELSE NOREG.FLAG% = FALSE%

	FOR LOOP.ONE% = 1 TO WORD.COUNT%
	   FOR LOOP.TWO% = 1 TO NUMBER.OF.STATEMENTS%

	      IF KEYWORDS$(01, LOOP.TWO%) = WORDS$(LOOP.ONE%) THEN \
		 GOTO THIS.WORD.IS.GOOD

	   NEXT LOOP.TWO%

skip.checking.this.one:
	IF WORDS$(LOOP.ONE%) = "" THEN GOTO THIS.WORD.IS.GOOD
	CALL CHECK.FOR.NUMBER(WORDS$(LOOP.ONE%))

	IF CHECK.RESULT% AND LOOP.ONE% = 1 THEN \
	   PRINT "Numeric value encountered. What do I do with it?": \
	   PRINT "Line: "; LINE.COUNT%; " --->";WORDS$(LOOP.ONE%);"<---": STOP

	IF CHECK.RESULT% THEN CODE$ = CODE$ + NUMERIC$: GOTO CONTINUE.CODE

	CALL CHECK.FOR.VARIABLE(WORDS$(LOOP.ONE%))
	IF CHECK.RESULT% THEN GOTO SKIP.CHECKING.THIS.ONE

	GOTO BAD.KEYWORD.FOUND

THIS.WORD.IS.GOOD:
	IF NOREG.FLAG% THEN GOSUB PLUG.REGISTER: GOTO CONTINUE.CODE

	IF DATA.FLAG% AND KEYWORDS$(01, LOOP.TWO%) = "=" THEN \
	   GOTO CONTINUE.CODE

	IF IF.REG.FLAG% AND KEYWORDS$(01, LOOP.TWO%) = "=" THEN \
	   GOTO ALLOW.AN.EQUALS

	IF IF.FLAG% AND KEYWORDS$(01, LOOP.TWO%) = "=" THEN \
	   GOTO CONTINUE.CODE

ALLOW.AN.EQUALS:
	IF LOOP.ONE% = 2 AND REG.FLAG% THEN GOTO CONTINUE.CODE

	IF LOOP.ONE% = 1 AND REG.FLAG% THEN \
	   CODE$ = CODE$ + "44" + RIGHT$(KEYWORDS$(02,LOOP.TWO%), 2): \
	   GOTO CONTINUE.CODE

	CODE$ = CODE$ + KEYWORDS$(02, LOOP.TWO%)
	IF LEFT$(KEYWORDS$(01, LOOP.TWO%), 3) = "REG" THEN \
	   IF WORDS$(LOOP.ONE% + 1) = "OR" OR \
	      WORDS$(LOOP.ONE% + 1) = "AND" OR\
	      WORDS$(LOOP.ONE% + 1) = "COMPLIMENT" OR \
	      WORDS$(LOOP.ONE% + 1) = "DECREMENT" OR \
	      WORDS$(LOOP.ONE% + 1) = "INCREMENT" OR \
	      WORDS$(LOOP.ONE% + 1) = "SHIFT-LEFT" OR \
	      WORDS$(LOOP.ONE% + 1) = "SHIFT-RIGHT" THEN \
	      NO.1C.HERE.FLAG% = TRUE% ELSE NO.1C.HERE.FLAG% = FALSE% \
	   ELSE NO.1C.HERE.FLAG% = FALSE%

	IF NOREG.FLAG% THEN GOTO SKIP.AN.APPEND.OF.1C
	IF RIGHT$(CODE$, 2) = "1C" THEN GOTO SKIP.AN.APPEND.OF.1C
	IF NOT SET.BINARY.END.FLAG% THEN GOTO SKIP.AN.APPEND.OF.1C
	IF IF.REG.FLAG% THEN GOTO SKIP.AN.APPEND.OF.1C
	IF EQUATE.FLAG% THEN GOTO SKIP.AN.APPEND.OF.1C
	IF NO.1C.HERE.FLAG% THEN GOTO SKIP.AN.APPEND.OF.1C

	IF LEFT$(KEYWORDS$(01, LOOP.TWO%), 3) = "REG" THEN CODE$ = CODE$ + "1C"

SKIP.AN.APPEND.OF.1C:
	IF KEYWORDS$(01, LOOP.TWO%) = "LABEL" THEN GOSUB PLUG.LABEL
	IF KEYWORDS$(01, LOOP.TWO%) = "GOTO"  THEN GOSUB PLUG.GOTO

CONTINUE.CODE:
	NEXT LOOP.ONE%

	IF NOREG.FLAG% THEN GOTO DONT.APPEND.ANOTHER.1C
	IF RIGHT$(CODE$, 2) = "1C" THEN GOTO DONT.APPEND.ANOTHER.1C
	IF NOT SET.BINARY.END.FLAG% THEN GOTO DONT.APPEND.ANOTHER.1C
	IF IF.REG.FLAG% THEN GOTO DONT.APPEND.ANOTHER.1C
	IF NO.1C.HERE.FLAG% THEN GOTO DONT.APPEND.ANOTHER.1C
	IF END.REG.FLAG% AND APPEND.FLAG% = FALSE% THEN CODE$ = CODE$ + "1C"
	IF RUN.FLAG% THEN CODE$ = CODE$ + "1C"

DONT.APPEND.ANOTHER.1C:
	IF RIGHT$(CODE$, 4) = "1C1C" THEN CODE$ = LEFT$(CODE$, LEN(CODE$) - 2)
	CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + "*" + CHECKSUM$: RETURN

BAD.KEYWORD.FOUND:
	PRINT "Unknown keyword found on line ";LINE.NUMBER%;", ";
	PRINT WORDS$(LOOP.ONE%): PRINT RESULT$: STOP

PLUG.LABEL:
	IF LEN(WORDS$(LOOP.ONE% + 1)) <> 1 THEN GOTO ERROR.MESSAGE.ONE
	TEST.PROG% = MATCH(WORDS$(LOOP.ONE% + 1), INDEX$, 1)
	IF TEST.PROG% = 0 THEN GOTO ERROR.MESSAGE.ONE

	IF LABEL.LOCATION%(TEST.PROG%) <> 0 AND SOURCE.FLAG% = 0 THEN \
	   PRINT "Duplicate label on line ";LINE.COUNT%: PRINT RECORD$: STOP

	IF LABEL.LOCATION%(TEST.PROG%) <> 0 AND SOURCE.FLAG% = 1 THEN \
	   PRINT "Macro causes duplicate label. Line "; LINE.COUNT%; RECORD$: \
	   STOP

	LABEL.LOCATION%(TEST.PROG%) = CODE.COUNT% + 3
	LABEL.FLAG%(TEST.PROG%) = TRUE%: RETURN

ERROR.MESSAGE.ONE:
	PRINT "Label number invalid on line ";LINE.NUMBER%: PRINT RECORD$
	STOP

PLUG.GOTO:
	IF LEN(WORDS$(LOOP.ONE% + 1)) <> 1 THEN GOTO ERROR.MESSAGE.TWO
	TEST.PROG% = MATCH(WORDS$(LOOP.ONE% + 1), INDEX$, 1)
	IF TEST.PROG% = 0 THEN GOTO ERROR.MESSAGE.TWO
	GOTO.FLAG%(TEST.PROG%) = TRUE%: RETURN

ERROR.MESSAGE.TWO:
	PRINT "Goto number invalid on line ";LINE.NUMBER%: PRINT RECORD$
	STOP

PLUG.REGISTER:
	CODE$ = CODE$ + RIGHT$(KEYWORDS$(02, LOOP.TWO%), 2): RETURN
FEND

REM	*****************************************************************
REM	* This section is entered when a new program is called. This 	*
REM	* ROUTINE builds the code required to append to a code line to	*
REM	* show an end of program.					*
REM	*								*
REM	*****************************************************************

DEF	BUILD.LABEL.DATA
	CODE$  = ":50"
	FOR BUILD.LOOP% = 1 TO 16
	   IF LABEL.LOCATION%(BUILD.LOOP%) = 0 THEN GOTO SKIP.THIS.LABEL
	   NUMBER% = BUILD.LOOP% - 1: CALL HEX.IT.UP
	   CODE$ = CODE$ + HEX.NUMBER$

	   TOTAL.NUMBER% = LABEL.LOCATION%(BUILD.LOOP%)

	   NUMBER1 = INT(TOTAL.NUMBER% / 256)
	   NUMBER2 = TOTAL.NUMBER% - (NUMBER1 * 256)

	   NUMBER% = NUMBER2: CALL HEX.IT.UP
	   CODE$ = CODE$ + HEX.NUMBER$

	   NUMBER% = NUMBER1: CALL HEX.IT.UP
	   CODE$ = CODE$ + HEX.NUMBER$

SKIP.THIS.LABEL:
	NEXT BUILD.LOOP%
	CALL CREATE.CHECKSUM(CODE$)
	CODE$ = CODE$ + "$" + CHECKSUM$
	FOR BUILD.LOOP% = 1 TO 16: LABEL.LOCATION%(BUILD.LOOP%) = 0
	NEXT BUILD.LOOP%

	IF NOT CODE.FLAG% THEN PRINT CODE$
	CALL STORE.THIS.DATA
FEND

REM	*****************************************************************
REM	* If a program execution has been requested with an alpha	*
REM	* character as the first byte, then check to see if there is	*
REM	* a symbolic name available.					*
REM	*								*
REM	*****************************************************************

DEF	LOOK.FOR.SYMBOLIC.NAME
	SYM.ENDING% = LEN(RESULT$)
	SYM.NAME$ = MID$(RESULT$, 10, SYM.ENDING% - 9)
	FOR SYM.LOOP% = 1 TO 100
	   IF SYMBOLIC.NAME$(SYM.LOOP%) = SYM.NAME$ THEN GOTO OK.THATS.IT
	NEXT SYM.LOOP%
	PRINT "Unable to locate symbolic program: ";SYM.NAME$: STOP

OK.THATS.IT:
	NUMBER% = SYM.LOOP% - 1
FEND

REM	*****************************************************************
REM	* Here is where the data gets stored.				*
REM	*								*
REM	*****************************************************************

DEF	BUILD.PROGRAM.BLOCK
	IF ABOVE.FLAG% THEN CALL BUILD.LABEL.DATA
	
	CODE.COUNT% = 0
	START.FLAG%= TRUE%: ABOVE.FLAG% = TRUE%: GOSUB TEST.FLAGS

	FOR ZERO.LOOP% = 1 TO 16: LABEL.FLAG%(ZERO.LOOP%) = FALSE%
	GOTO.FLAG%(ZERO.LOOP%) = FALSE%: NEXT ZERO.LOOP%

	CODE$ = ":1A": LOCATE% = MATCH(",", RESULT$, 1)
	NUMBER% = VAL(MID$(RESULT$, LOCATE% + 1, 2))

	IF 0 = MATCH(MID$(RESULT$, LOCATE% + 1, 1), PROG.INDEX$, 1) THEN \
	   CALL LOOK.FOR.SYMBOLIC.NAME else next.count% = next.count% + 1

	PROGRAM.NUMBER% = NUMBER%
	CALL HEX.IT.UP: CODE$ = CODE$ + HEX.NUMBER$
	CURRENT.PROG$ = HEX.NUMBER$
	CALL CREATE.CHECKSUM(CODE$): CODE$ = CODE$ + CHECKSUM$

	IF NOT CODE.FLAG% THEN PRINT CODE$
	CALL STORE.THIS.DATA: RETURN

TEST.FLAGS:
	FOR TEST.LOOP% = 1 TO 16
	IF GOTO.FLAG%(TEST.LOOP%) AND NOT LABEL.FLAG%(TEST.LOOP%) THEN \
	   PRINT "Program number ";PROGRAM.NUMBER%;" is missing label ";:\
	   NUMBER% = TEST.LOOP% - 1: CALL HEX.IT.UP: PRINT HEX.NUMBER$: \
	   STOP
	NEXT TEST.LOOP%: RETURN
FEND

DEF	CHECK.THE.FOR.VALUE(FOR.VALUE$)

START.WITH.THIS:
	IF VAL(FOR.VALUE$) <> 0 THEN CHECKED.THE% = VAL(FOR.VALUE$): \
	   NO.GOOD% = FALSE: RETURN

	CHECKED.RETURN% = TRUE%
	CALL CHECK.FOR.VARIABLE(FOR.VALUE$): CHECKED.RETURN% = FALSE%
	IF CHECK.RESULT% THEN GOTO START.WITH.THIS

	PRINT "The value used as a variable is invalid on line: "
	PRINT LINE.COUNT%; " in your FOR/NEXT statement": STOP
FEND

REM	*****************************************************************
REM	* Here we will be taking the for next code and inserting it	*
REM	* into the source.						*
REM	*								*
REM	*****************************************************************

DEF	INSERT.FOR.NEXT
	CALL DELIMIT.RECORD: CALL PLUG.WORDS
	FOR.LABEL$ = WORDS$(8): FOR.REG$ = WORDS$(2)
	FOR.START$ = WORDS$(4): FOR.END$ = WORDS$(6)

	IF WORDS$(9) <> "STEP" THEN \
	   FOR.STEP% = 2: GOTO WE.HAVE.A.STEP

	FOR.STEP% = MATCH(LEFT$(WORDS$(10), 1), INDEX$, 1)

	IF FOR.STEP% = 0 THEN LOOP.ONE% = 10: \
	   CALL CHECK.FOR.VARIABLE(WORDS$(LOOP.ONE%)): \
	   IF NOT CHECK.RESULT% THEN \
	   PRINT "Step parameter in FOR/NEXT statement is not valid.": \
	   PRINT RECORD$: PRINT "Error is on line: ";LINE.COUNT%: STOP ELSE \
	   FOR.STEP% = MATCH(LEFT$(WORDS$(10), 1), INDEX$, 1): \
	   IF FOR.STEP% = 0 THEN \
	   PRINT "Step parameter variable is not valid.": \
	   PRINT RECORD$: PRINT "Error is on line: ";LINE.COUNT%: STOP

WE.HAVE.A.STEP:
	IF FOR.REG$ = "REGD" THEN \
	   PRINT "FOR/NEXT statement in line: ";LINE.COUNT%;" uses REGD": \
	   PRINT "as its variable register. This register is not allowed": \
	   PRINT "to be used with a FOR/NEXT statement. Use another.": STOP

	IF FOR.LABEL$ = "" OR FOR.REG$ = "" OR \
	   FOR.START$ = "" OR FOR.END$ = "" THEN \
	   PRINT "FOR/NEXT statement is missing parameters on line: ";: \
	   PRINT LINE.COUNT%: STOP

	FOR.RECORD$ = FOR.REG$ + " = " + FOR.START$ + \
	   " | REGD = " + FOR.END$ + " | LABEL " + FOR.LABEL$ + " | "

	IF END #1 THEN END.OF.FOR.NEXT.FILE

READ.FOR.FOR.NEXT:
	READ #1; LINE RECORD$: CALL DELIMIT.RECORD

	IF LEFT$(RESULT$, 4) = ":FOR" THEN \
	   PRINT "You have a nested FOR/NEXT statement starting on line: ";: \
	   PRINT LINE.COUNT%: STOP

	IF LEFT$(RESULT$, 5) = ":NEXT" THEN \
	   GOTO END.OF.FOR.NEXT.STATEMENT ELSE \
	   FOR.RECORD$ = FOR.RECORD$ + RECORD$ + " | "

	GOTO READ.FOR.FOR.NEXT

END.OF.FOR.NEXT.FILE:
	PRINT "You are missing a :NEXT statement for your :FOR"
	PRINT "Line: ";LINE.COUNT%

END.OF.FOR.NEXT.STATEMENT:
	FOR FOR.LOOP% = 1 TO FOR.STEP% - 1
           FOR.RECORD$ = FOR.RECORD$ + "INCREMENT " + FOR.REG$
	   FOR.RECORD$ = FOR.RECORD$ + "| DECREMENT REGD | "
	NEXT FOR.LOOP%
	FOR.RECORD$ = FOR.RECORD$ + "IF REGD > 0 GOTO " + FOR.LABEL$
	RECORD$ = FOR.RECORD$
FEND

REM	*****************************************************************
REM	* Find out what is in the command string.			*
REM	*								*
REM	*****************************************************************

	PROG.INDEX$ = "0123456789": SOURCE$ = COMMAND$
	LOOP% = MATCH(".", SOURCE$, 1)
	IF LOOP% = 0 THEN LOOP% = 9
	OBJECT$ = LEFT$(SOURCE$, LOOP% - 1) + ".FLK"

REM	*****************************************************************
REM	* Here is the program display to anounce the serial, revision,	*
REM	* and the source file name and the object file name.		*
REM	*								*
REM	*****************************************************************

	INDEX$ = "0123456789ABCDEF0": serial% = 1
	PRINT chr$(218); STRING$(43, chr$(196)); chr$(191)
	PRINT chr$(179); "Fluke compiler.                    P.C.C.I.";
	print chr$(179)
	PRINT chr$(179); "Serial number: ";: PRINT USING "#####";SERIAL%;
	PRINT "        Revision: ";: PRINT USING "##.##";REVISION;
	print chr$(179)
	PRINT chr$(179); "Source: ";RIGHT$("            " + SOURCE$, 12);
	PRINT "   Object: ";RIGHT$("            " + OBJECT$,12); chr$(179)
	PRINT chr$(192); STRING$(43, chr$(196)); chr$(217)
	PASS.ONE% = 0

REM	*****************************************************************
REM	* Check to see if a file name was offered. If not, abort run.	*
REM	* If yes, attempt to open the file. If not openable, give an	*
REM	* error.							*
REM	*								*
REM	*****************************************************************

	IF LEN(SOURCE$) = 0 THEN PRINT "No file name offered": STOP
	IF END #1 THEN DOES.NOT.EXIST
	OPEN SOURCE$ AS 1: GOTO FILE.IS.OPENED

DOES.NOT.EXIST:
	PRINT "The file you gave me is not on the disk.": STOP

FILE.IS.OPENED:
REM	*****************************************************************
REM	* RENAME THE EXISTING OBJECT FILE TO OBJECT FILE MASTER NAME	*
REM	* WITH A BAK EXTENTION. IF THE OBJECT FILE DOES NOT EXIST,	*
REM	* GO AHEAD WITH COMPILE, HOWEVER, IF IT DOES EXIST, THEN GIVE	*
REM	* A MESSAGE.							*
REM	*								*
REM	*****************************************************************

	ANOTHER$ =  LEFT$(OBJECT$, LEN(OBJECT$) - 3) + "BAK"

	IF END #8 THEN SKIP.THE.DELETE
	OPEN ANOTHER$ AS 8: DELETE 8

SKIP.THE.DELETE:
	DUMMY% = RENAME(ANOTHER$, OBJECT$)
	IF DUMMY% = -1 THEN \
	   PRINT "Existing file: ";OBJECT$;" renamed to: ";ANOTHER$

REM	*****************************************************************
REM	* Here we open the work file. If it is found on the disk,	*
REM	* delete it and then create it again.				*
REM	*								*
REM	*****************************************************************

	IF END #2 THEN CREATE.WORK.FILE
	OPEN OBJECT$ AS 2: DELETE 2

CREATE.WORK.FILE:
	IF END #2 THEN UNABLE.TO.CREATE
	CREATE OBJECT$ AS 2: GOTO WORK.FILE.OPENED

UNABLE.TO.CREATE:
	PRINT "I am unable to create the work files on the disk."
	PRINT "Check to see if directory space is available.": STOP

WORK.FILE.OPENED:
	IF PASS.ONE% = 0 THEN CALL MAKE.A.PASS.FOR.MACROS: PASS.ONE% = 1: \
	   CALL WRITE.SETUP.DATA.TO.DISK

	SOURCE.FLAG% = 0: SLASH% = 0

END.OF.SETUP.DATA:
	IF SLASH% = 0 THEN GOTO I.ALREADY.HAVE.MY.RECORD
	RECORD$ = RIGHT$(OLD.RECORD$, LEN(OLD.RECORD$) - SLASH%)
	GOTO TEST.FOR.ANOTHER.SLASH

I.ALREADY.HAVE.MY.RECORD:
	IF END #1 THEN CLOSE.IT.ALL
	IF END #2 THEN UNABLE.TO.STORE

	IF SOURCE.FLAG% = 0 THEN \
	   READ #1; LINE RECORD$: RECORD$ = UCASE$(RECORD$)

	IF SOURCE.FLAG% = 1 THEN \
	   CALL GET.MACRO.RECORD: RECORD$ = UCASE$(RECORD$)

TEST.FOR.ANOTHER.SLASH:
	SLASH% = MATCH("|", RECORD$, 1): OLD.RECORD$ = RECORD$
	IF SLASH% <> 0 THEN RECORD$ = LEFT$(OLD.RECORD$, SLASH% - 1)
	LINE.NUMBER% = LINE.NUMBER% + 1: LINE.COUNT% = LINE.NUMBER%
	CALL DELIMIT.RECORD

MORE.OF.SAME.OK:
	IF LEFT$(RECORD$, 1) = " " OR LEFT$(RECORD$, 1) = CHR$(9) THEN \
	   RECORD$ = RIGHT$(RECORD$, LEN(RECORD$) - 1): GOTO MORE.OF.SAME.OK

	IF CODE.FLAG% THEN PRINT RECORD$

	IF LEFT$(RESULT$, 8) = ":NOMACRO" THEN GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 3) = "AUX" OR LEFT$(RESULT$, 5) = "DISPL" THEN \
	   CALL ENCODE.MESSAGE: GOTO TIE.IT.ALL.UP

	IF LEFT$(RESULT$, 4) = ":REM" OR LEN(RESULT$) = 0 THEN \
	   SLASH% = 0: GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 7) = ":EQUATE" THEN GOTO END.OF.SETUP.DATA
	IF LEFT$(RESULT$, 9) = ":VARIABLE" THEN GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 6) = ":MACRO" THEN GOSUB IGNORE.THIS.CODE: \
	   SLASH% = 0: GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 4) = ":I/O" OR LEFT$(RESULT$, 4) = ":RAM" OR \
	   LEFT$(RESULT$, 4) = ":ROM" THEN CALL ENCODE.ATTRIBUTE: \
	   SLASH% = 0: GOTO TIE.IT.ALL.UP

	IF LEFT$(RESULT$, 5) = ":PROG" THEN CALL BUILD.PROGRAM.BLOCK: \
	   SLASH% = 0: GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 5) = ":CODE" THEN \
	   IF CODE.FLAG% THEN CODE.FLAG% = FALSE% ELSE \
	   CODE.FLAG% = TRUE%

	IF LEFT$(RESULT$, 5) = ":CODE" THEN GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 4) = ":FOR" THEN CALL INSERT.FOR.NEXT: \
	   GOTO TEST.FOR.ANOTHER.SLASH

	IF LEFT$(RESULT$, 5) = ":NEXT" THEN \
	   PRINT "You have a :NEXT without a :FOR on line: ";LINE.COUNT%: \
	   PRINT "I can't do anything with your :NEXT": STOP

	IF LEFT$(RESULT$, 6) = ":SETUP" THEN GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 1) = ":" THEN GOSUB PLUG.IN.A.MACRO: \
	   GOTO END.OF.SETUP.DATA

	IF LEFT$(RESULT$, 7) = "PRINT,[" THEN \
	   GOSUB ENCODE.PRINT.STATEMENT: GOTO TEST.FOR.ANOTHER.SLASH

	CALL ENCODE.STATEMENTS: GOTO TIE.IT.ALL.UP

REM	*****************************************************************
REM	* Take the string within the quotes and convert it into a	*
REM	* bunch of WRITE statements with the ascii equivelents being	*
REM	* written to the address within the [] characters.		*
REM	*								*
REM	*****************************************************************

ENCODE.PRINT.STATEMENT:
	AD.START% = MATCH("[", RECORD$, 1): P.CODE$ = ""
	AD.END% = MATCH("]", RECORD$, AD.START% + 1)
	ST.START% = MATCH(CHR$(34), RECORD$, AD.END% + 1)
	ST.END% = MATCH(CHR$(34), RECORD$, ST.START% + 1)

	IF LEN(RECORD$) > ST.END% THEN \
	   PRINT "You have code following a PRINT statement.": \
	   PRINT "Line: "; LINE.COUNT%: STOP

REM	*****************************************************************
REM	* Make certain that all of the parameters are offered.		*
REM	*								*
REM	*****************************************************************

	IF AD.START% = 0 OR AD.END% = 0 OR ST.START% = 0 OR ST.END% = 0 THEN \
	   PRINT "PRINT missing a parameter on line:"; LINE.COUNT%;: STOP

	AD.PRINT$ = MID$(RECORD$, AD.START% + 1, (AD.END% - AD.START%) - 1)
	OLD.P$ = "": p.code$ = "REGF = " + AD.PRINT$ + " | "

REM	*****************************************************************
REM	* Go through the string and concat a large string with all of	*
REM	* the writes that we will need. Spaces get the high bits set	*
REM	* on them so that they go through the pre-processor.		*
REM	*								*
REM	*****************************************************************

	FOR PRINT.LOOP% = ST.START% + 1 TO ST.END% - 1
	   P.CHAR$ = MID$(RECORD$, PRINT.LOOP%, 1)
	   IF P.CHAR$ = " " THEN P.CHAR$ = CHR$(160)
	   IF P.CHAR$ = "," THEN P.CHAR$ = CHR$(172)
	   P.CODE$ = P.CODE$ + "WRITE REGF " + chr$(34) + \
	   P.CHAR$ + chr$(34) + " | "
	NEXT PRINT.LOOP%: RECORD$ = UCASE$(P.CODE$): RETURN

REM	*****************************************************************
REM	* We are in a macro. Keep reading file until end is found.	*
REM	*								*
REM	*****************************************************************

IGNORE.THIS.CODE:
	READ #1; LINE RECORD$: CALL DELIMIT.RECORD
	IF LEFT$(RESULT$, 9) = ":ENDMACRO" THEN RETURN
	GOTO IGNORE.THIS.CODE

REM	*****************************************************************
REM	* See if the macro name requested is in the dictionary.		*
REM	* If it is, then set the source.flag% to 1 for a macro.		*
REM	*								*
REM	*****************************************************************

PLUG.IN.A.MACRO:
	MACRO.NAME.END% = MATCH(",", RESULT$, 2)
	IF MACRO.NAME.END% = 0 THEN MACRO.NAME.END% = LEN(RESULT$)

REMOVE.THE.EXTRA.SPACES:
	IF MID$(RESULT$, MACRO.NAME.END%, 1) = " " OR \
	   MID$(RESULT$, MACRO.NAME.END%, 1) = "," THEN \
	   MACRO.NAME.END% = MACRO.NAME.END% - 1: \
	   GOTO REMOVE.THE.EXTRA.SPACES

	MACRO.NAME.LENGTH% = MACRO.NAME.END% - 1
	MACRO.NAME$ = MID$(RESULT$, 2, MACRO.NAME.LENGTH%)

	IF LEN(MACRO.NAME$) = 0 THEN \
	   PRINT "Invalid MACRO name requested": STOP

	FOR PLUG.MAC% = 1 TO 40
	   IF MACROS$(PLUG.MAC%) = MACRO.NAME$ THEN GOTO HAVE.MACRO.NAME
	NEXT PLUG.MAC%

	PRINT "Unable to locate macro ";MACRO.NAME$;" on line: ";
	PRINT LINE.COUNT%: STOP

HAVE.MACRO.NAME:
	MACRO.IS.CALLED% = 1
	TOTAL.CALLS%(PLUG.MAC%) = TOTAL.CALLS%(PLUG.MAC%) + 1
	SOURCE.FLAG% = 1: MACRO.LINE.COUNT% = 1: RETURN

CLOSE.IT.ALL:
	CALL BUILD.LABEL.DATA
	CODE$ = ":00": CALL STORE.THIS.DATA
	CLOSE 1: CLOSE 2
	PRINT "Total of ";ERRORS%;" errors encountered."

	IF MACRO.STATUS% > 0 THEN \
	   FOR LOOP% = 1 TO MACRO.STATUS%: PRINT "Macro: ";: \
	   PRINT MACROS$(LOOP%);" called "; TOTAL.CALLS%(LOOP%);: \
	   PRINT " times": NEXT LOOP%: 

	IF MACRO.IS.CALLED% = 1 THEN DELETE 14

	if abs(xxxx) = xxxx then \
	   PRINT "Internal memory available: ";XXXX; else \
	   PRINT "Internal memory available: "; abs(XXXX) + 32767

	stop

UNABLE.TO.STORE:
	PRINT "There is not enough space on the disk for object file.": STOP

TIE.IT.ALL.UP:
	IF NOT CODE.FLAG% THEN PRINT code$
	CALL STORE.THIS.DATA: GOTO END.OF.SETUP.DATA



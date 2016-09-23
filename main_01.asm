


;*******************************************************************************
;                                                                              *
;    Microchip licenses this software to you solely for use with Microchip     *
;    products. The software is owned by Microchip and/or its licensors, and is *
;    protected under applicable copyright laws.  All rights reserved.          *
;                                                                              *
;    This software and any accompanying information is for suggestion only.    *
;    It shall not be deemed to modify Microchip?s standard warranty for its    *
;    products.  It is your responsibility to ensure that this software meets   *
;    your requirements.                                                        *
;                                                                              *
;    SOFTWARE IS PROVIDED "AS IS".  MICROCHIP AND ITS LICENSORS EXPRESSLY      *
;    DISCLAIM ANY WARRANTY OF ANY KIND, WHETHER EXPRESS OR IMPLIED, INCLUDING  *
;    BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS    *
;    FOR A PARTICULAR PURPOSE, OR NON-INFRINGEMENT. IN NO EVENT SHALL          *
;    MICROCHIP OR ITS LICENSORS BE LIABLE FOR ANY INCIDENTAL, SPECIAL,         *
;    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, HARM TO     *
;    YOUR EQUIPMENT, COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR    *
;    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT LIMITED TO ANY   *
;    DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR CONTRIBUTION, OR OTHER      *
;    SIMILAR COSTS.                                                            *
;                                                                              *
;    To the fullest extend allowed by law, Microchip and its licensors         *
;    liability shall not exceed the amount of fee, if any, that you have paid  *
;    directly to Microchip to use this software.                               *
;                                                                              *
;    MICROCHIP PROVIDES THIS SOFTWARE CONDITIONALLY UPON YOUR ACCEPTANCE OF    *
;    THESE TERMS.                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Filename:                                                                 *
;    Date:                                                                     *
;    File Version:                                                             *
;    Author:                                                                   *
;    Company:                                                                  *
;    Description:                                                              *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Notes: In the MPLAB X Help, refer to the MPASM Assembler documentation    *
;    for information on assembly instructions.                                 *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Known Issues: This template is designed for relocatable code.  As such,   *
;    build errors such as "Directive only allowed when generating an object    *
;    file" will result when the 'Build in Absolute Mode' checkbox is selected  *
;    in the project properties.  Designing code in absolute mode is            *
;    antiquated - use relocatable mode.                                        *
;                                                                              *
;*******************************************************************************
;                                                                              *
;    Revision History:                                                         *
;                                                                              *
;*******************************************************************************



;*******************************************************************************
; Processor Inclusion
;
; TODO Step #1 Open the task list under Window > Tasks.  Include your
; device .inc file - e.g. #include <device_name>.inc.  Available
; include files are in C:\Program Files\Microchip\MPLABX\mpasmx
; assuming the default installation path for MPLAB X.  You may manually find
; the appropriate include file for your device here and include it, or
; simply copy the include generated by the configuration bits
; generator (see Step #2).
;
;*******************************************************************************

; TODO INSERT INCLUDE CODE HERE
#include "p16F18323.inc"

;*******************************************************************************
;
; TODO Step #2 - Configuration Word Setup
;
; The 'CONFIG' directive is used to embed the configuration word within the
; .asm file. MPLAB X requires users to embed their configuration words
; into source code.  See the device datasheet for additional information
; on configuration word settings.  Device configuration bits descriptions
; are in C:\Program Files\Microchip\MPLABX\mpasmx\P<device_name>.inc
; (may change depending on your MPLAB X installation directory).
;
; MPLAB X has a feature which generates configuration bits source code.  Go to
; Window > PIC Memory Views > Configuration Bits.  Configure each field as
; needed and select 'Generate Source Code to Output'.  The resulting code which
; appears in the 'Output Window' > 'Config Bits Source' tab may be copied
; below.
;
;*******************************************************************************

; TODO INSERT CONFIG HERE

; PIC16F18313 Configuration Bit Settings

; ASM source line config statements



; CONFIG1
; __config 0xFFFF
 __CONFIG _CONFIG1, _FEXTOSC_ECH & _RSTOSC_EXT1X & _CLKOUTEN_OFF & _CSWEN_ON & _FCMEN_ON
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _MCLRE_ON & _PWRTE_OFF & _WDTE_OFF & _LPBOREN_OFF & _BOREN_ON & _BORV_LOW & _PPS1WAY_ON & _STVREN_ON & _DEBUG_OFF
; CONFIG3
; __config 0x2003
 __CONFIG _CONFIG3, _WRT_OFF & _LVP_ON
; CONFIG4
; __config 0x3
 __CONFIG _CONFIG4, _CP_OFF & _CPD_OFF





;*******************************************************************************
;
; TODO Step #3 - Variable Definitions
;
; Refer to datasheet for available data memory (RAM) organization assuming
; relocatible code organization (which is an option in project
; properties > mpasm (Global Options)).  Absolute mode generally should
; be used sparingly.
;
; Example of using GPR Uninitialized Data
;
;   GPR_VAR        UDATA
;   MYVAR1         RES        1      ; User variable linker places
;   MYVAR2         RES        1      ; User variable linker places
;   MYVAR3         RES        1      ; User variable linker places
;
;   ; Example of using Access Uninitialized Data Section (when available)
;   ; The variables for the context saving in the device datasheet may need
;   ; memory reserved here.
;   INT_VAR        UDATA_ACS
;   W_TEMP         RES        1      ; w register for context saving (ACCESS)
;   STATUS_TEMP    RES        1      ; status used for context saving
;   BSR_TEMP       RES        1      ; bank select used for ISR context saving
;
;*******************************************************************************

; TODO PLACE VARIABLE DEFINITIONS GO HERE
temp_1	equ     H'30'	;delay count
temp_2	equ	H'31'	;delay count
temp_3	equ	H'32'	;delay count
	
RandHi	equ	H'33'	;Random Number Hi Byte
RandLo	equ	H'34'	;Random Number Lo Byte
RandMs	equ	H'35'	;Masked selection bits
temp_ncl    equ	H'36'	;Temp register to compute Rotate Left without carry
			;used in RNG
Int_Call    equ	H'37'	;Flag for interrupt being recently called
			
STATUS_TEMP equ	    H'38'   ;Store status register on interrupt
W_TEMP equ	    H'39'   ;Storing WREG on interrupt, and restoring bank
    
	
;PORTA

PORTA   equ     00Ch
LATA    equ     10Ch
ANSELA  equ     18Ch
TRISA   equ     08Ch

   
;PORTC
   
PORTC	equ	00Eh
LATC	equ	10Eh
ANSELC	equ	18Eh
TRISC	equ	08Eh
	

;T2
	
T2CON	equ	01Fh
PR2	equ	01Eh
TMR2	equ	01Dh
   
	
;Interrupts

PIR0	equ	010h
INTCON  equ	00Bh	;Can be accessed from any Bank
IOCAP	equ	391h
IOCAN	equ	392h
IOCAF	equ	393h
PIE0	equ	090h

	
;*******************************************************************************
; Reset Vector
;*******************************************************************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;*******************************************************************************
; TODO Step #4 - Interrupt Service Routines
;
; There are a few different ways to structure interrupt routines in the 8
; bit device families.  On PIC18's the high priority and low priority
; interrupts are located at 0x0008 and 0x0018, respectively.  On PIC16's and
; lower the interrupt is at 0x0004.  Between device families there is subtle
; variation in the both the hardware supporting the ISR (for restoring
; interrupt context) as well as the software used to restore the context
; (without corrupting the STATUS bits).
;
; General formats are shown below in relocatible format.
;
;------------------------------PIC16's and below--------------------------------
;
ISR       CODE    0x0004           ; interrupt vector location
       
       GOTO	PBISR

;
;     <Search the device datasheet for 'context' and copy interrupt
;     context saving code here.  Older devices need context saving code,
;     but newer devices like the 16F#### don't need context saving code.>
;
;
;----------------------------------PIC18's--------------------------------------
;
; ISRHV     CODE    0x0008
;     GOTO    HIGH_ISR
; ISRLV     CODE    0x0018
;     GOTO    LOW_ISR
;
; ISRH      CODE                     ; let linker place high ISR routine
; HIGH_ISR
;     <Insert High Priority ISR Here - no SW context saving>
;     RETFIE  FAST
;
; ISRL      CODE                     ; let linker place low ISR routine
; LOW_ISR
;       <Search the device datasheet for 'context' and copy interrupt
;       context saving code here>
;     RETFIE
;
;*******************************************************************************

; TODO INSERT ISR HERE
PBISR	;Push button press/release ISR, create random number
	
	
        BANKSEL	    IOCAF
        MOVLW	    B'11111111'	;Mask to clear external interrupt flag
        XORWF	    IOCAF, W	;
        ANDWF	    IOCAF, F	;Clear interrupt flag
	
	
	BANKSEL	    Int_Call
	BTFSS	    Int_Call, 0
	GOTO	    STR_Lo
       
STR_Hi
	
	BANKSEL	    TMR2	;Select Bank containing TMR2	(TIMER2 COUNT)
	MOVF	    TMR2, W	;Move count to working register
	;Move timer data into RandHi
	BANKSEL	    RandHi
	MOVWF	    RandHi	;Move TMR2 data into RandHi
	
	;Set the interrupt call flag
	MOVLW	    B'00000010'
	IORWF	    Int_Call	;High byte and Low byte ares stored
	
;	BANKSEL	    IOCAN
;	BCF	    IOCAN, 2	;Disable interrupt on falling edge RA2
;	BANKSEL	    IOCAP
;	BSF	    IOCAP, 2	;Enable interrupt on rising edge RA2
	
	RETFIE
	
STR_Lo
	
	BANKSEL	    TMR2	;Select Bank containing TMR2	(TIMER2 COUNT)
	MOVF	    TMR2, W	;Move count to working register
	;TMove timer data into RandLo
	BANKSEL	    RandLo
	MOVWF	    RandLo	;Move TMR2 data to RandLo
	
	;Set the interrupt call flag
	MOVLW	    B'00000001'	
	MOVWF	    Int_Call	;Write to Int_CalL, Low byte is now stored
	
;	BANKSEL	    IOCAP
;	BCF	    IOCAP, 2	;Disable interrupt on rising edge RA2
;	BANKSEL	    IOCAN
;	BSF	    IOCAN, 2	;Enable interrupt on falling edge RA2

	
	RETFIE			;Return from interrupt call

;*******************************************************************************
; MAIN PROGRAM
;*******************************************************************************

MAIN_PROG CODE                      ; let linker place main program

START

    ; TODO Step #5 - Insert Your Program Here
    
Varinit
	BANKSEL	    RandHi  ;Select Bank containing RNG seed/solution
	MOVLW	    0x30   ;Seed for the High byte
	MOVWF	    RandHi  ;Move 30 into high byte
	MOVLW	    0x45   ;Seed for the Low byte
	MOVWF	    RandLo  ;Move 45 into low byte

Ainit
        BANKSEL     PORTA   ;Select Bank containing PORTA   (OUTPUT READ)
        CLRF        PORTA   ;Clear contents of PORTA
        BANKSEL     LATA    ;Select Bank containing LATA    (OUTPUT LATCH)
        CLRF        LATA    ;Clear LATA
        BANKSEL     ANSELA  ;Select Bank containing ANSELA (ANALOG)
        CLRF        ANSELA  ;Clear ANSELA
        BANKSEL     TRISA   ;Select Bank containing TRISA   (DIRECTION)
        MOVLW       B'00001100'
        MOVWF       TRISA   ;[PIN 4, 5 is LED output][2 is PB input]
    
Cinit
	BANKSEL	    PORTC   ;Select Bank containing PORTC	(OUTPUT READ)
        CLRF        PORTC   ;Clear contents of PORTC
        BANKSEL     LATC    ;Select Bank containing LATC    (OUTPUT LATCH)
        CLRF        LATC    ;Clear LATC
        BANKSEL     ANSELC  ;Select Bank containing ANSELC (ANALOG)
        CLRF        ANSELC  ;Clear ANSELC
        BANKSEL     TRISC   ;Select Bank containing TRISC   (DIRECTION)
        MOVLW       B'00000000'
        MOVWF       TRISC   ;[PIN 1 - 5 are LED outputs]
	
T2init	;Setup timer counter 2 to have 64 prescaler, 1 postscaler, and count to top
	BANKSEL	    PR2	    ;Select Bank containing PR2	    (TIMER 2 PERIOD)
	MOVLW	    .255
	MOVWF	    PR2	    ;T2 will count to top 255
	BANKSEL	    T2CON   ;Select Bank containing T2CON	(TIMER 2 CONTROL)
	MOVLW	    B'00000111'
	MOVWF	    T2CON   ;1 postscaler, 64 prescaler, timer ON

INTinit	;enable interrupts for change on RA4
	BANKSEL	    IOCAN
	BSF	    IOCAN, 2	;Enable interrupt on falling edge RA2
	BANKSEL	    IOCAP
	BSF	    IOCAP, 2	;Enable interrupt on positive edge RA2
	BANKSEL	    PIE0
	BSF	    PIE0, 4	;Enable interrupt on change
	;Can always access INTCON
	BSF	    INTCON, 7	;Enable global interrupts
	BSF	    INTCON, 6	;Enable peripheral interrupts
	
Display_LED
	
	
;	BANKSEL	    LATA
;	BSF	    LATA, 4
;	
;	BANKSEL	    PORTA
;	BTFSC	    PORTA, 2
;	GOTO	    TEST_JMP
;	
;	BANKSEL	    LATA
;	BCF	    LATA, 4
	
	
TEST_JMP
	
	;CALL	    Random16
	BANKSEL	    RandLo  ;Select Random Data Bank
	NOP
	NOP
	BTFSS	    Int_Call, 1	;Check Int_Call variable '0' bit flag
	GOTO	    Display_LED	;Goto Display LED if no new random number
	;Interrupt has been called, there is a new random number
	

	
					;Clear Int_Call
	MOVLW	    B'00000000'	;Clear interrupt call flag
	MOVWF	    Int_Call	;Reset interrupt call flag
	
;	BANKSEL	    LATC
;	BSF	    LATC, 2
	
	CALL	    Delay
	CALL	    Delay
	

Cycle_Rand	
	
	CALL	    Random16	    ;Initially randomize the 16bit number
	
	;Mask out three bits to test for a good number [1 - 6]
	MOVLW	    B'00000111'
	BANKSEL	    RandLo
	ANDWF	    RandLo, W
	MOVWF	    RandMs
	
	;Clear current LED display
	CALL	    Clear_LED
	
	;Spin LEDs for a short time
	CALL	    Spin_LED
	
	
	BANKSEL	    RandMs	 ;Select Random Bank Masked
	
	BTFSS	    RandMs, 2
	GOTO	    R_0xx
	BTFSS	    RandMs, 1
	GOTO	    R_10x
	BTFSS	    RandMs, 0
R_110	GOTO	    Disp_6
R_111	GOTO	    Cycle_Rand		;7, reroll
R_0xx	BTFSS	    RandMs, 1
	GOTO	    R_00x
	BTFSS	    RandMs, 0
R_010	GOTO	    Disp_2
R_011	GOTO	    Disp_3
R_10x	BTFSS	    RandMs, 0
R_100	GOTO	    Disp_4
R_101	GOTO	    Disp_5
R_00x	BTFSS	    RandMs, 0
R_001	GOTO	    Disp_1
R_000	GOTO	    Cycle_Rand		;0, reroll
	
	;Setup for displaying different numbers
	
	;Display 1
Disp_1
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	BSF	    LATC, 1
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	
	GOTO	    Display_LED
	
	
	;Display 2
Disp_2
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	BSF	    LATC, 2
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	BSF	    LATA, 5
	
	GOTO	    Display_LED
	
	
	
		;Display 3
Disp_3
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	MOVLW	    B'00000110'
	IORWF	    LATC, F
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	BSF	    LATA, 5
	
	GOTO	    Display_LED
	
		;Display 4
Disp_4
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	MOVLW	    B'00110100'
	IORWF	    LATC, F
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	BSF	    LATA, 5
	
	GOTO	    Display_LED
	
	
		;Display 5
Disp_5
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	MOVLW	    B'00110110'
	IORWF	    LATC, F
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	BSF	    LATA, 5
	
	GOTO	    Display_LED
	
	
		;Display 6
Disp_6
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	MOVLW	    B'00111100'
	IORWF	    LATC, F
	;------------------------------------------
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	MOVLW	    B'00110000'
	IORWF	    LATA, F
	
	GOTO	    Display_LED
	
	
;	MOVF	    RandLo, W	;Move Lower Byte into WREG
;	BANKSEL	    LATC    ;Select LATC, PORTC OUTPUT LATCH
;	MOVWF	    LATC    ;Turn on LEDs on PORTC
;	RRF	    WREG, F
;	RRF	    WREG, F
;	RRF	    WREG, F
;	RRF	    WREG, F
;	BANKSEL	    LATA    ;Select LATA, PORTA OUTPUT LATCH
;	ANDLW	    B'00000100'	;Mask out all but RA2
;	MOVWF	    LATA    ;Turn on LEDs on PORTA
;	CALL	    Delay
	
	
Delay
	BANKSEL	    temp_1	;Select Bank with Delay registers
	MOVLW	    0x7F
	MOVWF	    temp_1
	MOVWF	    temp_2
;	MOVLW	    0x0B
	MOVLW	    0x05
	MOVWF	    temp_3
	
	DECFSZ	    temp_1, f
	GOTO	    $-1
	
	DECFSZ	    temp_2, 1
	GOTO	    $-3
	
	DECFSZ	    temp_3, 1
	GOTO	    $-5
	
	RETURN
	
Random16
	BANKSEL	    RandHi	;Select Bank containing the random number solution
	RLF	    RandHi, W
	XORWF	    RandHi, W
	RLF	    WREG, F	;carry bit = xorwf(15,14)
	
	SWAPF	    RandHi, F
	SWAPF	    RandLo, W
	
	;   Rotate WREG Left with no carry
	MOVWF	    temp_ncl	;store WREG
	RLF	    temp_ncl, W
	RLF	    temp_ncl, F	;temp_ncl now has rotate left without carry
	RRF	    WREG, F	;restore carry bit
	MOVF	    temp_ncl, W	;move rotate left without carry to WREG
	;
	
	XORWF	    RandHi, W	;LSB = xorwf(12,3)
	
	SWAPF	    RandHi, F
	ANDLW	    0x01	;Mask out all but LSB
	RLF	    RandLo, F
	XORWF	    RandLo, F	;Create random LSB
	RLF	    RandHi, F	;Rotate left RandHi
	
	RETURN
	
Clear_LED
	MOVLW	    B'11000001'
	BANKSEL	    LATC
	ANDWF	    LATC, F
	
	MOVLW	    B'11001111'
	BANKSEL	    LATA
	ANDWF	    LATA, F
	
	RETURN
		
Spin_LED
	BANKSEL	    LATA
	BSF	    LATA, 5
	CALL	    Delay
	CALL	    Clear_LED
	
	BANKSEL	    LATA
	BSF	    LATA, 4
	CALL	    Delay
	CALL	    Clear_LED
	
	BANKSEL	    LATC
	BSF	    LATC, 5
	CALL	    Delay
	CALL	    Clear_LED
	
	BANKSEL	    LATC
	BSF	    LATC, 4
	CALL	    Delay
	CALL	    Clear_LED
	
	BANKSEL	    LATC
	BSF	    LATC, 3
	CALL	    Delay
	CALL	    Clear_LED
	
	BANKSEL	    LATC
	BSF	    LATC, 2
	CALL	    Delay
	CALL	    Clear_LED
	
	RETURN
	
	
    END
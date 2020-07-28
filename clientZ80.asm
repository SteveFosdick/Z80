; Z80 Tube Client Source Code
; ===========================

; Commentary Copyright (C) J.G.Harston
; See mdfs.net/Software/Tube

            .processor  Z80
            .org        $0000
            .rorg       $F000

; On RESET, ROM is paged into low memory. ROM is paged out by an
; instruction fetch from an address in the top 32K of memory.

RESET:      DI
            LD      DE,RESET    ; Copy code from low memory to
            LD      HL,$0000    ; high memory
            LD      BC,$1000
            LDIR
            JP      STARTUP     ; Enter code in high memory

            DB      "Copyright Acorn Computers Ltd. 1984"
            DB      $0D
            DB      "Patch by J. G. Harston"
            DB      $0D
            DS      $66-$$

; NMI code, paged in when NMI occurs
; ----------------------------------

LF066:      JP      LFC61       ; Jump to NMI handler in high memory

; Acknowledgments
; ---------------
            DB      "The Business Systems Group would like to thank "
            DB      "Mike Bolley, Mike $ Allen Boothroyd, Richard "
            DB      "Clayton, Andrew Gordon, Chris Hall, Kim "
            DB      "Spence-Jones, Paul Overell, David Parkinson,"
            DB      "John Tuten and Eric the half TUBE.  The BSG is "
            DB      "Big Arthur The Toucan, J Mark Carrington, "
            DB      "Howard Fisher, Ian G Jack, Neil Robinson, "
            DB      "Simon Woodward, John Corrall, Toby Cross, "
            DB      "Ian Miller, Boris Southears."

            DS      $53         ; adjust to get the vectors right.

; STARTUP - Startup routine
; =========================
; Tube data  via R1, string $00  --  via R2, $7F or $80

STARTUP:    LD      SP,SysStack ; Initialise a stack
            CALL    InitErr     ; Initialise RST $38 error call
            LD      A,$FF       ; Set interrupts to vector via $FFFE
            LD      I,A
            IM      2
            EI
            CALL    PrText      ; Print banner
            DB      22,8,13     ; MODE 8, usually gives MODE 0
            DB      "Acorn TUBE Z80 64K 1.20"
            DB      13,13,0     ; NEWL, NEWL
            CALL    WaitR2      ; Wait for, but ignore,response from TubeR2
            CALL    InitFF      ; Set up OSWORD $FF code
            LD      A,$FD       ; Fetch BREAK type
            LD      HL,$FF00
            CALL    osBYTE
            LD      A,L
            OR      A
            JP      Z,CLIPrompt ; If a soft BREAK, enter CLI Prompt
            LD      A,$0F       ; Clear buffers
            LD      HL,$0001
            CALL    osBYTE
            JP      EnterCode   ; Try to enter current program or boot CPM

LF2CB:      CALL OSNEWL

; Minimal Command prompt
; ======================
;
; Used as a default language.

CLIPrompt:  LD      SP,SysStack ; Set up a stack
LF2D1:      LD      A,(ESCFLG)  ; Skip if no pending Escape
            BIT     7,A
            JR      Z,LF2E0
LF2D8:      LD      A,$7E       ; Acknowledge Escape
            LD      HL,$0000
            CALL    osBYTE
LF2E0:      LD      A,'*'       ; Print '*' prompt
            CALL    osWRCH             ;
            LD      HL,LFC9D    ; Read a line
            XOR     A
            CALL    osWORD
            JP      C,LF2FA     ; Jump if Escape pressed
            LD      HL,LFCB0
            CALL    osCLI       ; Pass line to OSCLI
            JR      CLIPrompt   ; Loop back for another

LF2F7:      CALL    OSNEWL
LF2FA:      LD      A,$7E       ; Ack. escape condition
            CALL    osBYTE
            RST     $38         ; Generate error
            DB      0
            DB      "Escape"
            DB      0

; *GO <addr> - call or enter machine code
; =======================================

LF308:      INC     DE          ; Fetch next character
            LD      A,(DE)
            AND     $DF
            CP      'O'         ; Not '*GO', jump to pass to Tube
            JP      NZ,LF7CC
            CALL    SkipSpace1  ; Skip past any spaces
            LD      B,$00
            CALL    ScanHex     ; Read hex value
            CALL    SkipSpaces  ; and move past spaces
            CP      $0D
            JP      NZ,LF7CC    ; More parameters, pass to Tube to deal with
            LD      A,(ADDR+0)  ; Set program start to address read
            LD      (PROG+0),A
            LD      A,(ADDR+1)
            LD      (PROG+1),A
            JP      LF7DF       ; Jump to execute in non-CPM mode


; *S <addr> - set memory
; ========================

LF330:      CALL    SkipSpace1  ; Skip past any spaces
            LD      B,$00
            CALL    ScanHex     ; Read hex value
            CALL    SkipSpaces  ; and move past spaces
            CP      $0D         ; More parameters, pass to Tube to deal with
            JP      NZ,LF7CC
            LD      A,$04       ; Cursor keys return characters
            LD      HL,$0001
            CALL    OSBYTE
            LD      A,L         ; Save old cursor key setting
            LD      (OLDFX4),A             ;
            LD      HL,(ADDR)   ; Get initial address
LF34F:      CALL    OSNEWL
            CALL    Pr2Hex      ; print address
            CALL    PrSpc       ; then space
LF358:      LD      A,(HL)      ; get byte
            CALL    PrChDot     ; print character or dot
            CALL    PrSpc       ; print a space.
            LD      A,(HL)      ; get byte again
            CALL    PrHex       ; Print byte as hex
            PUSH    HL          ; save address
            LD      B,$01       ; B=fetch 1 digit
            LD      HL,$0000    ; HL=accumulator
            CALL    OSRDCH      ; Get key.
            CALL    LF440       ; Add hex digit to accumulator
            LD      B,$00
            LD      E,L         ; E=hex digit
            POP     HL          ; restore address
            CP      $8A         ; If up, jump to move back
            JR      Z,LF39E
            CP      $8B         ; If down, jump to move forward
            JR      Z,LF39B
            CP      $01         ; If not a valid digit, jump to exit
            JP      NZ,LF3A1               ;
            LD      A,(HL)      ; Get current byte
            SLA     A           ; Multiply by 16
            SLA     A
            SLA     A
            SLA     A
            ADD     A,E         ; Add entered digit
            LD      (HL),A      ; and store
            LD      A,$08       ; Move back 4 chars to overwrite
            CALL    osWRCH
            CALL    osWRCH
            CALL    osWRCH
            CALL    osWRCH
            JR      LF358       ; Loop back for another keypress
LF39B:      INC     HL          ; Increment address
            JR      LF34F       ; jump to display
LF39E:      DEC     HL          ; Decrement address
            JR      LF34F       ; jump to display
;
; Exit from *S
; ------------

LF3A1:      LD      A,(OLDFX4)  ; Get old cursor key status
            LD      L,A
            LD      H,$00       ; Restore cursor key state
            LD      A,$04
            CALL    OSBYTE
            CALL    OSNEWL
            POP     DE          ; Restore registers and return
            POP     BC
            POP     AF
            RET

; *D <addr> (<addr>) - dump memory
; ================================

LF3B3:      CALL    SkipSpace1  ; Skip past any spaces
            LD      B,$00
            CALL    ScanHex     ; Read hex value
            CP      $0D         ; No more parameters, jump to dump
            JR      Z,LF3CF
            CP      ' '         ; Wasn't a hex number, jump to pass to Tube
            JP      NZ,LF7CC
            LD      HL,(ADDR)   ; Get first address for start address
            CALL    SkipSpaces  ; skip spaces.
            CALL    ScanHex     ; and read hex value for end address
            JR      LF3D2       ; Jump to dump between addresses
;
; *D <addr>
; ---------

LF3CF:      LD      HL,(ADDR)
LF3D2:      LD      DE,(ADDR)
LF3D6:      LD      A,(ESCFLG)
            BIT     7,A         ; If Escape set, jump to exit
            JP      NZ,LF2F7
            CALL    OSNEWL
            CALL    Pr2Hex      ; Print address
            LD      B,$08       ; B=8 for eight bytes
            PUSH    HL          ; save address
LF3E7:      CALL    PrSpc       ; Print space.
            LD      A,(HL)      ; print the hex byte
            CALL    PrHex
            INC     HL
            DJNZ    LF3E7       ; Loop to print 8 bytes
            LD      B,$08       ; B=8 for eight bytes
            POP     HL          ; get address back
            CALL    PrSpc       ; Print another space
LF3F7:      LD      A,(HL)
            CALL    PrChDot     ; Print byte as a character
            INC     HL
            DJNZ    LF3F7       ; Loop to print 8 characters
            CALL    LF423       ; check address
            JP      NC,LF406    ; jump to exit if at end
            JR      LF3D6       ; Loop back to dump another 8 bytes
;
; Exit *D
; -------
LF406:      CALL    OSNEWL
            POP     DE          ; Restore registers
            POP     BC
            POP     AF
            RET                 ;  and return
;
; Print character or dot
; ======================

PrChDot:    CP      ' '         ; control character, print a dot
            JR      C,LF417
            CP      $7F         ; DEL or topbit set, print a dot
            JR      NC,LF417               ;
            JR      LF419       ; Jump to print character
LF417:      LD      A,'.'       ; Print a dot if not SPC-'~'
LF419:      CALL    osWRCH
            RET

PrSpc:      LD      A,' '       ; print a space.
            CALL    osWRCH
            RET
;
; Check address against end
; -------------------------

LF423:      PUSH    HL
            LD      BC,$0008
            CP      A
            SBC     HL,BC       ; HL=ADDR-8
            JR      NC,LF432    ; If ADDR-8>0, jump to check end
            LD      HL,$0000    ; ADDR-8<0, replace with $0000
            CP      A           ; clear carry
            JR      LF434       ; jump to check end
LF432:      POP     HL          ; Get address back
            PUSH HL
LF434:      DEC     HL          ; Check against end address in DE
            SBC     HL,DE
            POP     HL          ; Restore address
            RET                 ; C=not at end

; Scan hex value from line
; ========================
; On entry, DE=>first hex character to scan
;            B=1, scan one digit
;            B<>1, scan all digits
; On exit,  DE=>first nonhex character
;           BC,HL preserved
;           A=first nonhex character

ScanHex:    LD      (CTRL),HL   ; save HL
            LD      HL,$0000    ; clear accumulator
LF43F:      LD      A,(DE)      ; Get current character
;
; Check A for hex digit
; ---------------------
LF440:      BIT     6,A         ; If lower case, skip past
            JR      Z,LF446
            AND     $DF         ; Convert lower case to upper
LF446:      CP      '0'         ; If <'0', exit
            JP      M,LF46D
            CP      'G'         ; If >'G', exit
            JP      P,LF46D                ;
            CP      $3A         ; If '0'..'9', add to accumulator
            JR      C,LF45B                ;
            CP      'A'         ; If >'9', <'A', exit
            JP      M,LF46D
            ADD     A,$09       ; Convert letter
LF45B:      AND     $0F         ; Keep bottom nybble
            ADD     HL,HL       ; *16
            ADD     HL,HL
            ADD     HL,HL
            ADD     HL,HL
            OR      L           ; Add in current digit
            LD      L,A
            LD      A,$01       ; If B=1, return single digit
            CP      B
            RET     Z
            INC     DE          ; Step to next character
            LD      (ADDR),HL   ; Store value
            JR      LF43F       ;  and loop for next digit
LF46D:      LD      HL,(CTRL)   ; restore HL
            RET                 ; and return

; Pr2Hex - Print HL as hex string
; ===============================

Pr2Hex:     LD      A,H
            CALL    PrHex
            LD      A,L

; Prhex - Print a as hex string
; =============================

PrHex:      PUSH AF
            RRCA                ; shift top nybble into bottom.
            RRCA
            RRCA
            RRCA
            CALL    LF47F       ; print first digit.
            POP     AF
LF47F:      AND     $0F
            ADD     A,'0'
            CP      $3A
            JP      M,osWRCH
            ADD     A,$07
            JP      osWRCH

; *CPM - boot CPM
; ===============

LF48D:      INC     DE          ; Get next character in upper case
            LD      A,(DE)
            AND     $DF
            CP      'P'         ; Not '*CP', jump to pass to Tube
            JP      NZ,LF7CC
            INC     DE          ; Get next character in upper case
            LD      A,(DE)
            AND     $DF
            CP      'M'
            JP      NZ,LF7CC    ; Not '*CPM', jump to pass to Tube
            CALL    SkipSpace1  ; Skip any spaces
            CP      $0D
            JP      NZ,LF7CC    ; More characters, jump to pass to Tube
            JR      LF4CF       ; Jump to boot CPM

LF4A9:      CALL    PrText      ; Print in-line text
            DB      "Insert CP/M System disc in drive A"
            DB      0


; Boot CPM - Load BIOS, BDOS and CCP
; ==================================

LF4CF:      LD      A,$E5       ; Disable Escape
            LD      HL,$0001
            CALL    osBYTE
            LD      HL,LF582
            CALL    LF54A       ; Load BIOS code from sectors 0-7
            LD      HL,$EB00    ; Move code down over DFS names
            LD      DE,$EAF0
            LD      BC,$00F0
            LDIR
            LD      HL,$EBF0    ; Move code down over DFS addresses
            LD      DE,$EBE0
            LD      BC,$0600
            LDIR
            CALL    LoadCCP     ; Load CCP and BDOS from sectors 8-29
            LD      A,($D400)   ; Get first byte from CCP
            CP      $C3         ; If it is 'JP opcode, jump to enter BDOS
            JP      Z,LF522
            LD      A,$E5       ; Enable Escape
            LD      HL,$0000
            CALL    osBYTE
            CALL    OSNEWL      ; Print newline
            RST     $38         ; Generate error
            DB      0
            DB      "Not a CP/M System disc"
            DB      0

LF522:      LD      HL,DfltErrHnd
            LD      (BRKV),HL   ; Set up default error handler
            LD      A,$90       ; Set flag.
            LD      (LFCA3),A
            LD      HL,$0100
            CALL    osBYTE      ; Do *TV 0,1
            CALL    PrText      ; Do MODE 0.
            DB      $16
            DB      0
            JP      $EA00       ; Enter BDOS

; Load CCP - also loads BDOS
; ==========================

LoadCCP:    LD      HL,LF58D    ; Load CCP from sectors 8-9
            CALL    LF54A
            LD      HL,LF598    ; Load CCP and BDOS from sectors 10-19
            CALL    LF54A
            LD      HL,LF5A3    ; Load BDOS from sectors 20-29
LF54A:      CALL    DiskAccess  ; Read data from disk
            OR      A           ; Check returned status
            RET     Z           ; Return if no error
            RST     $38         ; Generate error
            DB      $C7
            DB      "Disc fault"
            DB      0

; Load from FM disk using control block pointed to by HL
; ======================================================

DiskAccess: LD      B,$04       ; We're going to do four main passes,
LF55E:      LD      C,B         ; and in each pass we try ten times
            LD      B,$0A       ; before reseeking track zero
LF561:      LD      A,$7F       ; OSWORD $7F - DiskOpFM
            CALL    osWORD
            LD      DE,$000A
            EX      DE,HL       ; Save pointer in DE
LF56A:      ADD     HL,DE       ; Point to result byte
            LD      A,(HL)      ; Fetch it
            LD      (RESULT),A  ; Store it in main result store
            EX      DE,HL       ; Get pointer back to HL
            CP      $12         ; DskFM_ReadOnly?
            RET     Z           ; Return if so
            OR      A           ; DskFM_Ok?
            RET     Z           ; Return is so
            DJNZ    LF561       ; Loop back up to ten times to retry
            LD      A,(HL)      ; Get drive number from control block
            CALL    Seek0       ; Seek track 0 on this drive
            LD      B,C
            DJNZ    LF55E       ; Loop back up to 4 times
            LD      A,(RESULT)  ; Get result byte. By here it will be
            RET                 ; an error other than ReadOnly

; OSWORD control blocks for disk reads
; ====================================

; Load BIOS code.

LF582:      DB      $00         ; Drive 0
            DW      $E9F0       ; Load to $0000E9F0
            DW      $0000
            DB      $03
            DB      $53         ; Command=Load
            DB      $00         ; Track 0
            DB      $00         ; Sector 0
            DB      $28         ; 8 sectors
            DB      $FF

; Load start of CCP code

LF58D:      DB      $00         ; Drive 0
            DW      $D400       ; Load to $0000D400
            DW      $0000
            DB      $03
            DB      $53         ; Command=Load
            DB      $00         ; Track 0
            DB      $08         ; Sector 8
            DB      $22         ; 2 sectors
            DB      $FF

; Load CCP and BDOS code

LF598:      DB      $00         ; Drive 0
            DW      $D600       ; Load to $0000D600
            DW      $0000
            DB      $03
            DB      $53         ; Command=Load
            DB      $01         ; Track 1
            DB      $00         ; Sector 0
            DB      $2A         ; 10 sectors
            DB      $FF

; Load end of BDOS code

LF5A3:      DB      $00         ; Drive 0
            DW      $E000
            DW      $0000       ; Load to $0000E000
            DB      $03
            DB      $53         ; Command=Load
            DB      $02         ; Track 2
            DB      $00         ; Sector 0
            DB      $2A         ; 10 sectors
            DB      $FF

; Initialise OSWORD $FF code in the I/O processor
; ===============================================
; Copies the code over, copies USERV to the code, and then sets
; USERV to call the code.
;
; BBCBASIC ROM puts patch code here
;
InitFF:     DI                  ; Disable interrupts while copying code
            LD      HL,$2500    ;  Copy to $2500 in i/o processor
            LD      DE,OswFF    ;  from $FD30 in Z80 processor
            LD      B,END6502-OswFF
            CALL    LF5F3       ; Copy OSWORD $FF code over
            LD      HL,$0200    ;  Copy from $0200 in i/o processor
            LD      DE,LFDDC    ;  to $FDDC in Z80 processor
            LD      B,$02       ;  2 bytes
            CALL    LF5DD       ; Fetch USERV
            LD      HL,$2503    ;  Copy to $2503 in i/o processor
            LD      DE,LFDDC    ;  from $FDDC in Z80 processor
            LD      B,$02       ;  2 bytes
            CALL    LF5F3       ; Set oldUSERV in OSWORD $FF code
            LD      HL,$0200    ;  Copy to $0200 in i/o processor
            LD      DE,LFDDA    ;  from $FDDA in Z80 processor
            LD      B,$02       ;  2 bytes
            CALL    LF5F3       ; Set USERV to point to OSWORD $FF code
            EI                  ; Turn interrupts back on
            RET

LF5DD:      LD      (LF609),HL
            PUSH    HL
            LD      HL,LF609
            LD      A,$05
            CALL    OSWORD
            LD      A,(LF60D)
            LD      (DE),A
            INC     DE
            POP     HL
            INC     HL
            DJNZ    LF5DD
            RET

LF5F3:      LD      (LF609),HL
            LD      A,(DE)
            LD      (LF60D),A
            PUSH    HL
            LD      HL,LF609
            LD      A,$06
            CALL    OSWORD
            INC     DE
            POP     HL
            INC     HL
            DJNZ    LF5F3
            RET

; Control block for I/O memory read/write

LF609:      DB      $00, $00, $00, $00
LF60D:      DB      $00
;
; BBCBASIC ROM puts OSCLI stack here
;
PrText:     EX      (SP),HL     ; return address into HL which then
            PUSH    AF          ; points to the character string.
LF610:      LD      A,(HL)
            CALL    OSASCI
            INC     HL
            OR      A
            JR      NZ,LF610
            POP     AF
            EX      (SP),HL     ; return to byte after string.
            RET
;
; Seek to track zero on drive supplied in A
;
Seek0:      PUSH    AF
            PUSH    HL
            LD      (LF636),A
            LD      HL,LF636
            LD      A,$7F
            LD      (DISKSP),SP
            LD      SP,SysStack ; *BUG* This overwites stack when called from *CPM
            CALL    osWORD
            LD      SP,(DISKSP)
            POP     HL
            POP     AF
            RET

LF636:      DB   $00, $00, $00, $00, $00, $01, $69, $00, $00

KeyTest:    PUSH    HL
            LD      HL,$FFFF
            LD      A,$80
            CALL    osBYTE
            LD      A,L
            OR      A
            JR      Z,LF64E
            JR      LF65A
LF64E:      LD      A,$D8
            LD      HL,$FF00
            CALL    osBYTE
            LD      A,L
            OR      A
            JR      Z,LF65C
LF65A:      LD      A,$FF
LF65C:      POP     HL
            RET

InitErr:    PUSH    BC
            PUSH    DE
            PUSH    HL
            LD      HL,RST38
            LD      DE,$0038
            LD      BC,$0003
            LDIR
            POP     HL
            POP     DE
            POP     BC
            RET

EventHand:  RET

; MOS INTERFACE
; =============
;
;
; OSWRCH - Send character to output stream
; ========================================
; On entry, A =character
; On exit,  A =preserved
;
; Tube data  character  --

osWRCH:     PUSH    AF          ; Save character
LF672:      IN      A,($00)     ; Read Tube R1 status
            BIT     6,A         ; Loop until b6 set
            JR      Z,LF672
            POP     AF
            OUT     ($01),A     ; Send character to Tube R1
            RET


; Wait for byte in R1 while allowing requests via R4
; ==================================================

LF67C:      IN      A,($00)     ; Check R1 status
            BIT     7,A         ; Data present, fetch it
            JR      NZ,LF68D
            IN      A,($06)     ; Check R4 status
            BIT     7,A         ; No data, loop back to check R1
            JR      Z,LF67C
            CALL    R4ErrXfr    ; Process R4 interrupt
            JR      LF67C       ; Jump back to wait for R1
LF68D:      IN      A,($01)     ; Read byte from R1
            RET

; OSRDCH - Wait for character from input stream
; =============================================
; On exit, A =char, Cy=Escape flag
;
; Tube data  $00  --  Carry Char

osRDCH:     LD      A,$00       ; Send command $00 - OSRDCH
            CALL    SendR2
WaitCarCh:  CALL    WaitR2      ; Wait for Cy and A
            SLA     A
WaitR2:     IN      A,($02)
            BIT     7,A
            JR      Z,WaitR2
            IN      A,($03)
            RET

SendR2:     PUSH    AF
LF6A4:      IN      A,($02)
            BIT     6,A
            JR      Z,LF6A4
            POP     AF
            OUT     ($03),A
            RET

; Wait for byte in R4
; ===================

WaitR4:     IN      A,($06)
            BIT     7,A
            JR      Z,WaitR4
            IN      A,($07)
            RET

; OSCLI - Execute command
; =======================
; On entry, HL=>command string
; On exit,  HL= corrupted

osCLI:      PUSH    AF
            PUSH    BC
            PUSH    DE
            LD      D,H
            LD      E,L
LF6BC:      CALL    SkipStars
            CALL    SkipSpaces
            CP      '*'
            JR      Z,LF6BC
            AND     $DF
            CP      'H'         ; Check for '*HELP'
            JR      Z,LF6EC
            LD      C,A
            LD      A,(LFCA3)
            OR      A
            LD      A,C
            JP      NZ,LF7CC    ; Pass to host
            CP      'G'
            JP      Z,LF308     ; Check for '*GO'
            CP      'D'
            JP      Z,LF3B3     ; Check for '*Dump'
            CP      'S'
            JP      Z,LF330     ; Check for '*Set'
            CP      'C'
            JP      Z,LF48D     ; Check for '*CPM'
            JP      LF7CC       ; Pass to host

LF6EC:      INC     DE
            LD      A,(DE)
            CP      '.'
            JR      Z,LF720
            AND     $DF
            CP      'E'
            JP      NZ,LF7CC
            INC     DE
            LD      A,(DE)
            CP      '.'
            JR      Z,LF720
            AND     $DF
            CP      'L'
            JP      NZ,LF7CC
            INC     DE
            LD      A,(DE)
            CP      '.'
            JR      Z,LF720
            AND     $DF
            CP      'P'
            JP      NZ,LF7CC
            INC     DE
            LD      A,(DE)
            CALL    LF86D
            JP      NC,LF7CC
            CALL    SkipSpaces
            JR      LF723

LF720:      CALL    SkipSpace1
LF723:      CALL    PrText
            DB      13
            DB      "Z80 TUBE 1.20"
            DB      13
            DB      0

            LD      C,A
            LD      A,(LFCA3)
            OR      A
            LD      A,C
            JP      NZ,LF7CC
            CP      $0D
            JR      Z,LF76C
LF743:      AND     $DF
            CP      'M'
            JR      Z,LF752
            CP      $0D
            JP      Z,LF7CC
LF74E:      INC     DE
            LD      A,(DE)
            JR      LF743
LF752:      INC     DE
            LD      A,(DE)
            AND     $DF
            CP      'O'
            JR      NZ,LF74E
LF75A:      INC     DE
            LD      A,(DE)
            AND     $DF
            CP      'N'
            JR      NZ,LF74E
LF762:      INC     DE
            LD      A,(DE)
            CALL    LF86D
            JP      NC,LF74E
            JR  LF778

LF76C:      CALL    PrText      ; Print inline text
            DB      "  MON"
            DB      13
            DB      0
            JR      LF7CC

LF778:      CALL    PrText      ; Print inline text
            DB      "  CPM"
            DB      13
            DB      "  Dump <start address> <end address>"
            DB      13
            DB      "  GO <address>"
            DB      13
            DB      "  Set <start address>"
            DB      13, 0

; OSCLI - Send command line to host
; =================================
; On entry, HL=>command string
;
; Tube data  $02 string $0D  --  $7F or $80

LF7CC:      LD      A,$02       ; Send command $02 - OSCLI
            CALL    SendR2
            CALL    SendR2Str ; Send command string
            CALL    WaitR2      ; Wait for response
            CP      $80
            JR      Z,LF7DF     ; Jump if code to be entered
            POP     DE          ; Restore registers and return
            POP     BC
            POP     AF
            RET
LF7DF:      LD      A,$01       ; Set 'not RESET'
            LD      (REBOOT),A
            CALL    EnterCode   ; Enter code
            POP     DE          ; Restore registers and return
            POP     BC
            POP     AF
            RET

; Attempt to enter current program, or boot CPM
; ---------------------------------------------
; REBOOT=$00 if called from RESET, <>$00 if called from OSCLI

EnterCode:  LD      HL,(PROG)   ; Fetch program address
            LD      DE,$0007
            ADD     HL,DE       ; Point to copyright message
            PUSH    HL
            LD      A,(HL)
            LD      HL,(PROG)
            LD      E,A
            ADD     HL,DE
            LD      (FAULT),HL  ; Point FAULT to (C) string
            LD      A,(HL)
            CP      $00
            JR      NZ,LF826    ; No ROM header, enter as raw code
            INC     HL
            LD      A,(HL)
            CP      '('
            JR      NZ,LF826    ; No ROM header, enter as raw code
            INC     HL
            LD      A,(HL)
            CP      'C'
            JR      NZ,LF826    ; No ROM header, enter as raw code
            INC     HL
            LD      A,(HL)
            CP      ')'
            JR      NZ,LF826    ; No ROM header, enter as raw code
            POP     HL
            DEC     HL          ; Point to ROM type
            LD      A,(REBOOT)  ; Get RESET/OSCLI flag
            OR      A
            JR      NZ,LF82A    ; Jump to non-booting test if called from OSCLI
            LD      A,(HL)      ; Get ROM type
            BIT     6,A
            JP      Z,LF4A9     ; If not a language try boot CPM
            BIT     3,A
            JP      Z,LF4A9     ; If not Z80 code try boot CPM
LF826:      LD      HL,(PROG)   ; Fetch program address
            JP      (HL)        ; And enter it

LF82A:      XOR     A
            LD      (REBOOT),A  ; Set flag to reboot to CPM
            LD      A,(HL)      ; Get ROM type
            BIT     6,A
            JR      Z,LF83D     ; If not a language, give error
            BIT     3,A
            JR      Z,LF856     ; If not Z80 code, give error
            LD      A,$01       ; Set 'starting language'
            LD      HL,(PROG)   ; Fetch program address
            JP      (HL)        ; And enter it with A=$01

LF83D:      RST     $38
            DB      0
            DB      "This is not a language"
            DB      0

LF856:      RST     $38
            DB      0
            DB      "This is not Z80 code"
            DB      0

LF86D:      AND     $DF
            CP      'A'
            RET     C
            CP      'Z'+1
            CCF
            RET

; Skip spaces
; -----------

SkipSpace1: INC     DE
SkipSpaces: LD      A,(DE)
            CP      ' '
            JR      Z,SkipSpace1
            RET

; Skip stars
; ----------

SkipStar1:  INC     DE
SkipStars:  LD      A,(DE)
            CP      '*'
            JR      Z,SkipStar1
            RET

; Send string to Tube R2
; ----------------------

SendR2Str:  LD      A,(HL)
            CALL    SendR2
            INC     HL
            CP      $0D
            JR      NZ,SendR2Str
            RET

; OSBYTE - Byte MOS functions
; ===========================
; On entry, A, HL=OSBYTE parameters
; On exit,  A  preserved
;           If A<$80, L=returned value
;           If A>$7F, HL, Carry=returned values
;
osBYTE:     CP      $80
            JR      NC,ByteHigh ; Jump for long OSBYTEs
;
; Tube data  $04 X A    --  X
;
            PUSH    AF
            LD      A,$04       ; Send command $04 - OSBYTELO
            CALL    SendR2
            LD      A,L
            CALL    SendR2      ; Send single parameter
            POP     AF
            PUSH    AF
            CALL    SendR2      ; Send function
            CALL    WaitR2
            LD      L,A         ; Get return value
            POP     AF
            RET
;
ByteHigh:   CP      $82         ; Read memory high word
            JR      Z,Byte82
            CP      $83         ; Read bottom of memory
            JR      Z,Byte83
            CP      $84         ; Read top of memory
            JR      Z,Byte84
;
; Tube data  $06 X Y A  --  Cy Y X
;
            PUSH    AF
            LD      A,$06       ; Send command $06 - OSBYTEHI
            CALL    SendR2
            LD      A,L         ; Send parameter 1
            CALL    SendR2
            LD      A,H         ; Send parameter 2
            CALL    SendR2
            POP     AF
            PUSH    AF
            CALL    SendR2      ; Send function
            CP      $9D
            JR      Z,LF8DA     ; Fast return with OSBYTE $9D
            CALL    WaitR2      ; Wait for returned Carry
            LD      L,A
            POP     AF
            SLA     L           ; Copy into Carry flag
            PUSH    AF
            CALL    WaitR2      ; Wait for return high byte
            LD      H,A
            CALL    WaitR2      ; Wait for return low byte
            LD      L,A
LF8DA:      POP     AF
            RET
;
Byte82:     LD      HL,$0000    ; Read memory high word
            RET
Byte83:     LD      HL,$3B03    ; Read bottom of memory
;           LD      HL,$0100    ; Bottom of TPA is bottom of memory
            RET
Byte84:     LD      HL,$DC00    ; Read top of memory
;           LD      HL,($0006)  ; Read top of memory from JP BDOS
            RET

; OSWORD_DE - Call OSWORD
; =======================
; On entry, A =function
;           HL=>control block
;           DE=>returned control block
;
osWORD_DE:  PUSH    AF
            LD      A,$01       ; Signal to return OSWORD data at DE
            LD      (WORDDE),A
            POP     AF          ; Continue into OSWORD

; OSWORD - Various functions
; ==========================
; On entry, A =function
;           HL=>control block
;
osWORD:     OR      A
            JR      Z,RDLINE    ; OSWORD 0, jump to read line
            PUSH    BC
            PUSH    HL
            PUSH    IX
            PUSH    AF
            LD      A,$08       ; Send command $08 - OSWORD
            CALL    SendR2
            POP     AF
            PUSH    AF
            CALL    SendR2      ; Send function
            LD      B,$00
            LD      C,A
            CP      $80
            JR      C,WorldLow  ; Low OSWORDs
            LD      B,(HL)      ; Fetch bytes to send/recv
            INC     HL          ; from control block.
            LD      C,(HL)
            DEC     HL
            JR      LF923

WorldLow:   CP      $15
            JR      C,WordLowst ; Lowest OSWORDs.
            LD      BC,$1010
            JR      LF923

LF917:      LD      IX,LFC75    ;  Look up the number of bytes to
            ADD     IX,BC       ; send/recv in a table.
            LD      B,(IX+-1)
            LD      C,(IX+19)
LF923:      PUSH    HL
            PUSH    BC
            LD      C,B         ; Get the number of bytes to send.
            LD      B,$00
            ADD     HL,BC       ; Bytes are send in reverse order so
            POP     BC          ; point to the end of the control block.
            LD      A,B
            CALL    SendR2      ; Send the count first.
            OR      A
            JR      Z,LF938     ; No bytes to send.
LF931:      DEC     HL
            LD      A,(HL)      ; Send byte.
            CALL    SendR2
            DJNZ    LF931       ; Loop until all sent.
LF938:      LD      A,C
            CALL    SendR2      ; Send count of bytes to receive.
            POP     HL
            PUSH    AF
            LD      A,(WORDDE)
            OR      A
            JR      Z,LF94A
            LD      H,D
            LD      L,E
            XOR     A
            LD      (WORDDE),A
LF94A:      POP     AF
            OR      A
            JR      Z,LF957
            ADD     HL,BC
            LD      B,C
LF950:      DEC     HL
            CALL    WaitR2
            LD      (HL),A
            DJNZ    LF950
LF957:      POP     AF
            POP     IX
            POP     HL
            POP     BC
            RET

; RDLINE - Read a line of text
; ============================
; On entry, A =0
;           HL=>control block
; On exit,  A =undefined
;           H =length of returned string
;           Cy=0 ok, Cy=1 Escape
;
; Tube data  $0A block  --  $FF or $7F string $0D
;
RDLINE:     PUSH    BC
            PUSH    AF
            LD      A,$0A       ; Send command $0A - RDLINE
            CALL    SendR2
            INC     HL          ; Point to end of control block
            INC     HL
            INC     HL
            INC     HL
            LD      B,$03       ; Three bytes to send from block
LF96A:      LD      A,(HL)
            CALL    SendR2      ; Send byte from control block
            DEC     HL
            DJNZ    LF96A
            LD      A,$07       ; Send $0700
            CALL    SendR2
            SUB     A
            CALL    SendR2
            CALL    WaitR2      ; Wait for response
            RLCA
            JR      C,RdLineEsc
            LD      A,(HL)      ; Get address of text buffer
            DEC     HL
            LD      L,(HL)
            LD      H,A
            LD      B,$FF       ; Initialise counter
RdLineLp:   CALL    WaitR2      ; Wait for a byte
            LD      (HL),A
            INC     HL          ; inc pointer.
            INC     B           ; inc counter.
            CP      $0D         ; Loop until <cr>
            JR      NZ,RdLineLp
            LD      L,$00
            LD      H,B         ; Copy counter to H
            POP     AF          ; Restore registers
            POP     BC
            SCF
            CCF
            RET                 ; Return A=0, Cy=0, H=length, L=0
;
RdLineEsc:  LD      HL,$00FF    ; Return count=0
            POP     AF          ; Restore registers
            POP     BC
            SCF                 ; Return A=0, Cy=1, H=0, L=$FF
            RET

; OSARGS - Read info on open file
; ===============================
; On entry, A =function
;           HL=>control block
;           E =handle
; On exit,  A =returned value
;           HL preserved
;           E  preserved
;
; Tube data  $0C handle block function  --  result block
;
osARGS:     PUSH    HL          ; Save registers
            PUSH    DE
            PUSH    BC
            PUSH    AF
            LD      A,$0C       ; Send command $0C - OSARGS
            CALL    SendR2
            LD      A,E         ; Send handle
            CALL    SendR2
            INC     HL          ; Point to end of data word
            INC     HL
            INC     HL
            LD      B,$04       ; Four bytes to send
LF9B1:
            LD      A,(HL)      ; Send a byte of data word
            CALL    SendR2
            DEC     HL
            DJNZ    LF9B1
            INC     HL          ; Point HL back to data word
            POP     AF
            CALL    SendR2      ; Send function
            CALL    WaitR2      ; Wait for result
            PUSH    AF
            INC     HL          ; Point to end of data word
            INC     HL
            INC     HL
            LD      B,$04       ; Four bytes to receive
LF9C6:      CALL    WaitR2      ; Get a byte of data word
            LD      (HL),A
            DEC     HL
            DJNZ    LF9C6
            POP     AF          ; Restore registers
            POP     BC
            POP     DE
            POP     HL
            RET

; OSFIND - Open of Close a file
; =============================
; On entry, A =function
;           H =handle or HL=>filename
; On exit,  A =zero or handle
;
; Tube data  $12 function string $0D  --  handle
;            $12 $00 handle  --  $7F
;
osFIND:     PUSH    AF          ; Save function
            LD      A,$12       ; Send command $12 - OSFIND
            CALL    SendR2
            POP     AF
            CALL    SendR2      ; Send function
            CP      $00         ; If <>0, jump to do OPEN
            JR      NZ,OPEN
CLOSE:      PUSH    AF
            LD      A,H
            CALL    SendR2      ; Send handle
            CALL    WaitR2      ; Wait for response
            POP     AF          ; restore registers
            RET
OPEN:       CALL    SendR2Str   ; Send pathname
            JP      WaitR2      ; Wait for and return handle

; OSBGet - Get a byte from open file
; ==================================
; On entry, H =handle
; On exit,  A =byte Read
;           H =preserved
;           Cy set if EOF
;
; Tube data  $0E handle --  Carry byte

osBGET:     LD      A,$0E       ; Send command $0E - OSBGET
            CALL    SendR2
            LD      A,H         ; Send handle
            CALL    SendR2
            JP      WaitCarCh   ; Jump to wait for Carry and byte

; OSBPut - Put a byte to an open file
; ===================================
; On entry, A =byte to write
;           H =handle
; On exit,  A =preserved
;           H =preserved
;
; Tube data  $10 handle byte  --  $7F
;
osBPUT:     PUSH    AF          ; Save byte
            LD      A,$10       ; Send command $10 - OSBPUT
            CALL    SendR2
            LD      A,H         ; Send handle
            CALL    SendR2
            POP     AF          ; Send byte
            CALL    SendR2
            PUSH    AF
            CALL    WaitR2      ; Wait for acknowledge
            POP     AF          ; restore A
            RET

; OSFILE - Operate on whole files
; ===============================
; On entry, A =function
;           HL=>control block
; On exit,  A =result
;           control block updated
;
; Tube data  $14 block string <cr> function  --  result block

osFILE:     PUSH    BC          ; save BC
            PUSH    AF          ; and function
            LD      (CTRL),HL   ; Save address of control block
            LD      A,$14       ; Send command $14 - OSFILE
            CALL    SendR2
            LD      BC,$0011    ; Point to end of control block
            ADD     HL,BC
            LD      B,$10       ; Sixteen bytes to send
LFA20:      LD      A,(HL)      ; Send control block
            CALL    SendR2
            DEC     HL
            DJNZ    LFA20
            LD      A,(HL)      ; Get address of pathname
            DEC     HL
            LD      L,(HL)
            LD      H,A
            CALL    SendR2Str   ; Send pathname
            POP     AF
            CALL    SendR2      ; Send function
            CALL    WaitR2      ; Wait for result
            AND     $7F         ; (Lose b7 - why?)
            PUSH    AF
            LD      HL,(CTRL)   ; Get control block address back
            LD      BC,$0011
            ADD     HL,BC       ; Point to end of control block
            LD      B,$10       ; Sixteen bytes to receive
LFA41:      CALL    WaitR2      ; receive Wait for result
            LD      (HL),A
            DEC     HL
            DJNZ    LFA41
            LD      HL,(CTRL)   ; Get control block address to restore HL
            POP     AF          ; get result.
            POP     BC
            RET

; OSGBPB - Multiple byte Read and write
; =====================================
; On entry, A =function
;           HL=>control block
; On exit,  A =returned value
;              control block updated
;
; Tube data  $16 block function  --   block Carry result

osGBPB:     PUSH    BC          ; Save BC
            PUSH    AF          ; and function
            LD      (CTRL),HL   ; Save address of control block
            LD      A,$16       ; Send command $16 - OSGBPB
            CALL    SendR2
            LD      BC,$000C
            ADD     HL,BC       ; Point to end of control block
            LD      B,$0D       ; Thirteen bytes to send
LFA5E:      LD      A,(HL)
            CALL    SendR2      ; Send control block
            DEC     HL
            DJNZ    LFA5E
            POP     AF
            CALL    SendR2      ; Send function
            LD      HL,(CTRL)   ; Get control block address
            LD      BC,$000C    ; Point to end of control block
            ADD     HL,BC
            LD      B,$0D       ; Thirteen bytes to receive
LFA72:      CALL    WaitR2      ; receive control block.
            LD      (HL),A      ; Get control block
            DEC     HL
            DJNZ    LFA72
            LD      HL,(CTRL)   ; Get control block address to restore HL
            POP     BC
            JP      WaitCarCh   ; Restore BC, jump to wait for Cy and A

; Interrupt SP store
; ==================
LFA80:      DW      $0000
;
; Interupt handler
; ================
IntHand:    LD      (LFA80),SP  ; Save SP
            LD      SP,TmpStack ; Point to temporary stack
            CALL    IntSub      ; Process interrupt
            LD      SP,(LFA80)  ; Restore SP
            EI                  ; Enable INTs
            RETI                ; and return from INT handler
;
; Interrupt handler subcode
; -------------------------
IntSub:     PUSH    AF
            IN      A,($06)     ; Check R4 status
            BIT     7,A
            JR      NZ,R4ErrXfr ; Jump to process errors and transfers
            IN      A,($00)     ; Check R1 status
            BIT     7,A
            JR      NZ,R1EscEvt ; Jump to process Escape and events
            POP     AF
            JP      USERINT     ; Continue into INT2 vector

; Restart Handler - Enter here from CALL ERRJMP or RST $38

RstHand:    POP     HL
            LD      (FAULT),HL
            LD      HL,(BRKV)
            JP      (HL)

; Error Handler.

ErrHand:    CALL    LFAC6       ; Print error message
            JP      LF2CB       ; Jump to CliCom
;
DfltErrHnd: LD      A,$03
            LD      L,$00
            CALL    osBYTE
            LD      A,$02
            LD      L,$02
            CALL    osBYTE
            CALL    LFAC6
            JP      $0000
;
LFAC6:      LD      HL,(FAULT)
            LD      A,$0D
            CALL    osWRCH
            LD      A,$0A
LFAD0:      CALL    osWRCH
            INC     HL
            LD      A,(HL)
            OR      A
            JR      NZ,LFAD0
            RET

; Interupt generated by data in R1
; --------------------------------

R1EscEvt:   IN      A,($01)     ; Get byte from TubeR1
            BIT     7,A         ; Bit 7 set, jump to set Escape state
            JR      NZ,SetEsc
            LD      A,$01       ; Set flag to indicate within
            LD      (INEVENT),A ;  event handler
            PUSH    HL          ; Save registers
            PUSH    IX
            CALL    LF67C       ; Wait for byte via R1
            LD      H,A         ; 6502 Y passed to H
            CALL    LF67C       ; Wait for byte via R1
            LD      L,A         ; 6502 X passed to L
            CALL    LF67C       ; Wait for byte via R1 in A
            CALL    LFAFE       ; Call event vector
            XOR     A           ; Clear flag, indicate not within
            LD      (INEVENT),A ;  event handler
            POP     IX          ; Restore registers
            POP     HL
            POP     AF
Null:       RET                 ; Return
;
LFAFE:      LD      IX,(EVENTV) ; Get EVENTV contents
            JP      (IX)        ; Jump to it
;
; Set Escape state
; ----------------

SetEsc:     SLA     A           ; Move escape flag to b7
            LD      (ESCFLG),A  ; Store in Escape flag
            POP     AF          ; Restore A
            RET                 ; Return

; Interupt generated by data in R4
; --------------------------------

R4ErrXfr:   IN      A,($07)     ; Get byte from R4
            BIT     7,A
            JR      Z,LFB56     ; b7=0, jump for data transfer
LFB11:      IN      A,($02)     ; Get R2 status
            BIT     7,A
            JR      Z,LFB11     ; Loop until data present
            IN      A,($03)     ; Get (unused) byte from R2
            EI                  ; Enable INTs
            LD      HL,LFCB0    ; Point to error buffer
            LD      (HL),$CD    ; Put CALL opcode in
            INC     HL
            LD      A,(RST38+1) ; Fetch destination address of default
            LD      (HL),A      ; error handler
            INC     HL
            LD      A,(RST38+2)
            LD      (HL),A
            INC     HL
            CALL    WaitR2      ; Wait for byte from R2
            LD      (HL),A      ; Store as ERR in error buffer
            OR      A           ; Was it error zero?
            JR      NZ,LFB4B    ; No, get rest of error block
; *BUG* Test for error zero should happen *after* collecting error block.
; This results in the client hanging as command transfer is now out of sequence.
; Change the above branch to JR LFB4B to fix this
            CALL    PrText      ; Print inline text
            DB      13          ; NEWL
            DB      "Fatal error"
            DB      0
;
            LD      A,(LFCA3)
            OR      A
            JP      NZ,$0000
            JP      LF2CB

LFB4B:      INC     HL
            CALL    WaitR2      ; Wait for byte from R2
            LD      (HL),A      ; Store in error buffer
            OR      A
            JR      NZ,LFB4B    ; Loop until final zero byte
            JP      LFCB0       ; Jump to error block to generate

; Data transfer initiated by INT via R4
; -------------------------------------
LFB56:      PUSH    BC
            PUSH    DE
            PUSH    HL
            PUSH    AF          ; Save transfer type
            RLCA                ; Multiply by length of an NMI
            LD      E,A         ; routine.
            RLCA
            RLCA
            ADD     A,E
            RLCA
            LD      E,A
            LD      D,$00
            LD      HL,NmiBase  ; Add to the NMI base address to get
            ADD     HL,DE       ; the address of the NMI handler for
            LD      DE,LFC61    ; this transfer type and then copy
            LD      BC,$0014    ; it into place.
            LDIR
            CALL    WaitR4      ; Wait for caller ID in R4
            POP     AF          ; Get transfer type back
            CP      $05         ;         Is it 'release'?
            JR      Z,LFBAA     ; Yes, exit
            PUSH    AF          ; Save transfer type again
            CALL    WaitR4      ; Wait for byte in R4
            CALL    WaitR4      ; Wait for byte in R4
            CALL    WaitR4      ; Wait for byte in R4
            LD      H,A         ;         High byte of address
            CALL    WaitR4      ; Wait for byte in R4
            LD      L,A         ;         Low byte of address
            LD      (PROG),HL   ; Set data address
            LD      C,$05       ; Set C to R3 port address
            LD      B,$00       ; Set B for 256 bytes to transfer
            CALL    WaitR4      ; Wait for sync byte in R4
            POP     AF          ; Get transfer type back
            CP      $06
            JR      C,LFBAA     ; Jump if not 256-byte transfers
            JR      NZ,recv256  ; Jump with 256-byte read
;
; Send 256 bytes to Tube via R3
; -----------------------------

send256:    IN      A,($04)     ; Check R3 status
            OR      A
            JP      P,send256   ; Loop until port free
            OUTI                ; Send byte from (HL), increment HL
            JP      NZ,send256  ; Loop until 256 bytes done
LFBA2:      IN      A,($04)     ; Check R3 status
            OR      A
            JP      P,LFBA2     ; Loop until port free
            OUT     ($05),A     ; Send final sync byte
LFBAA:      POP     HL
            POP     DE
            POP     BC
            LD      A,(INEVENT) ; Are we in the event handler?
            OR      A
            RET     NZ
            POP     AF
            RET
;
; Read 256 bytes from Tube via R3
; -------------------------------

recv256:    LD      A,H         ; Check high byte
            CP      $F2
            JR      NC,disc256  ; would overwrite client -discard.
            IN      A,($04)     ; Get R3 status
            OR      A
            JP      P,recv256   ; Loop until data present
            INI                 ; Get a byte and increment HL
            JP      NZ,recv256  ; Loop for 256 bytes
            JR      LFBAA       ; Jump to restore and exit
disc256:    IN      A,($04)     ; Get R3 status
            OR      A
            JP      P,disc256   ; Loop until data present
            IN      A,($05)     ; Get byte and discard
            DJNZ    disc256     ; Loop for 256 bytes
            JR      LFBAA       ; Jump to restore and exit
;
; NMI data transfer routines
; ==========================
;

; Transfer 0 - Single byte write to host
; --------------------------------------
NmiBase:    PUSH    HL
            PUSH    AF
            LD      HL,(PROG)   ; Get data address
            LD      A,(HL)      ; Get byte from memory
            OUT     ($05),A     ; Send to R3
            INC     HL          ; Increment data address
            LD      (PROG),HL
            POP     AF
            POP     HL
            RETN
;
            LD   A,($3B12)
            OR   A
;
; Transfer 1 - Single byte read from host
; ---------------------------------------
            PUSH    HL
            PUSH    AF
            IN      A,($05)
            LD      HL,(PROG)
            LD      (HL),A
            INC     HL
            LD      (PROG),HL
            POP     AF
            POP     HL
            RETN
;
            LD      BC,$EB3B
            DB      $CD
;
; Transfer 2 - Double byte write to host
; --------------------------------------
            PUSH    HL
            PUSH    AF
            LD      HL,(PROG)
            LD      A,(HL)
            OUT     ($05),A
            INC     HL
            LD      A,(HL)
            OUT     ($05),A
            INC     HL
            LD      (PROG),HL
            POP     AF
            POP     HL
            RETN
;
; Transfer 3 - Double byte read from host
; ---------------------------------------
            PUSH    HL
            PUSH    AF
            IN      A,($05)
            LD      HL,(PROG)
            LD      (HL),A
            INC     HL
            IN      A,($05)
            LD      (HL),A
            INC     HL
            LD      (PROG),HL
            POP     AF
            POP     HL
            RETN
;
; Transfer 4 - Execute code
; -------------------------
            OUT     ($05),A
            RETN                ; Code jumped to later
;
            INC     HL
            LD      D,(HL)
            DEC     HL
            RET
;
            LD      HL,($3CA0)
            ADD     HL,DE
            INC     HL
            INC     HL
            LD      E,(HL)
            INC     HL
            LD      D,(HL)
            DEC     HL
            RET
            DB      $2A
;
; Transfer 5 - Release Tube
; -------------------------
            OUT     ($05),A
            RETN
;
            JP      Z,$370D
            EX      DE,HL
            PUSH    DE
            CALL    $36ED
            EX      DE,HL
            LD      ($3CA4),HL
            POP     DE
            JP      $3739
;
; Transfer 6 - 256-byte transfer
; ------------------------------
            OUT     ($05),A     ; Write to R3
            RETN
;
            JR      NZ,LFC3F
LFC3F:      EX      DE,HL
            ADD     HL,DE
            LD      ($3CA2),HL
            PUSH    DE
            EX      DE,HL
            LD      HL,($3CAC)
            CALL    $0D82
            POP     DE
;
; Transfer 7 - 256-byte transfer
; ------------------------------
            OUT     ($05),A     ; Write to R3
            RETN
;
            LD      A,E
            INC     A
            DEC     H
            LD      A,($3C7D)
            OR      A
            CALL    $0D82
            JP      C,$3887
            LD      HL,($3CA0)
;
; Current data transfer NMI code copied to here
; =============================================
LFC61:      OUT     ($05),A    ; Write to R3
            RETN
;
            PUSH    DE
            CALL    $36E4
            EX      DE,HL
            LD      HL,($3CA8)
            EX      DE,HL
            LD      A,E
            AND     $E0
            LD      E,A
            LD      A,E
            OR       D
            DB      $C2
;

; OSWORD control block lengths
; ============================
LFC75:      DB      $00         ; =TIME
            DB      $05         ; TIME=
            DB      $00         ; =Timer
            DB      $05         ; Timer=
            DB      $02         ; =IO
            DB      $05         ; IO=
            DB      $08         ; SOUND
            DB      $0E         ; ENVELOPE
            DB      $04         ; =POINT
            DB      $01         ; =CHR$
            DB      $01         ; =Palette
            DB      $05         ; Palette=
            DB      $00         ; =Coords
            DB      $10         ; =TIME$
            DB      $19         ; TIME$= (Original code has TIME$= sending $10, should be $19)
            DB      $10         ; Net_Tx
            DB      $0D         ; Net_Args
            DB      $00         ; Net_Rx
            DB      $08         ; NetFS_Info
            DB      $80         ; NetFS_Op

            DB      $05         ; =TIME
            DB      $00         ; TIME=
            DB      $05         ; =Timer
            DB      $00         ; Timer=
            DB      $05         ; =IO
            DB      $00         ; IO=
            DB      $00         ; SOUND
            DB      $00         ; ENVELOPE
            DB      $05         ; =POINT
            DB      $09         ; =CHR$
            DB      $05         ; =Palette
            DB      $00         ; Palette=
            DB      $08         ; =Coords
            DB      $19         ; =TIME$ (Original code has =TIME$ receiving $10, it should be $19)
            DB      $10         ; TIME$=
            DB      $01         ; Net_Tx
            DB      $0D         ; Net_Args
            DB      $80         ; Net_Rx
            DB      $08         ; NetFS_Info
            DB      $80         ; NetFS_Op
;

; CLI Prompt input control block
LFC9D:      DW      LFCB0       ; Input buffer
            DB      $80         ; Maximum length
            DB      $20         ; min char
            DB      $FF         ; max char

; Variables
; =========
INEVENT:    DB      $00         ; Is code within the event handler?
LFCA3:      DB      $00
WORDDE:     DB      $00
DISKSP:     DW      $F5D5       ; Saved SP within disk access code
RESULT:     DB      $2A         ; Result of disk access
PROG:       DW      PROG        ; Current program start address
ADDR:       DW      $0000       ; Temporary store
CTRL:       DW      LFCB0       ; Line pointer
OLDFX4:     DB      $00         ; Previous fx4 state
REBOOT      DB      $00         ; Reboot flag 0=CPM, <>0=Not CPM

; Input and error buffer
; Contains leftover code from somewhere
;
LFCB0:      DS      $80
ENDZ80:

; 6502 code for OSWORD $FF
; ------------------------
;
; OSWORD $FF transfers blocks of memory between I/O and CoPro memory
;    X%?0 =$0D
;    X%?1 =$01
;    X%!2 =I/O address
;    X%!6 =CoPro address
;    X%!10=Number of bytes to transfer
;    X%?12=0 for C->H, =1 for H->C
;
;  I/O address can be:
;    $FFxRxxxx for ROM R
;    $FF8xxxxx for workspace RAM
;    $FFFFxxxx for I/O memory
;    $FFFExxxx for current display memory
;    $FFFDxxxx for shadow display memory

OswFF:
            .processor  6502
            .rorg       $2500

userv       .equ    $200
ctrl        .equ    $70
saveA       .equ    $72
addr        .equ    $74
count       .equ    $76

StartFF:    JMP     L2505       ; New USER entry
OldUserV:   DW      $2500       ; Old WORDV, swapped with USERV to claim
L2505:      CMP     #$FF        ; If my OSWORD, jump to respond
            BEQ     IsFF
            JMP     (OldUserV)
IsFF:       STX     ctrl+0      ; Save OSWORD parameters
            STY     ctrl+1
            STA     saveA
            LDY     #$02
            LDA     (ctrl),Y    ; I/O address low byte
            STA     addr+0
            INY
            LDA     (ctrl),Y    ; I/O address high byte
            STA     ctrl+1
            LDA     $F4         ; Save current ROM
            PHA
            INY
            LDA     (ctrl),Y    ; I/O address ROM/screen byte
            TAX
            CLC
            ADC     #$40        ; Allow $0x and $Fx for ROMs
            STA     $F4
            STA     $FE30       ; Page in ROM
            INX
            BEQ     GetIoMem    ; $FFFFxxxx - I/O memory
            INX
            BEQ     GetDisplay  ; $FFFExxxx - current screen
            INX
            BEQ     GetShadow   ; $FFFDxxxx - shadow memory
            BNE     GetIoMem
GetDisplay: LDA     #$84
            JSR     OSBYTE
            TYA
            BPL     GetIoMem    ; not shadow screen displayed.
GetShadow:  LDA     #$01
            JSR     vRamSelect
GetIoMem:   JSR     L259C       ; claim tube.
            LDY     #$0C
            LDA     (ctrl),Y    ; get read/write command
            PHA
            LDA     ctrl+0      ; point to control+6
            CLC
            ADC     #$06
            TAX
            LDA     #$00
            ADC     ctrl+1
            TAY                 ; XY->CoPro address in control block
            PLA
            PHA
            JSR     $0406       ; Initiate specified action
            LDY     #$0A
            LDA     (ctrl),Y    ; Get count low byte
            TAX
            INY
            LDA     (ctrl),Y    ; Get count high byte
            STA     count
            BNE     L2544       ; Jump forward if >255 bytes to do
            TXA
            BEQ     L2592       ; Jump to exit if no bytes to do
L2544:      TXA
            BEQ     L2549       ; Jump forward if multiple of 256 bytes
            INC     count       ; to balance DECs later.
L2549:      LDY     #$00        ; prepare zero offset for (zp),Y
            PLA                 ; get command back.
            ROR
            BCS     L2575       ; Jump if host->client
;
; Copy from Client to Host
;
L2558:      JSR     L259B       ; Delay before starting and between bytes
            JSR     L259B
            JSR     L259B
            LDA     $FEE5       ; receive byte
            STA     (addr),Y    ; store it.
            INC     addr+0      ; update I/O address
            BNE     L256C
            INC     addr+1
L256C:      DEX                 ; Loop for up to 256 bytes
            BNE     L2558
            DEC     count       ; Loop for each 256-byte block.
            BNE     L2558
            BEQ     L2592       ; jump to exit when finished.
;
; Copy Host to Client
;
L2575:      LDA     (ctrl),Y
            STA     $FEE5       ; Transfer byte H->C
            JSR     L259B       ; delay between bytes.
            JSR     L259B
            JSR     L259B
            INC     addr+0      ; Update I/O address.
            BNE     L258B
            INC     addr+1
L258B:      DEX                 ; Loop for up to 256 bytes
            BNE     L2575
            DEC     count       ; Loop for each 256-byte block.
            BNE     L2575

L2592:      JSR     L25A4       ; release tube.
            LDA     #$00        ; page main memory back in.
            JSR     vRamSelect
L2595:      PLA                 ; Restore ROM
            STA     $F4
            STA     $FE30
            LDX     ctrl+0      ; Restore entry registers
            LDY     ctrl+1
            LDA     saveA
L259B:      RTS                 ; Call here to delay 6us

L259C:      LDA     #$C0+7      ; Claim with ID=7
            JSR     $0406
            BNE     L259C       ; loop until claimed
vRamOk:     RTS

L25A4:      LDA     #$80+7      ; Release with ID=7
            JMP     $0406

vRamSelect: PHA
            TAX                 ; A=0 main RAM, A=1 video RAM
            LDA     #$6C        ; attempt to select master Video RAM
            JSR     OSBYTE
            PLA
            INX
            BNE     vRamOk      ; X<>255, successful
            EOR     #$01
            TAX                 ; A=1 main RAM, A=0 video RAM
            LDA     #$6F        ; Attempt to select Aries/Watford RAM
            JMP     OSBYTE
EndFF:
            .processor Z80
            .rorg   ENDZ80+EndFF-StartFF

LFDDA:      DW      $2500       ; Workspace for copying OSWORD $FF code
LFDDC:      DW      0
            DB      0
END6502:

; Set/Read terminal destination

Terminal:   CP      $02
            JR      NC,LFE04
            PUSH    AF
            LD      A,(TERMFLG)
            AND     $01
            LD      (ADDR),A
            POP      AF
            LD      (TERMFLG),A
            OR      A
            JR      NZ,LFDFA
            LD      HL,osWRCH
            LD      (PROUT+1),HL
            JR      LFE00
LFDFA:      LD      HL,LFE0D
            LD      (PROUT+1),HL
LFE00:      LD      A,(ADDR)
            RET

LFE04:      CP      $FF
            RET     NZ
            LD      A,(TERMFLG)
            AND     $01
            RET

LFE0D:      LD      C,A
TermOut:    LD      A,(TERMFLG)
            BIT     7,A
            JR      NZ,LFE27
            LD      A,C
            CP      $1B
            JR      Z,LFE1E
            CALL    osWRCH
            RET

LFE1E:      LD      A,(TERMFLG)
            SET     7,A
            LD      (TERMFLG),A
            RET

LFE27:      BIT     6,A
            JP      NZ,LFEA6
            BIT     5,A
            JP      NZ,LFED9
            LD      A,C
            CP      $3D
            JR      Z,LFE4B
            CP      $3E
            JR      Z,LFE54
            CP      $3F
            JR      Z,LFE5D
            CP      $40
            JR      Z,LFE80
LFE42:      LD      A,(TERMFLG)
            RES     7,A
            LD      (TERMFLG),A
            RET

LFE4B:      LD      A,(TERMFLG)
            SET     6,A
            LD      (TERMFLG),A
            RET

LFE54:      LD      A,(TERMFLG)
            SET     5,A
            LD      (TERMFLG),A
            RET

LFE5D:      CALL    LFEEC
            JR      Z,LFE6A
            CP      $00
            JR      NZ,LFE42
            LD      A,$1F
            JR      LFE6C

LFE6A:      LD      A,$18
LFE6C:      LD      (LFF18),A
            CALL    LFEF5
            LD      B,$10
            LD      HL,LFF10
LFE77:      LD      A,(HL)
            CALL    osWRCH
            INC     HL
            DJNZ    LFE77
            JR      LFE42

LFE80:      CALL    LFEEC
            JR      Z,LFE89
            CP      $00
            JR      NZ,LFE42
LFE89:      CALL    LFEF5
            LD      B,$06
            LD      HL,LFF10
LFE91:      LD      A,(HL)
            CALL    osWRCH
            INC     HL
            DJNZ    LFE91
            LD      B,$05
            LD      HL,LFF1B
LFE9D:      LD      A,(HL)
            CALL    osWRCH
            INC     HL
            DJNZ    LFE9D
            JR      LFE42

LFEA6:      BIT     4,A
            JR      NZ,LFEB9
            LD      A,C
            SUB     $20
            LD      (LFF21),A
            LD      A,(TERMFLG)
            SET     4,A
            LD      (TERMFLG),A
            RET

LFEB9:      LD      A,C
            SUB     $20
            LD      (LFF20),A
            LD      A,$1F
            CALL    osWRCH
            LD      A,(LFF20)
            CALL    osWRCH
            LD      A,(LFF21)
            CALL    osWRCH
            LD      A,(TERMFLG)
            AND     $01
            LD      (TERMFLG),A
            RET
;
LFED9:      LD      A,C
            OR      A
            JR      Z,LFEE3
            SUB     $20
            CALL    osWRCH
            RET

LFEE3:      LD      A,(TERMFLG)
            AND     $01
            LD      (TERMFLG),A
            RET

LFEEC:      LD      A,$87
            CALL    osBYTE
            LD      A,H
            CP      $03
            RET

LFEF5:      LD      A,$86
            CALL    osBYTE
            LD      A,L
            LD      (LFF11),A
            LD      (LFF1E),A
            LD      A,H
            LD      (LFF12),A
            LD      (LFF14),A
            LD      (LFF1F),A
            INC     A
            LD      (LFF1A),A
            RET

LFF10:      INC     E
LFF11:      DB      $C4
LFF12:      DB      $48,$4F
LFF14:      POP     BC
            INC     C
            INC     E
            DB      0
LFF18:      RRA
            LD      C,A
LFF1A:      LD      C,B
LFF1B:      INC     C
            LD      A,(DE)
            RRA
LFF1E:      DB      $22
LFF1F:      DB      $28
LFF20:      DB      $00
LFF21:      DB      0

            LD      L,E
            RLA
            LD      HL,($3B28)
            LD      (HL),$00
            LD      HL,$3B2A
            JP      $1977

            PUSH    BC
            PUSH    HL
            LD      A,(HL)
            AND     $03
            LD      B,A
            LD      C,$06
            INC     HL
            LD      E,(HL)
            INC     HL
            LD      D,(HL)
            CALL    $1A48
            POP     HL
            POP     BC
            RET

            LD      D,E
            DW      0
            DW      0
            DB      0
            LD      (BC),A
            DW      0
            DW      0
            LD      A,(BC)
            DW      0
            DW      0
            DW      0
            DW      0
            DW      0
            DW      0
            DW      0
            DW      0
            DW      0
            DB      0
TmpStack:                       ; Interrupt stack descends from here

            DW      0,0,0,0,0,0,0,0
            DW      0,0,0,0,0,0,0,0
SysStack:                       ; System stack descends from here

ESCFLG:     DB      $00         ; $FF80 - Bit 7 is set on escape.
TERMFLG:    DB      $00         ; $FF81 - TERM flag
FAULT:      DW      $0100       ; $FF82 - Fault pointer
ERRDEF:     DW      DfltErrHnd  ; $FF84 - Default error handler

            DB      $02,$14,$95,$00,$01,$00,$00,$3C
            DB      $1A,$FF,$FF,$01,$00,$00,$00,$00
            DB      $00,$00,$00,$00,$74,$4C,$D6,$5E

PROUT:      JP      osWRCH      ; $FF9E
INITFF:     JP      InitFF      ; $FFA1
DISKACC:    JP      DiskAccess  ; $FFA4
LDCCP:      JP      LoadCCP     ; $FFA7
PRHEX:      JP      PrHex       ; $FFAA
PR2HEX:     JP      Pr2Hex      ; $FFAD
USERINT:    JP      Null        ; $FFB0
PRTEXT:     JP      PrText      ; $FFB3
PRNTC:      JP      TermOut     ; $FFB6
CLICOM:     JP      CLIPrompt   ; $FFB9
RST38:      JP      RstHand     ; $FFBC
INITERR:    JP      InitErr     ; $FFBF
SEEK0:      JP      Seek0       ; $FFC2
KBDTST:     JP      KeyTest     ; $FFC5
TERM:       JP      Terminal    ; $FFC8
OSWORDDE:   JP      osWORD_DE   ; $FFCB
;
OSFIND:     JP      osFIND      ; $FFCE
OSGBPB:     JP      osGBPB      ; $FFD1
OSBPUT:     JP      osBPUT      ; $FFD4
OSBGET:     JP      osBGET      ; $FFD7
OSARGS:     JP      osARGS      ; $FFDA
OSFILE:     JP      osFILE      ; $FFDD
;
OSRDCH:     JP      osRDCH      ; $FFE0
OSASCI:     CP      $0D         ; $FFE3
            JR      NZ,OSWRCH
OSNEWL:     LD      A,$0A       ; $FFE7
            CALL    OSWRCH
OSWRCR:     LD      A,$0D       ; $FFEC
OSWRCH:     JP      osWRCH      ; $FFEE

OSWORD:     JP      osWORD      ; $FFF1
OSBYTE:     JP      osBYTE      ; $FFF4
OS_CLI:     JP      osCLI       ; $FFF7

BRKV:       DW      ErrHand     ; $FFFA: Error vector
EVENTV:     DW      EventHand   ; $FFFC: Event vector
IRQV:       DW      IntHand     ; $FFFE: Interrupt vector

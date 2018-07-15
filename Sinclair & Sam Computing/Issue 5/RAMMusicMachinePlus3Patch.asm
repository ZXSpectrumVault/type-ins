; RAM Music Machine DOS Patch
; Public Domain software by G.Rowland

ORG 26062-13

LOADDRV:    EQU #5B79
SAVEDRV:    EQU #5B7A
BANK_M:     EQU #5B5C
EXIT:       EQU #9998 ; Exit thru #9998 resets stack and returns to command loop

; copy INIT routine and patch into position
REMCPY:     ADD HL,BC   ; get address of INIT
            LD DE,#65EB ; destination
            LD BC,938   ; length of INIT+PATCH
            LDIR
            RET         ; Return to BASIC

; INIT copy of RRM Code
INIT:       LD HL,SSLD
            LD (#6AD2),HL   ; Replaces old table entry
            LD A,#C3        ; Call to save routine
            LD (#9A1A),A    ; redirected to new
            LD HL,DOSSV     ; DOS routine
            LD (#9A1B),HL
            ; Modify a calling routine so that it doesn't enter beyond recognition
            LD A,#1A
            LD (#9A0C),A
            ; Change start of SAVE TUNE routine so that it doesn't get filename twice
            LD HL,#99E8
            LD (#6ADC),HL
            ; Replaces SAVE EVERYTHING with CATALOGUE DRIVE
            LD HL,CATLG
            LD (#6AE1),HL
            LD HL,MENU4
            LD DE,#70C9
            LD BC,19
            LDIR
            RET

MENU4:      DEFN "Catalogue drive"
            DEFB 0
; +3 DOS Patch Starts Here
; Enter with drive to check in (DE)
CHKDRV:     LD A,(#A615)
            CP ":" ; Check for change of drive
            RET NZ
            LD A,(HL) ; Get new drive
            CP "A"
            JR Z,ACPTDR
            CP "B"
            JR Z,ACPTDR
            CP "M"
            JR Z,ACPTDR
            CP "T"
            RET NZ ; Ignore invalid drive
            LD (DE),A ; Set tape drive
            LD A,C
            CP 2
            RET NZ ; Return if "t:name"
            JP EXIT ; Exit if just "t:"

ACPTDR:     PUSH HL
            PUSH BC
            CALL BASDOS
            CALL #012D
            CALL BASDOS
            POP BC
            POP HL
            INC HL
            INC HL ; Skip filename
            DEC C
            DEC C
            JP Z,EXIT ; Exit to command if only drive change
            ; copy name over drive name
            INC C ; Include end marker
            LD DE,#A614
            LD B,0
            LDIR
            RET

; Get filename up to 12 char long
; Exit with number chat input in C
FNAME:      LD HL,#A6AD ; Init TAPE HEADER
            LD DE,#A6AE
            LD BC,9 ; Max length of tape name
            LD (HL),#20
            LDIR
            LD IX,#7080 ; Point to menu in use
            CALL #834D  ; Clear menu panel
            LD HL,#9A60 ; Print "Enter filename"
            CALL #81E7
            LD B,12 ; Max length of DOS name
            CALL #83E4 ; Input filename
            LD A,B
            OR C
            RET Z; return if no name
            ; Copy name to tape header
            LD DE,#A6AD
            LD HL,#A614
            LD A,B
            CP 11
            JR C,CPYNME
            LD B,10 ; Max 10 char for tape header

CPYNME:     LD A,(HL)
            LD (DE),A
            INC HL
            INC DE
            DJNZ CPYNME
            LD B,C ; Restore name length
            ; Now force caps on DOS copy of name
            LD HL,#A614

FCAP:       LD A,(HL)
            CALL #69E8
            LD (HL),A
            INC HL
            DJNZ FCAP
            LD (HL),#FF ; Mark end of DOS copy
            RET

; Flip Dos ROM & Pg 7/BASIC ROM & Pg 0
BASDOS:     DI
            PUSH AF
            PUSH BC
            LD BC,#7FFD
            LD A,(BANK_M)
            XOR #17
            LD (BANK_M),A
            OUT (C),A
            POP BC
            POP AF
            EI
            RET

; DOS Save routine
DOSSV:      CALL FNAME
            LD A,B
            OR C
            JP Z,EXIT
            LD DE,SAVDRV
            CALL CHKDRV
            LD B,C
            LD A,(SAVDRV)
            CP "T"
            JP Z,#9ACD    ; to tape save routine
            CALL BASDOS
            LD A,(SAVDRV)
            CALL #012D
            JP NC,DOSERR
            LD HL,#A614
            LD BC,#0102   ; File number and access mode
            LD DE,#0103   ; Create and open action
            CALL #0106    ; Open file named in (HL)
            JP NC,DOSERR
            LD B,1        ; File number
            CALL #010F    ; Create DOS ref head
            LD A,(#ACAC)  ; Get file type
            PUSH IX       ; Get ref head
            POP DE        ; into DE
            LD (DE),A
            INC DE
            LD HL,#A6B7   ; Copy tape head to DOS head
            LD BC,6
            LDIR
            LD BC,#0100   ; File & page number
            LD DE,(#A6B7) ; Get length
            LD HL,(#A6B9) ; Address of bytes to save
            CALL #0115    ; Write bytes
            JP NC,DOSERR
            LD B,1
            CALL #0109    ; Close file
            JP NC,DOSERR
            CALL BASDOS
            RET

; DOS Load Routine
DOSLD:      CALL BASDOS
            LD A,(LODDRV)
            CALL #012D    ; Set drive
            JR NC,DOSERR
            LD HL,#A614
            LD BC,#0101   ; File number and access mode
            LD DE,#0001   ; Create & open action
            CALL #0106    ; Open file named in (HL)
            JR NC,DOSERR
            LD B,1        ; File number
            CALL #010F    ; Create DOS ref head
            PUSH IX
            POP HL
            LD A,(HL)     ; Get file type
            LD (#A6AC),A
            INC HL
            LD DE,(#A6B7)
            LD BC,6       ; make copy of header
            LDIR
            CALL BASDOS
            SCF           ; File header read
            RET

; Continue with load if room for file
LDCONT:     CALL BASDOS
            LD DE,(#A6B7) ; Get length
            LD HL,(#A6B9) ; Get address to load to
            LD B,1        ; File number
            LD C,0        ; Page number
            CALL #0112    ; Read file
            JR NC,DOSERR
            LD B,1
            CALL #0109    ; Close file
            JR NC,DOSERR
            CALL BASDOS
            SCF           ; Flag successful load
            RET

ERRMSG:     DEFM "3 DOS error number "
ERRNUM:     DEFB #30,#30,0
; Errors must exit through here
; A=0    Close file and return
; A=1-26 Close file, report DOS error and return
; A>252  Close file and exit to RMM report

RMMERR:     CALL BASDOS ; make sure DOS paged in
DOSERR:     PUSH AF
            LD B,1
            CALL #0109 ; Try closing file
            LD B,1
            CALL NC,#010C ; Abort if you can't close
            CALL BASDOS
            POP AF
            OR A
            RET Z
            LD HL,#9D05 ; Wrong file type
            CP #FF
            JP Z,#9CAC
            LD HL,#9CF0 ; Not enough memory
            CP #FE
            JP Z,#9CAC
            LD HL,#9D18 ; No free samples!
            CP #FD
            JP Z,#9CAC
            CALL H2DEC
            LD (ERRNUM),DE
            LD IX,#7080
            CALL #834D ; Clear panel/window
            LD HL,ERMSG
            CALL #81E7 ;  Print DOS error message
            LD HL,#9D2B ; Hit any key!
            CALL #81E7
            LD B,2
            CALL #8499 ; Wait for key press
            OR A
            RET

; LOAD SAMPLE IF FILE TYPE=4
; LOAD SONG IF FILE TYPE=5
SSLD:       CALL FNAME
            LD DE,LODDRV
            LD A,B
            OR C
            JR NZ,SSLD2
            LD A,(DE)
            CP "T"
            ; Load from tape if "" null name
            JP Z,#9B8B
            ; No action when "" used on drive A/B or M
            JP EXIT
SSLD2:      CALL CHKDRV
            LD B,C
            LD A,(LODDRV)
            CP "T"
            JP Z,#9B8B ; Load from tape
            CALL DOSLD ; Get DOS header
            JP NC,EXIT ; Branch if load error
            LD A,(#A6AC)
            CP 4       ; Sample file type
            JP NZ,LDSNG
            ; Load Sample
            CALL CHKTOP ; Look for free sample area to load into
            LD B,9      ; Number of samples to test
            LD C,0      ; sample number
FNDFRE:     PUSH BC
            LD A,C
            LD HL,#A6BD
            CALL #872A ; get sample C
            CALL #99D5 ; get length of sample
            LD DE,6
            OR A
            SBC HL,DE
            POP BC
            JR Z,FRESMP ; branch if free sample found
            INC C       ; point to next sample
            DJNZ FNDFRE
            LD A,#FD    ; No free samples
            JP RAMERR
FRESMP:     LD A,C
            LD HL,#A6BD
            CALL #872A  ; get address of the free sample
            LD (#A6B9),HL
            PUSH HL
            LD HL,(#A6B7) ; get length of file to load
            LD DE,6
            OR A
            SBC HL,DE
            LD B,H
            LD C,L
            POP HL
            INC HL
            CALL MKROOM
            CALL LDCONT
            JP C,EXIT
            ; Make sure bad load leaves null sample
            LD HL,(#A6B7)
            LD BC,6
            OR A
            SBC HL,DE
            LD B,H
            LD C,L
            LD HL,(#A6B9)
            INC HL
            CALL #8DFC
            LD DE,(#A6B9)
            LD HL,#9D83
            LD BC,6
            LDIR
            JP EXIT

; Exit if room for file
CHKTOP:     CALL #8E4A
            LD DE,(#A6CF)
            INC DE
            LD HL,0
            OR A
            SBC HL,DE
            LD DE,(#A6B7) ; length of block
            OR A
            SBC HL,DE
            LD A,#FE     ; Not enough memory
            JP C,RMMERR
            RET

; Make room for file
MKROOM:     LD A,B
            OR A
            RET Z
            PUSH HL
            LD HL,(#A6CF) ; get top
            OR A
            ADC HL,BC ; Amonut of room rec.
            POP HL
            LD A,#FE
            JP C,RMMERR
            JP #8DE0 ; Make room

; LOAD SONG
LDSNG:      CP 5
            LD A,#FF ; Wrong file type
            JP NZ,RMERR
            CALL CHKTOP
            LD DE,#AACF
            LD HL,(#A6D1)
            OR A
            SBC HL,DE
            LD B,H
            LD C,L
            LD H,D
            LD L,E
            CALL #8DFC
            LD HL,(#A6B7) ; length of song to load
            LD BC,#01FE
            OR A
            SBC HL,BC
            LD B,H
            LD C,L ; length of song-510 bytes
            LD HL,#AACF
            CALL MKROOM
            LD HL,0
            LD BC,(#A6D3)
            OR A
            SBC HL,BC
            LD B,H
            LD C,L
            LD HL,1
            CALL #8E68
            CALL LDCONT ; Load song
            ; Tidy Up?
            LD BC,#A6D3
            LD HL,1
            CALL #8E68
            LD HL,#ACCF
            LD (#A6CD),HL
            JP EXIT
            ; catalogue current default device
            CALL #81C6 ; Clear screen
            LD BC,9
            CALL #82BA ; Set print colour
            CALL BASDOS
            LD HL,#C000 ; Clear directory area
            LD DE,#C001 ; on page 7
            LD BC,1024
            LD (HL),0
            LDIR
            LD B,64
            LD C,1
            LD DE,#C000
            LD HL,CATNME
            CALL #011E ; DOS catalogue
            JR NC,CATX
            LD A,B ; get directory length
            CP 2
            JR C,CATX ; branch if disc empty
            DEC A
            LD (CATN),A
            LD HL,#C00D
            PUSH HL
NXNME:      LD C,8
            CALL PNAME
            DEC HL
            LD A,"." ; Field separator
            LD C,4
            CALL PTYPE
            LD A,(H) ; Get MSB of file length
            CALL H2DEC
            LD (HL),E ; Place ASCII where it
            INC HL    ; can be picked up by
            LD (HL),D ; PTYPE call
            DEC HL
            DEC HL
            LD A," " ; one space between quote marks
            LD C,3
            CALL PTYPE
            PUSH HL
            CALL #82CD
            LD C,7
            INC B ; Move cursor down
            LD A,B
            CP 21
            JR C,CNME
            CALL #8499 ; Wait for key
            CALL #81C6 ; then start new screen
            LD BC,12
CNME:       CALL #82BA
            POP HL
            LD A,(CATN)
            DEC A
            LD (CATN),A
            JR NZ,NXNME
            CALL #8499
CATX:       CALL BASDOS
            ; Restore LOAD/SAVE screen
            LD IX,#7080
            LD HL,#6AC4
            PUSH HL
            JP #6A46
; Print filename
PNAME:      LD A,(HL) ; fetch character
; Enter here to print type file & K size
PTYPE:      PUSH HL
            CALL BASDOS ; Make sure BASIC paged in
            CALL #82E5
            CALL BASDOS ; Get cat page 7 back
            POP HL
            INC HL
            DEC C
            JR NZ,PNAME
            RET
; Convert hex t o two ASCII decimal numbers (0-99)
H2DEC:      LD DE,#0030
CNVN:       CP 10
            JR C,SETASC
            INC E
            SUB 10
            JR CNVN
SETASC:     ADD A,#30
            LD D,A
            RET

CATN:       DEFB 0
CATNME:     DEFM "*.*"
            DEFB #FF

; RAM Music Machine goes here
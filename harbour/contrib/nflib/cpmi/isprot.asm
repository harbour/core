;
; $Id$
;

; File......: ISPROT.ASM
; Author....: Ted Means
; CIS ID....: 73067,3332
;
; This is an original work by Ted Means and is placed in the
; public domain.
;
; Modification history:
; ---------------------
;     Rev 1.1   01 Feb 1995 03:02:00   TED
;  Fixed bug in which function would report incorrect mode when run under
;  OS/2 Warp.
;
;     Rev 1.0   01 Jan 1995 03:01:00   TED
;  Initial release
;

;  $DOC$
;  $FUNCNAME$
;     cpmiIsProtected()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Determine if the CPU is in protected mode
;  $SYNTAX$
;     int pascal cpmiIsProtected( void )
;  $ARGUMENTS$
;     None
;  $RETURNS$
;     Zero if the CPU is in real mode, or a non-zero value if it is running
;     in protected mode.
;  $DESCRIPTION$
;     This functions is useful for writing bi-modal code since you can take
;     different actions depending on the CPU mode.
;  $EXAMPLES$
;     if ( cpmiIsProtected() )
;     {
;        // Do protected mode stuff
;     }
;     else
;     {
;        // Do real mode stuff
;     }
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;  $END$
;

IDEAL
P286
          
Public    _hb_cpmiIsProtected

Group     DGROUP    _DATA

Segment   _DATA     Word      Public    "DATA"

ModeFlag  DW        -1

Ends      _DATA



Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor,DS:DGROUP

Proc      _hb_cpmiIsProtected    Far

          Mov       AX,[ModeFlag]                 ; Get mode indicator
          Cmp       AX,-1                         ; See if checked already
          JNE       @@Exit

@@Init:   Inc       AX                            ; Default to FALSE
          Push      SP                            ; Put SP on stack
          Pop       DX                            ; Now get it back
          Cmp       DX,SP                         ; If not ==, 8088/8086
          JNE       @@Done                        ; Return FALSE

          SMSW      AX                            ; Get status word
          And       AX,1                          ; Check for pmode bit
          JZ        @@Done                        ; Return FALSE if clear

          PushF                                   ; Mov flag word . . .
          Pop       DX                            ; . . . to DX
          And       DH,11001111b                  ; Set IOPL to zero
          Push      DX                            ; Move DX . . .
          PopF                                    ; . . . back to flag word
          PushF                                   ; Now move flags . . .
          Pop       DX                            ; . . . back to DX
          Test      DH,110000b                    ; Is IOPL still zero?
          JZ        @@Done                        ; If so, return TRUE
          Mov       AX,1686h                      ; DPMI -- get CPU mode
          Int       2Fh                           ; Call DPMI
          Cmp       AX,1                          ; Check for return >= 1
          Mov       AX,0                          ; Default to real mode
          ADC       AX,0                          ; CF set means protected

@@Done:   Mov       [ModeFlag],AX
@@Exit:   RetF
Endp      _hb_cpmiIsProtected
Ends      _NanFor
End

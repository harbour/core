;
; $Id$
;

; File......: SETBASE.ASM
; Author....: Ted Means
; CIS ID....: 73067,3332
;
; This is an original work by Ted Means and is placed in the
; public domain.
;
; Modification history:
; ---------------------
;     Rev 1.0   01 Jan 1995 03:01:00   TED
;  Initial release
;

;  $DOC$
;  $FUNCNAME$
;     cpmiSetBase()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Set the linear base address associated with a selector.
;  $SYNTAX$
;     int pascal cpmiSetBase( SELECTOR selector, LINEAR base )
;  $ARGUMENTS$
;     selector is the selector for which the base address is to be set.
;
;     base is the linear base address.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise.
;  $DESCRIPTION$
;     This function is useful for assigning physical memory to a selector.
;     Note that it is a linear address and not a real mode segment:offset.
;  $EXAMPLES$
;     auto char * Video;
;
;     FP_SEG( Video ) = cpmiAllocSelector();
;     FP_OFF( Video ) = 0;
;
;     cpmiSetBase( FP_SEG( Video ), 0xB8000 );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiGetBase(), cpmiGetLimit(), cpmiSetLimit()
;  $END$
;

IDEAL
P286

Public    cpmiSetBase

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      cpmiSetBase         Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,7                          ; DPMI -- set base
          Mov       BX,[Word Ptr BP + 10]         ; Get selector
          Mov       DX,[Word Ptr BP + 6]          ; Get low word of address
          Mov       CX,[Word Ptr BP + 8]          ; Get high word of address
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to SUCCEED
          SBB       AX,0                          ; Set to FAIL on error

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      6
Endp      cpmiSetBase
Ends      _NanFor
End

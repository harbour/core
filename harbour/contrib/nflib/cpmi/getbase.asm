;
; $Id$
;

; File......: GETBASE.ASM
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
;     cpmiGetBase()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Obtain the linear base address associated with a selector.
;  $SYNTAX$
;     LINEAR pascal cpmiGetBase( SELECTOR selector )
;  $ARGUMENTS$
;     selector is the selector for which the base address is needed.
;  $RETURNS$
;     The linear base address of the selector.  A null address is returned
;     if the function fails.
;  $DESCRIPTION$
;     This function is useful for determining the actual physical memory
;     associated with a selector.  Note that it is a linear address and
;     not a real mode segment:offset.
;  $EXAMPLES$
;     auto char * Video;
;
;     FP_SEG( Video ) = cpmiSeg2Sel( 0xB800 );
;     FP_OFF( Video ) = 0;
;
;     cpmiGetBase( FP_SEG( Video ) );   // Will return 0xB8000
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiSetBase(), cpmiGetLimit(), cpmiSetLimit()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiGetBase

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiGetBase         Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,6                          ; DPMI -- get base
          Mov       BX,[Word Ptr BP + 6]          ; Get selector
          Int       31h                           ; Call DPMI
          Mov       AX,DX                         ; Load low word
          Mov       DX,CX                         ; Load high word
          JNC       @@Exit                        ; Leave if no error
          Xor       AX,AX                         ; Return null
          Mov       DX,AX

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      2
Endp      _hb_cpmiGetBase
Ends      _NanFor
End

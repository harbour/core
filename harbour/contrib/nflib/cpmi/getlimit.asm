;
; $Id$
;

; File......: GETLIMIT.ASM
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
;     cpmiGetLimit()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Obtain the limit associated with a selector.
;  $SYNTAX$
;     int pascal cpmiGetLimit( SELECTOR selector )
;  $ARGUMENTS$
;     selector is the selector for which the limit is needed.
;  $RETURNS$
;     The limit (i.e. largest possible offset) of the selector.
;     If the function fails (e.g. because of an invalid selector) it
;     will return a limit of zero.
;  $DESCRIPTION$
;     This function is useful for determining the amount of data
;     a selector can access.  Note that attempting to access an offset
;     beyond a selector's limit will cause a GPF.
;  $EXAMPLES$
;     auto char * Video;
;
;     FP_SEG( Video ) = cpmiSeg2Sel( 0xB800 );
;     FP_OFF( Video ) = 0;
;
;     cpmiGetLimit( FP_SEG( Video ) );   // Will return 0xFFFF
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiSetLimit(), cpmiGetBase(), cpmiSetBase()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiGetLimit

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiGetLimit        Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,[Word Ptr BP + 6]          ; Get selector
          LSL       AX,AX                         ; Load limit
          JZ        @@Exit                        ; Leave if selector valid
          Xor       AX,AX                         ; Return zero limit

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      2
Endp      _hb_cpmiGetLimit
Ends      _NanFor
End

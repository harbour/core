;
; $Id$
;

; File......: SETLIMIT.ASM
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
;     cpmiSetLimit()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Set the limit associated with a selector.
;  $SYNTAX$
;     int pascal cpmiSetLimit( SELECTOR selector, unsigned int Limit )
;  $ARGUMENTS$
;     selector is the selector for which the limit is to be set.
;
;     limit is the selector's new limit in bytes.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise.
;  $DESCRIPTION$
;     This function is useful for changing a selector's limit, but use
;     care when making the limit larger that you do not overlap linear
;     memory that you do not own.
;  $EXAMPLES$
;     auto char * Video;
;
;     FP_SEG( Video ) = cpmiAllocSelector();
;     FP_OFF( Video ) = 0;
;
;     cpmiSetLimit( FP_SEG( Video ), 0x8000 );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiGetLimit(), cpmiGetBase(), cpmiSetBase()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiSetLimit

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiSetLimit        Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,8                          ; DPMI -- set limit
          Mov       BX,[Word Ptr BP + 8]          ; Get selector
          Xor       CX,CX                         ; Clear high word
          Mov       DX,[Word Ptr BP + 6]          ; Get limit
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to succeed
          SBB       AX,0                          ; Set to FAIL on error

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      4
Endp      _hb_cpmiSetLimit
Ends      _NanFor
End

;
; $Id$
;

; File......: FREESEL.ASM
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
;     cpmiFreeSelector()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Free a selector
;  $SYNTAX$
;     int pascal cpmiFreeSelector( SELECTOR Selector )
;  $ARGUMENTS$
;     Selector is the previously allocated selector.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise.
;  $DESCRIPTION$
;     This function is used to free selectors that were allocated
;     with cpmiProtectedPtr(), cpmiMakeAlias(), or cpmiAllocSel().
;  $EXAMPLES$
;     FP_SEG( buffer ) = cpmiMakeAlias( selector, AR_WRITE );
;     FP_OFF( buffer ) = 0;
;
;     // Do whatever
;
;     cpmiFreeSelector( FP_SEG( buffer ) );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiAllocSel(), cpmiProtectedPtr(), cpmiMakeAlias()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiFreeSelector

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiFreeSelector    Far

          Enter     0,0                           ; Create stack frame

          Mov       BX,[Word Ptr BP + 6]          ; Load selector
          Mov       AX,ES                         ; Load ES into AX
          Cmp       AX,BX                         ; ES contain selector?
          JNE       @@Free                        ; If not, continue
          Xor       AX,AX                         ; Clear AX
          Mov       ES,AX                         ; Set ES to null selector

@@Free:   Mov       AX,1                          ; DPMI -- Free selector
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to SUCCEED
          SBB       AX,0                          ; Set to FAIL if carry set

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      2
Endp      _hb_cpmiFreeSelector
Ends      _NanFor
End

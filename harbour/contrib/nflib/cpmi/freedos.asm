;
; $Id$
;

; File......: FREEDOS.ASM
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
;     cpmiFreeDOSMem()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Free a DOS memory block
;  $SYNTAX$
;     void pascal cpmiFreeDOSMem( SELECTOR Selector )
;  $ARGUMENTS$
;     Selector is the previously allocated selector
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise
;  $DESCRIPTION$
;     This function is used to free memory blocks that were allocated
;     with the cpmiAllocDOSMem() function.
;  $EXAMPLES$
;     char far * buffer;
;
;     FP_SEG( buffer ) = cpmiAllocDOSMem( 48 );
;     FP_OFF( buffer ) = 0;
;
;     // Do whatever
;
;     cpmiFreeDOSMem( FP_SEG( buffer ) );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiAllocDOSMem(), cpmiResizeDOSMem()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiFreeDOSMem

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiFreeDOSMem      Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,101h                       ; DPMI -- free DOS memory
          Mov       DX,[Word Ptr BP + 6]          ; Load selector
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to SUCCEED
          SBB       AX,0                          ; Set to FAIL on error

          Leave                                   ; Destroy stack frame
          RetF      2
Endp      _hb_cpmiFreeDOSMem
Ends      _NanFor
End

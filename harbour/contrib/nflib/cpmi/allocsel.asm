;
; $Id$
;

; File......: ALLOCSEL.ASM
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
;     cpmiAllocateSelector()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Allocate a selector
;  $SYNTAX$
;     SELECTOR pascal cpmiAllocateSelector( void )
;  $ARGUMENTS$
;     None
;  $RETURNS$
;     A selector with a base and limit of zero.  A null selector is returned
;     if the function fails.
;  $DESCRIPTION$
;     This function is used to create a new selector.  It must be
;     initialized with other CPMI calls before it will be useful.  Be
;     sure to free it with cpmiFreeSelector() when it is no longer needed.
;  $EXAMPLES$
;     auto SELECTOR sel = cpmiAllocateSelector();
;
;     cpmiSetBase( sel, 0xB8000 );
;
;     cpmiSetLimit( sel, 0x8000 );

;     // Do whatever
;
;     cpmiFreeSelector( sel );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiFreeSelector(), cpmiSetLimit(), cpmiSetBase()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiAllocateSelector

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiAllocateSelector          Far

          Xor       AX,AX                         ; DPMI -- alloc selector
          Mov       CX,1                          ; Set selector count
          Int       31h                           ; Call DPMI
          JNC       @@Exit                        ; Leave if no error
          Xor       AX,AX                         ; Return null selector

@@Exit:   RetF
Endp      _hb_cpmiAllocateSelector
Ends      _NanFor
End

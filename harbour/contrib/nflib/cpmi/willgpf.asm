;
; $Id$
;

; File......: WILLGPF.ASM
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
;     cpmiWillGPF()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Determine if accessing a pointer will cause a GPF.
;  $SYNTAX$
;     int pascal cpmiWillGPF( void * PMAddr, unsigned int Access,
;                                            unsigned int Bytes )
;  $ARGUMENTS$
;     PMAddr is the pointer to be tested.
;
;     Access indicates the intended use for the pointer, and should be
;     either AR_READ, AR_WRITE, or AR_EXECUTE (see CPMI.H).  You may
;     also combine these attributes with the | operator.
;
;     Bytes is the number of bytes you intend to access.
;  $RETURNS$
;     Zero if the specified pointer access will not result in a GPF,
;     otherwise a numeric value indicating the cause of the potential
;     protection fault:
;
;         INVALID_SELECTOR is returned if the selector portion of the
;         pointer is invalid.
;
;         INVALID_ACCESS is returned if you specify an access mode that
;         is invalid; e.g. AR_WRITE for a code selector, or AR_EXECUTE
;         for a data selector.
;
;         BEYOND_LIMIT is returned if the specified access will go
;         beyond the limit of the selector.
;
;  $DESCRIPTION$
;     This function can help to avoid GPFs by allowing you to validate
;     pointers before attempting to use them.
;  $EXAMPLES$
;     if ( cpmiWillGPF( thePointer, PA_READ | PA_EXECUTE, 128 ) )
;         _gtWriteCon( "GPF will occur.", 15 );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;  $END$
;

IDEAL
P286

Public    _hb_cpmiWillGPF

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiWillGPF         Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,-1                         ; Return == bad selector
          Mov       DX,[Word Ptr BP + 12]         ; Load selector
          LAR       CX,DX                         ; Load access rights
          JNZ       @@Exit                        ; Jump if bad selector

          Dec       AX                            ; Return == invalid access
          Mov       BX,[Word Ptr BP + 8]          ; Load requested access
          And       CX,0F00h                      ; Mask all but type bits

@@Read:   Test      BX,1                          ; Check read access
          JZ        @@Write                       ; If not, check write
          Cmp       CH,7                          ; Check all types that
          JBE       @@Write                       ; allow reading
          Cmp       CH,0Ah
          JE        @@Write
          Cmp       CH,0Bh
          JE        @@Write
          Cmp       CH,0Eh
          JB        @@Exit                        ; Read not allowed

@@Write:  Test      BX,2                          ; Check write access
          JZ        @@Execute                     ; If not, check execute
          Cmp       CH,2                          ; Check all types that
          JE        @@Execute                     ; allow execution
          Cmp       CH,3
          JE        @@Execute
          Cmp       CH,6
          JE        @@Execute
          Cmp       CH,7
          JNE       @@Exit                        ; Write not allowed

@@Execute:Test      BX,4                          ; Check execute access
          JZ        @@Limit                       ; If not, check limit
          Cmp       CH,8                          ; Check execution types
          JB        @@Exit                        ; Execute not allowed

@@Limit:  Dec       AX                            ; Return == beyond limit
          LSL       CX,DX                         ; Load limit
          Mov       DX,[Word Ptr BP + 10]         ; Get offset
          Add       DX,[Word Ptr BP + 6]          ; Add byte count
          Dec       DX                            ; Adjust for zero origin
          Cmp       DX,CX                         ; Compare to limit
          JA        @@Exit                        ; Bail if beyond
          Xor       AX,AX                         ; Return == okay

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      8                             ; Remove params
Endp      _hb_cpmiWillGPF
Ends      _NanFor
End

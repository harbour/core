;
; $Id$
;

; File......: PMPTR.ASM
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
;     cpmiProtectedPtr()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Generate a protected mode selector from a real mode address
;  $SYNTAX$
;     SELECTOR pascal cpmiProtectedPtr( void * RMAddr, unsigned int Size )
;  $ARGUMENTS$
;     RMAddr is a protected mode address (segment:offset)
;
;     Size is the number of bytes to which RMAddr points.
;  $RETURNS$
;     A selector which maps to the same physical memory.  A null selector
;     is returned if the function fails.
;  $DESCRIPTION$
;     This function is used to obtain the protected mode equivalent of a
;     real mode pointer, which is useful when attempting to access DOS or
;     BIOS memory locations.
;
;     Note that only the selector is generated; the offset is always zero.
;
;     Be sure to free with selector with cpmiFreeSelector() when it is no
;     longer needed.
;  $EXAMPLES$
;     auto long * Timer;
;     auto long TickCount;
;
;     FP_SEG( Timer ) = cpmiProtectedPtr( ( long * ) 0x0000046C,
;                                   sizeof( long ) );
;     FP_OFF( Timer ) = 0;
;
;     TickCount = *Timer;
;
;     cpmiFreeSelector( FPSEG( Timer ) );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiRealPtr(), cpmiFreeSelector()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiProtectedPtr

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiProtectedPtr    Far

          Enter     2,0                           ; Create stack frame

          Xor       AX,AX                         ; DPMI -- allocate selector
          Mov       CX,1                          ; Number of selectors
          Int       31h                           ; Call DPMI
          JC        @@Null                        ; Bail if error

          Mov       [Word Ptr BP - 2],AX          ; Store selector
          Mov       BX,AX                         ; Move selector to BX
          Xor       CX,CX                         ; Clear CX
          Mov       DX,[Word Ptr BP + 10]         ; Load segment value
          SHL       DX,1                          ; Move high bit . . .
          RCL       CX,1                          ; . . . into CX low bit
          SHL       DX,1                          ; Move high bit . . .
          RCL       CX,1                          ; . . . into CX low bit
          SHL       DX,1                          ; Move high bit . . .
          RCL       CX,1                          ; . . . into CX low bit
          SHL       DX,1                          ; Move high bit . . .
          RCL       CX,1                          ; . . . into CX low bit
          Add       DX,[Word Ptr BP + 8]          ; Form linear address
          ADC       CX,0                          ; Carry if necessary
          Mov       AX,7                          ; DPMI -- set base address
          Int       31h                           ; Call DPMI
          JC        @@Null                        ; Bail if error

          Mov       AX,8                          ; DPMI -- set segment size
          Mov       BX,[Word Ptr BP - 2]          ; Move selector to BX
          Xor       CX,CX                         ; Clear size high word
          Mov       DX,[Word Ptr BP + 6]          ; Get size low word
          Int       31h                           ; Call DPMI
          Mov       AX,[Word Ptr BP - 2]          ; Move selector to AX
          JNC       @@Exit                        ; If no error, quit

@@Null:   Xor       AX,AX                         ; Create null selector

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      6
Endp      _hb_cpmiProtectedPtr
Ends      _NanFor
End

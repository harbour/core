;
; $Id$
;

; File......: RMPTR.ASM
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
;     cpmiRealPtr()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Generate a real mode pointer from a protected mode address
;  $SYNTAX$
;     void * pascal cpmiRealPtr( void * PMAddr )
;  $ARGUMENTS$
;     PMAddr is a protected mode address (selector:offset)
;  $RETURNS$
;     The actual real mode physical address (segment:offset)
;  $DESCRIPTION$
;     This function is used to obtain the real mode equivalent of a
;     protected mode pointer, which is useful when attempting to call
;     real mode interrupts.
;
;     Note that if the protected mode address falls above the real mode
;     address space (1 mb) then a null pointer will be returned.
;  $EXAMPLES$
;     auto char far * buffer;
;
;     FP_SEG( buffer ) = cpmiAllocDOSMem( 128 );
;     FP_OFF( buffer ) = 0;
;
;     buffer = cpmiRealPtr( buffer );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiProtectedPtr(), cpmiAllocDOSMem(), cpmiFreeDOSMem(),
;     cpmiResizeDOSMem(), cpmiInt86()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiRealPtr

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiRealPtr         Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,6                          ; DPMI -- get base address
          Mov       BX,[Word Ptr BP + 8]          ; Load selector
          Int       31h                           ; Call DPMI
          JC        @@Null                        ; Bail if error

          Add       DX,[Word Ptr BP + 6]          ; Add offset to address
          ADC       CX,0                          ; Carry into high word

          Mov       AX,DX                         ; Load low part of address
          And       AX,1111b                      ; Mask off upper bits
          SHR       CX,1                          ; Move CX low bit . . .
          RCR       DX,1                          ; . . . into DX high bit
          SHR       CX,1                          ; Move CX low bit . . .
          RCR       DX,1                          ; . . . into DX high bit
          SHR       CX,1                          ; Move CX low bit . . .
          RCR       DX,1                          ; . . . into DX high bit
          SHR       CX,1                          ; Move CX low bit . . .
          RCR       DX,1                          ; . . . into DX high bit
          Or        CX,CX                         ; See if CX is clear
          JZ        @@Exit                        ; If so, okay

@@Null:   Xor       AX,AX                         ; Clear AX
          Mov       DX,AX                         ; Create null pointer

@@Exit:   Leave
          RetF      4
Endp      _hb_cpmiRealPtr
Ends      _NanFor
End

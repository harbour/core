;
; $Id$
;

; File......: ALLOCDOS.ASM
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
;     cpmiAllocDOSMem()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Allocate a DOS memory block from the lower 640K
;  $SYNTAX$
;     SELECTOR pascal cpmiAllocDOSMem( unsigned int Size )
;  $ARGUMENTS$
;     Size is the number of bytes to allocate.
;  $RETURNS$
;     A selector which is guaranteed to map to physical memory in the
;     lower 640K.
;  $DESCRIPTION$
;     This function is useful for allocating memory that needs to exist in
;     the DOS memory pool; e.g. buffers used by real-mode interrupts.
;
;     Note that only the selector is returned; the offset is always zero.
;
;     You can obtain the real mode segment:offset of the selector by calling
;     cpmiRealPtr().
;
;     When the memory is no longer needed, be sure to call cpmiFreeDOSMem()
;     to release it.
;  $EXAMPLES$
;     char far * buffer;
;
;     FP_SEG( buffer ) = cpmiAllocateDOSMem( 48 );
;     FP_OFF( buffer ) = 0;
;
;     // Do whatever
;
;     cpmiFreeDOSMem( FP_SEG( buffer ) );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiFreeDOSMem(), cpmiResizeDOSMem(), cpmiRealPtr()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiAllocateDOSMem

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiAllocateDOSMem  Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,100h                       ; DPMI -- alloc DOS memory
          Mov       BX,[Word Ptr BP + 6]          ; Get size in bytes
          Add       BX,15                         ; Round up if necessary
          SHR       BX,4                          ; Convert to paragraphs
          Int       31h                           ; Call DPMI
          Mov       AX,DX                         ; Move selector to AX
          JNC       @@Exit                        ; If no error, return

@@Null:   Xor       AX,AX                         ; Return null selector

@@Exit:   Leave                                   ; Destroy stack frame
          RetF      2
Endp      _hb_cpmiAllocateDOSMem
Ends      _NanFor
End

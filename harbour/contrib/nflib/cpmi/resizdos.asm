;
; $Id$
;

; File......: RESIZDOS.ASM
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
;

;  $DOC$
;  $FUNCNAME$
;     cpmiResizeDOSMem()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Resize a DOS memory block allocated with cpmiAllocDOSMem()
;  $SYNTAX$
;     int pascal cpmiAllocDOSMem( SELECTOR selector, unsigned int Size )
;  $ARGUMENTS$
;     selector identifies the DOS memory block.
;
;     size is the new size in bytes.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise.
;  $DESCRIPTION$
;     This function is useful for resizing buffers that reside in the
;     lower 640K; e.g. for DOS interrupts.
;  $EXAMPLES$
;     char far * buffer;
;
;     FP_SEG( buffer ) = cpmiAllocDOSMem( 48 );
;     FP_OFF( buffer ) = 0;
;
;     // Do whatever
;
;     cpmiResizeDOSMem( FP_SEG( buffer ), 128 );
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiAllocDOSMem(), cpmiFreeDOSMem()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiResizeDOSMem

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiResizeDOSMem    Far

          Enter     0,0                           ; Create stack frame

          Mov       AX,102h                       ; DPMI -- resize DOS memory
          Mov       BX,[Word Ptr BP + 6]          ; Get size in bytes
          Add       BX,15                         ; Round up if necessary
          SHR       BX,4                          ; Convert to paragraphs
          Mov       DX,[Word Ptr BP + 8]          ; Get selector
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to SUCCEED
          SBB       AX,0                          ; Set to FAIL on error

          Leave                                   ; Destroy stack frame
          RetF      4
Endp      _hb_cpmiResizeDOSMem
Ends      _NanFor
End

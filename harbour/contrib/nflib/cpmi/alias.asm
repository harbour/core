;
; $Id$
;


; File......: ALIAS.ASM
; Author....: Ted Means
; CIS ID....: 73067,3332
;
; This is an original work by Ted Means and is placed in the
; public domain.
;
; Modification history:
; ---------------------
;     Rev 1.0   01 Jan 1995 03:01:00   TED
;     Initial release
;

;  $DOC$
;  $FUNCNAME$
;     cpmiMakeAlias()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Create alias descriptor
;  $SYNTAX$
;     SELECTOR pascal cpmiMakeAlias( SELECTOR Selector, unsigned int Rights )
;  $ARGUMENTS$
;     Selector is a selector for which an alias should be created.
;
;     Rights is an integer describing the access rights for the selector,
;     and should be either AR_READ, AR_WRITE, or AR_EXECUTE.  You may
;     also combine these attributes with the | operator, although
;     you may not mix AR_WRITE and AR_EXECUTE.  Note that AR_WRITE and
;     AR_EXECUTE both implicitly allow AR_READ rights.
;  $RETURNS$
;     A selector which maps to the same physical memory as the source
;     selector.
;  $DESCRIPTION$
;     This function duplicates a selector while giving different access
;     rights.  This is useful for writing to a code segment, making a
;     data segment read-only, etc.
;
;     When no longer needed, the selector should be freed via a call to
;     cpmiFreeSelector().
;  $EXAMPLES$
;     OldSelector = FP_SEG( SomePointer );
;
;     // Create a read / write data selector
;
;     NewSelector = cpmiMakeAlias( OldSelector, AR_READ | AR_WRITE )
;
;     // You can also make it read-only
;
;     NewSelector = cpmiMakeAlias( OldSelector, AR_READ )
;
;     cpmiWillGPF( NewSelector, AR_WRITE, 1 )     // Returns INVALID_ACCESS
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiFreeSelector()
;  $END$
;

IDEAL
P286

Public    _hb_cpmiMakeAlias

Segment   _NanFor   Word      Public    "CODE"
          Assume    CS:_NanFor

Proc      _hb_cpmiMakeAlias     Far

          Enter     10,0                          ; Create stack frame
          Push      DI                            ; Save DI

          Mov       AX,0Ah                        ; DPMI -- create alias
          Mov       BX,[Word Ptr BP + 8]          ; Load selector
          Int       31h                           ; Call DPMI
          JC        @@Null                        ; Bail if error

          Mov       [Word Ptr BP - 2],AX          ; Store selector
          Mov       BX,AX                         ; Load selector into BX
          Push      SS                            ; Move SS . . .
          Pop       ES                            ; . . . to ES
          LEA       DI,[BP - 10]                  ; Load offset of buffer
          Mov       AX,0Bh                        ; DPMI -- get descriptor
          Int       31h                           ; Call DPMI
          JC        @@Free                        ; Bail if error

          Mov       AX,[Word Ptr BP + 6]          ; Load access rights
          Mov       CL,[Byte BP - 5]              ; Load existing rights
          Test      AX,4                          ; Check for code bit
          JZ        @@Data                        ; If not set, must be data
          Or        CL,00001010b                  ; Set bits for code
          And       CL,11111010b                  ; Turn off A & E/C bits
          Jmp       Short @@Reset                 ; Set new rights

@@Data:   And       CL,11110000b                  ; Set bits for data
          And       AL,2                          ; Isolate write bit
          Or        CL,AL                         ; Set / Clear write bit

@@Reset:  Mov       [Byte Ptr BP - 5],CL          ; Store new rights
          Push      SS                            ; Move SS . . .
          Pop       ES                            ; . . . to ES
          LEA       DI,[BP - 10]                  ; Load offset of buffer
          Mov       AX,0Ch                        ; DPMI -- set descriptor
          Int       31h                           ; Call DPMI
          Mov       AX,[Word Ptr BP - 2]          ; Load return selector
          JNC       @@Exit                        ; If no error, quit

@@Free:   Mov       AX,1                          ; DPMI -- Free selector
          Mov       BX,[Word Ptr BP - 2]          ; Get selector
          Int       31h                           ; Call DPMI

@@Null:   Xor       AX,AX                         ; Return NULL selector

@@Exit:   Pop       DI                            ; Restore DI
          Leave                                   ; Destroy stack frame
          RetF      4
Endp      _hb_cpmiMakeAlias
Ends      _NanFor
End

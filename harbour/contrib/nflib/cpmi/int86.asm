;
; $Id$
;

; File......: INT86.ASM
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
;     cpmiInt86()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Execute a real-mode interrupt
;  $SYNTAX$
;     int pascal cpmiInt86( unsigned char IntNo, CPUREGS * InRegs,
;                                                CPUREGS * OutRegs )
;  $ARGUMENTS$
;     IntNo is the interrupt to execute
;
;     InRegs is a pointer to a CPUREGS structure containing the incoming
;     register assignments.
;
;     OutRegs is a pointer to a CPUREGS structure where the outgoing
;     register contents will be stored.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise
;  $DESCRIPTION$
;     Use cpmiInt86() to execute a real mode interrupt.  This function
;     allows you to set real mode segment register values from protected
;     mode.  A call to cpmiInt86() sets the registers to the values you
;     provide in the structure to which InRegs points; then it invokes
;     interrupt IntNo.  After the interrupt is processed, the register
;     values will be stored in the structure pointed to by OutRegs.  The
;     structures pointed to by InRegs and OutRegs are both of type CPUREGS.
;
;  $EXAMPLES$
;     auto CPUREGS Regs;
;
;     Regs.Reg.AX = 0x19 << 8;          // Get default drive
;
;     cpmiInt86( 0x21, &Regs, &Regs )   // Call DOS
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiAllocDOSMem(), cpmiFreeDOSMem(), cpmiRealPtr(), cpmiFarCallReal()
;  $END$
;

IDEAL
P286

Struc     CPUREGS
DI        DW        ?
DIHi      DW        ?
SI        DW        ?
SIHi      DW        ?
BP        DW        ?
BPHi      DW        ?
Rsv       DW        ?
RsvHi     DW        ?
BX        DW        ?
BXHi      DW        ?
DX        DW        ?
DXHi      DW        ?
CX        DW        ?
CXHi      DW        ?
AX        DW        ?
AXHi      DW        ?
Flags     DW        ?
ES        DW        ?
DS        DW        ?
FS        DW        ?
GS        DW        ?
IP        DW        ?
CS        DW        ?
SP        DW        ?
SS        DW        ?
Ends      CPUREGS

Public    hb_cpmiInt86

Extrn     __bset:Far

Regs      EQU       (CPUREGS Ptr BP - Size CPUREGS)

Segment   _NanFor   Word      Public    "CODE"

Proc      hb_cpmiInt86 Far

          Enter     Size CPUREGS,0                ; Create stack frame
          Push      DS                            ; Save DS
          Push      SI                            ; Save SI
          Push      DI                            ; Save DI
          Push      BP                            ; Save BP

          LEA       AX,[BP - Size CPUREGS]        ; Calc offset
          Push      Size CPUREGS                  ; Set byte count
          Push      0                             ; Set fill character
          Push      SS                            ; Put segment on stack
          Push      AX                            ; Put offset on stack
          Call      __bset                        ; Init structure
          Add       SP,8                          ; Realign stack

          LES       BX,[DWord Ptr BP + 10]        ; Load pointer to in regs
          Push      [Word Ptr ES:BX]              ; Put AX value on stack
          Push      [Word Ptr ES:BX + 2]          ; Put BX value on stack
          Push      [Word Ptr ES:BX + 4]          ; Put CX value on stack
          Push      [Word Ptr ES:BX + 6]          ; Put DX value on stack
          Push      [Word Ptr ES:BX + 8]          ; Put SI value on stack
          Push      [Word Ptr ES:BX + 10]         ; Put DI value on stack
          Push      [Word Ptr ES:BX + 12]         ; Put BP value on stack
          Push      [Word Ptr ES:BX + 14]         ; Put DS value on stack
          Push      [Word Ptr ES:BX + 16]         ; Put ES value on stack
          Push      [Word Ptr ES:BX + 18]         ; Put flags value on stack
          Pop       [Regs.Flags]
          Pop       [Regs.ES]
          Pop       [Regs.DS]
          Pop       [Regs.BP]
          Pop       [Regs.DI]
          Pop       [Regs.SI]
          Pop       [Regs.DX]
          Pop       [Regs.CX]
          Pop       [Regs.BX]
          Pop       [Regs.AX]

          Mov       AX,300h                       ; DPMI -- real mode int
          Mov       BL,[Byte Ptr BP + 14]         ; Get interrupt number
          Mov       BH,0                          ; Clear BH
          Xor       CX,CX                         ; Clear CX
          Push      SS                            ; Move SS . . .
          Pop       ES                            ; . . . to ES
          LEA       DI,[BP - Size CPUREGS]        ; Load register offset
          Int       31h                           ; Call DPMI
          Mov       AX,1                          ; Default to SUCCEED
          SBB       AX,0                          ; Set to FAIL on error

          CLD                                     ; Restore preserved
          Pop       BP                            ; registers just in case
          Pop       DI                            ; DPMI host is ill-behaved
          Pop       SI
          Pop       DS

          LES       BX,[DWord Ptr BP + 6]         ; Load pointer to outregs
          Push      [Regs.Flags]                  ; Put register values on
          Push      [Regs.ES]                     ; stack
          Push      [Regs.DS]
          Push      [Regs.BP]
          Push      [Regs.DI]
          Push      [Regs.SI]
          Push      [Regs.DX]
          Push      [Regs.CX]
          Push      [Regs.BX]
          Push      [Regs.AX]
          Pop       [Word Ptr ES:BX]              ; Get AX value
          Pop       [Word Ptr ES:BX + 2]          ; Get BX value
          Pop       [Word Ptr ES:BX + 4]          ; Get CX value
          Pop       [Word Ptr ES:BX + 6]          ; Get DX value
          Pop       [Word Ptr ES:BX + 8]          ; Get SI value
          Pop       [Word Ptr ES:BX + 10]         ; Get DI value
          Pop       [Word Ptr ES:BX + 12]         ; Get BP value
          Pop       [Word Ptr ES:BX + 14]         ; Get DS value
          Pop       [Word Ptr ES:BX + 16]         ; Get ES value
          Pop       [Word Ptr ES:BX + 18]         ; Get flags value

          Leave
          RetF      10
Endp      hb_cpmiInt86
Ends      _NanFor
End

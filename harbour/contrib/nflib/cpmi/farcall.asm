;
; $Id$
;

; File......: FARCALL.ASM
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
;     cpmiFarCallReal()
;  $CATEGORY$
;     CPMI
;  $ONELINER$
;     Call a real mode function via pointer.
;  $SYNTAX$
;     int pascal cpmiFarCallReal( void * FuncAddr, CPUREGS * InRegs,
;                                                  CPUREGS * OutRegs )
;  $ARGUMENTS$
;     FuncAddr is the real mode address of the function to call.
;
;     InRegs is a pointer to a CPUREGS structure containing the incoming
;     register assignments.
;
;     OutRegs is a pointer to a CPUREGS structure where the outgoing
;     register contents will be stored.
;  $RETURNS$
;     SUCCEED if successful, FAIL otherwise
;  $DESCRIPTION$
;     Use cpmiFarCallReal() to execute a real mode function.  This function
;     allows you to set real mode segment register values from protected
;     mode.  A call to cpmiFarCallReal() sets the registers to the values
;     you provide in the structure to which InRegs points; then it invokes
;     the function at the address specified by FuncAddr.  After the call
;     has been completed, the register values will be stored in the structure
;     pointed to by OutRegs.  The structures pointed to by InRegs and OutRegs
;     are both of type CPUREGS.
;
;  $EXAMPLES$
;     auto CPUREGS Regs;
;
;     cpmiFarCallReal( ( void * ) 0xF000FFF0, &Regs, &Regs )   // Reboot
;  $INCLUDE$
;     CPMI.H
;  $SEEALSO$
;     cpmiAllocDOSMem(), cpmiFreeDOSMem(), cpmiRealPtr(), cpmiInt86()
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

Public    _hb_cpmiFarCallReal

Extrn     __bset:Far

Regs      EQU       (CPUREGS Ptr BP - Size CPUREGS)

Segment   _NanFor   Word      Public    "CODE"

Proc      _hb_cpmiFarCallReal     Far

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

          Mov       AX,[Word Ptr BP + 14]
          Mov       [Regs.IP],AX
          Mov       AX,[Word Ptr BP + 16]
          Mov       [Regs.CS],AX

          Mov       AX,301h                       ; DPMI -- real mode call
          Xor       BX,BX                         ; Clear BX
          Mov       CX,BX                         ; Clear CX
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
          Push      [Regs.ES]                     ; Put register values on
          Push      [Regs.DS]                     ; stack
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
Endp      _hb_cpmiFarCallReal
Ends      _NanFor
End

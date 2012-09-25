/*
 * $Id$
 */

/*
 * File......: putkey.asm
 * Author....: Ted Means
 * CIS ID....: 73067,3332
 *
 * This is an original work by Ted Means and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *     Rev 1.4   16 Oct 1992 00:00:56   GLENN
 *  Just making sure we have Ted's latest revisions.
 *
 *     Rev 1.3   01 Jul 1992 01:07:02   GLENN
 *  putkey.asm now bypasses the BIOS completely and uses Clipper's
 *  internal event handler to stuff the keystroke.  Modifications by
 *  Ted Means.
 *
 *     Rev 1.2   15 Aug 1991 23:07:10   GLENN
 *  Forest Belt proofread/edited/cleaned up doc
 *
 *     Rev 1.1   14 Jun 1991 19:54:56   GLENN
 *  Minor edit to file header
 *
 *     Rev 1.0   01 Apr 1991 01:03:48   GLENN
 *  Nanforum Toolkit
 */

/*This  is the Original FT_PUTKEY() code
   IDEAL

   Public   FT_PutKey

   Extrn    __ParInfo:Far
   Extrn    __Parni:Far
   Extrn    __RetL:Far
   Extrn    __evLow:Far                         ; Internal!!  Sniveling cowards
                                             ; beware!

   Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

   Proc     FT_PutKey Far

         Xor       AX,AX                     ; Prepare to count params
         Push      AX                        ; Save on stack
         Call      __ParInfo                 ; Get param count
         Add       SP,2                      ; Realign stack
         Or        AX,AX                     ; Zero params?
         JNZ       Test1                     ; If not, continue
         Jmp       Done                      ; Return value = false, go to end

   Test1:   Mov       AX,1                      ; Prepare to check parameter #1
         Push      AX                        ; Save parameter # on stack
         Call      __ParInfo                 ; Call parameter info routine
         Add       SP,2                      ; Realign stack
         Test      AX,2                      ; Is parameter numeric?
         JNZ       Get1                      ; If so, continue
   BadParam:Xor       AX,AX                     ; Set return value to false
         Jmp       Done                      ; Go to end

   Get1:    Mov       AX,1                      ; Prepare to retrieve parameter #1
         Push      AX                        ; Save parameter # on stack
         Call      __ParNI                   ; Retrieve parameter
         Add       SP,2                      ; Realign stack

         Cmp       AX,385                    ; Test highest inkey()
         JG        BadParam                  ; Bad INKEY() value
         Cmp       AX,-39                    ; Test lowest INKEY()
         JL        BadParam                  ; Bad INKEY() value

   CtrlF1:  Mov       CL,0                      ; Set ASCII value to null
         Cmp       AX,-10                    ; Is Ctrl F1 thru Alt F10?
         JG        F2                        ; If not, check next range
         Neg       AX                        ; Get absolute value of AX
         Add       AL,74                     ; Translate INKEY() to scan code
         Mov       CH,AL                     ; Move scan code to CH
         Jmp       StuffIt                   ; Stuff the keystroke

   F2:      Or        AX,AX                     ; See if key is F2 thru F10
         JNS       F1                        ; If not, check next range
         Neg       AX                        ; Get absolute value of AX
         Add       AL,59                     ; Translate INKEY() to scan code
         Mov       CH,AL                     ; Move scan code to CH
         Jmp       StuffIt                   ; Stuff the keystroke

   F1:      Cmp       AX,28                     ; See if key is F1
         JNE       CtrlF                     ; If not, check next key
         Mov       CH,59                     ; Supply scan code for F1
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlF:   Cmp       AX,6                      ; See if key is Ctrl F or End
         JNE       CtrlW                     ; If not, check next key
         Mov       CH,79                     ; Supply scan code for End
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlW:   Cmp       AX,23                     ; See if key is Ctrl W or Ctrl End
         JNE       CtrlHome                  ; If not, check next key
         Mov       CH,117                    ; Supply scan code for Ctrl End
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlHome:Cmp       AX,29                     ; See if key is Ctrl Home or Ctrl]
         JNE       CtrlV                     ; If not, check next key
         Mov       CH,119                    ; Supply scan code for Ctrl Home
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlV:   Cmp       AX,22                     ; See if key is Ins or Ctrl V
         JNE       ShiftTab                  ; If not, check next key
         Mov       CH,82                     ; Supply scan code for Ctrl V
         Jmp       StuffIt                   ; Stuff the keystroke

   ShiftTab:Cmp       AX,271                    ; See if key is Shift Tab
         JNE       CtrlPgDn                  ; If not, check next key
         Mov       CH,15                     ; Supply scan code for Shift Tab
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlPgDn:Cmp       AX,30                     ; See if key is Ctrl PgDn
         JNE       CtrlPgUp                  ; If not, check next key
         Mov       CH,118                    ; Supply scan code for Ctrl PgDn
         Jmp       StuffIt                   ; Stuff the keystroke

   CtrlPgUp:Cmp       AX,31                     ; See if key is Ctrl PgUp
         JNE       AltQ                      ; If not, check next key
         Mov       CH,132                    ; Supply scan code for Ctrl PgUp
         Jmp       StuffIt                   ; Stuff the keystroke

   AltQ:    Cmp       AX,272                    ; See if key is Alt Q . . .
         JL        ASCII
         Cmp       AX,281                    ; . . . thru Alt P
         JG        AltA
         Mov       CH,AL
         Jmp       StuffIt

   AltA:    Cmp       AX,286                    ; See if key is Alt A . . .
         JNL       AltL
         Jmp       BadParam
   AltL:    Cmp       AX,294                    ; . . . thru Alt L
         JG        AltZ
         Mov       CH,AL
         Jmp       StuffIt

   AltZ:    Cmp       AX,300                    ; See if key is Alt Z . . .
         JNL       AltM
         Jmp       BadParam
   AltM:    Cmp       AX,306                    ; . . . thru Alt M
         JG        Alt1
         Mov       CH,AL
         Mov       CL,0
         Jmp       StuffIt

   Alt1:    Cmp       AX,376                    ; See if key is Alt 1 . . .
         JNL       AltNum
         Jmp       BadParam
   AltNum:  Mov       CH,AL
         Mov       CL,0
         Jmp       StuffIt

   ASCII:   Or        AH,AH                     ; See if key is plain ASCII
         JZ        Okay
         Jmp       BadParam
   Okay:    Mov       CX,AX

   StuffIt: Push      BP                        ; Save BP
         Mov       BP,SP                     ; Set up stack reference
         Sub       SP,10                     ; Allocate local structure
         Mov       [Word Ptr BP - 10],5      ; Choose keystroke event
         Mov       [Word Ptr BP - 8],0       ; Init flag word to 0
         Mov       [Word Ptr BP - 6],CX      ; Store keystroke in structure
         Mov       [Word Ptr BP - 4],0       ; Init mouse row to 0
         Mov       [Word Ptr BP - 2],0       ; Init mouse col to 0
         Push      SS                        ; Put structure segment on stack
         LEA       AX,[BP - 10]              ; Load structure offset
         Push      AX                        ; Put structure offset on stack
         Mov       AX,3                      ; Post event subfunction code
         Push      AX                        ; Place onto stack
         Call      __evLow                   ; Call internal event handler
         Dec       AX                        ; Convert error code . . .
         Neg       AX                        ; . . . to a logical value
         Mov       SP,BP                     ; Restore stack alignment
         Pop       BP                        ; Restore BP

   Done:    Push      AX                        ; Save return value on stack
         Call      __RetL                    ; Return it to Clipper app
         Add       SP,2                      ; Realign stack
         Ret
   Endp     FT_PutKey
   Ends     _NanFor
   End
 */

/* This is the New one Rewriten in C*/

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_PUTKEY )
{
   HB_BOOL lSuccess = HB_FALSE;

   if( HB_ISNUM( 1 ) )
   {
      int iKey = hb_parni( 1 );

      if( iKey >= -39 && iKey <= 385 )
      {
         hb_inkeyPut( iKey );
         lSuccess = HB_TRUE;
      }
   }
   hb_retl( lSuccess );
}

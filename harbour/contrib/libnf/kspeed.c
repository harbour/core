/*
 * $Id$
 */

/* File......: KSPEED.ASM
* Author....: James R. Zack
* CIS ID....: 75410,1567
*
* This is an original work by James R. Zack and is placed in the
* public domain.
*
* Modification history:
* ---------------------
*
*     Rev 1.2   15 Aug 1991 23:06:54   GLENN
*  Forest Belt proofread/edited/cleaned up doc
*
*     Rev 1.1   14 Jun 1991 19:54:40   GLENN
*  Minor edit to file header
*
*     Rev 1.0   01 Apr 1991 01:03:28   GLENN
*  Nanforum Toolkit
*
*/


/* $DOC$
* $FUNCNAME$
*     FT_SETRATE()
* $CATEGORY$
*     Keyboard/Mouse
* $ONELINER$
*     Set the keyboard delay and repeat rate on PC/AT & PS/2
* $SYNTAX$
*     FT_SETRATE( [ <nDelayTime> ] [, <nRepeatRate> ] ) -> NIL
* $ARGUMENTS$
*     <nDelayTime> is the keyboard delay time.
*
*     <nRepeatRate> is the keyboard repeat rate.
*
*          ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
*          ³ nDelayTime      DELAY ³  ³ RepeatRate      SPEED  ³
*          ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´  ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
*          ³     0           250ms ³  ³    0           30.0cps ³
*          ³     1 (default) 500ms ³  ³    1           26.7cps ³
*          ³     2           750ms ³  ³    2           24.0cps ³
*          ³     3          1000ms ³  ³    3           21.8cps ³
*          ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ  ³    4           20.0cps ³
*                                     ³    5           18.5cps ³
*                                     ³    6           17.1cps ³
*                                     ³    7           16.0cps ³
*                                     ³    8           15.0cps ³
*                                     ³    9           13.3cps ³
*                                     ³   10           12.0cps ³
*                                     ³   11           10.9cps ³
*                                     ³   12 (default) 10.0cps ³
*                                     ³   13            9.2cps ³
*                                     ³   14            8.6cps ³
*                                     ³   15            8.0cps ³
*                                     ³   16            7.5cps ³
*                                     ³   17            6.7cps ³
*                                     ³   18            6.0cps ³
*                                     ³   19            5.5cps ³
*                                     ³   20            5.0cps ³
*                                     ³   21            4.6cps ³
*                                     ³   22            4.3cps ³
*                                     ³   23            4.0cps ³
*                                     ³   24            3.7cps ³
*                                     ³   25            3.3cps ³
*                                     ³   26            3.0cps ³
*                                     ³   27            2.7cps ³
*                                     ³   28            2.5cps ³
*                                     ³   29            2.3cps ³
*                                     ³   30            2.1cps ³
*                                     ³   31            2.0cps ³
*                                     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
* $RETURNS$
*    NIL
* $DESCRIPTION$
*    This routine is used to adjust the IBM PC/AT and PS/2 "typematic"
*    repeat and delay feature.  This is used to allow the users of your
*    application to adjust these speeds to the most comfortable level.
*
*    This source code is written for Microsoft Assembler v5.1.
* $EXAMPLES$
*    FT_SETRATE(0,0)    // Set keyboard to fastest possible settings
*    FT_SETRATE()       // Set keyboard to AT defaults (10.9cps,500ms delay)
*    FT_SETRATE(11,1)   // Set keyboard to PS/2 defaults (10cps,500ms delay)
* $END$
*/
/*This  is the Original FT_SETRATE() code
PUBLIC     FT_SETRATE                   * MAKE ROUTINE VISIBLE

EXTRN      __PARNI:FAR                  * DECLARE EXTERNALS
EXTRN      __RET:FAR
EXTRN      __PARINFO:FAR

_NANFOR   SEGMENT       'CODE'
           ASSUME        CS:_NANFOR     * POINT CS TO MY CODE
FT_SETRATE PROC          FAR
           PUSH          BP             * SAVE BASE POINTER
           MOV           BP,SP          * POINT TO TOP OF STACK
           PUSH          DS             * SAVE REGISTERS
           PUSH          ES
           PUSH          SI
           PUSH          DI
           MOV           AX,0           * LOOK AT NUMBER OF PARAMS PASSED
           PUSH          AX             * SET UP FOR __PARINFO
           CALL          __PARINFO      * GET NUMBER OF PARAMS PASSED
           ADD           SP,2           * ADJUST STACK
           CMP           AX,2           * WERE BOTH PARMS PASSED?
           JL            DEFAULTS       * NO, USE DEFAULTS
           JMP           GETPARMS       * OTHERWISE, LETS GET SOME PARAMS.
DEFAULTS:  MOV           BX,010CH       * SET UP DEFAULTS (for AT)
           jmp           goodparm       * and make the int call.
getparms:  mov           ax,01h         * First param is repeat rate
           push          ax             * Set up for __PARNI
           call          __PARNI        * Get first param
           add           sp,2           * Adjust stack
           mov           bl,al          * Put repeat rate into BL
           cmp           bl,20h         * Is BL > 20h? (max value)
           jg            defaults       * Yes, then use defaults
           mov           ax,02h         * Second parm is typeamatic delay
           push          ax             * Set up for __PARNI
           call          __PARNI        * Get second param
           add           sp,2           * Adjust stack
           mov           bh,al          * Put delay into BH
           cmp           bh,04h         * Is BH > 04h (max value)
           jg            defaults       * Yes, then use defaults
goodparm:  mov           ax,0305h       * BIOS Function 03 Subfunction 05
           int           16h            * Set Typematic Rate and Delay
exit:      pop           di             * Retore registers
           pop           si
           pop           es
           pop           ds
           pop           bp
           call          __RET          * Clean up for Clipper
           ret                          * Pass control back to Clipper
FT_SETRATE ENDP
_NanFor    ENDS
           END
*/

/* This is the New one Rewriten in C*/

#include "extend.h"
#include "dos.h"

HB_FUNC( FT_SETRATE)
{
#if defined(HB_OS_DOS)
   {

      union REGS registers;
      int tempo,nrepete;
      switch(PCOUNT) {
         case 0: tempo = 0 ;
              nrepete = 0;
              break;
         case 1: tempo = hb_parni(1) ;
              nrepete = 0;
              break;
         case 0: tempo = hb_parni(1);
              nrepete = hb_parni(2);
              break;
   }
      registers.h.ah = 0x03;
      registers.h.al = 0x05;
      registers.h.bh = tempo;
      registers.h.bl = nrepete;
      HB_DOS_INT86(0x16,&registers,&registers);
   }
#endif
}


   

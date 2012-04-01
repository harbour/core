/*
 * $Id$
 */

/*
* File......: kspeed.asm
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

#include "hbapi.h"
#if defined( HB_OS_DOS )
#  include "dos.h"
#endif

HB_FUNC( FT_SETRATE )
{
#if defined( HB_OS_DOS )
   {
      union REGS registers;
      int tempo = 0, nrepete = 0;

      switch( hb_pcount() )
      {
      case 0:
           tempo = 0;
           nrepete = 0;
           break;
      case 1:
           tempo = hb_parni( 1 );
           nrepete = 0;
           break;
      case 2:
           tempo = hb_parni( 1 );
           nrepete = hb_parni( 2 );
           break;
      }

      registers.h.ah = 0x03;
      registers.h.al = 0x05;
      registers.h.bh = tempo;
      registers.h.bl = nrepete;
      HB_DOS_INT86( 0x16, &registers, &registers );
   }
#endif
}

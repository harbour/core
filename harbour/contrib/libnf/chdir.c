/*
 * $Id$
 */

/* File......: CHDIR.ASM
* Author....: Ted Means
* CIS ID....: 73067,3332
*
* This is an original work by Ted Means and is placed in the
* public domain.
*
* Modification history:
* ---------------------
*
*     Rev 1.2   15 Aug 1991 23:07:20   GLENN
*  Forest Belt proofread/edited/cleaned up doc
*
*     Rev 1.1   14 Jun 1991 19:54:20   GLENN
*  Minor edit to file header
*
*     Rev 1.0   01 Apr 1991 01:03:10   GLENN
*  Nanforum Toolkit
*
*/


/*  $DOC$
*  $FUNCNAME$
*     FT_CHDIR()
*  $CATEGORY$
*     DOS/BIOS
*  $ONELINER$
*     Change the current directory
*  $SYNTAX$
*     FT_CHDIR( <cDirName> ) -> nResult
*  $ARGUMENTS$
*     <cDirName> is the name of the desired directory.
*  $RETURNS$
*     0  if successful
*     3  if path not found
*     99 if invalid parameters passed
*  $DESCRIPTION$
*     Use this function if you prefer to change the active directory
*     instead of relying on the SET PATH command.
*
*     The source code is written to adhere to Turbo Assembler's IDEAL mode.
*     To use another assembler, you will need to rearrange the PROC and
*     SEGMENT directives, and also the ENDP and ENDS directives (a very
*     minor task).
*  $EXAMPLES$
*     FT_CHDIR( "C:\CLIPPER" )
*     FT_CHDIR( "\" )
*     FT_CHDIR( "..\SOURCE" )
*  $END$
*/

/*This  is the Original FT_CHDIR() code
IDEAL
MODEL HUGE
Public   _HB_FUN_FT_CHDIR

Extrn    _hb_ftdir:Far

Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

Proc     _HB_FUN_FT_CHDIR  Far

         Mov       AH,3Bh                    * DOS service -- change directory
         Push      AX                        * Save on stack
         Call      _hb_ftdir                   * Call generic directory routine
         Add       SP,2                      * Realign stack
         RetF
Endp     _HB_FUN_FT_CHDIR
Ends     _NanFor
End
*/
/* This is the New one Rewriten in C*/
 
#include "extend.h"
#include "dos.h"

HB_FUNC( FT_CHDIR)
{
#if defined(HB_OS_DOS)
   {

    int Status;
    char *path=hb_parc(1);
    union REGS regs;
    struct SREGS sregs;
    segread(&sregs);
    regs.h.ah=0x3B;
    sregs.ds=FP_SEG(path);
    regs.HB_XREGS.dx=FP_OFF(path);
    int86x(0x21,&regs,&regs,&sregs);
    Status=regs.HB_XREGS.ax;
    hb_retl(Status);
   }
#endif
 }

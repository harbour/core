/*
 * $Id$
 */

/*
* File......: rmdir.asm
* Author....: Ted Means
* CIS ID....: 73067,3332
*
* This function is an original work by Ted Means and is placed in the
* public domain.
*
* Modification history:
* ---------------------
*
*     Rev 1.2   15 Aug 1991 23:07:12   GLENN
*  Forest Belt proofread/edited/cleaned up doc
*
*     Rev 1.1   14 Jun 1991 19:54:58   GLENN
*  Minor edit to file header
*
*     Rev 1.0   01 Apr 1991 01:03:52   GLENN
*  Nanforum Toolkit
*
*/


/*  $DOC$
 *  $FUNCNAME$
 *      FT_RMDIR()
 *  $CATEGORY$
 *      DOS/BIOS
 *  $ONELINER$
 *      Delete a subdirectory
 *  $SYNTAX$
 *      FT_RMDIR( <cDirName> ) -> nResult
 *  $ARGUMENTS$
 *      <cDirName> is the name of the directory to delete.
 *  $RETURNS$
 *       0  if successful
 *       3  if Path Not Found
 *       5  if Access Denied (directory not empty)
 *      16  if attempt to delete current directory.
 *      99  if invalid parameters passed
 *  $DESCRIPTION$
 *     This function is useful if you need to remove a subdirectory for
 *     some reason.
 *
 *     The source code is written to adhere to Turbo Assembler's IDEAL mode.
 *     To use another assembler, you will need to rearrange the PROC and
 *     SEGMENT directives, and also the ENDP and ENDS directives (a very
 *     minor task).
 *  $EXAMPLES$
 *     FT_RMDIR( "C:\clipper" )
 *     FT_RMDIR( "\example" )
 *     FT_RMDIR( "..\source" )
 *  $END$
 */
/*This  is the Original FT_RMDIR() code
IDEAL

Public   FT_RMDIR

Extrn    __ftdir:Far

Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

Proc     FT_RMDIR  Far

         Mov       AH,3Ah                    * DOS service--remove directory
         Push      AX                        * Save on stack
         Call      __ftdir                   * Call generic directory routine
         Add       SP,2                      * Realign stack
         Ret
Endp     FT_RMDIR
Ends     _NanFor
End
*/

/* This is the New one Rewriten in C*/

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC(FT_RMDIR)
{
   hb_retl( HB_ISCHAR( 1 ) && hb_fsRmDir( hb_parc(1) ) );
}

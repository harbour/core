/*
 * $Id$
 */

/* File......: chdir.asm
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

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( FT_CHDIR )
{
   hb_retl( HB_ISCHAR( 1 ) && hb_fsChDir( hb_parc( 1 ) ) );
}

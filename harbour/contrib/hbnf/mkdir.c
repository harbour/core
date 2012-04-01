/*
 * $Id$
 */

/*
; File......: mkdir.asm
; Author....: Ted Means
; CIS ID....: 73067,3332
;
; This is an original work by Ted Means and is placed in the
; public domain.
;
; Modification history:
; ---------------------
;
;     Rev 1.2   15 Aug 1991 23:06:58   GLENN
;  Forest Belt proofread/edited/cleaned up doc
;
;     Rev 1.1   14 Jun 1991 19:54:44   GLENN
;  Minor edit to file header
;
;     Rev 1.0   01 Apr 1991 01:03:32   GLENN
;  Nanforum Toolkit
;
;
*/

/*This  is the Original FT_CHDIR() code
IDEAL
MODEL HUGE
Public   _HB_FUN_FT_MKDIR

Extrn    _hb_ftdir:Far

Segment  _NanFor   Word      Public    "CODE"
         Assume    CS:_NanFor

Proc     _HB_FUN_FT_MKDIR  Far

         Mov       AH,39h                    * DOS service--create directory
         Push      AX                        * Save on stack
         Call      _hb_ftdir                   * Call generic directory routine
         Add       SP,2                      * Realign stack
         Ret
Endp     _HB_FUN_FT_MKDIR
Ends     _NanFor
End
*/

/* This is the New one Rewriten in C*/

#include "hbapi.h"
#include "hbapifs.h"

HB_FUNC( FT_MKDIR )
{
   hb_retl( HB_ISCHAR( 1 ) && hb_fsMkDir( hb_parc( 1 ) ) );
}

/*
 * Rewritten in 2012 by Viktor Szakats (vszakats.net/harbour) and kept in the
 * public domain.
 * This is an original work by Glenn Scott and is placed in the public domain.
 *
 * Modification history:
 *
 *    Rev 1.3   15 Aug 1991 23:03:30   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:12   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   14 Jun 1991 17:59:18   GLENN
 * Documentation change (minor), and checked for compatibility with new
 * ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:28   GLENN
 * Nanforum Toolkit
 *
 */

#include "setcurs.ch"

/* NOTE: In Harbour video pages are ignored. */
PROCEDURE ft_SetVcur( nPage, nRow, nCol )

   HB_SYMBOL_UNUSED( nPage )

   SetPos( nRow, nCol )

   RETURN

/* NOTE: In Harbour video pages are ignored. */
FUNCTION ft_GetVCur( nPage )

   LOCAL nTop, nBot

   HB_SYMBOL_UNUSED( nPage )

   SWITCH SetCursor()
   CASE SC_NORMAL   ; nTop := 6 ; nBot := 7 ; EXIT
   CASE SC_INSERT   ; nTop := 4 ; nBot := 7 ; EXIT
   CASE SC_SPECIAL1 ; nTop := 0 ; nBot := 7 ; EXIT
   CASE SC_SPECIAL2 ; nTop := 0 ; nBot := 3 ; EXIT
   OTHERWISE        ; nTop := nBot := 0
   ENDSWITCH

   RETURN { nTop, nBot, Row(), Col() }

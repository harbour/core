/*
 * File......: VIDCUR.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
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

#include "ftint86.ch"

#define VIDEO      16


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETVCUR()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Set the cursor position on a specified video page
 *  $SYNTAX$
 *     FT_SETVCUR( [ <nPage> ], [ <nRow> ], [ <nCol> ] ) -> NIL
 *  $ARGUMENTS$
 *     <nPage> is the video page (defaults to current page, determined
 *             by FT_GETVPG()
 *
 *     <nRow>  is the row coordinate (defaults to 0 )
 *
 *     <nCol>  is the column coordinate (defaults to 0 )
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     FT_SETVCUR() sets the cursor position on a specific video page.
 *     It uses FT_INT86() to invoke interrupt 10h, function 2.
 *
 *     For more information on graphics programming, cursors, and video
 *     pages, refer to Richard Wilton's _Programmer's Guide to PC and
 *     PS/2 Video Systems_ (Microsoft Press).
 *
 *  $EXAMPLES$
 *
 *     // Set the position to row 5, column 10 on video page 1:
 *
 *            FT_SETVCUR( 1, 5, 10 )
 *  $END$
 */


FUNCTION FT_SETVCUR( nPage, nRow, nCol )
  LOCAL aRegs[ INT86_MAX_REGS ]

  nPage := iif( nPage == nil, FT_GETVPG()  , nPage )
  nRow  := iif( nRow  == nil, 0            , nRow  )
  nCol  := iif( nCol  == nil, 0            , nCol  )

  aRegs[ AX ] := MAKEHI(  2    )
  aRegs[ BX ] := MAKEHI( nPage )
  aRegs[ DX ] := MAKEHI( nRow  ) + nCol

  FT_INT86( VIDEO, aRegs )

RETURN ( NIL )



/*  $DOC$
 *  $FUNCNAME$
 *     FT_GETVCUR()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Return info about the cursor on a specified video page
 *  $SYNTAX$
 *     FT_GETVCUR( [ <nPage> ] ) -> <aCurInfo>
 *  $ARGUMENTS$
 *    <nPage> is the video page to get the cursor information for.
 *    Defaults to the current page, as returned by FT_GETVPG().
 *  $RETURNS$
 *     A four-element array (<aCurInfo>), set up as follows:
 *
 *     aCurInfo[1] = Top line of cursor
 *     aCurInfo[2] = Bottom line of cursor
 *     aCurInfo[3] = Character row
 *     aCurInfo[4] = Character column
 *
 *  $DESCRIPTION$
 *
 *    FT_GETVCUR() uses FT_INT86() to invoke interrupt 10h, function
 *    3, to return the character cursor location for the specified
 *    video page.
 *
 *    The top line and bottom line of cursor are set depending on
 *    the current cursor mode, and are only meaningful in alphanumeric
 *    video modes.
 *
 *    For more information on graphics programming, cursors, and
 *    cursor modes, refer to Richard Wilton's _Programmer's Guide to
 *    PC and PS/2 Video Systems_ (Microsoft Press).
 *
 *  $EXAMPLES$
 *
 *     aCurInfo := getVCur( 1 )    // Get info on cursor pos in page 1
 *     QOut("Row: " + str( aCurInfo[3] ) + "  Col: " + str( aCurInfo[4] ) )
 *
 *
 *  $END$
 */


FUNCTION FT_GETVCUR( nPage )
  LOCAL aRegs[ INT86_MAX_REGS ]

  nPage := iif( nPage == nil, FT_GETVPG(), nPage )
  aRegs[ AX ] := MAKEHI( 3     )
  aRegs[ BX ] := MAKEHI( nPage )
  FT_INT86( VIDEO, aRegs )

RETURN ( { HIGHBYTE( aRegs[CX] ), LOWBYTE( aRegs[CX] ), HIGHBYTE( aRegs[DX] ), LOWBYTE( aRegs[DX] ) } )


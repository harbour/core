/*
 * File......: PAGE.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:05:18   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:52:36   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   12 Jun 1991 02:29:14   GLENN
 * Documentation mods and check for ft_int86() compatibility
 *
 *    Rev 1.0   01 Apr 1991 01:01:58   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define VIDEO      16

/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETVPG()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Set the current video page
 *  $SYNTAX$
 *     FT_SETVPG( <nPage> ) -> NIL
 *  $ARGUMENTS$
 *     <nMode> is a valid video page.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     Selects the video page.
 *
 *     For more information on graphics programming and video pages,
 *     consult a reference such as "Programmer's Guide to PC and PS/2
 *     Video Systems" (Microsoft Press).
 *  $EXAMPLES$
 *     // The following sets the current video page to 1
 *
 *     FT_SETVPG( 1 )
 *  $SEEALSO$
 *     FT_GETVPG()
 *  $END$
 */


FUNCTION FT_SETVPG( nPage )
/*
  LOCAL aRegs[ INT86_MAX_REGS ]

  aRegs[ AX ] = MAKEHI( 5 ) + nPage
  FT_INT86( VIDEO, aRegs )
  */
  _ft_setvpg(nPage)

  RETURN( NIL )



/*  $DOC$
 *  $FUNCNAME$
 *     FT_GETVPG()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Get the currently selected video page
 *  $SYNTAX$
 *     FT_GETVPG() -> <nPage>
 *  $ARGUMENTS$
 *     None.
 *  $RETURNS$
 *     The video page, as a numeric.
 *  $DESCRIPTION$
 *     Get the currently selected video page
 *
 *     For more information on graphics programming and video pages,
 *     consult a reference such as _Programmer's Guide to PC and PS/2
 *     Video Systems_ (Microsoft Press).
 *
 *  $EXAMPLES$
 *     nPage := FT_GETVPG()
 *  $SEEALSO$
 *     FT_SETVPG()
 *  $END$
 */



FUNCTION FT_GETVPG()
/*
  LOCAL aRegs[ INT86_MAX_REGS ]

  aRegs[ AX ] := MAKEHI( 15 )
  FT_INT86( VIDEO, aRegs )

  RETURN ( HIGHBYTE( aRegs[ BX ] ) ) */
 Return _ft_getvpg()



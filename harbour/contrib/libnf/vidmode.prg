/*
 * File......: VIDMODE.PRG
 * Author....: Glenn Scott
 * CIS ID....: 71620,1521
 *
 * This is an original work by Glenn Scott and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.3   15 Aug 1991 23:06:12   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.2   14 Jun 1991 19:53:14   GLENN
 * Minor edit to file header
 *
 *    Rev 1.1   14 Jun 1991 18:00:42   GLENN
 * Documentation change (minor), and checked for compatibility with new
 * ft_int86().
 *
 *    Rev 1.0   01 Apr 1991 01:02:30   GLENN
 * Nanforum Toolkit
 *
 */

#include "ftint86.ch"

#define VIDEO      16
#define GETMODE    15


#ifdef FT_TEST
  FUNCTION MAIN( cMode )

     FT_SETMODE( val( cMode ) )
     QOut( "Video mode is: " + str( FT_GETMODE() ) )
     return ( nil )

#endif


/*  $DOC$
 *  $FUNCNAME$
 *     FT_SETMODE()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Set the video mode
 *  $SYNTAX$
 *     FT_SETMODE( <nMode> ) -> NIL
 *  $ARGUMENTS$
 *     <nMode> is one of the DOS video modes.
 *  $RETURNS$
 *     NIL
 *  $DESCRIPTION$
 *     Use this function to put your display adapter into a video mode.
 *     Uses DOS interrupt 10h to set the mode.  For a table of modes
 *     available on various graphics adapters, refer to a book such
 *     as Wilton's "Programmer's Guide to PC & PS/2 Video Systems"
 *     (Microsoft Press)
 *  $EXAMPLES$
 *        FUNCTION Main( cMode )
 *
 *          FT_SETMODE( VAL( cMode ) )
 *          QOUT( "Video mode is: " + STR( FT_GETMODE() ) )
 *          RETURN ( NIL )
 *  $SEEALSO$
 *     FT_ADAPTER()
 *  $END$
 */


FUNCTION FT_SETMODE( nMode )
/*
  LOCAL aRegs[ INT86_MAX_REGS ]

  aRegs[ AX ] = nMode
  FT_INT86( VIDEO, aRegs )
*/
_ft_setmode(nMode)
  RETURN( NIL )



/*  $DOC$
 *  $FUNCNAME$
 *     FT_GETMODE()
 *  $CATEGORY$
 *     Video
 *  $ONELINER$
 *     Get the video mode
 *  $SYNTAX$
 *     FT_GETMODE() -> nVMode
 *  $ARGUMENTS$
 *     None.
 *  $RETURNS$
 *     The video mode, as a numeric.
 *  $DESCRIPTION$
 *     Use this function to find out what mode your display adapter is in.
 *     Uses DOS interrupt 10h to get the mode.  For a table of modes
 *     available on various graphics adapters, refer to a book such
 *     as Wilton's "Programmer's Guide to PC & PS/2 Video Systems"
 *     (Microsoft Press)
 *  $EXAMPLES$
 *        function main( cMode )
 *
 *          FT_SETMODE( val( cMode ) )
 *          QOut( "Video mode is: " + str( FT_GETMODE() ) )
 *          return ( nil )
 *
 *  $END$
 */



FUNCTION FT_GETMODE()
/*
  LOCAL aRegs[INT86_MAX_REGS]

  aRegs[ AX ] := MAKEHI( GETMODE )
  FT_INT86( VIDEO, aRegs )

  RETURN ( LOWBYTE( aRegs[ AX ] ) )
*/
 RETURN _ft_getmode()  


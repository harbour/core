/*
 * $Id$
 */

/*
 * File......: n2color.c
 * Author....: David Richardson
 * CIS ID....: 72271,53
 *
 * This function is an original work by David Richardson and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 2.1   22 Apr 2004 15:47:00   DGH
 * Made definitions of _ftI2Color and _ftGetColorStr static to match
 * their forward declarations. Commented out the extremely useless
 * #if defined(HB_OS_DOS) line and corresponding #endif line. (There
 * is nothing that is even remotely DOS-specific in the code!) And
 * converted tabs to spaces.
 *    Rev 2.0   03 Mar 1997 03:05:01   JO  / Phil Barnett
 *              commented out : if ( iColor > 15 ) in _ftI2Color()
 *    Rev 1.0   01 Jan 1995 03:01:00   TED
 * Initial release
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_N2COLOR()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Returns the string complement of a Clipper color number
 *  $SYNTAX$
 *     FT_COLOR2N( <nColor> ) -> cColor
 *  $ARGUMENTS$
 *     <nColor> a number representing a Clipper color
 *  $RETURNS$
 *     The string complement of a number representing a Clipper or a
 *     null string if the parameter is invalid
 *  $DESCRIPTION$
 *     This function is useful for converting a number to a Clipper color
 *     string.
 *  $EXAMPLES$
 *     cColor := FT_COLOR2N( 239 )         // returns "*+w/gr"
 *  $SEEALSO$
 *     FT_N2COLOR()
 *  $END$
 */

#include "hbapi.h"
#include "hbapigt.h"

HB_FUNC( FT_N2COLOR )
{
   int iColor = HB_ISNUM( 1 ) ? hb_parni( 1 ) : -1;

   if( iColor >= 0x00 && iColor <= 0xff )
   {
      char szColorString[ 10 ];
      hb_gtColorsToString( &iColor, 1, szColorString, 10 );
      hb_retc( szColorString );
   }
   else
      hb_retc( NULL );
}

/*
 * $Id$
 */

/*
 * File......: N2COLOR.C
 * Author....: David Richardson
 * CIS ID....: 72271,53
 *
 * This function is an original work by David Richardson and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
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

static void _ftI2Color( int iColor, char * cColor );
static int _ftGetColorStr( int iColor, char * cColor );

HB_FUNC(FT_N2COLOR )
{
#if defined(HB_OS_DOS)
   {

   char * cColor = "       ";

   // make sure parameter is a numeric type

   if ( ISNUM(1))
      _ftI2Color( hb_parni( 1 ), cColor );
	else
		cColor = NULL;

    hb_retc( cColor );

   return;
   }
#endif
}



// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
// Function  :  _ftI2Color
// Purpose   :  Converts a color int to an Xbase color string
// Parameters:  iColor  -  the color number
//              *cColor -  pointer to the color string
// Returns   :  void (string is modified directly)
// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

void _ftI2Color( int iColor, char * cColor )
{
   unsigned int iBack = 0, iFore = 0, i = 0;

   // check for blink attribute

   if ( iColor > 127 )
   {
      cColor[ i++ ] = '*';

      iColor %= 128;
   }

   // check for background and foreground colors

// if ( iColor > 15 )
// {
      iFore = iColor % 16;

      iBack = ( iColor - iFore ) / 16;
// }

    // check for intensity attrib

   if ( iFore > 7 )
   {
      cColor[ i++ ] = '+';

      iFore %= 8;
   }

   // get forground color

   i += _ftGetColorStr( iFore, ( cColor + i ) );

   // add the seperator

   cColor[ i++ ] = '/';

   // get background color

   i += _ftGetColorStr( iBack, ( cColor + i ) );

   // null terminate the color string

   cColor[ i ] = 0;

   return ;
}



// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
// Function  :  _ftGetColorStr
// Purpose   :  Returns the corresponding Xbase color for passed number
// Parameters:  iColor  -  a color number
//              *cColor -  pointer to the color string
// Returns   :  length of added color string
// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

int _ftGetColorStr( int iColor, char * cColor )
{
    int iLen = 0;

    switch ( iColor )
    {
       case  0 : cColor[iLen++] = 'n';  break;
       case  1 : cColor[iLen++] = 'b';  break;
       case  2 : cColor[iLen++] = 'g';  break;
       case  3 : cColor[iLen++] = 'b';
                 cColor[iLen++] = 'g';  break;
       case  4 : cColor[iLen++] = 'r';  break;
       case  5 : cColor[iLen++] = 'r';
                 cColor[iLen++] = 'b';  break;
       case  6 : cColor[iLen++] = 'g';
                 cColor[iLen++] = 'r';  break;
       case  7 : cColor[iLen++] = 'w';
     }

    return iLen;

}

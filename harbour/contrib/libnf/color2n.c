/*
 * $Id$
 */

/*
 * File......: COLOR2N.C
 * Author....: David Richardson
 * CIS ID....: 72271,53
 *
 * This function is an original work by David Richardson and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.0   01 Jan 1995 03:01:00   TED
 * Initial release
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_COLOR2N()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Returns the numeric complement of a Clipper color string
 *  $SYNTAX$
 *     FT_COLOR2N( <cColor> ) -> nValue
 *  $ARGUMENTS$
 *     <cColor> is a Clipper color string
 *  $RETURNS$
 *     The numeric complement of a color string or 0 if passed color
 *     is invalid.
 *  $DESCRIPTION$
 *     This function is useful when calling other functions that expect
 *     a numeric color parameter.  It is often more convenient to pass
 *     a converted color string than having to calculate or look up the
 *     corresponding number.
 *  $EXAMPLES$
 *     nColor := FT_COLOR2N( "gr+/b" )         // returns 30
 *
 *     FT_SETATTR( 0, 0, 10, 10, nColor )
 *  $SEEALSO$
 *     FT_N2COLOR()
 *  $END$
 */

#include "hbapi.h"

static int _ftColor2I( char * cColor );
static int _ftGetColorNum( char * cColor );
static char * _ftStripIt( char * cColor );

HB_FUNC(FT_COLOR2N)
{
#if defined(HB_OS_DOS) || defined(HB_OS_WIN_32)
   {

       int iRet = 0;

       // make sure parameter is a char type and that it is 8 chars or less

       if ( ISCHAR( 1 )  && hb_parclen( 1 ) < 8 )
          iRet = _ftColor2I( hb_parc( 1 ) );

       hb_retni( iRet );

       return;
   }
#endif
}


// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
// Function  :  _ftColor2I
// Purpose   :  Converts an Xbase color string to an int
// Parameters:  cColor  -  a pointer to the color string
// Returns   :  int complement of color string, or 0 if string is invalid
// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

static int _ftColor2I( char * cColor )
{
    char * cFore = "         ", * cBack = "      ";
    unsigned int iBlink = 0, iIntense = 0, iBack = 0, i = 0;

    // copy the Clipper string to buffer, check for attributes, and
    // make lower case

    while ( ( cFore[ i ] = cColor[ i ] ) != NULL )
    {
       // check for a blink attrib

       if ( cFore[ i ] == '*' && iBlink == 0 ) iBlink = 128;

       // check for an intensity attrib

       if ( cFore[ i ] == '+' && iIntense == 0 ) iIntense = 8;

        // make sure all chars are lower case

       if ( 91 > cFore[ i ] && cFore[ i ] > 64 ) cFore[ i ] += 32;

       i++;
    }

    // check for the background color

    while ( cColor[ iBack++ ] != '/' && cColor[ iBack ] != NULL );

    if ( cColor[--iBack ] == '/' )
    {
       cBack = cFore + iBack + 1;
       cFore[ iBack ] = NULL;
    }

    // calculate and return the value

    return ( iIntense + iBlink + _ftGetColorNum( _ftStripIt( cFore ) ) +
                          ( 16 * _ftGetColorNum( _ftStripIt( cBack ) ) ) );

}


// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
// Function  :  _ftGetColorNum
// Purpose   :  Returns the corresponding number for an Xbase color
// Parameters:  cColor  -  a pointer to the color string
// Returns   :  int complement of a single color
// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

static int _ftGetColorNum( char * cColor )
{
   unsigned * iColor = ( unsigned * ) cColor;

   if ( cColor[ 1 ] == 0 ) switch ( cColor[ 0 ] )
   {
         case 'n' : *iColor = 0; break;
         case 'b' : *iColor = 1; break;
         case 'g' : *iColor = 2; break;
         case 'r' : *iColor = 4; break;
         case 'w' : *iColor = 7; break;
   }
   else
   {
      if ( ( cColor[ 0 ] == 'b' ) && cColor[ 1 ] == 'g' ) *iColor = 3;
      if ( ( cColor[ 0 ] == 'r' ) && cColor[ 1 ] == 'b' ) *iColor = 5;
      if ( ( cColor[ 0 ] == 'g' ) && cColor[ 1 ] == 'r' ) *iColor = 6;
   }

   return *iColor;
}



// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
// Function  :  _ftStripIt
// Purpose   :  Removes the intensity/blink chars from the passed string
// Parameters:  cColor  -  a pointer to the color string
// Returns   :  a pointer to the modified color string
// 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

static char * _ftStripIt( char * cColor )
{
   unsigned i = 0;

   // move past any leading markers

   while ( *cColor == '+' || *cColor == '*' ) cColor++;

   // truncate any trailing markers

   while ( cColor[ i ] && cColor[ i ] != '+' && cColor[ i ] != '*' ) i++;

   // null terminate the string

   cColor[ i ] = 0;

   return cColor;
}

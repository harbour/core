/*
 * $Id$
 */

/*
 * File......: PROPER.C
 * Author....: Robert DiFalco and Glenn Scott
 * CIS ID....: 71610,1705
 *
 * This is an original work by Glenn Scott and Robert DiFalco
 * and is placed in the public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.4   01 Jan 1995 03:01:00   TED
 * Ted Means made a couple of minor mods to eliminate some (mostly
 * benign) compiler warnings.
 *
 *    Rev 1.3   28 Sep 1992 00:54:58   GLENN
 * Don Caton fixed the function to conform to extend system rules.
 *
 *    Rev 1.2   15 Aug 1991 23:08:22   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:53:50   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:02:56   GLENN
 * Nanforum Toolkit
 *
 *
 */


/*  $DOC$
 *  $FUNCNAME$
 *     FT_PROPER()
 *  $CATEGORY$
 *     String
 *  $ONELINER$
 *     Convert a string to proper-name case
 *  $SYNTAX$
 *     FT_PROPER( <cString> ) -> cProperName
 *  $ARGUMENTS$
 *     <cString> is the string to be converted.
 *  $RETURNS$
 *     A string of the same length as <cString>, only converted to
 *     proper name case (upper/lower case).
 *  $DESCRIPTION$
 *     FT_PROPER() uses a brute-force algorithm to convert a string
 *     to propername case.  First, it capitalizes the first letter of
 *     all words starting after a blank, dash, or apostrophe.  This
 *     catches most names, including special cases such as names
 *     beginning with O' (O'Malley, O'Reilly) and hyphenated names
 *     (such as Susan Chia-Mei Lo).
 *
 *     Next, it does a specific adjustment for words beginning in "Mc"
 *     It finds the first 'Mc' and capitalizes the next character after
 *     it.  It does this for all occurrences of Mc.
 *
 *     The original FT_PROPER() was written in Clipper by Glenn Scott
 *     and Mark Zechiel; it was re-written in C (and thus, optimized
 *     and enhanced) by Robert DiFalco.
 *  $EXAMPLES$
 *       FUNCTION main( cStr )
 *         OutStd( FT_PROPER( cStr ) + chr(13) + chr(10) )
 *       RETURN ( nil )
 *  $END$
 */


#include "hbapi.h"

static int _ftIsAlpha( char );
static char _ftToLower( char );
static char _ftToUpper( char );
static int _ftIsUpper( char );
static int _ftIsLower( char );

HB_FUNC(FT_PROPER )
{
#if defined(HB_OS_DOS) || defined(HB_OS_WIN_32)
   {

  int  iLen   =  hb_parclen(1);
  char *cStr;

  int i, fCap = TRUE, iPos = 0;

  hb_storc( NULL, 1 );
  cStr = hb_parc(1);

  for( i = 0; i < iLen + 1; i++ ) {
     if( _ftIsAlpha( cStr[i] ) == TRUE )  {
        if( fCap == TRUE )
           cStr[i] = _ftToUpper( cStr[i] );
        else cStr[i] = _ftToLower( cStr[i] );
        }
     fCap = ( cStr[i] == ' ' || cStr[i] == '-' || cStr[i] == 0x27 );
  }

  // Find "Mc"
  for( i = 0; i <= iLen; i++ )
     if( cStr[i] == 'M' && cStr[i+1] == 'c' ) {
        cStr[i+2] = _ftToUpper( cStr[i+2] );
     }

  /* // If "Mc" was found, Cap next letter if Alpha
  if( iPos > 1 )
     if( iPos < iLen )
        if( _ftIsUpper( cStr[iPos] ) == FALSE )
           cStr[iPos] = _ftToUpper( cStr[iPos] );
  */
  hb_retc( cStr );
  return;
   }
#endif
}



static int _ftIsAlpha( char c )
{
  return( _ftIsUpper(c) || _ftIsLower(c));
}

static char _ftToLower( char c )
{
  return(c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c);
}


static char _ftToUpper( char c )
{
  return(c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c);
}

static int _ftIsUpper( char c )
{
  return(c >= 'A' && c <= 'Z');
}


static int _ftIsLower( char c )
{
  return(c >= 'a' && c <= 'z');
}

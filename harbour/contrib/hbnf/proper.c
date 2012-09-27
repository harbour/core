/*
 * $Id$
 */

/*
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

#include "hbapi.h"

static char _ftToLower( char c )
{
   return c >= 'A' && c <= 'Z' ? c - 'A' + 'a' : c;
}

static char _ftToUpper( char c )
{
   return c >= 'a' && c <= 'z' ? c - 'a' + 'A' : c;
}

static HB_BOOL _ftIsUpper( char c )
{
   return c >= 'A' && c <= 'Z';
}

static HB_BOOL _ftIsLower( char c )
{
   return c >= 'a' && c <= 'z';
}

static HB_BOOL _ftIsAlpha( char c )
{
   return _ftIsUpper( c ) || _ftIsLower( c );
}

HB_FUNC( FT_PROPER )
{
   HB_ISIZ        iLen  = hb_parclen( 1 );
   const char *   cStr;
   char *         cDst  = NULL;
   HB_ISIZ        i; /*, iPos = 0; */
   HB_BOOL        fCap  = HB_TRUE;

   hb_storc( NULL, 1 );
   cStr = hb_parc( 1 );

   for( i = 0; i < iLen; i++ )
   {
      if( _ftIsAlpha( cStr[ i ] ) )
      {
         if( ! cDst )
         {
            cDst = ( char * ) hb_xgrab( iLen + 1 );
            memcpy( cDst, cStr, iLen + 1 );
         }
         if( fCap )
            cDst[ i ] = _ftToUpper( cDst[ i ] );
         else
            cDst[ i ] = _ftToLower( cDst[ i ] );
      }
      fCap = ( cStr[ i ] == ' ' || cStr[ i ] == '-' || cStr[ i ] == 0x27 );
   }

   /* Find "Mc" */
   if( cDst )
   {
      for( i = 0; i < iLen - 2; i++ )
         if( cStr[ i ] == 'M' && cStr[ i + 1 ] == 'c' )
         {
            cDst[ i + 2 ] = _ftToUpper( cDst[ i + 2 ] );
         }
   }
   /* // If "Mc" was found, Cap next letter if Alpha
      if( iPos > 1 )
      if( iPos < iLen )
         if( ! _ftIsUpper( cStr[iPos] ) )
            cStr[iPos] = _ftToUpper( cStr[iPos] );
    */
   if( cDst )
      hb_retclen_buffer( cDst, iLen );
   else
      hb_retclen( cStr, iLen );
}

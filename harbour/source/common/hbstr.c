/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour common string functions (accessed from standalone utilities and the RTL)
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    hb_stricmp()
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include <ctype.h> /* Needed by hb_strupr() */

#include "hbapi.h"
#include "hbmath.h"

ULONG hb_strAt( const char * szSub, ULONG ulSubLen, const char * szText, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strAt(%s, %lu, %s, %lu)", szSub, ulSubLen, szText, ulLen));

   if( ulSubLen > 0 && ulLen >= ulSubLen )
   {
      ULONG ulPos = 0;
      ULONG ulSubPos = 0;

      while( ulPos < ulLen && ulSubPos < ulSubLen )
      {
         if( szText[ ulPos ] == szSub[ ulSubPos ] )
         {
            ulSubPos++;
            ulPos++;
         }
         else if( ulSubPos )
         {
            /* Go back to the first character after the first match,
               or else tests like "22345" $ "012223456789" will fail. */
            ulPos -= ( ulSubPos - 1 );
            ulSubPos = 0;
         }
         else
            ulPos++;
      }

      return ( ulSubPos < ulSubLen ) ? 0 : ( ulPos - ulSubLen + 1 );
   }
   else
      return 0;
}

char * hb_strupr( char * pszText )
{
   char * pszPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_strupr(%s)", pszText));

   for( pszPos = pszText; *pszPos; pszPos++ )
      *pszPos = toupper( *pszPos );

   return pszText;
}

char * hb_strdup( const char * pszText )
{
   char * pszDup;
   int iLen = strlen( pszText ) + 1;

   HB_TRACE(HB_TR_DEBUG, ("hb_strdup(%s, %i)", pszText, iLen));

   pszDup = ( char * ) hb_xgrab( iLen );
   memcpy( pszDup, pszText, iLen );

   return pszDup;
}

int hb_stricmp( const char * s1, const char * s2 )
{
   int rc = 0;
   ULONG l1;
   ULONG l2;
   ULONG count;

   HB_TRACE(HB_TR_DEBUG, ("hb_stricmp(%s, %s)", s1, s2));

   l1 = strlen( s1 );
   l2 = strlen( s2 );
   count = ( l1 < l2 ? l1 : l2 );

   while( rc == 0 && count > 0 )
   {
      char c1 = toupper( *s1 );
      char c2 = toupper( *s2 );

      s1++;
      s2++;

      if( c1 != c2 )
         rc = ( c1 < c2 ? -1 : 1 );

      count--;
   }

   if( rc == 0 && l1 != l2 )
      rc = ( l1 < l2 ? -1 : 1 );

   return rc;
}

/*
 * This function copies szText to destination buffer.
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
HB_EXPORT char * hb_strncpy( char * pDest, const char * pSource, ULONG ulLen )
{
   char *pBuf = pDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_strncpy(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';

   while( ulLen && ( *pDest++ = *pSource++ ) != '\0' )
   {
      ulLen--;
   }

   while (ulLen--)
   {
      *pDest++ = '\0';
   }

   return pBuf;
}

/*
 * This function copies szText to destination buffer.
 * NOTE: Unlike the documentation for strncat, this routine will always append
 *       a null and the ulLen param is pDest size not pSource limit
 */
HB_EXPORT char * hb_strncat( char * pDest, const char * pSource, ULONG ulLen )
{
   char *pBuf = pDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_strncpy(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';

   while( ulLen && *pDest )
   {
      pDest++;
      ulLen--;
   }

   while( ulLen && ( *pDest++ = *pSource++ ) != '\0' )
   {
      ulLen--;
   }

/* if someone will need this then please uncomment the cleaning the rest of buffer. */
/*
   while (ulLen--)
   {
      *pDest++ = '\0';
   }
*/
   return pBuf;
}

/* This function copies and converts szText to upper case.
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
HB_EXPORT char * hb_strncpyUpper( char * pDest, const char * pSource, ULONG ulLen )
{
   char *pBuf = pDest;

   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyUpper(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';

   /* some compilers impliment toupper as a macro, and this has side effects! */
   /* *pDest++ = toupper( *pSource++ ); */
   while( ulLen && (*pDest++ = toupper( *pSource )) != '\0' )
   {
      ulLen--;
      pSource++;
   }

   while (ulLen--)
   {
      *pDest++ = '\0';
   }

   return pBuf;
}

/* This function copies and converts szText to upper case AND Trims it
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
HB_EXPORT char * hb_strncpyUpperTrim( char * pDest, const char * pSource, ULONG ulLen )
{
   char *pBuf = pDest;
   ULONG ulSLen = strlen( pSource );

   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyUpperTrim(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';

   while( ulSLen && pSource[ ulSLen - 1 ] == ' ')
   {
      ulSLen--;
   }

   /* some compilers impliment toupper as a macro, and this has side effects! */
   /* *pDest++ = toupper( *pSource++ ); */
   while( ulLen && ulSLen && (*pDest++ = toupper( *pSource )) != '\0' )
   {
      ulSLen--;
      ulLen--;
      pSource++;
   }

   while (ulLen--)
   {
      *pDest++ = '\0';
   }

   return pBuf;
}

/*
 * This function copies trimed szText to destination buffer.
 * NOTE: Unlike the documentation for strncpy, this routine will always append
 *       a null
 */
HB_EXPORT char * hb_strncpyTrim( char * pDest, const char * pSource, ULONG ulLen )
{
   char *pBuf = pDest;
   LONG lSLen = strlen( pSource );

   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyTrim(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';

   while( lSLen && pSource[ lSLen - 1 ] == ' ')
   {
      lSLen--;
   }

   /* some compilers impliment toupper as a macro, and this has side effects! */
   /* *pDest++ = toupper( *pSource++ ); */
   while( ulLen && lSLen && ( *pDest++ = *pSource++ ) != '\0' )
   {
      lSLen--;
      ulLen--;
   }

   while (ulLen--)
   {
      *pDest++ = '\0';
   }

   return pBuf;
}

static double hb_numPow10( int nPrecision )
{
   static double s_dPow10[16] = {                1.0,   /*  0 */
                                                10.0,   /*  1 */
                                               100.0,   /*  2 */
                                              1000.0,   /*  3 */
                                             10000.0,   /*  4 */
                                            100000.0,   /*  5 */
                                           1000000.0,   /*  6 */
                                          10000000.0,   /*  7 */
                                         100000000.0,   /*  8 */
                                        1000000000.0,   /*  9 */
                                       10000000000.0,   /* 10 */
                                      100000000000.0,   /* 11 */
                                     1000000000000.0,   /* 12 */
                                    10000000000000.0,   /* 13 */
                                   100000000000000.0,   /* 14 */
                                  1000000000000000.0 }; /* 15 */
   if ( nPrecision < 16 )
   {
      if ( nPrecision >= 0 )
      {
         return s_dPow10[ nPrecision ];
      }
      else if ( nPrecision > -16 )
      {
         return 1.0 / s_dPow10[ -nPrecision ];
      }
   }

   return pow(10.0, (double) nPrecision);
}

double hb_numRound( double dNum, int iDec )
{
   static const double doBase = 10.0f;
   double doComplete5, doComplete5i, dPow;

   HB_TRACE(HB_TR_DEBUG, ("hb_numRound(%lf, %d)", dNum, iDec));

   if( dNum == 0.0 )
      return 0.0;

   if ( iDec < 0 )
   {
      dPow = hb_numPow10( -iDec );
      doComplete5 = dNum / dPow * doBase;
   }
   else
   {
      dPow = hb_numPow10( iDec );
      doComplete5 = dNum * dPow * doBase;
   }

   if( dNum < 0.0f )
      doComplete5 -= 5.0f;
   else
      doComplete5 += 5.0f;

   doComplete5 /= doBase;

#if defined( HB_DBLFL_PREC_FACTOR ) && !defined( HB_C52_STRICT )
   /* similar operation is done by Cl5.3
      it's a hack to force rounding FL values UP */
   doComplete5 *= HB_DBLFL_PREC_FACTOR;
#endif

   modf( doComplete5, &doComplete5i );

   if ( iDec < 0 )
      return doComplete5i * dPow;
   else
      return doComplete5i / dPow;
}

double hb_numInt( double dNum )
{
   double dInt;

#if defined( HB_DBLFL_PREC_FACTOR ) && !defined( HB_C52_STRICT )
   /* Similar hack as in round to make this functions compatible */
   dNum *= HB_DBLFL_PREC_FACTOR;
#endif
   modf( dNum, &dInt );

   return dInt;
}

static BOOL hb_str2number( BOOL fPCode, const char* szNum, ULONG ulLen, HB_LONG * lVal, double * dVal, int * piDec, int * piWidth )
{
   BOOL fDbl = FALSE, fDec = FALSE, fNeg = FALSE, fHex = FALSE;
   ULONG ulPos = 0;
   int c, iWidth = 0, iDec = 0, iDecR = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_str2number(%d, %p, %ul %p, %p, %p, %p)", (int) fPCode, szNum, ulLen, lVal, dVal, piDec, piWidth ));

   while ( ulPos < ulLen && isspace( ( BYTE ) szNum[ulPos] ) )
      ulPos++;

   if ( ulPos >= ulLen )
   {
      fNeg = FALSE;
   }
   else if ( szNum[ulPos] == '-' )
   {
      fNeg = TRUE;
      ulPos++;
   }
   else
   {
      fNeg = FALSE;
      if ( szNum[ulPos] == '+' )
         ulPos++;
   }

   *lVal = 0;

   /* Hex Number */
   if( fPCode && ulPos + 1 < ulLen && szNum[ulPos] == '0' &&
       ( szNum[ulPos+1] == 'X' || szNum[ulPos+1] == 'x' ) )
   {
      ulPos += 2;
      iWidth = HB_DEFAULT_WIDTH;
      fHex = TRUE;
      for ( ; ulPos < ulLen; ulPos++ )
      {
         c = szNum[ulPos];
         if ( c >= '0' && c <= '9' )
            c -= '0';
         else if ( c >= 'A' && c <= 'F' )
            c -= 'A' - 10;
         else if ( c >= 'a' && c <= 'f' )
            c -= 'a' - 10;
         else
            break;
         *lVal = ( *lVal << 4 ) + c;
      }
   }
   else
   {
      HB_LONG lLimV;
      int iLimC;

      lLimV = HB_LONG_MAX / 10;
      iLimC = HB_LONG_MAX % 10;

      iWidth = ulPos;

      for ( ; ulPos < ulLen; ulPos++ )
      {
         c = szNum[ulPos];
         if ( c >= '0' && c <= '9' )
         {
            if ( fDbl )
            {
               *dVal = *dVal * 10.0 + ( c - '0' );
            }
            else if ( *lVal < lLimV || ( *lVal <= lLimV && ( c - '0' ) <= iLimC ) )
            {
               *lVal = *lVal * 10 + ( c - '0' );
            }
            else
            {
               *dVal = (double) *lVal * 10.0 + ( c - '0' );
               fDbl = TRUE;
            }
            if ( fDec )
               iDec++;
            else
               iWidth++;
         }
         else if ( c == '.' && !fDec )
         {
            fDec = TRUE;
         }
         else
         {
            while ( !fDec && ulPos < ulLen )
            {
               if ( szNum[ulPos++] == '.' )
                  fDec = TRUE;
               else
                  iWidth++;
            }
            if ( fDec )
               iDecR = ulLen - ulPos;
            break;
         }
      }
   }

   if ( fNeg )
   {
      if ( fDbl )
         *dVal = -*dVal;
      else
         *lVal = -*lVal;
   }
   if ( !fDbl && (
#if defined( PCODE_LONG_LIM )
        ( fPCode && !fHex && !PCODE_LONG_LIM( *lVal ) ) ||
#endif
        fDec ) )
   {
      *dVal = (double) *lVal;
      fDbl = TRUE;
   }
   if ( fDbl && iDec )
      *dVal /= hb_numPow10( iDec );

   if ( piDec )
      *piDec = iDec + iDecR;
   if ( piWidth )
   {
      if ( fHex )
         *piWidth = iWidth;
      else
      {
         int iSize = fDbl ? HB_DBL_LENGTH( *dVal ) : HB_LONG_LENGTH( *lVal );

         if ( fPCode )
         {
            if ( iWidth < 10 || fNeg )
               *piWidth = iSize;
            else
               *piWidth = iWidth + ( iDec == 0 ? 1 : 0 );
         }
         else
         {
            if ( iSize > 10 || iWidth > 10 )
               *piWidth = iSize;
            else if ( iDec + iDecR == 0 )
               *piWidth = ( int ) ulLen;
            else if ( iWidth == 0 )
               *piWidth = 1;
            else if ( fNeg && iWidth == 1 && *dVal != 0 )
               *piWidth = 2;
            else
               *piWidth = iWidth;
         }
      }
   }

   return fDbl;
}

BOOL HB_EXPORT hb_compStrToNum( const char* szNum, HB_LONG * plVal, double * pdVal, int * piDec, int * piWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_compStrToNum( %s, %p, %p, %p, %p)", szNum, plVal, pdVal, piDec, piWidth ));
   return hb_str2number( TRUE, szNum, strlen( szNum ), plVal, pdVal, piDec, piWidth );
}

BOOL HB_EXPORT hb_valStrnToNum( const char* szNum, ULONG ulLen, HB_LONG * plVal, double * pdVal, int * piDec, int * piWidth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_valStrToNum( %s, %l, %p, %p, %p, %p)", szNum, ulLen, plVal, pdVal, piDec, piWidth ));
   return hb_str2number( FALSE, szNum, ulLen, plVal, pdVal, piDec, piWidth );
}

BOOL HB_EXPORT hb_strToNum( const char* szNum, HB_LONG * plVal, double * pdVal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strToNum(%s, %p, %p)", szNum, plVal, pdVal ));
   return hb_str2number( FALSE, szNum, strlen( szNum ), plVal, pdVal, NULL, NULL );
}

BOOL HB_EXPORT hb_strnToNum( const char* szNum, ULONG ulLen, HB_LONG * plVal, double * pdVal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strToNum(%s, %l, %p, %p)", szNum, ulLen, plVal, pdVal ));
   return hb_str2number( FALSE, szNum, ulLen, plVal, pdVal, NULL, NULL );
}

/* returns the numeric value of a character string representation of a number */
double hb_strVal( const char * szText, ULONG ulLen )
{
   HB_LONG lVal;
   double dVal;

   HB_TRACE(HB_TR_DEBUG, ("hb_strVal(%s)", szText));

   if ( ! hb_str2number( FALSE, szText, ulLen, &lVal, &dVal, NULL, NULL ) )
      dVal = ( double ) lVal;
   return dVal;
}

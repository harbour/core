/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TRANSFORM() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
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
 * www - http://harbour-project.org
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *    String handling
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"
#include "hbset.h"
#include "hbapicdp.h"

/* Picture function flags */
#define PF_LEFT       0x0001   /* @B */
#define PF_CREDIT     0x0002   /* @C */
#define PF_DEBIT      0x0004   /* @X */
#define PF_PADL       0x0008   /* @L */ /* NOTE: This is a FoxPro/XPP extension [vszakats] */
#define PF_PARNEG     0x0010   /* @( */
#define PF_REMAIN     0x0020   /* @R */
#define PF_UPPER      0x0040   /* @! */
#define PF_DATE       0x0080   /* @D */
#define PF_BRITISH    0x0100   /* @E */
#define PF_EXCHANG    0x0100   /* @E. Also means exchange . and , */
#define PF_EMPTY      0x0200   /* @Z */
#define PF_WIDTH      0x0400   /* @S */
#define PF_PARNEGWOS  0x0800   /* @) Similar to PF_PARNEG but without leading spaces */
#define PF_TIME       0x1000   /* @T only time part from timestamp items, Harbour extension */

HB_FUNC( TRANSFORM )
{
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY ); /* Input parameter */
   PHB_ITEM pPic = hb_param( 2, HB_IT_STRING ); /* Picture string */

   HB_BOOL bError = HB_FALSE;

   if( pValue == NULL )
      bError = HB_TRUE;
   else if( pPic && hb_itemGetCLen( pPic ) > 0 )
   {
      char szPicDate[ 11 ];
      const char * szPic = hb_itemGetCPtr( pPic );
      HB_SIZE ulPicLen = hb_itemGetCLen( pPic );
      HB_USHORT uiPicFlags; /* Function flags */

      HB_SIZE ulParamS = 0; /* To avoid GCC -O2 warning */
      char    cParamL = '\0'; /* To avoid GCC -O2 warning */

      char *  szResult;
      HB_SIZE ulResultPos;

      HB_SIZE ulOffset = 0;

      /* ======================================================= */
      /* Analyze picture functions                               */
      /* ======================================================= */

      uiPicFlags = 0;

      /* If an "@" char is at the first pos, we have picture function */

      if( *szPic == '@' )
      {
         HB_BOOL bDone = HB_FALSE;

         /* Skip the "@" char */

         szPic++;
         ulPicLen--;

         /* Go through all function chars, until the end of the picture string
            or any whitespace found. */

         while( ulPicLen && ! bDone )
         {
            switch( hb_charUpper( *szPic ) )
            {
               case HB_CHAR_HT:
               case ' ':
                  bDone = HB_TRUE;     /* End of function string */
                  break;
               case '!':
                  uiPicFlags |= PF_UPPER;
                  break;
               case '(':
                  uiPicFlags |= PF_PARNEG;
                  break;
               case ')':
                  uiPicFlags |= PF_PARNEGWOS;
                  break;
#ifndef HB_CLP_STRICT
               /* Xbase++ and FoxPro compatibility */
               case 'L':
               case '0':
                  uiPicFlags |= PF_PADL;  /* FoxPro/XPP extension */
                  cParamL = '0';
                  break;
#endif
               case 'B':
                  uiPicFlags |= PF_LEFT;
                  break;
               case 'C':
                  uiPicFlags |= PF_CREDIT;
                  break;
               case 'D':
                  uiPicFlags |= PF_DATE;
                  break;
               case 'E':
                  uiPicFlags |= PF_BRITISH;
                  break;
               case 'R':
                  uiPicFlags |= PF_REMAIN;
                  break;
               case 'S':
                  uiPicFlags |= PF_WIDTH;
                  ulParamS = 0;
                  while( ulPicLen > 1 && *( szPic + 1 ) >= '0' && *( szPic + 1 ) <= '9' )
                  {
                     szPic++;
                     ulPicLen--;
                     ulParamS = ( ulParamS * 10 ) + ( ( HB_SIZE ) ( *szPic - '0' ) );
                  }
                  break;
               case 'T':
                  uiPicFlags |= PF_TIME;
                  break;
               case 'X':
                  uiPicFlags |= PF_DEBIT;
                  break;
               case 'Z':
                  uiPicFlags |= PF_EMPTY;
                  break;
            }

            szPic++;
            ulPicLen--;
         }
      }

      /* ======================================================= */
      /* Handle STRING values                                    */
      /* ======================================================= */

      if( HB_IS_STRING( pValue ) )
      {
         const char * szExp = hb_itemGetCPtr( pValue );
         HB_SIZE ulExpLen = hb_itemGetCLen( pValue );
         HB_SIZE ulExpPos = 0;
         HB_BOOL bAnyPic = HB_FALSE;
         HB_BOOL bFound  = HB_FALSE;

         /* Grab enough */

         /* Support date function for strings */
         if( uiPicFlags & ( PF_DATE | PF_BRITISH ) )
         {
            hb_dateFormat( "XXXXXXXX", szPicDate, hb_setGetDateFormat() );
            szPic = szPicDate;
            ulPicLen = ( HB_SIZE ) strlen( szPicDate );
         }

         szResult = ( char * ) hb_xgrab( ulExpLen + ulPicLen + 1 );
         ulResultPos = 0;

         /* Template string */
         if( ulPicLen )
         {
            while( ulPicLen )                        /* Analyze picture mask */
            {
               if( ulExpPos < ulExpLen )
               {
                  switch( *szPic )
                  {
                     /* Upper */
                     case '!':
                     {
                        szResult[ ulResultPos++ ] = ( char ) hb_charUpper( szExp[ ulExpPos ] );
                        ulExpPos++;
                        bAnyPic = HB_TRUE;
                        break;
                     }

                     /* Out the character */
                     case '#':
                     case '9':
                     case 'a':
                     case 'A':
                     case 'l':
                     case 'L':
                     case 'n':
                     case 'N':
                     case 'x':
                     case 'X':
                     {
                        szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? ( char ) hb_charUpper( szExp[ ulExpPos ] ) : szExp[ ulExpPos ];
                        ulExpPos++;
                        bAnyPic = HB_TRUE;
                        break;
                     }

                     /* Logical */
                     case 'y':
                     case 'Y':
                     {
                        szResult[ ulResultPos++ ] = ( szExp[ ulExpPos ] == 't' ||
                                                      szExp[ ulExpPos ] == 'T' ||
                                                      szExp[ ulExpPos ] == 'y' ||
                                                      szExp[ ulExpPos ] == 'Y' ) ? ( char ) 'Y' : ( char ) 'N';
                        ulExpPos++;
                        bAnyPic = HB_TRUE;
                        break;
                     }

                     /* Other choices */
                     default:
                     {
                        szResult[ ulResultPos++ ] = *szPic;

                        if( !( uiPicFlags & PF_REMAIN ) )
                           ulExpPos++;
                     }
                  }
               }
               else if( !( uiPicFlags & PF_REMAIN ) )
                  break;

               else
               {
/* NOTE: This is a FoxPro compatible [jarabal] */
#if defined( HB_COMPAT_FOXPRO )
                  ulPicLen = 0;
                  break;
#else
                  switch( *szPic )
                  {
                     case '!':
                     case '#':
                     case '9':
                     case 'a':
                     case 'A':
                     case 'l':
                     case 'L':
                     case 'n':
                     case 'N':
                     case 'x':
                     case 'X':
                     case 'y':
                     case 'Y':
                     {
                        szResult[ ulResultPos++ ] = ' ';
                        break;
                     }

                     default:
                        szResult[ ulResultPos++ ] = *szPic;
                  }
#endif
               }

               szPic++;
               ulPicLen--;
            }
         }
         else
         {
            while( ulExpPos++ < ulExpLen )
            {
               if( uiPicFlags & PF_EXCHANG )
               {
                  switch( *szExp )
                  {
                     case ',':
                     {
                        szResult[ ulResultPos++ ] = '.';
                        break;
                     }
                     case '.':
                     {
                        if( !bFound && ulResultPos )
                        {
                           szResult[ ulResultPos++ ] = ',';
                           bFound = HB_TRUE;
                        }

                        break;
                     }
                     default:
                        szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? ( char ) hb_charUpper( *szExp ) : *szExp;
                  }
               }
               else
               {
                  szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? ( char ) hb_charUpper( *szExp ) : *szExp;
               }
               szExp++;
            }
         }

         if( ( uiPicFlags & PF_REMAIN ) && ! bAnyPic )
         {
            while( ulExpPos++ < ulExpLen )
            {
               szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? ( char ) hb_charUpper( *szExp ) : *szExp;
               szExp++;
            }
         }

         /* Any chars left ? */
         if( ( uiPicFlags & PF_REMAIN ) && ulPicLen )
         {
            /* Export remainder */
            while( ulPicLen-- )
               szResult[ ulResultPos++ ] = ' ';
         }

         if( uiPicFlags & PF_BRITISH )
         {
            /* CA-Cl*pper do not check result size and always exchanges
             * bytes 1-2 with bytes 4-5. It's buffer overflow bug and I do
             * not want to replicate it. It also causes that the results of
             * @E conversion used for strings smaller then 5 bytes behaves
             * randomly.
             * In fact precise tests can show that it's not random behavior
             * but CA-Cl*pper uses static buffer for result and when current
             * one is smaller then 5 bytes then first two bytes are exchanged
             * with 4-5 bytes from previous result which was length enough,
             * f.e.:
             *          ? transform( "0123456789", "" )
             *          ? transform( "AB", "@E" )
             *          ? transform( "ab", "@E" )
             * [druzus]
             */
            if( ulResultPos >= 5 )
            {
               szPicDate[ 0 ] = szResult[ 0 ];
               szPicDate[ 1 ] = szResult[ 1 ];
               szResult[ 0 ] = szResult[ 3 ];
               szResult[ 1 ] = szResult[ 4 ];
               szResult[ 3 ] = szPicDate[ 0 ];
               szResult[ 4 ] = szPicDate[ 1 ];
            }
         }
      }

      /* ======================================================= */
      /* Handle NUMERIC values                                   */
      /* ======================================================= */

      else if( HB_IS_NUMERIC( pValue ) )
      {
         int      iWidth;                             /* Width of string          */
         int      iDec;                               /* Number of decimals       */
         int      iCount;
         HB_SIZE  i;
         char     cPic;
         PHB_ITEM pNumber = NULL;

         double dValue = hb_itemGetND( pValue );

         /* Support date function for numbers */
         if( uiPicFlags & PF_DATE )
         {
            hb_dateFormat( "99999999", szPicDate, hb_setGetDateFormat() );
            szPic = szPicDate;
            ulPicLen = ( HB_SIZE ) strlen( szPicDate );
         }

         for( i = iWidth = iDec = 0; i < ulPicLen; i++ )
         {
            if( szPic[ i ] == '.' )
            {
               while( ++i < ulPicLen )
               {
                  if( szPic[ i ] == '9' || szPic[ i ] == '#' ||
                      szPic[ i ] == '$' || szPic[ i ] == '*' )
                  {
                     iWidth++;
                     iDec++;
                  }
               }
               if( iDec )
                  iWidth++;
               break;
            }
            else if( szPic[ i ] == '9' || szPic[ i ] == '#' ||
                     szPic[ i ] == '$' || szPic[ i ] == '*' )
               iWidth++;
         }

         iCount = 0;
         if( iWidth == 0 )                             /* Width calculated ??      */
         {
            hb_itemGetNLen( pValue, &iWidth, &iDec );
            if( hb_setGetFixed() )
            {
               if( HB_IS_NUMINT( pValue ) )
                  iWidth += 2 + ( hb_setGetDecimals() << 1 );
               else
                  iDec = hb_setGetDecimals();
            }
            if( iDec )
               iWidth += iDec + 1;
         }
         else if( iDec > 0 && iWidth - iDec == 1 )
         {
            iCount = 1;
            iWidth++;
         }

         if( ( uiPicFlags & ( PF_DEBIT | PF_PARNEG | PF_PARNEGWOS ) ) && dValue < 0 )
         {
            /* Always convert absolute val */
            if( HB_IS_NUMINT( pValue ) ) /* workaround for 64bit integer conversion */
               pNumber = hb_itemPutNInt( NULL, - hb_itemGetNInt( pValue ) );
            else
               pNumber = hb_itemPutND( NULL, -dValue );
            pValue = pNumber;
         }

         if( dValue != 0 )
            /* Don't empty the result if the number is not zero */
            uiPicFlags &= ~PF_EMPTY;

         /* allocate 4 additional bytes for possible ") CR" or ") DB" suffix */
         szResult = ( char * ) hb_xgrab( iWidth + 5 );
         hb_itemStrBuf( szResult, pValue, iWidth, iDec );
         if( pNumber )
            hb_itemRelease( pNumber );

         if( iCount )
         {
            iWidth--;
            if( *szResult != '0' )
            {
               memset( szResult + 1, '*', iWidth );
               *szResult = '.';
            }
            else
               memmove( szResult, szResult + 1, iWidth );
            szResult[ iWidth ] = '\0';
         }

         /* Pad with padding char */
         if( uiPicFlags & PF_PADL )
         {
            for( i = 0; szResult[ i ] == ' '; i++ )
               szResult[ i ] = cParamL;

            /* please test it with FoxPro and Xbase++ to check
             * if they made the same [druzus]
             */
            if( i && szResult[ i ] == '-' )
            {
               szResult[ 0 ] = '-';
               szResult[ i ] = cParamL;
            }
         }

         if( ulPicLen == 0 )
         {
            if( uiPicFlags & PF_EXCHANG )
            {
               for( i = 0; i < ( HB_SIZE ) iWidth; ++i )
               {
                  if( szResult[ i ] == '.' )
                  {
                     szResult[ i ] = ',';
                     break;
                  }
               }
            }
            i = iWidth;
         }
         else
         {
            char * szStr = szResult;

            /* allocate 4 additional bytes for possible ") CR" or ") DB" suffix */
            szResult = ( char * ) hb_xgrab( ulPicLen + 5 );

            for( i = iCount = 0; i < ulPicLen; i++ )
            {
               cPic = szPic[ i ];
               if( cPic == '9' || cPic == '#' )
               {
                  szResult[ i ] = iCount < iWidth ? szStr[ iCount++ ] : ' ';
               }
               else if( cPic == '$' || cPic == '*' )
               {
                  if( iCount < iWidth )
                  {
                     szResult[ i ] = szStr[ iCount ] == ' ' ? cPic : szStr[ iCount ];
                     iCount++;
                  }
                  else
                     szResult[ i ] = ' ';
               }
               else if( cPic == '.' && iCount < iWidth )
               {
                  szResult[ i ] = ( uiPicFlags & PF_EXCHANG ) ? ',' : '.';
                  iCount++;
               }
               else if( cPic == ',' && i && iCount < iWidth )
               {
                  if( HB_ISDIGIT( ( HB_UCHAR ) szResult[ i - 1 ] ) )
                     szResult[ i ] = ( uiPicFlags & PF_EXCHANG ) ? '.' : ',';
                  else
                  {
                     szResult[ i ] = szResult[ i - 1 ];
                     if( szResult[ i - 1 ] == '-' )
                     {
                        szResult[ i - 1 ] = i > 1 && szResult[ i - 2 ] != '$' ?
                                            szResult[ i - 2 ] : ' ';
                     }
                  }
               }
               else
                  szResult[ i ] = cPic;
            }
            hb_xfree( szStr );
         }

         if( dValue < 0 )
         {
            /* PF_PARNEGWOS has higher priority then PF_PARNEG */
            if( ( uiPicFlags & PF_PARNEGWOS ) )
            {
               iCount = 0;
               if( ulPicLen && i > 1 )
               {
                  if( *szPic == *szResult && ( *szPic == '*' || *szPic == '$' ) &&
                      szResult[ 1 ] == ' ' )
                     ++iCount;
               }
               while( ( HB_SIZE ) iCount + 1 < i && szResult[ iCount + 1 ] == ' ' )
                  ++iCount;

#ifndef HB_CLP_STRICT
               /* This is not Clipper compatible */
               if( szResult[ iCount ] >= '1' && szResult[ iCount ] <= '9' &&
                   ( ulPicLen == 0 || szPic[ iCount ] == '9' ||
                     szPic[ iCount ] != szResult[ iCount ] ) )
               {
                  szResult[ iCount ] = '(';
                  for( ++iCount; ( HB_SIZE ) iCount < i; iCount++ )
                  {
                     if( szResult[ iCount ] >= '0' && szResult[ iCount ] <= '9' &&
                         ( ulPicLen == 0 || szPic[ iCount ] == '9' ||
                           szPic[ iCount ] != szResult[ iCount ] ) )
                        szResult[ iCount ] = '*';
                  }
               }
               else
#endif
                  szResult[ iCount ] = '(';
               szResult[ i++ ] = ')';
            }
            else if( ( uiPicFlags & PF_PARNEG ) )
            {
#ifndef HB_CLP_STRICT
               /* This is not Clipper compatible */
               if( *szResult >= '1' && *szResult <= '9' &&
                   ( ulPicLen == 0 || *szPic == '9' || *szPic != *szResult ) )
               {
                  for( iCount = 1; ( HB_SIZE ) iCount < i; iCount++ )
                  {
                     if( szResult[ iCount ] >= '0' && szResult[ iCount ] <= '9' &&
                         ( ulPicLen == 0 || szPic[ iCount ] == '9' ||
                           szPic[ iCount ] != szResult[ iCount ] ) )
                        szResult[ iCount ] = '*';
                  }
               }
#endif
               *szResult       = '(';
               szResult[ i++ ] = ')';
               ulOffset = 1;
            }

            if( ( uiPicFlags & PF_DEBIT ) )
            {
               szResult[ i++ ] = ' ';
               szResult[ i++ ] = 'D';
               szResult[ i++ ] = 'B';
            }
         }
         else if( ( uiPicFlags & PF_CREDIT ) && dValue > 0 )
         {
            szResult[ i++ ] = ' ';
            szResult[ i++ ] = 'C';
            szResult[ i++ ] = 'R';
         }

         ulResultPos = i;
         szResult[ i ] = '\0';
      }

      /* ======================================================= */
      /* Handle DATE values                                      */
      /* ======================================================= */

      else if( HB_IS_DATE( pValue ) )
      {
         const char * szDateFormat;
         char szNewFormat[ 11 ];
         char szDate[ 9 ];
         HB_SIZE nFor;

         szResult = ( char * ) hb_xgrab( 13 );
         szDateFormat = hb_setGetDateFormat();

#ifndef HB_CLP_STRICT
         if( uiPicFlags & PF_BRITISH )
         {
            /* When @E is used CA-Cl*pper do not update date format
             * pattern but wrongly moves 4-th and 5-th bytes of
             * formatted date to the beginning (see below). It causes
             * that date formats formats different then MM?DD?YY[YY]
             * are wrongly translated. The code below is not CA-Cl*pper
             * compatible but it tries to respect user date format
             * [druzus]
             */
            const char * szBritish = hb_setGetCentury() ?
                                     "DDMMYYYY" : "DDMMYY";
            char cLast = 'x';

            for( nFor = 0; nFor < 10; nFor++ )
            {
               if( *szBritish == cLast )
               {
                  szNewFormat[ nFor ] = cLast;
                  szBritish++;
               }
               else if( ! *szDateFormat )
                  break;
               else if( *szBritish &&
                        ( *szDateFormat == 'Y' || *szDateFormat == 'y' ||
                          *szDateFormat == 'D' || *szDateFormat == 'd' ||
                          *szDateFormat == 'M' || *szDateFormat == 'm' ) )
               {
                  szNewFormat[ nFor ] = cLast = *szBritish++;
                  do
                     szDateFormat++;
                  while( szDateFormat[ -1 ] == szDateFormat[ 0 ] );
               }
               else
                  szNewFormat[ nFor ] = *szDateFormat++;
            }
            szNewFormat[ nFor ] = '\0';
            szDateFormat = szNewFormat;
         }
#endif

         hb_dateFormat( hb_itemGetDS( pValue, szDate ), szResult, szDateFormat );
         ulResultPos = ( HB_SIZE ) strlen( szResult );

#ifdef HB_CLP_STRICT
         if( uiPicFlags & PF_BRITISH )
         {
            /* replicated wrong Clipper behavior, see note above.
             * It's not exact CA-Cl*pper behavior because it does
             * not check for size of results and can extract data
             * from static memory buffer used in previous conversions
             * (see my note for @E in string conversion above)
             * but this is buffer overflow and I do not plan to
             * replicated it too [druzus]
             */
            if( ulResultPos >= 5 )
            {
               szNewFormat[ 0 ] = szResult[ 0 ];
               szNewFormat[ 1 ] = szResult[ 1 ];
               szResult[ 0 ] = szResult[ 3 ];
               szResult[ 1 ] = szResult[ 4 ];
               szResult[ 3 ] = szNewFormat[ 0 ];
               szResult[ 4 ] = szNewFormat[ 1 ];
            }
         }
#endif
         if( uiPicFlags & PF_REMAIN )
         {
            /* Here we also respect the date format modified for @E [druzus]
             */
            hb_dateFormat( "99999999", szPicDate, szDateFormat );
            ulPicLen = ( HB_SIZE ) strlen( szPicDate );

            for( nFor = 0; nFor < ulPicLen; nFor++ )
            {
               if( szPicDate[ nFor ] != '9' )
               {
                  memmove( szResult + nFor + 1, szResult + nFor, 12 - nFor );
                  szResult[ nFor ] = szPicDate[ nFor ];
                  ulResultPos++;
               }
            }
            szResult[ 12 ] = '\0';
         }
      }

      /* ======================================================= */
      /* Handle TIMESTAMP values                                 */
      /* ======================================================= */

      else if( HB_IS_TIMESTAMP( pValue ) )
      {
         const char * szDateFormat = NULL, * szTimeFormat = NULL;
         char szNewFormat[ 11 ];
         long lDate, lTime;
         HB_SIZE nFor;

         szResult = ( char * ) hb_xgrab( 29 );
         if( ( uiPicFlags & ( PF_DATE | PF_TIME ) ) != PF_TIME )
            szDateFormat = hb_setGetDateFormat();
         if( ( uiPicFlags & ( PF_DATE | PF_TIME ) ) != PF_DATE )
            szTimeFormat = hb_setGetTimeFormat();

#ifndef HB_CLP_STRICT
         if( szDateFormat && ( uiPicFlags & PF_BRITISH ) )
         {
            /* When @E is used CA-Cl*pper do not update date format
             * pattern but wrongly moves 4-th and 5-th bytes of
             * formatted date to the beginning (see below). It causes
             * that date formats formats different then MM?DD?YY[YY]
             * are wrongly translated. The code below is not CA-Cl*pper
             * compatible but it tries to respect user date format
             * [druzus]
             */
            const char * szBritish = hb_setGetCentury() ?
                                     "DDMMYYYY" : "DDMMYY";
            char cLast = 'x';

            for( nFor = 0; nFor < 10; nFor++ )
            {
               if( *szBritish == cLast )
               {
                  szNewFormat[ nFor ] = cLast;
                  szBritish++;
               }
               else if( ! *szDateFormat )
                  break;
               else if( *szBritish &&
                        ( *szDateFormat == 'Y' || *szDateFormat == 'y' ||
                          *szDateFormat == 'D' || *szDateFormat == 'd' ||
                          *szDateFormat == 'M' || *szDateFormat == 'm' ) )
               {
                  szNewFormat[ nFor ] = cLast = *szBritish++;
                  do
                     szDateFormat++;
                  while( szDateFormat[ -1 ] == szDateFormat[ 0 ] );
               }
               else
                  szNewFormat[ nFor ] = *szDateFormat++;
            }
            szNewFormat[ nFor ] = '\0';
            szDateFormat = szNewFormat;
         }
#endif

         hb_itemGetTDT( pValue, &lDate, &lTime );
         if( szTimeFormat )
         {
            if( szDateFormat )
               hb_timeStampFormat( szResult, szDateFormat, szTimeFormat, lDate, lTime );
            else
               hb_timeFormat( szResult, szTimeFormat, lTime );
         }
         else
         {
            char szDate[ 9 ];
            hb_dateFormat( hb_dateDecStr( szDate, lDate ), szResult, szDateFormat );
         }
         ulResultPos = ( HB_SIZE ) strlen( szResult );

#ifdef HB_CLP_STRICT
         if( uiPicFlags & PF_BRITISH )
         {
            /* replicated wrong Clipper behavior, see note above.
             * It's not exact CA-Cl*pper behavior because it does
             * not check for size of results and can extract data
             * from static memory buffer used in previous conversions
             * (see my note for @E in string conversion above)
             * but this is buffer overflow and I do not plan to
             * replicated it too [druzus]
             */
            if( ulResultPos >= 5 )
            {
               szNewFormat[ 0 ] = szResult[ 0 ];
               szNewFormat[ 1 ] = szResult[ 1 ];
               szResult[ 0 ] = szResult[ 3 ];
               szResult[ 1 ] = szResult[ 4 ];
               szResult[ 3 ] = szNewFormat[ 0 ];
               szResult[ 4 ] = szNewFormat[ 1 ];
            }
         }
#endif
         if( szDateFormat && ( uiPicFlags & PF_REMAIN ) )
         {
            /* Here we also respect the date format modified for @E [druzus]
             */
            hb_dateFormat( "99999999", szPicDate, szDateFormat );
            ulPicLen = ( HB_SIZE ) strlen( szPicDate );

            for( nFor = 0; nFor < ulPicLen; nFor++ )
            {
               if( szPicDate[ nFor ] != '9' )
               {
                  memmove( szResult + nFor + 1, szResult + nFor, 28 - nFor );
                  szResult[ nFor ] = szPicDate[ nFor ];
                  ulResultPos++;
               }
            }
            szResult[ 28 ] = '\0';
         }
      }

      /* ======================================================= */
      /* Handle LOGICAL values                                   */
      /* ======================================================= */

      else if( HB_IS_LOGICAL( pValue ) )
      {
         HB_BOOL bDone = HB_FALSE;
         HB_BOOL bExit = HB_FALSE;
         char cPic;

         if( uiPicFlags & ( PF_DATE | PF_BRITISH ) )
         {
            hb_dateFormat( "99999999", szPicDate, hb_setGetDateFormat() );
            szPic = szPicDate;
            ulPicLen = ( HB_SIZE ) strlen( szPicDate );
         }

         ulResultPos = 0;
         szResult = ( char * ) hb_xgrab( ulPicLen + 2 );

         for( ; ( ulPicLen || !bDone ) && !bExit ; ulResultPos++, szPic++, ulPicLen-- )
         {
            if( ulPicLen )
               cPic = *szPic;
            else
            {
               cPic  = 'L';
               bExit = HB_TRUE;
            }

            switch( cPic )
            {
               case 'y':                     /* Yes/No                   */
               case 'Y':                     /* Yes/No                   */
               {
                  if( !bDone )
                  {
                     szResult[ ulResultPos ] = hb_itemGetL( pValue ) ? 'Y' : 'N';
                     bDone = HB_TRUE;           /* Logical written          */
                  }
                  else
                     szResult[ ulResultPos ] = ' ';

                  break;
               }

               case '#':
               case 'l':                     /* True/False               */
               case 'L':                     /* True/False               */
               {
                  if( !bDone )
                  {
                     szResult[ ulResultPos ] = hb_itemGetL( pValue ) ? 'T' : 'F';
                     bDone = HB_TRUE;
                  }
                  else
                     szResult[ ulResultPos ] = ' ';

                  break;
               }

               default:
                  szResult[ ulResultPos ] = cPic;
            }

            if( !( uiPicFlags & PF_REMAIN ) )
               bExit = HB_TRUE;
         }
      }

      /* ======================================================= */

      else
      {
         szResult = NULL; /* To avoid GCC -O2 warning */
         ulResultPos = 0; /* To avoid GCC -O2 warning */
         bError = HB_TRUE;
      }

      if( ! bError )
      {
         if( uiPicFlags & PF_EMPTY )
            memset( szResult, ' ', ulResultPos );
         else if( uiPicFlags & PF_LEFT )
         {
            /* Trim left and pad with spaces */
            HB_SIZE ulFirstChar = ulOffset;

            while( ulFirstChar < ulResultPos && szResult[ ulFirstChar ] == ' ' )
               ulFirstChar++;

            if( ulFirstChar > ulOffset && ulFirstChar < ulResultPos )
            {
               memmove( szResult + ulOffset, szResult + ulFirstChar, ulResultPos - ulFirstChar );
               memset( szResult + ulOffset + ulResultPos - ulFirstChar, ' ', ulFirstChar - ulOffset );
            }
         }

         hb_retclen_buffer( szResult, ( ulParamS && ulResultPos > ulParamS ) ? ulParamS : ulResultPos );
      }
   }
   else if( pPic || HB_ISNIL( 2 ) ) /* Picture is an empty string or NIL */
   {
      if( HB_IS_STRING( pValue ) )
      {
         hb_itemReturn( pValue );
      }
      else if( HB_IS_NUMERIC( pValue ) )
      {
         char * szStr;

         if( HB_IS_NUMINT( pValue ) && hb_setGetFixed() )
         {
            int iWidth, iDec;
            hb_itemGetNLen( pValue, &iWidth, &iDec );
            iWidth += 2 + ( hb_setGetDecimals() << 1 );
            szStr = ( char * ) hb_xgrab( iWidth + 1 );
            hb_itemStrBuf( szStr, pValue, iWidth, iDec );
            hb_retclen_buffer( szStr, iWidth );
         }
         else
         {
            HB_SIZE nLen;
            HB_BOOL bFreeReq;

            szStr = hb_itemString( pValue, &nLen, &bFreeReq );
            if( bFreeReq )
               hb_retclen_buffer( szStr, nLen );
            else
               hb_retclen( szStr, nLen );
         }
      }
      else if( HB_IS_DATE( pValue ) )
      {
         char szDate[ 9 ];
         char szResult[ 11 ];

         hb_retc( hb_dateFormat( hb_itemGetDS( pValue, szDate ), szResult, hb_setGetDateFormat() ) );
      }
      else if( HB_IS_TIMESTAMP( pValue ) )
      {
         char szResult[ 27 ];
         long lDate, lTime;

         hb_itemGetTDT( pValue, &lDate, &lTime );
         hb_retc( hb_timeStampFormat( szResult, hb_setGetDateFormat(), hb_setGetTimeFormat(), lDate, lTime ) );
      }
      else if( HB_IS_LOGICAL( pValue ) )
      {
         hb_retc_const( hb_itemGetL( pValue ) ? "T" : "F" );
      }
      else
         bError = HB_TRUE;
   }
   else
      bError = HB_TRUE;

   /* If there was any parameter error, launch a runtime error */

   if( bError )
      hb_errRT_BASE_SubstR( EG_ARG, 1122, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TRANSFORM() function
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
 *    String handling
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

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
#define PF_NUMDATE    0x0400   /* Internal flag. Ignore decimal dot */
#define PF_WIDTH      0x0800   /* @S */
#define PF_PARNEGWOS  0x1000   /* @) Similar to PF_PARNEG but without leading spaces */

extern PHB_CODEPAGE s_cdpage;
#define TOUPPER(c)    ((s_cdpage->nChars)? (char)s_cdpage->s_upper[c&255] : toupper(c))

HB_FUNC( TRANSFORM )
{
   PHB_ITEM pValue = hb_param( 1, HB_IT_ANY ); /* Input parameter */
   PHB_ITEM pPic = hb_param( 2, HB_IT_STRING ); /* Picture string */

   BOOL bError = FALSE;

   if( pPic && hb_itemGetCLen( pPic ) > 0 )
   {
      char * szPic = hb_itemGetCPtr( pPic );
      ULONG  ulPicLen = hb_itemGetCLen( pPic );
      USHORT uiPicFlags; /* Function flags */

      ULONG  ulParamS = 0; /* To avoid GCC -O2 warning */
      BYTE   byParamL = '\0'; /* To avoid GCC -O2 warning */

      char * szResult;
      ULONG  ulResultPos;

      double dValue = 0;

      /* ======================================================= */
      /* Analyze picture functions                               */
      /* ======================================================= */

      uiPicFlags = 0;

      /* If an "@" char is at the first pos, we have picture function */

      if( *szPic == '@' )
      {
         BOOL bDone = FALSE;

         /* Skip the "@" char */

         szPic++;
         ulPicLen--;

         /* Go through all function chars, until the end of the picture string
            or any whitespace found. */

         while( ulPicLen && ! bDone )
         {
            switch( toupper( *szPic ) )
            {
               case HB_CHAR_HT:
               case ' ':
                  bDone = TRUE;      /* End of function string */
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
               case 'L':
               case '0':
                  uiPicFlags |= PF_PADL;
                  byParamL = '0';
                  break;
               case 'B':
                  uiPicFlags |= PF_LEFT;
                  break;
               case 'C':
                  uiPicFlags |= PF_CREDIT;
                  break;
               case 'D':
                  uiPicFlags |= PF_DATE;
                  uiPicFlags |= PF_NUMDATE;
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

                     ulParamS = ( ulParamS * 10 ) + ( ( ULONG ) ( *szPic - '0' ) );
                  }

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
         char * szExp = hb_itemGetCPtr( pValue );
         ULONG  ulExpLen = hb_itemGetCLen( pValue );
         ULONG  ulExpPos = 0;

         char szPicDate[ 11 ];
         BOOL bAnyPic = FALSE;
         BOOL bFound  = FALSE;

         /* Grab enough */

         /* Support date function for strings */
         if( ( uiPicFlags & PF_DATE ) ||
           ( ( uiPicFlags & PF_BRITISH ) && ( uiPicFlags & PF_REMAIN ) ) )
         {
            hb_dateFormat( "XXXXXXXX", szPicDate, hb_set.HB_SET_DATEFORMAT );

            szPic = szPicDate;
            ulPicLen = strlen( szPicDate );
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
                        szResult[ ulResultPos++ ] = TOUPPER( szExp[ ulExpPos ] );
                        ulExpPos++;
                        bAnyPic = TRUE;
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
                        szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? TOUPPER( szExp[ ulExpPos ] ) : szExp[ ulExpPos ];
                        ulExpPos++;
                        bAnyPic = TRUE;
                        break;
                     }

                     /* Logical */
                     case 'y':
                     case 'Y':
                     {
                        szResult[ ulResultPos++ ] = ( szExp[ ulExpPos ] == 't' ||
                                                      szExp[ ulExpPos ] == 'T' ||
                                                      szExp[ ulExpPos ] == 'y' ||
                                                      szExp[ ulExpPos ] == 'Y' ) ? 'Y' : 'N';
                        ulExpPos++;
                        bAnyPic = TRUE;
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
                           bFound = TRUE;
                        }

                        break;
                     }
                     default:
                        szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? TOUPPER( *szExp ) : *szExp;
                  }
               }
               else
               {
                  szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? TOUPPER( *szExp ) : *szExp;
               }
               szExp++;
            }
         }

         if( ( uiPicFlags & PF_REMAIN ) && ! bAnyPic )
         {
            while( ulExpPos++ < ulExpLen )
            {
               szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? TOUPPER( *szExp ) : *szExp;
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

         if( ( ( uiPicFlags & PF_DATE ) && ( uiPicFlags & PF_BRITISH ) ) ||
           ( ( uiPicFlags & PF_BRITISH ) && ( uiPicFlags & PF_REMAIN ) ) )
         {
           szPicDate[ 0 ] = szResult[ 3 ];
           szPicDate[ 1 ] = szResult[ 4 ];

           szResult[ 3 ] = szResult[ 0 ];
           szResult[ 4 ] = szResult[ 1 ];

           szResult[ 0 ] = szPicDate[ 0 ];
           szResult[ 1 ] = szPicDate[ 1 ];
         }
      }

      /* ======================================================= */
      /* Handle NUMERIC values                                   */
      /* ======================================================= */

      else if( HB_IS_NUMERIC( pValue ) )
      {
         double   dPush;

         int      iOrigWidth;
         int      iOrigDec;
         int      iWidth;                             /* Width of string          */
         int      iDec;                               /* Number of decimals       */
         ULONG    i;
         int      iCount = 0;

         char *   szStr;
         char     cPic;
         char     szPicDate[ 11 ];

         PHB_ITEM pNumber;
         PHB_ITEM pWidth;
         PHB_ITEM pDec;

         BOOL     bFound = FALSE;
         BOOL     bInit  = FALSE;
         BOOL     bPDec  = FALSE;

         dValue = hb_itemGetND( pValue );
         hb_itemGetNLen( pValue, &iOrigWidth, &iOrigDec );

         /* Support date function for numbers */
         if( uiPicFlags & PF_DATE )
         {
            hb_dateFormat( "99999999", szPicDate,
                 hb_set.HB_SET_DATEFORMAT );

            szPic = szPicDate;
            ulPicLen = strlen( szPicDate );
         }

         /* TODO: maybe replace this 16 with something else */
         szResult = ( char * ) hb_xgrab( ulPicLen + 16 );   /* Grab enough */
         *szResult = '\0';

         for( i = 0; i < ulPicLen && !bFound; i++ )      /* Count number in front    */
         {
            if( szPic[ i ] == '.' )
/*               bFound = !( uiPicFlags & PF_NUMDATE );    / * Exit when numeric        */
               bFound =  TRUE;
            else if( szPic[ i ] == '9' || szPic[ i ] == '#' ||
                     szPic[ i ] == '$' || szPic[ i ] == '*' )
               iCount++;
         }
         iWidth = iCount;

         if( bFound )                                 /* Did we find a dot        */
         {
            iDec = 0;
            iWidth++;                                 /* Also adjust iWidth       */
            for( ; i < ulPicLen; i++ )
            {
               if( szPic[ i ] == '9' ||
                   szPic[ i ] == '#' ||
                   szPic[ i ] == '$' ||
                   szPic[ i ] == '*' )
               {
                  iWidth++;
                  iDec++;
               }
            }
         }
         else
            iDec = 0;

         if( ( uiPicFlags & ( PF_DEBIT + PF_PARNEG + PF_PARNEGWOS ) ) && dValue < 0 )
            dPush = -dValue;                           /* Always push absolute val */
         else
            dPush = dValue;

         /* Don't empty the result if the number is not zero */
         if( dPush != 0 && ( uiPicFlags & PF_EMPTY ) )
               uiPicFlags &= ~PF_EMPTY;

         if( iWidth == 0 )                             /* Width calculated ??      */
         {
            iWidth = iOrigWidth;                       /* Push original width      */
            iDec = iOrigDec;                           /* Push original decimals   */
         }

         pNumber = hb_itemPutNDLen( NULL, dPush, -1, iDec );
         pWidth = hb_itemPutNI( NULL, iWidth + ( !ulPicLen && iDec > 0 ? iDec + 1 : 0 ) );
         pDec = hb_itemPutNI( NULL, iDec );

         szStr = hb_itemStr( pNumber, pWidth, pDec );

         hb_itemRelease( pNumber );
         hb_itemRelease( pWidth );
         hb_itemRelease( pDec );

         if( szStr )
         {
            iCount = 0;

            /* Pad with Zero's */
            if( uiPicFlags & PF_PADL )
            {
               for( i = 0; szStr[ i ] == ' ' && i < ( ULONG ) iWidth; i++ )
                  szStr[ i ] = byParamL;
            }

            if( ulPicLen )
               for( i = 0; i < ulPicLen; i++ )
               {
                  cPic = szPic[ i ];

                  if( !bInit && ( cPic == '9' || cPic == '#' || cPic == '$' || cPic == '*' ) )
                     bInit = TRUE;

                  if( cPic == '9' || cPic == '#' )
                  {
                     if( iCount < iWidth )
                        szResult[ i ] = szStr[ iCount++ ];  /* Just copy                */
                     else
                        szResult[ i ] = ' ';
                  }
                  else if( cPic == '.' && bInit )
                  {
                     bPDec = TRUE;

                     if( uiPicFlags & PF_EXCHANG )  /* Exchange . and ,         */
                     {
                        szResult[ i ] = '.';
                        iCount++;
                     }
                     else
                     {
                        if( uiPicFlags & PF_NUMDATE || bPDec )    /* Dot in date              */
                        {
                           szResult[ i ] = cPic;
                           iCount++;
                        }
                        else                             /* Dot in number            */
                        {
                           szResult[ i ] = szStr[ iCount++ ];
                        }
                     }
                  }
                  else if( cPic == '$' || cPic == '*' )
                  {
                     if( szStr[ iCount ] == ' ' )
                     {
                        szResult[ i ] = cPic;
                        iCount++;
                     }
                     else
                        szResult[ i ] = szStr[ iCount++ ];
                  }
                  else if( cPic == ',' && bInit )    /* Comma                    */
                  {
                     if( iCount && isdigit( ( int ) szStr[ iCount - 1 ] ) )
                     {                                /* May we place it     */
                        if( uiPicFlags & PF_EXCHANG )
                          szResult[ i ] = '.';
                        else
                           szResult[ i ] = ',';
                     }
                     else
                     {
                        if( i && szResult[ i - 1 ] == '*' )
                           szResult[ i ] = '*';
                        else
                           szResult[ i ] = '0';
                     }
                  }
                  else
                     szResult[ i ] = cPic;
               }
            else
            {
               strcpy( szResult, szStr );
               i = strlen( szStr );
            }

            if( ( uiPicFlags & PF_PARNEG ) && dValue < 0 && !( uiPicFlags & PF_PARNEGWOS ) )
                              /* This is a behavior of Clipper,
                                 if exist PF_PARNEG and PF_PARNEGWOS,
                                 PR_PARNEGWOS prevails. */

            {
               for( iCount = 0; ( ULONG ) iCount < i; iCount++ )
                   /* Permit to detect overflow when picture init with mask */
               {
                  if( isdigit( ( int ) szResult[ iCount ] ) &&
                       !( szResult[ iCount ] == '0' ) &&       /* if not PF_PADL */
                       ( iCount == 0 ||
                       !isdigit( ( int ) szPic[ iCount ] ) ) ) /* if not mask symbol */
                                                               /* Overflow */
                  {
                     for( iCount++; ( ULONG ) iCount < i; iCount++ )
                     {
                        if( isdigit( ( int ) szResult[ iCount ] ) )
                           szResult[ iCount ] = '*';
                     }
                     break;
                  }
                  else
                  {
                     if( !isdigit( ( int ) szResult[ iCount ] ) ||
                            ( szResult[ iCount ] == '0' && !isdigit( ( int ) szPic[ iCount ] ) ) )
                        break;
                  }
               }
               *szResult       = '(';
               szResult[ i++ ] = ')';
            }

            if( ( uiPicFlags & PF_PARNEGWOS ) && dValue < 0 )
            {
               for( iCount = 0; ( ULONG ) iCount < i; iCount++ )
                   /* Permit to detect overflow when picture init with mask */
               {
                  if( isdigit( ( int ) szResult[ iCount ] ) &&
                       !( szResult[ iCount ] == '0' ) &&       /* if not PF_PADL */
                       ( iCount == 0 ||
                       !isdigit( ( int ) szPic[ iCount ] ) ) ) /* if not mask symbol */
                                                               /* Overflow */
                  {
                     for( iCount++; ( ULONG ) iCount < i; iCount++ )
                     {
                        if( isdigit( ( int ) szResult[ iCount ] ) )
                           szResult[ iCount ] = '*';
                     }
                     break;
                  }
                  else
                  {
                     if( !isdigit( ( int ) szResult[ iCount ] ) ||
                            ( szResult[ iCount ] == '0' && !isdigit( ( int ) szPic[ iCount ] ) ) )
                     {
                        for( ; ( ULONG ) iCount < i; iCount++ )
                        {
                           if( szResult[ iCount ] != ' ' )
                           {
                              if( iCount > 0 )
                                 szResult[ iCount - 1 ] = '(';
                              else
                                 *szResult       = '(';

                              break;
                           }
                        }
                        break;
                     }
                  }
               }
               szResult[ i++ ] = ')';
            }

            if( ( uiPicFlags & PF_CREDIT ) && dValue >= 0 )
            {
               szResult[ i++ ] = ' ';
               szResult[ i++ ] = 'C';
               szResult[ i++ ] = 'R';
            }

            if( ( uiPicFlags & PF_DEBIT ) && dValue < 0 )
            {
               szResult[ i++ ] = ' ';
               szResult[ i++ ] = 'D';
               szResult[ i++ ] = 'B';
            }

            ulResultPos = i;
            szResult[ i ] = '\0';

            hb_xfree( szStr );
         }
         else
         {
            ulResultPos = 0;
         }
      }

      /* ======================================================= */
      /* Handle DATE values                                      */
      /* ======================================================= */

      else if( HB_IS_DATE( pValue ) )
      {
         char szPicDate[ 11 ];
         char szDate[ 9 ];

         UINT nFor;

         szResult = ( char * ) hb_xgrab( 13 );

         hb_dateFormat( hb_itemGetDS( pValue, szDate ), szResult,
            ( uiPicFlags & PF_BRITISH ) ?
              ( hb_set.hb_set_century ? "DD/MM/YYYY" : "DD/MM/YY" ) :
              hb_set.HB_SET_DATEFORMAT );

         ulResultPos = strlen( szResult );

         if( uiPicFlags & PF_REMAIN )
         {
            hb_dateFormat( "99999999", szPicDate,
               ( uiPicFlags & PF_BRITISH ) ?
                 ( hb_set.hb_set_century ? "DD/MM/YYYY" : "DD/MM/YY" ) :
                 hb_set.HB_SET_DATEFORMAT );

            ulPicLen = strlen( szPicDate );

            for( nFor = 0; nFor < ulPicLen; nFor++ )
            {
               if( szPicDate[ nFor ] != '9' )
               {
                  memmove( szResult + nFor + 1, szResult + nFor, 12 - nFor );
                  szResult[ nFor ] = szPicDate[ nFor ];
                  ulResultPos++;
               }
            }
         }

      }

      /* ======================================================= */
      /* Handle LOGICAL values                                   */
      /* ======================================================= */

      else if( HB_IS_LOGICAL( pValue ) )
      {
         BOOL bDone = FALSE;
         BOOL bExit = FALSE;

         char cPic;

         ulResultPos = 0;

         szResult = ( char * ) hb_xgrab( ulPicLen + 2 );

         for( ; ( ulPicLen || !bDone ) && !bExit ; ulResultPos++, szPic++, ulPicLen-- )
         {

            if( ulPicLen )
               cPic = *szPic;
            else
            {
               cPic  = 'L';
               bExit = TRUE;
            }

            switch( cPic )
            {
               case 'y':                     /* Yes/No                   */
               case 'Y':                     /* Yes/No                   */
               {
                  if( !bDone )
                  {
                     szResult[ ulResultPos ] = hb_itemGetL( pValue ) ? 'Y' : 'N';
                     bDone = TRUE;           /* Logical written          */
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
                     bDone = TRUE;
                  }
                  else
                     szResult[ ulResultPos ] = ' ';

                  break;
               }

               default:
                  szResult[ ulResultPos ] = cPic;
            }

            if( !( uiPicFlags & PF_REMAIN ) )
               bExit = TRUE;
         }

      }

      /* ======================================================= */

      else
      {
         szResult = NULL; /* To avoid GCC -O2 warning */
         ulResultPos = 0; /* To avoid GCC -O2 warning */
         bError = TRUE;
      }

      if( ! bError )
      {
         /* Trim left and pad with spaces */
         if( uiPicFlags & PF_LEFT )
         {

            ULONG ulFirstChar = ( ( ( uiPicFlags & PF_PARNEG ) && dValue < 0 && !( uiPicFlags & PF_PARNEGWOS ) ) ? 1: 0 );

            while( ulFirstChar < ulResultPos && szResult[ ulFirstChar ] == ' ' )
               ulFirstChar++;

            if( ulFirstChar < ulResultPos )
            {
               memmove( szResult + ( ( ( uiPicFlags & PF_PARNEG ) && dValue < 0 && !( uiPicFlags & PF_PARNEGWOS ) ) ? 1: 0 ), szResult + ulFirstChar, ulResultPos - ulFirstChar );
               memset( szResult + ulResultPos - ulFirstChar + ( ( ( uiPicFlags & PF_PARNEG ) && dValue < 0 && !( uiPicFlags & PF_PARNEGWOS ) ) ? 1: 0 ), ' ', ulFirstChar );
            }
         }

         if( uiPicFlags & PF_EMPTY )
            memset( szResult, ' ', ulResultPos );

         hb_retclen_buffer( szResult, ( uiPicFlags & PF_WIDTH && ulResultPos > ulParamS && ulParamS > 0 ) ? ulParamS : ulResultPos );
      }
   }
   else if( pPic || ISNIL( 2 ) ) /* Picture is an empty string or NIL */
   {
      if( HB_IS_STRING( pValue ) )
      {
         hb_itemReturn( pValue );
      }
      else if( HB_IS_NUMERIC( pValue ) )
      {
         char * szStr = hb_itemStr( pValue, NULL, NULL );

         if( szStr )
         {
            hb_retc( szStr );
            hb_xfree( szStr );
         }
         else
            hb_retc( NULL );
      }
      else if( HB_IS_DATE( pValue ) )
      {
         char szDate[ 9 ];
         char szResult[ 11 ];

         hb_retc( hb_dateFormat( hb_itemGetDS( pValue, szDate ), szResult, hb_set.HB_SET_DATEFORMAT ) );
      }
      else if( HB_IS_LOGICAL( pValue ) )
      {
         hb_retc( hb_itemGetL( pValue ) ? "T" : "F" );
      }
      else
         bError = TRUE;
   }
   else
      bError = TRUE;

   /* If there was any parameter error, launch a runtime error */

   if( bError )
      hb_errRT_BASE_SubstR( EG_ARG, 1122, NULL, "TRANSFORM", 2, pValue, hb_paramError( 2 ) );
}

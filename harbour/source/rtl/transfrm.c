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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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

/* Picture function flags */
#define PF_LEFT    0x0001   /* @B */
#define PF_CREDIT  0x0002   /* @C */
#define PF_DEBIT   0x0004   /* @X */
#define PF_ZERO    0x0008   /* @0 */ /* NOTE: This is a Harbour extension [vszakats] */
#define PF_PARNEG  0x0010   /* @( */
#define PF_REMAIN  0x0020   /* @R */
#define PF_UPPER   0x0040   /* @! */
#define PF_DATE    0x0080   /* @D */
#define PF_BRITISH 0x0100   /* @E */
#define PF_EXCHANG 0x0100   /* @E. Also means exchange . and , */
#define PF_EMPTY   0x0200   /* @Z */
#define PF_NUMDATE 0x0400   /* Internal flag. Ignore decimal dot */
#define PF_STRING  0x0800   /* @S */

/*
   PictFunc -> Analyze function flags and return binary flags bits

   pszPic      : Pointer to the picture
   pulPicLen   : Pointer to the length.  Changed during execution.
   pulPicStart : Pointer to the starting position of the picture template.
*/
static USHORT PictFunc( char ** pszPic, ULONG * pulPicLen, ULONG * pulPicStart )
{
   BOOL bDone = FALSE;
   USHORT uiPicFlags = 0;
   ULONG ulPicLen = *pulPicLen;

   HB_TRACE(HB_TR_DEBUG, ("PictFunc(%p, %p)", pszPic, pulPicLen));

   /* If an "@" char is at the first pos, we have picture function */

   if( **pszPic == '@' )
   {
      /* Skip the "@" char */

      ( *pszPic )++;
      ( *pulPicLen )--;

      /* Go through all function chars, until the end of the picture string
         or any whitespace found. */

      while( *pulPicLen && ! bDone )
      {
         switch( toupper( **pszPic ) )
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
            case '0':
               uiPicFlags |= PF_ZERO;
               break;
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
               uiPicFlags |= PF_STRING;
               break;
            case 'X':
               uiPicFlags |= PF_DEBIT;
               break;
            case 'Z':
               uiPicFlags |= PF_EMPTY;
               break;
         }

         ( *pszPic )++;
         ( *pulPicLen )--;
      }
   }

   /* Get start pos of template */
   *pulPicStart = ulPicLen - *pulPicLen;

   return uiPicFlags;
}

/*
   NumPicture -> Handle a numeric picture.

   szPic        : Picture
   ulPicLen     : Length of picture
   puiPicFlags  : Function flags. NUM_DATE tells whether its a number or date
   dValue       : Number to picture
   pulResultLen : The size of the returned string is passed here !
   iOrigWidth   : Original width
   iOrigDec     : Original decimals
*/
static char * NumPicture( char * szPic, ULONG ulPicLen, USHORT * puiPicFlags, double dValue,
                          ULONG * pulResultLen, int iOrigWidth, int iOrigDec )
{
   int      iWidth;                             /* Width of string          */
   int      iDec;                               /* Number of decimals       */
   ULONG    i;
   int      iCount = 0;

   char *   szResult;
   char *   szStr;
   char     cPic;

   PHB_ITEM pNumber;
   PHB_ITEM pWidth;
   PHB_ITEM pDec;

   BOOL     bFound = FALSE;

   USHORT   uiPicFlags = *puiPicFlags;
   double   dPush;

   HB_TRACE(HB_TR_DEBUG, ("NumPicture(%s, %lu, %p, %lf)", szPic, ulPicLen, puiPicFlags, dValue));

   szResult = ( char * ) hb_xgrab( ulPicLen + 4 );   /* Grab enough              */
   *szResult = '\0';
   for( i = 0; i < ulPicLen && !bFound; i++ )      /* Count number in front    */
   {
      if( szPic[ i ] == '.' )
         bFound = !( uiPicFlags & PF_NUMDATE );  /* Exit when numeric        */
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

   if( ( uiPicFlags & ( PF_DEBIT + PF_PARNEG ) ) && ( dValue < 0 ) )
      dPush = -dValue;                           /* Always push absolute val */
   else
      dPush = dValue;

   /* Don't empty the result if the number is not zero */
   if( dPush != 0 && ( uiPicFlags & PF_EMPTY ) )
      *puiPicFlags &= ~uiPicFlags;

   if( !iWidth  )                                /* Width calculated ??      */
   {
      iWidth = iOrigWidth;                       /* Push original width      */
      iDec = iOrigDec;                           /* Push original decimals   */
   }

   pNumber = hb_itemPutNDLen( NULL, dPush, -1, iDec );
   pWidth = hb_itemPutNI( NULL, iWidth );
   pDec = hb_itemPutNI( NULL, iDec );

   szStr = hb_itemStr( pNumber, pWidth, pDec );

   hb_itemRelease( pNumber );
   hb_itemRelease( pWidth );
   hb_itemRelease( pDec );

   if( szStr )
   {
      iCount = 0;

      /* Pad with Zero's */
      if( uiPicFlags & PF_ZERO )
      {
         for( i = 0; szStr[ i ] == ' ' && i < ( ULONG ) iWidth; i++ )
            szStr[ i ] = '0';
      }

      for( i = 0; i < ulPicLen; i++ )
      {
         cPic = szPic[ i ];
         if( cPic == '9' || cPic == '#' )
            szResult[ i ] = szStr[ iCount++ ];  /* Just copy                */
         else if( cPic == '.' )
         {
            if( uiPicFlags & PF_NUMDATE )    /* Dot in date              */
               szResult[ i ] = cPic;
            else                             /* Dot in number            */
            {
               if( uiPicFlags & PF_EXCHANG ) /* Exchange . and ,         */
               {
                  szResult[ i ] = ',';
                  iCount++;
               }
               else
                  szResult[ i ] = szStr[ iCount++ ];
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
         else if( cPic == ',' )              /* Comma                    */
         {
            if( iCount && isdigit( ( int ) szStr[ iCount - 1 ] ) )
            {                                /* May we place it     */
               if( uiPicFlags & PF_EXCHANG )
                  szResult[ i ] = '.';
               else
                  szResult[ i ] = ',';
            }
            else
               szResult[ i ] = ' ';
         }
         else
            szResult[ i ] = cPic;
      }

      if( ( uiPicFlags & PF_PARNEG ) && ( dValue < 0 ) )
      {
         if( isdigit( ( int ) *szResult ) )          /* Overflow */
         {
            for( iCount = 1; ( ULONG ) iCount < i; iCount++ )
            {
               if( isdigit( ( int ) szResult[ iCount ] ) )
                  szResult[ iCount ] = '*';
            }
         }
         *szResult       = '(';
         szResult[ i++ ] = ')';
      }

      if( ( uiPicFlags & PF_CREDIT ) && ( dValue >= 0 ) )
      {
         szResult[ i++ ] = ' ';
         szResult[ i++ ] = 'C';
         szResult[ i++ ] = 'R';
      }

      if( ( uiPicFlags & PF_DEBIT ) && ( dValue < 0 ) )
      {
         szResult[ i++ ] = ' ';
         szResult[ i++ ] = 'D';
         szResult[ i++ ] = 'B';
      }

      *pulResultLen = i;
      szResult[ i ] = '\0';

      hb_xfree( szStr );
   }

   return szResult;
}

HB_FUNC( TRANSFORM )
{
   PHB_ITEM pExp = hb_param( 1, HB_IT_ANY ); /* Input parameter */
   PHB_ITEM pPic = hb_param( 2, HB_IT_STRING ); /* Picture string */

   BOOL bError = FALSE;

   if( pPic && hb_itemGetCLen( pPic ) > 0 )
   {
      char * szPic = hb_itemGetCPtr( pPic );
      ULONG  ulPicLen = hb_itemGetCLen( pPic );
      ULONG  ulPicStart;                     /* Start of template */
      USHORT uiPicFlags;                     /* Function flags */

      char * szResult;
      ULONG  ulResultPos;

      uiPicFlags = PictFunc( &szPic, &ulPicLen, &ulPicStart ); /* Evaluate picture string */

      if( HB_IS_STRING( pExp ) )
      {
         char * szExp = hb_itemGetCPtr( pExp );
         ULONG  ulExpLen = hb_itemGetCLen( pExp );
         ULONG  ulExpPos = 0;

         char szPicDate[ 11 ];
         BOOL bAnyPic = FALSE;

         /* Grab enough */
         szResult = ( char * ) hb_xgrab( ulExpLen + ulPicLen );
         ulResultPos = 0;

         /* [jlalin] */
         if( uiPicFlags & PF_STRING )
         {
            char * szPic2 = hb_itemGetCPtr( pPic );
            int ulLen = atol( szPic2 + 2 );

            if( ulLen & ( ulLen <= ulExpLen ) )
            {
               memcpy( szResult, szExp, ulLen );
               szResult[ ulLen ] = '\0';
               ulExpPos  = ulLen - 1;
            }

            if( uiPicFlags & PF_UPPER )
               hb_strupr( szResult );
         }

         /* Support date function for strings */
         if( uiPicFlags & PF_DATE )
         {
            hb_dateFormat( "XXXXXXXX", szPicDate,
               ( uiPicFlags & PF_BRITISH ) ?
                 ( hb_set.hb_set_century ? "DD/MM/YYYY" : "DD/MM/YY" ) :
                 hb_set.HB_SET_DATEFORMAT );

            szPic = szPicDate;
            ulPicLen = strlen( szPicDate );
            ulPicStart = 0;
         }

         /* Template string */
         if( ulPicLen )
         {
            while( ulPicLen && ulExpPos < ulExpLen ) /* Analyze picture mask */
            {
               switch( *szPic )
               {
                  /* Upper */
                  case '!':
                  {
                     szResult[ ulResultPos++ ] = toupper( szExp[ ulExpPos++ ] );
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
                     szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? toupper( szExp[ ulExpPos++ ] ) : szExp[ ulExpPos++ ];
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

               szPic++;
               ulPicLen--;
            }
         }
         else
         {
            while( ulExpPos++ < ulExpLen )
               szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? toupper( *szExp++ ) : *szExp++;
         }

         if( ( uiPicFlags & PF_REMAIN ) && ! bAnyPic )
         {
            while( ulExpPos++ < ulExpLen )
               szResult[ ulResultPos++ ] = ( uiPicFlags & PF_UPPER ) ? toupper( *szExp++ ) : *szExp++;
         }

         /* Any chars left ? */
         if( ( uiPicFlags & PF_REMAIN ) && ulPicLen )
         {
            /* Export remainder */
            while( ulPicLen-- )
               szResult[ ulResultPos++ ] = ' ';
         }
      }

      else if( HB_IS_NUMERIC( pExp ) )
      {
         int iOrigWidth;
         int iOrigDec;

         hb_itemGetNLen( pExp, &iOrigWidth, &iOrigDec );

         szResult = NumPicture( szPic, ulPicLen, &uiPicFlags,
            hb_itemGetND( pExp ), &ulResultPos, iOrigWidth, iOrigDec );
      }

      else if( HB_IS_DATE( pExp ) )
      {
         char szDate[ 9 ];

         szResult = ( char * ) hb_xgrab( 11 );

         hb_dateFormat( hb_itemGetDS( pExp, szDate ), szResult,
            ( uiPicFlags & PF_BRITISH ) ?
              ( hb_set.hb_set_century ? "DD/MM/YYYY" : "DD/MM/YY" ) :
              hb_set.HB_SET_DATEFORMAT );

         ulResultPos = strlen( szResult );
      }

      else if( HB_IS_LOGICAL( pExp ) )
      {
         BOOL bDone = FALSE;

         szResult = ( char * ) hb_xgrab( ulPicLen + 1 );
         ulResultPos = 1;

         if( ulPicLen )                      /* Template string          */
         {
            switch( *szPic )
            {
               case 'y':                     /* Yes/No                   */
               case 'Y':                     /* Yes/No                   */
               {
                  *szResult = hb_itemGetL( pExp ) ? 'Y' : 'N';
                  szPic++;
                  ulPicLen--;
                  bDone = TRUE;              /* Logical written          */
                  break;
               }

               case '#':
               case 'l':                     /* True/False               */
               case 'L':                     /* True/False               */
               {
                  *szResult = hb_itemGetL( pExp ) ? 'T' : 'F';
                  szPic++;
                  ulPicLen--;
                  bDone = TRUE;
                  break;
               }

               default:
               {
                  *szResult = *szPic++;
                  ulPicLen--;
               }
            }
         }
         else
            *szResult = hb_itemGetL( pExp ) ? 'T' : 'F';

         /* Any chars left */
         if( ( uiPicFlags & PF_REMAIN ) && ulPicLen )
         {
            /* Copy remainder */
            while( ulPicLen-- )
               szResult[ ulResultPos++ ] = *szPic++;

            /* Logical written ? */
            if( ! bDone )
               szResult[ ulResultPos++ ] = hb_itemGetL( pExp ) ? 'T' : 'F';
         }
      }
      else
      {
         szResult = NULL; /* To avoid GCC -O2 warning */
         bError = TRUE;
      }

      if( ! bError )
      {
         /* Trim left and pad with spaces */
         if( uiPicFlags & PF_LEFT )
         {
            ULONG ulFirstChar = 0;

            while( ulFirstChar < ulResultPos && szResult[ ulFirstChar ] == ' ' )
               ulFirstChar++;

            if( ulFirstChar < ulResultPos )
            {
               memmove( szResult, szResult + ulFirstChar, ulResultPos - ulFirstChar );
               memset( szResult + ulResultPos - ulFirstChar, ' ', ulFirstChar );
            }
         }

         if( uiPicFlags & PF_EMPTY )
            memset( szResult, ' ', ulResultPos );

         hb_retclen( szResult, ulResultPos );
         hb_xfree( szResult );
      }
   }
   else if( pPic || ISNIL( 2 ) ) /* Picture is an empty string or NIL */
   {
      if( HB_IS_STRING( pExp ) )
      {
         hb_itemReturn( pExp );
      }
      else if( HB_IS_NUMERIC( pExp ) )
      {
         char * szStr = hb_itemStr( pExp, NULL, NULL );

         if( szStr )
         {
            hb_retc( szStr );
            hb_xfree( szStr );
         }
         else
            hb_retc( "" );
      }
      else if( HB_IS_DATE( pExp ) )
      {
         char szDate[ 9 ];
         char szResult[ 11 ];

         hb_retc( hb_dateFormat( hb_itemGetDS( pExp, szDate ), szResult, hb_set.HB_SET_DATEFORMAT ) );
      }
      else if( HB_IS_LOGICAL( pExp ) )
      {
         hb_retc( hb_itemGetL( pExp ) ? "T" : "F" );
      }
      else
         bError = TRUE;
   }
   else
      bError = TRUE;

   /* If there was any parameter error, launch a runtime error */

   if( bError )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1122, NULL, "TRANSFORM" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}


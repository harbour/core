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
#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "dates.h"
#include "set.h"

/*                                                                          */
/*  Transform( xValue, cPicture )                                           */
/*                                                                          */
/*  Date : 29/04/1999                                                       */
/*                                                                          */

/* Function flags                                                           */

#define PF_LEFT    0x0001   /* @B */
#define PF_CREDIT  0x0002   /* @C */
#define PF_DEBIT   0x0004   /* @X */
#ifndef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
#define PF_ZERO    0x0008   /* @0 */
#endif
#define PF_PARNEG  0x0010   /* @( */
#define PF_REMAIN  0x0020   /* @R */
#define PF_UPPER   0x0040   /* @! */
#define PF_DATE    0x0080   /* @D */
#define PF_BRITISH 0x0100   /* @E */
#define PF_EXCHANG 0x0100   /* @E. Also means exchange . and , */
#define PF_EMPTY   0x0200   /* @Z */
#define PF_NUMDATE 0x0400   /* Internal flag. Ignore decimal dot            */

/* Date settings                                                            */

#define DF_CENTOFF   0
#define DF_CENTURY   1

#define DF_DMY 0
#define DF_MDY 1
#define DF_YMD 2
#define DF_EOT 3                                /* End of table for Century */

/*
   PictFunc -> Analyze function flags and return binary flags bits

   szPict   : Pointer to the picture
   ulPicLen : Pointer to the length.  Changed during execution.
*/
static USHORT PictFunc( char ** szPict, ULONG * pulPicLen )
{
   BOOL bDone = FALSE;
   USHORT uiPicFlags = 0;

   char * szPic = *szPict;

   HB_TRACE(HB_TR_DEBUG, ("PictFunc(%p, %p)", szPict, pulPicLen));

   szPic++;
   ( *pulPicLen )--;
   while( *pulPicLen && ! bDone )
   {
      switch( toupper( *szPic ) )
      {
         case ' ':                              /* End of function string   */
            bDone = TRUE;
            break;
         case '!':
            uiPicFlags |= PF_UPPER;
            break;
         case '(':
            uiPicFlags |= PF_PARNEG;
            break;
#ifndef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         case '0':
            uiPicFlags |= PF_ZERO;
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
         case 'X':
            uiPicFlags |= PF_DEBIT;
            break;
         case 'Z':
            uiPicFlags |= PF_EMPTY;
            break;
      }
      szPic++;
      ( *pulPicLen )--;
   }
   return uiPicFlags;
}

/*
    NumPicture -> Handle a numeric picture.

    szPic       : Picture
    lPic        : Length of picture
    uiPicFlags  : Function flags. NUM_DATE tells whether its a number or date
    dValue      : Number to picture
    lRetSize    : The size of the returned string is passed here !
    iOrigWidth  : Original width
    iOrigDec    : Original decimals
*/
static char * NumPicture( char * szPic, ULONG ulPic, USHORT uiPicFlags, double dValue,
                          ULONG * pulRetSize, int iOrigWidth, int iOrigDec )
{
   int      iWidth;                             /* Width of string          */
   int      iDec;                               /* Number of decimals       */
   ULONG    i;
   int      iCount = 0;

   char *   szRet;
   char *   szStr;
   char     cPic;

   PHB_ITEM pNumber;
   PHB_ITEM pWidth;
   PHB_ITEM pDec;

   BOOL     bFound = FALSE;
   BOOL     bEmpty;                             /* Suppress empty string    */

   double   dPush;

   HB_TRACE(HB_TR_DEBUG, ("NumPicture(%s, %lu, %hu, %lf)", szPic, ulPic, uiPicFlags, dValue));

   szRet  = ( char * ) hb_xgrab( ulPic + 4 );   /* Grab enough              */
   *szRet = '\0';
   for( i = 0; i < ulPic && !bFound; i++ )      /* Count number in front    */
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
      for( ; i < ulPic; i++ )
      {
         if( szPic[ i ] == '9' || szPic[ i ] == '#' ||
             szPic[ i ] == '$' || szPic[ i ] == '*' )
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

   bEmpty = !dPush && ( uiPicFlags & PF_EMPTY ); /* Suppress 0               */

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

#ifndef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      /* Pad with Zero's */
      if( uiPicFlags & PF_ZERO )
      {
         for( i = 0; szStr[ i ] == ' ' && i < iWidth; i++ )
            szStr[ i ] = '0';
      }
#endif

      /* Suppress empty value */
      if( bEmpty && strlen( szStr ) > 0 )
      {
         szStr[ strlen( szStr ) - 1 ] = ' ';
      }

      /* Left align */
      if( uiPicFlags & PF_LEFT )
      {
         for( i = 0; szStr[ i ] == ' ' && i <= iWidth; i++ );
                                                /* Find first non-space     */

         if( i && i != ( iWidth + 1 ) )         /* Any found or end of str  */
         {
            memcpy( szStr, szStr + i, iWidth - i );
            for( i = iWidth - i; i < iWidth; i++ )
               szStr[ i ] = ' ';                /* Pad with spaces          */
         }
      }

      /* TOFIX: iCount seem to always be zero at this point */

#if 0
      if( !iCount )                             /* No real picture          */
      {
         hb_xfree( szRet );
         szRet = ( char * ) hb_xgrab( iWidth + 1 );
                                                /* Grab enough              */
         memcpy( szRet, szStr, iWidth );
         szRet[ iWidth ] = 0;                   /* Terminate string         */
      }
      else
      {
#endif
         for( i = 0; i < ulPic; i++ )
         {
            cPic = szPic[ i ];
            if( cPic == '9' || cPic == '#' )
               szRet[ i ] = szStr[ iCount++ ];  /* Just copy                */
            else if( cPic == '.' )
            {
               if( uiPicFlags & PF_NUMDATE )     /* Dot in date              */
                  szRet[ i ] = cPic;
               else                             /* Dot in number            */
               {
                  if( uiPicFlags & PF_EXCHANG )  /* Exchange . and ,         */
                  {
                     szRet[ i ] = ',';
                     iCount++;
                  }
                  else
                     szRet[ i ] = szStr[ iCount++ ];
               }
            }
            else if( cPic == '$' || cPic == '*' )
            {
               if( szStr[ iCount ] == ' ' )
               {
                  szRet[ i ] = cPic;
                  iCount++;
               }
               else
                  szRet[ i ] = szStr[ iCount++ ];
            }
            else if( cPic == ',' )              /* Comma                    */
            {
               if( iCount && isdigit( szStr[ iCount - 1 ] ) )
               {                                /* May we place it     */
                  if( uiPicFlags & PF_EXCHANG )
                     szRet[ i ] = '.';
                  else
                     szRet[ i ] = ',';
               }
               else
                  szRet[ i ] = ' ';
            }
            else
               szRet[ i ] = cPic;
         }
#if 0
      }
#endif
      if( ( uiPicFlags & PF_CREDIT ) && ( dValue >= 0 ) )
      {
         szRet[ i++ ] = ' ';
         szRet[ i++ ] = 'C';
         szRet[ i++ ] = 'R';
      }

      if( ( uiPicFlags & PF_DEBIT ) && ( dValue < 0 ) )
      {
         szRet[ i++ ] = ' ';
         szRet[ i++ ] = 'D';
         szRet[ i++ ] = 'B';
      }

      if( ( uiPicFlags & PF_PARNEG ) && ( dValue < 0 ) )
      {
         if( isdigit( *szRet ) )                /* Overflow                 */
         {
            for( iCount = 1; iCount < i; iCount++ )
            {
               if( isdigit( szRet[ iCount ] ) )
                  szRet[ iCount ] = '*';
            }
         }
         *szRet     = '(';
         szRet[ i++ ] = ')';
      }

      *pulRetSize = i;
      szRet[ i ]  = '\0';

      hb_xfree( szStr );
   }

   return szRet;
}

/*
    DatePicture -> Handle dates.

    szDate      : Date to handle
    uiPicFlags  : Function flags
    szResult    : Buffer of at least size 11 to hold formatted date
*/
static char * DatePicture( char * szDate, USHORT uiPicFlags, char * szResult )
{
   HB_TRACE(HB_TR_DEBUG, ("DatePicture(%s, %hu, %s)", szDate, uiPicFlags, szResult));

   if( uiPicFlags & PF_BRITISH )
      hb_dtoc( szDate, szResult, hb_set.hb_set_century ? "DD/MM/YYYY" : "DD/MM/YY" );
   else
      hb_dtoc( szDate, szResult, hb_set.HB_SET_DATEFORMAT );

   return szResult;
}


HARBOUR HB_TRANSFORM( void )
{
   PHB_ITEM pExp = hb_param( 1, IT_ANY ); /* Input parameter */

   if( ISCHAR( 2 ) && hb_parclen( 2 ) > 0 )
   {
      PHB_ITEM pPic       = hb_param( 2, IT_STRING ); /* Picture string           */

      char    *szPic      = pPic->item.asString.value;
      char    *szTemp;
      char    *szResult;
      char    *szExp;

      ULONG   ulPic       = pPic->item.asString.length;
      ULONG   ulPicStart  = 0;                  /* Start of template        */
      ULONG   ulExpPos    = 0;
      ULONG   ulResultPos = 0;
      ULONG   n;

      USHORT  uiPicFlags  = 0;                  /* Function flags           */

      if( *szPic == '@' )                       /* Function marker found    */
      {
         uiPicFlags = PictFunc( &szPic, &ulPic ); /* Get length of function   */
         ulPicStart = pPic->item.asString.length - ulPic;
                                                /* Get start of template    */
      }

      switch( pExp->type & ~IT_BYREF )
      {
         case IT_STRING:
         {
            szExp = pExp->item.asString.value;
            szResult = ( char * ) hb_xgrab( ( ( ulPic-ulPicStart ) >
                       pExp->item.asString.length ) ?
                       ( ulPic - ulPicStart ) + 64 : pExp->item.asString.length + 64 );
                                                /* Grab enough              */
            szPic += ulPicStart;                /* Skip functions           */

            if( uiPicFlags & PF_UPPER )          /* Function : @!            */
            {
               szTemp = szExp;                  /* Convert to upper         */
               for( n = pExp->item.asString.length; n != 0; n-- )
               {
                  *szTemp = toupper( *szTemp );
                  szTemp++;
               }
            }

            if( ulPic )                         /* Template string          */
            {
               while( ulPic && ulExpPos < pExp->item.asString.length )
               {                                /* Analyze picture mask     */
                  switch( *szPic )
                  {
                     case '!':                  /* Upper                    */
                     {
                        szResult[ ulResultPos++ ] = toupper( szExp[ ulExpPos++ ] );
                        break;
                     }
                     case 'L':                  /* Ignored                  */
                     case 'Y':
                     case '*':
                     case '$':
                     case '.':
                     case ',':
                        break;

                     case '#':                  /* Out the character        */
                     case '9':
                     case 'A':
                     case 'N':
                     case 'X':
                     case ' ':
                     {
                        szResult[ ulResultPos++ ] = szExp[ ulExpPos++ ];
                        break;
                     }

                     default:                   /* Other choices            */
                     {
                        szResult[ ulResultPos++ ] = *szPic;
                        ulExpPos++;
                     }
                  }
                  szPic++;
                  ulPic--;
               }
            }
            else if( uiPicFlags & ( PF_UPPER + PF_REMAIN ) )
            {                                   /* Without template         */
               for( n = pExp->item.asString.length; n != 0; n-- )
                 szResult[ ulResultPos++ ] = *szExp++;
            }

            if( ( uiPicFlags & PF_REMAIN ) && ulPic )
            {                                   /* Any chars left           */
               for( n = ulPic; n != 0; n-- )
                  szResult[ ulResultPos++ ] = *szPic;
                                                /* Export remainder         */
            }
            hb_retclen( szResult, ulResultPos );
            hb_xfree( szResult );
            break;
         }

         case IT_LOGICAL:
         {
            BOOL bDone = FALSE;

            szResult    = ( char * ) hb_xgrab( ulPic + 1 );
                                                /* That's all folks        */
            szPic       += ulPicStart;          /* Skip functions           */
            ulResultPos =  1;

            if( ulPic )                         /* Template string          */
            {
               switch( *szPic )
               {
                  case 'Y':                     /* Yes/No                   */
                  {
                     *szResult = pExp->item.asLogical.value ? 'Y' : 'N';
                     szPic++;
                     ulPic--;
                     bDone = TRUE;              /* Logical written          */
                     break;
                  }

                  case '#':
                  case 'L':                     /* True/False               */
                  {
                     *szResult = pExp->item.asLogical.value ? 'T' : 'F';
                     szPic++;
                     ulPic--;
                     bDone = TRUE;
                     break;
                  }

                  default:
                  {
                    *szResult = *szPic++;
                    ulPic--;
                  }
               }
            }
            if( ( uiPicFlags & PF_REMAIN ) && ulPic )
            {                                   /* Any chars left           */
               for( n = ulPic; n; n--)          /* Copy remainder           */
                  szResult[ ulResultPos++ ] = *szPic++;
               if( !bDone )                     /* Logical written ?        */
                  szResult[ ulResultPos++ ] = pExp->item.asLogical.value ? 'T' : 'F';
            }
            hb_retclen( szResult, ulResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_INTEGER:
         {
            szResult = NumPicture( szPic + ulPicStart, ulPic, uiPicFlags,
                     ( double ) pExp->item.asInteger.value, &ulResultPos,
                     pExp->item.asInteger.length, 0 );
            hb_retclen( szResult, ulResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_LONG:
         {
            szResult = NumPicture( szPic + ulPicStart, ulPic, uiPicFlags,
                     ( double ) pExp->item.asLong.value, &ulResultPos,
                     pExp->item.asLong.length, 0 );
            hb_retclen( szResult, ulResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_DOUBLE:
         {
            szResult = NumPicture( szPic + ulPicStart, ulPic, uiPicFlags,
                     ( double ) pExp->item.asDouble.value, &ulResultPos,
                     pExp->item.asDouble.length, pExp->item.asDouble.decimal );
            hb_retclen( szResult, ulResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_DATE:
         {
            char szDate[ 9 ];
            char szResult[ 11 ];

            DatePicture( hb_pardsbuff( szDate, 1 ), uiPicFlags, szResult );
            hb_retc( szResult );
            break;
         }
         default:
            hb_errRT_BASE( EG_ARG, 1122, NULL, "TRANSFORM" );
      }
   }
   else if( ISCHAR( 2 ) || ISNIL( 2 ) )         /* No picture supplied      */
   {
      switch( pExp->type & ~IT_BYREF )          /* Default behaviour        */
      {
         case IT_STRING:
         {
            hb_retclen( pExp->item.asString.value, pExp->item.asString.length );
            break;
         }
         case IT_LOGICAL:
         {
            hb_retc( pExp->item.asLogical.value ? "T" : "F" );
            break;
         }
         case IT_INTEGER:
         case IT_LONG:
         case IT_DOUBLE:
         {
            char * szStr = hb_itemStr( pExp, NULL, NULL );

            if( szStr )
            {
               hb_retc( szStr );
               hb_xfree( szStr );
            }
            else
               hb_retc( "" );

            break;
         }
         case IT_DATE:
         {
            char szDate[ 9 ];
            char szResult[ 11 ];

            DatePicture( hb_pardsbuff( szDate, 1 ), 0, szResult );
            hb_retc( szResult );
            break;
         }
         default:
            hb_errRT_BASE( EG_ARG, 1122, NULL, "TRANSFORM" );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 1122, NULL, "TRANSFORM");
}


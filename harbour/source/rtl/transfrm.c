/*
 * $Id$
 */

/*
 * TransForm. Clipper transformation function
 *
 * Copyright (C) 1999 Eddie Runia (eddie@runia.com)
 * Part of the Harbour Project www.harbour-project.org
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
 * Partial Copyright Matthew Hamilton <mhamilton@bunge.com.au>
 *    partial copyright with regards to string handling
 */

/* TODO: Getting rid of calling back HARBOUR HB_STR() function */
/*       and #include "ctohard.h" */

#include <ctype.h>
#include "extend.h"
#include "init.h"
#include "ctoharb.h"
#include "itemapi.h"
#include "errorapi.h"
#include "dates.h"
#include "set.h"

/*                                                                          */
/*  Transform( xValue, cPicture )                                           */
/*                                                                          */
/*  Date : 29/04/1999                                                       */
/*                                                                          */

HARBOUR HB_TRANSFORM( void );


HB_INIT_SYMBOLS_BEGIN( Transfrm__InitSymbols )
{ "TRANSFORM" , FS_PUBLIC, HB_TRANSFORM  , 0 }
HB_INIT_SYMBOLS_END( Transfrm__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Transfrm__InitSymbols
#endif

/* Function flags                                                           */

#define PF_LEFT    0x0001   /* @B */
#define PF_CREDIT  0x0002   /* @C */
#define PF_DEBIT   0x0004   /* @X */
#define PF_ZERO    0x0008   /* @0 */
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

/* Multiplication factors for different formats. */

long lFactDay  [] = { 10000,   100,     1, 1000000,   10000,       1 };
long lFactMonth[] = {   100, 10000,   100,   10000, 1000000,     100 };
long lFactYear [] = {     1,     1, 10000,       1,       1,   10000 };

char *szBritish[]  = { "DD/MM/YY", "DD/MM/YYYY" }; /* For @E                */

/*
   PictFunc -> Analyze function flags and return binary flags bits

   szPict  : Pointer to the picture
   lPicLen : Pointer to the length.  Changed during execution.
*/
int PictFunc( char **szPict, long *lPicLen )
{
   int  bDone     = FALSE;
   int  iPicFlags = 0;

   char *szPic    = *szPict;

   szPic++;
   (*lPicLen)--;
   while( *lPicLen && !bDone )
   {
      switch( toupper(*szPic) )
      {
         case ' ':                              /* End of function string   */
            bDone = TRUE;
            break;
         case '!':
            iPicFlags |= PF_UPPER;
            break;
         case '(':
            iPicFlags |= PF_PARNEG;
            break;
         case '0':
            iPicFlags |= PF_ZERO;
            break;
         case 'B':
            iPicFlags |= PF_LEFT;
            break;
         case 'C':
            iPicFlags |= PF_CREDIT;
            break;
         case 'D':
            iPicFlags |= PF_DATE;
            break;
         case 'E':
            iPicFlags |= PF_BRITISH;
            break;
         case 'R':
            iPicFlags |= PF_REMAIN;
            break;
         case 'X':
            iPicFlags |= PF_DEBIT;
            break;
         case 'Z':
            iPicFlags |= PF_EMPTY;
            break;
      }
      szPic++;
      (*lPicLen)--;
   }
   return( iPicFlags );
}

/*
    NumPicture -> Handle a numeric picture.

    szPic       : Picture
    lPic        : Length of picture
    iPicFlags   : Function flags. NUM_DATE tells whether its a number or date
    dValue      : Number to picture
    lRetSize    : The size of the returned string is passed here !
    iOrigWidth  : Original width
    iOrigDec    : Original decimals
*/
char *NumPicture( char *szPic, long lPic, int iPicFlags, double dValue,
                  long *lRetSize, int iOrigWidth, int iOrigDec )
{
   int      iWidth;                             /* Width of string          */
   int      iDecimals;                          /* Number of decimals       */
   int      i;
   int      iCount = 0;

   char    *szRet;
   char    *szStr;
   char     cPic;

   PHB_ITEM pItem;

   BYTE     bFound = FALSE;
   BYTE     bEmpty;                             /* Suppress empty string    */

   double   dPush;

   szRet  = (char *) hb_xgrab( lPic+4 );        /* Grab enough              */
   *szRet = 0;
   for( i=0; i < lPic && !bFound; i++ )         /* Count number in front    */
   {
      if( szPic[i] == '.' )
         bFound = !( iPicFlags & PF_NUMDATE );  /* Exit when numeric        */
      else if( szPic[i] == '9' || szPic[i] == '#' ||
               szPic[i] == '$' || szPic[i] == '*' )
         iCount++;
   }
   iWidth = iCount;

   if( bFound )                                 /* Did we find a dot        */
   {
      iDecimals = 0;
      iWidth++;                                 /* Also adjust iWidth       */
      for( ; i<lPic; i++ )
      {
         if( szPic[i] == '9' || szPic[i] == '#' ||
             szPic[i] == '$' || szPic[i] == '*' )
         {
            iWidth++;
            iDecimals++;
         }
      }
   }
   else
      iDecimals = 0;

   if( ( iPicFlags & (PF_DEBIT + PF_PARNEG) ) && ( dValue < 0 ) )
      dPush = -dValue;                          /* Always push absolute val */
   else
      dPush = dValue;

   bEmpty = !dPush && ( iPicFlags & PF_EMPTY ); /* Suppress 0               */

   hb_vmPushSymbol ( hb_dynsymGet( "STR" )->pSymbol );  /* Push STR function        */
   hb_vmPushNil    ();                               /* Function call. No object */

   hb_vmPushDouble ( dPush, iDecimals );             /* Push value to transform  */
   if( !iWidth  )                               /* Width calculated ??      */
   {
      iWidth    = iOrigWidth;                   /* Push original width      */
      iDecimals = iOrigDec;                     /* Push original decimals   */
   }
   hb_vmPushInteger( iWidth );                       /* Push numbers width       */
   hb_vmPushInteger( iDecimals );                    /* Push decimals            */
   hb_vmFunction( 3 );                               /* 3 Parameters             */
   pItem = &stack.Return;
   if( IS_STRING( pItem ) )                     /* Is it a string           */
   {
      szStr  = pItem->item.asString.value;
      iCount = 0;

      if( iPicFlags & PF_ZERO )                 /* Pad with Zero's          */
      {
         for( i=0; szStr[i] == ' ' && i < iWidth; i++ )
            szStr[i] = '0';
      }
      if( bEmpty && pItem->item.asString.length )
                                                /* Suppress empty value     */
      {
         szStr[pItem->item.asString.length - 1] = ' ';
      }

      if( iPicFlags & PF_LEFT )                 /* Left align               */
      {
         for( i=0; szStr[i] == ' ' && i <= iWidth; i++ );
                                                /* Find first non-space     */

         if( i && i != ( iWidth + 1 ) )         /* Any found or end of str  */
         {
            memcpy( szStr, szStr + i, iWidth - i );
            for( i = iWidth - i; i < iWidth; i++ )
               szStr[i] = ' ';                  /* Pad with spaces          */
         }
      }

      if( !iCount )                             /* No real picture          */
      {
         hb_xfree( szRet );
         szRet = (char *) hb_xgrab( iWidth + 1 );
                                                /* Grab enough              */
         memcpy( szRet, szStr, iWidth );
         szRet[ iWidth ] = 0;                   /* Terminate string         */
      }
      else
      {
         for( i=0; i < lPic; i++ )
         {
            cPic = szPic[i];
            if( cPic == '9' || cPic == '#' )
               szRet[ i ] = szStr[ iCount++ ];  /* Just copy                */
            else if( cPic == '.' )
            {
               if( iPicFlags & PF_NUMDATE )     /* Dot in date              */
                  szRet[ i ] = cPic;
               else                             /* Dot in number            */
               {
                  if( iPicFlags & PF_EXCHANG )  /* Exchange . and ,         */
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
               if( szStr[iCount] == ' ' )
               {
                  szRet[i] = cPic;
                  iCount++;
               }
               else
                  szRet[i] = szStr[iCount++];
            }
            else if( cPic == ',' )              /* Comma                    */
            {
               if( iCount && isdigit( szStr[ iCount - 1 ] ) )
               {                                /* May we place it     */
                  if( iPicFlags & PF_EXCHANG )
                     szRet[i] = '.';
                  else
                     szRet[i] = ',';
               }
               else
                  szRet[i] = ' ';
            }
            else
               szRet[i] = cPic;
         }
      }
      if( ( iPicFlags & PF_CREDIT ) && ( dValue >= 0 ) )
      {
         szRet[i++] = ' ';
         szRet[i++] = 'C';
         szRet[i++] = 'R';
      }

      if( ( iPicFlags & PF_DEBIT ) && ( dValue < 0 ) )
      {
         szRet[i++] = ' ';
         szRet[i++] = 'D';
         szRet[i++] = 'B';
      }

      if( ( iPicFlags & PF_PARNEG ) && ( dValue < 0 ) )
      {
         if( isdigit(*szRet) )                  /* Overflow                 */
         {
            for( iCount = 1; iCount < i; iCount++ )
            {
               if( isdigit( szRet[iCount] ) )
                  szRet[iCount] = '*';
            }
         }
         *szRet     = '(';
         szRet[i++] = ')';
      }

      *lRetSize = i;
      szRet[i]  = 0;
   }
   else
   {
      printf( "\nNUMPICTURE: STR does not return string" );
      _exit(1);
   }
   return(szRet);
}

/*
    DatePicture -> Handle dates.

    szDate      : Date to handle
    iPicFlags   : Function flags
    szResult    : Buffer of at least size 11 to hold formatted date
    lRetSize    : The size of the returned string is passed here !
*/
char *DatePicture( char * szDate, int iPicFlags, char * szResult, long *lRetSize )
{
   char * szDateFormat;

   if( iPicFlags & PF_BRITISH )
   {
      szDateFormat = szBritish[ ( hb_set_century ? 1 : 0 ) ];
   }
   else
      szDateFormat = hb_set.HB_SET_DATEFORMAT;

   *lRetSize = strlen( hb_dtoc( szDate, szResult, szDateFormat ) );
   return( szResult );
}


HARBOUR HB_TRANSFORM( void )
{
   PHB_ITEM pPic      = hb_param( 2, IT_STRING);/* Picture string           */
   PHB_ITEM pExp      = hb_param( 1, IT_ANY );  /* Input parameter          */

   char    *szPic     = pPic->item.asString.value;
   char    *szTemp;
   char    *szResult;
   char    *szExp;


   long    lPic       = pPic->item.asString.length;
   long    lPicStart  = 0;                      /* Start of template        */
   long    lExpPos    = 0;
   long    lResultPos = 0;

   int     iPicFlags  = 0;                      /* Function flags           */
   int     n;

   BYTE    bDone      = FALSE;

   if( lPic )
   {
      if( *szPic == '@' )                       /* Function marker found    */
      {
         iPicFlags = PictFunc( &szPic, &lPic ); /* Get length of function   */
         lPicStart = pPic->item.asString.length - lPic;
                                                /* Get start of template    */
      }

      switch( pExp->type & ~IT_BYREF )
      {
         case IT_STRING:
         {
            szExp = pExp->item.asString.value;
            szResult = (char *)hb_xgrab( ( (ULONG) (lPic-lPicStart) >
                       pExp->item.asString.length) ?
                       (lPic-lPicStart) + 64 : pExp->item.asString.length + 64 );
                                                /* Grab enough              */
            szPic += lPicStart;                 /* Skip functions           */

            if( iPicFlags & PF_UPPER )          /* Function : @!            */
            {
               szTemp = szExp;                  /* Convert to upper         */
               for( n = pExp->item.asString.length; n ; n--)
               {
                  *szTemp = toupper( *szTemp );
                  szTemp++;
               }
            }

            if( lPic )                          /* Template string          */
            {
               while( lPic && (ULONG)lExpPos < pExp->item.asString.length )
               {                                /* Analyze picture mask     */
                  switch( *szPic )
                  {
                     case '!':                  /* Upper                    */
                     {
                        szResult[lResultPos++] = toupper(szExp[lExpPos++]);
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
                        szResult[lResultPos++] = szExp[lExpPos++];
                        break;
                     }

                     default:                   /* Other choices            */
                     {
                        szResult[lResultPos++] = *szPic;
                        lExpPos++;
                     }
                  }
                  szPic++;
                  lPic--;
               }
            }
            else if( iPicFlags & (PF_UPPER + PF_REMAIN) )
            {                                   /* Without template         */
               for( n = pExp->item.asString.length; n; n--)
                 szResult[lResultPos++] = *szExp++;
            }

            if( ( iPicFlags & PF_REMAIN ) && lPic )
            {                                   /* Any chars left           */
               for( n = lPic; n; n--)
                  szResult[lResultPos++] = *szPic;
                                                /* Export remainder         */
            }
            hb_retclen(szResult, lResultPos);
            hb_xfree(szResult);
            break;
         }

         case IT_LOGICAL:
         {
            szResult   =  (char *) hb_xgrab( lPic + 1 );
                                                /* That's all folks        */
            szPic      += lPicStart;            /* Skip functions           */
            lResultPos =  1;

            if( lPic )                          /* Template string          */
            {
               switch( *szPic )
               {
                  case 'Y':                     /* Yes/No                   */
                  {
                     *szResult = pExp->item.asLogical.value ? 'Y' : 'N';
                     szPic++;
                     lPic--;
                     bDone = TRUE;              /* Logical written          */
                     break;
                  }

                  case '#':
                  case 'L':                     /* True/False               */
                  {
                     *szResult = pExp->item.asLogical.value ? 'T' : 'F';
                     szPic++;
                     lPic--;
                     bDone = TRUE;
                     break;
                  }

                  default:
                  {
                    *szResult = *szPic++;
                    lPic--;
                  }
               }
            }
            if( ( iPicFlags & PF_REMAIN ) && lPic )
            {                                   /* Any chars left           */
               for( n = lPic; n; n--)           /* Copy remainder           */
                  szResult[lResultPos++] = *szPic++;
               if( !bDone )                     /* Logical written ?        */
                  szResult[lResultPos++] = pExp->item.asLogical.value ? 'T' : 'F';
            }
            hb_retclen( szResult, lResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_INTEGER:
         {
            szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                    (double) pExp->item.asInteger.value, &lResultPos,
                     pExp->item.asInteger.length, pExp->item.asInteger.decimal );
            hb_retclen( szResult, lResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_LONG:
         {
            szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                    (double) pExp->item.asLong.value, &lResultPos,
                     pExp->item.asLong.length, pExp->item.asLong.decimal );
            hb_retclen( szResult, lResultPos );
            hb_xfree( szResult );
            break;
         }
         case IT_DOUBLE:
         {
            szResult = NumPicture( szPic + lPicStart, lPic, iPicFlags,
                    (double) pExp->item.asDouble.value, &lResultPos,
                     pExp->item.asDouble.length, pExp->item.asDouble.decimal );
            hb_retclen( szResult, lResultPos);
            hb_xfree( szResult );
            break;
         }
         case IT_DATE:
         {
            char szResult[ 11 ];
            DatePicture( hb_pards( 1 ), iPicFlags, szResult, &lResultPos );
            hb_retclen( szResult, lResultPos );
            break;
         }
         default:
         {
            hb_errorRT_BASE(EG_ARG, 1122, NULL, "TRANSFORM");
         }
      }
   }
   else                                         /* No picture supplied      */
   {
      switch( pExp->type & ~IT_BYREF )          /* Default behaviour        */
      {
         case IT_STRING:
         {
            hb_retclen( pExp->item.asString.value, pExp->item.asString.length);
            break;
         }
         case IT_LOGICAL:
         {
            hb_retclen( pExp->item.asLogical.value ? "T" : "F", 1);
            break;
         }
         case IT_INTEGER:
         case IT_LONG:
         case IT_DOUBLE:
         {
            char * szStr = hb_itemStr(pExp, 0, 0);

            if (szStr)
            {
               hb_retc( szStr );
               hb_xfree( szStr );
            }
            else
               hb_retc("");

            break;
         }
         case IT_DATE:
         {
            char szResult[ 11 ];
            DatePicture( hb_pards( 1 ), iPicFlags, szResult, &lResultPos );
            hb_retclen( szResult, lResultPos );
            break;
         }
         default:
         {
           hb_errorRT_BASE(EG_ARG, 1122, NULL, "TRANSFORM");
         }
      }
   }
}



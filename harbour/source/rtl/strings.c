/*
 * $Id$

   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#include "hbsetup.h"
#include "dates.h"
#include "extend.h"
#include "init.h"
#include "itemapi.h"
#include "errorapi.h"
#include <ctype.h>
#include <math.h>
#include "set.h"

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
static double infinity = 0;
#endif

#define HB_ISSPACE(c) ((c) == 9 || (c) == 10 || (c) == 13 || (c) == 32)

/* DJGPP can sprintf a float that is almost 320 digits long */
#define HB_MAX_DOUBLE_LENGTH 320

HARBOUR HB_ALLTRIM( void );
HARBOUR HB_ASC( void );
HARBOUR HB_AT( void );
HARBOUR HB_CHR( void );
HARBOUR HB_ISALPHA( void );
HARBOUR HB_ISDIGIT( void );
HARBOUR HB_ISUPPER( void );
HARBOUR HB_ISLOWER( void );
HARBOUR HB_LEFT( void );
HARBOUR HB_LOWER( void );
HARBOUR HB_LTRIM( void );
HARBOUR HB_PAD( void );
HARBOUR HB_PADC( void );
HARBOUR HB_PADL( void );
HARBOUR HB_PADR( void );
HARBOUR HB_RAT( void );
HARBOUR HB_REPLICATE( void );
HARBOUR HB_RIGHT( void );
HARBOUR HB_RTRIM( void );
HARBOUR HB_SPACE( void );
HARBOUR HB_STR( void );
HARBOUR HB_STRTRAN( void );
HARBOUR HB_STUFF( void );
HARBOUR HB_SUBSTR( void );
HARBOUR HB_TRIM( void );
HARBOUR HB_UPPER( void );
HARBOUR HB_VAL( void );

HB_INIT_SYMBOLS_BEGIN( Strings__InitSymbols )
{ "ALLTRIM"      , FS_PUBLIC, HB_ALLTRIM    , 0 },
{ "ASC"          , FS_PUBLIC, HB_ASC        , 0 },
{ "AT"           , FS_PUBLIC, HB_AT         , 0 },
{ "CHR"          , FS_PUBLIC, HB_CHR        , 0 },
{ "ISALPHA"      , FS_PUBLIC, HB_ISALPHA    , 0 },
{ "ISDIGIT"      , FS_PUBLIC, HB_ISDIGIT    , 0 },
{ "ISUPPER"      , FS_PUBLIC, HB_ISUPPER    , 0 },
{ "ISLOWER"      , FS_PUBLIC, HB_ISLOWER    , 0 },
{ "LEFT"         , FS_PUBLIC, HB_LEFT       , 0 },
{ "LOWER"        , FS_PUBLIC, HB_LOWER      , 0 },
{ "LTRIM"        , FS_PUBLIC, HB_LTRIM      , 0 },
{ "PAD"          , FS_PUBLIC, HB_PAD        , 0 },
{ "PADC"         , FS_PUBLIC, HB_PADC       , 0 },
{ "PADL"         , FS_PUBLIC, HB_PADL       , 0 },
{ "PADR"         , FS_PUBLIC, HB_PADR       , 0 },
{ "RAT"          , FS_PUBLIC, HB_RAT        , 0 },
{ "REPLICATE"    , FS_PUBLIC, HB_REPLICATE  , 0 },
{ "RIGHT"        , FS_PUBLIC, HB_RIGHT      , 0 },
{ "RTRIM"        , FS_PUBLIC, HB_RTRIM      , 0 },
{ "SPACE"        , FS_PUBLIC, HB_SPACE      , 0 },
{ "STR"          , FS_PUBLIC, HB_STR        , 0 },
{ "STRTRAN"      , FS_PUBLIC, HB_STRTRAN    , 0 },
{ "STUFF"        , FS_PUBLIC, HB_STUFF      , 0 },
{ "SUBSTR"       , FS_PUBLIC, HB_SUBSTR     , 0 },
{ "TRIM"         , FS_PUBLIC, HB_TRIM       , 0 },
{ "UPPER"        , FS_PUBLIC, HB_UPPER      , 0 },
{ "VAL"          , FS_PUBLIC, HB_VAL        , 0 }
HB_INIT_SYMBOLS_END( Strings__InitSymbols )
#if ! defined(__GNUC__)
#pragma startup Strings__InitSymbols
#endif

/* The rest of functions is pulled automatically by initsymb.c */

#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
HB_CALL_ON_STARTUP_BEGIN( Strings_InitInfinity )
   infinity = -log( 0 );
HB_CALL_ON_STARTUP_END( Strings_InitInfinity )
#if ! defined(__GNUC__)
#pragma startup Strings_InitInfinity
#endif
#endif

BOOL hb_strempty( char * szText, ULONG ulLen )
{
   BOOL bRetVal = TRUE;

   while( ulLen-- )
   {
      char c = szText[ulLen];

      if( !HB_ISSPACE( c ) )
      {
         bRetVal = FALSE;
         break;
      }
   }

   return bRetVal;
}

/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following function is Copyright 1999 David G. Holm <dholm@jsd-llc.com>:
      hb_stricmp().
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

int hb_stricmp( const char *s1, const char *s2 )
{
   int rc = 0, c1, c2;
   USHORT l1, l2, count;
   l1 = strlen( s1 );
   l2 = strlen( s2 );
   if( l1 < l2 ) count = l1;
   else count = l2;
   while( rc == 0 && count > 0 )
   {
      count--;
      c1 = toupper( *s1++ );
      c2 = toupper( *s2++ );
      if( c1 != c2 ) rc = ( c1 < c2 ? -1 : 1 );
   }
   if( rc == 0 && l1 != l2 )
   {
      if( l1 < l2 ) rc = -1;
      else rc = 1;
   }
   return rc;
}


/* Harbour Project source code
   http://www.Harbour-Project.org/
   The following function is Copyright 1999 Victor Szel <info@szelvesz.hu>:
      hb_strMatchDOS().
   See doc/hdr_tpl.txt, Version 1.2 or later, for licensing terms.
*/

static BOOL  hb_strMatchDOS (char *pszString, char *pszMask)
{
   while (*pszMask && *pszString)
   {
      if (*pszMask == '*')
      {
         while (*pszMask == '*')
             pszMask++;
         if (!(*pszMask))
            return (TRUE);
         else
            if (*pszMask == '?')
               pszString++;
            else
            {
               while (toupper(*pszString) != toupper(*pszMask))
               {
                  if (!(*(++pszString)))
                     return (FALSE);
               }
               while (toupper(*pszString) == toupper(*pszMask))
               {
                  if (!(*(++pszString)))
                     break;
               }
               pszMask++;
            }
      }
      else
         if (toupper(*pszMask) != toupper(*pszString) && *pszMask != '?')
            return (FALSE);
         else
         {
            pszMask++;
            pszString++;
         }
   }
   return !((!(*pszString) && *pszMask && *pszMask != '*') ||
           (!(*pszMask) && *pszString));
}

/* TODO: Replace it with a code that supports real regular expressions
 *
 */
BOOL hb_strMatchRegExp( char *szString, char *szMask )
{
   return hb_strMatchDOS( szString, szMask );
}


/* determines if first char of string is letter */
/* TEST: QOUT( "isalpha( 'hello' ) = ", isalpha( 'hello' ) ) */
/* TEST: QOUT( "isalpha( '12345' ) = ", isalpha( '12345' ) ) */
HARBOUR HB_ISALPHA( void )
{
   hb_retl(isalpha(*hb_parc(1)));
}

/* determines if first char of string is digit */
/* TEST: QOUT( "isdigit( '12345' ) = ", isdigit( '12345' ) ) */
/* TEST: QOUT( "isdigit( 'abcde' ) = ", isdigit( 'abcde' ) ) */
HARBOUR HB_ISDIGIT( void )
{
   hb_retl(isdigit(*hb_parc(1)));
}

/* determines if first char of string is upper-case */
/* TEST: QOUT( "isupper( 'Abcde' ) = ", isupper( 'Abcde' ) ) */
/* TEST: QOUT( "isupper( 'abcde' ) = ", isupper( 'abcde' ) ) */
HARBOUR HB_ISUPPER( void )
{
   hb_retl(isupper(*hb_parc(1)));
}

/* determines if first char of string is lower-case */
/* TEST: QOUT( "islower( 'abcde' ) = ", islower( 'abcde' ) ) */
/* TEST: QOUT( "islower( 'Abcde' ) = ", islower( 'Abcde' ) ) */
HARBOUR HB_ISLOWER( void )
{
   hb_retl(islower(*hb_parc(1)));
}

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
char *hb_strLTrim( char *szText, ULONG *lLen )
{
   while( *lLen && HB_ISSPACE(*szText) )
   {
      szText++;
      (*lLen)--;
   }
   return szText;
}

/* trims leading spaces from a string */
/* TEST: QOUT( "ltrim( '  hello world  ' ) = '" + ltrim( '  hello world  ' ) + "'" ) */
HARBOUR HB_LTRIM( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         ULONG lLen = pText->item.asString.length;
         char *szText = hb_strLTrim(pText->item.asString.value, &lLen);

         hb_retclen(szText, lLen);
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1101, NULL, "LTRIM");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "LTRIM");
   }
}

/* returns szText and the new length in lLen */
ULONG hb_strRTrimLen( char *szText, ULONG lLen, BOOL bAnySpace )
{
   if( bAnySpace )
   {
      while( lLen && HB_ISSPACE(szText[lLen - 1]) )
         lLen--;
   }
   else
   {
      while( lLen && szText[lLen - 1] == ' ' )
         lLen--;
   }
   return lLen;
}


/* trims trailing spaces from a string */
/* TEST: QOUT( "rtrim( '  hello world  ' ) = '" + rtrim( '  hello world  ' ) + "'" ) */
HARBOUR HB_RTRIM( void )
{
   if( hb_pcount() > 0 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);
      if( pText )
      {
         BOOL bAnySpace = (hb_pcount() > 1? hb_parl(2): 0);
         hb_retclen(pText->item.asString.value, hb_strRTrimLen(pText->item.asString.value, pText->item.asString.length, bAnySpace));
      }
      else
      {
#ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
         /* Clipper doesn't error, but only in RTRIM. TRIM() throws an error, though */
         hb_retc("");
#else
         hb_errRT_BASE(EG_ARG, 1100, NULL, "RTRIM");
#endif
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "RTRIM");
   }
}

/* synonymn for RTRIM */
HARBOUR HB_TRIM( void )
{
   if( hb_pcount() > 0 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);
      if( pText )
      {
         BOOL bAnySpace = (hb_pcount() > 1? hb_parl(2): 0);
         hb_retclen(pText->item.asString.value, hb_strRTrimLen(pText->item.asString.value, pText->item.asString.length, bAnySpace));
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1100, NULL, "TRIM");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "TRIM");
   }
}

/* trims leading and trailing spaces from a string */
/* TEST: QOUT( "alltrim( '  hello world  ' ) = '" + alltrim( '  hello world  ' ) + "'" ) */
HARBOUR HB_ALLTRIM( void )
{
   if( hb_pcount() > 0 )
   {
      char *szText = hb_parc(1);
      BOOL bAnySpace = (hb_pcount() > 1? hb_parl(2): 0);
      ULONG lLen;

      lLen = hb_strRTrimLen(szText, hb_parclen(1), bAnySpace);

      szText = hb_strLTrim(szText, &lLen);

      hb_retclen(szText, lLen);
   }
   else
      /* Clipper doesn't error */
      hb_retc("");
}

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using hb_dtoc(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */
static char * hb_pad_prep( PHB_ITEM pItem, char * buffer, WORD * pwSize )
{
   char * szText = 0;

   if( pItem ) switch( pItem->type )
   {
      case IT_DATE:
         szText = hb_dtoc( hb_pards( 1 ), buffer, hb_set.HB_SET_DATEFORMAT );
         *pwSize = strlen( szText );
         break;
      case IT_INTEGER:
         sprintf( buffer, "%d", hb_parni( 1 ) );
         szText = buffer;
         *pwSize = strlen( szText );
         break;
      case IT_LONG:
         sprintf( buffer, "%ld", hb_parnl( 1 ) );
         szText = buffer;
         *pwSize = strlen( szText );
         break;
      case IT_DOUBLE:
         if( pItem->item.asDouble.decimal )
            sprintf( buffer, "%.*f", pItem->item.asDouble.decimal, hb_parnd( 1 ) );
         else
            sprintf( buffer, "%ld", hb_parnl( 1 ) );
         szText = buffer;
         *pwSize = strlen( szText );
         break;
      case IT_STRING:
         szText = hb_parc( 1 );
         *pwSize = hb_parclen( 1 );
         break;
   }
   return szText;
}

/* right-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padr( 'hello', 10 ) = '" + padr( 'hello', 10 ) + "'" ) */
HARBOUR HB_PADR( void )
{
   WORD wSize;
   char buffer[ 128 ];
   PHB_ITEM pItem = hb_param( 1, IT_ANY );
   char *szText = hb_pad_prep( pItem, buffer, &wSize );

   if( szText && hb_pcount() > 1 )
   {
      long lLen = hb_parnl(2);

      if( lLen > wSize )
      {
         char *szResult = (char *)hb_xgrab(lLen + 1);
         long lPos;
         char cPad;

         memcpy(szResult, szText, wSize);

         cPad = ( hb_pcount() > 2? *(hb_parc(3)): ' ' );

         for( lPos = wSize; lPos < lLen; lPos++ )
            szResult[lPos] = cPad;

         hb_retclen(szResult, lLen);
         hb_xfree(szResult);
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen(szText, lLen);
      }
   }
   else
      hb_retc("");
}

/* synonymn for PADR */
HARBOUR HB_PAD( void )
{
   HB_PADR();
}

/* left-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padl( 'hello', 10 ) = '" + padl( 'hello', 10 ) + "'" ) */
HARBOUR HB_PADL( void )
{
   WORD wSize;
   char buffer[ 128 ];
   PHB_ITEM pItem = hb_param( 1, IT_ANY );
   char *szText = hb_pad_prep( pItem, buffer, &wSize );

   if( szText && hb_pcount() > 1 )
   {
      long lLen = hb_parnl(2);

      if( lLen > wSize )
      {
         char *szResult = (char *)hb_xgrab(lLen + 1);
         long lPos = lLen - wSize;
         char cPad;

         memcpy(szResult + lPos, szText, wSize);

         cPad = (hb_pcount() > 2? *(hb_parc(3)): ' ');

         for(; lPos > 0; lPos--)
         {
            szResult[lPos - 1] = cPad;
         }

         hb_retclen(szResult, lLen);
         hb_xfree(szResult);
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen(szText, lLen);
      }
   }
   else
      hb_retc("");
}

/* centre-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padc( 'hello', 10 ) = '" + padc( 'hello', 10 ) + "'" ) */
HARBOUR HB_PADC( void )
{
   WORD wSize;
   char buffer[ 128 ];
   PHB_ITEM pItem = hb_param( 1, IT_ANY );
   char *szText = hb_pad_prep( pItem, buffer, &wSize );

   if( szText && hb_pcount() > 1 )
   {
      long lLen = hb_parnl(2);

      if( lLen > wSize )
      {
         char *szResult = (char *)hb_xgrab(lLen + 1);
         char cPad;
         long w, lPos = (lLen - wSize) / 2;

         memcpy(szResult + lPos, szText, wSize + 1);

         cPad = ( hb_pcount() > 2? *hb_parc(3): ' ' );

         for( w = 0; w < lPos; w++ )
            szResult[w] = cPad;

         for( w = wSize + lPos; w < lLen; w++ )
            szResult[w] = cPad;

         szResult[lLen] = 0;

         hb_retclen(szResult, lLen);
         hb_xfree(szResult);
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen(szText, lLen);
      }
   }
   else
      hb_retc("");
}

ULONG hb_strAt(char *szSub, long lSubLen, char *szText, long lLen)
{
   if( lSubLen )
   {
      if( lLen >= lSubLen )
      {
         long lPos = 0, lSubPos = 0;

         while( lPos < lLen && lSubPos < lSubLen )
         {
            if( *(szText + lPos) == *(szSub + lSubPos) )
            {
               lSubPos++;
               lPos++;
            }
            else if( lSubPos )
               lSubPos = 0;
            else
               lPos++;
         }
         return (lSubPos < lSubLen? 0: lPos - lSubLen + 1);
      }
      else
         return 0;
   }
   else
      return 1;
}

/* locates a substring in a string */
/* TEST: QOUT( "at( 'cde', 'abcdefgfedcba' ) = '" + at( 'cde', 'abcsefgfedcba' ) + "'" ) */
HARBOUR HB_AT( void )
{
   PHB_ITEM pSub = hb_param(1, IT_ANY);
   PHB_ITEM pText = hb_param(2, IT_ANY);

   if( pText && pSub )
   {
      if( IS_STRING( pText ) && IS_STRING( pSub ) )
      {
         hb_retnl( hb_strAt(pSub->item.asString.value, pSub->item.asString.length, pText->item.asString.value, pText->item.asString.length) );
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1108, NULL, "AT");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "AT");
   }
}

/* locates a substring in a string starting at the end */
/* TEST: QOUT( "rat( 'cde', 'abcdefgfedcba' ) = '" + rat( 'cde', 'abcdefgfedcba' ) + "'" ) */
HARBOUR HB_RAT( void )
{
   long lSubLen = hb_parclen(1);

   if( lSubLen )
   {
      long lPos = hb_parclen(2) - lSubLen;
      if( lPos < 0 )
         hb_retni(0);
      else
      {
         char *szSub = hb_parc(1);
         char *szText = hb_parc(2);
         int bFound = 0;

         while( lPos >= 0 && !bFound )
         {
            if( *(szText + lPos) == *szSub )
               bFound = !memcmp(szSub, szText + lPos, lSubLen);
            lPos--;
         }
         hb_retnl( bFound? lPos + 2: 0 );
      }
   }
   else
      /* This function never seems to raise an error */
      hb_retni(0);
}

/* converts an ASCII code to a character value */
HARBOUR HB_CHR( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pAsc = hb_param(1, IT_NUMERIC);

      if( pAsc )
      {
         char chr[2];

         /* Believe it or not, clipper does this! */
         chr[0] = hb_parnl(1) % 256;
         chr[1] = 0;
         hb_retclen(chr, 1);
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1104, NULL, "CHR");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "CHR");
   }
}

/* converts a character value to an ASCII code */
HARBOUR HB_ASC(void)
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         if( pText->item.asString.length > 0 )
            hb_retni((BYTE)*(pText->item.asString.value));
         else
            hb_retni(0);
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1107, NULL, "ASC");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "ASC");
   }
}

/* returns the left-most n characters in string */
HARBOUR HB_LEFT( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         PHB_ITEM pLen = hb_param(2, IT_NUMERIC);

         if( pLen )
         {
            LONG lLen = hb_parnl(2);

            if( lLen > (LONG)pText->item.asString.length )
               lLen = (LONG)pText->item.asString.length;

            else if( lLen < 0 )
               lLen = 0;

            hb_retclen(pText->item.asString.value, lLen);
         }
         else
         {
            hb_errRT_BASE(EG_ARG, 3009, NULL, "LEFT");
         }
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1124, NULL, "LEFT");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "LEFT");
   }
}

/* returns the right-most n characters in string */
HARBOUR HB_RIGHT( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         PHB_ITEM pLen = hb_param(2, IT_NUMERIC);

         if( pLen )
         {
            LONG lLen = hb_parnl(2);

            if( lLen > (LONG)pText->item.asString.length )
               lLen = (LONG)pText->item.asString.length;

            else if( lLen < 0 )
               lLen = 0;

            hb_retclen(pText->item.asString.value + pText->item.asString.length - lLen, lLen);
         }
         else
         {
            /* Clipper doesn't error */
            hb_retc("");
         }
      }
      else
      {
         /* Clipper doesn't error */
         hb_retc("");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      /* Clipper doesn't error */
      hb_retc("");
   }
}

/* returns l characters from n characters into string */
HARBOUR HB_SUBSTR( void )
{
   if( hb_pcount() > 1 && hb_pcount() < 4 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);
      PHB_ITEM pPos = hb_param(2, IT_NUMERIC);

      if( pText && pPos )
      {
         LONG lPos = hb_parnl(2);

         if( lPos < 0 )
         {
            lPos += (LONG)pText->item.asString.length;
            if( lPos < 0 )
               lPos = 0;
         }
         else if( lPos )
         {
            lPos--;
         }

         if( lPos < (LONG)pText->item.asString.length )
         {
            PHB_ITEM pLen = hb_param(3, IT_NUMERIC);
            LONG lLen;

            if( pLen )
            {
               lLen = hb_parnl(3);

               if( lLen > (LONG)pText->item.asString.length - lPos )
                  lLen = (LONG)pText->item.asString.length - lPos;
            }
            else
               lLen = (LONG)pText->item.asString.length - lPos;

            if( lLen > 0 )
               hb_retclen(pText->item.asString.value + lPos, lLen);
            else
               hb_retc("");
         }
         else
            hb_retc("");
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1110, NULL, "SUBSTR");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "SUBSTR");
   }
}

/* converts szText to lower case. Does not create a new string! */
char *hb_strLower(char *szText, long lLen)
{
   long i;
   for( i = 0; i < lLen; i++ )
      szText[i] = tolower(szText[i]);
   return szText;
}

/* converts string to lower case */
HARBOUR HB_LOWER( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         ULONG ulLen = pText->item.asString.length;

         hb_retclen(hb_strLower(pText->item.asString.value, ulLen), ulLen);
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1103, NULL, "LOWER");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "LOWER");
   }
}

/* converts szText to upper case. Does not create a new string! */
char *hb_strUpper(char *szText, long lLen)
{
   long i;
   for( i = 0; i < lLen; i++ )
      szText[i] = toupper(szText[i]);
   return szText;
}

/* converts string to upper case */
HARBOUR HB_UPPER( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         ULONG ulLen = pText->item.asString.length;

         hb_retclen(hb_strUpper(pText->item.asString.value, ulLen), ulLen);
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1102, NULL, "UPPER");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "UPPER");
   }
}

/* returns n copies of given string */
/* TEST: QOUT( "replicate( 'abc', 5 ) = " + replicate( 'abc', 5 ) ) */
HARBOUR HB_REPLICATE( void )
{
   if( hb_pcount() == 2 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);
      PHB_ITEM pTimes = hb_param(2, IT_NUMERIC);

      if( pText && pTimes )
      {
         long lTimes = hb_parnl(2);

         if( lTimes > 0 )
         {
            char *szText = pText->item.asString.value;
            long lLen = pText->item.asString.length;
            char *szResult = (char *)hb_xgrab((lLen * lTimes) + 1);
            char *szPtr = szResult;
            long i;

            for( i = 0; i < lTimes; i++ )
            {
               memcpy(szPtr, szText, lLen);
               szPtr += lLen;
            }

            /* TODO: Check for string overflow */
            /* hb_errRT_BASE(EG_STROVERFLOW, 1234, NULL, "REPLICATE"); */

            hb_retclen(szResult, lLen * lTimes);
            hb_xfree(szResult);
         }
         else
            hb_retc("");
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1106, NULL, "REPLICATE");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "REPLICATE");
   }
}

/* returns n copies of a single space */
/* TEST: QOUT( "space( 5 ) = '" + space( 5 ) + "'" ) */
HARBOUR HB_SPACE( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pLen = hb_param(1, IT_NUMERIC);

      if( pLen )
      {
         long lLen = hb_parnl(1);

         if( lLen > 0 )
         {
            char *szResult = (char *)hb_xgrab(lLen + 1);

            /* TODO: Check for string overflow */
            /* hb_errRT_BASE(EG_STROVERFLOW, 1233, NULL, "SPACE"); */

            memset(szResult, ' ', lLen);
            hb_retclen(szResult, lLen);
            hb_xfree(szResult);
         }
         else
            hb_retc("");
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1105, NULL, "SPACE");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "SPACE");
   }
}

/* replaces characters in a string */
HARBOUR HB_STUFF( void )
{
   PHB_ITEM pText;

   pText = hb_param(1, IT_STRING);
   if( pText )
   {
      char *szText = pText->item.asString.value;
      PHB_ITEM pPos, pDel, pInsert;
      ULONG lPos, lDel, lInsert, lTotalLen;
      char *szInsert;

      pPos = hb_param(2, IT_NUMERIC);
      lPos = (pPos? hb_itemGetNL( pPos ) - 1: 0);
      if( lPos > pText->item.asString.length )
         lPos = pText->item.asString.length;

      pDel = hb_param(3, IT_NUMERIC);
      if( pDel )
      {
         lDel = hb_itemGetNL( pDel );
         if( lDel > pText->item.asString.length - lPos )
            lDel = pText->item.asString.length - lPos;
      }
      else
         lDel = 0;

      pInsert = hb_param(4, IT_STRING);
      if( pInsert )
      {
         szInsert = pInsert->item.asString.value;
         lInsert = pInsert->item.asString.length;
      }
      else
      {
         szInsert = ""; /* shouldn't matter that we don't allocate */
         lInsert = 0;
      }

      if( (lTotalLen = pText->item.asString.length + lInsert - lDel) > 0 )
      {
         char *szResult = (char *)hb_xgrab(lTotalLen + 1);

         memcpy(szResult, szText, lPos);
         memcpy(szResult + lPos, szInsert, lInsert);
         memcpy(szResult + lPos + lInsert, szText + lPos + lDel,
                pText->item.asString.length - (lPos + lDel));

         szResult[lTotalLen] = 0;
         hb_retclen(szResult, lTotalLen);
         hb_xfree(szResult);
      }
      else
         hb_retc("");
   }
   else
      hb_retc("");
}

/* replaces lots of characters in a string */
HARBOUR HB_STRTRAN( void )
{
   PHB_ITEM pText = hb_param(1, IT_STRING);

   if( pText )
   {
      PHB_ITEM pSeek = hb_param(2, IT_STRING);
      if( pSeek )
      {
         char *szText = pText->item.asString.value;
         if( pSeek->item.asString.length && pSeek->item.asString.length <= pText->item.asString.length )
         {
            char *szSeek = pSeek->item.asString.value;
            PHB_ITEM pStart = hb_param(4, IT_NUMERIC);
            char *szReplace;
            ULONG iStart;

            iStart = (pStart? hb_parnl(4): 1);
            if( !iStart )
            {
               /* Clipper seems to work this way */
               hb_retc("");
            }
            else if( iStart > 0 )
            {
               PHB_ITEM pReplace = hb_param(3, IT_STRING);
               PHB_ITEM pCount = hb_param(5, IT_NUMERIC);
               ULONG iReplace;
               ULONG iCount;
               long bAll;

               if( pReplace )
               {
                  szReplace = pReplace->item.asString.value;
                  iReplace = pReplace->item.asString.length;
               }
               else
               {
                  szReplace = ""; /* shouldn't matter that we don't allocate */
                  iReplace = 0;
               }

               if( pCount )
               {
                  iCount = hb_itemGetNL( pCount );
                  bAll = 0;
               }
               else
               {
                  iCount = 0;
                  bAll = 1;
               }

               if( bAll || iCount > 0 )
               {
                  long iFound = 0;
                  long iReplaced = 0;
                  ULONG i = 0;
                  ULONG iLength = pText->item.asString.length;

                  while( i < pText->item.asString.length )
                  {
                     if( (bAll || iReplaced < iCount) && !memcmp(szText + i, szSeek, pSeek->item.asString.length) )
                     {
                        iFound++;
                        if( iFound >= iStart )
                        {
                           iReplaced++;
                           iLength = iLength - pSeek->item.asString.length + iReplace;
                           i += pSeek->item.asString.length;
                        }
                        else
                           i++;
                     }
                     else
                        i++;
                  }

                  if( iFound )
                  {
                     char *szResult = (char *)hb_xgrab(iLength + 1);
                     char *szPtr = szResult;

                     iFound = 0;
                     i = 0;
                     while( i < pText->item.asString.length )
                     {
                        if( iReplaced && !memcmp(szText + i, szSeek, pSeek->item.asString.length) )
                        {
                           iFound++;
                           if( iFound >= iStart )
                           {
                              iReplaced--;
                              memcpy(szPtr, szReplace, iReplace);
                              szPtr += iReplace;
                              i += pSeek->item.asString.length;
                           }
                           else
                           {
                              *szPtr = szText[i];
                              szPtr++;
                              i++;
                           }
                        }
                        else
                        {
                           *szPtr = szText[i];
                           szPtr++;
                           i++;
                        }
                     }
                     hb_retclen(szResult, iLength);
                     hb_xfree(szResult);
                  }
                  else
                     hb_retclen(szText, pText->item.asString.length);
               }
                else
                  hb_retclen(szText, pText->item.asString.length);
            }
            else
               hb_retclen(szText, pText->item.asString.length);
         }
         else
            hb_retclen(szText, pText->item.asString.length);
      }
      else
         hb_errRT_BASE(EG_ARG, 3010, NULL, "STRTRAN");
   }
   else
      hb_errRT_BASE(EG_ARG, 1126, NULL, "STRTRAN");
}

/* returns the numeric value of a character string representation of a number  */
double hb_strVal( char *szText )
{
   return atof(szText);
}

/* returns the numeric value of a character string representation of a number  */
HARBOUR HB_VAL( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pText = hb_param(1, IT_STRING);

      if( pText )
      {
         int nWidth, nDec = 0;
         char * ptr = strchr( pText->item.asString.value, '.' );
         if( ptr )
         {
            nWidth = ptr - pText->item.asString.value;
            nDec = strlen( ptr + 1 );
         }
         else nWidth = strlen( pText->item.asString.value );
         hb_retnd(hb_strVal(pText->item.asString.value));
         stack.Return.item.asDouble.length = nWidth;
         stack.Return.item.asDouble.decimal    = nDec;
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1098, NULL, "VAL");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "VAL");
   }
}

/* converts a numeric to a string with optional width & precision.
   This function should be used by any function that wants to format numeric
   data for displaying, printing, or putting in a database.

   Note: The caller is responsible for calling hb_xfree to free the results buffer,
         but ONLY if the return value is not a NULL pointer!
*/
/* TODO: Move it to itemapi.c */
char * hb_itemStr( PHB_ITEM pNumber, PHB_ITEM pWidth, PHB_ITEM pDec )
{
   char * szResult = 0;

   if( pNumber )
   {
      /* Default to the width and number of decimals specified by the item,
         with a limit of 20 integer places and 9 decimal places */
      int iWidth = pNumber->item.asDouble.length;
      int iDec   = pNumber->item.asDouble.decimal;
      if( iWidth > 20 )
         iWidth = 20;
      if( iDec > 9 )
         iDec = 9;
      if( hb_set.HB_SET_FIXED )
         iDec = hb_set.HB_SET_DECIMALS;

      if( pWidth )
      {
         /* If the width parameter is specified, override the default value
            and set the number of decimals to zero */
         iWidth =(int) hb_itemGetNL( pWidth );

         if( iWidth < 1 )
            iWidth = 10;                   /* If 0 or negative, use default */
         iDec = 0;
      }

      if( pDec )
      {
         /* This function does not include the decimal places in the width,
            so the width must be adjusted downwards, if the decimal places
            parameter is greater than 0  */
         iDec =(int) hb_itemGetNL( pDec );

         if( iDec < 0 )
            iDec = 0;
         else if( iDec > 0 )
            iWidth -= (iDec + 1);
      }

      if( iWidth )
      {
         /* We at least have a width value */
         int iBytes;
         int iSize = (iDec ? iWidth + 1 + iDec : iWidth);

         /* Be paranoid and use a large amount of padding */
         szResult = (char *)hb_xgrab( HB_MAX_DOUBLE_LENGTH );

         if( IS_DOUBLE( pNumber ) || iDec != 0 )
         {
            double dNumber =hb_itemGetND( pNumber );

            #ifdef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
            if( pNumber->item.asDouble.length == 99 || dNumber == infinity || dNumber == -infinity )
               /* Numeric overflow */
               iBytes = iSize + 1;
            else
            #endif
            {
               if( iDec > 0 )
                  iBytes = sprintf( szResult, "%*.*f", iSize, iDec, dNumber );
               else
                  iBytes = sprintf( szResult, "%*ld", iWidth, (long)dNumber );
            }
         }
         else switch( pNumber->type & ~IT_BYREF )
         {
            case IT_LONG:
                 iBytes = sprintf( szResult, "%*li", iWidth, pNumber->item.asLong.value );
                 break;

            case IT_INTEGER:
                 iBytes = sprintf( szResult, "%*i", iWidth, pNumber->item.asInteger.value );
                 break;

            default:
                 iBytes = 0;
                 *szResult = 0;
                 break;
         }
         /* Set to asterisks in case of overflow */
         if( iBytes > iSize )
         {
            memset( szResult, '*', iSize );
            szResult[ iSize ] = 0;
         }
      }
   }
   return( szResult );
}

/* converts a numeric to a string with optional width & precision.
   calls hb_itemStr() after validating parameters
*/
HARBOUR HB_STR( void )
{
   if( hb_pcount() > 0 && hb_pcount() < 4 )
   {
      BOOL bValid = TRUE;
      PHB_ITEM pNumber = hb_param( 1, IT_NUMERIC );
      PHB_ITEM pWidth  = 0;
      PHB_ITEM pDec    = 0;

      if( !pNumber )
         bValid = FALSE;
      else
      {
         if( hb_pcount() > 1 )
         {
            pWidth = hb_param( 2, IT_NUMERIC );
            if( !pWidth)
               bValid = FALSE;
         }
         if( hb_pcount() > 2 )
         {
            pDec = hb_param( 3, IT_NUMERIC );
            if( !pDec )
               bValid = FALSE;
         }
      }
      if( bValid )
      {
         char * szResult = hb_itemStr( pNumber, pWidth, pDec );

         if( szResult )
         {
            hb_retc( szResult );
            hb_xfree( szResult );
         }
         else hb_retc( "" );
      }
      else
      {
         hb_errRT_BASE(EG_ARG, 1099, NULL, "STR");
      }
   }
   else
   {
      /* QUESTION: Clipper catches this at compile time! */
      hb_errRT_BASE(EG_ARGCOUNT, 3000, NULL, "STR");
   }
}

/* Values returned : HB_STRGREATER_EQUAL, HB_STRGREATER_LEFT, HB_STRGREATER_RIGHT */

WORD hb_strgreater( char * sz1, char * sz2 )
{

   while( *( sz1 ) && *( sz2 ) && *( sz1 ) == *( sz2 ) )
   {
     sz1++;
     sz2++;
   }
   if ( ( *( sz1 ) == 0 && *( sz2 ) != 0 ) ||
        ( *( sz2 ) > *( sz1 ) )               )
      return HB_STRGREATER_RIGHT;

   if ( ( *( sz1 ) != 0 && *( sz2 ) == 0 ) ||
        ( *( sz1 ) > *( sz2 ) )               )
      return HB_STRGREATER_LEFT;

   return HB_STRGREATER_EQUAL;
}

void hb_strupr( char * szText )
{
   char *p;

   for( p = szText; *p; p++ )
      *p = toupper( *p );
}

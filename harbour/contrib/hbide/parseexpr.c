/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Andy Wos
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

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                       Code Forwarded by Andy Wos
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               22Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbapiitm.h"

/*----------------------------------------------------------------------*/

static HB_ISIZ ide_linearfind( const char ** array, const char * pszText, HB_ISIZ lenarray, HB_ISIZ lentext, HB_BOOL bMatchCase )
{
   HB_ISIZ i;

   if( bMatchCase )
   {
      for( i = 0; i < lenarray; i++ )
      {
         if( strncmp( pszText, array[ i ], lentext + 1 )  == 0 )
            return i + 1;
      }
   }
   else
   {
      for( i = 0; i < lenarray; i++ )
      {
         if( hb_strnicmp( pszText, array[ i ], lentext + 1 ) == 0 )
            return i + 1;
      }
   }
   return 0;
}

/*----------------------------------------------------------------------*/

static HB_BOOL ide_strempty( const char * pszString )
{
   HB_ISIZ i = 0;

   while( pszString[ i ] != 0 )
   {
      if( pszString[ i++ ] != ' ' )
         return HB_FALSE;
   }

   return HB_TRUE;
}

/*----------------------------------------------------------------------*/

static HB_ISIZ ide_atbuff( const char * pszChars, const char * pszString, HB_ISIZ StartFrom, HB_ISIZ Target, HB_ISIZ len_chars, HB_ISIZ len )
{
   if( len >= len_chars && StartFrom <= len - len_chars )
   {
      HB_ISIZ x;
      HB_ISIZ counter = 0;

      for( x = StartFrom; x <= ( len - len_chars ); x++ )
      {
         if( strncmp( pszString + x, pszChars, len_chars ) == 0 )
         {
            if( ++counter == Target )
               return x + 1;
         }
      }
   }
   return 0;
}

/*----------------------------------------------------------------------*/

static HB_ISIZ ide_getword( const char * pszText, HB_BOOL bHonorSpacing, char * pszWord, HB_ISIZ * pnpos )
{
   static const char s_szGood[]         = "''_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890.";
   static const char * s_szDoubleList[] = { "*/", "/*", "//", "->", "::", "||", "++", "--", "**", ":=",
                                            "<=", ">=", "<>", "!=", "==", "+=", "-=", "*=", "/=", "%=",
                                            "^=", "&&", "^^", ">>", "<<", "=>", "&=", "|=" };

   static HB_ISIZ s_lengood   = HB_SIZEOFARRAY( s_szGood ) - 1;
   static HB_ISIZ s_lendouble = HB_SIZEOFARRAY( s_szDoubleList );

   HB_ISIZ maxlen = strlen( pszText );
   HB_ISIZ npos = 0;
   HB_ISIZ wordlen = 0;

   if( maxlen > 0 )
   {
      char temp;
      char ch;
      char szSingle[ 2 ];
      char szDouble[ 3 ];

      szSingle[ 1 ] = '\0';
      szDouble[ 2 ] = '\0';

      ch = pszText[ 0 ];

      if( ch == ',' ) /* lists */
      {
         pszWord[ wordlen++ ] = ch;
         npos++;
      }
      else /* literals */
      {
         if( ch == '"' || ch == '\'' )
         {
            temp = ch;
            pszWord[ wordlen++ ] = ch;
            npos++;
            ch = ' ';
            while( npos < maxlen && ch != temp )
            {
               ch = pszText[ npos ];
               pszWord[ wordlen++ ] = ch;
               npos++;
            }
         }
         else
         {
            szSingle[ 0 ] = ch;
            if( ide_atbuff( szSingle, s_szGood, 0, 1, 1, s_lengood ) ) /* ch $ s_szGood ) // variables, commands, function names */
            {
               while( npos < maxlen && ide_atbuff( szSingle, s_szGood, 0, 1, 1, s_lengood ) )
               {
                  pszWord[ wordlen++ ] = ch;
                  npos++;
                  ch = pszText[ npos ];
                  szSingle[ 0 ] = ch;
               }
            }
            else if( ch == ' ' )
            {
               while( npos < maxlen && ch == ' ' )
               {
                  pszWord[ wordlen++ ] = ch;
                  npos++;
                  ch = pszText[ npos ];
               }

               if( ! bHonorSpacing )
               {
                  pszWord[ 0 ] = ' '; /* reduce spaces to 1 */
                  wordlen = 1;
               }
            }
            else  /* operators, punctuation */
            {
               pszWord[ wordlen++ ]= ch;
               npos++;
               ch = pszText[ npos ];
               if( maxlen > npos )
               {
                  szDouble[ 0 ] = pszWord[ 0 ];
                  szDouble[ 1 ] = ch;
                  if( ide_linearfind( s_szDoubleList, szDouble, s_lendouble, 2, HB_TRUE ) )  /* if( (pszWord + ch) $ s_szDoubleList) //aScan( s_szDoubleList, pszWord + ch ) > 0 */
                  {
                     pszWord[ wordlen++ ] = ch;
                     npos++;
                  }
               }
            }
         }
      }
   }

   pszWord[ wordlen ] = '\0';
   *pnpos = npos;

   return wordlen;
}

/*----------------------------------------------------------------------*/

HB_FUNC( PARSEXPR ) /* ( c, bHonorSpacing, bInRemark, bUpperKeyWord, bKeepComments, bPRG, bKeepSpaces ) */
{
   const char * pszExpr = hb_parcx( 1 );

   PHB_ITEM paExpr = hb_itemArrayNew( 0 );
   PHB_ITEM pTemp = hb_itemNew( NULL );

   HB_BOOL bHonorSpacing = hb_parl( 2 );
   HB_BOOL bInRemark     = hb_parl( 3 );
   HB_BOOL bKeepComments = hb_parldef( 5, 1 );
   HB_BOOL bPRG          = hb_parldef( 6, 1 );
   HB_BOOL bKeepSpaces   = hb_parldef( 7, 1 );
   HB_BOOL bFirst        = HB_TRUE;
   HB_ISIZ lenprocessed  = 0;
   HB_ISIZ lenwords      = 0;
   HB_ISIZ wordlen;
   HB_ISIZ npos;

   char szNextWord[ 2048 ];

   szNextWord[ 0 ] = '\0';

   while( ( wordlen = ide_getword( pszExpr, bHonorSpacing, szNextWord, &lenprocessed ) ) != 0  )
   {
      pszExpr += lenprocessed;

      if( strncmp( szNextWord, "*/", 3 ) == 0 ) /* remark end */
      {
         if( bKeepComments )
         {
            lenwords++;
            hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
         }
         bInRemark = HB_FALSE;
      }
      else if( ( strncmp( szNextWord, "/*", 3 ) == 0 ) || bInRemark ) /* remark start */
      {
         bInRemark = ( ( npos = ide_atbuff( "*/", pszExpr, 0, 1, 2, strlen( pszExpr ) ) ) == 0 );

         if( bInRemark )
         {
            if( bKeepComments )
            {
               hb_strncat( szNextWord, pszExpr, sizeof( szNextWord ) - 1 );
               lenwords++;
               hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
            }
            break;
         }
         else
         {
            if( bKeepComments )
            {
               hb_strncpy( szNextWord + wordlen, pszExpr, sizeof( szNextWord ) - 1 - wordlen );
               lenwords++;
               hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
            }
            pszExpr += npos + 1;
         }
      }
      else if( strncmp( szNextWord, "//", 3 ) == 0 || ( bPRG && strncmp( szNextWord, "&&", 3 ) == 0 ) ) /* inline remark */
      {
         if( bKeepComments )
         {
            hb_strncat( szNextWord, pszExpr, sizeof( szNextWord ) - 1 );
            lenwords++;
            hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
         }
         break;
      }
      else if( strncmp( szNextWord, "**", 3 ) == 0 && bFirst && bPRG )
      {
         if( bKeepComments )
         {
            hb_strncat( szNextWord, pszExpr, sizeof( szNextWord ) - 1 );
            lenwords++;
            hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
         }
         break;
      }
      else
      {
         if( bKeepSpaces || ! ide_strempty( szNextWord ) )
         {
            lenwords++;
            hb_arrayAdd( paExpr, hb_itemPutC( pTemp, szNextWord ) );
         }
      }

      if( ! ide_strempty( szNextWord ) )
         bFirst = HB_FALSE;
   }

   if( ! bKeepComments && !( lenwords > 0 ) && hb_arrayGetCPtr( paExpr, lenwords ) )
      hb_arraySize( paExpr, lenwords );

   hb_storl( bInRemark, 3 );

   hb_itemRelease( pTemp );
   hb_itemReturnRelease( paExpr );
}

/*----------------------------------------------------------------------*/

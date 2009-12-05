/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Andy Wos
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

#define MAX_LINE_LEN 2047

static const char s_good[]      = "''_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890.";
static const char * s_adouble[] = { "*/", "/*", "//", "->", "::", "||", "++", "--", "**", ":=",
                                    "<=", ">=", "<>", "!=", "==", "+=", "-=", "*=", "/=", "%=",
                                    "^=", "&&", "^^", ">>", "<<", "=>", "&=", "|=" };

static int s_lengood     = HB_SIZEOFARRAY( s_good ) - 1;
static int s_lendouble   = HB_SIZEOFARRAY( s_adouble );

/*----------------------------------------------------------------------*/

static HB_SIZE linearfind( const char ** array, const char * cText, HB_SIZE lenarray, HB_SIZE lentext, HB_BOOL bMatchCase )
{
   HB_SIZE i;

   if( bMatchCase )
   {
      for( i = 0; i < lenarray; i++ )
      {
         if( strncmp( cText, array[ i ], lentext + 1 )  == 0 )
            return i + 1;
      }
   }
   else
   {
      for( i = 0; i < lenarray; i++ )
      {
         if( hb_strnicmp( cText, array[ i ], lentext + 1 ) == 0 )
            return i + 1;
      }

   }
   return 0;
}

/*----------------------------------------------------------------------*/

static HB_BOOL strempty( const char * string )
{
   HB_SIZE i = 0;

   while( string[ i ] != 0 )
   {
      if( string[ i++ ] != ' ' )
         return HB_FALSE;
   }

   return HB_TRUE;
}

/*----------------------------------------------------------------------*/

static HB_SIZE atbuff( const char * chars, const char * string, HB_SIZE StartFrom, HB_SIZE Target, HB_SIZE len_chars, HB_SIZE len )
{
   if( len >= len_chars && StartFrom <= len - len_chars )
   {
      HB_SIZE x;
      HB_SIZE Counter = 0;

      for( x = StartFrom; x <= ( len - len_chars ); x++ )
      {
         if( strncmp( string + x, chars, len_chars ) == 0 )
         {
            if( ++Counter == Target )
               return x + 1;
         }
      }
   }

   return 0;
}

/*----------------------------------------------------------------------*/

static int _GetWord( const char * cText, HB_BOOL lHonorSpacing, char * cWord, int * pnpos )
{
   int maxlen = strlen( cText );
   int npos = 0;
   int wordlen = 0;

   if( maxlen > 0 )
   {
      char   temp;
      char   ch;
      char   csingle[ 2 ];
      char   cdouble[ 3 ];

      csingle[ 1 ] = '\0';
      cdouble[ 2 ] = '\0';

      ch = cText[ 0 ];

      if( ch == ',' ) /* lists */
      {
         cWord[ wordlen++ ] = ch;
         npos++;
      }
      else /* literals */
      {
         if( ch == '"' || ch == '\'' )
         {
            temp = ch;
            cWord[ wordlen++ ] = ch;
            npos++;
            ch = ' ';
            while( ( npos < maxlen ) && ( ch != temp ) )
            {
               ch = cText[ npos ];
               cWord[ wordlen++ ] = ch;
               npos++;
            }
         }
         else
         {
            csingle[0] = ch;
            if( atbuff( csingle, s_good, 0, 1, 1, s_lengood ) ) /* ch $ s_good ) // variables, commands, function names */
            {
              while( ( npos < maxlen ) && atbuff( csingle, s_good, 0, 1, 1, s_lengood ) )
              {
                 cWord[ wordlen++ ] = ch;
                 npos++;
                 ch = cText[ npos ];
                 csingle[ 0 ] = ch;
              }

            }
            else if( ch == ' ' )
            {
               while( ( npos < maxlen ) && ch == ' ' )
               {
                  cWord[ wordlen++ ] = ch;
                  npos++;
                  ch = cText[ npos ];
               }

               if( !lHonorSpacing )
               {
                  cWord[ 0 ] = ' '; /* reduce spaces to 1 */
                  wordlen = 1;
               }
            }
            else  /* operators, punctuation */
            {
               cWord[ wordlen++ ]= ch;
               npos++;
               ch = cText[ npos ];
               if( maxlen > npos )
               {
                  cdouble[ 0 ] = cWord[ 0 ];
                  cdouble[ 1 ] = ch;
                  if( linearfind( s_adouble, cdouble, s_lendouble, 2, HB_TRUE ) )  /* if( (cWord + ch) $ s_adouble) //aScan( s_adouble, cWord + ch ) > 0 */
                  {
                     cWord[ wordlen++ ] = ch;
                     npos++;
                  }
               }
            }
         }
      }
   }

   cWord[ wordlen ] = '\0';
   *pnpos = npos;

   return wordlen;
}

/*----------------------------------------------------------------------*/

/*
 * ( c, lHonorSpacing, lInRemark, lUpperKeyWord, lKeepComments, lPRG, lKeepSpaces )
 */
HB_FUNC( PARSEXPR )
{
   const char * c = hb_parcx( 1 );
   HB_BOOL   lHonorSpacing = hb_parl( 2 );
   HB_BOOL   lInRemark     = HB_ISLOG( 3 ) ? hb_parl( 3 ) : HB_FALSE;
   HB_BOOL   lKeepComments = HB_ISLOG( 5 ) ? hb_parl( 5 ) : HB_TRUE;
   HB_BOOL   bPRG          = HB_ISLOG( 6 ) ? hb_parl( 6 ) : HB_TRUE;
   HB_BOOL   lKeepSpaces   = HB_ISLOG( 7 ) ? hb_parl( 7 ) : HB_TRUE;
   PHB_ITEM  aExpr         = hb_itemArrayNew( 0 );
   PHB_ITEM  element       = hb_itemNew( NULL );
   HB_BOOL   lFirst        = HB_TRUE;
   int       lenprocessed  = 0;
   int       lenwords      = 0;
   int       wordlen;
   int       npos;

   char NextWord[ MAX_LINE_LEN + 1 ];

   NextWord[ 0 ] = '\0';

   while( ( wordlen = _GetWord( c, lHonorSpacing, NextWord, &lenprocessed ) ) != 0  )
   {
      c += lenprocessed;

      if( strncmp( NextWord, "*/", 3 ) == 0 ) /* remark end */
      {
         if( lKeepComments )
         {
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
         lInRemark = HB_FALSE;
      }
      else if( ( strncmp( NextWord, "/*", 3 ) == 0 ) || lInRemark ) /* remark start */
      {
         lInRemark = ( ( npos = atbuff( "*/", c, 0, 1, 2, strlen( c ) ) ) == 0 );

         if( lInRemark )
         {

            if( lKeepComments )
            {
               hb_strncat( NextWord, c, sizeof( NextWord ) - 1 );
               lenwords++;
               hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
            }
            break;
         }
         else
         {
            if( lKeepComments )
            {
               strncpy( NextWord + wordlen, c, npos + 1 );
               NextWord[ wordlen + npos + 1 ] = '\0';
               lenwords++;
               hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
            }
            c += ( npos + 1 );
         }
      }
      else if( strncmp( NextWord, "//", 3 ) == 0 || ( bPRG && strncmp( NextWord, "&&", 3 ) == 0 ) ) /* inline remark */
      {
         if( lKeepComments )
         {
            hb_strncat( NextWord, c, sizeof( NextWord ) - 1 );
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
         break;
      }
      else if( strncmp( NextWord, "**", 3 ) == 0 && lFirst && bPRG )
      {
         if( lKeepComments )
         {
            hb_strncat( NextWord, c, sizeof( NextWord ) - 1 );
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
         break;
      }
      else
      {
         if( lKeepSpaces || ! strempty( NextWord ) )
         {
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
      }

      if( ! strempty( NextWord ) )
         lFirst = HB_FALSE;
   }

   if( ! lKeepComments && !( lenwords > 0 ) && hb_arrayGetCPtr( aExpr, lenwords ) )
      hb_arraySize( aExpr, lenwords );

   hb_storl( lInRemark, 3 );

   hb_itemRelease( element );
   hb_itemReturnRelease( aExpr );
}

/*----------------------------------------------------------------------*/

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

static char * good     = "''_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890.";
static char* adouble[] = {"*/", "/*", "//", "->", "::", "||", "++", "--", "**", ":=",
                          "<=", ">=", "<>", "!=", "==", "+=", "-=", "*=", "/=", "%=",
                          "^=", "&&", "^^", ">>", "<<", "=>", "&=", "|=" };
static int lengood     = 66;
static int lendouble   = 28;

/*----------------------------------------------------------------------*/

UINT linearfind( char** array, char* cText, UINT lenarray, UINT lentext, int lMatchCase )
{
   UINT i;

   if( lMatchCase )
   {
      for( i = 0 ; i < lenarray ; i++ )
      {
         if( strncmp( cText, array[ i ], lentext + 1 )  == 0 )
         {
            return( i + 1 );
         }
      }
   }
   else
   {
      for( i = 0 ; i < lenarray ; i++ )
      {
         if( strnicmp( cText, array[ i ], lentext + 1 ) == 0 )
         {
           return( i++ );
         }
      }

   }
   return 0;
}

/*----------------------------------------------------------------------*/

BOOL strempty( char* string )
{
   BOOL lRet = TRUE;
   UINT i = 0;

   while( string[ i ] != 0 )
   {
      if( string[ i++ ] != 32 )
      {
         lRet = FALSE;
         break;
      }
   }
   return lRet;
}

/*----------------------------------------------------------------------*/

UINT atbuff( char * chars, char * string, UINT StartFrom, UINT Target, UINT len_chars, UINT len )
{
   UINT x ;
   UINT Counter = 0;

   if( len >= len_chars && StartFrom <= len - len_chars )
      for( x = StartFrom; x <= ( len - len_chars ); x++ )
         if( strncmp( string + x, chars, len_chars ) == 0 )
            if( ++Counter == Target )
               return( x + 1 );
   return 0;
}

/*----------------------------------------------------------------------*/

static int _GetWord( char * cText, BOOL lHonorSpacing, char * cWord, int * pnpos )
{
   int    maxlen     = strlen( cText );
   int    npos       = 0;
   int    wordlen    = 0;
   char   temp;
   char   ch;
   char   csingle[ 2 ];
   char   cdouble[ 3 ];

   // workaround
//   good[ 0 ] = '"';

   csingle[ 1 ] = 0;
   cdouble[ 2 ] = 0;

   if( maxlen > 0 )
   {
      ch = cText[ 0 ];

      if( ch == ',' ) // lists
      {
         cWord[ wordlen++ ] = ch ;
         npos++ ;
      }
      else // literals
      {
         if( ch == '"' || ch == '\'' )
         {
            temp     = ch;
            cWord[ wordlen++ ] = ch;
            npos++;
            ch       = ' ';
            while( ( npos < maxlen ) && ( ch != temp ) )
            {
               ch = cText[ npos ];
               cWord[ wordlen++ ] = ch;
               npos ++;
            }
         }
         else
         {
            csingle[0] = ch;
            if( atbuff( csingle, good, 0, 1, 1, lengood ) ) //ch $ good ) // variables, commands, function names
            {
              while( ( npos < maxlen ) && atbuff( csingle, good, 0, 1, 1, lengood ) )
              {
                 cWord[ wordlen++ ] = ch;
                 npos++ ;
                 ch = cText[ npos ];
                 csingle[ 0 ] = ch;
              }

            }
            else if( ch == ' ' )
            {
               while( ( npos < maxlen ) && ch == ' ' )
               {
                  cWord[ wordlen++ ] = ch;
                  npos ++;
                  ch = cText[ npos ];
               }

               if( !lHonorSpacing )
               {
                  cWord[ 0 ] = ' '; //reduce spaces to 1
                  wordlen = 1;
               }
            }
            else  //operators, punctuation
            {
               cWord[ wordlen++ ]= ch;
               npos ++;
               ch = cText[ npos ];
               if( maxlen > npos )
               {
                  cdouble[ 0 ] = cWord[ 0 ];
                  cdouble[ 1 ] = ch;
                  if( linearfind( adouble, cdouble, lendouble, 2, TRUE ) )  //if( (cWord + ch) $ adouble) //aScan( adouble, cWord + ch ) > 0
                  {
                     cWord[ wordlen++ ] = ch;
                     npos ++;
                  }
               }
            }
         }
      }
   }

   cWord[ wordlen ] = 0 ;
   *pnpos = npos;

   return wordlen;
}

/*----------------------------------------------------------------------*/

/*
 * ( c, lHonorSpacing, lInRemark, lUpperKeyWord, lKeepComments, lPRG, lKeepSpaces )
 */
HB_FUNC( PARSEXPR )
{
   char *   c             = ( char * ) hb_parc( 1 );
   BOOL     lHonorSpacing = hb_parl( 2 );
   BOOL     lInRemark     = HB_ISLOG( 3 ) ? hb_parl( 3 ) : FALSE;
   BOOL     lKeepComments = HB_ISLOG( 5 ) ? hb_parl( 5 ) : TRUE;
   BOOL     bPRG          = HB_ISLOG( 6 ) ? hb_parl( 6 ) : TRUE;
   BOOL     lKeepSpaces   = HB_ISLOG( 7 ) ? hb_parl( 7 ) : TRUE;
   PHB_ITEM aExpr         = hb_itemArrayNew( 0 );
   PHB_ITEM element       = hb_itemNew( NULL );
   BOOL     lFirst        = TRUE;
   int      lenprocessed  = 0;
   int      lenwords      = 0;
   int      wordlen          ;
   int      npos             ;

   char NextWord[ MAX_LINE_LEN+1 ];

   NextWord[ 0 ] = 0;

   while( ( wordlen = _GetWord( c, lHonorSpacing,  NextWord, &lenprocessed ) ) != 0  )
   {
      c += lenprocessed;

      if( strncmp( NextWord, "*/", 3 ) == 0 ) // remark end
      {
         if( lKeepComments )
         {
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
         lInRemark = FALSE;
      }
      else if( ( strncmp( NextWord, "/*", 3 ) == 0) || lInRemark ) // remark start
      {
         lInRemark = ( ( npos = atbuff( "*/", c, 0, 1, 2, strlen( c ) ) ) == 0 );

         if( lInRemark )
         {

            if( lKeepComments )
            {
               strcat( NextWord, c );
               lenwords++;
               hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
            }
            break;
         }
         else
         {
            if( lKeepComments )
            {
               strncpy( NextWord + wordlen, c, npos+1 );
               NextWord[ wordlen + npos + 1 ] = 0;
               lenwords++;
               hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
            }
            c += ( npos + 1 ) ;//2 );
         }
      }
      else if( strncmp( NextWord, "//", 3 ) == 0 || ( bPRG && strncmp( NextWord, "&&", 3 ) == 0 ) ) // inline remark
      {
         if( lKeepComments )
         {
            strcat( NextWord, c );
            lenwords++;
            hb_arrayAdd( aExpr, hb_itemPutC( element, NextWord ) );
         }
         break;
      }
      else if( strncmp( NextWord, "**", 3 ) == 0 && ( lFirst && bPRG ) )
      {
         if( lKeepComments )
         {
            strcat( NextWord, c );
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
      {
         lFirst = FALSE;
      }
   } // end while

   if( !lKeepComments && !lenwords > 0 && hb_arrayGetCPtr( aExpr, lenwords ) )
   {
      hb_arraySize( aExpr, lenwords );
   }

   if( HB_ISBYREF( 3 ) )
      hb_storl( lInRemark, 3 );

   hb_itemReturn( aExpr );
   hb_itemRelease( element );
   hb_itemRelease( aExpr );
}

/*----------------------------------------------------------------------*/


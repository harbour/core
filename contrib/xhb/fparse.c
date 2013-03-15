/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * FParse()
 * FParseEx()
 * FParseLine()
 * FLineCount()
 * FCharCount()
 * FWordCount()
 *
 * Copyright 2004 Andi Jahja <xharbour@cbn.net.id>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
   FPARSE( cFile, cDelimiter ) -> array

   Purpose:
      Parse a delimited text file.

   Parameters:
      cFile - file to process
      cDelimiter - delimiter, default is comma

   Returns:
      Upon success -> Two dimensional array, of which each element contains
                      the results of parsing
      Upon error   -> An empty array
 */

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbfast.h"

/* adjustable, but this should be sufficient in normal situation */
#define MAX_READ  4096

static void hb_ParseLine( PHB_ITEM pReturn, const char * szText, int iDelimiter, int * iWord )
{
   if( szText )
   {
      HB_ISIZ nLen = strlen( szText );

      if( nLen > 0 )
      {
         PHB_ITEM pTemp      = hb_itemNew( NULL );
         HB_ISIZ  i          = 0;
         int      word_count = 0;
         /* booked enough memory */
         char * szResult = ( char * ) hb_xgrab( nLen + 1 );

#if 0
         while( nLen )
         {
            if( szText[ nLen - 1 ] && ! HB_ISSPACE( szText[ nLen - 1 ] ) )
               break;

            nLen--;
         }

         szText[ nLen ] = 0;

         nLen = strlen( szText );
#endif

         while( i < nLen )
         {
            HB_ISIZ ui = 0;

            hb_xmemset( szResult, ' ', nLen + 1 );

            /* an '"' found, loop until the next one is found */
            if( szText[ i ] == '"' )
            {
               /* an '"' after '"' ? */
               if( szText[ i + 1 ] != '"' )
               {
                  szResult[ ui ] = szText[ i + 1 ];
               }
               else
               {
                  szResult[ ui ] = '\0';
               }

               ++i;

               while( ++i < nLen )
               {
                  if( szText[ i - 1 ] == '"' )
                  {
                     szResult[ ui + 1 ] = '\0';
                     break;
                  }
                  else
                  {
                     if( szText[ i ] == '"' )
                        szResult[ ui + 1 ] = '\0';
                     else
                        szResult[ ++ui ] = szText[ i ];
                  }
               }
               word_count++;
               hb_arrayAddForward( pReturn, hb_itemPutC( pTemp, szResult ) );
            }
            /* delimiter found */
            else if( szText[ i ] == iDelimiter )
            {
               /* first delimiter found but no word yet */
               if( word_count == 0 )
               {
                  /* add an empty string */
                  szResult[ ui ] = '\0';
               }
               else
               {
                  /* we have already have the first word */
                  /* check next character */
                  if( szText[ i - 1 ] == iDelimiter )
                  {
                     /* delimiter after delimiter */
                     /* just add an empty string */
                     szResult[ ui ] = '\0';
                  }
                  else
                  {
                     /* ",,0" */
                     /* it is not a delimiter */
                     /* move to next character */
                     ++i;
                     szResult[ ui ] = szText[ i ];

                     while( ++i < nLen )
                     {
                        if( szText[ i ] == iDelimiter )
                        {
                           break;
                        }
                        else
                        {
                           szResult[ ++ui ] = szText[ i ];
                        }
                     }
                  }
               }
               word_count++;
               szResult[ ui + 1 ] = '\0';
               hb_arrayAddForward( pReturn, hb_itemPutC( pTemp, szResult ) );
            }
            else
            {
               szResult[ ui ] = szText[ i ];

               while( ++i < nLen )
               {
                  if( szText[ i ] == iDelimiter )
                  {
                     szResult[ ui + 1 ] = '\0';
                     break;
                  }
                  else if( szText[ i ] == '"' )
                  {
                     szResult[ ui ] = szText[ i + 1 ];
                     ++i;

                     while( ++i < nLen )
                     {
                        if( szText[ i - 1 ] == '"' )
                        {
                           szResult[ ui + 1 ] = '\0';
                           break;
                        }
                        else
                        {
                           if( szText[ i ] == '"' )
                           {
                              szResult[ ui + 1 ] = '\0';
                              break;
                           }
                           else
                           {
                              szResult[ ++ui ] = szText[ i ];
                           }
                        }
                     }
                  }
                  else
                  {
                     szResult[ ++ui ] = szText[ i ];
                  }
               }
               word_count++;
               szResult[ ui + 1 ] = '\0';
               hb_arrayAddForward( pReturn, hb_itemPutC( pTemp, szResult ) );
            }

            i++;
         }

         /* last character in passed string is a delimiter */
         /* just add an empty string */
         if( szText[ nLen - 1 ] == iDelimiter )
         {
            word_count++;
            hb_arrayAddForward( pReturn, hb_itemPutC( pTemp, NULL ) );
         }

         /* store number of words */
         *iWord = word_count;

         /* clean up */
         hb_xfree( szResult );

         hb_itemRelease( pTemp );
      }
   }
}

static char ** hb_tokensplit( const char * string, HB_BYTE delimiter, int iCharCount, int * iWord )
{
   char *  buffer, * bufptr;
   char ** token_list;
   char    last_char  = '\0';
   int     word_count = 0, word_nbr;

   buffer = ( char * ) hb_xgrab( iCharCount + 1 );

   bufptr = buffer;

   while( *string )
   {
      if( ( HB_BYTE ) *string == delimiter )
      {
         while( ( HB_BYTE ) *string == delimiter )
         {
            string++;
         }

         if( bufptr > buffer )
         {
            word_count++;
            last_char = *bufptr++ = '\0';
         }
      }
      else
      {
         last_char = *bufptr++ = *string++;
      }
   }

   if( last_char > 0 )
   {
      word_count++;
   }

   *bufptr = '\0';

   token_list      = ( char ** ) hb_xgrab( sizeof( char * ) * ( word_count + 2 ) );
   token_list[ 0 ] = buffer;
   token_list++;

   bufptr = buffer;

   for( word_nbr = 0; word_nbr < word_count; word_nbr++ )
   {
      token_list[ word_nbr ] = bufptr;
      bufptr += strlen( bufptr ) + 1;
   }

   token_list[ word_count ] = NULL;

   *iWord = word_count;

   return token_list;
}

static HB_BOOL file_read( FILE * stream, char * string, int * iCharCount )
{
   int ch, cnbr = 0;

   memset( string, ' ', MAX_READ );

   for(;; )
   {
      ch = fgetc( stream );

      if( ( ch == '\n' ) || ( ch == EOF ) || ( ch == 26 ) )
      {
         *iCharCount    = cnbr;
         string[ cnbr ] = '\0';
         return ch == '\n' || cnbr;
      }
      else
      {
         if( cnbr < MAX_READ && ch != '\r' )
         {
            string[ cnbr++ ] = ( char ) ch;
         }
      }

      if( cnbr >= MAX_READ )
      {
         *iCharCount        = cnbr;
         string[ MAX_READ ] = '\0';
         return HB_TRUE;
      }
   }
}

HB_FUNC( FPARSE )
{
   FILE *   inFile;
   PHB_ITEM pSrc   = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pDelim = hb_param( 2, HB_IT_STRING );
   PHB_ITEM pArray;
   PHB_ITEM pItem;
   char *   string;
   char **  tokens;
   int      iToken, iCharCount = 0;
   HB_BYTE  nByte;

   /* file parameter correctly passed */
   if( ! pSrc )
   {
      hb_reta( 0 );
      return;
   }

   if( hb_itemGetCLen( pSrc ) == 0 )
   {
      hb_reta( 0 );
      return;
   }

   /* open file for read */
   inFile = hb_fopen( hb_itemGetCPtr( pSrc ), "r" );

   /* return empty array on failure */
   if( ! inFile )
   {
      hb_reta( 0 );
      return;
   }

   /* default delimiter to comma */
   nByte = pDelim ? ( HB_BYTE ) hb_itemGetCPtr( pDelim )[ 0 ] : ( HB_BYTE ) ',';

   /* the main array */
   pArray = hb_itemArrayNew( 0 );
   pItem  = hb_itemNew( NULL );

   /* book memory for line to read */
   string = ( char * ) hb_xgrab( MAX_READ + 1 );

   /* read the file until EOF */
   while( file_read( inFile, string, &iCharCount ) )
   {
      /* parse the read line */
      int iWord = 0;

      tokens = hb_tokensplit( string, nByte, iCharCount, &iWord );

      /* prepare empty array */
      hb_arrayNew( pItem, iWord );

      /* add parsed text to array */
      for( iToken = 0; tokens[ iToken ]; iToken++ )
         hb_arraySetC( pItem, iToken + 1, tokens[ iToken ] );

      /* add array containing parsed text to main array */
      hb_arrayAddForward( pArray, pItem );

      /* clean up */
      tokens--;
      hb_xfree( tokens[ 0 ] );
      hb_xfree( tokens );
   }

   /* return main array */
   hb_itemReturnRelease( pArray );
   hb_itemRelease( pItem );

   /* clean up */
   hb_xfree( string );
   fclose( inFile );
}

HB_FUNC( FPARSEEX )
{
   FILE *   inFile;
   PHB_ITEM pSrc   = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pDelim = hb_param( 2, HB_IT_STRING );
   PHB_ITEM pArray;
   PHB_ITEM pSubArray;
   char *   string;
   int      iCharCount = 0;
   HB_BYTE  nByte;

   /* file parameter correctly passed */
   if( ! pSrc )
   {
      hb_reta( 0 );
      return;
   }

   if( hb_itemGetCLen( pSrc ) == 0 )
   {
      hb_reta( 0 );
      return;
   }

   /* open file for read */
   inFile = hb_fopen( hb_itemGetCPtr( pSrc ), "r" );

   /* return empty array on failure */
   if( ! inFile )
   {
      hb_reta( 0 );
      return;
   }

   /* default delimiter to comma */
   nByte = pDelim ? ( HB_BYTE ) hb_itemGetCPtr( pDelim )[ 0 ] : ( HB_BYTE ) ',';

   /* the main array */
   pArray    = hb_itemArrayNew( 0 );
   pSubArray = hb_itemNew( NULL );

   /* book memory for line to read */
   string = ( char * ) hb_xgrab( MAX_READ + 1 );

   /* read the file until EOF */
   while( file_read( inFile, string, &iCharCount ) )
   {
      /* parse the read line */
      int iWord = 0;
      hb_arrayNew( pSubArray, 0 );

      hb_ParseLine( pSubArray, string, nByte, &iWord );

      /* add array containing parsed text to main array */
      hb_arrayAddForward( pArray, pSubArray );
   }

   /* return main array */
   hb_itemReturnRelease( pArray );
   hb_itemRelease( pSubArray );

   /* clean up */
   hb_xfree( string );
   fclose( inFile );
}

HB_FUNC( FWORDCOUNT )
{
   FILE *   inFile;
   PHB_ITEM pSrc = hb_param( 1, HB_IT_STRING );
   char *   string;
   char **  tokens;
   int      iCharCount = 0;
   HB_BYTE  nByte      = ' ';
   HB_SIZE  nWordCount = 0;

   /* file parameter correctly passed */
   if( ! pSrc )
   {
      hb_retns( 0 );
      return;
   }

   if( hb_itemGetCLen( pSrc ) == 0 )
   {
      hb_retns( 0 );
      return;
   }

   /* open file for read */
   inFile = hb_fopen( hb_itemGetCPtr( pSrc ), "r" );

   /* return 0 on failure */
   if( ! inFile )
   {
      hb_retns( 0 );
      return;
   }

   /* book memory for line to read */
   string = ( char * ) hb_xgrab( MAX_READ + 1 );

   /* read the file until EOF */
   while( file_read( inFile, string, &iCharCount ) )
   {
      int iWord = 0;

      tokens = hb_tokensplit( string, nByte, iCharCount, &iWord );

      nWordCount += iWord;

      /* clean up */
      tokens--;
      hb_xfree( tokens[ 0 ] );
      hb_xfree( tokens );
   }

   /* return number of words */
   hb_retns( nWordCount );

   /* clean up */
   hb_xfree( string );
   fclose( inFile );
}

HB_FUNC( FLINECOUNT )
{
   FILE *   inFile;
   PHB_ITEM pSrc       = hb_param( 1, HB_IT_STRING );
   HB_SIZE  nLineCount = 0;
   int      ch;

   /* file parameter correctly passed */
   if( ! pSrc )
   {
      hb_retns( 0 );
      return;
   }

   if( hb_itemGetCLen( pSrc ) == 0 )
   {
      hb_retns( 0 );
      return;
   }

   /* open file for read */
   inFile = hb_fopen( hb_itemGetCPtr( pSrc ), "r" );

   /* return 0 on failure */
   if( ! inFile )
   {
      hb_retns( 0 );
      return;
   }

   /* read the file until EOF */
   while( ( ch = fgetc( inFile ) ) != EOF )
   {
      if( ch == '\n' )
      {
         nLineCount++;
      }
   }

   /* return number of lines */
   hb_retns( nLineCount );

   /* clean up */
   fclose( inFile );
}

HB_FUNC( FCHARCOUNT )
{
   FILE *   inFile;
   PHB_ITEM pSrc    = hb_param( 1, HB_IT_STRING );
   HB_SIZE  nResult = 0;
   int      ch;

   /* file parameter correctly passed */
   if( ! pSrc )
   {
      hb_retns( 0 );
      return;
   }

   if( hb_itemGetCLen( pSrc ) == 0 )
   {
      hb_retns( 0 );
      return;
   }

   /* open file for read */
   inFile = hb_fopen( hb_itemGetCPtr( pSrc ), "r" );

   /* return 0 on failure */
   if( ! inFile )
   {
      hb_retns( 0 );
      return;
   }

   /* read the file until EOF */
   while( ( ch = fgetc( inFile ) ) != EOF )
   {
      switch( ch )
      {
         case '\n':
         case '\r':
         case ' ':
         case '\t':
            break;
         default:
            nResult++;
      }
   }

   /* return number of characters */
   hb_retns( nResult );

   /* clean up */
   fclose( inFile );
}

HB_FUNC( FPARSELINE )
{
   PHB_ITEM     pArray;
   HB_ISIZ      nWords = 0;
   const char * szText;

   pArray = hb_itemArrayNew( 0 );
   szText = hb_parc( 1 );

   if( szText )
   {
      const char * szDelim = hb_parc( 2 );
      int          iWords  = 0;
      hb_ParseLine( pArray, szText, szDelim ? ( unsigned char ) *szDelim : ',', &iWords );
      nWords = iWords;
   }

   hb_itemReturnRelease( pArray );
   hb_storns( nWords, 3 );
}

/*
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 */

#include "hbapi.h"

static const char * hb_strtoken( const char * szText,
                                 HB_ISIZ nText,
                                 HB_ISIZ nIndex,
                                 char cDelimiter,
                                 HB_ISIZ * pnLen )
{
   HB_ISIZ nStart;
   HB_ISIZ nEnd     = 0;
   HB_ISIZ nCounter = 0;

   HB_TRACE( HB_TR_DEBUG,
             ( "hb_strtoken(%s, %" HB_PFS "d, %" HB_PFS "d, %d, %p)", szText, nText, nIndex,
               ( int ) cDelimiter, pnLen ) );

   do
   {
      nStart = nEnd;

      if( cDelimiter != ' ' )
      {
         if( szText[ nStart ] == cDelimiter )
            nStart++;
      }
      else
      {
         while( nStart < nText && szText[ nStart ] == cDelimiter )
            nStart++;
      }

      if( nStart < nText && szText[ nStart ] != cDelimiter )
      {
         nEnd = nStart + 1;

         while( nEnd < nText && szText[ nEnd ] != cDelimiter )
            nEnd++;
      }
      else
         nEnd = nStart;
   }
   while( nCounter++ < nIndex - 1 && nEnd < nText );

   if( nCounter < nIndex )
   {
      *pnLen = 0;
      return "";
   }
   else
   {
      *pnLen = nEnd - nStart;
      return szText + nStart;
   }
}

/* returns the nth occurence of a substring within a token-delimited string */
HB_FUNC( STRTOKEN )
{
   const char * szText;
   HB_ISIZ      nIndex     = hb_parns( 2 );
   char         cDelimiter = *hb_parc( 3 );
   HB_ISIZ      nLen;

   if( ! cDelimiter )
      cDelimiter = ' ';

   szText = hb_strtoken( hb_parc( 1 ), hb_parclen( 1 ), nIndex, cDelimiter, &nLen );

   hb_storns( nLen, 4 );
   hb_retclen( szText, nLen );
}

/* debug function to dump the ASCII values of an entire string */
HB_FUNC( STRDUMP )
{
   const char * szText = hb_parc( 1 );
   HB_ISIZ      i, nLength = hb_parclen( 1 );

   for( i = 0; i < nLength; i++ )
      printf( "%d ", szText[ i ] );
   printf( "\n" );
}

HB_FUNC( ROT13 )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * szText = hb_parc( 1 );
      HB_SIZE      i, nLen = hb_parclen( 1 );
      char *       szResult = ( char * ) hb_xgrab( nLen + 1 );

      for( i = 0; i < nLen; i++ )
      {
         char c = szText[ i ];
         if( ( c >= 'A' && c <= 'M' ) || ( c >= 'a' && c <= 'm' ) )
            c += 13;
         else if( ( c >= 'N' && c <= 'Z' ) || ( c >= 'n' && c <= 'z' ) )
            c -= 13;

         szResult[ i ] = c;
      }
      hb_retclen( szResult, nLen );
      hb_xfree( szResult );
   }
   else
      hb_retc_null();
}

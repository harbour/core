/*
 * Harbour Project source code:
 * BubbleBabbleEncode()
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbapi.h"

/*
   Algorithm:
      http://wiki.yak.net/589/Bubble_Babble_Encoding.txt
   Implementation based on this code:
      http://www.nitrxgen.net/source/bubblebabble.txt
 */

HB_FUNC( BUBBLEBABBLEENCODE )
{
   static const char * s_szConsonants = "bcdfghklmnprstvzx";
   static const char * s_szVowels     = "aeiouy";

   const char * pszInput  = hb_parcx( 1 );
   HB_ISIZ      nInputLen = hb_parclen( 1 );
   char *       pszResult = ( char * ) hb_xgrab( ( nInputLen * 4 ) + 1 );

   HB_ISIZ nPos = 0;
   HB_ISIZ i;

   HB_BYTE byte1;
   HB_BYTE byte2;

   int iSeed = 1;

   pszResult[ nPos++ ] = 'x';

   for( i = 0;; i += 2 )
   {
      if( i >= nInputLen )
      {
         pszResult[ nPos++ ] = s_szVowels[ iSeed % 6 ];
         pszResult[ nPos++ ] = s_szConsonants[ 16 ];
         pszResult[ nPos++ ] = s_szVowels[ iSeed / 6 ];
         break;
      }

      byte1 = pszInput[ i ];

      pszResult[ nPos++ ] = s_szVowels[ ( ( ( byte1 >> 6 ) & 3 ) + iSeed ) % 6 ];
      pszResult[ nPos++ ] = s_szConsonants[ ( byte1 >> 2 ) & 15 ];
      pszResult[ nPos++ ] = s_szVowels[ ( ( byte1 & 3 ) + ( iSeed / 6 ) ) % 6 ];

      if( i + 1 >= nInputLen )
         break;

      byte2 = pszInput[ i + 1 ];

      pszResult[ nPos++ ] = s_szConsonants[ ( byte2 >> 4 ) & 15 ];
      pszResult[ nPos++ ] = '-';
      pszResult[ nPos++ ] = s_szConsonants[ byte2 & 15 ];

      iSeed = ( iSeed * 5 + byte1 * 7 + byte2 ) % 36;
   }

   pszResult[ nPos++ ] = 'x';
   pszResult[ nPos ]   = '\0';

   hb_retc_buffer( pszResult );
}

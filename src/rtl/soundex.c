/*
 * Harbour Project source code:
 * SoundEx() function
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapiitm.h"

#define SOUNDEX_LEN_MAX  4

HB_FUNC( SOUNDEX )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );
   char szResult[ SOUNDEX_LEN_MAX + 1 ];

   /* NOTE: The result will always be a zero terminated string without any
            embedded zeros and special characters. [vszakats] */

   memset( szResult, '0', SOUNDEX_LEN_MAX );
   szResult[ SOUNDEX_LEN_MAX ] = '\0';

   if( pString )
   {
      const char * pszString = hb_itemGetCPtr( pString );
      HB_SIZE nLen = hb_itemGetCLen( pString );
      HB_SIZE nPos = 0;
      HB_SIZE nResultPos = 0;
      char cCharPrev = '0';

      while( nPos < nLen && nResultPos < SOUNDEX_LEN_MAX )
      {
         char cChar = pszString[ nPos ];

         /* NOTE: Intentionally not using toupper()/IsAlpha() to be 100%
                  Clipper compatible here, these ANSI C functions may behave
                  differently for accented and national characters. It's also
                  faster this way. [vszakats] */

         /* Convert to uppercase: HB_TOUPPER() */
         if( cChar >= 'a' && cChar <= 'z' )
            cChar -= ( 'a' - 'A' );

         /* Check if IsAlpha() */
         if( cChar >= 'A' && cChar <= 'Z' )
         {
            static const char s_szTable[] = "01230120022455012623010202"; /* NOTE: SoundEx result codes for letters from "A" to "Z" */
                                         /* "ABCDEFGHIJKLMNOPQRSTUVWXYZ" */
            char cCharConverted = ( ( cChar - 'A' ) > ( ( int ) sizeof( s_szTable ) - 1 ) ) ? '9' : s_szTable[ cChar - 'A' ];

            if( nResultPos == 0 )
               szResult[ nResultPos++ ] = cChar;
            else if( cCharConverted != '0' && cCharConverted != cCharPrev )
               szResult[ nResultPos++ ] = cCharConverted;

            cCharPrev = cCharConverted;
         }

         nPos++;
      }
   }

   hb_retc( szResult );
}

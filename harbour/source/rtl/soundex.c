/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SOUNDEX() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"

#define SOUNDEX_LEN_MAX         4

HARBOUR HB_SOUNDEX( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );
   char szResult[ SOUNDEX_LEN_MAX + 1 ];

   /* NOTE: The result will always be a zero terminated string without any
            embedded zeros and special characters. [vszakats] */

   memset( szResult, '0', SOUNDEX_LEN_MAX );
   szResult[ SOUNDEX_LEN_MAX ] = '\0';

   if( pString )
   {
      char * pszString = hb_itemGetCPtr( pString );
      ULONG ulLen = hb_itemGetCLen( pString );
      ULONG nPos = 0;
      ULONG nResultPos = 0;
      char cCharPrev = '0';

      while( nPos < ulLen && nResultPos < SOUNDEX_LEN_MAX )
      {
         char cChar = pszString[ nPos ];

         /* NOTE: Intentionally not using toupper()/isalpha() to be 100%
                  Clipper compatible here, these ANSI C functions may behave
                  differently for accented and national characters. It's also
                  faster this way. [vszakats] */

         /* Convert to uppercase: toupper() */
         if( cChar >= 'a' && cChar <= 'z' )
            cChar -= ( 'a' - 'A' );

         /* Check if isalpha() */
         if( cChar >= 'A' && cChar <= 'Z' )
         {
            static const char s_szTable[] = "01230120022455012623010202"; /* NOTE: SoundEx result codes for letters from "A" to "Z" */
                                         /* "ABCDEFGHIJKLMNOPQRSTUVWXYZ" */
            char cCharConverted = ( ( cChar - 'A' ) > ( sizeof( s_szTable ) - 1 ) ) ? '9' : s_szTable[ cChar - 'A' ];

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


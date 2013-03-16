/*
 * Harbour Project source code:
 * Crypt() CA-T*ols compatible function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

HB_FUNC( CRYPT )
{
   HB_SIZE nCryptLen = hb_parclen( 2 );

   if( nCryptLen >= 2 )
   {
      const HB_BYTE * pbyCrypt = ( const HB_BYTE * ) hb_parc( 2 );
      HB_SIZE nCryptPos = 0;

      const HB_BYTE * pbyString = ( const HB_BYTE * ) hb_parc( 1 );
      HB_SIZE nStringLen = hb_parclen( 1 );
      HB_SIZE nStringPos;

      HB_BYTE * pbyResult = ( HB_BYTE * ) hb_xgrab( nStringLen + 1 );

      HB_USHORT uiCount2 =
         ( ( ( HB_USHORT ) ( pbyCrypt[ nCryptPos ] + ( HB_USHORT ) ( pbyCrypt[ nCryptPos + 1 ] * 256 ) ) ) &
           0xFFFF ) ^ ( ( HB_USHORT ) nCryptLen & 0xFFFF );
      HB_USHORT uiCount1 = 0xAAAA;

      for( nStringPos = 0; nStringPos < nStringLen; )
      {
         HB_USHORT uiTmpCount1 = uiCount1;
         HB_USHORT uiTmpCount2 = uiCount2;
         HB_BYTE byte = pbyString[ nStringPos ] ^ pbyCrypt[ nCryptPos++ ];
         HB_USHORT tmp;

         uiTmpCount2 =
            HB_MKUSHORT( ( HB_LOBYTE( uiTmpCount2 ) ^ HB_HIBYTE( uiTmpCount2 ) ),
                         HB_HIBYTE( uiTmpCount2 ) );

         for( tmp = HB_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
            uiTmpCount2 = ( uiTmpCount2 >> 1 ) | ( ( uiTmpCount2 & 1 ) << 15 );

         uiTmpCount2 ^= uiTmpCount1;
         uiTmpCount2 += 16;

         uiCount2 = uiTmpCount2;

         uiTmpCount2 &= 0x1E;
         uiTmpCount2 += 2;

         do
         {
            HB_BYTE byTmp;

            uiTmpCount2--;

            for( tmp = HB_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
               uiTmpCount1 = ( uiTmpCount1 >> 1 ) | ( ( uiTmpCount1 & 1 ) << 15 );

            uiTmpCount1 = HB_MKUSHORT( HB_HIBYTE( uiTmpCount1 ), HB_LOBYTE( uiTmpCount1 ) );
            uiTmpCount1 =
               HB_MKUSHORT( ( HB_LOBYTE( uiTmpCount1 ) ^ 0xFF ), HB_HIBYTE( uiTmpCount1 ) );
            uiTmpCount1 = ( uiTmpCount1 << 1 ) | ( ( uiTmpCount1 & 0x8000 ) >> 15 );
            uiTmpCount1 ^= 0xAAAA;

            byTmp = HB_LOBYTE( uiTmpCount1 );
            byTmp = ( byTmp << 1 ) | ( ( byTmp & 0x80 ) >> 7 );

            uiTmpCount1 = HB_MKUSHORT( byTmp, HB_HIBYTE( uiTmpCount1 ) );

         }
         while( --uiTmpCount2 );

         uiCount1 = uiTmpCount1;

         pbyResult[ nStringPos++ ] = byte ^ HB_LOBYTE( uiTmpCount1 );

         if( nCryptPos == nCryptLen )
            nCryptPos = 0;
      }

      hb_retclen_buffer( ( char * ) pbyResult, nStringLen );
   }
   else
      hb_retc_null();
}

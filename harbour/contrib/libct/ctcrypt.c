/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CRYPT() CA-Tools compatible function
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
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

#include "hbapi.h"

HB_FUNC( CRYPT )
{
   ULONG ulCryptLen = hb_parclen( 2 );

   if( ulCryptLen >= 2 )
   {
      BYTE * pbyCrypt = ( BYTE * ) hb_parc( 2 );
      ULONG ulCryptPos = 0;

      BYTE * pbyString = ( BYTE * ) hb_parc( 1 );
      ULONG ulStringLen = hb_parclen( 1 );
      ULONG ulStringPos;

      BYTE * pbyResult = ( BYTE * ) hb_xgrab( ulStringLen + 1 );

      USHORT uiCount2 = ( ( ( USHORT ) ( pbyCrypt[ ulCryptPos ] + ( USHORT ) ( pbyCrypt[ ulCryptPos + 1 ] * 256 ) ) ) & 0xFFFF ) ^ ( ( USHORT ) ulCryptLen & 0xFFFF );
      USHORT uiCount1 = 0xAAAA;

      for( ulStringPos = 0; ulStringPos < ulStringLen; )
      {
         USHORT uiTmpCount1 = uiCount1;
         USHORT uiTmpCount2 = uiCount2;
         BYTE byte = pbyString[ ulStringPos ] ^ pbyCrypt[ ulCryptPos++ ];
         USHORT tmp;

         uiTmpCount2 = HB_MKUSHORT( ( HB_LOBYTE( uiTmpCount2 ) ^ HB_HIBYTE( uiTmpCount2 ) ), HB_HIBYTE( uiTmpCount2 ) );

         for( tmp = HB_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
            uiTmpCount2 = ( uiTmpCount2 >> 1 ) | ( ( uiTmpCount2 & 1 ) << 15 );

         uiTmpCount2 ^= uiTmpCount1;
         uiTmpCount2 += 16;

         uiCount2 = uiTmpCount2;

         uiTmpCount2 &= 0x1E;
         uiTmpCount2 += 2;

         do
         {
            BYTE byTmp;

            uiTmpCount2--;

            for( tmp = HB_LOBYTE( uiTmpCount2 ); tmp; tmp-- )
               uiTmpCount1 = ( uiTmpCount1 >> 1 ) | ( ( uiTmpCount1 & 1 ) << 15 );

            uiTmpCount1 = HB_MKUSHORT( HB_HIBYTE( uiTmpCount1 ), HB_LOBYTE( uiTmpCount1 ) );
            uiTmpCount1 = HB_MKUSHORT( ( HB_LOBYTE( uiTmpCount1 ) ^ 0xFF ), HB_HIBYTE( uiTmpCount1 ) );
            uiTmpCount1 = ( uiTmpCount1 << 1 ) | ( ( uiTmpCount1 & 0x8000 ) >> 15 );
            uiTmpCount1 ^= 0xAAAA;

            byTmp = HB_LOBYTE( uiTmpCount1 );
            byTmp = ( byTmp << 1 ) | ( ( byTmp & 0x80 ) >> 7 );

            uiTmpCount1 = HB_MKUSHORT( byTmp, HB_HIBYTE( uiTmpCount1 ) );

         } while ( --uiTmpCount2 );

         uiCount1 = uiTmpCount1;

         pbyResult[ ulStringPos++ ] = byte ^ HB_LOBYTE( uiTmpCount1 );

         if( ulCryptPos == ulCryptLen )
            ulCryptPos = 0;
      }

      hb_retclen( ( char * ) pbyResult, ulStringLen );
      hb_xfree( pbyResult );
   }
   else
      hb_retc( "" );
}

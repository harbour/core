/*
 * Functions to create session id and some utils
 *
 * Copyright 2008 Lorenzo Fiorini <lorenzo.fiorini@gmail.com>
 * Copyright 2003 Giancarlo Niccolai <gian@niccolai.ws> (CGI Session Manager Class)
 * Copyright 2003-2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

#define SID_LENGTH       25
#define BASE_KEY_STRING  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
#define CRC_KEY_STRING   "Ak3yStR1Ng"  // Max Length must be 10 chars

FUNCTION tip_GenerateSID( cCRCKey )

   LOCAL cSID, nSIDCRC, cSIDCRC, n, cTemp, nKey, nRand
   LOCAL cBaseKeys   := BASE_KEY_STRING
   LOCAL nLenKeys    := Len( cBaseKeys )

   cCRCKey := Left( hb_defaultValue( cCRCKey, CRC_KEY_STRING ), 10 )  // Max Length must to be of 10 chars

   /* Let's generate the sequence */
   cSID := ""
   nKey := 0
   FOR n := 1 TO SID_LENGTH
      nRand := hb_randInt( nLenKeys )
      cSID  += SubStr( cBaseKeys, nRand, 1 )
      nKey  += nRand
   NEXT

   nSIDCRC := nKey * 51 // Max Value is 99603 a 5 chars number
   cTemp   := StrZero( nSIDCRC, 5 )

   cSIDCRC := ""
   FOR n := 1 TO Len( cTemp )
      cSIDCRC += SubStr( cCRCKey, Val( SubStr( cTemp, n, 1 ) ) + 1, 1 )
   NEXT

   RETURN cSID + cSIDCRC

FUNCTION tip_CheckSID( cSID, cCRCKey )

   LOCAL nSIDCRC, cSIDCRC, n, cTemp, nKey

   cCRCKey := Left( hb_defaultValue( cCRCKey, CRC_KEY_STRING ), 10 )  // Max Length must to be of 10 chars

   /* Calculate the key */
   nKey := 0
   FOR n := 1 TO SID_LENGTH
      nKey += At( SubStr( cSID, n, 1 ), BASE_KEY_STRING )
   NEXT

   /* Recalculate the CRC */
   nSIDCRC := nKey * 51  // Max Value is 99603. a 5 chars number
   cTemp   := StrZero( nSIDCRC, 5 )

   cSIDCRC := ""
   FOR n := 1 TO Len( cTemp )
      cSIDCRC += SubStr( cCRCKey, Val( SubStr( cTemp, n, 1 ) ) + 1, 1 )
   NEXT

   RETURN Right( cSID, 5 ) == cSIDCRC

FUNCTION tip_DateToGMT( tDate )

   hb_default( @tDate, hb_DateTime() )

   RETURN ;
      { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" }[ DoW( tDate ) ] + ", " + ;
      StrZero( Day( tDate ), 2 ) + " " + ;
      { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }[ Month( tDate ) ] + " " + ;
      StrZero( Year( tDate ), 4 ) + " " + ;
      hb_TToC( tDate, "", "hh:mm:ss" ) + " GMT"

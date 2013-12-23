/*
 * Harbour Project source code:
 * NTP functions
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
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

#include "hbsocket.ch"
#include "hbver.ch"

FUNCTION hb_ntp_GetTimeUTC( cServer, nPort, nTimeOut )

   LOCAL tTime := hb_SToT( "" )
   LOCAL hSocket, cBuffer

   IF HB_ISSTRING( cServer ) .AND. ! Empty( cServer )

      hb_default( @nPort, 123 )
      hb_default( @nTimeOut, 10000 )  /* 10s */

      IF ! Empty( hSocket := hb_socketOpen( , HB_SOCKET_PT_DGRAM ) )
         cBuffer := hb_BChar( 8 ) + Replicate( hb_BChar( 0 ), 47 )
         IF hb_socketSendTo( hSocket, cBuffer,,, { HB_SOCKET_AF_INET, hb_socketResolveAddr( cServer ), nPort } ) == hb_BLen( cBuffer )
            cBuffer := Space( 12 * 4 )
            IF hb_socketRecvFrom( hSocket, @cBuffer,,,, nTimeOut ) == hb_BLen( cBuffer )
               tTime := hb_SToT( "19700101" ) + ( ( Bin2U( ntohl( hb_BSubStr( cBuffer, ( 10 * 4 ) + 1, 4 ) ) ) - 2208988800 ) * ( 1 / 86400 ) )
            ENDIF
         ENDIF
         hb_socketClose( hSocket )
      ENDIF
   ENDIF

   RETURN tTime

STATIC FUNCTION ntohl( c )

   IF hb_Version( HB_VERSION_ENDIANNESS ) == HB_VERSION_ENDIAN_LITTLE
      RETURN ;
         hb_BSubStr( c, 4, 1 ) + ;
         hb_BSubStr( c, 3, 1 ) + ;
         hb_BSubStr( c, 2, 1 ) + ;
         hb_BSubStr( c, 1, 1 )
   ENDIF

   RETURN c

STATIC FUNCTION Bin2U( c )

   LOCAL l := Bin2L( c )

   RETURN iif( l < 0, l + 4294967296, l )

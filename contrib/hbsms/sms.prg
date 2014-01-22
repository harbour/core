/*
 * Harbour Project source code:
 * SMS library. EXPERIMENTAL CODE. USE AT YOUR OWN RISK. NO GUARANTEES.
 *
 * Copyright 2009-2010 Viktor Szakats (vszakats.net/harbour)
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

/* TODO: - Unicode support */
/* TODO: - Receive support */

/* NOTE: Source of information:
         http://www.smssolutions.net/tutorials/gsm/sendsmsat/
         http://www.smssolutions.net/tutorials/gsm/receivesmsat/
         http://www.developershome.com/sms/sendSmsByAtCommands.asp
         http://www.developershome.com/sms/readSmsByAtCommands.asp
         http://www.dreamfabric.com/sms/
         [vszakats] */

STATIC FUNCTION port_send( h, s )
   RETURN hb_comSend( h, s )

STATIC FUNCTION port_rece( h, n, t )

   LOCAL cString

   hb_default( @n, 64 )
   hb_default( @t, 5 )

   cString := Space( n )

   hb_comRecv( h, @cString,, t )

   RETURN cString

FUNCTION sms_Send( cPort, cPhoneNo, cText, lNotification, cPIN )

   LOCAL smsctx
   LOCAL nRetVal

   IF ! Empty( smsctx := smsctx_New( cPort ) )
      smsctx_PIN( smsctx, cPIN )
      nRetVal := smsctx_Send( smsctx, cPhoneNo, cText, lNotification )
      smsctx_Close( smsctx )
   ELSE
      nRetVal := -99
   ENDIF

   RETURN nRetVal

FUNCTION sms_ReceiveAll( cPort, cPIN )

   LOCAL smsctx
   LOCAL aRetVal

   IF ! Empty( smsctx := smsctx_New( cPort ) )
      smsctx_PIN( smsctx, cPIN )
      aRetVal := smsctx_Receive( smsctx )
      smsctx_Close( smsctx )
   ELSE
      aRetVal := NIL
   ENDIF

   RETURN aRetVal

/* --------------------- */

#define _SMSCTX_xHnd          1
#define _SMSCTX_cPIN          2
#define _SMSCTX_cPrevName     3
#define _SMSCTX_MAX_          3

FUNCTION smsctx_New( xPort )

   LOCAL smsctx[ _SMSCTX_MAX_ ]

   IF HB_ISNUMERIC( xPort )
      smsctx[ _SMSCTX_xHnd ] := xPort
      smsctx[ _SMSCTX_cPrevName ] := NIL
   ELSEIF HB_ISSTRING( xPort )
      smsctx[ _SMSCTX_xHnd ] := 1
      smsctx[ _SMSCTX_cPrevName ] := hb_comGetDevice( smsctx[ _SMSCTX_xHnd ] )
      hb_comSetDevice( smsctx[ _SMSCTX_xHnd ], xPort )
   ELSE
      smsctx[ _SMSCTX_xHnd ] := NIL
   ENDIF

   IF smsctx[ _SMSCTX_xHnd ] != NIL
      IF hb_comOpen( smsctx[ _SMSCTX_xHnd ] )
         IF hb_comInit( smsctx[ _SMSCTX_xHnd ], 9600, "N", 8, 1 )
            RETURN smsctx
         ELSE
            hb_comClose( smsctx[ _SMSCTX_xHnd ] )
         ENDIF
      ENDIF
   ENDIF

   RETURN NIL

FUNCTION smsctx_Close( smsctx )

   IF ! HB_ISARRAY( smsctx ) .OR. Len( smsctx ) != _SMSCTX_MAX_
      RETURN .F.
   ENDIF

   IF ! hb_comClose( smsctx[ _SMSCTX_xHnd ] )
      RETURN .F.
   ENDIF

   /* Restore com port name */
   IF smsctx[ _SMSCTX_cPrevName ] != NIL
      hb_comSetDevice( smsctx[ _SMSCTX_xHnd ], smsctx[ _SMSCTX_cPrevName ] )
   ENDIF

   RETURN .T.

FUNCTION smsctx_Send( smsctx, cPhoneNo, cText, lNotification )

   LOCAL tmp

   IF ! HB_ISARRAY( smsctx ) .OR. Len( smsctx ) != _SMSCTX_MAX_
      RETURN -1
   ENDIF

   port_send( smsctx[ _SMSCTX_xHnd ], "ATE0V1Q0" + Chr( 13 ) )
   IF IsOK( port_rece( smsctx[ _SMSCTX_xHnd ] ) )

      port_send( smsctx[ _SMSCTX_xHnd ], "AT+CMGF=1" + Chr( 13 ) )
      IF StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) ) == "OK"

         IF ! Empty( smsctx[ _SMSCTX_cPIN ] )
            port_send( smsctx[ _SMSCTX_xHnd ], 'AT+CPIN="' + smsctx[ _SMSCTX_cPIN ] + '"' + Chr( 13 ) )
            IF !( StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) ) == "OK" )
               RETURN -5
            ENDIF
         ENDIF

         port_send( smsctx[ _SMSCTX_xHnd ], "AT+CMGF=1" + Chr( 13 ) )
         IF StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) ) == "OK"

            IF HB_ISLOGICAL( lNotification )
               port_send( smsctx[ _SMSCTX_xHnd ], "AT+CSMP?" + Chr( 13 ) )
               tmp := GetLines( port_rece( smsctx[ _SMSCTX_xHnd ] ) )
               IF Len( tmp ) < 2
                  RETURN -6
               ENDIF
               IF !( ATail( tmp ) == "OK" )
                  RETURN -7
               ENDIF
               IF ! hb_LeftIs( tmp[ 1 ], "+CSMP: " )
                  RETURN -8
               ENDIF
               tmp := GetList( SubStr( tmp[ 1 ], Len( "+CSMP: " ) + 1 ) )
               IF Len( tmp ) > 1
                  IF lNotification
                     tmp[ 1 ] := hb_ntos( hb_bitSet( Val( tmp[ 1 ] ), 5 ) )
                  ELSE
                     tmp[ 1 ] := hb_ntos( hb_bitReset( Val( tmp[ 1 ] ), 5 ) )
                  ENDIF
                  port_send( smsctx[ _SMSCTX_xHnd ], "AT+CSMP=" + MakeList( tmp ) + Chr( 13 ) )
                  IF !( StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) ) == "OK" )
                     RETURN -9
                  ENDIF
               ENDIF
            ENDIF

            port_send( smsctx[ _SMSCTX_xHnd ], 'AT+CMGS="' + cPhoneNo + '"' + Chr( 13 ) )
            IF StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) ) == "> "
               port_send( smsctx[ _SMSCTX_xHnd ], StrTran( cText, Chr( 13 ) ) + Chr( 26 ) )
               tmp := StripCRLF( port_rece( smsctx[ _SMSCTX_xHnd ] ) )
               IF hb_LeftIs( tmp, "+CMGS: " )
                  RETURN 0
               ELSE
                  RETURN -10
               ENDIF
            ELSE
               RETURN -11
            ENDIF
         ELSE
            RETURN -12
         ENDIF
      ELSE
         RETURN -4
      ENDIF
   ELSE
      RETURN -3
   ENDIF

   RETURN -2

FUNCTION smsctx_Receive( smsctx )

   IF ! HB_ISARRAY( smsctx ) .OR. Len( smsctx ) != _SMSCTX_MAX_
      RETURN NIL
   ENDIF

   // ...

   RETURN {}

FUNCTION smsctx_PIN( smsctx, cPIN )

   LOCAL cOldValue

   IF ! HB_ISARRAY( smsctx ) .OR. Len( smsctx ) != _SMSCTX_MAX_
      RETURN NIL
   ENDIF

   cOldValue := smsctx[ _SMSCTX_cPIN ]
   IF cPIN == NIL .OR. ( HB_ISSTRING( cPIN ) .AND. Len( cPIN ) == 4 )
      smsctx[ _SMSCTX_cPIN ] := cPIN
   ENDIF

   RETURN cOldValue

#if 0
STATIC FUNCTION StripCR( cString )
   RETURN StrTran( cString, Chr( 13 ) )
#endif

STATIC FUNCTION StripCRLF( cString )
   RETURN StrTran( cString, Chr( 13 ) + Chr( 10 ) )

STATIC FUNCTION IsOK( cString )

   LOCAL tmp := GetLines( cString )

   RETURN ! Empty( tmp ) .AND. ATail( tmp ) == "OK"

STATIC FUNCTION GetLines( cString )

   LOCAL aLine := {}
   LOCAL tmp

   IF hb_LeftIs( cString, Chr( 13 ) + Chr( 10 ) )
      cString := SubStr( cString, Len( Chr( 13 ) + Chr( 10 ) ) + 1 )
   ENDIF
   IF Right( cString, 2 ) == Chr( 13 ) + Chr( 10 )
      cString := hb_StrShrink( cString, Len( Chr( 13 ) + Chr( 10 ) ) )
   ENDIF

   FOR EACH tmp IN hb_ATokens( StrTran( cString, Chr( 13 ) ), Chr( 10 ) )
      AAdd( aLine, tmp )
   NEXT

   RETURN aLine

STATIC FUNCTION GetList( cString )

   LOCAL aList := {}
   LOCAL tmp

   FOR EACH tmp IN hb_ATokens( cString, "," )
      AAdd( aList, tmp )
   NEXT

   RETURN aList

STATIC FUNCTION MakeList( aList )

   LOCAL cString := ""
   LOCAL tmp

   FOR EACH tmp IN aList
      cString += tmp + ","
   NEXT

   RETURN hb_StrShrink( cString, Len( "," ) )

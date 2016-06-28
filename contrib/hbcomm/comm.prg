/*
 * HBCOMM compatibility library. EXPERIMENTAL CODE. USE AT YOUR OWN RISK. NO GUARANTEES.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#include "hbcom.ch"

STATIC s_hPort := { => }
STATIC s_hbcomm_mutex := hb_mutexCreate()

FUNCTION INIT_PORT( cPort, nBaud, nData, nParity, nStop, nBufferSize )

   LOCAL nPort
   LOCAL cParity

   hb_mutexLock( s_hbcomm_mutex )

   IF HB_ISSTRING( cPort ) .AND. ;
      ( nPort := hb_comFindPort( cPort, .T. ) ) != 0 .AND. ;
      hb_comOpen( nPort )

      HB_SYMBOL_UNUSED( nBufferSize )

      cParity := "N"
      IF HB_ISNUMERIC( nParity )
         SWITCH nParity
         CASE 1 ; cParity := "O" ; EXIT
         CASE 2 ; cParity := "M" ; EXIT
         CASE 3 ; cParity := "E" ; EXIT
         ENDSWITCH
      ENDIF

      IF hb_comInit( nPort, hb_defaultValue( nBaud, 9600 ), cParity, nData, hb_defaultValue( nStop, 1 ) )
         s_hPort[ nPort ] := NIL
      ELSE
         hb_comClose( nPort )
         nPort := 0
      ENDIF
   ELSE
      nPort := 0
   ENDIF

   hb_mutexUnlock( s_hbcomm_mutex )

   RETURN nPort

/* Purge output buffer */
FUNCTION OUTBUFCLR( nPort )
   RETURN hb_comFlush( nPort, HB_COM_OFLUSH )

/* See if port is opened correctly */
FUNCTION ISWORKING( nPort )
   RETURN nPort $ s_hPort

/* NOTE: INCOMPATIBILITY.
         In contratry to original HBCOMM, here <cData> must be passed by reference.
         HBCOMM could corrupt HVM because of its buggy way of returning data.
         [vszakats] */
/* Fetch <nCount> chars into <cData> */
FUNCTION INCHR( nPort, nCount, /* @ */ cData )

   hb_default( @nCount, 0 )

   cData := Replicate( hb_BChar( 0 ), nCount )

   RETURN hb_comRecv( nPort, @cData, nCount )

/* Send out characters. Returns .T. if successful. */
FUNCTION OUTCHR( nPort, cData )

   LOCAL nLen

   DO WHILE ! HB_ISNULL( cData )

      /* I expect at least some data to be sent in a second */
      IF ( nLen := hb_comSend( nPort, cData,, 1000 ) ) <= 0
         RETURN .F.
      ENDIF

      cData := hb_BSubStr( cData, nLen + 1 )
   ENDDO

   RETURN .T.

/* Find out how many chars are in input buffer */
FUNCTION INBUFSIZE( nPort )
   RETURN hb_comInputCount( nPort )

/* Find out how many characters are in output buffer */
FUNCTION OUTBUFSIZE( nPort )
   RETURN hb_comOutputCount( nPort )

/* Close port and clear handle */
FUNCTION UNINT_PORT( nPort )

   LOCAL lRetVal

   hb_mutexLock( s_hbcomm_mutex )

   IF nPort $ s_hPort .AND. hb_comClose( nPort )
      hb_HDel( s_hPort, nPort )
      lRetVal := .T.
   ELSE
      lRetVal := .F.
   ENDIF

   hb_mutexUnlock( s_hbcomm_mutex )

   RETURN lRetVal

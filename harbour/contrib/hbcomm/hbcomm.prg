/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HBCOMM compatibility library. EXPERIMENTAL CODE. USE AT YOUR OWN RISK. NO GUARANTEES.
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
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

#include "common.ch"

STATIC s_hPort := { => }
STATIC s_hbcomm_mutex := hb_mutexCreate()

FUNCTION INIT_PORT( cPort, nBaud, nData, nParity, nStop, nBufferSize )
   LOCAL nPort
   LOCAL cOldPortName
   LOCAL cParity

   hb_mutexLock( s_hbcomm_mutex )

   /* TOFIX: We should get that number from core to avoid
             getting mixed up with com port access outside this
             compatibility interface. [vszakats] */
   nPort := Len( s_hPort ) + 1

   IF ISCHARACTER( cPort )
      cOldPortName := com_DevName( nPort, cPort )
   ENDIF

   IF com_Open( nPort )

      IF ! ISNUMBER( nBaud )
         nBaud := 9600
      ENDIF

      cParity := "N"
      IF ISNUMBER( nParity )
         SWITCH nParity
         CASE 0 ; cParity := "N" ; EXIT
         CASE 1 ; cParity := "O" ; EXIT
         CASE 2 ; cParity := "M" ; EXIT
         CASE 3 ; cParity := "E" ; EXIT
         ENDSWITCH
      ENDIF

      IF ! ISNUMBER( nStop )
         nStop := 1
      ENDIF

      HB_SYMBOL_UNUSED( nBufferSize )

      IF com_Init( nPort, nBaud, cParity, nData, nStop )
         s_hPort[ nPort ] := cOldPortName
         hb_mutexUnLock( s_hbcomm_mutex )
         RETURN nPort
      ELSE
         com_Close( nPort )
      ENDIF
   ENDIF

   IF cOldPortName != NIL
      com_DevName( nPort, cOldPortName )
   ENDIF

   hb_mutexUnLock( s_hbcomm_mutex )

   RETURN 0

/* Purge output buffer */
FUNCTION OUTBUFCLR( nPort )
   RETURN com_sflush( nPort )

/* See if port is opened correctly */
FUNCTION ISWORKING( nPort )
   RETURN nPort $ s_hPort

/* NOTE: INCOMPATIBILITY.
         In contratry to original HBCOMM, here <cString> must be passed by reference.
         HBCOMM could corrupt HVM because of its buggy way of returning data.
         [vszakats] */
/* Fetch <nCount> chars into <cString> */
FUNCTION INCHR( nPort, nCount, /* @ */ cString )
   cString := com_read( nPort, nCount )
   RETURN Len( cString )

/* Send out characters. Returns .t. if successful. */
FUNCTION OUTCHR( nPort, cString )
   RETURN com_send( nPort, cString ) == 0

/* Find out how many chars are in input buffer */
FUNCTION INBUFSIZE( nPort )
   RETURN com_count( nPort )

/* Find out how many characters are in out buf? */
FUNCTION OUTBUFSIZE( nPort )
   RETURN com_scount( nPort )

/* Close port and clear handle */
FUNCTION UNINT_PORT( nPort )
   LOCAL lRetVal := .F.

   hb_mutexLock( s_hbcomm_mutex )

   IF nPort $ s_hPort
      IF com_Close( nPort )
         /* Restore com port name */
         IF s_hPort[ nPort ] != NIL
            com_DevName( nPort, s_hPort[ nPort ] )
         ENDIF
         hb_HDel( s_hPort, nPort )
         lRetVal := .T.
      ENDIF
   ENDIF

   hb_mutexUnLock( s_hbcomm_mutex )

   RETURN lRetVal

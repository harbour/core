/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "error.ch"

/*----------------------------------------------------------------------*/

CREATE CLASS HbQtObjectHandler

   VAR    pPtr     /* TODO: Rename to __pPtr */

   VAR    __pSlots   PROTECTED
   VAR    __pEvents  PROTECTED

   METHOD fromPointer( pPtr )
   METHOD hasValidPointer()

   METHOD connect( cnEvent, bBlock )
   METHOD disconnect( cnEvent )

   ERROR HANDLER onError()

ENDCLASS

/*----------------------------------------------------------------------*/

/* TODO: Drop this function when all raw QT pointers are fully eliminated from .prg level. */
METHOD HbQtObjectHandler:fromPointer( pPtr )
   /* NOTE: Non-GC collected pointers are allowed here. */
   IF hb_isPointer( pPtr )
      ::pPtr := pPtr
   ELSE
      __hbqt_error()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

/* TODO: Drop this function, as it's not desired to have invalid QT pointers wrapped
         into valid .prg level QT objects.
         Currently it will return .F. for objects created using :fromPointer() */
METHOD HbQtObjectHandler:hasValidPointer()
   RETURN __hbqt_isPointer( ::pPtr )

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:onError()
   LOCAL cMsg := __GetMessage()
   LOCAL oError

   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   cMsg := "Message not found :" + cMsg

   oError := ErrorNew()

   oError:severity    := ES_ERROR
   oError:genCode     := EG_NOMETHOD
   oError:subSystem   := "HBQT"
   oError:subCode     := 1000
   oError:canRetry    := .F.
   oError:canDefault  := .F.
   oError:Args        := hb_AParams()
   oError:operation   := ProcName()
   oError:Description := cMsg

   Eval( ErrorBlock(), oError )

   RETURN NIL

/*----------------------------------------------------------------------*/

/* TOFIX: Eliminate that */
STATIC FUNCTION HBQEventsFromPointer( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   RETURN HB_HBQEvents():fromPointer( ... )

METHOD HbQtObjectHandler:connect( cnEvent, bBlock )
   LOCAL nResult

   SWITCH ValType( cnEvent )
   CASE "C"

      IF Empty( ::__pSlots )
         ::__pSlots := __hbqt_slots_new()
      ENDIF
      nResult := __hbqt_slots_connect( ::__pSlots, ::pPtr, cnEvent, bBlock )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE 8 /* QT connect call failure */
         RETURN .F.
      ENDSWITCH
      EXIT

   CASE "N"

      IF Empty( ::__pEvents )
         ::__pEvents := __hbqt_events_new()
         ::installEventFilter( HBQEventsFromPointer( ::__pEvents ) )
      ENDIF
      nResult := __hbqt_events_connect( ::__pEvents, ::pPtr, cnEvent, bBlock )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE -3 /* event already connected */
         RETURN .F.
      ENDSWITCH
      EXIT

   OTHERWISE
      nResult := 99
   ENDSWITCH

   __hbqt_error( 1200 + nResult )

   RETURN .F.

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:disconnect( cnEvent )
   LOCAL nResult

   SWITCH ValType( cnEvent )
   CASE "C"

      nResult := __hbqt_slots_disconnect( ::__pSlots, ::pPtr, cnEvent )

      SWITCH nResult
      CASE 0
      CASE 4 /* signal not found in object */
      CASE 5 /* disconnect failure */
         RETURN .T.
      CASE 1 /* wrong slot container, no connect was called yet */
      CASE 3 /* event not found */
         RETURN .F.
      ENDSWITCH
      EXIT

   CASE "N"

      nResult := __hbqt_events_disconnect( ::__pEvents, ::pPtr, cnEvent )

      SWITCH nResult
      CASE 0
         RETURN .T.
      CASE -3 /* event not found */
         RETURN .F.
      ENDSWITCH
      EXIT

   OTHERWISE
      nResult := 99
   ENDSWITCH

   __hbqt_error( 1300 + nResult )

   RETURN .F.

/*----------------------------------------------------------------------*/

FUNCTION hbqt_promotWidget( oWidget, cWidgetTo )
   LOCAL oObj := Eval( &( "{|| HB_" + cWidgetTo + "() }" ) )

   oObj:pPtr := oWidget:pPtr

   RETURN oObj

/*----------------------------------------------------------------------*/

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

CLASS HbQtObjectHandler

   VAR    pPtr
   VAR    pSlots
   VAR    pEvents

   METHOD from( xObject )
   METHOD fromPointer( pPtr )
   METHOD hasValidPointer()

   METHOD connect( cnEvent, bBlock )
   METHOD disconnect( cnEvent )

   ERROR HANDLER onError()

ENDCLASS

/*----------------------------------------------------------------------*/

/* NOTE: Deprecated: passing raw pointers to this function
   TODO: Generate RTE when non QT object is passed.
   TODO: Move thid to class implementation level so that proper object
         type checking can be done. */
METHOD HbQtObjectHandler:from( xObject )
   LOCAL pPtr
   IF hb_isPointer( pPtr := hbqt_ptr( xObject ) )
      ::pPtr := pPtr
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

/* TODO: Drop this function when all raw QT pointers are fully eliminated from .prg level. */
METHOD HbQtObjectHandler:fromPointer( pPtr )
   IF hb_isPointer( pPtr )
      ::pPtr := pPtr
   ELSE
      hbqt_Error()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

/* TODO: Drop this function, as it's not desired to have invalid QT pointers wrapped
         into valid .prg level QT objects. */
METHOD HbQtObjectHandler:hasValidPointer()
   RETURN __hbqt_IsValidPointer( ::pPtr )

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

METHOD HbQtObjectHandler:connect( cnEvent, bBlock )

   SWITCH ValType( cnEvent )
   CASE "C"

      IF Empty( ::pSlots )
         ::pSlots := Qt_Slots_New()
      ENDIF
      RETURN Qt_Slots_Connect( ::pSlots, ::pPtr, cnEvent, bBlock )

   CASE "N"

      IF Empty( ::pEvents )
         ::pEvents := Qt_Events_New()
         ::installEventFilter( HBQEventsFromPointer( ::pEvents ) )
      ENDIF
      RETURN Qt_Events_Connect( ::pEvents, ::pPtr, cnEvent, bBlock )

   ENDSWITCH

   RETURN .F.

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:disconnect( cnEvent )

   SWITCH ValType( cnEvent )
   CASE "C"

      IF ! Empty( ::pSlots )
         RETURN Qt_Slots_DisConnect( ::pSlots, ::pPtr, cnEvent )
      ENDIF
      EXIT

   CASE "N"

      IF ! Empty( ::pEvents )
         RETURN Qt_Events_DisConnect( ::pEvents, ::pPtr, cnEvent )
      ENDIF
      EXIT

   ENDSWITCH

   RETURN .F.

/*----------------------------------------------------------------------*/

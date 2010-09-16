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
#include "common.ch"
#include "error.ch"
#include "hbtrace.ch"

/*----------------------------------------------------------------------*/

CLASS HbQtObjectHandler

   VAR    pPtr
   VAR    pSlots
   VAR    pEvents

   METHOD configure( xObject )
   METHOD from( xObject )                         INLINE ::configure( xObject )

   METHOD connect( cnEvent, bBlock )
   METHOD disconnect( cnEvent )

   ERROR HANDLER onError()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:configure( xObject )
   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF
   RETURN Self

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

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:connect( cnEvent, bBlock )
   LOCAL cType := valtype( cnEvent )

   IF cType == "C"
      IF empty( ::pSlots )
         ::pSlots := Qt_Slots_New()
      ENDIF
      RETURN Qt_Slots_Connect( ::pSlots, ::pPtr, cnEvent, bBlock )

   ELSEIF cType == "N"
      IF empty( ::pEvents )
         ::pEvents := Qt_Events_New()
         ::installEventFilter( ::pEvents )
      ENDIF
      RETURN Qt_Events_Connect( ::pEvents, ::pPtr, cnEvent, bBlock )

   ENDIF
   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD HbQtObjectHandler:disconnect( cnEvent )
   LOCAL cType := valtype( cnEvent )
   IF cType == "C"
      IF ! empty( ::pSlots )
         RETURN Qt_Slots_DisConnect( ::pSlots, ::pPtr, cnEvent )
      ENDIF

   ELSEIF cType == "N"
      IF ! empty( ::pEvents )
         RETURN Qt_Events_DisConnect( ::pEvents, ::pPtr, cnEvent )
      ENDIF

   ENDIF
   RETURN .f.

/*----------------------------------------------------------------------*/

FUNCTION hbqt_ptr( xParam )
   #if 0
   LOCAL cClsName

   IF hb_isObject( xParam )
      cClsName := __ObjGetClsName( xParam )

      IF left( cClsName, 1 ) == "Q"
         RETURN xParam:pPtr

      ELSEIF left( cClsName, 2 ) == "HB"
         RETURN xParam:pPtr

      ELSE /* we don't care, programmer is at a fault */

      ENDIF
   ENDIF
   #else
   IF hb_isObject( xParam ) .AND. __objHasMsg( xParam, "PPTR" )
      RETURN xParam:pPtr
   ENDIF
   #endif

   RETURN xParam

/*----------------------------------------------------------------------*/

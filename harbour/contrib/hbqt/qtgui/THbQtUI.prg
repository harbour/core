/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * THbQtUI class
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                     Harbour Parts HbQtUI Class
 *
 *                 Pritpal Bedi <pritpal@vouchcac.com>
 *                              28Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "error.ch"
#include "hbtrace.ch"

/*----------------------------------------------------------------------*/

CREATE CLASS HbQtUI INHERIT HbQtObjectHandler

   VAR oWidget          /* TOFIX: User code uses this directly. Then rename this to __oRootWidget and make it PROTECTED. */
   VAR qObj INIT { => } /* TOFIX: User code uses this directly. Then rename this to __hWidget and make it PROTECTED. */

   METHOD new( oRootWidget, hWidget )
   METHOD destroy()
   DESTRUCTOR _destroy()

   ERROR HANDLER __OnError( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbQtUI:new( oRootWidget, hWidget )

   ::oWidget := oRootWidget
   ::qObj := hWidget

   /* QUESTION: Is this needed? */
   ::pPtr := oRootWidget:pPtr

   RETURN Self

/*----------------------------------------------------------------------*/

/* QUESTION: Is this needed? */
METHOD HbQtUI:_destroy()
   RETURN ::destroy()

/*----------------------------------------------------------------------*/

METHOD HbQtUI:destroy()
   LOCAL oObj

   IF !empty( ::oWidget )
      ::oWidget := NIL

      FOR EACH oObj IN ::qObj DESCEND
         oObj := NIL
      NEXT
      ::qObj := NIL
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD HbQtUI:__OnError( ... )
   LOCAL cMsg := __GetMessage()

   LOCAL oError

   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   IF Left( cMsg, 2 ) == "Q_"
      IF SubStr( cMsg, 3 ) $ ::qObj
         RETURN ::qObj[ SubStr( cMsg, 3 ) ]
      ELSE
         oError := ErrorNew()

         oError:severity    := ES_ERROR
         oError:genCode     := EG_ARG
         oError:subSystem   := "HBQT"
         oError:subCode     := 1001
         oError:canRetry    := .F.
         oError:canDefault  := .F.
         oError:Args        := hb_AParams()
         oError:operation   := ProcName()
         oError:Description := "Control <" + substr( cMsg, 3 ) + "> does not exist"

         Eval( ErrorBlock(), oError )
      ENDIF
   ELSEIF ::oWidget:hasValidPointer() /* QUESTION: Why do we need this? */
      RETURN ::oWidget:&cMsg( ... )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION q__tr( p1, p2, p3, p4 )

   HB_SYMBOL_UNUSED( p1 )
   HB_SYMBOL_UNUSED( p3 )
   HB_SYMBOL_UNUSED( p4 )

   RETURN p2

/*----------------------------------------------------------------------*/

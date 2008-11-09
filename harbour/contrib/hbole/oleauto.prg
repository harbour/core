/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OLE library
 *
 * Copyright 2000,2003 Jose F. Gimenez (JFG) <jfgimenez@wanadoo.es>
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


#include "hbclass.ch"
#include "common.ch"
#include "error.ch"

#define EG_OLEEXCEPTION 1001

CLASS TOleAuto

   DATA hObj

   METHOD New( cAutoObj ) CONSTRUCTOR
   METHOD GetActiveObject( cClass )
   METHOD End()

   METHOD Invoke( cMember, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   
   ERROR HANDLER OnError( cMsg, nError )

ENDCLASS

//--------------------------------------------------------------------

STATIC PROCEDURE THROW( oError )
   LOCAL lError := Eval( ErrorBlock(), oError )
   IF !HB_ISLOGICAL( lError ) .OR. lError
      __ErrInHandler()
   ENDIF
   Break( oError )
RETURN

METHOD New( uObj ) CLASS TOleAuto
   LOCAL oErr

   IF ISCHARACTER( uObj )
      ::hObj := CreateOleObject( uObj )
   ELSE
      ::hObj := uObj
   ENDIF

   IF Empty( ::hObj )
       oErr := ErrorNew()
       oErr:Args          := hb_AParams()
       oErr:CanDefault    := .F.
       oErr:CanRetry      := .F.
       oErr:CanSubstitute := .T.
       oErr:Description   := Ole2TxtError()
       oErr:GenCode       := EG_OLEEXCEPTION
       oErr:Operation     := ProcName()
       oErr:Severity      := ES_ERROR
       oErr:SubCode       := -1
       oErr:SubSystem     := "TOleAuto"

       RETURN Throw( oErr )
   ENDIF

   RETURN Self

METHOD GetActiveObject( cClass ) CLASS TOleAuto

   IF ISCHARACTER( cClass )
      ::hObj := GetOleObject( cClass )
      // ::cClassName := cClass
   ELSE
      Alert( "OLE interface: Invalid parameter type to constructor TOleAuto():GetActiveObject()" )
      ::hObj := NIL
   ENDIF

   RETURN Self

//--------------------------------------------------------------------

METHOD End() CLASS TOleAuto

   ::hObj := NIL

   RETURN NIL

//--------------------------------------------------------------------

METHOD Invoke( cMethod, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj

   IF uParam6 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSEIF uParam5 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1, uParam2, uParam3, uParam4, uParam5 )
   ELSEIF uParam4 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1, uParam2, uParam3, uParam4 )
   ELSEIF uParam3 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1, uParam2, uParam3 )
   ELSEIF uParam2 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1, uParam2 )
   ELSEIF uParam1 != NIL
      uObj := OLEInvoke( ::hObj, cMethod, uParam1 )
   ELSE
      uObj := OLEInvoke( ::hObj, cMethod )
   ENDIF

   IF OleIsObject()
      RETURN TOleAuto():New( uObj )
   ELSEIF Ole2TxtError() == "DISP_E_EXCEPTION"
      Alert( "OLE exception: " + OleExceptionSource() + ": " + OleExceptionDescription() )
      RETURN Self
   ELSEIF OleError() != 0
      Alert( "OLE error1: " + cMethod + ":   " + Ole2TxtError() )
   ENDIF

   RETURN uObj

//--------------------------------------------------------------------

METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   IF uParam6 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSEIF uParam5 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4, uParam5 )
   ELSEIF uParam4 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4 )
   ELSEIF uParam3 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3 )
   ELSEIF uParam2 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1, uParam2 )
   ELSEIF uParam1 != NIL
      OLESetProperty( ::hObj, cProperty, uParam1 )
   ENDIF

   IF Ole2TxtError() == "DISP_E_EXCEPTION"
      OLEShowException()
   ELSEIF OleError() != 0
      Alert( "OLE error2: " + cProperty + ":   " + Ole2TxtError() )
   ENDIF

   RETURN nil

//--------------------------------------------------------------------

METHOD Get( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj

   IF uParam6 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSEIF uParam5 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4, uParam5 )
   ELSEIF uParam4 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3, uParam4 )
   ELSEIF uParam3 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1, uParam2, uParam3 )
   ELSEIF uParam2 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1, uParam2 )
   ELSEIF uParam1 != NIL
      uObj := OLEGetProperty( ::hObj, cProperty, uParam1 )
   ELSE
      uObj := OLEGetProperty( ::hObj, cProperty )
   ENDIF

   IF Ole2TxtError() $ "DISP_E_MEMBERNOTFOUND | "+;
                       "DISP_E_BADPARAMCOUNT | " +;
                       "DISP_E_EXCEPTION"
      uObj := ::Invoke( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSE
      IF OleIsObject()
         RETURN TOleAuto():New( uObj )
      ELSEIF OleError() != 0
         Alert( "OLE error3: " + cProperty + ":   " + Ole2TxtError() )
      ENDIF
   ENDIF

   RETURN uObj

//--------------------------------------------------------------------

METHOD OnError( uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL cMsg := __GetMessage()

   IF !( Left( cMsg, 1 ) == "_" )
      RETURN ::Get( cMsg, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ENDIF

   RETURN ::Set( SubStr( cMsg, 2 ), uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )

EXIT PROCEDURE OLEEXIT()

   OLEUninitialize()

   RETURN

FUNCTION CreateObject( cString )
   RETURN TOleAuto():New( cString )

FUNCTION GetActiveObject( cString )
   RETURN TOleAuto():GetActiveObject( cString )

PROCEDURE OleShowException()

   Alert( OleExceptionSource() + ": " + OleExceptionDescription() )

   RETURN

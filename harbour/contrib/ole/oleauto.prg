/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OLE library
 *
 * Copyright 2000,2003 José F. Giménez (JFG) <jfgimenez@wanadoo.es>
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

METHOD New( uObj ) CLASS TOleAuto

   IF ValType( uObj )="C"
      ::hObj := CreateOleObject( uObj )
   ELSE
      ::hObj := uObj
   ENDIF

RETURN Self

METHOD GetActiveObject( cClass ) CLASS TOleAuto

   IF ValType( cClass ) = 'C'
      ::hObj := GetOleObject( cClass )
      // ::cClassName := cClass
   ELSE
      MessageBox( 0,"Invalid parameter type to constructor TOleAuto():GetActiveObject()!", "OLE Interface",0 )
      ::hObj := 0
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
      OLEShowException()
      RETURN Self
   ELSEIF OleError() != 0
      MessageBox( 0,cMethod + ":   " + Ole2TxtError(), "OLE Error",0 )
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD Set( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL uObj

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
      MessageBox( 0,cProperty + ":   " + Ole2TxtError(), "OLE Error",0 )
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

   IF Ole2TxtError() $ "DISP_E_MEMBERNOTFOUND | DISP_E_BADPARAMCOUNT | " + ;
                       "DISP_E_EXCEPTION"
      uObj := ::Invoke( cProperty, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSE
      IF OleIsObject()
         RETURN TOleAuto():New( uObj )
      ELSEIF OleError() != 0
         MessageBox( 0,cProperty + ":   " + Ole2TxtError(), "OLE Error",0 )
      ENDIF
   ENDIF

RETURN uObj

//--------------------------------------------------------------------

METHOD OnError( uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 ) CLASS TOleAuto

   LOCAL cMsg := __GetMessage()

   LOCAL uObj

   IF LEFT( cMsg, 1 ) == '_'
      ::Set( SUBS( cMsg, 2 ), uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ELSE
      uObj := ::Get( cMsg, uParam1, uParam2, uParam3, uParam4, uParam5, uParam6 )
   ENDIF

RETURN uObj

EXIT PROCEDURE OLEEXIT

   OLEUninitialize()

RETURN
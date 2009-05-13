/*
 * $Id$
 */

/*
 * Copyright 2002  Jose F. Gimenez (JFG) - <jfgimenez@wanadoo.es>
 *                 Ron Pinkas            - <ron@ronpinkas.com>
 *
 * www - http://www.xharbour.org
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * xHarbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef __PLATFORM__WINDOWS
  FUNCTION CreateObject()
     RETURN NIL

  FUNCTION GetActiveObject()
     RETURN NIL
#else

#define HB_CLS_NOTOBJECT

#include "common.ch"
#include "hbclass.ch"
#include "error.ch"

#ifndef __XHARBOUR__

#define EG_OLEEXCEPTION 1001

#xcommand TRY              => BEGIN SEQUENCE WITH t_bBreak
#xcommand CATCH [<!oErr!>] => RECOVER [USING <oErr>] <-oErr->
#xcommand FINALLY          => ALWAYS

THREAD STATIC t_bBreak := { |oErr| break( oErr ) }

STATIC PROCEDURE THROW( oError )
   LOCAL lError := Eval( ErrorBlock(), oError )
   IF !HB_ISLOGICAL( lError ) .OR. lError
      __ErrInHandler()
   ENDIF
   Break( oError )
RETURN

#endif

//----------------------------------------------------------------------------//

FUNCTION CreateObject( cString, cLicense )

RETURN TOleAuto():New( cString, , cLicense )

//----------------------------------------------------------------------------//

FUNCTION GetActiveObject( cString )

RETURN TOleAuto():GetActiveObject( cString )

//----------------------------------------------------------------------------//

INIT PROCEDURE HB_OleInit()

   /* It's important to store value returned by __HB_OLE_INIT() in
    * STATIC variable. When HVM will clear STATICs on HVM exit
    * then it will execute destructor bound with this variable which
    * calls OleUninitialize() - such method causes that OleUninitialize()
    * will be called very lately after all user EXIT functions, ALWAYS
    * blocks and .prg object destructors which may also use OLE.
    */
   static s_ole

   s_ole := __HB_OLE_INIT()

RETURN

//----------------------------------------------------------------------------//

CLASS VTWrapper
   DATA vt
   DATA Value

   METHOD New( vt, xVal ) CONSTRUCTOR
ENDCLASS

//----------------------------------------------------------------------------//
METHOD New( vt, xVal ) CLASS VTWrapper

   ::vt := vt
   ::Value := xVal

   //TraceLog( vt, ::vt, xVal, ::Value )

RETURN Self

//----------------------------------------------------------------------------//
CLASS VTArrayWrapper FROM VTWrapper

   METHOD AsArray( nIndex, xValue ) OPERATOR "[]"
   METHOD __enumStart( enum, lDescend )

ENDCLASS

//----------------------------------------------------------------------------//
METHOD AsArray( nIndex, xValue ) CLASS VTArrayWrapper

RETURN IIF( PCount() == 1, ::Value[nIndex], ::Value[nIndex] := xValue )

//----------------------------------------------------------------------------//
METHOD __enumStart( enum, lDescend ) CLASS VTarrayWrapper

   HB_SYMBOL_UNUSED( lDescend )

   /* set base value for enumerator */
   (@enum):__enumBase( ::Value )

RETURN !Empty( ::Value )

//----------------------------------------------------------------------------//
CLASS TOleAuto

   DATA hObj
   DATA cClassName
   DATA pOleEnumerator

   METHOD New( uObj, cClass, cLicense ) CONSTRUCTOR
   METHOD GetActiveObject( cClass ) CONSTRUCTOR

   METHOD Invoke()
   MESSAGE CallMethod  METHOD Invoke()

   METHOD Set()
   MESSAGE SetProperty METHOD Set()

   METHOD Get()
   MESSAGE GetProperty METHOD Get()

   METHOD OleValue()
   METHOD _OleValue( xSetValue )

   METHOD OleNewEnumerator()

   METHOD OleCollection( xIndex, xValue ) OPERATOR "[]"

   METHOD OleValuePlus( xArg )            OPERATOR "+"
   METHOD OleValueMinus( xArg )           OPERATOR "-"
   METHOD OleValueMultiply( xArg )        OPERATOR "*"
   METHOD OleValueDivide( xArg )          OPERATOR "/"
   METHOD OleValueModulus( xArg )         OPERATOR "%"
   METHOD OleValueInc()                   OPERATOR "++"
   METHOD OleValueDec()                   OPERATOR "--"
   METHOD OleValuePower( xArg )           OPERATOR "^"

   METHOD OleValueEqual( xArg )           OPERATOR "="
   METHOD OleValueExactEqual( xArg )      OPERATOR "=="
   METHOD OleValueNotEqual( xArg )        OPERATOR "!="

   METHOD __enumStart( enum, lDescend )
   METHOD __enumSkip( enum, lDescend )
   METHOD __enumStop()

   ERROR HANDLER OnError()

   DESTRUCTOR Release()

   // Needed to refernce, or hb_dynsymFindName() will fail
   METHOD ForceSymbols() INLINE ::cClassName()

ENDCLASS

//--------------------------------------------------------------------
METHOD New( uObj, cClass, cLicense ) CLASS TOleAuto

   LOCAL oErr

   // Hack in case OLE Server already created and New() is attempted as an OLE Method.
   IF ::hObj != NIL
      RETURN HB_ExecFromArray( Self, "_New", HB_aParams() )
   ENDIF

   IF ISCHARACTER( uObj )
      ::hObj := CreateOleObject( uObj, , cLicense )

      IF OleError() != 0
         IF Ole2TxtError() == "DISP_E_EXCEPTION"
            oErr := ErrorNew()
            oErr:Args          := HB_aParams()
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := OLEExceptionDescription()
            oErr:GenCode       := EG_OLEEXCEPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := OLEExceptionSource()

            RETURN Throw( oErr )
         ELSE
            oErr := ErrorNew()
            oErr:Args          := HB_aParams()
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
      ENDIF

      ::cClassName := uObj
   ELSEIF ISNUMBER( uObj )
      OleAddRef( uObj )
      ::hObj := uObj

      IF ISCHARACTER( cClass )
         ::cClassName := cClass
      ELSE
         ::cClassName := LTrim( Str( uObj ) )
      ENDIF
   ELSE
      oErr := ErrorNew()
      oErr:Args          := HB_aParams()
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Invalid argument to contructor!"
      oErr:GenCode       := 0
      oErr:Operation     := ProcName()
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "TOleAuto"

      RETURN Throw( oErr )
   ENDIF

RETURN Self

//--------------------------------------------------------------------
// Destructor!
PROCEDURE Release() CLASS TOleAuto

   //TraceLog( ::cClassName, ::hObj )

   IF ! Empty( ::hObj )
      //TraceLog( ::cClassName, ::hObj )
      OleReleaseObject( ::hObj )
      //::hObj := NIL
   ENDIF

RETURN

//--------------------------------------------------------------------
METHOD GetActiveObject( cClass ) CLASS TOleAuto

   LOCAL oErr

   IF ISCHARACTER( cClass )
      ::hObj := GetOleObject( cClass )

      IF OleError() != 0
         IF Ole2TxtError() == "DISP_E_EXCEPTION"
            oErr := ErrorNew()
            oErr:Args          := { cClass }
            oErr:CanDefault    := .F.
            oErr:CanRetry      := .F.
            oErr:CanSubstitute := .T.
            oErr:Description   := OLEExceptionDescription()
            oErr:GenCode       := EG_OLEEXCEPTION
            oErr:Operation     := ProcName()
            oErr:Severity      := ES_ERROR
            oErr:SubCode       := -1
            oErr:SubSystem     := OLEExceptionSource()

            RETURN Throw( oErr )
         ELSE
            oErr := ErrorNew()
            oErr:Args          := { cClass }
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
      ENDIF

      ::cClassName := cClass
   ELSE
      Alert( "OLE interface: Invalid parameter type to constructor TOleAuto():GetActiveObject()" )
      ::hObj := 0
   ENDIF

RETURN Self

//--------------------------------------------------------------------
METHOD OleCollection( xIndex, xValue ) CLASS TOleAuto

   LOCAL xRet

   //TraceLog( PCount(), xIndex, xValue )

   IF PCount() == 1
      RETURN ::Item( xIndex )
   ENDIF

   IF ISNUMBER( xIndex ) .AND. xIndex < 0
      xIndex += ( ::Count + 1 )
   ENDIF

   TRY
      // ASP Collection syntax.
      xRet := ::_Item( xIndex, xValue )
   CATCH
      xRet := ::SetItem( xIndex, xValue )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValuePlus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue + xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "+"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1081
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueMinus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue - xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "-"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1082
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueMultiply( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue * xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "*"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1083
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueDivide( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue / xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "/"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1084
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueModulus( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue % xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "%"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueInc() CLASS TOleAuto

   LOCAL oErr

   TRY
      ++::OleValue
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "++"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1086
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN Self

//--------------------------------------------------------------------
METHOD OleValueDec() CLASS TOleAuto

   LOCAL oErr

   TRY
      --::OleValue
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "--"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1087
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN Self

//--------------------------------------------------------------------
METHOD OleValuePower( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue ^ xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "^"
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1088
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ( ::OleValue = xArg ) /* NOTE: Intentionally using '=' operator. */
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "="
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueExactEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ( ::OleValue == xArg )
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "=="
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------
METHOD OleValueNotEqual( xArg ) CLASS TOleAuto

   LOCAL xRet, oErr

   TRY
      xRet := ::OleValue != xArg
   CATCH
      oErr := ErrorNew()
      oErr:Args          := { Self, xArg }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "argument error"
      oErr:GenCode       := EG_ARG
      oErr:Operation     := "!="
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := 1085
      oErr:SubSystem     := "BASE"

      RETURN Throw( oErr )
   END

RETURN xRet

//--------------------------------------------------------------------

METHOD __enumStart( enum, lDescend ) CLASS TOleAuto

   /* TODO: add support for descend order */
   ::pOleEnumerator := ::OleNewEnumerator()

RETURN ::__enumSkip( @enum, lDescend )

//--------------------------------------------------------------------

METHOD __enumSkip( enum, lDescend ) CLASS TOleAuto

   LOCAL lContinue, xValue

   /* TODO: add support for descend order */
   HB_SYMBOL_UNUSED( lDescend )

   xValue := __OLEENUMNEXT( ::pOleEnumerator, @lContinue )

   /* set enumerator value */
   (@enum):__enumValue( xValue )

RETURN lContinue

//--------------------------------------------------------------------

METHOD PROCEDURE __enumStop() CLASS TOleAuto

   __OLEENUMSTOP( ::pOleEnumerator )
   ::pOleEnumerator := NIL

RETURN

PROCEDURE OleShowException()

   Alert( OleExceptionSource() + ": " + OleExceptionDescription() )

   RETURN

#endif

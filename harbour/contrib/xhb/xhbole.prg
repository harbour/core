/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compatibility calls.
 *
 * Copyright 2011 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

#ifndef __PLATFORM__WINDOWS

FUNCTION TOleAuto()
   RETURN NIL

FUNCTION CreateObject()
   RETURN NIL

FUNCTION GetActiveObject()
   RETURN NIL

FUNCTION CreateOleObject()
   RETURN NIL

#else


#define HB_CLS_NOTOBJECT  /* avoid definition of method: INIT */

#include "hbclass.ch"

#include "error.ch"

#define EG_OLEEXCEPTION 1001
#define DISPID_VALUE    0

STATIC s_bBreak := {| oError | Break( oError ) }

STATIC FUNCTION s_oleOpError( cOperator, ... )

   STATIC sc_hErrCode := { ;
      "==" => 1070, ;
      "="  => 1071, ;
      "!=" => 1072, ;
      "+"  => 1081, ;
      "-"  => 1082, ;
      "*"  => 1083, ;
      "/"  => 1084, ;
      "%"  => 1085, ;
      "++" => 1086, ;
      "--" => 1087, ;
      "^"  => 1088 }
   LOCAL oErr

   oErr := ErrorNew()
   oErr:Args          := { ... }
   oErr:CanDefault    := .F.
   oErr:CanRetry      := .F.
   oErr:CanSubstitute := .T.
   oErr:Description   := "argument error"
   oErr:GenCode       := EG_ARG
   oErr:Operation     := cOperator
   oErr:Severity      := ES_ERROR
   oErr:SubCode       := sc_hErrCode[ cOperator ]
   oErr:SubSystem     := "BASE"

   RETURN oErr

STATIC FUNCTION s_oleError( nGenCode, cDescript )

   LOCAL oErr

   oErr := ErrorNew()
   oErr:Args          := hb_AParams( 1 )
   oErr:CanDefault    := .F.
   oErr:CanRetry      := .F.
   oErr:CanSubstitute := .T.
   IF PCount() != 0
      oErr:GenCode       := nGenCode
      oErr:Description   := cDescript
   ELSE
      oErr:GenCode       := EG_OLEEXCEPTION
      oErr:Description   := win_oleErrorText()
   ENDIF
   oErr:Operation     := ProcName( 1 )
   oErr:Severity      := ES_ERROR
   oErr:SubCode       := -1
   oErr:SubSystem     := "TOleAuto"

   RETURN oErr


CREATE CLASS TOLEAUTO FROM WIN_OLEAUTO

   VAR cClassName

   METHOD hObj( xOle ) SETGET
   METHOD New( xOle, cClass, cLicense )
   METHOD GetActiveObject( cClass )

   METHOD Invoke()      EXTERN __oleInvokeMethod()
   MESSAGE CallMethod   EXTERN __oleInvokeMethod()

   METHOD Set()         EXTERN __oleInvokePut()
   MESSAGE SetProperty  EXTERN __oleInvokePut()

   METHOD Get()         EXTERN __oleInvokeGet()
   MESSAGE GetProperty  EXTERN __oleInvokeGet()

   METHOD OleValue()
   METHOD _OleValue( xValue )

   METHOD OleValueExactEqual( xArg )      OPERATOR "=="
   METHOD OleValueEqual( xArg )           OPERATOR "="
   METHOD OleValueNotEqual( xArg )        OPERATOR "!="
   METHOD OleValuePlus( xArg )            OPERATOR "+"
   METHOD OleValueMinus( xArg )           OPERATOR "-"
   METHOD OleValueMultiply( xArg )        OPERATOR "*"
   METHOD OleValueDivide( xArg )          OPERATOR "/"
   METHOD OleValueModulus( xArg )         OPERATOR "%"
   METHOD OleValuePower( xArg )           OPERATOR "^"
   METHOD OleValueInc()                   OPERATOR "++"
   METHOD OleValueDec()                   OPERATOR "--"

ENDCLASS

METHOD hObj( xOle ) CLASS TOLEAUTO

   IF xOle != NIL
      IF HB_ISNUMERIC( xOle )
         xOle := __olePDisp( xOle )
      ENDIF
      IF __oleIsDisp( xOle )
         ::__hObj := xOle
      ENDIF
   ENDIF

   RETURN ::__hObj

METHOD New( xOle, cClass, cLicense ) CLASS TOLEAUTO

   LOCAL hOle

   IF HB_ISSTRING( xOle )
      hOle := __oleCreateObject( xOle,, cLicense )
      IF ! Empty( hOle )
         ::__hObj := hOle
         ::cClassName := xOle
      ELSE
         RETURN Throw( s_oleError() )
      ENDIF
   ELSE
      ::hObj := xOle
      IF ::__hObj == NIL
         RETURN Throw( s_oleError( 0, "Invalid argument to contructor!" ) )
      ELSEIF HB_ISSTRING( cClass )
         ::cClassName := cClass
      ELSE
         ::cClassName := hb_ntos( win_P2N( ::__hObj ) )
      ENDIF
   ENDIF

   RETURN Self

METHOD GetActiveObject( cClass ) CLASS TOLEAUTO

   IF HB_ISSTRING( cClass )
      IF ! Empty( ::__hObj := __oleGetActiveObject( cClass ) )
         ::cClassName := cClass
      ELSE
         RETURN Throw( s_oleError() )
      ENDIF
   ELSE
      wapi_MessageBox( , "Invalid parameter type to constructor TOleAuto():GetActiveObject()!", ;
         "OLE Interface", )
      ::__hObj := NIL
   ENDIF

   RETURN Self

METHOD OleValue() CLASS TOLEAUTO
   RETURN __oleInvokeGet( ::__hObj, DISPID_VALUE )

METHOD _OleValue( xValue ) CLASS TOLEAUTO
   RETURN __oleInvokePut( ::__hObj, DISPID_VALUE, xValue )

#xcommand OLE OPERATOR <op> METHOD <!mth!> [WITH <!arg!>] IS <exp> => ;
   METHOD <mth>( <arg> ) CLASS TOLEAUTO                     ;;
   LOCAL xRet                                               ;;
   BEGIN SEQUENCE WITH s_bBreak                             ;;
      xRet := ( <exp> )                                     ;;
   RECOVER                                                  ;;
      RETURN Throw( s_oleOpError( <op>, Self [, <arg>] ) )  ;;
   END SEQUENCE                                             ;;
   RETURN xRet

OLE OPERATOR "==" METHOD OleValueExactEqual WITH xArg IS ::OleValue == xArg

OLE OPERATOR "=" METHOD OleValueEqual WITH xArg IS ::OleValue = xArg

OLE OPERATOR "!=" METHOD OleValueNotEqual WITH xArg IS ::OleValue != xArg /* Intentionally using != operator */

OLE OPERATOR "+" METHOD OleValuePlus WITH xArg IS ::OleValue + xArg

OLE OPERATOR "-" METHOD OleValueMinus WITH xArg IS ::OleValue - xArg

OLE OPERATOR "*" METHOD OleValueMultiply WITH xArg IS ::OleValue * xArg

OLE OPERATOR "/" METHOD OleValueDivide WITH xArg IS ::OleValue / xArg

OLE OPERATOR "%" METHOD OleValueModulus WITH xArg IS ::OleValue % xArg

OLE OPERATOR "^" METHOD OleValuePower WITH xArg IS ::OleValue ^ xArg

OLE OPERATOR "++" METHOD OleValueInc IS ++::OleValue

OLE OPERATOR "--" METHOD OleValueDec IS --::OleValue


FUNCTION CreateObject( xOle, cLicense )
   RETURN TOleAuto():New( xOle,, cLicense )

FUNCTION GetActiveObject( cString )
   RETURN TOleAuto():GetActiveObject( cString )

FUNCTION CreateOleObject( ... )
   RETURN __oleCreateObject( ... )

#endif /* __PLATFORM__WINDOWS */

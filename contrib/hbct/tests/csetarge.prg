/*
 * Harbour Project source code:
 *   Test CT3 function CSetArgErr()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
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

#ifdef __HARBOUR__
#require "hbct"
#else
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

#include "ct.ch"

PROCEDURE Main()

   LOCAL cRet, olderr

   ctinit()

   ? "Begin test of CSetArgErr()"
   ?

   ?
   ? "Local error handler: "

   olderr := ErrorBlock( {| oerr | myerrhandler( oerr ) } )

   // standard behaviour on argument error
   ?
   ? "Standard behaviour"
   ? "  Call to AddAscii( 5789676, 1, 2, .T. ):"
   cRet := AddAscii( 5789676, 1, 2, .T. )
   ? "  return value was", cRet
   ?
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_WHOCARES on argument error
   ?
   ? "CT_ARGERR_WHOCARES behaviour"
   CSetArgErr( CT_ARGERR_WHOCARES )
   ? "  Call to AddAscii( 5789676, 1, 2, .T. ):"
   cRet := AddAscii( 5789676, 1, 2, .T. )
   ? "  return value was", cRet
   ?
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_WARNING on argument error
   ?
   ? "CT_ARGERR_WARNING behaviour"
   CSetArgErr( CT_ARGERR_WARNING )
   ? "  Call to AddAscii( 5789676, 1, 2, .T. ):"
   cRet := AddAscii( 5789676, 1, 2, .T. )
   ? "  return value was", cRet
   ?
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_ERROR on argument error
   ?
   ? "CT_ARGERR_ERROR behaviour"
   CSetArgErr( CT_ARGERR_ERROR )
   ? "  Call to AddAscii( 5789676, 1, 2, .T. ):"
   cRet := AddAscii( 5789676, 1, 2, .T. )
   ? "  return value was", cRet
   ?
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_CATASTROPHIC on argument error
   ?
   ? "CT_ARGERR_CATASTROPHIC behaviour"
   CSetArgErr( CT_ARGERR_CATASTROPHIC )
   ? "  Call to AddAscii( 5789676, 1, 2, .T. ):"
   cRet := AddAscii( 5789676, 1, 2, .T. )
   ? "  return value was", cRet
   ?
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   ?
   ? "Standard error handler: "
   ErrorBlock( olderr )

   // standard behaviour on argument error
   ?
   ? "Standard behaviour"
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_WHOCARES on argument error
   ?
   ? "CT_ARGERR_WHOCARES behaviour"
   CSetArgErr( CT_ARGERR_WHOCARES )
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_WARNING on argument error
   ?
   ? "CT_ARGERR_WARNING behaviour"
   CSetArgErr( CT_ARGERR_WARNING )
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_ERROR on argument error
   ?
   ? "CT_ARGERR_ERROR behaviour"
   CSetArgErr( CT_ARGERR_ERROR )
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   // CT_ARGERR_CATASTROPHIC on argument error
   ?
   ? "CT_ARGERR_CATASTROPHIC behaviour"
   CSetArgErr( CT_ARGERR_CATASTROPHIC )
   ? "  Call to CharAdd( 'AA', .F. ):"
   cRet := CharAdd( "AA", .F. )
   ? "  return value was", cRet, "<Press any key>"
   ?
   Inkey( 0 )

   ? "End test of CSetArgErr()"

   ctexit()

   RETURN

STATIC FUNCTION myerrhandler( oerr )

   LOCAL ni, nDigit

   LOCAL cInput

   ? "    Error handler called:"
   ? "      err:severity.....:", oerr:severity
   ? "      err:subSystem....:", oerr:subSystem
   ? "      err:operation....:", oerr:operation
   ? "      Len(err:args)....:", Len( oerr:args )
   FOR ni := 1 TO Len( oerr:args )
      ? "          err:args[ " + hb_ntos( ni ) + " ]:", oerr:args[ ni ]
   NEXT
   ? "      err:genCode......:", oerr:genCode
   ? "      err:subCode......:", oerr:subCode
   ? "      err:osCode.......:", oerr:osCode
   ? "      err:filename.....:", oerr:filename
   ? "      err:tries........:", oerr:tries
   ? "      err:cargo........:", oerr:cargo
   ? "      err:canDefault...:", oerr:canDefault
   ? "      err:canRetry.....:", oerr:canRetry
   ? "      err:canSubstitute:", oerr:canSubstitute
   ?

   IF oerr:canSubstitute

      ? "    Error handler can substitute return value, so please"
      ACCEPT "    type in return value <Return for default>: " TO cInput

      IF Empty( cInput )
         ? "    You have chosen the default return value. Ok, this should "
         ? "    be now problem, since the last digit of err:subCode indicates"
         ? "    the type of the return value:"
         ? "      0 is NIL,    1 is String,    2 is Integer,"
         ? "      3 is Float,  4 is Boolean,   5 is Date"
         ? "      6 is Block,  7 is Array,    8 is Object"
         ? "      9 is unknown"
         nDigit := Int( oerr:subCode % 10 )
         ? "    Here it's a " + hb_ntos( nDigit ) + ", so I return a "
         DO CASE
         CASE nDigit == 0
            ?? "NIL."
            cInput  := NIL

         CASE nDigit == 1
            ?? "String."
            cInput := ""

         CASE nDigit == 2
            ?? "Integer."
            cInput := 0

         CASE nDigit == 3
            ?? "Float."
            cInput := 0.0

         CASE nDigit == 4
            ?? "Boolean."
            cInput := .F.

         CASE nDigit == 5
            ?? "Date."
            cInput := CToD( "" ) /* Need to compile with CA-Cl*pper, too */

         CASE nDigit == 6
            ?? "Block."
            cInput := {|| NIL }

         CASE nDigit == 7
            ?? "Array."
            cInput := {}

         CASE nDigit == 8
            ?? "Object."
            cInput := GetNew()

         CASE nDigit == 9
            ?? "<don't know, NIL would be best."
            cInput := NIL

         ENDCASE

      ENDIF

      RETURN cInput

   ENDIF

   IF oerr:canDefault
      ? "    Subsystem can set the default value itself, so this error"
      ? "    is only informative."
   ENDIF

   RETURN .F.

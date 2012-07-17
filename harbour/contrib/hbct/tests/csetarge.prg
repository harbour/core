/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   Test CT3 function CSETARGERR()
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


#include "ct.ch"

PROCEDURE main

   LOCAL cRet, olderr

   ctinit()

   QOut( "Begin test of CSETARGERR()" )
   QOut( "" )

   QOut( "" )
   QOut( "Local error handler: " )

   olderr := ErrorBlock( { | oerr | myerrhandler( oerr ) } )

   // standard behaviour on argument error
   QOut( "" )
   QOut( "Standard behaviour" )
   QOut( "  Call to addascii(5789676,1,2,.T.):" )
   cRet := addascii( 5789676, 1, 2, .T. )
   QOut( "  return value was", cRet )
   QOut( "" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_WHOCARES on argument error
   QOut( "" )
   QOut( "CT_ARGERR_WHOCARES behaviour" )
   CSETARGERR( CT_ARGERR_WHOCARES )
   QOut( "  Call to addascii(5789676,1,2,.T.):" )
   cRet := addascii( 5789676, 1, 2, .T. )
   QOut( "  return value was", cRet )
   QOut( "" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_WARNING on argument error
   QOut( "" )
   QOut( "CT_ARGERR_WARNING behaviour" )
   CSETARGERR( CT_ARGERR_WARNING )
   QOut( "  Call to addascii(5789676,1,2,.T.):" )
   cRet := addascii( 5789676, 1, 2, .T. )
   QOut( "  return value was", cRet )
   QOut( "" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_ERROR on argument error
   QOut( "" )
   QOut( "CT_ARGERR_ERROR behaviour" )
   CSETARGERR( CT_ARGERR_ERROR )
   QOut( "  Call to addascii(5789676,1,2,.T.):" )
   cRet := addascii( 5789676, 1, 2, .T. )
   QOut( "  return value was", cRet )
   QOut( "" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_CATASTROPHIC on argument error
   QOut( "" )
   QOut( "CT_ARGERR_CATASTROPHIC behaviour" )
   CSETARGERR( CT_ARGERR_CATASTROPHIC )
   QOut( "  Call to addascii(5789676,1,2,.T.):" )
   cRet := addascii( 5789676, 1, 2, .T. )
   QOut( "  return value was", cRet )
   QOut( "" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   QOut( "" )
   QOut( "Standard error handler: " )
   ErrorBlock( olderr )

   // standard behaviour on argument error
   QOut( "" )
   QOut( "Standard behaviour" )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_WHOCARES on argument error
   QOut( "" )
   QOut( "CT_ARGERR_WHOCARES behaviour" )
   CSETARGERR( CT_ARGERR_WHOCARES )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_WARNING on argument error
   QOut( "" )
   QOut( "CT_ARGERR_WARNING behaviour" )
   CSETARGERR( CT_ARGERR_WARNING )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_ERROR on argument error
   QOut( "" )
   QOut( "CT_ARGERR_ERROR behaviour" )
   CSETARGERR( CT_ARGERR_ERROR )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   // CT_ARGERR_CATASTROPHIC on argument error
   QOut( "" )
   QOut( "CT_ARGERR_CATASTROPHIC behaviour" )
   CSETARGERR( CT_ARGERR_CATASTROPHIC )
   QOut( "  Call to charadd('AA',.F.):" )
   cRet := charadd( "AA", .F. )
   QOut( "  return value was", cRet, "<Press any key>" )
   QOut( "" )
   Inkey( 0 )

   QOut( "End test of CSETARGERR()" )

   ctexit()

   RETURN

FUNCTION myerrhandler( oerr )

   LOCAL ni, nDigit

   MEMVAR INPUT

   QOut( "    Error handler called:" )
   QOut( "      err:severity.....:", oerr:severity )
   QOut( "      err:subSystem....:", oerr:subSystem )
   QOut( "      err:operation....:", oerr:operation )
   QOut( "      len(err:args)....:", Len( oerr:args ) )
   FOR ni := 1 TO Len( oerr:args )
      QOut( "          err:args[" + hb_ntos( ni ) + "]..:", oerr:args[ni] )
   NEXT
   QOut( "      err:genCode......:", oerr:genCode )
   QOut( "      err:subCode......:", oerr:subCode )
   QOut( "      err:osCode.......:", oerr:osCode )
   QOut( "      err:filename.....:", oerr:filename )
   QOut( "      err:tries........:", oerr:tries )
   QOut( "      err:cargo........:", oerr:cargo )
   QOut( "      err:canDefault...:", oerr:canDefault )
   QOut( "      err:canRetry.....:", oerr:canRetry )
   QOut( "      err:canSubstitute:", oerr:canSubstitute )
   QOut()

   IF oerr:canSubstitute

      PRIVATE Input := ""

      QOut( "    Error handler can substitute return value, so please" )
      ACCEPT "    type in return value <Return for default>: " TO Input

      IF Empty( Input )
         QOut( "    You have chosen the default return value. Ok, this should " )
         QOut( "    be now problem, since the last digit of err:subCode indicates" )
         QOut( "    the type of the return value:" )
         QOut( "      0 is NIL,    1 is String,    2 is Integer," )
         QOut( "      3 is Float,  4 is Boolean,   5 is Date" )
         QOut( "      6 is Block,  7 is Array,    8 is Object" )
         QOut( "      9 is unknown" )
         nDigit := Int( oerr:subCode % 10 )
         QOut( "    Here it's a " + AllTrim( Str(nDigit ) ) + ", so I return a " )
         DO CASE
         CASE nDigit == 0
            QQOut( "NIL." )
            Input := NIL

         CASE nDigit == 1
            QQOut( "String." )
            Input := ""

         CASE nDigit == 2
            QQOut( "Integer." )
            Input := 0

         CASE nDigit == 3
            QQOut( "Float." )
            Input := 0.0

         CASE nDigit == 4
            QQOut( "Boolean." )
            Input := .F.

         CASE nDigit == 5
            QQOut( "Date." )
            Input := CToD( "" )

         CASE nDigit == 6
            QQOut( "Block." )
            Input := { ||NIL }

         CASE nDigit == 7
            QQOut( "Array." )
            Input := {}

         CASE nDigit == 8
            QQOut( "Object." )
            Input := GetNew()

         CASE nDigit == 9
            QQOut( "<don't know, NIL would be best." )
            Input := NIL

         ENDCASE

      ENDIF

      RETURN INPUT

   ENDIF

   IF oerr:canDefault
      QOut( "    Subsystem can set the default value itself, so this error" )
      QOut( "    is only informative." )
   ENDIF

   RETURN .F.

/*
 * Harbour Project source code:
 * Regression tests for class Get
 *
 * Copyright 1999-2007 Viktor Szakats (vszakats.net/harbour)
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

/* NOTE: This source can be compiled with both Harbour and CA-Cl*pper. */

#include "error.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "setcurs.ch"

#ifndef __HARBOUR__
   #define hb_eol()     ( Chr( 13 ) + Chr( 10 ) )
   #define hb_ntos( n ) LTrim( Str( n ) )
#endif

#xtranslate TEST_LINE( <x> ) => TEST_CALL( o, #<x>, {|| <x> } )

STATIC s_cTest := ""
STATIC s_xVar := NIL
STATIC s_fhnd
STATIC s_lCallBackStack
STATIC s_lRTEDetails
STATIC s_lObjectDump

PROCEDURE Main( cArg01, cArg02, cArg03, cArg04 )
   LOCAL uNIL := NIL
   LOCAL nInt01 := 98
   LOCAL nInt02 := 0
   LOCAL cStr01 := "AbC DF 974"
   LOCAL cStr02E := ""
   LOCAL cStr03 := ""
   LOCAL cStr04 := ""
   LOCAL cStr05 := ""
   LOCAL cStr06 := ""
   LOCAL cStr07 := ""
   LOCAL dDate01

   LOCAL bOldBlock
   LOCAL o

   LOCAL cCommandLine

   LOCAL nOldRow
   LOCAL nOldCol

   IF cArg01 == NIL
      cArg01 := ""
   ENDIF
   IF cArg02 == NIL
      cArg02 := ""
   ENDIF
   IF cArg03 == NIL
      cArg03 := ""
   ENDIF
   IF cArg04 == NIL
      cArg04 := ""
   ENDIF

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )
   SetCursor( SC_NONE )

   //

   cCommandLine := cArg01 + " " + cArg02 + " " + cArg03 + " " + cArg04

   s_lCallBackStack := "CALLBACKSTACK" $ Upper( cCommandLine )
   s_lRTEDetails := "RTEDETAILS" $ Upper( cCommandLine )
   s_lObjectDump := !( "NODUMP" $ Upper( cCommandLine ) )

   //

   #ifdef __HARBOUR__
      s_fhnd := FCreate( "tget_hb.txt" )
   #else
      s_fhnd := FCreate( "tget_cl5.txt" )
   #endif

   IF s_fhnd == F_ERROR
      RETURN
   ENDIF

   FWrite( s_fhnd, Set( _SET_DATEFORMAT ) + hb_eol() )

   // Delimiter handling.

   SetColor( "B/N, RB/N" )

   Set( _SET_DELIMITERS, .T. )

   Set( _SET_DELIMCHARS, "<>" )
   o := GetNew( 14, 16, {| x | iif( x == NIL, cStr01, cStr01 := x ) }, "cStr01",, "W+/N,BG/N" )
   TEST_LINE( o:display() )
   Set( _SET_DELIMCHARS, "()" )
   TEST_LINE( o:display() )
   Set( _SET_DELIMITERS, .F. )
   TEST_LINE( o:display() )
   Set( _SET_DELIMITERS, .T. )
   TEST_LINE( o:display() )
   TEST_LINE( o:display() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:display() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:display() )
   TEST_LINE( SetColor( "G+/N, RB/N" ) )
   TEST_LINE( o:display() )
   Set( _SET_DELIMITERS, .F. )
   Set( _SET_DELIMCHARS, "<>" )
   TEST_LINE( o:display() )
   TEST_LINE( o:Col := 30 )
   TEST_LINE( o:display() )

   Set( _SET_DELIMCHARS, "::" )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" )
   TEST_LINE( o:display() )
   TEST_LINE( o:Col := 20 )

   Set( _SET_DELIMITERS, .F. )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" )
   TEST_LINE( o:display() )

   SetColor( "" )

   // colorDisp / VarPut / display (::nDispLen recalc)

   SetPos( 14, 16 ) ; o := _GET_( cStr03, "cStr03" )
   TEST_LINE( o:colorDisp( "GR/N" ) )
   TEST_LINE( o:VarPut( "<hello>" ) )
   TEST_LINE( o:display() )

   SetPos( 14, 16 ) ; o := _GET_( cStr04, "cStr04" )
   TEST_LINE( o:colorSpec := "GR/N" )
   TEST_LINE( o:VarPut( "<hello>" ) )
   TEST_LINE( o:display() )

   SetPos( 14, 16 ) ; o := _GET_( cStr05, "cStr05",,, )
   TEST_LINE( o:VarPut( Space( 30 ) ) )
   TEST_LINE( o:display() )
   TEST_LINE( o:VarPut( 1 ) )
   TEST_LINE( o:VarGet() )
   TEST_LINE( o:VarPut("abcdefghijklm1234nopqrstuvwxyz") )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:assign() )
   TEST_LINE( o:VarPut("abcdefghijklmnopqrstuvwxyz1234") )
   TEST_LINE( o:updateBuffer() )
   nOldRow := o:row
   nOldCol := o:col
   TEST_LINE( o:row := 50 )
   TEST_LINE( o:col := 80 )
   TEST_LINE( o:VarPut( 2 ) )
   TEST_LINE( o:VarGet() )
   TEST_LINE( o:VarPut("1234abcdefghijklmnopqrstuvwxyz") )
   TEST_LINE( o:updateBuffer() )
   TEST_LINE( o:row := nOldRow )
   TEST_LINE( o:col := nOldCol )
   TEST_LINE( o:killFocus() )
   TEST_LINE( o:VarPut( 4 ) )

   SetPos( 14, 16 ) ; o := _GET_( cStr05, "cStr05",,, )
   TEST_LINE( o:VarPut( Space( 30 ) ) )
   TEST_LINE( o:display() )
   TEST_LINE( o:VarPut( 1 ) )
   TEST_LINE( o:VarGet() )
   TEST_LINE( o:VarPut("abcdefghijklm1234nopqrstuvwxyz") )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:assign() )
   TEST_LINE( o:VarPut("abcdefghijklmnopqrstuvwxyz1234") )
   TEST_LINE( o:updateBuffer() )
   TEST_LINE( o:VarPut( 2 ) )
   TEST_LINE( o:Type )

   //

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut(Replicate( "a", 30 ) ) )
   TEST_LINE( o:display() )
   TEST_LINE( o:VarPut( 1 ) )

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut( Replicate( "a", 30 ) ) )
   TEST_LINE( o:display() )
   TEST_LINE( o:VarPut( Replicate( "b", 20 ) ) )

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut( Replicate( "a", 30 ) ) )
   TEST_LINE( o:display() )
   TEST_LINE( o:VarPut( NIL ) )
   TEST_LINE( o:VarPut( Replicate( "b", 20 ) ) )
   TEST_LINE( o:VarPut( {|| "" } ) )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:VarPut( {|| "" } ) )
   TEST_LINE( o:VarPut( {} ) )
   TEST_LINE( o:VarPut( ErrorNew() ) )

   cStr06 := ""

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut( Replicate( "a", 30 ) ) )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:VarPut( 1 ) )

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut( Replicate( "a", 30 ) ) )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:VarPut( Replicate( "b", 20 ) ) )

   SetPos( 14, 16 ) ; o := _GET_( cStr06, "cStr06",,, )
   TEST_LINE( o:VarPut( Replicate( "a", 30 ) ) )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:VarPut( NIL ) )
   TEST_LINE( o:VarPut( Replicate( "b", 20 ) ) )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:assign() )

   // Minus

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Minus := .T. )
   TEST_LINE( o:Minus := .F. )

   // Picture

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999999999",, )
   TEST_LINE( o:Picture := "99" )
   TEST_LINE( o:Picture := "!!" )
   TEST_LINE( o:Picture := NIL )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Picture := "99" )
   TEST_LINE( o:Picture := "!!" )
   TEST_LINE( o:Picture := NIL )

   // Picture "Y"

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "Y",, )
   TEST_LINE( o:display() )
   TEST_LINE( o:setFocus() )
   TGetTOVS( o, { "NnYyAa" } )

   // Assign

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999999999",, )
   o:SetFocus()
   TEST_LINE( o:OverStrike( "z" ) )
   TEST_LINE( o:Assign() )

   // Edmer #1

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@R   999 9999 999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@R  999  9999 999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@R  999 9999  999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   //

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@ER   999 9999 999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@ER  999  9999 999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   cStr07 := Space(10)
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "@ER  999 9999  999999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1231234123456" } )
   TEST_LINE( o:Assign() )

   // Edmer #2

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "9,999,999.99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "12345" } )
   TEST_LINE( o:Assign() )

   // Lorenzo/Przemek #1

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@E 99.99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1234" } )
   TEST_LINE( o:Assign() )

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@E 99.99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1" } )
   TEST_LINE( o:Assign() )

   //

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@E 9,999,999.9999",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "12345" } )
   TEST_LINE( o:Assign() )

   // EMG

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@EZ 999,999.99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "1", K_RIGHT } )
   TEST_LINE( o:Assign() )

   //

   cStr07 := "12:34:56"
   SetPos( 14, 16 ) ; o := _GET_( cStr07, "cStr07", "99:99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "78" } )
   TEST_LINE( o:Assign() )

   //

   nInt02 := 1234.56
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@Z 9999999.9999",, )
   o:display()
   o:setFocus()
   TEST_LINE( o:OverStrike( "0" ) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:reset() )
   TEST_LINE( o:OverStrike( "1" ) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:reset() )
   TEST_LINE( o:killFocus() )

   // Quique

   nInt02 := 198.12
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02",,, )
   TEST_LINE( o:display() )
   TEST_LINE( o:killFocus() )
   TEST_LINE( o:picture := NIL )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:display() )

   nInt02 := 198.12
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "999.999",, )
   TEST_LINE( o:display() )
   TEST_LINE( o:killFocus() )
   TEST_LINE( o:picture := NIL )
   TEST_LINE( o:setFocus() )
   TEST_LINE( o:display() )

   // Mauricio and variations

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", ".99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "12" } )
   TEST_LINE( o:Assign() )

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "-.99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "12" } )
   TEST_LINE( o:Assign() )

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", ".-99",, )
   o:display()
   o:setFocus()
   TGetTOVS( o, { "12" } )
   TEST_LINE( o:Assign() )

   // Heinz V Bergen

   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@R 999-",, )
   o:display()
   o:setFocus()
   TEST_LINE( o:OverStrike( "." ) )
   TEST_LINE( o:OverStrike( "," ) )
   TEST_LINE( o:OverStrike( "0" ) )

   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "@R 999-",, )
   o:display()
   o:setFocus()
   TEST_LINE( o:Insert( "." ) )
   TEST_LINE( o:Insert( "," ) )
   TEST_LINE( o:Insert( "0" ) )

   // Overstrike/Insert

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "9999999999",, )
   o:display()
   o:setFocus()
   TEST_LINE( o:OverStrike( "12" ) )
   TEST_LINE( o:OverStrike( "9" ) )
   TEST_LINE( o:OverStrike( "13" ) )
   TEST_LINE( o:OverStrike( "9" ) )
   TEST_LINE( o:OverStrike( NIL ) )
   TEST_LINE( o:OverStrike( "9" ) )
   TEST_LINE( o:OverStrike( 1 ) )
   TEST_LINE( o:OverStrike( "9" ) )
   TEST_LINE( o:OverStrike( "" ) )
   TEST_LINE( o:Assign() )

   nInt02 := 0
   SetPos( 14, 16 ) ; o := _GET_( nInt02, "nInt02", "9999999999",, )
   o:display()
   o:setFocus()
   TEST_LINE( o:Insert( "12" ) )
   TEST_LINE( o:Insert( "9" ) )
   TEST_LINE( o:Insert( "13" ) )
   TEST_LINE( o:Insert( "9" ) )
   TEST_LINE( o:Insert( NIL ) )
   TEST_LINE( o:Insert( "9" ) )
   TEST_LINE( o:Insert( 1 ) )
   TEST_LINE( o:Insert( "9" ) )
   TEST_LINE( o:Insert( "" ) )
   TEST_LINE( o:Assign() )

   // Buffer

   s_xVar := "abcdefg"
   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar",,, )
   TEST_LINE( o:buffer := "1234567" )
   TEST_LINE( o:buffer := "abcdefg" )

   s_xVar := "abcdefg"
   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar",,, )
   o:SetFocus()
   TEST_LINE( o:buffer := "1234567" )
   TEST_LINE( o:buffer := "abcdefg" )

   // Clear

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Clear := .T. )
   TEST_LINE( o:Clear := .F. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Clear := .T. )
   TEST_LINE( o:Clear := .F. )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Clear := .T. )
   TEST_LINE( o:Clear := .F. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Clear := .F. )
   TEST_LINE( o:Clear := .T. )

   // Minus

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Minus := .T. )
   TEST_LINE( o:Minus := .F. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Minus := .T. )
   TEST_LINE( o:Minus := .F. )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Minus := .F. )
   TEST_LINE( o:Minus := .T. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Minus := .F. )
   TEST_LINE( o:Minus := .T. )

   // Changed

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Changed := .T. )
   TEST_LINE( o:Changed := .F. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Changed := .T. )
   TEST_LINE( o:Changed := .F. )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   TEST_LINE( o:Changed := .F. )
   TEST_LINE( o:Changed := .T. )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Changed := .F. )
   TEST_LINE( o:Changed := .T. )

   // ColorSpec

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01",,, )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := NIL )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .F. )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .T. )
   SetColor( "W+/R,G+/BR,RG+/B,BG+/G,N/GR,GR+/BG,B/GR*" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .F. )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .T. )
   SetColor( "" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := 100 )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := {} )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := ",N/G" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "," )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G,N/N" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G,N /N" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G,N/ N" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G, N/N" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G, N/N " )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "N/G,hkjhkj" )
   o:ColorSpec := "BG/RB,RG+/B" ; TEST_LINE( o:ColorSpec := "n/g,n/bg" )

   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := NIL )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .F. )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .T. )
   SetColor( "W+/R,G+/BR,RG+/B,BG+/G,N/GR,GR+/BG,B/GR*" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .F. )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := .T. )
   Set( _SET_INTENSITY, .T. )
   SetColor( "" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := 100 )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := {} )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := ",N/G" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "," )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G,N/N" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G,N /N" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G,N/ N" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G, N/N" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G, N/N " )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "N/G,hkjhkj" )
   o:ColorSpec := "BG/RB,RG+/B,N/GR,W+/R" ; TEST_LINE( o:ColorSpec := "n/g,n/bg" )

   // Pos

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.99",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:ToDecPos() )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999.",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:ToDecPos() )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:ToDecPos() )
   TEST_LINE( o:Pos := 0 )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 10 )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01", "9999",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 0 )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999--9999",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:Pos := 6 )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999------",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:Pos := 6 )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "----------",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 5 )
   TEST_LINE( o:Pos := 6 )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999999999",, )
   o:SetFocus()
   TEST_LINE( o:Pos := 11 )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01", "9999999999",, )
   o:SetFocus()
// TEST_LINE( o:Pos := -2 )

   SetPos( 14, 16 ) ; o := _GET_( cStr02E, "cStr02E",,, )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Pos := 1 )

   // Error conditions

   TGetAssign( NIL )
// TGetAssign( -1 ) // CA-Cl*pper has too many differences due to the low level implementation here
   TGetAssign( 0 )
   TGetAssign( 1 )
   TGetAssign( 3 )
   TGetAssign( 3.3 )
   TGetAssign( 3.7 )
   TGetAssign( 100 )
   TGetAssign( "" )
   TGetAssign( "az" )
   TGetAssign( hb_SToD( "20070425" ) )
   TGetAssign( .F. )
   TGetAssign( .T. )
   TGetAssign( {|| NIL } )
   TGetAssign( {} )
   TGetAssign( { "" } )

   // Type change N -> C

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, cStr01, cStr01 := h ) } )
   TEST_LINE( o:SetFocus() )

   // Reform

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:picture := "!!!!!!!!" )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:picture := "!!!!AAAA" )
   TEST_LINE( o:Reform() )

   // Minus

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" )
   TEST_LINE( OBJ_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:insert("-") )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:SetFocus() )
   o:minus := .T.
   TEST_LINE( o:SetFocus() )

   //

   SET CENTURY ON

   SetPos( 14, 16 ) ; dDate01 := hb_SToD( "20070425" )
   o := _GET_( dDate01, "dDate01" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; dDate01 := hb_SToD( "20070425" )
   o := _GET_( dDate01, "dDate01", "@E" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SET CENTURY OFF

   SetPos( 14, 16 ) ; dDate01 := hb_SToD( "20070425" )
   o := _GET_( dDate01, "dDate01" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; dDate01 := hb_SToD( "20070425" )
   o := _GET_( dDate01, "dDate01", "@E" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; cStr01 := "hello world"
   o := _GET_( cStr01, "cStr01", "!!LY!!!!!!" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; cStr01 := "hello world"
   o := _GET_( cStr01, "cStr01", "!!!.!!!!!!" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; cStr01 := "hello world"
   o := _GET_( cStr01, "cStr01", "@R !!LY!!!!!!" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   SetPos( 14, 16 ) ; cStr01 := "hello world"
   o := _GET_( cStr01, "cStr01", "@R !!!.!!!!!!" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:SetFocus() )
   TGetTOVS( o, { "12345678" } )
   TEST_LINE( o:KillFocus() )

   // Exercises

   TGetTest( 98, NIL )
   TGetTest( 98, "99999" )
   TGetTest( 98, "99999." )
   TGetTest( 98, "99999.99" )
   TGetTest( -98, NIL )
   TGetTest( -98, "99999" )
   TGetTest( -98, "99999." )
   TGetTest( -98, "99999.99" )
   TGetTest( "hello world", NIL )
   TGetTest( "hello world", "@!" )
   TGetTest( "hello world", "!!!" )
   TGetTest( "hello world", "@S5" )
   TGetTest( .T., NIL )
   TGetTest( .T., "Y" )
   SET CENTURY ON
   TGetTest( hb_SToD( "20070425" ), NIL )
   SET CENTURY OFF
   TGetTest( hb_SToD( "20070425" ), NIL )
   TGetTest( NIL, NIL )
   TGetTest( NIL, "!!!!" )
   TGetTest( {|| "" }, NIL )

   FClose( s_fhnd )

   RETURN

STATIC PROCEDURE TGetTOVS( o, aKeys, lInsert )
   LOCAL tmp, tmp1

   IF !( ValType( lInsert ) == "L" )
      lInsert := .F.
   ENDIF

   FOR tmp := 1 TO Len( aKeys )
      IF ValType( aKeys[ tmp ] ) == "C"
         FOR tmp1 := 1 TO Len( aKeys[ tmp ] )
            IF lInsert
               TEST_CALL( o, "o:insert( '" + SubStr( aKeys[ tmp ], tmp1, 1 ) + "' )", {|| o:insert( SubStr( aKeys[ tmp ], tmp1, 1 ) ) } )
            ELSE
               TEST_CALL( o, "o:overStrike( '" + SubStr( aKeys[ tmp ], tmp1, 1 ) + "' )", {|| o:overStrike( SubStr( aKeys[ tmp ], tmp1, 1 ) ) } )
            ENDIF
         NEXT
      ELSEIF ValType( aKeys[ tmp ] ) == "N"
         DO CASE
         CASE aKeys[ tmp ] == K_INS                          ; lInsert := ! lInsert
         CASE aKeys[ tmp ] == K_HOME                         ; TEST_LINE( o:Home() )
         CASE aKeys[ tmp ] == K_END                          ; TEST_LINE( o:End() )
         CASE aKeys[ tmp ] == K_RIGHT                        ; TEST_LINE( o:Right() )
         CASE aKeys[ tmp ] == K_LEFT                         ; TEST_LINE( o:Left() )
         CASE aKeys[ tmp ] == K_CTRL_RIGHT                   ; TEST_LINE( o:WordRight() )
         CASE aKeys[ tmp ] == K_CTRL_LEFT                    ; TEST_LINE( o:WordLeft() )
         CASE aKeys[ tmp ] == K_BS                           ; TEST_LINE( o:BackSpace() )
         CASE aKeys[ tmp ] == K_DEL                          ; TEST_LINE( o:Delete() )
         CASE aKeys[ tmp ] == K_CTRL_T                       ; TEST_LINE( o:DelWordRight() )
         CASE aKeys[ tmp ] == K_CTRL_Y                       ; TEST_LINE( o:DelEnd() )
         CASE aKeys[ tmp ] == K_CTRL_BS                      ; TEST_LINE( o:DelWordLeft() )
         CASE aKeys[ tmp ] == K_CTRL_U                       ; TEST_LINE( o:Undo() )
         CASE o:type == "N" .AND. Chr( aKeys[ tmp ] ) $ ".," ; TEST_LINE( o:ToDecPos() )
         ENDCASE
      ENDIF
   NEXT

   RETURN

#ifdef _COMMENT_
STATIC FUNCTION TGetTIns( o, aKeys )
   RETURN TGetTOVS( o, aKeys, .T. )
#endif

STATIC PROCEDURE TGetAssign( xVar )
   LOCAL o
   LOCAL nInt01 := 76
   LOCAL cStr01 := "AbC DeF 974"
   LOCAL dDat01 := hb_SToD( "20070425" )
   LOCAL lLog01 := .F.
   LOCAL bBlo01 := {|| NIL }

   s_xVar := xVar

   s_cTest := "Non-Focus Assign To N: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "Non-Focus Assign To C: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "Non-Focus Assign To D: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "Non-Focus Assign To L: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "Non-Focus Assign To B: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus Assign to N: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ):SetFocus ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus Assign to C: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ):SetFocus ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus Assign to D: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ):SetFocus ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus Assign to L: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ):SetFocus ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus Assign to B: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:BadDate   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Block     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Buffer    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Cargo     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Changed   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Clear     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Col       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:ColorSpec := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:DecPos    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:ExitState := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:HasFocus  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Minus     := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Name      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Original  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Picture   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Pos       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:PostBlock := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:PreBlock  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Reader    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Rejected  := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Row       := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:SubScript := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:Type      := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:TypeOut   := xVar )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:control   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:message   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:caption   := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:capRow    := xVar )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ):SetFocus ; TEST_LINE( o:capCol    := xVar )
#endif

   s_cTest := "InFocus/SetFocus " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:SetFocus )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:SetFocus )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:SetFocus )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:SetFocus )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:SetFocus )

   /* ----------------------------------------------- */

   s_cTest := "Non-Focus Assign as function call To N: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:capCol   ( xVar ) )
#endif

   s_cTest := "Non-Focus Assign as function call To C: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "cStr01" ) ; TEST_LINE( o:TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( cStr01, "nStr01" ) ; TEST_LINE( o:capCol   ( xVar ) )
#endif

/* NOTE: Clipper will say 'memory overbooked error' */
#ifdef _COMMENT_

   s_cTest := "Non-Focus Assign as function call To D: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( dDat01, "dDat01" ) ; TEST_LINE( o:capCol   ( xVar ) )
#endif

#endif

   s_cTest := "Non-Focus Assign as function call To L: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( lLog01, "lLog01" ) ; TEST_LINE( o:capCol   ( xVar ) )
#endif

   s_cTest := "Non-Focus Assign as function call To B: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( bBlo01, "bBlo01" ) ; TEST_LINE( o:capCol   ( xVar ) )
#endif

   s_cTest := "Non-Focus Assign as _function call To N: " + XToStr( xVar )

   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_BadDate  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Block    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Buffer   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Cargo    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Changed  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Clear    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Col      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_ColorSpec( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_DecPos   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_ExitState( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_HasFocus ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Minus    ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Name     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Original ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Picture  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Pos      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_PostBlock( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_PreBlock ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Reader   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Rejected ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Row      ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_SubScript( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_Type     ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_TypeOut  ( xVar ) )
#ifdef HB_COMPAT_C53
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_control  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_message  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_caption  ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_capRow   ( xVar ) )
   SetPos( 14, 16 ) ; o := _GET_( nInt01, "nInt01" ) ; TEST_LINE( o:_capCol   ( xVar ) )
#endif

   RETURN

STATIC PROCEDURE TGetTest( xVar, cPic )
   LOCAL bOldBlock
   LOCAL o

   s_xVar := xVar

   // Display

   s_cTest := "Display Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar" )
   TEST_LINE( OBJ_CREATE() )
   TEST_LINE( o:Display() )

   // In focus

   s_cTest := "InFocus Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar" )
   TEST_LINE( OBJ_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   IF cPic != NIL
      TEST_LINE( o:picture := "99999" )
      TEST_LINE( o:picture := cPic )
      TEST_LINE( o:picture := NIL )
   ENDIF
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:Display() )
   TEST_LINE( o:KillFocus() )

   // Not in focus

   s_cTest := "NotFocus Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar" )
   TEST_LINE( OBJ_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   IF cPic != NIL
      TEST_LINE( o:picture := "99999" )
      TEST_LINE( o:picture := cPic )
      TEST_LINE( o:picture := NIL )
   ENDIF
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:Display() )
   TEST_LINE( o:KillFocus() )

   // In Focus editing

   s_cTest := "InFocus #2 Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar" )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "6" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:Insert( "5" ) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:Reset() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:VarPut( "newvalue " ) )
   TEST_LINE( o:Insert( "7" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "3" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:VarPut( 0 ) )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "3" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:KillFocus() )

   //

   s_xVar := xVar

   SetPos( 14, 16 ) ; o := _GET_( s_xVar, "s_xVar" )
   TEST_LINE( o:picture := cPic )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( PCount(), h ), iif( PCount() == 0, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "6" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:Insert( "5" ) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:Reset() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:VarPut( "newvalue " ) )
   TEST_LINE( o:Insert( "7" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:Assign() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "3" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:VarPut( 0 ) )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:Insert( "3" ) )
   TEST_LINE( o:Undo(.T.) )
   TEST_LINE( o:KillFocus() )

   //

   s_cTest := ""

   RETURN

STATIC PROCEDURE TEST_CALL( o, cBlock, bBlock )
   LOCAL xResult
   LOCAL bOldError
   LOCAL oError

   SetPos( 0, 0 ) // To check where the cursor was moved after evaluating the block.

   bOldError := ErrorBlock( {| oError | oError:cargo := CallStack(), Break( oError ) } )

   BEGIN SEQUENCE
      xResult := Eval( bBlock )
   RECOVER USING oError
      xResult := ErrorMessage( oError )
   END SEQUENCE

   ErrorBlock( bOldError )

   LogGETVars( o, cBlock, xResult )

   RETURN

STATIC FUNCTION CallStack()
   LOCAL tmp := 1
   LOCAL cString := ""

   DO WHILE ! Empty( ProcName( tmp ) )
      cString += ProcName( tmp ) + " (" + hb_ntos( ProcLine( tmp ) ) + ") "
      tmp++
   ENDDO

   RETURN RTrim( cString )

STATIC PROCEDURE LogMe( nPCount, data, desc )
   LOCAL nLevel
   LOCAL cStack

   cStack := ""
   FOR nLevel := 2 TO 5
      IF Empty( ProcName( nLevel ) )
         EXIT
      ENDIF
      cStack += ProcName( nLevel ) + " (" + hb_ntos( ProcLine( nLevel ) ) + ") "
   NEXT

   IF desc == NIL
        desc := ""
   ENDIF
   desc := s_cTest + " " + desc

   IF ! s_lCallBackStack
      cStack := ""
   ENDIF

   IF nPCount == 0
      FWrite( s_fhnd, cStack + "BLOCK_GET  " + desc + hb_eol() )
   ELSE
      FWrite( s_fhnd, cStack + "BLOCK_SET  " + XToStr( data ) + "  " + desc + hb_eol() )
   ENDIF

   RETURN

STATIC PROCEDURE LogGETVars( o, desc, xResult )
   LOCAL nLevel
   LOCAL cStack

   LOCAL tmp

   cStack := ""
   FOR nLevel := 2 TO 2
      IF Empty( ProcName( nLevel ) )
         EXIT
      ENDIF
      cStack += ProcName( nLevel ) + " (" + hb_ntos( ProcLine( nLevel ) ) + ") "
   NEXT

   IF desc == NIL
        desc := ""
   ENDIF
   desc := s_cTest + " " + XToStr( desc )

   FWrite( s_fhnd, cStack + "  " + desc + hb_eol() )
   FWrite( s_fhnd, "---------------------" + hb_eol() )
   FWrite( s_fhnd, "   s_xVar        " + XToStr( s_xVar      ) + hb_eol() )
   FWrite( s_fhnd, "   xResult       " + XToStr( xResult     ) + hb_eol() )
   FWrite( s_fhnd, "   Row()         " + XToStr( Row()       ) + hb_eol() )
   FWrite( s_fhnd, "   Col()         " + XToStr( Col()       ) + hb_eol() )
   FWrite( s_fhnd, "   UnTransform() " + XToStr( o:UnTransform() ) + hb_eol() )
   FWrite( s_fhnd, "   BadDate       " + XToStr( o:BadDate   ) + hb_eol() )
   FWrite( s_fhnd, "   Block         " + XToStr( o:Block     ) + hb_eol() )
   FWrite( s_fhnd, "   Buffer        " + XToStr( o:Buffer    ) + hb_eol() )
   FWrite( s_fhnd, "   Cargo         " + XToStr( o:Cargo     ) + hb_eol() )
   FWrite( s_fhnd, "   Changed       " + XToStr( o:Changed   ) + hb_eol() )
   FWrite( s_fhnd, "   Clear         " + XToStr( o:Clear     ) + hb_eol() )
   FWrite( s_fhnd, "   Col           " + XToStr( o:Col       ) + hb_eol() )
   FWrite( s_fhnd, "   ColorSpec     " + XToStr( o:ColorSpec ) + hb_eol() )
   FWrite( s_fhnd, "   DecPos        " + XToStr( o:DecPos    ) + hb_eol() )
   FWrite( s_fhnd, "   ExitState     " + XToStr( o:ExitState ) + hb_eol() )
   FWrite( s_fhnd, "   HasFocus      " + XToStr( o:HasFocus  ) + hb_eol() )
   FWrite( s_fhnd, "   Minus         " + XToStr( o:Minus     ) + hb_eol() )
   FWrite( s_fhnd, "   Name          " + XToStr( o:Name      ) + hb_eol() )
   FWrite( s_fhnd, "   Original      " + XToStr( o:Original  ) + hb_eol() )
   FWrite( s_fhnd, "   Picture       " + XToStr( o:Picture   ) + hb_eol() )
   FWrite( s_fhnd, "   Pos           " + XToStr( o:Pos       ) + hb_eol() )
   FWrite( s_fhnd, "   PostBlock     " + XToStr( o:PostBlock ) + hb_eol() )
   FWrite( s_fhnd, "   PreBlock      " + XToStr( o:PreBlock  ) + hb_eol() )
   FWrite( s_fhnd, "   Reader        " + XToStr( o:Reader    ) + hb_eol() )
   FWrite( s_fhnd, "   Rejected      " + XToStr( o:Rejected  ) + hb_eol() )
   FWrite( s_fhnd, "   Row           " + XToStr( o:Row       ) + hb_eol() )
   FWrite( s_fhnd, "   SubScript     " + XToStr( o:SubScript ) + hb_eol() )
   FWrite( s_fhnd, "   Type          " + XToStr( o:Type      ) + hb_eol() )
   FWrite( s_fhnd, "   TypeOut       " + XToStr( o:TypeOut   ) + hb_eol() )
   IF s_lObjectDump
#ifdef __HARBOUR__
#ifdef HB_COMPAT_C53
      FOR tmp := 1 TO iif( o:hasFocus, 19, 16 )
#else
      FOR tmp := 1 TO iif( o:hasFocus, 13, 10 )
#endif
#else
      FOR tmp := 1 TO Len( o )
#endif
         /* Both indexes contain binary trash
            (except the first char of [11] which is type. [vszakats] */
#ifdef HB_COMPAT_C53
         IF tmp != 8 .AND. tmp != 17
#else
         IF tmp != 8 .AND. tmp != 11
#endif
            FWrite( s_fhnd, "   [ " + Str( tmp, 3 ) + " ]       " + XToStrX( o[ tmp ] ) + hb_eol() )
         ENDIF
      NEXT
   ENDIF
   FWrite( s_fhnd, "---------------------" + hb_eol() )

   RETURN

STATIC FUNCTION XToStr( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN '"' + xValue + '"'

   CASE cType == "N" ; RETURN hb_ntos( xValue )
   CASE cType == "D" ; RETURN 'hb_SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...} -> " + XToStr( Eval( xValue ) )
   CASE cType == "A" ; RETURN "{ " + ArrayToList( xValue ) + " }"
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
   ENDCASE

   RETURN ""

STATIC FUNCTION ArrayToList( a )
   LOCAL tmp
   LOCAL cString := ""

   FOR tmp := 1 TO Len( a )
      cString += XToStr( a[ tmp ] )
      IF tmp < Len( a )
         cString += ", "
      ENDIF
   NEXT

   RETURN cString

STATIC FUNCTION XToStrE( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE cType == "N" ; RETURN hb_ntos( xValue )
   CASE cType == "D" ; RETURN DToS( xValue )
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...} -> " + XToStr( Eval( xValue ) )
   CASE cType == "A" ; RETURN "{ " + ArrayToList( xValue ) + " }"
   CASE cType == "M" ; RETURN "M:" + xValue
   ENDCASE

   RETURN ""

STATIC FUNCTION XToStrX( xValue )
   LOCAL cType := ValType( xValue )

   LOCAL tmp
   LOCAL cRetVal

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN xValue

   CASE cType == "N" ; RETURN hb_ntos( xValue )
   CASE cType == "D" ; RETURN DToS( xValue )
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN "{||...} -> " + XToStrX( Eval( xValue ) )
   CASE cType == "A"

      cRetVal := "{ "

      FOR tmp := 1 TO Len( xValue )
         cRetVal += XToStrX( xValue[ tmp ] )
         IF tmp < Len( xValue )
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + " }"

   CASE cType == "M" ; RETURN "M:" + xValue
   ENDCASE

   RETURN ""

STATIC FUNCTION ErrorMessage( oError )
   LOCAL cMessage
   LOCAL tmp

   IF s_lRTEDetails

      cMessage := ""

      IF ValType( oError:severity ) == "N"
         DO CASE
         CASE oError:severity == ES_WHOCARES     ; cMessage += "M "
         CASE oError:severity == ES_WARNING      ; cMessage += "W "
         CASE oError:severity == ES_ERROR        ; cMessage += "E "
         CASE oError:severity == ES_CATASTROPHIC ; cMessage += "C "
         ENDCASE
      ENDIF
      IF ValType( oError:subsystem ) == "C"
         cMessage += oError:subsystem + " "
      ENDIF
      IF ValType( oError:subCode ) == "N"
         cMessage += hb_ntos( oError:subCode ) + " "
      ENDIF
      IF ValType( oError:description ) == "C"
         cMessage += oError:description + " "
      ENDIF
      IF ! Empty( oError:operation )
         cMessage += oError:operation + " "
      ENDIF
      IF ! Empty( oError:filename )
         cMessage += oError:filename + " "
      ENDIF

      IF ValType( oError:Args ) == "A"
         cMessage += "A:" + hb_ntos( Len( oError:Args ) ) + ":"
         FOR tmp := 1 TO Len( oError:Args )
            cMessage += ValType( oError:Args[ tmp ] ) + ":" + XToStrE( oError:Args[ tmp ] )
            IF tmp < Len( oError:Args )
               cMessage += ";"
            ENDIF
         NEXT
         cMessage += " "
      ENDIF

      IF oError:canDefault .OR. ;
         oError:canRetry .OR. ;
         oError:canSubstitute

         cMessage += "F:"
         IF oError:canDefault
            cMessage += "D"
         ENDIF
         IF oError:canRetry
            cMessage += "R"
         ENDIF
         IF oError:canSubstitute
            cMessage += "S"
         ENDIF
      ENDIF

      IF ! Empty( oError:cargo )
         cMessage += " " + oError:cargo
      ENDIF
   ELSE
      cMessage := "(ERROR)"
   ENDIF

   RETURN cMessage

#ifdef __XPP__
STATIC FUNCTION hb_SToD( cDate )
   RETURN SToD( cDate )
#endif

#ifndef HAVE_HBCLIP
#ifndef __HARBOUR__
#ifndef __XPP__

STATIC FUNCTION hb_SToD( s )

   LOCAL cDf := Set( _SET_DATEFORMAT, "yyyy-mm-dd" ), dt

   dt := CToD( Stuff( Stuff( s, 7, 0, "-" ), 5, 0, "-" ) )
   Set( _SET_DATEFORMAT, cDf )

   RETURN dt

#endif
#endif
#endif

STATIC PROCEDURE OBJ_CREATE()

   // Dummy

   RETURN

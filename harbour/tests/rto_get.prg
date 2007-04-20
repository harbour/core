/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for class Get
 *
 * Copyright 1999-2007 Viktor Szakats <viktor.szakats@syenar.hu>
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

/* NOTE: This source can be compiled with both Harbour and CA-Cl*pper. */

#include "fileio.ch"

#ifndef __HARBOUR__
   #define HB_OSNewLine() ( Chr( 13 ) + Chr( 10 ) )
#endif

#translate TEST_LINE( <x> ) => TEST_CALL( o, #<x>, {|| <x> } )

STATIC s_cTest := ""
STATIC s_xVar := NIL
STATIC s_fhnd

FUNCTION Main()
   LOCAL nInt01 := 98
   LOCAL nStr01 := "AbC DeF 974"

   LOCAL bOldBlock
   LOCAL o

   #ifdef __HARBOUR__
      s_fhnd := FCreate( "tget_hb.txt", FC_NORMAL )
   #else
      s_fhnd := FCreate( "tget_cl5.txt", FC_NORMAL )
   #endif

   IF s_fhnd == F_ERROR
      RETURN 1
   ENDIF

   // ; Type change N -> C

   SetPos( 14, 14 )
   o := _GET_( nInt01, "nInt01",,, )
   TEST_LINE( GET_CREATE() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:block := {| h | LogMe( h ), iif( h == NIL, nStr01, nStr01 := h ) } )
   TEST_LINE( o:SetFocus() )

   // ; Reform

   SetPos( 14, 14 )
   o := _GET_( nStr01, "nStr01",,, )
   TEST_LINE( GET_CREATE() )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:picture := "!!!!!!!!" )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:picture := "!!!!AAAA" )
   TEST_LINE( o:Reform() )

   // ; Minus

   SetPos( 14, 14 )
   o := _GET_( nInt01, "nInt01",,, )
   TEST_LINE( GET_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( h ), iif( h == NIL, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   TEST_LINE( o:insert("-") )
   TEST_LINE( o:KillFocus() )
   TEST_LINE( o:SetFocus() )
   o:minus := .T.
   TEST_LINE( o:SetFocus() )

   // ; Exercises

   TGetTest( 98, NIL )
   TGetTest( 98, "99999.99" )
   TGetTest( -98, NIL )
   TGetTest( -98, "99999.99" )
   TGetTest( "hello world", NIL )
   TGetTest( "hello world", "@!" )
   TGetTest( "hello world", "!!!" )
   TGetTest( "hello world", "@S5" )

   FClose( s_fhnd )

   RETURN 0

PROCEDURE TGetTest( xVar, cPic )
   LOCAL bOldBlock
   LOCAL o

   s_xVar := xVar

   // ; In focus

   s_cTest := "InFocus Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 14 )
   o := _GET_( s_xVar, "s_xVar",,, )
   TEST_LINE( GET_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( h ), iif( h == NIL, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   TEST_LINE( o:SetFocus() )
   IF cPic != NIL
      TEST_LINE( o:picture := cPic )
      TEST_LINE( o:picture := NIL )
   ENDIF
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:Display() )
   TEST_LINE( o:KillFocus() )

   // ; Not in focus

   s_cTest := "NotFocus Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 14 )
   o := _GET_( s_xVar, "s_xVar",,, )
   TEST_LINE( GET_CREATE() )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( h ), iif( h == NIL, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
   IF cPic != NIL
      TEST_LINE( o:picture := cPic )
      TEST_LINE( o:picture := NIL )
   ENDIF
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:UpdateBuffer() )
   TEST_LINE( o:Reform() )
   TEST_LINE( o:Display() )
   TEST_LINE( o:KillFocus() )

   // ; In Focus editing

   s_cTest := "InFocus #2 Var: " + ValType( xVar ) + " Pic: " + iif( cPic == NIL, "(none)", cPic )

   SetPos( 14, 14 )
   o := _GET_( s_xVar, "s_xVar",,, )
   bOldBlock := o:block
   TEST_LINE( o:block := {| h | LogMe( h ), iif( h == NIL, Eval( bOldBlock ), Eval( bOldBlock, h ) ) } )
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

   // ;

   s_cTest := ""

   RETURN

PROCEDURE TEST_CALL( o, cBlock, bBlock )
   LOCAL xRetVal := Eval( bBlock )

   LogGETVars( o, cBlock, xRetVal )

   RETURN

PROCEDURE LogMe( data, desc )
   LOCAL nLevel
   LOCAL cStack

   cStack := ""
   FOR nLevel := 2 TO 2
      IF Empty( ProcName( nLevel ) )
         EXIT
      ENDIF
      cStack += ProcName( nLevel ) + " (" + LTrim( Str( ProcLine( nLevel ) ) ) + ") "
   NEXT

   IF desc == NIL
        desc := ""
   ENDIF
   desc := s_cTest + " " + desc

   cStack := ""

   IF PCount() > 2
      FWrite( s_fhnd, cStack + "BLOCK_SET  " + iif( data == NIL, "NIL", data ) + "  " + desc + HB_OSNewLine() )
   ELSE
      FWrite( s_fhnd, cStack + "BLOCK_GET  " + desc + HB_OSNewLine() )
   ENDIF

   RETURN

PROCEDURE LogGETVars( o, desc, xRetVal )
   LOCAL nLevel
   LOCAL cStack

   cStack := ""
   FOR nLevel := 2 TO 2
      IF Empty( ProcName( nLevel ) )
         EXIT
      ENDIF
      cStack += ProcName( nLevel ) + " (" + LTrim( Str( ProcLine( nLevel ) ) ) + ") "
   NEXT

   IF desc == NIL
        desc := ""
   ENDIF
   desc := s_cTest + " " + XToStr( desc )

   FWrite( s_fhnd, cStack + "  " + desc + HB_OSNewLine() )
   FWrite( s_fhnd, "---------------------" + HB_OSNewLine() )
   FWrite( s_fhnd, "   s_xVar        " + XToStr( s_xVar      ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   xRetVal       " + XToStr( xRetVal     ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Row()         " + XToStr( Row()       ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Col()         " + XToStr( Col()       ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   BadDate       " + XToStr( o:BadDate   ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Block         " + XToStr( o:Block     ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Buffer        " + XToStr( o:Buffer    ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Cargo         " + XToStr( o:Cargo     ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Changed       " + XToStr( o:Changed   ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Clear         " + XToStr( o:Clear     ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Col           " + XToStr( o:Col       ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   ColorSpec     " + XToStr( o:ColorSpec ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   DecPos        " + XToStr( o:DecPos    ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   ExitState     " + XToStr( o:ExitState ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   HasFocus      " + XToStr( o:HasFocus  ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Minus         " + XToStr( o:Minus     ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Name          " + XToStr( o:Name      ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Original      " + XToStr( o:Original  ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Picture       " + XToStr( o:Picture   ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Pos           " + XToStr( o:Pos       ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   PostBlock     " + XToStr( o:PostBlock ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   PreBlock      " + XToStr( o:PreBlock  ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Reader        " + XToStr( o:Reader    ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Rejected      " + XToStr( o:Rejected  ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Row           " + XToStr( o:Row       ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   SubScript     " + XToStr( o:SubScript ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   Type          " + XToStr( o:Type      ) + HB_OSNewLine() )
   FWrite( s_fhnd, "   TypeOut       " + XToStr( o:TypeOut   ) + HB_OSNewLine() )
   FWrite( s_fhnd, "---------------------" + HB_OSNewLine() )

   RETURN

FUNCTION XToStr( xValue )
   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr(0), '"+Chr(0)+"' )
      xValue := StrTran( xValue, Chr(9), '"+Chr(9)+"' )
      xValue := StrTran( xValue, Chr(10), '"+Chr(10)+"' )
      xValue := StrTran( xValue, Chr(13), '"+Chr(13)+"' )
      xValue := StrTran( xValue, Chr(26), '"+Chr(26)+"' )

      RETURN '"' + xValue + '"'

   CASE cType == "N" ; RETURN LTrim( Str( xValue ) )
   CASE cType == "D" ; RETURN 'HB_SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN '{||...}'
   CASE cType == "A" ; RETURN '{.[' + LTrim( Str( Len( xValue ) ) ) + '].}'
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
   ENDCASE

   RETURN ""

PROCEDURE GET_CREATE()

   // ; Dummy

   RETURN

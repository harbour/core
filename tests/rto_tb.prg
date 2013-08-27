/*
 * Harbour Project source code:
 * Regression tests for classes TBrowse/TBColumn
 *
 * Copyright 1999-2007 Viktor Szakats (harbour syenar.net)
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    eInstVar() (from RTL)
 *
 * See COPYING.txt for licensing terms.
 *
 */

/* NOTE: This source can be compiled with both Harbour and CA-Cl*pper. */

#include "error.ch"
#include "fileio.ch"

#ifndef __HARBOUR__
   #define hb_eol()     ( Chr( 13 ) + Chr( 10 ) )
   #define hb_ntos( n ) LTrim( Str( n ) )
#endif

#ifdef __XHARBOUR__
   #ifndef HB_COMPAT_C53
      /* It makes xhb crash. */
      /* #define HB_COMPAT_C53 */
   #endif
#endif

#translate TEST_L_TBR( <x> ) => TEST_C_TBR( o, #<x>, {|| <x> } )
#translate TEST_L_TBC( <x> ) => TEST_C_TBC( o, #<x>, {|| <x> } )

STATIC s_cTest := ""
STATIC s_xVar := NIL
STATIC s_fhnd
STATIC s_lCallBackStack
STATIC s_lRTEDetails
STATIC s_lIgnoreErrOp
STATIC s_lObjectDump
STATIC s_lCatchErr
STATIC s_lCheckResult

PROCEDURE Main( cArg01, cArg02, cArg03, cArg04 )

   LOCAL o

   LOCAL cCommandLine

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

   // ;

   cCommandLine := cArg01 + " " + cArg02 + " " + cArg03 + " " + cArg04

   s_lCallBackStack := "CALLBACKSTACK" $ Upper( cCommandLine )
   s_lRTEDetails := "RTEDETAILS" $ Upper( cCommandLine )
   s_lIgnoreErrOp := "IGNERROP" $ Upper( cCommandLine )
   s_lObjectDump := !( "NODUMP" $ Upper( cCommandLine ) )
   s_lCatchErr := .T.
   s_lCheckResult := .F.

   s_lRTEDetails := .T.
// s_lIgnoreErrOp := .T.

   // ;

   #ifdef __HARBOUR__
      s_fhnd := FCreate( "tb_hb.txt", FC_NORMAL )
   #else
      s_fhnd := FCreate( "tb_cl5.txt", FC_NORMAL )
   #endif

   IF s_fhnd == F_ERROR
      RETURN
   ENDIF

   // ;

   o := TBColumnNew( "test00", {|| "test00" } )
   TEST_L_TBC( OBJ_CREATE() )
   TEST_L_TBC( o:defColor := {} )
   TEST_L_TBC( o:defColor := { 1 } )
   TEST_L_TBC( o:defColor := NIL )
   TEST_L_TBC( o:defColor := { 1, 2 } )
   TEST_L_TBC( o:defColor := { 1, 2, 3 } )
   TEST_L_TBC( o:defColor := { 1, 2, 3, 4 } )
   TEST_L_TBC( o:defColor := { 1, 2, 3, 4, 5 } )
   TEST_L_TBC( o:defColor := { "1" } )
   TEST_L_TBC( o:defColor := { "1", "2" } )
   TEST_L_TBC( o:defColor := { "1", "2", "3" } )
   TEST_L_TBC( o:defColor := { "1", "2", "3", "4" } )
   TEST_L_TBC( o:defColor := { "1", "2", "3", "4", "5" } )
   TEST_L_TBC( o:defColor := { "1", 2, "3" } )

   // ;

   s_lCheckResult := .T.

   TEST_L_TBC( TBColumnNew( NIL                  , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( -1                   , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( 0                    , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( 1                    , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( 3                    , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( 25                   , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( ""                   , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( "az"                 , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( hb_SToD( "20070425" ), {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( .F.                  , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( .T.                  , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( {|| NIL }            , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( {}                   , {|| "test00" } ) )
   TEST_L_TBC( TBColumnNew( { "" }               , {|| "test00" } ) )

   TEST_L_TBC( TBColumnNew( "test00", NIL                   ) )
   TEST_L_TBC( TBColumnNew( "test00", -1                    ) )
   TEST_L_TBC( TBColumnNew( "test00", 0                     ) )
   TEST_L_TBC( TBColumnNew( "test00", 1                     ) )
   TEST_L_TBC( TBColumnNew( "test00", 3                     ) )
   TEST_L_TBC( TBColumnNew( "test00", 25                    ) )
   TEST_L_TBC( TBColumnNew( "test00", ""                    ) )
   TEST_L_TBC( TBColumnNew( "test00", "az"                  ) )
   TEST_L_TBC( TBColumnNew( "test00", hb_SToD( "20070425" ) ) )
   TEST_L_TBC( TBColumnNew( "test00", .F.                   ) )
   TEST_L_TBC( TBColumnNew( "test00", .T.                   ) )
   TEST_L_TBC( TBColumnNew( "test00", {|| NIL }             ) )
   TEST_L_TBC( TBColumnNew( "test00", {}                    ) )
   TEST_L_TBC( TBColumnNew( "test00", { "" }                ) )

   s_lCheckResult := .F.

   // ;

   TBRAssign( NIL )
   TBRAssign( -1 )
   TBRAssign( 0 )
   TBRAssign( 1 )
   TBRAssign( 3 )
   TBRAssign( 3.3 )
   TBRAssign( 3.7 )
   TBRAssign( 25 )
   TBRAssign( 25.3 )
   TBRAssign( 25.7 )
   TBRAssign( "" )
   TBRAssign( "az" )
   TBRAssign( hb_SToD( "20070425" ) )
   TBRAssign( .F. )
   TBRAssign( .T. )
   TBRAssign( {|| NIL } )
   TBRAssign( {} )
   TBRAssign( { "" } )

   // ;

   TBCAssign( NIL )
   TBCAssign( -1 )
   TBCAssign( 0 )
   TBCAssign( 1 )
   TBCAssign( 3 )
   TBCAssign( 3.3 )
   TBCAssign( 3.7 )
   TBCAssign( 25 )
   TBCAssign( 25.3 )
   TBCAssign( 25.7 )
   TBCAssign( "" )
   TBCAssign( "az" )
   TBCAssign( hb_SToD( "20070425" ) )
   TBCAssign( .F. )
   TBCAssign( .T. )
   TBCAssign( {|| NIL } )
   TBCAssign( {} )
   TBCAssign( { "" } )

   // ;

   s_cTest := ""

   // ;

   s_lCatchErr := .F.

   o := TBrowseNew( 10, 10, 20, 50 )
   TEST_L_TBR( OBJ_CREATE() )
   TEST_L_TBR( o:AddColumn( TBColumnNew( "test01h", {|| "test01d" } )  ) )
   TEST_L_TBR( o:DelColumn( 1 ) )
   TEST_L_TBR( o:Left() )
   TEST_L_TBR( o:Right() )

   // ;

#ifdef HB_COMPAT_C53

   o := TBColumnNew( "test01h", {|| "test01d" } )
   TEST_L_TBC( OBJ_CREATE() )
   TEST_L_TBC( o:SetStyle( 1 )      )
   TEST_L_TBC( o:SetStyle( 2 )      )
   TEST_L_TBC( o:SetStyle( 3 )      )
   TEST_L_TBC( o:SetStyle( 4, .T. ) )
   TEST_L_TBC( o:SetStyle( 4 )      )
   TEST_L_TBC( o:SetStyle( 4, NIL ) )
   TEST_L_TBC( o:SetStyle( 4 )      )
   TEST_L_TBC( o:SetStyle( 5 )      )
   TEST_L_TBC( o:SetStyle( 5, .T. ) )
   TEST_L_TBC( o:SetStyle( 5, .F. ) )

   o := TBrowseNew( 10, 10, 20, 50 )
   TEST_L_TBR( OBJ_CREATE() )
   TEST_L_TBR( o:SetStyle( 1 )      )
   TEST_L_TBR( o:SetStyle( 2 )      )
   TEST_L_TBR( o:SetStyle( 3 )      )
   TEST_L_TBR( o:SetStyle( 4 )      )
   TEST_L_TBR( o:SetStyle( 5 )      )
   TEST_L_TBR( o:SetStyle( 4, .T. ) )
   TEST_L_TBR( o:SetStyle( 4 )      )
   TEST_L_TBR( o:SetStyle( 4, NIL ) )
   TEST_L_TBR( o:SetStyle( 4 )      )
   TEST_L_TBR( o:SetStyle( 6 )      )
   TEST_L_TBR( o:SetStyle( 6, .T. ) )
   TEST_L_TBR( o:SetStyle( 6, .F. ) )

#endif

   // ;

   FClose( s_fhnd )

   RETURN

PROCEDURE TBRAssign( xVar )
   LOCAL o

   s_xVar := xVar

   s_cTest := "TBrowse (empty) assigning: " + XToStr( xVar )

   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:AutoLite      := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:Cargo         := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:ColCount      := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:ColorSpec     := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:ColPos        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:ColSep        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:FootSep       := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:Freeze        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:GoBottomBlock := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:GoTopBlock    := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:HeadSep       := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:HitBottom     := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:HitTop        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:LeftVisible   := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:nBottom       := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:nLeft         := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:nRight        := xVar )
   // ; This is needed for CA-Cl*pper 5.x otherwise an unmaskable (bug?) RTE would be thrown. [vszakats]
   IF ValType( xVar ) == "N" .AND. xVar < o:nBottom
      o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:nTop          := xVar )
   ENDIF
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:RightVisible  := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:RowCount      := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:RowPos        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:SkipBlock     := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:Stable        := xVar )
#ifdef HB_COMPAT_C53
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:border        := xVar )
   o := TBrowseNew( 10, 10, 20, 50 ) ; TEST_L_TBR( o:message       := xVar )
#endif

   RETURN

PROCEDURE TBCAssign( xVar )
   LOCAL o

   s_xVar := xVar

   s_cTest := "TBColumn assigning: " + XToStr( xVar )

   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Block      := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Cargo      := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:ColorBlock := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:ColSep     := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:DefColor   := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Footing    := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:FootSep    := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Heading    := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:HeadSep    := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Picture    := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:Width      := xVar )
#ifdef HB_COMPAT_C53
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:preBlock   := xVar )
   o := TBColumnNew( "test01h", {|| "test01d" } ) ; TEST_L_TBC( o:postBlock  := xVar )
#endif

   RETURN

PROCEDURE TEST_C_TBR( o, cBlock, bBlock )
   LOCAL xResult
   LOCAL bOldError
   LOCAL oError

   SetPos( 0, 0 ) // ; To check where the cursor was moved after evaluating the block.

   IF s_lCatchErr
      bOldError := ErrorBlock( {| oError | Break( oError ) } )
   ENDIF

   BEGIN SEQUENCE
      xResult := Eval( bBlock )
   RECOVER USING oError
      xResult := ErrorMessage( oError )
   END SEQUENCE

   IF s_lCatchErr
      ErrorBlock( bOldError )
   ENDIF

   LogTBRVars( o, cBlock, xResult )

   RETURN

PROCEDURE TEST_C_TBC( o, cBlock, bBlock )
   LOCAL xResult
   LOCAL bOldError
   LOCAL oError

   SetPos( 0, 0 ) // ; To check where the cursor was moved after evaluating the block.

   IF s_lCatchErr
      bOldError := ErrorBlock( {| oError | Break( oError ) } )
   ENDIF

   BEGIN SEQUENCE
      xResult := Eval( bBlock )
   RECOVER USING oError
      xResult := ErrorMessage( oError )
   END SEQUENCE

   IF s_lCatchErr
      ErrorBlock( bOldError )
   ENDIF

   IF s_lCheckResult
      LogTBCVars( xResult, cBlock, xResult )
   ELSE
      LogTBCVars( o, cBlock, xResult )
   ENDIF

   RETURN

PROCEDURE LogMe( data, desc )
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

   IF PCount() > 2
      FWrite( s_fhnd, cStack + "BLOCK_SET  " + iif( data == NIL, "NIL", data ) + "  " + desc + hb_eol() )
   ELSE
      FWrite( s_fhnd, cStack + "BLOCK_GET  " + desc + hb_eol() )
   ENDIF

   RETURN

PROCEDURE LogTBRVars( o, desc, xResult )
   LOCAL nLevel
   LOCAL cStack

   LOCAL tmp
   LOCAL col

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
   FWrite( s_fhnd, "   s_xVar        " + XToStr( s_xVar          ) + hb_eol() )
   FWrite( s_fhnd, "   xResult       " + XToStr( xResult         ) + hb_eol() )
   FWrite( s_fhnd, "   Row()         " + XToStr( Row()           ) + hb_eol() )
   FWrite( s_fhnd, "   Col()         " + XToStr( Col()           ) + hb_eol() )
   FWrite( s_fhnd, "   AutoLite      " + XToStr( o:AutoLite      ) + hb_eol() )
   FWrite( s_fhnd, "   Cargo         " + XToStr( o:Cargo         ) + hb_eol() )
   FWrite( s_fhnd, "   ColCount      " + XToStr( o:ColCount      ) + hb_eol() )
   FWrite( s_fhnd, "   ColorSpec     " + XToStr( o:ColorSpec     ) + hb_eol() )
   FWrite( s_fhnd, "   ColPos        " + XToStr( o:ColPos        ) + hb_eol() )
   FWrite( s_fhnd, "   ColSep        " + XToStr( o:ColSep        ) + hb_eol() )
   FWrite( s_fhnd, "   FootSep       " + XToStr( o:FootSep       ) + hb_eol() )
   FWrite( s_fhnd, "   Freeze        " + XToStr( o:Freeze        ) + hb_eol() )
   FWrite( s_fhnd, "   GoBottomBlock " + XToStr( o:GoBottomBlock ) + hb_eol() )
   FWrite( s_fhnd, "   GoTopBlock    " + XToStr( o:GoTopBlock    ) + hb_eol() )
   FWrite( s_fhnd, "   HeadSep       " + XToStr( o:HeadSep       ) + hb_eol() )
   FWrite( s_fhnd, "   HitBottom     " + XToStr( o:HitBottom     ) + hb_eol() )
   FWrite( s_fhnd, "   HitTop        " + XToStr( o:HitTop        ) + hb_eol() )
   FWrite( s_fhnd, "   LeftVisible   " + XToStr( o:LeftVisible   ) + hb_eol() )
   FWrite( s_fhnd, "   nBottom       " + XToStr( o:nBottom       ) + hb_eol() )
   FWrite( s_fhnd, "   nLeft         " + XToStr( o:nLeft         ) + hb_eol() )
   FWrite( s_fhnd, "   nRight        " + XToStr( o:nRight        ) + hb_eol() )
   FWrite( s_fhnd, "   nTop          " + XToStr( o:nTop          ) + hb_eol() )
   FWrite( s_fhnd, "   RightVisible  " + XToStr( o:RightVisible  ) + hb_eol() )
   FWrite( s_fhnd, "   RowCount      " + XToStr( o:RowCount      ) + hb_eol() )
   FWrite( s_fhnd, "   RowPos        " + XToStr( o:RowPos        ) + hb_eol() )
   FWrite( s_fhnd, "   SkipBlock     " + XToStr( o:SkipBlock     ) + hb_eol() )
   FWrite( s_fhnd, "   Stable        " + XToStr( o:Stable        ) + hb_eol() )
#ifdef HB_COMPAT_C53
   FWrite( s_fhnd, "   border       " + XToStr( o:border        ) + hb_eol() )
   FWrite( s_fhnd, "   message      " + XToStr( o:message       ) + hb_eol() )
#endif
   IF s_lObjectDump
#ifdef __HARBOUR__
#ifdef HB_COMPAT_C53
      FOR tmp := 1 TO 18
#else
      FOR tmp := 1 TO 13
#endif
#else
      FOR tmp := 1 TO Len( o )
#endif
         /* [14] is binary data, not replicated in Harbour. */
         IF tmp != 14
            FWrite( s_fhnd, "   [ " + Str( tmp, 3 ) + " ]       " + XToStrX( o[ tmp ] ) + hb_eol() )
         ENDIF
      NEXT
   ENDIF
   FOR tmp := 1 TO o:colCount
      FWrite( s_fhnd, "   Column: " + StrZero( tmp, 3 ) + hb_eol() )
      col := o:GetColumn( tmp )
      IF ValType( col ) == "O"
         FWrite( s_fhnd, "      Block         " + XToStr( col:Block       ) + hb_eol() )
         FWrite( s_fhnd, "      Cargo         " + XToStr( col:Cargo       ) + hb_eol() )
         FWrite( s_fhnd, "      ColorBlock    " + XToStr( col:ColorBlock  ) + hb_eol() )
         FWrite( s_fhnd, "      ColSep        " + XToStr( col:ColSep      ) + hb_eol() )
         FWrite( s_fhnd, "      DefColor      " + XToStr( col:DefColor    ) + hb_eol() )
         FWrite( s_fhnd, "      Footing       " + XToStr( col:Footing     ) + hb_eol() )
         FWrite( s_fhnd, "      FootSep       " + XToStr( col:FootSep     ) + hb_eol() )
         FWrite( s_fhnd, "      Heading       " + XToStr( col:Heading     ) + hb_eol() )
         FWrite( s_fhnd, "      HeadSep       " + XToStr( col:HeadSep     ) + hb_eol() )
         FWrite( s_fhnd, "      Picture       " + XToStr( col:Picture     ) + hb_eol() )
         FWrite( s_fhnd, "      Width         " + XToStr( col:Width       ) + hb_eol() )
#ifdef HB_COMPAT_C53
         FWrite( s_fhnd, "      preBlock      " + XToStr( col:preBlock    ) + hb_eol() )
         FWrite( s_fhnd, "      postBlock     " + XToStr( col:postBlock   ) + hb_eol() )
#endif
      ELSE
         FWrite( s_fhnd, "      Col:          " + XToStr( col             ) + hb_eol() )
      ENDIF
   NEXT
   FWrite( s_fhnd, "---------------------" + hb_eol() )

   RETURN

PROCEDURE LogTBCVars( o, desc, xResult )
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
   FWrite( s_fhnd, "   s_xVar        " + XToStr( s_xVar        ) + hb_eol() )
   FWrite( s_fhnd, "   xResult       " + XToStr( xResult       ) + hb_eol() )
   IF ValType( o ) == "O"
      FWrite( s_fhnd, "   Block         " + XToStr( o:Block       ) + hb_eol() )
      FWrite( s_fhnd, "   Cargo         " + XToStr( o:Cargo       ) + hb_eol() )
      FWrite( s_fhnd, "   ColorBlock    " + XToStr( o:ColorBlock  ) + hb_eol() )
      FWrite( s_fhnd, "   ColSep        " + XToStr( o:ColSep      ) + hb_eol() )
      FWrite( s_fhnd, "   DefColor      " + XToStr( o:DefColor    ) + hb_eol() )
      FWrite( s_fhnd, "   Footing       " + XToStr( o:Footing     ) + hb_eol() )
      FWrite( s_fhnd, "   FootSep       " + XToStr( o:FootSep     ) + hb_eol() )
      FWrite( s_fhnd, "   Heading       " + XToStr( o:Heading     ) + hb_eol() )
      FWrite( s_fhnd, "   HeadSep       " + XToStr( o:HeadSep     ) + hb_eol() )
      FWrite( s_fhnd, "   Picture       " + XToStr( o:Picture     ) + hb_eol() )
      FWrite( s_fhnd, "   Width         " + XToStr( o:Width       ) + hb_eol() )
#ifdef HB_COMPAT_C53
      FWrite( s_fhnd, "   preBlock      " + XToStr( o:preBlock    ) + hb_eol() )
      FWrite( s_fhnd, "   postBlock     " + XToStr( o:postBlock   ) + hb_eol() )
#endif
      IF s_lObjectDump
#ifdef __HARBOUR__
#ifdef HB_COMPAT_C53
         FOR tmp := 1 TO 14
#else
         FOR tmp := 1 TO 11
#endif
#else
         FOR tmp := 1 TO Len( o )
#endif
            FWrite( s_fhnd, "   [ " + Str( tmp, 3 ) + " ]       " + XToStrX( o[ tmp ] ) + hb_eol() )
         NEXT
      ENDIF
   ELSE
      FWrite( s_fhnd, "   o             " + XToStr( o ) + hb_eol() )
   ENDIF
   FWrite( s_fhnd, "---------------------" + hb_eol() )

   RETURN

FUNCTION XToStr( xValue )
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
   CASE cType == "B" ; RETURN '{||...} -> ' + XToStr( Eval( xValue ) )
   CASE cType == "A" ; RETURN '{ ' + ArrayToList( xValue ) + ' }'
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
   ENDCASE

   RETURN ""

FUNCTION ArrayToList( a )
   LOCAL tmp
   LOCAL cString := ""

   FOR tmp := 1 TO Len( a )
      cString += XToStr( a[ tmp ] )
      IF tmp < Len( a )
         cString += ", "
      ENDIF
   NEXT

   RETURN cString

FUNCTION XToStrE( xValue )
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
   CASE cType == "B" ; RETURN '{||...} -> ' + XToStrE( Eval( xValue ) )
   CASE cType == "A" ; RETURN '{ ' + ArrayToEList( xValue ) + ' }'
   CASE cType == "M" ; RETURN 'M:' + xValue
   ENDCASE

   RETURN ""

FUNCTION XToStrX( xValue )
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
   CASE cType == "B" ; RETURN '{||...} -> ' + XToStrX( Eval( xValue ) )
   CASE cType == "A"

      cRetVal := '{ '

      FOR tmp := 1 TO Len( xValue )
         cRetVal += XToStrX( xValue[ tmp ] )
         IF tmp < Len( xValue )
            cRetVal += ", "
         ENDIF
      NEXT

      RETURN cRetVal + ' }'

   CASE cType == "M" ; RETURN 'M:' + xValue
   ENDCASE

   RETURN ""

FUNCTION ArrayToEList( a )
   LOCAL tmp
   LOCAL cString := ""

   FOR tmp := 1 TO Len( a )
      cString += XToStrE( a[ tmp ] )
      IF tmp < Len( a )
         cString += ", "
      ENDIF
   NEXT

   RETURN cString

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
      IF ! Empty( oError:operation ) .AND. ! s_lIgnoreErrOp
         /* NOTE: Clipping this to hide the difference in maximum symbol name length in error messages. [vszakats] */
         cMessage += Left( oError:operation, 9 ) + " "
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
   ELSE
      cMessage := "(ERROR)"
   ENDIF

   RETURN cMessage

#ifdef __XPP__
FUNCTION hb_SToD( cDate )
   RETURN SToD( cDate )
#endif

#ifdef __XHARBOUR__
FUNCTION hb_SToD( cDate )
   RETURN SToD( cDate )
#endif

#ifndef HAVE_HBCLIP
#ifndef __HARBOUR__
#ifndef __XPP__

FUNCTION hb_SToD( s )

   LOCAL cDf := Set( _SET_DATEFORMAT, "YYYY/MM/DD" ), dt

   dt := CToD( Stuff( Stuff( s, 7, 0, "/" ), 5, 0, "/" ) )
   Set( _SET_DATEFORMAT, cDf )

   RETURN dt

#endif
#endif
#endif

PROCEDURE OBJ_CREATE()

   // ; Dummy

   RETURN

/* We use this to wash out a small incompatibility in Harbour's built-in __eInstVar53(). */

FUNCTION __eInstVar53( oVar, cMethod, xValue, cType, nSubCode, bValid )

   LOCAL oError

   IF !( ValType( xValue ) == cType ) .OR. ;
      ( bValid != NIL .AND. ! Eval( bValid, oVar, xValue ) )
      oError := ErrorNew()
      oError:description := hb_langErrMsg( 1 )
      oError:gencode := 1
      oError:severity := 2
      oError:cansubstitute := .T.
      oError:subsystem := oVar:classname
      HB_SYMBOL_UNUSED( cMethod )
      oError:subcode := nSubCode
      oError:args := { xValue }
      xValue := Eval( ErrorBlock(), oError )
      IF !( ValType( xValue ) == cType )
         __errInHandler()
      ENDIF
   ENDIF

   RETURN xValue

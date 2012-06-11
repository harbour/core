/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "inkey.ch"
#include "gra.ch"
#include "set.ch"

#ifdef __XPP__
   #pragma library("XppUi2")
#endif

#ifndef __HARBOUR__
   #define HB_SYMBOL_UNUSED( x ) ( x := x )
#endif

#ifndef __XPP__
   REQUEST HB_GT_QTC
#endif

/*----------------------------------------------------------------------*/

#define CRLF    chr( 13 )+chr( 10 )

STATIC snCount := 1
STATIC s_oDlg

/*----------------------------------------------------------------------*/

PROCEDURE Main()
   BuildADialog()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE BuildADialog()
   LOCAL oDlg, mp1, mp2, oXbp, nEvent, aSize, oDa

   /* Create Application Window */
   oDlg := GuiStdDialog( "Harbour - hbQT - hbXBP + GtQtc ( Xbase++ )" )

   oDlg:close := {|| MsgBox( "You can also close me by pressing [ESC]" ) }
   SetAppWindow( oDlg )
   oDlg:Show()

   oDa := oDlg:drawingArea

   /* Obtain desktop dimensions */
   aSize := AppDesktop():currentSize()
   /* Place on the center of desktop */
   oDlg:setPos( { ( aSize[ 1 ] - oDlg:currentSize()[ 1 ] ) / 2, ;
                  ( aSize[ 2 ] - oDlg:currentSize()[ 2 ] ) / 2 } )

   /* Make background color of :drawingArea different */
   oDa:setFontCompoundName( "10.Tohama italics" )
   //oDa:setColorFG( GraMakeRGBColor( { 255,255,255 } ) )

   /* Install menu system */
   Build_MenuBar( oDlg )
   /* Install Statusbar */
   Build_StatusBar( oDa )
   /* Install Toolbar */
   Build_ToolBar( oDlg )

   s_oDlg := oDlg
   /* Present the dialog on the screen */
   oDlg:Show()

   /* Enter Xbase++ Event Loop - working */
   DO WHILE .t.
      nEvent := AppEvent( @mp1, @mp2, @oXbp )
      IF ( nEvent == xbeP_Close ) .OR. ( nEvent == xbeP_Keyboard .and. mp1 == xbeK_ESC )
         EXIT
      ENDIF
      HB_TRACE( HB_TR_DEBUG, nEvent, valtype( oXbp ), iif( HB_ISOBJECT( oXbp ), oXbp:title, "Nothing" ) )
      oXbp:handleEvent( nEvent, mp1, mp2 )
   ENDDO

   oDlg:destroy()

   RETURN

/*----------------------------------------------------------------------*/

STATIC FUNCTION GuiStdDialog( cTitle )
   LOCAL oDlg

   DEFAULT cTitle TO "Standard Dialog Window"

   oDlg          := XbpDialog():new( , , {10,10}, {900,500}, , .f. )

   /* NOTE: method to install the windows icon is bit different than Windows */
   /* So curretly we can only place disk icon file only */
   oDlg:icon     := hb_DirBase() + "test.ico"
   /* TODO: still not implemented*/
   oDlg:taskList := .T.
   oDlg:title    := cTitle
   oDlg:create()

   RETURN oDlg

/*----------------------------------------------------------------------*/

STATIC FUNCTION Build_MenuBar( oDlg )
   LOCAL oMenuBar, oSubMenu

   //oMenuBar := XbpMenuBar():new( oDlg ):create()
   oMenuBar := oDlg:MenuBar()

   /* Define submenu in procedural style.
    * The numeric index of the selected menu item
    * is passed to the Callback code block -> mp1
    */
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   //
   oSubMenu:title := "~Procedural"
   oSubMenu:addItem( { "Play Charge ~1",   } )
   oSubMenu:addItem( { "Play Nannyboo ~2", } )
   oSubMenu:itemSelected := {|mp1| MyFunctionXbp( 100+mp1 ) }
   //
   oMenuBar:addItem( { oSubMenu, NIL } )
   //
   oSubMenu:disableItem( 2 )

   /* Define submenu in the functional style:
    * A menu item executes a code block that calls a function
    */
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Functional"
   oSubMenu:addItem( { "Play Opening ~1"+chr(K_TAB)+"Ctrl+U", {|| MyFunctionXbp( 1 ) } } )
   oSubMenu:addItem( { "Play Closing ~2"                    , {|| MyFunctionXbp( 2 ) } } )
   oSubMenu:addItem( { NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, NIL } )
   oSubMenu:addItem( { "new.png|~MessageBox"                , {|| MyFunctionXbp( 3 ) }  , , XBPMENUBAR_MIA_HILITED } )
   oMenuBar:addItem( { oSubMenu, NIL } )
   //
   oSubMenu:insItem( 2, { "This executes MsgBox()"          , {|| MyFunctionXbp( 103 ) }, , XBPMENUBAR_MIA_CHECKED } )
   oSubMenu:itemMarked := {|mp1| IF( mp1 == 5, MsgBox( "WOW - ::itemMarked - Activated" ), NIL ) }

   /* Menu colors are being honored in Harbour only */
   oSubMenu:setColorBG( GraMakeRGBColor( { 134,128,250 } ) )
   oSubMenu:setColorFG( GraMakeRGBColor( { 255,  1,  1 } ) )

   Return nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION MyFunctionXbp( nMode )

   DO CASE
   CASE nMode == 1
      MsgBox( "Play Opening" + STR( GraMakeRGBColor( { 134,128,164 } ) ) )

   CASE nMode == 2
      MsgBox( "Play Closing ~2" )

   CASE nMode == 3
      MsgBox( "new.png|~MessageBox" )

   CASE nMode == 101
      MsgBox( "101 - Play Charge" )

   CASE nMode == 102
      MsgBox( "102 - Play Nanyboo" )

   CASE nMode == 103
      MsgBox( "This executes MsgBox()" )

   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_ToolBar( oDlg )
   LOCAL oTBar
   LOCAL oDa := oDlg:drawingArea

   // Create an XbpToolBar object and
   // add it at the top of the dialog
   //
   oTBar := XbpToolBar():new( oDa )
   oTBar:create( , , { 0, oDA:currentSize()[ 2 ]-60 }, ;
                     { oDA:currentSize()[ 1 ], 60 } )
   //
   // Add two tool bar buttons, each with a
   // caption and an image. Constrict the
   // button image sizes to 32 pixels and
   // ensure transparency is turned off.
   //
   oTBar:imageWidth  := 32
   oTBar:imageHeight := 32

   /* Harbour does not support resource IDs so giving bitmap files */
   #ifdef __HARBOUR__
      oTBar:addItem( "Save"        , hb_DirBase() + "new.png"  , , , , , "1" )
      oTBar:addItem( "Open"        , hb_DirBase() + "open.png" , , , , , "2" )
      oTBar:addItem( "Font Dialog" , hb_DirBase() + "copy.png" , , , , , "3" )
      oTBar:addItem( "Print Dialog", hb_DirBase() + "print.png", , , , , "4" )
      oTBar:addSeparator()
      oTBar:addItem( "Inner-Dlg"    )
      oTBar:addItem( "Free-Dlg"     )
      oTBar:addItem( "Owned-Dlg"    )
      oTBar:addItem( "Crt-Dlg"      )
      oTBar:addItem( "Crt-Dlg-T"    )
   #else
      oTBar:addItem( "Save"         )
      oTBar:addItem( "Open"         )
      oTBar:addItem( "Font Dialog"  )
      oTBar:addItem( "Print Dialog" )
      oTBar:addSeparator()
      oTBar:addItem( "Inner-Dlg"    )
      oTBar:addItem( "Free-Dlg"     )
      oTBar:addItem( "Owned-Dlg"    )
      oTBar:addItem( "Crt-Dlg"      )
      oTBar:addItem( "Crt-Dlg-T"    )
   #endif

   oTBar:transparentColor := GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| ExeToolbar( oButton, oDa, oDlg ) }

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION ExeToolbar( oButton, oDa, oDlg )

   DO CASE
   CASE oButton:caption == "Inner-Dlg"
      CreateInnerChildWindow( oDlg )
   CASE oButton:caption == "Free-Dlg"
      CreateOuterChildWindow( oDlg, .f. )
   CASE oButton:caption == "Owned-Dlg"
      CreateOuterChildWindow( oDlg, .t. )
   CASE oButton:caption == "Crt-Dlg"
      CreateGtQtc( oDlg, .f. )
   CASE oButton:caption == "Crt-Dlg-T"
      CreateGtQtc( oDlg, .t. )
   CASE oButton:caption == "Save"
      Build_FileDialog( oDA,"save" )
   CASE oButton:caption == "Open"
      Build_FileDialog( oDA,"open" )
   CASE oButton:caption == "Font Dialog"
      Build_FontDialog( oDa )
   CASE oButton:caption == "Print Dialog"
      Build_PrintDialog( oDa )
   ENDCASE

   RETURN NIL

/*----------------------------------------------------------------------*/

// Create a child window
Function CreateInnerChildWindow( oODlg )
   LOCAL oParent, oDlg

   oParent := oODlg:drawingArea

   oDlg          := XbpDialog():new( oParent, /*oOwner*/, {48,48}, {240,180} )
   oDlg:title    := "Inner Dlg#" + Ltrim(Str( snCount++ ) )
   oDlg:taskList := .F.
   oDlg:close    := {|mp1,mp2,obj| mp1 := mp1, mp2 := mp2, obj:destroy() }
   oDLg:clipSiblings := .T.
   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( "10.Arial" )
   oDlg:show()
   SetAppFocus( oDlg )

   Return( NIL )

/*----------------------------------------------------------------------*/

// Create a Outer child window
Function CreateOuterChildWindow( oODlg, lMoveWithOwner )
   Local oOwner := oODlg
   LOCAL oParent, oDlg, aPos, aSize

   oParent := AppDesktop()
   aPos    := oOwner:currentPos()
   aPos    := { aPos[ 1 ] + oOwner:currentSize()[ 1 ], aPos[ 2 ] }
   aSize   := { 250,200 }

   oDlg := XbpDialog():new( oParent, oOwner, aPos, aSize )

   oDlg:title         := iif( lMoveWithOwner, "O:Attached", "O:Free" ) + " Dlg # " + Ltrim( Str( snCount++ ) )
   oDlg:taskList      := .F.
   oDlg:close         := {|mp1,mp2,obj| mp1 := mp1, mp2 := mp2, obj:destroy() }
   oDLg:clipSiblings  := .T.
   oDLg:moveWithOwner := lMoveWithOwner

   oDlg:create()
   oDlg:drawingArea:setFontCompoundName( "10.Arial" )
   oDlg:show()

   SetAppFocus( oDlg )

   Return( NIL )

/*----------------------------------------------------------------------*/

FUNCTION CreateGtQtc( oDlg, lThreaded )
   if lThreaded
      hb_threadDetach( hb_threadStart( { || CreateGtQtc_1() } ) )
   else
      CreateGtQtc_1( oDlg )
   endif
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION CreateGtQtc_1( oDlg )
   LOCAL oXbp, cReply
   LOCAL GetList := {}

   SET SCOREBOARD OFF

   ErrorBlock( {|o| MyError( o ) } )

   oXbp := XbpCrt():new( iif( !empty( oDlg ), oDlg:drawingArea, NIL ), , { 10,10 }, { 10,80 } )
   oXbp:title       := "My First CRT"
   oXbp:toolTiptext := "Really My First XbpCRT()"
   oXbp:lModal      := .f.
   oXbp:icon        := "E:\harbour\contrib\hbide\build.png" /* not working yet */
   oXbp:create()

   SetColor( "N/W" )
   SetKey( K_F2, {|| AnotherGet() } )
   SetMode( 24,80 )

   DO WHILE .T.
      CLS

      cReply := space( 13 )

      @  0, 0  SAY padc( "This is Console in GUI Interface", maxcol()+1 ) COLOR "W+/B"
      @ maxrow(), 0 SAY padc( "Press F2 : Another Get", maxcol()+1 ) COLOR "N/W*"

      @ 09, 20 SAY padc( " Hello World ", 38 ) COLOR "N/GR*"
      @ 15, 20 SAY " How are You " COLOR "W+/G"

      @ 15, 45 GET cReply COLOR "W/N,W+/R"
      READ

      IF lastkey() == 27
         EXIT
      ENDIF
   ENDDO

   oXbp:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION AnotherGet()
   LOCAL nRow := row(), nCol := col()
   LOCAL cScr := SaveScreen( 0,0,maxrow(),maxcol() )
   LOCAL cColor := SetColor( "BG/B" )
   LOCAL cReply := space( 21 )
   LOCAL GetList := {}

   CLS

   @ 13, 29 SAY " So it is Harbour QT " COLOR "W+/G"
   @ 15, 29 GET cReply COLOR "W/N,W+/R"
   READ

   RestScreen( 0, 0, maxrow(), maxcol(), cScr )
   SetColor( cColor )
   SetPos( nRow,nCol )
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION MyError( o )
   LOCAL s, i

   s := o:description + ";"
   s += o:operation + ";"
   FOR i := 1 TO 7
      s += ProcName( i ) + ";"
   NEXT
   alert( s )
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Build_StatusBar( oWnd )
   LOCAL oSBar, oPanel

   oSBar := XbpStatusBar():new( oWnd )
   oSBar:create( oWnd, , { 0,0 }, { oWnd:currentSize()[1],30 } )

   oPanel := oSBar:getItem( 1 )
   oPanel:caption  := "Harbour-QT-Xbase++ is Ready"
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   #ifdef __XPP__
   oSBar:setPointer( , XBPSTATIC_SYSICON_SIZEWE, XBPWINDOW_POINTERTYPE_SYSPOINTER )
   #else
   oSBar:setPointer( , hb_DirBase() + "vr.png", XBPWINDOW_POINTERTYPE_ICON )
   #endif

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION Build_FileDialog( oWnd, cMode )
   LOCAL oDlg, aFiles

   oDlg := XbpFileDialog():new():create( oWnd, , { 10,10 } )
   IF cMode == "open"
      oDlg:title       := "Open Index or Database"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "Index Files", "*.ntx" }, { "Database Files", "*.dbf" } }
      //oDlg:setColorBG( GraMakeRGBColor( { 170,170,170 } ) )
      aFiles := oDlg:open( "c:\temp", , .t. )
      IF !empty( aFiles )
         aeval( aFiles, {|e| HB_SYMBOL_UNUSED( e ) } )
      ENDIF
   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      oDlg:saveAs( "c:\temp\myfile.dbf" )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Build_FontDialog( oWnd )
   LOCAL oDlg

   oDlg := XbpFontDialog():new( oWnd, , , , { 20,20 } )
   oDlg:activateOk := {|oFont| DisplayFontInfo( oFont ) }
   oDlg:create()

   oDlg:display( 0 )

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION DisplayFontInfo( /*oFont*/ )
   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION Build_PrintDialog( oWnd )
   LOCAL oDlg, oPrn

   oDlg := XbpPrintDialog():new( oWnd ):create()
   oDlg:enablePrintToFile := .t.
   ODlg:pageRange := { 1,3 }
   oDlg:printRange := XBPPDLG_PRINT_PAGERANGE

   IF valtype( oPrn := oDlg:display() ) == "O"
      MyDebug( oPrn:devName           , ;
               oPrn:setOrientation()  , ;
               oPrn:setFormSize()     , ;
               oPrn:setResolution()[1], ;
               oPrn:setNumCopies()    , ;
               oPrn:setPaperBin()     , ;
               oPrn:setColorMode()    , ;
               oPrn:setDuplexMode()   , ;
               oPrn:setCollationMode()  ;
             )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

#ifdef __XPP__
FUNCTION Hb_Symbol_Unused()  ; RETURN NIL
FUNCTION Hb_NtoS( n )        ; RETURN ltrim( str( n ) )
FUNCTION Hb_ThreadStart()    ; RETURN NIL
FUNCTION hb_DirBase()        ; RETURN CurDir()
#endif

/*----------------------------------------------------------------------*/

STATIC FUNCTION uiXtoS( xVar )
   LOCAL cType

   cType := valtype( xVar )
   DO CASE
   CASE cType == "N"
      RETURN str( xVar )
   CASE cType == "D"
      RETURN dtoc( xVar )
   CASE cType == "L"
      RETURN IF( xVar, "Yes", "No" )
   CASE cType == "M"
      RETURN xVar
   CASE cType == "C"
      RETURN xVar
   CASE cType == "A"
      RETURN "A:"+hb_ntos( len( xVar ) )
   CASE cType == "O"
      RETURN "[OBJECT]"
   OTHERWISE
      RETURN "["+cType+"]"
   ENDCASE

   RETURN xVar

/*----------------------------------------------------------------------*/

FUNCTION MyDebug( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 )
   LOCAL s

   s := uiXtoS( p1 ) + CRLF
   s += uiXtoS( p2 ) + CRLF
   s += uiXtoS( p3 ) + CRLF
   s += uiXtoS( p4 ) + CRLF
   s += uiXtoS( p5 ) + CRLF
   s += uiXtoS( p6 ) + CRLF
   s += uiXtoS( p7 ) + CRLF
   s += uiXtoS( p8 ) + CRLF
   s += uiXtoS( p9 ) + CRLF
   s += uiXtoS( p10 )

   MsgBox( s )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION PP_Debug( oXbp )
   LOCAL aPP := oXbp:setPresParam()
   LOCAL s := ''

   aeval( aPP, {|e_| s += ( hb_ntos( e_[ 1 ] ) +' '+ valtype( e_[ 2 ] ) +' '+ ;
        IF( valtype( e_[ 2 ] )=='N', hb_ntos( e_[ 2 ] ), ' ' ) + ';  '+ CRLF ) } )

   MsgBox( s )

   RETURN nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION RGB( r, g, b )
   RETURN GraMakeRGBColor( { b,g,r } )           /* a bug in Qt */

/*----------------------------------------------------------------------*/

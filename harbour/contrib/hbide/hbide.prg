/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               17Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "inkey.ch"
#include "gra.ch"
#include "set.ch"
#include "hbclass.ch"

#ifdef __HARBOUR__
   #define UNU( x ) HB_SYMBOL_UNUSED( x )
#else
   #define UNU( x ) ( x := x )
#endif

/*----------------------------------------------------------------------*/

#define TAB_1   1
#define TAB_2   2
#define TAB_3   3
#define TAB_4   4
#define TAB_5   5
#define TAB_6   6
#define TAB_7   7
#define TAB_8   8

#define CRLF    chr( 13 )+chr( 10 )

STATIC s_resPath

/*----------------------------------------------------------------------*/

PROCEDURE Main( cProjectOrSource )
   LOCAL oIde

   s_resPath := hb_DirBase() + "resources" + hb_OsPathSeparator()

   oIde := HbIde():new( cProjectOrSource )
   oIde:create()
   oIde:destroy()

   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE AppSys()
   RETURN

/*----------------------------------------------------------------------*/

PROCEDURE JustACall()
   RETURN

/*----------------------------------------------------------------------*/

CLASS HbIde

   DATA   mp1, mp2, oXbp, nEvent
   DATA   aTabs                                   INIT {}
   DATA   cProjFile

   DATA   oCurTab                                 INIT NIL
   DATA   nCurTab                                 INIT 0
   DATA   nPrevTab                                INIT 0

   /* HBQT Objects */
   DATA   qLeftArea
   DATA   qMidArea
   DATA   qRightArea
   DATA   qLeftLayout
   DATA   qMidLayout
   DATA   qRightLayout
   DATA   qLayout
   DATA   qSplitter
   DATA   qSplitterL
   DATA   qSplitterR

   /* XBP Objects */
   DATA   oDlg
   DATA   oDa
   DATA   oSBar
   DATA   oMenu
   DATA   oTBar
   DATA   oFont
   DATA   oProjTree
   DATA   oDockR
   DATA   oDockB
   DATA   oDockB1
   DATA   oDockB2
   DATA   oFuncList
   DATA   oOutputResult
   DATA   oCompileResult
   DATA   oLinkResult

   DATA   oProjRoot
   DATA   oExes
   DATA   oLibs
   DATA   oDlls
   DATA   aProjData                               INIT {}

   DATA   lDockRVisible                           INIT .t.
   DATA   lDockBVisible                           INIT .t.

   METHOD new( cProjectOrSource )
   METHOD create( cProjectOrSource )
   METHOD destroy()

   METHOD buildDialog()
   METHOD buildMenu()
   METHOD buildStatusBar()
   METHOD buildToolbar()
   METHOD executeAction()
   METHOD buildTabPage()
   METHOD buildProjectTree()

   METHOD buildFuncList()
   METHOD buildBottomArea()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()

   METHOD editSource()
   METHOD selectSource()
   METHOD closeSource()
   METHOD closeAllSources()
   METHOD saveSource()

   METHOD updateFuncList()
   METHOD fetchNewProject()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbIde:destroy()

   ::oSBar := NIL
   ::oMenu := NIL
   ::oTBar := NIL

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:new( cProjectOrSource )

   ::cProjFile := cProjectOrSource

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:create( cProjectOrSource )
   // LOCAL qWidget

   IF hb_isChar( cProjectOrSource )
      ::cProjFile := cProjectOrSource
   ENDIF

   ::BuildDialog()
   ::oDa := ::oDlg:drawingArea
   SetAppWindow( ::oDlg )
   ::oDlg:Show()

   ::oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
   ::oDa:oTabWidget:oWidget:setTabsClosable( .t. )

   ::buildProjectTree()
   ::buildFuncList()
   ::buildBottomArea()

#if 0
   ::qLeftLayout := QGridLayout():new()
   ::qLeftLayout:setContentsMargins( 0,0,0,0 )
   ::qLeftLayout:setHorizontalSpacing( 0 )
   ::qLeftLayout:setVerticalSpacing( 0 )

   ::qMidLayout  := QGridLayout():new()
   ::qMidLayout:setContentsMargins( 0,0,0,0 )
   ::qMidLayout:setHorizontalSpacing( 0 )
   ::qMidLayout:setVerticalSpacing( 0 )

   ::qLeftArea   := QWidget():new()
   ::qLeftArea:setLayout( QT_PTROF( ::qLeftLayout ) )
//   qWidget:oWidget:setLayout( QT_PTROF( ::qLeftLayout ) )
   ::qMidArea    := QWidget():new()
   ::qMidArea:setLayout( QT_PTROF( ::qMidLayout ) )

   ::qLeftLayout:addWidget_1( QT_PTROFXBP( ::oProjTree ), 0, 0, 1, 1 )
   ::qLeftLayout:addWidget_1( QT_PTROFXBP( qWidget ), 1, 0, 1, 1 )

   ::qMidLayout:addWidget_1( QT_PTROFXBP( ::oDa:oTabWidget ), 0, 0, 1, 1 )

   ::qSplitter := QSplitter():new( QT_PTROF( ::oDa:oWidget ) )

   ::qSplitter:addWidget( QT_PTROF( ::qLeftArea   ) )
   ::qSplitter:addWidget( QT_PTROF( ::qMidArea    ) )

   ::qSplitter:show()

#else
   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setHorizontalSpacing( 0 )
   ::qLayout:setVerticalSpacing( 0 )

   ::oDa:oWidget:setLayout( QT_PTROF( ::qLayout ) )

   ::qSplitter := QSplitter():new( QT_PTROF( ::oDa:oWidget ) )

   ::qLayout:addWidget_1( QT_PTROF( ::qSplitter ), 0, 0, 1, 1 )

   ::qSplitter:addWidget( QT_PTROFXBP( ::oProjTree      ) )
   ::qSplitter:addWidget( QT_PTROFXBP( ::oDa:oTabWidget ) )

   ::qSplitter:show()
#endif

   ::oDlg:setPos( { 100, 60 } )

   /* Editor's Font */
   ::oFont := XbpFont():new()
   ::oFont:fixed := .t.
   ::oFont:create( "10.Courier" )

   ::buildMenu()
   ::buildStatusBar()
   ::buildToolBar()

   ::editSource( ::cProjFile )
   ::updateFuncList()

   ::oDlg:Show()

   /* Enter Xbase++ Event Loop - working */
   DO WHILE .t.
      ::nEvent := AppEvent( @::mp1, @::mp2, @::oXbp )
      IF ::nEvent == xbeP_Quit
         EXIT
      ENDIF

      HBXBP_DEBUG( ::nEvent, ::mp1, ::mp2 )

      IF ( ::nEvent == xbeP_Close ) .OR. ( ::nEvent == xbeP_Keyboard .and. ::mp1 == xbeK_ESC )
         IF ::nEvent == xbeP_Close
            ::closeAllSources()
            EXIT
         ELSE
            ::closeSource( ::nCurTab, .t. )
            IF empty( ::aTabs )
               EXIT
            ENDIF
         ENDIF
      ENDIF

      ::oXbp:handleEvent( ::nEvent, ::mp1, ::mp2 )
   ENDDO

   /* Very important - destroy resources */
   ::oDlg:destroy()

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildTabPage( oWnd, cSource )
   LOCAL aPos    := { 5,5 }
   LOCAL aSize   := { 890, 420 }
   LOCAL oTab, o
   LOCAL cPath, cFile, cExt//, qIcon
   LOCAL nIndex  := len( ::aTabs )

   DEFAULT cSource TO "Untitled"

   hb_fNameSplit( cSource, @cPath, @cFile, @cExt )

   oTab := XbpTabPage():new( oWnd, , aPos, aSize, , .t. )
   oTab:caption    := cFile + cExt
   oTab:minimized  := .F.

   oTab:create()

   IF lower( cExt ) $ ".c;.cpp"
      ::oDa:oTabWidget:oWidget:setTabIcon( nIndex, s_resPath + "filec.png" )
   ELSE
      ::oDa:oTabWidget:oWidget:setTabIcon( nIndex, s_resPath + "fileprg.png" )
   ENDIF
   ::oDa:oTabWidget:oWidget:setTabTooltip( nIndex, cSource )

   oTab:tabActivate    := {|mp1,mp2,oXbp| HB_SYMBOL_UNUSED( mp1 ), o := oXbp, ;
                                mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == o } ), ::nCurTab := mp2 }
   oTab:closeRequested := {|mp1,mp2,oXbp| HB_SYMBOL_UNUSED( mp1 ), o := oXbp, ;
                                mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == o } ), ::nCurTab := mp2, ::closeSource( ::nCurTab, .t. ) }
   RETURN oTab

/*----------------------------------------------------------------------*/

METHOD HbIde:editSource( cSourceFile )
   LOCAL oTab, qEdit, qHiliter, qLayout

   DEFAULT cSourceFile TO ::cProjFile

   oTab := ::buildTabPage( ::oDa, cSourceFile )

   qEdit := QTextEdit():new( QT_PTROFXBP( oTab ) )
   qEdit:setLineWrapMode( QTextEdit_NoWrap )
   qEdit:setPlainText( memoread( cSourceFile ) )
   qEdit:setFont( QT_PTROFXBP( ::oFont ) )
   qEdit:setTextBackgroundColor( QT_PTROF( QColor():new( 255,255,255 ) ) )

   qLayout := QBoxLayout():new()
   qLayout:setDirection( 0 )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:addWidget( QT_PTROF( qEdit ) )

   oTab:oWidget:setLayout( QT_PTROF( qLayout ) )

   qHiliter := QSyntaxHighlighter():new( qEdit:document() )

   qEdit:show()

   aadd( ::aTabs, { oTab, qEdit, qHiliter, qLayout, cSourceFile } )

   ::nPrevTab := ::nCurTab
   ::nCurTab  := len( ::aTabs )
   ::oDa:oTabWidget:oWidget:setCurrentIndex( ::nCurTab - 1 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:saveSource( nTab )
   LOCAL cBuffer
   LOCAL qDocument := QTextDocument():configure( ::aTabs[ nTab, 2 ]:document() )

   IF qDocument:isModified()
      HBXBP_DEBUG( "Document be Saved", "YES", ::aTabs[ nTab, 5 ] )
      cBuffer := ::aTabs[ nTab, 2 ]:toPlainText()
      memowrit( ::aTabs[ nTab, 5 ], cBuffer )
   ELSE
      HBXBP_DEBUG( "Document Modified", "NO ", ::aTabs[ nTab, 5 ] )
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeSource( nTab, lDel )

   DEFAULT lDel TO .T.

   HBXBP_DEBUG( "  .  " )
   HBXBP_DEBUG( "HbIde:closeSource( nTab, lDel )", nTab, lDel )

   IF !empty( ::aTabs ) .and. !empty( nTab )
      ::saveSource( nTab )

      /* Destroy at Qt level */
      ::oDa:oTabWidget:oWidget:removeTab( nTab - 1 )

      /* Destroy at XBP level */
      ::aTabs[ nTab,1 ]:destroy()

      /* Destroy at this object level */
      IF lDel
         adel( ::aTabs, nTab )
         asize( ::aTabs, len( ::aTabs ) - 1 )
      ENDIF

      IF empty( ::aTabs )
         PostAppEvent( xbeP_Quit )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeAllSources()
   LOCAL nTab

   FOR nTab := len( ::aTabs ) TO 1 STEP -1
      ::closeSource( nTab, .f. )
   NEXT
   ::aTabs := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:selectSource( cMode )
   LOCAL oDlg, cFile//, aFiles

   oDlg := XbpFileDialog():new():create( ::oDa, , { 10,10 } )
   IF cMode == "open"
      oDlg:title       := "Select a Source File"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "PRG Sources", "*.prg" }, { "C Sources", "*.c" }, { "CPP Sources", "*.cpp" }, ;
                                                            { "H Headers", "*.h" }, { "CH Headers", "*.ch" } }

      cFile := oDlg:open( CurDrive() +":\"+ CurDir(), , .f. )
   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      cFile := oDlg:saveAs( "c:\temp\myfile.dbf" )
      IF !empty( cFile )
         HBXBP_DEBUG( cFile )
      ENDIF
   ENDIF

   RETURN cFile

/*----------------------------------------------------------------------*/

METHOD HbIde:buildProjectTree()
   LOCAL aExe, aExeD, i, j, aPrjs

   ::oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDa, , { 0,0 }, { 10,10 }, , .t. )
   ::oProjTree:setColorBG( GraMakeRGBColor( { 223,240,255 } ) )

   ::oProjTree:oWidget:setMaximumWidth( 200 )

   ::oProjRoot := ::oProjTree:rootItem:addItem( "Projects" )

   aadd( ::aProjData, { ::oProjRoot:addItem( "Executables" ), "Executables", NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Libs" )       , "Libs"       , NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Dlls" )       , "Dlls"       , NIL, NIL } )

   ::oProjRoot:expand( .t. )

   /* Just a prototype : to be filled with project data */

   /* Executables */
   aExeD := {}
   aExe  := ::aProjData[ 1 ]
   aPrjs := { "Vouch", "CacheMGR" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 1,3 ] := aExeD
   ::aProjData[ 1,1 ]:expand( .t. )

   /* Libs */
   aExeD := {}
   aExe  := ::aProjData[ 2 ]
   aPrjs := { "V32Lib", "Vouch32", "CacheRDD" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 2,3 ] := aExeD
   ::aProjData[ 2,1 ]:expand( .t. )

   /* Libs */
   aExeD := {}
   aExe  := ::aProjData[ 3 ]
   aPrjs := { "VouchActiveX" }
   FOR i := 1 TO len( aPrjs )
      aadd( aExeD, { aExe[ 1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
   NEXT
   ::aProjData[ 3,3 ] := aExeD
   ::aProjData[ 3,1 ]:expand( .t. )
#if 1
   /* Next classification by type of source */
   aExe  := ::aProjData[ 1,3 ]
   aPrjs := { "PRG Sources", "C Sources", "CPP Sources", "CH Headers", "H Headers" }
   FOR j := 1 TO len( aExe )
      aExeD := {}
      FOR i := 1 TO len( aPrjs )
         aadd( aExeD, { aExe[ j,1 ]:addItem( aPrjs[ i ] ), aPrjs[ i ], NIL, NIL } )
      NEXT
      aExe[ j,3 ] := aExeD
      aExe[ j,1 ]:expand( .t. )
   NEXT
#endif

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:executeAction( cKey )
   LOCAL cFile

   DO CASE
   CASE cKey == "3"
      IF !empty( cFile := ::selectSource( "open" ) )
         ::oProjRoot:addItem( cFile )
         ::editSource( cFile )
      ENDIF

   CASE cKey == "4"
      ::saveSource( ::nCurTab )

   CASE cKey == "5"
      ::closeSource( ::nCurTab, .t. )

   CASE cKey == "11"
      IF ::lDockBVisible
         ::oDockB:hide()
         ::oDockB1:hide()
         ::oDockB2:hide()
      ELSE
         ::oDockB:show()
         ::oDockB1:show()
         ::oDockB2:show()
      ENDIF
      ::lDockBVisible := !( ::lDockBVisible )

   CASE cKey == "12"
      IF ::lDockRVisible
         ::oDockR:hide()
      ELSE
         ::oDockR:show()
      ENDIF
      ::lDockRVisible := !( ::lDockRVisible )

   CASE cKey == "NewProject"
      ::fetchNewProject()

   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbIde:buildToolBar()

   ::oTBar := XbpToolBar():new( ::oDA )
   ::oTBar:create( , , { 0, ::oDa:currentSize()[ 2 ]-60 }, ;
                                 { ::oDa:currentSize()[ 1 ], 60 } )

   ::oTBar:imageWidth  := 20
   ::oTBar:imageHeight := 20

   ::oTBar:oWidget:setMaximumHeight( 30 )

   ::oTBar:addItem( "Exit"                       , s_resPath + "exit.png"           , , , , , "1"  )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Properties"                 , s_resPath + "properties.png"     , , , , , "2"  )
   ::oTBar:addItem( "Open"                       , s_resPath + "open.png"           , , , , , "3"  )
   ::oTBar:addItem( "Save"                       , s_resPath + "save.png"           , , , , , "4"  )
   ::oTBar:addItem( "Close"                      , s_resPath + "close.png"          , , , , , "5"  )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Compile"                    , s_resPath + "compile.png"        , , , , , "5"  )
   ::oTBar:addItem( "Compile to PPO"             , s_resPath + "ppo.png"            , , , , , "6"  )
   ::oTBar:addItem( "Build Project"              , s_resPath + "build.png"          , , , , , "7"  )
   ::oTBar:addItem( "Build and Launch Project"   , s_resPath + "buildlaunch.png"    , , , , , "8"  )
   ::oTBar:addItem( "Rebuild Project"            , s_resPath + "rebuild.png"        , , , , , "9"  )
   ::oTBar:addItem( "Rebuild and Launch Project" , s_resPath + "rebuildlaunch.png"  , , , , , "10" )
   ::oTBar:addItem( "Show/Hide Build Error Info" , s_resPath + "builderror.png"     , , , , , "11" )
   ::oTBar:addItem( "Module Function List"       , s_resPath + "modulelist.png"     , , , , , "12" )
   //
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Undo"                       , s_resPath + "undo.png"           , , , , , "13" )
   ::oTBar:addItem( "Redo"                       , s_resPath + "redo.png"           , , , , , "14" )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Cut"                        , s_resPath + "cut.png"            , , , , , "15" )
   ::oTBar:addItem( "Copy"                       , s_resPath + "copy.png"           , , , , , "16" )
   ::oTBar:addItem( "Paste"                      , s_resPath + "paste.png"          , , , , , "17" )
   ::oTBar:addItem( "Select All"                 , s_resPath + "selectall.png"      , , , , , "18" )
   ::oTBar:addItem( "Column/Stream Selection"    , s_resPath + "stream.png"         , , , , , "19" )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Find"                       , s_resPath + "find.png"           , , , , , "20" )
   ::oTBar:addItem( "Search"                     , s_resPath + "search.png"         , , , , , "21" )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "Place/Remove Mark"          , s_resPath + "placeremovemark.png", , , , , "22" )
   ::oTBar:addItem( "Goto Mark"                  , s_resPath + "gotomark.png"       , , , , , "23" )
   ::oTBar:addItem( "Goto Line"                  , s_resPath + "gotoline.png"       , , , , , "24" )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   ::oTBar:addItem( "To Upper"                   , s_resPath + "toupper.png"        , , , , , "25" )
   ::oTBar:addItem( "To Lower"                   , s_resPath + "tolower.png"        , , , , , "26" )
   ::oTBar:addItem( "Invert Case"                , s_resPath + "invertcase.png"     , , , , , "27" )
   ::oTBar:addItem( "Match Pairs"                , s_resPath + "matchobj.png"       , , , , , "28" )
   ::oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )

   ::oTBar:transparentColor := GraMakeRGBColor( { 0,255,255 } ) // GRA_CLR_INVALID
   ::oTBar:buttonClick := {|oButton| ::executeAction( oButton:key ) }

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbIde:buildMenu()
   LOCAL oMenuBar, oSubMenu

   oMenuBar := SetAppWindow():MenuBar()

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~File"
   oSubMenu:addItem( { "Open"                         , {|| ::executeAction( "Open"               ) } } )
   oSubMenu:addItem( { "Save"                         , {|| ::executeAction( "Save"               ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Exit"                         , {|| ::executeAction( "Exit"               ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Project"
   oSubMenu:addItem( { "New"                          , {|| ::executeAction( "NewProject"         ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Build"               , {|| ::executeAction( "SaveBuild"          ) } } )
   oSubMenu:addItem( { "Save, Build and Launch"       , {|| ::executeAction( "SaveBuildLaunch"    ) } } )
   oSubMenu:addItem( { "Save and Re-build"            , {|| ::executeAction( "SaveRebuild"        ) } } )
   oSubMenu:addItem( { "Save, Re-build and Launch"    , {|| ::executeAction( "SaveRebuildLaunch"  ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Compile Current File", {|| ::executeAction( "SaveCompileCurrent" ) } } )
   oSubMenu:addItem( { "Save and Create PPO Output"   , {|| ::executeAction( "SavePPO"            ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Project Properties"           , {|| ::executeAction( "Properties"         ) } } )

   oMenuBar:addItem( { oSubMenu, NIL } )

   Return Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildStatusBar()
   LOCAL oPanel

   ::oSBar := XbpStatusBar():new()
   ::oSBar:create( ::oDlg, , { 0,0 }, { ::oDlg:currentSize()[1],30 } )

   oPanel := ::oSBar:getItem( 1 )
   oPanel:caption  := "Ready"
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildDialog()

   ::oDlg := XbpDialog():new( , , {10,10}, {1100,700}, , .f. )

   ::oDlg:icon := s_resPath + "vr.png" //"hbide.ico"
   ::oDlg:title := "Harbour-Qt IDE"

   ::oDlg:create()

   ::oDlg:close := {|| MsgBox( "You can also close me by pressing [ESC]" ), .T. }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:updateFuncList()

   ::oFuncList:addItem( "Procedure Main"   )
   ::oFuncList:addItem( "Class HBIDE"      )
   ::oFuncList:addItem( "Method HbIde:new" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildFuncList()

   ::oDockR := XbpWindow():new( ::oDa )
   ::oDockR:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockR )
   ::oDockR:oWidget:setFeatures( QDockWidget_DockWidgetClosable + QDockWidget_DockWidgetMovable )
   ::oDockR:oWidget:setAllowedAreas( Qt_RightDockWidgetArea )
   ::oDockR:oWidget:setWindowTitle( "Module Function List" )
   ::oDockR:oWidget:setMinimumWidth( 100 )
   ::oDockR:oWidget:setMaximumWidth( 150 )

   ::oFuncList := XbpListBox():new( ::oDockR ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:setStyleSheet( GetStyleSheet( "QListView" ) )

   ::oDockR:oWidget:setWidget( QT_PTROFXBP( ::oFuncList ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, QT_PTROFXBP( ::oDockR ), Qt_Horizontal )
   //::oDockR:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildBottomArea()

   ::buildCompileResults()
   ::buildLinkResults()
   ::buildOutputResults()

   ::oDlg:oWidget:tabifyDockWidget( QT_PTROFXBP( ::oDockB ), QT_PTROFXBP( ::oDockB1 ) )
   ::oDlg:oWidget:tabifyDockWidget( QT_PTROFXBP( ::oDockB1 ), QT_PTROFXBP( ::oDockB2 ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildCompileResults()

   ::oDockB := XbpWindow():new( ::oDa )
   ::oDockB:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB )
   ::oDockB:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB:oWidget:setWindowTitle( "Compile Results" )
   ::oDockB:oWidget:setMinimumHeight(  75 )
   ::oDockB:oWidget:setMaximumHeight( 100 )

   ::oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( QT_PTROFXBP( ::oCompileResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB ), Qt_Vertical )
   //::oDockB:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildLinkResults()

   ::oDockB1 := XbpWindow():new( ::oDa )
   ::oDockB1:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB1 )
   ::oDockB1:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB1:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB1:oWidget:setWindowTitle( "Link Results" )
   ::oDockB1:oWidget:setMinimumHeight(  75 )
   ::oDockB1:oWidget:setMaximumHeight( 100 )

   ::oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB1:oWidget:setWidget( QT_PTROFXBP( ::oLinkResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB1 ), Qt_Vertical )
   //::oDockB1:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildOutputResults()

   ::oDockB2 := XbpWindow():new( ::oDa )
   ::oDockB2:oWidget := QDockWidget():new( QT_PTROFXBP( ::oDlg ) )
   ::oDlg:addChild( ::oDockB2 )
   ::oDockB2:oWidget:setFeatures( QDockWidget_DockWidgetClosable )
   ::oDockB2:oWidget:setAllowedAreas( Qt_BottomDockWidgetArea )
   ::oDockB2:oWidget:setWindowTitle( "Output Console" )
   ::oDockB2:oWidget:setMinimumHeight(  75 )
   ::oDockB2:oWidget:setMaximumHeight( 100 )

   ::oOutputResult := XbpMLE():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB2:oWidget:setWidget( QT_PTROFXBP( ::oOutputResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB2 ), Qt_Vertical )
   //::oDockB2:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:fetchNewProject()

   RETURN self

/*----------------------------------------------------------------------*/

STATIC FUNCTION GetStyleSheet( cWidget )
   LOCAL s, txt_:={}

   DO CASE
   CASE cWidget == "QListView"
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView {                                                                                 ' )
      aadd( txt_, '     alternate-background-color: yellow;                                                     ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView {                                                                                 ' )
      aadd( txt_, '     show-decoration-selected: 1; /* make the selection span the entire width of the view */ ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView::item:alternate {                                                                 ' )
      aadd( txt_, '     background: #EEEEEE;                                                                    ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView::item:selected {                                                                  ' )
      aadd( txt_, '     border: 1px solid #6a6ea9;                                                              ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView::item:selected:!active {                                                          ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
      aadd( txt_, '                                 stop: 0 #ABAFE5, stop: 1 #8588B2);                          ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView::item:selected:active {                                                           ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
      aadd( txt_, '                                 stop: 0 #6a6ea9, stop: 1 #888dd9);                          ' )
      aadd( txt_, ' }                                                                                           ' )
      aadd( txt_, '                                                                                             ' )
      aadd( txt_, ' QListView::item:hover {                                                                     ' )
      aadd( txt_, '     background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,                                 ' )
      aadd( txt_, '                                 stop: 0 #FAFBFE, stop: 1 #DCDEF1);                          ' )
      aadd( txt_, '}                                                                                            ' )
      aadd( txt_, '                                                                                             ' )

   ENDCASE

   s := ""
   aeval( txt_, {|e| s += e + chr( 13 )+chr( 10 ) } )

   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION MenuAddSep( oMenu )

   oMenu:addItem( { NIL, NIL, XBPMENUBAR_MIS_SEPARATOR, NIL } )

   RETURN nil

/*----------------------------------------------------------------------*/

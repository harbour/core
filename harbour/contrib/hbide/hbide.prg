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

#include "hbide.ch"

#include "common.ch"
#include "xbp.ch"
#include "appevent.ch"
#include "inkey.ch"
#include "gra.ch"
#include "set.ch"
#include "hbclass.ch"

#define UNU( x ) HB_SYMBOL_UNUSED( x )

/*----------------------------------------------------------------------*/

STATIC s_resPath
STATIC s_pathSep

/*----------------------------------------------------------------------*/

PROCEDURE Main( cProjIni )
   LOCAL oIde

   s_resPath := hb_DirBase() + "resources" + hb_OsPathSeparator()
   s_pathSep := hb_OsPathSeparator()

   oIde := HbIde():new( cProjIni )
   oIde:create()
   oIde:destroy()

   RETURN

/*----------------------------------------------------------------------*/

CLASS HbIde

   DATA   mp1, mp2, oXbp, nEvent
   DATA   aTabs                                   INIT {}
   DATA   cProjIni

   DATA   oCurTab                                 INIT NIL
   DATA   nCurTab                                 INIT 0
   DATA   aIni                                    INIT {}

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
   DATA   qTabWidget
   DATA   qFindDlg
   DATA   qSBLine
   DATA   qSBCol

   ACCESS qCurEdit                                INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), 2 ], NIL )
   ACCESS qCurDocument                            INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), 6 ], NIL )
   ACCESS qCurCursor                              INLINE ::getCurCursor()
   DATA   qCursor

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
   DATA   oNewDlg
   DATA   oTabWidget
   DATA   oPBFind, oPBRepl, oPBClose, oFind, oRepl

   DATA   oCurProjItem
   DATA   oCurProject

   DATA   oProjRoot
   DATA   oExes
   DATA   oLibs
   DATA   oDlls
   DATA   aProjData                               INIT {}
   DATA   aPrpObjs                                INIT {}
   DATA   aEditorPath                             INIT {}

   DATA   lProjTreeVisible                        INIT .t.
   DATA   lDockRVisible                           INIT .f.
   DATA   lDockBVisible                           INIT .f.
   DATA   lTabCloseRequested                      INIT .f.

   DATA   cSaveTo                                 INIT ""
   DATA   oOpenedSources

   METHOD new( cProjectOrSource )
   METHOD create( cProjectOrSource )
   METHOD destroy()

   METHOD loadConfig()
   METHOD saveConfig()
   METHOD setPosAndSizeByIni()

   METHOD buildDialog()
   METHOD buildStatusBar()
   METHOD executeAction()
   METHOD buildTabPage()
   METHOD buildProjectTree()

   METHOD manageFuncContext()
   METHOD manageProjectContext()

   METHOD buildFuncList()
   METHOD buildBottomArea()
   METHOD buildCompileResults()
   METHOD buildLinkResults()
   METHOD buildOutputResults()

   METHOD loadSources()
   METHOD editSource()
   METHOD selectSource()
   METHOD closeSource()
   METHOD closeAllSources()
   METHOD saveSource()

   METHOD updateFuncList()
   METHOD gotoFunction()
   METHOD fetchProjectProperties()
   METHOD loadProjectProperties()
   METHOD appendProjectInTree()
   METHOD manageItemSelected()
   METHOD addSourceInTree()
   METHOD closeTab()
   METHOD activateTab()
   METHOD getCurrentTab()
   METHOD dispEditInfo()
   METHOD getCurCursor()

   DATA   aTags                                   INIT {}
   DATA   aText                                   INIT {}
   DATA   aSources                                INIT {}
   DATA   aFuncList                               INIT {}
   DATA   aLines                                  INIT {}
   DATA   aComments                               INIT {}
   DATA   aPrjProps                               INIT {}
   DATA   aProjects                               INIT {}

   METHOD createTags()
   METHOD findReplace()
   METHOD manageFocusInEditor()
   METHOD convertSelection()
   METHOD printPreview()
   METHOD paintRequested()
   METHOD setTabImage()
   METHOD loadUI()
   METHOD updateHbp()
   METHOD saveProject()
   METHOD addSourcesToProject()


   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD HbIde:destroy()

   ::oSBar := NIL
   ::oMenu := NIL
   ::oTBar := NIL

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:new( cProjIni )

   ::cProjIni := cProjIni

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:create( cProjIni )
   // LOCAL qWidget

   ::loadConfig( cProjIni )

   ::BuildDialog()
   ::oDa := ::oDlg:drawingArea
   SetAppWindow( ::oDlg )
   ::oDlg:Show()

   ::oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
   //   ::oDa:oTabWidget:oWidget:setTabsClosable( .t. )
   ::oDa:oTabWidget:oWidget:setUsesScrollButtons( .f. )
   ::oTabWidget := ::oDa:oTabWidget
   ::qTabWidget := ::oDa:oTabWidget:oWidget

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

   ::qCursor := QTextCursor():new()

   /* Editor's Font */
   ::oFont := XbpFont():new()
   ::oFont:fixed := .t.
   ::oFont:create( "10.Courier" )

   buildMainMenu( ::oDlg, Self )
   ::oTBar := buildToolBar( ::oDlg, Self )

   ::buildStatusBar()

   ::setPosAndSizeByIni( ::oProjTree:oWidget, ProjectTreeGeometry )

   ::oDlg:Show()

   ::loadSources()

   /* Enter Xbase++ Event Loop - working */
   DO WHILE .t.
      ::nEvent := AppEvent( @::mp1, @::mp2, @::oXbp )

      IF ::nEvent == xbeP_Quit
         HBXBP_DEBUG( "xbeP_Quit" )
         EXIT
      ENDIF

      // HBXBP_DEBUG( ::nEvent, ::mp1, ::mp2 )

      IF ::nEvent == xbeP_Close
         ::saveConfig()
         ::closeAllSources()
         EXIT

      ELSEIF ::nEvent == xbeP_Keyboard
         DO CASE

         CASE ::mp1 == xbeK_INS
            IF !empty( ::qCurEdit )
               ::qCurEdit:setOverwriteMode( ! ::qCurEdit:overwriteMode() )
               ::dispEditInfo()
            ENDIF

         CASE ::mp1 == xbeK_ESC
            ::closeSource()

         CASE ::mp1 == xbeK_CTRL_S
            ::saveSource( ::getCurrentTab(), .f. )

         CASE ::mp1 == xbeK_CTRL_F
            IF !empty( ::qCurEdit )
               ::findReplace( "finddialog" )
            ENDIF

         ENDCASE
      ENDIF

      ::oXbp:handleEvent( ::nEvent, ::mp1, ::mp2 )
   ENDDO

   HBXBP_DEBUG( "EXITING.................." )

   /* Very important - destroy resources */
   ::oDlg:destroy()

   HBXBP_DEBUG( "EXITING after destroy .................." )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:saveConfig()
   LOCAL nTab, pTab, n, txt_, qEdit, qHScr, qVScr
   LOCAL nTabs := ::qTabWidget:count()

   txt_:= {}
   //    Properties
   aadd( txt_, "[HBIDE]" )
   aadd( txt_, "MainWindowGeometry     = " + PosAndSize( ::oDlg:oWidget )           )
   aadd( txt_, "ProjectTreeVisible     = " + IIF( ::lProjTreeVisible, "YES", "NO" ) )
   aadd( txt_, "ProjectTreeGeometry    = " + PosAndSize( ::oProjTree:oWidget )      )
   aadd( txt_, "FunctionListVisible    = " + IIF( ::lDockRVisible, "YES", "NO" )    )
   aadd( txt_, "FunctionListGeometry   = " + PosAndSize( ::oFuncList:oWidget )      )
   aadd( txt_, "RecentTabIndex         = " + hb_ntos( ::qTabWidget:currentIndex() ) )
   aadd( txt_, "CurrentProject         = " + ""                                     )
   aadd( txt_, " " )

   //    Projects
   aadd( txt_, "[PROJECTS]" )
   FOR n := 1 TO len( ::aProjects )
      aadd( txt_, ::aProjects[ n, 2 ] )
   NEXT
   aadd( txt_, " " )

   //    Files
   aadd( txt_, "[FILES]" )
   FOR n := 1 TO nTabs
      pTab      := ::qTabWidget:widget( n-1 )
      nTab      := ascan( ::aTabs, {|e_| HBQT_QTPTR_FROM_GCPOINTER( QT_PTROFXBP( e_[ 1 ] ) ) == pTab } )
      qEdit     := ::aTabs[ nTab, 2 ]
      qHScr     := QScrollBar():configure( qEdit:horizontalScrollBar() )
      qVScr     := QScrollBar():configure( qEdit:verticalScrollBar() )
      ::qCursor := QTextCursor():configure( qEdit:textCursor() )

      aadd( txt_, ::aTabs[ nTab, 5 ] +","+ ;
                  hb_ntos( ::qCursor:position() ) +","+ ;
                  hb_ntos( qHScr:value() ) + "," + ;
                  hb_ntos( qVScr:value() ) + ","   ;
           )
   NEXT
   aadd( txt_, " " )

   RETURN CreateTarget( ::cProjIni, txt_ )

/*----------------------------------------------------------------------*/

METHOD HbIde:loadConfig( cHbideIni )
   LOCAL aElem, s, n, nPart, cKey, cVal, a_
   LOCAL aIdeEle := { "mainwindowgeometry", "projecttreevisible", "projecttreegeometry", "functionlistvisible", ;
                      "functionlistgeometry", "recenttabindex", "currentproject" }

   DEFAULT cHbideIni TO "hbide.ini"

   cHbideIni := lower( cHbideIni )

   IF !file( cHbideIni )
      cHbideIni := hb_dirBase() + "hbide.ini"
   ENDIF

   IF !file( cHbideIni )
      cHbideIni := hb_dirBase() + "hbide.ini"
   ENDIF

   ::cProjIni := cHbideIni

   ::aIni := { afill( array( INI_HBIDE_VRBLS ), "" ), {}, {} }

   IF file( ::cProjIni )
      #if 0
      [HBIDE]
      MainWindowGeometry   =
      ProjectTreeVisible   =
      ProjectTreeGeometry  =
      FunctionListVisible  =
      FunctionListGeometry =
      RecentTabIndex       =
      CurrentProject       =
      [PROJECTS]
      c:\harbour\contrib\hbide\projects\vouch.hbi
      ...
      [FILES]
      c:\dev_sources\vouch\source\vouch.prg : scrollpos horz : scrollpos vert : CursorPos :
      ...
      #endif

      aElem := ReadSource( ::cProjIni )

      FOR EACH s IN aElem
         s := alltrim( s )
         IF !empty( s )
            DO CASE
            CASE s == "[HBIDE]"
               nPart := INI_HBIDE
            CASE s == "[PROJECTS]"
               nPart := INI_PROJECTS
            CASE s == "[FILES]"
               nPart := INI_FILES
            OTHERWISE
               DO CASE
               CASE nPart == INI_HBIDE
                  IF ( n := at( "=", s ) ) > 0
                     cKey := alltrim( substr( s, 1, n-1 ) )
                     cVal := alltrim( substr( s, n+1 ) )
                     cKey := lower( cKey )
                     IF ( n := ascan( aIdeEle, cKey ) ) > 0
                        ::aIni[ nPart, n ] := cVal  /* Further process */
                     ENDIF
                  ENDIF

               CASE nPart == INI_PROJECTS
                  aadd( ::aIni[ nPart ], s )
                  ::loadProjectProperties( s, .f., .f. )

               CASE nPart == INI_FILES
                  a_:= hb_atokens( s, "," )
                  asize( a_, 4 )
                  DEFAULT a_[ 1 ] TO ""
                  DEFAULT a_[ 2 ] TO ""
                  DEFAULT a_[ 3 ] TO ""
                  DEFAULT a_[ 4 ] TO ""
                  //
                  a_[ 2 ] := val( a_[ 2 ] )
                  a_[ 3 ] := val( a_[ 3 ] )
                  a_[ 4 ] := val( a_[ 4 ] )
                  aadd( ::aIni[ nPart ], a_ )

               ENDCASE
            ENDCASE
         ENDIF
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:convertSelection( cKey )
   LOCAL cBuffer, i, s, nLen, c
   LOCAL nB, nL

   IF !empty( ::qCurEdit )
      ::qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
      IF ::qCursor:hasSelection() .and. !empty( cBuffer := ::qCursor:selectedText() )
         DO CASE
         CASE cKey == "ToUpper"
            cBuffer := upper( cBuffer )
         CASE cKey == "ToLower"
            cBuffer := lower( cBuffer )
         CASE cKey == "Invert"
            s := ""
            nLen := len( cBuffer )
            FOR i := 1 TO nLen
               c := substr( cBuffer, i, 1 )
               s += IIF( isUpper( c ), lower( c ), upper( c ) )
            NEXT
            cBuffer := s
         ENDCASE
         nL := len( cBuffer )
         nB := ::qCursor:position() - nL

         ::qCursor:beginEditBlock()
         ::qCursor:removeSelectedText()
         ::qCursor:insertText( cBuffer )
         ::qCursor:setPosition( nB )
         ::qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, nL )
         ::qCurEdit:setTextCursor( QT_PTROF( ::qCursor ) )
         ::qCursor:endEditBlock()
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:setPosAndSizeByIni( qWidget, nPart )
   LOCAL aRect

   IF !empty( ::aIni[ INI_HBIDE, nPart ] )
      aRect := hb_atokens( ::aIni[ INI_HBIDE, nPart ], "," )
      aeval( aRect, {|e,i| aRect[ i ] := val( e ) } )

      qWidget:move( aRect[ 1 ], aRect[ 2 ] )
      qWidget:resize( aRect[ 3 ], aRect[ 4 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageFocusInEditor()

   IF ::getCurrentTab() > 0
      ::aTabs[ ::getCurrentTab(), 2 ]:setFocus()
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:getCurCursor()
   LOCAL iTab

   IF ( iTab := ::getCurrentTab() ) > 0
      ::qCursor:configure( ::aTabs[ iTab, 1 ]:textCutsor() )
   ENDIF

   RETURN ::qCursor

/*----------------------------------------------------------------------*/
//                          Source Editor
/*----------------------------------------------------------------------*/

METHOD HbIde:editSource( cSourceFile, nPos, nHPos, nVPos )
   LOCAL oTab, qEdit, qHiliter, qLayout, qDocument, qHScr, qVScr
   LOCAL lFirst := .t.

   IF !( IsValidSource( cSourceFile ) )
      RETURN Self
   ENDIF

   DEFAULT cSourceFile TO ::cProjIni
   DEFAULT nPos        TO 0
   DEFAULT nHPos       TO 0
   DEFAULT nVPos       TO 0

   oTab := ::buildTabPage( ::oDa, cSourceFile )

   qEdit := QPlainTextEdit():new( QT_PTROFXBP( oTab ) )
   qEdit:setPlainText( memoread( cSourceFile ) )
   qEdit:setLineWrapMode( QTextEdit_NoWrap )
   qEdit:setFont( QT_PTROFXBP( ::oFont ) )
   qEdit:ensureCursorVisible()

   qDocument := QTextDocument():configure( qEdit:document() )

   qLayout := QBoxLayout():new()
   qLayout:setDirection( 0 )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:addWidget( QT_PTROF( qEdit ) )

   oTab:oWidget:setLayout( QT_PTROF( qLayout ) )

   qHiliter := QSyntaxHighlighter():new( qEdit:document() )

   qEdit:show()

   ::qCursor := QTextCursor():configure( qEdit:textCursor() )
   ::qCursor:setPosition( nPos )
   //
   qHScr   := QScrollBar():configure( qEdit:horizontalScrollBar() )
   qHScr:setValue( nHPos )
   //
   qVScr   := QScrollBar():configure( qEdit:verticalScrollBar() )
   qVScr:setValue( nVPos )

   aadd( ::aTabs, { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument } )

   ::nCurTab  := len( ::aTabs )

   ::aSources := { cSourceFile }
   ::createTags()
   ::updateFuncList()
   ::addSourceInTree( cSourceFile )
   ::manageFocusInEditor()
   ::dispEditInfo()

   Qt_Connect_Signal( QT_PTROF( qEdit ), "textChanged()", ;
                          {|| ::setTabImage( oTab, qEdit, nPos, @lFirst, qDocument ) } )

   Qt_Connect_Signal( QT_PTROF( qEdit ), "cursorPositionChanged()", {|| ::dispEditInfo() } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:buildTabPage( oWnd, cSource )
   LOCAL oTab, cPath, cFile, cExt

   hb_fNameSplit( cSource, @cPath, @cFile, @cExt )

   oTab := XbpTabPage():new( oWnd, , { 5,5 }, { 700,400 }, , .t. )
   oTab:caption   := cFile + cExt
   oTab:minimized := .F.

   oTab:create()

   ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) ) )
   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) ), cSource )

   oTab:tabActivate    := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }
   oTab:closeRequested := {|mp1,mp2,oXbp| ::closeTab( mp1, mp2, oXbp ) }

   RETURN oTab

/*----------------------------------------------------------------------*/

METHOD HbIde:setTabImage( oTab, qEdit, nPos, lFirst, qDocument )
   LOCAL nIndex    := ::qTabWidget:indexOf( QT_PTROFXBP( oTab ) )
   LOCAL lModified := qDocument:isModified()

   ::qTabWidget:setTabIcon( nIndex, s_resPath + iif( lModified, "tabmodified.png", "tabunmodified.png" ) )

   ::oSBar:getItem( 7 ):caption := IIF( lModified, "Modified", " " )

   IF lFirst
      lFirst := .f.
      ::qCursor:configure( qEdit:textCursor() )
      ::qCursor:setPosition( nPos, QTextCursor_MoveAnchor )
      qEdit:setTextCursor( QT_PTROF( ::qCursor ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:loadSources()
   LOCAL i

   IF !empty( ::aIni[ INI_FILES ] )
      FOR i := 1 TO len( ::aIni[ INI_FILES ] )
         ::editSource( ::aIni[ INI_FILES, i, 1 ], ::aIni[ INI_FILES, i, 2 ], ::aIni[ INI_FILES, i, 3 ], ::aIni[ INI_FILES, i, 4 ] )
      NEXT
      ::qTabWidget:setCurrentIndex( val( ::aIni[ INI_HBIDE, RecentTabIndex ] ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeSource( nTab )
   LOCAL n, cSource

   DEFAULT nTab TO ::getCurrentTab()

   IF !empty( nTab  )
      ::oFuncList:clear()
      ::saveSource( nTab )
      ::qTabWidget:removeTab( ::qTabWidget:indexOf( QT_PTROFXBP( ::aTabs[ nTab,1 ] ) ) )

      cSource := ::aTabs[ nTab, 5 ]

      /* Destroy all objects */
      // { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument }
      //
      Qt_DisConnect_Signal( QT_PTROF( ::aTabs[ nTab, 2 ] ), "textChanged()" )
      Qt_DisConnect_Signal( QT_PTROF( ::aTabs[ nTab, 2 ] ), "cursorPositionChanged()" )

      ::aTabs[ nTab, 6 ]:pPtr := 0
      ::aTabs[ nTab, 4 ]:pPtr := 0
      ::aTabs[ nTab, 3 ]:pPtr := 0
      ::aTabs[ nTab, 2 ]:pPtr := 0

      //::aTabs[ nTab, 1 ] := NIL
      ::aTabs[ nTab, 5 ] := ""

      IF ( n := ascan( ::aProjData, {|e_| e_[ 4 ] == cSource } ) ) > 0
         ::aProjData[ n,3 ]:delItem( ::aProjData[ n,1 ] )
         adel( ::aProjData, n )
         asize( ::aProjData, len( ::aProjData )-1 )
      ENDIF
   ENDIF

   IF ::qTabWidget:count() == 0
      ::oDockR:hide()
      ::lDockRVisible := .f.
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeAllSources()
   LOCAL i, nTabs := ::qTabWidget:count()

   FOR i := 1 TO nTabs
      ::closeSource()
   NEXT
   ::aTabs := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:saveSource( nTab, lConfirm )
   LOCAL cBuffer, qDocument, nIndex
   LOCAL lSave := .t.

   DEFAULT lConfirm TO .t.

   IF nTab > 0
      qDocument := ::aTabs[ nTab, 6 ]

      IF qDocument:isModified()

         IF lConfirm .and. !GetYesNo( ::aTabs[ nTab, 5 ],  "Has been modified, save this source ?" )
            lSave := .f.
         ENDIF

         IF lSave
            cBuffer := ::aTabs[ nTab, 2 ]:toPlainText()
            hb_memowrit( ::aTabs[ nTab, 5 ], cBuffer )
            qDocument:setModified( .f. )
            ::createTags()
            ::updateFuncList()
         ENDIF
      ENDIF

      nIndex := ::qTabWidget:indexOf( QT_PTROFXBP( ::aTabs[ nTab, 1 ] ) )
      ::qTabWidget:setTabIcon( nIndex, s_resPath + "tabunmodified.png" )
      ::oSBar:getItem( 7 ):caption := " "
   ENDIF

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
      cFile := oDlg:open( , , .f. )
   ELSEIF cMode == "openmany"
      oDlg:title       := "Select Sources"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "All Files"  , "*.*"   }, { "PRG Sources", "*.prg" }, { "C Sources" , "*.c"  },;
                            { "CPP Sources", "*.cpp" }, { "H Headers"  , "*.h"   }, { "CH Headers", "*.ch" } }
      cFile := oDlg:open( , , .t. )
   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      cFile := oDlg:saveAs( "myfile.dbf" )
      IF !empty( cFile )
         HBXBP_DEBUG( cFile )
      ENDIF
   ENDIF

   RETURN cFile

/*----------------------------------------------------------------------*/

METHOD HbIde:closeTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == oXbp } ) ) > 0
      ::closeSource( mp2, .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:activateTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == oXbp } ) ) > 0
      ::nCurTab  := mp2
      ::aSources := { ::aTabs[ ::nCurTab, 5 ] }
      ::createTags()
      ::updateFuncList()
      ::dispEditInfo()
      ::manageFocusInEditor()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:getCurrentTab()
   LOCAL qTab, nTab

   qTab := ::qTabWidget:currentWidget()
   nTab := ascan( ::aTabs, {|e_| HBQT_QTPTR_FROM_GCPOINTER( e_[ 1 ]:oWidget:pPtr ) == qTab } )

   RETURN nTab

/*----------------------------------------------------------------------*/
//                            Project Tree
/*----------------------------------------------------------------------*/

METHOD HbIde:buildProjectTree()
   LOCAL i

   ::oProjTree := XbpTreeView():new()
   ::oProjTree:hasLines   := .T.
   ::oProjTree:hasButtons := .T.
   ::oProjTree:create( ::oDa, , { 0,0 }, { 10,10 }, , .t. )
   ::oProjTree:setStyleSheet( GetStyleSheet( "QTreeWidget" ) )

   //::oProjTree:itemMarked    := {|oItem| ::manageItemSelected( 0, oItem ), ::oCurProjItem := oItem }
   ::oProjTree:itemMarked    := {|oItem| ::oCurProjItem := oItem, ::manageFocusInEditor() }
   ::oProjTree:itemSelected  := {|oItem| ::manageItemSelected( oItem ) }
   ::oProjTree:hbContextMenu := {|mp1, mp2, oXbp| ::manageProjectContext( mp1, mp2, oXbp ) }

   ::oProjTree:oWidget:setMaximumWidth( 200 )

   ::setPosAndSizeByIni( ::oProjTree:oWidget, ProjectTreeGeometry )

   ::oProjRoot      := ::oProjTree:rootItem:addItem( "Projects" )
   ::oOpenedSources := ::oProjTree:rootItem:addItem( "Editor" )

   aadd( ::aProjData, { ::oProjRoot:addItem( "Executables" ), "Executables", ::oProjRoot, NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Libs"        ), "Libs"       , ::oProjRoot, NIL, NIL } )
   aadd( ::aProjData, { ::oProjRoot:addItem( "Dlls"        ), "Dlls"       , ::oProjRoot, NIL, NIL } )

   ::oProjRoot:expand( .t. )

   IF ::aIni[ INI_HBIDE, ProjectTreeVisible ] == "NO"
      ::lProjTreeVisible := .f.
      ::oProjTree:hide()
   ENDIF

   FOR i := 1 TO len( ::aProjects )
      ::appendProjectInTree( ::aProjects[ i, 3 ] )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:appendProjectInTree( aPrj )
   LOCAL cType, oParent, oP, aSrc, j, cProject, nPath
   LOCAL aPath, cPath, cFile, cExt, oPP, cPathA

   IF !empty( aPrj ) .and. !empty( cProject := aPrj[ PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
      IF ascan( ::aProjData, {|e_| e_[ 2 ] == "Project Name" .and. e_[ 4 ] == cProject } ) == 0

         cType := aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ]

         DO CASE
         CASE cType == "Executable"
            oParent := ::aProjData[ 1, 1 ]
         CASE cType == "Lib"
            oParent := ::aProjData[ 2, 1 ]
         CASE cType == "Dll"
            oParent := ::aProjData[ 3, 1 ]
         ENDCASE

         oParent:expand( .t. )

         oP := oParent:addItem( cProject )
         aadd( ::aProjData, { oP, "Project Name", oParent, cProject, aPrj } )
         oParent := oP

         aPath := {}
         aSrc := aPrj[ PRJ_PRP_SOURCES, 2 ]
         FOR j := 1 TO len( aSrc )
            hb_fNameSplit( aSrc[ j ], @cPath, @cFile, @cExt )
            cPathA := lower( strtran( cPath, "\", "/" ) )
            IF ( nPath := ascan( aPath, {|e_| e_[ 1 ] == cPathA } ) ) == 0
               oPP := oParent:addItem( cPath )
               aadd( ::aProjData, { oPP, "Path", oParent, cPathA, cProject } )
               aadd( aPath, { cPathA, oPP } )
               nPath := len( aPath )
            ENDIF

            oPP := aPath[ nPath,2 ]
            aadd( ::aProjData, { oPP:addItem( cFile+cExt ), "Source File", oPP, aSrc[ j ], PathNormalized( aSrc[ j ] ) } )
         NEXT
      ELSE
         // Handle duplicate opening

      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:addSourceInTree( cSourceFile )
   LOCAL cPath, cPathA, cFile, cExt, n, oParent
   LOCAL oGrand := ::oOpenedSources

   hb_fNameSplit( cSourceFile, @cPath, @cFile, @cExt )
   cPathA := PathNormalized( cPath )

   n := ascan( ::aEditorPath, {|e_| e_[ 2 ] == cPathA } )

   IF n == 0
      oParent := oGrand:addItem( cPath )
      aadd( ::aProjData, { oParent, "Editor Path", oGrand, cPathA, cSourceFile } )
      aadd( ::aEditorPath, { oParent, cPathA } )
   ELSE
      oParent := ::aEditorPath[ n,1 ]
   ENDIF

   aadd( ::aProjData, { oParent:addItem( cFile+cExt ), "Opened Source", oParent, ;
                                                        cSourceFile, PathNormalized( cSourceFile ) } )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageItemSelected( oXbpTreeItem )
   LOCAL n, cHbi, aPrj, cSource

   IF     oXbpTreeItem == ::oProjRoot
      n  := -1
   ELSEIF oXbpTreeItem == ::oOpenedSources
      n  := -2
   ELSE
      n := ascan( ::aProjData, {|e_| e_[ 1 ] == oXbpTreeItem } )
   ENDIF

   ::aPrjProps := {}

   DO CASE

   CASE n ==  0  // Source File - nothing to do
   CASE n == -2  // "Files"
   CASE n == -1
   CASE ::aProjData[ n, 2 ] == "Project Name"
      aPrj := ::aProjData[ n, 5 ]

      cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + s_pathSep + ;
              aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"

      ::loadProjectProperties( cHbi, .f., .t. )

   CASE ::aProjData[ n, 2 ] == "Source File"
      cSource := ::aProjData[ n, 5 ]
      IF ( n := ascan( ::aTabs, {|e_| PathNormalized( e_[ 5 ] ) == cSource } ) ) == 0
         ::editSource( cSource )
      ELSE
         ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( QT_PTROFXBP( ::aTabs[ n,1 ] ) ) )
      ENDIF

   CASE ::aProjData[ n, 2 ] == "Opened Source"
      cSource := ::aProjData[ n, 5 ]
      IF ( n := ascan( ::aTabs, {|e_| PathNormalized( e_[ 5 ] ) == cSource } ) ) > 0
         ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( QT_PTROFXBP( ::aTabs[ n,1 ] ) ) )
      ENDIF

   CASE ::aProjData[ n, 2 ] == "Path"

   ENDCASE

   ::manageFocusInEditor()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageProjectContext( mp1, mp2, oXbpTreeItem )
   LOCAL n, cHbi, aPrj, cSource
   LOCAL aPops := {}

   HB_SYMBOL_UNUSED( mp2 )

   ::aPrjProps := {}

   oXbpTreeItem := ::oCurProjItem

   IF     oXbpTreeItem == ::oProjRoot
      n  := -1
   ELSEIF oXbpTreeItem == ::oOpenedSources
      n  := -2
   ELSE
      n := ascan( ::aProjData, {|e_| e_[ 1 ] == oXbpTreeItem } )
   ENDIF

   DO CASE
   CASE n ==  0  // Source File - nothing to do
   CASE n == -2  // "Files"
   CASE n == -1  // Project Root
      aadd( aPops, { "New Project" , {|| ::loadProjectProperties( , .t., .t. ), ::appendProjectInTree( ::aPrjProps ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Load Project", {|| ::loadProjectProperties( , .f., .f. ), ::appendProjectInTree( ::aPrjProps ) } } )
      ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Project Name"
      aPrj := ::aProjData[ n, 5 ]
      cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + s_pathSep + ;
              aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"
      //
      aadd( aPops, { "Properties"               , {|| ::loadProjectProperties( cHbi, .f., .t. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Save and Build"           , {|| NIL } } )
      aadd( aPops, { "Save, Build and Launch"   , {|| NIL } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Save and Re-Build"        , {|| NIL } } )
      aadd( aPops, { "Save, Re-Build and Launch", {|| NIL } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Drop Project"             , {|| NIL } } )
      //
      ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Source File"
      //

   CASE ::aProjData[ n, 2 ] == "Opened Source"
      cSource := ::aProjData[ n, 5 ]
      n := ascan( ::aTabs, {|e_| PathNormalized( e_[ 5 ] ) == cSource } )
      //
      aadd( aPops, { "Save" , {|| ::saveSource( n, .f. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Close", {|| ::closeSource( n ) } } )
      //
      ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Path"

   ENDCASE

   ::manageFocusInEditor()
   RETURN Self

/*----------------------------------------------------------------------*/
//                             Status Bar
/*----------------------------------------------------------------------*/

METHOD HbIde:buildStatusBar()
   LOCAL oPanel

   ::oSBar := XbpStatusBar():new()
   ::oSBar:create( ::oDlg, , { 0,0 }, { ::oDlg:currentSize()[1],30 } )
   ::oSBar:oWidget:showMessage( "" )

   oPanel := ::oSBar:getItem( 1 )
   oPanel:autosize := XBPSTATUSBAR_AUTOSIZE_SPRING

   ::oSBar:addItem( "", , , , "Ready"  ):oWidget:setMinimumWidth( 80 )

   ::oSBar:addItem( "", , , , "Line"   ):oWidget:setMinimumWidth( 110 )
   ::oSBar:addItem( "", , , , "Col"    ):oWidget:setMinimumWidth( 40 )
   ::oSBar:addItem( "", , , , "Caps"   ):oWidget:setMinimumWidth( 30 )
   ::oSBar:addItem( "", , , , "Misc"   ):oWidget:setMinimumWidth( 30 )
   ::oSBar:addItem( "", , , , "State"  ):oWidget:setMinimumWidth( 50 )
   ::oSBar:addItem( "", , , , "Misc_2" ):oWidget:setMinimumWidth( 30 )
   ::oSBar:addItem( "", , , , "Misc_3" ):oWidget:setMinimumWidth( 20 )
   ::oSBar:addItem( "", , , , "Misc_4" ):oWidget:setMinimumWidth( 20 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:dispEditInfo()
   LOCAL s, qEdit, qDoc

   IF !empty( ::qCurEdit )
      qEdit := ::qCurEdit
      qDoc  := ::qCurDocument

      ::oSBar:getItem( 2 ):caption := "Ready"

      ::qCursor := QTextCursor():configure( qEdit:textCursor() )

      s := "<b>Line "+ hb_ntos( ::qCursor:blockNumber()+1 ) + " of " + ;
                       hb_ntos( qDoc:blockCount() ) + "</b>"
      ::oSBar:getItem( 3 ):caption := s

      ::oSBar:getItem( 4 ):caption := "Col " + hb_ntos( ::qCursor:columnNumber()+1 )

      ::oSBar:getItem( 5 ):caption := IIF( qEdit:overwriteMode(), " ", "Ins" )

      ::oSBar:getItem( 7 ):caption := IIF( qDoc:isModified(), "Modified", " " )

      ::oSBar:getItem( 9  ):caption := "Stream"
      ::oSBar:getItem( 10 ):caption := "Edit"
      ::oSBar:getItem( 1  ):caption := "Success"

   ELSE
      ::oSBar:getItem(  2 ):caption := " "
      ::oSBar:getItem(  3 ):caption := " "
      ::oSBar:getItem(  4 ):caption := " "
      ::oSBar:getItem(  5 ):caption := " "
      ::oSBar:getItem(  6 ):caption := " "
      ::oSBar:getItem(  7 ):caption := " "
      ::oSBar:getItem(  8 ):caption := " "
      ::oSBar:getItem(  9 ):caption := " "
      ::oSBar:getItem( 10 ):caption := " "
      ::oSBar:getItem(  1 ):caption := " "

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//                              Main Window
/*----------------------------------------------------------------------*/

METHOD HbIde:buildDialog()

   ::oDlg := XbpDialog():new( , , {10,10}, {1100,700}, , .f. )

   ::oDlg:icon := s_resPath + "vr.png" // "hbide.png"
   ::oDlg:title := "Harbour-Qt IDE"

   ::oDlg:create()

   ::setPosAndSizeByIni( ::oDlg:oWidget, MainWindowGeometry )

   ::oDlg:close := {|| MsgBox( "You can also close me by pressing [ESC]" ), .T. }
   ::oDlg:oWidget:setDockOptions( QMainWindow_AllowTabbedDocks + QMainWindow_ForceTabbedDocks )
   ::oDlg:oWidget:setTabPosition( Qt_BottomDockWidgetArea, QTabWidget_South )

   RETURN Self

/*----------------------------------------------------------------------*/
//                          Function List
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
   ::oDockR:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oFuncList := XbpListBox():new( ::oDockR ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oFuncList:setStyleSheet( GetStyleSheet( "QListView" ) )

   //::oFuncList:ItemMarked := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   ::oFuncList:ItemSelected  := {|mp1, mp2, oXbp| ::gotoFunction( mp1, mp2, oXbp ) }
   /* Harbour Extension : prefixed with "hb" */
   ::oFuncList:hbContextMenu := {|mp1, mp2, oXbp| ::manageFuncContext( mp1, mp2, oXbp ) }

   ::oFuncList:oWidget:setEditTriggers( QAbstractItemView_NoEditTriggers )

   ::oDockR:oWidget:setWidget( QT_PTROFXBP( ::oFuncList ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_RightDockWidgetArea, QT_PTROFXBP( ::oDockR ), Qt_Horizontal )

   IF ::aIni[ INI_HBIDE, FunctionListVisible ] == "YES"
      ::lDockRVisible := .t.
      //::setSizeAndPosByIni( ::oDockR:oWidget, FunctionListGeometry )
   ELSE
      ::lDockRVisible := .f.
      ::oDockR:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:updateFuncList()
   LOCAL o

   ::oFuncList:clear()
   IF !empty( ::aTags )
      aeval( ::aTags, {|e_| o := ::oFuncList:addItem( e_[ 7 ] ) } )
   ELSE
      ::lDockRVisible := .f.
      ::oDockR:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:gotoFunction( mp1, mp2, oListBox )
   LOCAL n, cAnchor

   mp1 := oListBox:getData()
   mp2 := oListBox:getItem( mp1 )

   IF ( n := ascan( ::aTags, {|e_| mp2 == e_[ 7 ] } ) ) > 0
      cAnchor := trim( ::aText[ ::aTags[ n,3 ] ] )
      IF !( ::aTabs[ ::nCurTab, 2 ]:find( cAnchor, QTextDocument_FindCaseSensitively ) )
         ::aTabs[ ::nCurTab, 2 ]:find( cAnchor, QTextDocument_FindBackward + QTextDocument_FindCaseSensitively )

      ENDIF
   ENDIF
   ::manageFocusInEditor()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageFuncContext( mp1 )
   LOCAL aPops := {}

   IF ::oFuncList:numItems() > 0
      aadd( aPops, { 'Comment out'           , {|| NIL } } )
      aadd( aPops, { 'Reformat'              , {|| NIL } } )
      aadd( aPops, { 'Print'                 , {|| NIL } } )
      aadd( aPops, { 'Delete'                , {|| NIL } } )
      aadd( aPops, { 'Move to another source', {|| NIL } } )

      ExecPopup( aPops, mp1, ::oFuncList:oWidget )

      ::manageFocusInEditor()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:CreateTags()
   LOCAL aSumData := ""
   LOCAL cComments, aSummary, i, cPath, cSource, cExt

   ::aTags := {}

   FOR i := 1 TO Len( ::aSources )
      HB_FNameSplit( ::aSources[ i ], @cPath, @cSource, @cExt )

      IF Upper( cExt ) $ ".PRG.CPP"
         IF !empty( ::aText := ReadSource( ::aSources[ i ] ) )
            aSumData  := {}

            cComments := CheckComments( ::aText )
            aSummary  := Summarize( ::aText, cComments, @aSumData , IIf( Upper( cExt ) == ".PRG", 9, 1 ) )
            ::aTags   := UpdateTags( ::aSources[ i ], aSummary, aSumData, @::aFuncList, @::aLines )

            #if 0
            IF !empty( aTags )
               aeval( aTags, {|e_| aadd( ::aTags, e_ ) } )
               ::hData[ cSource+cExt ] := { a[ i ], aTags, aclone( ::aText ), cComments, ::aFuncList, ::aLines }
               aadd( ::aSrcLines, ::aText   )
               aadd( ::aComments, cComments )
            ENDIF
            #endif
         ENDIF
      ENDIF
   NEXT

   RETURN ( NIL )

//----------------------------------------------------------------------//
//                            Dock Widgets
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
   ::oDockB:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oCompileResult := XbpMLE():new( ::oDockB ):create( , , { 0,0 }, { 100,400 }, , .t. )
   ::oDockB:oWidget:setWidget( QT_PTROFXBP( ::oCompileResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB ), Qt_Vertical )
   ::oDockB:hide()

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
   ::oDockB1:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oLinkResult := XbpMLE():new( ::oDockB1 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB1:oWidget:setWidget( QT_PTROFXBP( ::oLinkResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB1 ), Qt_Vertical )
   ::oDockB1:hide()

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
   ::oDockB2:oWidget:setFocusPolicy( Qt_NoFocus )

   ::oOutputResult := XbpMLE():new( ::oDockB2 ):create( , , { 0,0 }, { 100, 400 }, , .t. )
   ::oDockB2:oWidget:setWidget( QT_PTROFXBP( ::oOutputResult ) )

   ::oDlg:oWidget:addDockWidget_1( Qt_BottomDockWidgetArea, QT_PTROFXBP( ::oDockB2 ), Qt_Vertical )
   ::oDockB2:hide()

   RETURN Self

/*----------------------------------------------------------------------*/
//                               Printing
/*----------------------------------------------------------------------*/

METHOD HbIde:printPreview()
   LOCAL qDlg

   qDlg := QPrintPreviewDialog():new( QT_PTROFXBP( ::oDlg ) )
   qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
   Qt_Connect_Signal( qDlg:pPtr, "paintRequested(QPrinter)", {|o,p| ::paintRequested( p,o ) } )
   qDlg:exec()
   Qt_DisConnect_Signal( qDlg:pPtr, "paintRequested(QPrinter)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:paintRequested( pPrinter )
   LOCAL qPrinter

   qPrinter := QPrinter():configure( pPrinter )

   ::qCurEdit:print( QT_PTROF( qPrinter ) )

   RETURN Self

/*----------------------------------------------------------------------*/
//                       Menu and Toolbar Actions
/*----------------------------------------------------------------------*/

METHOD HbIde:executeAction( cKey )
   LOCAL cFile

   DO CASE

   CASE cKey == "Exit"
      PostAppEvent( xbeP_Close, NIL, NIL, ::oDlg )
   CASE cKey == "ToggleProjectTree"
      ::lProjTreeVisible := !::lProjTreeVisible
      IF !( ::lProjTreeVisible )
         ::oProjTree:hide()
      ELSE
         ::oProjTree:show()
      ENDIF
   CASE cKey == "NewProject"
      ::fetchProjectProperties( .t. )
   CASE cKey == "Open"
      IF !empty( cFile := ::selectSource( "open" ) )
         ::editSource( cFile )
      ENDIF
   CASE cKey == "Save"
      ::saveSource( ::getCurrentTab(), .f. )
   CASE cKey == "Close"
      ::closeSource()
   CASE cKey == "Print"
      IF !empty( ::qCurEdit )
         ::printPreview()
      ENDIF
   CASE cKey == "Undo"
      IF !empty( ::qCurEdit )
         ::qCurEdit:undo()
      ENDIF
   CASE cKey == "Redo"
      IF !empty( ::qCurEdit )
         ::qCurEdit:redo()
      ENDIF
   CASE cKey == "Cut"
      IF !empty( ::qCurEdit )
         ::qCurEdit:cut()
      ENDIF
   CASE cKey == "Copy"
      IF !empty( ::qCurEdit )
         ::qCurEdit:copy()
      ENDIF
   CASE cKey == "Paste"
      IF !empty( ::qCurEdit )
         ::qCurEdit:paste()
      ENDIF
   CASE cKey == "SelectAll"
      IF !empty( ::qCurEdit )
         ::qCurEdit:selectAll()
      ENDIF
   CASE cKey == "Find"
      IF !empty( ::qCurEdit )
         ::findReplace( "finddialog" )
      ENDIF
   CASE cKey == "ToUpper"
      ::convertSelection( cKey )
   CASE cKey == "ToLower"
      ::convertSelection( cKey )
   CASE cKey == "Invert"
      ::convertSelection( cKey )
   CASE cKey == "ZoomIn"
      IF !empty( ::qCurEdit )
         //::qCurEdit:zoomIn()
      ENDIF
   CASE cKey == "ZoomOut"
      IF !empty( ::qCurEdit )
         //::qCurEdit:zoomOut()
      ENDIF
   CASE cKey == "11"
      IF ::lDockBVisible
         ::oDockB:hide()
         ::oDockB1:hide()
         ::oDockB2:hide()
         ::lDockBVisible := .f.
      ELSEIF ::qTabWidget:count() > 0
         ::oDockB:show()
         ::oDockB1:show()
         ::oDockB2:show()
         ::lDockBVisible := .t.
      ENDIF
   CASE cKey == "12"
      IF ::lDockRVisible
         ::oDockR:hide()
      ELSE
         ::oDockR:show()
      ENDIF
      ::lDockRVisible := !( ::lDockRVisible )

   CASE cKey == "Compile"
   CASE cKey == "CompilePPO"

   ENDCASE

   ::manageFocusInEditor()

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbIde:loadUI( cUi )
   LOCAL cUiFull := s_resPath + cUi + ".ui"
   LOCAL qDialog, qUiLoader, qFile

   IF file( cUiFull )
      qFile := QFile():new( cUiFull )
      IF qFile:open( 1 )
         qUiLoader  := QUiLoader():new()
         qDialog    := QDialog():configure( qUiLoader:load( QT_PTROF( qFile ), QT_PTROFXBP( ::oDlg ) ) )
         qFile:close()
      ENDIF
   ENDIF

   RETURN qDialog

/*----------------------------------------------------------------------*/

METHOD HbIde:findReplace( cUi )
   LOCAL qUiLoader, qFile, cUiFull

   IF ::qFindDlg == NIL
      cUiFull := s_resPath + cUi + ".ui"
      qFile := QFile():new( cUiFull )
      IF qFile:open( 1 )
         qUiLoader  := QUiLoader():new()
         ::qFindDlg := QDialog():configure( qUiLoader:load( QT_PTROF( qFile ), QT_PTROFXBP( ::oDlg ) ) )
         qFile:close()
         //
         ::qFindDlg:setWindowFlags( Qt_Sheet )
         //
         ::oFind := XbpComboBox():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "comboFindWhat" ) )
         ::oRepl := XbpComboBox():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "comboReplaceWith" ) )

         ::oPBFind := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonFind" ) )
         ::oPBFind:activate := {|| ::qCurEdit:find( QLineEdit():configure( ::oFind:oWidget:lineEdit() ):text() ) }

         ::oPBRepl := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonReplace" ) )
         ::oPBRepl:activate := {|t| t := QLineEdit():configure( ::oRepl:oWidget:lineEdit() ):text() }

         ::oPBClose := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( QT_PTROF( ::qFindDlg ), "buttonClose" ) )
         ::oPBClose:activate := {|| ::qFindDlg:hide() }
      ENDIF
   ENDIF

   ::oFind:setFocus()
   ::qFindDlg:show()

   RETURN Self

/*----------------------------------------------------------------------*/
//                           Project Properties
/*----------------------------------------------------------------------*/

METHOD HbIde:loadProjectProperties( cProject, lNew, lFetch )
   LOCAL cWrkProject
   LOCAL n := 0

   DEFAULT cProject TO ""
   DEFAULT lNew     TO .F.
   DEFAULT lFetch   TO .T.

   ::aPrjProps := {}

   IF lNew
      lFetch := .t.
   ENDIF

   IF !( lNew )
      IF empty( cProject )
         cProject := fetchAFile( ::oDlg, "Select a Harbour IDE Project", { { "Harbour IDE Projects", "*.hbi" } } )
      ENDIF
      IF empty( cProject )
         RETURN Self
      ENDIF
   ENDIF

   IF !empty( cProject )
      cWrkProject := lower( cProject )  // normalized

      IF !empty( ::aProjects )
         IF ( n := ascan( ::aProjects, {|e_| e_[ 1 ] == cWrkProject } ) ) > 0
            ::aPrjProps := ::aProjects[ n, 3 ]
         ENDIF
      ENDIF

      IF empty( ::aPrjProps )
         ::aPrjProps := fetchHbiStructFromFile( cProject )
      ENDIF
   ENDIF

   IF lFetch
      ::cSaveTo := ""
      ::fetchProjectProperties()
      IF !empty( ::cSaveTo ) .and. file( ::cSaveTo )
         cProject := ::cSaveTo
         /* Reload from file */
         ::aPrjProps := fetchHbiStructFromFile( cProject )
      ENDIF
   ENDIF

   IF n == 0
      aadd( ::aProjects, { lower( cProject ), cProject, aclone( ::aPrjProps ) } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:fetchProjectProperties()
   LOCAL qPrpDlg, qPrjType, oPrjTtl, oPBOk, oPBCn, pPrpDlg, oTabWidget, oPBSv, oPBSelect
   LOCAL oPrjLoc, oPrjWrk, oPrjDst, oPrjOut, oPrjInc, oPrjLau, oPrjLEx, oPrjSrc, oPrjMta, oPrjHbp, oPrjCmp
   LOCAL cPrjLoc   := hb_dirBase() + "projects"
   LOCAL aPrjProps := ::aPrjProps

   IF !empty( qPrpDlg := ::loadUI( "projectproperties" ) )
      pPrpDlg := QT_PTROF( qPrpDlg )

      qPrjType := QComboBox():configure( Qt_findChild( pPrpDlg, "comboPrjType" ) )
      qPrjType:addItem( "Executable" )
      qPrjType:addItem( "Library"    )
      qPrjType:addItem( "Dll"        )

      oPrjTtl := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editPrjTitle"     ) )
      oPrjLoc := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editPrjLoctn"     ) )
      oPrjWrk := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editWrkFolder"    ) )
      oPrjDst := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editDstFolder"    ) )
      oPrjOut := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editOutName"      ) )
      oPrjInc := QTextEdit():configure( Qt_FindChild( pPrpDlg, "editFlags"        ) )
      oPrjLau := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editLaunchParams" ) )
      oPrjLEx := QLineEdit():configure( Qt_FindChild( pPrpDlg, "editLaunchExe"    ) )
      oPrjSrc := QTextEdit():configure( Qt_FindChild( pPrpDlg, "editSources"      ) )
      oPrjMta := QTextEdit():configure( Qt_FindChild( pPrpDlg, "editMetaData"     ) )
      oPrjHbp := QTextEdit():configure( Qt_FindChild( pPrpDlg, "editHbp"          ) )
      oPrjCmp := QTextEdit():configure( Qt_FindChild( pPrpDlg, "editCompilers"    ) )

      ::aPrpObjs := { qPrjType, oPrjTtl, oPrjLoc, oPrjWrk, oPrjDst, oPrjOut, oPrjLau, oPrjLEx, oPrjInc, oPrjSrc, oPrjMta, oPrjHbp, oPrjCmp }

      oPBCn := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( pPrpDlg, "buttonCn" ) )
      oPBCn:activate := {|| qPrpDlg:close() }
      oPBSv := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( pPrpDlg, "buttonSave" ) )
      oPBSv:activate := {|| ::saveProject() }
      oPBOk := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( pPrpDlg, "buttonSaveExit" ) )
      oPBOk:activate := {|| ::saveProject(), qPrpDlg:close() }
      oPBSelect := XbpPushButton():new():createFromQtPtr( , , , , , , Qt_findChild( pPrpDlg, "buttonSelect" ) )
      oPBSelect:activate := {|| ::addSourcesToProject() }

      oTabWidget := QTabWidget():configure( Qt_FindChild( pPrpDlg, "tabWidget" ) )
      Qt_Connect_Signal( QT_PTROF( oTabWidget ), "currentChanged(int)", {|o,p| ::updateHbp( p, o ) } )

      IF empty( aPrjProps )
         oPrjTtl:setText( "untitled"   )
         oPrjLoc:setText( cPrjLoc      )
         oPrjWrk:setText( hb_dirBase() )
         oPrjDst:setText( cPrjLoc      )
         oPrjOut:setText( "untitled"   )

      ELSE
         oPrjTtl:setText( aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_TITLE     ] )
         oPrjLoc:setText( aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_LOCATION  ] )
         oPrjWrk:setText( aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_WRKFOLDER ] )
         oPrjDst:setText( aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_DSTFOLDER ] )
         oPrjOut:setText( aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_OUTPUT    ] )

         oPrjInc:setPlainText( ArrayToMemo( aPrjProps[ PRJ_PRP_FLAGS   , 1 ] ) )
         oPrjSrc:setPlainText( ArrayToMemo( aPrjProps[ PRJ_PRP_SOURCES , 1 ] ) )
         oPrjMta:setPlainText( ArrayToMemo( aPrjProps[ PRJ_PRP_METADATA, 1 ] ) )
         oPrjCmp:setPlainText( memoread( hb_dirBase() + "hbide.env" ) )

      ENDIF

      qPrpDlg:exec()

      oPBOk:destroy()
      oPBSv:destroy()
      oPBCn:destroy()
      oPBSelect:destroy()

      Qt_DisConnect_Signal( QT_PTROF( oTabWidget ), "currentChanged(int)" )
   ENDIF

   ::aPrpObjs := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:updateHbp( iIndex )
   LOCAL a_, a4_1, o_, txt_, s
   LOCAL cExt

   IF iIndex != 3
      RETURN nil
   ENDIF
   IF empty( o_:= ::aPrpObjs )
      RETURN nil
   ENDIF

   a_:= hb_atokens( strtran( o_[ E_oPrjMta ]:toPlainText(), chr( 13 ) ), _EOL )
   a4_1 := SetupMetaKeys( a_ )

   txt_:= {}
   /* This block will be absent when submitting to hbmk engine */
   aadd( txt_, "#   " + ParseWithMetaData( o_[ E_oPrjWrk ]:text(), a4_1 ) + s_pathSep + ;
                        ParseWithMetaData( o_[ E_oPrjOut ]:text(), a4_1 ) + ".hbp" )
   aadd( txt_, " " )

   /* Flags */
   a_:= hb_atokens( o_[ E_oPrjInc ]:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
         s := ParseWithMetaData( s, a4_1 )
         aadd( txt_, s )
      ENDIF
   NEXT
   aadd( txt_, " " )

   /* Sources */
   a_:= hb_atokens( o_[ E_oPrjSrc ]:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
         s := ParseWithMetaData( s, a4_1 )
         hb_FNameSplit( s, , , @cExt )
         IF lower( cExt ) $ ".c,.cpp,.prg,.rc,.res"
            aadd( txt_, s )
         ENDIF
      ENDIF
   NEXT
   aadd( txt_, " " )

   /* Final assault */
   ::aPrpObjs[ E_oPrjHbp ]:setPlainText( ArrayToMemo( txt_ ) )

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD HbIde:saveProject()
   LOCAL txt_, a_, a4_1, o_//, s, b_, i, j
   LOCAL typ_:= { "Executable", "Lib", "Dll" }

   IF empty( o_:= ::aPrpObjs )
      RETURN nil
   ENDIF

   txt_:= {}

   aadd( txt_, "[ PROPERTIES ]" )
   aadd( txt_, "Type              = " + typ_[ o_[ E_qPrjType ]:currentIndex()+1 ] )
   aadd( txt_, "Title             = " + o_[ E_oPrjTtl ]:text() )
   aadd( txt_, "Location          = " + o_[ E_oPrjLoc ]:text() )
   aadd( txt_, "WorkingFolder     = " + o_[ E_oPrjWrk ]:text() )
   aadd( txt_, "DestinationFolder = " + o_[ E_oPrjDst ]:text() )
   aadd( txt_, "Output            = " + o_[ E_oPrjOut ]:text() )
   aadd( txt_, "LaunchParams      = " + o_[ E_oPrjLau ]:text() )
   aadd( txt_, "LaunchProgram     = " + o_[ E_oPrjLEx ]:text() )
   aadd( txt_, " " )

   aadd( txt_, "[ FLAGS ]" )
   a_:= MemoToArray( o_[ E_oPrjInc ]:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ SOURCES ]" )
   a_:= MemoToArray( o_[ E_oPrjSrc ]:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ METADATA ]" )
   a_:= MemoToArray( o_[ E_oPrjMta ]:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )

   /* Setup Meta Keys */
   a4_1 := SetupMetaKeys( a_ )

   ::cSaveTo := ParseWithMetaData( o_[ E_oPrjLoc ]:text(), a4_1 ) + ;
                      hb_OsPathSeparator() + ;
                ParseWithMetaData( o_[ E_oPrjOut ]:text(), a4_1 ) + ;
                      ".hbi"

   CreateTarget( ::cSaveTo, txt_ )
   hb_MemoWrit( hb_dirBase() + "hbide.env", o_[ E_oPrjCmp ]:toPlainText() )

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD HbIde:addSourcesToProject()
   LOCAL aFiles, a_, b_, a4_1, s

   IF !empty( aFiles := ::selectSource( "openmany" ) )
      a_:= MemoToArray( ::aPrpObjs[ E_oPrjMta ]:toPlainText() )
      a4_1 := SetupMetaKeys( a_ )

      a_:= MemoToArray( ::aPrpObjs[ E_oPrjSrc ]:toPlainText() )

      b_:={}
      aeval( aFiles, {|e| aadd( b_, ApplyMetaData( e, a4_1 ) ) } )

      FOR EACH s IN b_
         IF ascan( a_, s ) == 0
            aadd( a_, s )
         ENDIF
      NEXT

      ::aPrpObjs[ E_oPrjSrc ]:setPlainText( ArrayToMemo( a_ ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
/*----------------------------------------------------------------------*/

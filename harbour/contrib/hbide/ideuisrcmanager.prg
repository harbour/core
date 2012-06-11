/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               28Feb2012
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "fileio.ch"
#include "hbide.ch"
#include "hbhrb.ch"

/*----------------------------------------------------------------------*/

#define  PNL_UI                                   1
#define  PNL_OBJECTS                              2
#define  PNL_TYPE                                 3

/*----------------------------------------------------------------------*/
//                            CLASS UISrcData
/*----------------------------------------------------------------------*/

CLASS UISrcData

   DATA   qObj
   DATA   cName                                   INIT ""
   DATA   hSource                                 INIT {=>}

   METHOD new( qObj, cObj )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD UISrcData:new( qObj, cObj )

   hb_hCaseMatch( ::hSource, .f. )

   ::qObj  := qObj
   ::cName := cObj

   RETURN Self

/*----------------------------------------------------------------------*/
//                         CLASS IdeUISrcManager
/*----------------------------------------------------------------------*/

CLASS IdeUISrcManager INHERIT IdeObject

   DATA   qU
   DATA   qMdiArea
   DATA   qMdiSub
   DATA   qWidget
   DATA   qToolbar
   DATA   qLayout
   DATA   qStatus
   DATA   aStatusPnls                             INIT {}
   DATA   aToolBtns                               INIT {}
   DATA   sp0
   DATA   pHrb
   DATA   qWdg
   DATA   qFocus
   DATA   qCurrent
   DATA   qTree
   DATA   qEdit
   DATA   qFont
   DATA   qSplitter
   DATA   qFrame
   DATA   qHBLayout
   DATA   qHiliter

   DATA   oEdit

   DATA   hMethods                                INIT {=>}
   DATA   hObjects                                INIT {=>}
   DATA   cPath                                   INIT ""
   DATA   cName                                   INIT ""
   DATA   cExt                                    INIT ""
   DATA   lChanged                                INIT .f.
   DATA   aPrg                                    INIT {}
   DATA   aObjByName                              INIT {}
   DATA   cSrcFile                                INIT ""
   DATA   cSource                                 INIT ""
   DATA   aSource                                 INIT {}
   DATA   oProcess
   DATA   cCurAction                              INIT ""
   DATA   cClsPrefix                              INIT "ui_"

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD show()
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
   METHOD buildToolbar()
   METHOD buildToolButton( qToolbar, aBtn )
   METHOD buildStatusPanels()
   METHOD openUi( cUI )
   METHOD reloadIfOpen( cUI )
   METHOD buildUiWidget( cUI )
   METHOD buildWidget( cBuffer, cPath, cName, cExt, aPrg )
   METHOD runHbmk2( cUI )
   METHOD clear()
   METHOD checkUpdates()
   METHOD buildSource()
   METHOD loadSource()
   METHOD finished( nExitCode, nExitStatus )
   METHOD outputText( cText )
   METHOD loadActions( oWidget, cName )
   METHOD exposeAction()
   METHOD loadMethod()
   METHOD saveMethod()
   METHOD buildClassSkeleton( cCls, cUiName )
   METHOD getCurrentSlot()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:new( oIde )

   ::oIde := oIde

   hb_hKeepOrder( ::hObjects, .t. )
   hb_hCaseMatch( ::hObjects, .f. )

   hb_hKeepOrder( ::hMethods, .t. )
   hb_hCaseMatch( ::hMethods, .f. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:show()
   LOCAL qDock

   IF ! empty( ::oUI )
      RETURN Self
   ENDIF

   ::oUI := 1

   qDock := ::oIde:oUiSrcDock:oWidget

   qDock:setAcceptDrops( .t. )
   qDock:connect( QEvent_DragEnter, {|p| ::execEvent( "dockUIScr_dragEnterEvent", p ) } )
   qDock:connect( QEvent_Drop     , {|p| ::execEvent( "dockUISrc_dropEvent"     , p ) } )

   ::qWidget := QWidget()

   qDock:setWidget( ::qWidget )

   /* Layout applied to dbu widget */
   ::qLayout := QGridLayout()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setSpacing( 0 )

   ::qWidget:setLayout( ::qLayout )

   /* Toolbar */
   ::buildToolbar()
   ::qLayout:addWidget( ::qToolbar, 0, 0, 1, 1 )

   ::qSplitter := QSplitter()
   ::qSplitter:setOrientation( Qt_Vertical )
   ::qLayout:addWidget( ::qSplitter, 1, 0, 1, 1 )

   /* StatusBar */
   ::qStatus := QStatusBar()
   ::qStatus:setSizeGripEnabled( .f. )
   ::qLayout:addWidget( ::qStatus  , 2, 0, 1, 1 )

   ::qMdiArea := QMdiArea()
   ::qMdiArea:setDocumentMode( .t. )
   ::qMdiArea:setOption( QMdiArea_DontMaximizeSubWindowOnActivation, .t. )
   ::qMdiArea:setVerticalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qMdiArea:setHorizontalScrollBarPolicy( Qt_ScrollBarAsNeeded )
   ::qMdiArea:setViewMode( QMdiArea_SubWindowView )
   ::qMdiArea:connect( "subWindowActivated(QMdiSubWindow*)", {|p| ::execEvent( "mdiArea_subWindowActivated", p ) } )

   ::qSplitter:addWidget( ::qMdiArea )

   ::qFrame := QFrame()
   ::qHBLayout := QHBoxLayout()
   ::qHBLayout:setContentsMargins( 0, 0, 0, 0 )
   ::qFrame:setLayout( ::qHBLayout )

   ::qSplitter:addWidget( ::qFrame )

   ::qTree := QTreeWidget()
   ::qTree:setMaximumWidth( 150 )
   ::qTree:setHeaderHidden( .t. )
   ::qTree:connect( "itemSelectionChanged()", {|| ::exposeAction() } )
   ::qHBLayout:addWidget( ::qTree )

   ::qEdit := QPlainTextEdit()
   ::qHBLayout:addWidget( ::qEdit )

   ::oEdit := IdeEdit():new( ::oIde )
   ::oEdit:qEdit := ::qEdit

   ::qFont := QFont()
   ::qFont:setFamily( "Courier" )
   ::qFont:setFixedPitch( .t. )
   ::qFont:setPointSize( 10 )
   ::qEdit:setFont( ::qFont )

   ::qEdit:setLineWrapMode( QTextEdit_NoWrap )
   ::qEdit:setFont( ::oIde:oFont:oWidget )
   ::qEdit:ensureCursorVisible()

   ::qHiliter := ::oTH:setSyntaxHilighting( ::qEdit, "Bare Minimum", .t., .t. )
   ::qHiliter:hbSetInitialized( .t. )

   /* Statusbar Panels */
   ::buildStatusPanels()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:execEvent( cEvent, p, p1 )
   LOCAL qList, qMime, qUrl, cExt, cUI

   SWITCH cEvent
   CASE "buttonBuild_clicked"
      ::buildSource()
      EXIT

   CASE "buttonOpen_clicked"
      IF ! empty( cUI := hbide_fetchAFile( ::oIde:oDlg, "Select a .UI", { { "Qt Designer .UI File", "*.ui" } }, ::oIde:cWrkFolderLast ) )
         ::buildUiWidget( cUI )
      ENDIF
      EXIT

   CASE "dockUIScr_dragEnterEvent"
      p:acceptProposedAction()
      EXIT

   CASE "dockUISrc_dropEvent"
      qMime := p:mimeData()
      IF qMime:hasUrls()
         qList := qMime:urls()
         qUrl := qList:at( 0 )
         hb_fNameSplit( qUrl:toLocalFile(), , , @cExt )
         IF lower( cExt ) == ".ui"
            ::buildUiWidget( qUrl:toLocalFile() )
         ENDIF
      ENDIF
      EXIT

   CASE "child_object"
      IF empty( ::qCurrent ) .OR. ! ( ::qCurrent == p )
         ::saveMethod()

         ::qCurrent := p
         ::cCurAction := ""

         ::qFocus:setWidget( p )
         ::aStatusPnls[ PNL_OBJECTS ]:setText( "<font color = blue>OBJ: " + p1 + "</font>" )
         ::aStatusPnls[ PNL_TYPE ]:setText( "<font color = green>CLASS: " + lower(__objGetClsName( p ) ) + "</font>" )
         p:clearFocus()
         ::loadActions( p, p1 )
      ENDIF
      EXIT

   CASE "mdiArea_subWindowActivated"
      EXIT

   ENDSWITCH

   RETURN .f.

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:getCurrentSlot()
   LOCAL cCls := __objGetClsName( ::qCurrent )

   SWITCH ::cCurAction
   CASE "Activated"
      IF cCls == "QTOOLBUTTON" .OR. "QPUSHBUTTON"
         RETURN "clicked()"
      ENDIF
      EXIT
   CASE "Icon"  /* just */
      EXIT
   ENDSWITCH

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:saveMethod()
   LOCAL cSrc, n, n0, n1, n2, n3, cMtd, i, aSrc, cSearch, cSlot
   LOCAL cMethod, cObjName, cAction

   IF empty( ::qCurrent )
      RETURN Self
   ENDIF

   cObjName := ::qCurrent:objectName()
   cAction  := ::cCurAction
   IF empty( cAction ) .OR. ! ::qEdit:document():isModified()
      RETURN Self
   ENDIF

   cMethod := cObjName + "_" + upper( left( cAction,1 ) ) + lower( substr( cAction, 2 ) )
   cMtd    := "METHOD " + ::cClsPrefix + ::cName + ":" + cMethod + "( ... )"
   cSrc    := ::qEdit:toPlainText()

   n0 := ascan( ::aSource, {|e| "<METHODSEVENTS>" $ e } )
   n1 := ascan( ::aSource, {|e| "</METHODSEVENTS>" $ e } )

   n2 := ascan( ::aSource, {|e| "METHOD " + cMethod $ e }, n0, n1 )
   IF n2 > 0 .AND. empty( cSrc )
      hb_adel( ::aSource, n2, .t. )
   ELSEIF n2 == 0 .AND. ! empty( cSrc )
      ::aSource := hb_aIns( ::aSource, n0+1, "   " + "METHOD " + cMethod + "( ... )", .t. )
   ENDIF

   n2 := ascan( ::aSource, {|e| cMtd $ e } )
   n3 := ascan( ::aSource, {|e| "RETURN Self" $ e }, n2 )

   IF empty( cSrc )
      IF n2 > 0
         FOR i := n3 + 1 TO n2 STEP - 1
            hb_adel( ::aSource, i, .t. )
         NEXT
      ENDIF
   ELSE
      cSrc := strtran( cSrc, chr( 13 ) + chr( 10 ), chr( 10 ) )
      aSrc := hb_aTokens( cSrc, chr( 10 ) )

      IF n2 > 0
         FOR i := n3-1 TO n2 + 1 STEP - 1
            hb_adel( ::aSource, i, .t. )
         NEXT
         FOR i := 1 TO len( aSrc )
            ::aSource := hb_ains( ::aSource, n2 + i, "   " + aSrc[ i ], .t. )
         NEXT
      ELSE
         n := ascan( ::aSource, {|e| "<EVENTSMETHODAREA>" $ e } )
         ::aSource := hb_ains( ::aSource, ++n, cMtd, .t. )
         FOR i := 1 TO len( aSrc )
            ::aSource := hb_ains( ::aSource, ++n, "   " + aSrc[ i ], .t. )
         NEXT
         ::aSource := hb_ains( ::aSource, ++n, "  ", .t. )
         ::aSource := hb_ains( ::aSource, ++n, "   RETURN Self", .t. )
         ::aSource := hb_ains( ::aSource, ++n, "  ", .t. )
      ENDIF
   ENDIF

   IF ! empty( cSlot := ::getCurrentSlot() )
      // Connections
      n0 := ascan( ::aSource, {|e| "<CONNECTS>" $ e } )
      n1 := ascan( ::aSource, {|e| "</CONNECTS>" $ e }, n0 )
      cSearch := '::oUI:qObj[ "' + cObjName + '" ]'
      n2 := ascan( ::aSource, {|e| cSearch $ e }, n0+1, n1-n0-1 )
      IF empty( cSrc )
         IF n2 > 0
            hb_adel( ::aSource, n2, .t. )
         ENDIF
      ELSE
         IF n2 == 0
            hb_ains( ::aSource, n0+1, '   ::oUI:qObj[ "' + cObjName + '" ]:connect( "' + cSlot + '", {|...| ::' + cMethod + '( ... ) } )' )
         ENDIF
      ENDIF
      // Disconnections
      n0 := ascan( ::aSource, {|e| "<DISCONNECTS>" $ e } )
      n1 := ascan( ::aSource, {|e| "</DISCONNECTS>" $ e }, n0 )
      cSearch := '::oUI:qObj[ "' + cObjName + '" ]'
      n2 := ascan( ::aSource, {|e| cSearch $ e }, n0+1, n1-n0-1 )
      IF empty( cSrc )
         IF n2 > 0
            hb_adel( ::aSource, n2, .t. )
         ENDIF
      ELSE
         IF n2 == 0
            hb_ains( ::aSource, n0+1, '   ::oUI:qObj[ "' + cObjName + '" ]:disconnect( "' + cSlot + '" )' )
         ENDIF
      ENDIF

   ENDIF

   ::qEdit:document():clear()
   ::buildSource() /* Temporary */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:loadMethod()
   LOCAL cSrc := "", n0, n1, n2, n3, cMtd, i
   LOCAL cObjName, cAction, cMethod

   IF empty( ::qCurrent )
      RETURN Self
   ENDIF

   cObjName := ::qCurrent:objectName()
   cAction  := ::cCurAction
   cMethod  := cObjName + "_" + upper( left( cAction,1 ) ) + lower( substr( cAction, 2 ) )

   n0 := ascan( ::aSource, {|e| "<METHODSEVENTS>" $ e } )
   n1 := ascan( ::aSource, {|e| "</METHODSEVENTS>" $ e } )

   n2 := ascan( ::aSource, {|e| "METHOD " + cMethod $ e }, n0, n1 )
   IF n2 > 0
      cMtd := "METHOD " + ::cClsPrefix + ::cName + ":" + cMethod + "( ... )"
      IF ( n2 := ascan( ::aSource, {|e| cMtd $ e } ) ) > 0
         n3 := ascan( ::aSource, {|e| "RETURN Self" $ e }, n2 )
         FOR i := n2 + 1 TO n3 - 1
            cSrc += substr( ::aSource[ i ], 4 ) + chr( 10 )
         NEXT
         cSrc := substr( cSrc, 1, len( cSrc ) - 1 )
      ENDIF
   ENDIF

   RETURN cSrc

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:exposeAction()
   LOCAL qItem := ::qTree:currentItem()
   LOCAL cText := qItem:text( 0 )

   IF cText == ::cCurAction
      RETURN Self
   ENDIF

   ::saveMethod()
   ::cCurAction := cText

   SWITCH __objGetClsName( ::qCurrent )
   CASE "QPUSHBUTTON"
   CASE "QTOOLBUTTON"
      SWITCH ::cCurAction

      CASE "Activated"
         ::qEdit:setPlainText( ::loadMethod() )
         ::qEdit:setFocus()
         EXIT

      CASE "Icon"
         ::qEdit:setPlainText( ::loadMethod() )
         ::qEdit:setFocus()
         EXIT

      ENDSWITCH

      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:loadActions( oWidget, cName )
   LOCAL qItem

   HB_SYMBOL_UNUSED( cName )

   ::qTree:clear()

   SWITCH __objGetClsName( oWidget )
   CASE "QPUSHBUTTON"
   CASE "QTOOLBUTTON"
      qItem := QTreeWidgetItem()
      qItem:setText( 0, "Activated" )
      ::qTree:addTopLevelItem( qItem )
      qItem := QTreeWidgetItem()
      qItem:setText( 0, "Icon" )
      ::qTree:addTopLevelItem( qItem )
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:checkUpdates()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:clear()

   ::qCurrent   := NIL
   ::cCurAction := ""
   ::qEdit:document():clear()
   ::qTree:clear()

   ::aObjByName := {}
   ::aPrg   := {}
   ::qFocus := NIL
   ::qFocus := QFocusFrame()

   IF ! empty( ::pHrb  )
      hb_hrbUnload( ::pHrb  )
      ::pHrb := NIL
   ENDIF

   IF HB_ISOBJECT( ::qMdiSub )
      ::qMdiArea:removeSubWindow( ::qMdiSub )
      ::qMdiSub := NIL
   ENDIF

   ::hObjects := {=>}
   hb_hCaseMatch( ::hObjects, .f. )
   hb_hKeepOrder( ::hObjects, .t. )

   ::cSource  := ""
   ::cSrcFile := ""
   ::aSource  := {}

   ::cPath      := ""
   ::cName      := ""
   ::cExt       := ""

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:reloadIfOpen( cUI )
   LOCAL cPath, cName, cExt
   LOCAL cObjName, cAction, qList

   cUI := hbide_pathToOSPath( lower( cUI ) )
   hb_fNameSplit( cUI, @cPath, @cName, @cExt )
   IF ::cPath == cPath .AND. ::cName == cName
      IF ! empty( ::qCurrent )
         cObjName := ::qCurrent:objectName()
         cAction  := ::cCurAction
      ENDIF

      ::openUi( cUI )

      IF ! empty( cObjName )
         IF hb_hHasKey( ::qU:qObj, cObjName )
            ::execEvent( "child_object", ::qU:qObj[ cObjName ], cObjName )
         ENDIF
         IF ! empty( cAction )
            qList := ::qTree:findItems( cAction, Qt_MatchExactly, 0 )
            ::qTree:setCurrentItem( qList:at( 0 ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:openUi( cUI )

   ::show()
   ::oIde:oUiSrcDock:show()
   ::clear()
   ::buildUiWidget( cUI )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildUiWidget( cUI )
   LOCAL cPath, cName, cExt, cBuffer, cPrg, aPrg

   hb_fNameSplit( cUI, @cPath, @cName, @cExt )

   ::runHbmk2( cUI )

   cPrg := cPath + "uic_" + cName + ".prg"
   IF ! hb_fileExists( cPrg )
      RETURN Self
   ENDIF
   cBuffer := hb_memoRead( cPrg )

   cBuffer := strtran( cBuffer, "hbqtui_", "hbide_ui_" )
   ferase( cPrg )

   IF !( hb_eol() == Chr( 10 ) )
      cBuffer := StrTran( cBuffer, hb_eol(), Chr( 10 ) )
   ENDIF
   IF !( hb_eol() == Chr( 13 ) + Chr( 10 ) )
      cBuffer := StrTran( cBuffer, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
   ENDIF
   aPrg := hb_aTokens( cBuffer, chr( 10 ) )

   ::buildWidget( cBuffer, cPath, cName, cExt, aPrg )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildWidget( cBuffer, cPath, cName, cExt, aPrg )
   LOCAL cCode, s, n, cObj, cCls, i, pHrb, oObj

   cBuffer := hb_compileFromBuf( cBuffer, "-n2", "-w3", "-es2", "-q0", "-i" + ::oINI:getHarbourPath() + "include" )
   IF ! empty( cBuffer )
      pHrb := hb_hrbLoad( HB_HRB_BIND_OVERLOAD, cBuffer )
      IF ! empty( pHrb )
         ::checkUpdates()
         ::clear()

         ::aPrg  := aPrg
         ::pHrb  := pHrb
         ::cPath := lower( cPath )
         ::cName := lower( cName )
         ::cExt  := lower( cExt  )
         ::cSrcFile := ::cPath + "cls_" + ::cName + ".prg"

         ::qU := eval( &( "{|q| " +  "hbide_ui_" + cName + "( q ) }" ) )
         IF HB_ISOBJECT( ::qU )
            ::loadSource()

            ::qMdiSub := QMdiSubWindow()
            ::qMdiSub:setWidget( ::qU:oWidget )
            ::qMdiSub:resize( ::qU:width()+8, ::qU:height()+40 )
            ::qMdiArea:addSubWindow( ::qMdiSub )
            ::qMdiSub:show()
            ::qMdiSub:move( 10,10 )
            ::aStatusPnls[ PNL_UI ]:setText( "<font color = red  >UI: " + cName + "</font>" )

            FOR i := 1 to len( aPrg )
               cCode := aPrg[ i ]
               IF " := " $ cCode
                  IF ! ( "oRootWidget" $ cCode ) .AND. ! ( "LOCAL" $ cCode )
                     s := substr( cCode, 1, at( " := ", cCode ) )
                     n := at( '"', s )
                     s := substr( s, n+1 )
                     n := at( '"', s )
                     cObj := substr( s, 1, n-1 )
                     oObj := ::qU:qObj[ cObj ]
                     cCls := __objGetClsName( oObj )
                     IF ! ( cCls $ "QSIZEPOLICY,QFONT,QGRIDLAYOUT,QHBOXLAYOUT,QVBOXLAYOUT,QSPACERITEM,QLAYOUT,QSPLITTER,QSCROLLAREA,QTREEWIDGETITEM,QLISTWIDGETITEM" )
                        aadd( ::aObjByName, cObj )
                        oObj:setObjectName( cObj )

                        IF ( cCls $ "QLINEEDIT" )
                           oObj:setFocusPolicy( Qt_NoFocus )
                        ENDIF

                        SWITCH cCls
                        CASE "QPLAINTEXTEDIT"
                        CASE "QTEXTEDIT"
                           oObj:connect( QEvent_FocusIn, getObject( Self, ::qU, cObj ) )
                           oObj:setCursorWidth( 0 )
                           EXIT
                        OTHERWISE
                           oObj:connect( QEvent_MouseButtonRelease, getObject( Self, ::qU, cObj ) )
                        ENDSWITCH

                        ::hObjects[ cObj ] := UISrcData():new( oObj, cObj )
                     ENDIF
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:runHbmk2( cUI )
   LOCAL cPath, cName, cExt, cExeHbMk2, cCmdParams, cCmd, cC, cBuf, fhnd, cHbpFileName, cHbpFile, cBatch
   LOCAL aHbp := {}, aHbp0 := {}
   LOCAL cbRed := "<font color=blue>", ceRed := "</font>"

   hb_fNameSplit( cUI, @cPath, @cName, @cExt )

   aadd( aHbp, "-hbraw"       )
   aadd( aHbp, "-trace"       )

   aadd( aHbp0, "-q"          )
   aadd( aHbp0, "-info"       )
   aadd( aHbp0, "-rebuild"    )
   aadd( aHbp0, "-s"          )
   aadd( aHbp0, "-hblib"      )
   aadd( aHbp0, "-workdir=" + cPath )
   aadd( aHbp0, "hbqt.hbc"    )
   aadd( aHbp0, cUI           )

   ::oOutputResult:oWidget:clear()

   IF .t.
      ::oOutputResult:oWidget:append( hbide_outputLine() )

      IF ( fhnd := hb_FTempCreateEx( @cHbpFileName, NIL, NIL, ".hbp" ) ) != F_ERROR
         cHbpFile := ""
         FOR EACH cBuf IN aHbp0
            cHbpFile += cBuf + hb_eol()
         NEXT
         FWrite( fhnd, cHbpFile )
         FClose( fhnd )
      ELSE
         RETURN Self
      ENDIF

      ::oIDE:oEV := IdeEnvironments():new():create( ::oIDE )
      cBatch   := ::oEV:prepareBatch( ::cWrkEnvironment )
      aeval( ::oEV:getHbmk2Commands( ::cWrkEnvironment ), {|e| aadd( aHbp, e ) } )

      cExeHbMk2  := ::oINI:getHbmk2File()
      cCmdParams := hbide_array2cmdParams( aHbp )

      ::oProcess := HbpProcess():new()
      //
      ::oProcess:output      := {|cOut| hbide_convertBuildStatusMsgToHtml( cOut, ::oOutputResult:oWidget ) }
      ::oProcess:finished    := {|nEC , nES| ::finished( nEC ,nES ) }
      ::oProcess:workingPath := cPath
      //
      cCmd := hbide_getShellCommand()
      cC   := iif( hbide_getOS() == "nix", "", "/E:20000 /C " )

      IF hb_fileExists( cBatch )
         cBuf := memoread( cBatch )
         cBuf += "SET " + hb_eol()
         cBuf += cExeHbMk2 + " " + cHbpFileName + " " + cCmdParams + hb_eol()
         hb_memowrit( cBatch, cBuf )
      ENDIF
      //
      ::outputText( cbRed + "Batch File " + iif( hb_fileExists( cBatch ), " : Exists", " : doesn't Exist" ) + " => " + ceRed + trim( cBatch ) )
      ::outputText( cbRed + "Batch File Contents => " + ceRed )
      ::outputText( memoread( cBatch ) )
      ::outputText( cbRed + "Command => " + ceRed + cCmd )
      ::outputText( cbRed + "Arguments => " + ceRed + cC + cBatch )
      ::outputText( hbide_outputLine() )
      //
      ::oProcess:addArg( cC + cBatch )
      ::oProcess:start( cCmd )
      ::oProcess:waitForFinished()
      ferase( cHbpFileName )
      ferase( cBatch )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:finished( nExitCode, nExitStatus )
   LOCAL cTmp

   ::outputText( hbide_outputLine() )
   cTmp := "Exit Code [ " + hb_ntos( nExitCode ) + " ]    Exit Status [ " + hb_ntos( nExitStatus ) + " ]    " +;
           "Finished at [ " + time() + " ]    Done in [ " + hb_ntos( seconds() - ::oProcess:started ) + " Secs ]"
   ::outputText( cTmp )
   ::outputText( hbide_outputLine() )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:outputText( cText )
   ::oOutputResult:oWidget:append( "<font color=black>" + cText + "</font>" )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildToolbar()
   LOCAL nW := 25

   ::sp0 := QLabel()
   ::sp0:setMinimumWidth( nW )

   ::qToolbar := QToolbar()
   ::qToolbar:setIconSize( QSize( 16,16 ) )
   ::qToolbar:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )

   ::buildToolButton( ::qToolbar, { "Open a .UI"  , "open3"  , {|| ::execEvent( "buttonOpen_clicked"  ) }, .f. } )
   ::qToolbar:addWidget( ::sp0 )
   ::buildToolButton( ::qToolbar, { "Build Source", "fileprg", {|| ::execEvent( "buttonBuild_clicked" ) }, .f. } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildToolButton( qToolbar, aBtn )
   LOCAL qBtn

   IF empty( aBtn )
      qToolbar:addSeparator()
   ELSE
      qBtn := QToolButton()
      qBtn:setTooltip( aBtn[ 1 ] )
      qBtn:setAutoRaise( .t. )
      qBtn:setIcon( QIcon( hbide_image( aBtn[ 2 ] ) ) )
      IF aBtn[ 4 ]
         qBtn:setCheckable( .t. )
      ENDIF
      qBtn:connect( "clicked()",  aBtn[ 3 ] )
      qToolBar:addWidget( qBtn )
      aadd( ::aToolBtns, qBtn )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildStatusPanels()
   LOCAL qLabel

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 0 )
   aadd( ::aStatusPnls, qLabel )

   qLabel := QLabel(); qLabel:setMinimumWidth( 40 )
   ::qStatus:addPermanentWidget( qLabel, 1 )
   aadd( ::aStatusPnls, qLabel )

   RETURN Self

/*------------------------------------------------------------------------*/

STATIC FUNCTION getObject( oSelf, oHbQtUi, cObj )
   RETURN {|...| oSelf:execEvent( "child_object", oHbQtUi:qObj[ cObj ], cObj, ... ) }

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:loadSource()

   IF hb_fileExists( ::cSrcFile )
      ::aSource := hbide_readSource( ::cSrcFile )
   ENDIF
   ::buildSource()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildSource()
   LOCAL qHScr, qVScr, qCursor, qCurPos, qHVal, qVVal, qEdit

   IF empty( ::aSource )
      ::aSource := ::buildClassSkeleton( 'ui_' + ::cName, ::cName )
   ENDIF

   ::cSource := ""
   aeval( ::aSource, {|e| ::cSource += e + hb_eol() } )

   hb_memowrit( ::cSrcFile, ::cSource )

   ::oSM:editSource( ::cSrcFile, 0, 0, 0, NIL, NIL, .f., .t. )
   IF ::oEM:isOpen( ::cSrcFile )
      qEdit   := ::oEM:getEditCurrent()
      qHScr   := qEdit:horizontalScrollBar()
      qVScr   := qEdit:verticalScrollBar()
      qCursor := qEdit:textCursor()

      qCurPos := qCursor:position()
      qHVal   := qHScr:value()
      qVVal   := qVScr:value()

      ::oEM:reLoad( ::cSrcFile )

      qCursor := qEdit:textCursor()
      qCursor:setPosition( qCurPos )
      qEdit:setTextCursor( qCursor )
      qHScr:setValue( qHVal )
      qVScr:setValue( qVVal )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUISrcManager:buildClassSkeleton( cCls, cUiName )
   LOCAL aSrc := {}
   LOCAL cClsC := cCls + ":"

   aadd( aSrc, '/*' )
   aadd( aSrc, ' * $Id$' )
   aadd( aSrc, ' */' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*' )
   aadd( aSrc, ' <CLASS> . Do not edit lines in this section!' )
   aadd( aSrc, ' NAME = ' + cCls )
   aadd( aSrc, ' </CLASS>' )
   aadd( aSrc, ' */' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, '#include "hbclass.ch"' )
   aadd( aSrc, '#include "error.ch"' )
   aadd( aSrc, '#include "hbqtgui.ch"' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'CREATE CLASS ' + cCls )
   aadd( aSrc, '' )
   aadd( aSrc, '   VAR    oParent' )
   aadd( aSrc, '' )
   aadd( aSrc, '   /* <METHODSCOMMON> . Do not edit lines in this section! */' )
   aadd( aSrc, '   METHOD new( oParent )' )
   aadd( aSrc, '   METHOD create( oParent )' )
   aadd( aSrc, '   METHOD destroy()' )
   aadd( aSrc, '   METHOD connects()' )
   aadd( aSrc, '   METHOD disconnects()' )
   aadd( aSrc, '   ERROR HANDLER __OnError( ... )' )
   aadd( aSrc, '   /* </METHODSCOMMON> */' )
   aadd( aSrc, '' )
   aadd( aSrc, '   /* <METHODSEVENTS> . Do not edit lines in this section! */' )
   aadd( aSrc, '   /* </METHODSEVENTS> */' )
   aadd( aSrc, '' )
   aadd( aSrc, '   PROTECTED:' )
   aadd( aSrc, '' )
   aadd( aSrc, '   VAR    oUI' )
   aadd( aSrc, '' )
   aadd( aSrc, 'ENDCLASS' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + 'new( oParent )' )
   aadd( aSrc, '' )
   aadd( aSrc, '   hb_default( @oParent, ::oParent )' )
   aadd( aSrc, '   ::oParent := oParent' )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN Self' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + 'create( oParent )' )
   aadd( aSrc, '' )
   aadd( aSrc, '   hb_default( @oParent, ::oParent )' )
   aadd( aSrc, '   ::oParent := oParent' )
   aadd( aSrc, '' )
   aadd( aSrc, '   ::oUI := hbqtui_' + cUiName + '( ::oParent )' )
   aadd( aSrc, '' )
   aadd( aSrc, '   ::connects()' )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN Self' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + 'destroy()' )
   aadd( aSrc, '' )
   aadd( aSrc, '   IF HB_ISOBJECT( ::oUI )' )
   aadd( aSrc, '      ::disconnects()' )
   aadd( aSrc, '      ::oUI:destroy()' )
   aadd( aSrc, '   ENDIF'  )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN Self' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + '__OnError( ... )' )
   aadd( aSrc, '   LOCAL cMsg := __GetMessage()' )
   aadd( aSrc, '   LOCAL oError' )
   aadd( aSrc, '' )
   aadd( aSrc, '   IF SubStr( cMsg, 1, 1 ) == "_"' )
   aadd( aSrc, '      cMsg := SubStr( cMsg, 2 )' )
   aadd( aSrc, '   ENDIF' )
   aadd( aSrc, '' )
   aadd( aSrc, '   IF Left( cMsg, 2 ) == "Q_"' )
   aadd( aSrc, '      IF SubStr( cMsg, 3 ) $ ::oUI:qObj' )
   aadd( aSrc, '         RETURN ::oUI:qObj[ SubStr( cMsg, 3 ) ]' )
   aadd( aSrc, '      ELSE' )
   aadd( aSrc, '         oError := ErrorNew()' )
   aadd( aSrc, '' )
   aadd( aSrc, '         oError:severity    := ES_ERROR' )
   aadd( aSrc, '         oError:genCode     := EG_ARG' )
   aadd( aSrc, '         oError:subSystem   := "HBQT"' )
   aadd( aSrc, '         oError:subCode     := 1001' )
   aadd( aSrc, '         oError:canRetry    := .F.' )
   aadd( aSrc, '         oError:canDefault  := .F.' )
   aadd( aSrc, '         oError:Args        := hb_AParams()' )
   aadd( aSrc, '         oError:operation   := ProcName()' )
   aadd( aSrc, '         oError:Description := "Control <" + substr( cMsg, 3 ) + "> does not exist"' )
   aadd( aSrc, '' )
   aadd( aSrc, '         Eval( ErrorBlock(), oError )' )
   aadd( aSrc, '      ENDIF' )
   aadd( aSrc, '   ELSEIF ::oUI:oWidget:hasValidPointer()' )
   aadd( aSrc, '      RETURN ::oUI:oWidget:&cMsg( ... )' )
   aadd( aSrc, '   ENDIF' )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN NIL' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + 'connects()' )
   aadd( aSrc, '' )
   aadd( aSrc, '   /* <CONNECTS> . Do not edit lines in this section! */' )
   aadd( aSrc, '   /* </CONNECTS> */' )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN Self' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )
   aadd( aSrc, 'METHOD ' + cClsC + 'disconnects()' )
   aadd( aSrc, '' )
   aadd( aSrc, '   /* <DISCONNECTS> . Do not edit lines in this section! */' )
   aadd( aSrc, '   /* </DISCONNECTS> */' )
   aadd( aSrc, '' )
   aadd( aSrc, '   RETURN Self' )
   aadd( aSrc, '' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '/* <EVENTSMETHODAREA> . Do not edit method names in this section, but can edit method body! */' )
   aadd( aSrc, '/* </EVENTSMETHODAREA> */' )
   aadd( aSrc, '/*----------------------------------------------------------------------*/' )
   aadd( aSrc, '' )

   RETURN aSrc

/*----------------------------------------------------------------------*/

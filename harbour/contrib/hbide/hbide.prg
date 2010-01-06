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
/*
 *     Many thanks to Vailton Renato for adding new functionalities.
 */
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

REQUEST HB_QT

STATIC s_resPath
STATIC s_pathSep

/*----------------------------------------------------------------------*/

PROCEDURE Main( cProjIni )
   LOCAL oIde

   SET CENTURY ON
   SET EPOCH TO 1970

   //HBQT_SET_RELEASE_METHOD( HBQT_RELEASE_WITH_DESTRUTOR )             // Exits cleanly
   HBQT_SET_RELEASE_METHOD( HBQT_RELEASE_WITH_DELETE )                // Exits cleanly
   //HBQT_SET_RELEASE_METHOD( HBQT_RELEASE_WITH_DELETE_LATER )          // Exits cleanly

   s_resPath := hb_DirBase() + "resources" + hb_OsPathSeparator()
   s_pathSep := hb_OsPathSeparator()

   oIde := HbIde():new( cProjIni ):create()
   oIde:destroy()

   RETURN

/*----------------------------------------------------------------------*/

CLASS HbIde

   ACCESS pSlots                                  INLINE hbxbp_getSlotsPtr()
   ACCESS pEvents                                 INLINE hbxbp_getEventsPtr()

   DATA   oPM
   DATA   oFR
   DATA   oDK
   DATA   oED
   DATA   oAC

   DATA   mp1, mp2, oXbp, nEvent
   DATA   aTabs                                   INIT {}
   DATA   cProjIni

   DATA   oCurTab                                 INIT NIL
   DATA   nCurTab                                 INIT 0
   DATA   aIni                                    INIT {}

   /* HBQT Objects */
   DATA   qLayout
   DATA   qTabWidget
   DATA   qFindDlg

   ACCESS oCurEditor                              INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), TAB_OEDITOR ], NIL )
   ACCESS qCurEdit                                INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), TAB_QEDIT ], NIL )
   ACCESS qCurDocument                            INLINE iif( ::getCurrentTab() > 0, ::aTabs[ ::getCurrentTab(), TAB_QDOCUMENT ], NIL )
   ACCESS qCurCursor                              INLINE ::getCurCursor()

   DATA   qCursor
   DATA   qFontWrkProject
   DATA   qBrushWrkProject

   /* XBP Objects */
   DATA   oDlg
   DATA   oDa
   DATA   oSBar
   DATA   oMenu
   DATA   oTBar
   DATA   oFont
   DATA   oProjTree
   DATA   oEditTree
   DATA   oDockR
   DATA   oDockB
   DATA   oDockB1
   DATA   oDockB2
   DATA   oDockPT
   DATA   oDockED
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

   DATA   resPath                                 INIT hb_DirBase() + "resources" + hb_OsPathSeparator()
   DATA   pathSep                                 INIT hb_OsPathSeparator()

   DATA   aTags                                   INIT {}
   DATA   aText                                   INIT {}
   DATA   aSources                                INIT {}
   DATA   aFuncList                               INIT {}
   DATA   aLines                                  INIT {}
   DATA   aComments                               INIT {}
   DATA   aProjects                               INIT {}
   DATA   cWrkProject                             INIT ''
   DATA   cWrkTheme                               INIT ''
   DATA   oProps

   DATA   cProcessInfo
   DATA   qProcess

   DATA   aEdits                                  INIT {}

   DATA   cIniThemes
   DATA   oThemes


   METHOD new( cProjectOrSource )
   METHOD create( cProjectOrSource )
   METHOD destroy()

   METHOD setPosAndSizeByIni()
   METHOD setPosByIni()

   METHOD execAction()
   METHOD manageFuncContext()
   METHOD manageProjectContext()

   METHOD loadSources()
   METHOD editSource()
   METHOD selectSource()
   METHOD closeSource()
   METHOD closeAllSources()
   METHOD closeAllOthers()
   METHOD saveSource()
   METHOD saveAllSources()
   METHOD saveAndExit()
   METHOD revertSource()

   METHOD updateFuncList()
   METHOD gotoFunction()

   METHOD updateProjectMenu()
   METHOD updateProjectTree()

   METHOD manageItemSelected()
   METHOD getCurrentTab()
   METHOD getCurCursor()
   METHOD addSourceInTree()

   METHOD createTags()

   METHOD manageFocusInEditor()
   METHOD loadUI()

   METHOD setCodec()

   METHOD updateTitleBar()

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
   /* Setup GUI Error Reporting System*/
   hbqt_errorsys()

   /* Initialte Project Manager */
   ::oPM := IdeProjManager():new( Self ):create()

   /* Load IDE Settings */
   hbide_loadINI( Self, cProjIni )

   /* Setup DOCKing windows and ancilliary windows */
   ::oDK := IdeDocks():new():create( Self )
   /* Build IDE's Main Window */
   ::oDK:buildDialog()
   /* Build Actions */
   ::oAC := IdeActions():new( Self ):create()
   /* Build Toolbar */
   ::oAC:buildToolBar()
   /* Build Main Menu */
   ::oAC:buildMainMenu()
   //::oDK:buildMainMenu()
   ::oDK:buildStatusBar()
   ::oDK:buildDockWidgets()

   /* Once create Find/Replace dialog */
   ::oFR := IdeFindReplace():new():create( Self )

   /* Edits Manager */
   ::oED := IdeEditsManager():new( Self ):create()

   /* Load IDE|User defined Themes */
   hbide_loadThemes( Self )

   /* Prepare Editor's Tabs */
   ::oDa:oTabWidget := XbpTabWidget():new():create( ::oDa, , {0,0}, {10,10}, , .t. )
   ::oDa:oTabWidget:oWidget:setUsesScrollButtons( .f. )
   ::oTabWidget := ::oDa:oTabWidget
   ::qTabWidget := ::oDa:oTabWidget:oWidget

   /* Attach GRID Layout to Editor Area - Futuristic */
   ::qLayout := QGridLayout():new()
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setHorizontalSpacing( 0 )
   ::qLayout:setVerticalSpacing( 0 )
   //
   ::oDa:oWidget:setLayout( ::qLayout )
   //
   ::qLayout:addWidget_1( ::oDa:oTabWidget:oWidget, 0, 0, 1, 1 )

   /* Just to spare some GC calls */
   ::qCursor := QTextCursor():new()
   ::qBrushWrkProject := QBrush():new( "QColor", QColor():new( 255,0,0 ) )

   /* Editor's Font - TODO: User Managed Interface */
   ::oFont := XbpFont():new()
   ::oFont:fixed := .t.
   ::oFont:create( "10.Courier" )

   /* Request Main Window to Appear on the Screen */
   ::oDlg:Show()

   /* Fill various elements of the IDE */
   ::cWrkProject := ::aINI[ INI_HBIDE, CurrentProject ]
   ::oPM:populate()
   ::loadSources()
   ::updateProjectMenu()
   ::updateTitleBar()
   /* Set some last settings */
   ::oPM:setCurrentProject( ::cWrkProject, .f. )
   ::cWrkTheme := ::aINI[ INI_HBIDE, CurrentTheme ]

   DO WHILE .t.
      ::nEvent := AppEvent( @::mp1, @::mp2, @::oXbp )

      IF ::nEvent == xbeP_Quit
         HBXBP_DEBUG( "xbeP_Quit" )
         hbide_saveINI( Self )
         EXIT
      ENDIF

      IF ::nEvent == xbeP_Close
         hbide_saveINI( Self )
         ::closeAllSources()
         EXIT

      ELSEIF ::nEvent == xbeP_Keyboard
         DO CASE

         CASE ::mp1 == xbeK_INS
            IF !empty( ::qCurEdit )
               ::qCurEdit:setOverwriteMode( ! ::qCurEdit:overwriteMode() )
               ::oCurEditor:dispEditInfo()
            ENDIF

         CASE ::mp1 == xbeK_ESC
            ::closeSource()

         CASE ::mp1 == xbeK_CTRL_G
            ::oED:goto()

         CASE ::mp1 == xbeK_CTRL_F
            IF !empty( ::qCurEdit )
               ::oFR:show()
            ENDIF
         CASE ::mp1 == xbeK_CTRL_N
            IF !empty( ::qCurEdit )
               ::oFR:find()
            ENDIF
         CASE ::mp1 == xbeK_CTRL_R
            IF !empty( ::qCurEdit )
               ::oFR:replace()
            ENDIF

         ENDCASE
      ENDIF

      ::oXbp:handleEvent( ::nEvent, ::mp1, ::mp2 )
   ENDDO

   ::oFR:destroy()

   /* Very important - destroy resources */
   hbide_dbg( "======================================================" )
   hbide_dbg( "Before    ::oDlg:destroy()", memory( 1001 ), hbqt_getMemUsed() )
   hbide_dbg( "                                                      " )

   ::oDlg:destroy()
   ::oAC:destroy()

   hbide_dbg( "                                                      " )
   hbide_dbg( "After     ::oDlg:destroy()", memory( 1001 ), hbqt_getMemUsed() )
   hbide_dbg( "======================================================" )

   ::qCursor:pPtr := 0
   ::oFont        := NIL

   hbide_dbg( "EXITING after destroy ....", memory( 1001 ), hbqt_getMemUsed() )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:execAction( cKey )
   LOCAL aPrj, cHbi, cTmp, n, aSrc

   DO CASE
   CASE cKey == "Exit"
      PostAppEvent( xbeP_Close, NIL, NIL, ::oDlg )

   CASE cKey == "NewProject"
      ::oPM:loadProperties( , .t., .t., .t. )
   CASE cKey == "LoadProject"
      ::oPM:loadProperties( , .f., .f., .t. )
   CASE cKey == "LaunchProject"
      ::oPM:launchProject()
   CASE cKey == "Build"
      ::oPM:buildProject( '', .F., .F. )
   CASE cKey == "BuildLaunch"
      ::oPM:buildProject( '', .T., .F. )
   CASE cKey == "Rebuild"
      ::oPM:buildProject( '', .F., .T. )
   CASE cKey == "RebuildLaunch"
      ::oPM:buildProject( '', .T., .T. )
   CASE cKey == "Compile"
      //
   CASE cKey == "CompilePPO"
      ::oPM:buildProject( '', .F., .F., .T. )
   CASE cKey == "Properties"
      IF Empty( ::cWrkProject )
         MsgBox( 'No active project detected!' )
      ENDIF
      cTmp := ::oPM:getCurrentProject()
      IF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cTmp } ) ) > 0
         aPrj := ::aProjects[ n, 3 ]
         cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + ::pathSep + ;
                 aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"

         ::oPM:loadProperties( cHbi, .f., .t., .t. )
      ELSE
         MsgBox( 'Invalid project: ' + cTmp )
      ENDIF
   CASE cKey == "SelectProject"
      ::oPM:selectCurrentProject()
   CASE cKey == "CloseProject"
      ::oPM:closeProject()

   CASE cKey == "New"
      ::editSource( '' )
   CASE cKey == "Open"
      IF !empty( aSrc := ::selectSource( "openmany" ) )
         aEval( aSrc, {|e| ::editSource( e ) } )
      ENDIF
   CASE cKey == "Save"
      ::saveSource( ::getCurrentTab(), , .f. )
   CASE cKey == "SaveAs"
      ::saveSourceAs( ::getCurrentTab(), , .t. )
   CASE cKey == "SaveAll"
      ::saveAllSources()
   CASE cKey == "SaveExit"
      ::saveAndExit()
   CASE cKey == "Revert"
      ::RevertSource()
   CASE cKey == "Close"
      ::closeSource()
   CASE cKey == "CloseAll"
      ::closeAllSources()
   CASE cKey == "CloseOther"
      ::closeAllOthers()

   CASE cKey == "Print"
      ::oED:printPreview()
   CASE cKey == "Undo"
      ::oED:undo()
   CASE cKey == "Redo"
      ::oED:redo()
   CASE cKey == "Cut"
      ::oED:cut()
   CASE cKey == "Copy"
      ::oED:copy()
   CASE cKey == "Paste"
      ::oED:paste()
   CASE cKey == "SelectAll"
      ::oED:selectAll()

   CASE cKey == "switchReadOnly"
      ::oED:switchToReadOnly()
   CASE cKey == "Find"
      IF !Empty( ::qCurEdit )
         ::oFR:show()
      ENDIF
   CASE cKey == "SetMark"
      //
   CASE cKey == "GotoMark"
      //
   CASE cKey == "Goto"
      ::oED:goto()
   CASE cKey == "ToUpper"
      ::oED:convertSelection( cKey )
   CASE cKey == "ToLower"
      ::oED:convertSelection( cKey )
   CASE cKey == "Invert"
      ::oED:convertSelection( cKey )
   CASE cKey == "InsertDateTime"
      ::oED:insertText( cKey )
   CASE cKey == "InsertRandomName"
      ::oED:insertText( cKey )
   CASE cKey == "InsertExternalFile"
      ::oED:insertText( cKey )
   CASE cKey == "ZoomIn"
      ::oED:zoom( cKey )
   CASE cKey == "ZoomOut"
      ::oED:zoom( cKey )

   CASE cKey == "ToggleProjectTree"
      ::oDK:toggleLeftDocks()
   CASE cKey == "ToggleBuildInfo"
      ::oDK:toggleBottomDocks()
   CASE cKey == "ToggleFuncList"
      ::oDK:toggleRightDocks()
   ENDCASE

   ::manageFocusInEditor()

   RETURN nil

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

METHOD HbIde:setPosByIni( qWidget, nPart )
   LOCAL aRect

   IF !empty( ::aIni[ INI_HBIDE, nPart ] )
      aRect := hb_atokens( ::aIni[ INI_HBIDE, nPart ], "," )
      aeval( aRect, {|e,i| aRect[ i ] := val( e ) } )

      qWidget:move( aRect[ 1 ], aRect[ 2 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageFocusInEditor()

   IF ::getCurrentTab() > 0
      ::aTabs[ ::getCurrentTab(), TAB_QEDIT ]:setFocus()
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD HbIde:getCurCursor()
   LOCAL iTab

   IF ( iTab := ::getCurrentTab() ) > 0
      ::qCursor:configure( ::aTabs[ iTab, TAB_OTAB ]:textCutsor() )
   ENDIF

   RETURN ::qCursor

/*----------------------------------------------------------------------*/
//                          Source Editor
/*----------------------------------------------------------------------*/

METHOD HbIde:loadSources()
   LOCAL a_

   IF !empty( ::aIni[ INI_FILES ] )
      FOR EACH a_ IN ::aIni[ INI_FILES ]
         ::editSource( a_[ 1 ], a_[ 2 ], a_[ 3 ], a_[ 4 ], a_[ 5 ] )
      NEXT
      ::oED:setSourceVisibleByIndex( val( ::aIni[ INI_HBIDE, RecentTabIndex ] ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 *   Save selected Tab on harddisk and return .T. if successfull!
 */
METHOD HbIde:saveSource( nTab, lCancel, lAs )
   LOCAL oEdit, lNew, cBuffer, qDocument, nIndex, cSource, cFile, cExt, cNewFile
   LOCAL cFileToSave

   DEFAULT nTab TO ::getCurrentTab()
   DEFAULT lAs  TO .F.

   lCancel := .F.

   IF !empty( oEdit := ::oED:getEditorByTabPosition( nTab ) )
      cSource := oEdit:sourceFile
      lNew := Empty( cSource ) .OR. lAs
      IF lNew
         cNewFile := ::selectSource( 'save', ;
                                    iif( !Empty( cSource ), cSource, hb_dirBase() + "projects\" ),;
                                           "Save " + oEdit:oTab:caption + " as..." )
         IF empty( cNewFile )
            // will check later what decision to take
            RETURN .f.
         ENDIF
         IF hbide_pathNormalized( cNewFile ) == hbide_pathNormalized( cSource )
            lNew := .f.
         ENDIF
      ENDIF

      cFileToSave := iif( lNew, cNewFile, cSource )
      qDocument := oEdit:qDocument

      cBuffer := oEdit:qEdit:toPlainText()
      /*
       * If the burn process fails, we should change the name of the previous file.
       * 01/01/2010 - 21:24:41 - vailtom
       */
      IF !hb_memowrit( cFileToSave, cBuffer )
         MsgBox( "Error saving the file " + oEdit:sourceFile + ".",, 'Error saving file!' )
         lCancel := .T.
         RETURN .F.
      ENDIF

      IF lNew
         hb_fNameSplit( cFileToSave, , @cFile, @cExt )

         ::aTabs[ nTab, TAB_SOURCEFILE ] := cFileToSave
         oEdit:sourceFile                := cFileToSave

         oEdit:oTab:Caption := cFile + cExt
         ::qTabWidget:setTabText( nIndex, cFile + cExt )
         ::qTabWidget:setTabTooltip( nIndex, cSource )
      ENDIF

      IF empty( cSource )
         /* The file is not populated in editors tree. Inject */
         ::addSourceInTree( oEdit:sourceFile )
      ELSEIF lNew
         /* Rename the existing nodes in tree */
      ENDIF

      qDocument:setModified( .f. )
      ::aSources := { oEdit:sourceFile }
      ::createTags()
      ::updateFuncList()
      nIndex := ::qTabWidget:indexOf( oEdit:oTab:oWidget )
      ::qTabWidget:setTabIcon( nIndex, ::resPath + "tabunmodified.png" )
      ::oSBar:getItem( SB_PNL_MODIFIED ):caption := " "
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/

METHOD HbIde:editSource( cSourceFile, nPos, nHPos, nVPos, cTheme, lAlert )

   DEFAULT lAlert TO .T.

   IF !Empty( cSourceFile )
      IF !( hbide_isValidText( cSourceFile ) )
         MsgBox( 'File type unknown or unsupported: ' + cSourceFile )
         RETURN Self
      ELSEIF !hb_FileExists( cSourceFile )
         MsgBox( 'File not found: ' + cSourceFile )
         RETURN Self
      ENDIF
      IF ::oED:isOpen( cSourceFile )
         IF lAlert
            IF hbide_getYesNo( cSourceFile + " is already open.", ;
                                        "Want to re-load it again ?", "File Open Info!" )
               ::oED:reLoad( cSourceFile )
            ENDIF
         ENDIF
         ::oED:setSourceVisible( cSourceFile )
         RETURN Self
      ENDIF
   ENDIF

   DEFAULT nPos  TO 0
   DEFAULT nHPos TO 0
   DEFAULT nVPos TO 0

   ::oED:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme )

   IF !Empty( cSourceFile )
      hbide_mnuAddFileToMRU( Self, cSourceFile, INI_RECENTFILES )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:closeSource( nTab, lCanCancel, lCanceled )
   LOCAL lSave, n, oEdit

   DEFAULT nTab TO ::getCurrentTab()

   IF !empty( oEdit := ::oED:getEditorByTabPosition( nTab ) )

      DEFAULT lCanCancel TO .F.
      lCanceled := .F.

      IF !( oEdit:qDocument:isModified() )
         * File has not changed, ignore the question to User
         lSave := .F.

      ELSEIF lCanCancel
         n := hbide_getYesNoCancel( oEdit:oTab:Caption, "Has been modified, save this source?", 'Save?' )
         IF ( lCanceled := ( n == QMessageBox_Cancel ) )
            RETURN .F.
         ENDIF
         lSave := ( n == QMessageBox_Yes    )

      ELSE
         lSave := hbide_getYesNo( oEdit:oTab:Caption, "Has been modified, save this source?", 'Save?' )

      ENDIF

      IF lSave .AND. !( ::saveSource( nTab, @lCanceled ) )
         IF lCanCancel
            RETURN .F.
         ENDIF
      ENDIF

      oEdit:removeTabPage()
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Close all opened files.
 * 02/01/2010 - 15:31:44
 */
METHOD HbIde:closeAllSources()
   LOCAL lCanceled
   LOCAL i := 0

   DO WHILE ( ++i <= Len( ::aTabs ) )

       IF ::closeSource( i, .T., @lCanceled )
          i --
          Loop
       ENDIF

       IF lCanceled
          RETURN .F.
       ENDIF
   ENDDO

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Close all opened files except current.
 * 02/01/2010 - 15:47:19
 */
METHOD HbIde:closeAllOthers( nTab )
   LOCAL lCanceled, a_

   FOR EACH a_ IN ::aTabs
      IF a_:__enumIndex() != nTab
         ::closeSource( a_:__enumIndex(), .T., @lCanceled )
         IF lCanceled
            RETURN .f.
         ENDIF
      ENDIF
   NEXT

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Save all opened files...
 * 01/01/2010 - 22:44:36 - vailtom
 */
METHOD HbIde:saveAllSources()
   LOCAL n

   FOR n := 1 TO Len( ::aTabs )
      ::saveSource( n )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Save current file and exits HBIDE
 * 02/01/2010 - 18:45:06 - vailtom
 */
METHOD HbIde:saveAndExit()

   IF ::saveSource()
      ::execAction( "Exit" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Revert current file to a previous saved file.
 * 02/01/2010 - 19:45:34
 */
METHOD HbIde:revertSource( nTab )

   DEFAULT nTab TO ::getCurrentTab()

   IF nTab < 1
      RETURN .F.
   End

   IF !::aTabs[ nTab, TAB_QDOCUMENT ]:isModified()
      * File has not changed, ignore the question to User
   ELSE
      IF !hbide_getYesNo( 'Revert ' + ::aTabs[ nTab, TAB_OTAB ]:Caption + '?',  ;
                    'The file ' + ::aTabs[ nTab, TAB_SOURCEFILE ] + ' has changed. '+;
                    'Discard current changes and revert contents to the previously saved on disk?', 'Revert file?' )
         RETURN Self
      ENDIF
   ENDIF

   ::aTabs[ nTab, TAB_QEDIT ]:setPlainText( hb_memoRead( ::aTabs[ nTab, TAB_SOURCEFILE ] ) )
   ::aTabs[ nTab, TAB_QEDIT ]:ensureCursorVisible()
   ::manageFocusInEditor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:selectSource( cMode, cFile, cTitle )
   LOCAL oDlg, cPath

   oDlg := XbpFileDialog():new():create( ::oDa, , { 10,10 } )
   IF cMode == "open"
      oDlg:title       := "Select a Source File"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "All Files"  , "*.*"   }, { "PRG Sources", "*.prg" }, { "C Sources" , "*.c"  },;
                            { "CPP Sources", "*.cpp" }, { "H Headers"  , "*.h"   }, { "CH Headers", "*.ch" } }
      cFile := oDlg:open( , , .f. )

   ELSEIF cMode == "openmany"
      oDlg:title       := "Select Sources"
      oDlg:center      := .t.
      oDlg:defExtension:= 'prg'
      oDlg:fileFilters := { { "All Files"  , "*.*"   }, { "PRG Sources", "*.prg" }, { "C Sources" , "*.c"  },;
                            { "CPP Sources", "*.cpp" }, { "H Headers"  , "*.h"   }, { "CH Headers", "*.ch" } }
      cFile := oDlg:open( , , .t. )

   ELSEIF cMode == "save"
      oDlg:title       := iif( !hb_isChar(cTitle), "Save as...", cTitle )
      oDlg:center      := .t.
      oDlg:defExtension:= 'prg'

      IF hb_isChar( cFile ) .AND. !Empty( cFile )
         IF Right( cFile, 1 ) $ '/\'
            cPath := cFile
         ELSE
            hb_fNameSplit( cFile, @cPath )
         Endif
      Endif

      oDlg:fileFilters := { { "PRG Sources", "*.prg" }, { "C Sources", "*.c" }, { "CPP Sources", "*.cpp" }, ;
                                                            { "H Headers", "*.h" }, { "CH Headers", "*.ch" } }
      cFile := oDlg:saveAs( cPath )

   ELSE
      oDlg:title       := "Save this Database"
      oDlg:fileFilters := { { "Database Files", "*.dbf" } }
      oDlg:quit        := {|| MsgBox( "Quitting the Dialog" ), 1 }
      cFile := oDlg:saveAs( "myfile.dbf" )
      IF !empty( cFile )
         hbide_dbg( cFile )
      ENDIF

   ENDIF

   RETURN cFile

/*----------------------------------------------------------------------*/

METHOD HbIde:getCurrentTab()
   LOCAL qTab, nTab

   qTab := ::qTabWidget:currentWidget()
   nTab := ascan( ::aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, qTab ) } )

   RETURN nTab

/*----------------------------------------------------------------------*/

METHOD HbIde:updateProjectTree( aPrj, lRemove )
   LOCAL cType, oParent, oP, aSrc, j, cProject, nPath
   LOCAL aPath, aInUse, cPath, cFile, cExt, oPP, cPathA, nPos

   DEFAULT lRemove TO .F.

   IF Empty( aPrj )
      RETURN Self
   ENDIF

   cProject := aPrj[ PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]

   IF Empty( cProject )
      RETURN Self
   ENDIF

   nPos  := aScan( ::aProjData, {|e_| e_[ TRE_TYPE ] == "Project Name" .and. e_[ TRE_ORIGINAL ] == cProject } )
   cType := aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ]

   DO CASE
   CASE cType == "Executable"
      oParent := ::aProjData[ 1, 1 ]
   CASE cType == "Lib"
      oParent := ::aProjData[ 2, 1 ]
   CASE cType == "Dll"
      oParent := ::aProjData[ 3, 1 ]
   ENDCASE

   IF !( lRemove )

    * It is a new node?
      IF nPos == 0
         oParent:expand( .t. )

         oP := oParent:addItem( cProject )
         aadd( ::aProjData, { oP, "Project Name", oParent, cProject, aPrj } )
         oParent := oP

    * Need to be an update?
      ELSE
         nPos := aScan( oParent:aChilds, {|o| o:caption == cProject } )

         IF nPos <> 00
            oParent := oParent:aChilds[ nPos ]
         ENDIF
      ENDIF

      aSrc  := aClone( aPrj[ PRJ_PRP_SOURCES, 2 ] )
      aSrc  := ASort( aSrc )
      aPath := {}
      aInUse:= {}

    * Load previous aPath used to fill ::aProjData
    * 03/01/2010 - 16:08:25 - vailtom
      FOR j := 1 TO LEN( ::aProjData )
          IF !hb_isChar( ::aProjData[ j, 5 ] ).OR. ;  // It is not a char?
             ::aProjData[ j, TRE_TYPE ] != 'Path'    .OR. ;  // Is not an path?
             ::aProjData[ j, TRE_DATA ] != cProject          // Is not from same project?
             LOOP
          ENDIF
          AAdd( aPath, { ::aProjData[ j, TRE_ORIGINAL ], ::aProjData[ j, 1 ]} )
      NEXT

    * Add new nodes with file names to tree...
      FOR j := 1 TO len( aSrc )
         hb_fNameSplit( aSrc[ j ], @cPath, @cFile, @cExt )

         cPathA := hbide_pathNormalized( cPath, .T. )
         nPath  := aScan( aPath, {|e_|  e_[ 1 ] == cPathA } )

         AAdd( aInUse, cPathA )

       * Find an node with Caption == cPathA
         IF ( nPath == 0 )
            oPP := oParent:addItem( hbide_pathNormalized( cPath, .f. ) )
            aadd( ::aProjData, { oPP, "Path", oParent, cPathA, cProject } )
            aadd( aPath, { cPathA, oPP } )
            nPath := len( aPath )
         ENDIF

         oPP   := aPath[ nPath,2 ]
         cFile := cFile + cExt
         nPos  := aScan( ::aProjData, {|e_| e_[ TRE_TYPE ] == "Source File" .AND. ;
                                            e_[ TRE_ORIGINAL ] == aSrc[ j ] } )
         IF nPos == 00
            aadd( ::aProjData, { oPP:addItem( cFile ), "Source File", oPP, aSrc[ j ], cProject } )
         ENDIF
      NEXT

    * Remove deleted nodes from tree
      FOR j := 1 TO LEN( ::aProjData )

        * Is not from same project?
          IF !hb_isChar( ::aProjData[ j, TRE_DATA ] ) .OR. ;
                ::aProjData[ j, TRE_DATA ] != cProject
             LOOP
          ENDIF

        * It is a path?
          IF ::aProjData[ j, 2 ] == 'Path'
             cPathA := ::aProjData[ j, TRE_ORIGINAL ]
             nPath  := aScan( aInUse, {|e_|  e_ == cPathA } )

             IF nPath == 00
                ::aProjData[ j, TRE_OPARENT ]:delItem( ::aProjData[ j, 1 ] )
                hb_aDel( ::aProjData, j, .T. )
             ENDIF

             LOOP
          ENDIF

        * It is a filename?
          IF ::aProjData[ j, 2 ] == 'Source File'
             cFile := ::aProjData[ j, TRE_ORIGINAL ]
             nPos  := aScan( aSrc, {|e_|  e_ == cFile } )

             IF nPos == 00
                ::aProjData[ j, TRE_OPARENT ]:delItem( ::aProjData[ j, TRE_OITEM ] )
                hb_aDel( ::aProjData, j, .T. )
             ENDIF

             LOOP
          ENDIF
      NEXT

   ELSE

      IF nPos != 0
         nPos := aScan( oParent:aChilds, {|e_| e_:Caption == cProject } )

         IF nPos > 0
            oParent:delItem( oParent:aChilds[ nPos ] )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:addSourceInTree( cSourceFile )
   LOCAL cPath, cPathA, cFile, cExt, n, oParent
   LOCAL oGrand := ::oOpenedSources

   IF Empty( cSourceFile )
      RETURN nil
   End

   hb_fNameSplit( cSourceFile, @cPath, @cFile, @cExt )
   cPathA := hbide_pathNormalized( cPath )

   n := ascan( ::aEditorPath, {|e_| e_[ 2 ] == cPathA } )

   IF n == 0
      oParent := oGrand:addItem( cPath )
      aadd( ::aProjData, { oParent, "Editor Path", oGrand, cPathA, cSourceFile } )
      aadd( ::aEditorPath, { oParent, cPathA } )
   ELSE
      oParent := ::aEditorPath[ n,1 ]
   ENDIF

   aadd( ::aProjData, { oParent:addItem( cFile+cExt ), "Opened Source", oParent, ;
                                   cSourceFile, hbide_pathNormalized( cSourceFile ) } )
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

   DO CASE
   CASE n ==  0  // Source File - nothing to do
   CASE n == -2  // "Files"
   CASE n == -1
   CASE ::aProjData[ n, TRE_TYPE ] == "Project Name"
      aPrj := ::aProjData[ n, 5 ]

      cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + s_pathSep + ;
              aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"

      ::oPM:loadProperties( cHbi, .f., .t., .f. )

   CASE ::aProjData[ n, TRE_TYPE ] == "Source File"
      cSource := ::aProjData[ n, TRE_ORIGINAL ]
      ::editSource( cSource )

   CASE ::aProjData[ n, TRE_TYPE ] == "Opened Source"
      ::oED:setSourceVisible( ::aProjData[ n, TRE_DATA ] )

   CASE ::aProjData[ n, TRE_TYPE ] == "Path"

   ENDCASE

   ::manageFocusInEditor()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:manageProjectContext( mp1, mp2, oXbpTreeItem )
   LOCAL n, cHbi, aPrj, cSource
   LOCAL aPops := {}

   HB_SYMBOL_UNUSED( mp2 )

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
      aadd( aPops, { "New Project"                       , {|| ::oPM:loadProperties( , .t., .t., .t. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Load Project"                      , {|| ::oPM:loadProperties( , .f., .f., .t. ) } } )
      hbide_ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Project Name"
      aPrj := ::aProjData[ n, 5 ]
      cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + s_pathSep + ;
              aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"
      //
      IF Alltrim( Upper( ::cWrkProject ) ) != Alltrim( Upper( oXbpTreeItem:caption ) )
         aadd( aPops, { "Set as Current"                 , {|| ::oPM:setCurrentProject( oXbpTreeItem:caption ) } } )
      End
      aadd( aPops, { "Properties"                        , {|| ::oPM:loadProperties( cHbi, .f., .t., .t. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { ::oAC:getAction( "BuildQt"         ), {|| ::oPM:buildProject( oXbpTreeItem:caption, .F.,    , , .T. ) } } )
      aadd( aPops, { ::oAC:getAction( "BuildLaunchQt"   ), {|| ::oPM:buildProject( oXbpTreeItem:caption, .T.,    , , .T. ) } } )
      aadd( aPops, { ::oAC:getAction( "ReBuildQt"       ), {|| ::oPM:buildProject( oXbpTreeItem:caption, .F., .T., , .T. ) } } )
      aadd( aPops, { ::oAC:getAction( "ReBuildLaunchQt" ), {|| ::oPM:buildProject( oXbpTreeItem:caption, .T., .T., , .T. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Launch"                            , {|| ::oPM:launchProject( oXbpTreeItem:caption ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Close This Project"                , {|| ::oPM:closeProject( oXbpTreeItem:caption ) } } )
      //
      hbide_ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Source File"
      //

   CASE ::aProjData[ n, 2 ] == "Opened Source"
      cSource := ::aProjData[ n, 5 ]
      n := ascan( ::aTabs, {|e_| hbide_pathNormalized( e_[ 5 ] ) == cSource } )
      //
      aadd( aPops, { "Save"                              , {|| ::saveSource( n ) } } )
      aadd( aPops, { "Save As"                           , {|| ::saveSource( n, , .t. ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Close"                             , {|| ::closeSource( n ) } } )
      aadd( aPops, { "Close Others"                      , {|| ::closeAllOthers( n ) } } )
      aadd( aPops, { "" } )
      aadd( aPops, { "Apply Theme", {|| ::aTabs[ n, TAB_OEDITOR ]:applyTheme() } } )
      //
      hbide_ExecPopup( aPops, mp1, ::oProjTree:oWidget )

   CASE ::aProjData[ n, 2 ] == "Path"

   ENDCASE

   ::manageFocusInEditor()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:updateFuncList()
   LOCAL o

   ::oFuncList:clear()
   IF !empty( ::aTags )
      aeval( ::aTags, {|e_| o := ::oFuncList:addItem( e_[ 7 ] ) } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:gotoFunction( mp1, mp2, oListBox )
   LOCAL n, cAnchor

   mp1 := oListBox:getData()
   mp2 := oListBox:getItem( mp1 )

   IF ( n := ascan( ::aTags, {|e_| mp2 == e_[ 7 ] } ) ) > 0
      cAnchor := trim( ::aText[ ::aTags[ n,3 ] ] )
      IF !( ::aTabs[ ::nCurTab, TAB_QEDIT ]:find( cAnchor, QTextDocument_FindCaseSensitively ) )
         ::aTabs[ ::nCurTab, TAB_QEDIT ]:find( cAnchor, QTextDocument_FindBackward + QTextDocument_FindCaseSensitively )

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

      hbide_ExecPopup( aPops, mp1, ::oFuncList:oWidget )

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
         IF !empty( ::aText := hbide_readSource( ::aSources[ i ] ) )
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

METHOD HbIde:loadUI( cUi )
   LOCAL cUiFull := s_resPath + cUi + ".ui"
   LOCAL qDialog, qUiLoader, qFile

   IF hb_FileExists( cUiFull )
      qFile := QFile():new( cUiFull )
      IF qFile:open( 1 )
         qUiLoader  := QUiLoader():new()
         qDialog    := QDialog():configure( qUiLoader:load( qFile, ::oDlg:oWidget ) )
         qFile:close()
      ENDIF
   ENDIF

   RETURN qDialog

/*----------------------------------------------------------------------*/
/*
 * Update the project menu to show current info.
 * 03/01/2010 - 12:48:18 - vailtom
 */
METHOD HbIde:updateProjectMenu()
   LOCAL oItem := hbide_mnuFindItem( Self, 'Project' )

   IF Empty( oItem )
      RETURN Self
   ENDIF

*  msgbox( ToString( oMenuBar:aMenuItems[ n ] ))

   IF Empty( ::cWrkProject )
      oItem[ 2 ]:setDisabled( .T. )
      RETURN Self
   ENDIF

   oItem[ 2 ]:setEnabled( .T. )
   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Updates the title bar of the main window, indicating the project and the
 * current filename.
 * 02/01/2010 - 16:30:06 - vailtom
 */
METHOD HbIde:updateTitleBar()
   LOCAL cTitle := "Harbour-Qt IDE"

   IF Empty( ::oDlg )
      RETURN Self
   ENDIF

   IF !Empty( ::cWrkProject )
      cTitle += " - " + ::cWrkProject + ""
   ENDIF

   IF ::nCurTab > 0 .AND. ::nCurTab <= Len( ::aTabs )
      IF Empty( ::aTabs[ ::nCurTab, TAB_SOURCEFILE ] )
         cTitle += " - [" + ::aTabs[ ::nCurTab, TAB_OTAB ]:Caption + "]"
      ELSE
         cTitle += " - [" + ::aTabs[ ::nCurTab, TAB_SOURCEFILE ] + "]"
      ENDIF
   ENDIF

   ::oDlg:Title := cTitle
   ::oDlg:oWidget:setWindowTitle( ::oDlg:Title )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD HbIde:setCodec( cCodec )

   HbXbp_SetCodec( cCodec )
   ::oSBar:getItem( SB_PNL_CODEC ):caption := cCodec

   RETURN Self

/*----------------------------------------------------------------------*/


/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               09Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

CLASS IdeSourcesManager INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD loadSources()
   METHOD saveSource( nTab, lCancel, lAs )
   METHOD saveNamedSource( cSource )
   METHOD editSource( cSourceFile, nPos, nHPos, nVPos, cTheme, cView, lAlert, lVisible, aBookMarks )
   METHOD closeSource( nTab, lCanCancel, lCanceled )
   METHOD closeAllSources( lCanCancel )
   METHOD closeAllOthers( nTab )
   METHOD saveAllSources()
   METHOD saveAndExit()
   METHOD revertSource( nTab )
   METHOD openSource()
   METHOD selectSource( cMode, cFile, cTitle, cDftPath )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:loadSources()
   LOCAL a_

   IF !empty( ::oIni:aFiles )
      FOR EACH a_ IN ::oIni:aFiles
         /*            File     nPos     nVPos    nHPos    cTheme  cView lAlert lVisible, aBookMarks */
         ::editSource( a_[ 1 ], a_[ 2 ], a_[ 3 ], a_[ 4 ], a_[ 5 ], a_[ 6 ], .t., .f., a_[ 7 ] )
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:saveNamedSource( cSource )
   LOCAL lSaved, oEditor, a_, cBuffer

   cSource := hbide_pathNormalized( cSource, .t. )

   FOR EACH a_ IN ::aTabs
      oEditor := a_[ TAB_OEDITOR ]
      IF hb_isObject( oEditor )
         IF hb_FileMatch( hbide_pathNormalized( oEditor:sourceFile, .t. ), cSource )
            IF oEditor:lLoaded
               IF oEditor:qDocument:isModified()
                  cBuffer := oEditor:prepareBufferToSave( oEditor:qEdit:toPlainText() )

                  IF ( lSaved := hb_memowrit( hbide_pathToOSPath( cSource ), cBuffer ) )
                     oEditor:qDocument:setModified( .f. )
                     oEditor:setTabImage()
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   NEXT

   RETURN lSaved

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:editSource( cSourceFile, nPos, nHPos, nVPos, cTheme, cView, lAlert, lVisible, aBookMarks )
   LOCAL lNew

   DEFAULT lAlert   TO .T.
   DEFAULT lVisible TO .T.

   IF ( lNew := empty( cSourceFile ) )
      cSourceFile := hbide_saveAFile( ::oDlg, "Provide source filename", /*aFltr*/, hbide_SetWrkFolderLast(), /*cDftSuffix*/ )
      IF empty( cSourceFile )
         RETURN Self
      ENDIF
      hbide_SetWrkFolderLast( cSourceFile )
   ENDIF

   IF !Empty( cSourceFile )
      IF !( hbide_isValidText( cSourceFile ) )
         MsgBox( 'File type unknown or unsupported: ' + cSourceFile )
         RETURN .f.
      ELSEIF ! lNew .AND. ! hb_FileExists( cSourceFile )
         MsgBox( 'File not found: ' + cSourceFile )
         RETURN .f.
      ENDIF
      IF ::oEM:isOpen( cSourceFile )
         IF lAlert
            IF hbide_getYesNo( cSourceFile + " is already open.", ;
                                        "Want to re-load it again ?", "File Open Info!" )
               ::oEM:reLoad( cSourceFile )
            ENDIF
         ENDIF
         ::oEM:setSourceVisible( cSourceFile )
         RETURN .t.
      ENDIF
   ENDIF

   DEFAULT nPos  TO 0
   DEFAULT nHPos TO 0
   DEFAULT nVPos TO 0

   ::oEM:buildEditor( cSourceFile, nPos, nHPos, nVPos, cTheme, cView, aBookMarks )
   IF lVisible
      ::oEM:setSourceVisible( cSourceFile )
   ENDIF

   IF !Empty( cSourceFile ) .AND. !hbide_isSourcePPO( cSourceFile )
      hbide_mnuAddFileToMRU( Self, cSourceFile, "recent_files" )
   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/
/*
 *   Save selected Tab on harddisk and return .T. if successfull!
 */
METHOD IdeSourcesManager:saveSource( nTab, lCancel, lAs )
   LOCAL oEdit, lNew, cBuffer, qDocument, nIndex, cSource, cFileTemp
   LOCAL cFileToSave, cFile, cExt, cNewFile, oItem

   DEFAULT nTab TO ::EM:getTabCurrent()
   DEFAULT lAs  TO .F.

   lCancel := .F.

   IF !empty( oEdit := ::oEM:getEditorByTabPosition( nTab ) )
      nIndex  := ::qTabWidget:indexOf( oEdit:oTab:oWidget )
      cSource := oEdit:sourceFile

      // IF !Empty( oEdit:sourceFile ) .AND. oEdit:lLoaded .AND. oEdit:qDocument:isModified()
      IF lAs .OR. empty( oEdit:sourceFile ) .OR. ( oEdit:lLoaded .AND. oEdit:qDocument:isModified() )

         lNew := Empty( cSource ) .OR. lAs
         IF lNew
            cNewFile := ::selectSource( 'save', ;
                                       iif( !Empty( cSource ), cSource, hb_dirBase() + "projects" + hb_ps() ),;
                                              "Save " + oEdit:oTab:caption + " as..." )
            IF empty( cNewFile )
               // will check later what decision to take
               RETURN .f.
            ENDIF
            IF hb_FileMatch( hbide_pathNormalized( cNewFile ), hbide_pathNormalized( cSource ) )
               lNew := .f.
            ENDIF
         ENDIF

         cFileToSave := iif( lNew, cNewFile, cSource )
         qDocument := oEdit:qDocument

         /*
          * If the burn process fails, we should change the name of the previous file.
          * 01/01/2010 - 21:24:41 - vailtom
          */
         cBuffer := oEdit:prepareBufferToSave( oEdit:qEdit:toPlainText() )
         //
         IF !hb_memowrit( cFileToSave, cBuffer )
            MsgBox( "Error saving the file " + oEdit:sourceFile + ".",, 'Error saving file!' )
            lCancel := .T.
            RETURN .F.
         ENDIF

         hb_fNameSplit( cFileToSave, , @cFile, @cExt )

         IF lNew
            oEdit:sourceFile := cFileToSave

            oEdit:oTab:Caption := cFile + cExt

            ::qTabWidget:setTabText( nIndex, cFile + cExt )
            ::qTabWidget:setTabTooltip( nIndex, cFileToSave )

            IF empty( cSource )
               /* The file is not populated in editors tree. Inject */
               ::oEM:addSourceInTree( oEdit:sourceFile )
            ELSEIF lAs
               /* Rename the existing nodes in tree */
               IF !empty( oItem := hbide_findProjTreeItem( ::oIde, oEdit:sourceFile, "Opened Source" ) )
                  oItem:oWidget:caption := cFile + cExt
               ENDIF
            ENDIF
         ENDIF

         qDocument:setModified( .f. )
         ::oIde:aSources := { oEdit:sourceFile }
         ::createTags()
         ::updateFuncList()
         ::qTabWidget:setTabIcon( nIndex, ::resPath + "tabunmodified.png" )
         ::oDK:setStatusText( SB_PNL_MODIFIED, " " )

         cFileTemp := hbide_pathToOSPath( oEdit:cPath + oEdit:cFile + oEdit:cExt + ".tmp" )
         ferase( cFileTemp )
      ENDIF
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:closeSource( nTab, lCanCancel, lCanceled )
   LOCAL lSave, n, oEditor

   DEFAULT nTab TO ::oEM:getTabCurrent()

   IF !empty( oEditor := ::oEM:getEditorByTabPosition( nTab ) )

      DEFAULT lCanCancel TO .F.
      lCanceled := .F.

      IF !( oEditor:qDocument:isModified() ) /* File has not changed, ignore the question to User */
         lSave := .F.

      ELSEIF lCanCancel
         n := hbide_getYesNoCancel( oEditor:oTab:Caption, "has been modified, save this source?", 'Save?' )
         IF ( lCanceled := ( n == QMessageBox_Cancel ) )
            RETURN .F.
         ENDIF
         lSave := ( n == QMessageBox_Yes )

      ELSE
         lSave := hbide_getYesNo( oEditor:oTab:Caption, "has been modified, save this source?", 'Save?' )

      ENDIF

      IF lSave .AND. !( ::saveSource( nTab, @lCanceled ) )
         IF lCanCancel
            RETURN .F.
         ENDIF
      ENDIF

      oEditor:destroy()
      ::oIde:updateTitleBar()
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Close all opened files.
 * 02/01/2010 - 15:31:44
 */
METHOD IdeSourcesManager:closeAllSources( lCanCancel )
   LOCAL lCanceled
   LOCAL i := 0

   DEFAULT lCanCancel TO .t.

   DO WHILE ( ++i <= Len( ::aTabs ) )

       IF ::closeSource( i, lCanCancel, @lCanceled )
          i --
          LOOP
       ENDIF

       IF lCanceled
          RETURN .F.
       ENDIF
   ENDDO

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Close all opened files except current.
 * 02/01/2010 - 15:47:19 - vailtom
 */
METHOD IdeSourcesManager:closeAllOthers( nTab )
   LOCAL lCanceled
   LOCAL oEdit
   LOCAL nID

   DEFAULT nTab TO ::oEM:getTabCurrent()

   IF empty( oEdit := ::oEM:getEditorByTabPosition( nTab ) )
      RETURN .F.
   ENDIF

   nID  := oEdit:nID
   nTab := 0

 * Finally now we will close all tabs.
   DO WHILE ( ++nTab <= Len( ::aTabs ) )

       oEdit := ::oEM:getEditorByTabPosition( nTab )

       IF empty( oEdit ) .OR. oEdit:nID == nID
          LOOP
       ENDIF

       IF ::closeSource( nTab, .T., @lCanceled )
          nTab --
          LOOP
       ENDIF

       IF lCanceled
          RETURN .F.
       ENDIF
   ENDDO

   RETURN .T.

/*----------------------------------------------------------------------*/
/*
 * Save all opened files...
 * 01/01/2010 - 22:44:36 - vailtom
 */
METHOD IdeSourcesManager:saveAllSources()
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
METHOD IdeSourcesManager:saveAndExit()

   IF ::saveSource()
      ::execAction( "Exit" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Revert current file to a previous saved file.
 * 02/01/2010 - 19:45:34
 */
METHOD IdeSourcesManager:revertSource( nTab )
   LOCAL oEditor

   DEFAULT nTab TO ::oEM:getTabCurrent()

   IF empty( oEditor := ::oEM:getEditorByTabPosition( nTab ) )
      RETURN .F.
   ENDIF

   IF !( oEditor:qDocument:isModified() )
      * File has not changed, ignore the question to User
   ELSE
      IF !hbide_getYesNo( 'Revert ' + oEditor:oTab:Caption + '?',  ;
                    'The file ' + oEditor:sourceFile + ' has changed. '+;
                    'Discard current changes and revert contents to the previously saved on disk?', 'Revert file?' )
         RETURN Self
      ENDIF
   ENDIF

   oEditor:qEdit:setPlainText( hb_memoRead( oEditor:sourceFile ) )
   oEditor:qEdit:ensureCursorVisible()
   ::manageFocusInEditor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:openSource()
   LOCAL aSrc, cSource

   IF !empty( aSrc := ::selectSource( "openmany" ) )
      FOR EACH cSource IN aSrc
         ::editSource( cSource )
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSourcesManager:selectSource( cMode, cFile, cTitle, cDftPath )
   LOCAL oDlg, cPath

   DEFAULT cDftPath TO ::cLastFileOpenPath

   oDlg := XbpFileDialog():new():create( ::oDlg, , { 10,10 } )

   IF cMode == "open"
      oDlg:title       := "Select a Source File"
      oDlg:center      := .t.
      oDlg:fileFilters := { { "All Files"  , "*.*"   }, { "PRG Sources", "*.prg" }, { "C Sources" , "*.c"  },;
                            { "CPP Sources", "*.cpp" }, { "H Headers"  , "*.h"   }, { "CH Headers", "*.ch" } }
      cFile := oDlg:open( cDftPath, , .f. )
      IF !empty( cFile )
         ::oIde:cLastFileOpenPath := cFile
      ENDIF

   ELSEIF cMode == "openmany"
      oDlg:title       := "Select Sources"
      oDlg:center      := .t.
      oDlg:defExtension:= 'prg'
      oDlg:fileFilters := { { "All Files"  , "*.*"   }, { "PRG Sources", "*.prg" }, { "C Sources" , "*.c"  },;
                            { "CPP Sources", "*.cpp" }, { "H Headers"  , "*.h"   }, { "CH Headers", "*.ch" } }
      cFile := oDlg:open( cDftPath, , .t. )
      IF !empty( cFile ) .AND. !empty( cFile[ 1 ] )
         ::oIde:cLastFileOpenPath := cFile[ 1 ]
      ENDIF

   ELSEIF cMode == "save"
      oDlg:title       := iif( !hb_isChar( cTitle ), "Save as...", cTitle )
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
         HB_TRACE( HB_TR_DEBUG, cFile )
      ENDIF

   ENDIF

   RETURN cFile

/*----------------------------------------------------------------------*/

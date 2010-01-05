/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               03Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqt.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

CLASS IdeProjManager INHERIT IdeObject

   DATA   cSaveTo
   DATA   aPrjProps                               INIT {}

   DATA   qProcess
   DATA   nStarted                                INIT 0
   DATA   cFileOut
   DATA   cFileErr

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD populate()
   METHOD loadProperties()
   METHOD fetchProperties()
   METHOD save()
   METHOD updateHbp()
   METHOD addSources()
   METHOD getCurrentProject()
   METHOD setCurrentProject()
   METHOD selectCurrentProject()
   METHOD closeProject()
   METHOD promptForPath()
   METHOD launchProject()
   METHOD buildProject()
   METHOD buildProjectViaQt()
   METHOD readProcessInfo()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:populate()
   LOCAL cProject

   FOR EACH cProject IN ::aINI[ INI_PROJECTS ]
      ::loadProperties( cProject, .f., .f., .T. )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadProperties( cProject, lNew, lFetch, lUpdateTree )
   LOCAL n, t, cWrkProject

   DEFAULT cProject    TO ""
   DEFAULT lNew        TO .F.
   DEFAULT lFetch      TO .T.
   DEFAULT lUpdateTree TO .F.

   /* Never touch original project file sent */

   ::aPrjProps := {}
   ::cSaveTo   := ""

   IF lNew
      lFetch := .t.
   ELSE
      IF empty( cProject )
         cProject := hbide_fetchAFile( ::oDlg, "Load Project...", { { "Harbour IDE Projects (*.hbi)", "*.hbi" } } )
      ENDIF
      IF empty( cProject )
         RETURN Self
      ENDIF
   ENDIF

   n := 0
   IF !empty( cProject )
      cWrkProject := hbide_pathNormalized( cProject )                                 /* normalize project name */
      IF ( n := ascan( ::aProjects, {|e_| hbide_pathNormalized( e_[ 1 ] ) == cWrkProject } ) ) > 0
         ::aPrjProps := ::aProjects[ n, 3 ]
         t := ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ]
      ENDIF
      IF empty( ::aPrjProps )
         ::aPrjProps := hbide_fetchHbiStructFromFile( hbide_pathToOSPath( cProject ) )
      ENDIF
   ENDIF

   IF lFetch
      ::fetchProperties()
      IF !empty( ::cSaveTo ) .and. hb_FileExists( ::cSaveTo )
         cProject := ::cSaveTo
         ::aPrjProps := hbide_fetchHbiStructFromFile( hbide_pathToOSPath( cProject ) ) /* Reload from file */
      ENDIF
   ENDIF

   IF n == 0
      aadd( ::oIde:aProjects, { lower( cProject ), cProject, aclone( ::aPrjProps ) } )
      IF lUpdateTree
         ::updateProjectTree( ::aPrjProps )
      ENDIF
      hbide_mnuAddFileToMRU( ::oIde, cProject, INI_RECENTPROJECTS )
   ELSE
      ::aProjects[ n, 3 ] := aclone( ::aPrjProps )
      IF lUpdateTree
         ::updateProjectTree( ::aPrjProps )
      ENDIF
      IF lUpdateTree .AND. ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] <> t
         MsgBox( "::removeProjectFromTree( ::aPrjProps )" )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:fetchProperties()
   LOCAL cPrjLoc := hb_dirBase() + "projects"

   ::oUI := XbpQtUiLoader():new( ::oDlg )
   ::oUI:file := ::resPath + "projectproperties.ui"
   ::oUI:create()

   ::oUI:q_comboPrjType:addItem( "Executable" )
   ::oUI:q_comboPrjType:addItem( "Library"    )
   ::oUI:q_comboPrjType:addItem( "Dll"        )

   DO CASE
   CASE empty( ::aPrjProps )
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )
   CASE ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Lib"
      ::oUI:q_comboPrjType:setCurrentIndex( 1 )
   CASE ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Dll"
      ::oUI:q_comboPrjType:setCurrentIndex( 2 )
   OTHERWISE
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )
   ENDCASE

   ::oUI:signal( "buttonCn"      , "clicked()", {|| ::oUI:oWidget:close() } )
   ::oUI:signal( "buttonSave"    , "clicked()", {|| ::save( .F. ) } )
   ::oUI:signal( "buttonSaveExit", "clicked()", {|| ::save( .T. ) } )
   ::oUI:signal( "buttonSelect"  , "clicked()", {|| ::addSources() } )
   ::oUI:signal( "tabWidget"     , "currentChanged(int)", {|o,p| ::updateHbp( p, o ) } )

   // TODO: Loading lookup.png inside these buttons...
   ::oUI:signal( "buttonChoosePrjLoc", "clicked()", {|| ::PromptForPath( 'editPrjLoctn',  'Choose the Project Location...', 'editOutName', "editWrkFolder", "editDstFolder" ) } )
   ::oUI:signal( "buttonChooseWd"    , "clicked()", {|| ::PromptForPath( 'editWrkFolder', 'Choose a Working Folder...' ) } )
   ::oUI:signal( "buttonChooseDest"  , "clicked()", {|| ::PromptForPath( 'editDstFolder', 'Choose a Destination Folder...' ) } )

   IF empty( ::aPrjProps )
      /*
       * When they click on the button to confirm the name of the project, we
       * will adjust the other parameters. (vailtoms)
       * 25/12/2009 - 20:40:22
       */
      ::oUI:q_editPrjLoctn:setText( StrTran( cPrjLoc, '\', '/' ) )

   ELSE
      ::oUI:q_editPrjTitle:setText( ::aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_TITLE     ] )
      ::oUI:q_editPrjLoctn:setText( ::aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_LOCATION  ] )
      ::oUI:q_editWrkFolder:setText( ::aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_WRKFOLDER ] )
      ::oUI:q_editDstFolder:setText( ::aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_DSTFOLDER ] )
      ::oUI:q_editOutName:setText( ::aPrjProps[ PRJ_PRP_PROPERTIES, 1, PRJ_PRP_OUTPUT    ] )

      ::oUI:q_editFlags:setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_FLAGS   , 1 ] ) )
      ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_SOURCES , 1 ] ) )
      ::oUI:q_editMetaData:setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_METADATA, 1 ] ) )
      ::oUI:q_editCompilers:setPlainText( memoread( hb_dirBase() + "hbide.env" ) )

      #if 0
      ::oUI:q_editLaunchParams:setText()
      ::oUI:q_editLaunchExe:setText()
      ::oUI:q_editHbp:setPlainText()
      #endif
   ENDIF

   IF empty( ::aPrjProps )
      ::oUI:oWidget:setWindowTitle( 'New Project...' )
   ELSE
      ::oUI:oWidget:setWindowTitle( 'Properties for "' + ::oUI:q_editPrjTitle:Text() + '"' )
   ENDIF

   ::setPosByIni( ::oUI:oWidget, PropsDialogGeometry )
   //
   ::oUI:exec()
   //
   ::aIni[ INI_HBIDE, PropsDialogGeometry ] := hbide_posAndSize( ::oUI:oWidget )

   ::oUI:destroy()
   ::oUI := NIL

   ::manageFocusInEditor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:save( lCanClose )
   LOCAL a_, a4_1
   LOCAL typ_:= { "Executable", "Lib", "Dll" }
   LOCAL txt_:= {}
   LOCAL lOk

   * Validate certain parameters before continuing ... (vailtom)
   IF Empty( ::oUI:q_editOutName:text() )
      IF Empty( ::oUI:q_editPrjTitle:text() )
         MsgBox( 'Invalid Output FileName!' )
         ::oUI:q_editOutName:setFocus()
         RETURN .F.
      ENDIF
      ::oUI:q_editOutName:setText( ::oUI:q_editPrjTitle:text() )
   ENDIF

   IF Empty( ::oUI:q_editPrjTitle:text() )
      ::oUI:q_editPrjTitle:setText( ::oUI:q_editOutName:text() )
   ENDIF

   IF !hbide_isValidPath( ::oUI:q_editPrjLoctn:text(), 'Project Location' )
      ::oUI:q_editPrjLoctn:setFocus()
      RETURN .F.
   ENDIF

   IF !hbide_isValidPath( ::oUI:q_editWrkFolder:text(), 'Working Folder' )
      ::oUI:q_editWrkFolder:setText( ::oUI:q_editPrjLoctn:text() )
      RETURN .F.
   ENDIF

   IF !hbide_isValidPath( ::oUI:q_editDstFolder:text(), 'Destination Folder' )
      ::oUI:q_editDstFolder:setText( ::oUI:q_editPrjLoctn:text() )
      RETURN .F.
   ENDIF

   aadd( txt_, "[ PROPERTIES ]" )
   aadd( txt_, "Type              = " + typ_[ ::oUI:q_comboPrjType:currentIndex()+1 ] )
   aadd( txt_, "Title             = " + ::oUI:q_editPrjTitle:text() )
   aadd( txt_, "Location          = " + ::oUI:q_editPrjLoctn:text() )
   aadd( txt_, "WorkingFolder     = " + ::oUI:q_editWrkFolder:text() )
   aadd( txt_, "DestinationFolder = " + ::oUI:q_editDstFolder:text() )
   aadd( txt_, "Output            = " + ::oUI:q_editOutName:text() )
   aadd( txt_, "LaunchParams      = " + ::oUI:q_editLaunchParams:text() )
   aadd( txt_, "LaunchProgram     = " + ::oUI:q_editLaunchExe:text() )
   aadd( txt_, " " )

   aadd( txt_, "[ FLAGS ]" )
   a_:= hbide_memoToArray( ::oUI:q_editFlags:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ SOURCES ]" )
   a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ METADATA ]" )
   a_:= hbide_memoToArray( ::oUI:q_editMetaData:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )

   /* Setup Meta Keys */
   a4_1 := hbide_setupMetaKeys( a_ )

   ::cSaveTo := hbide_parseWithMetaData( ::oUI:q_editPrjLoctn:text(), a4_1 ) + ;
                      ::pathSep + ;
                hbide_parseWithMetaData( ::oUI:q_editOutName:text(), a4_1 ) + ;
                      ".hbi"
   ::cSaveTo := hbide_pathToOSPath( ::cSaveTo )

   IF ( lOk := hbide_createTarget( ::cSaveTo, txt_ ) )
      *MsgBox( 'The project file was saved successfully: ' + ::cSaveTo, 'Saving project ...' )
      hb_MemoWrit( hb_dirBase() + "hbide.env", ::oUI:q_editCompilers:toPlainText() )
   ELSE
      MsgBox( 'Error saving project file: ' + ::cSaveTo, 'Error saving project ...' )
   ENDIF

   IF lCanClose .AND. lOk
      ::oUI:oWidget:close()
   ENDIF

   RETURN lOk

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:updateHbp( iIndex )
   LOCAL a_, a4_1, txt_, s
   LOCAL cExt

   IF iIndex != 3
      RETURN nil
   ENDIF

   a_:= hb_atokens( strtran( ::oUI:q_editMetaData:toPlainText(), chr( 13 ) ), _EOL )
   a4_1 := hbide_setupMetaKeys( a_ )

   txt_:= {}
   /* This block will be absent when submitting to hbmk engine */
   aadd( txt_, "#   " + hbide_parseWithMetaData( ::oUI:q_editWrkFolder:text(), a4_1 ) + ::pathSep + ;
                        hbide_parseWithMetaData( ::oUI:q_editOutName:text(), a4_1 ) + ".hbp" )
   aadd( txt_, " " )

   /* Flags */
   a_:= hb_atokens( ::oUI:q_editFlags:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
         s := hbide_parseWithMetaData( s, a4_1 )
         aadd( txt_, s )
      ENDIF
   NEXT
   aadd( txt_, " " )

   /* Sources */
   a_:= hb_atokens( ::oUI:q_editSources:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
         s := hbide_parseWithMetaData( s, a4_1 )
         hb_FNameSplit( s, , , @cExt )
         IF lower( cExt ) $ ".c,.cpp,.prg,.rc,.res"
            aadd( txt_, s )
         ENDIF
      ENDIF
   NEXT
   aadd( txt_, " " )

   /* Final assault */
   ::oUI:q_editHbp:setPlainText( hbide_arrayToMemo( txt_ ) )

   RETURN txt_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:addSources()
   LOCAL aFiles, a_, b_, a4_1, s

   IF !empty( aFiles := ::selectSource( "openmany" ) )
      a_:= hbide_memoToArray( ::oUI:q_editMetaData:toPlainText() )
      a4_1 := hbide_setupMetaKeys( a_ )

      a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() )

      b_:={}
      aeval( aFiles, {|e| aadd( b_, hbide_applyMetaData( e, a4_1 ) ) } )

      FOR EACH s IN b_
         IF ascan( a_, s ) == 0
            aadd( a_, s )
         ENDIF
      NEXT

      ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( a_ ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/* Set current project for build - vailtom
 * 26/12/2009 - 02:19:38
 */
METHOD IdeProjManager:setCurrentProject( cProjectName )
   LOCAL aPrjProps, n
   LOCAL cOldProject := ::cWrkProject
   LOCAL lValid      := .T.

   IF Empty( cProjectName )
      ::oIde:cWrkProject := ''

   ELSEIF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cProjectName } ) ) > 0
      aPrjProps     := ::aProjects[ n, 3 ]
      ::oIde:cWrkProject := aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]

   ELSE
      MsgBox( 'Invalid project selected: "' + cProjectName + '"' )
      lValid := .F.
   ENDIF

   IF lValid
      IF !Empty( ::oSBar )
         ::oSBar:getItem( SB_PNL_PROJECT ):caption := ::cWrkProject
      ENDIF

      ::oIde:updateTitleBar()
      ::oIde:updateProjectMenu()
   ENDIF

   RETURN cOldProject

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getCurrentProject()

   IF !Empty( ::cWrkProject )
      RETURN ::cWrkProject
   ENDIF

   IF Empty( ::aProjects )
      MsgBox( "No longer available projects!" )
      RETURN ::cWrkProject
   ENDIF

   IF Len( ::aProjects ) == 1
      ::setCurrentProject( ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
      RETURN ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]
   ENDIF

   RETURN ::selectCurrentProject()

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:selectCurrentProject()
   LOCAL oDlg, i, p, t

   IF Empty( ::aProjects )
      MsgBox( "No longer available projects!" )
      RETURN ::cWrkProject
   ENDIF

   oDlg := XbpQtUiLoader():new( ::oDlg )
   oDlg:file := ::oIde:resPath + "selectproject.ui"
   oDlg:create()

 * Fill ComboBox with current project names
   FOR i := 1 TO Len( ::aProjects )
       p := ::aProjects[i]
       t := p[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]

       IF !Empty( t )
          oDlg:qObj[ "cbProjects" ]:addItem( t )
       ENDIF
   NEXT

   oDlg:signal( "btnOk"    , "clicked()", {|| ::setCurrentProject( oDlg:qObj[ "cbProjects" ]:currentText() ), ;
                                                                                       oDlg:oWidget:close() } )
   oDlg:signal( "btnCancel", "clicked()", {|| oDlg:oWidget:close() } )

   oDlg:exec()
   oDlg:destroy()
   oDlg := NIL

   RETURN ::cWrkProject

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:closeProject( cProject )
   LOCAL nPos

   IF Empty( ::aProjects )
      MsgBox( "No longer available projects!" )
      RETURN Self
   ENDIF

   DEFAULT cProject TO ::getCurrentProject()

   nPos := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cProject } )

   IF ( nPos < 0 )
      MsgBox( 'Invalid project: "' + cProject + '"' )
      RETURN Self
   ENDIF

 * aadd( ::aProjects, { lower( cProject ), cProject, aclone( ::aPrjProps ) } )

   ::aPrjProps := {}
   ::updateProjectTree( ::aProjects[ nPos, 3 ], .T. )
   hb_adel( ::aProjects, nPos, .T. )
   ::setCurrentProject( '' )

   RETURN Self

/*----------------------------------------------------------------------*/
/* Prompt for user to select a existing folder
 * 25/12/2009 - 19:03:09 - vailtom
 */
METHOD IdeProjManager:promptForPath( cObjPathName, cTitle, cObjFileName, cObjPath2, cObjPath3 )
   LOCAL cTemp, cPath, cFile

   IF !hb_isChar( cObjFileName )
      cTemp := ::oUI:qObj[ cObjPathName ]:Text()
      cPath := hbide_fetchADir( ::oDlg, cTitle, cTemp )
      cPath := StrTran( cPath, "\", "/" )

   ELSE
      cTemp := ::oUI:qObj[ cObjPathName ]:Text()
      cTemp := hbide_fetchAFile( ::oDlg, cTitle, { { "Harbour IDE Projects", "*.hbi" } }, cTemp )

      IF !Empty( cTemp )
         cTemp := strtran( cTemp, "\", '/' )

         hb_fNameSplit( cTemp, @cPath, @cFile )

         ::oUI:qObj[ cObjFileName ]:setText( cFile )
      ENDIF
   ENDIF

   IF !Empty( cPath )
      IF Right( cPath, 1 ) == '/'
         cPath := Left( cPath, Len( cPath ) - 1 )
      ENDIF
      ::oUI:qObj[ cObjPathName ]:setText( cPath )

      IF hb_isChar( cObjPath2 ) .AND. Empty( ::oUI:qObj[ cObjPath2 ]:Text() )
         ::oUI:qObj[ cObjPath2 ]:setText( cPath )
      ENDIF

      IF hb_isChar( cObjPath3 ) .AND. Empty( ::oUI:qObj[ cObjPath3 ]:Text() )
         ::oUI:qObj[ cObjPath3 ]:setText( cPath )
      ENDIF
   ENDIF

   ::oUI:qObj[ cObjPathName ]:setFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildProjectViaQt( cProject )

   ::buildProject( cProject, , , , .t. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   LOCAL cOutput, cErrors, n, aPrj, cHbpPath, aHbp, qStringList
   LOCAL cTmp, nResult, nSeconds, cTargetFN, cPath, cFileName, lDelHbp

   DEFAULT lLaunch   TO .F.
   DEFAULT lRebuild  TO .F.
   DEFAULT lPPO      TO .F.
   DEFAULT lViaQt    TO .F.

   lDelHbp := lPPO

   IF lPPO .AND. ::getCurrentTab() == 0
      MsgBox( 'No file open issue to be compiled!' )
      RETURN Self
   End

   IF empty( cProject )
      cProject := ::getCurrentProject()
   ENDIF

   IF empty( cProject )
      RETURN Self
   ENDIF

   n    := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProject } )
   aPrj := ::aProjects[ n, 3 ]
   aHbp := {}

   cTargetFN := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + ::pathSep + aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT ]
   cTargetFN := hbide_pathToOSPath( cTargetFN )
   /*
    * Creates a temporary file to avoid erase the file. Hbp correct this project.
    * 26/12/2009 - 04:17:56 - vailtom
    */
   IF lDelHbp
      cHbpPath  := cTargetFN + '.' + hb_md5( alltrim( str( seconds() ) ) ) + ".hbp"
   ELSE
      cHbpPath  := cTargetFN + ".hbp"
   ENDIF

   DO CASE
   CASE aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Lib"
      aadd( aHbp, "-hblib" )
   CASE aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Dll"
      aadd( aHbp, "-hbdyn" )
   ENDCASE

   aadd( aHbp, "-o" + cTargetFN )
   aadd( aHbp, "-q"             )
   aadd( aHbp, "-trace"         )
   aadd( aHbp, "-info"          )

   IF lRebuild
      aadd( aHbp, "-rebuild" )
   ENDIF

   aeval( aPrj[ PRJ_PRP_FLAGS, 2 ], {|e| aadd( aHbp, e ) } )

   IF !( lPPO )
      aeval( hbide_filesToSources( aPrj[ PRJ_PRP_SOURCES, 2 ] ), {|e| aadd( aHbp, e ) } )

   ELSE
      aadd( aHbp, "-hbcmp -s -p" )

      n := ::getCurrentTab()

      hb_FNameSplit( ::aTabs[ n, TAB_SOURCEFILE ], @cPath, @cFileName, @cTmp )

      IF !( lower( cTmp ) $ ".prg,?" )
         MsgBox( 'Operation not supported for this file type: "' + cTmp + '"' )
         RETURN Self
      ENDIF

      cFileName := cPath + cFileName + '.ppo'

      // TODO: We have to test if the current file is part of a project, and we
      // pull your settings, even though this is not the active project - vailtom
      aadd( aHbp, ::aTabs[ n, TAB_SOURCEFILE ] )

      FErase( cFileName )
   ENDIF

   IF !hbide_createTarget( cHbpPath, aHbp )
      cTmp := 'Error saving: ' + cHbpPath

   ELSE
      ::lDockBVisible := .t.
      ::oDockB2:show()
      ::oOutputResult:oWidget:clear()

      nSeconds := seconds()  // time elapsed

      cTmp := hbide_outputLine() + CRLF + ;
              "Project [ " + cProject                     + " ]    " + ;
              "Launch [ "  + iif( lLaunch , 'Yes', 'No' ) + " ]    " + ;
              "Rebuild [ " + iif( lRebuild, 'Yes', 'No' ) + " ]    " + ;
              "Started [ " + time() + " ]" + CRLF + ;
              hbide_outputLine() + CRLF

      IF lViaQt
         qStringList := QStringList():new()
         qStringList:append( cHbpPath )

         ::qProcess := QProcess():new()

         ::cFileOut := hbide_pathToOSPath( cTargetFN + '.' + hb_md5( alltrim( str( seconds() ) ) ) + ".out" )
         ::cFileErr := hbide_pathToOSPath( cTargetFN + '.' + hb_md5( alltrim( str( seconds() ) ) ) + ".err" )

         ::qProcess:setStandardOutputFile( ::cFileOut )
         ::qProcess:setStandardErrorFile( ::cFileErr )

         Qt_Slots_Connect( ::pSlots, ::qProcess, "readyReadStandardOutput()", {|o,i| ::readProcessInfo( 2, i, o ) } )
         Qt_Slots_Connect( ::pSlots, ::qProcess, "readyReadStandardError()" , {|o,i| ::readProcessInfo( 3, i, o ) } )
         Qt_Slots_Connect( ::pSlots, ::qProcess, "finished(int,int)"        , {|o,i,ii| ::readProcessInfo( 4, i, ii, o ) } )

         ::oOutputResult:oWidget:clear()
         ::oOutputResult:oWidget:append( cTmp )
         ::nStarted := seconds()

         ::qProcess:start( "hbmk2", qStringList )

      ELSE
         cOutput := "" ; cErrors := ""
         nResult := hb_processRun( ( "hbmk2 " + cHbpPath ), , @cOutput, @cErrors )

       * Show detailed status about compile process...
         cTmp += cOutput + CRLF
         cTmp += IIF( empty( cErrors ), "", cErrors ) + CRLF
         cTmp += "errorlevel: " + hb_ntos( nResult ) + CRLF
         cTmp += '-----------------------------------------------------------------' + CRLF
         cTmp += 'Finished at ' + time() + CRLF
         cTmp += "Done in " + ltrim( str( seconds() - nseconds ) ) +" seconds."  + CRLF

         hbide_convertBuildStatusMsgToHtml( cTmp, ::oOutputResult:oWidget )

         IF ( nResult == 0 ) .AND. ( lLaunch )
            ::LaunchProject( cProject )
         ENDIF
      ENDIF

   ENDIF

   IF lDelHbp
      FErase( cHbpPath )
   ENDIF

   IF lPPO .AND. hb_FileExists( cFileName )
      ::oED:showPPO( cFileName )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:readProcessInfo( nMode, i, ii )
   LOCAL cLine, cTmp

   DO CASE
   CASE nMode == 1


   CASE nMode == 2
      ::qProcess:setReadChannel( 0 )
      cLine := space( 4096 )
      ::qProcess:readLine( @cLine, 4096 )
      IF !empty( cLine )
         ::oOutputResult:oWidget:append( cLine )
      ENDIF

   CASE nMode == 3
      ::qProcess:setReadChannel( 1 )
      cLine := space( 4096 )
      ::qProcess:readLine( @cLine, 4096 )

      IF !empty( cLine )
         IF ( "Warning" $ cLine )
            cLine := '<font color=blue>' + cLine + '</font>'
         ELSEIF ( "Error" $ cLine )
            cLine := '<font color=red>' + cLine + '</font>'
         ENDIF

         ::oOutputResult:oWidget:append( cLine )
      ENDIF

   CASE nMode == 4
      cTmp := memoread( ::cFileOut )
      hbide_convertBuildStatusMsgToHtml( cTmp, ::oOutputResult:oWidget )
      cTmp := memoread( ::cFileErr )
      hbide_convertBuildStatusMsgToHtml( cTmp, ::oOutputResult:oWidget )

      cTmp := hbide_outputLine() + CRLF
      cTmp += "Exit Code [ " + hb_ntos( i ) + " ]    Exit Status [ " + hb_ntos( ii ) + " ]    " +;
              "Finished at [ " + time()     + " ]    Done in [ " + hb_ntos( seconds() - ::nStarted ) +" Secs ]" + CRLF
      cTmp += hbide_outputLine() + CRLF

      ::oOutputResult:oWidget:append( cTmp )

      Qt_Slots_disConnect( ::pSlots, ::qProcess, "finished(int,int)"         )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readyReadStandardOutput()" )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readyReadStandardError()"  )

      ::qProcess:kill()
      ::qProcess:pPtr := 0
      ::qProcess := NIL

      ferase( ::cFileOut )
      ferase( ::cFileErr )
   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * Launch selected project.
 * 03/01/2010 - 09:24:50
 */
METHOD IdeProjManager:LaunchProject( cProject )
   LOCAL qProcess
   LOCAL cTargetFN
   LOCAL cTmp, aPrj, n

   IF empty( cProject )
      cProject := ::oPM:getCurrentProject()
   ENDIF

   IF empty( cProject )
      RETURN Self
   ENDIF

   n    := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1,2,PRJ_PRP_TITLE ] == cProject } )
   aPrj := ::aProjects[ n,3 ]

   cTargetFN := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + ::pathSep + aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT ]
   cTargetFN := StrTran( cTargetFN, '/', ::pathSep )
   cTargetFN := StrTran( cTargetFN, '\', ::pathSep )

#ifdef __PLATFORM__WINDOWS
   IF aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Executable"
      cTargetFN += '.exe'
   ENDIF
#endif

   IF !hb_FileExists( cTargetFN )
      cTmp := "Launch application error: file not found " + cTargetFN + "!"

   ELSEIF aPrj[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] == "Executable"
      cTmp := "Launch application " + cTargetFN + "... "

      qProcess := QProcess():new()
      qProcess:startDetached_2( cTargetFN )
      qProcess:waitForStarted()
      qProcess:pPtr := 0
      qProcess := NIL
   ELSE
      cTmp := "Launch application " + cTargetFN + "... (not applicable)"
   ENDIF

   ::oOutputResult:oWidget:append( cTmp )
   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_outputLine( cLine, nOccur )

   DEFAULT cLine  TO "-"
   DEFAULT nOccur TO 100

   RETURN replicate( cLine, nOccur )

/*----------------------------------------------------------------------*/


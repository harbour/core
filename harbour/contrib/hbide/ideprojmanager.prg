/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
//
//                             Class IdeSource
//
/*----------------------------------------------------------------------*/

CLASS IdeSource

   DATA  original
   DATA  normalized
   DATA  filter
   DATA  path
   DATA  file
   DATA  ext
   DATA  projPath

   METHOD new( cSource )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSource:new( cSource )
   LOCAL cFilt, cPathFile, cPath, cFile, cExt

   hbide_parseHbpFilter( cSource, @cFilt, @cPathFile )
   hb_fNameSplit( cPathFile, @cPath, @cFile, @cExt )

   ::original   := cSource
   ::normalized := hbide_pathNormalized( cSource, .t. )
   ::filter     := cFilt
   ::path       := hbide_pathNormalized( cPath, .t. )
   ::file       := cFile
   ::ext        := lower( cExt )

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                             Class IdeProject
//
/*----------------------------------------------------------------------*/

CLASS IdeProject

   DATA   aProjProps                              INIT {}

   DATA   fileName                                INIT ""
   DATA   normalizedName                          INIT ""

   DATA   type                                    INIT "Executable"
   DATA   title                                   INIT ""
   DATA   location                                INIT hb_dirBase() + "projects"
   DATA   wrkDirectory                            INIT ""
   DATA   destination                             INIT ""
   DATA   outputName                              INIT ""
   DATA   backup                                  INIT ""
   DATA   launchParams                            INIT ""
   DATA   launchProgram                           INIT ""
   DATA   hbpFlags                                INIT {}
   DATA   sources                                 INIT {}
   DATA   dotHbp                                  INIT ""
   DATA   compilers                               INIT ""
   DATA   cPathMk2                                INIT hb_getenv( "HBIDE_DIR_HBMK2" )
   DATA   cPathEnv                                INIT hb_DirBase() + "resources"
   DATA   hSources                                INIT {=>}
   DATA   hPaths                                  INIT {=>}
   DATA   lPathAbs                                INIT .F.  // Lets try relative paths first . xhp and hbp will be relative anyway
   DATA   projPath                                INIT ""

   METHOD new( oIDE, aProps )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProject:new( oIDE, aProps )
   LOCAL b_, a_, oSource, cSource

   IF hb_isArray( aProps ) .AND. !empty( aProps )
      ::aProjProps := aProps

      b_:= aProps
      a_:= b_[ PRJ_PRP_PROPERTIES, 2 ]

      ::type           := a_[ E_qPrjType ]
      ::title          := a_[ E_oPrjTtl  ]
      ::wrkDirectory   := a_[ E_oPrjWrk  ]
      ::destination    := a_[ E_oPrjDst  ]
      ::outputName     := a_[ E_oPrjOut  ]
      ::launchParams   := a_[ E_oPrjLau  ]
      ::launchProgram  := a_[ E_oPrjLEx  ]

      ::projPath       := oIde:oPM:getProjectPathFromTitle( ::title )
      IF empty( ::projPath )
         ::projPath := hb_dirBase() /* In case of new project */
      ENDIF
      ::location       := ::projPath

      ::hbpFlags       := aclone( b_[ PRJ_PRP_FLAGS   , 2 ] )
      ::sources        := aclone( b_[ PRJ_PRP_SOURCES , 2 ] )
      ::dotHbp         := ""
      ::compilers      := ""

      IF !empty( oIDE:aINI[ INI_HBIDE, PathMk2 ] )
         ::cPathMk2 := oIDE:aINI[ INI_HBIDE, PathMk2 ]
      ENDIF
      IF !empty( oIDE:aINI[ INI_HBIDE, PathEnv ] )
         ::cPathEnv := oIDE:aINI[ INI_HBIDE, PathEnv ]
      ENDIF

      FOR EACH cSource IN ::sources
         cSource := hbide_syncProjPath( ::projPath, cSource )

         oSource := IdeSource():new( cSource )
         oSource:projPath := ::projPath
         ::hSources[ oSource:normalized ] := oSource
         ::hPaths[ oSource:path ] := NIL
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//                            IdeProjectManager
/*----------------------------------------------------------------------*/

CLASS IdeProjManager INHERIT IdeObject

   DATA   cSaveTo
   DATA   aPrjProps                               INIT {}

   DATA   nStarted                                INIT 0

   DATA   lLaunch                                 INIT .F.
   DATA   cProjectInProcess                       INIT ""
   DATA   cPPO                                    INIT ""
   DATA   lPPO                                    INIT .F.
   DATA   oProject
   DATA   cBatch
   DATA   oProcess
   DATA   lSaveOK                                 INIT .F.

   DATA   cProjFileName                           INIT ""
   DATA   lNew                                    INIT .F.
   DATA   lFetch                                  INIT .T.
   DATA   lUpdateTree                             INIT .F.

   METHOD new( oIDE )
   METHOD create( oIDE )
   METHOD destroy()

   METHOD populate()
   METHOD loadProperties( cProjFileName, lNew, lFetch, lUpdateTree )
   METHOD fetchProperties()
   METHOD getProperties()
   METHOD sortSources( cMode )
   METHOD save( lCanClose )
   METHOD updateHbp( iIndex )
   METHOD addSources()

   METHOD setCurrentProject( cProjectName )
   METHOD selectCurrentProject()

   METHOD getCurrentProject( lAlert )
   METHOD getProjectProperties( cProjectTitle )

   METHOD getProjectByFile( cProjectFile )
   METHOD getProjectByTitle( cProjectTitle )
   METHOD getProjectsTitleList()

   METHOD getProjectFileNameFromTitle( cProjectTitle )
   METHOD getProjectTypeFromTitle( cProjectTitle )
   METHOD getProjectPathFromTitle( cProjectTitle )
   METHOD getSourcesByProjectTitle( cProjectTitle )

   METHOD removeProject( cProjectTitle )
   METHOD closeProject( cProjectTitle )
   METHOD promptForPath( cObjPathName, cTitle, cObjFileName, cObjPath2, cObjPath3 )
   METHOD buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   METHOD launchProject( cProject, cExe )
   METHOD showOutput( cOutput, mp2, oProcess )
   METHOD finished( nExitCode, nExitStatus, oProcess )
   METHOD isValidProjectLocation( lTell )
   METHOD setProjectLocation( cPath )
   METHOD buildInterface()
   METHOD pullHbpData( cHbp )
   METHOD synchronizeAlienProject( cProjFileName )

   METHOD harbourFlags()
   METHOD hbmk2Flags()
   METHOD xppCompileFlags()
   METHOD xppLinkFlags()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:new( oIDE )

   ::oIDE := oIDE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:create( oIDE )

   DEFAULT oIDE TO ::oIDE

   ::oIDE := oIDE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:destroy()

   IF !empty( ::oUI )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:populate()
   LOCAL cProject

   FOR EACH cProject IN ::aINI[ INI_PROJECTS ]
      ::loadProperties( cProject, .f., .f., .T. )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProperties()
   LOCAL cTmp, n

   cTmp := ::getCurrentProject()
   IF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cTmp } ) ) > 0
      ::loadProperties( ::aProjects[ n, 1 ], .f., .t., .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadProperties( cProjFileName, lNew, lFetch, lUpdateTree )
   LOCAL nAlready

   DEFAULT cProjFileName TO ""
   DEFAULT lNew          TO .F.
   DEFAULT lFetch        TO .T.
   DEFAULT lUpdateTree   TO .F.

   ::cProjFileName  := cProjFileName
   ::lNew           := lNew
   ::lFetch         := lFetch
   ::lUpdateTree    := lUpdateTree

   ::aPrjProps      := {}
   ::cSaveTo        := ""
   ::oProject       := NIL

   IF lNew
      lFetch := .t.
   ELSE
      IF empty( cProjFileName )
         cProjFileName := hbide_fetchAFile( ::oDlg, "Open Project...", { { "Harbour Projects", "*.hbp" } , ;
                                                                         { "xMate Projects"  , "*.xhp" } } )
         cProjFileName := ::synchronizeAlienProject( cProjFileName )
      ENDIF
      IF empty( cProjFileName )
         RETURN Self
      ENDIF
   ENDIF

   cProjFileName := hbide_pathToOSPath( cProjFileName )

   nAlready := ascan( ::aProjects, {|e_| e_[ 1 ] == hbide_pathNormalized( cProjFileName ) } )

   IF !empty( cProjFileName ) .AND. hb_fileExists( cProjFileName )
      ::aPrjProps  := ::pullHbpData( hbide_pathToOSPath( cProjFileName ) )
   ENDIF

   IF lFetch
      /* Access/Assign via this object */
      ::oProject := IdeProject():new( ::oIDE, ::aPrjProps )
      //
      ::oPropertiesDock:hide()
      ::oPropertiesDock:show()
   ELSE
      IF !empty( ::aPrjProps )
         IF nAlready == 0
            aadd( ::oIDE:aProjects, { hbide_pathNormalized( cProjFileName ), cProjFileName, aclone( ::aPrjProps ) } )
            IF lUpdateTree
               ::oIDE:updateProjectTree( ::aPrjProps )
            ENDIF
            hbide_mnuAddFileToMRU( ::oIDE, cProjFileName, INI_RECENTPROJECTS )
         ELSE
            ::aProjects[ nAlready, 3 ] := aclone( ::aPrjProps )
            IF lUpdateTree
               ::oIDE:updateProjectTree( ::aPrjProps )
            ENDIF
         ENDIF
      ENDIF

      ::oHM:refresh()  /* Rearrange Projects Data */
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
//  Without user-interface
//
METHOD IdeProjManager:pullHbpData( cHbp )
   LOCAL n, n1, s, cKey, cVal, aOptns, aFiles, c3rd, nL, aData, cHome, cOutName, cType
   LOCAL aPrp := { ;
            "hbide_type="              , ;
            "hbide_title="             , ;
            "hbide_location="          , ;  // do not pull out this slot
            "hbide_workingfolder="     , ;
            "hbide_destinationfolder=" , ;
            "hbide_output="            , ;
            "hbide_launchparams="      , ;
            "hbide_launchprogram="     , ;
            "hbide_backupfolder="        }

   LOCAL a1_0 := afill( array( PRJ_PRP_PRP_VRBLS ), "" )
   LOCAL a1_1 := {}
   LOCAL a2_0 := {}
   LOCAL a2_1 := {}
   LOCAL a3_0 := {}
   LOCAL a3_1 := {}
   LOCAL a4_0 := {}
   LOCAL a4_1 := {}
   LOCAL a3rd := {}

   hb_fNameSplit( cHbp, @cHome, @cOutName )
   cHome  := hbide_pathStripLastSlash( cHome )

   c3rd   := "-3rd="
   nL     := len( c3rd )
   aData  := hbide_fetchHbpData( cHbp )
   aOptns := aData[ 1 ]
   aFiles := aData[ 2 ]

   FOR EACH s IN aFiles
      s := hbide_stripRoot( cHome, s )
   NEXT

   IF ( n := ascan( aOptns, {|e| lower( e ) $ "-hbexec,-hblib,-hbdyn" } ) ) > 0
      cType := lower( aOptns[ n ] )
      cType := iif( cType == "-hblib", "Lib", iif( cType == "-hbdyn", "Dll", "Executable" ) )
   ELSE
      cType := "Executable"
   ENDIF

   /* Separate hbIDE specific flags */
   FOR EACH s IN aOptns
      IF ( n := at( c3rd, s ) ) > 0
         IF ( n1 := hb_at( " ", s, n ) ) > 0
            aadd( a3rd, substr( s, n + nL, n1 - n - nL ) )
            s := substr( s, 1, n - 1 ) + substr( s, n1 )
         ELSE
            aadd( a3rd, substr( s, n + nL ) )
            s := substr( s, 1, n - 1 )
         ENDIF
      ENDIF
   NEXT

   /* PRJ_PRP_PROPERTIES */
   FOR EACH s IN a3rd
      IF ( n := at( "=", s ) ) > 0
         cKey := alltrim( substr( s, 1, n ) )
         cVal := alltrim( substr( s, n + 1 ) )

         IF ( n := ascan( aPrp, {|e| e == cKey } ) ) > 0
            a1_0[ n ] := hbide_amp2space( cVal )
         ENDIF
      ENDIF
   NEXT

   a1_0[ PRJ_PRP_TYPE     ] := iif( empty( a1_0[ PRJ_PRP_TYPE  ] ), cType   , a1_0[ PRJ_PRP_TYPE  ] )
   a1_0[ PRJ_PRP_TITLE    ] := iif( empty( a1_0[ PRJ_PRP_TITLE ] ), cOutName, a1_0[ PRJ_PRP_TITLE ] )
   a1_0[ PRJ_PRP_OUTPUT   ] := cOutName
   a1_0[ PRJ_PRP_LOCATION ] := hbide_pathNormalized( cHome )

   /* PRJ_PRP_FLAGS */
   FOR EACH s IN aOptns
//hbide_dbg( "FLAGS   ", s )
      IF !empty( s )
         aadd( a2_0, s )
      ENDIF
   NEXT

   /* PRJ_PRP_SOURCES */
   FOR EACH s IN aFiles
//hbide_dbg( "SOURCE  ", s )
      aadd( a3_0, s )
   NEXT

   /* Properties */
   FOR EACH s IN a1_0
      aadd( a1_1, s )
   NEXT

   /* Flags */
   IF !empty( a2_0 )
      FOR EACH s IN a2_0
         aadd( a2_1, s )
      NEXT
   ENDIF

   /* Sources */
   IF !empty( a3_0 )
      FOR EACH s IN a3_0
         IF !( "#" == left( s,1 ) ) .and. !empty( s )
            aadd( a3_1, hbide_stripRoot( cHome, hbide_stripFilter( s ) ) )
         ENDIF
      NEXT
   ENDIF

   RETURN { { a1_0, a1_1 }, { a2_0, a2_1 }, { a3_0, a3_1 }, { a4_0, a4_1 } }

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:save( lCanClose )
   LOCAL a_, lOk, txt_, nAlready
   LOCAL c3rd := "-3rd="

   * Validate certain parameters before continuing ... (vailtom)

   IF Empty( ::oUI:q_editPrjTitle:text() )
      ::oUI:q_editPrjTitle:setText( ::oUI:q_editOutName:text() )
   ENDIF

   IF Empty( ::oUI:q_editOutName:text() )
      MsgBox( 'Invalid Output FileName' )
      ::oUI:q_editOutName:setFocus()
      RETURN .F.
   ENDIF

   /* This must be valid, we cannot skip */
   IF !hbide_isValidPath( ::oUI:q_editPrjLoctn:text(), 'Project Location' )
      ::oUI:q_editPrjLoctn:setFocus()
      RETURN .F.
   ENDIF

   txt_:= {}
   //
   aadd( txt_, c3rd + "hbide_version="           + "1.0" )
   aadd( txt_, c3rd + "hbide_type="              + { "Executable", "Lib", "Dll" }[ ::oUI:q_comboPrjType:currentIndex() + 1 ] )
   aadd( txt_, c3rd + "hbide_title="             + hbide_space2amp( ::oUI:q_editPrjTitle    :text() ) )
   aadd( txt_, c3rd + "hbide_location="          + hbide_space2amp( ::oUI:q_editPrjLoctn    :text() ) )
   aadd( txt_, c3rd + "hbide_workingfolder="     + hbide_space2amp( ::oUI:q_editWrkFolder   :text() ) )
   aadd( txt_, c3rd + "hbide_destinationfolder=" + hbide_space2amp( ::oUI:q_editDstFolder   :text() ) )
   aadd( txt_, c3rd + "hbide_output="            + hbide_space2amp( ::oUI:q_editOutName     :text() ) )
   aadd( txt_, c3rd + "hbide_launchparams="      + hbide_space2amp( ::oUI:q_editLaunchParams:text() ) )
   aadd( txt_, c3rd + "hbide_launchprogram="     + hbide_space2amp( ::oUI:q_editLaunchExe   :text() ) )
   aadd( txt_, c3rd + "hbide_backupfolder="      + hbide_space2amp( ::oUI:q_editBackup      :text() ) )
   aadd( txt_, " " )
   a_:= hbide_memoToArray( ::oUI:q_editFlags:toPlainText() )   ; aeval( a_, {|e| aadd( txt_, e ) } )
   aadd( txt_, " " )
   a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() ) ; aeval( a_, {|e| aadd( txt_, e ) } )
   aadd( txt_, " " )

   ::cSaveTo := ::oUI:q_editPrjLoctn:text() + ::pathSep + ::oUI:q_editOutName:text() + ".hbp"

   ::cSaveTo := hbide_pathToOSPath( ::cSaveTo )

   IF ( lOk := hbide_createTarget( ::cSaveTo, txt_ ) )
      ::aPrjProps := ::pullHbpData( hbide_pathToOSPath( ::cSaveTo ) )

      IF ( nAlready := ascan( ::aProjects, {|e_| e_[ 1 ] == hbide_pathNormalized( ::cSaveTo ) } ) ) == 0
         aadd( ::oIDE:aProjects, { hbide_pathNormalized( ::cSaveTo ), ::cSaveTo, aclone( ::aPrjProps ) } )
         IF ::lUpdateTree
            ::oIDE:updateProjectTree( ::aPrjProps )
         ENDIF
         hbide_mnuAddFileToMRU( ::oIDE, ::cSaveTo, INI_RECENTPROJECTS )
      ELSE
         ::aProjects[ nAlready, 3 ] := aclone( ::aPrjProps )
         IF ::lUpdateTree
            ::oIDE:updateProjectTree( ::aPrjProps )
         ENDIF
      ENDIF

      ::oHM:refresh()  /* Rearrange Projects Data */
   ELSE
      MsgBox( 'Error saving project file: ' + ::cSaveTo, 'Error saving project ...' )

   ENDIF

   IF lCanClose .AND. lOk
      ::oPropertiesDock:hide()
   ENDIF

   RETURN lOk

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:updateHbp( iIndex )
   LOCAL a_, txt_, s, cExt

   IF iIndex != 3
      RETURN nil
   ENDIF

   txt_:= {}

   /* Flags */
   a_:= hb_atokens( ::oUI:q_editFlags:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
         aadd( txt_, s )
      ENDIF
   NEXT
   aadd( txt_, " " )

   /* Sources */
   a_:= hb_atokens( ::oUI:q_editSources:toPlainText(), _EOL )
   FOR EACH s IN a_
      s := alltrim( s )
      IF !( "#" == left( s,1 ) ) .and. !empty( s )
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

METHOD IdeProjManager:fetchProperties()
   LOCAL cPrjLoc := hb_dirBase() + "projects"

   IF empty( ::oProject )
      ::oProject := IdeProject():new( ::oIDE, ::aPrjProps )
   ENDIF

   IF empty( ::oUI )
      ::buildInterface()
   ENDIF

   IF empty( ::aPrjProps )
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )

      ::oUI:q_editPrjTitle :setText( "" )
      ::oUI:q_editPrjLoctn :setText( hbide_pathNormalized( cPrjLoc, .F. ) )
      ::oUI:q_editWrkFolder:setText( "" )
      ::oUI:q_editDstFolder:setText( "" )
      ::oUI:q_editBackup   :setText( "" )
      ::oUI:q_editOutName  :setText( "" )

      ::oUI:q_editFlags    :setPlainText( "" )
      ::oUI:q_editSources  :setPlainText( "" )

      ::oUI:q_editLaunchParams:setText( "" )
      ::oUI:q_editLaunchExe:setText( "" )
      ::oUI:q_editHbp:setPlainText( "" )

      ::oUI:oWidget:setWindowTitle( 'New Project...' )

   ELSE
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

      ::oUI:q_editPrjTitle :setText( ::oProject:title        )
      ::oUI:q_editPrjLoctn :setText( ::oProject:location     )
      ::oUI:q_editWrkFolder:setText( ::oProject:wrkDirectory )
      ::oUI:q_editDstFolder:setText( ::oProject:destination  )
      ::oUI:q_editBackup   :setText( ::oProject:backup       )
      ::oUI:q_editOutName  :setText( ::oProject:outputName )

      ::oUI:q_editFlags    :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_FLAGS   , 1 ] ) )
      ::oUI:q_editSources  :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_SOURCES , 1 ] ) )

      ::oUI:q_editLaunchParams:setText( ::oProject:launchParams )
      ::oUI:q_editLaunchExe:setText( ::oProject:launchProgram )

      ::oUI:q_editHbp:setPlainText( "" )

      ::oUI:oWidget:setWindowTitle( 'Properties for "' + ::oUI:q_editPrjTitle:Text() + '"' )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildInterface()
   LOCAL cLukupPng

   ::oUI := HbQtUI():new( hbide_uic( "projectpropertiesex" ) ):build()
   ::oPropertiesDock:oWidget:setWidget( ::oUI )

   ::oUI:q_comboPrjType:addItem( "Executable" )
   ::oUI:q_comboPrjType:addItem( "Library"    )
   ::oUI:q_comboPrjType:addItem( "Dll"        )

   cLukupPng := hbide_image( "folder" )
   //
   ::oUI:q_buttonChoosePrjLoc:setIcon( cLukupPng )
   ::oUI:q_buttonChooseWd    :setIcon( cLukupPng )
   ::oUI:q_buttonChooseDest  :setIcon( cLukupPng )
   ::oUI:q_buttonBackup      :setIcon( cLukupPng )

   ::oUI:q_buttonSelect :setIcon( hbide_image( "open"        ) )
   ::oUI:q_buttonSort   :setIcon( hbide_image( "sort"        ) )
   ::oUI:q_buttonSortZA :setIcon( hbide_image( "sortdescend" ) )
   ::oUI:q_buttonSortOrg:setIcon( hbide_image( "invertcase"  ) )

   ::oUI:signal( "buttonCn"          , "clicked()", {|| ::lSaveOK := .f., ::oPropertiesDock:hide() } )
   ::oUI:signal( "buttonSave"        , "clicked()", {|| ::lSaveOK := .t., ::save( .F. )          } )
   ::oUI:signal( "buttonSaveExit"    , "clicked()", {|| ::lSaveOK := .t., ::save( .T. )          } )
   //
   ::oUI:signal( "buttonSelect"      , "clicked()", {|| ::addSources()         } )
   ::oUI:signal( "buttonSort"        , "clicked()", {|| ::sortSources( "az"  ) } )
   ::oUI:signal( "buttonSortZA"      , "clicked()", {|| ::sortSources( "za"  ) } )
   ::oUI:signal( "buttonSortOrg"     , "clicked()", {|| ::sortSources( "org" ) } )
   //
   ::oUI:signal( "tabWidget"         , "currentChanged(int)", {|p| ::updateHbp( p ) } )

   ::oUI:signal( "buttonChoosePrjLoc", "clicked()", {|| ::PromptForPath( 'editPrjLoctn' , 'Choose Project Location...'   ) } )
   ::oUI:signal( "buttonChooseWd"    , "clicked()", {|| ::PromptForPath( 'editWrkFolder', 'Choose Working Folder...'     ) } )
   ::oUI:signal( "buttonChooseDest"  , "clicked()", {|| ::PromptForPath( 'editDstFolder', 'Choose Destination Folder...' ) } )
   ::oUI:signal( "buttonBackup"      , "clicked()", {|| ::PromptForPath( 'editBackup'   , 'Choose Backup Folder...'      ) } )

   ::oUI:signal( "editPrjLoctn"      , "textChanged(QString)", {|cPath| ::setProjectLocation( cPath ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:synchronizeAlienProject( cProjFileName )
   LOCAL cPath, cFile, cExt, cHbp

   hb_fNameSplit( cProjFileName, @cPath, @cFile, @cExt )
   IF lower( cExt ) == ".hbp"              /* Nothing to do */
      RETURN cProjFileName
   ENDIF

   IF !( lower( cExt ) $ ".xhp" )          /* Not a valid alien project file */
      RETURN ""
   ENDIF

   cHbp := cPath + cFile + ".hbp"
   IF hb_fileExists( cHbp )
      IF ! hbide_getYesNo( "A .hbp with convered name already exists, overwrite ?", "", "Project exists" )
         RETURN ""
      ENDIF
   ENDIF

   convert_xhp_to_hbp( cProjFileName, cHbp )

   RETURN cHbp

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:sortSources( cMode )
   LOCAL a_, cTyp, s, d_, n
   LOCAL aSrc := { ".ch", ".prg", ".c", ".cpp", ".h", ".obj", ".o", ".lib", ".a", ".rc", ".res" }
   LOCAL aTxt := { {}   , {}    , {}  , {}    , {}  , {}    , {}  , {}   , {} , {}, {}    }
   LOCAL aRst := {}

   a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() )

   IF     cMode == "az"
      asort( a_, , , {|e,f| lower( hbide_stripFilter( e ) ) < lower( hbide_stripFilter( f ) ) } )
   ELSEIF cMode == "za"
      asort( a_, , , {|e,f| lower( hbide_stripFilter( f ) ) < lower( hbide_stripFilter( e ) ) } )
   ELSEIF cMode == "org"
      asort( a_, , , {|e,f| lower( hbide_stripFilter( e ) ) < lower( hbide_stripFilter( f ) ) } )

      FOR EACH s IN a_
         s := alltrim( s )
         IF left( s, 1 ) != "#"
            cTyp := hbide_sourceType( s )

            IF ( n := ascan( aSrc, {|e| cTyp == e } ) ) > 0
               aadd( aTxt[ n ], s )
            ELSE
               aadd( aRst, s )
            ENDIF
         ENDIF
      NEXT

      a_:= {}
      FOR EACH d_ IN aTxt
         IF !empty( d_ )
            aadd( a_, " #" )
            aadd( a_, " #" + aSrc[ d_:__enumIndex() ] )
            aadd( a_, " #" )
            FOR EACH s IN d_
               aadd( a_, s )
            NEXT
         ENDIF
      NEXT
      IF !empty( aRst )
         aadd( a_, " #" )
         aadd( a_, " #" + "Unrecognized..." )
         aadd( a_, " #" )
         FOR EACH s IN aRst
            aadd( a_, s )
         NEXT
      ENDIF
   ENDIF

   ::oUI:q_editSources:clear()
   ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( a_ ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:setProjectLocation( cPath )

   IF ! hb_dirExists( cPath )
      ::oUI:q_editPrjLoctn:setStyleSheet( "background-color: rgba( 240,120,120,255 );" )
      ::oUI:q_editSources:setEnabled( .f. )
      ::oUI:q_buttonSelect:setEnabled( .f. )
   ELSE
      ::oProject:location := cPath
      ::oUI:q_editPrjLoctn:setStyleSheet( "" )
      ::oUI:q_editSources:setEnabled( .T. )
      ::oUI:q_buttonSelect:setEnabled( .T. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:isValidProjectLocation( lTell )
   LOCAL lOk := .f.

   IF empty( ::oUI:q_editPrjLoctn:text() )
      IF lTell
         MsgBox( "Please supply 'Project Location' first" )
      ENDIF
   ELSEIF ! hb_dirExists( ::oUI:q_editPrjLoctn:text() )
      IF lTell
         MsgBox( "Please ensure 'Project Location' is correct" )
      ENDIF
   ELSE
      lOk := .t.
   ENDIF

   RETURN lOk

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:addSources()
   LOCAL aFiles, a_, b_, s

   IF ::isValidProjectLocation( .t. )
      IF !empty( aFiles := ::oSM:selectSource( "openmany" ) )
         a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() )

         b_:={}
         aeval( aFiles, {|e| aadd( b_, e ) } )

         FOR EACH s IN b_
            IF ascan( a_, s ) == 0
               aadd( a_, hbide_stripRoot( ::oUI:q_editPrjLoctn:text(), s ) )
            ENDIF
         NEXT

         ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( a_ ) )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
/* Set current project for build - vailtom
 * 26/12/2009 - 02:19:38
 */
METHOD IdeProjManager:setCurrentProject( cProjectName )
   LOCAL aPrjProps, n, oItem
   LOCAL cOldProject := ::cWrkProject
   LOCAL lValid      := .T.

   IF Empty( cProjectName )
      ::oIDE:cWrkProject := ''

   ELSEIF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cProjectName } ) ) > 0
      aPrjProps     := ::aProjects[ n, 3 ]
      ::oIDE:cWrkProject := aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]

   ELSE
    * MsgBox( 'Invalid project selected: "' + cProjectName + '"' )
      lValid := .F.

   ENDIF

   IF lValid
      IF !Empty( ::oSBar )
         ::oDK:setStatusText( SB_PNL_PROJECT, ::cWrkProject )
      ENDIF

      ::oIDE:updateTitleBar()
      #if 0  /* It must not be as there are more actions attached */
      ::oIDE:updateProjectMenu()
      #endif

      /* Reset Old Color */
      IF !empty( cOldProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIDE, cOldProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, QBrush():new( "QColor", QColor():new( 0,0,0 ) ) )
            //oItem:oWidget:setBackground( 0, QBrush():new( "QColor", QColor():new( 255,255,255 ) ) )
         ENDIF
      ENDIF
      /* Set New Color */
      IF !empty( ::cWrkProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIDE, ::cWrkProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, ::qBrushWrkProject )
            //oItem:oWidget:setBackground( 0, ::qBrushWrkProject )
            //hbide_expandChildren( ::oIDE, oItem )
            ::oProjTree:oWidget:setCurrentItem( oItem:oWidget )
         ENDIF
      ENDIF
   ENDIF

   RETURN cOldProject

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getCurrentProject( lAlert )

   DEFAULT lAlert TO .t.

   IF !Empty( ::cWrkProject )
      RETURN ::cWrkProject
   ENDIF

   IF Empty( ::aProjects )
      IF lAlert
         MsgBox( "No Projects Available" )
      ENDIF
      RETURN ""
   ENDIF

   IF Len( ::aProjects ) == 1
      ::setCurrentProject( ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
      RETURN ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]
   ENDIF

   RETURN ::selectCurrentProject()

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:selectCurrentProject()
   LOCAL oDlg, p, t

   IF Empty( ::aProjects )
      MsgBox( "No Projects Available" )
      RETURN ::cWrkProject
   ENDIF

   oDlg := HbQtUI():new( ::oIDE:resPath + "selectproject.uic", ::oDlg:oWidget ):build()

   FOR EACH p IN ::aProjects
      IF !empty( t := p[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
         oDlg:qObj[ "cbProjects" ]:addItem( t )
      ENDIF
   NEXT

   oDlg:signal( "btnCancel", "clicked()", {|| oDlg:oWidget:done( 1 ) } )
   oDlg:signal( "btnOk"    , "clicked()", {|| ::setCurrentProject( oDlg:qObj[ "cbProjects" ]:currentText() ), ;
                                                                                             oDlg:done( 1 ) } )
   oDlg:exec()
   oDlg:destroy()
   oDlg := NIL

   RETURN ::cWrkProject

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectsTitleList()
   LOCAL a_, aList := {}

   FOR EACH a_ IN ::aProjects
      aadd( aList, a_[ 3, PRJ_PRP_PROPERTIES, 2, PRJ_PRP_TITLE ] )
   NEXT

   RETURN aList

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectProperties( cProjectTitle )
   LOCAL n

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      RETURN ::aProjects[ n, 3 ]
   ENDIF

   RETURN {}

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectByFile( cProjectFile )
   LOCAL n, aProj

   cProjectFile := hbide_pathNormalized( cProjectFile )

   IF ( n := ascan( ::aProjects, {|e_| e_[ 1 ] == cProjectFile } ) ) > 0
      aProj := ::aProjects[ n ]
   ENDIF

   RETURN IdeProject():new( ::oIDE, aProj )

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectTypeFromTitle( cProjectTitle )
   LOCAL n, cType := ""

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      cType := ::aProjects[ n, 3, PRJ_PRP_PROPERTIES, 1, PRJ_PRP_TYPE ]
   ENDIF

   RETURN cType

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectPathFromTitle( cProjectTitle )
   LOCAL cPath

   hb_fNameSplit( ::getProjectFileNameFromTitle( cProjectTitle ), @cPath )

   RETURN cPath

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectFileNameFromTitle( cProjectTitle )
   LOCAL n, cProjFileName := ""

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      cProjFileName := ::aProjects[ n, 2 ]
   ENDIF

   RETURN cProjFileName

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getSourcesByProjectTitle( cProjectTitle )
   LOCAL n, aProj

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      aProj := ::aProjects[ n, 3 ]
      RETURN aProj[ PRJ_PRP_SOURCES, 2 ] /* 2 == parsed sources */
   ENDIF

   RETURN {}

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectByTitle( cProjectTitle )
   LOCAL n, aProj

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      aProj := ::aProjects[ n, 3 ]
   ENDIF

   RETURN IdeProject():new( ::oIDE, aProj )

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:removeProject( cProjectTitle )
   LOCAL cProjFileName, nPos

   IF !empty( cProjFileName := ::getProjectFileNameFromTitle( cProjectTitle ) )
      ::closeProject( cProjectTitle )

      nPos := ascan( ::aProjects, {|e_| e_[ 2 ] == cProjFileName } )
      IF nPos > 0
         hb_adel( ::aProjects, nPos, .T. )
         hbide_saveINI( ::oIDE )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:closeProject( cProjectTitle )
   LOCAL oProject, aProp

   IF Empty( ::aProjects )
      RETURN Self
   ENDIF

   aProp := ::getProjectProperties( cProjectTitle )
   oProject := IdeProject():new( ::oIDE, aProp )
   IF empty( oProject:title )
      RETURN Self
   ENDIF

   ::oIDE:removeProjectTree( aProp )

   ::setCurrentProject( '' )

   RETURN Self

/*----------------------------------------------------------------------*/
/* Prompt for user to select a existing folder
 * 25/12/2009 - 19:03:09 - vailtom
 */
METHOD IdeProjManager:promptForPath( cObjPathName, cTitle, cObjFileName, cObjPath2, cObjPath3 )
   LOCAL cTemp, cPath, cFile

   IF hb_isObject( ::oProject )
      cTemp := ::oUI:qObj[ cObjPathName ]:Text()
   ELSE
      cTemp := ""
   ENDIF

   IF !hb_isChar( cObjFileName )
      cPath := hbide_fetchADir( ::oDlg, cTitle, cTemp )

   ELSE
      cTemp := hbide_fetchAFile( ::oDlg, cTitle, { { "Harbour IDE Projects", "*.hbp" } }, cTemp )

      IF !Empty( cTemp )
         hb_fNameSplit( hbide_pathNormalized( cTemp, .f. ), @cPath, @cFile )

         ::oUI:qObj[ cObjFileName ]:setText( cFile )
      ENDIF
   ENDIF

   IF !Empty( cPath )
      IF Right( cPath, 1 ) $ '/\'
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

METHOD IdeProjManager:buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   LOCAL cHbpPath, oEdit, cHbpFN, cTmp, cExeHbMk2, aHbp, cCmd, cC, cArg, oSource, cCmdParams

   aHbp := {}

   DEFAULT lLaunch   TO .F.
   DEFAULT lRebuild  TO .F.
   DEFAULT lPPO      TO .F.
   DEFAULT lViaQt    TO .F.

   ::lPPO    := lPPO
   ::lLaunch := lLaunch
   ::cProjectInProcess := cProject

   IF ::lPPO .AND. empty( ::oEM:getEditCurrent() )
      MsgBox( 'No source available to be compiled' )
      RETURN Self
   ENDIF
   IF empty( cProject )
      cProject := ::getCurrentProject( .f. )
   ENDIF
   IF empty( cProject ) .AND. !( ::lPPO )
      RETURN Self
   ENDIF
   IF ::lPPO
      lRebuild := .t.
   ENDIF

   ::oProject := ::getProjectByTitle( cProject )
   // attempt to save the sources if are open in editors       should it be controlled by some option ?
   FOR EACH oSource IN ::oProject:hSources
      ::oSM:saveNamedSource( oSource:original )
   NEXT

   cHbpFN     := hbide_pathFile( ::oProject:location, iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )
   cHbpPath   := cHbpFN + iif( ::lPPO, '_tmp', "" ) + ".hbp"

   IF !( ::lPPO )
      IF     ::oProject:type == "Lib"
         aadd( aHbp, "-hblib" )
      ELSEIF ::oProject:type == "Dll"
         aadd( aHbp, "-hbdyn" )
      ENDIF
   ENDIF

   aadd( aHbp, "-q"          )
   aadd( aHbp, "-trace"      )
   aadd( aHbp, "-info"       )
   aadd( aHbp, "-lang=en"    )
   IF lRebuild
      aadd( aHbp, "-rebuild" )
   ENDIF

   IF ::lPPO
      IF !empty( oEdit := ::oEM:getEditorCurrent() )
         IF hbide_isSourcePRG( oEdit:sourceFile )
            aadd( aHbp, "-hbcmp" )
            aadd( aHbp, "-s"     )
            aadd( aHbp, "-p"     )
            aadd( aHbp, "-hbraw" )

            // TODO: We have to test if the current file is part of a project, and we
            // pull your settings, even though this is not the active project - vailtom
            aadd( aHbp, hbide_pathToOSPath( oEdit:sourceFile ) )

            ::cPPO := hbide_pathFile( oEdit:cPath, oEdit:cFile + '.ppo' )
            FErase( ::cPPO )

         ELSE
            MsgBox( 'Operation not supported for this file type: "' + oEdit:sourceFile + '"' )
            RETURN Self

         ENDIF

         lViaQt := .t.   /* Donot know why it fails with Qt */
      ENDIF
   ENDIF

   ::oDockB2:show()
   ::oOutputResult:oWidget:clear()

   IF .f.
      ::oOutputResult:oWidget:append( 'Error saving: ' + cHbpPath )

   ELSE
      cTmp := hbide_outputLine() + CRLF + ;
              "Project [ " + cProject                     + " ]    " + ;
              "Launch [ "  + iif( lLaunch , 'Yes', 'No' ) + " ]    " + ;
              "Rebuild [ " + iif( lRebuild, 'Yes', 'No' ) + " ]    " + ;
              "Started [ " + time() + " ]" + CRLF + ;
              hbide_outputLine() + CRLF
      ::oOutputResult:oWidget:append( cTmp )

      ::oIDE:oEV := IdeEnvironments():new():create( ::oIDE, hbide_pathFile( ::aINI[ INI_HBIDE, PathEnv ], "hbide.env" ) )
      ::cBatch   := ::oEV:prepareBatch( ::cWrkEnvironment )

      cExeHbMk2  := "hbmk2"   /* Needs that path is already set before calling hbmk2 */

      cCmdParams := hbide_array2cmdParams( aHbp )

      ::oProcess := HbpProcess():new()
      //
      ::oProcess:output      := {|cOut, mp2, oHbp| ::showOutput( cOut,mp2,oHbp ) }
      ::oProcess:finished    := {|nEC , nES, oHbp| ::finished( nEC ,nES,oHbp ) }
      ::oProcess:workingPath := hbide_pathToOSPath( ::oProject:location )
      //

      cCmd := hbide_getShellCommand()
      cC   := iif( hbide_getOS() == "nix", "", "/C " )
      cArg := iif( empty( ::cBatch ), cC, cC + ::cBatch + " && "  )
      //
      ::oOutputResult:oWidget:append( ::cBatch + iif( hb_fileExists( ::cBatch ), " : Exists", " : Do Not Exists" ) )
      ::oOutputResult:oWidget:append( cArg + cExeHbMk2 + " " + cHbpPath + cCmdParams  )
      ::oOutputResult:oWidget:append( hbide_outputLine() )
      //
      ::oProcess:addArg( cArg + cExeHbMk2 + " " + cHbpPath + cCmdParams )
      ::oProcess:start( cCmd )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:showOutput( cOutput, mp2, oProcess )

   hbide_justACall( mp2, oProcess )

   hbide_convertBuildStatusMsgToHtml( cOutput, ::oOutputResult:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:finished( nExitCode, nExitStatus, oProcess )
   LOCAL cTmp, n, n1, cTkn, cExe, cT

   hbide_justACall( oProcess )

   cTmp := hbide_outputLine() + CRLF
   cTmp += "Exit Code [ " + hb_ntos( nExitCode ) + " ]    Exit Status [ " + hb_ntos( nExitStatus ) + " ]    " +;
           "Finished at [ " + time()     + " ]    Done in [ " + hb_ntos( seconds() - oProcess:started ) +" Secs ]" + CRLF
   cTmp += hbide_outputLine() + CRLF

   ::oOutputResult:oWidget:append( cTmp )

   ferase( ::cBatch )

   IF ::lLaunch
      cTmp := ::oOutputResult:oWidget:toPlainText()
      cExe := ""
      IF empty( cExe )
         cTkn := "hbmk2: Linking... "
         IF ( n := at( cTkn, cTmp ) ) > 0
            cT   := ".exe" // Chr( 13 )
            n1   := hb_at( cT, cTmp, n + len( cTkn ) )
            cExe := substr( cTmp, n + len( cTkn ), n1 - n - len( cTkn ) + len( cT ) )
hbide_dbg( 1, cTkn, cExe )
         ENDIF
      ENDIF
      IF empty( cExe )
         cTkn := "hbmk2: Target up to date: "
         IF ( n := at( cTkn, cTmp ) ) > 0
            cT   := ".exe" // Chr( 13 )
            n1   := hb_at( cT, cTmp, n + len( cTkn ) )
            cExe := substr( cTmp, n + len( cTkn ), n1 - n - len( cTkn ) + len( cT ) )
hbide_dbg( 2, cTkn, cExe )
         ENDIF
      ENDIF

      IF nExitCode == 0
         ::launchProject( ::cProjectInProcess, cExe )
      ELSE
         ::oOutputResult:oWidget:append( "Sorry, cannot launch project because of errors..." )
      ENDIF
   ENDIF
   IF ::lPPO .AND. hb_FileExists( ::cPPO )
      ::editSource( ::cPPO )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Launch selected project.
 * 03/01/2010 - 09:24:50
 */
METHOD IdeProjManager:launchProject( cProject, cExe )
   LOCAL cTargetFN, cTmp, oProject, qProcess, qStr, cExt

   IF empty( cProject )
      cProject := ::oPM:getCurrentProject()
   ENDIF
   IF empty( cProject )
      RETURN Self
   ENDIF

   oProject  := ::getProjectByTitle( cProject )

   IF !empty( cExe )
      hb_fNameSplit( cExe, , , @cExt )
   ENDIF
   IF !empty( cExt ) .AND. lower( cExt ) == ".exe"
      cTargetFN := cExe
   ELSE
      cTargetFN := hbide_pathFile( oProject:destination, iif( empty( oProject:outputName ), "_temp", oProject:outputName ) )
      #ifdef __PLATFORM__WINDOWS
      IF oProject:type == "Executable"
         cTargetFN += '.exe'
      ENDIF
      #endif
   ENDIF

   IF !hb_FileExists( cTargetFN )
      cTmp := "Launch error: file not found - " + cTargetFN

   ELSEIF oProject:type == "Executable"
      cTmp := "Launching application [ " + cTargetFN + " ]"

      #if 1
      qProcess := QProcess():new()
      qProcess:setWorkingDirectory( hbide_pathToOSPath( oProject:wrkDirectory ) )
      IF !empty( oProject:launchParams )
         qStr := QStringList():new()
         qStr:append( oProject:launchParams )
         qProcess:startDetached_1( cTargetFN, qStr )
      ELSE
         qProcess:startDetached_2( cTargetFN )
      ENDIF
      qProcess:waitForStarted()
      qProcess := NIL

      #else
      ::oProcess := HbpProcess():new()
      ::oProcess:output := {|s| hbide_dbg( s ) }
      ::oProcess:finished := {|n,nn| hbide_dbg( "Finished", n, nn ) }
      ::oProcess:start( cTargetFN )
      #endif

   ELSE
      cTmp := "Launching application [ " + cTargetFN + " ] ( not applicable )."

   ENDIF

   ::oOutputResult:oWidget:append( cTmp )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:harbourFlags()
   LOCAL a_:= {}

   aadd( a_, { "/a             ", "automatic memvar declaration                         " } )
   aadd( a_, { "/b             ", "debug info                                           " } )
   aadd( a_, { "/build         ", "display detailed version info                        " } )
   aadd( a_, { "/credits       ", "display credits                                      " } )
   aadd( a_, { "/d<id>[=<val>] ", "#define <id>                                         " } )
   aadd( a_, { "/es[<level>]   ", "set exit severity                                    " } )
   aadd( a_, { "/fn[:[l|u]|-]  ", "set filename casing (l=lower u=upper)                " } )
   aadd( a_, { "/fd[:[l|u]|-]  ", "set directory casing (l=lower u=upper)               " } )
   aadd( a_, { "/fp[:<char>]   ", "set path separator                                   " } )
   aadd( a_, { "/fs[-]         ", "turn filename space trimming on or off (default)     " } )
   aadd( a_, { "/g<type>       ", "output type generated is <type> (see below)          " } )
   aadd( a_, { "/gc[<type>]    ", "output type: C source (.c) (default)                 " } )
   aadd( a_, { "               ", "<type>: 0=compact (default) 1=normal 2=verbose       " } )
   aadd( a_, { "               ", "        3=generate real C code                       " } )
   aadd( a_, { "/gh            ", "output type: Harbour Portable Object (.hrb)          " } )
   aadd( a_, { "/gd[.<destext>]", "generate dependencies list into (.d) file            " } )
   aadd( a_, { "/ge[<mode>]    ", "error output <mode>: 0=Clipper (default)             " } )
   aadd( a_, { "               ", "                     1=IDE friendly                  " } )
   aadd( a_, { "/i<path>       ", "#include file search path                            " } )
   aadd( a_, { "/i[-|+]        ", "disable/enable support for INCLUDE envvar            " } )
   aadd( a_, { "/j[<file>]     ", "generate i18n gettext file (.pot)                    " } )
   aadd( a_, { "/k             ", "compilation mode (type -k? for more data)            " } )
   aadd( a_, { "/l             ", "suppress line number information                     " } )
   aadd( a_, { "/m             ", "compile module only                                  " } )
   aadd( a_, { "/n[<type>]     ", "no implicit starting procedure                       " } )
   aadd( a_, { "               ", "<type>: 0=no implicit starting procedure             " } )
   aadd( a_, { "               ", "        1=no starting procedure at all               " } )
   aadd( a_, { "               ", "        2=add starting procedure if necessary        " } )
   aadd( a_, { "/o<path>       ", "object file drive and/or path                        " } )
   aadd( a_, { "/p[<path>]     ", "generate pre-processed output (.ppo) file            " } )
   aadd( a_, { "/p+            ", "generate pre-processor trace (.ppt) file             " } )
   aadd( a_, { "/q             ", "quiet                                                " } )
   aadd( a_, { "/q0            ", "quiet and don't display program header               " } )
   aadd( a_, { "/q2            ", "disable all output messages                          " } )
   aadd( a_, { "/r:<max>       ", "set maximum number of preprocessor iterations        " } )
   aadd( a_, { "/s[m]          ", "syntax check only [minimal for dependencies list]    " } )
   aadd( a_, { "/u[<file>]     ", "use command def set in <file> (or none)              " } )
   aadd( a_, { "/u+<file>      ", "add command def set from <file>                      " } )
   aadd( a_, { "/undef:<id>    ", "#undef <id>                                          " } )
   aadd( a_, { "/v             ", "variables are assumed M->                            " } )
   aadd( a_, { "/w[<level>]    ", "set warning level number (0..3, default 1)           " } )
   aadd( a_, { "/x[<prefix>]   ", "set symbol init function name prefix (for .c only)   " } )
   aadd( a_, { "/z             ", "suppress shortcutting (.and. & .or.)                 " } )

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:hbmk2Flags()
   LOCAL a_:= {}

   aadd( a_, { "-o<outname>       ", "output file name                                                 " } )
   aadd( a_, { "-l<libname>       ", "link with <libname> library. <libname> should be without         " } )
   aadd( a_, { "                  ", "path, extension and 'lib' prefix (unless part of libname).       " } )
   aadd( a_, { "-L<libpath>       ", "additional path to search for libraries                          " } )
   aadd( a_, { "-i<p>|-incpath=<p>", "additional path to search for headers                            " } )
   aadd( a_, { "-static|-shared   ", "link with static/shared libs                                     " } )
   aadd( a_, { "-mt|-st           ", "link with multi/single-thread VM                                 " } )
   aadd( a_, { "-gt<name>         ", "link with GT<name> GT driver, can be repeated to link with       " } )
   aadd( a_, { "                  ", "more GTs. First one will be the default at runtime               " } )
   aadd( a_, { "-hbexe            ", "create executable (default)                                      " } )
   aadd( a_, { "-hblib            ", "create static library                                            " } )
   aadd( a_, { "-hbdyn            ", "create dynamic library                                           " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-gui|-std         ", "create GUI/console executable                                    " } )
   aadd( a_, { "-main=<mainfunc>  ", "override the name of starting function/procedure                 " } )
   aadd( a_, { "-fullstatic       ", "link with all static libs                                        " } )
   aadd( a_, { "-[full|fix]shared ", "create shared Harbour binaries without/with absolute dir         " } )
   aadd( a_, { "                  ", "reference to Harbour library (default: 'fullshared' when         " } )
   aadd( a_, { "                  ", "Harbour is installed on system location, 'fixshared'             " } )
   aadd( a_, { "                  ", "otherwise) (fix/full option in *nix only)                        " } )
   aadd( a_, { "-nulrdd[-]        ", "link with nulrdd                                                 " } )
   aadd( a_, { "-[no]debug        ", "add/exclude C compiler debug info. For Harbour level             " } )
   aadd( a_, { "                  ", "debug, use Harbour option -b as usual                            " } )
   aadd( a_, { "-[no]optim        ", "toggle C compiler optimizations (default: on)                    " } )
   aadd( a_, { "-[no]cpp[=def]    ", "force C/C++ mode or reset to default                             " } )
   aadd( a_, { "-[no]map          ", "create (or not) a map file                                       " } )
   aadd( a_, { "-[no]implib       ", "create (or not) an import library (in -hbdyn mode)               " } )
   aadd( a_, { "-[no]strip        ", "strip (no strip) binaries                                        " } )
   aadd( a_, { "-[no]trace        ", "show commands executed                                           " } )
   aadd( a_, { "-[no]beep         ", "enable (or disable) single beep on successful exit, double       " } )
   aadd( a_, { "                  ", "beep on failure                                                  " } )
   aadd( a_, { "-[no]ignore       ", "ignore errors when running compiler tools (default: off)         " } )
   aadd( a_, { "-[no]hbcppmm      ", "forces to override standard C++ memory management                " } )
   aadd( a_, { "                  ", "functions with Harbour ones                                      " } )
   aadd( a_, { "-nohblib[-]       ", "do not use static core Harbour libraries when linking            " } )
   aadd( a_, { "-nolibgrouping[-] ", "disable library grouping on gcc based compilers                  " } )
   aadd( a_, { "-nomiscsyslib[-]  ", "don't add extra list of system libraries to default              " } )
   aadd( a_, { "                  ", "library list                                                     " } )
   aadd( a_, { "-traceonly        ", "show commands to be executed, but don't execute them             " } )
   aadd( a_, { "-[no]warn[=lev]   ", "set C compiler warning level                                     " } )
   aadd( a_, { "                  ", "<lev> can be: yes, no, def (default: yes)                        " } )
   aadd( a_, { "-[no]compr[=lev]  ", "compress executable/dynamic lib (needs UPX)                      " } )
   aadd( a_, { "                  ", "<lev> can be: min, max, def                                      " } )
   aadd( a_, { "-[no]run          ", "run/don't run output executable                                  " } )
   aadd( a_, { "-vcshead=<file>   ", "generate .ch header file with local repository                   " } )
   aadd( a_, { "                  ", "information. SVN, CVS, Git, Mercurial, Bazaar and Fossil         " } )
   aadd( a_, { "                  ", "are currently supported. Generated header will define            " } )
   aadd( a_, { "                  ", "macro _HBMK_VCS_TYPE_ with the name of detected VCS and          " } )
   aadd( a_, { "                  ", "_HBMK_VCS_ID_ with the unique ID of local repository             " } )
   aadd( a_, { "-tshead=<file>    ", "generate .ch header file with timestamp information.             " } )
   aadd( a_, { "                  ", "Generated header will define macros _HBMK_BUILD_DATE_,           " } )
   aadd( a_, { "                  ", "_HBMK_BUILD_TIME_, _HBMK_BUILD_TIMESTAMP_ with the               " } )
   aadd( a_, { "                  ", "date/time of build                                               " } )
   aadd( a_, { "-icon=<file>      ", "set <file> as application icon. <file> should be a               " } )
   aadd( a_, { "                  ", "supported format on the target platform (experimental)           " } )
   aadd( a_, { "-instpath=<path>  ", "copy target to <path>. if <path> is a directory, it should       " } )
   aadd( a_, { "                  ", "end with path separator. can be specified multiple times         " } )
   aadd( a_, { "-nohbc            ", "do not process .hbc files in current directory                   " } )
   aadd( a_, { "-stop             ", "stop without doing anything                                      " } )
   aadd( a_, { "-echo=<text>      ", "echo text on screen                                              " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-bldf[-]          ", "inherit all/no (default) flags from Harbour build                " } )
   aadd( a_, { "-bldf=[p][c][l]   ", "inherit .prg/.c/linker flags (or none) from Harbour build        " } )
   aadd( a_, { "-inctrypath=<p>   ", "additional path to autodetect .c header locations                " } )
   aadd( a_, { "-prgflag=<f>      ", "pass flag to Harbour                                             " } )
   aadd( a_, { "-cflag=<f>        ", "pass flag to C compiler                                          " } )
   aadd( a_, { "-resflag=<f>      ", "pass flag to resource compiler (Windows only)                    " } )
   aadd( a_, { "-ldflag=<f>       ", "pass flag to linker (executable)                                 " } )
   aadd( a_, { "-aflag=<f>        ", "pass flag to linker (static library)                             " } )
   aadd( a_, { "-dflag=<f>        ", "pass flag to linker (dynamic library)                            " } )
   aadd( a_, { "-runflag=<f>      ", "pass flag to output executable when -run option is used          " } )
   aadd( a_, { "-3rd=<f>          ", "options/flags reserved for 3rd party tools, always ignored       " } )
   aadd( a_, { "                  ", "by hbmk2 itself                                                  " } )
   aadd( a_, { "-jobs=<n>         ", "start n compilation threads (multiprocess platforms only)        " } )
   aadd( a_, { "-inc              ", "enable incremental build mode                                    " } )
   aadd( a_, { "-[no]head[=<m>]   ", "control source header parsing (in incremental build mode)        " } )
   aadd( a_, { "                  ", "<m> can be: native (uses compiler to extract                     " } )
   aadd( a_, { "                  ", "dependencies), full (uses simple text parser on the whole        " } )
   aadd( a_, { "                  ", "file), partial (default, uses simple text parser on 1st          " } )
   aadd( a_, { "                  ", "16KB chunk of the file), off                                     " } )
   aadd( a_, { "-rebuild          ", "rebuild all (in incremental build mode)                          " } )
   aadd( a_, { "-clean            ", "clean (in incremental build mode)                                " } )
   aadd( a_, { "-workdir=<dir>    ", "working directory                                                " } )
   aadd( a_, { "                  ", "(default: .hbmk/plat/comp in incremental mode, OS temp           " } )
   aadd( a_, { "                  ", "directory otherwise)                                             " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-hbl[=<output>]   ", "output .hbl filename. %{hb_lng} macro is accepted in             " } )
   aadd( a_, { "                  ", "filename                                                         " } )
   aadd( a_, { "-lng=<languages>  ", "list of languages to be replaced in %{hb_lng} macros in          " } )
   aadd( a_, { "                  ", ".pot/.po filenames and output .hbl/.po filenames. Comma          " } )
   aadd( a_, { "                  ", "separared list:                                                  " } )
   aadd( a_, { "                  ", "-lng=en,hu-HU,de                                                 " } )
   aadd( a_, { "-po=<output>      ", "create/update .po file from source. Merge it with previous       " } )
   aadd( a_, { "                  ", ".po file of the same name                                        " } )
   aadd( a_, { "-[no]minipo       ", "don't (or do) add Harbour version number and source file         " } )
   aadd( a_, { "                  ", "reference to .po (default: add them)                             " } )
   aadd( a_, { "-rebuildpo        ", "recreate .po file, thus removing all obsolete entries in         " } )
   aadd( a_, { "                  ", "it                                                               " } )
   aadd( a_, { "                  ", "                                                                 " } )
*  aadd( a_, { "Options below are ", "vailable on command line only:                                   " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-target=<script>  ", "specify a new build target. <script> can be .prg (or no          " } )
   aadd( a_, { "                  ", "extension) or .hbm/.hbp file                                     " } )
   aadd( a_, { "-target           ", "marks beginning of options belonging to a new build target       " } )
   aadd( a_, { "-alltarget        ", "marks beginning of common options belonging to all targets       " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-env:<e>[<o>[<v>]]", "alter local environment. <e> is the name of the                  " } )
   aadd( a_, { "                  ", "environment variable to alter. <o> can be '=' to                 " } )
   aadd( a_, { "                  ", "set/override, '-' to delete, '+' to append to the end of         " } )
   aadd( a_, { "                  ", "existing value, '#' to insert to the beginning of existing       " } )
   aadd( a_, { "                  ", "value. <v> is the value to set/append/insert. If multiple        " } )
   aadd( a_, { "                  ", "options are passed, they are processed from left to right.       " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-hbrun            ", "run target                                                       " } )
   aadd( a_, { "-hbraw            ", "stop after running Harbour compiler                              " } )
   aadd( a_, { "-hbcmp|-clipper   ", "stop after creating the object files                             " } )
   aadd( a_, { "                  ", "create link/copy hbmk2 to hbcmp/clipper for the same             " } )
   aadd( a_, { "                  ", "effect                                                           " } )
   aadd( a_, { "-hbcc             ", "stop after creating the object files and accept raw C            " } )
   aadd( a_, { "                  ", "flags                                                            " } )
   aadd( a_, { "                  ", "create link/copy hbmk2 to hbcc for the same effect               " } )
   aadd( a_, { "-hblnk            ", "accept raw linker flags                                          " } )
   aadd( a_, { "-hb10             ", "enable Harbour 1.0.x compatibility mode (experimental)           " } )
   aadd( a_, { "-xhb              ", "enable xhb mode (experimental)                                   " } )
   aadd( a_, { "-hbc              ", "enable pure C mode (experimental)                                " } )
   aadd( a_, { "-exospace         ", "emulate Clipper compatible linker behavior                       " } )
   aadd( a_, { "                  ", "create link/copy hbmk2 to rtlink/blinker/exospace for the        " } )
   aadd( a_, { "                  ", "same effect                                                      " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-hbmake=<file>    ", "convert hbmake project file to .hbp file (experimental)          " } )
   aadd( a_, { "-xbp=<file>       ", "convert .xbp (xbuild) project file to .hbp file                  " } )
   aadd( a_, { "                  ", "(experimental)                                                   " } )
   aadd( a_, { "-xhp=<file>       ", "convert .xhp (xMate) project file to .hbp file                   " } )
   aadd( a_, { "                  ", "(experimental)                                                   " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "--hbdirbin        ", "output Harbour binary directory                                  " } )
   aadd( a_, { "--hbdirdyn        ", "output Harbour dynamic library directory                         " } )
   aadd( a_, { "--hbdirlib        ", "output Harbour static library directory                          " } )
   aadd( a_, { "--hbdirinc        ", "output Harbour header directory                                  " } )
   aadd( a_, { "                  ", "                                                                 " } )
   aadd( a_, { "-plat[form]=<plat>", "select target platform.                                          " } )
   aadd( a_, { "-comp[iler]=<comp>", "select C compiler.                                               " } )
   aadd( a_, { "                  ", "Special value:                                                   " } )
   aadd( a_, { "                  ", " - bld: use original build settings (default on *nix)            " } )
   aadd( a_, { "-build=<name>     ", "use a specific build name                                        " } )
   aadd( a_, { "-lang=<lang>      ", "override default language. Similar to HB_LANG envvar.            " } )
   aadd( a_, { "-width=<n>        ", "set output width to <n> characters.                              " } )
   aadd( a_, { "--version         ", "display version header only                                      " } )
   aadd( a_, { "-pause            ", "force waiting for a key on exit in case of failure (with         " } )
   aadd( a_, { "                  ", "alternate GTs only)                                              " } )
   aadd( a_, { "-info             ", "turn on informational messages                                   " } )
   aadd( a_, { "-quiet            ", "suppress all screen messages                                     " } )

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:xppCompileFlags()
   LOCAL a_:= {}

   aadd( a_, { "/a                   ", "automatic MEMVAR declaration                             " } )
   aadd( a_, { "/b                   ", "include debug information                                " } )
   aadd( a_, { "/coff                ", "create COFF object file                                  " } )
   aadd( a_, { "/com                 ", "Compatibility-Mode (max. 10 significant chars for ident.)" } )
   aadd( a_, { "/d<id>[=<val>]       ", "#define <id>                                             " } )
   aadd( a_, { "/dll[:DYNAMIC|STATIC]", "create output file that can be used in a                 " } )
   aadd( a_, { "                     ", "dynamic or static DLL; default: static                   " } )
   aadd( a_, { "/err:<count>         ", "abort compilation after <count> errors (default: 20)     " } )
   aadd( a_, { "/es                  ", "compiler returns with error code if warnings are detected" } )
   aadd( a_, { "/ga                  ", "convert string characters ANSI -> OEM                    " } )
   aadd( a_, { "/go                  ", "convert string characters OEM -> ANSI                    " } )
   aadd( a_, { "/i<path>             ", "include file search path                                 " } )
   aadd( a_, { "/l                   ", "suppress linenumber embedding                            " } )
   aadd( a_, { '/link[:"<options>"]  ', "invoke linker with <options> to build exe file           " } )
   aadd( a_, { "/m                   ", "do not process SET PROCEDURE TO (ProcRequest)            " } )
   aadd( a_, { "/n                   ", "no implicit starting procedure (MAIN)                    " } )
   aadd( a_, { "/nod                 ", "suppress request for default library in output file      " } )
   aadd( a_, { "/o<name>             ", "rename output file                                       " } )
   aadd( a_, { "/omf                 ", "create OMF object file                                   " } )
   aadd( a_, { "/p                   ", "create pre-processed output file                         " } )
   aadd( a_, { "/pptrace             ", "trace preprocessor's work                                " } )
   aadd( a_, { "/profile             ", "generate profiling information                           " } )
   aadd( a_, { "/q                   ", "quiet mode                                               " } )
   aadd( a_, { "/r<libname>          ", "add request for library <libname>                        " } )
   aadd( a_, { "/s                   ", "syntax check only                                        " } )
   aadd( a_, { "/u[<filename>]       ", "use command definition set in <filename> (or none)       " } )
   aadd( a_, { "/v                   ", "undeclared variables are assumed to be MEMVARs           " } )
   aadd( a_, { "/w                   ", "enable standard warnings                                 " } )
   aadd( a_, { "/wi                  ", "warn about access of uninitialized lex. variables        " } )
   aadd( a_, { "/wl                  ", "warn about use of dynamic scoped variables               " } )
   aadd( a_, { "/wn                  ", "warn about suspicious implicitly declared NILs           " } )
   aadd( a_, { "/wu                  ", "warn about unused lexical variables                      " } )
   aadd( a_, { "/z                   ", "suppress short-cut optimization                          " } )

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:xppLinkFlags()
   LOCAL a_:= {}

 * usage: ALINK [options] [files] [@commandfile

   aadd( a_, { "/BASE:address                         " } )
   aadd( a_, { "/DE[BUG]                              " } )
   aadd( a_, { "/DEFAULTLIB:library                   " } )
   aadd( a_, { "/DLL                                  " } )
   aadd( a_, { "/FORCE:{MULTIPLE|UNRESOLVED}          " } )
   aadd( a_, { "/HELP or /?                           " } )
   aadd( a_, { "/MAP[:filename]                       " } )
   aadd( a_, { "/NOD[EFAULTLIB][:library]             " } )
   aadd( a_, { "/NOL[OGO]                             " } )
   aadd( a_, { "/OUT:filename                         " } )
   aadd( a_, { "/PM[TYPE]:{PM|VIO}                    " } )
   aadd( a_, { "/ST[ACK]:max[,min]                    " } )
   aadd( a_, { "/SUBSYSTEM:{WINDOWS|CONSOLE}[,#[.##]] " } )
   aadd( a_, { "/VERBOSE                              " } )
   aadd( a_, { "/VERSION:#[.#]                        " } )

   RETURN a_

/*----------------------------------------------------------------------*/

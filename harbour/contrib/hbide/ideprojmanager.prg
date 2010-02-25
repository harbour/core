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
//
//                           Class IdeEnvironments
//
/*----------------------------------------------------------------------*/

CLASS IdeEnvironments

   DATA   oIDE
   DATA   cEnvFile
   DATA   aNames                                  INIT {}
   DATA   aEnvrns                                 INIT {}
   DATA   aShellContents                          INIT {}
   DATA   aCommons                                INIT {}

   METHOD new( oIDE, cEnvFile )
   METHOD create( oIDE, cEnvFile )
   METHOD parse( aContents )
   METHOD prepareBatch( cEnviron )
   METHOD getNames()                              INLINE ::aNames

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:new( oIDE, cEnvFile )

   ::oIDE     := oIDE
   ::cEnvFile := cEnvFile

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:create( oIDE, cEnvFile )
   LOCAL a_

   DEFAULT oIDE     TO ::oIDE
   DEFAULT cEnvFile TO ::cEnvFile

   ::oIDE     := oIDE
   ::cEnvFile := cEnvFile

   IF empty( ::cEnvFile ) .OR. !hb_fileExists( ::cEnvFile )
      RETURN Self
   ENDIF

   a_:= hbide_readSource( ::cEnvFile )

   ::parse( a_ )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:parse( aContents )
   LOCAL s, cPart, cEnv, a_, cKey, cVal

   a_:= {}
   cEnv := ""
   FOR EACH s IN aContents
      s := alltrim( s )
      IF empty( s ) .OR. left( s, 1 ) == "#"                     /* Remark */
         LOOP
      ENDIF
      IF left( s, 1 ) == "["
         s := alltrim( strtran( s, "[", "" ) )
         s := alltrim( strtran( s, "]", "" ) )
         IF lower( s ) == "common"
            cPart := "common"
         ELSE
            cPart := "environment"
            IF ( s != cEnv ) .AND. !empty( cEnv )
               aadd( ::aNames, cEnv )
               aadd( ::aEnvrns, { cEnv, a_ } )
            ENDIF
            cEnv := s
            a_:= {}
         ENDIF
      ELSE
         IF cPart == "common"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               aadd( ::aCommons, { lower( cKey ), cVal } )       /* Format Later */
            ENDIF
         ELSEIF cPart == "environment"
            IF hbide_parseFilter( s, @cKey, @cVal )
               aadd( a_, { lower( cKey ), cVal } )
            ENDIF
         ENDIF
      ENDIF
   NEXT
   IF !empty( cEnv ) .AND. !empty( a_ )
      aadd( ::aNames, cEnv )
      aadd( ::aEnvrns, { cKey, a_ } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEnvironments:prepareBatch( cEnviron )
   LOCAL n, s, a_, aCmd
   LOCAL cFile := space( 255 )
   //LOCAL k, nHandle

   IF ( n := ascan( ::aEnvrns, {|e_| e_[ 1 ] == cEnviron } ) ) > 0
      aCmd := {}
      FOR EACH a_ IN ::aEnvrns[ n, 2 ]
         s := a_[ 1 ]
         IF s == "content"
            aadd( aCmd, a_[ 2 ] )
         ENDIF
      NEXT

      cFile := hbide_getShellCommandsTempFile( aCmd )

      #if 0
      IF ( nHandle := HB_FTempCreateEx( @cFile, NIL, "ide_", ".bat" ) ) > 0
         k := ""
         FOR EACH a_ IN ::aEnvrns[ n, 2 ]
            s := a_[ 1 ]
            IF s == "content"
               k += a_[ 2 ] + CRLF
            ENDIF
         NEXT
         fWrite( nHandle, k )
         fClose( nHandle )
      ENDIF
      #endif
   ENDIF

   RETURN cFile

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
   DATA   wrkDirectory                            INIT hb_dirBase() + "projects"
   DATA   destination                             INIT hb_dirBase() + "projects"
   DATA   outputName                              INIT hb_dirBase() + "projects"
   DATA   backup                                  INIT ""
   DATA   launchParams                            INIT ""
   DATA   launchProgram                           INIT ""
   DATA   hbpFlags                                INIT {}
   DATA   sources                                 INIT {}
   DATA   metaData                                INIT {}
   DATA   dotHbp                                  INIT ""
   DATA   compilers                               INIT ""
   DATA   cPathMk2                                INIT hb_getenv( "HBIDE_DIR_HBMK2" )
   DATA   cPathEnv                                INIT hb_DirBase() + "resources"
   DATA   hSources                                INIT {=>}
   DATA   hPaths                                  INIT {=>}
   DATA   lPathAbs                                INIT .F.  // Lets try relative paths first . xhp and hbp will be relative anyway

   METHOD new( oIDE, aProps )
   METHOD applyMeta( s )
   METHOD expandMeta( s )

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
      ::location       := a_[ E_oPrjLoc  ]
      ::wrkDirectory   := a_[ E_oPrjWrk  ]
      ::destination    := a_[ E_oPrjDst  ]
      ::outputName     := a_[ E_oPrjOut  ]
      ::launchParams   := a_[ E_oPrjLau  ]
      ::launchProgram  := a_[ E_oPrjLEx  ]

      ::hbpFlags       := aclone( b_[ PRJ_PRP_FLAGS   , 2 ] )
      ::sources        := aclone( b_[ PRJ_PRP_SOURCES , 2 ] )
      ::metaData       := aclone( b_[ PRJ_PRP_METADATA, 2 ] )
      ::dotHbp         := ""
      ::compilers      := ""

      IF empty( ::destination )
         ::destination := ::location
      ENDIF
      IF empty( ::wrkDirectory )
         ::wrkDirectory := ::location
      ENDIF
      IF !empty( oIDE:aINI[ INI_HBIDE, PathMk2 ] )
         ::cPathMk2 := oIDE:aINI[ INI_HBIDE, PathMk2 ]
      ENDIF
      IF !empty( oIDE:aINI[ INI_HBIDE, PathEnv ] )
         ::cPathEnv := oIDE:aINI[ INI_HBIDE, PathEnv ]
      ENDIF

      FOR EACH a_ IN ::metaData
         a_[ 2 ] := hbide_pathNormalized( a_[ 2 ], .f. )
      NEXT

      FOR EACH cSource IN ::sources
         oSource := IdeSource():new( cSource )
         ::hSources[ oSource:normalized ] := oSource
         ::hPaths[ oSource:path ] := NIL
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProject:applyMeta( s )
   LOCAL k
   LOCAL a_:= ::metaData

   IF ! Empty( a_ )
      FOR EACH k IN a_
         s := StrTran( hbide_pathNormalized( s, .f. ), k[ 2 ],  k[ 1 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/

METHOD IdeProject:expandMeta( s )
   LOCAL k
   LOCAL a_:= ::metaData

   IF ! Empty( a_ )
      FOR EACH k IN a_ DESCEND
         s := StrTran( hbide_pathNormalized( s, .f. ), k[ 1 ], k[ 2 ] )
      NEXT
   ENDIF

   RETURN s

/*----------------------------------------------------------------------*/
//                            IdeProjectManager
/*----------------------------------------------------------------------*/

CLASS IdeProjManager INHERIT IdeObject

   DATA   cSaveTo
   DATA   aPrjProps                               INIT {}

   DATA   nStarted                                INIT 0

   DATA   lLaunch                                 INIT .f.
   DATA   cProjectInProcess                       INIT ""
   DATA   cPPO                                    INIT ""
   DATA   lPPO                                    INIT .f.
   DATA   oProject
   DATA   cBatch
   DATA   oProcess
   DATA   lSaveOK                                 INIT .f.

   METHOD new( oIDE )
   METHOD create( oIDE )
   METHOD destroy()

   METHOD populate()
   METHOD loadProperties( cProjFileName, lNew, lFetch, lUpdateTree )
   METHOD fetchProperties()
   METHOD getProperties()
   METHOD sortSources( cMode )
   METHOD updateMetaData()
   METHOD save( lCanClose )
   METHOD updateHbp( iIndex )
   METHOD addSources()
   METHOD getProjectsTitleList()
   METHOD setCurrentProject( cProjectName )
   METHOD getCurrentProject( lAlert )
   METHOD selectCurrentProject()
   METHOD getProjectProperties( cProjectTitle )
   METHOD getProjectByFile( cProjectFile )
   METHOD getProjectFileNameFromTitle( cProjectTitle )
   METHOD getProjectByTitle( cProjectTitle )
   METHOD getSourcesByProjectTitle( cProjectTitle )
   METHOD removeProject( cProjectTitle )
   METHOD closeProject( cProjectTitle )
   METHOD promptForPath( cObjPathName, cTitle, cObjFileName, cObjPath2, cObjPath3 )
   METHOD buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   METHOD launchProject( cProject )
   METHOD showOutput( cOutput, mp2, oProcess )
   METHOD finished( nExitCode, nExitStatus, oProcess )
   METHOD saveEnvironments()
   METHOD manageEnvironments()
   METHOD loadXhpProject()
   METHOD loadHbpProject()
   METHOD isValidProjectLocation( lTell )
   METHOD setProjectLocation( cPath )
   METHOD buildInterface()

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
      // ::oUI:destroy()
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
   LOCAL aPrj, cHbi, cTmp, n

   IF Empty( ::cWrkProject )
      MsgBox( 'No active project detected' )
   ENDIF
   cTmp := ::getCurrentProject()
   IF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cTmp } ) ) > 0
      aPrj := ::aProjects[ n, 3 ]
      cHbi := aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_LOCATION ] + ::pathSep + ;
              aPrj[ PRJ_PRP_PROPERTIES, 2, PRJ_PRP_OUTPUT   ] + ".hbi"

      ::loadProperties( cHbi, .f., .t., .t. )
   ELSE
      MsgBox( 'Invalid project: ' + cTmp )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadProperties( cProjFileName, lNew, lFetch, lUpdateTree )
   LOCAL n, t, cWrkProject

   DEFAULT cProjFileName TO ""
   DEFAULT lNew          TO .F.
   DEFAULT lFetch        TO .T.
   DEFAULT lUpdateTree   TO .F.

   /* Never touch original project file name */

   ::aPrjProps := {}
   ::cSaveTo   := ""

   IF lNew
      lFetch := .t.
   ELSE
      IF empty( cProjFileName )
         cProjFileName := hbide_fetchAFile( ::oDlg, "Load Project...", { { "Harbour IDE Projects (*.hbi)", "*.hbi" } } )
      ENDIF
      IF empty( cProjFileName )
         RETURN Self
      ENDIF
   ENDIF

   n := 0
   IF !empty( cProjFileName )
      cWrkProject := hbide_pathNormalized( cProjFileName )                                 /* normalize project name */
      IF ( n := ascan( ::aProjects, {|e_| hbide_pathNormalized( e_[ 1 ] ) == cWrkProject } ) ) > 0
         ::aPrjProps := ::aProjects[ n, 3 ]
         t := ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ]
      ENDIF
      IF empty( ::aPrjProps )
         ::aPrjProps := hbide_fetchHbiStructFromFile( hbide_pathToOSPath( cProjFileName ) )
      ENDIF
      ::oIDE:aMeta := ::aPrjProps[ PRJ_PRP_METADATA, 2 ]
   ENDIF

   IF lFetch
      /* Access/Assign via this object */
      ::oProject := IdeProject():new( ::oIDE, ::aPrjProps )
      //
      //::fetchProperties()
      //
      ::oPropertiesDock:show()
      //
      IF !empty( ::cSaveTo ) .and. hb_FileExists( ::cSaveTo )
         cProjFileName := ::cSaveTo
         ::aPrjProps := hbide_fetchHbiStructFromFile( hbide_pathToOSPath( cProjFileName ) ) /* Reload from file */
      ENDIF
   ENDIF

   IF lFetch .AND. empty( ::cSaveTo )
      RETURN Self
   ENDIF

   IF n == 0
      aadd( ::oIDE:aProjects, { hbide_pathNormalized( cProjFileName ), cProjFileName, aclone( ::aPrjProps ) } )
      IF lUpdateTree
         ::oIDE:updateProjectTree( ::aPrjProps )
      ENDIF
      hbide_mnuAddFileToMRU( ::oIDE, cProjFileName, INI_RECENTPROJECTS )
   ELSE
      ::aProjects[ n, 3 ] := aclone( ::aPrjProps )
      IF lUpdateTree
         ::oIDE:updateProjectTree( ::aPrjProps )
      ENDIF
      IF lUpdateTree .AND. ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] <> t
         MsgBox( "::removeProjectFromTree( ::aPrjProps )" )
      ENDIF
   ENDIF

   RETURN Self

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
      ::oUI:q_editMetaData :setPlainText( "" )

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

      ::oUI:q_editPrjTitle :setText( ::oProject:applyMeta( ::oProject:title        ) )
      ::oUI:q_editPrjLoctn :setText( ::oProject:applyMeta( ::oProject:location     ) )
      ::oUI:q_editWrkFolder:setText( ::oProject:applyMeta( ::oProject:wrkDirectory ) )
      ::oUI:q_editDstFolder:setText( ::oProject:applyMeta( ::oProject:destination  ) )
      ::oUI:q_editBackup   :setText( ::oProject:applyMeta( ::oProject:backup       ) )
      ::oUI:q_editOutName  :setText( ::oProject:outputName )

      ::oUI:q_editFlags    :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_FLAGS   , 1 ] ) )
      ::oUI:q_editSources  :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_SOURCES , 1 ] ) )
      ::oUI:q_editMetaData :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_METADATA, 1 ] ) )

      ::oUI:q_editLaunchParams:setText( ::oProject:launchParams )
      ::oUI:q_editLaunchExe:setText( ::oProject:launchProgram )

      ::oUI:q_editHbp:setPlainText( "" )

      ::oUI:oWidget:setWindowTitle( 'Properties for "' + ::oUI:q_editPrjTitle:Text() + '"' )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildInterface()
   LOCAL cLukupPng

   ::oUI := HbQtUI():new( ::resPath + "projectpropertiesex.uic", ::oDlg:oWidget ):build()

   ::oPropertiesDock:oWidget:setWidget( ::oUI )
   ::oPropertiesDock:qtObject := ::oUI

   ::oUI:q_comboPrjType:addItem( "Executable" )
   ::oUI:q_comboPrjType:addItem( "Library"    )
   ::oUI:q_comboPrjType:addItem( "Dll"        )

   cLukupPng := hbide_image( "folder" )
   //
   ::oUI:q_buttonChoosePrjLoc:setIcon( cLukupPng )
   ::oUI:q_buttonChooseWd    :setIcon( cLukupPng )
   ::oUI:q_buttonChooseDest  :setIcon( cLukupPng )
   ::oUI:q_buttonBackup      :setIcon( cLukupPng )
   ::oUI:q_buttonXmate       :setIcon( hbide_image( "xmate" ) )
   ::oUI:q_buttonHbp         :setIcon( hbide_image( "open"  ) )

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
   ::oUI:signal( "editMetaData"      , "textChanged()"      , {|| ::updateMetaData() } )

   ::oUI:signal( "buttonChoosePrjLoc", "clicked()", {|| ::PromptForPath( 'editPrjLoctn' , 'Choose Project Location...'   ) } )
   ::oUI:signal( "buttonChooseWd"    , "clicked()", {|| ::PromptForPath( 'editWrkFolder', 'Choose Working Folder...'     ) } )
   ::oUI:signal( "buttonChooseDest"  , "clicked()", {|| ::PromptForPath( 'editDstFolder', 'Choose Destination Folder...' ) } )
   ::oUI:signal( "buttonBackup"      , "clicked()", {|| ::PromptForPath( 'editBackup'   , 'Choose Backup Folder...'      ) } )
   ::oUI:signal( "buttonXmate"       , "clicked()", {|| ::loadXhpProject() } )
   ::oUI:signal( "buttonHbp"         , "clicked()", {|| ::loadHbpProject() } )

   ::oUI:signal( "editPrjLoctn"      , "textChanged(QString)", {|cPath| ::setProjectLocation( cPath ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:save( lCanClose )
   LOCAL a_, lOk, cPath, txt_

   * Validate certain parameters before continuing ... (vailtom)

   /*   Title cannot be the output name, but reverse is possible
        --------------------------------------------------------
        We must also consider that user may be building the project in parts
        OR may be basic definition must be in place
    */
   IF Empty( ::oUI:q_editPrjTitle:text() )
      ::oUI:q_editPrjTitle:setText( ::oUI:q_editOutName:text() )
   ENDIF

   IF Empty( ::oUI:q_editOutName:text() )
      MsgBox( 'Invalid Output FileName' )
      ::oUI:q_editOutName:setFocus()
      RETURN .F.
   ENDIF

   /* This must be valid, we cannot skip */
   IF !hbide_isValidPath( ::oProject:expandMeta( ::oUI:q_editPrjLoctn:text() ), 'Project Location' )
      ::oUI:q_editPrjLoctn:setFocus()
      RETURN .F.
   ENDIF

   /* This we can skip now: later at project building we can check: TO:RECONSIDER */
   IF !empty( cPath := ::oUI:q_editWrkFolder:text() )
      IF !hbide_isValidPath( ::oProject:expandMeta( cPath ), 'Working Folder' )
         // ::oUI:q_editWrkFolder:setText( ::oUI:q_editPrjLoctn:text() )
         RETURN .F.
      ENDIF
   ENDIF

   /* This we can skip now: later at project building we can check: TO:RECONSIDER */
   IF !empty( cPath := ::oUI:q_editDstFolder:text() )
      IF !hbide_isValidPath( ::oProject:expandMeta( cPath ), 'Destination Folder' )
         // ::oUI:q_editDstFolder:setText( ::oUI:q_editPrjLoctn:text() )
         RETURN .F.
      ENDIF
   ENDIF

   txt_:= {}
   //
   aadd( txt_, "[ PROPERTIES ]" )
   aadd( txt_, "Type              = " + { "Executable", "Lib", "Dll" }[ ::oUI:q_comboPrjType:currentIndex()+1 ] )
   aadd( txt_, "Title             = " + ::oUI:q_editPrjTitle    :text() )
   aadd( txt_, "Location          = " + ::oUI:q_editPrjLoctn    :text() )
   aadd( txt_, "WorkingFolder     = " + ::oUI:q_editWrkFolder   :text() )
   aadd( txt_, "DestinationFolder = " + ::oUI:q_editDstFolder   :text() )
   aadd( txt_, "Output            = " + ::oUI:q_editOutName     :text() )
   aadd( txt_, "LaunchParams      = " + ::oUI:q_editLaunchParams:text() )
   aadd( txt_, "LaunchProgram     = " + ::oUI:q_editLaunchExe   :text() )
   aadd( txt_, "BackupFolder      = " + ::oUI:q_editBackup      :text() )
   aadd( txt_, " " )
   //
   aadd( txt_, "[ FLAGS ]" )
   a_:= hbide_memoToArray( ::oUI:q_editFlags:toPlainText() )   ; aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ SOURCES ]" )
   a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() ) ; aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )
   aadd( txt_, "[ METADATA ]" )
   a_:= hbide_memoToArray( ::oUI:q_editMetaData:toPlainText() ); aeval( a_, {|e| aadd( txt_, e ) } ) ; aadd( txt_, " " )

   #if 0
   /* Setup Meta Keys */
   a4_1 := hbide_setupMetaKeys( a_ )

   ::cSaveTo := hbide_parseWithMetaData( ::oUI:q_editPrjLoctn:text(), a4_1 ) + ;
                      ::pathSep + ;
                hbide_parseWithMetaData( ::oUI:q_editOutName:text(), a4_1 ) + ;
                      ".hbi"
   ::cSaveTo := hbide_pathToOSPath( ::cSaveTo )
   #endif

   ::cSaveTo := ::oProject:expandMeta( ::oUI:q_editPrjLoctn:text() ) + ;
                      ::pathSep + ;
                ::oProject:expandMeta( ::oUI:q_editOutName:text() ) + ;
                      ".hbi"
   ::cSaveTo := hbide_pathToOSPath( ::cSaveTo )

   IF ( lOk := hbide_createTarget( ::cSaveTo, txt_ ) )
*     MsgBox( 'The project file is saved successfully: ' + ::cSaveTo, 'Saving project ...' )
   ELSE
      MsgBox( 'Error saving project file: ' + ::cSaveTo, 'Error saving project ...' )
   ENDIF

   IF lCanClose .AND. lOk
      ::oPropertiesDock:hide()
   ENDIF

   RETURN lOk

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadHbpProject()
   LOCAL cHbp, aData, aOptns, aFiles, cHome, cOutName, cType, n

   cHbp := hbide_fetchAFile( ::oDlg, "Selecet Harbour Project File", { { "Harbour Project Files", "*.hbp" } } )
   IF empty( cHbp )
      RETURN Self
   ENDIF
   hb_fNameSplit( cHbp, @cHome, @cOutName )
   cHome  := hbide_pathStripLastSlash( cHome )

   aData  := hbide_fetchHbpData( cHbp )
   aOptns := aData[ 1 ]
   aFiles := aData[ 2 ]

   IF ( n := ascan( aOptns, {|e| lower( e ) $ "-hbexec,-hblib,-hbdyn" } ) ) > 0
      cType := lower( aOptns[ n ] )
   ELSE
      cType := ""
   ENDIF
   /* Basic Parsing is complete , parse paths from keywords */
   SWITCH cType
   CASE "-hblib"
      ::oUI:q_comboPrjType:setCurrentIndex( 1 )
      EXIT
   CASE "-hbdyn"
      ::oUI:q_comboPrjType:setCurrentIndex( 2 )
      EXIT
   OTHERWISE
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )
      EXIT
   ENDSWITCH

   ::oUI:q_editPrjTitle :setText( cOutName )
   ::oUI:q_editPrjLoctn :setText( cHome    )
*  ::oUI:q_editWrkFolder:setText( ""       )
   ::oUI:q_editDstFolder:setText( ""       )  /* To parse -o : but first fix path verification to honor filters */
*  ::oUI:q_editBackup   :setText( ""       )
   ::oUI:q_editOutName  :setText( cOutName )
*  ::oUI:q_editLaunchParams:setText( cParams )
*  ::oUI:q_editLaunchExe:setText( cRun )

   ::oUI:q_editFlags  :setPlainText( hbide_arrayToMemo( aOptns ) )
   ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( aFiles ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadXhpProject()
   LOCAL cXhp, a_, s, n, cPart, cKey, cVal
   LOCAL cHome, cOutname, cType, cDefine, cInclude
   LOCAL cRun, cParams, cDestntn, cMap
   LOCAL cPath, cFile, cExt
   LOCAL hLIBPATH := {=>}
   LOCAL aLibs := {}, aFlags := {}, aFiles := {}, aSrc := {}

   cXhp := hbide_fetchAFile( ::oDlg, "Selecet xMate Project File", { { "xMate Project Files", "*.xhp" } } )

   IF empty( cXhp )
      RETURN Self
   ENDIF
   hb_fNameSplit( cXhp, @cHome, @cOutName )
   cHome := hbide_pathStripLastSlash( cHome )

   a_:= hbide_readSource( cXhp )

   cPart := ""
   FOR EACH s IN a_
      s := alltrim( s )
      IF empty( s )
         LOOP
      ENDIF
      IF left( s, 1 ) == "["
         IF ( n := at( "]", s ) ) > 0
            cPart := substr( s, 2, n-2 )
         ELSE
            cPart := ""
         ENDIF
      ELSE
         SWITCH lower( cPart )
         CASE "version"
            EXIT
         CASE "xmate"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               IF cKey == "Create Map/List File" .AND. cVal == "Yes"
                  cMap := "-map"
               ENDIF
            ENDIF
            EXIT
         CASE "info"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               SWITCH lower( cKey )
               CASE "type"
                  cType := cVal
                  EXIT
               CASE "include"
                  cInclude := cVal
                  EXIT
               CASE "define"
                  cDefine := cVal
                  EXIT
               CASE "architecture"
                  EXIT
               ENDSWITCH
            ENDIF
            EXIT
         CASE "xmate"
            EXIT
         CASE "project"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               SWITCH lower( cKey )
               CASE "run"
                  cRun := cVal
                  EXIT
               CASE "params"
                  cParams := cVal
                  EXIT
               CASE "final path"
                  cDestntn := hbide_pathStripLastSlash( cVal )
                  EXIT
               ENDSWITCH
            ENDIF
            EXIT
         CASE "data path"
            EXIT
         CASE "editor"
            EXIT
         CASE "files"
            IF hbide_parseKeyValPair( s, @cKey, @cVal )
               aadd( aFiles, cKey )
            ENDIF
            EXIT
         ENDSWITCH
      ENDIF
   NEXT

   /* Basic Parsing is complete , parse paths from keywords */
   SWITCH cType
   CASE "Executable"
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )
      EXIT
   CASE "Library"
      ::oUI:q_comboPrjType:setCurrentIndex( 1 )
      EXIT
   CASE "Dll"
      ::oUI:q_comboPrjType:setCurrentIndex( 2 )
      EXIT
   OTHERWISE
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )
      EXIT
   ENDSWITCH

   ::oUI:q_editPrjTitle :setText( cOutName )
   ::oUI:q_editPrjLoctn :setText( cHome    )
*  ::oUI:q_editWrkFolder:setText( ""       )
   ::oUI:q_editDstFolder:setText( iif( cDestntn == "%HOME%", cHome, strtran( cDestntn, "%HOME%\", ) ) )
*  ::oUI:q_editBackup   :setText( ""       )
   ::oUI:q_editOutName  :setText( cOutName )
   ::oUI:q_editLaunchParams:setText( cParams )
   ::oUI:q_editLaunchExe:setText( cRun )

   IF !empty( cMap )
      aadd( aFlags, cMap )
   endif
   aadd( aFlags, "-inc" )
   IF !empty( cDefine )
      FOR EACH s IN hb_aTokens( cDefine, ";" )
         IF !empty( s )
            aadd( aFlags, "-D" + StrTran( s, "%HOME%\", ) )
         ENDIF
      NEXT
   ENDIF
   IF !empty( cInclude )
      FOR EACH s IN hb_aTokens( cInclude, ";" )
         IF !empty( s )
            IF !( "%HB_INSTALL%" $ s .OR. "%HB_LIB_INSTALL%" $ s .OR. "%C_LIB_INSTALL%" $ s )
               aadd( aFlags, "-incpath=" + StrTran( s, "%HOME%\", ) )
            ENDIF
         ENDIF
      NEXT
   ENDIF
   FOR EACH s IN aFiles
      IF !( "%HB_INSTALL%" $ s .OR. "%HB_LIB_INSTALL%" $ s .OR. "%C_LIB_INSTALL%" $ s )
         hb_fNameSplit( s, @cPath, @cFile, @cExt )
         SWITCH lower( cExt )
         CASE ".lib"
         CASE ".a"
            IF !( cPath $ hLIBPATH )
               hLIBPATH[ cPath ] := NIL
            ENDIF
            aadd( aLibs, cFile )
            EXIT
         OTHERWISE
            aadd( aSrc, StrTran( s, "%HOME%\", ) )
            EXIT
         ENDSWITCH
      ENDIF
   NEXT
   FOR EACH s IN hLIBPATH
      aadd( aFlags, "-L" + s:__enumKey() )
   NEXT
   FOR EACH s IN aLibs
      aadd( aFlags, "-l" + s )
   NEXT

   ::oUI:q_editFlags  :setPlainText( hbide_arrayToMemo( aFlags ) )
   ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( aSrc   ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:manageEnvironments()
   LOCAL oUIEnv

   IF empty( ::oEnvironDock:qtObject )
      oUIEnv := HbQtUI():new( ::resPath + "environments.uic", ::oEnvironDock:oWidget ):build()
      ::oEnvironDock:qtObject := oUIEnv
      ::oEnvironDock:oWidget:setWidget( oUIEnv )

      oUIEnv:q_buttonPathMk2:setIcon( hbide_image( "folder" ) )
      oUIEnv:q_buttonPathEnv:setIcon( hbide_image( "folder" ) )

      oUIEnv:signal( "buttonCn"      , "clicked()", {|| ::oEnvironDock:hide() } )
      oUIEnv:signal( "buttonSave"    , "clicked()", {|| ::saveEnvironments()     } )
      oUIEnv:signal( "buttonSaveExit", "clicked()", {|| ::saveEnvironments(), ::oEnvironDock:hide() } )
      oUIEnv:signal( "buttonPathMk2" , "clicked()", {|| ::PromptForPath( 'editPathMk2', 'Choose hbmk2 Executable Folder...'   ) } )
      oUIEnv:signal( "buttonPathEnv" , "clicked()", {|| ::PromptForPath( 'editPathEnv', 'Choose hbIDE.env Folder...'   ), ;
                  ::oEnvironDock:qtObject:q_editCompilers:setPlainText( ;
                       hb_memoread( hbide_pathFile( ::oEnvironDock:qtObject:q_editPathEnv:text(), "hbide.env" ) ) ) } )
   ENDIF

   ::oEnvironDock:qtObject:q_editPathMk2  :setText( ::aINI[ INI_HBIDE, PathMk2 ] )
   ::oEnvironDock:qtObject:q_editPathEnv  :setText( ::aINI[ INI_HBIDE, PathEnv ] )
   ::oEnvironDock:qtObject:q_editCompilers:setPlainText( hb_memoread( hbide_pathFile( ::aINI[ INI_HBIDE, PathEnv ], "hbide.env" ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:saveEnvironments()
   LOCAL cText, cPathMk2, cPathEnv
   LOCAL oUIEnv := ::oEnvironDock:qtObject

   cPathMk2 := oUIEnv:q_editPathMk2:text()
   cPathEnv := oUIEnv:q_editPathEnv:text()

   ::oIDE:aINI[ INI_HBIDE, PathMk2 ] := cPathMk2
   ::oIDE:aINI[ INI_HBIDE, PathEnv ] := cPathEnv
   //
   ::oIDE:cWrkPathMk2 := cPathMk2
   ::oIDE:cWrkPathEnv := cPathEnv

   IF !empty( cText := oUIEnv:q_editCompilers:toPlainText() )
      hb_MemoWrit( hbide_pathFile( cPathEnv, "hbide.env" ), cText )
   ENDIF

   RETURN Self

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

METHOD IdeProjManager:sortSources( cMode )
   LOCAL a_, cTyp, s, d_, n
   LOCAL aSrc := { ".ch", ".prg", ".c", ".cpp", ".h", ".obj", ".o", ".lib", ".a", ".rc", ".res" }
   LOCAL aTxt := { {}   , {}    , {}  , {}    , {}  , {}    , {}  , {}   , {} , {}, {}    }
   LOCAL aRst := {}

   a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() )

   IF     cMode == "az"
      asort( a_, , , {|e,f| lower( e ) < lower( f ) } )
   ELSEIF cMode == "za"
      asort( a_, , , {|e,f| lower( f ) < lower( e ) } )
   ELSEIF cMode == "org"
      asort( a_, , , {|e,f| lower( e ) < lower( f ) } )

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
            aadd( a_, " # " )
            aadd( a_, " # " + aSrc[ d_:__enumIndex() ] )
            aadd( a_, " # " )
            FOR EACH s IN d_
               aadd( a_, s )
            NEXT
         ENDIF
      NEXT
      IF !empty( aRst )
         aadd( a_, " # " )
         aadd( a_, " # " + "Unrecognized..." )
         aadd( a_, " # " )
         FOR EACH s IN aRst
            aadd( a_, s )
         NEXT
      ENDIF
   ENDIF

   ::oUI:q_editSources:clear()
   ::oUI:q_editSources:setPlainText( hbide_arrayToMemo( a_ ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:updateMetaData()
   LOCAL a_, s, n, cKey, cVal
   LOCAL a4_1 := {}

   a_:= hbide_memoToArray( ::oUI:q_editMetaData:toPlainText() )

   FOR EACH s IN a_
      IF !( "#" == left( s,1 ) )
         IF ( n := at( "=", s ) ) > 0
            cKey := alltrim( substr( s, 1, n-1 ) )
            cVal := hbide_evalAsString( alltrim( substr( s, n+1 ) ) )
            aadd( a4_1, { "<"+ cKey +">", cVal } )
         ENDIF
      ENDIF
   NEXT

   ::oProject:metaData := a4_1

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
   LOCAL aFiles, a_, b_, a4_1, s

   IF ::isValidProjectLocation( .t. )
      IF !empty( aFiles := ::oSM:selectSource( "openmany" ) )
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

   #ifdef HBIDE_USE_UIC
   oDlg := HbQtUI():new( ::oIDE:resPath + "selectproject.uic", ::oDlg:oWidget ):build()
   #else
   oDlg := HbQtUI():new( ::oIDE:resPath + "selectproject.ui", ::oDlg:oWidget ):create()
   #endif

 * Fill ComboBox with current project names
   FOR EACH p IN ::aProjects
       IF !empty( t := p[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
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
      cTemp := ::oProject:expandMeta( ::oUI:qObj[ cObjPathName ]:Text() )
   ELSE
      cTemp := ""
   ENDIF

   IF !hb_isChar( cObjFileName )
      cPath := hbide_fetchADir( ::oDlg, cTitle, cTemp )

   ELSE
      cTemp := hbide_fetchAFile( ::oDlg, cTitle, { { "Harbour IDE Projects", "*.hbi" } }, cTemp )

      IF !Empty( cTemp )
         hb_fNameSplit( hbide_pathNormalized( cTemp, .f. ), @cPath, @cFile )

         ::oUI:qObj[ cObjFileName ]:setText( cFile )
      ENDIF
   ENDIF

   IF !Empty( cPath )
      IF Right( cPath, 1 ) $ '/\'
         cPath := Left( cPath, Len( cPath ) - 1 )
      ENDIF
      IF hb_isObject( ::oProject )
         cPath := ::oProject:applyMeta( cPath )
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
   LOCAL cHbpPath, oEdit, cHbpFN, cTmp, cTargetFN, cExeHbMk2, aHbp, cCmd, cC, cArg

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

   cTargetFN  := hbide_pathFile( ::oProject:destination, iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )
   cHbpFN     := hbide_pathFile( ::oProject:wrkDirectory, iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )
 * cHbpPath   := cHbpFN + iif( ::lPPO, '.' + hb_md5( hb_ntos( seconds() ) ), "" ) + ".hbp"
   cHbpPath   := cHbpFN + iif( ::lPPO, '_tmp', "" ) + ".hbp"

   IF !( ::lPPO )
      IF     ::oProject:type == "Lib"
         aadd( aHbp, "-hblib" )
      ELSEIF ::oProject:type == "Dll"
         aadd( aHbp, "-hbdyn" )
      ENDIF
   ENDIF

   aadd( aHbp, "# User Supplied Flags" )
   aadd( aHbp, " " )
   IF !empty( ::oProject:hbpFlags )
      aeval( ::oProject:hbpFlags, {|e| aadd( aHbp, e ) } )
   ENDIF

   aadd( aHbp, " " )
   aadd( aHbp, "# hbIDE Supplied Flags" )
   aadd( aHbp, " " )
   IF !( ::lPPO )
      aadd( aHbp, "-o" + cTargetFN )
 *    aadd( aHbp, "-workdir=" + ::oProject:wrkDirectory + "/${hb_plat}/${hb_comp}" )
   ENDIF
   aadd( aHbp, "-q"          )
   aadd( aHbp, "-trace"      )
   aadd( aHbp, "-info"       )
   IF lRebuild
      aadd( aHbp, "-rebuild" )
   ENDIF
   aadd( aHbp, " " )

   IF !( ::lPPO )
      /* Add all sources to .hbp */
      aadd( aHbp, "# Source Files" )
      aadd( aHbp, " " )
      aeval( hbide_filesToSources( ::oProject:sources ), {|e| aadd( aHbp, e ) } )

   ELSE
      IF !empty( oEdit := ::oEM:getEditorCurrent() )
         IF hbide_isSourcePRG( oEdit:sourceFile )
            aadd( aHbp, "-hbcmp" )
            aadd( aHbp, "-s"     )
            aadd( aHbp, "-p"     )
            aadd( aHbp, "  "     )
            aadd( aHbp, "# Source File - PPO" )
            aadd( aHbp, " "      )

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

   ::oIDE:lDockBVisible := .t.
   ::oDockB2:show()
   ::oOutputResult:oWidget:clear()

   IF !hbide_createTarget( cHbpPath, aHbp )
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
      #if 0                           /* This does not works - reason being it picks up hbmk2 from fixed location */
      cExeHbMk2  := hbide_pathFile( ::oProject:cPathMk2, "hbmk2" )
      #else                           /* This works properly */
      cExeHbMk2 := "hbmk2"            /* Needs that path is already set before calling hbmk2 */
      #endif

      ::oProcess := HbpProcess():new()
      //
      ::oProcess:output      := {|cOut, mp2, oHbp| ::showOutput( cOut,mp2,oHbp ) }
      ::oProcess:finished    := {|nEC , nES, oHbp| ::finished( nEC ,nES,oHbp ) }
      ::oProcess:workingPath := hbide_pathToOSPath( ::oProject:wrkDirectory )
      //
      cCmd := hbide_getShellCommand()
      cC   := iif( hbide_getOS() == "nix", "", "/C " )
      cArg := iif( empty( ::cBatch ), cC, cC + ::cBatch + " && "  )
      //
      ::oProcess:addArg( cArg + cExeHbMk2 + " " + cHbpPath + iif( ::lPPO, " -hbraw", "" ) )
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
   LOCAL cTmp

   hbide_justACall( oProcess )

   cTmp := hbide_outputLine() + CRLF
   cTmp += "Exit Code [ " + hb_ntos( nExitCode ) + " ]    Exit Status [ " + hb_ntos( nExitStatus ) + " ]    " +;
           "Finished at [ " + time()     + " ]    Done in [ " + hb_ntos( seconds() - oProcess:started ) +" Secs ]" + CRLF
   cTmp += hbide_outputLine() + CRLF

   ::oOutputResult:oWidget:append( cTmp )

   ferase( ::cBatch )

   IF ::lLaunch
      IF nExitCode == 0
         ::launchProject( ::cProjectInProcess )
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
METHOD IdeProjManager:launchProject( cProject )
   LOCAL cTargetFN, cTmp, oProject, qProcess, qStr

   IF empty( cProject )
      cProject := ::oPM:getCurrentProject()
   ENDIF
   IF empty( cProject )
      RETURN Self
   ENDIF

   oProject  := ::getProjectByTitle( cProject )

   cTargetFN := hbide_pathFile( oProject:destination, iif( empty( oProject:outputName ), "_temp", oProject:outputName ) )
#ifdef __PLATFORM__WINDOWS
   IF oProject:type == "Executable"
      cTargetFN += '.exe'
   ENDIF
#endif

   IF !hb_FileExists( cTargetFN )
      cTmp := "Launch application error: file not found " + cTargetFN + "!"

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
      qProcess:pPtr := NIL
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


/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               03Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqtgui.ch"
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

   cSource := hbide_pathToOSPath( cSource )

   hbide_parseHbpFilter( cSource, @cFilt, @cPathFile )
   hb_fNameSplit( cPathFile, @cPath, @cFile, @cExt )

   ::original   := cSource
   ::normalized := hbide_pathNormalized( cSource, .t. )
   ::filter     := cFilt
   ::path       := hbide_pathNormalized( cPath, .t. )
   ::file       := cFile
   //::ext        := lower( cExt )
   ::ext        := cExt

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
   DATA   destination                             INIT ""
   DATA   outputName                              INIT ""
   DATA   backup                                  INIT ""
   DATA   launchParams                            INIT ""
   DATA   launchProgram                           INIT ""
   DATA   wrkDirectory                            INIT ""
   DATA   isXhb                                   INIT .f.
   DATA   isXpp                                   INIT .f.
   DATA   isClp                                   INIT .f.

   DATA   hbpFlags                                INIT {}
   DATA   sources                                 INIT {}
   DATA   dotHbp                                  INIT ""
   DATA   compilers                               INIT ""
   DATA   cPathHbMk2
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

      ::type           := a_[ PRJ_PRP_TYPE      ]
      ::title          := a_[ PRJ_PRP_TITLE     ]
      ::location       := ""                        /* See below */
      ::wrkDirectory   := a_[ PRJ_PRP_WRKFOLDER ]
      ::destination    := a_[ PRJ_PRP_DSTFOLDER ]
      ::outputName     := a_[ PRJ_PRP_OUTPUT    ]
      ::launchParams   := a_[ PRJ_PRP_LPARAMS   ]
      ::launchProgram  := a_[ PRJ_PRP_LPROGRAM  ]
      ::backup         := a_[ PRJ_PRP_BACKUP    ]
      ::isXhb          := a_[ PRJ_PRP_XHB       ] == "YES"
      ::isXpp          := a_[ PRJ_PRP_XPP       ] == "YES"
      ::isClp          := a_[ PRJ_PRP_CLP       ] == "YES"

      ::projPath       := oIde:oPM:getProjectPathFromTitle( ::title )
      IF empty( ::projPath )
         ::projPath := hb_dirBase() /* In case of new project */
      ENDIF
      ::location       := ::projPath

      ::hbpFlags       := aclone( b_[ PRJ_PRP_FLAGS   , 2 ] )
      ::sources        := aclone( b_[ PRJ_PRP_SOURCES , 2 ] )
      ::dotHbp         := ""
      ::compilers      := ""

      ::cPathHbMk2 := oIde:oINI:getHbmk2File()

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

   DATA   cargo
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

   DATA   cIfError                                INIT NIL

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
   METHOD getCurrentProjectTitle()
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
   METHOD buildSource( lExecutable )
   METHOD buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   METHOD launchProject( cProject, cExe )
   METHOD showOutput( cOutput, mp2, oProcess )
   METHOD finished( nExitCode, nExitStatus, oProcess )
   METHOD isValidProjectLocation( lTell )
   METHOD setProjectLocation( cPath )
   METHOD buildInterface()
   METHOD pullHbpData( cHbp )
   METHOD synchronizeAlienProject( cProjFileName )
   METHOD outputText( cText )
   METHOD runAsScript()
   METHOD insertHeader( aHdr, aHbp )
   METHOD stripHeader( aHbp )
   METHOD moveLine( nDirection )

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
      ::oUI:q_buttonCn          :disconnect( "clicked()" )
      ::oUI:q_buttonSave        :disconnect( "clicked()" )
      ::oUI:q_buttonSaveExit    :disconnect( "clicked()" )
      ::oUI:q_buttonSelect      :disconnect( "clicked()" )
      ::oUI:q_buttonUp          :disconnect( "clicked()" )
      ::oUI:q_buttonDown        :disconnect( "clicked()" )
   // ::oUI:q_buttonSort        :disconnect( "clicked()" )
   // ::oUI:q_buttonSortZA      :disconnect( "clicked()" )
   // ::oUI:q_buttonSortOrg     :disconnect( "clicked()" )
      ::oUI:q_tabWidget         :disconnect( "currentChanged(int)" )
      ::oUI:q_buttonChoosePrjLoc:disconnect( "clicked()" )
      ::oUI:q_buttonChooseWd    :disconnect( "clicked()" )
      ::oUI:q_buttonChooseDest  :disconnect( "clicked()" )
      ::oUI:q_buttonBackup      :disconnect( "clicked()" )
      ::oUI:q_editPrjLoctn      :disconnect( "textChanged(QString)" )

      ::oUI:destroy()
   ENDIF

   IF !empty( ::aPrjProps )
      ::aPrjProps[ 1,1 ] := NIL
      ::aPrjProps[ 1,2 ] := NIL
      ::aPrjProps[ 1 ] := NIL
      ::aPrjProps[ 2,1 ] := NIL
      ::aPrjProps[ 2,2 ] := NIL
      ::aPrjProps[ 2 ] := NIL
      ::aPrjProps[ 3,1 ] := NIL
      ::aPrjProps[ 3,2 ] := NIL
      ::aPrjProps[ 3 ] := NIL
      ::aPrjProps[ 4,1 ] := NIL
      ::aPrjProps[ 4,2 ] := NIL
      ::aPrjProps[ 4 ] := NIL
      ::aPrjProps[ 5 ] := NIL

      ::aPrjProps := NIL
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:populate()
   LOCAL cProject

   FOR EACH cProject IN ::oINI:aProjFiles
      ::loadProperties( cProject, .f., .f., .T. )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProperties()
   LOCAL cTmp, n

   cTmp := ::getCurrentProject()
   IF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, PRJ_PRP_TITLE ] == cTmp } ) ) > 0
      ::loadProperties( ::aProjects[ n, 1 ], .f., .t., .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:loadProperties( cProjFileName, lNew, lFetch, lUpdateTree )
   LOCAL nAlready, cProjPath

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
                                                                         { "xMate Projects"  , "*.xhp" } , ;
                                                                         { "xBuild Projects" , "*.xbp" } } )
         cProjFileName := ::synchronizeAlienProject( cProjFileName )
         ::oDockPT:show()
      ENDIF
      IF empty( cProjFileName )
         RETURN Self
      ENDIF
   ENDIF

   cProjFileName := hbide_pathToOSPath( cProjFileName )

   ::oIde:oPropertiesDock:setWindowTitle( cProjFileName )

   nAlready := ascan( ::aProjects, {|e_| hb_FileMatch( e_[ 1 ], hbide_pathNormalized( cProjFileName ) ) } )

   IF !empty( cProjFileName ) .AND. hb_fileExists( cProjFileName )
      ::aPrjProps  := ::pullHbpData( hbide_pathToOSPath( cProjFileName ) )
   ENDIF

   IF lFetch
      IF lNew
         IF empty( cProjPath := hbide_fetchADir( ::oDlg, "Project Path", hbide_SetWrkFolderLast() ) )
            RETURN Self
         ENDIF
         cProjPath := hbide_pathAppendLastSlash( cProjPath )
         hbide_SetWrkFolderLast( cProjPath )
      ENDIF
      /* Access/Assign via this object */
      ::oProject := IdeProject():new( ::oIDE, ::aPrjProps )
      IF !empty( cProjPath )
         ::oProject:location := hbide_pathNormalized( cProjPath, .f. )
         ::oProject:projPath := ::oProject:location
      ENDIF
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
            hbide_mnuAddFileToMRU( ::oIDE, cProjFileName, "recent_projects" )
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
            "hbide_location="          , ;
            "hbide_workingfolder="     , ;
            "hbide_destinationfolder=" , ;
            "hbide_output="            , ;
            "hbide_launchparams="      , ;
            "hbide_launchprogram="     , ;
            "hbide_backupfolder="      , ;
            "hbide_xhb="               , ;
            "hbide_xpp="               , ;
            "hbide_clp="               , ;
            "hbide_launchim="            ;
         }

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

   IF ( n := ascan( aOptns, {|e| lower( e ) $ "-hbexe,-hblib,-hbdyn" } ) ) > 0
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
      IF !empty( s )
         aadd( a2_0, s )
      ENDIF
   NEXT

   /* PRJ_PRP_SOURCES */
   FOR EACH s IN aFiles
      aadd( a3_0, s )
   NEXT

   /* Check sources which are not compilable but make-up source list */
   FOR EACH s IN a3rd
      IF "hbide_file=" == lower( left( s, 11 ) )
         aadd( a3_0, hbide_stripRoot( cHome, alltrim( substr( s, 12 ) ) ) )
      ENDIF
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

   RETURN { { a1_0, a1_1 }, { a2_0, a2_1 }, { a3_0, a3_1 }, { a4_0, a4_1 }, hbide_readSource( cHbp ) }

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:save( lCanClose )
   LOCAL a_, lOk, txt_, nAlready
   LOCAL c3rd := "-3rd="
   LOCAL hdr_:= {}

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
   aadd( hdr_, c3rd + "hbide_version="              + "1.0" )
   //
   IF ::oUI:q_comboPrjType:currentIndex() != 0
      aadd( hdr_, c3rd + "hbide_type="              + { "Executable", "Lib", "Dll" }[ ::oUI:q_comboPrjType:currentIndex() + 1 ] )
   ENDIF
   IF ! Empty( ::oUI:q_editPrjTitle    :text() )
      aadd( hdr_, c3rd + "hbide_title="             + hbide_space2amp( ::oUI:q_editPrjTitle    :text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editWrkFolder   :text() )
      aadd( hdr_, c3rd + "hbide_workingfolder="     + hbide_space2amp( ::oUI:q_editWrkFolder   :text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editDstFolder   :text() )
      aadd( hdr_, c3rd + "hbide_destinationfolder=" + hbide_space2amp( ::oUI:q_editDstFolder   :text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editOutName     :text() )
      aadd( hdr_, c3rd + "hbide_output="            + hbide_space2amp( ::oUI:q_editOutName     :text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editLaunchParams:text() )
      aadd( hdr_, c3rd + "hbide_launchparams="      + hbide_space2amp( ::oUI:q_editLaunchParams:text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editLaunchExe   :text() )
      aadd( hdr_, c3rd + "hbide_launchprogram="     + hbide_space2amp( ::oUI:q_editLaunchExe   :text() ) )
   ENDIF
   IF ! Empty( ::oUI:q_editBackup      :text() )
      aadd( hdr_, c3rd + "hbide_backupfolder="      + hbide_space2amp( ::oUI:q_editBackup      :text() ) )
   ENDIF
   IF ::oUI:q_checkXhb:isChecked()
      aadd( hdr_, c3rd + "hbide_xhb="               + iif( ::oUI:q_checkXhb:isChecked(), "YES", "NO" )   )
   ENDIF
   IF ::oUI:q_checkXpp:isChecked()
      aadd( hdr_, c3rd + "hbide_xpp="               + iif( ::oUI:q_checkXpp:isChecked(), "YES", "NO" )   )
   ENDIF
   IF ::oUI:q_checkClp:isChecked()
      aadd( hdr_, c3rd + "hbide_clp="               + iif( ::oUI:q_checkClp:isChecked(), "YES", "NO" )   )
   ENDIF

   a_:= hbide_synchronizeForHbp( hbide_memoToArray( ::oUI:q_editSources:toPlainText() ) )
   a_:= ::insertHeader( hdr_, a_ )
   aeval( a_, {|e| aadd( txt_, e ) } )
   aadd( txt_, " " )

   ::cSaveTo := ::oUI:q_editPrjLoctn:text() + ::pathSep + ::oUI:q_editOutName:text() + ".hbp"

   ::cSaveTo := hbide_pathToOSPath( ::cSaveTo )

   IF ( lOk := hbide_createTarget( ::cSaveTo, txt_ ) )
      ::aPrjProps := ::pullHbpData( hbide_pathToOSPath( ::cSaveTo ) )

      IF ( nAlready := ascan( ::aProjects, {|e_| hb_FileMatch( e_[ 1 ], hbide_pathNormalized( ::cSaveTo ) ) } ) ) == 0
         aadd( ::oIDE:aProjects, { hbide_pathNormalized( ::cSaveTo ), ::cSaveTo, aclone( ::aPrjProps ) } )
         IF ::lUpdateTree
            ::oIDE:updateProjectTree( ::aPrjProps )
         ENDIF
         hbide_mnuAddFileToMRU( ::oIDE, ::cSaveTo, "recent_projects" )
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

   IF lOk
      ::oDockPT:show()
   ENDIF
   RETURN lOk

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:insertHeader( aHdr, aHbp )
   LOCAL txt_:={}

   aadd( txt_, "#" )
   aadd( txt_, "# $" + "Id" + "$" )
   aadd( txt_, "#" )
   aadd( txt_, "" )
   aeval( aHdr, {|e| aadd( txt_, e ) } )
   aadd( txt_, "" )
   aeval( aHbp, {|e| aadd( txt_, e ) } )
   aadd( txt_, "" )

   RETURN txt_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:stripHeader( aHbp )
   LOCAL nStart, n, s
   LOCAL a_:= {}

   FOR EACH s IN aHbp
      n := s:__enumIndex()
      s := alltrim( s )
      IF left( s, 1 ) == "#" .AND. n <= 3
         nStart := n
         LOOP
      ENDIF
      IF empty( s )
         LOOP
      ENDIF
      IF "-3rd=hbide_file" $ s
         nStart := n
         EXIT
      ENDIF
      IF ! ( "-3rd=hbide_" $ s )
         nStart := n
         EXIT
      ENDIF
   NEXT

   IF ! empty( nStart )
      FOR EACH s IN aHbp
         IF s:__enumIndex() < nStart
            LOOP
         ENDIF
         aadd( a_, s )
      NEXT
   ELSE
      RETURN aHbp
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:updateHbp( iIndex )
   LOCAL txt_

   IF iIndex != 3
      RETURN NIL
   ENDIF

   /* Sources */
   txt_:= hbide_synchronizeForHbp( hb_atokens( ::oUI:q_editSources:toPlainText(), _EOL ) )

   /* Final assault */
   ::oUI:q_editHbp:setPlainText( hbide_arrayToMemo( txt_ ) )

   RETURN txt_

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:fetchProperties()

   IF empty( ::oProject )
      ::oProject := IdeProject():new( ::oIDE, ::aPrjProps )
   ENDIF

   IF empty( ::oUI )
      ::buildInterface()
   ENDIF

   IF empty( ::aPrjProps )
      ::oUI:q_comboPrjType:setCurrentIndex( 0 )

      ::oUI:q_editPrjTitle :setText( "" )
      ::oUI:q_editPrjLoctn :setText( hbide_pathNormalized( ::oProject:location, .F. ) )
      ::oUI:q_editDstFolder:setText( "" )
      ::oUI:q_editBackup   :setText( "" )
      ::oUI:q_editOutName  :setText( "" )

      ::oUI:q_editFlags    :setPlainText( "" )
      ::oUI:q_editSources  :setPlainText( "" )

      ::oUI:q_editLaunchParams:setText( "" )
      ::oUI:q_editLaunchExe:setText( "" )
      ::oUI:q_editWrkFolder:setText( "" )
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
      ::oUI:q_editDstFolder:setText( ::oProject:destination  )
      ::oUI:q_editOutName  :setText( ::oProject:outputName   )
      ::oUI:q_editBackup   :setText( ::oProject:backup       )

      ::oUI:q_checkXhb     :setChecked( ::oProject:isXhb )
      ::oUI:q_checkXpp     :setChecked( ::oProject:isXpp )
      ::oUI:q_checkClp     :setChecked( ::oProject:isClp )

      ::oUI:q_editFlags    :setPlainText( hbide_arrayToMemo( ::aPrjProps[ PRJ_PRP_FLAGS   , 1 ] ) )
      ::oUI:q_editSources  :setPlainText( hbide_arrayToMemo( ::stripHeader( ::aPrjProps[ 5 ] ) ) )

      ::oUI:q_editLaunchParams:setText( ::oProject:launchParams )
      ::oUI:q_editLaunchExe:setText( ::oProject:launchProgram )
      ::oUI:q_editWrkFolder:setText( ::oProject:wrkDirectory )

      ::oUI:q_editHbp:setPlainText( "" )

      ::oUI:oWidget:setWindowTitle( 'Properties for "' + ::oUI:q_editPrjTitle:Text() + '"' )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildInterface()
   LOCAL cLukupPng

   ::oUI := hbide_getUI( "projectpropertiesex" )

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
   ::oUI:q_buttonUp     :setIcon( hbide_image( "dc_up"       ) )
   ::oUI:q_buttonDown   :setIcon( hbide_image( "dc_down"     ) )

   ::oUI:q_buttonSort   :setIcon( hbide_image( "sort"        ) )
   ::oUI:q_buttonSortZA :setIcon( hbide_image( "sortdescend" ) )
   ::oUI:q_buttonSortOrg:setIcon( hbide_image( "invertcase"  ) )

   ::oUI:q_buttonSort   :hide()
   ::oUI:q_buttonSortZA :hide()
   ::oUI:q_buttonSortOrg:hide()

   ::oUI:q_buttonCn          :connect( "clicked()", {|| ::lSaveOK := .f., ::oPropertiesDock:hide() } )
   ::oUI:q_buttonSave        :connect( "clicked()", {|| ::lSaveOK := .t., ::save( .F. )          } )
   ::oUI:q_buttonSaveExit    :connect( "clicked()", {|| ::lSaveOK := .t., ::save( .T. )          } )
   ::oUI:q_buttonSelect      :connect( "clicked()", {|| ::addSources()         } )
   ::oUI:q_buttonUp          :connect( "clicked()", {|| ::moveLine( -1 )       } )
   ::oUI:q_buttonDown        :connect( "clicked()", {|| ::moveLine( +1 )       } )
// ::oUI:q_buttonSort        :connect( "clicked()", {|| ::sortSources( "az"  ) } )
// ::oUI:q_buttonSortZA      :connect( "clicked()", {|| ::sortSources( "za"  ) } )
// ::oUI:q_buttonSortOrg     :connect( "clicked()", {|| ::sortSources( "org" ) } )
   ::oUI:q_tabWidget         :connect( "currentChanged(int)", {|p| ::updateHbp( p ) } )
   ::oUI:q_buttonChoosePrjLoc:connect( "clicked()", {|| ::PromptForPath( 'editPrjLoctn' , 'Choose Project Location...'   ) } )
   ::oUI:q_buttonChooseWd    :connect( "clicked()", {|| ::PromptForPath( 'editWrkFolder', 'Choose Working Folder...'     ) } )
   ::oUI:q_buttonChooseDest  :connect( "clicked()", {|| ::PromptForPath( 'editDstFolder', 'Choose Destination Folder...' ) } )
   ::oUI:q_buttonBackup      :connect( "clicked()", {|| ::PromptForPath( 'editBackup'   , 'Choose Backup Folder...'      ) } )
   ::oUI:q_editPrjLoctn      :connect( "textChanged(QString)", {|cPath| ::setProjectLocation( cPath ) } )

   /* Set monospaced fonts */
   ::oUI:q_editFlags       :setFont( ::oFont:oWidget )
   ::oUI:q_editSources     :setFont( ::oFont:oWidget )
   ::oUI:q_editHbp         :setFont( ::oFont:oWidget )

   #if 0
   ::oUI:q_editPrjTitle    :setFont( ::oFont:oWidget )
   ::oUI:q_editPrjLoctn    :setFont( ::oFont:oWidget )
   ::oUI:q_editWrkFolder   :setFont( ::oFont:oWidget )
   ::oUI:q_editDstFolder   :setFont( ::oFont:oWidget )
   ::oUI:q_editOutName     :setFont( ::oFont:oWidget )
   ::oUI:q_editBackup      :setFont( ::oFont:oWidget )
   ::oUI:q_editLaunchParams:setFont( ::oFont:oWidget )
   ::oUI:q_editLaunchExe   :setFont( ::oFont:oWidget )
   #endif

   ::oUI:setTabOrder( ::oUI:q_comboPrjType    , ::oUI:q_editPrjTitle     )
   ::oUI:setTabOrder( ::oUI:q_editPrjTitle    , ::oUI:q_editPrjLoctn     )
   ::oUI:setTabOrder( ::oUI:q_editPrjLoctn    , ::oUI:q_editOutName      )
   ::oUI:setTabOrder( ::oUI:q_editOutName     , ::oUI:q_checkXhb         )
   ::oUI:setTabOrder( ::oUI:q_checkXhb        , ::oUI:q_checkXpp         )
   ::oUI:setTabOrder( ::oUI:q_checkXpp        , ::oUI:q_checkClp         )
   ::oUI:setTabOrder( ::oUI:q_checkClp        , ::oUI:q_editDstFolder    )
   ::oUI:setTabOrder( ::oUI:q_editDstFolder   , ::oUI:q_editBackup       )
   ::oUI:setTabOrder( ::oUI:q_editBackup      , ::oUI:q_editLaunchParams )
   ::oUI:setTabOrder( ::oUI:q_editLaunchParams, ::oUI:q_editLaunchExe    )
   ::oUI:setTabOrder( ::oUI:q_editLaunchExe   , ::oUI:q_editWrkFolder    )
   ::oUI:setTabOrder( ::oUI:q_editWrkFolder   , ::oUI:q_tabFiles         )
   //
   ::oUI:setTabOrder( ::oUI:q_tabFiles        , ::oUI:q_editSources      )
   ::oUI:setTabOrder( ::oUI:q_editSources     , ::oUI:q_tabFlags         )
   //
   ::oUI:setTabOrder( ::oUI:q_tabFlags        , ::oUI:q_editFlags        )
   ::oUI:setTabOrder( ::oUI:q_editFlags       , ::oUI:q_tabHbp           )
   //
   ::oUI:setTabOrder( ::oUI:q_tabHbp          , ::oUI:q_editHbp          )
   //
   ::oUI:setTabOrder( ::oUI:q_editHbp         , ::oUI:q_buttonSaveExit   )
   ::oUI:setTabOrder( ::oUI:q_buttonSaveExit  , ::oUI:q_buttonSave       )
   ::oUI:setTabOrder( ::oUI:q_buttonSave      , ::oUI:q_buttonCn         )

   ::oUI:q_tabHbp:hide()
   ::oUI:q_tabFlags:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:synchronizeAlienProject( cProjFileName )
   LOCAL cPath, cFile, cExt, cHbp
   LOCAL cExeHbMk2, oProcess, cCmd

   hb_fNameSplit( cProjFileName, @cPath, @cFile, @cExt )
   IF lower( cExt ) == ".hbp"              /* Nothing to do */
      RETURN cProjFileName
   ENDIF

   IF !( lower( cExt ) $ ".xhp|.xbp" )          /* Not a valid alien project file */
      RETURN ""
   ENDIF

   cHbp := cPath + cFile + ".hbp"
   IF hb_fileExists( cHbp )
      IF ! hbide_getYesNo( "A .hbp with converted name already exists, overwrite?", "", "Project exists" )
         RETURN ""
      ENDIF
   ENDIF

   cExeHbMk2 := ::oINI:getHbmk2File()

   SWITCH lower( cExt )
   CASE ".xhp"
      cCmd := cExeHbMk2 + " -xhp=" + cProjFileName
      EXIT
   CASE ".xbp"
      cCmd := cExeHbMk2 + " -xbp=" + cProjFileName
      EXIT
   CASE "???"
      cCmd := cExeHbMk2 + " -hbmake=" + cProjFileName
      EXIT
   ENDSWITCH

   oProcess := QProcess()
   oProcess:start( cCmd )
   oProcess:waitForFinished()

   RETURN cHbp

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:sortSources( cMode )
   LOCAL a_, cTyp, s, d_, n
   LOCAL aSrc := { ".prg", ".ch", ".c", ".cpp", ".h", ".obj", ".o", ".lib", ".a", ".rc", ".res" }
   LOCAL aTxt := { {}    , {}   , {}  , {}    , {}  , {}    , {}  , {}    , {}  , {}   , {}     }
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
         IF !( left( s, 1 ) == "#" )
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

METHOD IdeProjManager:moveLine( nDirection )

   IF nDirection == -1

   ELSE

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:addSources()
   LOCAL aFiles, a_, b_, s, cHome

   IF ::isValidProjectLocation( .t. )
      IF !empty( aFiles := ::oSM:selectSource( "openmany", , , ::oUI:q_editPrjLoctn:text() ) )
         a_:= hbide_memoToArray( ::oUI:q_editSources:toPlainText() )

         b_:={}
         aeval( aFiles, {|e| aadd( b_, e ) } )

         cHome := ::oUI:q_editPrjLoctn:text()
         FOR EACH s IN b_
            s := hbide_prepareSourceForHbp( hbide_stripRoot( cHome, s ) )
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
      lValid := .F.

   ENDIF

   IF lValid
      IF !Empty( ::oSBar )
         ::oDK:setStatusText( SB_PNL_PROJECT, ::cWrkProject )
      ENDIF

      ::oIDE:updateTitleBar()

      /* Reset Old Color */
      IF !empty( cOldProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIDE, cOldProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, QBrush( QColor( 0,0,0 ) ) )
         ENDIF
      ENDIF
      /* Set New Color */
      IF !empty( ::cWrkProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIDE, ::cWrkProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, QBrush( QColor( 255,0,0 ) ) )
            ::oProjTree:oWidget:setCurrentItem( oItem:oWidget )
         ENDIF
         ::loadProperties( ::getProjectFileNameFromTitle( ::cWrkProject ), .f., .f., .f. )
      ENDIF
   ENDIF

   RETURN cOldProject

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getCurrentProjectTitle()

   IF Empty( ::aProjects )
      RETURN ""
   ENDIF
   IF ! Empty( ::cWrkProject )
      RETURN ::cWrkProject
   ENDIF
   IF Len( ::aProjects ) == 1
      ::setCurrentProject( ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
      RETURN ::aProjects[ 1, 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]
   ENDIF

   RETURN ""

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

   oDlg := hbide_getUI( "selectproject", ::oDlg:oWidget )

   FOR EACH p IN ::aProjects
      IF !empty( t := p[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] )
         oDlg:qObj[ "cbProjects" ]:addItem( t )
      ENDIF
   NEXT

   oDlg:q_btnCancel:connect( "clicked()", {|| oDlg:oWidget:done( 1 ) } )
   oDlg:q_btnOk    :connect( "clicked()", {|| ::setCurrentProject( oDlg:qObj[ "cbProjects" ]:currentText() ), ;
                                                                                             oDlg:done( 1 ) } )
   oDlg:exec()

   oDlg:q_btnCancel:disconnect( "clicked()" )
   oDlg:q_btnOk    :disconnect( "clicked()" )
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

   IF ( n := ascan( ::aProjects, {|e_| hb_FileMatch( e_[ 1 ], cProjectFile ) } ) ) > 0
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
         ::oINI:save()
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

METHOD IdeProjManager:buildSource( lExecutable )
   LOCAL oEdit, cTmp, cExeHbMk2, cCmd, cC, cCmdParams, cBuf
   LOCAL cbRed    := "<font color=blue>", ceRed := "</font>"
   LOCAL lRebuild := .T.
   LOCAL aHbp     := {}

   ::lPPO              := .t.
   ::lLaunch           := lExecutable
   ::cProjectInProcess := NIL

   IF !empty( oEdit := ::oEM:getEditorCurrent() )
      IF ! hbide_isSourcePRG( oEdit:sourceFile )
         MsgBox( 'Operation not supported for this file type: "' + oEdit:sourceFile + '"' )
         RETURN Self
      ENDIF
   ELSE
      MsgBox( "No active editing source available !" )
      RETURN Self
   ENDIF
   IF ::oINI:lSaveSourceWhenComp
      ::oSM:saveNamedSource( oEdit:sourceFile )
   ENDIF

   ::cargo := oEdit

   aadd( aHbp, "-q"         )
   aadd( aHbp, "-trace"     )
   aadd( aHbp, "-info"      )
   aadd( aHbp, "-lang=en"   )
   aadd( aHbp, "-width=0"   )
   aadd( aHbp, "-rebuild"   )
   IF lExecutable
      aadd( aHbp, "-hbexe"  )
   ELSE
      aadd( aHbp, "-s"      )
   ENDIF
   aadd( aHbp, hbide_pathToOSPath( oEdit:sourceFile ) )

   ::oDockB2:show()
   ::oOutputResult:oWidget:clear()

   ::oOutputResult:oWidget:append( hbide_outputLine() )
   cTmp := "Project [ " + oEdit:sourceFile              + " ]    " + ;
           "Launch [ "  + iif( ::lLaunch, 'Yes', 'No' ) + " ]    " + ;
           "Rebuild [ " + iif( lRebuild , 'Yes', 'No' ) + " ]    " + ;
           "Started [ " + time() + " ]"
   ::oOutputResult:oWidget:append( cTmp )
   ::oOutputResult:oWidget:append( hbide_outputLine() )

   ::oIDE:oEV := IdeEnvironments():new():create( ::oIDE )
   ::cBatch   := ::oEV:prepareBatch( ::cWrkEnvironment )
   aeval( ::oEV:getHbmk2Commands( ::cWrkEnvironment ), {|e| aadd( aHbp, e ) } )

   cExeHbMk2  := "hbmk2"

   cCmdParams := hbide_array2cmdParams( aHbp )

   ::oProcess := HbpProcess():new()
   //
   ::oProcess:output      := {|cOut, mp2, oHbp| ::showOutput( cOut,mp2,oHbp ) }
   ::oProcess:finished    := {|nEC , nES, oHbp| ::finished( nEC ,nES,oHbp ) }
   ::oProcess:workingPath := hbide_pathToOSPath( oEdit:cPath )
   //
   cCmd := hbide_getShellCommand()
   cC   := iif( hbide_getOS() == "nix", "", "/C " )

   IF hb_fileExists( ::cBatch )
      cBuf := memoread( ::cBatch )
      cBuf += hb_eol() + cExeHbMk2 + " " + cCmdParams + hb_eol()
      hb_memowrit( ::cBatch, cBuf )
   ENDIF
   //
   ::outputText( cbRed + "Batch File " + iif( hb_fileExists( ::cBatch ), " Exists", " : doesn't Exist" ) + " => " + ceRed + trim( ::cBatch ) )
   ::outputText( cbRed + "Batch File Contents => " + ceRed )
   ::outputText( memoread( ::cBatch ) )
   ::outputText( cbRed + "Command => " + ceRed + cCmd )
   ::outputText( cbRed + "Arguments => " + ceRed + cC + ::cBatch )
   ::outputText( hbide_outputLine() )
   //
   ::oProcess:addArg( cC + ::cBatch )
   ::oProcess:start( cCmd )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   LOCAL cHbpPath, oEdit, cHbpFN, cTmp, cExeHbMk2, aHbp, cCmd, cC, oSource, cCmdParams, cBuf
   LOCAL cbRed := "<font color=blue>", ceRed := "</font>"

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

   /* Set it as current project */
   ::setCurrentProject( cProject )

   /* Make Macros happy */
   hbide_setProjectTitle( cProject )

   ::oProject := ::getProjectByTitle( cProject )
   // attempt to save the sources if are open in editors       should it be controlled by some option ?
   IF ::oINI:lSaveSourceWhenComp
      FOR EACH oSource IN ::oProject:hSources
         ::oSM:saveNamedSource( oSource:original )
      NEXT
   ENDIF

   cHbpFN     := hbide_pathFile( ::oProject:location, iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )
   cHbpPath   := cHbpFN + iif( ::lPPO, '_tmp', "" ) + ".hbp"

   IF !( ::lPPO )
      IF     ::oProject:type == "Lib"
         aadd( aHbp, "-hblib" )
      ELSEIF ::oProject:type == "Dll"
      // aadd( aHbp, "-hbdynvm" )  /* Better if is provided as a flag -hbdyn  or   -hbdynvm */
      ENDIF
   ENDIF

   IF ::oProject:isXhb
      aadd( aHbp, "-xhb" )
   ENDIF
   aadd( aHbp, "-q"          )
   aadd( aHbp, "-trace"      )
   aadd( aHbp, "-info"       )
   aadd( aHbp, "-lang=en"    )
   aadd( aHbp, "-width=512"  )
   IF lRebuild
      aadd( aHbp, "-rebuild" )
   ENDIF

   IF ::lPPO
      IF !empty( oEdit := ::oEM:getEditorCurrent() )
         IF hbide_isSourcePRG( oEdit:sourceFile )
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
      ::oOutputResult:oWidget:append( hbide_outputLine() )
      cTmp := "Project [ " + cProject                     + " ]    " + ;
              "Launch [ "  + iif( lLaunch , 'Yes', 'No' ) + " ]    " + ;
              "Rebuild [ " + iif( lRebuild, 'Yes', 'No' ) + " ]    " + ;
              "Started [ " + time() + " ]"
      ::oOutputResult:oWidget:append( cTmp )
      ::oOutputResult:oWidget:append( hbide_outputLine() )

      ::oIDE:oEV := IdeEnvironments():new():create( ::oIDE )
      ::cBatch   := ::oEV:prepareBatch( ::cWrkEnvironment )
      aeval( ::oEV:getHbmk2Commands( ::cWrkEnvironment ), {|e| aadd( aHbp, e ) } )

      cExeHbMk2  := ::oINI:getHbmk2File()
      cCmdParams := hbide_array2cmdParams( aHbp )

      ::oProcess := HbpProcess():new()
      //
      ::oProcess:output      := {|cOut, mp2, oHbp| ::showOutput( cOut,mp2,oHbp ) }
      ::oProcess:finished    := {|nEC , nES, oHbp| ::finished( nEC ,nES,oHbp ) }
      ::oProcess:workingPath := hbide_pathToOSPath( ::oProject:location )
      //
      cCmd := hbide_getShellCommand()
      cC   := iif( hbide_getOS() == "nix", "", "/C " )

      IF hb_fileExists( ::cBatch )
         cBuf := memoread( ::cBatch )
         cBuf += hb_eol() + cExeHbMk2 + " " + cHbpPath + cCmdParams + hb_eol()
         hb_memowrit( ::cBatch, cBuf )
      ENDIF
      //
      ::outputText( cbRed + "Batch File " + iif( hb_fileExists( ::cBatch ), " Exists", " : doesn't Exist" ) + " => " + ceRed + trim( ::cBatch ) )
      ::outputText( cbRed + "Batch File Contents => " + ceRed )
      ::outputText( memoread( ::cBatch ) )
      ::outputText( cbRed + "Command => " + ceRed + cCmd )
      ::outputText( cbRed + "Arguments => " + ceRed + cC + ::cBatch )
      ::outputText( hbide_outputLine() )
      //
      ::oProcess:addArg( cC + ::cBatch )
      ::oProcess:start( cCmd )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:showOutput( cOutput, mp2, oProcess )
   LOCAL cIfError

   HB_SYMBOL_UNUSED( mp2 )
   HB_SYMBOL_UNUSED( oProcess )

   cIfError := hbide_convertBuildStatusMsgToHtml( cOutput, ::oOutputResult:oWidget )
   IF ! empty( cIfError ) .AND. empty( ::cIfError )
      ::cIfError := cIfError
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:finished( nExitCode, nExitStatus, oProcess )
   LOCAL cTmp, n, n1, cTkn, cExe, qDoc, qCursor

   HB_SYMBOL_UNUSED( oProcess )

   ::outputText( hbide_outputLine() )
   cTmp := "Exit Code [ " + hb_ntos( nExitCode ) + " ]    Exit Status [ " + hb_ntos( nExitStatus ) + " ]    " +;
           "Finished at [ " + time() + " ]    Done in [ " + hb_ntos( seconds() - oProcess:started ) + " Secs ]"
   ::outputText( cTmp )
   ::outputText( hbide_outputLine() )

   ferase( ::cBatch )

   IF ! empty( ::cIfError )
      ::oOutputResult:SelStart := 0
      ::oOutputResult:find( ::cIfError )
      ::oOutputResult:SelBold  := .T.

      qDoc := ::oOutputResult:document()
      FOR n := 0 TO qDoc:blockCount() - 1
         IF ::cIfError == qDoc:findBlockByNumber( n ):text()
            qCursor := qDoc:find( ::cIfError )
            ::oOutputResult:setTextCursor( qCursor )
            EXIT
         ENDIF
      NEXT
   ENDIF

   cTmp := ::oOutputResult:oWidget:toPlainText()
   cExe := ""
   IF empty( cExe )
      cTkn := "hbmk2: Linking... "
      IF ( n := at( cTkn, cTmp ) ) > 0
         n1   := hb_at( Chr( 10 ), cTmp, n + len( cTkn ) )
         cExe := StrTran( substr( cTmp, n + len( cTkn ), n1 - n - len( cTkn ) ), Chr( 13 ) )
      ENDIF
   ENDIF
   IF empty( cExe )
      cTkn := "hbmk2: Target up to date: "
      IF ( n := at( cTkn, cTmp ) ) > 0
         n1   := hb_at( Chr( 10 ), cTmp, n + len( cTkn ) )
         cExe := StrTran( substr( cTmp, n + len( cTkn ), n1 - n - len( cTkn ) ), Chr( 13 ) )
      ENDIF
   ENDIF

   IF hb_isObject( ::cargo )
      cExe := hb_PathJoin( hbide_pathToOSPath( ::cargo:cPath ), cExe )
   ELSE
      cExe := hb_PathJoin( hbide_pathToOSPath( ::oProject:location ), cExe )
   ENDIF

   IF !empty( cExe )
      hb_fNameSplit( cExe, @cTmp )
      hbide_setProjectOutputPath( cTmp )
   ENDIF

   IF ::lLaunch
      ::outputText( " " )
      IF empty( cExe )
         ::outputText( "<font color=red>" + "Executable could not been detected from linker output!" + "</font>" )
      ELSE
         cExe := alltrim( cExe )
         ::outputText( "<font color=blue>" + "Detected executable => " + cExe + "</font>" )
      ENDIF
      ::outputText( " " )

      IF nExitCode == 0
         ::launchProject( ::cProjectInProcess, cExe )
      ELSE
         ::outputText( "Sorry, cannot launch project because of errors..." )
      ENDIF
   ENDIF
   IF ::lPPO .AND. hb_FileExists( ::cPPO )
      ::editSource( ::cPPO )
   ENDIF

   ::cIfError := NIL
   ::oOutputResult:ensureCursorVisible()
   IF !empty( qCursor )
      qCursor:clearSelection()
      ::oOutputResult:setTextCursor( qCursor )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Launch selected project.
 * 03/01/2010 - 09:24:50
 */
METHOD IdeProjManager:launchProject( cProject, cExe )
   LOCAL cTargetFN, cTmp, oProject, cPath
   LOCAL qProcess, qStr

   IF empty( cProject )
      cProject := ::oPM:getCurrentProject( .f. )
   ENDIF

   IF !empty( cProject )
      oProject  := ::getProjectByTitle( cProject )
   ENDIF

   IF empty( cExe ) .AND. !empty( oProject )
      cTargetFN := hbide_pathFile( oProject:destination, iif( empty( oProject:outputName ), "_temp", oProject:outputName ) )
      #ifdef __PLATFORM__WINDOWS
      IF oProject:type == "Executable"
         cTargetFN += '.exe'
      ENDIF
      #endif
      IF ! hb_FileExists( cTargetFN )
         cTargetFN := oProject:launchProgram
      ENDIF
   ELSE
      cTargetFN := cExe
   ENDIF
   IF empty( cTargetFN )
      cTargetFN := ""
   ENDIF
   cTargetFN := hbide_pathToOSPath( cTargetFN )

   IF ! hb_FileExists( cTargetFN )
      cTmp := "Launch error: file not found - " + cTargetFN

   ELSEIF empty( oProject ) .OR. oProject:type == "Executable"
      cTmp := "Launching application [ " + cTargetFN + " ]"

      if .t.
         qProcess := QProcess()

         qStr := QStringList()

         IF !empty( oProject )
            IF !empty( oProject:launchParams )
               qStr:append( oProject:launchParams )
            ENDIF
            qProcess:startDetached( cTargetFN, qStr, hbide_pathToOSPath( oProject:wrkDirectory ) )
         ELSE
            hb_fNameSplit( cTargetFN, @cPath )
            qProcess:startDetached( cTargetFN, qStr, hbide_pathToOSPath( cPath ) )
         ENDIF

         qProcess:waitForStarted( 3000 )
         qProcess := NIL

      else
         hb_processRun( cTargetFN, , , , .t. )

      endif
   ELSE
      cTmp := "Launching application [ " + cTargetFN + " ] ( not applicable )."

   ENDIF

   ::oOutputResult:oWidget:append( cTmp )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:runAsScript()
   LOCAL oEdit

   IF !empty( oEdit := ::oEM:getEditorCurrent() )
      hbide_runAScript( oEdit:qEdit:toPlainText() )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:outputText( cText )

   ::oOutputResult:oWidget:append( "<font color=black>" + cText + "</font>" )

   RETURN Self

/*----------------------------------------------------------------------*/

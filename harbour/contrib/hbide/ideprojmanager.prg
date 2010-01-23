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

#define CHN_BGN                                   1
#define CHN_OUT                                   2
#define CHN_ERR                                   3
#define CHN_FIN                                   4
#define CHN_STT                                   5
#define CHN_ERE                                   6
#define CHN_CLO                                   7
#define CHN_BYT                                   8
#define CHN_RCF                                   9
#define CHN_REA                                   10

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
   DATA   launchParams                            INIT ""
   DATA   launchProgram                           INIT ""
   DATA   hbpFlags                                INIT {}
   DATA   sources                                 INIT {}
   DATA   metaData                                INIT {}
   DATA   dotHbp                                  INIT ""
   DATA   compilers                               INIT ""

   METHOD new( aProps )
   METHOD applyMeta( s )
   METHOD expandMeta( s )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProject:new( aProps )
   LOCAL b_, a_

   IF hb_isArray( aProps ) .AND. !empty( aProps )
      ::aProjProps := aProps

      b_:= aProps

      //::normalizedName := b_[ 1 ]
      //::fileName       := b_[ 2 ]

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

      FOR EACH a_ IN ::metaData
         a_[ 2 ] := hbide_pathNormalized( a_[ 2 ], .f. )
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

   DATA   qProcess
   DATA   nStarted                                INIT 0

   DATA   lLaunch                                 INIT .f.
   DATA   cProjectInProcess                       INIT ""
   DATA   cPPO                                    INIT ""
   DATA   lPPO                                    INIT .f.
   DATA   oProject

   METHOD new( oIde )
   METHOD create( oIde )
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
   METHOD setCurrentProject( cProjectName )
   METHOD getCurrentProject( lAlert )
   METHOD selectCurrentProject()
   METHOD getProjectProperties( cProjectTitle )
   METHOD getProjectByFile( cProjectFile )
   METHOD getProjectByTitle( cProjectTitle )
   METHOD closeProject( cProject )
   METHOD promptForPath( cObjPathName, cTitle, cObjFileName, cObjPath2, cObjPath3 )
   METHOD buildProject( cProject, lLaunch, lRebuild, lPPO, lViaQt )
   METHOD buildProcess()
   METHOD readProcessInfo( nMode, i, ii )
   METHOD launchProject( cProject )

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
      ::oIde:aMeta := ::aPrjProps[ PRJ_PRP_METADATA, 2 ]
   ENDIF

   IF lFetch
      /* Access/Assign via this object */
      ::oProject := IdeProject():new( ::aPrjProps )
      //
      ::fetchProperties()
      IF !empty( ::cSaveTo ) .and. hb_FileExists( ::cSaveTo )
         cProjFileName := ::cSaveTo
         ::aPrjProps := hbide_fetchHbiStructFromFile( hbide_pathToOSPath( cProjFileName ) ) /* Reload from file */
      ENDIF
   ENDIF

   IF n == 0
      aadd( ::oIde:aProjects, { hbide_pathNormalized( cProjFileName ), cProjFileName, aclone( ::aPrjProps ) } )
      IF lUpdateTree
         ::oIde:updateProjectTree( ::aPrjProps )
      ENDIF
      hbide_mnuAddFileToMRU( ::oIde, cProjFileName, INI_RECENTPROJECTS )
   ELSE
      ::aProjects[ n, 3 ] := aclone( ::aPrjProps )
      IF lUpdateTree
         ::oIde:updateProjectTree( ::aPrjProps )
      ENDIF
      IF lUpdateTree .AND. ::aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_qPrjType ] <> t
         MsgBox( "::removeProjectFromTree( ::aPrjProps )" )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:fetchProperties()
   LOCAL cLukupPng
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

   cLukupPng := ::resPath + "folder.png"
   ::oUI:q_buttonChoosePrjLoc:setIcon( cLukupPng )
   ::oUI:q_buttonChooseWd:setIcon( cLukupPng )
   ::oUI:q_buttonChooseDest:setIcon( cLukupPng )

   ::oUI:q_buttonSelect:setIcon( ::resPath + "open.png" )
   ::oUI:q_buttonSort:setIcon( ::resPath + "toupper.png" )     // TODO: toupper.png => atoz.png
   ::oUI:q_buttonSortZA:setIcon( ::resPath + "tolower.png" )   //       tolower.png => ztoa.png
   ::oUI:q_buttonSortOrg:setIcon( ::resPath + "invertcase.png" )   //       tolower.png => ztoa.png

   ::oUI:signal( "buttonCn"          , "clicked()", {|| ::oUI:oWidget:close()  } )
   ::oUI:signal( "buttonSave"        , "clicked()", {|| ::save( .F. )          } )
   ::oUI:signal( "buttonSaveExit"    , "clicked()", {|| ::save( .T. )          } )
   ::oUI:signal( "buttonSelect"      , "clicked()", {|| ::addSources()         } )
   ::oUI:signal( "buttonSort"        , "clicked()", {|| ::sortSources( "az"  ) } )
   ::oUI:signal( "buttonSortZA"      , "clicked()", {|| ::sortSources( "za"  ) } )
   ::oUI:signal( "buttonSortOrg"     , "clicked()", {|| ::sortSources( "org" ) } )
   //
   ::oUI:signal( "tabWidget"         , "currentChanged(int)", {|o,p| ::updateHbp( p, o ) } )
   ::oUI:signal( "editMetaData"      , "textChanged()", {|o| ::updateMetaData( o ) } )

   ::oUI:signal( "buttonChoosePrjLoc", "clicked()", {|| ::PromptForPath( 'editPrjLoctn',  'Choose the Project Location...'  ) } )//, ;
                       //                                                       'editOutName', "editWrkFolder", "editDstFolder" ) } )
   ::oUI:signal( "buttonChooseWd"    , "clicked()", {|| ::PromptForPath( 'editWrkFolder', 'Choose a Working Folder...'     ) } )
   ::oUI:signal( "buttonChooseDest"  , "clicked()", {|| ::PromptForPath( 'editDstFolder', 'Choose a Destination Folder...' ) } )

   IF empty( ::aPrjProps )
      /*
       * When they click on the button to confirm the name of the project, we
       * will adjust the other parameters. (vailtoms)
       * 25/12/2009 - 20:40:22
       */
      ::oUI:q_editPrjLoctn:setText( hbide_pathNormalized( cPrjLoc, .F. ) )

   ELSE
      ::oUI:q_editPrjTitle:setText( ::oProject:applyMeta( ::oProject:title ) )
      ::oUI:q_editPrjLoctn:setText( ::oProject:applyMeta( ::oProject:location ) )
      ::oUI:q_editWrkFolder:setText( ::oProject:applyMeta( ::oProject:wrkDirectory ) )
      ::oUI:q_editDstFolder:setText( ::oProject:applyMeta( ::oProject:destination ) )
      ::oUI:q_editOutName:setText( ::oProject:outputName )

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

   ::setPosAndSizeByIni( ::oUI:oWidget, PropsDialogGeometry )
   //
   ::oUI:exec()
   //
   ::aIni[ INI_HBIDE, PropsDialogGeometry ] := hbide_posAndSize( ::oUI:oWidget )

   ::oUI:destroy()
   ::oUI := NIL

   ::manageFocusInEditor()

   RETURN Self

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
   aadd( txt_, "Title             = " + ::oUI:q_editPrjTitle:text() )
   aadd( txt_, "Location          = " + ::oUI:q_editPrjLoctn:text() )
   aadd( txt_, "WorkingFolder     = " + ::oUI:q_editWrkFolder:text() )
   aadd( txt_, "DestinationFolder = " + ::oUI:q_editDstFolder:text() )
   aadd( txt_, "Output            = " + ::oUI:q_editOutName:text() )
   aadd( txt_, "LaunchParams      = " + ::oUI:q_editLaunchParams:text() )
   aadd( txt_, "LaunchProgram     = " + ::oUI:q_editLaunchExe:text() )
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
      ::oIde:cWrkProject := ''

   ELSEIF ( n := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cProjectName } ) ) > 0
      aPrjProps     := ::aProjects[ n, 3 ]
      ::oIde:cWrkProject := aPrjProps[ PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ]

   ELSE
    * MsgBox( 'Invalid project selected: "' + cProjectName + '"' )
      lValid := .F.

   ENDIF

   IF lValid
      IF !Empty( ::oSBar )
         ::oSBar:getItem( SB_PNL_PROJECT ):caption := ::cWrkProject
      ENDIF

      ::oIde:updateTitleBar()
      ::oIde:updateProjectMenu()

      /* Reset Old Color */
      IF !empty( cOldProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIde, cOldProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, QBrush():new( "QColor", QColor():new( 0,0,0 ) ) )
            //oItem:oWidget:setBackground( 0, QBrush():new( "QColor", QColor():new( 255,255,255 ) ) )
         ENDIF
      ENDIF
      /* Set New Color */
      IF !empty( ::cWrkProject )
         IF !empty( oItem := hbide_findProjTreeItem( ::oIde, ::cWrkProject, "Project Name" ) )
            oItem:oWidget:setForeground( 0, ::qBrushWrkProject )
            //oItem:oWidget:setBackground( 0, ::qBrushWrkProject )
            hbide_expandChildren( ::oIde, oItem )
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
   LOCAL oDlg, i, p, t

   IF Empty( ::aProjects )
      MsgBox( "No Projects Available" )
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

   RETURN IdeProject():new( aProj )

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:getProjectByTitle( cProjectTitle )
   LOCAL n, aProj

   IF ( n := ascan( ::aProjects, {|e_, x| x := e_[ 3 ], x[ 1, 2, PRJ_PRP_TITLE ] == cProjectTitle } ) ) > 0
      aProj := ::aProjects[ n, 3 ]
   ENDIF

   RETURN IdeProject():new( aProj )

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:closeProject( cProject )
   LOCAL nPos

   IF Empty( ::aProjects )
      MsgBox( "No Projects Available" )
      RETURN Self
   ENDIF

   DEFAULT cProject TO ::getCurrentProject()

   nPos := ascan( ::aProjects, {|e_| e_[ 3, PRJ_PRP_PROPERTIES, 2, E_oPrjTtl ] == cProject } )
   IF ( nPos < 0 )
      MsgBox( 'Invalid project: "' + cProject + '"' )
      RETURN Self
   ENDIF

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

   cTemp := ::oProject:expandMeta( ::oUI:qObj[ cObjPathName ]:Text() )

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

      cPath := ::oProject:applyMeta( cPath )

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
   LOCAL cOutput, cErrors, cHbpPath, qStringList, oEdit, cHbpFN, cTmp, nResult, cTargetFN
   LOCAL aHbp := {}

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

   cTargetFN  := hbide_pathToOSPath( ::oProject:destination + ::pathSep + ;
                      iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )

   cHbpFN     := hbide_pathToOSPath( ::oProject:wrkDirectory + ::pathSep + ;
                      iif( empty( ::oProject:outputName ), "_temp", ::oProject:outputName ) )

   //cHbpPath  := cHbpFN + iif( ::lPPO, '.' + hb_md5( hb_ntos( seconds() ) ), "" ) + ".hbp"
   cHbpPath  := cHbpFN + iif( ::lPPO, '._tmp', "" ) + ".hbp"

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
      aadd( aHbp, "-workdir=" + ::oProject:wrkDirectory + "/${hb_plat}/${hb_comp}" )
   ENDIF
   aadd( aHbp, "-q"             )
   aadd( aHbp, "-trace"         )
   aadd( aHbp, "-info"          )
   IF lRebuild
      aadd( aHbp, "-rebuild"    )
   ENDIF
   aadd( aHbp, " " )

   IF !( ::lPPO )
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
            aadd( aHbp, " " )

            // TODO: We have to test if the current file is part of a project, and we
            // pull your settings, even though this is not the active project - vailtom
            aadd( aHbp, hbide_pathToOSPath( oEdit:sourceFile ) )

            ::cPPO := hbide_pathToOSPath( oEdit:cPath + oEdit:cFile + '.ppo' )
            FErase( ::cPPO )

         ELSE
            MsgBox( 'Operation not supported for this file type: "' + oEdit:sourceFile + '"' )
            RETURN Self

         ENDIF

         lViaQt := .t.   /* Donot know why it fails with Qt */
      ENDIF
   ENDIF

   ::oIde:lDockBVisible := .t.
   ::oDockB2:show()
   ::oOutputResult:oWidget:clear()

   IF !hbide_createTarget( cHbpPath, aHbp )
      ::oOutputResult:oWidget:append( 'Error saving: ' + cHbpPath )

   ELSE
      ::nStarted := seconds()

      cTmp := hbide_outputLine() + CRLF + ;
              "Project [ " + cProject                     + " ]    " + ;
              "Launch [ "  + iif( lLaunch , 'Yes', 'No' ) + " ]    " + ;
              "Rebuild [ " + iif( lRebuild, 'Yes', 'No' ) + " ]    " + ;
              "Started [ " + time() + " ]" + CRLF + ;
              hbide_outputLine() + CRLF
      ::oOutputResult:oWidget:append( cTmp )

      IF lViaQt
         ::buildProcess()

         #if 0  /* Mechanism to supply environment variables to called process */
                /* I do not know nixes but assume that Qt must be issueing proper */
                /* shell command for the target OS to set them. */
                /* If I am not wrong, HBMK2 can have these variables alread set */
                /* and hence developer can choose any compiler of his choice. */
                /*                                                                */
                /* Actually, this was the intension in hbIDE.env I commited in IDE root */
         qStringList := QStringList():new()
         qStringList:append( "HB_WITH_QT=c:\qt\4.5.3\lib" )
         ::qProcess:setEnvironment( qStringList )

         qStringList := QStringList():new()
         qStringList:append( [/c  c:\batches\SetMinGW-harbour-E.bat && hbMK2.exe ] + cHbpPath )
         ::qProcess:start( "cmd.exe", qStringList )
         #else

         qStringList := QStringList():new()
         IF ::lPPO
            qStringList:append( "-hbraw" )
         ENDIF
         qStringList:append( cHbpPath )
         //
         ::qProcess:setWorkingDirectory( ::oProject:wrkDirectory )
         //
         ::qProcess:start( "hbmk2", qStringList )
         #endif
      ELSE
         cOutput := "" ; cErrors := ""
         nResult := hb_processRun( ( "hbmk2 " + iif( ::lPPO, "-hbraw ", "" ) + cHbpPath ), , @cOutput, @cErrors )

         cTmp := cOutput + CRLF
         cTmp += IIF( empty( cErrors ), "", cErrors ) + CRLF
         cTmp += hbide_outputLine() + CRLF
         cTmp += "Exit Code [ " + hb_ntos( nResult ) + " ]    " + ;
                 "Finished at [ " + time() + " ]    " + ;
                 "Done in [ " + hb_ntos( seconds() - ::nStarted ) + " Secs ]" + CRLF
         cTmp += hbide_outputLine() + CRLF

         hbide_convertBuildStatusMsgToHtml( cTmp, ::oOutputResult:oWidget )

         IF ( nResult == 0 ) .AND. ( lLaunch )
            ::LaunchProject( cProject )
         ENDIF

         IF ::lPPO .AND. hb_FileExists( ::cPPO )
            ::editSource( ::cPPO )
         ENDIF
      ENDIF

   ENDIF

   IF ::lPPO
      FErase( cHbpPath )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:buildProcess()

   ::qProcess := QProcess():new()
   ::qProcess:setReadChannel( 1 )

   Qt_Slots_Connect( ::pSlots, ::qProcess, "readyRead()"              , {|o,i| ::readProcessInfo( CHN_REA, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "readChannelFinished()"    , {|o,i| ::readProcessInfo( CHN_RCF, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "aboutToClose()"           , {|o,i| ::readProcessInfo( CHN_CLO, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "bytesWritten(int)"        , {|o,i| ::readProcessInfo( CHN_BYT, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "stateChanged(int)"        , {|o,i| ::readProcessInfo( CHN_STT, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "error(int)"               , {|o,i| ::readProcessInfo( CHN_ERE, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "started()"                , {|o,i| ::readProcessInfo( CHN_BGN, o, i ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "readyReadStandardOutput()", {|o,i| ::readProcessInfo( CHN_OUT, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "readyReadStandardError()" , {|o,i| ::readProcessInfo( CHN_ERR, i, o ) } )
   Qt_Slots_Connect( ::pSlots, ::qProcess, "finished(int,int)"        , {|o,i,ii| ::readProcessInfo( CHN_FIN, i, ii, o ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjManager:readProcessInfo( nMode, i, ii )
   LOCAL cLine, cTmp, nSize

   nSize := 16384

   DO CASE
   CASE nMode == CHN_REA   // ReadReady()

   CASE nMode == CHN_OUT
      ::qProcess:setReadChannel( 0 )
      cLine := space( nSize )
      ::qProcess:read( @cLine, nSize )
      IF !empty( cLine )
         hbide_convertBuildStatusMsgToHtml( trim( cLine ), ::oOutputResult:oWidget )
      ENDIF

   CASE nMode == CHN_ERR
      ::qProcess:setReadChannel( 1 )
      cLine := space( nSize )
      ::qProcess:read( @cLine, nSize )
      IF !empty( cLine )
         hbide_convertBuildStatusMsgToHtml( trim( cLine ), ::oOutputResult:oWidget )
      ENDIF

   CASE nMode == CHN_FIN
      cTmp := hbide_outputLine() + CRLF
      cTmp += "Exit Code [ " + hb_ntos( i ) + " ]    Exit Status [ " + hb_ntos( ii ) + " ]    " +;
              "Finished at [ " + time()     + " ]    Done in [ " + hb_ntos( seconds() - ::nStarted ) +" Secs ]" + CRLF
      cTmp += hbide_outputLine() + CRLF

      ::oOutputResult:oWidget:append( cTmp )

      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readyRead()"               )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readChannelFinished()"     )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "aboutToClose()"            )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "bytesWritten(int)"         )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "stateChanged(int)"         )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "error(int)"                )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "started()"                 )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readyReadStandardOutput()" )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "readyReadStandardError()"  )
      Qt_Slots_disConnect( ::pSlots, ::qProcess, "finished(int,int)"         )

      ::qProcess:kill()
      ::qProcess:pPtr := 0
      ::qProcess := NIL

      IF ::lLaunch
         ::launchProject( ::cProjectInProcess )
      ENDIF
      IF ::lPPO .AND. hb_FileExists( ::cPPO )
         ::editSource( ::cPPO )
      ENDIF
   ENDCASE

   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * Launch selected project.
 * 03/01/2010 - 09:24:50
 */
METHOD IdeProjManager:launchProject( cProject )
   LOCAL qProcess, cTargetFN, cTmp, oProject

   IF empty( cProject )
      cProject := ::oPM:getCurrentProject()
   ENDIF
   IF empty( cProject )
      RETURN Self
   ENDIF

   oProject := ::getProjectByTitle( cProject )

   cTargetFN := hbide_pathToOSPath( oProject:destination + ::pathSep + ;
                      iif( empty( oProject:outputName ), "_temp", oProject:outputName ) )

#ifdef __PLATFORM__WINDOWS
   IF oProject:type == "Executable"
      cTargetFN += '.exe'
   ENDIF
#endif

   IF !hb_FileExists( cTargetFN )
      cTmp := "Launch application error: file not found " + cTargetFN + "!"

   ELSEIF oProject:type == "Executable"
      cTmp := "Launch application " + cTargetFN + "... "

      qProcess := QProcess():new()

      qProcess:setWorkingDirectory( hbide_pathToOSPath( oProject:wrkDirectory ) )
hbide_dbg( oProject:wrkDirectory, qProcess:workingDirectory()  )
      qProcess:startDetached_2( cTargetFN )
      //qProcess:waitForStarted()
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


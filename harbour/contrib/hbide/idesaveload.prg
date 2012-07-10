/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               28Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

#define INI_SECTIONS_COUNT                        14
#define INI_HBIDE_VRBLS                           30

/*----------------------------------------------------------------------*/
//
//                            Class IdeINI
//
/*----------------------------------------------------------------------*/

CLASS IdeINI INHERIT IdeObject

   DATA   aINI                                    INIT  {}

   DATA   cMainWindowGeometry                     INIT  ""
   DATA   cGotoDialogGeometry                     INIT  ""
   DATA   cFindDialogGeometry                     INIT  ""
   DATA   cToolsDialogGeometry                    INIT  ""
   DATA   cSetupDialogGeometry                    INIT  ""
   DATA   cShortcutsDialogGeometry                INIT  ""
   DATA   cDbStructDialogGeometry                 INIT  ""
   DATA   cTablesDialogGeometry                   INIT  ""
   DATA   cChangelogDialogGeometry                INIT  ""
   DATA   cStatsDialogGeometry                    INIT  ""
   //
   DATA   cRecentTabIndex                         INIT  ""
   //
   DATA   cIdeTheme                               INIT  ""
   DATA   cIdeAnimated                            INIT  ""
   //
   DATA   cPathHrbRoot                            INIT  ""
   DATA   cPathHbmk2                              INIT  ""
   DATA   cPathResources                          INIT  ""
   DATA   cPathTemp                               INIT  ""
   DATA   cPathEnv                                INIT  ""
   DATA   cPathShortcuts                          INIT  ""
   DATA   cPathSnippets                           INIT  ""
   DATA   cPathThemes                             INIT  ""

   DATA   cVSSExe                                 INIT  ""
   DATA   cVSSDatabase                            INIT  ""

   DATA   cCurrentProject                         INIT  ""
   DATA   cCurrentTheme                           INIT  ""
   DATA   cCurrentCodec                           INIT  ""
   DATA   cCurrentEnvironment                     INIT  ""
   DATA   cCurrentFind                            INIT  ""
   DATA   cCurrentFolderFind                      INIT  ""
   DATA   cCurrentReplace                         INIT  ""
   DATA   cCurrentView                            INIT  ""
   //
   DATA   cTextFileExtensions                     INIT  ".c,.cpp,.prg,.h,.ch,.txt,.log,.ini,.env,.ppo,.qtp,.hb," + ;
                                                        ".cc,.hbc,.hbp,.hbm,.xml,.bat,.sh,.rc,.ui,.uic,.bak,.fmg,.qth,.qrc"
   DATA   aProjFiles                              INIT  {}
   DATA   aFiles                                  INIT  {}
   DATA   aFind                                   INIT  {}
   DATA   aReplace                                INIT  {}
   DATA   aRecentProjects                         INIT  {}
   DATA   aRecentFiles                            INIT  {}
   DATA   aFolders                                INIT  {}
   DATA   aViews                                  INIT  {}
   DATA   aTaggedProjects                         INIT  {}
   DATA   aTools                                  INIT  {}
   DATA   aUserToolbars                           INIT  {}
   DATA   aKeywords                               INIT  {}
   DATA   aDbuPanelNames                          INIT  {}
   DATA   aDbuPanelsInfo                          INIT  {}
   DATA   aDictionaries                           INIT  {}
   DATA   aLogTitle                               INIT  {}
   DATA   aLogSources                             INIT  {}

   DATA   cFontName                               INIT  "Courier New"
   DATA   nPointSize                              INIT  10
   DATA   cLineEndingMode                         INIT  ""

   DATA   lTrimTrailingBlanks                     INIT  .f.
   DATA   lSaveSourceWhenComp                     INIT  .t.
   DATA   lSupressHbKWordsToUpper                 INIT  .f.
   DATA   lReturnAsBeginKeyword                   INIT  .f.
   DATA   lConvTabToSpcWhenLoading                INIT  .f.
   DATA   lAutoIndent                             INIT  .t.
   DATA   lSmartIndent                            INIT  .t.
   DATA   lTabToSpcInEdits                        INIT  .t.
 //DATA   nTabSpaces                              INIT  ::oIde:nTabSpaces
   DATA   nIndentSpaces                           INIT  3

   DATA   nTmpBkpPrd                              INIT  60
   DATA   cBkpPath                                INIT  ""
   DATA   cBkpSuffix                              INIT  ".bkp"

   DATA   lCompletionWithArgs                     INIT  .t.
   DATA   lCompleteArgumented                     INIT  .f.

   DATA   aAppThemes                              INIT  {}
   DATA   lEditsMdi                               INIT  .t.

   DATA   lShowEditsLeftToolbar                   INIT  .t.
   DATA   lShowEditsTopToolbar                    INIT  .t.

   DATA   nDocksTabShape                          INIT  QTabWidget_Triangular
   DATA   nDocksLeftTabPos                        INIT  QTabWidget_South
   DATA   nDocksTopTabPos                         INIT  QTabWidget_South
   DATA   nDocksBottomTabPos                      INIT  QTabWidget_South
   DATA   nDocksRightTabPos                       INIT  QTabWidget_South

   DATA   cChangeLog                              INIT  ""
   DATA   cUserChangeLog                          INIT  ""

   DATA   lShowHideDocks                          INIT  .t.
   DATA   nEditsViewStyle                         INIT  0
   DATA   cToolbarSize                            INIT  "12"

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD load( cHbideIni )
   METHOD save( cHbideIni )

   METHOD getIniPath()
   METHOD getResourcesPath()
   METHOD getTempPath()
   METHOD getHarbourPath()
   METHOD getIniFile()
   METHOD getEnvFile()
   METHOD getHbmk2File()
   METHOD getSnippetsFile()
   METHOD getShortcutsFile()
   METHOD getThemesFile()
   METHOD showHideDocks()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeINI:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:destroy()

   ::aINI              := NIL
   ::aAppThemes        := NIL

   ::aProjFiles        := NIL
   ::aFiles            := NIL
   ::aFind             := NIL
   ::aReplace          := NIL
   ::aRecentProjects   := NIL
   ::aRecentFiles      := NIL
   ::aFolders          := NIL
   ::aViews            := NIL
   ::aTaggedProjects   := NIL
   ::aTools            := NIL
   ::aUserToolbars     := NIL
   ::aKeywords         := NIL
   ::aDbuPanelNames    := NIL
   ::aDbuPanelsInfo    := NIL
   ::aDictionaries     := NIL

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeINI:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:getINIPath()
   LOCAL cPath
   hb_fNameSplit( ::oIde:cProjIni, @cPath )
   RETURN cPath

/*------------------------------------------------------------------------*/

METHOD IdeINI:getResourcesPath()
   LOCAL cPath := iif( empty( ::cPathResources ), ::getINIPath(), ::cPathResources )
   RETURN iif( empty( cPath ), cPath, hbide_pathToOSPath( hbide_pathAppendLastSlash( cPath ) ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getHarbourPath()
   LOCAL cPath := ::cPathHrbRoot

   IF empty( cPath )
      IF empty( cPath := hb_getEnv( "HB_INSTALL_PREFIX" ) ) /* This covers Harbour developers */
         hb_fNameSplit( hb_dirBase(), @cPath )              /* This covers USERS of nightly builds */
         IF ! hb_fileExists( cPath + "harbour.exe" )
            IF ! hb_fileExists( cPath + "harbour" )
               cPath := ""
            ENDIF
         ENDIF
         IF ! empty( cPath )
            cPath := hbide_pathAppendLastSlash( cPath ) + ".." + hb_ps()
         ENDIF
      ENDIF
   ENDIF
   ::cPathHrbRoot := iif( empty( cPath ), "", hbide_pathToOSPath( hbide_pathAppendLastSlash( cPath ) ) )

   RETURN ::cPathHrbRoot

/*------------------------------------------------------------------------*/

METHOD IdeINI:getTempPath()
   RETURN hbide_pathToOSPath( ::cPathTemp )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getINIFile()
   RETURN hbide_pathToOSPath( hbide_pathFile( ::getINIPath(), "hbide.ini" ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getHbmk2File()
   LOCAL cFile

   IF empty( ::cPathHbmk2 )
      IF empty( cFile := hb_getenv( "HBIDE_DIR_HBMK2" ) )
         cFile := "hbmk2"
      ELSE
         cFile := hbide_pathFile( cFile, "hbmk2" )
      ENDIF
   ELSE
      cFile := ::cPathHbmk2
   ENDIF

   RETURN hbide_pathToOSPath( cFile )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getEnvFile()
   RETURN hbide_pathToOSPath( iif( empty( ::cPathEnv ), hbide_pathFile( ::getINIPath(), "hbide.skl" ), ::cPathEnv ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getSnippetsFile()
   RETURN hbide_pathToOSPath( iif( empty( ::cPathSnippets ), hbide_pathFile( ::getINIPath(), "hbide.skl" ), ::cPathSnippets ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getShortcutsFile()
   RETURN hbide_pathToOSPath( iif( empty( ::cPathShortcuts ), hbide_pathFile( ::getINIPath(), "hbide.scu" ), ::cPathShortcuts ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:getThemesFile()
   RETURN hbide_pathToOSPath( iif( empty( ::cPathThemes ), hbide_pathFile( ::getINIPath(), "hbide.hbt" ), ::cPathThemes ) )

/*------------------------------------------------------------------------*/

METHOD IdeINI:showHideDocks()

   IF ::lShowHideDocks  /* Assumed visible, hide all */
      hbide_saveSettings( ::oIde, "tempsettings.ide" )
      ::oDK:hideAllDocks()
   ELSE
      hbide_restSettings( ::oIde, "tempsettings.ide" )
   ENDIF

   ::lShowHideDocks := ! ::lShowHideDocks

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:save( cHbideIni )
   LOCAL j, nTab, pTab, n, txt_, oEdit, nTabs, nn, a_, s

   DEFAULT cHbideIni TO ::oIde:cProjIni

   IF ::oIde:nRunMode != HBIDE_RUN_MODE_INI
      RETURN Nil
   ENDIF

   IF ! ::lShowHideDocks
      ::showHideDocks()
      ::lShowHideDocks := .f.
   ENDIF

   txt_:= {}

   aadd( txt_, "[HBIDE]" )
   aadd( txt_, " " )
   //
   aadd( txt_, "MainWindowGeometry"        + "=" +   hbide_posAndSize( ::oDlg:oWidget )                 )
   aadd( txt_, "GotoDialogGeometry"        + "=" +   ::cGotoDialogGeometry                              )
   aadd( txt_, "FindDialogGeometry"        + "=" +   ::cFindDialogGeometry                              )
   aadd( txt_, "ToolsDialogGeometry"       + "=" +   ::cToolsDialogGeometry                             )
   aadd( txt_, "ShortcutsDialogGeometry"   + "=" +   ::cShortcutsDialogGeometry                         )
   aadd( txt_, "SetupDialogGeometry"       + "=" +   ::cSetupDialogGeometry                             )
   aadd( txt_, "DbStructDialogGeometry"    + "=" +   ::cDbStructDialogGeometry                          )
   aadd( txt_, "TablesDialogGeometry"      + "=" +   ::cTablesDialogGeometry                            )
   aadd( txt_, "ChangelogDialogGeometry"   + "=" +   ::cChangelogDialogGeometry                         )
   aadd( txt_, "StatsDialogGeometry"       + "=" +   ::cStatsDialogGeometry                             )
   //
   aadd( txt_, "CurrentLineHighlightMode"  + "=" +   iif( ::lCurrentLineHighlightEnabled, "YES", "NO" ) )
   aadd( txt_, "LineNumbersDisplayMode"    + "=" +   iif( ::lLineNumbersVisible, "YES", "NO" )          )
   aadd( txt_, "HorzRulerDisplayMode"      + "=" +   iif( ::lHorzRulerVisible, "YES", "NO" )            )
   //
   aadd( txt_, "RecentTabIndex"            + "=" +   hb_ntos( ::qTabWidget:currentIndex() )             )
   //
   aadd( txt_, "IdeTheme"                  + "=" +   ::cIdeTheme                                        )
   aadd( txt_, "IdeAnimated"               + "=" +   ::cIdeAnimated                                     )
   //
   aadd( txt_, "PathHrbRoot"               + "=" +   ::cPathHrbRoot                                     )
   aadd( txt_, "PathMk2"                   + "=" +   ::cPathHbMk2                                       )
   aadd( txt_, "PathResources"             + "=" +   ::cPathResources                                   )
   aadd( txt_, "PathTemp"                  + "=" +   ::cPathTemp                                        )
   aadd( txt_, "PathEnv"                   + "=" +   ::cPathEnv                                         )
   aadd( txt_, "PathShortcuts"             + "=" +   ::cPathShortcuts                                   )
   aadd( txt_, "PathSnippets"              + "=" +   ::cPathSnippets                                    )
   aadd( txt_, "PathThemes"                + "=" +   ::cPathThemes                                      )
   //
   aadd( txt_, "CurrentProject"            + "=" +   ::oIde:cWrkProject                                 )
   aadd( txt_, "CurrentTheme"              + "=" +   ::oIde:cWrkTheme                                   )
   aadd( txt_, "CurrentCodec"              + "=" +   ::oIde:cWrkCodec                                   )
   aadd( txt_, "CurrentEnvironment"        + "=" +   ::oIde:cWrkEnvironment                             )
   aadd( txt_, "CurrentFind"               + "=" +   ::oIde:cWrkFind                                    )
   aadd( txt_, "CurrentFolderFind"         + "=" +   ::oIde:cWrkFolderFind                              )
   aadd( txt_, "CurrentReplace"            + "=" +   ::oIde:cWrkReplace                                 )
   aadd( txt_, "CurrentView"               + "=" +   ::oIde:cWrkView                                    )
   aadd( txt_, "TextFileExtensions"        + "=" +   ::cTextFileExtensions                              )
   //
   aadd( txt_, "FontName"                  + "=" +   ::cFontName                                        )
   aadd( txt_, "PointSize"                 + "=" +   hb_ntos( ::nPointSize )                            )
   aadd( txt_, "LineEndingMode"            + "=" +   ::cLineEndingMode                                  )
// aadd( txt_, ""        + "=" +   ::c                             )
   //
   aadd( txt_, " " )
   aadd( txt_, "TrimTrailingBlanks"        + "=" +   iif( ::lTrimTrailingBlanks     , "YES", "NO" )     )
   aadd( txt_, "SaveSourceWhenComp"        + "=" +   iif( ::lSaveSourceWhenComp     , "YES", "NO" )     )
   aadd( txt_, "SupressHbKWordsToUpper"    + "=" +   iif( ::lSupressHbKWordsToUpper , "YES", "NO" )     )
   aadd( txt_, "ReturnAsBeginKeyword"      + "=" +   iif( ::lReturnAsBeginKeyword   , "YES", "NO" )     )
   aadd( txt_, "ConvTabToSpcWhenLoading"   + "=" +   iif( ::lConvTabToSpcWhenLoading, "YES", "NO" )     )
   aadd( txt_, "AutoIndent"                + "=" +   iif( ::lAutoIndent             , "YES", "NO" )     )
   aadd( txt_, "SmartIndent"               + "=" +   iif( ::lSmartIndent            , "YES", "NO" )     )
   aadd( txt_, "TabToSpcInEdits"           + "=" +   iif( ::lTabToSpcInEdits        , "YES", "NO" )     )
   aadd( txt_, "TabSpaces"                 + "=" +   hb_ntos( ::oIde:nTabSpaces )                       )
   aadd( txt_, "IndentSpaces"              + "=" +   hb_ntos( ::nIndentSpaces )                         )
   aadd( txt_, "TmpBkpPrd"                 + "=" +   hb_ntos( ::nTmpBkpPrd )                            )
   aadd( txt_, "BkpPath"                   + "=" +   ::cBkpPath                                         )
   aadd( txt_, "BkpSuffix"                 + "=" +   ::cBkpSuffix                                       )
   aadd( txt_, "CodeListWithArgs"          + "=" +   iif( ::lCompletionWithArgs     , "YES", "NO" )     )
   aadd( txt_, "CompletionWithArgs"        + "=" +   iif( ::lCompleteArgumented     , "YES", "NO" )     )
   aadd( txt_, "EditsMdi"                  + "=" +   iif( ::lEditsMdi               , "YES", "NO" )     )
   //
   aadd( txt_, "ShowEditsLeftToolbar"      + "=" +   iif( ::lShowEditsLeftToolbar   , "YES", "NO" )     )
   aadd( txt_, "ShowEditsTopToolbar"       + "=" +   iif( ::lShowEditsTopToolbar    , "YES", "NO" )     )
   aadd( txt_, "DocksTabShape"             + "=" +   hb_ntos( ::nDocksTabShape )                        )
   aadd( txt_, "DocksLeftTabPos"           + "=" +   hb_ntos( ::nDocksLeftTabPos )                      )
   aadd( txt_, "DocksTopTabPos"            + "=" +   hb_ntos( ::nDocksTopTabPos )                       )
   aadd( txt_, "DocksBottomTabPos"         + "=" +   hb_ntos( ::nDocksRightTabPos )                     )
   aadd( txt_, "DocksRightTabPos"          + "=" +   hb_ntos( ::nDocksBottomTabPos )                    )
   aadd( txt_, "ShowHideDocks"             + "=" +   iif( ::lShowHideDocks          , "YES", "NO" )     )
   aadd( txt_, "ChangeLog"                 + "=" +   ::cChangeLog                                       )
   aadd( txt_, "UserChangeLog"             + "=" +   ::cUserChangeLog                                   )
   aadd( txt_, "VSSExe"                    + "=" +   ::cVSSExe                                          )
   aadd( txt_, "VSSDatabase"               + "=" +   ::cVSSDatabase                                     )
   aadd( txt_, "EditsViewStyle"            + "=" +   hb_ntos( ::nEditsViewStyle )                       )
   aadd( txt_, "ToolbarSize"               + "=" +   ::cToolbarSize                                     )

   aadd( txt_, "" )
   aadd( txt_, "[PROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::oIde:aProjects )
      aadd( txt_, "project_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::oIde:aProjects[ n, 2 ], .f. ) )
   NEXT
   aadd( txt_, " " )

   /*-------------------   FILES   -------------------*/
   aadd( txt_, "[FILES]" )
   aadd( txt_, " " )
   nn := 0
   FOR j := 1 TO Len( ::oIde:aViews )
      ::oIde:lClosing := .t.
      ::oDK:setView( ::oIde:aViews[ j ]:oWidget:objectName() )

      nTabs := ::oIde:qTabWidget:count()
      FOR n := 1 TO nTabs
         pTab  := ::oIde:qTabWidget:widget( n - 1 )
         nTab  := ascan( ::oIde:aTabs, {|e_| hbqt_IsEqual( e_[ 1 ]:oWidget, pTab ) } )
         oEdit := ::oIde:aTabs[ nTab, TAB_OEDITOR ]

         IF !Empty( oEdit:sourceFile ) .AND. !( ".ppo" == lower( oEdit:cExt ) )
            IF oEdit:lLoaded
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_getEditInfoAsString( oEdit ) )

            ELSE
               aadd( txt_, "file_" + hb_ntos( ++nn ) + "=" + hbide_pathNormalized( oEdit:sourceFile, .f. ) + "," + ;
                           hb_ntos( oEdit:nPos  ) +  ","  + ;
                           hb_ntos( oEdit:nHPos ) +  ","  + ;
                           hb_ntos( oEdit:nVPos ) +  ","  + ;
                           oEdit:cTheme           +  ","  + ;
                           oEdit:cView            +  ","  + ;
                           hbide_nArray2string( oEdit:oEdit:aBookMarks ) +  ","  )
            ENDIF
         ENDIF
      NEXT
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FIND]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aFind )
      aadd( txt_, "find_" + hb_ntos( n ) + "=" + ::aFind[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[REPLACE]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aReplace )
      aadd( txt_, "replace_" + hb_ntos( n ) + "=" + ::aReplace[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTFILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aRecentFiles )
      aadd( txt_, "recentfile_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentFiles[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aRecentProjects )
      aadd( txt_, "recentproject_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentProjects[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FOLDERS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aFolders )
      aadd( txt_, "folder_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aFolders[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[VIEWS]" )
   aadd( txt_, " " )
   FOR EACH s IN ::oDK:getEditorPanelsInfo()
      aadd( txt_, "view_" + hb_ntos( s:__enumIndex() ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TAGGEDPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aTaggedProjects )
      aadd( txt_, "taggedproject_" + hb_ntos( n ) + "=" + ::aTaggedProjects[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TOOLS]" )
   aadd( txt_, " " )
   FOR EACH a_ IN ::aTools
      aadd( txt_, "tool_" + hb_ntos( a_:__enumIndex() ) + "=" + hbide_array2string( a_, "," ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[USERTOOLBARS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aUserToolbars )
      aadd( txt_, "usertoolbars_" + hb_ntos( n ) + "=" + hbide_array2string( ::aUserToolbars[ n ], "," ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[KEYWORDS]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aKeywords )
      aadd( txt_, "keyword_" + hb_ntos( n ) + "=" + hbide_array2string( ::aKeywords[ n ], "~" ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[DBUPANELS]" )
   aadd( txt_, " " )
   FOR EACH s IN ::oBM:getPanelNames()
      aadd( txt_, "dbupanel_" + hb_ntos( s:__enumIndex() ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[DBUPANELSINFO]" )
   aadd( txt_, " " )
   FOR EACH s IN ::oBM:getPanelsInfo()
      aadd( txt_, "dbupanelinfo_" + hb_ntos( s:__enumIndex() ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[APPTHEMES]" )
   aadd( txt_, " " )
   FOR EACH s IN ::aAppThemes
      aadd( txt_, "apptheme_" + hb_ntos( s:__enumIndex() ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[DICTIONARIES]" )
   aadd( txt_, " " )
   FOR EACH s IN ::aDictionaries
      aadd( txt_, "dictionary_" + hb_ntos( s:__enumIndex() ) + "=" + s )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[LOGTITLE]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aLogTitle )
      aadd( txt_, "logtitle_" + hb_ntos( n ) + "=" + ::aLogTitle[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[LOGSOURCES]" )
   aadd( txt_, " " )
   FOR n := 1 TO Len( ::aLogSources )
      aadd( txt_, "logsources_" + hb_ntos( n ) + "=" + ::aLogSources[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[General]" )
   aadd( txt_, " " )

   hbide_createTarget( ::oIde:cProjIni, txt_ )

   RETURN hbide_saveSettings( ::oIde )

/*----------------------------------------------------------------------*/

METHOD IdeINI:load( cHbideIni )
   LOCAL aElem, s, nPart, cKey, cVal, a_

   ::oIde:cProjIni := hbide_getIniPath( cHbideIni )

   IF hb_FileExists( ::oIde:cProjIni )
      aElem := hbide_readSource( ::oIde:cProjIni )

      FOR EACH s IN aElem

         s := alltrim( s )
         IF !empty( s )
            SWITCH Upper( s )

            CASE "[GENERAL]"
               nPart := "INI_GENERAL"
               EXIT
            CASE "[HBIDE]"
               nPart := "INI_HBIDE"
               EXIT
            CASE "[PROJECTS]"
               nPart := "INI_PROJECTS"
               EXIT
            CASE "[FILES]"
               nPart := "INI_FILES"
               EXIT
            CASE "[FIND]"
               nPart := "INI_FIND"
               EXIT
            CASE "[REPLACE]"
               nPart := "INI_REPLACE"
               EXIT
            CASE "[RECENTFILES]"
               nPart := "INI_RECENTFILES"
               EXIT
            CASE "[RECENTPROJECTS]"
               nPart := "INI_RECENTPROJECTS"
               EXIT
            CASE "[FOLDERS]"
               nPart := "INI_FOLDERS"
               EXIT
            CASE "[VIEWS]"
               nPart := "INI_VIEWS"
               EXIT
            CASE "[TAGGEDPROJECTS]"
               nPart := "INI_TAGGEDPROJECTS"
               EXIT
            CASE "[TOOLS]"
               nPart := "INI_TOOLS"
               EXIT
            CASE "[USERTOOLBARS]"
               nPart := "INI_USERTOOLBARS"
               EXIT
            CASE "[KEYWORDS]"
               nPart := "INI_KEYWORDS"
               EXIT
            CASE "[DBUPANELS]"
               nPart := "INI_DBUPANELS"
               EXIT
            CASE "[DBUPANELSINFO]"
               nPart := "INI_DBUPANELSINFO"
               EXIT
            CASE "[APPTHEMES]"
               nPart := "INI_APPTHEMES"
               EXIT
            CASE "[DICTIONARIES]"
               nPart := "INI_DICTIONARIES"
               EXIT
            CASE "[LOGTITLE]"
               nPart := "INI_LOGTITLE"
               EXIT
            CASE "[LOGSOURCES]"
               nPart := "INI_LOGSOURCES"
               EXIT
            OTHERWISE
               DO CASE
               CASE Left( s, 1 ) $ '#['
                  * Nothing todo!

               CASE nPart == "INI_GENERAL"
                  * Qt Setttings, do nothing.

               CASE nPart == "INI_HBIDE"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )

                     SWITCH cKey

                     CASE "MainWindowGeometry"          ; ::cMainWindowGeometry               := cVal ; EXIT
                     CASE "GotoDialogGeometry"          ; ::cGotoDialogGeometry               := cVal ; EXIT
                     CASE "FindDialogGeometry"          ; ::cFindDialogGeometry               := cVal ; EXIT
                     CASE "ToolsDialogGeometry"         ; ::cToolsDialogGeometry              := cVal ; EXIT
                     CASE "SetupDialogGeometry"         ; ::cSetupDialogGeometry              := cVal ; EXIT
                     CASE "ShortcutsDialogGeometry"     ; ::cShortcutsDialogGeometry          := cVal ; EXIT
                     CASE "DbStructDialogGeometry"      ; ::cDbStructDialogGeometry           := cVal ; EXIT
                     CASE "TablesDialogGeometry"        ; ::cTablesDialogGeometry             := cVal ; EXIT
                     CASE "ChangelogDialogGeometry"     ; ::cChangelogDialogGeometry          := cVal ; EXIT
                     CASE "StatsDialogGeometry"         ; ::cStatsDialogGeometry              := cVal ; EXIT
                     //
                     CASE "CurrentLineHighlightMode"    ; ::oIde:lCurrentLineHighlightEnabled := !( cVal == "NO" ); EXIT
                     CASE "LineNumbersDisplayMode"      ; ::oIde:lLineNumbersVisible          := !( cVal == "NO" ); EXIT
                     CASE "HorzRulerDisplayMode"        ; ::oIde:lHorzRulerVisible            := !( cVal == "NO" ); EXIT
                     //
                     CASE "RecentTabIndex"              ; ::cRecentTabIndex                   := cVal ; EXIT
                     //
                     CASE "IdeTheme"                    ; ::cIdeTheme                         := cVal ; EXIT
                     CASE "IdeAnimated"                 ; ::cIdeAnimated                      := cVal ; EXIT
                     //
                     CASE "PathHrbRoot"                 ; ::cPathHrbRoot                      := cVal ; EXIT
                     CASE "PathMk2"                     ; ::cPathHbMk2                        := cVal ; EXIT
                     CASE "PathResources"               ; ::cPathResources                    := cVal ; EXIT
                     CASE "PathTemp"                    ; ::cPathTemp                         := cVal ; EXIT
                     CASE "PathEnv"                     ; ::cPathEnv                          := cVal ; EXIT
                     CASE "PathShortcuts"               ; ::cPathShortcuts                    := cVal ; EXIT
                     CASE "PathSnippets"                ; ::cPathSnippets                     := cVal ; EXIT
                     CASE "PathThemes"                  ; ::cPathThemes                       := cVal ; EXIT
                     //
                     CASE "CurrentProject"              ; ::oIde:cWrkProject                  := cVal ; EXIT
                     CASE "CurrentTheme"                ; ::oIde:cWrkTheme                    := cVal ; EXIT
                     CASE "CurrentCodec"                ; ::oIde:cWrkCodec                    := cVal ; EXIT
                     CASE "CurrentEnvironment"          ; ::oIde:cWrkEnvironment              := cVal ; EXIT
                     CASE "CurrentFind"                 ; ::oIde:cWrkFind                     := cVal ; EXIT
                     CASE "CurrentFolderFind"           ; ::oIde:cWrkFolderFind               := cVal ; EXIT
                     CASE "CurrentReplace"              ; ::oIde:cWrkReplace                  := cVal ; EXIT
                     CASE "CurrentView"                 ; ::oIde:cWrkView                     := cVal ; EXIT
                     CASE "TextFileExtensions"          ; ::cTextFileExtensions               := cVal ; EXIT
                     //
                     CASE "FontName"                    ; ::cFontName                         := cVal ; EXIT
                     CASE "PointSize"                   ; ::nPointSize                        := val( cVal ); EXIT
                     CASE "LineEndingMode"              ; ::cLineEndingMode                   := cVal ; EXIT
                     //
                     CASE "TrimTrailingBlanks"          ; ::lTrimTrailingBlanks               := !( cVal == "NO" ) ; EXIT
                     CASE "SaveSourceWhenComp"          ; ::lSaveSourceWhenComp               := !( cVal == "NO" ) ; EXIT
                     CASE "SupressHbKWordsToUpper"      ; ::lSupressHbKWordsToUpper           := !( cVal == "NO" ) ; EXIT
                     CASE "ReturnAsBeginKeyword"        ; ::lReturnAsBeginKeyword             := !( cVal == "NO" ) ; EXIT
                     CASE "ConvTabToSpcWhenLoading"     ; ::lConvTabToSpcWhenLoading          := !( cVal == "NO" ) ; EXIT
                     CASE "AutoIndent"                  ; ::lAutoIndent                       := !( cVal == "NO" ) ; EXIT
                     CASE "SmartIndent"                 ; ::lSmartIndent                      := !( cVal == "NO" ) ; EXIT
                     CASE "TabToSpcInEdits"             ; ::lTabToSpcInEdits                  := !( cVal == "NO" ) ; EXIT
                     CASE "TabSpaces"                   ; ::oIde:nTabSpaces                   := val( cVal )  ; EXIT
                     CASE "IndentSpaces"                ; ::nIndentSpaces                     := val( cVal )  ; EXIT
                     CASE "TmpBkpPrd"                   ; ::nTmpBkpPrd                        := val( cVal )  ; EXIT
                     CASE "BkpPath"                     ; ::cBkpPath                          := cVal ; EXIT
                     CASE "BkpSuffix"                   ; ::cBkpSuffix                        := cVal ; EXIT
                     CASE "CodeListWithArgs"            ; ::lCompletionWithArgs               := !( cVal == "NO" ) ; EXIT
                     CASE "CompletionWithArgs"          ; ::lCompleteArgumented               := !( cVal == "NO" ) ; EXIT
                     CASE "EditsMdi"                    ; ::lEditsMdi                         := !( cVal == "NO" ) ; EXIT

                     CASE "ShowEditsLeftToolbar"        ; ::lShowEditsLeftToolbar             := !( cVal == "NO" ) ; EXIT
                     CASE "ShowEditsTopToolbar"         ; ::lShowEditsTopToolbar              := !( cVal == "NO" ) ; EXIT
                     CASE "DocksTabShape"               ; ::nDocksTabShape                    := val( cVal )  ; EXIT
                     CASE "DocksLeftTabPos"             ; ::nDocksLeftTabPos                  := val( cVal )  ; EXIT
                     CASE "DocksTopTabPos"              ; ::nDocksTopTabPos                   := val( cVal )  ; EXIT
                     CASE "DocksBottomTabPos"           ; ::nDocksRightTabPos                 := val( cVal )  ; EXIT
                     CASE "DocksRightTabPos"            ; ::nDocksBottomTabPos                := val( cVal )  ; EXIT
                     CASE "ShowHideDocks"               ; ::lShowHideDocks                    := !( cVal == "NO" ) ; EXIT
                     CASE "ChangeLog"                   ; ::cChangeLog                        := cVal ; EXIT
                     CASE "UserChangeLog"               ; ::cUserChangeLog                    := cVal ; EXIT
                     //
                     CASE "VSSExe"                      ; ::cVSSExe                           := cVal ; EXIT
                     CASE "VSSDatabase"                 ; ::cVSSDatabase                      := cVal ; EXIT
                     CASE "EditsViewStyle"              ; ::nEditsViewStyle                   := val( cVal ); EXIT
                     CASE "ToolbarSize"                 ; ::cToolbarSize                      := cVal ; EXIT

                     ENDSWITCH
                  ENDIF

               CASE nPart == "INI_PROJECTS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aProjFiles, cVal )
                  ENDIF

               CASE nPart == "INI_FILES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     a_:= hbide_parseSourceComponents( cVal )
                     IF !Empty( a_[ 1 ] )
                        aadd( ::aFiles, a_ )
                     ENDIF
                  ENDIF

               CASE nPart == "INI_FIND"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aFind, cVal )
                  ENDIF

               CASE nPart == "INI_REPLACE"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aReplace, cVal )
                  ENDIF

               CASE nPart == "INI_RECENTPROJECTS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( ::aRecentProjects ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( ::aRecentProjects, {|e| hb_FileMatch( hbide_pathNormalized( e, .f. ), cVal ) } ) == 0
                           AAdd( ::aRecentProjects, cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == "INI_RECENTFILES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( ::aRecentFiles ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( ::aRecentFiles, {|e| hb_FileMatch( hbide_pathNormalized( e, .f. ), cVal ) } ) == 0
                           AAdd( ::aRecentFiles, cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == "INI_FOLDERS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aFolders, cVal )
                  ENDIF

               CASE nPart == "INI_VIEWS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aViews, cVal )
                  ENDIF

               CASE nPart == "INI_TAGGEDPROJECTS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd(::aTaggedProjects, cVal )
                  ENDIF

               CASE nPart == "INI_TOOLS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aTools, hbide_parseToolComponents( cVal ) )
                  ENDIF

               CASE nPart == "INI_USERTOOLBARS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aUserToolbars, hbide_parseUserToolbarComponents( cVal ) )
                  ENDIF

               CASE nPart == "INI_KEYWORDS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aKeywords, hbide_parseKeywordsComponents( cVal ) )
                  ENDIF

               CASE nPart == "INI_DBUPANELS"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aDbuPanelNames, cVal )
                  ENDIF

               CASE nPart == "INI_DBUPANELSINFO"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aDbuPanelsInfo, cVal )
                  ENDIF

               CASE nPart == "INI_APPTHEMES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aAppThemes, cVal )
                  ENDIF

               CASE nPart == "INI_DICTIONARIES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aDictioaries, cVal )
                  ENDIF

               CASE nPart == "INI_LOGTITLE"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aLogTitle, cVal )
                  ENDIF

               CASE nPart == "INI_LOGSOURCES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     aadd( ::aLogSources, cVal )
                  ENDIF

               ENDCASE
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   ::lEditsMdi := .t.  /* Enabled Permanently - scheduled to be removed by next commit */

   RETURN Self

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveSettings( oIde, cFile )
   LOCAL cPath

   DEFAULT cFile TO "settings.ide"

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_saveSettings( cPath + cFile, "hbidesettings", oIde:oDlg:oWidget )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_restSettings( oIde, cFile )
   LOCAL cPath

   DEFAULT cFile TO "settings.ide"

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_restSettings( cPath + cFile, "hbidesettings", oIde:oDlg:oWidget )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_getEditInfoAsString( oEdit )
   LOCAL qHScr   := oEdit:qEdit:horizontalScrollBar()
   LOCAL qVScr   := oEdit:qEdit:verticalScrollBar()
   LOCAL qCursor := oEdit:qEdit:textCursor()
   LOCAL cBMarks := hbide_nArray2string( oEdit:oEdit:aBookMarks )

   RETURN hbide_pathNormalized( oEdit:sourceFile, .f. ) +  ","  + ;
                          hb_ntos( qCursor:position() ) +  ","  + ;
                          hb_ntos( qHScr:value()      ) +  ","  + ;
                          hb_ntos( qVScr:value()      ) +  ","  + ;
                          oEdit:cTheme                  +  ","  + ;
                          oEdit:cView                   +  ","  + ;
                          cBMarks                       +  ","

/*----------------------------------------------------------------------*/

FUNCTION hbide_getIniPath( cHbideIni )
   LOCAL cPath, cIni

   IF empty( cHbideIni )
      IF ! hb_FileExists( cIni := hb_dirBase() + "hbide.ini" )
      #if defined( __PLATFORM__WINDOWS )
         cPath := hb_DirSepAdd( GetEnv( "APPDATA" ) ) + "hbide\"
      #elif defined( __PLATFORM__UNIX )
         cPath := hb_DirSepAdd( GetEnv( "HOME" ) ) + ".hbide/"
      #elif defined( __PLATFORM__OS2 )
         cPath := hb_DirSepAdd( GetEnv( "HOME" ) ) + ".hbide/"
      #endif
         IF ! hb_dirExists( cPath )
            hb_DirCreate( cPath )
         ENDIF
         cIni := cPath + "hbide.ini"
      ENDIF
   ELSE
      cIni := cHbideIni
   ENDIF

   RETURN cIni

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadSkltns( oIde, cPathSkltns )
   LOCAL s, n, cSkltn, cCode

   IF empty( cPathSkltns )
      cPathSkltns := oIde:oINI:getSnippetsFile()
   ENDIF

   IF hb_fileExists( cPathSkltns )
      s := hb_memoread( cPathSkltns )

      DO WHILE .t.
         IF ( n := at( "<", s ) ) == 0
            EXIT
         ENDIF
         s := substr( s, n + 1 )
         IF ( n := at( ">", s ) ) == 0
            EXIT
         ENDIF
         cSkltn := substr( s, 1, n - 1 )
         s := substr( s, n + 1 )
         IF ( n := at( "</" + cSkltn + ">", s ) ) > 0
            cCode := substr( s, 1, n - 1 )
            cCode := alltrim( cCode )
            IF left( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 2 )
            ENDIF
            IF left( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 2 )
            ENDIF
            IF right( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 1, Len( cCode ) - 1 )
            ENDIF
            IF right( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 1, Len( cCode ) - 1 )
            ENDIF

            aadd( oIde:aSkltns, { cSkltn, cCode } )
            s := substr( s, n + Len( "</" + cSkltn + ">" ) )
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveSkltns( oIde )
   LOCAL a_, txt_:= {}

   FOR EACH a_ IN oIde:aSkltns
      aadd( txt_, "<" + a_[ 1 ] + ">" )
      aeval( hbide_memoToArray( a_[ 2 ] ), {|e| aadd( txt_, e ) } )
      aadd( txt_, "</" + a_[ 1 ] + ">" )
      aadd( txt_, "" )
   NEXT

   RETURN hbide_createTarget( oIde:oINI:getSnippetsFile(), txt_ )

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadShortcuts( oIde, cFileShortcuts )
   LOCAL a_:= {}

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:oINI:getShortcutsFile()
   ENDIF
   IF hb_fileExists( cFileShortcuts )
      a_:= hb_deSerialize( hb_memoread( cFileShortcuts ) )
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveShortcuts( oIde, a_, cFileShortcuts )

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:oINI:getShortcutsFile()
   ENDIF
   hb_memowrit( cFileShortcuts, hb_serialize( a_ ) )

   RETURN hb_fileExists( cFileShortcuts )

/*------------------------------------------------------------------------*/

FUNCTION hbide_loadHarbourProtos( oIde )

   HB_SYMBOL_UNUSED( oIde )

   RETURN NIL //hbide_harbourProtos()

/*------------------------------------------------------------------------*/

FUNCTION hbide_saveHarbourProtos( oIde, aProto )
   LOCAL cFile := hb_dirBase() + "idehbprotos.prg"
   LOCAL txt_  := {}
   LOCAL cTxt  := ""

   HB_SYMBOL_UNUSED( oIde )

   aadd( txt_, "/*"                                                                            )
   aadd( txt_, " * $Id$"                 )
   aadd( txt_, " */"                                                                           )
   aadd( txt_, ""                                                                              )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, "/* WARNING: Automatically generated source file. DO NOT EDIT!           */"    )
   aadd( txt_, "/*          Instead, edit corresponding .qth file,                      */"    )
   aadd( txt_, "/*          or the generator tool itself, and run regenarate.           */"    )
   aadd( txt_, "/* -------------------------------------------------------------------- */"    )
   aadd( txt_, " " )

   aadd( txt_, "" )
   aadd( txt_, "FUNCTION hbide_harbourProtos()" )
   aadd( txt_, "   LOCAL aProto := {}" )
   aadd( txt_, "" )
   aeval( aProto, {|e| aadd( txt_, '   aadd( aProto, "' + strtran( e, '"', "'" ) + '" )' ) } )
   aadd( txt_, "" )
   aadd( txt_, "   RETURN aProto" )
   aadd( txt_, "" )


   aeval( txt_, {|e| cTxt += e + chr( 13 ) + chr( 10 ) } )

   hb_memoWrit( cFile, cTxt )

   RETURN hb_fileExists( cFile )

/*----------------------------------------------------------------------*/
//
//                             Class IdeSetup
//
/*----------------------------------------------------------------------*/

CLASS IdeSetup INHERIT IdeObject

   DATA   oINI
   DATA   qOrgPalette
   DATA   aItems                                  INIT {}
   DATA   aTree                                   INIT { "General", "Selections", "Font", "Paths", "Variables", "Dictionaries", "Themes", "Formatting", "VSS" }
   DATA   aStyles                                 INIT { "cleanlooks", "windows", "windowsxp", ;
                                                         "windowsvista", "cde", "motif", "plastique", "macintosh" }
   DATA   aKeyItems                               INIT {}
   DATA   aDictionaries                           INIT {}

   DATA   nCurThemeSlot                           INIT 0
   DATA   aHilighters                             INIT {}
   DATA   aTBSize                                 INIT { "8","9","10","11","12","13","14","15","16","17","18","19","20" }

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( cEvent, p, p1 )
   METHOD buildTree()
   METHOD setSystemStyle( cStyle )
   METHOD setBaseColor()
   METHOD connectSlots()
   METHOD disConnectSlots()
   METHOD setIcons()
   METHOD populate()
   METHOD retrieve()
   METHOD eol()
   METHOD buildKeywords()
   METHOD populateKeyTableRow( nRow, cTxtCol1, cTxtCol2 )
   METHOD populateThemeColors( nSlot, aRGB )
   METHOD pullThemeColors( nSlot )
   METHOD fetchThemeColorsString( nSlot )
   METHOD pushThemeColors( nTheme )
   METHOD pushThemesData()
   METHOD getThemeData( nTheme )
   METHOD viewIt( cFileName, lSaveAs, lSave, lReadOnly, lApplyHiliter )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSetup:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   ::oINI := ::oIde:oINI
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:destroy()

   IF !empty( ::oUI )
      ::disConnectSlots()
      ::oUI:destroy()
   ENDIF

   ::oINI             := NIL
   ::qOrgPalette      := NIL
   ::aItems           := NIL
   ::aTree            := NIL
   ::aStyles          := NIL
   ::aKeyItems        := NIL
   ::aDictionaries    := NIL
   ::nCurThemeSlot    := NIL
   ::aHilighters      := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:eol()
   RETURN iif( ::oINI:cLineEndingMode == "CRLF", hb_eol(), iif( ::oINI:cLineEndingMode == "CR", chr( 13 ), ;
                                                         iif( ::oINI:cLineEndingMode == "LF", chr( 10 ), hb_eol() ) ) )
/*----------------------------------------------------------------------*/

METHOD IdeSetup:setIcons()

   ::oUI:buttonAddTextExt    : setIcon( QIcon( hbide_image( "dc_plus"   ) ) )
   ::oUI:buttonDelTextExt    : setIcon( QIcon( hbide_image( "dc_delete" ) ) )

   ::oUI:buttonKeyAdd        : setIcon( QIcon( hbide_image( "dc_plus"   ) ) )
   ::oUI:buttonKeyDel        : setIcon( QIcon( hbide_image( "dc_delete" ) ) )
   ::oUI:buttonKeyUp         : setIcon( QIcon( hbide_image( "dc_up"     ) ) )
   ::oUI:buttonKeyDown       : setIcon( QIcon( hbide_image( "dc_down"   ) ) )

   /* Paths */
   ::oUI:buttonPathHrbRoot   : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathHbmk2     : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathEnv       : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathResources : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathTemp      : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathShortcuts : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathSnippets  : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonPathThemes    : setIcon( QIcon( hbide_image( "open"      ) ) )

   ::oUI:buttonViewIni       : setIcon( QIcon( hbide_image( "file-open" ) ) )
   ::oUI:buttonViewEnv       : setIcon( QIcon( hbide_image( "file-open" ) ) )
   ::oUI:buttonViewSnippets  : setIcon( QIcon( hbide_image( "file-open" ) ) )
   ::oUI:buttonViewThemes    : setIcon( QIcon( hbide_image( "file-open" ) ) )

   ::oUI:buttonSelFont       : setIcon( QIcon( hbide_image( "font"      ) ) )

   ::oUI:buttonThmAdd        : setIcon( QIcon( hbide_image( "dc_plus"   ) ) )
   ::oUI:buttonThmDel        : setIcon( QIcon( hbide_image( "dc_delete" ) ) )
   ::oUI:buttonThmApp        : setIcon( QIcon( hbide_image( "copy"      ) ) )
   ::oUI:buttonThmSav        : setIcon( QIcon( hbide_image( "save"      ) ) )

   /* Dictionaries */
   ::oUI:buttonDictPath      : setIcon( QIcon( hbide_image( "open"      ) ) )

   /* VSS */
   ::oUI:buttonVSSExe        : setIcon( QIcon( hbide_image( "open"      ) ) )
   ::oUI:buttonVSSDatabase   : setIcon( QIcon( hbide_image( "open"      ) ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:disConnectSlots()

   ::oUI:buttonAddTextExt    :disconnect( "clicked()"                )
   ::oUI:buttonDelTextExt    :disconnect( "clicked()"                )

   ::oUI:buttonKeyAdd        :disconnect( "clicked()"                )
   ::oUI:buttonKeyDel        :disconnect( "clicked()"                )
   ::oUI:buttonKeyUp         :disconnect( "clicked()"                )
   ::oUI:buttonKeyDown       :disconnect( "clicked()"                )

   ::oUI:tableVar            :disconnect( "itemActivated(QTableWidgetItem*)" )

   ::oUI:buttonSelFont       :disconnect( "clicked()"                )
   ::oUI:buttonClose         :disConnect( "clicked()"                )
   ::oUI:buttonOK            :disConnect( "clicked()"                )
   ::oUI:buttonCancel        :disConnect( "clicked()"                )
   ::oUI:treeWidget          :disConnect( "itemSelectionChanged()"   )
   ::oUI:comboStyle          :disconnect( "currentIndexChanged(int)" )

   ::oUI:checkAnimated       :disconnect( "stateChanged(int)"        )

   ::oUI:checkHilightLine    :disconnect( "stateChanged(int)"        )
   ::oUI:checkHorzRuler      :disconnect( "stateChanged(int)"        )
   ::oUI:checkLineNumbers    :disconnect( "stateChanged(int)"        )
   ::oUI:checkShowLeftToolbar:disconnect( "stateChanged(int)"        )
   ::oUI:checkShowTopToolbar :disconnect( "stateChanged(int)"        )

   ::oUI:sliderRed           :disconnect( "valueChanged(int)"        )
   ::oUI:sliderGreen         :disconnect( "valueChanged(int)"        )
   ::oUI:sliderBlue          :disconnect( "valueChanged(int)"        )

   ::oUI:radioSec1           :disconnect( "clicked()"                )
   ::oUI:radioSec2           :disconnect( "clicked()"                )
   ::oUI:radioSec3           :disconnect( "clicked()"                )
   ::oUI:radioSec4           :disconnect( "clicked()"                )
   ::oUI:radioSec5           :disconnect( "clicked()"                )

   ::oUI:buttonThmAdd        :disconnect( "clicked()"                )
   ::oUI:buttonThmDel        :disconnect( "clicked()"                )
   ::oUI:buttonThmApp        :disconnect( "clicked()"                )
   ::oUI:buttonThmSav        :disconnect( "clicked()"                )

   ::oUI:listThemes          :disconnect( "currentRowChanged(int)"   )

   ::oUI:buttonPathHrbRoot   :disconnect( "clicked()"                )
   ::oUI:buttonPathHbmk2     :disconnect( "clicked()"                )
   ::oUI:buttonPathEnv       :disconnect( "clicked()"                )
   ::oUI:buttonPathResources :disconnect( "clicked()"                )
   ::oUI:buttonPathTemp      :disconnect( "clicked()"                )
   ::oUI:buttonPathShortcuts :disconnect( "clicked()"                )
   ::oUI:buttonPathSnippets  :disconnect( "clicked()"                )
   ::oUI:buttonPathThemes    :disconnect( "clicked()"                )

   ::oUI:buttonViewIni       :disconnect( "clicked()"                )
   ::oUI:buttonViewEnv       :disconnect( "clicked()"                )
   ::oUI:buttonViewSnippets  :disconnect( "clicked()"                )
   ::oUI:buttonViewThemes    :disconnect( "clicked()"                )

   /* Dictionaries */
   ::oUI:buttonDictPath      :disconnect( "clicked()"                )

   ::oUI:comboTabsShape      :disconnect( "currentIndexChanged(int)" )
   ::oUI:comboLeftTabPos     :disconnect( "currentIndexChanged(int)" )
   ::oUI:comboTopTabPos      :disconnect( "currentIndexChanged(int)" )
   ::oUI:comboRightTabPos    :disconnect( "currentIndexChanged(int)" )
   ::oUI:comboBottomTabPos   :disconnect( "currentIndexChanged(int)" )
   ::oUI:comboTBSize         :disconnect( "currentIndexChanged(int)" )

   ::oUI:buttonVSSExe        :disconnect( "clicked()"                )
   ::oUI:buttonVSSDatabase   :disconnect( "clicked()"                )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:connectSlots()

   ::oUI:buttonAddTextExt    :connect( "clicked()"               , {| | ::execEvent( "buttonAddTextext_clicked"          ) } )
   ::oUI:buttonDelTextExt    :connect( "clicked()"               , {| | ::execEvent( "buttonDelTextext_clicked"          ) } )

   ::oUI:buttonKeyAdd        :connect( "clicked()"               , {| | ::execEvent( "buttonKeyAdd_clicked"              ) } )
   ::oUI:buttonKeyDel        :connect( "clicked()"               , {| | ::execEvent( "buttonKeyDel_clicked"              ) } )
   ::oUI:buttonKeyUp         :connect( "clicked()"               , {| | ::execEvent( "buttonKeyUp_clicked"               ) } )
   ::oUI:buttonKeyDown       :connect( "clicked()"               , {| | ::execEvent( "buttonKeyDown_clicked"             ) } )

   ::oUI:tableVar            :connect( "itemActivated(QTableWidgetItem*)", {|p| ::execEvent( "tableVar_keyPress", p              ) } )

   ::oUI:buttonSelFont       :connect( "clicked()"               , {| | ::execEvent( "buttonSelFont_clicked"             ) } )
   ::oUI:buttonClose         :connect( "clicked()"               , {| | ::execEvent( "buttonClose_clicked"               ) } )
   ::oUI:buttonOk            :connect( "clicked()"               , {| | ::execEvent( "buttonOk_clicked"                  ) } )
   ::oUI:buttonCancel        :connect( "clicked()"               , {| | ::execEvent( "buttonCancel_clicked"              ) } )
   ::oUI:treeWidget          :connect( "itemSelectionChanged()"  , {| | ::execEvent( "treeWidget_itemSelectionChanged"   ) } )
   ::oUI:comboStyle          :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboStyle_currentIndexChanged", i ) } )

   ::oUI:checkAnimated       :connect( "stateChanged(int)"       , {|i| ::execEvent( "checkAnimated_stateChanged", i     ) } )

   ::oUI:checkHilightLine    :connect( "stateChanged(int)"       , {|i| ::execEvent( "checkHilightLine_stateChanged", i  ) } )
   ::oUI:checkHorzRuler      :connect( "stateChanged(int)"       , {|i| ::execEvent( "checkHorzRuler_stateChanged"  , i  ) } )
   ::oUI:checkLineNumbers    :connect( "stateChanged(int)"       , {|i| ::execEvent( "checkLineNumbers_stateChanged", i  ) } )
   ::oUI:checkShowLeftToolbar:connect( "stateChanged(int)"       , {|i| ::execEvent( "checkShowLeftToolbar_stateChanged", i  ) } )
   ::oUI:checkShowTopToolbar :connect( "stateChanged(int)"       , {|i| ::execEvent( "checkShowTopToolbar_stateChanged", i  ) } )

   ::oUI:sliderRed           :connect( "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "R"       ) } )
   ::oUI:sliderGreen         :connect( "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "G"       ) } )
   ::oUI:sliderBlue          :connect( "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "B"       ) } )

   ::oUI:radioSec1           :connect( "clicked()"               , {| | ::execEvent( "radioSection_clicked", 1           ) } )
   ::oUI:radioSec2           :connect( "clicked()"               , {| | ::execEvent( "radioSection_clicked", 2           ) } )
   ::oUI:radioSec3           :connect( "clicked()"               , {| | ::execEvent( "radioSection_clicked", 3           ) } )
   ::oUI:radioSec4           :connect( "clicked()"               , {| | ::execEvent( "radioSection_clicked", 4           ) } )
   ::oUI:radioSec5           :connect( "clicked()"               , {| | ::execEvent( "radioSection_clicked", 5           ) } )

   ::oUI:buttonThmAdd        :connect( "clicked()"               , {| | ::execEvent( "buttonThmAdd_clicked"              ) } )
   ::oUI:buttonThmDel        :connect( "clicked()"               , {| | ::execEvent( "buttonThmDel_clicked"              ) } )
   ::oUI:buttonThmApp        :connect( "clicked()"               , {| | ::execEvent( "buttonThmApp_clicked"              ) } )
   ::oUI:buttonThmSav        :connect( "clicked()"               , {| | ::execEvent( "buttonThmSav_clicked"              ) } )

   ::oUI:listThemes          :connect( "currentRowChanged(int)"  , {|i| ::execEvent( "listThemes_currentRowChanged", i   ) } )

   ::oUI:buttonPathHrbRoot   :connect( "clicked()"               , {| | ::execEvent( "buttonHrbRoot_clicked"             ) } )
   ::oUI:buttonPathHbmk2     :connect( "clicked()"               , {| | ::execEvent( "buttonHbmk2_clicked"               ) } )
   ::oUI:buttonPathEnv       :connect( "clicked()"               , {| | ::execEvent( "buttonEnv_clicked"                 ) } )
   ::oUI:buttonPathResources :connect( "clicked()"               , {| | ::execEvent( "buttonResources_clicked"           ) } )
   ::oUI:buttonPathTemp      :connect( "clicked()"               , {| | ::execEvent( "buttonTemp_clicked"                ) } )
   ::oUI:buttonPathShortcuts :connect( "clicked()"               , {| | ::execEvent( "buttonShortcuts_clicked"           ) } )
   ::oUI:buttonPathSnippets  :connect( "clicked()"               , {| | ::execEvent( "buttonSnippets_clicked"            ) } )
   ::oUI:buttonPathThemes    :connect( "clicked()"               , {| | ::execEvent( "buttonThemes_clicked"              ) } )

   ::oUI:buttonViewIni       :connect( "clicked()"               , {| | ::execEvent( "buttonViewIni_clicked"             ) } )
   ::oUI:buttonViewEnv       :connect( "clicked()"               , {| | ::execEvent( "buttonViewEnv_clicked"             ) } )
   ::oUI:buttonViewSnippets  :connect( "clicked()"               , {| | ::execEvent( "buttonViewSnippets_clicked"        ) } )
   ::oUI:buttonViewThemes    :connect( "clicked()"               , {| | ::execEvent( "buttonViewThemes_clicked"          ) } )

   ::oUI:buttonDictPath      :connect( "clicked()"               , {| | ::execEvent( "buttonDictPath_clicked"            ) } )

   ::oUI:comboTabsShape      :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboTabsShape_currentIndexChanged"   , i ) } )
   ::oUI:comboLeftTabPos     :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboLeftTabPos_currentIndexChanged"  , i ) } )
   ::oUI:comboTopTabPos      :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboTopTabPos_currentIndexChanged"   , i ) } )
   ::oUI:comboRightTabPos    :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboRightTabPos_currentIndexChanged" , i ) } )
   ::oUI:comboBottomTabPos   :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboBottomTabPos_currentIndexChanged", i ) } )
   ::oUI:comboTBSize         :connect( "currentIndexChanged(int)", {|i| ::execEvent( "comboTBSize_currentIndexChanged"      , i ) } )

   ::oUI:buttonVSSExe        :connect( "clicked()"               , {| | ::execEvent( "buttonVSSExe_clicked"              ) } )
   ::oUI:buttonVSSDatabase   :connect( "clicked()"               , {| | ::execEvent( "buttonVSSDatabase_clicked"         ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:retrieve()
   LOCAL a_, i, s, qItm

   ::oINI:cLineEndingMode          := iif( ::oUI:radioLineEndCRLF : isChecked(), "CRLF", ;
                                      iif( ::oUI:radioLineEndCR   : isChecked(), "CR"  , ;
                                      iif( ::oUI:radioLineEndLF   : isChecked(), "LF"  , ;
                                      iif( ::oUI:radioLineEndOS   : isChecked(), "OS"  , ;
                                      iif( ::oUI:radioLineEndAuto : isChecked(), "AUTO", "CRLF" ) ) ) ) )

   ::oINI:lTrimTrailingBlanks      := ::oUI:checkTrimTrailingBlanks      : isChecked()
   ::oINI:lSaveSourceWhenComp      := ::oUI:checkSaveSourceWhenComp      : isChecked()
   ::oINI:lSupressHbKWordsToUpper  := ::oUI:checkSupressHbKWordsToUpper  : isChecked()
   ::oINI:lReturnAsBeginKeyword    := ::oUI:checkReturnAsBeginKeyword    : isChecked()
   ::oINI:lConvTabToSpcWhenLoading := ::oUI:checkConvTabToSpcWhenLoading : isChecked()
   ::oINI:lTabToSpcInEdits         := ::oUI:checkTabToSpcInEdits         : isChecked()
   ::oINI:lAutoIndent              := ::oUI:checkAutoIndent              : isChecked()
   ::oINI:lSmartIndent             := ::oUI:checkSmartIndent             : isChecked()
   ::oIde:nTabSpaces               := val( ::oUI:editTabSpaces           : text() )
   ::oINI:nIndentSpaces            := val( ::oUI:editIndentSpaces        : text() )
   ::oINI:lEditsMdi                := ::oUI:checkEditsMdi                : isChecked()

   ::oINI:aKeywords := {}
   FOR EACH a_ IN ::aKeyItems
      aadd( ::oINI:aKeywords, { alltrim( ::aKeyItems[ a_:__enumIndex(),1 ]:text() ), alltrim( ::aKeyItems[ a_:__enumIndex(),2 ]:text() ) } )
   NEXT

   s := ""
   FOR i := 1 TO ::oUI:listTextExt:count()
      qItm := ::oUI:listTextExt:item( i - 1 )
      s += "." + qItm:text() + ","
   NEXT
   s := substr( s, 1, Len( s ) - 1 )
   ::oINI:cTextFileExtensions := s

   ::oINI:nTmpBkpPrd               := val( ::oUI:editTmpBkpPrd : text() )
   ::oINI:cBkpPath                 := ::oUI:editBkpPath        : text()
   ::oINI:cBkpSuffix               := ::oUI:editBkpSuffix      : text()
   ::oINI:lCompletionWithArgs      := ::oUI:checkListlWithArgs : isChecked()
   ::oINI:lCompleteArgumented      := ::oUI:checkCmplInclArgs  : isChecked()

   /* Paths */
   ::oINI:cPathHrbRoot             := ::oUI:editPathHrbRoot    : text()
   ::oINI:cPathHbMk2               := ::oUI:editPathHbMk2      : text()
   ::oINI:cPathResources           := ::oUI:editPathResources  : text()
   ::oINI:cPathTemp                := ::oUI:editPathTemp       : text()
   ::oINI:cPathEnv                 := ::oUI:editPathEnv        : text()
   ::oINI:cPathShortcuts           := ::oUI:editPathShortcuts  : text()
   ::oINI:cPathSnippets            := ::oUI:editPathSnippets   : text()
   ::oINI:cPathThemes              := ::oUI:editPathThemes     : text()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:populate()
   LOCAL s, a_

   ::disconnectSlots()

   ::oUI:checkAnimated                : setChecked( val( ::oINI:cIdeAnimated ) > 0      )

   ::oUI:checkHilightLine             : setChecked( ::oIde:lCurrentLineHighlightEnabled )
   ::oUI:checkHorzRuler               : setChecked( ::oIde:lHorzRulerVisible            )
   ::oUI:checkLineNumbers             : setChecked( ::oIde:lLineNumbersVisible          )
   ::oUI:checkShowLeftToolbar         : setChecked( ::oINI:lShowEditsLeftToolbar        )
   ::oUI:checkShowTopToolbar          : setChecked( ::oINI:lShowEditsTopToolbar         )

   /* Line Ending Mode */
   s := ::oINI:cLineEndingMode
   //
   ::oUI:radioLineEndCRLF             : setChecked( s == "CRLF" .OR. empty( s )         )
   ::oUI:radioLineEndCR               : setChecked( s == "CR"                           )
   ::oUI:radioLineEndLF               : setChecked( s == "LF"                           )
   ::oUI:radioLineEndOS               : setChecked( s == "OS"                           )
   ::oUI:radioLineEndAuto             : setChecked( s == "AUTO"                         )

   ::oUI:checkTrimTrailingBlanks      : setChecked( ::oINI:lTrimTrailingBlanks          )
   ::oUI:checkSaveSourceWhenComp      : setChecked( ::oINI:lSaveSourceWhenComp          )
   ::oUI:checkSupressHbKWordsToUpper  : setChecked( ::oINI:lSupressHbKWordsToUpper      )
   ::oUI:checkReturnAsBeginKeyword    : setChecked( ::oINI:lReturnAsBeginKeyword        )
   ::oUI:checkConvTabToSpcWhenLoading : setChecked( ::oINI:lConvTabToSpcWhenLoading     )
   ::oUI:checkTabToSpcInEdits         : setChecked( ::oINI:lTabToSpcInEdits             )
   ::oUI:checkAutoIndent              : setChecked( ::oINI:lAutoIndent                  )
   ::oUI:checkSmartIndent             : setChecked( ::oINI:lSmartIndent                 )
   ::oUI:editTabSpaces                : setText( hb_ntos( ::oIde:nTabSpaces    )        )
   ::oUI:editIndentSpaces             : setText( hb_ntos( ::oINI:nIndentSpaces )        )
   ::oUI:checkEditsMdi                : setChecked( ::oINI:lEditsMdi                    )

   /* Paths */
   ::oUI:editPathIni                  : setText( ::oIde:cProjIni                        )
   //
   ::oUI:editPathHrbRoot              : setText( ::oINI:cPathHrbRoot                    )
   ::oUI:editPathHbMk2                : setText( ::oINI:cPathHbMk2                      )
   ::oUI:editPathResources            : setText( ::oINI:cPathResources                  )
   ::oUI:editPathTemp                 : setText( ::oINI:cPathTemp                       )
   ::oUI:editPathEnv                  : setText( ::oINI:cPathEnv                        )
   ::oUI:editPathShortcuts            : setText( ::oINI:cPathShortcuts                  )
   ::oUI:editPathSnippets             : setText( ::oINI:cPathSnippets                   )
   ::oUI:editPathThemes               : setText( ::oINI:cPathThemes                     )

   /* Variables */
   ::oUI:tableVar:clearContents()
   ::aKeyItems := {}
   FOR EACH a_ IN ::oINI:aKeywords
      ::populateKeyTableRow( a_:__enumIndex(), a_[ 1 ], a_[ 2 ] )
   NEXT

   ::oUI:listTextExt:clear()
   a_:= hb_atokens( ::oINI:cTextFileExtensions, ",." )
   FOR EACH s IN a_
      ::oUI:listTextExt:addItem( strtran( s, "." ) )
   NEXT
   ::oUI:listTextExt:setSortingEnabled( .t. )
   ::oUI:listTextExt:sortItems()

   ::oUI:editTmpBkpPrd      : setText( hb_ntos( ::oINI:nTmpBkpPrd ) )
   ::oUI:editBkpPath        : setText( ::oINI:cBkpPath   )
   ::oUI:editBkpSuffix      : setText( ::oINI:cBkpSuffix )

   /* Selections - Code Completion */
   ::oUI:checkListlWithArgs : setChecked( ::oINI:lCompletionWithArgs )
   ::oUI:checkCmplInclArgs  : setChecked( ::oINI:lCompleteArgumented )

   /* Themes */
   ::oUI:sliderRed:setMinimum( 0 )
   ::oUI:sliderRed:setMaximum( 255 )

   ::oUI:sliderGreen:setMinimum( 0 )
   ::oUI:sliderGreen:setMaximum( 255 )

   ::oUI:sliderBlue:setMinimum( 0 )
   ::oUI:sliderBlue:setMaximum( 255 )

   ::oUI:editSec1:setText( "0" )
   ::oUI:editSec5:setText( "1" )

   ::oUI:editSec1:setReadOnly( .t. )
   ::oUI:editSec5:setReadOnly( .t. )

   /* Dock Widgets */
   ::oUI:comboTabsShape:setCurrentIndex( ::oINI:nDocksTabShape )
   ::oUI:comboLeftTabPos:setCurrentIndex( ::oINI:nDocksLeftTabPos )
   ::oUI:comboTopTabPos:setCurrentIndex( ::oINI:nDocksTopTabPos )
   ::oUI:comboRightTabPos:setCurrentIndex( ::oINI:nDocksRightTabPos )
   ::oUI:comboBottomTabPos:setCurrentIndex( ::oINI:nDocksBottomTabPos )

   ::oUI:editVSSExe:setText( ::oINI:cVSSExe )
   ::oUI:editVSSDatabase:setText( ::oINI:cVSSDatabase )

   ::connectSlots()

   ::pushThemesData()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:show()
   LOCAL cStyle

   IF empty( ::oUI )

      ::oUI := hbide_getUI( "setup", ::oDlg:oWidget )

      ::oUI:setWindowFlags( Qt_Sheet )

      ::oUI:setMaximumWidth( ::oUI:width() )
      ::oUI:setMinimumWidth( ::oUI:width() )
      ::oUI:setMaximumHeight( ::oUI:height() )
      ::oUI:setMinimumHeight( ::oUI:height() )

      ::buildTree()
      ::buildKeywords()

      /* Dock Widgets */
      ::oUI:comboTabsShape:addItem( "Rounded" )
      ::oUI:comboTabsShape:addItem( "Triangular" )

      ::oUI:comboLeftTabPos:addItem( "Top"    )
      ::oUI:comboLeftTabPos:addItem( "Bottom" )
      ::oUI:comboLeftTabPos:addItem( "Left"   )
      ::oUI:comboLeftTabPos:addItem( "Right"  )

      ::oUI:comboTopTabPos:addItem( "Top"    )
      ::oUI:comboTopTabPos:addItem( "Bottom" )
      ::oUI:comboTopTabPos:addItem( "Left"   )
      ::oUI:comboTopTabPos:addItem( "Right"  )

      ::oUI:comboBottomTabPos:addItem( "Top"    )
      ::oUI:comboBottomTabPos:addItem( "Bottom" )
      ::oUI:comboBottomTabPos:addItem( "Left"   )
      ::oUI:comboBottomTabPos:addItem( "Right"  )

      ::oUI:comboRightTabPos:addItem( "Top"    )
      ::oUI:comboRightTabPos:addItem( "Bottom" )
      ::oUI:comboRightTabPos:addItem( "Left"   )
      ::oUI:comboRightTabPos:addItem( "Right"  )

      ::oUI:editFontName:setText( ::oINI:cFontName )
      ::oUI:editPointSize:setText( hb_ntos( ::oINI:nPointSize ) )

      FOR EACH cStyle IN ::aStyles
         ::oUI:comboStyle:addItem( cStyle )
      NEXT
      ::oUI:comboStyle:setCurrentIndex( ascan( ::aStyles, {|e| e == ::oINI:cIdeTheme } ) - 1 )

      aeval( ::aTBSize, {|e| ::oUI:comboTBSize:addItem( e ) } )
      ::oUI:comboTBSize:setCurrentIndex( ascan( ::aTBSize, {|e| e == ::oINI:cToolbarSize } ) - 1 )

      ::setIcons()
      ::connectSlots()

      ::oUI:stackedWidget:setCurrentIndex( 0 )
      ::oUI:hide()
   ENDIF

   ::populate()
   ::oIde:setPosByIniEx( ::oUI:oWidget, ::oINI:cSetupDialogGeometry )
   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:execEvent( cEvent, p, p1 )
   LOCAL qItem, nIndex, qFontDlg, qFont, nOK, nRow, b_, q0, q1, nCol, w0, w1
   LOCAL aRGB, nSlot, qFrame, aGrad, n, cCSS, cTheme, cPath, cBuffer

   HB_SYMBOL_UNUSED( p1 )

   IF ::lQuitting
      RETURN Self
   ENDIF

   SWITCH cEvent

   CASE "buttonSelFont_clicked"
      qFont := QFont( ::oINI:cFontName, ::oINI:nPointSize )
      qFont:setFixedPitch( .t. )
      qFontDlg := QFontDialog( ::oUI:oWidget )
      qFontDlg:setCurrentFont( qFont )
      nOK := qFontDlg:exec()
      IF nOK == 1
         qFont := qFontDlg:currentFont()

         ::oUI:editFontName:setText( qFont:family() )
         ::oUI:editPointSize:setText( hb_ntos( qFont:pointSize() ) )

         ::oINI:cFontName  := ::oUI:editFontName:text()
         ::oINI:nPointSize := val( ::oUI:editPointSize:text() )
      ENDIF
      EXIT

   CASE "checkAnimated_stateChanged"
      ::oDK:animateComponents( iif( p == 0, 0, 1 ) )
      EXIT

   CASE "checkHilightLine_stateChanged"
      ::oEM:toggleCurrentLineHighlightMode()
      EXIT

   CASE "checkHorzRuler_stateChanged"
      ::oEM:toggleHorzRuler()
      EXIT

   CASE "checkLineNumbers_stateChanged"
      ::oEM:toggleLineNumbers()
      EXIT

   CASE "checkShowTopToolbar_stateChanged"
      IF ::oDK:qMdiToolbar:oWidget:isVisible()
         ::oDK:qMdiToolbar:hide()
      ELSE
         ::oDK:qMdiToolbar:show()
      ENDIF
      ::oINI:lShowEditsTopToolbar := ::oDK:qMdiToolbar:oWidget:isVisible()
      EXIT
   CASE "checkShowLeftToolbar_stateChanged"
      IF ::oDK:qMdiToolbarL:oWidget:isVisible()
         ::oDK:qMdiToolbarL:hide()
      ELSE
         ::oDK:qMdiToolbarL:show()
      ENDIF
      ::oINI:lShowEditsLeftToolbar := ::oDK:qMdiToolbarL:oWidget:isVisible()
      EXIT

   CASE "treeWidget_itemSelectionChanged"
      qItem  := ::oUI:treeWidget:currentItem()
      IF ( nIndex := ascan( ::aTree, qItem:text( 0 ) ) ) > 0
         ::oUI:stackedWidget:setCurrentIndex( nIndex - 1 )
      ENDIF
      EXIT

   CASE "buttonCancel_clicked"
      ::oIde:oINI:cSetupDialogGeometry := hbide_posAndSize( ::oUI:oWidget )
      ::oUI:done( 0 )
      EXIT

   CASE "buttonClose_clicked"
   CASE "buttonOk_clicked"
      ::oIde:oINI:cSetupDialogGeometry := hbide_posAndSize( ::oUI:oWidget )
      ::retrieve()
      ::oUI:done( 1 )
      EXIT

   CASE "comboStyle_currentIndexChanged"
      IF ( nIndex := ::oUI:comboStyle:currentIndex() ) > -1
         ::oINI:cIdeTheme := ::aStyles[ nIndex + 1 ]
         ::setSystemStyle( ::aStyles[ nIndex + 1 ] )
      ENDIF
      EXIT

   CASE "buttonAddTextext_clicked"
      q0 := hbide_fetchAString( ::oUI, "", "Text File Extension" )
      IF !empty( q0 )
         ::oUI:listTextExt:addItem( lower( strtran( q0, "." ) ) )
      ENDIF
      EXIT

   CASE "buttonDelTextext_clicked"
      IF ::oUI:listTextExt:currentRow() >= 0
         ::oUI:listTextExt:takeItem( ::oUI:listTextExt:currentRow() )
      ENDIF
      EXIT

   CASE "buttonKeyAdd_clicked"
      ::populateKeyTableRow( Len( ::aKeyItems ) + 1, "", "" )
      ::oUI:tableVar:setCurrentItem( ::aKeyItems[ Len( ::aKeyItems ), 1 ] )
      EXIT

   CASE "buttonKeyDel_clicked"
      IF ( nRow := ::oUI:tableVar:currentRow() ) >= 0
         ::oUI:tableVar:removeRow( nRow )
         hb_adel( ::aKeyItems     , nRow + 1, .t. )
         hb_adel( ::oINI:aKeywords, nRow + 1, .t. )
      ENDIF
      EXIT

   CASE "buttonKeyUp_clicked"
      IF ( nRow := ::oUI:tableVar:currentRow() ) >= 1
         nCol := ::oUI:tableVar:currentColumn()

         b_ := ::aKeyItems[ nRow+1 ]
         q0 := QTableWidgetItem(); q0:setText( b_[ 1 ]:text() )
         q1 := QTableWidgetItem(); q1:setText( b_[ 2 ]:text() )

         b_ := ::aKeyItems[ nRow+0 ]
         w0 := QTableWidgetItem(); w0:setText( b_[ 1 ]:text() )
         w1 := QTableWidgetItem(); w1:setText( b_[ 2 ]:text() )

         ::oUI:tableVar:setItem( nRow-0, 0, w0 )
         ::oUI:tableVar:setItem( nRow-0, 1, w1 )

         ::oUI:tableVar:setItem( nRow-1, 0, q0 )
         ::oUI:tableVar:setItem( nRow-1, 1, q1 )

         ::aKeyItems[ nRow+1 ] := { w0,w1 }
         ::aKeyItems[ nRow+0 ] := { q0,q1 }

         ::oUI:tableVar:setCurrentItem( iif( nCol == 0, q0, q1 ) )
      ENDIF
      EXIT

   CASE "buttonKeyDown_clicked"
      nRow := ::oUI:tableVar:currentRow()
      IF nRow >= 0 .AND. nRow + 1 < Len( ::aKeyItems )

         nCol := ::oUI:tableVar:currentColumn()

         b_ := ::aKeyItems[ nRow + 1 ]
         q0 := QTableWidgetItem(); q0:setText( b_[ 1 ]:text() )
         q1 := QTableWidgetItem(); q1:setText( b_[ 2 ]:text() )

         b_ := ::aKeyItems[ nRow + 2 ]
         w0 := QTableWidgetItem(); w0:setText( b_[ 1 ]:text() )
         w1 := QTableWidgetItem(); w1:setText( b_[ 2 ]:text() )

         ::oUI:tableVar:setItem( nRow, 0, w0 )
         ::oUI:tableVar:setItem( nRow, 1, w1 )

         ::oUI:tableVar:setItem( nRow+1, 0, q0 )
         ::oUI:tableVar:setItem( nRow+1, 1, q1 )

         ::aKeyItems[ nRow + 1 ] := { w0,w1 }
         ::aKeyItems[ nRow + 2 ] := { q0,q1 }

         ::oUI:tableVar:setCurrentItem( iif( nCol == 0, q0, q1 ) )
      ENDIF
      EXIT

   CASE "tableVar_keyPress"
      IF ( nRow := ::oUI:tableVar:currentRow() ) >= 0
         HB_TRACE( HB_TR_DEBUG, "RECEIVING ENTER KEY" )
         ::oUI:tableVar:editItem( p )
         HB_SYMBOL_UNUSED( nRow )
         #if 0
         IF ::oUI:tableVar:currentColumn() == 0
            ::oUI:tableVar:setCurrentCell( ::oUI:tableVar:currentRow(), 1 )
         ENDIF
         #endif
      ENDIF

   CASE "radioSection_clicked"
      ::nCurThemeSlot := p
      IF empty( aRGB := ::pullThemeColors( p ) )
         aRGB := { 0,0,0 }
      ENDIF
      ::oUI:sliderRed   : setValue( aRGB[ 1 ] )
      ::oUI:sliderGreen : setValue( aRGB[ 2 ] )
      ::oUI:sliderBlue  : setValue( aRGB[ 3 ] )
      EXIT

   CASE "sliderValue_changed"
      nSlot := ::nCurThemeSlot

      IF nSlot > 0
         qFrame := { ::oUI:frameSec1, ::oUI:frameSec2, ::oUI:frameSec3, ::oUI:frameSec4, ::oUI:frameSec5 }[ nSlot ]
         aRGB   := { ::oUI:sliderRed:value(), ::oUI:sliderGreen:value(), ::oUI:sliderBlue:value() }
         ::populateThemeColors( nSlot, aRGB )
         qFrame:setStyleSheet( "background-color: " + hbide_rgbString( aRGB ) + ";" )
      ENDIF

      aGrad := {}
      FOR nSlot := 1 TO 5
         n  := val( { ::oUI:editSec1, ::oUI:editSec2, ::oUI:editSec3, ::oUI:editSec4, ::oUI:editSec5 }[ nSlot ]:text() )

         IF !empty( aRGB := ::pullThemeColors( nSlot ) )
            aadd( aGrad, { n, aRGB[ 1 ], aRGB[ 2 ], aRGB[ 3 ] } )
         ENDIF
      NEXT
      IF !empty( aGrad )
         cCSS := 'background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0, ' + hbide_buildGradientString( aGrad ) + ");"
         ::oUI:frameHorz:setStyleSheet( cCSS )
         cCSS := 'background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, ' + hbide_buildGradientString( aGrad ) + ");"
         ::oUI:frameVert:setStyleSheet( cCSS )
      ENDIF
      EXIT

   CASE "listThemes_currentRowChanged"
      ::pushThemeColors( p + 1 )
      EXIT
   CASE "buttonThmAdd_clicked"
      IF !empty( cTheme := hbide_fetchAString( ::oDlg:oWidget, cTheme, "Name the Theme", "New Theme" ) )
         aadd( ::oINI:aAppThemes, cTheme + "," + ::fetchThemeColorsString() )
         qItem := QListWidgetItem()
         qItem:setText( cTheme )
         //::oUI:listThemes:addItem_1( qItem )
         ::oUI:listThemes:addItem( qItem )
         ::oUI:listThemes:setCurrentRow( Len( ::oINI:aAppThemes ) - 1 )
      ENDIF
      EXIT
   CASE "buttonThmApp_clicked"
      IF ( n := ::oUI:listThemes:currentRow() ) > -1
         hbide_setAppTheme( ::getThemeData( n + 1 ) )
         ::oDK:animateComponents( HBIDE_ANIMATION_GRADIENT )
      ENDIF
      EXIT
   CASE "buttonThmDel_clicked"
      EXIT
   CASE "buttonThmSav_clicked"
      IF ( n := ::oUI:listThemes:currentRow() ) > -1
         ::oINI:aAppThemes[ n + 1 ] := ::oUI:listThemes:currentItem():text() + "," + ;
                                       ::fetchThemeColorsString()
      ENDIF
      EXIT

   CASE "buttonIni_clicked"
      EXIT
   CASE "buttonVSSExe_clicked"
      IF ! empty( cPath := hbide_fetchADir( ::oDlg, "Visual SourceSafe Installation Path", ::oINI:cVSSExe ) )
         ::oINI:cVSSExe := cPath
         ::oUI:editVSSExe:setText( hbide_pathStripLastSlash( cPath ) )
      ENDIF
      EXIT
   CASE "buttonVSSDatabase_clicked"
      IF ! empty( cPath := hbide_fetchADir( ::oDlg, "Visual SourceSafe Database Path", ::oINI:cVSSDatabase ) )
         ::oINI:cVSSDatabase := cPath
         ::oUI:editVSSDatabase:setText( hbide_pathStripLastSlash( cPath ) )
      ENDIF
      EXIT
   CASE "buttonHrbRoot_clicked"
      IF ! empty( cPath := hbide_fetchADir( ::oDlg, "Harbour's Root Path", ::oINI:cPathHrbRoot ) )
         ::oINI:cPathHrbRoot := cPath
         ::oUI:editPathHrbRoot:setText( hbide_pathStripLastSlash( cPath ) )
      ENDIF
      EXIT
   CASE "buttonHbmk2_clicked"
      IF !empty( cPath := hbide_fetchAFile( ::oDlg, "Location of hbmk2", ;
                                                       { { "Harbour Make - hbmk2", "*" } }, ::oINI:cPathHbMk2 ) )
         ::oINI:cPathhbMk2 := cPath
         ::oUI:editPathHbMk2:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonEnv_clicked"
      IF !empty( cPath := hbide_fetchAFile( ::oDlg, "Environment Definitions File ( .env )", ;
                                                       { { "Environment Files", "*.env" } }, ::oINI:getEnvFile() ) )
         ::oINI:cPathEnv := cPath
         ::oUI:editPathEnv:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonResources_clicked"
      IF ! empty( cPath := hbide_fetchADir( ::oDlg, "Location of Resources ( Plugins, Dialogs, Images, Scripts )", ::oINI:getResourcesPath() ) )
         ::oINI:cPathResources := cPath
         ::oUI:editPathResources:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonTemp_clicked"
      IF ! empty( cPath := hbide_fetchADir( ::oDlg, "Location for Temporary and Transitory Files", ::oINI:getTempPath() ) )
         ::oINI:cPathTemp := cPath
         ::oUI:editPathTemp:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonShortcuts_clicked"
      IF !empty( cPath := hbide_fetchAFile( ::oDlg, "Keyboard Mapping Definitions File ( .scu )", ;
                                                       { { "Keyboard Mappings", "*.scu" } }, ::oINI:getShortcutsFile() ) )
         ::oINI:cPathShortcuts := cPath
         ::oUI:editPathShortcuts:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonSnippets_clicked"
      IF !empty( cPath := hbide_fetchAFile( ::oDlg, "Code Snippets File ( .skl )", ;
                                                       { { "Code Snippets", "*.skl" } }, ::oINI:getSnippetsFile() ) )
         ::oINI:cPathSnippets := cPath
         ::oUI:editPathSnippets:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonThemes_clicked"
      IF !empty( cPath := hbide_fetchAFile( ::oDlg, "Syntax Highlighting Theme File ( .hbt )", ;
                                                       { { "Syntax Theme", "*.hbt" } }, ::oINI:getThemesFile() ) )
         ::oINI:cPathThemes := cPath
         ::oUI:editPathThemes:setText( cPath )
      ENDIF
      EXIT
   CASE "buttonViewIni_clicked"
      ::viewIt( ::oINI:getIniFile(), .t., .f., .f., .f. ) /* FileName, shouldSaveAs, shouldSave, shouldReadOnly, applyHiliter */
      EXIT
   CASE "buttonViewEnv_clicked"
      ::viewIt( ::oINI:getEnvFile(), .t., .t., .f., .f. )
      EXIT
   CASE "buttonViewSnippets_clicked"
      ::viewIt( ::oINI:getSnippetsFile(), .t., .t., .f., .t. )
      EXIT
   CASE "buttonViewThemes_clicked"
      ::viewIt( ::oINI:getThemesFile(), .t., .t., .f., .f. )
      EXIT

   CASE "buttonEditorSaveAs_clicked"
      IF ! empty( cBuffer := p:plainText:toPlainText() )
         IF ! empty( cPath := hbide_saveAFile( ::oDlg, "Save: " + p1, NIL, p1 ) )
            hb_memowrit( cPath, cBuffer )
         ENDIF
      ENDIF
      EXIT
   CASE "buttonEditorSave_clicked"
      IF ! empty( cBuffer := p:plainText:toPlainText() )
         hb_memowrit( p1, cBuffer )
      ENDIF
      EXIT
   CASE "buttonEditorClose_clicked"
   CASE "buttonEditorX_clicked"
      p:oWidget:disconnect( QEvent_Close )
      p:buttonSaveAs:disconnect( "clicked()"  )
      p:buttonSave:disconnect( "clicked()"  )
      p:buttonClose:disconnect( "clicked()"  )

      p:close()
      p := NIL   /* Must Destroy It */
      EXIT

   /* Docking Widgets */
   CASE "comboTabsShape_currentIndexChanged"
      ::oINI:nDocksTabShape := p
      ::oDlg:setTabShape( ::oINI:nDocksTabShape )
      EXIT
   CASE "comboLeftTabPos_currentIndexChanged"
      ::oINI:nDocksLeftTabPos := p
      ::oDlg:setTabPosition( Qt_LeftDockWidgetArea  , ::oINI:nDocksLeftTabPos   )
      EXIT
   CASE "comboTopTabPos_currentIndexChanged"
      ::oINI:nDocksTopTabPos := p
      ::oDlg:setTabPosition( Qt_TopDockWidgetArea   , ::oINI:nDocksTopTabPos    )
      EXIT
   CASE "comboRightTabPos_currentIndexChanged"
      ::oINI:nDocksRightTabPos := p
      ::oDlg:setTabPosition( Qt_RightDockWidgetArea , ::oINI:nDocksRightTabPos  )
      EXIT
   CASE "comboBottomTabPos_currentIndexChanged"
      ::oINI:nDocksBottomTabPos := p
      ::oDlg:setTabPosition( Qt_BottomDockWidgetArea, ::oINI:nDocksBottomTabPos )
      EXIT

   CASE "comboTBSize_currentIndexChanged"
      ::oINI:cToolbarSize := ::oUI:comboTBSize:currentText()
      ::oDK:setToolbarSize( val( ::oINI:cToolbarSize ) )
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:viewIt( cFileName, lSaveAs, lSave, lReadOnly, lApplyHiliter )
   LOCAL oUI

   oUI := hbide_getUI( "editor", ::oUI:oWidget )
   oUI:setWindowFlags( Qt_Sheet + Qt_CustomizeWindowHint + Qt_WindowTitleHint + Qt_WindowContextHelpButtonHint )

   oUI:plainText:setReadOnly( lReadOnly )
   oUI:buttonSaveAs:setEnabled( lSaveAs )
   oUI:buttonSave:setEnabled( lSave )

   oUI:plainText:setLineWrapMode( QPlainTextEdit_NoWrap )

   oUI:plainText:setPlainText( hb_memoRead( cFileName ) )
   oUI:plainText:setFont( ::oIde:oFont:oWidget )
   IF lApplyHiliter
      aadd( ::aHilighters, ::oTH:setSyntaxHilighting( oUI:plainText, "Bare Minimum" ) )
   ENDIF

   oUI:oWidget       :connect( QEvent_Close, {|| ::execEvent( "buttonEditorX_clicked"     , oUI            ) } )
   oUI:buttonSaveAs:connect( "clicked()" , {|| ::execEvent( "buttonEditorSaveAs_clicked", oUI, cFileName ) } )
   oUI:buttonSave  :connect( "clicked()" , {|| ::execEvent( "buttonEditorSave_clicked"  , oUI, cFileName ) } )
   oUI:buttonClose :connect( "clicked()" , {|| ::execEvent( "buttonEditorClose_clicked" , oUI            ) } )

   oUI:show()

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:pushThemesData()
   LOCAL s, a_, qItem

   IF ::nCurThemeSlot == 0
      FOR EACH s IN ::oINI:aAppThemes
         a_:= hb_aTokens( s, "," )
         qItem := QListWidgetItem()
         qItem:setText( a_[ 1 ] )
         //::oUI:listThemes:addItem_1( qItem )
         ::oUI:listThemes:addItem( qItem )
         ::pushThemeColors( s:__enumIndex() )
      NEXT
   ENDIF
   IF !empty( ::oINI:aAppThemes )
      ::oUI:listThemes:setCurrentRow( -1 )
      ::oUI:listThemes:setCurrentRow( Len( ::oINI:aAppThemes ) - 1 )
      ::oUI:listThemes:setCurrentRow( 0 )
   ENDIF
   ::oUI:radioSec1:click()

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:getThemeData( nTheme )
   LOCAL a_, i, aTheme := {}

   IF nTheme >= 1 .AND. nTheme <= Len( ::oINI:aAppThemes )
      a_:= hbide_parseThemeComponent( ::oINI:aAppThemes[ nTheme ] )

      FOR i := 2 TO 6
         IF !empty( a_[ i ] )
            aadd( aTheme, a_[ i ] )
         ENDIF
      NEXT
   ENDIF

   RETURN aTheme

/*----------------------------------------------------------------------*/

METHOD IdeSetup:pushThemeColors( nTheme )
   LOCAL n, a_, i, aRGB, nSlot

   IF nTheme >= 1 .AND. nTheme <= Len( ::oINI:aAppThemes )
      a_:= hb_aTokens( ::oINI:aAppThemes[ nTheme ], "," )
      aSize( a_, 6 )
      DEFAULT a_[ 1 ] TO ""
      DEFAULT a_[ 2 ] TO ""
      DEFAULT a_[ 3 ] TO ""
      DEFAULT a_[ 4 ] TO ""
      DEFAULT a_[ 5 ] TO ""
      DEFAULT a_[ 6 ] TO ""

      FOR i := 2 TO 6
         nSlot := i - 1
         IF !empty( a_[ i ] )
            aRGB := hb_aTokens( a_[ i ], " " )
            FOR EACH n IN aRGB
               n := val( n )
            NEXT
            { ::oUI:editSec1, ::oUI:editSec2, ::oUI:editSec3, ::oUI:editSec4, ::oUI:editSec5 }[ nSlot ]:setText( hb_ntos( aRGB[ 1 ] ) )

            ::populateThemeColors( nSlot, { aRGB[ 2 ], aRGB[ 3 ], aRGB[ 4 ] } )
         ENDIF
      NEXT
      { ::oUI:radioSec1, ::oUI:radioSec2, ::oUI:radioSec3, ::oUI:radioSec4, ::oUI:radioSec5 }[ nSlot ]:click()
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:populateThemeColors( nSlot, aRGB )
   LOCAL qFrame

   { ::oUI:editR1, ::oUI:editR2, ::oUI:editR3, ::oUI:editR4, ::oUI:editR5 }[ nSlot ]:setText( hb_ntos( aRGB[ 1 ] ) )
   { ::oUI:editG1, ::oUI:editG2, ::oUI:editG3, ::oUI:editG4, ::oUI:editG5 }[ nSlot ]:setText( hb_ntos( aRGB[ 2 ] ) )
   { ::oUI:editB1, ::oUI:editB2, ::oUI:editB3, ::oUI:editB4, ::oUI:editB5 }[ nSlot ]:setText( hb_ntos( aRGB[ 3 ] ) )

   qFrame := { ::oUI:frameSec1, ::oUI:frameSec2, ::oUI:frameSec3, ::oUI:frameSec4, ::oUI:frameSec5 }[ nSlot ]
   qFrame:setStyleSheet( "background-color: " + hbide_rgbString( aRGB ) + ";" )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:fetchThemeColorsString( nSlot )
   LOCAL s := ""

   IF empty( nSlot )
      FOR nSlot := 1 TO 5
         s += { ::oUI:editSec1, ::oUI:editSec2, ::oUI:editSec3, ::oUI:editSec4, ::oUI:editSec5 }[ nSlot ]:text() + " "

         s += { ::oUI:editR1, ::oUI:editR2, ::oUI:editR3, ::oUI:editR4, ::oUI:editR5 }[ nSlot ]:text() + " "
         s += { ::oUI:editG1, ::oUI:editG2, ::oUI:editG3, ::oUI:editG4, ::oUI:editG5 }[ nSlot ]:text() + " "
         s += { ::oUI:editB1, ::oUI:editB2, ::oUI:editB3, ::oUI:editB4, ::oUI:editB5 }[ nSlot ]:text()

         s += ","
      NEXT
   ELSE

   ENDIF

   RETURN s

/*------------------------------------------------------------------------*/

METHOD IdeSetup:pullThemeColors( nSlot )
   LOCAL aRGB := {}

   IF !empty( { ::oUI:editSec1, ::oUI:editSec2, ::oUI:editSec3, ::oUI:editSec4, ::oUI:editSec5 }[ nSlot ]:text() )
      aadd( aRGB, val( { ::oUI:editR1, ::oUI:editR2, ::oUI:editR3, ::oUI:editR4, ::oUI:editR5 }[ nSlot ]:text() ) )
      aadd( aRGB, val( { ::oUI:editG1, ::oUI:editG2, ::oUI:editG3, ::oUI:editG4, ::oUI:editG5 }[ nSlot ]:text() ) )
      aadd( aRGB, val( { ::oUI:editB1, ::oUI:editB2, ::oUI:editB3, ::oUI:editB4, ::oUI:editB5 }[ nSlot ]:text() ) )
   ENDIF

   RETURN aRGB

/*------------------------------------------------------------------------*/

METHOD IdeSetup:populateKeyTableRow( nRow, cTxtCol1, cTxtCol2 )
   LOCAL lAppend := Len( ::aKeyItems ) < nRow
   LOCAL q0, q1

   IF lAppend
      ::oUI:tableVar:setRowCount( nRow )

      q0 := QTableWidgetItem()
      q0:setText( cTxtCol1 )
      ::oUI:tableVar:setItem( nRow-1, 0, q0 )

      q1 := QTableWidgetItem()
      q1:setText( cTxtCol2 )
      ::oUI:tableVar:setItem( nRow-1, 1, q1 )

      aadd( ::aKeyItems, { q0, q1 } )

      ::oUI:tableVar:setRowHeight( nRow-1, 16 )

   ELSE
      ::aKeyItems[ nRow, 1 ]:setText( cTxtCol1 )
      ::aKeyItems[ nRow, 2 ]:setText( cTxtCol2 )

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:buildKeywords()
   LOCAL hdr_:= { { "Keyword", 100 }, { "Value", 230 } }
   LOCAL oTbl, n, qItm

   oTbl := ::oUI:tableVar

   oTbl:verticalHeader():hide()
   oTbl:horizontalHeader():setStretchLastSection( .t. )

   oTbl:setAlternatingRowColors( .t. )
   oTbl:setColumnCount( Len( hdr_ ) )
   oTbl:setShowGrid( .t. )
   FOR n := 1 TO Len( hdr_ )
      qItm := QTableWidgetItem()
      qItm:setText( hdr_[ n,1 ] )
      oTbl:setHorizontalHeaderItem( n-1, qItm )
      oTbl:setColumnWidth( n-1, hdr_[ n,2 ] )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:buildTree()
   LOCAL oRoot, oChild, s

   ::oUI:treeWidget:setHeaderHidden( .t. )
   ::oUI:treeWidget:setIconSize( QSize( 12,12 ) )
   ::oUI:treeWidget:setIndentation( 12 )

   oRoot := QTreeWidgetItem()
   oRoot:setText( 0, "Parts" )
   oRoot:setToolTip( 0, "Parts" )

   ::oUI:treeWidget:addTopLevelItem( oRoot )

   aadd( ::aItems, oRoot )

   FOR EACH s IN ::aTree
      oChild := QTreeWidgetItem()
      oChild:setText( 0, s )
      oChild:setToolTip( 0, s )
      oRoot:addChild( oChild )
      aadd( ::aItems, oChild )
   NEXT

   oRoot:setExpanded( .t. )
   ::oUI:treeWidget:setCurrentItem( ::aItems[ 2 ] ) /* General */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setSystemStyle( cStyle )
   LOCAL qFactory

   IF !empty( cStyle )
      qFactory := QStyleFactory()
      QApplication():setStyle( qFactory:create( cStyle ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setBaseColor()
   #if 0
   LOCAL qPalette, oApp, qBrush, qColor

   oApp := QApplication()

   ::qOrgPalette := oApp:palette()

   qColor := QColor( Qt_red )
   qBrush := QBrush( qColor )

   qPalette := oApp:palette()
   qPalette:setBrush( QPalette_Window, qBrush )
   qPalette:setColor( QPalette_Window, qColor )
   qPalette:setColor( QPalette_Base, qColor )

   oApp:setPalette( qPalette )
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

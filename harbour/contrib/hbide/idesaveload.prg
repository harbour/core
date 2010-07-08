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
#include "hbqt.ch"

/*----------------------------------------------------------------------*/
#if 0
#define INI_HBIDE                                 1
#define INI_PROJECTS                              2
#define INI_FILES                                 3
#define INI_FIND                                  4
#define INI_REPLACE                               5
#define INI_RECENTFILES                           6
#define INI_RECENTPROJECTS                        7
#define INI_FOLDERS                               8
#define INI_VIEWS                                 9
#define INI_TAGGEDPROJECTS                        10
#define INI_GENERAL                               11
#define INI_TOOLS                                 12
#define INI_USERTOOLBARS                          13
#define INI_KEYWORDS                              14
#endif

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
   //
   DATA   cRecentTabIndex                         INIT  ""
   //
   DATA   cIdeTheme                               INIT  ""
   DATA   cIdeAnimated                            INIT  ""
   //
   DATA   cPathMk2                                INIT  ""
   DATA   cPathEnv                                INIT  ""
   // /* Not used yet but planned for future */
   DATA   cCurrentProject                         INIT  ""
   DATA   cCurrentTheme                           INIT  ""
   DATA   cCurrentCodec                           INIT  ""
   DATA   cCurrentEnvironment                     INIT  ""
   DATA   cCurrentFind                            INIT  ""
   DATA   cCurrentFolderFind                      INIT  ""
   DATA   cCurrentReplace                         INIT  ""
   DATA   cCurrentView                            INIT  ""
   DATA   cCurrentHarbour                         INIT  ""
   DATA   cCurrentShortcuts                       INIT  ""
   //
   DATA   cTextFileExtensions                     INIT  ".c,.cpp,.prg,.h,.ch,.txt,.log,.ini,.env,.ppo,.qtp,.hbs," + ;
                                                        ".cc,.hbc,.hbp,.hbm,.xml,.bat,.sh,.rc,.ui,.uic,.bak,.fmg,.qth"
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

   DATA   cFontName                               INIT  "Courier New"
   DATA   nPointSize                              INIT  10
   DATA   cLineEndingMode                         INIT  ""

   DATA   lTrimTrailingBlanks                     INIT  .t.
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

   DATA   lCompletionWithArgs                     INIT  .f.
   DATA   lCompleteArgumented                     INIT  .f.

   DATA   aAppThemes                              INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD load( cHbideIni )
   METHOD save( cHbideIni )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeINI:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeINI:save( cHbideIni )
   LOCAL j, nTab, pTab, n, txt_, oEdit, nTabs, nn, a_, s

   DEFAULT cHbideIni TO ::oIde:cProjIni

   IF ::oIde:nRunMode != HBIDE_RUN_MODE_INI
      RETURN Nil
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
   //
   aadd( txt_, "CurrentLineHighlightMode"  + "=" +   iif( ::lCurrentLineHighlightEnabled, "YES", "NO" ) )
   aadd( txt_, "LineNumbersDisplayMode"    + "=" +   iif( ::lLineNumbersVisible, "YES", "NO" )          )
   aadd( txt_, "HorzRulerDisplayMode"      + "=" +   iif( ::lHorzRulerVisible, "YES", "NO" )            )
   //
   aadd( txt_, "RecentTabIndex"            + "=" +   hb_ntos( ::qTabWidget:currentIndex() )             )
   //
   aadd( txt_, "IdeTheme"                  + "=" +   ::cIdeTheme                                        )
   aadd( txt_, "IdeAnimated"               + "=" +   ::cIdeAnimated                                     )

   aadd( txt_, "PathMk2"                   + "=" +   ::cPathMk2                                         )
   aadd( txt_, "PathEnv"                   + "=" +   ::cPathEnv                                         )
   //
   aadd( txt_, "CurrentProject"            + "=" +   ::oIde:cWrkProject                                 )
   aadd( txt_, "CurrentTheme"              + "=" +   ::oIde:cWrkTheme                                   )
   aadd( txt_, "CurrentCodec"              + "=" +   ::oIde:cWrkCodec                                   )
   aadd( txt_, "CurrentEnvironment"        + "=" +   ::oIde:cWrkEnvironment                             )
   aadd( txt_, "CurrentFind"               + "=" +   ::oIde:cWrkFind                                    )
   aadd( txt_, "CurrentFolderFind"         + "=" +   ::oIde:cWrkFolderFind                              )
   aadd( txt_, "CurrentReplace"            + "=" +   ::oIde:cWrkReplace                                 )
   aadd( txt_, "CurrentView"               + "=" +   ::oIde:cWrkView                                    )
   aadd( txt_, "CurrentHarbour"            + "=" +   ::oIde:cWrkHarbour                                 )
   aadd( txt_, "CurrentShortcuts"          + "=" +   ::oIde:cPathShortcuts                              )
   aadd( txt_, "TextFileExtensions"        + "=" +   ::oINI:cTextFileExtensions                         )
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

   aadd( txt_, "[PROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::oIde:aProjects )
      aadd( txt_, "project_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::oIde:aProjects[ n, 2 ], .f. ) )
   NEXT
   aadd( txt_, " " )

   /*-------------------   FILES   -------------------*/
   aadd( txt_, "[FILES]" )
   aadd( txt_, " " )
   nn := 0
   FOR j := 2 TO len( ::oIde:aViews )
      ::oIde:lClosing := .t.
      ::oDK:setView( ::oIde:aViews[ j ]:oWidget:objectName() )

      nTabs := ::oIde:qTabWidget:count()
      FOR n := 1 TO nTabs
         pTab  := ::oIde:qTabWidget:widget( n - 1 )
         nTab  := ascan( ::oIde:aTabs, {|e_| hbqt_IsEqualGcQtPointer( e_[ 1 ]:oWidget:pPtr, pTab ) } )
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
   FOR n := 1 TO len( ::aFind )
      aadd( txt_, "find_" + hb_ntos( n ) + "=" + ::aFind[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[REPLACE]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aReplace )
      aadd( txt_, "replace_" + hb_ntos( n ) + "=" + ::aReplace[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTFILES]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aRecentFiles )
      aadd( txt_, "recentfile_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentFiles[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[RECENTPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aRecentProjects )
      aadd( txt_, "recentproject_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aRecentProjects[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[FOLDERS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aFolders )
      aadd( txt_, "folder_" + hb_ntos( n ) + "=" + hbide_pathNormalized( ::aFolders[ n ], .f. ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[VIEWS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aViews )
      aadd( txt_, "view_" + hb_ntos( n ) + "=" + ::aViews[ n ] )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[TAGGEDPROJECTS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aTaggedProjects )
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
   FOR n := 1 TO len( ::aUserToolbars )
      aadd( txt_, "usertoolbars_" + hb_ntos( n ) + "=" + hbide_array2string( ::aUserToolbars[ n ], "," ) )
   NEXT
   aadd( txt_, " " )

   aadd( txt_, "[KEYWORDS]" )
   aadd( txt_, " " )
   FOR n := 1 TO len( ::aKeywords )
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
                     //
                     CASE "CurrentLineHighlightMode"    ; ::oIde:lCurrentLineHighlightEnabled := ( cVal != "NO" ); EXIT
                     CASE "LineNumbersDisplayMode"      ; ::oIde:lLineNumbersVisible          := ( cVal != "NO" ); EXIT
                     CASE "HorzRulerDisplayMode"        ; ::oIde:lHorzRulerVisible            := ( cVal != "NO" ); EXIT
                     //
                     CASE "RecentTabIndex"              ; ::cRecentTabIndex                   := cVal ; EXIT
                     //
                     CASE "IdeTheme"                    ; ::cIdeTheme                         := cVal ; EXIT
                     CASE "IdeAnimated"                 ; ::cIdeAnimated                      := cVal ; EXIT
                     //          /* Subject to be identified under this object only */
                     CASE "PathMk2"                     ; ::cPathMk2                          := cVal ; EXIT
                     CASE "PathEnv"                     ; ::cPathEnv                          := cVal ; EXIT
                     //
                     CASE "CurrentProject"              ; ::oIde:cWrkProject                  := cVal ; EXIT
                     CASE "CurrentTheme"                ; ::oIde:cWrkTheme                    := cVal ; EXIT
                     CASE "CurrentCodec"                ; ::oIde:cWrkCodec                    := cVal ; EXIT
                     CASE "CurrentEnvironment"          ; ::oIde:cWrkEnvironment              := cVal ; EXIT
                     CASE "CurrentFind"                 ; ::oIde:cWrkFind                     := cVal ; EXIT
                     CASE "CurrentFolderFind"           ; ::oIde:cWrkFolderFind               := cVal ; EXIT
                     CASE "CurrentReplace"              ; ::oIde:cWrkReplace                  := cVal ; EXIT
                     CASE "CurrentView"                 ; ::oIde:cWrkView                     := cVal ; EXIT
                     CASE "CurrentHarbour"              ; ::oIde:cWrkHarbour                  := cVal ; EXIT
                     CASE "CurrentShortcuts"            ; ::oIde:cPathShortcuts               := cVal ; EXIT
                     CASE "TextFileExtensions"          ; ::oINI:cTextFileExtensions          := cVal ; EXIT
                     //
                     CASE "FontName"                    ; ::cFontName                         := cVal ; EXIT
                     CASE "PointSize"                   ; ::nPointSize                        := val( cVal ); EXIT
                     CASE "LineEndingMode"              ; ::cLineEndingMode                   := cVal ; EXIT
                     //
                     CASE "TrimTrailingBlanks"          ; ::oINI:lTrimTrailingBlanks          := cVal != "NO" ; EXIT
                     CASE "SaveSourceWhenComp"          ; ::oINI:lSaveSourceWhenComp          := cVal != "NO" ; EXIT
                     CASE "SupressHbKWordsToUpper"      ; ::oINI:lSupressHbKWordsToUpper      := cVal != "NO" ; EXIT
                     CASE "ReturnAsBeginKeyword"        ; ::oINI:lReturnAsBeginKeyword        := cVal != "NO" ; EXIT
                     CASE "ConvTabToSpcWhenLoading"     ; ::oINI:lConvTabToSpcWhenLoading     := cVal != "NO" ; EXIT
                     CASE "AutoIndent"                  ; ::oINI:lAutoIndent                  := cVal != "NO" ; EXIT
                     CASE "SmartIndent"                 ; ::oINI:lSmartIndent                 := cVal != "NO" ; EXIT
                     CASE "TabToSpcInEdits"             ; ::oINI:lTabToSpcInEdits             := cVal != "NO" ; EXIT
                     CASE "TabSpaces"                   ; ::oIde:nTabSpaces                   := val( cVal )  ; EXIT
                     CASE "IndentSpaces"                ; ::oINI:nIndentSpaces                := val( cVal )  ; EXIT
                     CASE "TmpBkpPrd"                   ; ::oINI:nTmpBkpPrd                   := val( cVal )  ; EXIT
                     CASE "BkpPath"                     ; ::oINI:cBkpPath                     := cVal ; EXIT
                     CASE "BkpSuffix"                   ; ::oINI:cBkpSuffix                   := cVal ; EXIT
                     CASE "CodeListWithArgs"            ; ::oINI:lCompletionWithArgs          := cVal != "NO" ; EXIT
                     CASE "CompletionWithArgs"          ; ::oINI:lCompleteArgumented          := cVal != "NO" ; EXIT

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
                        IF aScan( ::aRecentProjects, {|e| hbide_pathNormalized( e, .f. ) == cVal } ) == 0
                           AAdd( ::aRecentProjects, cVal )
                        ENDIF
                     ENDIF
                  ENDIF

               CASE nPart == "INI_RECENTFILES"
                  IF hbide_parseKeyValPair( s, @cKey, @cVal )
                     IF Len( ::aRecentFiles ) < 25
                        cVal := hbide_pathNormalized( cVal, .f. )
                        IF aScan( ::aRecentFiles, {|e| hbide_pathNormalized( e, .f. ) == cVal } ) == 0
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

               ENDCASE
               EXIT
            ENDSWITCH
         ENDIF
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_saveSettings( oIde )
   LOCAL cPath

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_saveSettings( cPath + "settings.ide", "hbidesettings", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_restSettings( oIde )
   LOCAL cPath

   hb_fNameSplit( oIde:cProjIni, @cPath )
   hbqt_QMainWindow_restSettings( cPath + "settings.ide", "hbidesettings", oIde:oDlg:oWidget:pPtr )

   RETURN nil

/*----------------------------------------------------------------------*/

FUNCTION hbide_getEditInfoAsString( oEdit )
   LOCAL qHScr   := QScrollBar():configure( oEdit:qEdit:horizontalScrollBar() )
   LOCAL qVScr   := QScrollBar():configure( oEdit:qEdit:verticalScrollBar() )
   LOCAL qCursor := QTextCursor():configure( oEdit:qEdit:textCursor() )
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
         cPath := hbide_DirAddPathSep( GetEnv( "APPDATA" ) ) + "hbide\"
      #elif defined( __PLATFORM__UNIX )
         cPath := hbide_DirAddPathSep( GetEnv( "HOME" ) ) + ".hbide/"
      #elif defined( __PLATFORM__OS2 )
         cPath := hbide_DirAddPathSep( GetEnv( "HOME" ) ) + ".hbide/"
      #endif
         IF ! hb_dirExists( cPath )
            MakeDir( cPath )
         ENDIF
         cIni := cPath + "hbide.ini"
      ENDIF
   ELSE
      cIni := cHbideIni
   ENDIF

   RETURN cIni

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadSkltns( oIde, cPathSkltns )
   LOCAL cPath, s, n, cSkltn, cCode

   IF empty( cPathSkltns )
      hb_fNameSplit( oIde:cProjIni, @cPath )
      cPath += "hbide.skl"

      IF hb_fileExists( cPath )
         cPathSkltns := cPath
      ELSE
         cPathSkltns := hb_dirBase() + "hbide.skl"
      ENDIF
   ENDIF
   oIde:cPathSkltns := cPathSkltns

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
               cCode := substr( cCode, 1, len( cCode ) - 1 )
            ENDIF
            IF right( cCode, 1 ) $ chr( 13 ) + chr( 10 )
               cCode := substr( cCode, 1, len( cCode ) - 1 )
            ENDIF

            aadd( oIde:aSkltns, { cSkltn, cCode } )
            s := substr( s, n + len( "</" + cSkltn + ">" ) )
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

   RETURN hbide_createTarget( oIde:cPathSkltns, txt_ )

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadShortcuts( oIde, cFileShortcuts )
   LOCAL cPath, a_:= {}

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:cPathShortcuts
      IF empty( cFileShortcuts )
         cFileShortcuts := oIde:cProjIni
      ENDIF

      hb_fNameSplit( cFileShortcuts, @cPath )
      cPath += "hbide.scu"

      IF hb_fileExists( cPath )
         cFileShortcuts := cPath
      ELSE
         cFileShortcuts := hb_dirBase() + "hbide.scu"
      ENDIF
   ENDIF
   oIde:cPathShortcuts := cFileShortcuts

   IF hb_fileExists( cFileShortcuts )
      a_:= hb_deSerialize( hb_memoread( cFileShortcuts ) )
   ENDIF

   RETURN a_

/*----------------------------------------------------------------------*/

FUNCTION hbide_saveShortcuts( oIde, a_, cFileShortcuts )
   LOCAL cPath

   IF empty( cFileShortcuts )
      cFileShortcuts := oIde:cPathShortcuts
      IF empty( cFileShortcuts )
         cFileShortcuts := oIde:cProjIni
      ENDIF

      hb_fNameSplit( cFileShortcuts, @cPath )
      cPath += "hbide.scu"

      IF hb_fileExists( cPath )
         cFileShortcuts := cPath
      ELSE
         cFileShortcuts := hb_dirBase() + "hbide.scu"
      ENDIF
   ENDIF

   hb_memowrit( cFileShortcuts, hb_serialize( a_ ) )

   RETURN hb_fileExists( cFileShortcuts )

/*----------------------------------------------------------------------*/
//
//                             Class IdeSetup
//
/*----------------------------------------------------------------------*/

CLASS IdeSetup INHERIT IdeObject

   DATA   oINI
   DATA   qOrgPalette
   DATA   aItems                                  INIT {}
   DATA   aTree                                   INIT { "General", "Selections", "Font", "Paths", "Variables", "Dictionaries", "Themes" }
   DATA   aStyles                                 INIT { "cleanlooks", "windows", "windowsxp", ;
                                                         "windowsvista", "cde", "motif", "plastique", "macintosh" }
   DATA   aKeyItems                               INIT {}

   DATA   nCurThemeSlot                           INIT 0

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

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:eol()
   RETURN iif( ::oINI:cLineEndingMode == "CRLF", CRLF, iif( ::oINI:cLineEndingMode == "CR", chr( 13 ), ;
                                                         iif( ::oINI:cLineEndingMode == "LF", chr( 10 ), CRLF ) ) )
/*----------------------------------------------------------------------*/

METHOD IdeSetup:setIcons()

   ::oUI:q_buttonAddTextExt    : setIcon( hbide_image( "dc_plus"   ) )
   ::oUI:q_buttonDelTextExt    : setIcon( hbide_image( "dc_delete" ) )

   ::oUI:q_buttonKeyAdd        : setIcon( hbide_image( "dc_plus"   ) )
   ::oUI:q_buttonKeyDel        : setIcon( hbide_image( "dc_delete" ) )
   ::oUI:q_buttonKeyUp         : setIcon( hbide_image( "dc_up"     ) )
   ::oUI:q_buttonKeyDown       : setIcon( hbide_image( "dc_down"   ) )

   ::oUI:q_buttonPathIni       : setIcon( hbide_image( "open"      ) )
   ::oUI:q_buttonPathHbmk2     : setIcon( hbide_image( "open"      ) )
   ::oUI:q_buttonPathSnippets  : setIcon( hbide_image( "open"      ) )
   ::oUI:q_buttonPathEnv       : setIcon( hbide_image( "open"      ) )
   ::oUI:q_buttonPathShortcuts : setIcon( hbide_image( "open"      ) )
   ::oUI:q_buttonPathThemes    : setIcon( hbide_image( "open"      ) )

   ::oUI:q_buttonSelFont       : setIcon( hbide_image( "font"      ) )

   ::oUI:q_buttonThmAdd        : setIcon( hbide_image( "dc_plus"   ) )
   ::oUI:q_buttonThmDel        : setIcon( hbide_image( "dc_delete" ) )
   ::oUI:q_buttonThmApp        : setIcon( hbide_image( "copy"      ) )
   ::oUI:q_buttonThmSav        : setIcon( hbide_image( "save"      ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:disConnectSlots()

   ::disconnect( ::oUI:q_buttonAddTextExt, "clicked()"                )
   ::disconnect( ::oUI:q_buttonDelTextExt, "clicked()"                )

   ::disconnect( ::oUI:q_buttonKeyAdd    , "clicked()"                )
   ::disconnect( ::oUI:q_buttonKeyDel    , "clicked()"                )
   ::disconnect( ::oUI:q_buttonKeyUp     , "clicked()"                )
   ::disconnect( ::oUI:q_buttonKeyDown   , "clicked()"                )

   ::disconnect( ::oUI:q_tableVar        , "itemActivated(QTblWItem)" )

   ::disconnect( ::oUI:q_buttonSelFont   , "clicked()"                )
   ::disConnect( ::oUI:q_buttonClose     , "clicked()"                )
   ::disConnect( ::oUI:q_buttonOK        , "clicked()"                )
   ::disConnect( ::oUI:q_buttonCancel    , "clicked()"                )
   ::disConnect( ::oUI:q_treeWidget      , "itemSelectionChanged()"   )
   ::disconnect( ::oUI:q_comboStyle      , "currentIndexChanged(int)" )

   ::disconnect( ::oUI:q_checkAnimated   , "stateChanged(int)"        )

   ::disconnect( ::oUI:q_checkHilightLine, "stateChanged(int)"        )
   ::disconnect( ::oUI:q_checkHorzRuler  , "stateChanged(int)"        )
   ::disconnect( ::oUI:q_checkLineNumbers, "stateChanged(int)"        )

   ::disconnect( ::oUI:q_sliderRed       , "valueChanged(int)"        )
   ::disconnect( ::oUI:q_sliderGreen     , "valueChanged(int)"        )
   ::disconnect( ::oUI:q_sliderBlue      , "valueChanged(int)"        )

   ::disconnect( ::oUI:q_radioSec1       , "clicked()"                )
   ::disconnect( ::oUI:q_radioSec2       , "clicked()"                )
   ::disconnect( ::oUI:q_radioSec3       , "clicked()"                )
   ::disconnect( ::oUI:q_radioSec4       , "clicked()"                )
   ::disconnect( ::oUI:q_radioSec5       , "clicked()"                )

   ::disconnect( ::oUI:q_buttonThmAdd    , "clicked()"                )
   ::disconnect( ::oUI:q_buttonThmDel    , "clicked()"                )
   ::disconnect( ::oUI:q_buttonThmApp    , "clicked()"                )
   ::disconnect( ::oUI:q_buttonThmSav    , "clicked()"                )

   ::disconnect( ::oUI:q_listThemes      , "currentRowChanged(int)"   )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:connectSlots()

   ::connect( ::oUI:q_buttonAddTextExt, "clicked()"               , {| | ::execEvent( "buttonAddTextext_clicked"          ) } )
   ::connect( ::oUI:q_buttonDelTextExt, "clicked()"               , {| | ::execEvent( "buttonDelTextext_clicked"          ) } )

   ::connect( ::oUI:q_buttonKeyAdd    , "clicked()"               , {| | ::execEvent( "buttonKeyAdd_clicked"              ) } )
   ::connect( ::oUI:q_buttonKeyDel    , "clicked()"               , {| | ::execEvent( "buttonKeyDel_clicked"              ) } )
   ::connect( ::oUI:q_buttonKeyUp     , "clicked()"               , {| | ::execEvent( "buttonKeyUp_clicked"               ) } )
   ::connect( ::oUI:q_buttonKeyDown   , "clicked()"               , {| | ::execEvent( "buttonKeyDown_clicked"             ) } )

   ::connect( ::oUI:q_tableVar        , "itemActivated(QTblWItem)", {|p| ::execEvent( "tableVar_keyPress", p              ) } )

   ::connect( ::oUI:q_buttonSelFont   , "clicked()"               , {| | ::execEvent( "buttonSelFont_clicked"             ) } )
   ::connect( ::oUI:q_buttonClose     , "clicked()"               , {| | ::execEvent( "buttonClose_clicked"               ) } )
   ::connect( ::oUI:q_buttonOk        , "clicked()"               , {| | ::execEvent( "buttonOk_clicked"                  ) } )
   ::connect( ::oUI:q_buttonCancel    , "clicked()"               , {| | ::execEvent( "buttonCancel_clicked"              ) } )
   ::connect( ::oUI:q_treeWidget      , "itemSelectionChanged()"  , {| | ::execEvent( "treeWidget_itemSelectionChanged"   ) } )
   ::connect( ::oUI:q_comboStyle      , "currentIndexChanged(int)", {|i| ::execEvent( "comboStyle_currentIndexChanged", i ) } )

   ::connect( ::oUI:q_checkAnimated   , "stateChanged(int)"       , {|i| ::execEvent( "checkAnimated_stateChanged", i     ) } )

   ::connect( ::oUI:q_checkHilightLine, "stateChanged(int)"       , {|i| ::execEvent( "checkHilightLine_stateChanged", i  ) } )
   ::connect( ::oUI:q_checkHorzRuler  , "stateChanged(int)"       , {|i| ::execEvent( "checkHorzRuler_stateChanged"  , i  ) } )
   ::connect( ::oUI:q_checkLineNumbers, "stateChanged(int)"       , {|i| ::execEvent( "checkLineNumbers_stateChanged", i  ) } )

   ::connect( ::oUI:q_sliderRed       , "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "R"       ) } )
   ::connect( ::oUI:q_sliderGreen     , "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "G"       ) } )
   ::connect( ::oUI:q_sliderBlue      , "valueChanged(int)"       , {|i| ::execEvent( "sliderValue_changed", i, "B"       ) } )

   ::connect( ::oUI:q_radioSec1       , "clicked()"               , {| | ::execEvent( "radioSection_clicked", 1           ) } )
   ::connect( ::oUI:q_radioSec2       , "clicked()"               , {| | ::execEvent( "radioSection_clicked", 2           ) } )
   ::connect( ::oUI:q_radioSec3       , "clicked()"               , {| | ::execEvent( "radioSection_clicked", 3           ) } )
   ::connect( ::oUI:q_radioSec4       , "clicked()"               , {| | ::execEvent( "radioSection_clicked", 4           ) } )
   ::connect( ::oUI:q_radioSec5       , "clicked()"               , {| | ::execEvent( "radioSection_clicked", 5           ) } )

   ::connect( ::oUI:q_buttonThmAdd    , "clicked()"               , {| | ::execEvent( "buttonThmAdd_clicked"              ) } )
   ::connect( ::oUI:q_buttonThmDel    , "clicked()"               , {| | ::execEvent( "buttonThmDel_clicked"              ) } )
   ::connect( ::oUI:q_buttonThmApp    , "clicked()"               , {| | ::execEvent( "buttonThmApp_clicked"              ) } )
   ::connect( ::oUI:q_buttonThmSav    , "clicked()"               , {| | ::execEvent( "buttonThmSav_clicked"              ) } )

   ::connect( ::oUI:q_listThemes      , "currentRowChanged(int)"  , {|i| ::execEvent( "listThemes_currentRowChanged", i   ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:retrieve()
   LOCAL a_, i, s, qItm

   ::oINI:cLineEndingMode := iif( ::oUI:q_radioLineEndCRLF : isChecked(), "CRLF", ;
                             iif( ::oUI:q_radioLineEndCR   : isChecked(), "CR"  , ;
                             iif( ::oUI:q_radioLineEndLF   : isChecked(), "LF"  , ;
                             iif( ::oUI:q_radioLineEndOS   : isChecked(), "OS"  , ;
                             iif( ::oUI:q_radioLineEndAuto : isChecked(), "AUTO", "CRLF" ) ) ) ) )

   ::oINI:lTrimTrailingBlanks      := ::oUI:q_checkTrimTrailingBlanks      : isChecked()
   ::oINI:lSaveSourceWhenComp      := ::oUI:q_checkSaveSourceWhenComp      : isChecked()
   ::oINI:lSupressHbKWordsToUpper  := ::oUI:q_checkSupressHbKWordsToUpper  : isChecked()
   ::oINI:lReturnAsBeginKeyword    := ::oUI:q_checkReturnAsBeginKeyword    : isChecked()
   ::oINI:lConvTabToSpcWhenLoading := ::oUI:q_checkConvTabToSpcWhenLoading : isChecked()
   ::oINI:lTabToSpcInEdits         := ::oUI:q_checkTabToSpcInEdits         : isChecked()
   ::oINI:lAutoIndent              := ::oUI:q_checkAutoIndent              : isChecked()
   ::oINI:lSmartIndent             := ::oUI:q_checkSmartIndent             : isChecked()
   ::oIde:nTabSpaces               := val( ::oUI:q_editTabSpaces           : text() )
   ::oINI:nIndentSpaces            := val( ::oUI:q_editIndentSpaces        : text() )

   ::oINI:aKeywords := {}
   FOR EACH a_ IN ::aKeyItems
      aadd( ::oINI:aKeywords, { alltrim( ::aKeyItems[ a_:__enumIndex(),1 ]:text() ), alltrim( ::aKeyItems[ a_:__enumIndex(),2 ]:text() ) } )
   NEXT

   s := ""
   FOR i := 1 TO ::oUI:q_listTextExt:count()
      qItm := QListWidgetItem():from( ::oUI:q_listTextExt:item( i - 1 ) )
      s += "." + qItm:text() + ","
   NEXT
   s := substr( s, 1, len( s ) - 1 )
   ::oINI:cTextFileExtensions := s

   ::oINI:nTmpBkpPrd          := val( ::oUI:q_editTmpBkpPrd : text() )
   ::oINI:cBkpPath            := ::oUI:q_editBkpPath        : text()
   ::oINI:cBkpSuffix          := ::oUI:q_editBkpSuffix      : text()
   ::oINI:lCompletionWithArgs := ::oUI:q_checkListlWithArgs : isChecked()
   ::oINI:lCompleteArgumented := ::oUI:q_checkCmplInclArgs  : isChecked()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:populate()
   LOCAL s, a_

   ::disconnectSlots()

   ::oUI:q_checkAnimated                : setChecked( val( ::oINI:cIdeAnimated ) > 0      )

   ::oUI:q_checkHilightLine             : setChecked( ::oIde:lCurrentLineHighlightEnabled )
   ::oUI:q_checkHorzRuler               : setChecked( ::oIde:lHorzRulerVisible            )
   ::oUI:q_checkLineNumbers             : setChecked( ::oIde:lLineNumbersVisible          )

   /* Line Ending Mode */
   s := ::oINI:cLineEndingMode
   //
   ::oUI:q_radioLineEndCRLF             : setChecked( s == "CRLF" .OR. empty( s )         )
   ::oUI:q_radioLineEndCR               : setChecked( s == "CR"                           )
   ::oUI:q_radioLineEndLF               : setChecked( s == "LF"                           )
   ::oUI:q_radioLineEndOS               : setChecked( s == "OS"                           )
   ::oUI:q_radioLineEndAuto             : setChecked( s == "AUTO"                         )

   ::oUI:q_checkTrimTrailingBlanks      : setChecked( ::oINI:lTrimTrailingBlanks          )
   ::oUI:q_checkSaveSourceWhenComp      : setChecked( ::oINI:lSaveSourceWhenComp          )
   ::oUI:q_checkSupressHbKWordsToUpper  : setChecked( ::oINI:lSupressHbKWordsToUpper      )
   ::oUI:q_checkReturnAsBeginKeyword    : setChecked( ::oINI:lReturnAsBeginKeyword        )
   ::oUI:q_checkConvTabToSpcWhenLoading : setChecked( ::oINI:lConvTabToSpcWhenLoading     )
   ::oUI:q_checkTabToSpcInEdits         : setChecked( ::oINI:lTabToSpcInEdits             )
   ::oUI:q_checkAutoIndent              : setChecked( ::oINI:lAutoIndent                  )
   ::oUI:q_checkSmartIndent             : setChecked( ::oINI:lSmartIndent                 )
   ::oUI:q_editTabSpaces                : setText( hb_ntos( ::oIde:nTabSpaces    )        )
   ::oUI:q_editIndentSpaces             : setText( hb_ntos( ::oINI:nIndentSpaces )        )

   ::oUI:q_tableVar:clearContents()
   ::aKeyItems := {}
   FOR EACH a_ IN ::oINI:aKeywords
      ::populateKeyTableRow( a_:__enumIndex(), a_[ 1 ], a_[ 2 ] )
   NEXT

   a_:= hb_atokens( ::oINI:cTextFileExtensions, ",." )
   FOR EACH s IN a_
      ::oUI:q_listTextExt:addItem( strtran( s, "." ) )
   NEXT
   ::oUI:q_listTextExt:setSortingEnabled( .t. )
   ::oUI:q_listTextExt:sortItems()

   ::oUI:q_editTmpBkpPrd      : setText( hb_ntos( ::oINI:nTmpBkpPrd ) )
   ::oUI:q_editBkpPath        : setText( ::oINI:cBkpPath   )
   ::oUI:q_editBkpSuffix      : setText( ::oINI:cBkpSuffix )

   /* Selections - Code Completion */
   ::oUI:q_checkListlWithArgs : setChecked( ::oINI:lCompletionWithArgs )
   ::oUI:q_checkCmplInclArgs  : setChecked( ::oINI:lCompleteArgumented )

   /* Themes */
   ::oUI:q_sliderRed:setMinimum( 0 )
   ::oUI:q_sliderRed:setMaximum( 255 )

   ::oUI:q_sliderGreen:setMinimum( 0 )
   ::oUI:q_sliderGreen:setMaximum( 255 )

   ::oUI:q_sliderBlue:setMinimum( 0 )
   ::oUI:q_sliderBlue:setMaximum( 255 )

   ::oUI:q_editSec1:setText( "0" )
   ::oUI:q_editSec5:setText( "1" )

   ::oUI:q_editSec1:setReadOnly( .t. )
   ::oUI:q_editSec5:setReadOnly( .t. )

   ::connectSlots()

   ::pushThemesData()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:show()
   LOCAL cStyle

   IF empty( ::oUI )

      ::oUI := hbide_getUI( "setup", ::oDlg:oWidget )

      ::oUI:setMaximumWidth( ::oUI:width() )
      ::oUI:setMinimumWidth( ::oUI:width() )
      ::oUI:setMaximumHeight( ::oUI:height() )
      ::oUI:setMinimumHeight( ::oUI:height() )

      ::buildTree()
      ::buildKeywords()

      ::oUI:q_editFontName:setText( ::oINI:cFontName )
      ::oUI:q_editPointSize:setText( hb_ntos( ::oINI:nPointSize ) )

      FOR EACH cStyle IN ::aStyles
         ::oUI:q_comboStyle:addItem( cStyle )
      NEXT
      ::oUI:q_comboStyle:setCurrentIndex( ascan( ::aStyles, {|e| e == ::oINI:cIdeTheme } ) - 1 )

      ::setIcons()
      ::connectSlots()

      ::oUI:hide()
   ENDIF

   ::populate()
   ::oIde:setPosByIniEx( ::oUI:oWidget, ::oINI:cSetupDialogGeometry )
   ::oUI:exec()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:execEvent( cEvent, p, p1 )
   LOCAL qItem, nIndex, qFontDlg, qFont, nOK, nRow, b_, q0, q1, nCol, w0, w1
   LOCAL aRGB, nSlot, qFrame, aGrad, n, cCSS, cTheme

   HB_SYMBOL_UNUSED( p1 )

   SWITCH cEvent

   CASE "buttonSelFont_clicked"
      qFont := QFont():new( ::oINI:cFontName, ::oINI:nPointSize )
      qFont:setFixedPitch( .t. )
      qFontDlg := QFontDialog():new( ::oUI )
      qFontDlg:setCurrentFont( qFont )
      nOK := qFontDlg:exec()
      IF nOK == 1
         qFont := QFont():from( qFontDlg:currentFont() )

         ::oUI:q_editFontName:setText( qFont:family() )
         ::oUI:q_editPointSize:setText( hb_ntos( qFont:pointSize() ) )

         ::oINI:cFontName  := ::oUI:q_editFontName:text()
         ::oINI:nPointSize := val( ::oUI:q_editPointSize:text() )
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

   CASE "treeWidget_itemSelectionChanged"
      qItem  := QTreeWidgetItem():from( ::oUI:q_treeWidget:currentItem() )
      IF ( nIndex := ascan( ::aTree, qItem:text() ) ) > 0
         ::oUI:q_stackedWidget:setCurrentIndex( nIndex - 1 )
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
      IF ( nIndex := ::oUI:q_comboStyle:currentIndex() ) > -1
         ::oINI:cIdeTheme := ::aStyles[ nIndex + 1 ]
         ::setSystemStyle( ::aStyles[ nIndex + 1 ] )
      ENDIF
      EXIT

   CASE "buttonAddTextext_clicked"
      q0 := hbide_fetchAString( ::oUI, "", "Text File Extension" )
      IF !empty( q0 )
         ::oUI:q_listTextExt:addItem( lower( strtran( q0, "." ) ) )
      ENDIF
      EXIT

   CASE "buttonDelTextext_clicked"
      IF ::oUI:q_listTextExt:currentRow() >= 0
         ::oUI:q_listTextExt:takeItem( ::oUI:q_listTextExt:currentRow() )
      ENDIF
      EXIT

   CASE "buttonKeyAdd_clicked"
      ::populateKeyTableRow( len( ::aKeyItems ) + 1, "", "" )
      ::oUI:q_tableVar:setCurrentItem( ::aKeyItems[ len( ::aKeyItems ), 1 ] )
      EXIT

   CASE "buttonKeyDel_clicked"
      IF ( nRow := ::oUI:q_tableVar:currentRow() ) >= 0
         ::oUI:q_tableVar:removeRow( nRow )
         hb_adel( ::aKeyItems     , nRow + 1, .t. )
         hb_adel( ::oINI:aKeywords, nRow + 1, .t. )
      ENDIF
      EXIT

   CASE "buttonKeyUp_clicked"
      IF ( nRow := ::oUI:q_tableVar:currentRow() ) >= 1
         nCol := ::oUI:q_tableVar:currentColumn()

         b_ := ::aKeyItems[ nRow+1 ]
         q0 := QTableWidgetItem():new(); q0:setText( b_[ 1 ]:text() )
         q1 := QTableWidgetItem():new(); q1:setText( b_[ 2 ]:text() )

         b_ := ::aKeyItems[ nRow+0 ]
         w0 := QTableWidgetItem():new(); w0:setText( b_[ 1 ]:text() )
         w1 := QTableWidgetItem():new(); w1:setText( b_[ 2 ]:text() )

         ::oUI:q_tableVar:setItem( nRow-0, 0, w0 )
         ::oUI:q_tableVar:setItem( nRow-0, 1, w1 )

         ::oUI:q_tableVar:setItem( nRow-1, 0, q0 )
         ::oUI:q_tableVar:setItem( nRow-1, 1, q1 )

         ::aKeyItems[ nRow+1 ] := { w0,w1 }
         ::aKeyItems[ nRow+0 ] := { q0,q1 }

         ::oUI:q_tableVar:setCurrentItem( iif( nCol == 0, q0, q1 ) )
      ENDIF
      EXIT

   CASE "buttonKeyDown_clicked"
      nRow := ::oUI:q_tableVar:currentRow()
      IF nRow >= 0 .AND. nRow + 1 < len( ::aKeyItems )

         nCol := ::oUI:q_tableVar:currentColumn()

         b_ := ::aKeyItems[ nRow + 1 ]
         q0 := QTableWidgetItem():new(); q0:setText( b_[ 1 ]:text() )
         q1 := QTableWidgetItem():new(); q1:setText( b_[ 2 ]:text() )

         b_ := ::aKeyItems[ nRow + 2 ]
         w0 := QTableWidgetItem():new(); w0:setText( b_[ 1 ]:text() )
         w1 := QTableWidgetItem():new(); w1:setText( b_[ 2 ]:text() )

         ::oUI:q_tableVar:setItem( nRow, 0, w0 )
         ::oUI:q_tableVar:setItem( nRow, 1, w1 )

         ::oUI:q_tableVar:setItem( nRow+1, 0, q0 )
         ::oUI:q_tableVar:setItem( nRow+1, 1, q1 )

         ::aKeyItems[ nRow + 1 ] := { w0,w1 }
         ::aKeyItems[ nRow + 2 ] := { q0,q1 }

         ::oUI:q_tableVar:setCurrentItem( iif( nCol == 0, q0, q1 ) )
      ENDIF
      EXIT

   CASE "tableVar_keyPress"
      IF ( nRow := ::oUI:q_tableVar:currentRow() ) >= 0
         HB_TRACE( HB_TR_ALWAYS, "RECEIVING ENTER KEY" )
         ::oUI:q_tableVar:editItem( p )
         #if 0
         IF ::oUI:q_tableVar:currentColumn() == 0
            ::oUI:q_tableVar:setCurrentCell( ::oUI:q_tableVar:currentRow(), 1 )
         ENDIF
         #endif
      ENDIF

   CASE "radioSection_clicked"
      ::nCurThemeSlot := p
      IF empty( aRGB := ::pullThemeColors( p ) )
         aRGB := { 0,0,0 }
      ENDIF
      ::oUI:q_sliderRed   : setValue( aRGB[ 1 ] )
      ::oUI:q_sliderGreen : setValue( aRGB[ 2 ] )
      ::oUI:q_sliderBlue  : setValue( aRGB[ 3 ] )
      EXIT

   CASE "sliderValue_changed"
      #if 0
      nSlot := iif( ::oUI:q_radioSec1:isChecked(), 1, ;
                  iif( ::oUI:q_radioSec2:isChecked(), 2, ;
                     iif( ::oUI:q_radioSec3:isChecked(), 3, ;
                        iif( ::oUI:q_radioSec4:isChecked(), 4, ;
                           iif( ::oUI:q_radioSec5:isChecked(), 5, 0 ) ) ) ) )
      #endif
      nSlot := ::nCurThemeSlot

      IF nSlot > 0
         qFrame := { ::oUI:q_frameSec1, ::oUI:q_frameSec2, ::oUI:q_frameSec3, ::oUI:q_frameSec4, ::oUI:q_frameSec5 }[ nSlot ]

         aRGB := { ::oUI:q_sliderRed:value(), ::oUI:q_sliderGreen:value(), ::oUI:q_sliderBlue:value() }

         ::populateThemeColors( nSlot, aRGB )

         qFrame:setStyleSheet( "background-color: " + hbide_rgbString( aRGB ) + ";" )
      ENDIF

      aGrad := {}
      FOR nSlot := 1 TO 5
         n  := val( { ::oUI:q_editSec1, ::oUI:q_editSec2, ::oUI:q_editSec3, ::oUI:q_editSec4, ::oUI:q_editSec5 }[ nSlot ]:text() )

         IF !empty( aRGB := ::pullThemeColors( nSlot ) )
            aadd( aGrad, { n, aRGB[ 1 ], aRGB[ 2 ], aRGB[ 3 ] } )
         ENDIF
      NEXT
      IF !empty( aGrad )
         cCSS := 'background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0, ' + hbide_buildGradientString( aGrad ) + ");"
         ::oUI:q_frameHorz:setStyleSheet( cCSS )
         cCSS := 'background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, ' + hbide_buildGradientString( aGrad ) + ");"
         ::oUI:q_frameVert:setStyleSheet( cCSS )
      ENDIF
      EXIT

   CASE "listThemes_currentRowChanged"
      ::pushThemeColors( p + 1 )
      EXIT
   CASE "buttonThmAdd_clicked"
      IF !empty( cTheme := hbide_fetchAString( ::oDlg:oWidget, cTheme, "Name the Theme", "New Theme" ) )
         aadd( ::oINI:aAppThemes, cTheme + "," + "," + "," + "," )
         qItem := QListWidgetItem():new()
         qItem:setText( cTheme )
         ::oUI:q_listThemes:addItem_1( qItem )
         ::oUI:q_listThemes:setCurrentRow( len( ::oINI:aAppThemes ) - 1 )
      ENDIF
      EXIT
   CASE "buttonThmApp_clicked"
      IF ( n := ::oUI:q_listThemes:currentRow() ) > -1
         hbide_setAppTheme( ::getThemeData( n + 1 ) )
         ::oDK:animateComponents( HBIDE_ANIMATION_GRADIENT )
      ENDIF
      EXIT
   CASE "buttonThmDel_clicked"
      EXIT
   CASE "buttonThmSav_clicked"
      IF ( n := ::oUI:q_listThemes:currentRow() ) > -1
         ::oINI:aAppThemes[ n + 1 ] := QListWidgetItem():from( ::oUI:q_listThemes:currentItem() ):text() + "," + ;
                                       ::fetchThemeColorsString()
      ENDIF
      EXIT

   ENDSWITCH

   RETURN nRow //Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:pushThemesData()
   LOCAL s, a_, qItem

   IF ::nCurThemeSlot == 0
      FOR EACH s IN ::oINI:aAppThemes
         a_:= hb_aTokens( s, "," )
         qItem := QListWidgetItem():new()
         qItem:setText( a_[ 1 ] )
         ::oUI:q_listThemes:addItem_1( qItem )
         ::pushThemeColors( s:__enumIndex() )
      NEXT
   ENDIF
   IF !empty( ::oINI:aAppThemes )
      ::oUI:q_listThemes:setCurrentRow( -1 )
      ::oUI:q_listThemes:setCurrentRow( len( ::oINI:aAppThemes ) - 1 )
      ::oUI:q_listThemes:setCurrentRow( 0 )
   ENDIF
   ::oUI:q_radioSec1:click()

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:getThemeData( nTheme )
   LOCAL a_, i, aTheme := {}

   IF nTheme >= 1 .AND. nTheme <= len( ::oINI:aAppThemes )
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

   IF nTheme >= 1 .AND. nTheme <= len( ::oINI:aAppThemes )
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
            { ::oUI:q_editSec1, ::oUI:q_editSec2, ::oUI:q_editSec3, ::oUI:q_editSec4, ::oUI:q_editSec5 }[ nSlot ]:setText( hb_ntos( aRGB[ 1 ] ) )

            ::populateThemeColors( nSlot, { aRGB[ 2 ], aRGB[ 3 ], aRGB[ 4 ] } )
         ENDIF
      NEXT
      { ::oUI:q_radioSec1, ::oUI:q_radioSec2, ::oUI:q_radioSec3, ::oUI:q_radioSec4, ::oUI:q_radioSec5 }[ nSlot ]:click()
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:populateThemeColors( nSlot, aRGB )
   LOCAL qFrame

   { ::oUI:q_editR1, ::oUI:q_editR2, ::oUI:q_editR3, ::oUI:q_editR4, ::oUI:q_editR5 }[ nSlot ]:setText( hb_ntos( aRGB[ 1 ] ) )
   { ::oUI:q_editG1, ::oUI:q_editG2, ::oUI:q_editG3, ::oUI:q_editG4, ::oUI:q_editG5 }[ nSlot ]:setText( hb_ntos( aRGB[ 2 ] ) )
   { ::oUI:q_editB1, ::oUI:q_editB2, ::oUI:q_editB3, ::oUI:q_editB4, ::oUI:q_editB5 }[ nSlot ]:setText( hb_ntos( aRGB[ 3 ] ) )

   qFrame := { ::oUI:q_frameSec1, ::oUI:q_frameSec2, ::oUI:q_frameSec3, ::oUI:q_frameSec4, ::oUI:q_frameSec5 }[ nSlot ]
   qFrame:setStyleSheet( "background-color: " + hbide_rgbString( aRGB ) + ";" )

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeSetup:fetchThemeColorsString( nSlot )
   LOCAL s := ""

   IF empty( nSlot )
      FOR nSlot := 1 TO 5
         s += { ::oUI:q_editSec1, ::oUI:q_editSec2, ::oUI:q_editSec3, ::oUI:q_editSec4, ::oUI:q_editSec5 }[ nSlot ]:text() + " "

         s += { ::oUI:q_editR1, ::oUI:q_editR2, ::oUI:q_editR3, ::oUI:q_editR4, ::oUI:q_editR5 }[ nSlot ]:text() + " "
         s += { ::oUI:q_editG1, ::oUI:q_editG2, ::oUI:q_editG3, ::oUI:q_editG4, ::oUI:q_editG5 }[ nSlot ]:text() + " "
         s += { ::oUI:q_editB1, ::oUI:q_editB2, ::oUI:q_editB3, ::oUI:q_editB4, ::oUI:q_editB5 }[ nSlot ]:text()

         s += ","
      NEXT
   ELSE

   ENDIF

   RETURN s

/*------------------------------------------------------------------------*/

METHOD IdeSetup:pullThemeColors( nSlot )
   LOCAL aRGB := {}

   IF !empty( { ::oUI:q_editSec1, ::oUI:q_editSec2, ::oUI:q_editSec3, ::oUI:q_editSec4, ::oUI:q_editSec5 }[ nSlot ]:text() )
      aadd( aRGB, val( { ::oUI:q_editR1, ::oUI:q_editR2, ::oUI:q_editR3, ::oUI:q_editR4, ::oUI:q_editR5 }[ nSlot ]:text() ) )
      aadd( aRGB, val( { ::oUI:q_editG1, ::oUI:q_editG2, ::oUI:q_editG3, ::oUI:q_editG4, ::oUI:q_editG5 }[ nSlot ]:text() ) )
      aadd( aRGB, val( { ::oUI:q_editB1, ::oUI:q_editB2, ::oUI:q_editB3, ::oUI:q_editB4, ::oUI:q_editB5 }[ nSlot ]:text() ) )
   ENDIF

   RETURN aRGB

/*------------------------------------------------------------------------*/

METHOD IdeSetup:populateKeyTableRow( nRow, cTxtCol1, cTxtCol2 )
   LOCAL lAppend := len( ::aKeyItems ) < nRow
   LOCAL q0, q1

   IF lAppend
      ::oUI:q_tableVar:setRowCount( nRow )

      q0 := QTableWidgetItem():new()
      q0:setText( cTxtCol1 )
      ::oUI:q_tableVar:setItem( nRow-1, 0, q0 )

      q1 := QTableWidgetItem():new()
      q1:setText( cTxtCol2 )
      ::oUI:q_tableVar:setItem( nRow-1, 1, q1 )

      aadd( ::aKeyItems, { q0, q1 } )

      ::oUI:q_tableVar:setRowHeight( nRow-1, 16 )

   ELSE
      ::aKeyItems[ nRow, 1 ]:setText( cTxtCol1 )
      ::aKeyItems[ nRow, 2 ]:setText( cTxtCol2 )

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:buildKeywords()
   LOCAL hdr_:= { { "Keyword", 100 }, { "Value", 230 } }
   LOCAL oTbl, n, qItm

   oTbl := ::oUI:q_tableVar

   QHeaderView():from( oTbl:verticalHeader() ):hide()
   QHeaderView():from( oTbl:horizontalHeader() ):stretchLastSection( .t. )

   oTbl:setAlternatingRowColors( .t. )
   oTbl:setColumnCount( len( hdr_ ) )
   oTbl:setShowGrid( .t. )
   FOR n := 1 TO len( hdr_ )
      qItm := QTableWidgetItem():new()
      qItm:setText( hdr_[ n,1 ] )
      oTbl:setHorizontalHeaderItem( n-1, qItm )
      oTbl:setColumnWidth( n-1, hdr_[ n,2 ] )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:buildTree()
   LOCAL oRoot, oChild, s

   ::oUI:q_treeWidget:setHeaderHidden( .t. )
   ::oUI:q_treeWidget:setIconSize( QSize():new( 12,12 ) )
   ::oUI:q_treeWidget:setIndentation( 12 )

   oRoot := QTreeWidgetItem():new()
   oRoot:setText( 0, "Parts" )
   oRoot:setToolTip( 0, "Parts" )

   ::oUI:q_treeWidget:addTopLevelItem( oRoot )

   aadd( ::aItems, oRoot )

   FOR EACH s IN ::aTree
      oChild := QTreeWidgetItem():new()
      oChild:setText( 0, s )
      oChild:setToolTip( 0, s )
      oRoot:addChild( oChild )
      aadd( ::aItems, oChild )
   NEXT

   oRoot:setExpanded( .t. )
   ::oUI:q_treeWidget:setCurrentItem( ::aItems[ 2 ] ) /* General */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setSystemStyle( cStyle )
   LOCAL oApp, qFactory

   IF !empty( cStyle )
      oApp     := QApplication():new()
      qFactory := QStyleFactory():new()
      oApp:setStyle( qFactory:create( cStyle ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSetup:setBaseColor()
   #if 0
   LOCAL qPalette, oApp, qBrush, qColor

   oApp := QApplication():new()

   ::qOrgPalette := QPalette():from( oApp:palette() )

   qColor := QColor():new( Qt_red )
   qBrush := QBrush():new( "QColor", qColor )

   qPalette := QPalette():from( oApp:palette() )
   qPalette:setBrush( QPalette_Window, qBrush )
   qPalette:setColor( QPalette_Window, qColor )
   qPalette:setColor( QPalette_Base, qColor )

   oApp:setPalette( qPalette )
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

   /*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               23Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "xbp.ch"
#include "inkey.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

#define _T( x )  ( mnuNormalizeItem( x ) )

#define __buttonViewTabbed_clicked__              2007
#define __buttonViewOrganized_clicked__           2009
#define __buttonSaveLayout_clicked__              2010
#define __buttonViewCascaded_clicked__            2011
#define __buttonViewTiled_clicked__               2012
#define __buttonViewMaximized_clicked__           2013
#define __buttonViewStackedVert_clicked__         2014
#define __buttonViewStackedHorz_clicked__         2015
#define __buttonViewZoomedIn_clicked__            2016
#define __buttonViewZoomedOut_clicked__           2017

/*----------------------------------------------------------------------*/

CLASS IdeActions INHERIT IdeObject

   DATA   qWidget
   DATA   hActions                                INIT { => }
   DATA   oActToolsBtn

   DATA   qMainToolbar
   DATA   qFilesToolbar
   DATA   qPartsToolbar
   DATA   qProjectToolbar
   DATA   qTBarDocks
   DATA   qMdiToolbar
   DATA   qMdiToolbarL
   DATA   qSelToolbar

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()

   METHOD getAction( cKey )
   METHOD buildActions()
   METHOD loadActions()

   METHOD buildMainMenu()

   METHOD buildToolBars()
   METHOD buildToolbarMain()
   METHOD buildToolbarFiles()
   METHOD buildToolbarParts()
   METHOD buildToolbarProject()
   METHOD buildToolbarDocks()
   METHOD buildMdiToolbar()
   METHOD buildMdiToolbarLeft()
   METHOD buildToolbarSelectedText()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeActions:new( oIde )

   hb_hCaseMatch( ::hActions, .f. )
   ::oIde := oIde
   ::qWidget := QWidget()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   ::buildActions()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:destroy()
   LOCAL qAction

   FOR EACH qAction IN ::hActions
      qAction := NIL
   NEXT
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:getAction( cKey )

   IF hb_hHasKey( ::hActions, cKey )
      RETURN ::hActions[ cKey ]
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildActions()
   LOCAL qAction, aAct, a_

   aAct := ::loadActions()

   FOR EACH a_ IN aAct
      IF !( hb_hHasKey( ::hActions, a_[ ACT_NAME ] ) )

         qAction := QAction( ::qWidget )
         qAction:setCheckable( iif( empty( a_[ ACT_CHECKABLE ] ), .F., upper( a_[ ACT_CHECKABLE ] ) == "YES" ) )
         qAction:setText( strtran( a_[ ACT_TEXT ], "~", "&" ) )
         IF !empty( a_[ ACT_IMAGE ] )
            qAction:setIcon( QIcon( hbide_image( a_[ ACT_IMAGE ] ) ) )
         ENDIF

         #if 0
         IF !empty( a_[ ACT_SHORTCUT ] )
            k := a_[ ACT_SHORTCUT ]
            k := strtran( k, "Sh+", "Shift+" )
            k := strtran( k, "SH+", "Shift+" )
            k := strtran( k, "^"  , "Ctrl+"  )
            qAction:setShortcut( QKeySequence( k ) )
         ENDIF
         #endif
         qAction:setTooltip( strtran( a_[ ACT_TEXT ], "~", "" ) )

         ::hActions[ a_[ ACT_NAME ] ] := qAction

      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:loadActions()
   LOCAL aAct := {}

   //    <Text> can be loaded from .ini or similar mechanism given <Name>
   //
   //            Name                     Text                             Image             Shortcut  Checkable  IconVisInMenu
   //
   aadd( aAct, { "TB_Exit"              , "E~xit"                        , "exit3"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Home"              , "~Home"                        , "home3"/*"dc_home"*/, ""  , "No", "Yes" } )
   aadd( aAct, { "TB_New"               , "~Source"                      , "new"            , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Open"              , "~Open"                        , "open3"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Save"              , "~Save"                        , "save3"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Close"             , "~Close"                       , "close3"         , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Print"             , "~Print"                       , "print"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Script"            , "Run as script"                , "runscript"      , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Compile"           , "Co~mpile"                     , "compile"        , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_CompilePPO"        , "Com~pile to PPO"              , "ppo"            , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_BuildSource"       , "Build Source"                 , "buildsource"    , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Build"             , "Build Project"                , "build"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_BuildLaunch"       , "Build and Launch"             , "buildlaunch"    , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Rebuild"           , "Rebuild Project"              , "rebuild"        , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_RebuildLaunch"     , "Rebuild and Launch"           , "rebuildlaunch"  , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ToggleProjectTree" , "Toggle Project Tree"          , "properties"     , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ToggleBuildInfo"   , "Toggle Build Info"            , "builderror"     , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ToggleFuncList"    , "Toggle Function List"         , "modulelist"     , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Undo"              , "~Undo"                        , "undo"           , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Redo"              , "~Redo"                        , "redo"           , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Cut"               , "C~ut"                         , "cut"            , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Copy"              , "~Copy"                        , "copy"           , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Paste"             , "~Paste"                       , "paste"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_SelectAll"         , "Select ~All"                  , "selectall"      , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_SelectionMode"     , "Toggle Selection Mode"        , "stream"         , ""     , "Yes", "Yes" } )
   aadd( aAct, { "TB_Find"              , "~Find / Replace"              , "find"           , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Search"            , "Search"                       , "search"         , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_SetMark"           , "Toggle Mark"                  , "placeremovemark", ""     , "No", "Yes" } )
   aadd( aAct, { "TB_GotoMark"          , "Goto Mark"                    , "gotomark"       , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Goto"              , "~Goto Line"                   , "gotoline"       , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ToUpper"           , "To Upper"                     , "toupper"        , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ToLower"           , "To Lower"                     , "tolower"        , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Invert"            , "Invert"                       , "invertcase"     , ""     , "No", "Yes" } )
 * aadd( aAct, { "TB_MatchPairs"        , "Match Pairs"                  , "matchobj"       , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Tools"             , "Tools and Utilities"          , "tools"          , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ZoomIn"            , "ZoomIn"                       , "zoomin"         , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_ZoomOut"           , "ZoomOut"                      , "zoomout"        , ""     , "No", "Yes" } )
   //
   aadd( aAct, { "Help"                 , "~Help"                        , "help"           , "F1"   , "No", "Yes" } )
   aadd( aAct, { "Exit"                 , "E~xit"                        , "exit"           , "Sh+^W", "No", "Yes" } )
   aadd( aAct, { "New"                  , "~Source"                      , "new"            , "^N"   , "No", "Yes" } )
   aadd( aAct, { "Open"                 , "~Open..."                     , "open"           , "^O"   , "No", "Yes" } )
   aadd( aAct, { "Save"                 , "~Save"                        , "save"           , "^S"   , "No", "Yes" } )
   aadd( aAct, { "Close"                , "~Close"                       , "close"          , "^W"   , "No", "Yes" } )
   aadd( aAct, { "Print"                , "~Print..."                    , "print"          , "^P"   , "No", "Yes" } )
   aadd( aAct, { "Compile"              , "Co~mpile"                     , "compile"        , ""     , "No", "Yes" } )
   aadd( aAct, { "CompilePPO"           , "Com~pile to PPO"              , "ppo"            , ""     , "No", "Yes" } )
   aadd( aAct, { "Build"                , "Build Project"                , "build"          , ""     , "No", "Yes" } )
   aadd( aAct, { "BuildSource"          , "Build Source"                 , "buildsource"    , ""     , "No", "Yes" } )
   aadd( aAct, { "BuildLaunch"          , "Build and Launch"             , "buildlaunch"    , ""     , "No", "Yes" } )
   aadd( aAct, { "Rebuild"              , "Rebuild Project"              , "rebuild"        , ""     , "No", "Yes" } )
   aadd( aAct, { "RebuildLaunch"        , "Rebuild and Launch"           , "rebuildlaunch"  , ""     , "No", "Yes" } )
   aadd( aAct, { "ToggleProjectTree"    , "Toggle Project Tree"          , "properties"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ToggleBuildInfo"      , "Toggle Build Info"            , "builderror"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ToggleFuncList"       , "Toggle Function List"         , "modulelist"     , ""     , "No", "Yes" } )
   aadd( aAct, { "Undo"                 , "~Undo"                        , "undo"           , ""     , "No", "Yes" } )
   aadd( aAct, { "Redo"                 , "~Redo"                        , "redo"           , ""     , "No", "Yes" } )
   aadd( aAct, { "Cut"                  , "C~ut"                         , "cut"            , ""     , "No", "Yes" } )
   aadd( aAct, { "Copy"                 , "~Copy"                        , "copy"           , ""     , "No", "Yes" } )
   aadd( aAct, { "Paste"                , "~Paste"                       , "paste"          , ""     , "No", "Yes" } )
   aadd( aAct, { "SelectAll"            , "Select ~All"                  , "selectall"      , ""     , "No", "Yes" } )
   aadd( aAct, { "SelectionMode"        , "Toggle Selection Mode"        , "stream"         , ""     , "No", "Yes" } )
   aadd( aAct, { "Find"                 , "~Find / Replace..."           , "find"           , "^F"   , "No", "Yes" } )
   aadd( aAct, { "FindEx"               , "~Find / Replace Ex"           , "find"           , "Sh+^F", "No", "Yes" } )
   aadd( aAct, { "Search"               , "F~ind in Files"               , "search"         , ""     , "No", "Yes" } )
   aadd( aAct, { "SetMark"              , "Set Mark"                     , "placeremovemark", ""     , "No", "Yes" } )
   aadd( aAct, { "GotoMark"             , "Goto Mark"                    , "gotomark"       , ""     , "No", "Yes" } )
   aadd( aAct, { "Goto"                 , "~Goto Line..."                , "gotoline"       , "^G"   , "No", "Yes" } )
   aadd( aAct, { "MatchPairs"           , "Match Pairs"                  , "matchobj"       , ""     , "No", "Yes" } )
   aadd( aAct, { "ZoomIn"               , "ZoomIn"                       , "zoomin"         , ""     , "No", "Yes" } )
   aadd( aAct, { "ZoomOut"              , "ZoomOut"                      , "zoomout"        , ""     , "No", "Yes" } )
   //
   aadd( aAct, { "NewProject"           , "new Project Wizard"           , "project"        , ""     , "No", "Yes" } )
   aadd( aAct, { "LoadProject"          , "Open Projec~t..."             , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "SaveAs"               , "Save ~as..."                  , "saveas"         , ""     , "No", "Yes" } )
   aadd( aAct, { "SaveAll"              , "Save A~ll"                    , "saveall"        , "Sh+^s", "No", "Yes" } )
   aadd( aAct, { "CloseAll"             , "Clos~e All"                   , "closeall"       , ""     , "No", "Yes" } )
   aadd( aAct, { "CloseOther"           , "Close Ot~hers"                , "closeexcept"    , ""     , "No", "Yes" } )
   aadd( aAct, { "Revert"               , "~Revert to Saved"             , ""               , "Sh+^R", "No", "Yes" } )
   aadd( aAct, { "ExportHTML"           , "~Export to HTML"              , "exporthtml"     , ""     , "No", "Yes" } )
   aadd( aAct, { "InsertDateTime"       , "~Date && Time"                , "insert-datetime", "Sh+F7", "No", "Yes" } )
   aadd( aAct, { "InsertRandomName"     , "~Random Function Name"        , "insert-procname", "Sh+F8", "No", "Yes" } )
   aadd( aAct, { "InsertExternalFile"   , "~External File at Cursor"     , "insert-external-file", "", "No", "Yes" } )
   aadd( aAct, { "InsertSeparator"      , "~Separator"                   , "insert-separator", "F7"  , "No", "Yes" } )
   aadd( aAct, { "switchReadOnly"       , "Switch Read~Only Mode"        , "readonly"       , ""     , "No", "Yes" } )
   aadd( aAct, { "Properties"           , "Properties"                   , "properties"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ProjAddSource"        , "Add Source to Project"        , "projectadd"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ProjRemSource"        , "Remove Source"                , "projectdel"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ProjMainModule"       , "Select Main Module"           , "setmain"        , ""     , "No", "Yes" } )
   aadd( aAct, { "SelectProject"        , "Select Current Project"       , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "CloseProject"         , "Close Current Project"        , "projectdel"     , ""     , "No", "Yes" } )
   aadd( aAct, { "Build"                , "Build Project"                , "build"          , "^F9"  , "No", "Yes" } )
   aadd( aAct, { "BuildLaunch"          , "Build and Launch Project"     , "buildlaunch"    , "F9"   , "No", "Yes" } )
   aadd( aAct, { "ReBuild"              , "Rebuild Project"              , "rebuild"        , ""     , "No", "Yes" } )
   aadd( aAct, { "ReBuildLaunch"        , "Rebuild and Launch Project"   , "rebuildlaunch"  , ""     , "No", "Yes" } )
   aadd( aAct, { "Compile"              , "Compile Current Source"       , "compile"        , ""     , "No", "Yes" } )
   aadd( aAct, { "CompilePPO"           , "Compile Current Source to PPO", "ppo"            , ""     , "No", "Yes" } )
   aadd( aAct, { "LaunchProject"        , "Launch Project"               , "launch_r"       , "^F10" , "No", "Yes" } )
   aadd( aAct, { "ConfigureTools"       , "Configure Tools...*"          , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "CuiEditor"            , "CUI Screen Edirot"            , "cuied"          , ""     , "No", "Yes" } )
   aadd( aAct, { "UISrcManager"         , "UI Source Manager"            , "fileprg"        , ""     , "No", "Yes" } )

   aadd( aAct, { "ManageThemes"         , "Manage Themes"                , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "DefaultTheme"         , "Set Default Theme"            , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "AboutIDE"             , "About Harbour IDE"            , "hbide"          , ""     , "No", "Yes" } )
   aadd( aAct, { "AboutHarbour"         , "About Harbour"                , "hb-16x16"       , ""     , "No", "Yes" } )
   aadd( aAct, { "HarbourUsersList"     , "Harbour Users (Mailing Lists)", "list-users"     , ""     , "No", "Yes" } )
   aadd( aAct, { "HarbourDevList"       , "Harbour Developers (Mailing Lists)", "list-developers", "", "No", "Yes" } )

   aadd( aAct, { "BuildQt"              , "Build Project"                , "build"          , ""     , "No", "Yes" } )
   aadd( aAct, { "BuildLaunchQt"        , "Build and Launch"             , "buildlaunch"    , ""     , "No", "Yes" } )
   aadd( aAct, { "RebuildQt"            , "Rebuild Project"              , "rebuild"        , ""     , "No", "Yes" } )
   aadd( aAct, { "RebuildLaunchQt"      , "Rebuild and Launch"           , "rebuildlaunch"  , ""     , "No", "Yes" } )

   aadd( aAct, { "RemoveTabs"           , "Replace Tabs with Spaces"     , "tabstospaces"   , ""     , "No", "Yes" } )
   aadd( aAct, { "Spaces2Tabs"          , "Replace Spaces with Tabs"     , ""               , ""     , "No", "Yes" } )
   aadd( aAct, { "RemoveTrailingSpaces" , "Remove Trailing Spaces"       , "removetrailingspaces", "", "No", "Yes" } )
   aadd( aAct, { "FormatBraces"         , "Format Braces"                , "ormatbraces"    , ""     , "No", "Yes" } )
   aadd( aAct, { "UpperCaseKeywords"    , "UpperCase Harbour Keywords"   , "ormatbraces"    , ""     , "No", "Yes" } )

   aadd( aAct, { "StreamComment"        , "Stream Comment"               , "streamcomment"  , ""     , "No", "Yes" } )
   aadd( aAct, { "BlockComment"         , "Block Comment"                , "blockcomment"   , ""     , "No", "Yes" } )
   aadd( aAct, { "BlockIndentR"         , "Indent Right"                 , "blockindentr"   , ""     , "No", "Yes" } )
   aadd( aAct, { "BlockIndentL"         , "Indent Left"                  , "blockindentl"   , ""     , "No", "Yes" } )
   aadd( aAct, { "BlockDbl2Sgl"         , "Double Quotes to Single"      , "dbl2sglquote"   , ""     , "No", "Yes" } )
   aadd( aAct, { "BlockSgl2Dbl"         , "Single Quotes to Double"      , "sgl2dblquote"   , ""     , "No", "Yes" } )

   aadd( aAct, { "DuplicateLine"        , "Duplicate Current Line"       , "duplicateline"  , ""     , "No", "Yes" } )
   aadd( aAct, { "DeleteLine"           , "Delete Line"                  , "deleteline"     , ""     , "No", "Yes" } )
   aadd( aAct, { "MoveLineUp"           , "Move Line Up"                 , "movelineup"     , ""     , "No", "Yes" } )
   aadd( aAct, { "MoveLineDown"         , "Move Line Down"               , "movelinedown"   , ""     , "No", "Yes" } )
   aadd( aAct, { "Environments"         , "Environments..."              , "envconfig"      , ""     , "No", "Yes" } )

   aadd( aAct, { "ToUpper"              , "To Upper"                     , "toupper"        , ""     , "No", "Yes" } )
   aadd( aAct, { "ToLower"              , "To Lower"                     , "tolower"        , ""     , "No", "Yes" } )
   aadd( aAct, { "Invert"               , "Invert"                       , "invertcase"     , ""     , "No", "Yes" } )

   aadd( aAct, { "GotoFunc"             , "Goto Function"                , "dc_function"    , ""     , "No", "Yes" } )
   aadd( aAct, { "Shortcuts"            , "Keyboard Mappings"            , "keyboardmappings",""     , "No", "Yes" } )
   aadd( aAct, { "Setup"                , "hbIDE Setup"                  , "idepreferences" , ""     , "No", "Yes" } )
   aadd( aAct, { "ChangeLog"            , "Manage ChangeLog(s)"          , "memo"           , ""     , "No", "Yes" } )
   aadd( aAct, { "TB_Hide"              , "Hide Show Docks"              , "hideshow"       , ""     , "No", "Yes" } )
   aadd( aAct, { "Delete"               , "Delete"                       , "dc_delete"      , ""     , "No", "Yes" } )
   aadd( aAct, { "SortAZ"               , "Sort Ascending"               , "sort"           , ""     , "No", "Yes" } )
   aadd( aAct, { "SortZA"               , "Sort Descending"              , "sortdescend"    , ""     , "No", "Yes" } )
   //
   aadd( aAct, { "Split"               , "Split Editing Instance"        , "split"          , ""     , "No", "Yes" } )
   aadd( aAct, { "SplitClose"          , "Close Splitted Instance"       , "split_close"    , ""     , "No", "Yes" } )
   aadd( aAct, { "SplitH"              , "Split Horizontally"            , "split_h"        , ""     , "No", "Yes" } )
   aadd( aAct, { "SplitV"              , "Split Vertically"              , "split_v"        , ""     , "No", "Yes" } )
   aadd( aAct, { "Dictionary"          , "Create .tag Dictionary"        , "dictionary"     , ""     , "No", "Yes" } )
   aadd( aAct, { "ConfigToolbars"      , "Configure Toolbars"            , "configtoolbars" , ""     , "No", "Yes" } )
   //
   aadd( aAct, { "DBU"                 , "ideDBU"                        , "browser"        , ""     , "No", "Yes" } )
   aadd( aAct, { "EDITOR"              , "ideEDITOR"                     , "editor"         , ""     , "No", "Yes" } )

   RETURN aAct

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildMainMenu()
   LOCAL oMenuBar, oSubMenu, oSubMenu2, n, f
   LOCAL oIde := ::oIde

   oMenuBar := ::oDlg:MenuBar()

   /*----------------------------------------------------------------------------*/
   /*                                   File                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~File"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:addItem( { ::getAction( "New"        ), {|| oIde:execAction( "New"            ) } } )
   oSubMenu2:addItem( { ::getAction( "NewProject" ), {|| oIde:execAction( "NewProject"     ) } } )
   oMenuBar:addItem( { oSubMenu2,  _T( "~New" ) } )
   oMenuBar:aMenuItems[ oMenuBar:numItems(), 2 ]:setIcon( QIcon( oIde:resPath + 'new.png' ) )

   oSubMenu:addItem( { ::getAction( "Open"        ), {|| oIde:execAction( "Open"           ) } } )
   oSubMenu:addItem( { ::getAction( "LoadProject" ), {|| oIde:execAction( "LoadProject"    ) } } )

   hbide_menuAddSep( oSubMenu )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:itemSelected := {| nIndex, cFile | cFile := oIde:oIni:aRecentFiles[ nIndex ], ;
                                                oIde:oEM:editSource( cFile ) }
   IF !empty( oIde:oIni:aRecentFiles )
      FOR n := 1 TO Len( oIde:oIni:aRecentFiles )
         f := hbide_pathNormalized( oIde:oIni:aRecentFiles[ n ], .F. )
         oSubMenu2:addItem( { _T( '~' + hb_NumToHex(n) + '. ' + f ), nil } )
         IF !hb_FileExists( f )
            oSubMenu2:disableItem( n )
         ENDIF
      NEXT
   ELSE
      oSubMenu2:addItem( { _T( "** No recent files found **" )   , nil } )
      oSubMenu2:disableItem( 1 )
   ENDIF
   oMenuBar:addItem( { oSubMenu2,  _T( "Recent Files" ) } )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:itemSelected := {| nIndex, cFile | cFile := oIde:oIni:aRecentProjects[ nIndex ], ;
                                                ::oPM:loadProperties( cFile, .F., .F., .T. ) }
   IF !empty( oIde:oIni:aRecentProjects )
      FOR n := 1 TO Len( oIde:oIni:aRecentProjects )
          f := hbide_pathNormalized( oIde:oIni:aRecentProjects[ n ], .F. )
          oSubMenu2:addItem( { _T( '~' + hb_NumToHex( n ) + '. ' + f )   , nil } )
          IF !hb_FileExists( f )
             oSubMenu2:disableItem( n )
          ENDIF
      NEXT
   ELSE
      oSubMenu2:addItem( { _T( "** No recent projects found **" )   , nil } )
      oSubMenu2:disableItem( 1 )
   ENDIF
   oMenuBar:addItem( { oSubMenu2,  _T( "Recent Projects" ) } )

   hbide_menuAddSep( oSubMenu )

   oSubMenu:addItem( { ::getAction( "Save"                ), {|| oIde:execAction( "Save"           ) } } )
   oSubMenu:addItem( { ::getAction( "SaveAs"              ), {|| oIde:execAction( "SaveAs"         ) } } )
   oSubMenu:addItem( { ::getAction( "SaveAll"             ), {|| oIde:execAction( "SaveAll"        ) } } )
   oSubMenu:addItem( { ::getAction( "Close"               ), {|| oIde:execAction( "Close"          ) } } )
   oSubMenu:addItem( { ::getAction( "CloseAll"            ), {|| oIde:execAction( "CloseAll"       ) } } )
   oSubMenu:addItem( { ::getAction( "CloseOther"          ), {|| oIde:execAction( "CloseOther"     ) } } )
   oSubMenu:addItem( { ::getAction( "Revert"              ), {|| oIde:execAction( "Revert"         ) } } )
   hbide_menuAddSep( oSubMenu )

   oSubMenu:addItem( { ::getAction( "ExportHTML"          ), {|| oIde:execAction( "ExportHTML"     ) } } )
   oSubMenu:addItem( { ::getAction( "Print"               ), {|| oIde:execAction( "Print"          ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "SaveExit"            ), {|| oIde:execAction( "SaveExit"       ) } } )
   oSubMenu:addItem( { ::getAction( "Exit"                ), {|| oIde:execAction( "Exit"           ) } } )

   /*----------------------------------------------------------------------------*/
   /*                                   Edit                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~Edit"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "Undo"                ), {|| oIde:execAction( "Undo"           ) } } )
   oSubMenu:addItem( { ::getAction( "Redo"                ), {|| oIde:execAction( "Redo"           ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "Cut"                 ), {|| oIde:execAction( "Cut"            ) } } )
   oSubMenu:addItem( { ::getAction( "Copy"                ), {|| oIde:execAction( "Copy"           ) } } )
   oSubMenu:addItem( { ::getAction( "Paste"               ), {|| oIde:execAction( "Paste"          ) } } )
   oSubMenu:addItem( { ::getAction( "SelectAll"           ), {|| oIde:execAction( "SelectAll"      ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "Find"                ), {|| oIde:execAction( "Find"           ) } } )
   oSubMenu:addItem( { ::getAction( "FindEx"              ), {|| oIde:execAction( "FindEx"         ) } } )
   oSubMenu:oWidget:addAction( ::oFindDock:oWidget:toggleViewAction() )
   oSubMenu:addItem( { ::getAction( "Goto"                ), {|| oIde:execAction( "Goto"           ) } } )
   hbide_menuAddSep( oSubMenu )
   //
   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:addItem( { ::getAction( "DuplicateLine"      ), {|| oIde:execAction( "DuplicateLine"  ) } } )
   oSubMenu2:addItem( { ::getAction( "DeleteLine"         ), {|| oIde:execAction( "DeleteLine"     ) } } )
   oSubMenu2:addItem( { ::getAction( "MoveLineUp"         ), {|| oIde:execAction( "MoveLineUp"     ) } } )
   oSubMenu2:addItem( { ::getAction( "MoveLineDown"       ), {|| oIde:execAction( "MoveLineDown"   ) } } )
   oSubMenu:addItem( { oSubMenu2,  _T( "~Line" ) } )
   //
   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:addItem( { ::getAction( "ToUpper"            ), {|| oIde:execAction( "ToUpper"        ) } } )
   oSubMenu2:addItem( { ::getAction( "ToLower"            ), {|| oIde:execAction( "ToLower"        ) } } )
   oSubMenu2:addItem( { ::getAction( "Invert"             ), {|| oIde:execAction( "Invert"         ) } } )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:addItem( { ::getAction( "StreamComment"      ), {|| oIde:execAction( "StreamComment"  ) } } )
   oSubMenu2:addItem( { ::getAction( "BlockComment"       ), {|| oIde:execAction( "BlockComment"   ) } } )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:addItem( { ::getAction( "BlockIndentR"       ), {|| oIde:execAction( "BlockIndentR"   ) } } )
   oSubMenu2:addItem( { ::getAction( "BlockIndentL"       ), {|| oIde:execAction( "BlockIndentL"   ) } } )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:addItem( { ::getAction( "BlockSgl2Dbl"       ), {|| oIde:execAction( "BlockSgl2Dbl"   ) } } )
   oSubMenu2:addItem( { ::getAction( "BlockDbl2Sgl"       ), {|| oIde:execAction( "BlockDbl2Sgl"   ) } } )
   oSubMenu:addItem( { oSubMenu2,  _T( "~Block" ) } )

   hbide_menuAddSep( oSubMenu )
   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:addItem( { ::getAction( "InsertSeparator"    ), {|| oIde:execAction( "InsertSeparator"    ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu2:addItem( { ::getAction( "InsertDateTime"     ), {|| oIde:execAction( "InsertDateTime"     ) } } )
// oSubMenu2:addItem( { ::getAction( "InsertRandomName"   ), {|| oIde:execAction( "InsertRandomName"   ) } } )
   oSubMenu2:addItem( { ::getAction( "InsertExternalFile" ), {|| oIde:execAction( "InsertExternalFile" ) } } )
   oSubMenu:addItem( { oSubMenu2,  _T( "~Insert" ) } )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu2:oWidget:addAction( ::oFormatDock:oWidget:toggleViewAction() )
   oSubMenu2:addItem( { ::getAction( "RemoveTabs"         ), {|| oIde:execAction( "RemoveTabs"         ) } } )
   oSubMenu2:addItem( { ::getAction( "Spaces2Tabs"        ), {|| oIde:execAction( "Spaces2Tabs"        ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu2:addItem( { ::getAction( "RemoveTrailingSpaces"), {|| oIde:execAction( "RemoveTrailingSpaces" ) } } )
   oSubMenu2:addItem( { ::getAction( "FormatBraces"       ), {|| oIde:execAction( "FormatBraces"       ) } } )
   oSubMenu2:addItem( { ::getAction( "UpperCaseKeywords"  ), {|| oIde:execAction( "UpperCaseKeywords"  ) } } )
   oSubMenu:addItem( { oSubMenu2,  _T( "~Format" ) } )

   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "switchReadOnly"      ), {|| oIde:execAction( "switchReadOnly"     ) } } )

   /*----------------------------------------------------------------------------*/
   /*                                   View                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~View"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "TB_Hide" ), {|| oIde:execAction( "Hide" ) } }    )

   ::oIde:qAnimateAction := QAction( oSubMenu:oWidget )
   ::qAnimateAction:setText( "Animation" )
   ::qAnimateAction:setCheckable( .t. )
   oSubMenu:addItem( { ::qAnimateAction, {|| oIde:execAction( "Animate" ) } }         )

   hbide_menuAddSep( oSubMenu )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu:addItem( { oSubMenu2, "Toolbars" } )

   oSubMenu2:oWidget:addAction( ::oIde:oMainToolbar:oWidget:toggleViewAction()         )
   oSubMenu2:oWidget:addAction( ::qFilesToolbar:toggleViewAction()                     )
   oSubMenu2:oWidget:addAction( ::qPartsToolbar:toggleViewAction()                     )
   oSubMenu2:oWidget:addAction( ::qProjectToolbar:toggleViewAction()                   )
   oSubMenu2:oWidget:addAction( ::qTBarDocks:toggleViewAction()                        )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:oWidget:addAction( ::qMdiToolbarL:oWidget:toggleViewAction()              )
   oSubMenu2:oWidget:addAction( ::qMdiToolbar:oWidget:toggleViewAction()               )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:addItem( { ::getAction( "ConfigToolbars" ), {|| NIL } }                   )

   hbide_menuAddSep( oSubMenu )

   oSubMenu2 := XbpMenu():new( oSubMenu, , .t. ):create()
   oSubMenu:addItem( { oSubMenu2, "Docking Widgets" } )

   oSubMenu2:oWidget:addAction( ::oDockPT:oWidget:toggleViewAction()                   )
   oSubMenu2:oWidget:addAction( ::oDockED:oWidget:toggleViewAction()                   )
   oSubMenu2:oWidget:addAction( ::oSkltnsTreeDock:oWidget:toggleViewAction()           )
   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:oWidget:addAction( ::oHelpDock:oWidget:toggleViewAction()                 )
   oSubMenu2:oWidget:addAction( ::oDocViewDock:oWidget:toggleViewAction()              )
   oSubMenu2:oWidget:addAction( ::oDocWriteDock:oWidget:toggleViewAction()             )
   oSubMenu2:oWidget:addAction( ::oFuncDock:oWidget:toggleViewAction()                 )
   oSubMenu2:oWidget:addAction( ::oFunctionsDock:oWidget:toggleViewAction()            )
   oSubMenu2:oWidget:addAction( ::oPropertiesDock:oWidget:toggleViewAction()           )
   oSubMenu2:oWidget:addAction( ::oEnvironDock:oWidget:toggleViewAction()              )
   oSubMenu2:oWidget:addAction( ::oSkeltnDock:oWidget:toggleViewAction()               )
   oSubMenu2:oWidget:addAction( ::oThemesDock:oWidget:toggleViewAction()               )
   oSubMenu2:oWidget:addAction( ::oFindDock:oWidget:toggleViewAction()                 )
   oSubMenu2:oWidget:addAction( ::oSourceThumbnailDock:oWidget:toggleViewAction()      )
   oSubMenu2:oWidget:addAction( ::oQScintillaDock:oWidget:toggleViewAction()           )

   oSubMenu2:oWidget:addAction( ::oReportsManagerDock:toggleViewAction()               )
   oSubMenu2:oWidget:addAction( ::oCuiEdDock:toggleViewAction()                        )
   oSubMenu2:oWidget:addAction( ::oIde:oUISrcDock:toggleViewAction()                   )

   hbide_menuAddSep( oSubMenu2 )
   oSubMenu2:oWidget:addAction( ::oDockB2:oWidget:toggleViewAction()                   )
 * oSubMenu:oWidget:addAction( ::oDockB1:oWidget:toggleViewAction()                    )
 * oSubMenu:oWidget:addAction( ::oDockB:oWidget:toggleViewAction()                     )

    hbide_menuAddSep( oSubMenu )

   ::oIde:qStatusBarAction := QAction( oSubMenu:oWidget )
   ::qStatusBarAction:setText( "Statusbar" )
   ::qStatusBarAction:setCheckable( .t. )
   oSubMenu:addItem( { ::qStatusBarAction, {|| oIde:execAction( "ToggleStatusBar" ) } } )
   ::qStatusBarAction:setChecked( ::lStatusBarVisible )

   /*----------------------------------------------------------------------------*/
   /*                                   Project                                  */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~Project"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "Properties"          ), {|| oIde:execAction( "Properties"     ) } } )
#if 0
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "ProjAddSource"       ), {|| oIde:execAction( "ProjAddSource"  ) } } )
   oSubMenu:addItem( { ::getAction( "ProjRemSource"       ), {|| oIde:execAction( "ProjRemSource"  ) } } )
   oSubMenu:addItem( { ::getAction( "ProjMainModule"      ), {|| oIde:execAction( "ProjMainModule" ) } } )
   oSubMenu:disableItem( oSubMenu:numItems )
#endif
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "SelectProject"       ), {|| oIde:execAction( "SelectProject"  ) } } )
   oSubMenu:addItem( { ::getAction( "CloseProject"        ), {|| oIde:execAction( "CloseProject"   ) } } )
   hbide_menuAddSep( oSubMenu )
// oSubMenu:addItem( { ::getAction( "Environments"        ), {|| oIde:execAction( "Environments"   ) } } )
   oSubMenu:addItem( { ::getAction( "NewProject"          ), {|| oIde:oPWZ:show() } } )

   /*----------------------------------------------------------------------------*/
   /*                                   Build                                    */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~Build"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "Compile"             ), {|| oIde:execAction( "Compile"            ) } } )
   oSubMenu:addItem( { ::getAction( "CompilePPO"          ), {|| oIde:execAction( "CompilePPO"         ) } } )
   oSubMenu:addItem( { ::getAction( "BuildSource"         ), {|| oIde:execAction( "BuildSource"        ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "Build"               ), {|| oIde:execAction( "Build"              ) } } )
   oSubMenu:addItem( { ::getAction( "BuildLaunch"         ), {|| oIde:execAction( "BuildLaunch"        ) } } )
   oSubMenu:addItem( { ::getAction( "Rebuild"             ), {|| oIde:execAction( "Rebuild"            ) } } )
   oSubMenu:addItem( { ::getAction( "RebuildLaunch"       ), {|| oIde:execAction( "RebuildLaunch"      ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "LaunchProject"       ), {|| oIde:execAction( "LaunchProject"      ) } } )

   /*----------------------------------------------------------------------------*/
   /*                                   Setup                                    */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~Setup"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "Setup"               ), {|| oIde:execAction( "Setup"              ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "Shortcuts"           ), {|| oIde:execAction( "Shortcuts"          ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "TB_Tools"            ), {|| oIde:execAction( "Tools"              ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "ChangeLog"           ), {|| oIde:execAction( "ChangeLog"          ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu2 := hbide_buildCDPMenu( oIde, oSubMenu ) //hbide_buildCodecMenu( oIde, oSubMenu )
//   oSubMenu2:title := "~CodePage"
   oSubMenu:addItem( { oSubMenu2, _T( "~CodePage" ) } )

   /*----------------------------------------------------------------------------*/
   /*                                   Help                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar, , .t. ):create()
   oSubMenu:title := "~Help"
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu:addItem( { ::getAction( "AboutIDE"            ), {|| hbide_help( 1 ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "AboutHarbour"        ), {|| hbide_help( 4 ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { ::getAction( "HarbourUsersList"    ), {|| hbide_help( 3 ) } } )
   oSubMenu:addItem( { ::getAction( "HarbourDevList"      ), {|| hbide_help( 2 ) } } )

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Normalizes a caption for an menu item with shortcut (or not).
 * TODO: add support for translation of menu items AND support changing shortcuts
 *       loading from a text file for customing hotkeys AND icons. (vailtom)
 * 27/12/2009 - 16:05:32 - vailtom
 */
STATIC FUNCTION mnuNormalizeItem( cCaption )
   LOCAL cKey, cIco, p

   /* Retrieve and update the ICON name for this menu item */
   IF ( ( p := Rat( '|', cCaption ) ) != 00 )
      cIco := Substr( cCaption, p + 1 )
      cIco := alltrim( cIco )

      cCaption := Substr( cCaption, 1, p - 1 )
      cCaption := Alltrim( cCaption )

      IF !Empty( cIco )
         cIco := StrTran( cIco, '/', hb_ps() )
         cIco := StrTran( cIco, '\', hb_ps() )

         IF !( hb_ps() $ cIco )
            cIco := ":/resources" + hb_ps() + cIco + "|"
         ELSE
            cIco := cIco + "|"
         Endif
      Endif
   ELSE
      cIco := ''
   ENDIF

   /* Update the key shortcut for this menu item */
   IF ( ( p := Rat( ',', cCaption ) ) != 00 )
      cKey     := Substr( cCaption, p + 1 )
      cCaption := Substr( cCaption, 1, p - 1 )
      cCaption := alltrim( cCaption )

      cKey := alltrim( cKey )
      cKey := StrTran( cKey, '^', 'Ctrl+' )
      cKey := StrTran( cKey, 'Sh+', 'Shift+' )

      IF !Empty( cKey )
         cKey := Chr( K_TAB ) + cKey
      End
   ELSE
      cKey := ''
   ENDIF

   RETURN cIco + cCaption + cKey

/*----------------------------------------------------------------------*/
/*
 * Add a file name to MRU menu item.
 * 02/01/2010 - 23:23:22 - vailtom
 */
FUNCTION hbide_mnuAddFileToMRU( oIde, cFileName, cType )
   LOCAL nPos, cFileNormal, a_

   cFileNormal := hbide_pathNormalized( cFileName )

   IF cType == "recent_projects"
      a_:= oIde:oINI:aRecentProjects
   ELSE
      a_:= oIde:oINI:aRecentFiles
   ENDIF

   IF ( nPos := aScan( a_, {|f| hb_FileMatch( hbide_pathNormalized( f ), cFileNormal ) } ) ) > 0
      hb_aDel( a_, nPos, .T. )
   ENDIF

   ASize( a_, Len( a_ ) + 1 )
   AIns( a_, 1 )
   a_[ 1 ] := cFileName

   IF Len( a_ ) > 25
      aSize( a_, 25 )
   ENDIF

   IF nPos == 0
      hbide_mnuUpdateMRUpopup( oIde, cType )
   ENDIF
   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * 02/01/2010 - 22:44:19
 */
#define QMF_POPUP  1

STATIC FUNCTION hbide_mnuUpdateMRUpopup( oIde, cType )
   LOCAL oMenuBar, oItem, cFindStr, nPos, n, c, a_

   IF Empty( oIde:oDlg )
      RETURN NIL
   ENDIF

   IF cType == "recent_projects"
      a_:= oIde:oINI:aRecentProjects
   ELSE
      a_:= oIde:oINI:aRecentFiles
   ENDIF

   oMenuBar := oIde:oDlg:MenuBar()
   nPos     := 0
   cFindStr := iif( cType == "recent_files", 'RECENT FILES', 'RECENT PROJECTS' )

   FOR n := 1 TO oMenuBar:numItems()
       IF oMenuBar:aMenuItems[ n, 1 ] != QMF_POPUP
          LOOP
       ENDIF

       oItem := oMenuBar:aMenuItems[ n ]
       c := Upper( oItem[ 3 ] )
       c := StrTran( c, '~', '' )
       c := StrTran( c, '&', '' )

       IF cFindStr == alltrim( c )
          nPos := n
          EXIT
       ENDIF
   NEXT

   IF nPos == 0
      RETURN nil
   ENDIF

   oItem[ 4 ]:delAllItems()

   IF !empty( a_ )
      FOR n := 1 TO Len( a_ )
          c := hbide_pathNormalized( a_[ n ], .F. )

          oItem[ 4 ]:addItem( { _T( '~' + hb_NumToHex( n ) + '. ' + c ), nil } )

          IF !hb_FileExists( c )
             oItem[ 4 ]:disableItem( n )
          ENDIF
      NEXT
   ELSE
      IF cType == "recent_files"
         oItem[ 4 ]:addAction( "** No recent files found **" )
      ELSE
         oItem[ 4 ]:addAction( "** No recent projects found **" )
      ENDIF
      oItem[ 4 ]:disableItem( 1 )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * Find a menu item with same caption as passed on argument.
 * 03/01/2010 - 13:12:42
 */
FUNCTION hbide_mnuFindItem( oIde, cCaption )
   LOCAL oMenuBar, oItem, n, c

   IF Empty( oIde:oDlg )
      RETURN nil
   ENDIF

   oMenuBar := oIde:oDlg:MenuBar()
   cCaption := Alltrim( Upper( cCaption ) )

   FOR n := 1 TO oMenuBar:numItems()

      oItem := oMenuBar:aMenuItems[ n ]
      c := Upper( oItem[ 3 ] )
      c := StrTran( c, '~', '' )
      c := StrTran( c, '&', '' )

      IF cCaption == alltrim( c )
         RETURN oItem
      ENDIF
   NEXT

   RETURN nil

/*----------------------------------------------------------------------*/

#include "hbextcdp.ch"

STATIC FUNCTION hbide_buildCDPMenu( oIde, oMenu )
   LOCAL oSubMenu, cdp

   oSubMenu := XbpMenu():new( oMenu, , .t. ):create()

   FOR EACH cdp IN get_list_of_real_codepages()
      oSubMenu:addItem( { hb_cdpUniID( cdp ), get_cdp_block( oIde, hb_cdpUniID( cdp ) ) } )
   NEXT

   RETURN oSubMenu

STATIC FUNCTION get_cdp_block( oIde, cCodePage )
   RETURN {|| oIde:setCodec( cCodePage ) }

STATIC FUNCTION get_list_of_real_codepages()
   LOCAL cdp

   STATIC s_uni

   IF empty( s_uni )
      s_uni := { => }
      FOR EACH cdp IN hb_cdpList()
         s_uni[ hb_cdpUniID( cdp ) ] := cdp
      NEXT
   ENDIF

   RETURN s_uni

FUNCTION hbide_getCDPforID( cCodec )
   LOCAL cdp

   FOR EACH cdp IN hb_cdpList()
      IF hb_cdpUniID( cdp ) == cCodec
         RETURN cdp
      ENDIF
   NEXT

   RETURN cCodec

/*----------------------------------------------------------------------*/

/*
STATIC FUNCTION hbide_buildCodecMenu( oIde, oMenu )
   LOCAL oSubMenu, oSub1

   oSubMenu := XbpMenu():new( oMenu ):create()

   oSubMenu:addItem( { "Apple Roman "         , {|| oIde:setCodec( "Apple Roman"         ) } } )
   oSubMenu:addItem( { "Big5        "         , {|| oIde:setCodec( "Big5"                ) } } )
   oSubMenu:addItem( { "Big5-HKSCS  "         , {|| oIde:setCodec( "Big5-HKSCS"          ) } } )
   oSubMenu:addItem( { "CP949       "         , {|| oIde:setCodec( "CP949"               ) } } )
   oSubMenu:addItem( { "EUC-JP      "         , {|| oIde:setCodec( "EUC-JP"              ) } } )
   oSubMenu:addItem( { "EUC-KR      "         , {|| oIde:setCodec( "EUC-KR"              ) } } )
   oSubMenu:addItem( { "GB18030-0   "         , {|| oIde:setCodec( "GB18030-0"           ) } } )
   oSubMenu:addItem( { "IBM 850     "         , {|| oIde:setCodec( "IBM 850"             ) } } )
   oSubMenu:addItem( { "IBM 866     "         , {|| oIde:setCodec( "IBM 866"             ) } } )
   oSubMenu:addItem( { "IBM 874     "         , {|| oIde:setCodec( "IBM 874"             ) } } )
   oSubMenu:addItem( { "ISO 2022-JP "         , {|| oIde:setCodec( "ISO 2022-JP"         ) } } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "ISO 8859-1 to 10"
   oSub1:addItem( { "ISO 8859-1"              , {|| oIde:setCodec( "ISO 8859-1"          ) } } )
   oSub1:addItem( { "ISO 8859-2"              , {|| oIde:setCodec( "ISO 8859-2"          ) } } )
   oSub1:addItem( { "ISO 8859-3"              , {|| oIde:setCodec( "ISO 8859-3"          ) } } )
   oSub1:addItem( { "ISO 8859-4"              , {|| oIde:setCodec( "ISO 8859-4"          ) } } )
   oSub1:addItem( { "ISO 8859-5"              , {|| oIde:setCodec( "ISO 8859-5"          ) } } )
   oSub1:addItem( { "ISO 8859-6"              , {|| oIde:setCodec( "ISO 8859-6"          ) } } )
   oSub1:addItem( { "ISO 8859-7"              , {|| oIde:setCodec( "ISO 8859-7"          ) } } )
   oSub1:addItem( { "ISO 8859-8"              , {|| oIde:setCodec( "ISO 8859-8"          ) } } )
   oSub1:addItem( { "ISO 8859-9"              , {|| oIde:setCodec( "ISO 8859-9"          ) } } )
   oSub1:addItem( { "ISO 8859-10"             , {|| oIde:setCodec( "ISO 8859-10"         ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "ISO 8859-13 to 16"
   oSub1:addItem( { "ISO 8859-13"             , {|| oIde:setCodec( "ISO 8859-13"         ) } } )
   oSub1:addItem( { "ISO 8859-14"             , {|| oIde:setCodec( "ISO 8859-14"         ) } } )
   oSub1:addItem( { "ISO 8859-15"             , {|| oIde:setCodec( "ISO 8859-15"         ) } } )
   oSub1:addItem( { "ISO 8859-16"             , {|| oIde:setCodec( "ISO 8859-16"         ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "Iscii"
   oSub1:addItem( { "Iscii-Bng"               , {|| oIde:setCodec( "Iscii-Bng"           ) } } )
   oSub1:addItem( { "Iscii-Dev"               , {|| oIde:setCodec( "Iscii-Dev"           ) } } )
   oSub1:addItem( { "Iscii-Gjr"               , {|| oIde:setCodec( "Iscii-Gjr"           ) } } )
   oSub1:addItem( { "Iscii-Knd"               , {|| oIde:setCodec( "Iscii-Knd"           ) } } )
   oSub1:addItem( { "Iscii-Mlm"               , {|| oIde:setCodec( "Iscii-Mlm"           ) } } )
   oSub1:addItem( { "Iscii-Ori"               , {|| oIde:setCodec( "Iscii-Ori"           ) } } )
   oSub1:addItem( { "Iscii-Pnj"               , {|| oIde:setCodec( "Iscii-Pnj"           ) } } )
   oSub1:addItem( { "Iscii-Tlg"               , {|| oIde:setCodec( "Iscii-Tlg"           ) } } )
   oSub1:addItem( { "Iscii-Tml"               , {|| oIde:setCodec( "Iscii-Tml"           ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSubMenu:addItem( { "JIS X 0201  "         , {|| oIde:setCodec( "JIS X 0201"          ) } } )
   oSubMenu:addItem( { "JIS X 0208  "         , {|| oIde:setCodec( "JIS X 0208"          ) } } )
   oSubMenu:addItem( { "KOI8-R      "         , {|| oIde:setCodec( "KOI8-R"              ) } } )
   oSubMenu:addItem( { "KOI8-U      "         , {|| oIde:setCodec( "KOI8-U"              ) } } )
   oSubMenu:addItem( { "MuleLao-1   "         , {|| oIde:setCodec( "MuleLao-1"           ) } } )
   oSubMenu:addItem( { "ROMAN8      "         , {|| oIde:setCodec( "ROMAN8"              ) } } )
   oSubMenu:addItem( { "Shift-JIS   "         , {|| oIde:setCodec( "Shift-JIS"           ) } } )
   oSubMenu:addItem( { "TIS-620     "         , {|| oIde:setCodec( "TIS-620"             ) } } )
   oSubMenu:addItem( { "TSCII       "         , {|| oIde:setCodec( "TSCII"               ) } } )
   oSubMenu:addItem( { "UTF-8       "         , {|| oIde:setCodec( "UTF-8"               ) } } )
   oSubMenu:addItem( { "UTF-16      "         , {|| oIde:setCodec( "UTF-16"              ) } } )
   oSubMenu:addItem( { "UTF-16BE    "         , {|| oIde:setCodec( "UTF-16BE"            ) } } )
   oSubMenu:addItem( { "UTF-16LE    "         , {|| oIde:setCodec( "UTF-16LE"            ) } } )
   oSubMenu:addItem( { "UTF-32      "         , {|| oIde:setCodec( "UTF-32"              ) } } )
   oSubMenu:addItem( { "UTF-32BE    "         , {|| oIde:setCodec( "UTF-32BE"            ) } } )
   oSubMenu:addItem( { "UTF-32LE    "         , {|| oIde:setCodec( "UTF-32LE"            ) } } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "Windows-1250 to 1258"
   oSub1:addItem( { "Windows-1250"            , {|| oIde:setCodec( "Windows-1250"        ) } } )
   oSub1:addItem( { "Windows-1251"            , {|| oIde:setCodec( "Windows-1251"        ) } } )
   oSub1:addItem( { "Windows-1252"            , {|| oIde:setCodec( "Windows-1252"        ) } } )
   oSub1:addItem( { "Windows-1253"            , {|| oIde:setCodec( "Windows-1253"        ) } } )
   oSub1:addItem( { "Windows-1254"            , {|| oIde:setCodec( "Windows-1254"        ) } } )
   oSub1:addItem( { "Windows-1255"            , {|| oIde:setCodec( "Windows-1255"        ) } } )
   oSub1:addItem( { "Windows-1256"            , {|| oIde:setCodec( "Windows-1256"        ) } } )
   oSub1:addItem( { "Windows-1257"            , {|| oIde:setCodec( "Windows-1257"        ) } } )
   oSub1:addItem( { "Windows-1258"            , {|| oIde:setCodec( "Windows-1258"        ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSubMenu:addItem( { "WINSAMI2    "         , {|| oIde:setCodec( "WINSAMI2"            ) } } )

   RETURN oSubMenu
*/

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolBars()

   ::buildToolbarMain()
   ::buildToolbarFiles()
   ::buildToolbarParts()
   ::buildToolbarProject()
   ::buildToolBarDocks()

   /* User defined toolbars via Tools & Utilities */
   ::oTM:buildUserToolbars()

   ::buildToolbarSelectedText()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolbarMain()
   LOCAL oTBar
   LOCAL nSep := XBPTOOLBAR_BUTTON_SEPARATOR

   oTBar := XbpToolBar():new( ::oDlg )
   oTBar:imageWidth  := 12
   oTBar:imageHeight := 12
   oTBar:create( , , { 0, ::oDlg:currentSize()[ 2 ]-60 }, { ::oDlg:currentSize()[ 1 ], 60 } )
   oTBar:oWidget:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )
   oTBar:oWidget:setAllowedAreas( Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea )
   oTBar:oWidget:setFocusPolicy( Qt_NoFocus )

   oTBar:buttonClick := {|oButton| ::oIde:execAction( oButton:key ) }

   oTBar:addItem( ::getAction( "TB_Exit"              ), , , , , , "Exit"              )
   oTBar:addItem( ::getAction( "TB_Hide"              ), , , , , , "Hide"              )
   oTBar:addItem( ::getAction( "TB_Home"              ), , , , , , "Home"              )
   oTBar:addItem( , , , , , nSep )
   ::oActToolsBtn := oTBar:oWidget:addWidget( ::oIde:oTM:buildToolsButton() )
   oTBar:addItem( , , , , , nSep )
   ::oActToolsBtn := oTBar:oWidget:addWidget( ::oIde:oTM:buildViewsButton() )

   ::oIde:oMainToolbar := oTBar

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolbarFiles()

   ::qFilesToolbar := HBQToolBar():new( "ToolBar_Files" )

   ::qFilesToolbar:cName := "ToolBar_Files"
   ::qFilesToolbar:allowedAreas := Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea
   ::qFilesToolbar:size := QSize( 12, 12 )

   ::qFilesToolbar:create()

   ::qFilesToolbar:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qFilesToolbar:setWindowTitle( "Ide Files" )
   ::qFilesToolbar:setToolButtonStyle( Qt_ToolButtonIconOnly )

   ::qFilesToolbar:addAction( "IdeNew" , ::getAction( "TB_New"   ), {|| ::oIde:execAction( "new"   ) } )
   ::qFilesToolbar:addAction( "IdeOpen", ::getAction( "TB_Open"  ), {|| ::oIde:execAction( "Open"  ) } )
   ::qFilesToolbar:addAction( "IdeOpen", ::getAction( "TB_Save"  ), {|| ::oIde:execAction( "Save"  ) } )
   ::qFilesToolbar:addAction( "IdeOpen", ::getAction( "TB_Close" ), {|| ::oIde:execAction( "Close" ) } )
   ::qFilesToolbar:addAction( "IdeOpen", ::getAction( "TB_Print" ), {|| ::oIde:execAction( "Print" ) } )

   ::oDlg:oWidget:addToolBar( Qt_TopToolBarArea, ::qFilesToolbar:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolbarParts()

   ::qPartsToolbar := HBQToolBar():new( "ToolBar_Parts" )

   ::qPartsToolbar:cName := "ToolBar_Parts"
   ::qPartsToolbar:allowedAreas := Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea
   ::qPartsToolbar:size := QSize( 12, 12 )

   ::qPartsToolbar:create()

   ::qPartsToolbar:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qPartsToolbar:setWindowTitle( "Ide Parts" )
   ::qPartsToolbar:setToolButtonStyle( Qt_ToolButtonIconOnly )

   ::qPartsToolbar:addAction( "IdeDBU"   , ::getAction( "DBU"    ), {|| ::oIde:execAction( "DBU"    ) } )
   ::qPartsToolbar:addAction( "IdeEDITOR", ::getAction( "EDITOR" ), {|| ::oIde:execAction( "EDITOR" ) } )

   ::oDlg:oWidget:addToolBar( Qt_TopToolBarArea, ::qPartsToolbar:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolbarProject()
   LOCAL oTBar

   ::qProjectToolbar := HBQToolBar():new( "ToolBar_Docks" )

   ::qProjectToolbar:cName := "ToolBar_Project"
   ::qProjectToolbar:allowedAreas := Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea
   ::qProjectToolbar:size := QSize( 12, 12 )

   ::qProjectToolbar:create()

   ::qProjectToolbar:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qProjectToolbar:setWindowTitle( "Ide Projects" )
   ::qProjectToolbar:setToolButtonStyle( Qt_ToolButtonIconOnly )

   oTBar := ::qProjectToolbar

   oTBar:addAction( "TB_Script"       , ::getAction( "TB_Script"        ), {|| ::oIde:execAction( "RunAsScript"   ) } )
   oTBar:addAction( "TB_Compile"      , ::getAction( "TB_Compile"       ), {|| ::oIde:execAction( "Compile"       ) } )
   oTBar:addAction( "TB_CompilePPO"   , ::getAction( "TB_CompilePPO"    ), {|| ::oIde:execAction( "CompilePPO"    ) } )
   oTBar:addAction( "TB_BuildSource"  , ::getAction( "TB_BuildSource"   ), {|| ::oIde:execAction( "BuildSource"   ) } )
   oTBar:addAction( "TB_Build"        , ::getAction( "TB_Build"         ), {|| ::oIde:execAction( "Build"         ) } )
   oTBar:addAction( "TB_BuildLaunch"  , ::getAction( "TB_BuildLaunch"   ), {|| ::oIde:execAction( "BuildLaunch"   ) } )
   oTBar:addAction( "TB_Rebuild"      , ::getAction( "TB_Rebuild"       ), {|| ::oIde:execAction( "Rebuild"       ) } )
   oTBar:addAction( "TB_RebuildLaunch", ::getAction( "TB_RebuildLaunch" ), {|| ::oIde:execAction( "RebuildLaunch" ) } )

  ::oDlg:oWidget:addToolBar( Qt_TopToolBarArea, ::qProjectToolbar:oWidget )

  RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolBarDocks()
   LOCAL a_, qAct
   LOCAL aBtns := {}

   /* Right-hand docks toolbar */

   aadd( aBtns, { ::oDockPT             , "projtree"      } )
   aadd( aBtns, { ::oDockED             , "editstree"     } )
   aadd( aBtns, { ::oSkltnsTreeDock     , "projtree"      } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oHelpDock           , "help"          } )
   aadd( aBtns, { ::oDocViewDock        , "harbourhelp"   } )
   aadd( aBtns, { ::oDocWriteDock       , "docwriter"     } )
   aadd( aBtns, { ::oFuncDock           , "dc_function"   } )
   aadd( aBtns, { ::oFunctionsDock      , "ffn"           } )
   aadd( aBtns, { ::oPropertiesDock     , "properties"    } )
   aadd( aBtns, { ::oEnvironDock        , "envconfig"     } )
   aadd( aBtns, { ::oSkeltnDock         , "codeskeletons" } )
   aadd( aBtns, { ::oThemesDock         , "syntaxhiliter" } )
   aadd( aBtns, { ::oFindDock           , "search"        } )
   aadd( aBtns, { ::oSourceThumbnailDock, "thumbnail"     } )
   aadd( aBtns, { ::oQScintillaDock     , "browser"       } )
   aadd( aBtns, { ::oReportsManagerDock , "designer"      } )
   aadd( aBtns, { ::oCuiEdDock          , "cuied"         } )
   aadd( aBtns, { ::oUiSrcDock          , "fileprg"       } )
   aadd( aBtns, {} )
   aadd( aBtns, { ::oDockB2             , "builderror"    } )

   ::qTBarDocks := HBQToolBar():new( "ToolBar_Docks" )

   ::qTBarDocks:cName := "ToolBar_Docks"
   ::qTBarDocks:allowedAreas := Qt_LeftToolBarArea + Qt_RightToolBarArea + Qt_TopToolBarArea + Qt_BottomToolBarArea
   ::qTBarDocks:size := QSize( 12,12 )

   ::qTBarDocks:create()

   ::qTBarDocks:setStyleSheet( GetStyleSheet( "QToolBarLR5", ::nAnimantionMode ) )
   ::qTBarDocks:setWindowTitle( "Dockable Widgets" )
   ::qTBarDocks:setToolButtonStyle( Qt_ToolButtonIconOnly )

   FOR EACH a_ IN aBtns
      IF empty( a_ )
         ::qTBarDocks:addSeparator()
      ELSE
         qAct := a_[ 1 ]:oWidget:toggleViewAction()
         qAct:setIcon( QIcon( hbide_image( a_[ 2 ] ) ) )
         ::qTBarDocks:addAction( a_[ 2 ], qAct )
      ENDIF
   NEXT

   ::oDlg:oWidget:addToolBar( Qt_TopToolBarArea, ::qTBarDocks:oWidget )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildMdiToolbarLeft()

   ::qMdiToolbarL := HbqToolbar():new()
   ::qMdiToolbarL:orientation := Qt_Vertical
   ::qMdiToolbarL:size := QSize(  val( ::oINI:cToolbarSize ), val( ::oINI:cToolbarSize ) )
   ::qMdiToolbarL:create( "EditsManager_Left_Toolbar" )
   ::qMdiToolbarL:setWindowTitle( "Editing Area's Left" )
   ::qMdiToolbarL:setObjectName( "ToolbarEditingAreaLeft" )
   ::qMdiToolbarL:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )

   ::qMdiToolbarL:addToolButton( "ViewTabbed"     , "Toggle tabbed view"         , hbide_image( "view_tabbed"      ), {|| ::oDK:execEvent( __buttonViewTabbed_clicked__      ) }, .f. )

   ::qMdiToolbarL:addToolButton( "ViewArranged"   , "View as arranged"           , hbide_image( "view_organized"   ), {|| ::oDK:execEvent( __buttonViewOrganized_clicked__   ) }, .f. )
   ::qMdiToolbarL:addToolButton( "SaveLayout"     , "Save layout"                , hbide_image( "save3"            ), {|| ::oDK:execEvent( __buttonSaveLayout_clicked__      ) }, .f. )

   ::qMdiToolbarL:addToolButton( "ViewCascaded"   , "View as cascaded"           , hbide_image( "view_cascaded"    ), {|| ::oDK:execEvent( __buttonViewCascaded_clicked__    ) }, .f. )
   ::qMdiToolbarL:addToolButton( "viewTiled"      , "View as tiled"              , hbide_image( "view_tiled"       ), {|| ::oDK:execEvent( __buttonViewTiled_clicked__       ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewMaximized"  , "View Maximized"             , hbide_image( "fullscreen"       ), {|| ::oDK:execEvent( __buttonViewMaximized_clicked__   ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewTiledVert"  , "View Vertically Tiled"      , hbide_image( "view_vertstacked" ), {|| ::oDK:execEvent( __buttonViewStackedVert_clicked__ ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewTiledHorz"  , "View Horizontally Tiled"    , hbide_image( "view_horzstacked" ), {|| ::oDK:execEvent( __buttonViewStackedHorz_clicked__ ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewZoomedIn"   , "View Zoom In"               , hbide_image( "view_zoomin"      ), {|| ::oDK:execEvent( __buttonViewZoomedIn_clicked__    ) }, .f. )
   ::qMdiToolbarL:addToolButton( "ViewZoomedOut"  , "View Zoom Out"              , hbide_image( "view_zoomout"     ), {|| ::oDK:execEvent( __buttonViewZoomedOut_clicked__   ) }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ToggleLineNos"  , "Toggle Line Numbers"        , hbide_image( "togglelinenumber" ), {|| ::oEM:toggleLineNumbers()                      }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleHorzRuler", "Toggle Horizontal Ruler"    , hbide_image( "horzruler"        ), {|| ::oEM:toggleHorzRuler()                        }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleCurLine"  , "Toggle Current Line Hilight", hbide_image( "curlinehilight"   ), {|| ::oEM:toggleCurrentLineHighlightMode()         }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ToggleCodeComp" , "Toggle Code Completion"     , hbide_image( "help1"            ), {|| ::oEM:toggleCodeCompetion()                    }, .f. )
   ::qMdiToolbarL:addToolButton( "ToggleCompTips" , "Toggle Completion Tips"     , hbide_image( "infotips"         ), {|| ::oEM:toggleCompetionTips()                    }, .f. )
   ::qMdiToolbarL:addSeparator()
   ::qMdiToolbarL:addToolButton( "ZoomIn"         , "Zoom In"                    , hbide_image( "zoomin3"          ), {|| ::oEM:zoom( +1 )                               }, .f. )
   ::qMdiToolbarL:addToolButton( "ZoomOut"        , "Zoom Out"                   , hbide_image( "zoomout3"         ), {|| ::oEM:zoom( -1 )                               }, .f. )
   ::qMdiToolbarL:addSeparator()

   IF ! ::oINI:lShowEditsLeftToolbar
      ::qMdiToolbarL:hide()
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeActions:buildMdiToolbar()
   LOCAL qTBar, nW := 25

   STATIC sp0,sp1,sp2,sp3
   IF empty( sp0 )
      sp0 := QLabel(); sp0:setMinimumWidth( nW )
      sp1 := QLabel(); sp1:setMinimumWidth( nW )
      sp2 := QLabel(); sp2:setMinimumWidth( nW )
      sp3 := QLabel(); sp3:setMinimumWidth( nW )
   ENDIF

   ::qMdiToolbar := HbqToolbar():new()
   ::qMdiToolbar:orientation := Qt_Horizontal
   ::qMdiToolbar:size := QSize(  val( ::oINI:cToolbarSize ), val( ::oINI:cToolbarSize ) )
   ::qMdiToolbar:create( "EditsManager_Top_Toolbar" )
   ::qMdiToolbar:setStyleSheet( GetStyleSheet( "QToolBar", ::nAnimantionMode ) )
   ::qMdiToolbar:setObjectName( "ToolbarEditingAreaTop" )
   ::qMdiToolbar:setWindowTitle( "Editing Area's Top" )

   qTBar := ::qMdiToolbar

   qTBar:addWidget( "Panels", ::oIde:oTM:buildPanelsButton() )
   qTBar:addWidget( "Label0", sp0 )
   qTBar:addToolButton( "Undo"      , "Undo"                       , hbide_image( "undo"          ), {|| ::oEM:undo()                        }, .f. )
   qTBar:addToolButton( "Redo"      , "Redo"                       , hbide_image( "redo"          ), {|| ::oEM:redo()                        }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "Cut"       , "Cut"                        , hbide_image( "cut"           ), {|| ::oEM:cut()                         }, .f. )
   qTBar:addToolButton( "Copy"      , "Copy"                       , hbide_image( "copy"          ), {|| ::oEM:copy()                        }, .f. )
   qTBar:addToolButton( "Paste"     , "Paste"                      , hbide_image( "paste"         ), {|| ::oEM:paste()                       }, .f. )
   qTBar:addToolButton( "SelectAll" , "Select all"                 , hbide_image( "selectall"     ), {|| ::oEM:selectAll()                   }, .f. )
   qTBar:addToolButton( "SelectionMode", "Selection mode"          , hbide_image( "stream"        ), {|| ::oEM:toggleSelectionMode(), ::oIDE:manageFocusInEditor() }, .t. )
   qTBar:addWidget( "Label1", sp1 )
   qTBar:addToolButton( "Find"      , "Find / Replace"             , hbide_image( "find"          ), {|| ::oEM:find()                        }, .f. )
   qTBar:addToolButton( "BookMark"  , "Toggle Mark"                , hbide_image( "bookmark"      ), {|| ::oEM:setMark()                     }, .f. )
   qTBar:addToolButton( "GotoLine"  , "Goto Line"                  , hbide_image( "gotoline3"     ), {|| ::oEM:goTo()                        }, .f. )
   qTBar:addToolButton( "Reload"    , "Reload Source"              , hbide_image( "view_refresh"  ), {|| ::oEM:reload()                      }, .f. )
   qTBar:addWidget( "Label2", sp2 )
   qTBar:addToolButton( "MoveUp"    , "Move Current Line Up"       , hbide_image( "movelineup"    ), {|| ::oEM:moveLine( -1 )                }, .f. )
   qTBar:addToolButton( "MoveDn"    , "Move Current Line Down"     , hbide_image( "movelinedown"  ), {|| ::oEM:moveLine(  1 )                }, .f. )
   qTBar:addToolButton( "DelLine"   , "Delete Current Line"        , hbide_image( "deleteline"    ), {|| ::oEM:deleteLine()                  }, .f. )
   qTBar:addToolButton( "Duplicate" , "Duplicate Current Line"     , hbide_image( "duplicateline" ), {|| ::oEM:duplicateLine()               }, .f. )
   qTBar:addWidget( "Label3", sp3 )
   qTBar:addToolButton( "ToUpper"   , "To Upper"                   , hbide_image( "toupper"       ), {|| ::oEM:convertSelection( "ToUpper" ) }, .f. )
   qTBar:addToolButton( "ToLower"   , "To Lower"                   , hbide_image( "tolower"       ), {|| ::oEM:convertSelection( "ToLower" ) }, .f. )
   qTBar:addToolButton( "InvertCase", "Invert Case"                , hbide_image( "invertcase"    ), {|| ::oEM:convertSelection( "Invert"  ) }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "BlockCmnt" , "Block Comment"              , hbide_image( "blockcomment"  ), {|| ::oEM:blockComment()                }, .f. )
   qTBar:addToolButton( "StreamCmnt", "Stream Comment"             , hbide_image( "streamcomment" ), {|| ::oEM:streamComment()               }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "IndentR"   , "Indent Right"               , hbide_image( "blockindentr"  ), {|| ::oEM:indent(  1 )                  }, .f. )
   qTBar:addToolButton( "IndentL"   , "Indent Left"                , hbide_image( "blockindentl"  ), {|| ::oEM:indent( -1 )                  }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "Sgl2Dbl"   , "Single to Double Quotes"    , hbide_image( "sgl2dblquote"  ), {|| ::oEM:convertDQuotes()              }, .f. )
   qTBar:addToolButton( "Dbl2Sgl"   , "Double to Single Quotes"    , hbide_image( "dbl2sglquote"  ), {|| ::oEM:convertQuotes()               }, .f. )
   qTBar:addToolButton( "Stringify" , "Stringify Selection"        , hbide_image( "stringify"     ), {|| ::oEM:stringify()                   }, .f. )
   qTBar:addSeparator()
   qTBar:addToolButton( "AlignAt"   , "Align At..."                , hbide_image( "align_at"      ), {|| ::oEM:alignAt()                     }, .f. )

   IF ! ::oINI:lShowEditsTopToolbar
      ::qMdiToolbar:hide()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeActions:buildToolbarSelectedText()
   LOCAL qTBar

   ::qSelToolbar := HbqToolbar():new( "ToolbarSelectedText", ::oDlg:oWidget )
   ::qSelToolbar:orientation := Qt_Horizontal
   ::qSelToolbar:size := QSize(  val( ::oINI:cToolbarSize ), val( ::oINI:cToolbarSize ) )
   ::qSelToolbar:create( "SelectedText_Toolbar" )
   ::qSelToolbar:setObjectName( "ToolbarSelectedText" )
   ::qSelToolbar:setWindowTitle( "Actions on Selected Text" )
   //::qSelToolbar:setWindowFlags( hb_bitOr( Qt_Tool, Qt_CustomizeWindowHint, Qt_WindowTitleHint, Qt_WindowCloseButtonHint ) )
   ::qSelToolbar:setWindowFlags( hb_bitOr( Qt_Tool, Qt_CustomizeWindowHint ) )
   ::qSelToolbar:setMovable( .T. )
   ::qSelToolbar:setFloatable( .T. )
   ::qSelToolbar:hide()

   qTBar := ::qSelToolbar

   qTBar:addToolButton( "Undo"      , "Undo"                       , hbide_image( "undo"          ), {|| ::oEM:undo()                        }, .f. )
   qTBar:addToolButton( "Redo"      , "Redo"                       , hbide_image( "redo"          ), {|| ::oEM:redo()                        }, .f. )
   qTBar:addToolButton( "Cut"       , "Cut"                        , hbide_image( "cut"           ), {|| ::oEM:cut()                         }, .f. )
   qTBar:addToolButton( "Copy"      , "Copy"                       , hbide_image( "copy"          ), {|| ::oEM:copy()                        }, .f. )
   qTBar:addToolButton( "Paste"     , "Paste"                      , hbide_image( "paste"         ), {|| ::oEM:paste()                       }, .f. )
   qTBar:addToolButton( "SelMode"   , "Selection Mode"             , hbide_image( "stream"        ), {|| ::oEM:toggleSelectionMode(), ::oIDE:manageFocusInEditor() }, .t. )
   qTBar:addToolButton( "ToUpper"   , "To Upper"                   , hbide_image( "toupper"       ), {|| ::oEM:convertSelection( "ToUpper" ) }, .f. )
   qTBar:addToolButton( "ToLower"   , "To Lower"                   , hbide_image( "tolower"       ), {|| ::oEM:convertSelection( "ToLower" ) }, .f. )
   qTBar:addToolButton( "InvertCase", "Invert Case"                , hbide_image( "invertcase"    ), {|| ::oEM:convertSelection( "Invert"  ) }, .f. )
   qTBar:addToolButton( "BlockCmnt" , "Block Comment"              , hbide_image( "blockcomment"  ), {|| ::oEM:blockComment()                }, .f. )
   qTBar:addToolButton( "StreamCmnt", "Stream Comment"             , hbide_image( "streamcomment" ), {|| ::oEM:streamComment()               }, .f. )
   qTBar:addToolButton( "IndentR"   , "Indent Right"               , hbide_image( "blockindentr"  ), {|| ::oEM:indent(  1 )                  }, .f. )
   qTBar:addToolButton( "IndentL"   , "Indent Left"                , hbide_image( "blockindentl"  ), {|| ::oEM:indent( -1 )                  }, .f. )
   qTBar:addToolButton( "Sgl2Dbl"   , "Single to Double Quotes"    , hbide_image( "sgl2dblquote"  ), {|| ::oEM:convertDQuotes()              }, .f. )
   qTBar:addToolButton( "Dbl2Sgl"   , "Double to Single Quotes"    , hbide_image( "dbl2sglquote"  ), {|| ::oEM:convertQuotes()               }, .f. )
   qTBar:addToolButton( "Stringify" , "Stringify Selection"        , hbide_image( "stringify"     ), {|| ::oEM:stringify()                   }, .f. )
   qTBar:addToolButton( "AlignAt"   , "Align At..."                , hbide_image( "align_at"      ), {|| ::oEM:alignAt()                     }, .f. )

   RETURN Self

/*----------------------------------------------------------------------*/


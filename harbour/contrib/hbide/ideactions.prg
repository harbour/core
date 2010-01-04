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
 *                               23Nov2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "xbp.ch"
#include "inkey.ch"
#include "hbide.ch"

/*----------------------------------------------------------------------*/

FUNCTION buildToolBar( oWnd, oIde )
   LOCAL oTBar
   LOCAL cResPath := hb_DirBase() + "resources" + hb_OsPathSeparator()

   oTBar := XbpToolBar():new( oWnd )
   oTBar:imageWidth  := 22
   oTBar:imageHeight := 22
   oTBar:create( , , { 0, oWnd:currentSize()[ 2 ]-60 }, { oWnd:currentSize()[ 1 ], 60 } )
   oTBar:setStyleSheet( GetStyleSheet( "QToolBar" ) )

   oTBar:oWidget:setMaximumHeight( 28 )

   oTBar:addItem( "Exit"                       , cResPath + "exit.png"           , , , , , "Exit"              )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "New"                        , cResPath + "new.png"            , , , , , "New"               )
   oTBar:addItem( "Open"                       , cResPath + "open.png"           , , , , , "Open"              )
   oTBar:addItem( "Save"                       , cResPath + "save.png"           , , , , , "Save"              )
 * oTBar:addItem( "Save As"                    , cResPath + "saveas.png"         , , , , , "SaveAs"            )
 * oTBar:addItem( "Save All"                   , cResPath + "saveall.png"        , , , , , "SaveAll"           )
   oTBar:addItem( "Close"                      , cResPath + "close.png"          , , , , , "Close"             )
 * oTBar:addItem( "Close all"                  , cResPath + "closeall.png"       , , , , , "CloseAll"          )
   oTBar:addItem( "Print"                      , cResPath + "print.png"          , , , , , "Print"             )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Compile"                    , cResPath + "compile.png"        , , , , , "Compile"           )
   oTBar:addItem( "Compile to PPO"             , cResPath + "ppo.png"            , , , , , "CompilePPO"        )
   #if 0
 * oTBar:addItem( "Build Project"              , cResPath + "build.png"          , , , , , "SaveBuild"         )
   oTBar:addItem( "Build and Run"              , cResPath + "run.png"            , , , , , "SaveBuildLaunch"   )
   oTBar:addItem( "Build and Run without Debug", cResPath + "runnodebug.png"     , , , , , "SaveBuildLaunch"   )
   oTBar:addItem( "Rebuild Project"            , cResPath + "clean.png"          , , , , , "SaveRebuild"       )
   oTBar:addItem( "Rebuild and Launch Project" , cResPath + "cleanrun.png"       , , , , , "SaveRebuildLaunch" )
   #else
   oTBar:addItem( "Build Project"              , cResPath + "build.png"          , , , , , "SaveBuild"         )
   oTBar:addItem( "Build and Run Project"      , cResPath + "buildlaunch.png"    , , , , , "SaveBuildLaunch"   )
   oTBar:addItem( "Rebuild Project"            , cResPath + "rebuild.png"        , , , , , "SaveRebuild"       )
   oTBar:addItem( "Rebuild and Launch Project" , cResPath + "rebuildlaunch.png"  , , , , , "SaveRebuildLaunch" )
   #endif
   //
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Show/hide Project Tree"     , cResPath + "properties.png"     , , , , , "ToggleProjectTree" )
   oTBar:addItem( "Show/hide Build Info"       , cResPath + "builderror.png"     , , , , , "ToggleBuildInfo"   )
   oTBar:addItem( "Show/hide Function List"    , cResPath + "modulelist.png"     , , , , , "ToggleFuncList"    )
   //
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Undo"                       , cResPath + "undo.png"           , , , , , "Undo"              )
   oTBar:addItem( "Redo"                       , cResPath + "redo.png"           , , , , , "Redo"              )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Cut"                        , cResPath + "cut.png"            , , , , , "Cut"               )
   oTBar:addItem( "Copy"                       , cResPath + "copy.png"           , , , , , "Copy"              )
   oTBar:addItem( "Paste"                      , cResPath + "paste.png"          , , , , , "Paste"             )
   oTBar:addItem( "Select All"                 , cResPath + "selectall.png"      , , , , , "SelectAll"         )
   oTBar:addItem( "Column/Stream Selection"    , cResPath + "stream.png"         , , , , , "19"                )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Find"                       , cResPath + "find.png"           , , , , , "Find"              )
   oTBar:addItem( "Search"                     , cResPath + "search.png"         , , , , , "Search"            )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Place/Remove Mark"          , cResPath + "placeremovemark.png", , , , , "SetMark"           )
   oTBar:addItem( "Goto Mark"                  , cResPath + "gotomark.png"       , , , , , "GotoMark"          )
   oTBar:addItem( "Goto Line"                  , cResPath + "gotoline.png"       , , , , , "Goto"              )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "To Upper"                   , cResPath + "toupper.png"        , , , , , "ToUpper"           )
   oTBar:addItem( "To Lower"                   , cResPath + "tolower.png"        , , , , , "ToLower"           )
   oTBar:addItem( "Invert Case"                , cResPath + "invertcase.png"     , , , , , "Invert"            )
   oTBar:addItem( "Match Pairs"                , cResPath + "matchobj.png"       , , , , , "28"                )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   #if 0
   oTBar:addItem( "ZoomIn"                     , cResPath + "zoomin.png"         , , , , , "ZoomIn"            )
   oTBar:addItem( "ZoomOut"                    , cResPath + "zoomout.png"        , , , , , "ZoomOut"           )
   oTBar:addItem(                              ,                                 , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   #endif

   oTBar:transparentColor := GraMakeRGBColor( { 0,255,255 } ) // GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| oIde:execAction( oButton:key ) }

   RETURN oTBar

/*----------------------------------------------------------------------*/
/*
 * Normalizes a caption for an menu item with shortcut (or not).
 * TODO: add support for translation of menu items AND support changing shortcuts
 *       loading from a text file for customing hotkeys AND icons. (vailtom)
 * 27/12/2009 - 16:05:32 - vailtom
 */
STATIC FUNCTION mnuNormalizeItem( cCaption )
   LOCAL cKey
   LOCAL cIco
   LOCAL p

   /* Retrieve and update the ICON name for this menu item */
   IF ( ( p := Rat( '|', cCaption ) ) != 00 )
      cIco := Substr( cCaption, p + 1 )
      cIco := alltrim( cIco )

      cCaption := Substr( cCaption, 1, p - 1 )
      cCaption := Alltrim( cCaption )

    * cIco := s_resPath + Alltrim( cIco ) ---> "s_resPath" is need here!
      IF !Empty( cIco )
         cIco := StrTran( cIco, '/', hb_OsPathSeparator() )
         cIco := StrTran( cIco, '\', hb_OsPathSeparator() )

         IF !( hb_OsPathSeparator() $ cIco )
            cIco := hb_DirBase() + "resources" + hb_OsPathSeparator() + cIco + "|"
         ELSE
            cIco := cIco + "|"
         Endif
      Endif
   ELSE
      cIco := ''
   ENDIF

   /* Update the key shortcut for this menu item */
   IF ( ( p := Rat( ',', cCaption ) ) != 00 )
      cKey := Substr( cCaption, p + 1 )
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

   RETURN ( cIco + cCaption + cKey )

/*----------------------------------------------------------------------*/
/*
 * This pseudo function helps to create is a menu item with text and shortcut.
 */
#define _T( x )  ( mnuNormalizeItem( x ) )

/*----------------------------------------------------------------------*/

FUNCTION buildMainMenu( oWnd, oIde )
   LOCAL oMenuBar, oSubMenu, oSubMenu2, n, f, lEmpty

   oMenuBar := oWnd:MenuBar()

   oMenuBar:setStyleSheet( GetStyleSheet( "QMenuBar" ) )

   /*----------------------------------------------------------------------------*/
   /*                                   File                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~File"

   oSubMenu2 := XbpMenu():new( oSubMenu ):create()
   oSubMenu2:addItem( { _T( "~Source, ^N | new.png" )            , {|| oIde:execAction( "New"            ) } } )
   oSubMenu2:addItem( { _T( "~Project | project.png" )           , {|| oIde:execAction( "NewProject"     ) } } )
   oMenuBar:addItem( { oSubMenu2,  _T( "~New" ) } )
   oMenuBar:aMenuItems[ oMenuBar:numItems(), 2 ]:seticon( oIde:resPath + 'new.png' )

// oSubMenu:addItem( { _T( "~New File, ^N | new.png" )           , {|| oIde:execAction( "New"            ) } } )
// oSubMenu:addItem( { _T( "New Pro~ject, Sh+^N | project.png" ) , {|| oIde:execAction( "NewProject"     ) } } )
   oSubMenu:addItem( { _T( "~Open, ^O | open.png" )              , {|| oIde:execAction( "Open"           ) } } )
   oSubMenu:addItem( { _T( "Open Projec~t" )                     , {|| oIde:execAction( "LoadProject"    ) } } )

   hbide_menuAddSep( oSubMenu )

   oSubMenu2 := XbpMenu():new( oSubMenu ):create()
   oSubMenu2:itemSelected := {| nIndex, cFile | cFile := oIde:aIni[ INI_RECENTFILES, nIndex ], ;
                                                oIde:editSource( cFile ) }
   lEmpty := .T.
   FOR n := 1 TO Len( oIde:aIni[ INI_RECENTFILES ] )
       f := hbide_pathNormalized( oIde:aIni[ INI_RECENTFILES, n ], .F. )
       lEmpty := .F.
       oSubMenu2:addItem( { _T( '~' + hb_NumToHex(n) + '. ' + f ), nil } )
       IF !hb_FileExists(f)
          oSubMenu2:disableItem( n )
       ENDIF
   NEXT
   IF lEmpty
      oSubMenu2:addItem( { _T( "** No recent files found **" )   , nil } )
      oSubMenu2:disableItem( 1 )
   ENDIF
   oMenuBar:addItem( { oSubMenu2,  _T( "Recent Files" ) } )

   oSubMenu2 := XbpMenu():new( oSubMenu ):create()
   oSubMenu2:itemSelected := {| nIndex, cFile | cFile := oIde:aIni[ INI_RECENTPROJECTS, nIndex ], ;
                                                oIde:loadProjectProperties( cFile, .F., .F., .T. ) }

   lEmpty := .T.
   FOR n := 1 TO Len( oIde:aIni[ INI_RECENTPROJECTS ] )
       f := hbide_pathNormalized( oIde:aIni[ INI_RECENTPROJECTS, n ], .F. )
       lEmpty := .F.
       oSubMenu2:addItem( { _T( '~' + hb_NumToHex(n) + '. ' + f )   , nil } )
       IF !hb_FileExists(f)
          oSubMenu2:disableItem( n )
       ENDIF
   NEXT
   IF lEmpty
      oSubMenu2:addItem( { _T( "** No recent projects found **" )   , nil } )
      oSubMenu2:disableItem( 1 )
   ENDIF
   oMenuBar:addItem( { oSubMenu2,  _T( "Recent Projects" ) } )

   hbide_menuAddSep( oSubMenu )

   oSubMenu:addItem( { _T( "~Save, ^S | save.png" )             , {|| oIde:execAction( "Save"           ) } } )
   oSubMenu:addItem( { _T( "Save ~As | saveas.png" )            , {|| oIde:execAction( "SaveAs"         ) } } )
   oSubMenu:addItem( { _T( "Save A~ll, Sh+^S | saveall.png")    , {|| oIde:execAction( "SaveAll"        ) } } )
   oSubMenu:addItem( { _T( "~Close, ^W | close.png" )           , {|| oIde:execAction( "Close"          ) } } )
   oSubMenu:addItem( { _T( "Clos~e All | closeall.png" )        , {|| oIde:execAction( "CloseAll"       ) } } )
   oSubMenu:addItem( { _T( "Close ~Others| closeexcept.png" )   , {|| oIde:execAction( "CloseOther"     ) } } )
   oSubMenu:addItem( { _T( "~Revert to Saved, Sh+^R" )          , {|| oIde:execAction( "Revert"         ) } } )
   hbide_menuAddSep( oSubMenu )

 * oSubMenu:addItem( { _T( "~Export as HTML* | exporthtml.png" ), {|| oIde:execAction( ""               ) } } )
   oSubMenu:addItem( { _T( "~Print, ^P | print.png" )           , {|| oIde:execAction( "Print"          ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Sa~ve and Exit, Sh+^W" )            , {|| oIde:execAction( "SaveExit"       ) } } )
   oSubMenu:addItem( { _T( "E~xit | exit.png" )                 , {|| oIde:execAction( "Exit"           ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Edit                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Edit"
   oSubMenu:addItem( { _T( "~Undo | undo.png" )                 , {|| oIde:execAction( "Undo"           ) } } )
   oSubMenu:addItem( { _T( "~Redo | redo.png" )                 , {|| oIde:execAction( "Redo"           ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "C~ut  | cut.png" )                  , {|| oIde:execAction( "Cut"            ) } } )
   oSubMenu:addItem( { _T( "~Copy | copy.png" )                 , {|| oIde:execAction( "Copy"           ) } } )
   oSubMenu:addItem( { _T( "~Paste| paste.png" )                , {|| oIde:execAction( "Paste"          ) } } )
   oSubMenu:addItem( { _T( "Select ~All | selectall.png" )      , {|| oIde:execAction( "SelectAll"      ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "~Find/Replace, ^F | find.png" )     , {|| oIde:execAction( "Find"           ) } } )
   oSubMenu:addItem( { _T( "~Go To Line..., ^G| gotoline.png" ) , {|| oIde:execAction( "Goto"           ) } } )
   hbide_menuAddSep( oSubMenu )

   oSubMenu2 := XbpMenu():new( oSubMenu ):create()
   oSubMenu2:addItem( { _T( "~Date && Time, Sh+F7| insert-datetime.png" )           , {|| oIde:execAction( "InsertDateTime"     ) } } )
   oSubMenu2:addItem( { _T( "~Random Function Name, Sh+^F8| insert-procname.png" )  , {|| oIde:execAction( "InsertRandomName"   ) } } )
   oSubMenu2:addItem( { _T( "~External File at cursor| insert-external-file.png" )  , {|| oIde:execAction( "InsertExternalFile" ) } } )
   oMenuBar:addItem( { oSubMenu2,  _T( "~Insert" ) } )

   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Switch Read~Only Mode | readonly.png" )                 , {|| oIde:execAction( "switchReadOnly" ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Project                                  */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Project"
   oSubMenu:addItem( { _T( "Properties" )                                     , {|| oIde:execAction( "Properties"     ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Add File* | projectadd.png" )                     , {|| oIde:execAction( "" ) } } )
   oSubMenu:addItem( { _T( "Remove File* | projectdel.png" )                  , {|| oIde:execAction( "" ) } } )
   oSubMenu:addItem( { _T( "Select Main Module | setmain.png" )               , {|| oIde:execAction( "" ) } } )
   oSubMenu:disableItem( oSubMenu:numItems )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Change Current Project" )                         , {|| oIde:execAction( "SelectProject" ) } } )
   oSubMenu:addItem( { _T( "Close Current Project" )                          , {|| oIde:execAction( "CloseProject"   ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Build                                    */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Build"
   oSubMenu:addItem( { _T( "Build, ^F9 | build.png" )                         , {|| oIde:execAction( "SaveBuild"          ) } } )
   oSubMenu:addItem( { _T( "Build and Launch, F9 | buildlaunch.png" )         , {|| oIde:execAction( "SaveBuildLaunch"    ) } } )
   oSubMenu:addItem( { _T( "Re-build | rebuild.png" )                         , {|| oIde:execAction( "SaveRebuild"        ) } } )
   oSubMenu:addItem( { _T( "Re-build and Launch, Sh+^F9 | rebuildlaunch.png" ), {|| oIde:execAction( "SaveRebuildLaunch"  ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Save and Compile Current File | compile.png")     , {|| oIde:execAction( "SaveCompileCurrent" ) } } )
   oSubMenu:addItem( { _T( "Save and Compile to PPO | ppo.png" )              , {|| oIde:execAction( "CompilePPO"        ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Launch, ^F10" )                                   , {|| oIde:execAction( "LaunchProject"      ) } } )
 * oSubMenu:addItem( { _T( "Run without Debug*, Sh+^F10 | runnodebug.png" ), {|| oIde:execAction( "" ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Tools                                    */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Tools"
   oSubMenu:addItem( { _T( "Configure Tools...*" )             , {|| oIde:execAction( ""               ) } } )
   hbide_menuAddSep( oSubMenu )
   // TODO: Load custom TOOLS LINK from .INI file
#ifdef __PLATFORM__WINDOWS
   oSubMenu:addItem( { _T( "Command Prompt...*" )              , {|| oIde:execAction( ""               ) } } )
#else
   oSubMenu:addItem( { _T( "Terminal" )                        , {|| oIde:execAction( ""               ) } } )
#endif
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Options                                  */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Options"
   oSubMenu:addItem( { _T( "Themes" )                          , {|| oIde:oThemes:fetch()                   } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Codec                                    */
   /*----------------------------------------------------------------------------*/
   oSubMenu := hbide_buildCodecMenu( oIde, oMenuBar )
   oMenuBar:addItem( { oSubMenu, NIL } )

   /*----------------------------------------------------------------------------*/
   /*                                   Help                                     */
   /*----------------------------------------------------------------------------*/
   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Help"
   oSubMenu:addItem( { _T( "About hbIDE   | vr-16x16.png" )    , {|| hbide_help( 1 ) } } )
   oSubMenu:addItem( { _T( "About Harbour | hb-16x16.png" )    , {|| hbide_help( 4 ) } } )
   hbide_menuAddSep( oSubMenu )
   oSubMenu:addItem( { _T( "Harbour Users (Mailing Lists)   | list-users.png" )        , {|| hbide_help( 3 ) } } )
   oSubMenu:addItem( { _T( "Harbour Developers (Mailing Lists) | list-developers.png" ), {|| hbide_help( 2 ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   Return Nil

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_buildCodecMenu( oIde, oMenuBar )
   LOCAL oSubMenu, oSub1

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Codec"
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
   oSub1:addItem( { "-1"                      , {|| oIde:setCodec( "ISO 8859-1"          ) } } )
   oSub1:addItem( { "-2"                      , {|| oIde:setCodec( "ISO 8859-2"          ) } } )
   oSub1:addItem( { "-3"                      , {|| oIde:setCodec( "ISO 8859-3"          ) } } )
   oSub1:addItem( { "-4"                      , {|| oIde:setCodec( "ISO 8859-4"          ) } } )
   oSub1:addItem( { "-5"                      , {|| oIde:setCodec( "ISO 8859-5"          ) } } )
   oSub1:addItem( { "-6"                      , {|| oIde:setCodec( "ISO 8859-6"          ) } } )
   oSub1:addItem( { "-7"                      , {|| oIde:setCodec( "ISO 8859-7"          ) } } )
   oSub1:addItem( { "-8"                      , {|| oIde:setCodec( "ISO 8859-8"          ) } } )
   oSub1:addItem( { "-9"                      , {|| oIde:setCodec( "ISO 8859-9"          ) } } )
   oSub1:addItem( { "-10"                     , {|| oIde:setCodec( "ISO 8859-10"         ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "ISO 8859-13 to 16"
   oSub1:addItem( { "-13"                     , {|| oIde:setCodec( "ISO 8859-13"         ) } } )
   oSub1:addItem( { "-14"                     , {|| oIde:setCodec( "ISO 8859-14"         ) } } )
   oSub1:addItem( { "-15"                     , {|| oIde:setCodec( "ISO 8859-15"         ) } } )
   oSub1:addItem( { "-16"                     , {|| oIde:setCodec( "ISO 8859-16"         ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "Iscii"
   oSub1:addItem( { "-Bng"                    , {|| oIde:setCodec( "Iscii-Bng"           ) } } )
   oSub1:addItem( { "-Dev"                    , {|| oIde:setCodec( "Iscii-Dev"           ) } } )
   oSub1:addItem( { "-Gjr"                    , {|| oIde:setCodec( "Iscii-Gjr"           ) } } )
   oSub1:addItem( { "-Knd"                    , {|| oIde:setCodec( "Iscii-Knd"           ) } } )
   oSub1:addItem( { "-Mlm"                    , {|| oIde:setCodec( "Iscii-Mlm"           ) } } )
   oSub1:addItem( { "-Ori"                    , {|| oIde:setCodec( "Iscii-Ori"           ) } } )
   oSub1:addItem( { "-Pnj"                    , {|| oIde:setCodec( "Iscii-Pnj"           ) } } )
   oSub1:addItem( { "-Tlg"                    , {|| oIde:setCodec( "Iscii-Tlg"           ) } } )
   oSub1:addItem( { "-Tml"                    , {|| oIde:setCodec( "Iscii-Tml"           ) } } )
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
   oSub1:addItem( { "-1250"                   , {|| oIde:setCodec( "Windows-1250"        ) } } )
   oSub1:addItem( { "-1251"                   , {|| oIde:setCodec( "Windows-1251"        ) } } )
   oSub1:addItem( { "-1252"                   , {|| oIde:setCodec( "Windows-1252"        ) } } )
   oSub1:addItem( { "-1253"                   , {|| oIde:setCodec( "Windows-1253"        ) } } )
   oSub1:addItem( { "-1254"                   , {|| oIde:setCodec( "Windows-1254"        ) } } )
   oSub1:addItem( { "-1255"                   , {|| oIde:setCodec( "Windows-1255"        ) } } )
   oSub1:addItem( { "-1256"                   , {|| oIde:setCodec( "Windows-1256"        ) } } )
   oSub1:addItem( { "-1257"                   , {|| oIde:setCodec( "Windows-1257"        ) } } )
   oSub1:addItem( { "-1258"                   , {|| oIde:setCodec( "Windows-1258"        ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSubMenu:addItem( { "WINSAMI2    "         , {|| oIde:setCodec( "WINSAMI2"            ) } } )

   RETURN oSubMenu

/*----------------------------------------------------------------------*/
/*
 * 02/01/2010 - 22:44:19
 */
#define QMF_POPUP  1

STATIC FUNCTION hbide_mnuUpdateMRUpopup( oIde, nType )
   LOCAL oMenuBar
   LOCAL lEmpty
   LOCAL oItem
   LOCAL cFindStr
   LOCAL nPos
   LOCAL n, c

   IF Empty(oIde:oDlg )
      RETURN NIL
   ENDIF

   oMenuBar := oIde:oDlg:MenuBar()
   lEmpty   := .T.
   nPos     := 0
   cFindStr := IIF( nType == INI_RECENTFILES, 'RECENT FILES', 'RECENT PROJECTS')

   FOR n := 1 TO oMenuBar:numItems()

       IF oMenuBar:aMenuItems[ n, 1 ] != QMF_POPUP
          LOOP
       ENDIF

//     msgbox( ToString( oMenuBar:aMenuItems[ n ] ))

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

   FOR n := 1 TO Len( oIde:aIni[ nType ] )
       c := hbide_pathNormalized( oIde:aIni[ nType , n ], .F. )
       lEmpty := .F.

       oItem[ 4 ]:addItem( { _T( '~' + hb_NumToHex( n ) + '. ' + c )   , nil } )

       IF !hb_FileExists(c)
          oItem[ 4 ]:disableItem( n )
       ENDIF
   NEXT
   IF lEmpty
      IF nType == INI_RECENTFILES
         oItem[ 4 ]:addAction( "** No recent files found **" )
      ELSE
         oItem[ 4 ]:addAction( "** No recent projects found **" )
      ENDIF
      oItem[ 4 ]:disableItem( 1 )
   ENDIF
   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * Add a file name to MRU menu item.
 * 02/01/2010 - 23:23:22 - vailtom
 */
FUNCTION hbide_mnuAddFileToMRU( oIde, cFileName, nType )
   LOCAL nPos

   IF nType != INI_RECENTPROJECTS .AND. nType != INI_RECENTFILES
      RETURN nil
   ENDIF

   cFileName := hbide_pathNormalized( cFileName )

   nPos := aScan( oIde:aIni[ nType ], {|f| hbide_pathNormalized( f ) == cFileName } )

   IF nPos > 0
      hb_aDel( oIde:aIni[ nType ], nPos, .T. )
   ENDIF

   AAdd( oIde:aIni[ nType ], '' )
   AIns( oIde:aIni[ nType ], 1 )

   oIde:aIni[ nType,1 ] := cFileName

   IF Len( oIde:aIni[ nType ] ) > 15
      aSize( oIde:aIni[ nType ], 15 )
   ENDIF

   hbide_mnuUpdateMRUpopup( oIde, nType )
   RETURN nil

/*----------------------------------------------------------------------*/
/*
 * Find a menu item with same caption as passed on argument.
 * 03/01/2010 - 13:12:42
 */
FUNCTION hbide_mnuFindItem( oIde, cCaption )
   LOCAL oMenuBar
   LOCAL oItem
   LOCAL n, c

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

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

/*----------------------------------------------------------------------*/

FUNCTION buildToolBar( oWnd, oIde )
   LOCAL oTBar
   LOCAL cResPath := hb_DirBase() + "resources" + hb_OsPathSeparator()

   oTBar := XbpToolBar():new( oWnd )
   oTBar:create( , , { 0, oWnd:currentSize()[ 2 ]-60 }, { oWnd:currentSize()[ 1 ], 60 } )

   oTBar:imageWidth  := 20
   oTBar:imageHeight := 20

   oTBar:oWidget:setMaximumHeight( 30 )

   oTBar:addItem( "Exit"                       , cResPath + "exit.png"           , , , , , "Exit"       )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Toggle Project Tree"        , cResPath + "properties.png"     , , , , , "ToggleProjectTree" )
   oTBar:addItem( "New"                        , cResPath + "new.png"            , , , , , "New"        )
   oTBar:addItem( "Open"                       , cResPath + "open.png"           , , , , , "Open"       )
   oTBar:addItem( "Save"                       , cResPath + "save.png"           , , , , , "Save"       )
   oTBar:addItem( "Close"                      , cResPath + "close.png"          , , , , , "Close"      )
 * oTBar:addItem( "Close all"                  , cResPath + "close.png"          , , , , , "Close all"  )
   oTBar:addItem( "Print"                      , cResPath + "print.png"          , , , , , "Print"      )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Compile"                    , cResPath + "compile.png"        , , , , , "Compile"    )
   oTBar:addItem( "Compile to PPO"             , cResPath + "ppo.png"            , , , , , "CompilePPO" )
   oTBar:addItem( "Build Project"              , cResPath + "build.png"          , , , , , "SaveBuild"  )
   oTBar:addItem( "Build and Launch Project"   , cResPath + "buildlaunch.png"    , , , , , "SaveBuildLaunch"  )
   oTBar:addItem( "Rebuild Project"            , cResPath + "rebuild.png"        , , , , , "SaveRebuild"  )
   oTBar:addItem( "Rebuild and Launch Project" , cResPath + "rebuildlaunch.png"  , , , , , "SaveRebuildLaunch" )
   oTBar:addItem( "Show/Hide Build Error Info" , cResPath + "builderror.png"     , , , , , "11" )
   oTBar:addItem( "Module Function List"       , cResPath + "modulelist.png"     , , , , , "12" )
   //
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Undo"                       , cResPath + "undo.png"           , , , , , "Undo" )
   oTBar:addItem( "Redo"                       , cResPath + "redo.png"           , , , , , "Redo" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Cut"                        , cResPath + "cut.png"            , , , , , "Cut" )
   oTBar:addItem( "Copy"                       , cResPath + "copy.png"           , , , , , "Copy" )
   oTBar:addItem( "Paste"                      , cResPath + "paste.png"          , , , , , "Paste" )
   oTBar:addItem( "Select All"                 , cResPath + "selectall.png"      , , , , , "SelectAll" )
   oTBar:addItem( "Column/Stream Selection"    , cResPath + "stream.png"         , , , , , "19" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Find"                       , cResPath + "find.png"           , , , , , "Find" )
   oTBar:addItem( "Search"                     , cResPath + "search.png"         , , , , , "Search" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Place/Remove Mark"          , cResPath + "placeremovemark.png", , , , , "SetMark" )
   oTBar:addItem( "Goto Mark"                  , cResPath + "gotomark.png"       , , , , , "GotoMark" )
   oTBar:addItem( "Goto Line"                  , cResPath + "gotoline.png"       , , , , , "Goto" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "To Upper"                   , cResPath + "toupper.png"        , , , , , "ToUpper" )
   oTBar:addItem( "To Lower"                   , cResPath + "tolower.png"        , , , , , "ToLower" )
   oTBar:addItem( "Invert Case"                , cResPath + "invertcase.png"     , , , , , "Invert" )
   oTBar:addItem( "Match Pairs"                , cResPath + "matchobj.png"       , , , , , "28" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   #if 0
   oTBar:addItem( "ZoomIn"                     , cResPath + "zoomin.png"         , , , , , "ZoomIn" )
   oTBar:addItem( "ZoomOut"                    , cResPath + "zoomout.png"        , , , , , "ZoomOut" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   #endif

   oTBar:transparentColor := GraMakeRGBColor( { 0,255,255 } ) // GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| oIde:executeAction( oButton:key ) }

   RETURN oTBar

/*----------------------------------------------------------------------*/

FUNCTION buildMainMenu( oWnd, oIde )
   LOCAL oMenuBar, oSubMenu, oSub1

   oMenuBar := oWnd:MenuBar()

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~File"
   oSubMenu:addItem( { "New"                          , {|| oIde:executeAction( "New"                ) } } )
   oSubMenu:addItem( { "Open"                         , {|| oIde:executeAction( "Open"               ) } } )
   oSubMenu:addItem( { "Save"                         , {|| oIde:executeAction( "Save"               ) } } )
   oSubMenu:addItem( { "Close"                        , {|| oIde:executeAction( "Close"              ) } } )
   oSubMenu:addItem( { "Close all"                    , {|| oIde:executeAction( "CloseAll"           ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Print"                        , {|| oIde:executeAction( "Print"              ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Exit"                         , {|| oIde:executeAction( "Exit"               ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Project"
   oSubMenu:addItem( { "New"                          , {|| oIde:executeAction( "NewProject"         ) } } )
   oSubMenu:addItem( { "Load Project..."              , {|| oIde:executeAction( "LoadProject"        ) } } )
   oSubMenu:addItem( { "Close Project"                , {|| oIde:executeAction( "CloseProject"       ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Build"               , {|| oIde:executeAction( "SaveBuild"          ) } } )
   oSubMenu:addItem( { "Save, Build and Launch"       , {|| oIde:executeAction( "SaveBuildLaunch"    ) } } )
   oSubMenu:addItem( { "Save and Re-build"            , {|| oIde:executeAction( "SaveRebuild"        ) } } )
   oSubMenu:addItem( { "Save, Re-build and Launch"    , {|| oIde:executeAction( "SaveRebuildLaunch"  ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Compile Current File", {|| oIde:executeAction( "SaveCompileCurrent" ) } } )
   oSubMenu:addItem( { "Save and Compile to PPO"      , {|| oIde:executeAction( "CompilePPO"         ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Project Properties"           , {|| oIde:executeAction( "Properties"         ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~CodePage"
   oSubMenu:addItem( { "Apple Roman " , {|| oIde:setCodec( "Apple Roman" ) } } )
   oSubMenu:addItem( { "Big5        " , {|| oIde:setCodec( "Big5"        ) } } )
   oSubMenu:addItem( { "Big5-HKSCS  " , {|| oIde:setCodec( "Big5-HKSCS"  ) } } )
   oSubMenu:addItem( { "CP949       " , {|| oIde:setCodec( "CP949"       ) } } )
   oSubMenu:addItem( { "EUC-JP      " , {|| oIde:setCodec( "EUC-JP"      ) } } )
   oSubMenu:addItem( { "EUC-KR      " , {|| oIde:setCodec( "EUC-KR"      ) } } )
   oSubMenu:addItem( { "GB18030-0   " , {|| oIde:setCodec( "GB18030-0"   ) } } )
   oSubMenu:addItem( { "IBM 850     " , {|| oIde:setCodec( "IBM 850"     ) } } )
   oSubMenu:addItem( { "IBM 866     " , {|| oIde:setCodec( "IBM 866"     ) } } )
   oSubMenu:addItem( { "IBM 874     " , {|| oIde:setCodec( "IBM 874"     ) } } )
   oSubMenu:addItem( { "ISO 2022-JP " , {|| oIde:setCodec( "ISO 2022-JP" ) } } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "ISO 8859-1 to 10"
   oSub1:addItem( { "-1"  , {|| oIde:setCodec( "ISO 8859-1"  ) } } )
   oSub1:addItem( { "-2"  , {|| oIde:setCodec( "ISO 8859-2"  ) } } )
   oSub1:addItem( { "-3"  , {|| oIde:setCodec( "ISO 8859-3"  ) } } )
   oSub1:addItem( { "-4"  , {|| oIde:setCodec( "ISO 8859-4"  ) } } )
   oSub1:addItem( { "-5"  , {|| oIde:setCodec( "ISO 8859-5"  ) } } )
   oSub1:addItem( { "-6"  , {|| oIde:setCodec( "ISO 8859-6"  ) } } )
   oSub1:addItem( { "-7"  , {|| oIde:setCodec( "ISO 8859-7"  ) } } )
   oSub1:addItem( { "-8"  , {|| oIde:setCodec( "ISO 8859-8"  ) } } )
   oSub1:addItem( { "-9"  , {|| oIde:setCodec( "ISO 8859-9"  ) } } )
   oSub1:addItem( { "-10" , {|| oIde:setCodec( "ISO 8859-10" ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "ISO 8859-13 to 16"
   oSub1:addItem( { "-13" , {|| oIde:setCodec( "ISO 8859-13" ) } } )
   oSub1:addItem( { "-14" , {|| oIde:setCodec( "ISO 8859-14" ) } } )
   oSub1:addItem( { "-15" , {|| oIde:setCodec( "ISO 8859-15" ) } } )
   oSub1:addItem( { "-16" , {|| oIde:setCodec( "ISO 8859-16" ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   //oSubMenu:addItem( { "Iscii       " , {|| oIde:setCodec( "Iscii" ) } } )  //-Bng, Dev, Gjr, Knd, Mlm, Ori, Pnj, Tlg, Tml
   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "Iscii"
   oSub1:addItem( { "-Bng" , {|| oIde:setCodec( "Iscii-Bng" ) } } )
   oSub1:addItem( { "-Dev" , {|| oIde:setCodec( "Iscii-Dev" ) } } )
   oSub1:addItem( { "-Gjr" , {|| oIde:setCodec( "Iscii-Gjr" ) } } )
   oSub1:addItem( { "-Knd" , {|| oIde:setCodec( "Iscii-Knd" ) } } )
   oSub1:addItem( { "-Mlm" , {|| oIde:setCodec( "Iscii-Mlm" ) } } )
   oSub1:addItem( { "-Ori" , {|| oIde:setCodec( "Iscii-Ori" ) } } )
   oSub1:addItem( { "-Pnj" , {|| oIde:setCodec( "Iscii-Pnj" ) } } )
   oSub1:addItem( { "-Tlg" , {|| oIde:setCodec( "Iscii-Tlg" ) } } )
   oSub1:addItem( { "-Tml" , {|| oIde:setCodec( "Iscii-Tml" ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSubMenu:addItem( { "JIS X 0201  " , {|| oIde:setCodec( "JIS X 0201"  ) } } )
   oSubMenu:addItem( { "JIS X 0208  " , {|| oIde:setCodec( "JIS X 0208"  ) } } )
   oSubMenu:addItem( { "KOI8-R      " , {|| oIde:setCodec( "KOI8-R"      ) } } )
   oSubMenu:addItem( { "KOI8-U      " , {|| oIde:setCodec( "KOI8-U"      ) } } )
   oSubMenu:addItem( { "MuleLao-1   " , {|| oIde:setCodec( "MuleLao-1"   ) } } )
   oSubMenu:addItem( { "ROMAN8      " , {|| oIde:setCodec( "ROMAN8"      ) } } )
   oSubMenu:addItem( { "Shift-JIS   " , {|| oIde:setCodec( "Shift-JIS"   ) } } )
   oSubMenu:addItem( { "TIS-620     " , {|| oIde:setCodec( "TIS-620"     ) } } )
   oSubMenu:addItem( { "TSCII       " , {|| oIde:setCodec( "TSCII"       ) } } )
   oSubMenu:addItem( { "UTF-8       " , {|| oIde:setCodec( "UTF-8"       ) } } )
   oSubMenu:addItem( { "UTF-16      " , {|| oIde:setCodec( "UTF-16"      ) } } )
   oSubMenu:addItem( { "UTF-16BE    " , {|| oIde:setCodec( "UTF-16BE"    ) } } )
   oSubMenu:addItem( { "UTF-16LE    " , {|| oIde:setCodec( "UTF-16LE"    ) } } )
   oSubMenu:addItem( { "UTF-32      " , {|| oIde:setCodec( "UTF-32"      ) } } )
   oSubMenu:addItem( { "UTF-32BE    " , {|| oIde:setCodec( "UTF-32BE"    ) } } )
   oSubMenu:addItem( { "UTF-32LE    " , {|| oIde:setCodec( "UTF-32LE"    ) } } )

   //oSubMenu:addItem( { "Windows     " , {|| oIde:setCodec( "Windows"     ) } } )  //-1250 to 1258
   oSub1 := XbpMenu():new( oSubMenu ):create()
   oSub1:title := "Windows-1250 to 1258"
   oSub1:addItem( { "-1250" , {|| oIde:setCodec( "Windows-1250" ) } } )
   oSub1:addItem( { "-1251" , {|| oIde:setCodec( "Windows-1251" ) } } )
   oSub1:addItem( { "-1252" , {|| oIde:setCodec( "Windows-1252" ) } } )
   oSub1:addItem( { "-1253" , {|| oIde:setCodec( "Windows-1253" ) } } )
   oSub1:addItem( { "-1254" , {|| oIde:setCodec( "Windows-1254" ) } } )
   oSub1:addItem( { "-1255" , {|| oIde:setCodec( "Windows-1255" ) } } )
   oSub1:addItem( { "-1256" , {|| oIde:setCodec( "Windows-1256" ) } } )
   oSub1:addItem( { "-1257" , {|| oIde:setCodec( "Windows-1257" ) } } )
   oSub1:addItem( { "-1258" , {|| oIde:setCodec( "Windows-1258" ) } } )
   oSubMenu:addItem( { oSub1, NIL } )

   oSubMenu:addItem( { "WINSAMI2    " , {|| oIde:setCodec( "WINSAMI2"    ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   Return Nil

/*----------------------------------------------------------------------*/


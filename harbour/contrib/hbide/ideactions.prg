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
   oTBar:addItem( "New Project"                , cResPath + "properties.png"     , , , , , "NewProject" )
   oTBar:addItem( "Open"                       , cResPath + "open.png"           , , , , , "Open"       )
   oTBar:addItem( "Save"                       , cResPath + "save.png"           , , , , , "Save"       )
   oTBar:addItem( "Close"                      , cResPath + "close.png"          , , , , , "Close"      )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Compile"                    , cResPath + "compile.png"        , , , , , "Compile"    )
   oTBar:addItem( "Compile to PPO"             , cResPath + "ppo.png"            , , , , , "6"  )
   oTBar:addItem( "Build Project"              , cResPath + "build.png"          , , , , , "7"  )
   oTBar:addItem( "Build and Launch Project"   , cResPath + "buildlaunch.png"    , , , , , "8"  )
   oTBar:addItem( "Rebuild Project"            , cResPath + "rebuild.png"        , , , , , "9"  )
   oTBar:addItem( "Rebuild and Launch Project" , cResPath + "rebuildlaunch.png"  , , , , , "10" )
   oTBar:addItem( "Show/Hide Build Error Info" , cResPath + "builderror.png"     , , , , , "11" )
   oTBar:addItem( "Module Function List"       , cResPath + "modulelist.png"     , , , , , "12" )
   //
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Undo"                       , cResPath + "undo.png"           , , , , , "13" )
   oTBar:addItem( "Redo"                       , cResPath + "redo.png"           , , , , , "14" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Cut"                        , cResPath + "cut.png"            , , , , , "15" )
   oTBar:addItem( "Copy"                       , cResPath + "copy.png"           , , , , , "16" )
   oTBar:addItem( "Paste"                      , cResPath + "paste.png"          , , , , , "17" )
   oTBar:addItem( "Select All"                 , cResPath + "selectall.png"      , , , , , "18" )
   oTBar:addItem( "Column/Stream Selection"    , cResPath + "stream.png"         , , , , , "19" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Find"                       , cResPath + "find.png"           , , , , , "20" )
   oTBar:addItem( "Search"                     , cResPath + "search.png"         , , , , , "21" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "Place/Remove Mark"          , cResPath + "placeremovemark.png", , , , , "22" )
   oTBar:addItem( "Goto Mark"                  , cResPath + "gotomark.png"       , , , , , "23" )
   oTBar:addItem( "Goto Line"                  , cResPath + "gotoline.png"       , , , , , "24" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )
   oTBar:addItem( "To Upper"                   , cResPath + "toupper.png"        , , , , , "25" )
   oTBar:addItem( "To Lower"                   , cResPath + "tolower.png"        , , , , , "26" )
   oTBar:addItem( "Invert Case"                , cResPath + "invertcase.png"     , , , , , "27" )
   oTBar:addItem( "Match Pairs"                , cResPath + "matchobj.png"       , , , , , "28" )
   oTBar:addItem(                              ,                                  , , , , XBPTOOLBAR_BUTTON_SEPARATOR )

   oTBar:transparentColor := GraMakeRGBColor( { 0,255,255 } ) // GRA_CLR_INVALID
   oTBar:buttonClick := {|oButton| oIde:executeAction( oButton:key ) }

   RETURN oTBar

/*----------------------------------------------------------------------*/

FUNCTION buildMainMenu( oWnd, oIde )
   LOCAL oMenuBar, oSubMenu

   oMenuBar := oWnd:MenuBar()

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~File"
   oSubMenu:addItem( { "Open"                         , {|| oIde:executeAction( "Open"               ) } } )
   oSubMenu:addItem( { "Save"                         , {|| oIde:executeAction( "Save"               ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Exit"                         , {|| oIde:executeAction( "Exit"               ) } } )
   oMenuBar:addItem( { oSubMenu, NIL } )

   oSubMenu := XbpMenu():new( oMenuBar ):create()
   oSubMenu:title := "~Project"
   oSubMenu:addItem( { "New"                          , {|| oIde:executeAction( "NewProject"         ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Build"               , {|| oIde:executeAction( "SaveBuild"          ) } } )
   oSubMenu:addItem( { "Save, Build and Launch"       , {|| oIde:executeAction( "SaveBuildLaunch"    ) } } )
   oSubMenu:addItem( { "Save and Re-build"            , {|| oIde:executeAction( "SaveRebuild"        ) } } )
   oSubMenu:addItem( { "Save, Re-build and Launch"    , {|| oIde:executeAction( "SaveRebuildLaunch"  ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Save and Compile Current File", {|| oIde:executeAction( "SaveCompileCurrent" ) } } )
   oSubMenu:addItem( { "Save and Create PPO Output"   , {|| oIde:executeAction( "SavePPO"            ) } } )
   MenuAddSep( oSubMenu )
   oSubMenu:addItem( { "Project Properties"           , {|| oIde:executeAction( "Properties"         ) } } )

   oMenuBar:addItem( { oSubMenu, NIL } )

   Return Nil

/*----------------------------------------------------------------------*/


/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

CLASS IdeObject

   DATA   xD
   DATA   xD1
   DATA   xD2

   DATA   oIde
   DATA   oUI
   DATA   qContextMenu

   ACCESS oFR                                     INLINE ::oIde:oFR
   ACCESS oBM                                     INLINE ::oIde:oBM
   ACCESS oEM                                     INLINE ::oIde:oEM
   ACCESS oPM                                     INLINE ::oIde:oPM
   ACCESS oDK                                     INLINE ::oIde:oDK
   ACCESS oAC                                     INLINE ::oIde:oAC
   ACCESS oSM                                     INLINE ::oIde:oSM
   ACCESS oEV                                     INLINE ::oIde:oEV
   ACCESS oHL                                     INLINE ::oIde:oHL
   ACCESS oHM                                     INLINE ::oIde:oHM
   ACCESS oFN                                     INLINE ::oIde:oFN
   ACCESS oDW                                     INLINE ::oIde:oDW
   ACCESS oSK                                     INLINE ::oIde:oSK
   ACCESS oSC                                     INLINE ::oIde:oSC
   ACCESS oTM                                     INLINE ::oIde:oTM
   ACCESS oTH                                     INLINE ::oIde:oTH
   ACCESS oFF                                     INLINE ::oIde:oFF
   ACCESS oRM                                     INLINE ::oIde:oRM
   ACCESS oSetup                                  INLINE ::oIde:oSetup
   ACCESS oINI                                    INLINE ::oIde:oINI
   ACCESS oFmt                                    INLINE ::oIde:oFmt
   ACCESS oCL                                     INLINE ::oIde:oCL
   ACCESS oCUI                                    INLINE ::oIde:oCUI
   ACCESS oUiS                                    INLINE ::oIde:oUiS
   ACCESS oPWZ                                    INLINE ::oIde:oPWZ
   ACCESS oParts                                  INLINE ::oIde:oParts

   ACCESS aMeta                                   INLINE ::oIde:aMeta
   ACCESS aTags                                   INLINE ::oIde:aTags

   ACCESS oFont                                   INLINE ::oIde:oFont
   ACCESS oSBar                                   INLINE ::oIde:oSBar
   ACCESS oDlg                                    INLINE ::oIde:oDlg
   ACCESS oDA                                     INLINE ::oIde:oDA

   ACCESS qLayout                                 INLINE ::oIde:qLayout
   ACCESS qCurEdit                                INLINE ::oIde:qCurEdit
   ACCESS qCurDocument                            INLINE ::oIde:qCurDocument
   ACCESS oCurEditor                              INLINE ::oIde:oCurEditor
   ACCESS qTabWidget                              INLINE ::oIde:qTabWidget
   ACCESS oTabParent                              INLINE ::oIde:oTabParent
   ACCESS qBrushWrkProject                        INLINE ::oIde:qBrushWrkProject
   ACCESS qViewsCombo                             INLINE ::oIde:qViewsCombo
   ACCESS qHelpBrw                                INLINE ::oIde:qHelpBrw
   ACCESS qAnimateAction                          INLINE ::oIde:qAnimateAction
   ACCESS qStatusBarAction                        INLINE ::oIde:qStatusBarAction

   ACCESS qTBarLines                              INLINE ::oIde:qTBarLines
   ACCESS qTBarPanels                             INLINE ::oIde:qTBarPanels
   ACCESS qTBarDocks                              INLINE ::oIde:qTBarDocks
   ACCESS qCompleter                              INLINE ::oIde:qCompleter
   ACCESS qCompModel                              INLINE ::oIde:qCompModel
   ACCESS qProtoList                              INLINE ::oIde:qProtoList

   ACCESS cWrkProject                             INLINE ::oIde:cWrkProject
   ACCESS cWrkTheme                               INLINE ::oIde:cWrkTheme
   ACCESS cWrkCodec                               INLINE ::oIde:cWrkCodec
   ACCESS cWrkPathMk2                             INLINE ::oIde:cWrkPathMk2
   ACCESS cWrkPathEnv                             INLINE ::oIde:cWrkPathEnv
   ACCESS cWrkEnvironment                         INLINE ::oIde:cWrkEnvironment
   ACCESS cWrkFind                                INLINE ::oIde:cWrkFind
   ACCESS cWrkFolderFind                          INLINE ::oIde:cWrkFolderFind
   ACCESS cWrkReplace                             INLINE ::oIde:cWrkReplace
   ACCESS cWrkView                                INLINE ::oIde:cWrkView
   ACCESS cWrkHarbour                             INLINE ::oIde:cWrkHarbour
   ACCESS cPathShortcuts                          INLINE ::oIde:cPathShortcuts
   ACCESS cTextExtensions                         INLINE ::oIde:cTextExtensions
   ACCESS cWrkFolderLast                          INLINE ::oIde:cWrkFolderLast
   //
   ACCESS resPath                                 INLINE ::oIde:resPath
   ACCESS pathSep                                 INLINE ::oIde:pathSep
   ACCESS cLastFileOpenPath                       INLINE ::oIde:cLastFileOpenPath
   ACCESS nAnimantionMode                         INLINE ::oIde:nAnimantionMode
   ACCESS nModeUI                                 INLINE ::oIde:nModeUI

   ACCESS aProjects                               INLINE ::oIde:aProjects
   ACCESS aSources                                INLINE ::oIde:aSources
   ACCESS aEditorPath                             INLINE ::oIde:aEditorPath
   ACCESS aProjData                               INLINE ::oIde:aProjData
   ACCESS aTabs                                   INLINE ::oIde:aTabs
   ACCESS aViews                                  INLINE ::oIde:aViews
   ACCESS aSkltns                                 INLINE ::oIde:aSkltns
   ACCESS aUserDict                               INLINE ::oIde:aUserDict

   ACCESS nTabSpaces                              INLINE ::oIde:nTabSpaces
   ACCESS cTabSpaces                              INLINE ::oIde:cTabSpaces
   ACCESS cSeparator                              INLINE ::oIde:cSeparator
   ACCESS cPathSkltns                             INLINE ::oIde:cPathSkltns

   ACCESS oDockPT                                 INLINE ::oIde:oDockPT
   ACCESS oProjTree                               INLINE ::oIde:oProjTree
   ACCESS oProjRoot                               INLINE ::oIde:oProjRoot
   ACCESS oDockED                                 INLINE ::oIde:oDockED
   ACCESS oEditTree                               INLINE ::oIde:oEditTree
   ACCESS oOpenedSources                          INLINE ::oIde:oOpenedSources
   ACCESS oFuncDock                               INLINE ::oIde:oFuncDock
   ACCESS oFuncList                               INLINE ::oIde:oFuncList
   ACCESS oDockB                                  INLINE ::oIde:oDockB
   ACCESS oCompileResult                          INLINE ::oIde:oCompileResult
   ACCESS oDockB1                                 INLINE ::oIde:oDockB1
   ACCESS oLinkResult                             INLINE ::oIde:oLinkResult
   ACCESS oDockB2                                 INLINE ::oIde:oDockB2
   ACCESS oOutputResult                           INLINE ::oIde:oOutputResult
   ACCESS oStackedWidget                          INLINE ::oIde:oStackedWidget
   ACCESS oStackedWidgetMisc                      INLINE ::oIde:oStackedWidgetMisc
   ACCESS oFrame                                  INLINE ::oIde:oFrame
   ACCESS oHelpDock                               INLINE ::oIde:oHelpDock
   ACCESS oSkeltnDock                             INLINE ::oIde:oSkeltnDock
   ACCESS oGeneral                                INLINE ::oIde:oGeneral
   ACCESS oThemesDock                             INLINE ::oIde:oThemesDock
   ACCESS oPropertiesDock                         INLINE ::oIde:oPropertiesDock
   ACCESS oEnvironDock                            INLINE ::oIde:oEnvironDock
   ACCESS oSearchReplace                          INLINE ::oIde:oSearchReplace
   ACCESS oDocViewDock                            INLINE ::oIde:oDocViewDock
   ACCESS oDocWriteDock                           INLINE ::oIde:oDocWriteDock
   ACCESS oFunctionsDock                          INLINE ::oIde:oFunctionsDock
   ACCESS oSkltnsTreeDock                         INLINE ::oIde:oSkltnsTreeDock
   ACCESS oFindDock                               INLINE ::oIde:oFindDock
   ACCESS oSourceThumbnailDock                    INLINE ::oIde:oSourceThumbnailDock
   ACCESS oQScintillaDock                         INLINE ::oIde:oQScintillaDock
   ACCESS oMainToolbar                            INLINE ::oIde:oMainToolbar
   ACCESS oUpDn                                   INLINE ::oIde:oUpDn
   ACCESS oReportsManagerDock                     INLINE ::oIde:oReportsManagerDock
   ACCESS oFormatDock                             INLINE ::oIde:oFormatDock
   ACCESS oSys                                    INLINE ::oIde:oSys
   ACCESS oSysMenu                                INLINE ::oIde:oSysMenu
   ACCESS oCuiEdDock                              INLINE ::oIde:oCuiEdDock
   ACCESS oUISrcDock                              INLINE ::oIde:oUISrcDock

   ACCESS lProjTreeVisible                        INLINE ::oIde:lProjTreeVisible
   ACCESS lDockRVisible                           INLINE ::oIde:lDockRVisible
   ACCESS lDockBVisible                           INLINE ::oIde:lDockBVisible
   ACCESS lTabCloseRequested                      INLINE ::oIde:lTabCloseRequested
   ACCESS isColumnSelectionEnabled                INLINE ::oIde:isColumnSelectionEnabled
   ACCESS lLineNumbersVisible                     INLINE ::oIde:lLineNumbersVisible
   ACCESS lHorzRulerVisible                       INLINE ::oIde:lHorzRulerVisible
   ACCESS lStatusBarVisible                       INLINE ::oIde:lStatusBarVisible
   ACCESS lCurrentLineHighlightEnabled            INLINE ::oIde:lCurrentLineHighlightEnabled

   ACCESS aMarkTBtns                              INLINE ::oIde:aMarkTBtns
   ACCESS lQuitting                               INLINE ::oIde:lQuitting

   DATA   aSlots                                  INIT   {}
   DATA   aEvents                                 INIT   {}

   METHOD createTags( ... )                       INLINE ::oIde:createTags( ... )
   METHOD addSourceInTree( ... )                  INLINE ::oIde:addSourceInTree( ... )
   METHOD setPosAndSizeByIni( ... )               INLINE ::oIde:setPosAndSizeByIni( ... )
   METHOD setPosByIni( ... )                      INLINE ::oIde:setPosByIni( ... )
   METHOD setSizeByIni( ... )                     INLINE ::oIde:setSizeByIni( ... )
   METHOD execAction( ... )                       INLINE ::oIde:execAction( ... )
   METHOD manageFuncContext( ... )                INLINE ::oIde:manageFuncContext( ... )
   METHOD manageProjectContext( ... )             INLINE ::oIde:manageProjectContext( ... )
   METHOD updateFuncList( ... )                   INLINE ::oIde:updateFuncList( ... )
   METHOD gotoFunction( ... )                     INLINE ::oIde:gotoFunction( ... )
   METHOD updateProjectMenu( ... )                INLINE ::oIde:updateProjectMenu( ... )
   METHOD updateProjectTree( ... )                INLINE ::oIde:updateProjectTree( ... )
   METHOD manageItemSelected( ... )               INLINE ::oIde:manageItemSelected( ... )
   METHOD manageFocusInEditor( ... )              INLINE ::oIde:manageFocusInEditor( ... )
   METHOD setCodec( ... )                         INLINE ::oIde:setCodec( ... )
   METHOD updateTitleBar( ... )                   INLINE ::oIde:updateTitleBar( ... )
   METHOD showApplicationCursor( ... )            INLINE ::oIde:showApplicationCursor( ... )

   METHOD editSource( ... )                       INLINE ::oSM:editSource( ... )
   METHOD getEditorByIndex( ... )                 INLINE ::oSM:getEditorByIndex( ... )

   ENDCLASS

/*----------------------------------------------------------------------*/

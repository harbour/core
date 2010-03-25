/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               20Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define buttonAdd_clicked                         1
#define buttonDelete_clicked                      2
#define buttonUp_clicked                          3
#define buttonDown_clicked                        4
#define buttonBrowse_clicked                      5
#define buttonUpdate_clicked                      6
#define buttonClose_clicked                       7
#define buttonExec_clicked                        8

#define listNames_itemSelectionChanged            9

/*----------------------------------------------------------------------*/

CLASS IdeToolsManager INHERIT IdeObject

   DATA   aAct                                    INIT   {}
   DATA   qToolsMenu
   DATA   qToolsButton
   DATA   aPanelsAct                              INIT   {}
   DATA   qPanelsButton
   DATA   qPanelsMenu

   ACCESS aTools                                  INLINE ::aINI[ INI_TOOLS ]

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( nMode, p )
   METHOD clearList()
   METHOD populateList( aList )
   METHOD execTool( cName )
   METHOD buildToolsButton()
   METHOD buildPanelsButton()
   METHOD addPanelsMenu( cPrompt )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:create( oIde )
   LOCAL oAct

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   IF !empty( oAct := ::oAC:getAction( "TB_Tools" ) )
      oAct:setMenu( QMenu():new() )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:destroy()
   LOCAL qAct

   IF !empty( ::oUI )
      FOR EACH qAct IN ::aAct
         ::disconnect( qAct, "triggered(bool)" )
         qAct := NIL
      NEXT
      ::disconnect( ::qToolsButton, "clicked()" )
      ::qToolsButton := NIL

      ::clearList()

      ::disconnect( ::oUI:q_buttonAdd   , "clicked()" )
      ::disconnect( ::oUI:q_buttonDelete, "clicked()" )
      ::disconnect( ::oUI:q_buttonUp    , "clicked()" )
      ::disconnect( ::oUI:q_buttonDown  , "clicked()" )
      ::disconnect( ::oUI:q_buttonExec  , "clicked()" )
      ::disconnect( ::oUI:q_buttonBrowse, "clicked()" )
      ::disconnect( ::oUI:q_buttonUpdate, "clicked()" )
      ::disconnect( ::oUI:q_buttonClose , "clicked()" )
      ::disconnect( ::oUI:q_listNames   , "itemSelectionChanged()" )

      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:show()

   IF empty( ::oUI )
      ::oUI := HbQtUI():new( hbide_uic( "toolsutilities" ), ::oDlg:oWidget ):build()

      ::oUI:setWindowIcon( hbide_image( "hbide" ) )
      ::oUI:setMaximumWidth( ::oUI:width() )
      ::oUI:setMinimumWidth( ::oUI:width() )
      ::oUI:setMaximumHeight( ::oUI:height() )
      ::oUI:setMinimumHeight( ::oUI:height() )

      ::connect( ::oUI:q_buttonAdd   , "clicked()", {|| ::execEvent( buttonAdd_clicked     ) } )
      ::connect( ::oUI:q_buttonDelete, "clicked()", {|| ::execEvent( buttonDelete_clicked  ) } )
      ::connect( ::oUI:q_buttonUp    , "clicked()", {|| ::execEvent( buttonUp_clicked      ) } )
      ::connect( ::oUI:q_buttonDown  , "clicked()", {|| ::execEvent( buttonDown_clicked    ) } )
      ::connect( ::oUI:q_buttonExec  , "clicked()", {|| ::execEvent( buttonExec_clicked    ) } )
      ::connect( ::oUI:q_buttonBrowse, "clicked()", {|| ::execEvent( buttonBrowse_clicked  ) } )
      ::connect( ::oUI:q_buttonUpdate, "clicked()", {|| ::execEvent( buttonUpdate_clicked  ) } )
      ::connect( ::oUI:q_buttonClose , "clicked()", {|| ::execEvent( buttonClose_clicked   ) } )

      ::connect( ::oUI:q_listNames   , "itemSelectionChanged()", {|| ::execEvent( listNames_itemSelectionChanged ) } )

   ENDIF

   ::clearList()
   ::populateList( ::aINI[ INI_TOOLS ] )
   ::oUI:exec()

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:execEvent( nMode, p )
   LOCAL cFile, cFileName, nIndex, qItem, cName
   LOCAL aTools := ::aINI[ INI_TOOLS ]

   HB_SYMBOL_UNUSED( p )

   SWITCH nMode
   CASE listNames_itemSelectionChanged
      qItem := QListWidgetItem():from( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF ( nIndex := ascan( aTools, {|e_| e_[ 1 ] == cName } ) ) > 0
         ::oUI:q_editName    : setText( aTools[ nIndex, 1 ] )
         ::oUI:q_editCmdLine : setText( aTools[ nIndex, 2 ] )
         ::oUI:q_editParams  : setText( aTools[ nIndex, 3 ] )
      ENDIF
      EXIT
   CASE buttonAdd_clicked
      aadd( ::aINI[ INI_TOOLS ], { ::oUI:q_editName:text(), ::oUI:q_editCmdLine:text(), ::oUI:q_editParams:text() } )
      ::oUI:q_listNames:addItem( ::oUI:q_editName:text() )
      EXIT
   CASE buttonDelete_clicked
      qItem := QListWidgetItem():from( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF ( nIndex := ascan( aTools, {|e_| e_[ 1 ] == cName } ) ) > 0
         hb_adel( ::aINI[ INI_TOOLS ], nIndex, .t. )
         ::clearList()
         ::populateList()
      ENDIF
      EXIT
   CASE buttonUp_clicked
      EXIT
   CASE buttonDown_clicked
      EXIT
   CASE buttonExec_clicked
      qItem := QListWidgetItem():from( ::oUI:q_listNames:currentItem() )
      ::execTool( qItem:text() )
      EXIT
   CASE buttonBrowse_clicked
      IF !empty( cFile := hbide_fetchAFile( ::oDlg, "Select a Tool" ) )
         hb_fNameSplit( cFile, , @cFileName )
         ::oUI:q_editName    : setText( cFileName )
         ::oUI:q_editCmdLine : setText( cFile )
         ::oUI:q_editParams  : setText( "" )
      ENDIF
      EXIT
   CASE buttonUpdate_clicked
      qItem := QListWidgetItem():from( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()

      IF ( nIndex := ascan( aTools, {|e_| e_[ 1 ] == cName } ) ) > 0
         ::aINI[ INI_TOOLS, nIndex ] := { ::oUI:q_editName:text(), ::oUI:q_editCmdLine:text(), ::oUI:q_editParams:text() }
         ::clearList()
         ::populateList()
      ENDIF
      EXIT
   CASE buttonClose_clicked
      ::oUI:done( 1 )
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:clearList()

   ::oUI:q_listNames:clear()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:populateList( aList )
   LOCAL a_

   DEFAULT aList TO ::aINI[ INI_TOOLS ]

   FOR EACH a_ IN aList
      ::oUI:q_listNames:addItem( a_[ 1 ] )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:execTool( cName )
   LOCAL nIndex, cCmd, cParams, qProcess, qStr

   IF ( nIndex := ascan( ::aTools, {|e_| e_[ 1 ] == cName } ) ) > 0
      cCmd     := ::aTools[ nIndex, 2 ]
      cParams  := ::aTools[ nIndex, 3 ]

      qProcess := QProcess():new()

      IF !empty( cParams )
         qStr := QStringList():new()
         qStr:append( cParams )
         qProcess:startDetached_1( cCmd, qStr )
      ELSE
         qProcess:startDetached_2( cCmd )
      ENDIF
      qProcess:waitForStarted()
      qProcess := NIL
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:buildToolsButton()
   LOCAL a_, qAct

   ::qToolsMenu := QMenu():new()
   FOR EACH a_ IN ::aTools
      qAct := ::qToolsMenu:addAction( a_[ 1 ] )
      ::connect( qAct, "triggered(bool)", {|| ::execTool( a_[ 1 ] ) } )
      aadd( ::aAct, qAct )
   NEXT
   ::qToolsButton := QToolButton():new()
   ::qToolsButton:setTooltip( "Tools & Utilities" )
   ::qToolsButton:setIcon( hbide_image( "tools" ) )
   ::qToolsButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qToolsButton:setMenu( ::qToolsMenu )

   ::connect( ::qToolsButton, "clicked()", {|| ::show() } )

   RETURN ::qToolsButton

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:buildPanelsButton()
   LOCAL cView

   ::qPanelsMenu := QMenu():new()
   ::addPanelsMenu( "Main" )
   FOR EACH cView IN ::aINI[ INI_VIEWS ]
      ::addPanelsMenu( cView )
   NEXT
   ::qPanelsButton := QToolButton():new()
   ::qPanelsButton:setTooltip( "Panels" )
   ::qPanelsButton:setIcon( hbide_image( "panel_8" ) )
   ::qPanelsButton:setPopupMode( QToolButton_MenuButtonPopup )
   ::qPanelsButton:setMenu( ::qPanelsMenu )

   ::connect( ::qPanelsButton, "clicked()", {|| ::oDK:setView( "New..." ) } )

   RETURN ::qPanelsButton

/*----------------------------------------------------------------------*/

METHOD IdeToolsManager:addPanelsMenu( cPrompt )
   LOCAL qAct

   qAct := ::qPanelsMenu:addAction( cPrompt )
   QAction():from( qAct ):setIcon( ::oDK:getPanelIcon( cPrompt ) )
   ::connect( qAct, "triggered(bool)", {|| ::oDK:setView( cPrompt ) } )
   aadd( ::aPanelsAct, qAct )

   RETURN Self

/*----------------------------------------------------------------------*/

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
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               20Feb2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define buttonInstall_clicked                     1
#define editInstall_textChanged                   2
#define buttonHome_clicked                        3
#define buttonBackward_clicked                    4
#define buttonForward_clicked                     5
#define buttonRefresh_clicked                     6
#define buttonPrint_clicked                       7
#define buttonPdf_clicked                         8
#define editIndex_textChanged                     9
#define treeDoc_doubleClicked                     10
#define treeDoc_itemSelectionChanged              11
#define editIndex_returnPressed                   12
#define lostIndex_ItemDoubleClicked               13
#define buttonUp_clicked                          14
#define browserView_anchorClicked                 15
#define tabWidgetContents_currentChanged          16
#define treeCategory_itemSelectionChanged         17

/*----------------------------------------------------------------------*/

#define DOC_FUN_BEGINS                           -5
#define DOC_FUN_ENDS                             -1
#define DOC_FUN_NONE                              0
#define DOC_FUN_TEMPLATE                          1
#define DOC_FUN_FUNCNAME                          2
#define DOC_FUN_CATEGORY                          3
#define DOC_FUN_SUBCATEGORY                       4
#define DOC_FUN_ONELINER                          5
#define DOC_FUN_SYNTAX                            6
#define DOC_FUN_ARGUMENTS                         7
#define DOC_FUN_RETURNS                           8
#define DOC_FUN_DESCRIPTION                       9
#define DOC_FUN_EXAMPLES                          10
#define DOC_FUN_TESTS                             11
#define DOC_FUN_FILES                             12
#define DOC_FUN_STATUS                            13
#define DOC_FUN_PLATFORMS                         14
#define DOC_FUN_SEEALSO                           15

/*----------------------------------------------------------------------*/

CLASS IdeDocFunction

   DATA   cName                                   INIT ""
   DATA   cTemplate                               INIT ""
   DATA   cCategory                               INIT ""
   DATA   cSubCategory                            INIT ""
   DATA   cOneliner                               INIT ""
   DATA   aSyntax                                 INIT {}
   DATA   aArguments                              INIT {}
   DATA   aReturns                                INIT {}
   DATA   aDescription                            INIT {}
   DATA   aExamples                               INIT {}
   DATA   aTests                                  INIT {}
   DATA   aFiles                                  INIT {}
   DATA   cStatus                                 INIT ""
   DATA   cPlatforms                              INIT ""
   DATA   cSeaAlso                                INIT ""

   DATA   aSource                                 INIT {}

   DATA   oTVItem
   DATA   cSourceTxt                              INIT ""

   METHOD new()                                   INLINE Self

   ENDCLASS

/*----------------------------------------------------------------------*/

CLASS IdeHarbourHelp INHERIT IdeObject

   DATA   oUI
   DATA   cPathInstall                            INIT ""
   DATA   cDocPrefix

   DATA   aNodes                                  INIT {}
   DATA   aFunctions                              INIT {}
   DATA   aFuncByFile                             INIT {}
   DATA   aHistory                                INIT {}
   DATA   aCategory                               INIT {}

   DATA   nCurTVItem                              INIT 0
   DATA   nCurInHist                              INIT 0

   DATA   qHiliter

   DATA   hIndex                                  INIT {=>}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()

   METHOD execEvent( nMode, p, p1 )

   METHOD setImages()
   METHOD setTooltips()
   METHOD setParameters()

   METHOD installSignals()
   METHOD refreshDocTree()
   METHOD updateViewer( aHtm )
   METHOD populateFuncDetails( n )
   METHOD populateTextFile( cTextFile )
   METHOD populateRootInfo()
   METHOD populatePathInfo( cPath )
   METHOD populateIndex()
   METHOD populateIndexedSelection()
   METHOD buildView( oFunc )
   METHOD print()
   METHOD exportAsPdf()
   METHOD paintRequested( pPrinter )
   METHOD parseTextFile( cTextFile, oParent )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::oUI := HbQtUI():new( ::resPath + "docviewgenerator.uic", ::oDlg:oWidget ):build()

   ::setImages()
   ::setTooltips()
   ::installSignals()
   ::setParameters()

   ::populateRootInfo()

   /* Should it be here */
   ::refreshDocTree()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:destroy()
   #if 0
   LOCAL a_
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "Function"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "File"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   FOR EACH a_ IN ::aNodes
      IF a_[ 2 ] == "Path"
         a_[ 1 ] := NIL
      ENDIF
   NEXT
   ::aNodes[ 1, 1 ] := NIL

   ::aNodes := NIL
   #endif

   ::oUI:q_treeDoc:clear()
   //::disconnect( ::oUI:q_treeDoc, "itemSelectionChanged()" )

   ::oUI:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setImages()
   LOCAL oUI := ::oUI

   oUI:q_buttonHome:setIcon( ::resPath + "dc_home.png" )
   oUI:q_buttonBackward:setIcon( ::resPath + "dc_left.png" )
   oUI:q_buttonForward:setIcon( ::resPath + "dc_right.png" )
   oUI:q_buttonUp:setIcon( ::resPath + "dc_up.png" )
   oUI:q_buttonRefresh:setIcon( ::resPath + "dc_refresh.png" )
   oUI:q_buttonPrint:setIcon( ::resPath + "dc_print.png" )
   oUI:q_buttonPdf:setIcon( ::resPath + "dc_pdffile.png" )

   oUI:q_buttonSave:setIcon( ::resPath + "save.png" )
   oUI:q_buttonExit:setIcon( ::resPath + "dc_quit.png" )

   oUI:q_buttonInstall:setIcon( ::resPath + "dc_folder.png" )

   oUI:q_buttonArgPlus:setIcon( ::resPath + "dc_plus.png" )
   oUI:q_buttonArgMinus:setIcon( ::resPath + "dc_delete.png" )
   oUI:q_buttonArgUp:setIcon( ::resPath + "dc_up.png" )
   oUI:q_buttonArgDown:setIcon( ::resPath + "dc_down.png" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setTooltips()
   LOCAL oUI := ::oUI

   oUI:q_buttonHome:setToolTip( "Home" )
   oUI:q_buttonBackward:setToolTip( "Backward" )
   oUI:q_buttonForward:setToolTip( "Forward" )
   oUI:q_buttonRefresh:setToolTip( "Refresh" )
   oUI:q_buttonUp:setToolTip( "Up" )
   oUI:q_buttonPrint:setToolTip( "Print" )
   oUI:q_buttonPdf:setToolTip( "Export as PDF Document" )

   oUI:q_buttonSave:setToolTip( "Save" )
   oUI:q_buttonExit:setToolTip( "Exit" )

   oUI:q_buttonInstall:setToolTip( "Select Harbour Installation Path" )

   oUI:q_buttonArgPlus:setToolTip( "Add new argument" )
   oUI:q_buttonArgMinus:setToolTip( "Delete argument" )
   oUI:q_buttonArgUp:setToolTip( "Up one position" )
   oUI:q_buttonArgDown:setToolTip( "Down one position" )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:setParameters()
   LOCAL oUI := ::oUI

   oUI:q_treeDoc:setHeaderHidden( .t. )
   oUI:q_treeCategory:setHeaderHidden( .t. )
   oUI:q_editInstall:setText( ::cWrkHarbour )

   ::qHiliter := ::oThemes:SetSyntaxHilighting( oUI:q_plainExamples, "Bare Minimum" )

   oUI:q_plainExamples:setFont( ::oFont:oWidget )
   oUI:q_plainDescription:setFont( ::oFont:oWidget )
   oUI:q_plainArguments:setFont( ::oFont:oWidget )
   oUI:q_plainArgDesc:setFont( ::oFont:oWidget )
   oUI:q_plainTests:setFont( ::oFont:oWidget )

   oUI:q_plainExamples:setLineWrapMode( QTextEdit_NoWrap )
   oUI:q_plainTests:setLineWrapMode( QTextEdit_NoWrap )

   oUI:q_treeDoc:expandsOnDoubleClick( .f. )

   oUI:q_browserView:setOpenLinks( .f. )
   oUI:q_tabWidgetContents:setFocusPolicy( Qt_NoFocus )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:installSignals()

   ::oUI:signal( "buttonInstall" , "clicked()"                 , {| | ::execEvent( buttonInstall_clicked  )     } )
   ::oUI:signal( "buttonHome"    , "clicked()"                 , {| | ::execEvent( buttonHome_clicked     )     } )
   ::oUI:signal( "buttonBackward", "clicked()"                 , {| | ::execEvent( buttonBackward_clicked )     } )
   ::oUI:signal( "buttonForward" , "clicked()"                 , {| | ::execEvent( buttonForward_clicked  )     } )
   ::oUI:signal( "buttonUp"      , "clicked()"                 , {| | ::execEvent( buttonUp_clicked       )     } )
   ::oUI:signal( "buttonRefresh" , "clicked()"                 , {| | ::execEvent( buttonRefresh_clicked  )     } )
   ::oUI:signal( "buttonPrint"   , "clicked()"                 , {| | ::execEvent( buttonPrint_clicked    )     } )
   ::oUI:signal( "buttonPdf"     , "clicked()"                 , {| | ::execEvent( buttonPdf_clicked      )     } )
   ::oUI:signal( "editInstall"   , "textChanged(QString)"      , {|p| ::execEvent( editInstall_textChanged, p ) } )
   ::oUI:signal( "editIndex"     , "textChanged(QString)"      , {|p| ::execEvent( editIndex_textChanged, p   ) } )
   ::oUI:signal( "editIndex"     , "returnPressed()"           , {| | ::execEvent( editIndex_returnPressed    ) } )
   ::oUI:signal( "listIndex"     , "itemDoubleClicked(QLWItem)", {|p| ::execEvent( lostIndex_ItemDoubleClicked, p ) } )
   ::oUI:signal( "browserView"   , "anchorClicked(QUrl)"       , {|p| ::execEvent( browserView_anchorClicked, p ) } )
   ::oUI:signal( "tabWidgetContents", "currentChanged(int)"    , {|p| ::execEvent( tabWidgetContents_currentChanged, p ) } )

   ::connect( ::oUI:q_treeDoc       , "itemSelectionChanged()" , {| | ::execEvent( treeDoc_itemSelectionChanged ) } )
   ::connect( ::oUI:q_treeCategory  , "itemSelectionChanged()" , {| | ::execEvent( treeCategory_itemSelectionChanged ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:execEvent( nMode, p, p1 )
   LOCAL cPath, qTWItem, cText, n, nn, nLen, cLower, qUrl

   HB_SYMBOL_UNUSED( p1 )

   SWITCH nMode

   CASE buttonInstall_clicked
      cPath := hbide_fetchADir( ::oDocViewDock, "Harbour Install Root" )
      IF !empty( cPath )
         ::oUI:q_editInstall:setText( cPath )
      ENDIF
      EXIT

   CASE tabWidgetContents_currentChanged
      IF p == 1
         ::oUI:q_editIndex:setFocus_1()
      ENDIF
      EXIT

   CASE browserView_anchorClicked
      qUrl := QUrl():from( p )
      cText := lower( qUrl:toString() )
      nLen := len( cText )
      IF ( n := ascan( ::aFunctions, {|e_| left( e_[ 6 ], nLen ) == cText } ) ) > 0
         ::oUI:q_listIndex:setCurrentItem( ::aFunctions[ n, 5 ] )
         ::populateIndexedSelection()
      ENDIF
      EXIT

   CASE lostIndex_ItemDoubleClicked
   CASE editIndex_returnPressed
      ::populateIndexedSelection()
      ::oUI:q_editIndex:setFocus_1()
      EXIT

   CASE editIndex_textChanged
      nLen := len( p )
      cLower := lower( p )
      IF ( n := ascan( ::aFunctions, {|e_| left( e_[ 6 ], nLen ) == cLower } ) ) > 0
         ::oUI:q_listIndex:setCurrentItem( ::aFunctions[ n, 5 ] )
      ENDIF
      EXIT

   CASE editInstall_textChanged
      IF hb_dirExists( p )
         ::oUI:q_editInstall:setStyleSheet( "" )
         ::cPathInstall := hbide_pathStripLastSlash( hbide_pathNormalized( p, .f. ) )
         ::oIde:cWrkHarbour := ::cPathInstall
      ELSE
         ::oUI:q_editInstall:setStyleSheet( getStyleSheet( "PathIsWrong" ) )
      ENDIF
      EXIT

   CASE buttonHome_clicked
      ::oUI:q_treeDoc:setCurrentItem( ::aNodes[ 1, 1 ], 0 )
      EXIT

   CASE buttonBackward_clicked
      IF ::nCurInHist > 1
         ::oUI:q_treeDoc:setCurrentItem( ::aNodes[ ::aHistory[ ::nCurInHist - 1 ], 1 ], 0 )
      ENDIF
      EXIT

   CASE buttonForward_clicked
      IF ::nCurInHist < len( ::aHistory )
         ::oUI:q_treeDoc:setCurrentItem( ::aNodes[ ::aHistory[ ::nCurInHist + 1 ], 1 ], 0 )
      ENDIF
      EXIT

   CASE buttonUp_clicked
      IF ::nCurInHist > 0 .AND. ::nCurInHist <= len( ::aHistory )
         ::oUI:q_treeDoc:setCurrentItem( ::oUI:q_treeDoc:itemAbove( ::oUI:q_treeDoc:currentItem( 0 ) ), 0 )
      ENDIF
      EXIT

   CASE buttonRefresh_clicked
      ::refreshDocTree()
      EXIT

   CASE buttonPrint_clicked
      ::print()
      EXIT

   CASE buttonPdf_clicked
      ::exportAsPdf()
      EXIT

   CASE treeCategory_itemSelectionChanged
      qTWItem := ::oUI:q_treeCategory:currentItem()
      n := ascan( ::aCategory, {|e_| hbqt_IsEqualGcQtPointer( e_[ 5 ]:pPtr, qTWItem ) } )
      IF n > 0
         IF ::aCategory[ n, 5 ]:childCount() == 0
            ::oUI:q_treeDoc:setCurrentItem( ::aCategory[ n, 4 ], 0 )
         ENDIF
      ENDIF
      EXIT

   CASE treeDoc_itemSelectionChanged
      qTWItem := QTreeWidgetItem():from( ::oUI:q_treeDoc:currentItem() )
      cText   := qTWItem:text( 0 )

      IF ( n := ascan( ::aNodes, {|e_| e_[ 5 ] == cText } ) ) > 0
         IF ( nn := ascan( ::aHistory, n ) ) == 0
            aadd( ::aHistory, n )
            ::nCurInHist := len( ::aHistory )
         ELSE
            ::nCurInHist := nn
         ENDIF
         ::nCurTVItem := n


         IF     ::aNodes[ n, 2 ] == "Root"
            ::populateRootInfo()
         ELSEIF ::aNodes[ n, 2 ] == "Path"
            ::populatePathInfo( ::aNodes[ n, 4 ] )
         ELSEIF ::aNodes[ n, 2 ] == "File"
            ::populateTextFile( ::aNodes[ n, 4 ] )
         ELSEIF ::aNodes[ n, 2 ] == "Function"
            ::populateFuncDetails( n )
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateIndexedSelection()
   LOCAL qItem := QListWidgetItem():from( ::oUI:q_listIndex:currentItem() )
   LOCAL cText, n

   cText := qItem:text()
   IF ( n := ascan( ::aFunctions, {|e_| e_[ 2 ] == cText } ) ) > 0
      ::oUI:q_treeDoc:setCurrentItem( ::aFunctions[ n, 4 ] )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:refreshDocTree()
   LOCAL aPaths, cFolder, cNFolder, aDocs, oTWItem, oParent, oParentF, cIcon
   LOCAL aDir, a_, cTextFile, cStripped

   IF empty( ::cPathInstall ) .OR. ! hb_dirExists( ::cPathInstall )
      RETURN Self
   ENDIF
   IF !empty( ::aNodes )
      RETURN Self
   ENDIF

   cIcon := ::resPath + "dc_folder.png"

   /* Clean Environment */
   ::oUI:q_treeDoc:clear()
   //
   ::aNodes      := {}
   ::aFuncByFile := {}
   ::aHistory    := {}
   ::aFunctions  := {}
   ::nCurTVItem  := 0
   ::nCurInHist  := 0

   aPaths := {}
   aDocs  := {}
   hbide_fetchSubPaths( @aPaths, ::cPathInstall, .t. )

   FOR EACH cFolder IN aPaths
      cNFolder := hbide_pathNormalized( cFolder, .t. )
      IF ( "/doc" $ cNFolder ) .OR. ( "/doc/en" $ cNFolder )
         aadd( aDocs, cFolder )
      ENDIF
   NEXT

   oTWItem := QTreeWidgetItem():new()
   oTWItem:setText( 0, ::cPathInstall )
   oTWItem:setIcon( 0, ::resPath + "dc_home.png" )
   oTWItem:setToolTip( 0, ::cPathInstall )
   oTWItem:setExpanded( .t. )

   ::oUI:q_treeDoc:addTopLevelItem( oTWItem )
   aadd( ::aNodes, { oTWItem, "Root", NIL /*oParent*/, ::cPathInstall, ::cPathInstall } )
   oParent := oTWItem

   FOR EACH cFolder IN aDocs
      oTWItem := QTreeWidgetItem():new()
      oTWItem:setText( 0, ( cStripped := hbide_stripRoot( ::cPathInstall, cFolder ) ) )
      oTWItem:setIcon( 0, cIcon )
      oTWItem:setToolTip( 0, cFolder )
      oParent:addChild( oTWItem )
      aadd( ::aNodes, { oTWItem, "Path", oParent, cFolder, cStripped } )

      oParentF := oTWItem
      aDir := directory( cFolder + "*.txt" )
      FOR EACH a_ IN aDir
         IF a_[ 5 ] != "D"
            cTextFile := cFolder + a_[ 1 ]
            oTWItem := QTreeWidgetItem():new()
            oTWItem:setText( 0, a_[ 1 ]  )
            oTWItem:setIcon( 0, ::resPath + "dc_textdoc.png" )
            oTWItem:setToolTip( 0, cTextFile )
            oParentF:addChild( oTWItem )
            aadd( ::aNodes, { oTWItem, "File", oParentF, cTextFile, a_[ 1 ] } )
            ::parseTextFile( cTextFile, oTWItem )
         ENDIF
      NEXT
   NEXT

   ::populateIndex()

   ::oUI:q_treeDoc:expandItem( oParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateIndex()
   LOCAL a_, qItem, oFunc, oParent, n
   LOCAL aUnq := {}

   asort( ::aFunctions, , , {|e_, f_| e_[ 2 ] < f_[ 2 ] } )

   ::oUI:q_listIndex:setSortingEnabled( .t. )

   FOR EACH a_ IN ::aFunctions
      IF !empty( a_[ 2 ] )
         qItem := QListWidgetItem():new()
         qItem:setText( a_[ 2 ] )
         a_[ 5 ] := qItem
         ::oUI:q_listIndex:addItem_1( qItem )
      ENDIF
   NEXT

   FOR EACH a_ IN ::aFunctions
      oFunc := a_[ 3 ]
      IF !empty( oFunc:cCategory )
         aadd( ::aCategory, { oFunc:cCategory, oFunc:cSubCategory, oFunc, a_[ 4 ], NIL, NIL } )
         IF ascan( aUnq, {|e_| e_[ 1 ] == oFunc:cCategory } ) == 0
            aadd( aUnq, { oFunc:cCategory, NIL } )
         ENDIF
      ENDIF
   NEXT
   IF !empty( ::aCategory )
      asort( ::aCategory, , , {|e_, f_| e_[ 1 ] < f_[ 1 ] } )
   ENDIF
   FOR EACH a_ IN aUnq
      qItem := QTreeWidgetItem():new()
      qItem:setText( 0, a_[ 1 ] )
      ::oUI:q_treeCategory:addTopLevelItem( qItem )
      a_[ 2 ] := qItem
   NEXT
   FOR EACH a_ IN ::aCategory
      IF ( n := ascan( aUnq, {|e_| e_[ 1 ] == a_[ 1 ] } ) ) > 0
         oParent := aUnq[ n, 2 ]

         qItem := QTreeWidgetItem():new()
         qItem:setText( 0, a_[ 3 ]:cName )

         oParent:addChild( qItem )
         a_[ 5 ] := qItem
         a_[ 6 ] := oParent
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:parseTextFile( cTextFile, oParent )
   LOCAL a_, s, nPart, oFunc, oTWItem
   LOCAL lIsFunc := .f.
   LOCAL cIcon   := ::resPath + "dc_function.png"
   LOCAL aFn     := {}
   LOCAL nParsed := ascan( ::aFuncByFile, {|e_| e_[ 1 ] == cTextFile } )

   IF nParsed == 0
      nPart := DOC_FUN_NONE

      a_:= hbide_readSource( cTextFile )

      FOR EACH s IN a_
         DO CASE

         CASE "$DOC$"         $ s
            lIsFunc := .t.
            nPart   := DOC_FUN_BEGINS
            oFunc   := IdeDocFunction():new()

         CASE "$END$"         $ s
            IF lIsFunc
               lIsFunc := .f.
               nPart   := DOC_FUN_ENDS
               oTWItem := QTreeWidgetItem():new()
               oTWItem:setText( 0, oFunc:cName )
               oTWItem:setIcon( 0, cIcon )
               oTWItem:setTooltip( 0, oFunc:cName )
               oParent:addChild( oTWItem )
               aadd( ::aNodes, { oTWItem, "Function", oParent, cTextFile + "<::>" + oFunc:cName, oFunc:cName } )
               aadd( ::aFunctions, { cTextFile, oFunc:cName, oFunc, oTWItem, NIL, lower( oFunc:cName ) } )
               aadd( aFn, oFunc )
            ENDIF

         CASE "$TEMPLATE$"    $ s
            nPart := DOC_FUN_TEMPLATE
         CASE "$FUNCNAME$"    $ s   .OR.  "$NAME$" $ s
            nPart := DOC_FUN_FUNCNAME
         CASE "$CATEGORY$"    $ s
            nPart := DOC_FUN_CATEGORY
         CASE "$SUBCATEGORY$" $ s
            nPart := DOC_FUN_SUBCATEGORY
         CASE "$ONELINER$"    $ s
            nPart := DOC_FUN_ONELINER
         CASE "$SYNTAX$"      $ s
            nPart := DOC_FUN_SYNTAX
         CASE "$ARGUMENTS$"   $ s
            nPart := DOC_FUN_ARGUMENTS
         CASE "$RETURNS$"     $ s
            nPart := DOC_FUN_RETURNS
         CASE "$DESCRIPTION$" $ s
            nPart := DOC_FUN_DESCRIPTION
         CASE "$EXAMPLES$"    $ s
            nPart := DOC_FUN_EXAMPLES
         CASE "$TESTS$"       $ s
            nPart := DOC_FUN_TESTS
         CASE "$FILES$"       $ s
            nPart := DOC_FUN_FILES
         CASE "$STATUS$"       $ s
            nPart := DOC_FUN_STATUS
         CASE "$PLATFORMS$"   $ s  .OR.  "$COMPLIANCE$" $ s
            nPart := DOC_FUN_PLATFORMS
         CASE "$SEEALSO$"     $ s
            nPart := DOC_FUN_SEEALSO
         OTHERWISE
            IF ! lIsFunc
               LOOP   // It is a fake line not within $DOC$ => $END$ block
            ENDIF
            s := substr( s, 9 )

            SWITCH nPart
            CASE DOC_FUN_BEGINS
               EXIT
            CASE DOC_FUN_TEMPLATE
               oFunc:cTemplate    := s
               EXIT
            CASE DOC_FUN_FUNCNAME
               oFunc:cName        := alltrim( s )
               EXIT
            CASE DOC_FUN_CATEGORY
               oFunc:cCategory    := alltrim( s )
               EXIT
            CASE DOC_FUN_SUBCATEGORY
               oFunc:cSubCategory := alltrim( s )
               EXIT
            CASE DOC_FUN_ONELINER
               oFunc:cOneLiner    := s
               EXIT
            CASE DOC_FUN_SYNTAX
               aadd( oFunc:aSyntax     , s )
               EXIT
            CASE DOC_FUN_ARGUMENTS
               aadd( oFunc:aArguments  , s )
               EXIT
            CASE DOC_FUN_RETURNS
               aadd( oFunc:aReturns    , s )
               EXIT
            CASE DOC_FUN_DESCRIPTION
               aadd( oFunc:aDescription, s )
               EXIT
            CASE DOC_FUN_EXAMPLES
               aadd( oFunc:aExamples   , s )
               EXIT
            CASE DOC_FUN_TESTS
               aadd( oFunc:aTests      , s )
               EXIT
            CASE DOC_FUN_FILES
               aadd( oFunc:aFiles      , s )
               EXIT
            CASE DOC_FUN_STATUS
               oFunc:cStatus    := alltrim( s )
               EXIT
            CASE DOC_FUN_PLATFORMS
               oFunc:cPlatForms := alltrim( s )
               EXIT
            CASE DOC_FUN_SEEALSO
               oFunc:cSeaAlso   := alltrim( s )
               EXIT
            OTHERWISE
               nPart := DOC_FUN_NONE
               EXIT
            ENDSWITCH
         ENDCASE
      NEXT

      aadd( ::aFuncByFile, { cTextFile, aFn } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:updateViewer( aHtm )

   ::oUI:q_browserView:setHTML( hbide_arrayToMemo( aHtm ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateRootInfo()
   LOCAL aHtm := {}

   aadd( aHtm, "<HTML>" )
   //aadd( aHtm, ' <BODY ALIGN=center VALIGN=center background="resources/harbour.png" bgproperties="fixed">' )
   aadd( aHtm, ' <BODY ALIGN=center VALIGN=center>' )
   aadd( aHtm, '  <H1><FONT color=green>' + "Welcome" + '</FONT></H1>' )
   aadd( aHtm, '  <BR>' + '&nbsp;' + '</BR>' )
   aadd( aHtm, '  <H2><FONT color=blue>' + ::cPathInstall + '</FONT></H2>' )
   aadd( aHtm, '  <BR>&nbsp;</BR>' )
   aadd( aHtm, '  <BR>&nbsp;</BR>' )
   aadd( aHtm, '  <IMG src="' + 'resources/harbour.png' + '" width="300" height="200"</IMG></BR>' )
   aadd( aHtm, " </BODY>" )
   aadd( aHtm, "</HTML>" )

   ::updateViewer( aHtm )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populatePathInfo( cPath )
   LOCAL aHtm := {}

   aadd( aHtm, "<HTML>" )
   aadd( aHtm, " <BODY ALIGN=center VALIGN=center>" )
   aadd( aHtm, '  <H2><FONT color=blue>' + cPath + '</FONT></H2>' )
   aadd( aHtm, " </BODY>" )
   aadd( aHtm, "</HTML>" )

   ::updateViewer( aHtm )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateTextFile( cTextFile )
   LOCAL aHtm, aFn, oFunc
   LOCAL nParsed := ascan( ::aFuncByFile, {|e_| e_[ 1 ] == cTextFile } )

   /* Build HTML */
   aHtm := {}
   aadd( aHtm, "<HTML>" )
   aadd( aHtm, " <BODY>" )
   aadd( aHtm, '  <H3 align=center><FONT color=blue>' + cTextFile + '</FONT></H3>' )
   aadd( aHtm, '   <BR>' + '&nbsp;' + '</BR>' )
   IF nParsed > 0
      aFn := ::aFuncByFile[ nParsed, 2 ]
      FOR EACH oFunc IN aFn
         IF hb_isObject( oFunc )
            aadd( aHtm, '   <BR>' + oFunc:cName + '</BR>' )
         ENDIF
      NEXT
   ENDIF
   aadd( aHtm, " </BODY>" )
   aadd( aHtm, "</HTML>" )

   ::updateViewer( aHtm )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:populateFuncDetails( n )
   LOCAL oTWItem := ::aNodes[ n, 1 ]
   LOCAL nIndex, oFunc

   nIndex := ascan( ::aFunctions, {|e_| e_[ 4 ] == oTWItem } )
   oFunc := ::aFunctions[ nIndex, 3 ]

   ::oUI:q_editTemplate    :setText( iif( empty( oFunc:cTemplate ), "FUNCTION", oFunc:cTemplate ) )
   ::oUI:q_editName        :setText( oFunc:cName        )
   ::oUI:q_editCategory    :setText( oFunc:cCategory    )
   ::oUI:q_editSubCategory :setText( oFunc:cSubCategory )
   ::oUI:q_editOneLiner    :setText( oFunc:cOneLiner    )
   ::oUI:q_editSeeAlso     :setText( oFunc:cSeaAlso     )
   ::oUI:q_editStatus      :setText( oFunc:cStatus      )
   ::oUI:q_editPlatforms   :setText( oFunc:cPlatforms   )

   ::oUI:q_editReturns     :setText( hbide_arrayToMemoEx( oFunc:aReturns ) )  //TODO : a line

   ::oUI:q_plainSyntax     :setPlainText( hbide_arrayToMemoEx2( oFunc:aSyntax      ) )
   ::oUI:q_plainFiles      :setPlainText( hbide_arrayToMemoEx2( oFunc:aFiles       ) )
   ::oUI:q_plainDescription:setPlainText( hbide_arrayToMemoEx2( oFunc:aDescription ) )
   ::oUI:q_plainExamples   :setPlainText( hbide_arrayToMemoEx2( oFunc:aExamples    ) )
   ::oUI:q_plainTests      :setPlainText( hbide_arrayToMemoEx2( oFunc:aTests       ) )
   ::oUI:q_plainArguments  :setPlainText( hbide_arrayToMemoEx2( oFunc:aArguments   ) )

   ::oUI:q_editTextPath    :setText( ::aFunctions[ nIndex, 1 ] )

   ::buildView( oFunc )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:buildView( oFunc )
   LOCAL s, x, y, v, w, z, n, s1, a_
   LOCAL aHtm := {}

   aadd( aHtm, "<HTML>" )

   aadd( aHtm, '<head>                                                             ' )
   aadd( aHtm, '  <meta name="Author" CONTENT=Pritpal Bedi [pritpal@vouchcac.com]">' )
   aadd( aHtm, '  <meta http-equiv="content-style-type" content="text/css" >       ' )
   aadd( aHtm, '  <meta http-equiv="content-script-type" content="text/javascript">' )
   aadd( aHtm, '                                                                   ' )
   aadd( aHtm, '  <style type="text/css">                                          ' )
   aadd( aHtm, '    th                                                             ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      colspan          : 1;                                        ' )
   aadd( aHtm, '      text-align       : center;                                   ' )
   aadd( aHtm, '      vertical-align   : baseline;                                 ' )
   aadd( aHtm, '      horizontal-align : left;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '    td                                                             ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      vertical-align   : top;                                      ' )
   aadd( aHtm, '      horizontal-align : left;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '    pre                                                            ' )
   aadd( aHtm, '    {                                                              ' )
   aadd( aHtm, '      font-family      : Courier New;                              ' )
   aadd( aHtm, '      font-size        : .9;                                       ' )
   aadd( aHtm, '      color            : black;                                    ' )
   aadd( aHtm, '      cursor           : text;                                     ' )
   aadd( aHtm, '    }                                                              ' )
   aadd( aHtm, '  </style>                                                         ' )
   aadd( aHtm, '</head>                                                            ' )

   aadd( aHtm, ' <BODY>'    )
   aadd( aHtm, '  <CENTER>' )

   s := '   <TABLE '            +;
        'Border='      + '0 '   +;
        'Frame='       + 'ALL ' +;
        'CellPadding=' + '0 '   +;
        'CellSpacing=' + '0 '   +;
        'Cols='        + '1 '   +;
        'Width='       + '95% ' +;
        '   >'
   aadd( aHtm, s )

   aadd( aHtm, '<CAPTION align=TOP><FONT SIZE="6"><B>' + oFunc:cName + '</B></FONT></CAPTION>' )

   aadd( aHtm, "<TR><TD align=CENTER ><B>" + oFunc:cOneLiner + "</B></TD></TR>" )

   x := '<TR><TD align=LEFT><font size="5" color="#FF4719">' ; y := "</font></TD></TR>"
   v := '<TR><TD margin-left: 20px><pre>'                    ; w := "</pre></TD></TR>"
   z := "<TR><TD>&nbsp;</TD></TR>"

   aadd( aHtm, x + "Syntax"         + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aSyntax      ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Arguments"      + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aArguments   ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Returns"        + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aReturns     ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Description"    + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aDescription ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Examples"       + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aExamples    ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Files"          + y )
   aadd( aHtm, v + hbide_arrayToMemoHtml( oFunc:aFiles       ) + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "SeeAlso"        + y )
   aadd( aHtm, "<TR><TD>" )
   a_:= hb_atokens( oFunc:cSeaAlso, "," )
   IF !empty( a_ )
      FOR EACH s IN a_
         IF ( n := at( "(", s ) ) > 0
            s1 := substr( s, 1, n-1 )
            aadd( aHtm, '<a href="' + s1 + '">' + s  + "</a>" + ;
                                     iif( s:__enumIndex() == len( a_ ), "", ",&nbsp;" ) )
         ENDIF
      NEXT
   ELSE
      aadd( aHtm, "&nbsp;" )
   ENDIF
   aadd( aHtm, "</TD></TR>" )
   aadd( aHtm, z )
   aadd( aHtm, x + "Platforms"      + y )
   aadd( aHtm, v + oFunc:cPlatforms + w )
   aadd( aHtm, z )
   aadd( aHtm, x + "Status"         + y )
   aadd( aHtm, v + oFunc:cStatus    + w )
   aadd( aHtm, z )

   aadd( aHtm, "   </TABLE>"  )
   aadd( aHtm, "  </CENTER>"  )
   aadd( aHtm, " </BODY>"     )
   aadd( aHtm, "</HTML>"      )

   ::updateViewer( aHtm )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:exportAsPdf()
   LOCAL cPdf, qPrinter, cExt, cPath, cFile

   IF !empty( cPdf := hbide_fetchAFile( ::oDlg, "Provide a file name", { { "Pdf Documents", "*.pdf" } } ) )
      hb_fNameSplit( cPdf, @cPath, @cFile, @cExt )
      IF empty( cExt ) .OR. lower( cExt ) != ".pdf"
         cPdf := cPath + cFile + ".pdf"
      ENDIF
      qPrinter := QPrinter():new()
      qPrinter:setOutputFileName( cPdf )
      ::oUI:q_browserView:print( qPrinter )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:print()
   LOCAL qDlg

   qDlg := QPrintPreviewDialog():new( ::oUI )
   qDlg:setWindowTitle( "Harbour Help Document" )
   Qt_Slots_Connect( ::pSlots, qDlg, "paintRequested(QPrinter)", {|p| ::paintRequested( p ) } )
   qDlg:exec()
   Qt_Slots_disConnect( ::pSlots, qDlg, "paintRequested(QPrinter)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeHarbourHelp:paintRequested( pPrinter )
   LOCAL qPrinter := QPrinter():configure( pPrinter )

   ::oUI:q_browserView:print( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/


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
 *                               18Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define buttonNew_clicked                         101
#define buttonRename_clicked                      102
#define buttonDelete_clicked                      103
#define buttonClear_clicked                       104
#define buttonGetSel_clicked                      105
#define buttonUpdate_clicked                      106
#define listNames_itemSelectionChanged            107
#define oTree_itemSelected                        108
#define oTree_contextMenu                         109

/*----------------------------------------------------------------------*/

CLASS IdeSkeletons INHERIT IdeObject

   DATA   oRoot
   DATA   oTree
   DATA   nPosCursor
   DATA   aItems                                  INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD execEvent( nMode, p )
   METHOD postSkeleton( cSkeleton )
   METHOD selectByMenuAndPostText( qEdit )
   METHOD getText( cSkeleton )
   METHOD parseText( cText )
   METHOD parseMeta( cMeta )
   METHOD postText( qEdit, cText )
   METHOD showTree()
   METHOD clearTree()
   METHOD updateTree()
   METHOD saveAs( cText, cName )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:destroy()

   IF !empty( ::oUI )
      ::disconnect( ::oUI:q_buttonNew   , "clicked()" )
      ::disconnect( ::oUI:q_buttonRename, "clicked()" )
      ::disconnect( ::oUI:q_buttonDelete, "clicked()" )
      ::disconnect( ::oUI:q_buttonClear , "clicked()" )
      ::disconnect( ::oUI:q_buttonGetSel, "clicked()" )
      ::disconnect( ::oUI:q_buttonUpdate, "clicked()" )
      ::disconnect( ::oUI:q_listNames   , "itemSelectionChanged()" )

      ::oUI:destroy()
   ENDIF

   ::aItems := {}
   ::oTree  := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:show()

   IF empty( ::oUI )
      ::oUI := HbQtUI():new( hbide_uic( "skeletons" ) ):build()

      ::oSkeltnDock:oWidget:setWidget( ::oUI:oWidget )

      ::connect( ::oUI:q_buttonNew   , "clicked()"             , {|| ::execEvent( buttonNew_clicked              ) } )
      ::connect( ::oUI:q_buttonRename, "clicked()"             , {|| ::execEvent( buttonRename_clicked           ) } )
      ::connect( ::oUI:q_buttonDelete, "clicked()"             , {|| ::execEvent( buttonDelete_clicked           ) } )
      ::connect( ::oUI:q_buttonClear , "clicked()"             , {|| ::execEvent( buttonClear_clicked            ) } )
      ::connect( ::oUI:q_buttonGetSel, "clicked()"             , {|| ::execEvent( buttonGetSel_clicked           ) } )
      ::connect( ::oUI:q_buttonUpdate, "clicked()"             , {|| ::execEvent( buttonUpdate_clicked           ) } )
      ::connect( ::oUI:q_listNames   , "itemSelectionChanged()", {|| ::execEvent( listNames_itemSelectionChanged ) } )

      //::oUI:q_editCode:setFontFamily( "Courier New" )
      //::oUI:q_editCode:setFontPointSize( 10 )

      //::oUI:q_editCode:setFont( ::oFont:oWidget )
      aeval( ::aSkltns, {|e_| ::oUI:q_listNames:addItem( e_[ 1 ] ) } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:execEvent( nMode, p )
   LOCAL cName, cNewName, qItem, cCode, n

   HB_SYMBOL_UNUSED( p )

   SWITCH nMode

   CASE buttonNew_clicked
      IF !empty( cName := hbide_fetchAString( ::oUI:q_listNames, "", "Name", "New Skeleton" ) )
         ::oUI:q_listNames:addItem( cName )
         aadd( ::oIde:aSkltns, { cName, "" } )
         ::oUI:q_listNames:setCurrentRow( len( ::aSkltns ) - 1 )
      ENDIF
      EXIT

   CASE buttonRename_clicked
      qItem := QListWidgetItem():configure( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF !empty( cNewName := hbide_fetchAString( ::oUI:q_listNames, cName, "Name", "Change Skeleton's Name" ) )
         qItem:setText( cNewName )
         n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
         ::aSkltns[ n, 1 ] := cNewName
      ENDIF
      EXIT

   CASE buttonDelete_clicked
      qItem := QListWidgetItem():configure( ::oUI:q_listNames:currentItem() )
      ::oUI:q_listNames:removeItemWidget( qItem )
      EXIT

   CASE buttonClear_clicked
      ::oUI:q_editCode:clear()
      EXIT

   CASE buttonGetSel_clicked
      IF !empty( cCode := ::oEM:getSelectedText() )
         // TODO: Format cCode
         cCode := strtran( cCode, chr( 0x2029 ), chr( 10 ) )
         ::oUI:q_editCode:setPlainText( cCode )
      ENDIF
      EXIT

   CASE buttonUpdate_clicked
      // Update the skeleton code and save the skeleton's buffer | file
      qItem := QListWidgetItem():configure( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()
      IF !empty( cName )
         n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
         ::aSkltns[ n,2 ] := ::oUI:q_editCode:toPlainText()
         hbide_saveSkltns( ::oIde )
         ::updateTree()
      ENDIF
      EXIT

   CASE listNames_itemSelectionChanged
      qItem := QListWidgetItem():configure( ::oUI:q_listNames:currentItem() )
      cName := qItem:text()
      n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
      ::oUI:q_editCode:setPlainText( ::aSkltns[ n,2 ] )
      EXIT

   CASE oTree_contextMenu

      EXIT

   CASE oTree_itemSelected
      ::oIde:manageFocusInEditor()
      ::postSkeleton( p:caption )
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:saveAs( cText, cName )
   LOCAL n

   IF empty( cName )
      cName := hbide_fetchAString( ::oDlg, "", "Skeleton's Name" )
   ENDIF
   IF !empty( cName )
      n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cName } )
      IF n > 0
         ::aSkltns[ n, 2 ] := cText
      ELSE
         aadd( ::aSkltns, { cName, cText } )
         ::updateTree()
      ENDIF
      hbide_saveSkltns( ::oIde )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:postSkeleton( cSkeleton )
   LOCAL oEdit, cText := ::getText( cSkeleton )

   IF !empty( cText )
      IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
         ::postText( oEdit:qEdit, cText )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:selectByMenuAndPostText( qEdit )
   LOCAL cText, qCursor, qRect, qMenu, qAct, pAct, a_

   IF !empty( ::aSkltns )
      qCursor := QTextCursor():from( qEdit:textCursor() )
      qRect   := QRect():from( qEdit:cursorRect( qCursor ) )

      qMenu := QMenu():new( qEdit )
      FOR EACH a_ IN ::aSkltns
         qMenu:addAction( a_[ 1 ] )
      NEXT

      pAct := qMenu:exec_1( qEdit:mapToGlobal( QPoint():new( qRect:x(), qRect:y() ) ) )
      IF !hbqt_isEmptyQtPointer( pAct )
         qAct  := QAction():from( pAct )

         IF !empty( cText := ::getText( qAct:text() ) )
            ::postText( qEdit, cText )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:parseMeta( cMeta )
   LOCAL xVal := cMeta, cData, n

   IF ( n := at( ":", cMeta ) ) > 0
      cMeta := substr( cMeta, 1, n - 1 )
      cData := substr( cMeta, n + 1 )
   ENDIF

   SWITCH upper( cMeta )

   CASE "CUR"
      xVal := 0
      EXIT

   CASE "PROMPT"
      DEFAULT cData TO "A string value ?"
      xVal := hbide_fetchAString( ::oDlg, "", cData, "Fetch a skeleton's prompt" )
      EXIT

   ENDSWITCH

   RETURN xVal

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:parseText( cText )
   LOCAL n, n1, cTkn, xVal

   n := 0
   DO WHILE .t.
      IF ( n := hb_at( "<-", cText, n ) ) > 0
         IF ( n1 := hb_at( ">", cText, n ) ) > 0
            IF !empty( cTkn := substr( cText, n + 2, n1 - n - 2 ) )

               xVal := ::parseMeta( hbide_evalAsString( cTkn ) )

               IF valtype( xVal ) == "C"
                  cText := substr( cText, 1, n - 1 ) + xVal + substr( cText, n1 + 1 )

               ELSEIF valtype( xVal ) == "N"
                  cText := substr( cText, 1, n - 1 ) + "" + substr( cText, n1 + 1 )
                  ::nPosCursor := n

               ENDIF
            ENDIF
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:getText( cSkeleton )
   LOCAL n, cText := ""

   IF ( n := ascan( ::aSkltns, {|e_| e_[ 1 ] == cSkeleton } ) ) > 0
      cText := ::aSkltns[ n, 2 ]
   ENDIF

   RETURN cText

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:postText( qEdit, cText )
   LOCAL s, a_, nCol, nPos, nRowCur, nColCur, n
   LOCAL qCursor := QTextCursor():from( qEdit:textCursor() )

   ::nPosCursor := NIL

   nPos := qCursor:position()
   nCol := qCursor:columnNumber()

   a_:= hbide_memoToArray( cText )
   FOR EACH s IN a_
      IF ( n := s:__enumIndex() ) > 0
         s := iif( n > 1, space( nCol ), "" ) + ::parseText( s )
         IF valtype( ::nPosCursor ) == "N"
            IF empty( nRowCur )
               nRowCur := n
               nColCur := ::nPosCursor
            ENDIF
         ENDIF
      ENDIF
   NEXT

   qCursor:insertText( hbide_arrayToMemoEx( a_ ) )
   qCursor:setPosition( nPos )
   IF !empty( nRowCur )
      qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, nRowCur -1 )
   ENDIF
   IF !empty( nColCur )
      qCursor:movePosition( QTextCursor_StartOfBlock )
      qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, nCol + nColCur - 1 )
   ENDIF
   qEdit:setTextCursor( qCursor )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:showTree()

   IF empty( ::oTree )
      ::oTree := XbpTreeView():new()
      ::oTree:hasLines   := .T.
      ::oTree:hasButtons := .T.
      ::oTree:create( ::oSkltnsTreeDock, , { 0,0 }, { 10,10 }, , .t. )

      ::oTree:setStyleSheet( GetStyleSheet( "QTreeWidgetHB" ) )
      ::oTree:oWidget:setMinimumWidth( 100 )
      ::oTree:oWidget:setSizePolicy_1( QSizePolicy_MinimumExpanding, QSizePolicy_Preferred )
      ::oTree:oWidget:setIconSize( QSize():new( 12,12 ) )
      ::oTree:oWidget:setIndentation( 12 )

      ::oTree:itemSelected  := {|oItem         | ::execEvent( oTree_itemSelected, oItem ) }
      ::oTree:hbContextMenu := {|mp1, mp2, oXbp| ::execEvent( oTree_contextMenu , { mp1, mp2, oXbp } ) }

      ::oRoot := ::oTree:rootItem:addItem( "Skeletons" )

      ::updateTree()

      ::oRoot:expand( .t. )

      ::oSkltnsTreeDock:oWidget:setWidget( ::oTree:oWidget )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:updateTree()
   LOCAL oItem, a_

   ::clearTree()

   FOR EACH a_ IN ::aSkltns
      oItem := ::oRoot:addItem( a_[ 1 ] )
      oItem:tooltipText := a_[ 2 ]
      aadd( ::aItems, oItem )
   NEXT

   ::oRoot:oWidget:sortChildren( 0 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSkeletons:clearTree()
   LOCAL oItem

   FOR EACH oItem IN ::aItems
      ::oRoot:delItem( oItem )
      oItem := NIL
   NEXT
   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/


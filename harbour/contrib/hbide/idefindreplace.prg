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

CLASS IdeUpDown INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD position()
   METHOD execEvent( cEvent, p )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:position()
   LOCAL qRect, qHSBar, qVSBar, qEdit

   IF !empty( qEdit := ::oEM:getEditCurrent() )
      ::oUI:setParent( qEdit )

      qHSBar := QScrollBar():from( qEdit:horizontalScrollBar() )
      qVSBar := QScrollBar():from( qEdit:verticalScrollBar() )

      qRect  := QRect():from( qEdit:geometry() )

      ::oUI:move( qRect:width()  - ::oUI:width()  - iif( qVSBar:isVisible(), qVSBar:width() , 0 ), ;
                  qRect:height() - ::oUI:height() - iif( qHSBar:isVisible(), qHSBar:height(), 0 ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:show()
   LOCAL oEdit

   IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
      IF ::oIde:lCurEditsMdi
         oEdit:qEdit:hbGetSelectionInfo()
         IF oEdit:aSelectionInfo[ 1 ] > -1
            ::oUI:setEnabled( .t. )
         ELSE
            ::oUI:setEnabled( .f. )
         ENDIF
      ELSE
         ::position()
         oEdit:qEdit:hbGetSelectionInfo()
         IF oEdit:aSelectionInfo[ 1 ] > -1
            ::oUI:show()
         ELSE
            ::oUI:hide()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   IF ::oIde:lCurEditsMdi
      ::oUI := hbide_getUI( "updown_v" )
   ELSE
      ::oUI := hbide_getUI( "updown", ::oIde:oDlg:oWidget )
   ENDIF

   ::oUI:setWindowFlags( hb_bitOr( Qt_Tool, Qt_FramelessWindowHint ) )
   ::oUI:setFocusPolicy( Qt_NoFocus )

   ::oUI:q_buttonPrev:setIcon( hbide_image( "go-prev" ) )
   ::oUI:q_buttonPrev:setToolTip( "Find Previous" )
   ::oUI:q_buttonPrev:connect( "clicked()", {|| ::execEvent( "buttonPrev_clicked" ) } )
   //
   ::oUI:q_buttonNext:setIcon( hbide_image( "go-next" ) )
   ::oUI:q_buttonNext:setToolTip( "Find Next" )
   ::oUI:q_buttonNext:connect( "clicked()", {|| ::execEvent( "buttonNext_clicked" ) } )
   //
   ::oUI:q_buttonFirst:setIcon( hbide_image( "go-first" ) )
   ::oUI:q_buttonFirst:setToolTip( "Find First" )
   ::oUI:q_buttonFirst:connect( "clicked()", {|| ::execEvent( "buttonFirst_clicked" ) } )
   //
   ::oUI:q_buttonLast:setIcon( hbide_image( "go-last" ) )
   ::oUI:q_buttonLast:setToolTip( "Find Last" )
   ::oUI:q_buttonLast:connect( "clicked()", {|| ::execEvent( "buttonLast_clicked" ) } )
   //
   ::oUI:q_buttonAll:setIcon( hbide_image( "hilight-all" ) )
   ::oUI:q_buttonAll:setToolTip( "Highlight All" )
   ::oUI:q_buttonAll:connect( "clicked()", {|| ::execEvent( "buttonAll_clicked" ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:execEvent( cEvent, p )
   LOCAL cText, oEdit

   HB_SYMBOL_UNUSED( p )

   IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
      cText := oEdit:getSelectedText()
   ENDIF

   IF !empty( cText )
      SWITCH cEvent
      CASE "buttonPrev_clicked"
         oEdit:findEx( cText, QTextDocument_FindBackward, 0 )
         EXIT
      CASE "buttonNext_clicked"
         oEdit:findEx( cText, 0, 0 )
         EXIT
      CASE "buttonFirst_clicked"
         oEdit:findEx( cText, 0, 1 )
         EXIT
      CASE "buttonLast_clicked"
         oEdit:findEx( cText, QTextDocument_FindBackward, 2 )
         EXIT
      CASE "buttonAll_clicked"
         oEdit:highlightAll( cText )
         EXIT
      ENDSWITCH
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:destroy()

   IF hb_isObject( ::oUI )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
/*----------------------------------------------------------------------*/

CLASS IdeSearchReplace INHERIT IdeObject

   DATA   oXbp
   DATA   qFindLineEdit
   DATA   qReplLineEdit
   DATA   nCurDirection                           INIT 0
   DATA   cFind                                   INIT ""

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD beginFind()
   METHOD setFindString( cText )
   METHOD find( cText, lBackward )
   METHOD startFromTop()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::oUI := hbide_getUI( "searchreplace" )

   ::oUI:setFocusPolicy( Qt_StrongFocus )

   ::oUI:q_frameFind:setStyleSheet( "" )
   ::oUI:q_frameReplace:setStyleSheet( "" )

   ::oUI:q_buttonClose:setIcon( hbide_image( "closetab" ) )
   ::oUI:q_buttonClose:setToolTip( "Close" )
   ::oUI:q_buttonClose:connect( "clicked()", {|| ::oUI:hide() } )

   ::oUI:q_buttonNext:setIcon( hbide_image( "next" ) )
   ::oUI:q_buttonNext:setToolTip( "Find Next" )
   ::oUI:q_buttonNext:connect( "clicked()", {|| ::find( ::cFind ), ::oIde:manageFocusInEditor() } )

   ::oUI:q_buttonPrev:setIcon( hbide_image( "previous" ) )
   ::oUI:q_buttonPrev:setToolTip( "Find Previous" )
   ::oUI:q_buttonPrev:connect( "clicked()", {|| ::find( ::cFind, .t. ), ::oIde:manageFocusInEditor() } )

   ::oUI:q_checkReplace:setChecked( 0 )
   ::oUI:q_checkReplace:connect( "stateChanged(int)", {|i| ;
                               ::oUI:q_comboReplace:setEnabled( i == 2 ), ;
                               ::oUI:q_buttonReplace:setEnabled( i == 2 ), ;
                               iif( i == 2, ::oUI:q_frameReplace:show(), ::oUI:q_frameReplace:hide() ) } )

   ::qFindLineEdit := QLineEdit():from( ::oUI:q_comboFind:lineEdit() )
   ::qFindLineEdit:setFocusPolicy( Qt_StrongFocus )
   ::qFindLineEdit:setStyleSheet( "background-color: white;" )
   ::qFindLineEdit:connect( "textChanged(QString)", {|cText| ::setFindString( cText ) } )
   ::qFindLineEdit:connect( "returnPressed()"     , {|| ::find( ::cFind ) } )

   ::qReplLineEdit := QLineEdit():from( ::oUI:q_comboReplace:lineEdit() )
   ::qReplLineEdit:setFocusPolicy( Qt_StrongFocus )
   ::qReplLineEdit:setStyleSheet( "background-color: white;" )

   ::oUI:q_checkReplace:setEnabled( .f. )
   ::oUI:q_frameReplace:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:destroy()

   IF hb_isObject( ::oUI )

      ::qFindLineEdit:disconnect( "textChanged(QString)" )
      ::qFindLineEdit:disconnect( "returnPressed()"      )

      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:find( cText, lBackward )
   LOCAL qCursor, qDoc, qCur, qReg
   LOCAL lFound := .f.
   LOCAL nFlags := 0

   DEFAULT lBackward TO .f.

   ::nCurDirection := iif( lBackward, QTextDocument_FindBackward, 0 )

   IF len( cText ) > 0
      qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )

      IF ::oUI:q_checkRegEx:isChecked()
         qDoc := QTextDocument():from( ::qCurEdit:document() )
         qReg := QRegExp()
         qReg:setPattern( cText )
         qReg:setCaseSensitivity( iif( ::oUI:q_checkMatchCase:isChecked(), Qt_CaseSensitive, Qt_CaseInsensitive ) )

         nFlags += ::nCurDirection
         nFlags += iif( ::oUI:q_checkWhole:isChecked(), QTextDocument_FindWholeWords, 0 )

         qCur := QTextCursor():from( qDoc:find_1( qReg, qCursor, nFlags  ) )
         lFound := ! qCur:isNull()
         IF lFound
            ::qCurEdit:setTextCursor( qCur )
         ENDIF
      ELSE
         nFlags += iif( ::oUI:q_checkMatchCase:isChecked(), QTextDocument_FindCaseSensitively, 0 )
         nFlags += iif( ::oUI:q_checkWhole:isChecked(), QTextDocument_FindWholeWords, 0 )
         nFlags += ::nCurDirection

         lFound := ::oEM:getEditCurrent():find( cText, nFlags )
      ENDIF

      IF ! lFound
         ::qCurEdit:setTextCursor( qCursor )
         ::oUI:q_checkReplace:setChecked( .f. )
         ::oUI:q_checkReplace:setEnabled( .f. )
      ELSE
         ::oUI:q_checkReplace:setEnabled( .t. )
         ::qCurEdit:centerCursor()
      ENDIF
   ENDIF
   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:beginFind()

   ::oUI:q_checkReplace:setChecked( .f. )
   ::oUI:q_checkReplace:setEnabled( .f. )

   ::oUI:q_radioTop:setChecked( .t. )

   ::oUI:show()
   ::cFind := ""

   ::qFindLineEdit:activateWindow()
   ::qFindLineEdit:setFocus()
   ::qFindLineEdit:selectAll()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:setFindString( cText )
   LOCAL qCursor, nPos

   IF empty( cText )
      RETURN .f.
   ENDIF

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
   IF ::oUI:q_radioTop:isChecked()
      nPos := qCursor:position()
      qCursor:setPosition( 0 )
      ::qCurEdit:setTextCursor( qCursor )
   ENDIF

   IF ! ::find( cText )
      IF !empty( nPos )
         qCursor:setPosition( nPos )
         ::qCurEdit:setTextCursor( qCursor )
      ENDIF
      ::cFind := ""
      ::qFindLineEdit:setStyleSheet( getStyleSheet( "PathIsWrong", ::nAnimantionMode ) )
   ELSE
      ::cFind := cText
      ::qFindLineEdit:setStyleSheet( "background-color: white;" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:startFromTop()
   LOCAL qCursor

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
   qCursor:setPosition( 0 )
   ::qCurEdit:setTextCursor( qCursor )

   ::find( ::cFind )

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//                           IdeFindReplace
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS IdeFindReplace INHERIT IdeObject

   DATA   qLineEdit

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD onClickReplace()
   METHOD replaceSelection( cReplWith )
   METHOD replace()
   METHOD onClickFind()
   METHOD find( lWarn )
   METHOD updateFindReplaceData( cMode )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:destroy()

   IF !empty( ::oUI )
      ::qLineEdit:disConnect( "returnPressed()" )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::oUI := hbide_getUI( "finddialog", ::oIde:oDlg:oWidget )

   ::oUI:setWindowFlags( Qt_Sheet )

   aeval( ::oINI:aFind   , {|e| ::oUI:q_comboFindWhat:addItem( e ) } )
   aeval( ::oINI:aReplace, {|e| ::oUI:q_comboReplaceWith:addItem( e ) } )

   ::oUI:q_radioFromCursor:setChecked( .t. )
   ::oUI:q_radioDown:setChecked( .t. )

   ::oUI:q_buttonFind   :connect( "clicked()", {|| ::onClickFind()    } )
   ::oUI:q_buttonReplace:connect( "clicked()", {|| ::onClickReplace() } )
   ::oUI:q_buttonClose  :connect( "clicked()", {|| ::oIde:oINI:cFindDialogGeometry := hbide_posAndSize( ::oUI:oWidget ), ::oUI:hide() } )
   ::oUI:q_comboFindWhat:connect( "editTextChanged(QString)", {|| ::oUI:q_radioEntire:setChecked( .t. ) } )
   ::oUI:q_comboFindWhat:connect( "currentIndexChanged(QString)", {|p| ::oIde:oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + p } )
   ::oUI:q_checkListOnly:connect( "stateChanged(int)", {|p| ;
                                        ::oUI:q_comboReplaceWith:setEnabled( p == 0 ), ;
                                   iif( p == 1, ::oUI:q_buttonReplace:setEnabled( .f. ), NIL ) } )

   ::qLineEdit := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:show()
   LOCAL cText

   ::oUI:q_buttonReplace:setEnabled( .f. )
   ::oUI:q_checkGlobal:setEnabled( .f. )
   ::oUI:q_checkNoPrompting:setEnabled( .f. )
   ::oUI:q_checkListOnly:setChecked( .f. )
   ::oIde:setPosByIniEx( ::oUI:oWidget, ::oINI:cFindDialogGeometry )
   ::oUI:q_comboFindWhat:setFocus()

   IF !empty( cText := ::oEM:getSelectedText() )
      ::qLineEdit:setText( cText )
   ENDIF
   ::qLineEdit:selectAll()

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickReplace()

   ::updateFindReplaceData( "replace" )

   IF ::oUI:q_comboReplaceWith:isEnabled()
      ::replace()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replaceSelection( cReplWith )
   LOCAL nB, nL, cBuffer, qCursor

   DEFAULT cReplWith TO ""

   qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
   IF qCursor:hasSelection() .and. !empty( cBuffer := qCursor:selectedText() )
      nL := len( cBuffer )
      nB := qCursor:position() - nL

      qCursor:beginEditBlock()
      qCursor:removeSelectedText()
      qCursor:insertText( cReplWith )
      qCursor:setPosition( nB )
      qCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, len( cReplWith ) )
      ::qCurEdit:setTextCursor( qCursor )
      qCursor:endEditBlock()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replace()
   LOCAL cReplWith
   LOCAL nFound

   IF !empty( ::qCurEdit )
      cReplWith := QLineEdit():configure( ::oUI:q_comboReplaceWith:lineEdit() ):text()
      ::replaceSelection( cReplWith )

      IF ::oUI:q_checkGlobal:isChecked()
         IF ::oUI:q_checkNoPrompting:isChecked()
            nFound := 1
            DO WHILE ::find( .f. )
               nFound++
               ::replaceSelection( cReplWith )
            ENDDO
            ::oDK:setStatusText( SB_PNL_MAIN, "Replaced [" + hb_ntos( nFound ) + "] : " + cReplWith )
            ::oUI:q_buttonReplace:setEnabled( .f. )
            ::oUI:q_checkGlobal:setChecked( .f. )
            ::oUI:q_checkNoPrompting:setChecked( .f. )
         ELSE
            ::find()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickFind()
   LOCAL lFound, nPos, qCursor

   ::updateFindReplaceData( "find" )

   IF ::oUI:q_radioEntire:isChecked()
      ::oUI:q_radioFromCursor:setChecked( .t. )
      qCursor := QTextCursor():configure( ::qCurEdit:textCursor() )
      nPos := qCursor:position()

      qCursor:setPosition( 0 )
      ::qCurEdit:setTextCursor( qCursor )
      IF !( lFound := ::find() )
         qCursor:setPosition( nPos )
         ::qCurEdit:setTextCursor( qCursor )
      ENDIF
   ELSE
      lFound := ::find()
   ENDIF

   IF lFound
      ::oUI:q_buttonReplace:setEnabled( .t. )
      ::oUI:q_checkGlobal:setEnabled( .t. )
      ::oUI:q_checkNoPrompting:setEnabled( .t. )
   ELSE
      ::oUI:q_buttonReplace:setEnabled( .f. )
      ::oUI:q_checkGlobal:setEnabled( .f. )
      ::oUI:q_checkNoPrompting:setEnabled( .f. )
      ::oUI:q_buttonFind:activateWindow()
      ::oUI:q_buttonFind:setFocus()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:find( lWarn )
   LOCAL nFlags
   LOCAL cText := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() ):text()
   LOCAL lFound := .f.

   DEFAULT lWarn TO .t.

   IF !empty( cText )
      nFlags := 0
      nFlags += iif( ::oUI:q_checkMatchCase:isChecked(), QTextDocument_FindCaseSensitively, 0 )
      nFlags += iif( ::oUI:q_radioUp:isChecked(), QTextDocument_FindBackward, 0 )

      IF !( lFound := ::oEM:getEditObjectCurrent():findEx( cText, nFlags ) ) .AND. lWarn
         hbide_showWarning( "Cannot find : " + cText )
      ENDIF
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:updateFindReplaceData( cMode )
   LOCAL cData

   IF cMode == "find"
      cData := QLineEdit():configure( ::oUI:q_comboFindWhat:lineEdit() ):text()
      IF !empty( cData )
         IF ascan( ::oINI:aFind, {|e| e == cData } ) == 0
            hb_ains( ::oINI:aFind, 1, cData, .t. )
            ::oUI:q_comboFindWhat:insertItem( 0, cData )
         ENDIF
      ENDIF
      //
      ::oDK:setStatusText( SB_PNL_SEARCH, cData )
   ELSE
      cData := QLineEdit():configure( ::oUI:q_comboReplaceWith:lineEdit() ):text()
      IF !empty( cData )
         IF ascan( ::oINI:aReplace, cData ) == 0
            hb_ains( ::oINI:aReplace, 1, cData, .t. )
            ::oUI:q_comboReplaceWith:insertItem( 0, cData )
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                         Class IdeFindInFiles
//
/*----------------------------------------------------------------------*/

#define L2S( l )                                  iif( l, "Yes", "No" )

#define F_BLACK                                   '<font color = black>'
#define F_GREEN                                   '<font color = green>'
#define F_RED                                     '<font color = red>'
#define F_CYAN                                    '<font color = cyan>'
#define F_BLUE                                    '<font color = blue>'
#define F_YELLOW                                  '<font color = yellow>'

#define F_SECTION                                 '<font color=GoldenRod size="6">'
#define F_SECTION_ITEM                            '<font color=blue size="5">'
#define F_INFO                                    '<font color=LightBlue>'
#define F_FILE                                    '<font color=green>'
#define F_SEARCH                                  '<font color=IndianRed>'

#define F_END                                     '</font>'

#define LOG_MISSING                               1
#define LOG_FINDS                                 2
#define LOG_SEPARATOR                             3
#define LOG_FLAGS                                 4
#define LOG_TERMINATED                            5
#define LOG_SECTION                               6
#define LOG_SECTION_ITEM                          7
#define LOG_EMPTY                                 8
#define LOG_INFO                                  9

/*----------------------------------------------------------------------*/

CLASS IdeFindInFiles INHERIT IdeObject

   DATA   aItems                                  INIT {}
   DATA   lStop                                   INIT .f.
   DATA   aInfo                                   INIT {}

   DATA   nSearched                               INIT 0
   DATA   nFounds                                 INIT 0
   DATA   nMisses                                 INIT 0

   DATA   cOrigExpr
   DATA   compRegEx
   DATA   cReplWith
   DATA   lRegEx                                  INIT .F.
   DATA   lListOnly                               INIT .T.
   DATA   lMatchCase                              INIT .F.
   DATA   lNotDblClick                            INIT .F.
   DATA   lShowOnCreate                           INIT .T.
   DATA   lInDockWindow                           INIT .F.

   DATA   qEditFind

   METHOD new( oIde, lShowOnCreate )
   METHOD create( oIde, lShowOnCreate )
   METHOD destroy()
   METHOD show()
   METHOD print()
   METHOD paintRequested( pPrinter )
   METHOD find()
   METHOD findInABunch( aFiles )
   METHOD showLog( nType, cMsg, aLines )

   METHOD execEvent( cEvent, p )
   METHOD execContextMenu( p )
   METHOD buildUI()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:new( oIde, lShowOnCreate )

   ::oIde          := oIde
   ::lShowOnCreate := lShowOnCreate

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:create( oIde, lShowOnCreate )

   DEFAULT oIde          TO ::oIde
   DEFAULT lShowOnCreate TO ::lShowOnCreate

   ::oIde          := oIde
   ::lShowOnCreate := lShowOnCreate

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:destroy()
   LOCAL qItem

   IF !empty( ::oUI )
      ::qEditFind:disConnect( "returnPressed()" )

      FOR EACH qItem IN ::aItems
         qItem := NIL
      NEXT

      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:buildUI()
   LOCAL cText, qLineEdit, aProjList, cProj, qItem, n

   ::oUI := hbide_getUI( "findinfilesex" )

   ::oFindDock:oWidget:setWidget( ::oUI )

   ::oUI:q_buttonFolder:setIcon( ::resPath + "folder.png" )

   aeval( ::oINI:aFind   , {|e| ::oUI:q_comboExpr:addItem( e ) } )
   aeval( ::oINI:aReplace, {|e| ::oUI:q_comboRepl:addItem( e ) } )
   aeval( ::oINI:aFolders, {|e| ::oUI:q_comboFolder:addItem( e ) } )

   n := ascan( ::oINI:aFind, {|e| e == ::cWrkFind } )
   ::oUI:q_comboExpr:setCurrentIndex( n-1 )

   n := ascan( ::oINI:aReplace, {|e| e == ::cWrkReplace } )
   ::oUI:q_comboRepl:setCurrentIndex( n - 1 )

   n := ascan( ::oIni:aFolders, {|e| e == ::cWrkFolderFind } )
   ::oUI:q_comboFolder:setCurrentIndex( n - 1 )
   ::oUI:q_comboFolder:setEnabled( .f. )
   ::oUI:q_checkFolders:setChecked( .f. )
   ::oUI:q_checkSubFolders:setChecked( .f. )
   ::oUI:q_checkSubFolders:setEnabled( .f. )

   ::oUI:q_buttonRepl:setEnabled( .f. )
   ::oUI:q_buttonStop:setEnabled( .f. )
   ::oUI:q_comboRepl:setEnabled( .f. )

   ::oUI:q_checkListOnly:setChecked( .t. )
   ::oUI:q_checkPrg:setChecked( .t. )

   qLineEdit := QLineEdit():configure( ::oUI:q_comboExpr:lineEdit() )
   IF !empty( ::oEM )
      IF !empty( cText := ::oEM:getSelectedText() )
         qLineEdit:setText( cText )
      ENDIF
   ENDIF
   qLineEdit:selectAll()

   /* Populate Projects Name */
   IF !empty( ::oPM )
      aProjList := ::oPM:getProjectsTitleList()
      FOR EACH cProj IN aProjList
         IF !empty( cProj )
            qItem := QListWidgetItem()
            qItem:setFlags( Qt_ItemIsUserCheckable + Qt_ItemIsEnabled + Qt_ItemIsSelectable )
            qItem:setText( cProj )
            qItem:setCheckState( 0 )
            ::oUI:q_listProjects:addItem_1( qItem )
            aadd( ::aItems, qItem )
         ENDIF
      NEXT
   ENDIF

   ::oUI:q_editResults:setReadOnly( .t. )
   //::oUI:q_editResults:setFontFamily( "Courier New" )
   //::oUI:q_editResults:setFontPointSize( 10 )
   ::oUI:q_editResults:setFont( ::oIde:oFont:oWidget )
   ::oUI:q_editResults:setContextMenuPolicy( Qt_CustomContextMenu )

   ::oUI:q_labelStatus:setText( "Ready" )
   ::oUI:q_comboExpr:setFocus()

   /* Attach all signals */
   //
   ::oUI:q_buttonClose  :connect( "clicked()"                   , {| | ::execEvent( "buttonClose"      ) } )
   ::oUI:q_buttonFolder :connect( "clicked()"                   , {| | ::execEvent( "buttonFolder"     ) } )
   ::oUI:q_buttonFind   :connect( "clicked()"                   , {| | ::execEvent( "buttonFind"       ) } )
   ::oUI:q_buttonRepl   :connect( "clicked()"                   , {| | ::execEvent( "buttonRepl"       ) } )
   ::oUI:q_buttonStop   :connect( "clicked()"                   , {| | ::execEvent( "buttonStop"       ) } )
   ::oUI:q_checkAll     :connect( "stateChanged(int)"           , {|p| ::execEvent( "checkAll", p      ) } )
   ::oUI:q_comboExpr    :connect( "currentIndexChanged(QString)", {|p| ::execEvent( "comboFind", p     ) } )
   ::oUI:q_checkListOnly:connect( "stateChanged(int)"           , {|p| ::execEvent( "checkListOnly", p ) } )
   ::oUI:q_checkFolders :connect( "stateChanged(int)"           , {|p| ::execEvent( "checkFolders", p  ) } )
   ::oUI:q_editResults  :connect( "copyAvailable(bool)"         , {|l| ::execEvent( "editResults", l   ) } )
   ::oUI:q_editResults  :connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( "editResults-contextMenu", p ) } )

   ::qEditFind := QLineEdit():from( ::oUI:q_comboExpr:lineEdit() )
   ::qEditFind:connect( "returnPressed()", {|| ::execEvent( "buttonFind" ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:execEvent( cEvent, p )
   LOCAL cPath, qLineEdit, qCursor, cSource, v, nInfo

   SWITCH cEvent

   CASE "buttonClose"
      ::oFindDock:hide()
      EXIT

   CASE "comboFind"
      ::oIde:oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + p
      EXIT

   CASE "checkListOnly"
      ::oUI:q_comboRepl:setEnabled( p == 0 )
      ::oUI:q_buttonRepl:setEnabled( !( p == 1 ) )
      EXIT

   CASE "checkFolders"
      ::oUI:q_comboFolder:setEnabled( p == 2 )
      ::oUI:q_checkSubFolders:setEnabled( p == 2 )
      EXIT

   CASE "buttonFind"
      ::find()
      EXIT

   CASE "buttonRepl"
      EXIT

   CASE "buttonStop"
      ::lStop := .t.
      EXIT

   CASE "buttonFolder"
      cPath := hbide_fetchADir( ::oDlg, "Select a folder for search operation", ::cLastFileOpenPath )
      IF !empty( cPath )
         ::oIde:cLastFileOpenPath := cPath

         qLineEdit := QLineEdit():configure( ::oUI:q_comboFolder:lineEdit() )
         qLineEdit:setText( cPath )
         IF ascan( ::oINI:aFolders, {|e| e == cPath } ) == 0
            hb_ains( ::oINI:aFolders, 1, cPath, .t. )
         ENDIF
         ::oUI:q_comboFolder:insertItem( 0, cPath )
      ENDIF
      EXIT

   CASE "checkAll"
      v := !( p == 0 )
      ::oUI:q_checkPrg:setChecked( v )
      ::oUI:q_checkC:setChecked( v )
      ::oUI:q_checkCpp:setChecked( v )
      ::oUI:q_checkCh:setChecked( v )
      ::oUI:q_checkH:setChecked( v )
      ::oUI:q_checkRc:setChecked( v )
      EXIT

   CASE "editResults-contextMenu"
      ::execContextMenu( p )
      EXIT

   CASE "editResults"
      IF p .AND. ! ::lNotDblClick
         qCursor := QTextCursor():configure( ::oUI:q_editResults:textCursor() )
         nInfo := qCursor:blockNumber() + 1

         IF nInfo <= len( ::aInfo ) .AND. ::aInfo[ nInfo, 1 ] == -2
            cSource := ::aInfo[ nInfo, 2 ]

            ::oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
            qCursor := QTextCursor():configure( ::oIde:qCurEdit:textCursor() )
            qCursor:setPosition( 0 )
            qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, ::aInfo[ nInfo, 3 ] - 1 )
            qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, ::aInfo[ nInfo, 4 ] - 1 )
            qCursor:movePosition( QTextCursor_Right, QTextCursor_KeepAnchor, len( ::aInfo[ nInfo, 5 ] ) )
            ::oIde:qCurEdit:setTextCursor( qCursor )
            ::oIde:manageFocusInEditor()
         ENDIF
      ELSE
         ::lNotDblClick := .F.
      ENDIF
      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:execContextMenu( p )
   LOCAL nLine, qCursor, qMenu, pAct, qAct, cAct, cFind

   qCursor := QTextCursor():configure( ::oUI:q_editResults:textCursor() )
   nLine := qCursor:blockNumber() + 1

   IF nLine <= len( ::aInfo )
      qMenu := QMenu( ::oUI:q_editResults )
      qMenu:addAction( "Copy"       )
      qMenu:addAction( "Select All" )
      qMenu:addAction( "Clear"      )
      qMenu:addAction( "Print"      )
      qMenu:addAction( "Save as..." )
      qMenu:addSeparator()
      qMenu:addAction( "Find"       )
      qMenu:addSeparator()
      IF ::aInfo[ nLine, 1 ] == -2     /* Found Line */
         qMenu:addAction( "Replace Line" )
      ELSEIF ::aInfo[ nLine, 1 ] == -1 /* Source File */
         qMenu:addAction( "Open"        )
         qMenu:addAction( "Replace All" )
      ENDIF
      qMenu:addSeparator()
      qMenu:addAction( "Zoom In"  )
      qMenu:addAction( "Zoom Out" )

      pAct := qMenu:exec_1( ::oUI:q_editResults:mapToGlobal( p ) )
      IF !hbqt_isEmptyQtPointer( pAct )
         qAct := QAction():configure( pAct )
         cAct := qAct:text()

         SWITCH cAct
         CASE "Save as..."
            EXIT
         CASE "Find"
            IF !empty( cFind := hbide_fetchAString( ::oUI:q_editResults, , "Find what?", "Find" ) )
               ::lNotDblClick := .T.
               IF !( ::oUI:q_editResults:find( cFind, 0 ) )
                  MsgBox( "Not Found" )
               ENDIF
            ENDIF
            EXIT
         CASE "Print"
            ::print()
            EXIT
         CASE "Clear"
            ::oUI:q_editResults:clear()
            ::aInfo := {}
            EXIT
         CASE "Copy"
            ::lNotDblClick := .T.
            ::oUI:q_editResults:copy()
            EXIT
         CASE "Select All"
            ::oUI:q_editResults:selectAll()
            EXIT
         CASE "Replace Line"
            EXIT
         CASE "Replace Source"
            EXIT
         CASE "Zoom In"
            ::oUI:q_editResults:zoomIn()
            EXIT
         CASE "Zoom Out"
            ::oUI:q_editResults:zoomOut()
            EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:show()

   IF empty( ::oUI )
      ::buildUI()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:find()
   LOCAL lPrg, lC, lCpp, lH, lCh, lRc, a_
   LOCAL lTabs, lSubF, lSubP, cFolder, qItem, aFilter, cExt, cMask, cWrkFolder, cProjPath
   LOCAL nStart, nEnd, cSource, aDir, cProjTitle, aProjFiles
   LOCAL aOpenSrc   := {}
   LOCAL aFolderSrc := {}
   LOCAL aProjSrc   := {}
   LOCAL aProjs     := {}
   LOCAL aPaths     := {}

   IF empty( ::cOrigExpr := ::oUI:q_comboExpr:currentText() )
      RETURN Self
   ENDIF

   ::lListOnly  := ::oUI:q_checkListOnly:isChecked()
   ::lMatchCase := ::oUI:q_checkMatchCase:isChecked()
   ::cReplWith  := ::oUI:q_comboRepl:currentText()

   ::lRegEx := ::oUI:q_checkRegEx:isChecked()
   IF ::lRegEx
      ::compRegEx := hb_regExComp( ::cOrigExpr, ::lMatchCase )
      IF ! hb_isRegEx( ::compRegEx )
         MsgBox( "Error in Regular Expression" )
         RETURN Self
      ENDIF
   ENDIF

   cFolder      := ::oUI:q_comboFolder:currentText()
   cWrkFolder   := cFolder
   lTabs        := ::oUI:q_checkOpenTabs:isChecked()
   lSubF        := ::oUI:q_checkSubFolders:isChecked()
   lSubP        := ::oUI:q_checkSubProjects:isChecked()
   /* Type of files */
   lPrg         := ::oUi:q_checkPrg:isChecked()
   lC           := ::oUI:q_checkC:isChecked()
   lCpp         := ::oUI:q_checkCpp:isChecked()
   lH           := ::oUI:q_checkH:isChecked()
   lCh          := ::oUI:q_checkCh:isChecked()
   lRc          := ::oUI:q_checkRc:isChecked()

   aFilter := hbide_buildFilter( lPrg, lC, lCpp, lH, lCh, lRc )

   /* Process Open Tabs */
   IF lTabs
      FOR EACH a_ IN ::aTabs
         cSource := a_[ 2 ]:sourceFile
         IF hbide_isSourceOfType( cSource, aFilter )
            aadd( aOpenSrc, cSource )
         ENDIF
      NEXT
   ENDIF

   /* Process Folder */
   IF ::oUI:q_checkFolders:isChecked() .AND. ! empty( cFolder )
      hbide_fetchSubPaths( @aPaths, cFolder, ::oUI:q_checkSubFolders:isChecked() )

      FOR EACH cFolder IN aPaths
         FOR EACH cExt IN aFilter
            cMask := hbide_pathToOsPath( cFolder + cExt )
            aDir  := directory( cMask )
            FOR EACH a_ IN aDir
               aadd( aFolderSrc, cFolder + a_[ 1 ] )
            NEXT
         NEXT
      NEXT
   ENDIF

   /* Process Projects */
   IF !empty( ::aItems )
      FOR EACH qItem IN ::aItems
         IF qItem:checkState() == 2
            aadd( aProjs, qItem:text() )
         ENDIF
      NEXT
   ENDIF
   IF !empty( aProjs )
      FOR EACH cProjTitle IN aProjs
         a_:= {}
         IF !empty( aProjFiles := ::oPM:getSourcesByProjectTitle( cProjTitle ) )
            cProjPath := ::oPM:getProjectPathFromTitle( cProjTitle )
            FOR EACH cSource IN aProjFiles
               IF hbide_isSourceOfType( cSource, aFilter )
                  aadd( a_, hbide_syncProjPath( cProjPath, hbide_stripFilter( cSource ) ) )
               ENDIF
            NEXT
         ENDIF
         IF !empty( a_ )
            aadd( aProjSrc, { cProjTitle, a_ } )
         ENDIF
      NEXT
   ENDIF

   /* Supress Find button - user must not click it again */
   ::oUI:q_buttonFind:setEnabled( .f. )
   ::oUI:q_buttonStop:setEnabled( .t. )

   ::nSearched := 0
   ::nFounds   := 0
   ::nMisses   := 0

   ::oUI:q_labelStatus:setText( "Ready" )

   /* Fun Begins */
   ::showLog( LOG_SEPARATOR )
   ::showLog( LOG_FLAGS, "[Begins: " + dtoc( date() ) + "-" + time() + "][" + "Find Expression: " + ::cOrigExpr + "]" )
   ::showLog( LOG_FLAGS, hbide_getFlags( lPrg, lC, lCpp, lH, lCh, lRc, lTabs, lSubF, lSubP, ::lRegEx, ::lListOnly, ::lMatchCase ) )
   ::showLog( LOG_EMPTY )

   nStart := seconds()

   IF lTabs
      ::showLog( LOG_SECTION, "OpenTabs" )
      IF !empty( aOpenSrc )
         ::findInABunch( aOpenSrc )
      ELSE
         ::showLog( LOG_INFO, "No matching files found" )
      ENDIF
   ENDIF

   IF ::oUI:q_checkFolders:isChecked() .AND. ! empty( cFolder )
      ::showLog( LOG_SECTION, "Folders" )
      IF !empty( aFolderSrc )
         ::showLog( LOG_SECTION_ITEM, "Folder: " + cFolder )
         ::findInABunch( aFolderSrc )
      ELSE
         ::showLog( LOG_INFO, "No matching files found" )
      ENDIF
   ENDIF

   IF !empty( aProjs )
      ::showLog( LOG_SECTION, "Projects" )
      IF !empty( aProjSrc )
         FOR EACH a_ IN aProjSrc
            ::showLog( LOG_SECTION_ITEM, "Project: " + a_[ 1 ] )
            ::findInABunch( a_[ 2 ] )
         NEXT
      ELSE
         ::showLog( LOG_INFO, "No matching files found" )
      ENDIF
   ENDIF

   nEnd := seconds()

   ::showLog( LOG_EMPTY )
   ::showLog( LOG_FLAGS, "[Ends:" + dtoc( date() ) + "-" + time() + "-" + hb_ntos( nEnd - nStart ) + " Secs]" + ;
                         "[Searched: " + hb_ntos( ::nSearched ) + "][Finds: " + hb_ntos( ::nFounds ) + "]" + ;
                         "[Files not found: " + hb_ntos( ::nMisses ) + "]" )
   ::showLog( LOG_SEPARATOR )
   ::showLog( LOG_EMPTY )

   ::oUI:q_labelStatus:setText( "[ Time Taken: " + hb_ntos( nEnd - nStart ) + " ] " + ;
                         "[ Searched: " + hb_ntos( ::nSearched ) + " ] [ Finds: " + hb_ntos( ::nFounds ) + " ] " + ;
                         "[ Files not found: " + hb_ntos( ::nMisses ) + " ]" )
   ::lStop := .f.
   ::oUI:q_buttonStop:setEnabled( .f. )
   ::oUI:q_buttonFind:setEnabled( .t. )

   IF ::nFounds > 0
      IF ascan( ::oINI:aFind, {|e| e == ::cOrigExpr } ) == 0
         hb_ains( ::oINI:aFind, 1, ::cOrigExpr, .t. )
         ::oUI:q_comboFolder:insertItem( 0, ::cOrigExpr )
      ENDIF
      ::oIde:cWrkFind := ::cOrigExpr
      ::oIde:cWrkFolderFind := cWrkFolder
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:findInABunch( aFiles )
   LOCAL s, cExpr, nLine, aLines, aBuffer, cLine, nNoMatch, aMatch, regEx

   nNoMatch := 0
   FOR EACH s IN aFiles
      IF ::lStop                                 /* Stop button is pressed */
         ::showLog( LOG_EMPTY )
         ::showLog( LOG_TERMINATED )
         EXIT
      ENDIF
      aLines := {}
      s := hbide_pathToOSPath( s )
      IF hb_fileExists( s )
         ::nSearched++
         aBuffer := hb_ATokens( StrTran( hb_MemoRead( s ), Chr( 13 ) ), Chr( 10 ) )
         nLine := 0

         IF ::lRegEx
            regEx := ::compRegEx
            FOR EACH cLine IN aBuffer
               nLine++
               //       exp, string, lMatchCase, lNewLine, nMaxMatch, nMatchWhich, lMatchOnly
               IF !empty( aMatch := hb_regExAll( regEx, cLine, ::lMatchCase, .F., 0, 1, .F.  ) )
                  aadd( aLines, { nLine, cLine, aMatch } )
               ENDIF
            NEXT
         ELSE
            IF ::lMatchCase
               cExpr := ::cOrigExpr
               FOR EACH cLine IN aBuffer
                  nLine++
                  IF cExpr $ cLine
                     aadd( aLines, { nLine, cLine, NIL } )
                  ENDIF
               NEXT
            ELSE
               cExpr := lower( ::cOrigExpr )
               FOR EACH cLine IN aBuffer
                  nLine++
                  IF cExpr $ lower( cLine )
                     aadd( aLines, { nLine, cLine, NIL } )
                  ENDIF
               NEXT
            ENDIF
         ENDIF

         IF len( aLines ) > 0
            ::showLog( LOG_FINDS, s, aLines )
            ::nFounds++
         ELSE
            nNoMatch++
         ENDIF
      ELSE
         ::showLog( LOG_MISSING, s )
         ::nMisses++
      ENDIF
   NEXT
   IF nNoMatch == len( aFiles )
      ::showLog( LOG_INFO, "Searched (" + hb_ntos( len( aFiles ) ) + ") files, no matches found" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:showLog( nType, cMsg, aLines )
   LOCAL a_, n, cPre, cPost, nWidth, cText, nB, cL, nL, cT, cExp, aM
   LOCAL qCursor, qResult

   qResult := ::oUI:q_editResults

   DEFAULT cMsg TO ""
   cMsg := hbide_convertHtmlDelimiters( cMsg )

   qCursor := QTextCursor():configure( ::oUI:q_editResults:textCursor() )

   SWITCH nType

   CASE LOG_SEPARATOR
      qResult:append( F_BLACK + hbide_outputLine( "=", 68 ) + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_FLAGS
      qResult:append( F_BLACK + cMsg + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_INFO
      qResult:append( F_INFO + "<i>" + cMsg + "</i>" + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_SECTION
      qResult:append( F_SECTION + "<u>" + cMsg + "</u>" + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_SECTION_ITEM
      qResult:append( F_SECTION_ITEM + cMsg + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_FINDS
      cText := F_FILE + "<b>" + cMsg + "   ( "+ hb_ntos( len( aLines ) ) + " )" + "</b>" + F_END
      ::oUI:q_editResults:append( cText )
      ::oUI:q_labelStatus:setText( cText )
      aadd( ::aInfo, { -1, cMsg, NIL } )

      n := 0
      aeval( aLines, {|a_| n := max( n, a_[ 1 ] ) } )
      nWidth := iif( n < 10, 1, iif( n < 100, 2, iif( n < 1000, 3, iif( n < 10000, 4, iif( n < 100000, 5, 7 ) ) ) ) )

      IF ::lRegEx
         FOR EACH a_ IN aLines
            nL := a_[ 1 ]
            aM := a_[ 3 ]
            nB := aM[ 1, 2 ]
            cL := hbide_buildResultLine( a_[ 2 ], aM )
            cT := aM[ 1, 1 ]

            qResult:append( F_BLACK + "&nbsp;&nbsp;&nbsp;(" + strzero( nL, nWidth ) + ")&nbsp;&nbsp;" + cL + F_END )

            aadd( ::aInfo, { -2, cMsg, nL, nB, cT  } )
            qCursor:movePosition( QTextCursor_Down )
         NEXT
      ELSE
         cExp := iif( ::lMatchCase, ::cOrigExpr, lower( ::cOrigExpr ) )
         FOR EACH a_ IN aLines
            nL    := a_[ 1 ]
            cL    := a_[ 2 ]
            //nB    := at( cExp, cL )
            nB    := at( cExp, iif( ::lMatchCase, cL, lower( cL ) ) )
            cPre  := substr( cL, 1, nB - 1 )
            cPost := substr( cL, nB + len( cExp ) )
            cT    := substr( cL, nB, len( cExp ) )
            cL    := hbide_convertHtmlDelimiters( cPre ) + F_SEARCH + "<b>" + hbide_convertHtmlDelimiters( cT ) + ;
                                                             "</b>" + F_END + hbide_convertHtmlDelimiters( cPost )

            qResult:append( F_BLACK + "&nbsp;&nbsp;&nbsp;(" + strzero( nL, nWidth ) + ")&nbsp;&nbsp;" + cL + F_END )

            //            mode, source, line#, pos, slctn
            aadd( ::aInfo, { -2, cMsg, nL, nB, cT  } )
            qCursor:movePosition( QTextCursor_Down )
         NEXT
      ENDIF
      EXIT

   CASE LOG_TERMINATED
      qResult:append( F_RED + "---------------- Terminated ---------------" + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_MISSING
      qResult:append( F_RED + cMsg + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   CASE LOG_EMPTY
      qResult:append( F_BLACK + " " + F_END )
      aadd( ::aInfo, { 0, NIL, NIL } )
      EXIT

   ENDSWITCH

   qCursor:movePosition( QTextCursor_Down )
   ::oUI:q_editResults:setTextCursor( qCursor )

   QApplication():processEvents()
   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_buildResultLine( cLine, aM )
   LOCAL cT, cR, i

   FOR i := 1 TO len( aM )
      cR    := aM[ i, 1 ]
      cT    := replicate( chr( 255 ), len( aM[ i, 1 ] ) )
      cLine := strtran( cLine, cR, cT, 1, 1 )
   NEXT
   FOR i := 1 TO len( aM )
      cR    := replicate( chr( 255 ), len( aM[ i, 1 ] ) )
      cT    := F_SEARCH + "<b>" + hbide_convertHtmlDelimiters( aM[ i, 1 ] ) + "</b>" + F_END
      cLine := strtran( cLine, cR, cT, 1, 1 )
   NEXT

   RETURN cLine

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:print()
   LOCAL qDlg

   qDlg := QPrintPreviewDialog( ::oUI )
   qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
   qDlg:connect( "paintRequested(QPrinter)", {|p| ::paintRequested( p ) } )
   qDlg:exec()
   qDlg:disconnect( "paintRequested(QPrinter)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:paintRequested( pPrinter )
   LOCAL qPrinter

   qPrinter := QPrinter():configure( pPrinter )

   ::oUI:q_editResults:print( qPrinter )

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_buildFilter( lPrg, lC, lCpp, lH, lCh, lRc )
   LOCAL aFilter := {}

   IF lPrg
      aadd( aFilter, "*.prg" )
   ENDIF
   IF lC
      aadd( aFilter, "*.c" )
   ENDIF
   IF lCpp
      aadd( aFilter, "*.cpp" )
   ENDIF
   IF lh
      aadd( aFilter, "*.h" )
   ENDIF
   IF lCh
      aadd( aFilter, "*.ch" )
   ENDIF
   IF lRc
      aadd( aFilter, "*.rc" )
   ENDIF

   RETURN aFilter

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_getFlags( lPrg, lC, lCpp, lH, lCh, lRc, lTabs, lSubF, lSubP, lRegEx, lListOnly, lMatchCase )
   LOCAL s := ""

   HB_SYMBOL_UNUSED( lTabs )
   HB_SYMBOL_UNUSED( lSubF )
   HB_SYMBOL_UNUSED( lSubP )
   HB_SYMBOL_UNUSED( lListOnly )

   s += "[.prg="  + L2S( lPrg        ) + "]"
   s += "[.c="    + L2S( lC          ) + "]"
   s += "[.cpp="  + L2S( lCpp        ) + "]"
   s += "[.h="    + L2S( lH          ) + "]"
   s += "[.ch="   + L2S( lCh         ) + "]"
   s += "[.rc="   + L2S( lRc         ) + "]"
   s += "[RegEx=" + L2S( lRegEx      ) + "]"
   s += "[Case="  + L2S( lMatchCase  ) + "]"

   RETURN s

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_isSourceOfType( cSource, aFilter )
   LOCAL cExt

   hb_fNameSplit( cSource, , , @cExt )
   cExt := lower( cExt )

   RETURN  ascan( aFilter, {|e| cExt $ e } ) > 0

/*----------------------------------------------------------------------*/

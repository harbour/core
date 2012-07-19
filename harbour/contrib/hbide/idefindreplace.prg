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

#define __buttonPrev_clicked__                    2001
#define __buttonNext_clicked__                    2002
#define __buttonFirst_clicked__                   2003
#define __buttonLast_clicked__                    2004
#define __buttonAll_clicked__                     2005
#define __buttonClose__                           2006
#define __buttonFolder__                          2007
#define __buttonFind__                            2008
#define __buttonRepl__                            2009
#define __buttonStop__                            2010
#define __checkAll__                              2011
#define __comboFind__                             2012
#define __checkListOnly__                         2013
#define __checkFolders__                          2014
#define __editResults__                           2015
#define __editResults_contextMenu__               2016

/*----------------------------------------------------------------------*/

CLASS IdeUpDown INHERIT IdeObject

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show( oEdit )
   METHOD position()
   METHOD execEvent( nEvent, p )

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

      qHSBar := qEdit:horizontalScrollBar()
      qVSBar := qEdit:verticalScrollBar()

      qRect  := qEdit:geometry()

      ::oUI:move( qRect:width()  - ::oUI:width()  - iif( qVSBar:isVisible(), qVSBar:width() , 0 ), ;
                  qRect:height() - ::oUI:height() - iif( qHSBar:isVisible(), qHSBar:height(), 0 ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:show( oEdit )

   DEFAULT oEdit TO ::oEM:getEditObjectCurrent()

   IF ! empty( oEdit )
      IF oEdit:aSelectionInfo[ 1 ] > -1
         ::oUI:setEnabled( .t. )
      ELSE
         ::oUI:setEnabled( .f. )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::oUI := hbide_getUI( "updown_v" )

   ::oUI:setWindowFlags( hb_bitOr( Qt_Tool, Qt_FramelessWindowHint ) )
   ::oUI:setFocusPolicy( Qt_NoFocus )

   ::oUI:buttonPrev:setIcon( QIcon( hbide_image( "go-prev" ) ) )
   ::oUI:buttonPrev:setToolTip( "Find Previous" )
   ::oUI:buttonPrev:connect( "clicked()", {|| ::execEvent( __buttonPrev_clicked__ ) } )
   //
   ::oUI:buttonNext:setIcon( QIcon( hbide_image( "go-next" ) ) )
   ::oUI:buttonNext:setToolTip( "Find Next" )
   ::oUI:buttonNext:connect( "clicked()", {|| ::execEvent( __buttonNext_clicked__ ) } )
   //
   ::oUI:buttonFirst:setIcon( QIcon( hbide_image( "go-first" ) ) )
   ::oUI:buttonFirst:setToolTip( "Find First" )
   ::oUI:buttonFirst:connect( "clicked()", {|| ::execEvent( __buttonFirst_clicked__ ) } )
   //
   ::oUI:buttonLast:setIcon( QIcon( hbide_image( "go-last" ) ) )
   ::oUI:buttonLast:setToolTip( "Find Last" )
   ::oUI:buttonLast:connect( "clicked()", {|| ::execEvent( __buttonLast_clicked__ ) } )
   //
   ::oUI:buttonAll:setIcon( QIcon( hbide_image( "hilight-all" ) ) )
   ::oUI:buttonAll:setToolTip( "Highlight All" )
   ::oUI:buttonAll:connect( "clicked()", {|| ::execEvent( __buttonAll_clicked__ ) } )

   ::oUI:setEnabled( .f. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:execEvent( nEvent, p )
   LOCAL cText, oEdit

   HB_SYMBOL_UNUSED( p )
   IF ::lQuitting
      RETURN Self
   ENDIF

   IF !empty( oEdit := ::oEM:getEditObjectCurrent() )
      cText := oEdit:getSelectedText()
   ENDIF
   IF !empty( cText )
      SWITCH nEvent

      CASE __buttonPrev_clicked__
         oEdit:findEx( cText, QTextDocument_FindBackward, 0 )
         EXIT
      CASE __buttonNext_clicked__
         oEdit:findEx( cText, 0, 0 )
         EXIT
      CASE __buttonFirst_clicked__
         oEdit:findEx( cText, 0, 1 )
         EXIT
      CASE __buttonLast_clicked__
         oEdit:findEx( cText, QTextDocument_FindBackward, 2 )
         EXIT
      CASE __buttonAll_clicked__
         oEdit:highlightAll( cText )
         EXIT
      ENDSWITCH
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeUpDown:destroy()

   IF HB_ISOBJECT( ::oUI )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
//           IdeSearchReplace - Extended Window at the bottom
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

   ::oUI:frameFind:setStyleSheet( "" )
   ::oUI:frameReplace:setStyleSheet( "" )

   ::oUI:buttonClose:setIcon( QIcon( hbide_image( "closetab" ) ) )
   ::oUI:buttonClose:setToolTip( "Close" )
   ::oUI:buttonClose:connect( "clicked()", {|| ::oUI:hide() } )

   ::oUI:buttonNext:setIcon( QIcon( hbide_image( "next" ) ) )
   ::oUI:buttonNext:setToolTip( "Find Next" )
   ::oUI:buttonNext:connect( "clicked()", {|| ::find( ::cFind ), ::oIde:manageFocusInEditor() } )

   ::oUI:buttonPrev:setIcon( QIcon( hbide_image( "previous" ) ) )
   ::oUI:buttonPrev:setToolTip( "Find Previous" )
   ::oUI:buttonPrev:connect( "clicked()", {|| ::find( ::cFind, .t. ), ::oIde:manageFocusInEditor() } )

   ::oUI:checkReplace:setChecked( .f. )
   ::oUI:checkReplace:connect( "stateChanged(int)", {|i| ;
                               ::oUI:comboReplace:setEnabled( i == 2 ), ;
                               ::oUI:buttonReplace:setEnabled( i == 2 ), ;
                               iif( i == 2, ::oUI:frameReplace:show(), ::oUI:frameReplace:hide() ) } )

   ::qFindLineEdit := ::oUI:comboFind:lineEdit()
   ::qFindLineEdit:setFocusPolicy( Qt_StrongFocus )
   ::qFindLineEdit:setStyleSheet( "background-color: white;" )
   ::qFindLineEdit:connect( "textChanged(QString)", {|cText| ::setFindString( cText ) } )
   ::qFindLineEdit:connect( "returnPressed()"     , {|| ::find( ::cFind ) } )

   ::qReplLineEdit := ::oUI:comboReplace:lineEdit()
   ::qReplLineEdit:setFocusPolicy( Qt_StrongFocus )
   ::qReplLineEdit:setStyleSheet( "background-color: white;" )

   ::oUI:checkReplace:setEnabled( .f. )
   ::oUI:frameReplace:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:destroy()

   IF HB_ISOBJECT( ::oUI )

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

   IF Len( cText ) > 0
      qCursor := ::qCurEdit:textCursor()

      IF ::oUI:checkRegEx:isChecked()
         qDoc := ::qCurEdit:document()
         qReg := QRegExp()
         qReg:setPattern( cText )
         qReg:setCaseSensitivity( iif( ::oUI:checkMatchCase:isChecked(), Qt_CaseSensitive, Qt_CaseInsensitive ) )

         nFlags += ::nCurDirection
         nFlags += iif( ::oUI:checkWhole:isChecked(), QTextDocument_FindWholeWords, 0 )

         qCur := qDoc:find( qReg, qCursor, nFlags )
         lFound := ! qCur:isNull()
         IF lFound
            ::qCurEdit:setTextCursor( qCur )
         ENDIF
      ELSE
         nFlags += iif( ::oUI:checkMatchCase:isChecked(), QTextDocument_FindCaseSensitively, 0 )
         nFlags += iif( ::oUI:checkWhole:isChecked(), QTextDocument_FindWholeWords, 0 )
         nFlags += ::nCurDirection

         lFound := ::oEM:getEditCurrent():find( cText, nFlags )
      ENDIF

      IF ! lFound
         ::qCurEdit:setTextCursor( qCursor )
         ::oUI:checkReplace:setChecked( .f. )
         ::oUI:checkReplace:setEnabled( .f. )
      ELSE
         ::oUI:checkReplace:setEnabled( .t. )
         ::qCurEdit:centerCursor()
      ENDIF
   ENDIF
   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeSearchReplace:beginFind()

   ::oUI:checkReplace:setChecked( .f. )
   ::oUI:checkReplace:setEnabled( .f. )

   ::oUI:radioTop:setChecked( .t. )

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

   qCursor := ::qCurEdit:textCursor()
   IF ::oUI:radioTop:isChecked()
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

   qCursor := ::qCurEdit:textCursor()
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
   DATA   qReplaceEdit
   DATA   cText

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD show()
   METHOD onClickReplace( nFrom )
   METHOD replaceSelection( cReplWith )
   METHOD replace()
   METHOD onClickFind( nFrom )
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
      ::qLineEdit:disConnect( "textChanged(QString)" )
      ::qReplaceEdit:disConnect( "returnPressed()" )
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   ::oUI := hbide_getUI( "finddialog", ::oIde:oDlg:oWidget )

   ::oUI:setWindowFlags( Qt_Sheet )

   aeval( ::oINI:aFind   , {|e| ::oUI:comboFindWhat:addItem( e ) } )
   aeval( ::oINI:aReplace, {|e| ::oUI:comboReplaceWith:addItem( e ) } )

   ::oUI:radioFromCursor:setChecked( .t. )
   ::oUI:radioDown:setChecked( .t. )

   ::oUI:connect( QEvent_Close, {|| ::oIde:oINI:cFindDialogGeometry := hbide_posAndSize( ::oUI:oWidget ) } )

   ::oUI:buttonFind   :connect( "clicked()"                   , {| | ::onClickFind()    } )
   ::oUI:buttonReplace:connect( "clicked()"                   , {| | ::onClickReplace() } )
   ::oUI:buttonClose  :connect( "clicked()"                   , {| | ::oIde:oINI:cFindDialogGeometry := hbide_posAndSize( ::oUI:oWidget ), ::oUI:hide() } )
   ::oUI:comboFindWhat:connect( "editTextChanged(QString)"    , {| | ::oUI:radioEntire:setChecked( .t. ) } )
   ::oUI:comboFindWhat:connect( "currentIndexChanged(QString)", {|p| ::oIde:oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + p } )
   ::oUI:checkListOnly:connect( "stateChanged(int)"           , {|p| ::oUI:comboReplaceWith:setEnabled( p == 0 ), ;
                                                                             iif( p == 1, ::oUI:buttonReplace:setEnabled( .f. ), NIL ) } )

   ::qLineEdit := ::oUI:comboFindWhat:lineEdit()
   ::qLineEdit:connect( "returnPressed()", {|| iif( empty( ::cText ), NIL, ;
                                                  ::qLineEdit:setText( ::cText ) ), ::cText := "", ::onClickFind( 1 ) } )

   ::qReplaceEdit := ::oUI:comboReplaceWith:lineEdit()
   ::qReplaceEdit:connect( "returnPressed()", {|| ::onClickReplace( 1 ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:show()

   IF ! ::oUI:isHidden()
      ::oIde:oINI:cFindDialogGeometry := hbide_posAndSize( ::oUI:oWidget )
      ::oUI:hide()
   ENDIF

   ::oIde:setPosByIniEx( ::oUI:oWidget, ::oINI:cFindDialogGeometry )

   ::oUI:buttonReplace:setEnabled( .f. )
   ::oUI:checkGlobal:setEnabled( .f. )
   ::oUI:checkNoPrompting:setEnabled( .f. )
   ::oUI:checkListOnly:setChecked( .f. )
   ::oUI:comboFindWhat:setFocus()

   IF ! empty( ::cText := ::oEM:getSelectedText() )
      ::qLineEdit:setText( ::cText )
      ::updateFindReplaceData( "find" )
   ENDIF
   ::qLineEdit:selectAll()
   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickFind( nFrom )
   LOCAL lFound, nPos, qCursor

   DEFAULT nFrom TO 0  // Click on Find Button

   IF nFrom == 0
      ::updateFindReplaceData( "find" )
   ENDIF

   IF ::oUI:radioEntire:isChecked()
      ::oUI:radioFromCursor:setChecked( .t. )
      qCursor := ::qCurEdit:textCursor()
      nPos := qCursor:position()

      qCursor:setPosition( 0 )
      ::qCurEdit:setTextCursor( qCursor )
      IF ! ( lFound := ::find() )
         qCursor:setPosition( nPos )
         ::qCurEdit:setTextCursor( qCursor )
      ENDIF
   ELSE
      lFound := ::find()
   ENDIF

   IF lFound
      ::oUI:buttonReplace:setEnabled( .t. )
      ::oUI:checkGlobal:setEnabled( .t. )
      ::oUI:checkNoPrompting:setEnabled( .t. )
   ELSE
      ::oUI:buttonReplace:setEnabled( .f. )
      ::oUI:checkGlobal:setEnabled( .f. )
      ::oUI:checkNoPrompting:setEnabled( .f. )
      ::oUI:hide()
      ::oUI:show()
      ::oUI:comboFindWhat:setFocus()
      ::qLineEdit:selectAll()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:find( lWarn )
   LOCAL nFlags, qfocus
   LOCAL cText := ::oUI:comboFindWhat:lineEdit():text()
   LOCAL lFound := .f.

   DEFAULT lWarn TO .t.

   IF ! empty( cText )
      nFlags := 0
      nFlags += iif( ::oUI:checkMatchCase:isChecked(), QTextDocument_FindCaseSensitively, 0 )
      nFlags += iif( ::oUI:radioUp:isChecked(), QTextDocument_FindBackward, 0 )

      IF ! ( lFound := ::oEM:getEditObjectCurrent():findEx( cText, nFlags ) ) .AND. lWarn
         qFocus := ::oUI:focusWidget()
         hbide_showWarning( "Cannot find : " + cText )
         qFocus:setFocus( 0 )
      ENDIF
   ENDIF

   RETURN lFound

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:onClickReplace( nFrom )

   DEFAULT nFrom TO 0  // Click on Find Button
   IF nFrom == 0
      ::updateFindReplaceData( "replace" )
   ENDIF

   IF ::oUI:comboReplaceWith:isEnabled()
      ::replace()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replaceSelection( cReplWith )
   LOCAL nB, nL, cBuffer, qCursor

   DEFAULT cReplWith TO ""

   qCursor := ::qCurEdit:textCursor()
   IF qCursor:hasSelection() .and. ! empty( cBuffer := qCursor:selectedText() )
      nL := Len( cBuffer )
      nB := qCursor:position() - nL

      qCursor:beginEditBlock()
      qCursor:removeSelectedText()
      qCursor:insertText( cReplWith )
      qCursor:setPosition( nB + Len( cReplWith ) )
      ::qCurEdit:setTextCursor( qCursor )
      ::oEM:getEditObjectCurrent():clearSelection()
      qCursor:endEditBlock()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:replace()
   LOCAL cReplWith
   LOCAL nFound

   IF !empty( ::qCurEdit )
      cReplWith := ::oUI:comboReplaceWith:lineEdit():text()
      ::replaceSelection( cReplWith )

      IF ::oUI:checkGlobal:isChecked()
         IF ::oUI:checkNoPrompting:isChecked()
            nFound := 1
            DO WHILE ::find( .f. )
               nFound++
               ::replaceSelection( cReplWith )
            ENDDO
            ::oDK:setStatusText( SB_PNL_MAIN, "Replaced [" + hb_ntos( nFound ) + "] : " + cReplWith )
            ::oUI:buttonReplace:setEnabled( .f. )
            ::oUI:checkGlobal:setChecked( .f. )
            ::oUI:checkNoPrompting:setChecked( .f. )
         ELSE
            ::find()
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindReplace:updateFindReplaceData( cMode )
   LOCAL cData, nIndex

   IF cMode == "find"
      cData := ::oUI:comboFindWhat:lineEdit():text()
      IF ! empty( cData )
         IF ( nIndex := ascan( ::oINI:aFind, {|e| e == cData } ) ) == 0
            hb_ains( ::oINI:aFind, 1, cData, .t. )
            ::oUI:comboFindWhat:insertItem( 0, cData )
         ELSE
            ::oUI:comboFindWhat:setCurrentIndex( nIndex - 1 )
         ENDIF
      ENDIF
      //
      ::oDK:setStatusText( SB_PNL_SEARCH, cData )
   ELSE
      cData := ::oUI:comboReplaceWith:lineEdit():text()
      IF !empty( cData )
         IF ascan( ::oINI:aReplace, cData ) == 0
            hb_ains( ::oINI:aReplace, 1, cData, .t. )
            ::oUI:comboReplaceWith:insertItem( 0, cData )
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
   METHOD paintRequested( qPrinter )
   METHOD find()
   METHOD findInABunch( aFiles )
   METHOD showLog( nType, cMsg, aLines )

   METHOD execEvent( nEvent, p )
   METHOD execContextMenu( p )
   METHOD buildUI()
   METHOD replaceAll()

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

   ::oFindDock:oWidget:setWidget( ::oUI:oWidget )

   ::oUI:buttonFolder:setIcon( QIcon( ::resPath + "folder.png" ) )

   aeval( ::oINI:aFind   , {|e| ::oUI:comboExpr:addItem( e ) } )
   aeval( ::oINI:aReplace, {|e| ::oUI:comboRepl:addItem( e ) } )
   aeval( ::oINI:aFolders, {|e| ::oUI:comboFolder:addItem( e ) } )

   n := ascan( ::oINI:aFind, {|e| e == ::cWrkFind } )
   ::oUI:comboExpr:setCurrentIndex( n-1 )

   n := ascan( ::oINI:aReplace, {|e| e == ::cWrkReplace } )
   ::oUI:comboRepl:setCurrentIndex( n - 1 )

   n := ascan( ::oIni:aFolders, {|e| e == ::cWrkFolderFind } )
   ::oUI:comboFolder:setCurrentIndex( n - 1 )
   ::oUI:comboFolder:setEnabled( .f. )
   ::oUI:checkFolders:setChecked( .f. )
   ::oUI:checkSubFolders:setChecked( .f. )
   ::oUI:checkSubFolders:setEnabled( .f. )

   ::oUI:buttonRepl:setEnabled( .f. )
   ::oUI:buttonStop:setEnabled( .f. )
   ::oUI:comboRepl:setEnabled( .f. )

   ::oUI:checkListOnly:setChecked( .t. )
   ::oUI:checkPrg:setChecked( .t. )

   qLineEdit := ::oUI:comboExpr:lineEdit()
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
            //::oUI:listProjects:addItem_1( qItem )
            ::oUI:listProjects:addItem( qItem )
            aadd( ::aItems, qItem )
         ENDIF
      NEXT
   ENDIF

   ::oUI:editResults:setReadOnly( .t. )
   //::oUI:editResults:setFontFamily( "Courier New" )
   //::oUI:editResults:setFontPointSize( 10 )
   ::oUI:editResults:setFont( ::oIde:oFont:oWidget )
   ::oUI:editResults:setContextMenuPolicy( Qt_CustomContextMenu )

   ::oUI:labelStatus:setText( "Ready" )
   ::oUI:comboExpr:setFocus()

   /* Attach all signals */
   //
   ::oUI:buttonClose  :connect( "clicked()"                         , {| | ::execEvent( __buttonClose__                ) } )
   ::oUI:buttonFolder :connect( "clicked()"                         , {| | ::execEvent( __buttonFolder__               ) } )
   ::oUI:buttonFind   :connect( "clicked()"                         , {| | ::execEvent( __buttonFind__                 ) } )
   ::oUI:buttonRepl   :connect( "clicked()"                         , {| | ::execEvent( __buttonRepl__                 ) } )
   ::oUI:buttonStop   :connect( "clicked()"                         , {| | ::execEvent( __buttonStop__                 ) } )
   ::oUI:checkAll     :connect( "stateChanged(int)"                 , {|p| ::execEvent( __checkAll__               , p ) } )
   ::oUI:comboExpr    :connect( "currentIndexChanged(QString)"      , {|p| ::execEvent( __comboFind__              , p ) } )
   ::oUI:checkListOnly:connect( "stateChanged(int)"                 , {|p| ::execEvent( __checkListOnly__          , p ) } )
   ::oUI:checkFolders :connect( "stateChanged(int)"                 , {|p| ::execEvent( __checkFolders__           , p ) } )
   ::oUI:editResults  :connect( "copyAvailable(bool)"               , {|p| ::execEvent( __editResults__            , p ) } )
   ::oUI:editResults  :connect( "customContextMenuRequested(QPoint)", {|p| ::execEvent( __editResults_contextMenu__, p ) } )

   ::qEditFind := ::oUI:comboExpr:lineEdit()
   ::qEditFind:connect( "returnPressed()", {|| ::execEvent( __buttonFind__ ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:execEvent( nEvent, p )
   LOCAL cPath, qLineEdit, qCursor, cSource, v, nInfo

   IF ::lQuitting
      RETURN Self
   ENDIF

   SWITCH nEvent

   CASE __buttonClose__
      ::oFindDock:hide()
      EXIT

   CASE __comboFind__
      ::oIde:oSBar:getItem( SB_PNL_SEARCH ):caption := "FIND: " + p
      EXIT

   CASE __checkListOnly__
      ::oUI:comboRepl:setEnabled( p == 0 )
      ::oUI:buttonRepl:setEnabled( !( p == 1 ) )
      EXIT

   CASE __checkFolders__
      ::oUI:comboFolder:setEnabled( p == 2 )
      ::oUI:checkSubFolders:setEnabled( p == 2 )
      EXIT

   CASE __buttonFind__
      ::find()
      EXIT

   CASE __buttonRepl__
      ::replaceAll()
      EXIT

   CASE __buttonStop__
      ::lStop := .t.
      EXIT

   CASE __buttonFolder__
      cPath := hbide_fetchADir( ::oDlg, "Select a folder for search operation", ::cLastFileOpenPath )
      IF !empty( cPath )
         ::oIde:cLastFileOpenPath := cPath

         qLineEdit := ::oUI:comboFolder:lineEdit()
         qLineEdit:setText( cPath )
         IF ascan( ::oINI:aFolders, {|e| e == cPath } ) == 0
            hb_ains( ::oINI:aFolders, 1, cPath, .t. )
         ENDIF
         ::oUI:comboFolder:insertItem( 0, cPath )
      ENDIF
      EXIT

   CASE __checkAll__
      v := !( p == 0 )
      ::oUI:checkPrg:setChecked( v )
      ::oUI:checkC:setChecked( v )
      ::oUI:checkCpp:setChecked( v )
      ::oUI:checkCh:setChecked( v )
      ::oUI:checkH:setChecked( v )
      ::oUI:checkRc:setChecked( v )
      EXIT

   CASE __editResults_contextMenu__
      ::execContextMenu( p )
      EXIT

   CASE __editResults__
      IF p .AND. ! ::lNotDblClick
         qCursor := ::oUI:editResults:textCursor()
         nInfo := qCursor:blockNumber() + 1

         IF nInfo <= Len( ::aInfo ) .AND. ::aInfo[ nInfo, 1 ] == -2
            cSource := ::aInfo[ nInfo, 2 ]

            ::oSM:editSource( cSource, 0, 0, 0, NIL, NIL, .f., .t. )
            qCursor := ::oIde:qCurEdit:textCursor()
            qCursor:setPosition( 0 )
            qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, ::aInfo[ nInfo, 3 ] - 1 )
            qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, ::aInfo[ nInfo, 4 ] - 1 )
            qCursor:movePosition( QTextCursor_Right, QTextCursor_KeepAnchor, Len( ::aInfo[ nInfo, 5 ] ) )
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

METHOD IdeFindInFiles:replaceAll()
   LOCAL nL, nB, qCursor, aFind
   LOCAL cSource := ""

   IF empty( ::cReplWith  := ::oUI:comboRepl:currentText() )
      RETURN Self
   ENDIF
   nL := Len( ::cReplWith )

   IF ! hbide_getYesNo( "Starting REPLACE operation", "No way to interrupt", "Critical" )
      RETURN Self
   ENDIF

   FOR EACH aFind IN ::aInfo
      IF aFind[ 1 ] == -2
         IF ! ( cSource == aFind[ 2 ] )
            cSource := aFind[ 2 ]
            ::oSM:editSource( cSource, 0, 0, 0, NIL, "Main", .f., .t. )
         ENDIF

         qCursor := ::oIde:qCurEdit:textCursor()
         qCursor:setPosition( 0 )
         qCursor:movePosition( QTextCursor_Down, QTextCursor_MoveAnchor, aFind[ 3 ] - 1 )
         qCursor:movePosition( QTextCursor_Right, QTextCursor_MoveAnchor, aFind[ 4 ] - 1 )
         qCursor:movePosition( QTextCursor_Right, QTextCursor_KeepAnchor, Len( aFind[ 5 ] ) )
         ::qCurEdit:setTextCursor( qCursor )

         nB := qCursor:position()

         qCursor:beginEditBlock()
         qCursor:removeSelectedText()
         qCursor:insertText( ::cReplWith )
         qCursor:setPosition( nB + nL )
         ::qCurEdit:setTextCursor( qCursor )
         ::oEM:getEditObjectCurrent():clearSelection()
         qCursor:endEditBlock()
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:execContextMenu( p )
   LOCAL nLine, qCursor, qMenu, qAct, cFind

   qCursor := ::oUI:editResults:textCursor()
   nLine := qCursor:blockNumber() + 1

   IF nLine <= Len( ::aInfo )
      qMenu := QMenu()

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
      qMenu:addAction( "Zom In"  )
      qMenu:addAction( "Zoom Out" )

      IF ! empty( qAct := qMenu:exec( ::oUI:editResults:mapToGlobal( p ) ) )
         SWITCH qAct:text()

         CASE "Save as..."
            EXIT
         CASE "Find"
            IF !empty( cFind := hbide_fetchAString( ::oUI:editResults, , "Find what?", "Find" ) )
               ::lNotDblClick := .T.
               IF !( ::oUI:editResults:find( cFind, 0 ) )
                  MsgBox( "Not Found" )
               ENDIF
            ENDIF
            EXIT
         CASE "Print"
            ::print()
            EXIT
         CASE "Clear"
            ::oUI:editResults:clear()
            ::aInfo := {}
            EXIT
         CASE "Copy"
            ::lNotDblClick := .T.
            ::oUI:editResults:copy()
            EXIT
         CASE "Select All"
            ::oUI:editResults:selectAll()
            EXIT
         CASE "Replace Line"
            EXIT
         CASE "Replace Source"
            EXIT
         CASE "Zoom In"
            ::oUI:editResults:zoomIn()
            EXIT
         CASE "Zoom Out"
            ::oUI:editResults:zoomOut()
            EXIT
         ENDSWITCH
      ENDIF
   ENDIF

   RETURN NIL

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

   IF empty( ::cOrigExpr := ::oUI:comboExpr:currentText() )
      RETURN Self
   ENDIF

   ::lListOnly  := ::oUI:checkListOnly:isChecked()
   ::lMatchCase := ::oUI:checkMatchCase:isChecked()
   ::cReplWith  := ::oUI:comboRepl:currentText()

   ::lRegEx := ::oUI:checkRegEx:isChecked()
   IF ::lRegEx
      ::compRegEx := hb_regExComp( ::cOrigExpr, ::lMatchCase )
      IF ! hb_isRegEx( ::compRegEx )
         MsgBox( "Error in Regular Expression" )
         RETURN Self
      ENDIF
   ENDIF

   cFolder      := ::oUI:comboFolder:currentText()
   cWrkFolder   := cFolder
   lTabs        := ::oUI:checkOpenTabs:isChecked()
   lSubF        := ::oUI:checkSubFolders:isChecked()
   lSubP        := ::oUI:checkSubProjects:isChecked()
   /* Type of files */
   lPrg         := ::oUi:checkPrg:isChecked()
   lC           := ::oUI:checkC:isChecked()
   lCpp         := ::oUI:checkCpp:isChecked()
   lH           := ::oUI:checkH:isChecked()
   lCh          := ::oUI:checkCh:isChecked()
   lRc          := ::oUI:checkRc:isChecked()       /* Conceptually it is now lText */

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
   IF ::oUI:checkFolders:isChecked() .AND. ! empty( cFolder )
      hbide_fetchSubPaths( @aPaths, cFolder, ::oUI:checkSubFolders:isChecked() )

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
   ::oUI:buttonFind:setEnabled( .f. )
   ::oUI:buttonStop:setEnabled( .t. )

   ::nSearched := 0
   ::nFounds   := 0
   ::nMisses   := 0

   ::oUI:labelStatus:setText( "Ready" )

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

   IF ::oUI:checkFolders:isChecked() .AND. ! empty( cFolder )
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

   ::oUI:labelStatus:setText( "[ Time: " + hb_ntos( nEnd - nStart ) + " ] " + ;
                         "[ Searched: " + hb_ntos( ::nSearched ) + " ] [ Finds: " + hb_ntos( ::nFounds ) + " ] " + ;
                         "[ Files not found: " + hb_ntos( ::nMisses ) + " ]" )
   ::lStop := .f.
   ::oUI:buttonStop:setEnabled( .f. )
   ::oUI:buttonFind:setEnabled( .t. )

   IF ::nFounds > 0
      IF ascan( ::oINI:aFind, {|e| e == ::cOrigExpr } ) == 0
         hb_ains( ::oINI:aFind, 1, ::cOrigExpr, .t. )
         ::oUI:comboFolder:insertItem( 0, ::cOrigExpr )
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

         IF Len( aLines ) > 0
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
   IF nNoMatch == Len( aFiles )
      ::showLog( LOG_INFO, "Searched (" + hb_ntos( Len( aFiles ) ) + ") files, no matches found" )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:showLog( nType, cMsg, aLines )
   LOCAL a_, n, cPre, cPost, nWidth, cText, nB, cL, nL, cT, cExp, aM
   LOCAL qCursor, qResult

   qResult := ::oUI:editResults

   DEFAULT cMsg TO ""
   cMsg := hbide_convertHtmlDelimiters( cMsg )

   qCursor := ::oUI:editResults:textCursor()

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
      cText := F_FILE + "<b>" + cMsg + "   ( "+ hb_ntos( Len( aLines ) ) + " )" + "</b>" + F_END
      ::oUI:editResults:append( cText )
      ::oUI:labelStatus:setText( cText )
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
            cPost := substr( cL, nB + Len( cExp ) )
            cT    := substr( cL, nB, Len( cExp ) )
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
   ::oUI:editResults:setTextCursor( qCursor )

   QApplication():processEvents()
   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_buildResultLine( cLine, aM )
   LOCAL cT, cR, i

   FOR i := 1 TO Len( aM )
      cR    := aM[ i, 1 ]
      cT    := replicate( chr( 255 ), Len( aM[ i, 1 ] ) )
      cLine := strtran( cLine, cR, cT, 1, 1 )
   NEXT
   FOR i := 1 TO Len( aM )
      cR    := replicate( chr( 255 ), Len( aM[ i, 1 ] ) )
      cT    := F_SEARCH + "<b>" + hbide_convertHtmlDelimiters( aM[ i, 1 ] ) + "</b>" + F_END
      cLine := strtran( cLine, cR, cT, 1, 1 )
   NEXT

   RETURN cLine

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:print()
   LOCAL qDlg

   qDlg := QPrintPreviewDialog( ::oUI:oWidget )
   qDlg:setWindowTitle( "Harbour-QT Preview Dialog" )
   qDlg:connect( "paintRequested(QPrinter*)", {|p| ::paintRequested( p ) } )
   qDlg:exec()
   qDlg:disconnect( "paintRequested(QPrinter*)" )

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeFindInFiles:paintRequested( qPrinter )
   ::oUI:editResults:print( qPrinter )
   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_buildFilter( lPrg, lC, lCpp, lH, lCh, lTxt )
   LOCAL aFilter := {}
   LOCAL aExt

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
      aadd( aFilter, "*.h" )
   ENDIF
   IF lTxt
      aExt := hb_atokens( hbide_setIde():oINI:cTextFileExtensions, "," )
      aeval( aExt, {|e| iif( empty( e ), NIL, aadd( aFilter, e ) ) } )
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


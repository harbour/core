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
 *                               27Dec2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"

#include "hbqt.ch"

#include "hbide.ch"

/*----------------------------------------------------------------------*/

CLASS IdeEditor

   DATA   oIde

   DATA   oTab
   DATA   cPath
   DATA   cFile
   DATA   cExt
   DATA   cType
   DATA   cTheme
   DATA   qEdit
   DATA   qDocument
   DATA   qHiliter
   DATA   sourceFile
   DATA   qLayout

   DATA   nBlock                                  INIT   -1
   DATA   nColumn                                 INIT   -1
   DATA   nBlocks                                 INIT   0

   DATA   nPos                                    INIT   0
   DATA   nHPos                                   INIT   0
   DATA   nVPos                                   INIT   0

   ACCESS qTabWidget                              INLINE ::oIde:oDA:oTabWidget:oWidget

   DATA   qCursor

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD buildTabPage()
   METHOD activateTab()
   METHOD closeTab()
   METHOD dispEditInfo()
   METHOD onBlockCountChanged()
   METHOD setTabImage()
   METHOD applyTheme()
   METHOD showPPO()
   METHOD closePPO()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeEditor:new( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

   ::oIde       := oIde
   ::sourceFile := cSourceFile
   ::nPos       := nPos
   ::nHPos      := nHPos
   ::nVPos      := nVPos
   ::cTheme     := cTheme

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:create( oIde, cSourceFile, nPos, nHPos, nVPos, cTheme )

   DEFAULT oIde        TO ::oIde
   DEFAULT cSourceFile TO ::sourceFile
   DEFAULT nPos        TO ::nPos
   DEFAULT nHPos       TO ::nHPos
   DEFAULT nVPos       TO ::nVPos
   DEFAULT cTheme      TO ::cTheme

   ::oIde       := oIde
   ::SourceFile := cSourceFile
   ::nPos       := nPos
   ::nHPos      := nHPos
   ::nVPos      := nVPos
   ::cTheme     := cTheme

   hb_fNameSplit( cSourceFile, @::cPath, @::cFile, @::cExt )

   ::cType := upper( strtran( ::cExt, ".", "" ) )
   ::cType := iif( ::cType $ "PRG,C,CPP,H,CH", ::cType, "U" )

   ::buildTabPage( ::sourceFile )

   ::qEdit := QPlainTextEdit():new( ::oTab:oWidget )
   ::qEdit:setPlainText( hb_memoRead( ::sourceFile ) )
   ::qEdit:setLineWrapMode( QTextEdit_NoWrap )
   ::qEdit:setFont( ::oIde:oFont:oWidget )
   ::qEdit:ensureCursorVisible()
 * ::qEdit:setStyleSheet( GetStyleSheet( "QPlainTextEdit" ) )

   ::qDocument := QTextDocument():configure( ::qEdit:document() )

   ::qLayout := QBoxLayout():new()
   ::qLayout:setDirection( 0 )
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:addWidget( ::qEdit )

   ::oTab:oWidget:setLayout( ::qLayout )

   IF ::cType != "U"
      ::qHiliter := ::oIde:oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
   ENDIF

   Qt_Connect_Signal( ::qEdit    , "textChanged()"          , {|| ::setTabImage() } )
   Qt_Connect_Signal( ::qEdit    , "cursorPositionChanged()", {|| ::dispEditInfo() } )

   Qt_Connect_Signal( ::qDocument, "blockCountChanged(int)" , {|o,i| ::onBlockCountChanged( i, o ) } )

   ::qEdit:show()

   /* Restore State */
   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )
   ::qCursor:setPosition( ::nPos )
   ::qEdit:setTextCursor( ::qCursor )
   //
   QScrollBar():configure( ::qEdit:horizontalScrollBar() ):setValue( ::nHPos )
   //
   QScrollBar():configure( ::qEdit:verticalScrollBar() ):setValue( ::nVPos )

   /* Populate Tabs Array */
   aadd( ::oIde:aTabs, { ::oTab, ::qEdit, ::qHiliter, ::qLayout, ::sourceFile, ::qDocument, Self } )

   ::oIde:nCurTab := len( ::oIde:aTabs )

   ::oIde:aSources := { ::sourceFile }
   ::oIde:createTags()
   ::oIde:updateFuncList()
   ::oIde:addSourceInTree( ::sourceFile )

   ::oIde:manageFocusInEditor()

   ::nBlock := ::qCursor:blockNumber()
   ::nColumn := ::qCursor:columnNumber()

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget" ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildTabPage( cSource )

   ::oTab := XbpTabPage():new( ::oIde:oDA, , { 5,5 }, { 700,400 }, , .t. )
   ::oTab:caption   := ::cFile + ::cExt
   ::oTab:minimized := .F.

   ::oTab:create()

   ::qTabWidget:setCurrentIndex( ::qTabWidget:indexOf( ::oTab:oWidget ) )
   ::qTabWidget:setTabTooltip( ::qTabWidget:indexOf( ::oTab:oWidget ), cSource )

   ::oTab:tabActivate    := {|mp1,mp2,oXbp| ::activateTab( mp1, mp2, oXbp ) }
   ::oTab:closeRequested := {|mp1,mp2,oXbp| ::closeTab( mp1, mp2, oXbp ) }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:activateTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::oIde:aTabs, {|e_| e_[ TAB_OTAB ] == oXbp } ) ) > 0
      ::oIde:nCurTab  := mp2
      ::oIde:aSources := { ::oIde:aTabs[ ::oIde:nCurTab, TAB_SOURCEFILE ] }
      ::oIde:createTags()
      ::oIde:updateFuncList()
      ::oIde:aTabs[ mp2, TAB_OEDITOR ]:dispEditInfo()
      ::oIde:manageFocusInEditor()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:closeTab( mp1, mp2, oXbp )

   HB_SYMBOL_UNUSED( mp1 )

   IF ( mp2 := ascan( ::aTabs, {|e_| e_[ 1 ] == oXbp } ) ) > 0
      ::oIde:closeSource( mp2, .t. )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:onBlockCountChanged( nNewBlocks )
   LOCAL nLine

   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )
   nLine := ::qCursor:blockNumber()
   IF ::nBlock != nLine
      HB_TRACE( HB_TR_ALWAYS, nNewBlocks, nLine, ::nBlock )
   ENDIF
   ::nBlock := nLine

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:dispEditInfo()
   LOCAL s

   ::qCursor := QTextCursor():configure( ::qEdit:textCursor() )

   s := "<b>Line "+ hb_ntos( ::qCursor:blockNumber() + 1 ) + " of " + ;
                    hb_ntos( ::qDocument:blockCount() ) + "</b>"

   ::oIde:oSBar:getItem( SB_PNL_MAIN     ):caption := "Success"
   ::oIde:oSBar:getItem( SB_PNL_READY    ):caption := "Ready"
   ::oIde:oSBar:getItem( SB_PNL_LINE     ):caption := s
   ::oIde:oSBar:getItem( SB_PNL_COLUMN   ):caption := "Col " + hb_ntos( ::qCursor:columnNumber() + 1 )
   ::oIde:oSBar:getItem( SB_PNL_INS      ):caption := iif( ::qEdit:overwriteMode() , " ", "Ins" )
   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( ::qDocument:isModified(), "Modified", " " )
   ::oIde:oSBar:getItem( SB_PNL_STREAM   ):caption := "Stream"
   ::oIde:oSBar:getItem( SB_PNL_EDIT     ):caption := "Edit"

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setTabImage()
   LOCAL nIndex    := ::qTabWidget:indexOf( ::oTab:oWidget )
   LOCAL lModified := ::qDocument:isModified()

   ::qTabWidget:setTabIcon( nIndex, ::oIde:resPath + iif( lModified, "tabmodified.png", "tabunmodified.png" ) )

   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( lModified, "Modified", " " )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:showPPO( cFile )
   LOCAL qEdit, qHiliter

   IF file( cFile )
      qEdit := QPlainTextEdit():new()
      qEdit:setPlainText( hb_memoRead( cFile ) )
      qEdit:setLineWrapMode( QTextEdit_NoWrap )
      qEdit:setFont( ::oIde:oFont:oWidget )
      qEdit:ensureCursorVisible()

      qEdit:setWindowTitle( cFile )
      qEdit:resize( 600, 400 )

      qHiliter := ::oIde:oThemes:SetSyntaxHilighting( qEdit )

      Qt_Connect_Event( qEdit, QEvent_Close, {|| ::closePPO( qEdit, qHiliter, cFile, .t. ) } )

      qEdit:show()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:closePPO( qEdit, qHiliter, cFile, lDel )

   Qt_DisConnect_Event( qEdit, QEvent_Close )

   qHiliter:pPtr := 0
   qEdit:close()
   qEdit:pPtr := 0

   IF lDel
      ferase( cFile )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:applyTheme( cTheme )

   IF ::cType != "U"
      IF empty( cTheme )
         cTheme := ::oIde:oThemes:selectTheme()
      ENDIF

      IF ::oIde:oThemes:contains( cTheme )
         ::cTheme := cTheme
         ::qHiliter := ::oIde:oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/


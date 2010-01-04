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

CLASS IdeEditor INHERIT IdeObject

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
   DATA   nID

   DATA   qCursor

   METHOD new()
   METHOD create()
   METHOD destroy()

   METHOD buildTabPage()
   METHOD removeTabPage()
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
   ::nID        := hbide_getNextUniqueID()
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
   ::SourceFile := hbide_pathNormalized( cSourceFile, .F. )
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
   ::qEdit:setFont( ::oFont:oWidget )
   ::qEdit:ensureCursorVisible()
 * ::qEdit:setStyleSheet( GetStyleSheet( "QPlainTextEdit" ) )

   ::qDocument := QTextDocument():configure( ::qEdit:document() )

   ::qLayout := QBoxLayout():new()
   ::qLayout:setDirection( 0 )
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:addWidget( ::qEdit )

   ::oTab:oWidget:setLayout( ::qLayout )

   IF ::cType != "U"
      ::qHiliter := ::oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
   ENDIF

   Qt_Slots_Connect( ::pSlots, ::qEdit    , "textChanged()"          , {|| ::setTabImage() } )
   Qt_Slots_Connect( ::pSlots, ::qEdit    , "cursorPositionChanged()", {|| ::dispEditInfo() } )

   Qt_Slots_Connect( ::pSlots, ::qDocument, "blockCountChanged(int)" , {|o,i| ::onBlockCountChanged( i, o ) } )

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
   aadd( ::aTabs, { ::oTab, ::qEdit, ::qHiliter, ::qLayout, ::sourceFile, ::qDocument, Self } )

   ::oIde:nCurTab := len( ::oIde:aTabs )

   ::oIde:aSources := { ::sourceFile }
   ::oIde:createTags()
   ::oIde:updateFuncList()
   ::oIde:addSourceInTree( ::sourceFile )
   ::oIde:updateTitleBar()
   ::oIde:manageFocusInEditor()

   ::nBlock  := ::qCursor:blockNumber()
   ::nColumn := ::qCursor:columnNumber()

   ::qTabWidget:setStyleSheet( GetStyleSheet( "QTabWidget" ) )
   ::setTabImage()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:destroy()

   ::RemoveTabPage()

   RETURN Self

/*----------------------------------------------------------------------*/
/*
 * Remove the tab of the main screen and clean the objects from memory.Note that
 * this function does not question the User if he wants to save or not the
 * content combined with this TAB if you want to confirm the action use
 * HbIde:closeSource() instead.
 * 02/01/2010 - 12:58:53 - vailtom
 */
METHOD IdeEditor:removeTabPage()
   LOCAL cSource := ::sourceFile
   LOCAL n

   n := aScan( ::oIde:aTabs, {|e_| e_[ TAB_OEDITOR ]:nID == ::nID } )

   IF n > 0
      hb_aDel( ::oIde:aTabs, n, .T. )
   ENDIF

   n := ::oIde:qTabWidget:indexOf( ::oTab:oWidget )
   ::oIde:qTabWidget:removeTab( n  )

   /* Destroy all objects */
   // { oTab, qEdit, qHiliter, qLayout, cSourceFile, qDocument }
   //
   IF !Empty( ::qEdit )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "textChanged()" )
      Qt_Slots_disConnect( ::pSlots, ::qEdit, "cursorPositionChanged()" )
      /* To avoid recursive calls on invalid pointers */
   ENDIF

   IF !Empty( ::qDocument )
      Qt_Slots_disConnect( ::pSlots, ::qDocument, "blockCountChanged(int)" )
      ::qDocument:pPtr := 0
      ::qDocument      := nil
   ENDIF

   IF !Empty( ::qLayout )
      ::qLayout:pPtr := 0
      ::qLayout      := nil
   ENDIF

   IF !Empty( ::qHiliter )
      ::qHiliter:pPtr := 0
      ::qHiliter      := nil
   ENDIF

   IF !Empty( ::qEdit )
      ::qEdit:pPtr := 0
      ::qEdit      := nil

      ::oIde:oFuncList:clear()
   ENDIF

   IF !Empty( ::oTab )
//      ::oTab:Destroy()
//      ::oTab:pPtr := 0
//      ::oTab      := nil
   ENDIF

   IF ( n := aScan( ::oIde:aProjData, {|e_| e_[ 4 ] == cSource } ) ) > 0
      ::aProjData[ n,3 ]:delItem( ::oIde:aProjData[ n,1 ] )
      hb_aDel( ::aProjData, n, .T. )
   ENDIF

   IF ( n := aScan( ::oIde:aEdits, {|e_| e_:nID == ::nID } ) ) > 0
      hb_aDel( ::oIde:aEdits, n, .T. )
   ENDIF

   /*
    * TOFIX: Release memory from these objects & arrays
    *
    *  aTabs     - OK
    *  aSources
    *  aEdits    - OK
    *
    */
   IF ::qTabWidget:count() == 0
      IF ::lDockRVisible
         ::oDockR:hide()
         ::lDockRVisible := .f.
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:buildTabPage( cSource )

   ::oTab := XbpTabPage():new( ::oIde:oDA, , { 5,5 }, { 700,400 }, , .t. )

   IF Empty( cSource )
      ::oTab:caption   := "Untitled " + hb_ntos( hbide_getNextUntitled() )
   ELSE
      ::oTab:caption   := ::cFile + ::cExt
   ENDIF

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
      ::aTabs[ mp2, TAB_OEDITOR ]:dispEditInfo()
      ::oIde:updateTitleBar()
      ::oIde:manageFocusInEditor()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:closeTab( mp1, mp2, oXbp )

   IF PCount() == 00
      mp1 := ::nID
      mp2 := ascan( ::aTabs, {|e_| e_[ TAB_OEDITOR ]:nID == mp1  } )
   ELSE
      mp2 := ascan( ::aTabs, {|e_| e_[ TAB_OTAB ] == oXbp } )
   ENDIF

 * Requested tab exists?
   IF !Empty( mp2 )
      ::oIde:closeSource( mp2 )
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
   ::oIde:oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( ::qDocument:isModified(), "Modified", iif( ::qEdit:isReadOnly(), "ReadOnly", " " ) )

   ::oIde:oSBar:getItem( SB_PNL_STREAM   ):caption := "Stream"
   ::oIde:oSBar:getItem( SB_PNL_EDIT     ):caption := "Edit"

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:setTabImage()
   LOCAL nIndex    := ::qTabWidget:indexOf( ::oTab:oWidget )
   LOCAL lModified := ::qDocument:isModified()
   LOCAL lReadOnly := ::qEdit:isReadOnly()
   LOCAL cIcon

   IF lModified
      cIcon := "tabmodified.png"
   ELSEIF lReadOnly
      cIcon := "tabreadonly.png"
   ELSE
      cIcon := "tabunmodified.png"
   ENDIF

   ::qTabWidget:setTabIcon( nIndex, ::resPath + cIcon )
   ::oSBar:getItem( SB_PNL_MODIFIED ):caption := iif( lModified, "Modified", iif( lReadOnly, "ReadOnly", " " ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:showPPO( cFile )
   LOCAL qEdit, qHiliter

   IF hb_FileExists( cFile )
      qEdit := QPlainTextEdit():new()
      qEdit:setPlainText( hb_memoRead( cFile ) )
      qEdit:setLineWrapMode( QTextEdit_NoWrap )
      qEdit:setFont( ::oIde:oFont:oWidget )
      qEdit:ensureCursorVisible()

      qEdit:setWindowTitle( cFile )
      qEdit:resize( 600, 400 )

      qHiliter := ::oIde:oThemes:SetSyntaxHilighting( qEdit )

      Qt_Events_Connect( ::pEvents, qEdit, QEvent_Close, {|| ::closePPO( qEdit, qHiliter, cFile, .t. ) } )

      qEdit:show()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeEditor:closePPO( qEdit, qHiliter, cFile, lDel )

   Qt_Events_DisConnect( ::pEvents, qEdit, QEvent_Close )

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
         cTheme := ::oThemes:selectTheme()
      ENDIF

      IF ::oThemes:contains( cTheme )
         ::cTheme := cTheme
         ::qHiliter := ::oIde:oThemes:SetSyntaxHilighting( ::qEdit, @::cTheme )
      ENDIF
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

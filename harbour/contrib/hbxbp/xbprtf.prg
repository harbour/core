/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 *                    Xbase++ Compatible XbpRtf Class
 *
 *                            Pritpal Bedi
 *                              10Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpRtf INHERIT XbpWindow

   METHOD   init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )
   METHOD   configure()
   METHOD   destroy()
   METHOD   connect()
   METHOD   disconnect()
   METHOD   handleEvent( nEvent, mp1, mp2 )       VIRTUAL
   METHOD   execSlot( cSlot, p )
   METHOD   setStyle()                            VIRTUAL

   DATA     appearance                            INIT      XBP_APPEARANCE_3D
   DATA     bulletIndent                          INIT      0
   DATA     changed                               INIT      .F.
   DATA     hideSelection                         INIT      .T.
   DATA     locked                                INIT      .F.
   DATA     maxLength                             INIT      0
   DATA     popupMenu                             INIT      NIL // XbpMenu
   DATA     rightMargin                           INIT      0
   DATA     scrollbars                            INIT      XBP_SCROLLBAR_HORIZ + XBP_SCROLLBAR_VERT
   DATA     usePopupMenu                          INIT      .T.

   METHOD   text                                  SETGET
   METHOD   textRTF                               SETGET

   METHOD   selAlignment                          SETGET    // XBPRTF_ALIGN_LEFT
   METHOD   selBold                               SETGET    // .F.
   METHOD   selBullet                             SETGET    // .F.
   METHOD   selCharOffset                         SETGET    // 0
   METHOD   selColor                              SETGET    //
   METHOD   selFontName                           SETGET    // ""
   METHOD   selFontSize                           SETGET    // 0
   METHOD   selHangingIndent                      SETGET    // 0
   METHOD   selIndent                             SETGET    // 0
   METHOD   selItalic                             SETGET    // .F.
   METHOD   selLength                             SETGET    // 0
   METHOD   selRightIndent                        SETGET    // 0
   METHOD   selStart                              SETGET    // 0
   METHOD   selStrikeThru                         SETGET    // .F.
   METHOD   selTabCount                           SETGET    // 0
   METHOD   selReadOnly                           SETGET    // .F.
   METHOD   selText                               SETGET    // ""
   METHOD   selUnderline                          SETGET    // .F.

   METHOD   loadFile( cFile )
   METHOD   saveFile( cFile )
   METHOD   clear()
   METHOD   copy()
   METHOD   cut()
   METHOD   find( cSearchString, nStart, nEnd, nOptions )
   METHOD   getLineFromChar( nChar )
   METHOD   getLineStart( nLine )
   METHOD   undo()
   METHOD   paste()
   METHOD   print( oXbpPrinter, lOnlySelection )
   METHOD   selTabs( nTab, nPos )
   METHOD   span( cCharacters, bForward, bExclude )

   /*< Harbour Extensions >*/
   METHOD   redo()
   METHOD   insertText( cText )
   METHOD   insertImage( cImageFilename )
   METHOD   selFont                               SETGET
   /*</Harbour Extensions >*/

   DATA     sl_xbeRTF_Change
   METHOD   change( ... )                         SETGET

   DATA     sl_xbeRTF_SelChange
   METHOD   selChange( ... )                      SETGET

   PROTECTED:

   DATA     oTextCharFormat                       INIT      QTextCharFormat()
   DATA     oTextDocument                         INIT      QTextDocument()
   DATA     oTextCursor                           INIT      QTextCursor()

   DATA     oCurCursor                                                  // ontained by one of :sel*

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpRtf:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::xbpWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::oWidget := QTextEdit( ::pParent )

   ::connect()
   ::setPosAndSize()
   IF ::visible
      ::show()
   ENDIF
   ::oParent:AddChild( SELF )
   ::postCreate()

   ::oTextDocument   := ::oWidget:document()
   ::oTextCursor     := ::oWidget:textCursor()
   ::oTextCharFormat := ::oTextCursor:charFormat()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:execSlot( cSlot, p )

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE cSlot == "copyAvailable(bool)"
   CASE cSlot == "currentCharFormatChanged(QTextCharFormat)"

   CASE cSlot == "cursorPositionChanged()"
      ::oTextCursor := ::oWidget:textCursor()
      ::oCurCursor := ::oTextCursor
   CASE cSlot == "redoAvailable(bool)"
   CASE cSlot == "undoAvailable(bool)"
   CASE cSlot == "textChanged()"          /* Xbase++ Implements */
      ::changed := .t.                    /* .f. only at save */
      ::change()
   CASE cSlot == "selectionChanged()"     /* Xbase++ Implements */
      ::oTextCursor := ::oWidget:textCursor()
      ::oCurCursor := ::oTextCursor
      ::selChange()
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:configure()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:connect()

*  ::oWidget:connect( "copyAvailable(bool)"                      , {|p| ::execSlot( "copyAvailable(bool)"    , p ) } )
   ::oWidget:connect( "currentCharFormatChanged(QTextCharFormat)", {|p| ::execSlot( "currentCharFormatChanged(QTextCharFormat)", p ) } )
   ::oWidget:connect( "cursorPositionChanged()"                  , {|p| ::execSlot( "cursorPositionChanged()", p ) } )
   ::oWidget:connect( "redoAvailable(bool)"                      , {|p| ::execSlot( "redoAvailable(bool)"    , p ) } )
   ::oWidget:connect( "undoAvailable(bool)"                      , {|p| ::execSlot( "undoAvailable(bool)"    , p ) } )
   ::oWidget:connect( "textChanged()"                            , {|p| ::execSlot( "textChanged()"          , p ) } )
   ::oWidget:connect( "selectionChanged()"                       , {|p| ::execSlot( "selectionChanged()"     , p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:disconnect()

   IF ! empty( ::oWidget )
   *  ::oWidget:disconnect( "copyAvailable(bool)"                       )
      ::oWidget:disconnect( "currentCharFormatChanged(QTextCharFormat)" )
      ::oWidget:disconnect( "cursorPositionChanged()"                   )
      ::oWidget:disconnect( "redoAvailable(bool)"                       )
      ::oWidget:disconnect( "undoAvailable(bool)"                       )
      ::oWidget:disconnect( "textChanged()"                             )
      ::oWidget:disconnect( "selectionChanged()"                        )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:destroy()

   ::disconnect()
   ::xbpWindow:destroy()

   ::oTextDocument   := NIL
   ::oTextCursor     := NIL
   ::oTextCharFormat := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:loadFile( cFile )

   IF file( cFile )
      ::oWidget:setText( memoread( cFile ) )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:saveFile( cFile )

   IF ( '.txt' $ lower( cFile ) )
      memowrit( cFile, ::oWidget:toPlainText() )
   ELSE
      memowrit( cFile, ::oWidget:toHTML() )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:clear()

   ::oWidget:clear()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:copy()

   ::oWidget:copy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:cut()

   ::oWidget:cut()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:find( cSearchString, nStart, nEnd, nOptions )
   LOCAL nPos := 0

   HB_SYMBOL_UNUSED( nStart )
   HB_SYMBOL_UNUSED( nEnd )
   HB_SYMBOL_UNUSED( nOptions )

   IF hb_isChar( cSearchString )
      ::oTextDocument := ::oWidget:document()
      ::oTextCursor   := ::oTextDocument:find( cSearchString )
      ::oCurCursor         := ::oTextCursor
      nPos := ::oTextCursor:position()
   ENDIF
   RETURN nPos

/*----------------------------------------------------------------------*/

METHOD XbpRtf:getLineFromChar( nChar )
   LOCAL nLine := 0

   HB_SYMBOL_UNUSED( nChar )

   RETURN nLine

/*----------------------------------------------------------------------*/

METHOD XbpRtf:getLineStart( nLine )
   LOCAL nChar := 0

   HB_SYMBOL_UNUSED( nLine )

   RETURN nChar

/*----------------------------------------------------------------------*/

METHOD XbpRtf:undo()

   ::oWidget:undo()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:paste()

   ::oWidget:paste()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:print( oXbpPrinter, lOnlySelection )

   IF !HB_ISOBJECT( oXbpPrinter )
      oXbpPrinter := XbpPrinter():new():create()
   ENDIF

   IF HB_ISLOGICAL( lOnlySelection ) .and. lOnlySelection
      ::oWidget:print( oXbpPrinter:oWidget )
   ELSE
      ::oWidget:print( oXbpPrinter:oWidget )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:redo()

   ::oWidget:redo()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selTabs( nTab, nPos )

   HB_SYMBOL_UNUSED( nTab )
   HB_SYMBOL_UNUSED( nPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:span( cCharacters, bForward, bExclude )

   DEFAULT bForward TO .T.

   HB_SYMBOL_UNUSED( cCharacters )
   HB_SYMBOL_UNUSED( bForward )
   HB_SYMBOL_UNUSED( bExclude )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:change( ... )
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISBLOCK( aP[ 1 ] )
      ::sl_xbeRTF_Change := aP[ 1 ]
   ELSEIF HB_ISBLOCK( ::sl_xbeRTF_Change )
      asize( aP, 2 )
      eval( ::sl_xbeRTF_Change, aP[ 1 ], aP[ 2 ], Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selChange( ... )
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISBLOCK( aP[ 1 ] )
      ::sl_xbeRTF_SelChange := aP[ 1 ]
   ELSEIF HB_ISBLOCK( ::sl_xbeRTF_SelChange )
      asize( aP, 2 )
      eval( ::sl_xbeRTF_SelChange, aP[ 1 ], aP[ 2 ], Self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selAlignment( ... )                           // XBPRTF_ALIGN_LEFT
   LOCAL xRet := XBPRTF_ALIGN_LEFT
   LOCAL aP   := hb_aParams()
   LOCAL oTBFormat

   IF len( aP ) >= 1 .and. HB_ISNUMERIC( aP[ 1 ] )
      oTBFormat := ::oCurCursor:blockFormat()
      xRet := oTBFormat:alignment()
      oTBFormat:setAlignment( hbxbp_ConvertAFactFromXBP( "RTFSELALIGNMENT", aP[ 1 ] ) )
      ::oCurCursor:setBlockFormat( oTBFormat )
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selBold( ... )                                // .F.
   LOCAL xRet := .f.
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISLOGICAL( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         ::oTextCharFormat:setFontWeight( QFont_Bold )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selBullet( ... )                              // .F.
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   // ::bulletIndent

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selCharOffset( ... )                          // 0
   LOCAL xRet := 0, nAlign
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISNUMERIC( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         nAlign := IIF( aP[ 1 ] < 0, -1, IIF( aP[ 1 ] > 0, 1, 0 ) )
         xRet   := ::oTextCharFormat:verticalAlignment()
         ::oTextCharFormat:setVerticalAlignment( hbxbp_ConvertAFactFromXBP( "RtfVerticalAlign", nAlign ) )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selColor( ... )
   LOCAL xRet := 0
   LOCAL oBrush, nColor, oColor
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .AND. HB_ISNUMERIC( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         xRet   := ::oTextCharFormat:foreground()
         nColor := hbxbp_ConvertAFactFromXBP( "COLOR", aP[ 1 ] )
         oColor := QColor( nColor )
         oBrush := QBrush( oColor )
         ::oTextCharFormat:setForeground( oBrush )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/
//  This is Harour Extension
//
METHOD XbpRtf:selFont( ... )                            // ""
   LOCAL xRet := NIL
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISOBJECT( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         ::oTextCharFormat:setFont( aP[ 1 ]:oWidget )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selFontName( ... )                            // ""
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. hb_isChar( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         xRet := ::oTextCharFormat:fontFamily()
         ::oTextCharFormat:setFontFamily( aP[ 1 ] )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selFontSize( ... )                            // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   ::oTextCharFormat := ::oCurCursor:charFormat()
   IF ::oTextCharFormat:isValid()
      xRet := ::oTextCharFormat:fontPointSize()
      IF len( aP ) >= 1 .and. HB_ISNUMERIC( aP[ 1 ] )
         ::oTextCharFormat:setFontPointSize( aP[ 1 ] )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selHangingIndent( ... )                       // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selIndent( ... )                              // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selItalic( ... )                              // .F.
   LOCAL xRet := .f.
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISLOGICAL( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         ::oTextCharFormat:setFontItalic( aP[ 1 ] )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selLength( ... )                              // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISNUMERIC( ap[ 1 ] )
      ::oCurCursor:movePosition( QTextCursor_NextCharacter, QTextCursor_KeepAnchor, aP[ 1 ] )
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selRightIndent( ... )                         // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selStart( ... )                               // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISNUMERIC( aP[ 1 ] )
      ::oTextCursor := ::oWidget:textCursor()
      ::oCurCursor := ::oTextCursor
      xRet := ::oCurCursor:position()
      ::oCurCursor:setPosition( aP[ 1 ] )
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selStrikeThru( ... )                          // .F.
   LOCAL xRet := .f.
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISLOGICAL( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         ::oTextCharFormat:setFontStrikeOut( aP[ 1 ] )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selTabCount( ... )                            // 0
   LOCAL xRet := 0
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selReadOnly( ... )                            // .F.
   LOCAL xRet := .f.
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selText( ... )                                // ""
   LOCAL xRet := ""
   LOCAL aP := hb_aParams()

   IF ::oCurCursor:hasSelection()
      xRet := ::oCurCursor:selectedText()
   ENDIF
   IF len( aP ) >= 1 .and. hb_isChar( aP[ 1 ] )
      ::oCurCursor:removeSelectedText()
      ::oCurCursor:insertText( aP[ 1 ] )
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:selUnderline( ... )                           // .F.
   LOCAL xRet := .f.
   LOCAL aP := hb_aParams()

   IF len( aP ) >= 1 .and. HB_ISLOGICAL( aP[ 1 ] )
      ::oTextCharFormat := ::oCurCursor:charFormat()
      IF ::oTextCharFormat:isValid()
         ::oTextCharFormat:setFontUnderline( aP[ 1 ] )
         ::oCurCursor:setCharFormat( ::oTextCharFormat )
      ENDIF
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:text( ... )                                   // ""
   LOCAL xRet := ::oWidget:toPlainText()
   LOCAL aP := hb_aParams()

   IF len( aP ) == 1 .and. hb_isChar( aP[ 1 ] )
      ::oWidget:setPlainText( aP[ 1 ] )
   ENDIF

   IF len( aP ) >= 1
   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:textRTF( ... )                                // ""
   LOCAL xRet := ::oWidget:toHtml()
   LOCAL aP := hb_aParams()

   IF len( aP ) == 1 .and. hb_isChar( aP[ 1 ] )
      ::oWidget:setHTML( aP[ 1 ] )
   ENDIF

   IF len( aP ) >= 1

   ENDIF
   RETURN xRet

/*----------------------------------------------------------------------*/

METHOD XbpRtf:insertText( cText )

   ::oWidget:insertPlainText( cText )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpRtf:insertImage( cImageFilename )

   ::oCurCursor:insertImage( cImageFilename )

   RETURN Self

/*----------------------------------------------------------------------*/

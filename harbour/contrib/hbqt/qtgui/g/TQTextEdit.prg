/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QTextEdit( ... )
   RETURN HB_QTextEdit():new( ... )


CREATE CLASS QTextEdit INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QTextEdit

   METHOD  new( ... )

   METHOD  acceptRichText()
   METHOD  alignment()
   METHOD  anchorAt( pPos )
   METHOD  autoFormatting()
   METHOD  canPaste()
   METHOD  createStandardContextMenu( ... )
   METHOD  currentCharFormat()
   METHOD  currentFont()
   METHOD  cursorForPosition( pPos )
   METHOD  cursorRect( ... )
   METHOD  cursorWidth()
   METHOD  document()
   METHOD  documentTitle()
   METHOD  ensureCursorVisible()
   METHOD  find( cExp, nOptions )
   METHOD  fontFamily()
   METHOD  fontItalic()
   METHOD  fontPointSize()
   METHOD  fontUnderline()
   METHOD  fontWeight()
   METHOD  isReadOnly()
   METHOD  isUndoRedoEnabled()
   METHOD  lineWrapColumnOrWidth()
   METHOD  lineWrapMode()
   METHOD  loadResource( nType, pName )
   METHOD  mergeCurrentCharFormat( pModifier )
   METHOD  moveCursor( nOperation, nMode )
   METHOD  overwriteMode()
   METHOD  print( pPrinter )
   METHOD  setAcceptRichText( lAccept )
   METHOD  setAutoFormatting( nFeatures )
   METHOD  setCurrentCharFormat( pFormat )
   METHOD  setCursorWidth( nWidth )
   METHOD  setDocument( pDocument )
   METHOD  setDocumentTitle( cTitle )
   METHOD  setLineWrapColumnOrWidth( nW )
   METHOD  setLineWrapMode( nMode )
   METHOD  setOverwriteMode( lOverwrite )
   METHOD  setReadOnly( lRo )
   METHOD  setTabChangesFocus( lB )
   METHOD  setTabStopWidth( nWidth )
   METHOD  setTextCursor( pCursor )
   METHOD  setTextInteractionFlags( nFlags )
   METHOD  setUndoRedoEnabled( lEnable )
   METHOD  setWordWrapMode( nPolicy )
   METHOD  tabChangesFocus()
   METHOD  tabStopWidth()
   METHOD  textBackgroundColor()
   METHOD  textColor()
   METHOD  textCursor()
   METHOD  textInteractionFlags()
   METHOD  toHtml()
   METHOD  toPlainText()
   METHOD  wordWrapMode()
   METHOD  append( cText )
   METHOD  clear()
   METHOD  copy()
   METHOD  cut()
   METHOD  insertHtml( cText )
   METHOD  insertPlainText( cText )
   METHOD  paste()
   METHOD  redo()
   METHOD  scrollToAnchor( cName )
   METHOD  selectAll()
   METHOD  setAlignment( nA )
   METHOD  setCurrentFont( pF )
   METHOD  setFontFamily( cFontFamily )
   METHOD  setFontItalic( lItalic )
   METHOD  setFontPointSize( nS )
   METHOD  setFontUnderline( lUnderline )
   METHOD  setFontWeight( nWeight )
   METHOD  setHtml( cText )
   METHOD  setPlainText( cText )
   METHOD  setText( cText )
   METHOD  setTextBackgroundColor( pC )
   METHOD  setTextColor( pC )
   METHOD  undo()
   METHOD  zoomIn( nRange )
   METHOD  zoomOut( nRange )

   ENDCLASS


METHOD QTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextEdit( ... )
   RETURN Self


METHOD QTextEdit:acceptRichText()
   RETURN Qt_QTextEdit_acceptRichText( ::pPtr )


METHOD QTextEdit:alignment()
   RETURN Qt_QTextEdit_alignment( ::pPtr )


METHOD QTextEdit:anchorAt( pPos )
   RETURN Qt_QTextEdit_anchorAt( ::pPtr, hbqt_ptr( pPos ) )


METHOD QTextEdit:autoFormatting()
   RETURN Qt_QTextEdit_autoFormatting( ::pPtr )


METHOD QTextEdit:canPaste()
   RETURN Qt_QTextEdit_canPaste( ::pPtr )


METHOD QTextEdit:createStandardContextMenu( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QMenu * createStandardContextMenu ( const QPoint & position )
                // PO p QPoint
         RETURN QMenu():from( Qt_QTextEdit_createStandardContextMenu_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QMenu * createStandardContextMenu ()
      RETURN QMenu():from( Qt_QTextEdit_createStandardContextMenu( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QTextEdit:currentCharFormat()
   RETURN Qt_QTextEdit_currentCharFormat( ::pPtr )


METHOD QTextEdit:currentFont()
   RETURN Qt_QTextEdit_currentFont( ::pPtr )


METHOD QTextEdit:cursorForPosition( pPos )
   RETURN Qt_QTextEdit_cursorForPosition( ::pPtr, hbqt_ptr( pPos ) )


METHOD QTextEdit:cursorRect( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QRect cursorRect ( const QTextCursor & cursor ) const
                // PO p QTextCursor
         RETURN QRect():from( Qt_QTextEdit_cursorRect( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 0
             // QRect cursorRect () const
      RETURN QRect():from( Qt_QTextEdit_cursorRect_1( ::pPtr, ... ) )
   ENDCASE
   RETURN NIL


METHOD QTextEdit:cursorWidth()
   RETURN Qt_QTextEdit_cursorWidth( ::pPtr )


METHOD QTextEdit:document()
   RETURN Qt_QTextEdit_document( ::pPtr )


METHOD QTextEdit:documentTitle()
   RETURN Qt_QTextEdit_documentTitle( ::pPtr )


METHOD QTextEdit:ensureCursorVisible()
   RETURN Qt_QTextEdit_ensureCursorVisible( ::pPtr )


METHOD QTextEdit:find( cExp, nOptions )
   RETURN Qt_QTextEdit_find( ::pPtr, cExp, nOptions )


METHOD QTextEdit:fontFamily()
   RETURN Qt_QTextEdit_fontFamily( ::pPtr )


METHOD QTextEdit:fontItalic()
   RETURN Qt_QTextEdit_fontItalic( ::pPtr )


METHOD QTextEdit:fontPointSize()
   RETURN Qt_QTextEdit_fontPointSize( ::pPtr )


METHOD QTextEdit:fontUnderline()
   RETURN Qt_QTextEdit_fontUnderline( ::pPtr )


METHOD QTextEdit:fontWeight()
   RETURN Qt_QTextEdit_fontWeight( ::pPtr )


METHOD QTextEdit:isReadOnly()
   RETURN Qt_QTextEdit_isReadOnly( ::pPtr )


METHOD QTextEdit:isUndoRedoEnabled()
   RETURN Qt_QTextEdit_isUndoRedoEnabled( ::pPtr )


METHOD QTextEdit:lineWrapColumnOrWidth()
   RETURN Qt_QTextEdit_lineWrapColumnOrWidth( ::pPtr )


METHOD QTextEdit:lineWrapMode()
   RETURN Qt_QTextEdit_lineWrapMode( ::pPtr )


METHOD QTextEdit:loadResource( nType, pName )
   RETURN Qt_QTextEdit_loadResource( ::pPtr, nType, hbqt_ptr( pName ) )


METHOD QTextEdit:mergeCurrentCharFormat( pModifier )
   RETURN Qt_QTextEdit_mergeCurrentCharFormat( ::pPtr, hbqt_ptr( pModifier ) )


METHOD QTextEdit:moveCursor( nOperation, nMode )
   RETURN Qt_QTextEdit_moveCursor( ::pPtr, nOperation, nMode )


METHOD QTextEdit:overwriteMode()
   RETURN Qt_QTextEdit_overwriteMode( ::pPtr )


METHOD QTextEdit:print( pPrinter )
   RETURN Qt_QTextEdit_print( ::pPtr, hbqt_ptr( pPrinter ) )


METHOD QTextEdit:setAcceptRichText( lAccept )
   RETURN Qt_QTextEdit_setAcceptRichText( ::pPtr, lAccept )


METHOD QTextEdit:setAutoFormatting( nFeatures )
   RETURN Qt_QTextEdit_setAutoFormatting( ::pPtr, nFeatures )


METHOD QTextEdit:setCurrentCharFormat( pFormat )
   RETURN Qt_QTextEdit_setCurrentCharFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QTextEdit:setCursorWidth( nWidth )
   RETURN Qt_QTextEdit_setCursorWidth( ::pPtr, nWidth )


METHOD QTextEdit:setDocument( pDocument )
   RETURN Qt_QTextEdit_setDocument( ::pPtr, hbqt_ptr( pDocument ) )


METHOD QTextEdit:setDocumentTitle( cTitle )
   RETURN Qt_QTextEdit_setDocumentTitle( ::pPtr, cTitle )


METHOD QTextEdit:setLineWrapColumnOrWidth( nW )
   RETURN Qt_QTextEdit_setLineWrapColumnOrWidth( ::pPtr, nW )


METHOD QTextEdit:setLineWrapMode( nMode )
   RETURN Qt_QTextEdit_setLineWrapMode( ::pPtr, nMode )


METHOD QTextEdit:setOverwriteMode( lOverwrite )
   RETURN Qt_QTextEdit_setOverwriteMode( ::pPtr, lOverwrite )


METHOD QTextEdit:setReadOnly( lRo )
   RETURN Qt_QTextEdit_setReadOnly( ::pPtr, lRo )


METHOD QTextEdit:setTabChangesFocus( lB )
   RETURN Qt_QTextEdit_setTabChangesFocus( ::pPtr, lB )


METHOD QTextEdit:setTabStopWidth( nWidth )
   RETURN Qt_QTextEdit_setTabStopWidth( ::pPtr, nWidth )


METHOD QTextEdit:setTextCursor( pCursor )
   RETURN Qt_QTextEdit_setTextCursor( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QTextEdit:setTextInteractionFlags( nFlags )
   RETURN Qt_QTextEdit_setTextInteractionFlags( ::pPtr, nFlags )


METHOD QTextEdit:setUndoRedoEnabled( lEnable )
   RETURN Qt_QTextEdit_setUndoRedoEnabled( ::pPtr, lEnable )


METHOD QTextEdit:setWordWrapMode( nPolicy )
   RETURN Qt_QTextEdit_setWordWrapMode( ::pPtr, nPolicy )


METHOD QTextEdit:tabChangesFocus()
   RETURN Qt_QTextEdit_tabChangesFocus( ::pPtr )


METHOD QTextEdit:tabStopWidth()
   RETURN Qt_QTextEdit_tabStopWidth( ::pPtr )


METHOD QTextEdit:textBackgroundColor()
   RETURN Qt_QTextEdit_textBackgroundColor( ::pPtr )


METHOD QTextEdit:textColor()
   RETURN Qt_QTextEdit_textColor( ::pPtr )


METHOD QTextEdit:textCursor()
   RETURN Qt_QTextEdit_textCursor( ::pPtr )


METHOD QTextEdit:textInteractionFlags()
   RETURN Qt_QTextEdit_textInteractionFlags( ::pPtr )


METHOD QTextEdit:toHtml()
   RETURN Qt_QTextEdit_toHtml( ::pPtr )


METHOD QTextEdit:toPlainText()
   RETURN Qt_QTextEdit_toPlainText( ::pPtr )


METHOD QTextEdit:wordWrapMode()
   RETURN Qt_QTextEdit_wordWrapMode( ::pPtr )


METHOD QTextEdit:append( cText )
   RETURN Qt_QTextEdit_append( ::pPtr, cText )


METHOD QTextEdit:clear()
   RETURN Qt_QTextEdit_clear( ::pPtr )


METHOD QTextEdit:copy()
   RETURN Qt_QTextEdit_copy( ::pPtr )


METHOD QTextEdit:cut()
   RETURN Qt_QTextEdit_cut( ::pPtr )


METHOD QTextEdit:insertHtml( cText )
   RETURN Qt_QTextEdit_insertHtml( ::pPtr, cText )


METHOD QTextEdit:insertPlainText( cText )
   RETURN Qt_QTextEdit_insertPlainText( ::pPtr, cText )


METHOD QTextEdit:paste()
   RETURN Qt_QTextEdit_paste( ::pPtr )


METHOD QTextEdit:redo()
   RETURN Qt_QTextEdit_redo( ::pPtr )


METHOD QTextEdit:scrollToAnchor( cName )
   RETURN Qt_QTextEdit_scrollToAnchor( ::pPtr, cName )


METHOD QTextEdit:selectAll()
   RETURN Qt_QTextEdit_selectAll( ::pPtr )


METHOD QTextEdit:setAlignment( nA )
   RETURN Qt_QTextEdit_setAlignment( ::pPtr, nA )


METHOD QTextEdit:setCurrentFont( pF )
   RETURN Qt_QTextEdit_setCurrentFont( ::pPtr, hbqt_ptr( pF ) )


METHOD QTextEdit:setFontFamily( cFontFamily )
   RETURN Qt_QTextEdit_setFontFamily( ::pPtr, cFontFamily )


METHOD QTextEdit:setFontItalic( lItalic )
   RETURN Qt_QTextEdit_setFontItalic( ::pPtr, lItalic )


METHOD QTextEdit:setFontPointSize( nS )
   RETURN Qt_QTextEdit_setFontPointSize( ::pPtr, nS )


METHOD QTextEdit:setFontUnderline( lUnderline )
   RETURN Qt_QTextEdit_setFontUnderline( ::pPtr, lUnderline )


METHOD QTextEdit:setFontWeight( nWeight )
   RETURN Qt_QTextEdit_setFontWeight( ::pPtr, nWeight )


METHOD QTextEdit:setHtml( cText )
   RETURN Qt_QTextEdit_setHtml( ::pPtr, cText )


METHOD QTextEdit:setPlainText( cText )
   RETURN Qt_QTextEdit_setPlainText( ::pPtr, cText )


METHOD QTextEdit:setText( cText )
   RETURN Qt_QTextEdit_setText( ::pPtr, cText )


METHOD QTextEdit:setTextBackgroundColor( pC )
   RETURN Qt_QTextEdit_setTextBackgroundColor( ::pPtr, hbqt_ptr( pC ) )


METHOD QTextEdit:setTextColor( pC )
   RETURN Qt_QTextEdit_setTextColor( ::pPtr, hbqt_ptr( pC ) )


METHOD QTextEdit:undo()
   RETURN Qt_QTextEdit_undo( ::pPtr )


METHOD QTextEdit:zoomIn( nRange )
   RETURN Qt_QTextEdit_zoomIn( ::pPtr, nRange )


METHOD QTextEdit:zoomOut( nRange )
   RETURN Qt_QTextEdit_zoomOut( ::pPtr, nRange )


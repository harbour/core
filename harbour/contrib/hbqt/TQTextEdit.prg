/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


CREATE CLASS QTextEdit INHERIT QAbstractScrollArea

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTextEdit_destroy( ::pPtr )

   METHOD  acceptRichText()                    INLINE  Qt_QTextEdit_acceptRichText( ::pPtr )
   METHOD  alignment()                         INLINE  Qt_QTextEdit_alignment( ::pPtr )
   METHOD  anchorAt( pPos )                    INLINE  Qt_QTextEdit_anchorAt( ::pPtr, pPos )
   METHOD  autoFormatting()                    INLINE  Qt_QTextEdit_autoFormatting( ::pPtr )
   METHOD  canPaste()                          INLINE  Qt_QTextEdit_canPaste( ::pPtr )
   METHOD  createStandardContextMenu()         INLINE  Qt_QTextEdit_createStandardContextMenu( ::pPtr )
   METHOD  createStandardContextMenu_1( pPosition )  INLINE  Qt_QTextEdit_createStandardContextMenu_1( ::pPtr, pPosition )
   METHOD  currentCharFormat()                 INLINE  Qt_QTextEdit_currentCharFormat( ::pPtr )
   METHOD  currentFont()                       INLINE  Qt_QTextEdit_currentFont( ::pPtr )
   METHOD  cursorForPosition( pPos )           INLINE  Qt_QTextEdit_cursorForPosition( ::pPtr, pPos )
   METHOD  cursorRect( pCursor )               INLINE  Qt_QTextEdit_cursorRect( ::pPtr, pCursor )
   METHOD  cursorRect_1()                      INLINE  Qt_QTextEdit_cursorRect_1( ::pPtr )
   METHOD  cursorWidth()                       INLINE  Qt_QTextEdit_cursorWidth( ::pPtr )
   METHOD  document()                          INLINE  Qt_QTextEdit_document( ::pPtr )
   METHOD  documentTitle()                     INLINE  Qt_QTextEdit_documentTitle( ::pPtr )
   METHOD  ensureCursorVisible()               INLINE  Qt_QTextEdit_ensureCursorVisible( ::pPtr )
   METHOD  find( cExp, nOptions )              INLINE  Qt_QTextEdit_find( ::pPtr, cExp, nOptions )
   METHOD  fontFamily()                        INLINE  Qt_QTextEdit_fontFamily( ::pPtr )
   METHOD  fontItalic()                        INLINE  Qt_QTextEdit_fontItalic( ::pPtr )
   METHOD  fontPointSize()                     INLINE  Qt_QTextEdit_fontPointSize( ::pPtr )
   METHOD  fontUnderline()                     INLINE  Qt_QTextEdit_fontUnderline( ::pPtr )
   METHOD  fontWeight()                        INLINE  Qt_QTextEdit_fontWeight( ::pPtr )
   METHOD  isReadOnly()                        INLINE  Qt_QTextEdit_isReadOnly( ::pPtr )
   METHOD  isUndoRedoEnabled()                 INLINE  Qt_QTextEdit_isUndoRedoEnabled( ::pPtr )
   METHOD  lineWrapColumnOrWidth()             INLINE  Qt_QTextEdit_lineWrapColumnOrWidth( ::pPtr )
   METHOD  lineWrapMode()                      INLINE  Qt_QTextEdit_lineWrapMode( ::pPtr )
   METHOD  loadResource( nType, pName )        INLINE  Qt_QTextEdit_loadResource( ::pPtr, nType, pName )
   METHOD  mergeCurrentCharFormat( pModifier )  INLINE  Qt_QTextEdit_mergeCurrentCharFormat( ::pPtr, pModifier )
   METHOD  moveCursor( nOperation, nMode )     INLINE  Qt_QTextEdit_moveCursor( ::pPtr, nOperation, nMode )
   METHOD  overwriteMode()                     INLINE  Qt_QTextEdit_overwriteMode( ::pPtr )
   METHOD  print( pPrinter )                   INLINE  Qt_QTextEdit_print( ::pPtr, pPrinter )
   METHOD  setAcceptRichText( lAccept )        INLINE  Qt_QTextEdit_setAcceptRichText( ::pPtr, lAccept )
   METHOD  setAutoFormatting( nFeatures )      INLINE  Qt_QTextEdit_setAutoFormatting( ::pPtr, nFeatures )
   METHOD  setCurrentCharFormat( pFormat )     INLINE  Qt_QTextEdit_setCurrentCharFormat( ::pPtr, pFormat )
   METHOD  setCursorWidth( nWidth )            INLINE  Qt_QTextEdit_setCursorWidth( ::pPtr, nWidth )
   METHOD  setDocument( pDocument )            INLINE  Qt_QTextEdit_setDocument( ::pPtr, pDocument )
   METHOD  setDocumentTitle( cTitle )          INLINE  Qt_QTextEdit_setDocumentTitle( ::pPtr, cTitle )
   METHOD  setLineWrapColumnOrWidth( nW )      INLINE  Qt_QTextEdit_setLineWrapColumnOrWidth( ::pPtr, nW )
   METHOD  setLineWrapMode( nMode )            INLINE  Qt_QTextEdit_setLineWrapMode( ::pPtr, nMode )
   METHOD  setOverwriteMode( lOverwrite )      INLINE  Qt_QTextEdit_setOverwriteMode( ::pPtr, lOverwrite )
   METHOD  setReadOnly( lRo )                  INLINE  Qt_QTextEdit_setReadOnly( ::pPtr, lRo )
   METHOD  setTabChangesFocus( lB )            INLINE  Qt_QTextEdit_setTabChangesFocus( ::pPtr, lB )
   METHOD  setTabStopWidth( nWidth )           INLINE  Qt_QTextEdit_setTabStopWidth( ::pPtr, nWidth )
   METHOD  setTextCursor( pCursor )            INLINE  Qt_QTextEdit_setTextCursor( ::pPtr, pCursor )
   METHOD  setTextInteractionFlags( nFlags )   INLINE  Qt_QTextEdit_setTextInteractionFlags( ::pPtr, nFlags )
   METHOD  setUndoRedoEnabled( lEnable )       INLINE  Qt_QTextEdit_setUndoRedoEnabled( ::pPtr, lEnable )
   METHOD  setWordWrapMode( nPolicy )          INLINE  Qt_QTextEdit_setWordWrapMode( ::pPtr, nPolicy )
   METHOD  tabChangesFocus()                   INLINE  Qt_QTextEdit_tabChangesFocus( ::pPtr )
   METHOD  tabStopWidth()                      INLINE  Qt_QTextEdit_tabStopWidth( ::pPtr )
   METHOD  textBackgroundColor()               INLINE  Qt_QTextEdit_textBackgroundColor( ::pPtr )
   METHOD  textColor()                         INLINE  Qt_QTextEdit_textColor( ::pPtr )
   METHOD  textCursor()                        INLINE  Qt_QTextEdit_textCursor( ::pPtr )
   METHOD  textInteractionFlags()              INLINE  Qt_QTextEdit_textInteractionFlags( ::pPtr )
   METHOD  toHtml()                            INLINE  Qt_QTextEdit_toHtml( ::pPtr )
   METHOD  toPlainText()                       INLINE  Qt_QTextEdit_toPlainText( ::pPtr )
   METHOD  wordWrapMode()                      INLINE  Qt_QTextEdit_wordWrapMode( ::pPtr )
   METHOD  append( cText )                     INLINE  Qt_QTextEdit_append( ::pPtr, cText )
   METHOD  clear()                             INLINE  Qt_QTextEdit_clear( ::pPtr )
   METHOD  copy()                              INLINE  Qt_QTextEdit_copy( ::pPtr )
   METHOD  cut()                               INLINE  Qt_QTextEdit_cut( ::pPtr )
   METHOD  insertHtml( cText )                 INLINE  Qt_QTextEdit_insertHtml( ::pPtr, cText )
   METHOD  insertPlainText( cText )            INLINE  Qt_QTextEdit_insertPlainText( ::pPtr, cText )
   METHOD  paste()                             INLINE  Qt_QTextEdit_paste( ::pPtr )
   METHOD  redo()                              INLINE  Qt_QTextEdit_redo( ::pPtr )
   METHOD  scrollToAnchor( cName )             INLINE  Qt_QTextEdit_scrollToAnchor( ::pPtr, cName )
   METHOD  selectAll()                         INLINE  Qt_QTextEdit_selectAll( ::pPtr )
   METHOD  setAlignment( nA )                  INLINE  Qt_QTextEdit_setAlignment( ::pPtr, nA )
   METHOD  setCurrentFont( pF )                INLINE  Qt_QTextEdit_setCurrentFont( ::pPtr, pF )
   METHOD  setFontFamily( cFontFamily )        INLINE  Qt_QTextEdit_setFontFamily( ::pPtr, cFontFamily )
   METHOD  setFontItalic( lItalic )            INLINE  Qt_QTextEdit_setFontItalic( ::pPtr, lItalic )
   METHOD  setFontPointSize( nS )              INLINE  Qt_QTextEdit_setFontPointSize( ::pPtr, nS )
   METHOD  setFontUnderline( lUnderline )      INLINE  Qt_QTextEdit_setFontUnderline( ::pPtr, lUnderline )
   METHOD  setFontWeight( nWeight )            INLINE  Qt_QTextEdit_setFontWeight( ::pPtr, nWeight )
   METHOD  setHtml( cText )                    INLINE  Qt_QTextEdit_setHtml( ::pPtr, cText )
   METHOD  setPlainText( cText )               INLINE  Qt_QTextEdit_setPlainText( ::pPtr, cText )
   METHOD  setText( cText )                    INLINE  Qt_QTextEdit_setText( ::pPtr, cText )
   METHOD  setTextBackgroundColor( pC )        INLINE  Qt_QTextEdit_setTextBackgroundColor( ::pPtr, pC )
   METHOD  setTextColor( pC )                  INLINE  Qt_QTextEdit_setTextColor( ::pPtr, pC )
   METHOD  undo()                              INLINE  Qt_QTextEdit_undo( ::pPtr )
   METHOD  zoomIn( nRange )                    INLINE  Qt_QTextEdit_zoomIn( ::pPtr, nRange )
   METHOD  zoomOut( nRange )                   INLINE  Qt_QTextEdit_zoomOut( ::pPtr, nRange )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextEdit

   ::pParent := pParent

   ::pPtr := Qt_QTextEdit( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextEdit

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include 'hbclass.ch'


CLASS QTextEdit INHERIT QAbstractScrollArea

   DATA    pPtr

   METHOD  New()

   METHOD  acceptRichText()                    INLINE  Qt_QTextEdit_acceptRichText( ::pPtr )
   METHOD  alignment()                         INLINE  Qt_QTextEdit_alignment( ::pPtr )
   METHOD  anchorAt( aPointPos )               INLINE  Qt_QTextEdit_anchorAt( ::pPtr, aPointPos )
   METHOD  autoFormatting()                    INLINE  Qt_QTextEdit_autoFormatting( ::pPtr )
   METHOD  canPaste()                          INLINE  Qt_QTextEdit_canPaste( ::pPtr )
   METHOD  createStandardContextMenu()         INLINE  Qt_QTextEdit_createStandardContextMenu( ::pPtr )
   METHOD  createStandardContextMenu_1( aPointPosition )  INLINE  Qt_QTextEdit_createStandardContextMenu_1( ::pPtr, aPointPosition )
   METHOD  cursorRect()                        INLINE  Qt_QTextEdit_cursorRect( ::pPtr )
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
   METHOD  moveCursor( nOperation, nMode )     INLINE  Qt_QTextEdit_moveCursor( ::pPtr, nOperation, nMode )
   METHOD  overwriteMode()                     INLINE  Qt_QTextEdit_overwriteMode( ::pPtr )
   METHOD  print( pPrinter )                   INLINE  Qt_QTextEdit_print( ::pPtr, pPrinter )
   METHOD  setAcceptRichText( lAccept )        INLINE  Qt_QTextEdit_setAcceptRichText( ::pPtr, lAccept )
   METHOD  setAutoFormatting( nAutoFormatting )  INLINE  Qt_QTextEdit_setAutoFormatting( ::pPtr, nAutoFormatting )
   METHOD  setCursorWidth( nWidth )            INLINE  Qt_QTextEdit_setCursorWidth( ::pPtr, nWidth )
   METHOD  setDocument( pDocument )            INLINE  Qt_QTextEdit_setDocument( ::pPtr, pDocument )
   METHOD  setDocumentTitle( cTitle )          INLINE  Qt_QTextEdit_setDocumentTitle( ::pPtr, cTitle )
   METHOD  setLineWrapColumnOrWidth( nW )      INLINE  Qt_QTextEdit_setLineWrapColumnOrWidth( ::pPtr, nW )
   METHOD  setLineWrapMode( nLineWrapMode )    INLINE  Qt_QTextEdit_setLineWrapMode( ::pPtr, nLineWrapMode )
   METHOD  setOverwriteMode( lOverwrite )      INLINE  Qt_QTextEdit_setOverwriteMode( ::pPtr, lOverwrite )
   METHOD  setReadOnly( lRo )                  INLINE  Qt_QTextEdit_setReadOnly( ::pPtr, lRo )
   METHOD  setTabChangesFocus( lB )            INLINE  Qt_QTextEdit_setTabChangesFocus( ::pPtr, lB )
   METHOD  setTabStopWidth( nWidth )           INLINE  Qt_QTextEdit_setTabStopWidth( ::pPtr, nWidth )
   METHOD  setUndoRedoEnabled( lEnable )       INLINE  Qt_QTextEdit_setUndoRedoEnabled( ::pPtr, lEnable )
   METHOD  setWordWrapMode( nPolicy )          INLINE  Qt_QTextEdit_setWordWrapMode( ::pPtr, nPolicy )
   METHOD  tabChangesFocus()                   INLINE  Qt_QTextEdit_tabChangesFocus( ::pPtr )
   METHOD  tabStopWidth()                      INLINE  Qt_QTextEdit_tabStopWidth( ::pPtr )
   METHOD  textInteractionFlags()              INLINE  Qt_QTextEdit_textInteractionFlags( ::pPtr )
   METHOD  toHtml()                            INLINE  Qt_QTextEdit_toHtml( ::pPtr )
   METHOD  toPlainText()                       INLINE  Qt_QTextEdit_toPlainText( ::pPtr )
   METHOD  wordWrapMode()                      INLINE  Qt_QTextEdit_wordWrapMode( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextEdit

   ::pPtr := Qt_QTextEdit( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


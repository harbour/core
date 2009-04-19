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


#include "hbclass.ch"


CREATE CLASS QTextCursor

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  anchor()                            INLINE  Qt_QTextCursor_anchor( ::pPtr )
   METHOD  atBlockEnd()                        INLINE  Qt_QTextCursor_atBlockEnd( ::pPtr )
   METHOD  atBlockStart()                      INLINE  Qt_QTextCursor_atBlockStart( ::pPtr )
   METHOD  atEnd()                             INLINE  Qt_QTextCursor_atEnd( ::pPtr )
   METHOD  atStart()                           INLINE  Qt_QTextCursor_atStart( ::pPtr )
   METHOD  beginEditBlock()                    INLINE  Qt_QTextCursor_beginEditBlock( ::pPtr )
   METHOD  blockCharFormat()                   INLINE  Qt_QTextCursor_blockCharFormat( ::pPtr )
   METHOD  blockFormat()                       INLINE  Qt_QTextCursor_blockFormat( ::pPtr )
   METHOD  blockNumber()                       INLINE  Qt_QTextCursor_blockNumber( ::pPtr )
   METHOD  charFormat()                        INLINE  Qt_QTextCursor_charFormat( ::pPtr )
   METHOD  clearSelection()                    INLINE  Qt_QTextCursor_clearSelection( ::pPtr )
   METHOD  columnNumber()                      INLINE  Qt_QTextCursor_columnNumber( ::pPtr )
   METHOD  createList( pFormat )               INLINE  Qt_QTextCursor_createList( ::pPtr, pFormat )
   METHOD  createList_1( nStyle )              INLINE  Qt_QTextCursor_createList_1( ::pPtr, nStyle )
   METHOD  currentFrame()                      INLINE  Qt_QTextCursor_currentFrame( ::pPtr )
   METHOD  currentList()                       INLINE  Qt_QTextCursor_currentList( ::pPtr )
   METHOD  currentTable()                      INLINE  Qt_QTextCursor_currentTable( ::pPtr )
   METHOD  deleteChar()                        INLINE  Qt_QTextCursor_deleteChar( ::pPtr )
   METHOD  deletePreviousChar()                INLINE  Qt_QTextCursor_deletePreviousChar( ::pPtr )
   METHOD  document()                          INLINE  Qt_QTextCursor_document( ::pPtr )
   METHOD  endEditBlock()                      INLINE  Qt_QTextCursor_endEditBlock( ::pPtr )
   METHOD  hasComplexSelection()               INLINE  Qt_QTextCursor_hasComplexSelection( ::pPtr )
   METHOD  hasSelection()                      INLINE  Qt_QTextCursor_hasSelection( ::pPtr )
   METHOD  insertBlock()                       INLINE  Qt_QTextCursor_insertBlock( ::pPtr )
   METHOD  insertBlock_1( pFormat )            INLINE  Qt_QTextCursor_insertBlock_1( ::pPtr, pFormat )
   METHOD  insertBlock_2( pFormat, pCharFormat )  INLINE  Qt_QTextCursor_insertBlock_2( ::pPtr, pFormat, pCharFormat )
   METHOD  insertFragment( pFragment )         INLINE  Qt_QTextCursor_insertFragment( ::pPtr, pFragment )
   METHOD  insertFrame( pFormat )              INLINE  Qt_QTextCursor_insertFrame( ::pPtr, pFormat )
   METHOD  insertHtml( cHtml )                 INLINE  Qt_QTextCursor_insertHtml( ::pPtr, cHtml )
   METHOD  insertImage( pFormat )              INLINE  Qt_QTextCursor_insertImage( ::pPtr, pFormat )
   METHOD  insertImage_1( pFormat, nAlignment )  INLINE  Qt_QTextCursor_insertImage_1( ::pPtr, pFormat, nAlignment )
   METHOD  insertImage_2( cName )              INLINE  Qt_QTextCursor_insertImage_2( ::pPtr, cName )
   METHOD  insertImage_3( pImage, cName )      INLINE  Qt_QTextCursor_insertImage_3( ::pPtr, pImage, cName )
   METHOD  insertList( pFormat )               INLINE  Qt_QTextCursor_insertList( ::pPtr, pFormat )
   METHOD  insertList_1( nStyle )              INLINE  Qt_QTextCursor_insertList_1( ::pPtr, nStyle )
   METHOD  insertTable( nRows, nColumns, pFormat )  INLINE  Qt_QTextCursor_insertTable( ::pPtr, nRows, nColumns, pFormat )
   METHOD  insertTable_1( nRows, nColumns )    INLINE  Qt_QTextCursor_insertTable_1( ::pPtr, nRows, nColumns )
   METHOD  insertText( cText )                 INLINE  Qt_QTextCursor_insertText( ::pPtr, cText )
   METHOD  insertText_1( cText, pFormat )      INLINE  Qt_QTextCursor_insertText_1( ::pPtr, cText, pFormat )
   METHOD  isCopyOf( pOther )                  INLINE  Qt_QTextCursor_isCopyOf( ::pPtr, pOther )
   METHOD  isNull()                            INLINE  Qt_QTextCursor_isNull( ::pPtr )
   METHOD  joinPreviousEditBlock()             INLINE  Qt_QTextCursor_joinPreviousEditBlock( ::pPtr )
   METHOD  mergeBlockCharFormat( pModifier )   INLINE  Qt_QTextCursor_mergeBlockCharFormat( ::pPtr, pModifier )
   METHOD  mergeBlockFormat( pModifier )       INLINE  Qt_QTextCursor_mergeBlockFormat( ::pPtr, pModifier )
   METHOD  mergeCharFormat( pModifier )        INLINE  Qt_QTextCursor_mergeCharFormat( ::pPtr, pModifier )
   METHOD  movePosition( nOperation, nMode, nN )  INLINE  Qt_QTextCursor_movePosition( ::pPtr, nOperation, nMode, nN )
   METHOD  position()                          INLINE  Qt_QTextCursor_position( ::pPtr )
   METHOD  removeSelectedText()                INLINE  Qt_QTextCursor_removeSelectedText( ::pPtr )
   METHOD  select( nSelection )                INLINE  Qt_QTextCursor_select( ::pPtr, nSelection )
   METHOD  selectedTableCells( nFirstRow, nNumRows, nFirstColumn, nNumColumns )  INLINE  Qt_QTextCursor_selectedTableCells( ::pPtr, nFirstRow, nNumRows, nFirstColumn, nNumColumns )
   METHOD  selectedText()                      INLINE  Qt_QTextCursor_selectedText( ::pPtr )
   METHOD  selection()                         INLINE  Qt_QTextCursor_selection( ::pPtr )
   METHOD  selectionEnd()                      INLINE  Qt_QTextCursor_selectionEnd( ::pPtr )
   METHOD  selectionStart()                    INLINE  Qt_QTextCursor_selectionStart( ::pPtr )
   METHOD  setBlockCharFormat( pFormat )       INLINE  Qt_QTextCursor_setBlockCharFormat( ::pPtr, pFormat )
   METHOD  setBlockFormat( pFormat )           INLINE  Qt_QTextCursor_setBlockFormat( ::pPtr, pFormat )
   METHOD  setCharFormat( pFormat )            INLINE  Qt_QTextCursor_setCharFormat( ::pPtr, pFormat )
   METHOD  setPosition( nPos, nM )             INLINE  Qt_QTextCursor_setPosition( ::pPtr, nPos, nM )
   METHOD  setVisualNavigation( lB )           INLINE  Qt_QTextCursor_setVisualNavigation( ::pPtr, lB )
   METHOD  visualNavigation()                  INLINE  Qt_QTextCursor_visualNavigation( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextCursor

   ::pParent := pParent

   ::pPtr := Qt_QTextCursor( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


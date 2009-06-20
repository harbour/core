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


CREATE CLASS QLineEdit INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  alignment()                         INLINE  Qt_QLineEdit_alignment( ::pPtr )
   METHOD  backspace()                         INLINE  Qt_QLineEdit_backspace( ::pPtr )
   METHOD  completer()                         INLINE  Qt_QLineEdit_completer( ::pPtr )
   METHOD  createStandardContextMenu()         INLINE  Qt_QLineEdit_createStandardContextMenu( ::pPtr )
   METHOD  cursorBackward( lMark, nSteps )     INLINE  Qt_QLineEdit_cursorBackward( ::pPtr, lMark, nSteps )
   METHOD  cursorForward( lMark, nSteps )      INLINE  Qt_QLineEdit_cursorForward( ::pPtr, lMark, nSteps )
   METHOD  cursorPosition()                    INLINE  Qt_QLineEdit_cursorPosition( ::pPtr )
   METHOD  cursorPositionAt( pPos )            INLINE  Qt_QLineEdit_cursorPositionAt( ::pPtr, pPos )
   METHOD  cursorWordBackward( lMark )         INLINE  Qt_QLineEdit_cursorWordBackward( ::pPtr, lMark )
   METHOD  cursorWordForward( lMark )          INLINE  Qt_QLineEdit_cursorWordForward( ::pPtr, lMark )
   METHOD  del()                               INLINE  Qt_QLineEdit_del( ::pPtr )
   METHOD  deselect()                          INLINE  Qt_QLineEdit_deselect( ::pPtr )
   METHOD  displayText()                       INLINE  Qt_QLineEdit_displayText( ::pPtr )
   METHOD  dragEnabled()                       INLINE  Qt_QLineEdit_dragEnabled( ::pPtr )
   METHOD  echoMode()                          INLINE  Qt_QLineEdit_echoMode( ::pPtr )
   METHOD  end( lMark )                        INLINE  Qt_QLineEdit_end( ::pPtr, lMark )
   METHOD  getTextMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QLineEdit_getTextMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  hasAcceptableInput()                INLINE  Qt_QLineEdit_hasAcceptableInput( ::pPtr )
   METHOD  hasFrame()                          INLINE  Qt_QLineEdit_hasFrame( ::pPtr )
   METHOD  hasSelectedText()                   INLINE  Qt_QLineEdit_hasSelectedText( ::pPtr )
   METHOD  home( lMark )                       INLINE  Qt_QLineEdit_home( ::pPtr, lMark )
   METHOD  inputMask()                         INLINE  Qt_QLineEdit_inputMask( ::pPtr )
   METHOD  insert( cNewText )                  INLINE  Qt_QLineEdit_insert( ::pPtr, cNewText )
   METHOD  isModified()                        INLINE  Qt_QLineEdit_isModified( ::pPtr )
   METHOD  isReadOnly()                        INLINE  Qt_QLineEdit_isReadOnly( ::pPtr )
   METHOD  isRedoAvailable()                   INLINE  Qt_QLineEdit_isRedoAvailable( ::pPtr )
   METHOD  isUndoAvailable()                   INLINE  Qt_QLineEdit_isUndoAvailable( ::pPtr )
   METHOD  maxLength()                         INLINE  Qt_QLineEdit_maxLength( ::pPtr )
   METHOD  minimumSizeHint()                   INLINE  Qt_QLineEdit_minimumSizeHint( ::pPtr )
   METHOD  selectedText()                      INLINE  Qt_QLineEdit_selectedText( ::pPtr )
   METHOD  selectionStart()                    INLINE  Qt_QLineEdit_selectionStart( ::pPtr )
   METHOD  setAlignment( nFlag )               INLINE  Qt_QLineEdit_setAlignment( ::pPtr, nFlag )
   METHOD  setCompleter( pC )                  INLINE  Qt_QLineEdit_setCompleter( ::pPtr, pC )
   METHOD  setCursorPosition( nInt )           INLINE  Qt_QLineEdit_setCursorPosition( ::pPtr, nInt )
   METHOD  setDragEnabled( lB )                INLINE  Qt_QLineEdit_setDragEnabled( ::pPtr, lB )
   METHOD  setEchoMode( nEchoMode )            INLINE  Qt_QLineEdit_setEchoMode( ::pPtr, nEchoMode )
   METHOD  setFrame( lBool )                   INLINE  Qt_QLineEdit_setFrame( ::pPtr, lBool )
   METHOD  setInputMask( cInputMask )          INLINE  Qt_QLineEdit_setInputMask( ::pPtr, cInputMask )
   METHOD  setMaxLength( nInt )                INLINE  Qt_QLineEdit_setMaxLength( ::pPtr, nInt )
   METHOD  setModified( lBool )                INLINE  Qt_QLineEdit_setModified( ::pPtr, lBool )
   METHOD  setReadOnly( lBool )                INLINE  Qt_QLineEdit_setReadOnly( ::pPtr, lBool )
   METHOD  setSelection( nStart, nLength )     INLINE  Qt_QLineEdit_setSelection( ::pPtr, nStart, nLength )
   METHOD  setTextMargins( nLeft, nTop, nRight, nBottom )  INLINE  Qt_QLineEdit_setTextMargins( ::pPtr, nLeft, nTop, nRight, nBottom )
   METHOD  setValidator( pV )                  INLINE  Qt_QLineEdit_setValidator( ::pPtr, pV )
   METHOD  sizeHint()                          INLINE  Qt_QLineEdit_sizeHint( ::pPtr )
   METHOD  text()                              INLINE  Qt_QLineEdit_text( ::pPtr )
   METHOD  validator()                         INLINE  Qt_QLineEdit_validator( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QLineEdit_clear( ::pPtr )
   METHOD  copy()                              INLINE  Qt_QLineEdit_copy( ::pPtr )
   METHOD  cut()                               INLINE  Qt_QLineEdit_cut( ::pPtr )
   METHOD  paste()                             INLINE  Qt_QLineEdit_paste( ::pPtr )
   METHOD  redo()                              INLINE  Qt_QLineEdit_redo( ::pPtr )
   METHOD  selectAll()                         INLINE  Qt_QLineEdit_selectAll( ::pPtr )
   METHOD  setText( cQString )                 INLINE  Qt_QLineEdit_setText( ::pPtr, cQString )
   METHOD  undo()                              INLINE  Qt_QLineEdit_undo( ::pPtr )
   METHOD  event( pEvent )                     INLINE  Qt_QLineEdit_event( ::pPtr, pEvent )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QLineEdit

   ::pParent := pParent

   ::pPtr := Qt_QLineEdit( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


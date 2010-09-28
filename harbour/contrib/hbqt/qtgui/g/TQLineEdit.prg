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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QLineEdit( ... )
   RETURN HB_QLineEdit():new( ... )


CREATE CLASS QLineEdit INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QLineEdit

   METHOD  new( ... )

   METHOD  alignment()
   METHOD  backspace()
   METHOD  completer()
   METHOD  createStandardContextMenu()
   METHOD  cursorBackward( lMark, nSteps )
   METHOD  cursorForward( lMark, nSteps )
   METHOD  cursorPosition()
   METHOD  cursorPositionAt( pPos )
   METHOD  cursorWordBackward( lMark )
   METHOD  cursorWordForward( lMark )
   METHOD  del()
   METHOD  deselect()
   METHOD  displayText()
   METHOD  dragEnabled()
   METHOD  echoMode()
   METHOD  end( lMark )
   METHOD  getTextMargins( nLeft, nTop, nRight, nBottom )
   METHOD  hasAcceptableInput()
   METHOD  hasFrame()
   METHOD  hasSelectedText()
   METHOD  home( lMark )
   METHOD  inputMask()
   METHOD  insert( cNewText )
   METHOD  isModified()
   METHOD  isReadOnly()
   METHOD  isRedoAvailable()
   METHOD  isUndoAvailable()
   METHOD  maxLength()
   METHOD  minimumSizeHint()
   METHOD  selectedText()
   METHOD  selectionStart()
   METHOD  setAlignment( nFlag )
   METHOD  setCompleter( pC )
   METHOD  setCursorPosition( nInt )
   METHOD  setDragEnabled( lB )
   METHOD  setEchoMode( nEchoMode )
   METHOD  setFrame( lBool )
   METHOD  setInputMask( cInputMask )
   METHOD  setMaxLength( nInt )
   METHOD  setModified( lBool )
   METHOD  setReadOnly( lBool )
   METHOD  setSelection( nStart, nLength )
   METHOD  setTextMargins( nLeft, nTop, nRight, nBottom )
   METHOD  setValidator( pV )
   METHOD  sizeHint()
   METHOD  text()
   METHOD  validator()
   METHOD  clear()
   METHOD  copy()
   METHOD  cut()
   METHOD  paste()
   METHOD  redo()
   METHOD  selectAll()
   METHOD  setText( cQString )
   METHOD  undo()

   ENDCLASS


METHOD QLineEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLineEdit( ... )
   RETURN Self


METHOD QLineEdit:alignment()
   RETURN Qt_QLineEdit_alignment( ::pPtr )


METHOD QLineEdit:backspace()
   RETURN Qt_QLineEdit_backspace( ::pPtr )


METHOD QLineEdit:completer()
   RETURN HB_QCompleter():from( Qt_QLineEdit_completer( ::pPtr ) )


METHOD QLineEdit:createStandardContextMenu()
   RETURN HB_QMenu():from( Qt_QLineEdit_createStandardContextMenu( ::pPtr ) )


METHOD QLineEdit:cursorBackward( lMark, nSteps )
   RETURN Qt_QLineEdit_cursorBackward( ::pPtr, lMark, nSteps )


METHOD QLineEdit:cursorForward( lMark, nSteps )
   RETURN Qt_QLineEdit_cursorForward( ::pPtr, lMark, nSteps )


METHOD QLineEdit:cursorPosition()
   RETURN Qt_QLineEdit_cursorPosition( ::pPtr )


METHOD QLineEdit:cursorPositionAt( pPos )
   RETURN Qt_QLineEdit_cursorPositionAt( ::pPtr, hbqt_ptr( pPos ) )


METHOD QLineEdit:cursorWordBackward( lMark )
   RETURN Qt_QLineEdit_cursorWordBackward( ::pPtr, lMark )


METHOD QLineEdit:cursorWordForward( lMark )
   RETURN Qt_QLineEdit_cursorWordForward( ::pPtr, lMark )


METHOD QLineEdit:del()
   RETURN Qt_QLineEdit_del( ::pPtr )


METHOD QLineEdit:deselect()
   RETURN Qt_QLineEdit_deselect( ::pPtr )


METHOD QLineEdit:displayText()
   RETURN Qt_QLineEdit_displayText( ::pPtr )


METHOD QLineEdit:dragEnabled()
   RETURN Qt_QLineEdit_dragEnabled( ::pPtr )


METHOD QLineEdit:echoMode()
   RETURN Qt_QLineEdit_echoMode( ::pPtr )


METHOD QLineEdit:end( lMark )
   RETURN Qt_QLineEdit_end( ::pPtr, lMark )


METHOD QLineEdit:getTextMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QLineEdit_getTextMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QLineEdit:hasAcceptableInput()
   RETURN Qt_QLineEdit_hasAcceptableInput( ::pPtr )


METHOD QLineEdit:hasFrame()
   RETURN Qt_QLineEdit_hasFrame( ::pPtr )


METHOD QLineEdit:hasSelectedText()
   RETURN Qt_QLineEdit_hasSelectedText( ::pPtr )


METHOD QLineEdit:home( lMark )
   RETURN Qt_QLineEdit_home( ::pPtr, lMark )


METHOD QLineEdit:inputMask()
   RETURN Qt_QLineEdit_inputMask( ::pPtr )


METHOD QLineEdit:insert( cNewText )
   RETURN Qt_QLineEdit_insert( ::pPtr, cNewText )


METHOD QLineEdit:isModified()
   RETURN Qt_QLineEdit_isModified( ::pPtr )


METHOD QLineEdit:isReadOnly()
   RETURN Qt_QLineEdit_isReadOnly( ::pPtr )


METHOD QLineEdit:isRedoAvailable()
   RETURN Qt_QLineEdit_isRedoAvailable( ::pPtr )


METHOD QLineEdit:isUndoAvailable()
   RETURN Qt_QLineEdit_isUndoAvailable( ::pPtr )


METHOD QLineEdit:maxLength()
   RETURN Qt_QLineEdit_maxLength( ::pPtr )


METHOD QLineEdit:minimumSizeHint()
   RETURN HB_QSize():from( Qt_QLineEdit_minimumSizeHint( ::pPtr ) )


METHOD QLineEdit:selectedText()
   RETURN Qt_QLineEdit_selectedText( ::pPtr )


METHOD QLineEdit:selectionStart()
   RETURN Qt_QLineEdit_selectionStart( ::pPtr )


METHOD QLineEdit:setAlignment( nFlag )
   RETURN Qt_QLineEdit_setAlignment( ::pPtr, nFlag )


METHOD QLineEdit:setCompleter( pC )
   RETURN Qt_QLineEdit_setCompleter( ::pPtr, hbqt_ptr( pC ) )


METHOD QLineEdit:setCursorPosition( nInt )
   RETURN Qt_QLineEdit_setCursorPosition( ::pPtr, nInt )


METHOD QLineEdit:setDragEnabled( lB )
   RETURN Qt_QLineEdit_setDragEnabled( ::pPtr, lB )


METHOD QLineEdit:setEchoMode( nEchoMode )
   RETURN Qt_QLineEdit_setEchoMode( ::pPtr, nEchoMode )


METHOD QLineEdit:setFrame( lBool )
   RETURN Qt_QLineEdit_setFrame( ::pPtr, lBool )


METHOD QLineEdit:setInputMask( cInputMask )
   RETURN Qt_QLineEdit_setInputMask( ::pPtr, cInputMask )


METHOD QLineEdit:setMaxLength( nInt )
   RETURN Qt_QLineEdit_setMaxLength( ::pPtr, nInt )


METHOD QLineEdit:setModified( lBool )
   RETURN Qt_QLineEdit_setModified( ::pPtr, lBool )


METHOD QLineEdit:setReadOnly( lBool )
   RETURN Qt_QLineEdit_setReadOnly( ::pPtr, lBool )


METHOD QLineEdit:setSelection( nStart, nLength )
   RETURN Qt_QLineEdit_setSelection( ::pPtr, nStart, nLength )


METHOD QLineEdit:setTextMargins( nLeft, nTop, nRight, nBottom )
   RETURN Qt_QLineEdit_setTextMargins( ::pPtr, nLeft, nTop, nRight, nBottom )


METHOD QLineEdit:setValidator( pV )
   RETURN Qt_QLineEdit_setValidator( ::pPtr, hbqt_ptr( pV ) )


METHOD QLineEdit:sizeHint()
   RETURN HB_QSize():from( Qt_QLineEdit_sizeHint( ::pPtr ) )


METHOD QLineEdit:text()
   RETURN Qt_QLineEdit_text( ::pPtr )


METHOD QLineEdit:validator()
   RETURN HB_QValidator():from( Qt_QLineEdit_validator( ::pPtr ) )


METHOD QLineEdit:clear()
   RETURN Qt_QLineEdit_clear( ::pPtr )


METHOD QLineEdit:copy()
   RETURN Qt_QLineEdit_copy( ::pPtr )


METHOD QLineEdit:cut()
   RETURN Qt_QLineEdit_cut( ::pPtr )


METHOD QLineEdit:paste()
   RETURN Qt_QLineEdit_paste( ::pPtr )


METHOD QLineEdit:redo()
   RETURN Qt_QLineEdit_redo( ::pPtr )


METHOD QLineEdit:selectAll()
   RETURN Qt_QLineEdit_selectAll( ::pPtr )


METHOD QLineEdit:setText( cQString )
   RETURN Qt_QLineEdit_setText( ::pPtr, cQString )


METHOD QLineEdit:undo()
   RETURN Qt_QLineEdit_undo( ::pPtr )


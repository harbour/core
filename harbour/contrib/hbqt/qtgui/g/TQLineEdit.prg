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

   METHOD  alignment                     // (  )                                               -> nQt_Alignment
   METHOD  backspace                     // (  )                                               -> NIL
   METHOD  completer                     // (  )                                               -> oQCompleter
   METHOD  createStandardContextMenu     // (  )                                               -> oQMenu
   METHOD  cursorBackward                // ( lMark, nSteps )                                  -> NIL
   METHOD  cursorForward                 // ( lMark, nSteps )                                  -> NIL
   METHOD  cursorPosition                // (  )                                               -> nInt
   METHOD  cursorPositionAt              // ( oQPoint )                                        -> nInt
   METHOD  cursorWordBackward            // ( lMark )                                          -> NIL
   METHOD  cursorWordForward             // ( lMark )                                          -> NIL
   METHOD  del                           // (  )                                               -> NIL
   METHOD  deselect                      // (  )                                               -> NIL
   METHOD  displayText                   // (  )                                               -> cQString
   METHOD  dragEnabled                   // (  )                                               -> lBool
   METHOD  echoMode                      // (  )                                               -> nEchoMode
   METHOD  end                           // ( lMark )                                          -> NIL
   METHOD  getTextMargins                // ( @nLeft, @nTop, @nRight, @nBottom )               -> NIL
   METHOD  hasAcceptableInput            // (  )                                               -> lBool
   METHOD  hasFrame                      // (  )                                               -> lBool
   METHOD  hasSelectedText               // (  )                                               -> lBool
   METHOD  home                          // ( lMark )                                          -> NIL
   METHOD  inputMask                     // (  )                                               -> cQString
   METHOD  insert                        // ( cNewText )                                       -> NIL
   METHOD  isModified                    // (  )                                               -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  isRedoAvailable               // (  )                                               -> lBool
   METHOD  isUndoAvailable               // (  )                                               -> lBool
   METHOD  maxLength                     // (  )                                               -> nInt
   METHOD  minimumSizeHint               // (  )                                               -> oQSize
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  selectionStart                // (  )                                               -> nInt
   METHOD  setAlignment                  // ( nFlag )                                          -> NIL
   METHOD  setCompleter                  // ( oQCompleter )                                    -> NIL
   METHOD  setCursorPosition             // ( nInt )                                           -> NIL
   METHOD  setDragEnabled                // ( lB )                                             -> NIL
   METHOD  setEchoMode                   // ( nEchoMode )                                      -> NIL
   METHOD  setFrame                      // ( lBool )                                          -> NIL
   METHOD  setInputMask                  // ( cInputMask )                                     -> NIL
   METHOD  setMaxLength                  // ( nInt )                                           -> NIL
   METHOD  setModified                   // ( lBool )                                          -> NIL
   METHOD  setReadOnly                   // ( lBool )                                          -> NIL
   METHOD  setSelection                  // ( nStart, nLength )                                -> NIL
   METHOD  setTextMargins                // ( nLeft, nTop, nRight, nBottom )                   -> NIL
   METHOD  setValidator                  // ( oQValidator )                                    -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  text                          // (  )                                               -> cQString
   METHOD  validator                     // (  )                                               -> oQValidator
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  copy                          // (  )                                               -> NIL
   METHOD  cut                           // (  )                                               -> NIL
   METHOD  paste                         // (  )                                               -> NIL
   METHOD  redo                          // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  setText                       // ( cQString )                                       -> NIL
   METHOD  undo                          // (  )                                               -> NIL

   ENDCLASS


METHOD QLineEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLineEdit( ... )
   RETURN Self


METHOD QLineEdit:alignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_alignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:backspace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_backspace( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:completer( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QCompleter():from( Qt_QLineEdit_completer( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:createStandardContextMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QMenu():from( Qt_QLineEdit_createStandardContextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorBackward( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_cursorBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorForward( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_cursorForward( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorForward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_cursorPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorPositionAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorPositionAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorWordBackward( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorWordBackward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cursorWordForward( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_cursorWordForward( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:del( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_del( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:deselect( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_deselect( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:displayText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_displayText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:dragEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_dragEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:echoMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_echoMode( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:end( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_end( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:getTextMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLineEdit_getTextMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:hasAcceptableInput( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasAcceptableInput( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:hasFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasFrame( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:hasSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_hasSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:home( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_home( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:inputMask( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_inputMask( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:insert( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_insert( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:isModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isModified( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:isRedoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isRedoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:isUndoAvailable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_isUndoAvailable( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:maxLength( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_maxLength( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:minimumSizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QSize():from( Qt_QLineEdit_minimumSizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:selectionStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectionStart( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setCompleter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setCompleter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setCursorPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setCursorPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setDragEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setDragEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setEchoMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setEchoMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setFrame( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setInputMask( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setInputMask( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setMaxLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setMaxLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setModified( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setModified( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setSelection( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineEdit_setSelection( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setTextMargins( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLineEdit_setTextMargins( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setValidator( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setValidator( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QSize():from( Qt_QLineEdit_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_text( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:validator( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QValidator():from( Qt_QLineEdit_validator( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:copy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_copy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:cut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_cut( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:paste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_paste( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:redo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_redo( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QLineEdit_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineEdit:undo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineEdit_undo( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


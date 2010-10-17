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


FUNCTION QPlainTextEdit( ... )
   RETURN HB_QPlainTextEdit():new( ... )

FUNCTION QPlainTextEditFrom( ... )
   RETURN HB_QPlainTextEdit():from( ... )

FUNCTION QPlainTextEditFromPointer( ... )
   RETURN HB_QPlainTextEdit():fromPointer( ... )


CREATE CLASS QPlainTextEdit INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QPlainTextEdit

   METHOD  new( ... )

   METHOD  backgroundVisible             // (  )                                               -> lBool
   METHOD  blockCount                    // (  )                                               -> nInt
   METHOD  canPaste                      // (  )                                               -> lBool
   METHOD  centerOnScroll                // (  )                                               -> lBool
   METHOD  createStandardContextMenu     // (  )                                               -> oQMenu
   METHOD  currentCharFormat             // (  )                                               -> oQTextCharFormat
   METHOD  cursorForPosition             // ( oQPoint )                                        -> oQTextCursor
   METHOD  cursorRect                    // ( oQTextCursor )                                   -> oQRect
                                         // (  )                                               -> oQRect
   METHOD  cursorWidth                   // (  )                                               -> nInt
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  documentTitle                 // (  )                                               -> cQString
   METHOD  ensureCursorVisible           // (  )                                               -> NIL
   METHOD  find                          // ( cExp, nOptions )                                 -> lBool
   METHOD  isReadOnly                    // (  )                                               -> lBool
   METHOD  isUndoRedoEnabled             // (  )                                               -> lBool
   METHOD  lineWrapMode                  // (  )                                               -> nLineWrapMode
   METHOD  loadResource                  // ( nType, oQUrl )                                   -> oQVariant
   METHOD  maximumBlockCount             // (  )                                               -> nInt
   METHOD  mergeCurrentCharFormat        // ( oQTextCharFormat )                               -> NIL
   METHOD  moveCursor                    // ( nOperation, nMode )                              -> NIL
   METHOD  overwriteMode                 // (  )                                               -> lBool
   METHOD  print                         // ( oQPrinter )                                      -> NIL
   METHOD  setBackgroundVisible          // ( lVisible )                                       -> NIL
   METHOD  setCenterOnScroll             // ( lEnabled )                                       -> NIL
   METHOD  setCurrentCharFormat          // ( oQTextCharFormat )                               -> NIL
   METHOD  setCursorWidth                // ( nWidth )                                         -> NIL
   METHOD  setDocument                   // ( oQTextDocument )                                 -> NIL
   METHOD  setDocumentTitle              // ( cTitle )                                         -> NIL
   METHOD  setLineWrapMode               // ( nMode )                                          -> NIL
   METHOD  setMaximumBlockCount          // ( nMaximum )                                       -> NIL
   METHOD  setOverwriteMode              // ( lOverwrite )                                     -> NIL
   METHOD  setReadOnly                   // ( lRo )                                            -> NIL
   METHOD  setTabChangesFocus            // ( lB )                                             -> NIL
   METHOD  setTabStopWidth               // ( nWidth )                                         -> NIL
   METHOD  setTextCursor                 // ( oQTextCursor )                                   -> NIL
   METHOD  setTextInteractionFlags       // ( nFlags )                                         -> NIL
   METHOD  setUndoRedoEnabled            // ( lEnable )                                        -> NIL
   METHOD  setWordWrapMode               // ( nPolicy )                                        -> NIL
   METHOD  tabChangesFocus               // (  )                                               -> lBool
   METHOD  tabStopWidth                  // (  )                                               -> nInt
   METHOD  textCursor                    // (  )                                               -> oQTextCursor
   METHOD  textInteractionFlags          // (  )                                               -> nQt_TextInteractionFlags
   METHOD  toPlainText                   // (  )                                               -> cQString
   METHOD  wordWrapMode                  // (  )                                               -> nQTextOption_WrapMode
   METHOD  appendHtml                    // ( cHtml )                                          -> NIL
   METHOD  appendPlainText               // ( cText )                                          -> NIL
   METHOD  centerCursor                  // (  )                                               -> NIL
   METHOD  clear                         // (  )                                               -> NIL
   METHOD  copy                          // (  )                                               -> NIL
   METHOD  cut                           // (  )                                               -> NIL
   METHOD  insertPlainText               // ( cText )                                          -> NIL
   METHOD  paste                         // (  )                                               -> NIL
   METHOD  redo                          // (  )                                               -> NIL
   METHOD  selectAll                     // (  )                                               -> NIL
   METHOD  setPlainText                  // ( cText )                                          -> NIL
   METHOD  undo                          // (  )                                               -> NIL

   ENDCLASS


METHOD QPlainTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPlainTextEdit( ... )
   RETURN Self


METHOD QPlainTextEdit:backgroundVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_backgroundVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:blockCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_blockCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:canPaste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_canPaste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:centerOnScroll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_centerOnScroll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:createStandardContextMenu( ... )
   SWITCH PCount()
   CASE 0
      RETURN QMenuFromPointer( Qt_QPlainTextEdit_createStandardContextMenu( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:currentCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QPlainTextEdit_currentCharFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:cursorForPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextCursorFromPointer( Qt_QPlainTextEdit_cursorForPosition( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:cursorRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QPlainTextEdit_cursorRect( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QRectFromPointer( Qt_QPlainTextEdit_cursorRect_1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:cursorWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_cursorWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QPlainTextEdit_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:documentTitle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_documentTitle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:ensureCursorVisible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_ensureCursorVisible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:find( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPlainTextEdit_find( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_find( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:isReadOnly( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_isReadOnly( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:isUndoRedoEnabled( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_isUndoRedoEnabled( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:lineWrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_lineWrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:loadResource( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN QVariantFromPointer( Qt_QPlainTextEdit_loadResource( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:maximumBlockCount( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_maximumBlockCount( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:mergeCurrentCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_mergeCurrentCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:moveCursor( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPlainTextEdit_moveCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_moveCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:overwriteMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_overwriteMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:print( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_print( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setBackgroundVisible( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setBackgroundVisible( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setCenterOnScroll( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setCenterOnScroll( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setCurrentCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setCurrentCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setCursorWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setCursorWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setDocument( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setDocument( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setDocumentTitle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setDocumentTitle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setLineWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setLineWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setMaximumBlockCount( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setMaximumBlockCount( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setOverwriteMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setOverwriteMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setReadOnly( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setReadOnly( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setTabChangesFocus( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setTabChangesFocus( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setTabStopWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setTabStopWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setTextCursor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setTextCursor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setTextInteractionFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setTextInteractionFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setUndoRedoEnabled( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setUndoRedoEnabled( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setWordWrapMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setWordWrapMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:tabChangesFocus( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_tabChangesFocus( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:tabStopWidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_tabStopWidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:textCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCursorFromPointer( Qt_QPlainTextEdit_textCursor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:textInteractionFlags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_textInteractionFlags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:toPlainText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_toPlainText( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:wordWrapMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_wordWrapMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:appendHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_appendHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:appendPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_appendPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:centerCursor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_centerCursor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:copy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_copy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:cut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_cut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:insertPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_insertPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:paste( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_paste( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:redo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_redo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:selectAll( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_selectAll( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:setPlainText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QPlainTextEdit_setPlainText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPlainTextEdit:undo( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPlainTextEdit_undo( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


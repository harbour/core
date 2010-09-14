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


FUNCTION QPlainTextEdit( ... )
   RETURN HB_QPlainTextEdit():new( ... )


CREATE CLASS QPlainTextEdit INHERIT HbQtObjectHandler, HB_QAbstractScrollArea FUNCTION HB_QPlainTextEdit

   METHOD  new( ... )

   METHOD  backgroundVisible()
   METHOD  blockCount()
   METHOD  canPaste()
   METHOD  centerOnScroll()
   METHOD  createStandardContextMenu()
   METHOD  currentCharFormat()
   METHOD  cursorForPosition( pPos )
   METHOD  cursorRect( pCursor )
   METHOD  cursorRect_1()
   METHOD  cursorWidth()
   METHOD  document()
   METHOD  documentTitle()
   METHOD  ensureCursorVisible()
   METHOD  find( cExp, nOptions )
   METHOD  isReadOnly()
   METHOD  isUndoRedoEnabled()
   METHOD  lineWrapMode()
   METHOD  loadResource( nType, pName )
   METHOD  maximumBlockCount()
   METHOD  mergeCurrentCharFormat( pModifier )
   METHOD  moveCursor( nOperation, nMode )
   METHOD  overwriteMode()
   METHOD  print( pPrinter )
   METHOD  setBackgroundVisible( lVisible )
   METHOD  setCenterOnScroll( lEnabled )
   METHOD  setCurrentCharFormat( pFormat )
   METHOD  setCursorWidth( nWidth )
   METHOD  setDocument( pDocument )
   METHOD  setDocumentTitle( cTitle )
   METHOD  setLineWrapMode( nMode )
   METHOD  setMaximumBlockCount( nMaximum )
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
   METHOD  textCursor()
   METHOD  textInteractionFlags()
   METHOD  toPlainText()
   METHOD  wordWrapMode()
   METHOD  appendHtml( cHtml )
   METHOD  appendPlainText( cText )
   METHOD  centerCursor()
   METHOD  clear()
   METHOD  copy()
   METHOD  cut()
   METHOD  insertPlainText( cText )
   METHOD  paste()
   METHOD  redo()
   METHOD  selectAll()
   METHOD  setPlainText( cText )
   METHOD  undo()

   ENDCLASS


METHOD QPlainTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPlainTextEdit( ... )
   RETURN Self


METHOD QPlainTextEdit:backgroundVisible()
   RETURN Qt_QPlainTextEdit_backgroundVisible( ::pPtr )


METHOD QPlainTextEdit:blockCount()
   RETURN Qt_QPlainTextEdit_blockCount( ::pPtr )


METHOD QPlainTextEdit:canPaste()
   RETURN Qt_QPlainTextEdit_canPaste( ::pPtr )


METHOD QPlainTextEdit:centerOnScroll()
   RETURN Qt_QPlainTextEdit_centerOnScroll( ::pPtr )


METHOD QPlainTextEdit:createStandardContextMenu()
   RETURN Qt_QPlainTextEdit_createStandardContextMenu( ::pPtr )


METHOD QPlainTextEdit:currentCharFormat()
   RETURN Qt_QPlainTextEdit_currentCharFormat( ::pPtr )


METHOD QPlainTextEdit:cursorForPosition( pPos )
   RETURN Qt_QPlainTextEdit_cursorForPosition( ::pPtr, hbqt_ptr( pPos ) )


METHOD QPlainTextEdit:cursorRect( pCursor )
   RETURN Qt_QPlainTextEdit_cursorRect( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QPlainTextEdit:cursorRect_1()
   RETURN Qt_QPlainTextEdit_cursorRect_1( ::pPtr )


METHOD QPlainTextEdit:cursorWidth()
   RETURN Qt_QPlainTextEdit_cursorWidth( ::pPtr )


METHOD QPlainTextEdit:document()
   RETURN Qt_QPlainTextEdit_document( ::pPtr )


METHOD QPlainTextEdit:documentTitle()
   RETURN Qt_QPlainTextEdit_documentTitle( ::pPtr )


METHOD QPlainTextEdit:ensureCursorVisible()
   RETURN Qt_QPlainTextEdit_ensureCursorVisible( ::pPtr )


METHOD QPlainTextEdit:find( cExp, nOptions )
   RETURN Qt_QPlainTextEdit_find( ::pPtr, cExp, nOptions )


METHOD QPlainTextEdit:isReadOnly()
   RETURN Qt_QPlainTextEdit_isReadOnly( ::pPtr )


METHOD QPlainTextEdit:isUndoRedoEnabled()
   RETURN Qt_QPlainTextEdit_isUndoRedoEnabled( ::pPtr )


METHOD QPlainTextEdit:lineWrapMode()
   RETURN Qt_QPlainTextEdit_lineWrapMode( ::pPtr )


METHOD QPlainTextEdit:loadResource( nType, pName )
   RETURN Qt_QPlainTextEdit_loadResource( ::pPtr, nType, hbqt_ptr( pName ) )


METHOD QPlainTextEdit:maximumBlockCount()
   RETURN Qt_QPlainTextEdit_maximumBlockCount( ::pPtr )


METHOD QPlainTextEdit:mergeCurrentCharFormat( pModifier )
   RETURN Qt_QPlainTextEdit_mergeCurrentCharFormat( ::pPtr, hbqt_ptr( pModifier ) )


METHOD QPlainTextEdit:moveCursor( nOperation, nMode )
   RETURN Qt_QPlainTextEdit_moveCursor( ::pPtr, nOperation, nMode )


METHOD QPlainTextEdit:overwriteMode()
   RETURN Qt_QPlainTextEdit_overwriteMode( ::pPtr )


METHOD QPlainTextEdit:print( pPrinter )
   RETURN Qt_QPlainTextEdit_print( ::pPtr, hbqt_ptr( pPrinter ) )


METHOD QPlainTextEdit:setBackgroundVisible( lVisible )
   RETURN Qt_QPlainTextEdit_setBackgroundVisible( ::pPtr, lVisible )


METHOD QPlainTextEdit:setCenterOnScroll( lEnabled )
   RETURN Qt_QPlainTextEdit_setCenterOnScroll( ::pPtr, lEnabled )


METHOD QPlainTextEdit:setCurrentCharFormat( pFormat )
   RETURN Qt_QPlainTextEdit_setCurrentCharFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QPlainTextEdit:setCursorWidth( nWidth )
   RETURN Qt_QPlainTextEdit_setCursorWidth( ::pPtr, nWidth )


METHOD QPlainTextEdit:setDocument( pDocument )
   RETURN Qt_QPlainTextEdit_setDocument( ::pPtr, hbqt_ptr( pDocument ) )


METHOD QPlainTextEdit:setDocumentTitle( cTitle )
   RETURN Qt_QPlainTextEdit_setDocumentTitle( ::pPtr, cTitle )


METHOD QPlainTextEdit:setLineWrapMode( nMode )
   RETURN Qt_QPlainTextEdit_setLineWrapMode( ::pPtr, nMode )


METHOD QPlainTextEdit:setMaximumBlockCount( nMaximum )
   RETURN Qt_QPlainTextEdit_setMaximumBlockCount( ::pPtr, nMaximum )


METHOD QPlainTextEdit:setOverwriteMode( lOverwrite )
   RETURN Qt_QPlainTextEdit_setOverwriteMode( ::pPtr, lOverwrite )


METHOD QPlainTextEdit:setReadOnly( lRo )
   RETURN Qt_QPlainTextEdit_setReadOnly( ::pPtr, lRo )


METHOD QPlainTextEdit:setTabChangesFocus( lB )
   RETURN Qt_QPlainTextEdit_setTabChangesFocus( ::pPtr, lB )


METHOD QPlainTextEdit:setTabStopWidth( nWidth )
   RETURN Qt_QPlainTextEdit_setTabStopWidth( ::pPtr, nWidth )


METHOD QPlainTextEdit:setTextCursor( pCursor )
   RETURN Qt_QPlainTextEdit_setTextCursor( ::pPtr, hbqt_ptr( pCursor ) )


METHOD QPlainTextEdit:setTextInteractionFlags( nFlags )
   RETURN Qt_QPlainTextEdit_setTextInteractionFlags( ::pPtr, nFlags )


METHOD QPlainTextEdit:setUndoRedoEnabled( lEnable )
   RETURN Qt_QPlainTextEdit_setUndoRedoEnabled( ::pPtr, lEnable )


METHOD QPlainTextEdit:setWordWrapMode( nPolicy )
   RETURN Qt_QPlainTextEdit_setWordWrapMode( ::pPtr, nPolicy )


METHOD QPlainTextEdit:tabChangesFocus()
   RETURN Qt_QPlainTextEdit_tabChangesFocus( ::pPtr )


METHOD QPlainTextEdit:tabStopWidth()
   RETURN Qt_QPlainTextEdit_tabStopWidth( ::pPtr )


METHOD QPlainTextEdit:textCursor()
   RETURN Qt_QPlainTextEdit_textCursor( ::pPtr )


METHOD QPlainTextEdit:textInteractionFlags()
   RETURN Qt_QPlainTextEdit_textInteractionFlags( ::pPtr )


METHOD QPlainTextEdit:toPlainText()
   RETURN Qt_QPlainTextEdit_toPlainText( ::pPtr )


METHOD QPlainTextEdit:wordWrapMode()
   RETURN Qt_QPlainTextEdit_wordWrapMode( ::pPtr )


METHOD QPlainTextEdit:appendHtml( cHtml )
   RETURN Qt_QPlainTextEdit_appendHtml( ::pPtr, cHtml )


METHOD QPlainTextEdit:appendPlainText( cText )
   RETURN Qt_QPlainTextEdit_appendPlainText( ::pPtr, cText )


METHOD QPlainTextEdit:centerCursor()
   RETURN Qt_QPlainTextEdit_centerCursor( ::pPtr )


METHOD QPlainTextEdit:clear()
   RETURN Qt_QPlainTextEdit_clear( ::pPtr )


METHOD QPlainTextEdit:copy()
   RETURN Qt_QPlainTextEdit_copy( ::pPtr )


METHOD QPlainTextEdit:cut()
   RETURN Qt_QPlainTextEdit_cut( ::pPtr )


METHOD QPlainTextEdit:insertPlainText( cText )
   RETURN Qt_QPlainTextEdit_insertPlainText( ::pPtr, cText )


METHOD QPlainTextEdit:paste()
   RETURN Qt_QPlainTextEdit_paste( ::pPtr )


METHOD QPlainTextEdit:redo()
   RETURN Qt_QPlainTextEdit_redo( ::pPtr )


METHOD QPlainTextEdit:selectAll()
   RETURN Qt_QPlainTextEdit_selectAll( ::pPtr )


METHOD QPlainTextEdit:setPlainText( cText )
   RETURN Qt_QPlainTextEdit_setPlainText( ::pPtr, cText )


METHOD QPlainTextEdit:undo()
   RETURN Qt_QPlainTextEdit_undo( ::pPtr )


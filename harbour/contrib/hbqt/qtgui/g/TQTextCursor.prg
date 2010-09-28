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


FUNCTION QTextCursor( ... )
   RETURN HB_QTextCursor():new( ... )


CREATE CLASS QTextCursor INHERIT HbQtObjectHandler FUNCTION HB_QTextCursor

   METHOD  new( ... )

   METHOD  anchor()
   METHOD  atBlockEnd()
   METHOD  atBlockStart()
   METHOD  atEnd()
   METHOD  atStart()
   METHOD  beginEditBlock()
   METHOD  block()
   METHOD  blockCharFormat()
   METHOD  blockFormat()
   METHOD  blockNumber()
   METHOD  charFormat()
   METHOD  clearSelection()
   METHOD  columnNumber()
   METHOD  createList( ... )
   METHOD  currentFrame()
   METHOD  currentList()
   METHOD  deleteChar()
   METHOD  deletePreviousChar()
   METHOD  document()
   METHOD  endEditBlock()
   METHOD  hasComplexSelection()
   METHOD  hasSelection()
   METHOD  insertBlock( ... )
   METHOD  insertFragment( pFragment )
   METHOD  insertFrame( pFormat )
   METHOD  insertHtml( cHtml )
   METHOD  insertImage( ... )
   METHOD  insertList( ... )
   METHOD  insertText( ... )
   METHOD  isCopyOf( pOther )
   METHOD  isNull()
   METHOD  joinPreviousEditBlock()
   METHOD  mergeBlockCharFormat( pModifier )
   METHOD  mergeBlockFormat( pModifier )
   METHOD  mergeCharFormat( pModifier )
   METHOD  movePosition( nOperation, nMode, nN )
   METHOD  position()
   METHOD  removeSelectedText()
   METHOD  select( nSelection )
   METHOD  selectedTableCells( nFirstRow, nNumRows, nFirstColumn, nNumColumns )
   METHOD  selectedText()
   METHOD  selection()
   METHOD  selectionEnd()
   METHOD  selectionStart()
   METHOD  setBlockCharFormat( pFormat )
   METHOD  setBlockFormat( pFormat )
   METHOD  setCharFormat( pFormat )
   METHOD  setPosition( nPos, nM )
   METHOD  setVisualNavigation( lB )
   METHOD  visualNavigation()

   ENDCLASS


METHOD QTextCursor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCursor( ... )
   RETURN Self


METHOD QTextCursor:anchor()
   RETURN Qt_QTextCursor_anchor( ::pPtr )


METHOD QTextCursor:atBlockEnd()
   RETURN Qt_QTextCursor_atBlockEnd( ::pPtr )


METHOD QTextCursor:atBlockStart()
   RETURN Qt_QTextCursor_atBlockStart( ::pPtr )


METHOD QTextCursor:atEnd()
   RETURN Qt_QTextCursor_atEnd( ::pPtr )


METHOD QTextCursor:atStart()
   RETURN Qt_QTextCursor_atStart( ::pPtr )


METHOD QTextCursor:beginEditBlock()
   RETURN Qt_QTextCursor_beginEditBlock( ::pPtr )


METHOD QTextCursor:block()
   RETURN HB_QTextBlock():from( Qt_QTextCursor_block( ::pPtr ) )


METHOD QTextCursor:blockCharFormat()
   RETURN HB_QTextCharFormat():from( Qt_QTextCursor_blockCharFormat( ::pPtr ) )


METHOD QTextCursor:blockFormat()
   RETURN HB_QTextBlockFormat():from( Qt_QTextCursor_blockFormat( ::pPtr ) )


METHOD QTextCursor:blockNumber()
   RETURN Qt_QTextCursor_blockNumber( ::pPtr )


METHOD QTextCursor:charFormat()
   RETURN HB_QTextCharFormat():from( Qt_QTextCursor_charFormat( ::pPtr ) )


METHOD QTextCursor:clearSelection()
   RETURN Qt_QTextCursor_clearSelection( ::pPtr )


METHOD QTextCursor:columnNumber()
   RETURN Qt_QTextCursor_columnNumber( ::pPtr )


METHOD QTextCursor:createList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QTextList():from( Qt_QTextCursor_createList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QTextList():from( Qt_QTextCursor_createList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:currentFrame()
   RETURN HB_QTextFrame():from( Qt_QTextCursor_currentFrame( ::pPtr ) )


METHOD QTextCursor:currentList()
   RETURN HB_QTextList():from( Qt_QTextCursor_currentList( ::pPtr ) )


METHOD QTextCursor:deleteChar()
   RETURN Qt_QTextCursor_deleteChar( ::pPtr )


METHOD QTextCursor:deletePreviousChar()
   RETURN Qt_QTextCursor_deletePreviousChar( ::pPtr )


METHOD QTextCursor:document()
   RETURN HB_QTextDocument():from( Qt_QTextCursor_document( ::pPtr ) )


METHOD QTextCursor:endEditBlock()
   RETURN Qt_QTextCursor_endEditBlock( ::pPtr )


METHOD QTextCursor:hasComplexSelection()
   RETURN Qt_QTextCursor_hasComplexSelection( ::pPtr )


METHOD QTextCursor:hasSelection()
   RETURN Qt_QTextCursor_hasSelection( ::pPtr )


METHOD QTextCursor:insertBlock( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertBlock_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertBlock_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextCursor_insertBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:insertFragment( pFragment )
   RETURN Qt_QTextCursor_insertFragment( ::pPtr, hbqt_ptr( pFragment ) )


METHOD QTextCursor:insertFrame( pFormat )
   RETURN HB_QTextFrame():from( Qt_QTextCursor_insertFrame( ::pPtr, hbqt_ptr( pFormat ) ) )


METHOD QTextCursor:insertHtml( cHtml )
   RETURN Qt_QTextCursor_insertHtml( ::pPtr, cHtml )


METHOD QTextCursor:insertImage( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertImage_3( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertImage_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertImage( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QTEXTIMAGEFORMAT"
            RETURN Qt_QTextCursor_insertImage_1( ::pPtr, ... )
         CASE "QIMAGE"
            RETURN Qt_QTextCursor_insertImage_3( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:insertList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QTextList():from( Qt_QTextCursor_insertList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QTextList():from( Qt_QTextCursor_insertList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:insertText( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_insertText_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:isCopyOf( pOther )
   RETURN Qt_QTextCursor_isCopyOf( ::pPtr, hbqt_ptr( pOther ) )


METHOD QTextCursor:isNull()
   RETURN Qt_QTextCursor_isNull( ::pPtr )


METHOD QTextCursor:joinPreviousEditBlock()
   RETURN Qt_QTextCursor_joinPreviousEditBlock( ::pPtr )


METHOD QTextCursor:mergeBlockCharFormat( pModifier )
   RETURN Qt_QTextCursor_mergeBlockCharFormat( ::pPtr, hbqt_ptr( pModifier ) )


METHOD QTextCursor:mergeBlockFormat( pModifier )
   RETURN Qt_QTextCursor_mergeBlockFormat( ::pPtr, hbqt_ptr( pModifier ) )


METHOD QTextCursor:mergeCharFormat( pModifier )
   RETURN Qt_QTextCursor_mergeCharFormat( ::pPtr, hbqt_ptr( pModifier ) )


METHOD QTextCursor:movePosition( nOperation, nMode, nN )
   RETURN Qt_QTextCursor_movePosition( ::pPtr, nOperation, nMode, nN )


METHOD QTextCursor:position()
   RETURN Qt_QTextCursor_position( ::pPtr )


METHOD QTextCursor:removeSelectedText()
   RETURN Qt_QTextCursor_removeSelectedText( ::pPtr )


METHOD QTextCursor:select( nSelection )
   RETURN Qt_QTextCursor_select( ::pPtr, nSelection )


METHOD QTextCursor:selectedTableCells( nFirstRow, nNumRows, nFirstColumn, nNumColumns )
   RETURN Qt_QTextCursor_selectedTableCells( ::pPtr, nFirstRow, nNumRows, nFirstColumn, nNumColumns )


METHOD QTextCursor:selectedText()
   RETURN Qt_QTextCursor_selectedText( ::pPtr )


METHOD QTextCursor:selection()
   RETURN HB_QTextDocumentFragment():from( Qt_QTextCursor_selection( ::pPtr ) )


METHOD QTextCursor:selectionEnd()
   RETURN Qt_QTextCursor_selectionEnd( ::pPtr )


METHOD QTextCursor:selectionStart()
   RETURN Qt_QTextCursor_selectionStart( ::pPtr )


METHOD QTextCursor:setBlockCharFormat( pFormat )
   RETURN Qt_QTextCursor_setBlockCharFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QTextCursor:setBlockFormat( pFormat )
   RETURN Qt_QTextCursor_setBlockFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QTextCursor:setCharFormat( pFormat )
   RETURN Qt_QTextCursor_setCharFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QTextCursor:setPosition( nPos, nM )
   RETURN Qt_QTextCursor_setPosition( ::pPtr, nPos, nM )


METHOD QTextCursor:setVisualNavigation( lB )
   RETURN Qt_QTextCursor_setVisualNavigation( ::pPtr, lB )


METHOD QTextCursor:visualNavigation()
   RETURN Qt_QTextCursor_visualNavigation( ::pPtr )


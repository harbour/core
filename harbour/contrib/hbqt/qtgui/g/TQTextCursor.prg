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

FUNCTION QTextCursorFrom( ... )
   RETURN HB_QTextCursor():from( ... )

FUNCTION QTextCursorFromPointer( ... )
   RETURN HB_QTextCursor():fromPointer( ... )


CREATE CLASS QTextCursor INHERIT HbQtObjectHandler FUNCTION HB_QTextCursor

   METHOD  new( ... )

   METHOD  anchor                        // (  )                                               -> nInt
   METHOD  atBlockEnd                    // (  )                                               -> lBool
   METHOD  atBlockStart                  // (  )                                               -> lBool
   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  atStart                       // (  )                                               -> lBool
   METHOD  beginEditBlock                // (  )                                               -> NIL
   METHOD  block                         // (  )                                               -> oQTextBlock
   METHOD  blockCharFormat               // (  )                                               -> oQTextCharFormat
   METHOD  blockFormat                   // (  )                                               -> oQTextBlockFormat
   METHOD  blockNumber                   // (  )                                               -> nInt
   METHOD  charFormat                    // (  )                                               -> oQTextCharFormat
   METHOD  clearSelection                // (  )                                               -> NIL
   METHOD  columnNumber                  // (  )                                               -> nInt
   METHOD  createList                    // ( oQTextListFormat )                               -> oQTextList
                                         // ( nStyle )                                         -> oQTextList
   METHOD  currentFrame                  // (  )                                               -> oQTextFrame
   METHOD  currentList                   // (  )                                               -> oQTextList
   METHOD  deleteChar                    // (  )                                               -> NIL
   METHOD  deletePreviousChar            // (  )                                               -> NIL
   METHOD  document                      // (  )                                               -> oQTextDocument
   METHOD  endEditBlock                  // (  )                                               -> NIL
   METHOD  hasComplexSelection           // (  )                                               -> lBool
   METHOD  hasSelection                  // (  )                                               -> lBool
   METHOD  insertBlock                   // (  )                                               -> NIL
                                         // ( oQTextBlockFormat )                              -> NIL
                                         // ( oQTextBlockFormat, oQTextCharFormat )            -> NIL
   METHOD  insertFragment                // ( oQTextDocumentFragment )                         -> NIL
   METHOD  insertFrame                   // ( oQTextFrameFormat )                              -> oQTextFrame
   METHOD  insertHtml                    // ( cHtml )                                          -> NIL
   METHOD  insertImage                   // ( cName )                                          -> NIL
                                         // ( oQTextImageFormat )                              -> NIL
                                         // ( oQTextImageFormat, nAlignment )                  -> NIL
                                         // ( oQImage, cName )                                 -> NIL
   METHOD  insertList                    // ( oQTextListFormat )                               -> oQTextList
                                         // ( nStyle )                                         -> oQTextList
   METHOD  insertText                    // ( cText )                                          -> NIL
                                         // ( cText, oQTextCharFormat )                        -> NIL
   METHOD  isCopyOf                      // ( oQTextCursor )                                   -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  joinPreviousEditBlock         // (  )                                               -> NIL
   METHOD  mergeBlockCharFormat          // ( oQTextCharFormat )                               -> NIL
   METHOD  mergeBlockFormat              // ( oQTextBlockFormat )                              -> NIL
   METHOD  mergeCharFormat               // ( oQTextCharFormat )                               -> NIL
   METHOD  movePosition                  // ( nOperation, nMode, nN )                          -> lBool
   METHOD  position                      // (  )                                               -> nInt
   METHOD  removeSelectedText            // (  )                                               -> NIL
   METHOD  select                        // ( nSelection )                                     -> NIL
   METHOD  selectedTableCells            // ( @nFirstRow, @nNumRows, @nFirstColumn, @nNumColumns ) -> NIL
   METHOD  selectedText                  // (  )                                               -> cQString
   METHOD  selection                     // (  )                                               -> oQTextDocumentFragment
   METHOD  selectionEnd                  // (  )                                               -> nInt
   METHOD  selectionStart                // (  )                                               -> nInt
   METHOD  setBlockCharFormat            // ( oQTextCharFormat )                               -> NIL
   METHOD  setBlockFormat                // ( oQTextBlockFormat )                              -> NIL
   METHOD  setCharFormat                 // ( oQTextCharFormat )                               -> NIL
   METHOD  setPosition                   // ( nPos, nM )                                       -> NIL
   METHOD  setVisualNavigation           // ( lB )                                             -> NIL
   METHOD  visualNavigation              // (  )                                               -> lBool

   ENDCLASS


METHOD QTextCursor:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCursor( ... )
   RETURN Self


METHOD QTextCursor:anchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_anchor( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:atBlockEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atBlockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:atBlockStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atBlockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:atStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_atStart( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:beginEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_beginEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:block( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFromPointer( Qt_QTextCursor_block( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:blockCharFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextCursor_blockCharFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:blockFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextBlockFormatFromPointer( Qt_QTextCursor_blockFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:blockNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_blockNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:charFormat( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextCharFormatFromPointer( Qt_QTextCursor_charFormat( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:clearSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_clearSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:columnNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_columnNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:createList( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_createList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_createList( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:currentFrame( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextFrameFromPointer( Qt_QTextCursor_currentFrame( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:currentList( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextListFromPointer( Qt_QTextCursor_currentList( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:deleteChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_deleteChar( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:deletePreviousChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_deletePreviousChar( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:document( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFromPointer( Qt_QTextCursor_document( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:endEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_endEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:hasComplexSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_hasComplexSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:hasSelection( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_hasSelection( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


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


METHOD QTextCursor:insertFragment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertFragment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:insertFrame( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextFrameFromPointer( Qt_QTextCursor_insertFrame( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:insertHtml( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_insertHtml( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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
         RETURN QTextListFromPointer( Qt_QTextCursor_insertList_1( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QTextListFromPointer( Qt_QTextCursor_insertList( ::pPtr, ... ) )
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


METHOD QTextCursor:isCopyOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_isCopyOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:joinPreviousEditBlock( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_joinPreviousEditBlock( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:mergeBlockCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeBlockCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:mergeBlockFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeBlockFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:mergeCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_mergeCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:movePosition( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_movePosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:position( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_position( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:removeSelectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_removeSelectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:select( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_select( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:selectedTableCells( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QTextCursor_selectedTableCells( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:selectedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectedText( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:selection( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTextDocumentFragmentFromPointer( Qt_QTextCursor_selection( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:selectionEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectionEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:selectionStart( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_selectionStart( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:setBlockCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setBlockCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:setBlockFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setBlockFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:setCharFormat( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setCharFormat( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:setPosition( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCursor_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:setVisualNavigation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCursor_setVisualNavigation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCursor:visualNavigation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCursor_visualNavigation( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


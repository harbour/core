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


FUNCTION QTextDocument( ... )
   RETURN HB_QTextDocument():new( ... )


CREATE CLASS QTextDocument INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QTextDocument

   METHOD  new( ... )

   METHOD  addResource( nType, pName, pResource )
   METHOD  adjustSize()
   METHOD  begin()
   METHOD  blockCount()
   METHOD  characterAt( nPos )
   METHOD  characterCount()
   METHOD  clear()
   METHOD  clone( pParent )
   METHOD  defaultFont()
   METHOD  defaultStyleSheet()
   METHOD  defaultTextOption()
   METHOD  documentLayout()
   METHOD  documentMargin()
   METHOD  drawContents( pP, pRect )
   METHOD  end()
   METHOD  find( ... )
   METHOD  findBlock( nPos )
   METHOD  findBlockByLineNumber( nLineNumber )
   METHOD  findBlockByNumber( nBlockNumber )
   METHOD  firstBlock()
   METHOD  idealWidth()
   METHOD  indentWidth()
   METHOD  isEmpty()
   METHOD  isModified()
   METHOD  isRedoAvailable()
   METHOD  isUndoAvailable()
   METHOD  isUndoRedoEnabled()
   METHOD  lastBlock()
   METHOD  lineCount()
   METHOD  markContentsDirty( nPosition, nLength )
   METHOD  maximumBlockCount()
   METHOD  metaInformation( nInfo )
   METHOD  object( nObjectIndex )
   METHOD  objectForFormat( pF )
   METHOD  pageCount()
   METHOD  pageSize()
   METHOD  print( pPrinter )
   METHOD  redo( ... )
   METHOD  resource( nType, pName )
   METHOD  revision()
   METHOD  rootFrame()
   METHOD  setDefaultFont( pFont )
   METHOD  setDefaultStyleSheet( cSheet )
   METHOD  setDefaultTextOption( pOption )
   METHOD  setDocumentLayout( pLayout )
   METHOD  setDocumentMargin( nMargin )
   METHOD  setHtml( cHtml )
   METHOD  setIndentWidth( nWidth )
   METHOD  setMaximumBlockCount( nMaximum )
   METHOD  setMetaInformation( nInfo, cString )
   METHOD  setPageSize( pSize )
   METHOD  setPlainText( cText )
   METHOD  setTextWidth( nWidth )
   METHOD  setUndoRedoEnabled( lEnable )
   METHOD  setUseDesignMetrics( lB )
   METHOD  size()
   METHOD  textWidth()
   METHOD  toHtml( pEncoding )
   METHOD  toPlainText()
   METHOD  undo( ... )
   METHOD  useDesignMetrics()
   METHOD  setModified( lM )

   ENDCLASS


METHOD QTextDocument:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDocument( ... )
   RETURN Self


METHOD QTextDocument:addResource( nType, pName, pResource )
   RETURN Qt_QTextDocument_addResource( ::pPtr, nType, hbqt_ptr( pName ), hbqt_ptr( pResource ) )


METHOD QTextDocument:adjustSize()
   RETURN Qt_QTextDocument_adjustSize( ::pPtr )


METHOD QTextDocument:begin()
   RETURN HB_QTextBlock():from( Qt_QTextDocument_begin( ::pPtr ) )


METHOD QTextDocument:blockCount()
   RETURN Qt_QTextDocument_blockCount( ::pPtr )


METHOD QTextDocument:characterAt( nPos )
   RETURN HB_QChar():from( Qt_QTextDocument_characterAt( ::pPtr, nPos ) )


METHOD QTextDocument:characterCount()
   RETURN Qt_QTextDocument_characterCount( ::pPtr )


METHOD QTextDocument:clear()
   RETURN Qt_QTextDocument_clear( ::pPtr )


METHOD QTextDocument:clone( pParent )
   RETURN HB_QTextDocument():from( Qt_QTextDocument_clone( ::pPtr, hbqt_ptr( pParent ) ) )


METHOD QTextDocument:defaultFont()
   RETURN HB_QFont():from( Qt_QTextDocument_defaultFont( ::pPtr ) )


METHOD QTextDocument:defaultStyleSheet()
   RETURN Qt_QTextDocument_defaultStyleSheet( ::pPtr )


METHOD QTextDocument:defaultTextOption()
   RETURN HB_QTextOption():from( Qt_QTextDocument_defaultTextOption( ::pPtr ) )


METHOD QTextDocument:documentLayout()
   RETURN HB_QAbstractTextDocumentLayout():from( Qt_QTextDocument_documentLayout( ::pPtr ) )


METHOD QTextDocument:documentMargin()
   RETURN Qt_QTextDocument_documentMargin( ::pPtr )


METHOD QTextDocument:drawContents( pP, pRect )
   RETURN Qt_QTextDocument_drawContents( ::pPtr, hbqt_ptr( pP ), hbqt_ptr( pRect ) )


METHOD QTextDocument:end()
   RETURN HB_QTextBlock():from( Qt_QTextDocument_end( ::pPtr ) )


METHOD QTextDocument:find( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_2( ::pPtr, ... ) )
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_3( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_2( ::pPtr, ... ) )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QTextCursor():from( Qt_QTextDocument_find_3( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextDocument:findBlock( nPos )
   RETURN HB_QTextBlock():from( Qt_QTextDocument_findBlock( ::pPtr, nPos ) )


METHOD QTextDocument:findBlockByLineNumber( nLineNumber )
   RETURN HB_QTextBlock():from( Qt_QTextDocument_findBlockByLineNumber( ::pPtr, nLineNumber ) )


METHOD QTextDocument:findBlockByNumber( nBlockNumber )
   RETURN HB_QTextBlock():from( Qt_QTextDocument_findBlockByNumber( ::pPtr, nBlockNumber ) )


METHOD QTextDocument:firstBlock()
   RETURN HB_QTextBlock():from( Qt_QTextDocument_firstBlock( ::pPtr ) )


METHOD QTextDocument:idealWidth()
   RETURN Qt_QTextDocument_idealWidth( ::pPtr )


METHOD QTextDocument:indentWidth()
   RETURN Qt_QTextDocument_indentWidth( ::pPtr )


METHOD QTextDocument:isEmpty()
   RETURN Qt_QTextDocument_isEmpty( ::pPtr )


METHOD QTextDocument:isModified()
   RETURN Qt_QTextDocument_isModified( ::pPtr )


METHOD QTextDocument:isRedoAvailable()
   RETURN Qt_QTextDocument_isRedoAvailable( ::pPtr )


METHOD QTextDocument:isUndoAvailable()
   RETURN Qt_QTextDocument_isUndoAvailable( ::pPtr )


METHOD QTextDocument:isUndoRedoEnabled()
   RETURN Qt_QTextDocument_isUndoRedoEnabled( ::pPtr )


METHOD QTextDocument:lastBlock()
   RETURN HB_QTextBlock():from( Qt_QTextDocument_lastBlock( ::pPtr ) )


METHOD QTextDocument:lineCount()
   RETURN Qt_QTextDocument_lineCount( ::pPtr )


METHOD QTextDocument:markContentsDirty( nPosition, nLength )
   RETURN Qt_QTextDocument_markContentsDirty( ::pPtr, nPosition, nLength )


METHOD QTextDocument:maximumBlockCount()
   RETURN Qt_QTextDocument_maximumBlockCount( ::pPtr )


METHOD QTextDocument:metaInformation( nInfo )
   RETURN Qt_QTextDocument_metaInformation( ::pPtr, nInfo )


METHOD QTextDocument:object( nObjectIndex )
   RETURN HB_QTextObject():from( Qt_QTextDocument_object( ::pPtr, nObjectIndex ) )


METHOD QTextDocument:objectForFormat( pF )
   RETURN HB_QTextObject():from( Qt_QTextDocument_objectForFormat( ::pPtr, hbqt_ptr( pF ) ) )


METHOD QTextDocument:pageCount()
   RETURN Qt_QTextDocument_pageCount( ::pPtr )


METHOD QTextDocument:pageSize()
   RETURN HB_QSizeF():from( Qt_QTextDocument_pageSize( ::pPtr ) )


METHOD QTextDocument:print( pPrinter )
   RETURN Qt_QTextDocument_print( ::pPtr, hbqt_ptr( pPrinter ) )


METHOD QTextDocument:redo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocument_redo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextDocument_redo_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextDocument:resource( nType, pName )
   RETURN HB_QVariant():from( Qt_QTextDocument_resource( ::pPtr, nType, hbqt_ptr( pName ) ) )


METHOD QTextDocument:revision()
   RETURN Qt_QTextDocument_revision( ::pPtr )


METHOD QTextDocument:rootFrame()
   RETURN HB_QTextFrame():from( Qt_QTextDocument_rootFrame( ::pPtr ) )


METHOD QTextDocument:setDefaultFont( pFont )
   RETURN Qt_QTextDocument_setDefaultFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QTextDocument:setDefaultStyleSheet( cSheet )
   RETURN Qt_QTextDocument_setDefaultStyleSheet( ::pPtr, cSheet )


METHOD QTextDocument:setDefaultTextOption( pOption )
   RETURN Qt_QTextDocument_setDefaultTextOption( ::pPtr, hbqt_ptr( pOption ) )


METHOD QTextDocument:setDocumentLayout( pLayout )
   RETURN Qt_QTextDocument_setDocumentLayout( ::pPtr, hbqt_ptr( pLayout ) )


METHOD QTextDocument:setDocumentMargin( nMargin )
   RETURN Qt_QTextDocument_setDocumentMargin( ::pPtr, nMargin )


METHOD QTextDocument:setHtml( cHtml )
   RETURN Qt_QTextDocument_setHtml( ::pPtr, cHtml )


METHOD QTextDocument:setIndentWidth( nWidth )
   RETURN Qt_QTextDocument_setIndentWidth( ::pPtr, nWidth )


METHOD QTextDocument:setMaximumBlockCount( nMaximum )
   RETURN Qt_QTextDocument_setMaximumBlockCount( ::pPtr, nMaximum )


METHOD QTextDocument:setMetaInformation( nInfo, cString )
   RETURN Qt_QTextDocument_setMetaInformation( ::pPtr, nInfo, cString )


METHOD QTextDocument:setPageSize( pSize )
   RETURN Qt_QTextDocument_setPageSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QTextDocument:setPlainText( cText )
   RETURN Qt_QTextDocument_setPlainText( ::pPtr, cText )


METHOD QTextDocument:setTextWidth( nWidth )
   RETURN Qt_QTextDocument_setTextWidth( ::pPtr, nWidth )


METHOD QTextDocument:setUndoRedoEnabled( lEnable )
   RETURN Qt_QTextDocument_setUndoRedoEnabled( ::pPtr, lEnable )


METHOD QTextDocument:setUseDesignMetrics( lB )
   RETURN Qt_QTextDocument_setUseDesignMetrics( ::pPtr, lB )


METHOD QTextDocument:size()
   RETURN HB_QSizeF():from( Qt_QTextDocument_size( ::pPtr ) )


METHOD QTextDocument:textWidth()
   RETURN Qt_QTextDocument_textWidth( ::pPtr )


METHOD QTextDocument:toHtml( pEncoding )
   RETURN Qt_QTextDocument_toHtml( ::pPtr, hbqt_ptr( pEncoding ) )


METHOD QTextDocument:toPlainText()
   RETURN Qt_QTextDocument_toPlainText( ::pPtr )


METHOD QTextDocument:undo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextDocument_undo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QTextDocument_undo_1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextDocument:useDesignMetrics()
   RETURN Qt_QTextDocument_useDesignMetrics( ::pPtr )


METHOD QTextDocument:setModified( lM )
   RETURN Qt_QTextDocument_setModified( ::pPtr, lM )


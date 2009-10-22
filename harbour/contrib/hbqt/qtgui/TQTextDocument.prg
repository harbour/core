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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


CREATE CLASS QTextDocument INHERIT QObject

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  addResource( nType, pName, pResource )  INLINE  Qt_QTextDocument_addResource( ::pPtr, nType, pName, pResource )
   METHOD  adjustSize()                        INLINE  Qt_QTextDocument_adjustSize( ::pPtr )
   METHOD  begin()                             INLINE  Qt_QTextDocument_begin( ::pPtr )
   METHOD  blockCount()                        INLINE  Qt_QTextDocument_blockCount( ::pPtr )
   METHOD  characterCount()                    INLINE  Qt_QTextDocument_characterCount( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QTextDocument_clear( ::pPtr )
   METHOD  clone( pParent )                    INLINE  Qt_QTextDocument_clone( ::pPtr, pParent )
   METHOD  defaultFont()                       INLINE  Qt_QTextDocument_defaultFont( ::pPtr )
   METHOD  defaultStyleSheet()                 INLINE  Qt_QTextDocument_defaultStyleSheet( ::pPtr )
   METHOD  defaultTextOption()                 INLINE  Qt_QTextDocument_defaultTextOption( ::pPtr )
   METHOD  documentLayout()                    INLINE  Qt_QTextDocument_documentLayout( ::pPtr )
   METHOD  documentMargin()                    INLINE  Qt_QTextDocument_documentMargin( ::pPtr )
   METHOD  drawContents( pP, pRect )           INLINE  Qt_QTextDocument_drawContents( ::pPtr, pP, pRect )
   METHOD  end()                               INLINE  Qt_QTextDocument_end( ::pPtr )
   METHOD  find( cSubString, pCursor, nOptions )  INLINE  Qt_QTextDocument_find( ::pPtr, cSubString, pCursor, nOptions )
   METHOD  find_1( pExpr, pCursor, nOptions )  INLINE  Qt_QTextDocument_find_1( ::pPtr, pExpr, pCursor, nOptions )
   METHOD  find_2( cSubString, nPosition, nOptions )  INLINE  Qt_QTextDocument_find_2( ::pPtr, cSubString, nPosition, nOptions )
   METHOD  find_3( pExpr, nPosition, nOptions )  INLINE  Qt_QTextDocument_find_3( ::pPtr, pExpr, nPosition, nOptions )
   METHOD  findBlock( nPos )                   INLINE  Qt_QTextDocument_findBlock( ::pPtr, nPos )
   METHOD  findBlockByLineNumber( nLineNumber )  INLINE  Qt_QTextDocument_findBlockByLineNumber( ::pPtr, nLineNumber )
   METHOD  findBlockByNumber( nBlockNumber )   INLINE  Qt_QTextDocument_findBlockByNumber( ::pPtr, nBlockNumber )
   METHOD  firstBlock()                        INLINE  Qt_QTextDocument_firstBlock( ::pPtr )
   METHOD  idealWidth()                        INLINE  Qt_QTextDocument_idealWidth( ::pPtr )
   METHOD  indentWidth()                       INLINE  Qt_QTextDocument_indentWidth( ::pPtr )
   METHOD  isEmpty()                           INLINE  Qt_QTextDocument_isEmpty( ::pPtr )
   METHOD  isModified()                        INLINE  Qt_QTextDocument_isModified( ::pPtr )
   METHOD  isRedoAvailable()                   INLINE  Qt_QTextDocument_isRedoAvailable( ::pPtr )
   METHOD  isUndoAvailable()                   INLINE  Qt_QTextDocument_isUndoAvailable( ::pPtr )
   METHOD  isUndoRedoEnabled()                 INLINE  Qt_QTextDocument_isUndoRedoEnabled( ::pPtr )
   METHOD  lastBlock()                         INLINE  Qt_QTextDocument_lastBlock( ::pPtr )
   METHOD  lineCount()                         INLINE  Qt_QTextDocument_lineCount( ::pPtr )
   METHOD  markContentsDirty( nPosition, nLength )  INLINE  Qt_QTextDocument_markContentsDirty( ::pPtr, nPosition, nLength )
   METHOD  maximumBlockCount()                 INLINE  Qt_QTextDocument_maximumBlockCount( ::pPtr )
   METHOD  metaInformation( nInfo )            INLINE  Qt_QTextDocument_metaInformation( ::pPtr, nInfo )
   METHOD  object( nObjectIndex )              INLINE  Qt_QTextDocument_object( ::pPtr, nObjectIndex )
   METHOD  objectForFormat( pF )               INLINE  Qt_QTextDocument_objectForFormat( ::pPtr, pF )
   METHOD  pageCount()                         INLINE  Qt_QTextDocument_pageCount( ::pPtr )
   METHOD  pageSize()                          INLINE  Qt_QTextDocument_pageSize( ::pPtr )
   METHOD  print( pPrinter )                   INLINE  Qt_QTextDocument_print( ::pPtr, pPrinter )
   METHOD  redo( pCursor )                     INLINE  Qt_QTextDocument_redo( ::pPtr, pCursor )
   METHOD  resource( nType, pName )            INLINE  Qt_QTextDocument_resource( ::pPtr, nType, pName )
   METHOD  revision()                          INLINE  Qt_QTextDocument_revision( ::pPtr )
   METHOD  rootFrame()                         INLINE  Qt_QTextDocument_rootFrame( ::pPtr )
   METHOD  setDefaultFont( pFont )             INLINE  Qt_QTextDocument_setDefaultFont( ::pPtr, pFont )
   METHOD  setDefaultStyleSheet( cSheet )      INLINE  Qt_QTextDocument_setDefaultStyleSheet( ::pPtr, cSheet )
   METHOD  setDefaultTextOption( pOption )     INLINE  Qt_QTextDocument_setDefaultTextOption( ::pPtr, pOption )
   METHOD  setDocumentLayout( pLayout )        INLINE  Qt_QTextDocument_setDocumentLayout( ::pPtr, pLayout )
   METHOD  setDocumentMargin( nMargin )        INLINE  Qt_QTextDocument_setDocumentMargin( ::pPtr, nMargin )
   METHOD  setHtml( cHtml )                    INLINE  Qt_QTextDocument_setHtml( ::pPtr, cHtml )
   METHOD  setIndentWidth( nWidth )            INLINE  Qt_QTextDocument_setIndentWidth( ::pPtr, nWidth )
   METHOD  setMaximumBlockCount( nMaximum )    INLINE  Qt_QTextDocument_setMaximumBlockCount( ::pPtr, nMaximum )
   METHOD  setMetaInformation( nInfo, cString )  INLINE  Qt_QTextDocument_setMetaInformation( ::pPtr, nInfo, cString )
   METHOD  setPageSize( pSize )                INLINE  Qt_QTextDocument_setPageSize( ::pPtr, pSize )
   METHOD  setPlainText( cText )               INLINE  Qt_QTextDocument_setPlainText( ::pPtr, cText )
   METHOD  setTextWidth( nWidth )              INLINE  Qt_QTextDocument_setTextWidth( ::pPtr, nWidth )
   METHOD  setUndoRedoEnabled( lEnable )       INLINE  Qt_QTextDocument_setUndoRedoEnabled( ::pPtr, lEnable )
   METHOD  setUseDesignMetrics( lB )           INLINE  Qt_QTextDocument_setUseDesignMetrics( ::pPtr, lB )
   METHOD  size()                              INLINE  Qt_QTextDocument_size( ::pPtr )
   METHOD  textWidth()                         INLINE  Qt_QTextDocument_textWidth( ::pPtr )
   METHOD  toHtml( pEncoding )                 INLINE  Qt_QTextDocument_toHtml( ::pPtr, pEncoding )
   METHOD  toPlainText()                       INLINE  Qt_QTextDocument_toPlainText( ::pPtr )
   METHOD  undo( pCursor )                     INLINE  Qt_QTextDocument_undo( ::pPtr, pCursor )
   METHOD  useDesignMetrics()                  INLINE  Qt_QTextDocument_useDesignMetrics( ::pPtr )
   METHOD  redo_1()                            INLINE  Qt_QTextDocument_redo_1( ::pPtr )
   METHOD  setModified( lM )                   INLINE  Qt_QTextDocument_setModified( ::pPtr, lM )
   METHOD  undo_1()                            INLINE  Qt_QTextDocument_undo_1( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextDocument

   ::pParent := pParent

   ::pPtr := Qt_QTextDocument( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextDocument

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

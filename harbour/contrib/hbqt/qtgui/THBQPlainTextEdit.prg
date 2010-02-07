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


CREATE CLASS HBQPlainTextEdit INHERIT HbQtObjectHandler, QPlainTextEdit

   METHOD  new( ... )

   METHOD  getIndex( pCrQTextCursor )
   METHOD  getLine( pCrQTextCursor )
   METHOD  lineNumberAreaWidth()
   METHOD  getSpaces()
   METHOD  setSpaces( nNewSpaces )
   METHOD  bookmarks( nBlock )
   METHOD  nextBookmark( nBlock )
   METHOD  prevBookmark( nBlock )
   METHOD  gotoBookmark( nBlock )
   METHOD  numberBlockVisible( lB )
   METHOD  numberBlockVisible_1()
   METHOD  highlightCurrentLine( lB )
   METHOD  highlightCurrentLine_1()
   METHOD  updateLineNumberAreaWidth( nNewBlockCount )
   METHOD  caseUpper()
   METHOD  caseLower()
   METHOD  escapeQuotes()
   METHOD  escapeDQuotes()
   METHOD  unescapeQuotes()
   METHOD  unescapeDQuotes()
   METHOD  convertQuotes()
   METHOD  convertDQuotes()
   METHOD  blockComment()
   METHOD  streamComment()
   METHOD  duplicateLine()
   METHOD  replaceSelection( cTxt )
   METHOD  blockIndent( nSteps )
   METHOD  deleteLine()

   ENDCLASS


METHOD HBQPlainTextEdit:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQPlainTextEdit( ... )
   RETURN Self


METHOD HBQPlainTextEdit:getIndex( pCrQTextCursor )
   RETURN Qt_HBQPlainTextEdit_getIndex( ::pPtr, hbqt_ptr( pCrQTextCursor ) )


METHOD HBQPlainTextEdit:getLine( pCrQTextCursor )
   RETURN Qt_HBQPlainTextEdit_getLine( ::pPtr, hbqt_ptr( pCrQTextCursor ) )


METHOD HBQPlainTextEdit:lineNumberAreaWidth()
   RETURN Qt_HBQPlainTextEdit_lineNumberAreaWidth( ::pPtr )


METHOD HBQPlainTextEdit:getSpaces()
   RETURN Qt_HBQPlainTextEdit_getSpaces( ::pPtr )


METHOD HBQPlainTextEdit:setSpaces( nNewSpaces )
   RETURN Qt_HBQPlainTextEdit_setSpaces( ::pPtr, nNewSpaces )


METHOD HBQPlainTextEdit:bookmarks( nBlock )
   RETURN Qt_HBQPlainTextEdit_bookmarks( ::pPtr, nBlock )


METHOD HBQPlainTextEdit:nextBookmark( nBlock )
   RETURN Qt_HBQPlainTextEdit_nextBookmark( ::pPtr, nBlock )


METHOD HBQPlainTextEdit:prevBookmark( nBlock )
   RETURN Qt_HBQPlainTextEdit_prevBookmark( ::pPtr, nBlock )


METHOD HBQPlainTextEdit:gotoBookmark( nBlock )
   RETURN Qt_HBQPlainTextEdit_gotoBookmark( ::pPtr, nBlock )


METHOD HBQPlainTextEdit:numberBlockVisible( lB )
   RETURN Qt_HBQPlainTextEdit_numberBlockVisible( ::pPtr, lB )


METHOD HBQPlainTextEdit:numberBlockVisible_1()
   RETURN Qt_HBQPlainTextEdit_numberBlockVisible_1( ::pPtr )


METHOD HBQPlainTextEdit:highlightCurrentLine( lB )
   RETURN Qt_HBQPlainTextEdit_highlightCurrentLine( ::pPtr, lB )


METHOD HBQPlainTextEdit:highlightCurrentLine_1()
   RETURN Qt_HBQPlainTextEdit_highlightCurrentLine_1( ::pPtr )


METHOD HBQPlainTextEdit:updateLineNumberAreaWidth( nNewBlockCount )
   RETURN Qt_HBQPlainTextEdit_updateLineNumberAreaWidth( ::pPtr, nNewBlockCount )


METHOD HBQPlainTextEdit:caseUpper()
   RETURN Qt_HBQPlainTextEdit_caseUpper( ::pPtr )


METHOD HBQPlainTextEdit:caseLower()
   RETURN Qt_HBQPlainTextEdit_caseLower( ::pPtr )


METHOD HBQPlainTextEdit:escapeQuotes()
   RETURN Qt_HBQPlainTextEdit_escapeQuotes( ::pPtr )


METHOD HBQPlainTextEdit:escapeDQuotes()
   RETURN Qt_HBQPlainTextEdit_escapeDQuotes( ::pPtr )


METHOD HBQPlainTextEdit:unescapeQuotes()
   RETURN Qt_HBQPlainTextEdit_unescapeQuotes( ::pPtr )


METHOD HBQPlainTextEdit:unescapeDQuotes()
   RETURN Qt_HBQPlainTextEdit_unescapeDQuotes( ::pPtr )


METHOD HBQPlainTextEdit:convertQuotes()
   RETURN Qt_HBQPlainTextEdit_convertQuotes( ::pPtr )


METHOD HBQPlainTextEdit:convertDQuotes()
   RETURN Qt_HBQPlainTextEdit_convertDQuotes( ::pPtr )


METHOD HBQPlainTextEdit:blockComment()
   RETURN Qt_HBQPlainTextEdit_blockComment( ::pPtr )


METHOD HBQPlainTextEdit:streamComment()
   RETURN Qt_HBQPlainTextEdit_streamComment( ::pPtr )


METHOD HBQPlainTextEdit:duplicateLine()
   RETURN Qt_HBQPlainTextEdit_duplicateLine( ::pPtr )


METHOD HBQPlainTextEdit:replaceSelection( cTxt )
   RETURN Qt_HBQPlainTextEdit_replaceSelection( ::pPtr, cTxt )


METHOD HBQPlainTextEdit:blockIndent( nSteps )
   RETURN Qt_HBQPlainTextEdit_blockIndent( ::pPtr, nSteps )


METHOD HBQPlainTextEdit:deleteLine()
   RETURN Qt_HBQPlainTextEdit_deleteLine( ::pPtr )


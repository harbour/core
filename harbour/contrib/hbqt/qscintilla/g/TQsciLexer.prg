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


FUNCTION QsciLexer( ... )
   RETURN HB_QsciLexer():new( ... )


CREATE CLASS QsciLexer INHERIT HbQtObjectHandler FUNCTION HB_QsciLexer

   METHOD  new( ... )

   METHOD  language()
   METHOD  lexer()
   METHOD  lexerId()
   METHOD  apis()
   METHOD  autoCompletionFillups()
   METHOD  autoCompletionWordSeparators()
   METHOD  autoIndentStyle()
   METHOD  blockEnd( nStyle )
   METHOD  blockLookback()
   METHOD  blockStart( nStyle )
   METHOD  blockStartKeyword( nStyle )
   METHOD  braceStyle()
   METHOD  caseSensitive()
   METHOD  color( nStyle )
   METHOD  eolFill( nStyle )
   METHOD  font( nStyle )
   METHOD  indentationGuideView()
   METHOD  keywords( nSet )
   METHOD  defaultStyle()
   METHOD  description( nStyle )
   METHOD  paper( nStyle )
   METHOD  defaultColor()
   METHOD  defaultColor_1( nStyle )
   METHOD  defaultEolFill( nStyle )
   METHOD  defaultFont()
   METHOD  defaultFont_1( nStyle )
   METHOD  defaultPaper()
   METHOD  defaultPaper_1( nStyle )
   METHOD  editor()
   METHOD  setEditor( pEditor )
   METHOD  readSettings( pQs, pPrefix )
   METHOD  refreshProperties()
   METHOD  styleBitsNeeded()
   METHOD  wordCharacters()
   METHOD  writeSettings( pQs, pPrefix )
   METHOD  setAPIs( pApis )
   METHOD  setDefaultColor( pC )
   METHOD  setDefaultFont( pF )
   METHOD  setDefaultPaper( pC )
   METHOD  setAutoIndentStyle( nAutoindentstyle )
   METHOD  setColor( pC, nStyle )
   METHOD  setEolFill( lEoffill, nStyle )
   METHOD  setFont( pF, nStyle )
   METHOD  setPaper( pC, nStyle )

   ENDCLASS


METHOD QsciLexer:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexer( ... )
   RETURN Self


METHOD QsciLexer:language()
   RETURN Qt_QsciLexer_language( ::pPtr )


METHOD QsciLexer:lexer()
   RETURN Qt_QsciLexer_lexer( ::pPtr )


METHOD QsciLexer:lexerId()
   RETURN Qt_QsciLexer_lexerId( ::pPtr )


METHOD QsciLexer:apis()
   RETURN Qt_QsciLexer_apis( ::pPtr )


METHOD QsciLexer:autoCompletionFillups()
   RETURN Qt_QsciLexer_autoCompletionFillups( ::pPtr )


METHOD QsciLexer:autoCompletionWordSeparators()
   RETURN Qt_QsciLexer_autoCompletionWordSeparators( ::pPtr )


METHOD QsciLexer:autoIndentStyle()
   RETURN Qt_QsciLexer_autoIndentStyle( ::pPtr )


METHOD QsciLexer:blockEnd( nStyle )
   RETURN Qt_QsciLexer_blockEnd( ::pPtr, nStyle )


METHOD QsciLexer:blockLookback()
   RETURN Qt_QsciLexer_blockLookback( ::pPtr )


METHOD QsciLexer:blockStart( nStyle )
   RETURN Qt_QsciLexer_blockStart( ::pPtr, nStyle )


METHOD QsciLexer:blockStartKeyword( nStyle )
   RETURN Qt_QsciLexer_blockStartKeyword( ::pPtr, nStyle )


METHOD QsciLexer:braceStyle()
   RETURN Qt_QsciLexer_braceStyle( ::pPtr )


METHOD QsciLexer:caseSensitive()
   RETURN Qt_QsciLexer_caseSensitive( ::pPtr )


METHOD QsciLexer:color( nStyle )
   RETURN Qt_QsciLexer_color( ::pPtr, nStyle )


METHOD QsciLexer:eolFill( nStyle )
   RETURN Qt_QsciLexer_eolFill( ::pPtr, nStyle )


METHOD QsciLexer:font( nStyle )
   RETURN Qt_QsciLexer_font( ::pPtr, nStyle )


METHOD QsciLexer:indentationGuideView()
   RETURN Qt_QsciLexer_indentationGuideView( ::pPtr )


METHOD QsciLexer:keywords( nSet )
   RETURN Qt_QsciLexer_keywords( ::pPtr, nSet )


METHOD QsciLexer:defaultStyle()
   RETURN Qt_QsciLexer_defaultStyle( ::pPtr )


METHOD QsciLexer:description( nStyle )
   RETURN Qt_QsciLexer_description( ::pPtr, nStyle )


METHOD QsciLexer:paper( nStyle )
   RETURN Qt_QsciLexer_paper( ::pPtr, nStyle )


METHOD QsciLexer:defaultColor()
   RETURN Qt_QsciLexer_defaultColor( ::pPtr )


METHOD QsciLexer:defaultColor_1( nStyle )
   RETURN Qt_QsciLexer_defaultColor_1( ::pPtr, nStyle )


METHOD QsciLexer:defaultEolFill( nStyle )
   RETURN Qt_QsciLexer_defaultEolFill( ::pPtr, nStyle )


METHOD QsciLexer:defaultFont()
   RETURN Qt_QsciLexer_defaultFont( ::pPtr )


METHOD QsciLexer:defaultFont_1( nStyle )
   RETURN Qt_QsciLexer_defaultFont_1( ::pPtr, nStyle )


METHOD QsciLexer:defaultPaper()
   RETURN Qt_QsciLexer_defaultPaper( ::pPtr )


METHOD QsciLexer:defaultPaper_1( nStyle )
   RETURN Qt_QsciLexer_defaultPaper_1( ::pPtr, nStyle )


METHOD QsciLexer:editor()
   RETURN Qt_QsciLexer_editor( ::pPtr )


METHOD QsciLexer:setEditor( pEditor )
   RETURN Qt_QsciLexer_setEditor( ::pPtr, hbqt_ptr( pEditor ) )


METHOD QsciLexer:readSettings( pQs, pPrefix )
   RETURN Qt_QsciLexer_readSettings( ::pPtr, hbqt_ptr( pQs ), hbqt_ptr( pPrefix ) )


METHOD QsciLexer:refreshProperties()
   RETURN Qt_QsciLexer_refreshProperties( ::pPtr )


METHOD QsciLexer:styleBitsNeeded()
   RETURN Qt_QsciLexer_styleBitsNeeded( ::pPtr )


METHOD QsciLexer:wordCharacters()
   RETURN Qt_QsciLexer_wordCharacters( ::pPtr )


METHOD QsciLexer:writeSettings( pQs, pPrefix )
   RETURN Qt_QsciLexer_writeSettings( ::pPtr, hbqt_ptr( pQs ), hbqt_ptr( pPrefix ) )


METHOD QsciLexer:setAPIs( pApis )
   RETURN Qt_QsciLexer_setAPIs( ::pPtr, hbqt_ptr( pApis ) )


METHOD QsciLexer:setDefaultColor( pC )
   RETURN Qt_QsciLexer_setDefaultColor( ::pPtr, hbqt_ptr( pC ) )


METHOD QsciLexer:setDefaultFont( pF )
   RETURN Qt_QsciLexer_setDefaultFont( ::pPtr, hbqt_ptr( pF ) )


METHOD QsciLexer:setDefaultPaper( pC )
   RETURN Qt_QsciLexer_setDefaultPaper( ::pPtr, hbqt_ptr( pC ) )


METHOD QsciLexer:setAutoIndentStyle( nAutoindentstyle )
   RETURN Qt_QsciLexer_setAutoIndentStyle( ::pPtr, nAutoindentstyle )


METHOD QsciLexer:setColor( pC, nStyle )
   RETURN Qt_QsciLexer_setColor( ::pPtr, hbqt_ptr( pC ), nStyle )


METHOD QsciLexer:setEolFill( lEoffill, nStyle )
   RETURN Qt_QsciLexer_setEolFill( ::pPtr, lEoffill, nStyle )


METHOD QsciLexer:setFont( pF, nStyle )
   RETURN Qt_QsciLexer_setFont( ::pPtr, hbqt_ptr( pF ), nStyle )


METHOD QsciLexer:setPaper( pC, nStyle )
   RETURN Qt_QsciLexer_setPaper( ::pPtr, hbqt_ptr( pC ), nStyle )


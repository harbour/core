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


FUNCTION QsciLexerCPP( ... )
   RETURN HB_QsciLexerCPP():new( ... )


CREATE CLASS QsciLexerCPP INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerCPP

   METHOD  new( ... )

   METHOD  language()
   METHOD  lexer()
   METHOD  autoCompletionWordSeparators()
   METHOD  blockEnd( nStyle )
   METHOD  blockStart( nStyle )
   METHOD  blockStartKeyword( nStyle )
   METHOD  braceStyle()
   METHOD  wordCharacters()
   METHOD  defaultColor( nStyle )
   METHOD  defaultEolFill( nStyle )
   METHOD  defaultFont( nStyle )
   METHOD  defaultPaper( nStyle )
   METHOD  keywords( nSet )
   METHOD  description( nStyle )
   METHOD  refreshProperties()
   METHOD  foldAtElse()
   METHOD  foldComments()
   METHOD  foldCompact()
   METHOD  foldPreprocessor()
   METHOD  stylePreprocessor()
   METHOD  setDollarsAllowed( lAllowed )
   METHOD  dollarsAllowed()
   METHOD  setFoldAtElse( lFold )
   METHOD  setFoldComments( lFold )
   METHOD  setFoldCompact( lFold )
   METHOD  setFoldPreprocessor( lFold )
   METHOD  setStylePreprocessor( lStyle )

   ENDCLASS


METHOD QsciLexerCPP:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerCPP( ... )
   RETURN Self


METHOD QsciLexerCPP:language()
   RETURN Qt_QsciLexerCPP_language( ::pPtr )


METHOD QsciLexerCPP:lexer()
   RETURN Qt_QsciLexerCPP_lexer( ::pPtr )


METHOD QsciLexerCPP:autoCompletionWordSeparators()
   RETURN Qt_QsciLexerCPP_autoCompletionWordSeparators( ::pPtr )


METHOD QsciLexerCPP:blockEnd( nStyle )
   RETURN Qt_QsciLexerCPP_blockEnd( ::pPtr, nStyle )


METHOD QsciLexerCPP:blockStart( nStyle )
   RETURN Qt_QsciLexerCPP_blockStart( ::pPtr, nStyle )


METHOD QsciLexerCPP:blockStartKeyword( nStyle )
   RETURN Qt_QsciLexerCPP_blockStartKeyword( ::pPtr, nStyle )


METHOD QsciLexerCPP:braceStyle()
   RETURN Qt_QsciLexerCPP_braceStyle( ::pPtr )


METHOD QsciLexerCPP:wordCharacters()
   RETURN Qt_QsciLexerCPP_wordCharacters( ::pPtr )


METHOD QsciLexerCPP:defaultColor( nStyle )
   RETURN Qt_QsciLexerCPP_defaultColor( ::pPtr, nStyle )


METHOD QsciLexerCPP:defaultEolFill( nStyle )
   RETURN Qt_QsciLexerCPP_defaultEolFill( ::pPtr, nStyle )


METHOD QsciLexerCPP:defaultFont( nStyle )
   RETURN Qt_QsciLexerCPP_defaultFont( ::pPtr, nStyle )


METHOD QsciLexerCPP:defaultPaper( nStyle )
   RETURN Qt_QsciLexerCPP_defaultPaper( ::pPtr, nStyle )


METHOD QsciLexerCPP:keywords( nSet )
   RETURN Qt_QsciLexerCPP_keywords( ::pPtr, nSet )


METHOD QsciLexerCPP:description( nStyle )
   RETURN Qt_QsciLexerCPP_description( ::pPtr, nStyle )


METHOD QsciLexerCPP:refreshProperties()
   RETURN Qt_QsciLexerCPP_refreshProperties( ::pPtr )


METHOD QsciLexerCPP:foldAtElse()
   RETURN Qt_QsciLexerCPP_foldAtElse( ::pPtr )


METHOD QsciLexerCPP:foldComments()
   RETURN Qt_QsciLexerCPP_foldComments( ::pPtr )


METHOD QsciLexerCPP:foldCompact()
   RETURN Qt_QsciLexerCPP_foldCompact( ::pPtr )


METHOD QsciLexerCPP:foldPreprocessor()
   RETURN Qt_QsciLexerCPP_foldPreprocessor( ::pPtr )


METHOD QsciLexerCPP:stylePreprocessor()
   RETURN Qt_QsciLexerCPP_stylePreprocessor( ::pPtr )


METHOD QsciLexerCPP:setDollarsAllowed( lAllowed )
   RETURN Qt_QsciLexerCPP_setDollarsAllowed( ::pPtr, lAllowed )


METHOD QsciLexerCPP:dollarsAllowed()
   RETURN Qt_QsciLexerCPP_dollarsAllowed( ::pPtr )


METHOD QsciLexerCPP:setFoldAtElse( lFold )
   RETURN Qt_QsciLexerCPP_setFoldAtElse( ::pPtr, lFold )


METHOD QsciLexerCPP:setFoldComments( lFold )
   RETURN Qt_QsciLexerCPP_setFoldComments( ::pPtr, lFold )


METHOD QsciLexerCPP:setFoldCompact( lFold )
   RETURN Qt_QsciLexerCPP_setFoldCompact( ::pPtr, lFold )


METHOD QsciLexerCPP:setFoldPreprocessor( lFold )
   RETURN Qt_QsciLexerCPP_setFoldPreprocessor( ::pPtr, lFold )


METHOD QsciLexerCPP:setStylePreprocessor( lStyle )
   RETURN Qt_QsciLexerCPP_setStylePreprocessor( ::pPtr, lStyle )


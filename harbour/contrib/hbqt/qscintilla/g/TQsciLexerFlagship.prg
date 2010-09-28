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


FUNCTION QsciLexerFlagship( ... )
   RETURN HB_QsciLexerFlagship():new( ... )


CREATE CLASS QsciLexerFlagship INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerFlagship

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


METHOD QsciLexerFlagship:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerFlagship( ... )
   RETURN Self


METHOD QsciLexerFlagship:language()
   RETURN Qt_QsciLexerFlagship_language( ::pPtr )


METHOD QsciLexerFlagship:lexer()
   RETURN Qt_QsciLexerFlagship_lexer( ::pPtr )


METHOD QsciLexerFlagship:autoCompletionWordSeparators()
   RETURN HB_QStringList():from( Qt_QsciLexerFlagship_autoCompletionWordSeparators( ::pPtr ) )


METHOD QsciLexerFlagship:blockEnd( nStyle )
   RETURN Qt_QsciLexerFlagship_blockEnd( ::pPtr, nStyle )


METHOD QsciLexerFlagship:blockStart( nStyle )
   RETURN Qt_QsciLexerFlagship_blockStart( ::pPtr, nStyle )


METHOD QsciLexerFlagship:blockStartKeyword( nStyle )
   RETURN Qt_QsciLexerFlagship_blockStartKeyword( ::pPtr, nStyle )


METHOD QsciLexerFlagship:braceStyle()
   RETURN Qt_QsciLexerFlagship_braceStyle( ::pPtr )


METHOD QsciLexerFlagship:wordCharacters()
   RETURN Qt_QsciLexerFlagship_wordCharacters( ::pPtr )


METHOD QsciLexerFlagship:defaultColor( nStyle )
   RETURN HB_QColor():from( Qt_QsciLexerFlagship_defaultColor( ::pPtr, nStyle ) )


METHOD QsciLexerFlagship:defaultEolFill( nStyle )
   RETURN Qt_QsciLexerFlagship_defaultEolFill( ::pPtr, nStyle )


METHOD QsciLexerFlagship:defaultFont( nStyle )
   RETURN HB_QFont():from( Qt_QsciLexerFlagship_defaultFont( ::pPtr, nStyle ) )


METHOD QsciLexerFlagship:defaultPaper( nStyle )
   RETURN HB_QColor():from( Qt_QsciLexerFlagship_defaultPaper( ::pPtr, nStyle ) )


METHOD QsciLexerFlagship:keywords( nSet )
   RETURN Qt_QsciLexerFlagship_keywords( ::pPtr, nSet )


METHOD QsciLexerFlagship:description( nStyle )
   RETURN Qt_QsciLexerFlagship_description( ::pPtr, nStyle )


METHOD QsciLexerFlagship:refreshProperties()
   RETURN Qt_QsciLexerFlagship_refreshProperties( ::pPtr )


METHOD QsciLexerFlagship:foldAtElse()
   RETURN Qt_QsciLexerFlagship_foldAtElse( ::pPtr )


METHOD QsciLexerFlagship:foldComments()
   RETURN Qt_QsciLexerFlagship_foldComments( ::pPtr )


METHOD QsciLexerFlagship:foldCompact()
   RETURN Qt_QsciLexerFlagship_foldCompact( ::pPtr )


METHOD QsciLexerFlagship:foldPreprocessor()
   RETURN Qt_QsciLexerFlagship_foldPreprocessor( ::pPtr )


METHOD QsciLexerFlagship:stylePreprocessor()
   RETURN Qt_QsciLexerFlagship_stylePreprocessor( ::pPtr )


METHOD QsciLexerFlagship:setDollarsAllowed( lAllowed )
   RETURN Qt_QsciLexerFlagship_setDollarsAllowed( ::pPtr, lAllowed )


METHOD QsciLexerFlagship:dollarsAllowed()
   RETURN Qt_QsciLexerFlagship_dollarsAllowed( ::pPtr )


METHOD QsciLexerFlagship:setFoldAtElse( lFold )
   RETURN Qt_QsciLexerFlagship_setFoldAtElse( ::pPtr, lFold )


METHOD QsciLexerFlagship:setFoldComments( lFold )
   RETURN Qt_QsciLexerFlagship_setFoldComments( ::pPtr, lFold )


METHOD QsciLexerFlagship:setFoldCompact( lFold )
   RETURN Qt_QsciLexerFlagship_setFoldCompact( ::pPtr, lFold )


METHOD QsciLexerFlagship:setFoldPreprocessor( lFold )
   RETURN Qt_QsciLexerFlagship_setFoldPreprocessor( ::pPtr, lFold )


METHOD QsciLexerFlagship:setStylePreprocessor( lStyle )
   RETURN Qt_QsciLexerFlagship_setStylePreprocessor( ::pPtr, lStyle )


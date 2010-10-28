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


REQUEST __HBQSCINTILLA


FUNCTION QsciLexerFlagship( ... )
   RETURN HB_QsciLexerFlagship():new( ... )

FUNCTION QsciLexerFlagshipFromPointer( ... )
   RETURN HB_QsciLexerFlagship():fromPointer( ... )


CREATE CLASS QsciLexerFlagship INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerFlagship

   METHOD  new( ... )

   METHOD  language                      // (  )                                               -> cChar
   METHOD  lexer                         // (  )                                               -> cChar
   METHOD  autoCompletionWordSeparators  // (  )                                               -> oQStringList
   METHOD  blockEnd                      // ( @nStyle )                                        -> cChar
   METHOD  blockStart                    // ( @nStyle )                                        -> cChar
   METHOD  blockStartKeyword             // ( @nStyle )                                        -> cChar
   METHOD  braceStyle                    // (  )                                               -> nInt
   METHOD  wordCharacters                // (  )                                               -> cChar
   METHOD  defaultColor                  // ( nStyle )                                         -> oQColor
   METHOD  defaultEolFill                // ( nStyle )                                         -> lBool
   METHOD  defaultFont                   // ( nStyle )                                         -> oQFont
   METHOD  defaultPaper                  // ( nStyle )                                         -> oQColor
   METHOD  keywords                      // ( nSet )                                           -> cChar
   METHOD  description                   // ( nStyle )                                         -> cQString
   METHOD  refreshProperties             // (  )                                               -> NIL
   METHOD  foldAtElse                    // (  )                                               -> lBool
   METHOD  foldComments                  // (  )                                               -> lBool
   METHOD  foldCompact                   // (  )                                               -> lBool
   METHOD  foldPreprocessor              // (  )                                               -> lBool
   METHOD  stylePreprocessor             // (  )                                               -> lBool
   METHOD  setDollarsAllowed             // ( lAllowed )                                       -> NIL
   METHOD  dollarsAllowed                // (  )                                               -> lBool
   METHOD  setFoldAtElse                 // ( lFold )                                          -> NIL
   METHOD  setFoldComments               // ( lFold )                                          -> NIL
   METHOD  setFoldCompact                // ( lFold )                                          -> NIL
   METHOD  setFoldPreprocessor           // ( lFold )                                          -> NIL
   METHOD  setStylePreprocessor          // ( lStyle )                                         -> NIL

   ENDCLASS


METHOD QsciLexerFlagship:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerFlagship( ... )
   RETURN Self


METHOD QsciLexerFlagship:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_lexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:autoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciLexerFlagship_autoCompletionWordSeparators( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockEnd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockEnd( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockStart( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockStart( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:blockStartKeyword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_blockStartKeyword( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerFlagship_blockStartKeyword( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:braceStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_braceStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerFlagship_defaultColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_defaultEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexerFlagship_defaultFont( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:defaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerFlagship_defaultPaper( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:keywords( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_keywords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:description( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_description( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:refreshProperties( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_refreshProperties( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldAtElse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldAtElse( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldComments( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldComments( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldCompact( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldCompact( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:foldPreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_foldPreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:stylePreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_stylePreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setDollarsAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setDollarsAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:dollarsAllowed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerFlagship_dollarsAllowed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldAtElse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldAtElse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldComments( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldComments( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldCompact( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldCompact( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setFoldPreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setFoldPreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerFlagship:setStylePreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerFlagship_setStylePreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


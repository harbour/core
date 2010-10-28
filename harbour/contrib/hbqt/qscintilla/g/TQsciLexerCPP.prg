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


FUNCTION QsciLexerCPP( ... )
   RETURN HB_QsciLexerCPP():new( ... )

FUNCTION QsciLexerCPPFromPointer( ... )
   RETURN HB_QsciLexerCPP():fromPointer( ... )


CREATE CLASS QsciLexerCPP INHERIT HbQtObjectHandler, HB_QsciLexer FUNCTION HB_QsciLexerCPP

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


METHOD QsciLexerCPP:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QsciLexerCPP( ... )
   RETURN Self


METHOD QsciLexerCPP:language( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_language( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:lexer( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_lexer( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:autoCompletionWordSeparators( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QsciLexerCPP_autoCompletionWordSeparators( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockEnd( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockEnd( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockStart( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockStart( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockStart( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:blockStartKeyword( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_blockStartKeyword( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QsciLexerCPP_blockStartKeyword( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:braceStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_braceStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:wordCharacters( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_wordCharacters( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerCPP_defaultColor( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultEolFill( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_defaultEolFill( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QFontFromPointer( Qt_QsciLexerCPP_defaultFont( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:defaultPaper( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QColorFromPointer( Qt_QsciLexerCPP_defaultPaper( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:keywords( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_keywords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:description( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_description( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:refreshProperties( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_refreshProperties( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldAtElse( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldAtElse( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldComments( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldComments( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldCompact( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldCompact( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:foldPreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_foldPreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:stylePreprocessor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_stylePreprocessor( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setDollarsAllowed( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setDollarsAllowed( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:dollarsAllowed( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QsciLexerCPP_dollarsAllowed( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldAtElse( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldAtElse( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldComments( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldComments( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldCompact( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldCompact( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setFoldPreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setFoldPreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QsciLexerCPP:setStylePreprocessor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QsciLexerCPP_setStylePreprocessor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


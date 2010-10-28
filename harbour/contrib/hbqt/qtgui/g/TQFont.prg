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


REQUEST __HBQTGUI


FUNCTION QFont( ... )
   RETURN HB_QFont():new( ... )

FUNCTION QFontFromPointer( ... )
   RETURN HB_QFont():fromPointer( ... )


CREATE CLASS QFont INHERIT HbQtObjectHandler FUNCTION HB_QFont

   METHOD  new( ... )

   METHOD  bold                          // (  )                                               -> lBool
   METHOD  capitalization                // (  )                                               -> nCapitalization
   METHOD  defaultFamily                 // (  )                                               -> cQString
   METHOD  exactMatch                    // (  )                                               -> lBool
   METHOD  family                        // (  )                                               -> cQString
   METHOD  fixedPitch                    // (  )                                               -> lBool
   METHOD  fromString                    // ( cDescrip )                                       -> lBool
   METHOD  italic                        // (  )                                               -> lBool
   METHOD  kerning                       // (  )                                               -> lBool
   METHOD  key                           // (  )                                               -> cQString
   METHOD  lastResortFamily              // (  )                                               -> cQString
   METHOD  lastResortFont                // (  )                                               -> cQString
   METHOD  letterSpacing                 // (  )                                               -> nQreal
   METHOD  letterSpacingType             // (  )                                               -> nSpacingType
   METHOD  overline                      // (  )                                               -> lBool
   METHOD  pixelSize                     // (  )                                               -> nInt
   METHOD  pointSize                     // (  )                                               -> nInt
   METHOD  pointSizeF                    // (  )                                               -> nQreal
   METHOD  rawMode                       // (  )                                               -> lBool
   METHOD  rawName                       // (  )                                               -> cQString
   METHOD  setBold                       // ( lEnable )                                        -> NIL
   METHOD  setCapitalization             // ( nCaps )                                          -> NIL
   METHOD  setFamily                     // ( cFamily )                                        -> NIL
   METHOD  setFixedPitch                 // ( lEnable )                                        -> NIL
   METHOD  setItalic                     // ( lEnable )                                        -> NIL
   METHOD  setKerning                    // ( lEnable )                                        -> NIL
   METHOD  setLetterSpacing              // ( nType, nSpacing )                                -> NIL
   METHOD  setOverline                   // ( lEnable )                                        -> NIL
   METHOD  setPixelSize                  // ( nPixelSize )                                     -> NIL
   METHOD  setPointSize                  // ( nPointSize )                                     -> NIL
   METHOD  setPointSizeF                 // ( nPointSize )                                     -> NIL
   METHOD  setRawMode                    // ( lEnable )                                        -> NIL
   METHOD  setRawName                    // ( cName )                                          -> NIL
   METHOD  setStretch                    // ( nFactor )                                        -> NIL
   METHOD  setStrikeOut                  // ( lEnable )                                        -> NIL
   METHOD  setStyle                      // ( nStyle )                                         -> NIL
   METHOD  setStyleHint                  // ( nHint, nStrategy )                               -> NIL
   METHOD  setStyleStrategy              // ( nS )                                             -> NIL
   METHOD  setUnderline                  // ( lEnable )                                        -> NIL
   METHOD  setWeight                     // ( nWeight )                                        -> NIL
   METHOD  setWordSpacing                // ( nSpacing )                                       -> NIL
   METHOD  stretch                       // (  )                                               -> nInt
   METHOD  strikeOut                     // (  )                                               -> lBool
   METHOD  style                         // (  )                                               -> nStyle
   METHOD  styleHint                     // (  )                                               -> nStyleHint
   METHOD  styleStrategy                 // (  )                                               -> nStyleStrategy
   METHOD  toString                      // (  )                                               -> cQString
   METHOD  underline                     // (  )                                               -> lBool
   METHOD  weight                        // (  )                                               -> nInt
   METHOD  wordSpacing                   // (  )                                               -> nQreal
   METHOD  cleanup                       // (  )                                               -> NIL
   METHOD  initialize                    // (  )                                               -> NIL
   METHOD  insertSubstitution            // ( cFamilyName, cSubstituteName )                   -> NIL
   METHOD  insertSubstitutions           // ( cFamilyName, oQStringList )                      -> NIL
   METHOD  removeSubstitution            // ( cFamilyName )                                    -> NIL
   METHOD  substitute                    // ( cFamilyName )                                    -> cQString
   METHOD  substitutes                   // ( cFamilyName )                                    -> oQStringList
   METHOD  substitutions                 // (  )                                               -> oQStringList

   ENDCLASS


METHOD QFont:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFont( ... )
   RETURN Self


METHOD QFont:bold( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_bold( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:capitalization( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_capitalization( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:defaultFamily( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_defaultFamily( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:exactMatch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_exactMatch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:family( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_family( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:fixedPitch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_fixedPitch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:fromString( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFont_fromString( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:italic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_italic( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:kerning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_kerning( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:key( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_key( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:lastResortFamily( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_lastResortFamily( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:lastResortFont( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_lastResortFont( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:letterSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_letterSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:letterSpacingType( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_letterSpacingType( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:overline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_overline( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:pixelSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_pixelSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:pointSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_pointSize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:pointSizeF( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_pointSizeF( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:rawMode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_rawMode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:rawName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_rawName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setBold( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setBold( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setCapitalization( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setCapitalization( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setFixedPitch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setFixedPitch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setItalic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setItalic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setKerning( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setKerning( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setLetterSpacing( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFont_setLetterSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setOverline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setOverline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setPixelSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setPixelSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setPointSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setPointSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setPointSizeF( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setPointSizeF( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setRawMode( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setRawMode( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setRawName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setRawName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setStretch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setStretch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setStrikeOut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setStrikeOut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setStyleHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFont_setStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setStyleStrategy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setStyleStrategy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setUnderline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setUnderline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setWeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setWeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:setWordSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFont_setWordSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:stretch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_stretch( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:strikeOut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_strikeOut( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:style( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_style( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:styleHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_styleHint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:styleStrategy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_styleStrategy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:toString( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_toString( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:underline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_underline( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:weight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_weight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:wordSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_wordSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:cleanup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_cleanup( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:initialize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFont_initialize( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:insertSubstitution( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFont_insertSubstitution( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:insertSubstitutions( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QFont_insertSubstitutions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:removeSubstitution( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFont_removeSubstitution( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:substitute( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFont_substitute( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:substitutes( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QStringListFromPointer( Qt_QFont_substitutes( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFont:substitutions( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QFont_substitutions( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


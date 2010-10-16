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


FUNCTION QTextCharFormat( ... )
   RETURN HB_QTextCharFormat():new( ... )

FUNCTION QTextCharFormatFrom( ... )
   RETURN HB_QTextCharFormat():from( ... )

FUNCTION QTextCharFormatFromPointer( ... )
   RETURN HB_QTextCharFormat():fromPointer( ... )


CREATE CLASS QTextCharFormat INHERIT HbQtObjectHandler, HB_QTextFormat FUNCTION HB_QTextCharFormat

   METHOD  new( ... )

   METHOD  anchorHref                    // (  )                                               -> cQString
   METHOD  anchorNames                   // (  )                                               -> oQStringList
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  fontCapitalization            // (  )                                               -> nQFont_Capitalization
   METHOD  fontFamily                    // (  )                                               -> cQString
   METHOD  fontFixedPitch                // (  )                                               -> lBool
   METHOD  fontItalic                    // (  )                                               -> lBool
   METHOD  fontKerning                   // (  )                                               -> lBool
   METHOD  fontLetterSpacing             // (  )                                               -> nQreal
   METHOD  fontOverline                  // (  )                                               -> lBool
   METHOD  fontPointSize                 // (  )                                               -> nQreal
   METHOD  fontStrikeOut                 // (  )                                               -> lBool
   METHOD  fontStyleHint                 // (  )                                               -> nQFont_StyleHint
   METHOD  fontStyleStrategy             // (  )                                               -> nQFont_StyleStrategy
   METHOD  fontUnderline                 // (  )                                               -> lBool
   METHOD  fontWeight                    // (  )                                               -> nInt
   METHOD  fontWordSpacing               // (  )                                               -> nQreal
   METHOD  isAnchor                      // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  setAnchor                     // ( lAnchor )                                        -> NIL
   METHOD  setAnchorHref                 // ( cValue )                                         -> NIL
   METHOD  setAnchorNames                // ( oQStringList )                                   -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setFontCapitalization         // ( nCapitalization )                                -> NIL
   METHOD  setFontFamily                 // ( cFamily )                                        -> NIL
   METHOD  setFontFixedPitch             // ( lFixedPitch )                                    -> NIL
   METHOD  setFontItalic                 // ( lItalic )                                        -> NIL
   METHOD  setFontKerning                // ( lEnable )                                        -> NIL
   METHOD  setFontLetterSpacing          // ( nSpacing )                                       -> NIL
   METHOD  setFontOverline               // ( lOverline )                                      -> NIL
   METHOD  setFontPointSize              // ( nSize )                                          -> NIL
   METHOD  setFontStrikeOut              // ( lStrikeOut )                                     -> NIL
   METHOD  setFontStyleHint              // ( nHint, nStrategy )                               -> NIL
   METHOD  setFontStyleStrategy          // ( nStrategy )                                      -> NIL
   METHOD  setFontUnderline              // ( lUnderline )                                     -> NIL
   METHOD  setFontWeight                 // ( nWeight )                                        -> NIL
   METHOD  setFontWordSpacing            // ( nSpacing )                                       -> NIL
   METHOD  setTextOutline                // ( oQPen )                                          -> NIL
   METHOD  setToolTip                    // ( cText )                                          -> NIL
   METHOD  setUnderlineColor             // ( oQColor )                                        -> NIL
   METHOD  setUnderlineStyle             // ( nStyle )                                         -> NIL
   METHOD  setVerticalAlignment          // ( nAlignment )                                     -> NIL
   METHOD  textOutline                   // (  )                                               -> oQPen
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  underlineColor                // (  )                                               -> oQColor
   METHOD  underlineStyle                // (  )                                               -> nUnderlineStyle
   METHOD  verticalAlignment             // (  )                                               -> nVerticalAlignment

   ENDCLASS


METHOD QTextCharFormat:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextCharFormat( ... )
   RETURN Self


METHOD QTextCharFormat:anchorHref( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_anchorHref( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:anchorNames( ... )
   SWITCH PCount()
   CASE 0
      RETURN QStringListFromPointer( Qt_QTextCharFormat_anchorNames( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTextCharFormat_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontCapitalization( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontCapitalization( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontFamily( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontFamily( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontFixedPitch( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontFixedPitch( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontItalic( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontItalic( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontKerning( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontKerning( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontLetterSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontLetterSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontOverline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontOverline( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontPointSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontPointSize( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontStrikeOut( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStrikeOut( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontStyleHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStyleHint( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontStyleStrategy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontStyleStrategy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontUnderline( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontUnderline( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontWeight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontWeight( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:fontWordSpacing( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_fontWordSpacing( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:isAnchor( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_isAnchor( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setAnchor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setAnchorHref( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchorHref( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setAnchorNames( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setAnchorNames( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontCapitalization( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontCapitalization( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontFamily( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontFamily( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontFixedPitch( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontFixedPitch( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontItalic( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontItalic( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontKerning( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontKerning( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontLetterSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontLetterSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontOverline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontOverline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontPointSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontPointSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontStrikeOut( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStrikeOut( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontStyleHint( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QTextCharFormat_setFontStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStyleHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontStyleStrategy( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontStyleStrategy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontUnderline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontUnderline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontWeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontWeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setFontWordSpacing( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setFontWordSpacing( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setTextOutline( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setTextOutline( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setUnderlineColor( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setUnderlineColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setUnderlineStyle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setUnderlineStyle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:setVerticalAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTextCharFormat_setVerticalAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:textOutline( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPenFromPointer( Qt_QTextCharFormat_textOutline( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:underlineColor( ... )
   SWITCH PCount()
   CASE 0
      RETURN QColorFromPointer( Qt_QTextCharFormat_underlineColor( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:underlineStyle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_underlineStyle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QTextCharFormat:verticalAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTextCharFormat_verticalAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


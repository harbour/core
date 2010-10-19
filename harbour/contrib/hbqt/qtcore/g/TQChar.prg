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


FUNCTION QChar( ... )
   RETURN HB_QChar():new( ... )

FUNCTION QCharFromPointer( ... )
   RETURN HB_QChar():fromPointer( ... )


CREATE CLASS QChar INHERIT HbQtObjectHandler FUNCTION HB_QChar

   METHOD  new( ... )

   METHOD  category                      // (  )                                               -> nCategory
   METHOD  combiningClass                // (  )                                               -> nChar
   METHOD  decomposition                 // (  )                                               -> cQString
   METHOD  decompositionTag              // (  )                                               -> nDecomposition
   METHOD  digitValue                    // (  )                                               -> nInt
   METHOD  direction                     // (  )                                               -> nDirection
   METHOD  hasMirrored                   // (  )                                               -> lBool
   METHOD  isDigit                       // (  )                                               -> lBool
   METHOD  isHighSurrogate               // (  )                                               -> lBool
   METHOD  isLetter                      // (  )                                               -> lBool
   METHOD  isLetterOrNumber              // (  )                                               -> lBool
   METHOD  isLowSurrogate                // (  )                                               -> lBool
   METHOD  isLower                       // (  )                                               -> lBool
   METHOD  isMark                        // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isNumber                      // (  )                                               -> lBool
   METHOD  isPrint                       // (  )                                               -> lBool
   METHOD  isPunct                       // (  )                                               -> lBool
   METHOD  isSpace                       // (  )                                               -> lBool
   METHOD  isSymbol                      // (  )                                               -> lBool
   METHOD  isTitleCase                   // (  )                                               -> lBool
   METHOD  isUpper                       // (  )                                               -> lBool
   METHOD  joining                       // (  )                                               -> nJoining
   METHOD  mirroredChar                  // (  )                                               -> oQChar
   METHOD  toAscii                       // (  )                                               -> cChar
   METHOD  toCaseFolded                  // (  )                                               -> oQChar
   METHOD  toLatin1                      // (  )                                               -> cChar
   METHOD  toLower                       // (  )                                               -> oQChar
   METHOD  toTitleCase                   // (  )                                               -> oQChar
   METHOD  toUpper                       // (  )                                               -> oQChar
   METHOD  unicode                       // (  )                                               -> nUshort
   METHOD  unicodeVersion                // (  )                                               -> nUnicodeVersion

   ENDCLASS


METHOD QChar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QChar( ... )
   RETURN Self


METHOD QChar:category( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_category( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:combiningClass( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_combiningClass( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:decomposition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_decomposition( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:decompositionTag( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_decompositionTag( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:digitValue( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_digitValue( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:direction( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_direction( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:hasMirrored( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_hasMirrored( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isDigit( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isDigit( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isHighSurrogate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isHighSurrogate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLetter( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLetter( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLetterOrNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLetterOrNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLowSurrogate( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLowSurrogate( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isLower( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isMark( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isMark( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isNumber( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isNumber( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isPrint( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isPrint( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isPunct( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isPunct( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isSpace( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isSpace( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isSymbol( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isSymbol( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isTitleCase( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isTitleCase( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:isUpper( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_isUpper( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:joining( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_joining( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:mirroredChar( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_mirroredChar( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toAscii( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_toAscii( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toCaseFolded( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toCaseFolded( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toLatin1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_toLatin1( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toLower( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toLower( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toTitleCase( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toTitleCase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:toUpper( ... )
   SWITCH PCount()
   CASE 0
      RETURN QCharFromPointer( Qt_QChar_toUpper( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:unicode( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_unicode( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QChar:unicodeVersion( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QChar_unicodeVersion( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


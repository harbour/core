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


FUNCTION QChar( ... )
   RETURN HB_QChar():new( ... )


CREATE CLASS QChar INHERIT HbQtObjectHandler FUNCTION HB_QChar

   METHOD  new( ... )

   METHOD  category()
   METHOD  combiningClass()
   METHOD  decomposition()
   METHOD  decompositionTag()
   METHOD  digitValue()
   METHOD  direction()
   METHOD  hasMirrored()
   METHOD  isDigit()
   METHOD  isHighSurrogate()
   METHOD  isLetter()
   METHOD  isLetterOrNumber()
   METHOD  isLowSurrogate()
   METHOD  isLower()
   METHOD  isMark()
   METHOD  isNull()
   METHOD  isNumber()
   METHOD  isPrint()
   METHOD  isPunct()
   METHOD  isSpace()
   METHOD  isSymbol()
   METHOD  isTitleCase()
   METHOD  isUpper()
   METHOD  joining()
   METHOD  mirroredChar()
   METHOD  toAscii()
   METHOD  toCaseFolded()
   METHOD  toLatin1()
   METHOD  toLower()
   METHOD  toTitleCase()
   METHOD  toUpper()
   METHOD  unicode()
   METHOD  unicodeVersion()

   ENDCLASS


METHOD QChar:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QChar( ... )
   RETURN Self


METHOD QChar:category()
   RETURN Qt_QChar_category( ::pPtr )


METHOD QChar:combiningClass()
   RETURN Qt_QChar_combiningClass( ::pPtr )


METHOD QChar:decomposition()
   RETURN Qt_QChar_decomposition( ::pPtr )


METHOD QChar:decompositionTag()
   RETURN Qt_QChar_decompositionTag( ::pPtr )


METHOD QChar:digitValue()
   RETURN Qt_QChar_digitValue( ::pPtr )


METHOD QChar:direction()
   RETURN Qt_QChar_direction( ::pPtr )


METHOD QChar:hasMirrored()
   RETURN Qt_QChar_hasMirrored( ::pPtr )


METHOD QChar:isDigit()
   RETURN Qt_QChar_isDigit( ::pPtr )


METHOD QChar:isHighSurrogate()
   RETURN Qt_QChar_isHighSurrogate( ::pPtr )


METHOD QChar:isLetter()
   RETURN Qt_QChar_isLetter( ::pPtr )


METHOD QChar:isLetterOrNumber()
   RETURN Qt_QChar_isLetterOrNumber( ::pPtr )


METHOD QChar:isLowSurrogate()
   RETURN Qt_QChar_isLowSurrogate( ::pPtr )


METHOD QChar:isLower()
   RETURN Qt_QChar_isLower( ::pPtr )


METHOD QChar:isMark()
   RETURN Qt_QChar_isMark( ::pPtr )


METHOD QChar:isNull()
   RETURN Qt_QChar_isNull( ::pPtr )


METHOD QChar:isNumber()
   RETURN Qt_QChar_isNumber( ::pPtr )


METHOD QChar:isPrint()
   RETURN Qt_QChar_isPrint( ::pPtr )


METHOD QChar:isPunct()
   RETURN Qt_QChar_isPunct( ::pPtr )


METHOD QChar:isSpace()
   RETURN Qt_QChar_isSpace( ::pPtr )


METHOD QChar:isSymbol()
   RETURN Qt_QChar_isSymbol( ::pPtr )


METHOD QChar:isTitleCase()
   RETURN Qt_QChar_isTitleCase( ::pPtr )


METHOD QChar:isUpper()
   RETURN Qt_QChar_isUpper( ::pPtr )


METHOD QChar:joining()
   RETURN Qt_QChar_joining( ::pPtr )


METHOD QChar:mirroredChar()
   RETURN Qt_QChar_mirroredChar( ::pPtr )


METHOD QChar:toAscii()
   RETURN Qt_QChar_toAscii( ::pPtr )


METHOD QChar:toCaseFolded()
   RETURN Qt_QChar_toCaseFolded( ::pPtr )


METHOD QChar:toLatin1()
   RETURN Qt_QChar_toLatin1( ::pPtr )


METHOD QChar:toLower()
   RETURN Qt_QChar_toLower( ::pPtr )


METHOD QChar:toTitleCase()
   RETURN Qt_QChar_toTitleCase( ::pPtr )


METHOD QChar:toUpper()
   RETURN Qt_QChar_toUpper( ::pPtr )


METHOD QChar:unicode()
   RETURN Qt_QChar_unicode( ::pPtr )


METHOD QChar:unicodeVersion()
   RETURN Qt_QChar_unicodeVersion( ::pPtr )


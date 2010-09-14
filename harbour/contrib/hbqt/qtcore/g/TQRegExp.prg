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


FUNCTION QRegExp( ... )
   RETURN HB_QRegExp():new( ... )


CREATE CLASS QRegExp INHERIT HbQtObjectHandler FUNCTION HB_QRegExp

   METHOD  new( ... )

   METHOD  cap( nNth )
   METHOD  capturedTexts()
   METHOD  caseSensitivity()
   METHOD  errorString()
   METHOD  exactMatch( cStr )
   METHOD  indexIn( cStr, nOffset, nCaretMode )
   METHOD  isEmpty()
   METHOD  isMinimal()
   METHOD  isValid()
   METHOD  lastIndexIn( cStr, nOffset, nCaretMode )
   METHOD  matchedLength()
   METHOD  numCaptures()
   METHOD  pattern()
   METHOD  patternSyntax()
   METHOD  pos( nNth )
   METHOD  setCaseSensitivity( nCs )
   METHOD  setMinimal( lMinimal )
   METHOD  setPattern( cPattern )
   METHOD  setPatternSyntax( nSyntax )

   ENDCLASS


METHOD QRegExp:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegExp( ... )
   RETURN Self


METHOD QRegExp:cap( nNth )
   RETURN Qt_QRegExp_cap( ::pPtr, nNth )


METHOD QRegExp:capturedTexts()
   RETURN Qt_QRegExp_capturedTexts( ::pPtr )


METHOD QRegExp:caseSensitivity()
   RETURN Qt_QRegExp_caseSensitivity( ::pPtr )


METHOD QRegExp:errorString()
   RETURN Qt_QRegExp_errorString( ::pPtr )


METHOD QRegExp:exactMatch( cStr )
   RETURN Qt_QRegExp_exactMatch( ::pPtr, cStr )


METHOD QRegExp:indexIn( cStr, nOffset, nCaretMode )
   RETURN Qt_QRegExp_indexIn( ::pPtr, cStr, nOffset, nCaretMode )


METHOD QRegExp:isEmpty()
   RETURN Qt_QRegExp_isEmpty( ::pPtr )


METHOD QRegExp:isMinimal()
   RETURN Qt_QRegExp_isMinimal( ::pPtr )


METHOD QRegExp:isValid()
   RETURN Qt_QRegExp_isValid( ::pPtr )


METHOD QRegExp:lastIndexIn( cStr, nOffset, nCaretMode )
   RETURN Qt_QRegExp_lastIndexIn( ::pPtr, cStr, nOffset, nCaretMode )


METHOD QRegExp:matchedLength()
   RETURN Qt_QRegExp_matchedLength( ::pPtr )


METHOD QRegExp:numCaptures()
   RETURN Qt_QRegExp_numCaptures( ::pPtr )


METHOD QRegExp:pattern()
   RETURN Qt_QRegExp_pattern( ::pPtr )


METHOD QRegExp:patternSyntax()
   RETURN Qt_QRegExp_patternSyntax( ::pPtr )


METHOD QRegExp:pos( nNth )
   RETURN Qt_QRegExp_pos( ::pPtr, nNth )


METHOD QRegExp:setCaseSensitivity( nCs )
   RETURN Qt_QRegExp_setCaseSensitivity( ::pPtr, nCs )


METHOD QRegExp:setMinimal( lMinimal )
   RETURN Qt_QRegExp_setMinimal( ::pPtr, lMinimal )


METHOD QRegExp:setPattern( cPattern )
   RETURN Qt_QRegExp_setPattern( ::pPtr, cPattern )


METHOD QRegExp:setPatternSyntax( nSyntax )
   RETURN Qt_QRegExp_setPatternSyntax( ::pPtr, nSyntax )


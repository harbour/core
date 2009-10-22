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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QRegExp

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  cap( nNth )                         INLINE  Qt_QRegExp_cap( ::pPtr, nNth )
   METHOD  capturedTexts()                     INLINE  Qt_QRegExp_capturedTexts( ::pPtr )
   METHOD  caseSensitivity()                   INLINE  Qt_QRegExp_caseSensitivity( ::pPtr )
   METHOD  errorString()                       INLINE  Qt_QRegExp_errorString( ::pPtr )
   METHOD  exactMatch( cStr )                  INLINE  Qt_QRegExp_exactMatch( ::pPtr, cStr )
   METHOD  indexIn( cStr, nOffset, nCaretMode )  INLINE  Qt_QRegExp_indexIn( ::pPtr, cStr, nOffset, nCaretMode )
   METHOD  isEmpty()                           INLINE  Qt_QRegExp_isEmpty( ::pPtr )
   METHOD  isMinimal()                         INLINE  Qt_QRegExp_isMinimal( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QRegExp_isValid( ::pPtr )
   METHOD  lastIndexIn( cStr, nOffset, nCaretMode )  INLINE  Qt_QRegExp_lastIndexIn( ::pPtr, cStr, nOffset, nCaretMode )
   METHOD  matchedLength()                     INLINE  Qt_QRegExp_matchedLength( ::pPtr )
   METHOD  numCaptures()                       INLINE  Qt_QRegExp_numCaptures( ::pPtr )
   METHOD  pattern()                           INLINE  Qt_QRegExp_pattern( ::pPtr )
   METHOD  patternSyntax()                     INLINE  Qt_QRegExp_patternSyntax( ::pPtr )
   METHOD  pos( nNth )                         INLINE  Qt_QRegExp_pos( ::pPtr, nNth )
   METHOD  setCaseSensitivity( nCs )           INLINE  Qt_QRegExp_setCaseSensitivity( ::pPtr, nCs )
   METHOD  setMinimal( lMinimal )              INLINE  Qt_QRegExp_setMinimal( ::pPtr, lMinimal )
   METHOD  setPattern( cPattern )              INLINE  Qt_QRegExp_setPattern( ::pPtr, cPattern )
   METHOD  setPatternSyntax( nSyntax )         INLINE  Qt_QRegExp_setPatternSyntax( ::pPtr, nSyntax )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QRegExp

   ::pParent := pParent

   ::pPtr := Qt_QRegExp( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QRegExp

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

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


CREATE CLASS QTextLayout

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTextLayout_destroy( ::pPtr )

   METHOD  beginLayout()                       INLINE  Qt_QTextLayout_beginLayout( ::pPtr )
   METHOD  boundingRect()                      INLINE  Qt_QTextLayout_boundingRect( ::pPtr )
   METHOD  cacheEnabled()                      INLINE  Qt_QTextLayout_cacheEnabled( ::pPtr )
   METHOD  clearAdditionalFormats()            INLINE  Qt_QTextLayout_clearAdditionalFormats( ::pPtr )
   METHOD  clearLayout()                       INLINE  Qt_QTextLayout_clearLayout( ::pPtr )
   METHOD  createLine()                        INLINE  Qt_QTextLayout_createLine( ::pPtr )
   METHOD  drawCursor( pPainter, pPosition, nCursorPosition, nWidth )  INLINE  Qt_QTextLayout_drawCursor( ::pPtr, pPainter, pPosition, nCursorPosition, nWidth )
   METHOD  drawCursor_1( pPainter, pPosition, nCursorPosition )  INLINE  Qt_QTextLayout_drawCursor_1( ::pPtr, pPainter, pPosition, nCursorPosition )
   METHOD  endLayout()                         INLINE  Qt_QTextLayout_endLayout( ::pPtr )
   METHOD  font()                              INLINE  Qt_QTextLayout_font( ::pPtr )
   METHOD  isValidCursorPosition( nPos )       INLINE  Qt_QTextLayout_isValidCursorPosition( ::pPtr, nPos )
   METHOD  lineAt( nI )                        INLINE  Qt_QTextLayout_lineAt( ::pPtr, nI )
   METHOD  lineCount()                         INLINE  Qt_QTextLayout_lineCount( ::pPtr )
   METHOD  lineForTextPosition( nPos )         INLINE  Qt_QTextLayout_lineForTextPosition( ::pPtr, nPos )
   METHOD  maximumWidth()                      INLINE  Qt_QTextLayout_maximumWidth( ::pPtr )
   METHOD  minimumWidth()                      INLINE  Qt_QTextLayout_minimumWidth( ::pPtr )
   METHOD  nextCursorPosition( nOldPos, nMode )  INLINE  Qt_QTextLayout_nextCursorPosition( ::pPtr, nOldPos, nMode )
   METHOD  position()                          INLINE  Qt_QTextLayout_position( ::pPtr )
   METHOD  preeditAreaPosition()               INLINE  Qt_QTextLayout_preeditAreaPosition( ::pPtr )
   METHOD  preeditAreaText()                   INLINE  Qt_QTextLayout_preeditAreaText( ::pPtr )
   METHOD  previousCursorPosition( nOldPos, nMode )  INLINE  Qt_QTextLayout_previousCursorPosition( ::pPtr, nOldPos, nMode )
   METHOD  setCacheEnabled( lEnable )          INLINE  Qt_QTextLayout_setCacheEnabled( ::pPtr, lEnable )
   METHOD  setFont( pFont )                    INLINE  Qt_QTextLayout_setFont( ::pPtr, pFont )
   METHOD  setPosition( pP )                   INLINE  Qt_QTextLayout_setPosition( ::pPtr, pP )
   METHOD  setPreeditArea( nPosition, cText )  INLINE  Qt_QTextLayout_setPreeditArea( ::pPtr, nPosition, cText )
   METHOD  setText( cString )                  INLINE  Qt_QTextLayout_setText( ::pPtr, cString )
   METHOD  setTextOption( pOption )            INLINE  Qt_QTextLayout_setTextOption( ::pPtr, pOption )
   METHOD  text()                              INLINE  Qt_QTextLayout_text( ::pPtr )
   METHOD  textOption()                        INLINE  Qt_QTextLayout_textOption( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTextLayout

   ::pParent := pParent

   ::pPtr := Qt_QTextLayout( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTextLayout

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

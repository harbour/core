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


FUNCTION QTextLayout( ... )
   RETURN HB_QTextLayout():new( ... )


CREATE CLASS QTextLayout INHERIT HbQtObjectHandler FUNCTION HB_QTextLayout

   METHOD  new( ... )

   METHOD  beginLayout()
   METHOD  boundingRect()
   METHOD  cacheEnabled()
   METHOD  clearAdditionalFormats()
   METHOD  clearLayout()
   METHOD  createLine()
   METHOD  drawCursor( ... )
   METHOD  endLayout()
   METHOD  font()
   METHOD  isValidCursorPosition( nPos )
   METHOD  lineAt( nI )
   METHOD  lineCount()
   METHOD  lineForTextPosition( nPos )
   METHOD  maximumWidth()
   METHOD  minimumWidth()
   METHOD  nextCursorPosition( nOldPos, nMode )
   METHOD  position()
   METHOD  preeditAreaPosition()
   METHOD  preeditAreaText()
   METHOD  previousCursorPosition( nOldPos, nMode )
   METHOD  setCacheEnabled( lEnable )
   METHOD  setFont( pFont )
   METHOD  setPosition( pP )
   METHOD  setPreeditArea( nPosition, cText )
   METHOD  setText( cString )
   METHOD  setTextOption( pOption )
   METHOD  text()
   METHOD  textOption()

   ENDCLASS


METHOD QTextLayout:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextLayout( ... )
   RETURN Self


METHOD QTextLayout:beginLayout()
   RETURN Qt_QTextLayout_beginLayout( ::pPtr )


METHOD QTextLayout:boundingRect()
   RETURN Qt_QTextLayout_boundingRect( ::pPtr )


METHOD QTextLayout:cacheEnabled()
   RETURN Qt_QTextLayout_cacheEnabled( ::pPtr )


METHOD QTextLayout:clearAdditionalFormats()
   RETURN Qt_QTextLayout_clearAdditionalFormats( ::pPtr )


METHOD QTextLayout:clearLayout()
   RETURN Qt_QTextLayout_clearLayout( ::pPtr )


METHOD QTextLayout:createLine()
   RETURN Qt_QTextLayout_createLine( ::pPtr )


METHOD QTextLayout:drawCursor( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 4
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N" .AND. aV[ 4 ] $ "N"
                // void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition, int width ) const
                // PO p QPainter, PO p QPointF, N n int, N n int
         RETURN Qt_QTextLayout_drawCursor( ::pPtr, ... )
      ENDCASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "PO" .AND. aV[ 3 ] $ "N"
                // void drawCursor ( QPainter * painter, const QPointF & position, int cursorPosition ) const
                // PO p QPainter, PO p QPointF, N n int
         RETURN Qt_QTextLayout_drawCursor_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QTextLayout:endLayout()
   RETURN Qt_QTextLayout_endLayout( ::pPtr )


METHOD QTextLayout:font()
   RETURN Qt_QTextLayout_font( ::pPtr )


METHOD QTextLayout:isValidCursorPosition( nPos )
   RETURN Qt_QTextLayout_isValidCursorPosition( ::pPtr, nPos )


METHOD QTextLayout:lineAt( nI )
   RETURN Qt_QTextLayout_lineAt( ::pPtr, nI )


METHOD QTextLayout:lineCount()
   RETURN Qt_QTextLayout_lineCount( ::pPtr )


METHOD QTextLayout:lineForTextPosition( nPos )
   RETURN Qt_QTextLayout_lineForTextPosition( ::pPtr, nPos )


METHOD QTextLayout:maximumWidth()
   RETURN Qt_QTextLayout_maximumWidth( ::pPtr )


METHOD QTextLayout:minimumWidth()
   RETURN Qt_QTextLayout_minimumWidth( ::pPtr )


METHOD QTextLayout:nextCursorPosition( nOldPos, nMode )
   RETURN Qt_QTextLayout_nextCursorPosition( ::pPtr, nOldPos, nMode )


METHOD QTextLayout:position()
   RETURN Qt_QTextLayout_position( ::pPtr )


METHOD QTextLayout:preeditAreaPosition()
   RETURN Qt_QTextLayout_preeditAreaPosition( ::pPtr )


METHOD QTextLayout:preeditAreaText()
   RETURN Qt_QTextLayout_preeditAreaText( ::pPtr )


METHOD QTextLayout:previousCursorPosition( nOldPos, nMode )
   RETURN Qt_QTextLayout_previousCursorPosition( ::pPtr, nOldPos, nMode )


METHOD QTextLayout:setCacheEnabled( lEnable )
   RETURN Qt_QTextLayout_setCacheEnabled( ::pPtr, lEnable )


METHOD QTextLayout:setFont( pFont )
   RETURN Qt_QTextLayout_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QTextLayout:setPosition( pP )
   RETURN Qt_QTextLayout_setPosition( ::pPtr, hbqt_ptr( pP ) )


METHOD QTextLayout:setPreeditArea( nPosition, cText )
   RETURN Qt_QTextLayout_setPreeditArea( ::pPtr, nPosition, cText )


METHOD QTextLayout:setText( cString )
   RETURN Qt_QTextLayout_setText( ::pPtr, cString )


METHOD QTextLayout:setTextOption( pOption )
   RETURN Qt_QTextLayout_setTextOption( ::pPtr, hbqt_ptr( pOption ) )


METHOD QTextLayout:text()
   RETURN Qt_QTextLayout_text( ::pPtr )


METHOD QTextLayout:textOption()
   RETURN Qt_QTextLayout_textOption( ::pPtr )


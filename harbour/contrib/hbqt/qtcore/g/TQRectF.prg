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


FUNCTION QRectF( ... )
   RETURN HB_QRectF():new( ... )


CREATE CLASS QRectF INHERIT HbQtObjectHandler FUNCTION HB_QRectF

   METHOD  new( ... )

   METHOD  adjust( nDx1, nDy1, nDx2, nDy2 )
   METHOD  adjusted( nDx1, nDy1, nDx2, nDy2 )
   METHOD  bottom()
   METHOD  bottomLeft()
   METHOD  bottomRight()
   METHOD  center()
   METHOD  contains( ... )
   METHOD  getCoords( nX1, nY1, nX2, nY2 )
   METHOD  getRect( nX, nY, nWidth, nHeight )
   METHOD  height()
   METHOD  intersected( pRectangle )
   METHOD  intersects( pRectangle )
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  isValid()
   METHOD  left()
   METHOD  moveBottom( nY )
   METHOD  moveBottomLeft( pPosition )
   METHOD  moveBottomRight( pPosition )
   METHOD  moveCenter( pPosition )
   METHOD  moveLeft( nX )
   METHOD  moveRight( nX )
   METHOD  moveTo( ... )
   METHOD  moveTop( nY )
   METHOD  moveTopLeft( pPosition )
   METHOD  moveTopRight( pPosition )
   METHOD  normalized()
   METHOD  right()
   METHOD  setBottom( nY )
   METHOD  setBottomLeft( pPosition )
   METHOD  setBottomRight( pPosition )
   METHOD  setCoords( nX1, nY1, nX2, nY2 )
   METHOD  setHeight( nHeight )
   METHOD  setLeft( nX )
   METHOD  setRect( nX, nY, nWidth, nHeight )
   METHOD  setRight( nX )
   METHOD  setSize( pSize )
   METHOD  setTop( nY )
   METHOD  setTopLeft( pPosition )
   METHOD  setTopRight( pPosition )
   METHOD  setWidth( nWidth )
   METHOD  setX( nX )
   METHOD  setY( nY )
   METHOD  size()
   METHOD  toAlignedRect()
   METHOD  toRect()
   METHOD  top()
   METHOD  topLeft()
   METHOD  topRight()
   METHOD  translate( ... )
   METHOD  translated( ... )
   METHOD  united( pRectangle )
   METHOD  width()
   METHOD  x()
   METHOD  y()

   ENDCLASS


METHOD QRectF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRectF( ... )
   RETURN Self


METHOD QRectF:adjust( nDx1, nDy1, nDx2, nDy2 )
   RETURN Qt_QRectF_adjust( ::pPtr, nDx1, nDy1, nDx2, nDy2 )


METHOD QRectF:adjusted( nDx1, nDy1, nDx2, nDy2 )
   RETURN HB_QRectF():from( Qt_QRectF_adjusted( ::pPtr, nDx1, nDy1, nDx2, nDy2 ) )


METHOD QRectF:bottom()
   RETURN Qt_QRectF_bottom( ::pPtr )


METHOD QRectF:bottomLeft()
   RETURN HB_QPointF():from( Qt_QRectF_bottomLeft( ::pPtr ) )


METHOD QRectF:bottomRight()
   RETURN HB_QPointF():from( Qt_QRectF_bottomRight( ::pPtr ) )


METHOD QRectF:center()
   RETURN HB_QPointF():from( Qt_QRectF_center( ::pPtr ) )


METHOD QRectF:contains( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRectF_contains_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOINTF"
            RETURN Qt_QRectF_contains( ::pPtr, ... )
         CASE "QRECTF"
            RETURN Qt_QRectF_contains_2( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QRectF:getCoords( nX1, nY1, nX2, nY2 )
   RETURN Qt_QRectF_getCoords( ::pPtr, nX1, nY1, nX2, nY2 )


METHOD QRectF:getRect( nX, nY, nWidth, nHeight )
   RETURN Qt_QRectF_getRect( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QRectF:height()
   RETURN Qt_QRectF_height( ::pPtr )


METHOD QRectF:intersected( pRectangle )
   RETURN HB_QRectF():from( Qt_QRectF_intersected( ::pPtr, hbqt_ptr( pRectangle ) ) )


METHOD QRectF:intersects( pRectangle )
   RETURN Qt_QRectF_intersects( ::pPtr, hbqt_ptr( pRectangle ) )


METHOD QRectF:isEmpty()
   RETURN Qt_QRectF_isEmpty( ::pPtr )


METHOD QRectF:isNull()
   RETURN Qt_QRectF_isNull( ::pPtr )


METHOD QRectF:isValid()
   RETURN Qt_QRectF_isValid( ::pPtr )


METHOD QRectF:left()
   RETURN Qt_QRectF_left( ::pPtr )


METHOD QRectF:moveBottom( nY )
   RETURN Qt_QRectF_moveBottom( ::pPtr, nY )


METHOD QRectF:moveBottomLeft( pPosition )
   RETURN Qt_QRectF_moveBottomLeft( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:moveBottomRight( pPosition )
   RETURN Qt_QRectF_moveBottomRight( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:moveCenter( pPosition )
   RETURN Qt_QRectF_moveCenter( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:moveLeft( nX )
   RETURN Qt_QRectF_moveLeft( ::pPtr, nX )


METHOD QRectF:moveRight( nX )
   RETURN Qt_QRectF_moveRight( ::pPtr, nX )


METHOD QRectF:moveTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRectF_moveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QRectF:moveTop( nY )
   RETURN Qt_QRectF_moveTop( ::pPtr, nY )


METHOD QRectF:moveTopLeft( pPosition )
   RETURN Qt_QRectF_moveTopLeft( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:moveTopRight( pPosition )
   RETURN Qt_QRectF_moveTopRight( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:normalized()
   RETURN HB_QRectF():from( Qt_QRectF_normalized( ::pPtr ) )


METHOD QRectF:right()
   RETURN Qt_QRectF_right( ::pPtr )


METHOD QRectF:setBottom( nY )
   RETURN Qt_QRectF_setBottom( ::pPtr, nY )


METHOD QRectF:setBottomLeft( pPosition )
   RETURN Qt_QRectF_setBottomLeft( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:setBottomRight( pPosition )
   RETURN Qt_QRectF_setBottomRight( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:setCoords( nX1, nY1, nX2, nY2 )
   RETURN Qt_QRectF_setCoords( ::pPtr, nX1, nY1, nX2, nY2 )


METHOD QRectF:setHeight( nHeight )
   RETURN Qt_QRectF_setHeight( ::pPtr, nHeight )


METHOD QRectF:setLeft( nX )
   RETURN Qt_QRectF_setLeft( ::pPtr, nX )


METHOD QRectF:setRect( nX, nY, nWidth, nHeight )
   RETURN Qt_QRectF_setRect( ::pPtr, nX, nY, nWidth, nHeight )


METHOD QRectF:setRight( nX )
   RETURN Qt_QRectF_setRight( ::pPtr, nX )


METHOD QRectF:setSize( pSize )
   RETURN Qt_QRectF_setSize( ::pPtr, hbqt_ptr( pSize ) )


METHOD QRectF:setTop( nY )
   RETURN Qt_QRectF_setTop( ::pPtr, nY )


METHOD QRectF:setTopLeft( pPosition )
   RETURN Qt_QRectF_setTopLeft( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:setTopRight( pPosition )
   RETURN Qt_QRectF_setTopRight( ::pPtr, hbqt_ptr( pPosition ) )


METHOD QRectF:setWidth( nWidth )
   RETURN Qt_QRectF_setWidth( ::pPtr, nWidth )


METHOD QRectF:setX( nX )
   RETURN Qt_QRectF_setX( ::pPtr, nX )


METHOD QRectF:setY( nY )
   RETURN Qt_QRectF_setY( ::pPtr, nY )


METHOD QRectF:size()
   RETURN HB_QSizeF():from( Qt_QRectF_size( ::pPtr ) )


METHOD QRectF:toAlignedRect()
   RETURN HB_QRect():from( Qt_QRectF_toAlignedRect( ::pPtr ) )


METHOD QRectF:toRect()
   RETURN HB_QRect():from( Qt_QRectF_toRect( ::pPtr ) )


METHOD QRectF:top()
   RETURN Qt_QRectF_top( ::pPtr )


METHOD QRectF:topLeft()
   RETURN HB_QPointF():from( Qt_QRectF_topLeft( ::pPtr ) )


METHOD QRectF:topRight()
   RETURN HB_QPointF():from( Qt_QRectF_topRight( ::pPtr ) )


METHOD QRectF:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRectF_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QRectF:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QRectF():from( Qt_QRectF_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QRectF():from( Qt_QRectF_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QRectF:united( pRectangle )
   RETURN HB_QRectF():from( Qt_QRectF_united( ::pPtr, hbqt_ptr( pRectangle ) ) )


METHOD QRectF:width()
   RETURN Qt_QRectF_width( ::pPtr )


METHOD QRectF:x()
   RETURN Qt_QRectF_x( ::pPtr )


METHOD QRectF:y()
   RETURN Qt_QRectF_y( ::pPtr )


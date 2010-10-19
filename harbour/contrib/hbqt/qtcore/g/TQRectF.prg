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

FUNCTION QRectFFromPointer( ... )
   RETURN HB_QRectF():fromPointer( ... )


CREATE CLASS QRectF INHERIT HbQtObjectHandler FUNCTION HB_QRectF

   METHOD  new( ... )

   METHOD  adjust                        // ( nDx1, nDy1, nDx2, nDy2 )                         -> NIL
   METHOD  adjusted                      // ( nDx1, nDy1, nDx2, nDy2 )                         -> oQRectF
   METHOD  bottom                        // (  )                                               -> nQreal
   METHOD  bottomLeft                    // (  )                                               -> oQPointF
   METHOD  bottomRight                   // (  )                                               -> oQPointF
   METHOD  center                        // (  )                                               -> oQPointF
   METHOD  contains                      // ( oQPointF )                                       -> lBool
                                         // ( nX, nY )                                         -> lBool
                                         // ( oQRectF )                                        -> lBool
   METHOD  getCoords                     // ( @nX1, @nY1, @nX2, @nY2 )                         -> NIL
   METHOD  getRect                       // ( @nX, @nY, @nWidth, @nHeight )                    -> NIL
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  intersected                   // ( oQRectF )                                        -> oQRectF
   METHOD  intersects                    // ( oQRectF )                                        -> lBool
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  left                          // (  )                                               -> nQreal
   METHOD  moveBottom                    // ( nY )                                             -> NIL
   METHOD  moveBottomLeft                // ( oQPointF )                                       -> NIL
   METHOD  moveBottomRight               // ( oQPointF )                                       -> NIL
   METHOD  moveCenter                    // ( oQPointF )                                       -> NIL
   METHOD  moveLeft                      // ( nX )                                             -> NIL
   METHOD  moveRight                     // ( nX )                                             -> NIL
   METHOD  moveTo                        // ( nX, nY )                                         -> NIL
                                         // ( oQPointF )                                       -> NIL
   METHOD  moveTop                       // ( nY )                                             -> NIL
   METHOD  moveTopLeft                   // ( oQPointF )                                       -> NIL
   METHOD  moveTopRight                  // ( oQPointF )                                       -> NIL
   METHOD  normalized                    // (  )                                               -> oQRectF
   METHOD  right                         // (  )                                               -> nQreal
   METHOD  setBottom                     // ( nY )                                             -> NIL
   METHOD  setBottomLeft                 // ( oQPointF )                                       -> NIL
   METHOD  setBottomRight                // ( oQPointF )                                       -> NIL
   METHOD  setCoords                     // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setLeft                       // ( nX )                                             -> NIL
   METHOD  setRect                       // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  setRight                      // ( nX )                                             -> NIL
   METHOD  setSize                       // ( oQSizeF )                                        -> NIL
   METHOD  setTop                        // ( nY )                                             -> NIL
   METHOD  setTopLeft                    // ( oQPointF )                                       -> NIL
   METHOD  setTopRight                   // ( oQPointF )                                       -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  setX                          // ( nX )                                             -> NIL
   METHOD  setY                          // ( nY )                                             -> NIL
   METHOD  size                          // (  )                                               -> oQSizeF
   METHOD  toAlignedRect                 // (  )                                               -> oQRect
   METHOD  toRect                        // (  )                                               -> oQRect
   METHOD  top                           // (  )                                               -> nQreal
   METHOD  topLeft                       // (  )                                               -> oQPointF
   METHOD  topRight                      // (  )                                               -> oQPointF
   METHOD  translate                     // ( nDx, nDy )                                       -> NIL
                                         // ( oQPointF )                                       -> NIL
   METHOD  translated                    // ( nDx, nDy )                                       -> oQRectF
                                         // ( oQPointF )                                       -> oQRectF
   METHOD  united                        // ( oQRectF )                                        -> oQRectF
   METHOD  width                         // (  )                                               -> nQreal
   METHOD  x                             // (  )                                               -> nQreal
   METHOD  y                             // (  )                                               -> nQreal

   ENDCLASS


METHOD QRectF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRectF( ... )
   RETURN Self


METHOD QRectF:adjust( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRectF_adjust( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:adjusted( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QRectFFromPointer( Qt_QRectF_adjusted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:bottom( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_bottom( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:bottomLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRectF_bottomLeft( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:bottomRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRectF_bottomRight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:center( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRectF_center( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QRectF:getCoords( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRectF_getCoords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:getRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRectF_getRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QRectF_intersected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:intersects( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_intersects( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:left( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_left( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveBottomLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveBottomLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveBottomRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveBottomRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveCenter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveCenter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QRectF:moveTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveTopLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveTopLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:moveTopRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_moveTopRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:normalized( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFFromPointer( Qt_QRectF_normalized( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:right( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_right( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setBottomLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setBottomLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setBottomRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setBottomRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setCoords( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRectF_setCoords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRectF_setRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setTopLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setTopLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setTopRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setTopRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setX( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setX( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:setY( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRectF_setY( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFFromPointer( Qt_QRectF_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:toAlignedRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QRectF_toAlignedRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:toRect( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QRectF_toRect( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:top( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_top( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:topLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRectF_topLeft( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:topRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFFromPointer( Qt_QRectF_topRight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


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
   RETURN __hbqt_error()


METHOD QRectF:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QRectFFromPointer( Qt_QRectF_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QRectF_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFFromPointer( Qt_QRectF_united( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRectF:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRectF_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


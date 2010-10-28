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


REQUEST __HBQTCORE


FUNCTION QRect( ... )
   RETURN HB_QRect():new( ... )

FUNCTION QRectFromPointer( ... )
   RETURN HB_QRect():fromPointer( ... )


CREATE CLASS QRect INHERIT HbQtObjectHandler FUNCTION HB_QRect

   METHOD  new( ... )

   METHOD  adjust                        // ( nDx1, nDy1, nDx2, nDy2 )                         -> NIL
   METHOD  adjusted                      // ( nDx1, nDy1, nDx2, nDy2 )                         -> oQRect
   METHOD  bottom                        // (  )                                               -> nInt
   METHOD  bottomLeft                    // (  )                                               -> oQPoint
   METHOD  bottomRight                   // (  )                                               -> oQPoint
   METHOD  center                        // (  )                                               -> oQPoint
   METHOD  contains                      // ( oQPoint, lProper )                               -> lBool
                                         // ( nX, nY, lProper )                                -> lBool
                                         // ( nX, nY )                                         -> lBool
                                         // ( oQRect, lProper )                                -> lBool
   METHOD  getCoords                     // ( @nX1, @nY1, @nX2, @nY2 )                         -> NIL
   METHOD  getRect                       // ( @nX, @nY, @nWidth, @nHeight )                    -> NIL
   METHOD  height                        // (  )                                               -> nInt
   METHOD  intersected                   // ( oQRect )                                         -> oQRect
   METHOD  intersects                    // ( oQRect )                                         -> lBool
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  left                          // (  )                                               -> nInt
   METHOD  moveBottom                    // ( nY )                                             -> NIL
   METHOD  moveBottomLeft                // ( oQPoint )                                        -> NIL
   METHOD  moveBottomRight               // ( oQPoint )                                        -> NIL
   METHOD  moveCenter                    // ( oQPoint )                                        -> NIL
   METHOD  moveLeft                      // ( nX )                                             -> NIL
   METHOD  moveRight                     // ( nX )                                             -> NIL
   METHOD  moveTo                        // ( nX, nY )                                         -> NIL
                                         // ( oQPoint )                                        -> NIL
   METHOD  moveTop                       // ( nY )                                             -> NIL
   METHOD  moveTopLeft                   // ( oQPoint )                                        -> NIL
   METHOD  moveTopRight                  // ( oQPoint )                                        -> NIL
   METHOD  normalized                    // (  )                                               -> oQRect
   METHOD  right                         // (  )                                               -> nInt
   METHOD  setBottom                     // ( nY )                                             -> NIL
   METHOD  setBottomLeft                 // ( oQPoint )                                        -> NIL
   METHOD  setBottomRight                // ( oQPoint )                                        -> NIL
   METHOD  setCoords                     // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setLeft                       // ( nX )                                             -> NIL
   METHOD  setRect                       // ( nX, nY, nWidth, nHeight )                        -> NIL
   METHOD  setRight                      // ( nX )                                             -> NIL
   METHOD  setSize                       // ( oQSize )                                         -> NIL
   METHOD  setTop                        // ( nY )                                             -> NIL
   METHOD  setTopLeft                    // ( oQPoint )                                        -> NIL
   METHOD  setTopRight                   // ( oQPoint )                                        -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  setX                          // ( nX )                                             -> NIL
   METHOD  setY                          // ( nY )                                             -> NIL
   METHOD  size                          // (  )                                               -> oQSize
   METHOD  top                           // (  )                                               -> nInt
   METHOD  topLeft                       // (  )                                               -> oQPoint
   METHOD  topRight                      // (  )                                               -> oQPoint
   METHOD  translate                     // ( nDx, nDy )                                       -> NIL
                                         // ( oQPoint )                                        -> NIL
   METHOD  translated                    // ( nDx, nDy )                                       -> oQRect
                                         // ( oQPoint )                                        -> oQRect
   METHOD  united                        // ( oQRect )                                         -> oQRect
   METHOD  width                         // (  )                                               -> nInt
   METHOD  x                             // (  )                                               -> nInt
   METHOD  y                             // (  )                                               -> nInt

   ENDCLASS


METHOD QRect:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRect( ... )
   RETURN Self


METHOD QRect:adjust( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRect_adjust( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:adjusted( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN QRectFromPointer( Qt_QRect_adjusted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:bottom( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_bottom( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:bottomLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QRect_bottomLeft( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:bottomRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QRect_bottomRight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:center( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QRect_center( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:contains( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isLogical( hb_pvalue( 3 ) )
         RETURN Qt_QRect_contains_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRect_contains_2( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isLogical( hb_pvalue( 2 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QRect_contains_3( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QRect_contains( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECT"
            RETURN Qt_QRect_contains_3( ::pPtr, ... )
         CASE "QPOINT"
            RETURN Qt_QRect_contains( ::pPtr, ... )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:getCoords( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRect_getCoords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:getRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRect_getRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:intersected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QRect_intersected( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:intersects( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_intersects( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:left( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_left( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveBottomLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveBottomLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveBottomRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveBottomRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveCenter( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveCenter( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveTo( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRect_moveTo( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveTo_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveTopLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveTopLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:moveTopRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_moveTopRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:normalized( ... )
   SWITCH PCount()
   CASE 0
      RETURN QRectFromPointer( Qt_QRect_normalized( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:right( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_right( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setBottom( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setBottom( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setBottomLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setBottomLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setBottomRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setBottomRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setCoords( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRect_setCoords( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setRect( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QRect_setRect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setSize( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setSize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setTop( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setTop( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setTopLeft( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setTopLeft( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setTopRight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setTopRight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setX( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setX( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:setY( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QRect_setY( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QRect_size( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:top( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_top( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:topLeft( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QRect_topLeft( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:topRight( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPointFromPointer( Qt_QRect_topRight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QRect_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QRect_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QRectFromPointer( Qt_QRect_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QRect_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:united( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QRectFromPointer( Qt_QRect_united( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:x( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_x( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QRect:y( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QRect_y( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


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


REQUEST __HBQTGUI


FUNCTION QMatrix( ... )
   RETURN HB_QMatrix():new( ... )

FUNCTION QMatrixFromPointer( ... )
   RETURN HB_QMatrix():fromPointer( ... )


CREATE CLASS QMatrix INHERIT HbQtObjectHandler FUNCTION HB_QMatrix

   METHOD  new( ... )

   METHOD  m11                           // (  )                                               -> nQreal
   METHOD  m12                           // (  )                                               -> nQreal
   METHOD  m21                           // (  )                                               -> nQreal
   METHOD  m22                           // (  )                                               -> nQreal
   METHOD  det                           // (  )                                               -> nQreal
   METHOD  dx                            // (  )                                               -> nQreal
   METHOD  dy                            // (  )                                               -> nQreal
   METHOD  inverted                      // ( @lInvertible )                                   -> oQMatrix
   METHOD  isIdentity                    // (  )                                               -> lBool
   METHOD  isInvertible                  // (  )                                               -> lBool
   METHOD  map                           // ( nX, nY, @nTx, @nTy )                             -> NIL
                                         // ( nX, nY, @nTx, @nTy )                             -> NIL
                                         // ( oQPointF )                                       -> oQPointF
                                         // ( oQPoint )                                        -> oQPoint
                                         // ( oQLineF )                                        -> oQLineF
                                         // ( oQLine )                                         -> oQLine
                                         // ( oQPolygonF )                                     -> oQPolygonF
                                         // ( oQPolygon )                                      -> oQPolygon
                                         // ( oQRegion )                                       -> oQRegion
                                         // ( oQPainterPath )                                  -> oQPainterPath
   METHOD  mapRect                       // ( oQRectF )                                        -> oQRectF
                                         // ( oQRect )                                         -> oQRect
   METHOD  mapToPolygon                  // ( oQRect )                                         -> oQPolygon
   METHOD  reset                         // (  )                                               -> NIL
   METHOD  rotate                        // ( nDegrees )                                       -> oQMatrix
   METHOD  scale                         // ( nSx, nSy )                                       -> oQMatrix
   METHOD  setMatrix                     // ( nM11, nM12, nM21, nM22, nDx, nDy )               -> NIL
   METHOD  shear                         // ( nSh, nSv )                                       -> oQMatrix
   METHOD  translate                     // ( nDx, nDy )                                       -> oQMatrix

   ENDCLASS


METHOD QMatrix:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMatrix( ... )
   RETURN Self


METHOD QMatrix:m11( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m11( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m12( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m12( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m21( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m21( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:m22( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_m22( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:det( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_det( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:dx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_dx( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:dy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_dy( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:inverted( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_inverted( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 0
      RETURN QMatrixFromPointer( Qt_QMatrix_inverted( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:isIdentity( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_isIdentity( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:isInvertible( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_isInvertible( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:map( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QMatrix_map_1( ::pPtr, ... )
         // RETURN Qt_QMatrix_map( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QPOLYGON"
            RETURN QPolygonFromPointer( Qt_QMatrix_map_7( ::pPtr, ... ) )
         CASE "QLINE"
            RETURN QLineFromPointer( Qt_QMatrix_map_5( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN QPointFromPointer( Qt_QMatrix_map_3( ::pPtr, ... ) )
         CASE "QREGION"
            RETURN QRegionFromPointer( Qt_QMatrix_map_8( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN QPointFFromPointer( Qt_QMatrix_map_2( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN QPolygonFFromPointer( Qt_QMatrix_map_6( ::pPtr, ... ) )
         CASE "QLINEF"
            RETURN QLineFFromPointer( Qt_QMatrix_map_4( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN QPainterPathFromPointer( Qt_QMatrix_map_9( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:mapRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN QRectFFromPointer( Qt_QMatrix_mapRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN QRectFromPointer( Qt_QMatrix_mapRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:mapToPolygon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QPolygonFromPointer( Qt_QMatrix_mapToPolygon( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:reset( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QMatrix_reset( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:rotate( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_rotate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:scale( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_scale( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:setMatrix( ... )
   SWITCH PCount()
   CASE 6
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) ) .AND. hb_isNumeric( hb_pvalue( 5 ) ) .AND. hb_isNumeric( hb_pvalue( 6 ) )
         RETURN Qt_QMatrix_setMatrix( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:shear( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_shear( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QMatrix:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN QMatrixFromPointer( Qt_QMatrix_translate( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


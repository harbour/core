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


FUNCTION QMatrix( ... )
   RETURN HB_QMatrix():new( ... )


CREATE CLASS QMatrix INHERIT HbQtObjectHandler FUNCTION HB_QMatrix

   METHOD  new( ... )

   METHOD  m11()
   METHOD  m12()
   METHOD  m21()
   METHOD  m22()
   METHOD  det()
   METHOD  dx()
   METHOD  dy()
   METHOD  inverted( lInvertible )
   METHOD  isIdentity()
   METHOD  isInvertible()
   METHOD  map( ... )
   METHOD  mapRect( ... )
   METHOD  mapToPolygon( pRectangle )
   METHOD  reset()
   METHOD  rotate( nDegrees )
   METHOD  scale( nSx, nSy )
   METHOD  setMatrix( nM11, nM12, nM21, nM22, nDx, nDy )
   METHOD  shear( nSh, nSv )
   METHOD  translate( nDx, nDy )

   ENDCLASS


METHOD QMatrix:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QMatrix( ... )
   RETURN Self


METHOD QMatrix:m11()
   RETURN Qt_QMatrix_m11( ::pPtr )


METHOD QMatrix:m12()
   RETURN Qt_QMatrix_m12( ::pPtr )


METHOD QMatrix:m21()
   RETURN Qt_QMatrix_m21( ::pPtr )


METHOD QMatrix:m22()
   RETURN Qt_QMatrix_m22( ::pPtr )


METHOD QMatrix:det()
   RETURN Qt_QMatrix_det( ::pPtr )


METHOD QMatrix:dx()
   RETURN Qt_QMatrix_dx( ::pPtr )


METHOD QMatrix:dy()
   RETURN Qt_QMatrix_dy( ::pPtr )


METHOD QMatrix:inverted( lInvertible )
   RETURN HB_QMatrix():from( Qt_QMatrix_inverted( ::pPtr, lInvertible ) )


METHOD QMatrix:isIdentity()
   RETURN Qt_QMatrix_isIdentity( ::pPtr )


METHOD QMatrix:isInvertible()
   RETURN Qt_QMatrix_isInvertible( ::pPtr )


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
            RETURN HB_QPolygon():from( Qt_QMatrix_map_7( ::pPtr, ... ) )
         CASE "QLINE"
            RETURN HB_QLine():from( Qt_QMatrix_map_5( ::pPtr, ... ) )
         CASE "QPOINT"
            RETURN HB_QPoint():from( Qt_QMatrix_map_3( ::pPtr, ... ) )
         CASE "QREGION"
            RETURN HB_QRegion():from( Qt_QMatrix_map_8( ::pPtr, ... ) )
         CASE "QPOINTF"
            RETURN HB_QPointF():from( Qt_QMatrix_map_2( ::pPtr, ... ) )
         CASE "QPOLYGONF"
            RETURN HB_QPolygonF():from( Qt_QMatrix_map_6( ::pPtr, ... ) )
         CASE "QLINEF"
            RETURN HB_QLineF():from( Qt_QMatrix_map_4( ::pPtr, ... ) )
         CASE "QPAINTERPATH"
            RETURN HB_QPainterPath():from( Qt_QMatrix_map_9( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMatrix:mapRect( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         SWITCH __objGetClsName( hb_pvalue( 1 ) )
         CASE "QRECTF"
            RETURN HB_QRectF():from( Qt_QMatrix_mapRect( ::pPtr, ... ) )
         CASE "QRECT"
            RETURN HB_QRect():from( Qt_QMatrix_mapRect_1( ::pPtr, ... ) )
         ENDSWITCH
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QMatrix:mapToPolygon( pRectangle )
   RETURN HB_QPolygon():from( Qt_QMatrix_mapToPolygon( ::pPtr, hbqt_ptr( pRectangle ) ) )


METHOD QMatrix:reset()
   RETURN Qt_QMatrix_reset( ::pPtr )


METHOD QMatrix:rotate( nDegrees )
   RETURN HB_QMatrix():from( Qt_QMatrix_rotate( ::pPtr, nDegrees ) )


METHOD QMatrix:scale( nSx, nSy )
   RETURN HB_QMatrix():from( Qt_QMatrix_scale( ::pPtr, nSx, nSy ) )


METHOD QMatrix:setMatrix( nM11, nM12, nM21, nM22, nDx, nDy )
   RETURN Qt_QMatrix_setMatrix( ::pPtr, nM11, nM12, nM21, nM22, nDx, nDy )


METHOD QMatrix:shear( nSh, nSv )
   RETURN HB_QMatrix():from( Qt_QMatrix_shear( ::pPtr, nSh, nSv ) )


METHOD QMatrix:translate( nDx, nDy )
   RETURN HB_QMatrix():from( Qt_QMatrix_translate( ::pPtr, nDx, nDy ) )


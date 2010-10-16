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


FUNCTION QGraphicsItemAnimation( ... )
   RETURN HB_QGraphicsItemAnimation():new( ... )

FUNCTION QGraphicsItemAnimationFrom( ... )
   RETURN HB_QGraphicsItemAnimation():from( ... )

FUNCTION QGraphicsItemAnimationFromPointer( ... )
   RETURN HB_QGraphicsItemAnimation():fromPointer( ... )


CREATE CLASS QGraphicsItemAnimation INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QGraphicsItemAnimation

   METHOD  new( ... )

   METHOD  clear                         // (  )                                               -> NIL
   METHOD  horizontalScaleAt             // ( nStep )                                          -> nQreal
   METHOD  horizontalShearAt             // ( nStep )                                          -> nQreal
   METHOD  item                          // (  )                                               -> oQGraphicsItem
   METHOD  matrixAt                      // ( nStep )                                          -> oQMatrix
   METHOD  posAt                         // ( nStep )                                          -> oQPointF
   METHOD  rotationAt                    // ( nStep )                                          -> nQreal
   METHOD  setItem                       // ( oQGraphicsItem )                                 -> NIL
   METHOD  setPosAt                      // ( nStep, oQPointF )                                -> NIL
   METHOD  setRotationAt                 // ( nStep, nAngle )                                  -> NIL
   METHOD  setScaleAt                    // ( nStep, nSx, nSy )                                -> NIL
   METHOD  setShearAt                    // ( nStep, nSh, nSv )                                -> NIL
   METHOD  setTimeLine                   // ( oQTimeLine )                                     -> NIL
   METHOD  setTranslationAt              // ( nStep, nDx, nDy )                                -> NIL
   METHOD  timeLine                      // (  )                                               -> oQTimeLine
   METHOD  verticalScaleAt               // ( nStep )                                          -> nQreal
   METHOD  verticalShearAt               // ( nStep )                                          -> nQreal
   METHOD  xTranslationAt                // ( nStep )                                          -> nQreal
   METHOD  yTranslationAt                // ( nStep )                                          -> nQreal
   METHOD  setStep                       // ( nStep )                                          -> NIL

   ENDCLASS


METHOD QGraphicsItemAnimation:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsItemAnimation( ... )
   RETURN Self


METHOD QGraphicsItemAnimation:clear( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QGraphicsItemAnimation_clear( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:horizontalScaleAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_horizontalScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:horizontalShearAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_horizontalShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:item( ... )
   SWITCH PCount()
   CASE 0
      RETURN QGraphicsItemFromPointer( Qt_QGraphicsItemAnimation_item( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:matrixAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QMatrixFromPointer( Qt_QGraphicsItemAnimation_matrixAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:posAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QPointFFromPointer( Qt_QGraphicsItemAnimation_posAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:rotationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_rotationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setItem( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setItem( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setPosAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsItemAnimation_setPosAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setRotationAt( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QGraphicsItemAnimation_setRotationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setScaleAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setShearAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setTimeLine( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setTimeLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setTranslationAt( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QGraphicsItemAnimation_setTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:timeLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTimeLineFromPointer( Qt_QGraphicsItemAnimation_timeLine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:verticalScaleAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_verticalScaleAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:verticalShearAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_verticalShearAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:xTranslationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_xTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:yTranslationAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_yTranslationAt( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QGraphicsItemAnimation:setStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QGraphicsItemAnimation_setStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


CREATE CLASS QGraphicsItemAnimation INHERIT HbQtObjectHandler, HB_QObject FUNCTION HB_QGraphicsItemAnimation

   METHOD  new( ... )

   METHOD  clear()
   METHOD  horizontalScaleAt( nStep )
   METHOD  horizontalShearAt( nStep )
   METHOD  item()
   METHOD  matrixAt( nStep )
   METHOD  posAt( nStep )
   METHOD  rotationAt( nStep )
   METHOD  setItem( pItem )
   METHOD  setPosAt( nStep, pPoint )
   METHOD  setRotationAt( nStep, nAngle )
   METHOD  setScaleAt( nStep, nSx, nSy )
   METHOD  setShearAt( nStep, nSh, nSv )
   METHOD  setTimeLine( pTimeLine )
   METHOD  setTranslationAt( nStep, nDx, nDy )
   METHOD  timeLine()
   METHOD  verticalScaleAt( nStep )
   METHOD  verticalShearAt( nStep )
   METHOD  xTranslationAt( nStep )
   METHOD  yTranslationAt( nStep )
   METHOD  setStep( nStep )

   ENDCLASS


METHOD QGraphicsItemAnimation:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QGraphicsItemAnimation( ... )
   RETURN Self


METHOD QGraphicsItemAnimation:clear()
   RETURN Qt_QGraphicsItemAnimation_clear( ::pPtr )


METHOD QGraphicsItemAnimation:horizontalScaleAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_horizontalScaleAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:horizontalShearAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_horizontalShearAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:item()
   RETURN HB_QGraphicsItem():from( Qt_QGraphicsItemAnimation_item( ::pPtr ) )


METHOD QGraphicsItemAnimation:matrixAt( nStep )
   RETURN HB_QMatrix():from( Qt_QGraphicsItemAnimation_matrixAt( ::pPtr, nStep ) )


METHOD QGraphicsItemAnimation:posAt( nStep )
   RETURN HB_QPointF():from( Qt_QGraphicsItemAnimation_posAt( ::pPtr, nStep ) )


METHOD QGraphicsItemAnimation:rotationAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_rotationAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:setItem( pItem )
   RETURN Qt_QGraphicsItemAnimation_setItem( ::pPtr, hbqt_ptr( pItem ) )


METHOD QGraphicsItemAnimation:setPosAt( nStep, pPoint )
   RETURN Qt_QGraphicsItemAnimation_setPosAt( ::pPtr, nStep, hbqt_ptr( pPoint ) )


METHOD QGraphicsItemAnimation:setRotationAt( nStep, nAngle )
   RETURN Qt_QGraphicsItemAnimation_setRotationAt( ::pPtr, nStep, nAngle )


METHOD QGraphicsItemAnimation:setScaleAt( nStep, nSx, nSy )
   RETURN Qt_QGraphicsItemAnimation_setScaleAt( ::pPtr, nStep, nSx, nSy )


METHOD QGraphicsItemAnimation:setShearAt( nStep, nSh, nSv )
   RETURN Qt_QGraphicsItemAnimation_setShearAt( ::pPtr, nStep, nSh, nSv )


METHOD QGraphicsItemAnimation:setTimeLine( pTimeLine )
   RETURN Qt_QGraphicsItemAnimation_setTimeLine( ::pPtr, hbqt_ptr( pTimeLine ) )


METHOD QGraphicsItemAnimation:setTranslationAt( nStep, nDx, nDy )
   RETURN Qt_QGraphicsItemAnimation_setTranslationAt( ::pPtr, nStep, nDx, nDy )


METHOD QGraphicsItemAnimation:timeLine()
   RETURN HB_QTimeLine():from( Qt_QGraphicsItemAnimation_timeLine( ::pPtr ) )


METHOD QGraphicsItemAnimation:verticalScaleAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_verticalScaleAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:verticalShearAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_verticalShearAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:xTranslationAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_xTranslationAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:yTranslationAt( nStep )
   RETURN Qt_QGraphicsItemAnimation_yTranslationAt( ::pPtr, nStep )


METHOD QGraphicsItemAnimation:setStep( nStep )
   RETURN Qt_QGraphicsItemAnimation_setStep( ::pPtr, nStep )


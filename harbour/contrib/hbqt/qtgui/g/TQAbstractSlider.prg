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


FUNCTION QAbstractSlider( ... )
   RETURN HB_QAbstractSlider():new( ... )

FUNCTION QAbstractSliderFrom( ... )
   RETURN HB_QAbstractSlider():from( ... )

FUNCTION QAbstractSliderFromPointer( ... )
   RETURN HB_QAbstractSlider():fromPointer( ... )


CREATE CLASS QAbstractSlider INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QAbstractSlider

   METHOD  new( ... )

   METHOD  hasTracking                   // (  )                                               -> lBool
   METHOD  invertedAppearance            // (  )                                               -> lBool
   METHOD  invertedControls              // (  )                                               -> lBool
   METHOD  isSliderDown                  // (  )                                               -> lBool
   METHOD  maximum                       // (  )                                               -> nInt
   METHOD  minimum                       // (  )                                               -> nInt
   METHOD  orientation                   // (  )                                               -> nQt_Orientation
   METHOD  pageStep                      // (  )                                               -> nInt
   METHOD  setInvertedAppearance         // ( lBool )                                          -> NIL
   METHOD  setInvertedControls           // ( lBool )                                          -> NIL
   METHOD  setMaximum                    // ( nInt )                                           -> NIL
   METHOD  setMinimum                    // ( nInt )                                           -> NIL
   METHOD  setPageStep                   // ( nInt )                                           -> NIL
   METHOD  setRange                      // ( nMin, nMax )                                     -> NIL
   METHOD  setSingleStep                 // ( nInt )                                           -> NIL
   METHOD  setSliderDown                 // ( lBool )                                          -> NIL
   METHOD  setSliderPosition             // ( nInt )                                           -> NIL
   METHOD  setTracking                   // ( lEnable )                                        -> NIL
   METHOD  singleStep                    // (  )                                               -> nInt
   METHOD  sliderPosition                // (  )                                               -> nInt
   METHOD  triggerAction                 // ( nAction )                                        -> NIL
   METHOD  value                         // (  )                                               -> nInt
   METHOD  setOrientation                // ( nQt::Orientation )                               -> NIL
   METHOD  setValue                      // ( nInt )                                           -> NIL

   ENDCLASS


METHOD QAbstractSlider:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractSlider( ... )
   RETURN Self


METHOD QAbstractSlider:hasTracking( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_hasTracking( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:invertedAppearance( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_invertedAppearance( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:invertedControls( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_invertedControls( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:isSliderDown( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_isSliderDown( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:maximum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_maximum( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:minimum( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_minimum( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:orientation( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_orientation( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:pageStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_pageStep( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setInvertedAppearance( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setInvertedAppearance( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setInvertedControls( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setInvertedControls( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setMaximum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setMaximum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setMinimum( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setMinimum( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setPageStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setPageStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setRange( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QAbstractSlider_setRange( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setSingleStep( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSingleStep( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setSliderDown( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSliderDown( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setSliderPosition( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setSliderPosition( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setTracking( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setTracking( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:singleStep( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_singleStep( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:sliderPosition( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_sliderPosition( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:triggerAction( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_triggerAction( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:value( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QAbstractSlider_value( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setOrientation( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setOrientation( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QAbstractSlider:setValue( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QAbstractSlider_setValue( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


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


FUNCTION QLineF( ... )
   RETURN HB_QLineF():new( ... )


CREATE CLASS QLineF INHERIT HbQtObjectHandler FUNCTION HB_QLineF

   METHOD  new( ... )

   METHOD  p1                            // (  )                                               -> oQPointF
   METHOD  p2                            // (  )                                               -> oQPointF
   METHOD  x1                            // (  )                                               -> nQreal
   METHOD  x2                            // (  )                                               -> nQreal
   METHOD  y1                            // (  )                                               -> nQreal
   METHOD  y2                            // (  )                                               -> nQreal
   METHOD  angle                         // (  )                                               -> nQreal
   METHOD  angleTo                       // ( oQLineF )                                        -> nQreal
   METHOD  dx                            // (  )                                               -> nQreal
   METHOD  dy                            // (  )                                               -> nQreal
   METHOD  intersect                     // ( oQLineF, oQPointF )                              -> nIntersectType
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  length                        // (  )                                               -> nQreal
   METHOD  normalVector                  // (  )                                               -> oQLineF
   METHOD  pointAt                       // ( nT )                                             -> oQPointF
   METHOD  setP1                         // ( oQPointF )                                       -> NIL
   METHOD  setP2                         // ( oQPointF )                                       -> NIL
   METHOD  setAngle                      // ( nAngle )                                         -> NIL
   METHOD  setLength                     // ( nLength )                                        -> NIL
   METHOD  setLine                       // ( nX1, nY1, nX2, nY2 )                             -> NIL
   METHOD  setPoints                     // ( oQPointF, oQPointF )                             -> NIL
   METHOD  toLine                        // (  )                                               -> oQLine
   METHOD  translate                     // ( oQPointF )                                       -> NIL
                                         // ( nDx, nDy )                                       -> NIL
   METHOD  translated                    // ( oQPointF )                                       -> oQLineF
                                         // ( nDx, nDy )                                       -> oQLineF
   METHOD  unitVector                    // (  )                                               -> oQLineF

   ENDCLASS


METHOD QLineF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QLineF( ... )
   RETURN Self


METHOD QLineF:p1( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QPointF():from( Qt_QLineF_p1( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:p2( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QPointF():from( Qt_QLineF_p2( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:x1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_x1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:x2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_x2( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:y1( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_y1( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:y2( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_y2( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:angle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_angle( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:angleTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_angleTo( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:dx( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_dx( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:dy( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_dy( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:intersect( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QLineF_intersect( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:length( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QLineF_length( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:normalVector( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QLineF():from( Qt_QLineF_normalVector( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:pointAt( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QPointF():from( Qt_QLineF_pointAt( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setP1( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_setP1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setP2( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_setP2( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setAngle( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_setAngle( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setLength( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_setLength( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setLine( ... )
   SWITCH PCount()
   CASE 4
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) ) .AND. hb_isNumeric( hb_pvalue( 4 ) )
         RETURN Qt_QLineF_setLine( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:setPoints( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QLineF_setPoints( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:toLine( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QLine():from( Qt_QLineF_toLine( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:translate( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QLineF_translate_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QLineF_translate( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:translated( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QLineF():from( Qt_QLineF_translated_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QLineF():from( Qt_QLineF_translated( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QLineF:unitVector( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QLineF():from( Qt_QLineF_unitVector( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


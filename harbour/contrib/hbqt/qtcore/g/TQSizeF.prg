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


FUNCTION QSizeF( ... )
   RETURN HB_QSizeF():new( ... )

FUNCTION QSizeFFromPointer( ... )
   RETURN HB_QSizeF():fromPointer( ... )


CREATE CLASS QSizeF INHERIT HbQtObjectHandler FUNCTION HB_QSizeF

   METHOD  new( ... )

   METHOD  boundedTo                     // ( oQSizeF )                                        -> oQSizeF
   METHOD  expandedTo                    // ( oQSizeF )                                        -> oQSizeF
   METHOD  height                        // (  )                                               -> nQreal
   METHOD  isEmpty                       // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  rheight                       // (  )                                               -> nQreal
   METHOD  rwidth                        // (  )                                               -> nQreal
   METHOD  scale                         // ( nWidth, nHeight, nMode )                         -> NIL
                                         // ( oQSizeF, nMode )                                 -> NIL
   METHOD  setHeight                     // ( nHeight )                                        -> NIL
   METHOD  setWidth                      // ( nWidth )                                         -> NIL
   METHOD  toSize                        // (  )                                               -> oQSize
   METHOD  transpose                     // (  )                                               -> NIL
   METHOD  width                         // (  )                                               -> nQreal

   ENDCLASS


METHOD QSizeF:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSizeF( ... )
   RETURN Self


METHOD QSizeF:boundedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QSizeF_boundedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:expandedTo( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN QSizeFFromPointer( Qt_QSizeF_expandedTo( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:height( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_height( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isEmpty( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isEmpty( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:isValid( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_isValid( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:rheight( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_rheight( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:rwidth( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_rwidth( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:scale( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QSizeF_scale( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QSizeF_scale_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:setHeight( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizeF_setHeight( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:setWidth( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QSizeF_setWidth( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:toSize( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QSizeF_toSize( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:transpose( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_transpose( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QSizeF:width( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QSizeF_width( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


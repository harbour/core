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


FUNCTION QPen( ... )
   RETURN HB_QPen():new( ... )


CREATE CLASS QPen INHERIT HbQtObjectHandler FUNCTION HB_QPen

   METHOD  new( ... )

   METHOD  brush()
   METHOD  capStyle()
   METHOD  color()
   METHOD  dashOffset()
   METHOD  isCosmetic()
   METHOD  isSolid()
   METHOD  joinStyle()
   METHOD  miterLimit()
   METHOD  setBrush( pBrush )
   METHOD  setCapStyle( nStyle )
   METHOD  setColor( pColor )
   METHOD  setCosmetic( lCosmetic )
   METHOD  setDashOffset( nOffset )
   METHOD  setJoinStyle( nStyle )
   METHOD  setMiterLimit( nLimit )
   METHOD  setStyle( nStyle )
   METHOD  setWidth( nWidth )
   METHOD  setWidthF( nWidth )
   METHOD  style()
   METHOD  width()
   METHOD  widthF()

   ENDCLASS


METHOD QPen:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPen( ... )
   RETURN Self


METHOD QPen:brush()
   RETURN HB_QBrush():from( Qt_QPen_brush( ::pPtr ) )


METHOD QPen:capStyle()
   RETURN Qt_QPen_capStyle( ::pPtr )


METHOD QPen:color()
   RETURN HB_QColor():from( Qt_QPen_color( ::pPtr ) )


METHOD QPen:dashOffset()
   RETURN Qt_QPen_dashOffset( ::pPtr )


METHOD QPen:isCosmetic()
   RETURN Qt_QPen_isCosmetic( ::pPtr )


METHOD QPen:isSolid()
   RETURN Qt_QPen_isSolid( ::pPtr )


METHOD QPen:joinStyle()
   RETURN Qt_QPen_joinStyle( ::pPtr )


METHOD QPen:miterLimit()
   RETURN Qt_QPen_miterLimit( ::pPtr )


METHOD QPen:setBrush( pBrush )
   RETURN Qt_QPen_setBrush( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QPen:setCapStyle( nStyle )
   RETURN Qt_QPen_setCapStyle( ::pPtr, nStyle )


METHOD QPen:setColor( pColor )
   RETURN Qt_QPen_setColor( ::pPtr, hbqt_ptr( pColor ) )


METHOD QPen:setCosmetic( lCosmetic )
   RETURN Qt_QPen_setCosmetic( ::pPtr, lCosmetic )


METHOD QPen:setDashOffset( nOffset )
   RETURN Qt_QPen_setDashOffset( ::pPtr, nOffset )


METHOD QPen:setJoinStyle( nStyle )
   RETURN Qt_QPen_setJoinStyle( ::pPtr, nStyle )


METHOD QPen:setMiterLimit( nLimit )
   RETURN Qt_QPen_setMiterLimit( ::pPtr, nLimit )


METHOD QPen:setStyle( nStyle )
   RETURN Qt_QPen_setStyle( ::pPtr, nStyle )


METHOD QPen:setWidth( nWidth )
   RETURN Qt_QPen_setWidth( ::pPtr, nWidth )


METHOD QPen:setWidthF( nWidth )
   RETURN Qt_QPen_setWidthF( ::pPtr, nWidth )


METHOD QPen:style()
   RETURN Qt_QPen_style( ::pPtr )


METHOD QPen:width()
   RETURN Qt_QPen_width( ::pPtr )


METHOD QPen:widthF()
   RETURN Qt_QPen_widthF( ::pPtr )


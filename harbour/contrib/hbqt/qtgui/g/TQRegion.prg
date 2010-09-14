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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QRegion( ... )
   RETURN HB_QRegion():new( ... )


CREATE CLASS QRegion INHERIT HbQtObjectHandler FUNCTION HB_QRegion

   METHOD  new( ... )

   METHOD  boundingRect()
   METHOD  contains( pP )
   METHOD  contains_1( pR )
   METHOD  intersected( pR )
   METHOD  intersected_1( pRect )
   METHOD  intersects( pRegion )
   METHOD  intersects_1( pRect )
   METHOD  isEmpty()
   METHOD  numRects()
   METHOD  setRects( pRects, nNumber )
   METHOD  subtracted( pR )
   METHOD  translate( nDx, nDy )
   METHOD  translate_1( pPoint )
   METHOD  translated( nDx, nDy )
   METHOD  translated_1( pP )
   METHOD  united( pR )
   METHOD  united_1( pRect )
   METHOD  xored( pR )

   ENDCLASS


METHOD QRegion:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QRegion( ... )
   RETURN Self


METHOD QRegion:boundingRect()
   RETURN Qt_QRegion_boundingRect( ::pPtr )


METHOD QRegion:contains( pP )
   RETURN Qt_QRegion_contains( ::pPtr, hbqt_ptr( pP ) )


METHOD QRegion:contains_1( pR )
   RETURN Qt_QRegion_contains_1( ::pPtr, hbqt_ptr( pR ) )


METHOD QRegion:intersected( pR )
   RETURN Qt_QRegion_intersected( ::pPtr, hbqt_ptr( pR ) )


METHOD QRegion:intersected_1( pRect )
   RETURN Qt_QRegion_intersected_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QRegion:intersects( pRegion )
   RETURN Qt_QRegion_intersects( ::pPtr, hbqt_ptr( pRegion ) )


METHOD QRegion:intersects_1( pRect )
   RETURN Qt_QRegion_intersects_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QRegion:isEmpty()
   RETURN Qt_QRegion_isEmpty( ::pPtr )


METHOD QRegion:numRects()
   RETURN Qt_QRegion_numRects( ::pPtr )


METHOD QRegion:setRects( pRects, nNumber )
   RETURN Qt_QRegion_setRects( ::pPtr, hbqt_ptr( pRects ), nNumber )


METHOD QRegion:subtracted( pR )
   RETURN Qt_QRegion_subtracted( ::pPtr, hbqt_ptr( pR ) )


METHOD QRegion:translate( nDx, nDy )
   RETURN Qt_QRegion_translate( ::pPtr, nDx, nDy )


METHOD QRegion:translate_1( pPoint )
   RETURN Qt_QRegion_translate_1( ::pPtr, hbqt_ptr( pPoint ) )


METHOD QRegion:translated( nDx, nDy )
   RETURN Qt_QRegion_translated( ::pPtr, nDx, nDy )


METHOD QRegion:translated_1( pP )
   RETURN Qt_QRegion_translated_1( ::pPtr, hbqt_ptr( pP ) )


METHOD QRegion:united( pR )
   RETURN Qt_QRegion_united( ::pPtr, hbqt_ptr( pR ) )


METHOD QRegion:united_1( pRect )
   RETURN Qt_QRegion_united_1( ::pPtr, hbqt_ptr( pRect ) )


METHOD QRegion:xored( pR )
   RETURN Qt_QRegion_xored( ::pPtr, hbqt_ptr( pR ) )


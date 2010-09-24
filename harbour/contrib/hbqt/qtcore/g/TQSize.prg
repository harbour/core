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


FUNCTION QSize( ... )
   RETURN HB_QSize():new( ... )


CREATE CLASS QSize INHERIT HbQtObjectHandler FUNCTION HB_QSize

   METHOD  new( ... )

   METHOD  height()
   METHOD  isEmpty()
   METHOD  isNull()
   METHOD  isValid()
   METHOD  rheight()
   METHOD  rwidth()
   METHOD  scale( ... )
   METHOD  setHeight( nHeight )
   METHOD  setWidth( nWidth )
   METHOD  transpose()
   METHOD  width()
   METHOD  boundedTo( pOtherSize )
   METHOD  expandedTo( pOtherSize )

   ENDCLASS


METHOD QSize:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QSize( ... )
   RETURN Self


METHOD QSize:height()
   RETURN Qt_QSize_height( ::pPtr )


METHOD QSize:isEmpty()
   RETURN Qt_QSize_isEmpty( ::pPtr )


METHOD QSize:isNull()
   RETURN Qt_QSize_isNull( ::pPtr )


METHOD QSize:isValid()
   RETURN Qt_QSize_isValid( ::pPtr )


METHOD QSize:rheight()
   RETURN Qt_QSize_rheight( ::pPtr )


METHOD QSize:rwidth()
   RETURN Qt_QSize_rwidth( ::pPtr )


METHOD QSize:scale( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 3
      DO CASE
      CASE aV[ 1 ] $ "N" .AND. aV[ 2 ] $ "N" .AND. aV[ 3 ] $ "N"
                // void scale ( int width, int height, Qt::AspectRatioMode mode )
                // N n int, N n int, N n Qt::AspectRatioMode
         RETURN Qt_QSize_scale( ::pPtr, ... )
      ENDCASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "PO" .AND. aV[ 2 ] $ "N"
                // void scale ( const QSize & size, Qt::AspectRatioMode mode )
                // PO p QSize, N n Qt::AspectRatioMode
         RETURN Qt_QSize_scale_1( ::pPtr, ... )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QSize:setHeight( nHeight )
   RETURN Qt_QSize_setHeight( ::pPtr, nHeight )


METHOD QSize:setWidth( nWidth )
   RETURN Qt_QSize_setWidth( ::pPtr, nWidth )


METHOD QSize:transpose()
   RETURN Qt_QSize_transpose( ::pPtr )


METHOD QSize:width()
   RETURN Qt_QSize_width( ::pPtr )


METHOD QSize:boundedTo( pOtherSize )
   RETURN Qt_QSize_boundedTo( ::pPtr, hbqt_ptr( pOtherSize ) )


METHOD QSize:expandedTo( pOtherSize )
   RETURN Qt_QSize_expandedTo( ::pPtr, hbqt_ptr( pOtherSize ) )


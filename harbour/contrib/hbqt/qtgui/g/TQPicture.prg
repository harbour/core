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


FUNCTION QPicture( ... )
   RETURN HB_QPicture():new( ... )


CREATE CLASS QPicture INHERIT HbQtObjectHandler, HB_QPaintDevice FUNCTION HB_QPicture

   METHOD  new( ... )

   METHOD  boundingRect()
   METHOD  data()
   METHOD  isNull()
   METHOD  load( cFileName, pFormat )
   METHOD  load_1( pDev, pFormat )
   METHOD  play( pPainter )
   METHOD  save( cFileName, pFormat )
   METHOD  save_1( pDev, pFormat )
   METHOD  setBoundingRect( pR )
   METHOD  setData( pData, nSize )
   METHOD  size()

   ENDCLASS


METHOD QPicture:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPicture( ... )
   RETURN Self


METHOD QPicture:boundingRect()
   RETURN Qt_QPicture_boundingRect( ::pPtr )


METHOD QPicture:data()
   RETURN Qt_QPicture_data( ::pPtr )


METHOD QPicture:isNull()
   RETURN Qt_QPicture_isNull( ::pPtr )


METHOD QPicture:load( cFileName, pFormat )
   RETURN Qt_QPicture_load( ::pPtr, cFileName, hbqt_ptr( pFormat ) )


METHOD QPicture:load_1( pDev, pFormat )
   RETURN Qt_QPicture_load_1( ::pPtr, hbqt_ptr( pDev ), hbqt_ptr( pFormat ) )


METHOD QPicture:play( pPainter )
   RETURN Qt_QPicture_play( ::pPtr, hbqt_ptr( pPainter ) )


METHOD QPicture:save( cFileName, pFormat )
   RETURN Qt_QPicture_save( ::pPtr, cFileName, hbqt_ptr( pFormat ) )


METHOD QPicture:save_1( pDev, pFormat )
   RETURN Qt_QPicture_save_1( ::pPtr, hbqt_ptr( pDev ), hbqt_ptr( pFormat ) )


METHOD QPicture:setBoundingRect( pR )
   RETURN Qt_QPicture_setBoundingRect( ::pPtr, hbqt_ptr( pR ) )


METHOD QPicture:setData( pData, nSize )
   RETURN Qt_QPicture_setData( ::pPtr, hbqt_ptr( pData ), nSize )


METHOD QPicture:size()
   RETURN Qt_QPicture_size( ::pPtr )


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


FUNCTION QIcon( ... )
   RETURN HB_QIcon():new( ... )


CREATE CLASS QIcon INHERIT HbQtObjectHandler FUNCTION HB_QIcon

   METHOD  new( ... )

   METHOD  actualSize( pSize, nMode, nState )
   METHOD  addFile( cFileName, pSize, nMode, nState )
   METHOD  addPixmap( pPixmap, nMode, nState )
   METHOD  availableSizes( nMode, nState )
   METHOD  cacheKey()
   METHOD  isNull()
   METHOD  paint( pPainter, pRect, nAlignment, nMode, nState )
   METHOD  paint_1( pPainter, nX, nY, nW, nH, nAlignment, nMode, nState )
   METHOD  pixmap( pSize, nMode, nState )
   METHOD  pixmap_1( nW, nH, nMode, nState )
   METHOD  pixmap_2( nExtent, nMode, nState )

   ENDCLASS


METHOD QIcon:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QIcon( ... )
   RETURN Self


METHOD QIcon:actualSize( pSize, nMode, nState )
   RETURN Qt_QIcon_actualSize( ::pPtr, hbqt_ptr( pSize ), nMode, nState )


METHOD QIcon:addFile( cFileName, pSize, nMode, nState )
   RETURN Qt_QIcon_addFile( ::pPtr, cFileName, hbqt_ptr( pSize ), nMode, nState )


METHOD QIcon:addPixmap( pPixmap, nMode, nState )
   RETURN Qt_QIcon_addPixmap( ::pPtr, hbqt_ptr( pPixmap ), nMode, nState )


METHOD QIcon:availableSizes( nMode, nState )
   RETURN Qt_QIcon_availableSizes( ::pPtr, nMode, nState )


METHOD QIcon:cacheKey()
   RETURN Qt_QIcon_cacheKey( ::pPtr )


METHOD QIcon:isNull()
   RETURN Qt_QIcon_isNull( ::pPtr )


METHOD QIcon:paint( pPainter, pRect, nAlignment, nMode, nState )
   RETURN Qt_QIcon_paint( ::pPtr, hbqt_ptr( pPainter ), hbqt_ptr( pRect ), nAlignment, nMode, nState )


METHOD QIcon:paint_1( pPainter, nX, nY, nW, nH, nAlignment, nMode, nState )
   RETURN Qt_QIcon_paint_1( ::pPtr, hbqt_ptr( pPainter ), nX, nY, nW, nH, nAlignment, nMode, nState )


METHOD QIcon:pixmap( pSize, nMode, nState )
   RETURN Qt_QIcon_pixmap( ::pPtr, hbqt_ptr( pSize ), nMode, nState )


METHOD QIcon:pixmap_1( nW, nH, nMode, nState )
   RETURN Qt_QIcon_pixmap_1( ::pPtr, nW, nH, nMode, nState )


METHOD QIcon:pixmap_2( nExtent, nMode, nState )
   RETURN Qt_QIcon_pixmap_2( ::pPtr, nExtent, nMode, nState )


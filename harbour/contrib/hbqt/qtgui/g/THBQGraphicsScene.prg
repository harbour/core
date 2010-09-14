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


FUNCTION HBQGraphicsScene( ... )
   RETURN HB_HBQGraphicsScene():new( ... )


CREATE CLASS HBQGraphicsScene INHERIT HbQtObjectHandler, HB_QGraphicsScene FUNCTION HB_HBQGraphicsScene

   METHOD  new( ... )

   METHOD  hbSetBlock( xBlock )
   METHOD  pageSize()
   METHOD  setPageSize( nPageSize )
   METHOD  paperRect()
   METHOD  setPaperRect( pPaperRect )
   METHOD  orientation()
   METHOD  setOrientation( nOrientation )
   METHOD  geometry()
   METHOD  setGeometry( pRect )
   METHOD  magnetArea()
   METHOD  setMagnetArea( nMagnetArea )
   METHOD  showGrid()
   METHOD  setShowGrid( lShowGrid )
   METHOD  setLeftMagnet( lMagneted )
   METHOD  setRightMagnet( lMagneted )
   METHOD  setTopMagnet( lMagneted )
   METHOD  setBottomMagnet( lMagneted )
   METHOD  setHorizontalMagnet( lMagneted )
   METHOD  setVerticalMagnet( lMagneted )

   ENDCLASS


METHOD HBQGraphicsScene:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQGraphicsScene( ... )
   RETURN Self


METHOD HBQGraphicsScene:hbSetBlock( xBlock )
   RETURN Qt_HBQGraphicsScene_hbSetBlock( ::pPtr, xBlock )


METHOD HBQGraphicsScene:pageSize()
   RETURN Qt_HBQGraphicsScene_pageSize( ::pPtr )


METHOD HBQGraphicsScene:setPageSize( nPageSize )
   RETURN Qt_HBQGraphicsScene_setPageSize( ::pPtr, nPageSize )


METHOD HBQGraphicsScene:paperRect()
   RETURN Qt_HBQGraphicsScene_paperRect( ::pPtr )


METHOD HBQGraphicsScene:setPaperRect( pPaperRect )
   RETURN Qt_HBQGraphicsScene_setPaperRect( ::pPtr, hbqt_ptr( pPaperRect ) )


METHOD HBQGraphicsScene:orientation()
   RETURN Qt_HBQGraphicsScene_orientation( ::pPtr )


METHOD HBQGraphicsScene:setOrientation( nOrientation )
   RETURN Qt_HBQGraphicsScene_setOrientation( ::pPtr, nOrientation )


METHOD HBQGraphicsScene:geometry()
   RETURN Qt_HBQGraphicsScene_geometry( ::pPtr )


METHOD HBQGraphicsScene:setGeometry( pRect )
   RETURN Qt_HBQGraphicsScene_setGeometry( ::pPtr, hbqt_ptr( pRect ) )


METHOD HBQGraphicsScene:magnetArea()
   RETURN Qt_HBQGraphicsScene_magnetArea( ::pPtr )


METHOD HBQGraphicsScene:setMagnetArea( nMagnetArea )
   RETURN Qt_HBQGraphicsScene_setMagnetArea( ::pPtr, nMagnetArea )


METHOD HBQGraphicsScene:showGrid()
   RETURN Qt_HBQGraphicsScene_showGrid( ::pPtr )


METHOD HBQGraphicsScene:setShowGrid( lShowGrid )
   RETURN Qt_HBQGraphicsScene_setShowGrid( ::pPtr, lShowGrid )


METHOD HBQGraphicsScene:setLeftMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setLeftMagnet( ::pPtr, lMagneted )


METHOD HBQGraphicsScene:setRightMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setRightMagnet( ::pPtr, lMagneted )


METHOD HBQGraphicsScene:setTopMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setTopMagnet( ::pPtr, lMagneted )


METHOD HBQGraphicsScene:setBottomMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setBottomMagnet( ::pPtr, lMagneted )


METHOD HBQGraphicsScene:setHorizontalMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setHorizontalMagnet( ::pPtr, lMagneted )


METHOD HBQGraphicsScene:setVerticalMagnet( lMagneted )
   RETURN Qt_HBQGraphicsScene_setVerticalMagnet( ::pPtr, lMagneted )


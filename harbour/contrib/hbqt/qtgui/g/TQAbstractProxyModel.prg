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


FUNCTION QAbstractProxyModel( ... )
   RETURN HB_QAbstractProxyModel():new( ... )


CREATE CLASS QAbstractProxyModel INHERIT HbQtObjectHandler, HB_QAbstractItemModel FUNCTION HB_QAbstractProxyModel

   METHOD  new( ... )

   METHOD  mapFromSource( pSourceIndex )
   METHOD  mapSelectionFromSource( pSourceSelection )
   METHOD  mapSelectionToSource( pProxySelection )
   METHOD  mapToSource( pProxyIndex )
   METHOD  setSourceModel( pSourceModel )
   METHOD  sourceModel()

   ENDCLASS


METHOD QAbstractProxyModel:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QAbstractProxyModel( ... )
   RETURN Self


METHOD QAbstractProxyModel:mapFromSource( pSourceIndex )
   RETURN Qt_QAbstractProxyModel_mapFromSource( ::pPtr, hbqt_ptr( pSourceIndex ) )


METHOD QAbstractProxyModel:mapSelectionFromSource( pSourceSelection )
   RETURN Qt_QAbstractProxyModel_mapSelectionFromSource( ::pPtr, hbqt_ptr( pSourceSelection ) )


METHOD QAbstractProxyModel:mapSelectionToSource( pProxySelection )
   RETURN Qt_QAbstractProxyModel_mapSelectionToSource( ::pPtr, hbqt_ptr( pProxySelection ) )


METHOD QAbstractProxyModel:mapToSource( pProxyIndex )
   RETURN Qt_QAbstractProxyModel_mapToSource( ::pPtr, hbqt_ptr( pProxyIndex ) )


METHOD QAbstractProxyModel:setSourceModel( pSourceModel )
   RETURN Qt_QAbstractProxyModel_setSourceModel( ::pPtr, hbqt_ptr( pSourceModel ) )


METHOD QAbstractProxyModel:sourceModel()
   RETURN Qt_QAbstractProxyModel_sourceModel( ::pPtr )


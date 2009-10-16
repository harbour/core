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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QStandardItemModel INHERIT QAbstractItemModeL

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  appendRow( pItem )                  INLINE  Qt_QStandardItemModel_appendRow( ::pPtr, pItem )
   METHOD  clear()                             INLINE  Qt_QStandardItemModel_clear( ::pPtr )
   METHOD  horizontalHeaderItem( nColumn )     INLINE  Qt_QStandardItemModel_horizontalHeaderItem( ::pPtr, nColumn )
   METHOD  indexFromItem( pItem )              INLINE  Qt_QStandardItemModel_indexFromItem( ::pPtr, pItem )
   METHOD  insertColumn( nColumn, pParent )    INLINE  Qt_QStandardItemModel_insertColumn( ::pPtr, nColumn, pParent )
   METHOD  insertRow( nRow, pParent )          INLINE  Qt_QStandardItemModel_insertRow( ::pPtr, nRow, pParent )
   METHOD  insertRow_1( nRow, pItem )          INLINE  Qt_QStandardItemModel_insertRow_1( ::pPtr, nRow, pItem )
   METHOD  invisibleRootItem()                 INLINE  Qt_QStandardItemModel_invisibleRootItem( ::pPtr )
   METHOD  item( nRow, nColumn )               INLINE  Qt_QStandardItemModel_item( ::pPtr, nRow, nColumn )
   METHOD  itemFromIndex( pIndex )             INLINE  Qt_QStandardItemModel_itemFromIndex( ::pPtr, pIndex )
   METHOD  itemPrototype()                     INLINE  Qt_QStandardItemModel_itemPrototype( ::pPtr )
   METHOD  setColumnCount( nColumns )          INLINE  Qt_QStandardItemModel_setColumnCount( ::pPtr, nColumns )
   METHOD  setHorizontalHeaderItem( nColumn, pItem )  INLINE  Qt_QStandardItemModel_setHorizontalHeaderItem( ::pPtr, nColumn, pItem )
   METHOD  setHorizontalHeaderLabels( pLabels )  INLINE  Qt_QStandardItemModel_setHorizontalHeaderLabels( ::pPtr, pLabels )
   METHOD  setItem( nRow, nColumn, pItem )     INLINE  Qt_QStandardItemModel_setItem( ::pPtr, nRow, nColumn, pItem )
   METHOD  setItem_1( nRow, pItem )            INLINE  Qt_QStandardItemModel_setItem_1( ::pPtr, nRow, pItem )
   METHOD  setItemPrototype( pItem )           INLINE  Qt_QStandardItemModel_setItemPrototype( ::pPtr, pItem )
   METHOD  setRowCount( nRows )                INLINE  Qt_QStandardItemModel_setRowCount( ::pPtr, nRows )
   METHOD  setSortRole( nRole )                INLINE  Qt_QStandardItemModel_setSortRole( ::pPtr, nRole )
   METHOD  setVerticalHeaderItem( nRow, pItem )  INLINE  Qt_QStandardItemModel_setVerticalHeaderItem( ::pPtr, nRow, pItem )
   METHOD  setVerticalHeaderLabels( pLabels )  INLINE  Qt_QStandardItemModel_setVerticalHeaderLabels( ::pPtr, pLabels )
   METHOD  sortRole()                          INLINE  Qt_QStandardItemModel_sortRole( ::pPtr )
   METHOD  takeHorizontalHeaderItem( nColumn )  INLINE  Qt_QStandardItemModel_takeHorizontalHeaderItem( ::pPtr, nColumn )
   METHOD  takeItem( nRow, nColumn )           INLINE  Qt_QStandardItemModel_takeItem( ::pPtr, nRow, nColumn )
   METHOD  takeVerticalHeaderItem( nRow )      INLINE  Qt_QStandardItemModel_takeVerticalHeaderItem( ::pPtr, nRow )
   METHOD  verticalHeaderItem( nRow )          INLINE  Qt_QStandardItemModel_verticalHeaderItem( ::pPtr, nRow )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QStandardItemModel

   ::pParent := pParent

   ::pPtr := Qt_QStandardItemModel( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QStandardItemModel

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

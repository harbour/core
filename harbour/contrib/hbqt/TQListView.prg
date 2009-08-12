/*
 * $Id$
 */

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


CREATE CLASS QListView INHERIT QAbstractItemView

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QListView_destroy( ::pPtr )

   METHOD  batchSize()                         INLINE  Qt_QListView_batchSize( ::pPtr )
   METHOD  clearPropertyFlags()                INLINE  Qt_QListView_clearPropertyFlags( ::pPtr )
   METHOD  flow()                              INLINE  Qt_QListView_flow( ::pPtr )
   METHOD  gridSize()                          INLINE  Qt_QListView_gridSize( ::pPtr )
   METHOD  isRowHidden( nRow )                 INLINE  Qt_QListView_isRowHidden( ::pPtr, nRow )
   METHOD  isSelectionRectVisible()            INLINE  Qt_QListView_isSelectionRectVisible( ::pPtr )
   METHOD  isWrapping()                        INLINE  Qt_QListView_isWrapping( ::pPtr )
   METHOD  layoutMode()                        INLINE  Qt_QListView_layoutMode( ::pPtr )
   METHOD  modelColumn()                       INLINE  Qt_QListView_modelColumn( ::pPtr )
   METHOD  movement()                          INLINE  Qt_QListView_movement( ::pPtr )
   METHOD  resizeMode()                        INLINE  Qt_QListView_resizeMode( ::pPtr )
   METHOD  setBatchSize( nBatchSize )          INLINE  Qt_QListView_setBatchSize( ::pPtr, nBatchSize )
   METHOD  setFlow( nFlow )                    INLINE  Qt_QListView_setFlow( ::pPtr, nFlow )
   METHOD  setGridSize( pSize )                INLINE  Qt_QListView_setGridSize( ::pPtr, pSize )
   METHOD  setLayoutMode( nMode )              INLINE  Qt_QListView_setLayoutMode( ::pPtr, nMode )
   METHOD  setModelColumn( nColumn )           INLINE  Qt_QListView_setModelColumn( ::pPtr, nColumn )
   METHOD  setMovement( nMovement )            INLINE  Qt_QListView_setMovement( ::pPtr, nMovement )
   METHOD  setResizeMode( nMode )              INLINE  Qt_QListView_setResizeMode( ::pPtr, nMode )
   METHOD  setRowHidden( nRow, lHide )         INLINE  Qt_QListView_setRowHidden( ::pPtr, nRow, lHide )
   METHOD  setSelectionRectVisible( lShow )    INLINE  Qt_QListView_setSelectionRectVisible( ::pPtr, lShow )
   METHOD  setSpacing( nSpace )                INLINE  Qt_QListView_setSpacing( ::pPtr, nSpace )
   METHOD  setUniformItemSizes( lEnable )      INLINE  Qt_QListView_setUniformItemSizes( ::pPtr, lEnable )
   METHOD  setViewMode( nMode )                INLINE  Qt_QListView_setViewMode( ::pPtr, nMode )
   METHOD  setWordWrap( lOn )                  INLINE  Qt_QListView_setWordWrap( ::pPtr, lOn )
   METHOD  setWrapping( lEnable )              INLINE  Qt_QListView_setWrapping( ::pPtr, lEnable )
   METHOD  spacing()                           INLINE  Qt_QListView_spacing( ::pPtr )
   METHOD  uniformItemSizes()                  INLINE  Qt_QListView_uniformItemSizes( ::pPtr )
   METHOD  viewMode()                          INLINE  Qt_QListView_viewMode( ::pPtr )
   METHOD  wordWrap()                          INLINE  Qt_QListView_wordWrap( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QListView

   ::pParent := pParent

   ::pPtr := Qt_QListView( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QListView

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

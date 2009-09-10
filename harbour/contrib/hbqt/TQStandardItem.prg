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


CREATE CLASS QStandardItem

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QStandardItem_destroy( ::pPtr )

   METHOD  accessibleDescription()             INLINE  Qt_QStandardItem_accessibleDescription( ::pPtr )
   METHOD  accessibleText()                    INLINE  Qt_QStandardItem_accessibleText( ::pPtr )
   METHOD  appendRow( pItem )                  INLINE  Qt_QStandardItem_appendRow( ::pPtr, pItem )
   METHOD  background()                        INLINE  Qt_QStandardItem_background( ::pPtr )
   METHOD  checkState()                        INLINE  Qt_QStandardItem_checkState( ::pPtr )
   METHOD  child( nRow, nColumn )              INLINE  Qt_QStandardItem_child( ::pPtr, nRow, nColumn )
   METHOD  clone()                             INLINE  Qt_QStandardItem_clone( ::pPtr )
   METHOD  column()                            INLINE  Qt_QStandardItem_column( ::pPtr )
   METHOD  columnCount()                       INLINE  Qt_QStandardItem_columnCount( ::pPtr )
   METHOD  data( nRole )                       INLINE  Qt_QStandardItem_data( ::pPtr, nRole )
   METHOD  flags()                             INLINE  Qt_QStandardItem_flags( ::pPtr )
   METHOD  font()                              INLINE  Qt_QStandardItem_font( ::pPtr )
   METHOD  foreground()                        INLINE  Qt_QStandardItem_foreground( ::pPtr )
   METHOD  hasChildren()                       INLINE  Qt_QStandardItem_hasChildren( ::pPtr )
   METHOD  icon()                              INLINE  Qt_QStandardItem_icon( ::pPtr )
   METHOD  index()                             INLINE  Qt_QStandardItem_index( ::pPtr )
   METHOD  insertColumns( nColumn, nCount )    INLINE  Qt_QStandardItem_insertColumns( ::pPtr, nColumn, nCount )
   METHOD  insertRow( nRow, pItem )            INLINE  Qt_QStandardItem_insertRow( ::pPtr, nRow, pItem )
   METHOD  insertRows( nRow, nCount )          INLINE  Qt_QStandardItem_insertRows( ::pPtr, nRow, nCount )
   METHOD  isCheckable()                       INLINE  Qt_QStandardItem_isCheckable( ::pPtr )
   METHOD  isDragEnabled()                     INLINE  Qt_QStandardItem_isDragEnabled( ::pPtr )
   METHOD  isDropEnabled()                     INLINE  Qt_QStandardItem_isDropEnabled( ::pPtr )
   METHOD  isEditable()                        INLINE  Qt_QStandardItem_isEditable( ::pPtr )
   METHOD  isEnabled()                         INLINE  Qt_QStandardItem_isEnabled( ::pPtr )
   METHOD  isSelectable()                      INLINE  Qt_QStandardItem_isSelectable( ::pPtr )
   METHOD  isTristate()                        INLINE  Qt_QStandardItem_isTristate( ::pPtr )
   METHOD  model()                             INLINE  Qt_QStandardItem_model( ::pPtr )
   METHOD  parent()                            INLINE  Qt_QStandardItem_parent( ::pPtr )
   METHOD  read( pIn )                         INLINE  Qt_QStandardItem_read( ::pPtr, pIn )
   METHOD  removeColumn( nColumn )             INLINE  Qt_QStandardItem_removeColumn( ::pPtr, nColumn )
   METHOD  removeColumns( nColumn, nCount )    INLINE  Qt_QStandardItem_removeColumns( ::pPtr, nColumn, nCount )
   METHOD  removeRow( nRow )                   INLINE  Qt_QStandardItem_removeRow( ::pPtr, nRow )
   METHOD  removeRows( nRow, nCount )          INLINE  Qt_QStandardItem_removeRows( ::pPtr, nRow, nCount )
   METHOD  row()                               INLINE  Qt_QStandardItem_row( ::pPtr )
   METHOD  rowCount()                          INLINE  Qt_QStandardItem_rowCount( ::pPtr )
   METHOD  setAccessibleDescription( cAccessibleDescription )  INLINE  Qt_QStandardItem_setAccessibleDescription( ::pPtr, cAccessibleDescription )
   METHOD  setAccessibleText( cAccessibleText )  INLINE  Qt_QStandardItem_setAccessibleText( ::pPtr, cAccessibleText )
   METHOD  setBackground( pBrush )             INLINE  Qt_QStandardItem_setBackground( ::pPtr, pBrush )
   METHOD  setCheckState( nState )             INLINE  Qt_QStandardItem_setCheckState( ::pPtr, nState )
   METHOD  setCheckable( lCheckable )          INLINE  Qt_QStandardItem_setCheckable( ::pPtr, lCheckable )
   METHOD  setChild( nRow, nColumn, pItem )    INLINE  Qt_QStandardItem_setChild( ::pPtr, nRow, nColumn, pItem )
   METHOD  setChild_1( nRow, pItem )           INLINE  Qt_QStandardItem_setChild_1( ::pPtr, nRow, pItem )
   METHOD  setColumnCount( nColumns )          INLINE  Qt_QStandardItem_setColumnCount( ::pPtr, nColumns )
   METHOD  setData( pValue, nRole )            INLINE  Qt_QStandardItem_setData( ::pPtr, pValue, nRole )
   METHOD  setDragEnabled( lDragEnabled )      INLINE  Qt_QStandardItem_setDragEnabled( ::pPtr, lDragEnabled )
   METHOD  setDropEnabled( lDropEnabled )      INLINE  Qt_QStandardItem_setDropEnabled( ::pPtr, lDropEnabled )
   METHOD  setEditable( lEditable )            INLINE  Qt_QStandardItem_setEditable( ::pPtr, lEditable )
   METHOD  setEnabled( lEnabled )              INLINE  Qt_QStandardItem_setEnabled( ::pPtr, lEnabled )
   METHOD  setFlags( nFlags )                  INLINE  Qt_QStandardItem_setFlags( ::pPtr, nFlags )
   METHOD  setFont( pFont )                    INLINE  Qt_QStandardItem_setFont( ::pPtr, pFont )
   METHOD  setForeground( pBrush )             INLINE  Qt_QStandardItem_setForeground( ::pPtr, pBrush )
   METHOD  setIcon( cIcon )                    INLINE  Qt_QStandardItem_setIcon( ::pPtr, cIcon )
   METHOD  setRowCount( nRows )                INLINE  Qt_QStandardItem_setRowCount( ::pPtr, nRows )
   METHOD  setSelectable( lSelectable )        INLINE  Qt_QStandardItem_setSelectable( ::pPtr, lSelectable )
   METHOD  setSizeHint( pSize )                INLINE  Qt_QStandardItem_setSizeHint( ::pPtr, pSize )
   METHOD  setStatusTip( cStatusTip )          INLINE  Qt_QStandardItem_setStatusTip( ::pPtr, cStatusTip )
   METHOD  setText( cText )                    INLINE  Qt_QStandardItem_setText( ::pPtr, cText )
   METHOD  setTextAlignment( nAlignment )      INLINE  Qt_QStandardItem_setTextAlignment( ::pPtr, nAlignment )
   METHOD  setToolTip( cToolTip )              INLINE  Qt_QStandardItem_setToolTip( ::pPtr, cToolTip )
   METHOD  setTristate( lTristate )            INLINE  Qt_QStandardItem_setTristate( ::pPtr, lTristate )
   METHOD  setWhatsThis( cWhatsThis )          INLINE  Qt_QStandardItem_setWhatsThis( ::pPtr, cWhatsThis )
   METHOD  sizeHint()                          INLINE  Qt_QStandardItem_sizeHint( ::pPtr )
   METHOD  sortChildren( nColumn, nOrder )     INLINE  Qt_QStandardItem_sortChildren( ::pPtr, nColumn, nOrder )
   METHOD  statusTip()                         INLINE  Qt_QStandardItem_statusTip( ::pPtr )
   METHOD  takeChild( nRow, nColumn )          INLINE  Qt_QStandardItem_takeChild( ::pPtr, nRow, nColumn )
   METHOD  text()                              INLINE  Qt_QStandardItem_text( ::pPtr )
   METHOD  textAlignment()                     INLINE  Qt_QStandardItem_textAlignment( ::pPtr )
   METHOD  toolTip()                           INLINE  Qt_QStandardItem_toolTip( ::pPtr )
   METHOD  type()                              INLINE  Qt_QStandardItem_type( ::pPtr )
   METHOD  whatsThis()                         INLINE  Qt_QStandardItem_whatsThis( ::pPtr )
   METHOD  write( pOut )                       INLINE  Qt_QStandardItem_write( ::pPtr, pOut )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QStandardItem

   ::pParent := pParent

   ::pPtr := Qt_QStandardItem( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QStandardItem

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

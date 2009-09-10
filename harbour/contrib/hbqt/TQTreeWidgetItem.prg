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


CREATE CLASS QTreeWidgetItem

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTreeWidgetItem_destroy( ::pPtr )

   METHOD  addChild( pChild )                  INLINE  Qt_QTreeWidgetItem_addChild( ::pPtr, pChild )
   METHOD  background( nColumn )               INLINE  Qt_QTreeWidgetItem_background( ::pPtr, nColumn )
   METHOD  checkState( nColumn )               INLINE  Qt_QTreeWidgetItem_checkState( ::pPtr, nColumn )
   METHOD  child( nIndex )                     INLINE  Qt_QTreeWidgetItem_child( ::pPtr, nIndex )
   METHOD  childCount()                        INLINE  Qt_QTreeWidgetItem_childCount( ::pPtr )
   METHOD  childIndicatorPolicy()              INLINE  Qt_QTreeWidgetItem_childIndicatorPolicy( ::pPtr )
   METHOD  clone()                             INLINE  Qt_QTreeWidgetItem_clone( ::pPtr )
   METHOD  columnCount()                       INLINE  Qt_QTreeWidgetItem_columnCount( ::pPtr )
   METHOD  data( nColumn, nRole )              INLINE  Qt_QTreeWidgetItem_data( ::pPtr, nColumn, nRole )
   METHOD  flags()                             INLINE  Qt_QTreeWidgetItem_flags( ::pPtr )
   METHOD  font( nColumn )                     INLINE  Qt_QTreeWidgetItem_font( ::pPtr, nColumn )
   METHOD  foreground( nColumn )               INLINE  Qt_QTreeWidgetItem_foreground( ::pPtr, nColumn )
   METHOD  icon( nColumn )                     INLINE  Qt_QTreeWidgetItem_icon( ::pPtr, nColumn )
   METHOD  indexOfChild( pChild )              INLINE  Qt_QTreeWidgetItem_indexOfChild( ::pPtr, pChild )
   METHOD  insertChild( nIndex, pChild )       INLINE  Qt_QTreeWidgetItem_insertChild( ::pPtr, nIndex, pChild )
   METHOD  isDisabled()                        INLINE  Qt_QTreeWidgetItem_isDisabled( ::pPtr )
   METHOD  isExpanded()                        INLINE  Qt_QTreeWidgetItem_isExpanded( ::pPtr )
   METHOD  isFirstColumnSpanned()              INLINE  Qt_QTreeWidgetItem_isFirstColumnSpanned( ::pPtr )
   METHOD  isHidden()                          INLINE  Qt_QTreeWidgetItem_isHidden( ::pPtr )
   METHOD  isSelected()                        INLINE  Qt_QTreeWidgetItem_isSelected( ::pPtr )
   METHOD  parent()                            INLINE  Qt_QTreeWidgetItem_parent( ::pPtr )
   METHOD  read( pIn )                         INLINE  Qt_QTreeWidgetItem_read( ::pPtr, pIn )
   METHOD  removeChild( pChild )               INLINE  Qt_QTreeWidgetItem_removeChild( ::pPtr, pChild )
   METHOD  setBackground( nColumn, pBrush )    INLINE  Qt_QTreeWidgetItem_setBackground( ::pPtr, nColumn, pBrush )
   METHOD  setCheckState( nColumn, nState )    INLINE  Qt_QTreeWidgetItem_setCheckState( ::pPtr, nColumn, nState )
   METHOD  setChildIndicatorPolicy( nPolicy )  INLINE  Qt_QTreeWidgetItem_setChildIndicatorPolicy( ::pPtr, nPolicy )
   METHOD  setData( nColumn, nRole, pValue )   INLINE  Qt_QTreeWidgetItem_setData( ::pPtr, nColumn, nRole, pValue )
   METHOD  setDisabled( lDisabled )            INLINE  Qt_QTreeWidgetItem_setDisabled( ::pPtr, lDisabled )
   METHOD  setExpanded( lExpand )              INLINE  Qt_QTreeWidgetItem_setExpanded( ::pPtr, lExpand )
   METHOD  setFirstColumnSpanned( lSpan )      INLINE  Qt_QTreeWidgetItem_setFirstColumnSpanned( ::pPtr, lSpan )
   METHOD  setFlags( nFlags )                  INLINE  Qt_QTreeWidgetItem_setFlags( ::pPtr, nFlags )
   METHOD  setFont( nColumn, pFont )           INLINE  Qt_QTreeWidgetItem_setFont( ::pPtr, nColumn, pFont )
   METHOD  setForeground( nColumn, pBrush )    INLINE  Qt_QTreeWidgetItem_setForeground( ::pPtr, nColumn, pBrush )
   METHOD  setHidden( lHide )                  INLINE  Qt_QTreeWidgetItem_setHidden( ::pPtr, lHide )
   METHOD  setIcon( nColumn, cIcon )           INLINE  Qt_QTreeWidgetItem_setIcon( ::pPtr, nColumn, cIcon )
   METHOD  setSelected( lSelect )              INLINE  Qt_QTreeWidgetItem_setSelected( ::pPtr, lSelect )
   METHOD  setSizeHint( nColumn, pSize )       INLINE  Qt_QTreeWidgetItem_setSizeHint( ::pPtr, nColumn, pSize )
   METHOD  setStatusTip( nColumn, cStatusTip )  INLINE  Qt_QTreeWidgetItem_setStatusTip( ::pPtr, nColumn, cStatusTip )
   METHOD  setText( nColumn, cText )           INLINE  Qt_QTreeWidgetItem_setText( ::pPtr, nColumn, cText )
   METHOD  setTextAlignment( nColumn, nAlignment )  INLINE  Qt_QTreeWidgetItem_setTextAlignment( ::pPtr, nColumn, nAlignment )
   METHOD  setToolTip( nColumn, cToolTip )     INLINE  Qt_QTreeWidgetItem_setToolTip( ::pPtr, nColumn, cToolTip )
   METHOD  setWhatsThis( nColumn, cWhatsThis )  INLINE  Qt_QTreeWidgetItem_setWhatsThis( ::pPtr, nColumn, cWhatsThis )
   METHOD  sizeHint( nColumn )                 INLINE  Qt_QTreeWidgetItem_sizeHint( ::pPtr, nColumn )
   METHOD  sortChildren( nColumn, nOrder )     INLINE  Qt_QTreeWidgetItem_sortChildren( ::pPtr, nColumn, nOrder )
   METHOD  statusTip( nColumn )                INLINE  Qt_QTreeWidgetItem_statusTip( ::pPtr, nColumn )
   METHOD  takeChild( nIndex )                 INLINE  Qt_QTreeWidgetItem_takeChild( ::pPtr, nIndex )
   METHOD  text( nColumn )                     INLINE  Qt_QTreeWidgetItem_text( ::pPtr, nColumn )
   METHOD  textAlignment( nColumn )            INLINE  Qt_QTreeWidgetItem_textAlignment( ::pPtr, nColumn )
   METHOD  toolTip( nColumn )                  INLINE  Qt_QTreeWidgetItem_toolTip( ::pPtr, nColumn )
   METHOD  treeWidget()                        INLINE  Qt_QTreeWidgetItem_treeWidget( ::pPtr )
   METHOD  type()                              INLINE  Qt_QTreeWidgetItem_type( ::pPtr )
   METHOD  whatsThis( nColumn )                INLINE  Qt_QTreeWidgetItem_whatsThis( ::pPtr, nColumn )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTreeWidgetItem

   ::pParent := pParent

   ::pPtr := Qt_QTreeWidgetItem( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTreeWidgetItem

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

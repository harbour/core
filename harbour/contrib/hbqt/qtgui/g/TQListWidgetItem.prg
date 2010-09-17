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


FUNCTION QListWidgetItem( ... )
   RETURN HB_QListWidgetItem():new( ... )


CREATE CLASS QListWidgetItem INHERIT HbQtObjectHandler, HB_QWidget FUNCTION HB_QListWidgetItem

   METHOD  new( ... )

   METHOD  background()
   METHOD  checkState()
   METHOD  clone()
   METHOD  data( nRole )
   METHOD  flags()
   METHOD  font()
   METHOD  foreground()
   METHOD  icon()
   METHOD  isHidden()
   METHOD  isSelected()
   METHOD  listWidget()
   METHOD  read( pIn )
   METHOD  setBackground( pBrush )
   METHOD  setCheckState( nState )
   METHOD  setData( nRole, pValue )
   METHOD  setFlags( nFlags )
   METHOD  setFont( pFont )
   METHOD  setForeground( pBrush )
   METHOD  setHidden( lHide )
   METHOD  setIcon( pIcon )
   METHOD  setSelected( lSelect )
   METHOD  setSizeHint( pSize )
   METHOD  setStatusTip( cStatusTip )
   METHOD  setText( cText )
   METHOD  setTextAlignment( nAlignment )
   METHOD  setToolTip( cToolTip )
   METHOD  setWhatsThis( cWhatsThis )
   METHOD  sizeHint()
   METHOD  statusTip()
   METHOD  text()
   METHOD  textAlignment()
   METHOD  toolTip()
   METHOD  type()
   METHOD  whatsThis()
   METHOD  write( pOut )

   ENDCLASS


METHOD QListWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QListWidgetItem( ... )
   RETURN Self


METHOD QListWidgetItem:background()
   RETURN Qt_QListWidgetItem_background( ::pPtr )


METHOD QListWidgetItem:checkState()
   RETURN Qt_QListWidgetItem_checkState( ::pPtr )


METHOD QListWidgetItem:clone()
   RETURN Qt_QListWidgetItem_clone( ::pPtr )


METHOD QListWidgetItem:data( nRole )
   RETURN Qt_QListWidgetItem_data( ::pPtr, nRole )


METHOD QListWidgetItem:flags()
   RETURN Qt_QListWidgetItem_flags( ::pPtr )


METHOD QListWidgetItem:font()
   RETURN Qt_QListWidgetItem_font( ::pPtr )


METHOD QListWidgetItem:foreground()
   RETURN Qt_QListWidgetItem_foreground( ::pPtr )


METHOD QListWidgetItem:icon()
   RETURN Qt_QListWidgetItem_icon( ::pPtr )


METHOD QListWidgetItem:isHidden()
   RETURN Qt_QListWidgetItem_isHidden( ::pPtr )


METHOD QListWidgetItem:isSelected()
   RETURN Qt_QListWidgetItem_isSelected( ::pPtr )


METHOD QListWidgetItem:listWidget()
   RETURN Qt_QListWidgetItem_listWidget( ::pPtr )


METHOD QListWidgetItem:read( pIn )
   RETURN Qt_QListWidgetItem_read( ::pPtr, hbqt_ptr( pIn ) )


METHOD QListWidgetItem:setBackground( pBrush )
   RETURN Qt_QListWidgetItem_setBackground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QListWidgetItem:setCheckState( nState )
   RETURN Qt_QListWidgetItem_setCheckState( ::pPtr, nState )


METHOD QListWidgetItem:setData( nRole, pValue )
   RETURN Qt_QListWidgetItem_setData( ::pPtr, nRole, hbqt_ptr( pValue ) )


METHOD QListWidgetItem:setFlags( nFlags )
   RETURN Qt_QListWidgetItem_setFlags( ::pPtr, nFlags )


METHOD QListWidgetItem:setFont( pFont )
   RETURN Qt_QListWidgetItem_setFont( ::pPtr, hbqt_ptr( pFont ) )


METHOD QListWidgetItem:setForeground( pBrush )
   RETURN Qt_QListWidgetItem_setForeground( ::pPtr, hbqt_ptr( pBrush ) )


METHOD QListWidgetItem:setHidden( lHide )
   RETURN Qt_QListWidgetItem_setHidden( ::pPtr, lHide )


METHOD QListWidgetItem:setIcon( pIcon )
   RETURN Qt_QListWidgetItem_setIcon( ::pPtr, hbqt_ptr( pIcon ) )


METHOD QListWidgetItem:setSelected( lSelect )
   RETURN Qt_QListWidgetItem_setSelected( ::pPtr, lSelect )


METHOD QListWidgetItem:setSizeHint( pSize )
   RETURN Qt_QListWidgetItem_setSizeHint( ::pPtr, hbqt_ptr( pSize ) )


METHOD QListWidgetItem:setStatusTip( cStatusTip )
   RETURN Qt_QListWidgetItem_setStatusTip( ::pPtr, cStatusTip )


METHOD QListWidgetItem:setText( cText )
   RETURN Qt_QListWidgetItem_setText( ::pPtr, cText )


METHOD QListWidgetItem:setTextAlignment( nAlignment )
   RETURN Qt_QListWidgetItem_setTextAlignment( ::pPtr, nAlignment )


METHOD QListWidgetItem:setToolTip( cToolTip )
   RETURN Qt_QListWidgetItem_setToolTip( ::pPtr, cToolTip )


METHOD QListWidgetItem:setWhatsThis( cWhatsThis )
   RETURN Qt_QListWidgetItem_setWhatsThis( ::pPtr, cWhatsThis )


METHOD QListWidgetItem:sizeHint()
   RETURN Qt_QListWidgetItem_sizeHint( ::pPtr )


METHOD QListWidgetItem:statusTip()
   RETURN Qt_QListWidgetItem_statusTip( ::pPtr )


METHOD QListWidgetItem:text()
   RETURN Qt_QListWidgetItem_text( ::pPtr )


METHOD QListWidgetItem:textAlignment()
   RETURN Qt_QListWidgetItem_textAlignment( ::pPtr )


METHOD QListWidgetItem:toolTip()
   RETURN Qt_QListWidgetItem_toolTip( ::pPtr )


METHOD QListWidgetItem:type()
   RETURN Qt_QListWidgetItem_type( ::pPtr )


METHOD QListWidgetItem:whatsThis()
   RETURN Qt_QListWidgetItem_whatsThis( ::pPtr )


METHOD QListWidgetItem:write( pOut )
   RETURN Qt_QListWidgetItem_write( ::pPtr, hbqt_ptr( pOut ) )


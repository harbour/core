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


CREATE CLASS QListWidgetItem INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QListWidgetItem_destroy( ::pPtr )

   METHOD  background()                        INLINE  Qt_QListWidgetItem_background( ::pPtr )
   METHOD  checkState()                        INLINE  Qt_QListWidgetItem_checkState( ::pPtr )
   METHOD  clone()                             INLINE  Qt_QListWidgetItem_clone( ::pPtr )
   METHOD  data( nRole )                       INLINE  Qt_QListWidgetItem_data( ::pPtr, nRole )
   METHOD  flags()                             INLINE  Qt_QListWidgetItem_flags( ::pPtr )
   METHOD  font()                              INLINE  Qt_QListWidgetItem_font( ::pPtr )
   METHOD  foreground()                        INLINE  Qt_QListWidgetItem_foreground( ::pPtr )
   METHOD  icon()                              INLINE  Qt_QListWidgetItem_icon( ::pPtr )
   METHOD  isHidden()                          INLINE  Qt_QListWidgetItem_isHidden( ::pPtr )
   METHOD  isSelected()                        INLINE  Qt_QListWidgetItem_isSelected( ::pPtr )
   METHOD  listWidget()                        INLINE  Qt_QListWidgetItem_listWidget( ::pPtr )
   METHOD  read( pIn )                         INLINE  Qt_QListWidgetItem_read( ::pPtr, pIn )
   METHOD  setBackground( pBrush )             INLINE  Qt_QListWidgetItem_setBackground( ::pPtr, pBrush )
   METHOD  setCheckState( nState )             INLINE  Qt_QListWidgetItem_setCheckState( ::pPtr, nState )
   METHOD  setData( nRole, pValue )            INLINE  Qt_QListWidgetItem_setData( ::pPtr, nRole, pValue )
   METHOD  setFlags( nFlags )                  INLINE  Qt_QListWidgetItem_setFlags( ::pPtr, nFlags )
   METHOD  setFont( pFont )                    INLINE  Qt_QListWidgetItem_setFont( ::pPtr, pFont )
   METHOD  setForeground( pBrush )             INLINE  Qt_QListWidgetItem_setForeground( ::pPtr, pBrush )
   METHOD  setHidden( lHide )                  INLINE  Qt_QListWidgetItem_setHidden( ::pPtr, lHide )
   METHOD  setIcon( cIcon )                    INLINE  Qt_QListWidgetItem_setIcon( ::pPtr, cIcon )
   METHOD  setSelected( lSelect )              INLINE  Qt_QListWidgetItem_setSelected( ::pPtr, lSelect )
   METHOD  setSizeHint( pSize )                INLINE  Qt_QListWidgetItem_setSizeHint( ::pPtr, pSize )
   METHOD  setStatusTip( cStatusTip )          INLINE  Qt_QListWidgetItem_setStatusTip( ::pPtr, cStatusTip )
   METHOD  setText( cText )                    INLINE  Qt_QListWidgetItem_setText( ::pPtr, cText )
   METHOD  setTextAlignment( nAlignment )      INLINE  Qt_QListWidgetItem_setTextAlignment( ::pPtr, nAlignment )
   METHOD  setToolTip( cToolTip )              INLINE  Qt_QListWidgetItem_setToolTip( ::pPtr, cToolTip )
   METHOD  setWhatsThis( cWhatsThis )          INLINE  Qt_QListWidgetItem_setWhatsThis( ::pPtr, cWhatsThis )
   METHOD  sizeHint()                          INLINE  Qt_QListWidgetItem_sizeHint( ::pPtr )
   METHOD  statusTip()                         INLINE  Qt_QListWidgetItem_statusTip( ::pPtr )
   METHOD  text()                              INLINE  Qt_QListWidgetItem_text( ::pPtr )
   METHOD  textAlignment()                     INLINE  Qt_QListWidgetItem_textAlignment( ::pPtr )
   METHOD  toolTip()                           INLINE  Qt_QListWidgetItem_toolTip( ::pPtr )
   METHOD  type()                              INLINE  Qt_QListWidgetItem_type( ::pPtr )
   METHOD  whatsThis()                         INLINE  Qt_QListWidgetItem_whatsThis( ::pPtr )
   METHOD  write( pOut )                       INLINE  Qt_QListWidgetItem_write( ::pPtr, pOut )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QListWidgetItem

   ::pParent := pParent

   ::pPtr := Qt_QListWidgetItem( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QListWidgetItem

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/


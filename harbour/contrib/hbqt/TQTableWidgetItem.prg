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


CREATE CLASS QTableWidgetItem

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QTableWidgetItem_destroy( ::pPtr )

   METHOD  background()                        INLINE  Qt_QTableWidgetItem_background( ::pPtr )
   METHOD  checkState()                        INLINE  Qt_QTableWidgetItem_checkState( ::pPtr )
   METHOD  clone()                             INLINE  Qt_QTableWidgetItem_clone( ::pPtr )
   METHOD  column()                            INLINE  Qt_QTableWidgetItem_column( ::pPtr )
   METHOD  data( nRole )                       INLINE  Qt_QTableWidgetItem_data( ::pPtr, nRole )
   METHOD  flags()                             INLINE  Qt_QTableWidgetItem_flags( ::pPtr )
   METHOD  font()                              INLINE  Qt_QTableWidgetItem_font( ::pPtr )
   METHOD  foreground()                        INLINE  Qt_QTableWidgetItem_foreground( ::pPtr )
   METHOD  icon()                              INLINE  Qt_QTableWidgetItem_icon( ::pPtr )
   METHOD  isSelected()                        INLINE  Qt_QTableWidgetItem_isSelected( ::pPtr )
   METHOD  read( pIn )                         INLINE  Qt_QTableWidgetItem_read( ::pPtr, pIn )
   METHOD  row()                               INLINE  Qt_QTableWidgetItem_row( ::pPtr )
   METHOD  setBackground( pBrush )             INLINE  Qt_QTableWidgetItem_setBackground( ::pPtr, pBrush )
   METHOD  setCheckState( nState )             INLINE  Qt_QTableWidgetItem_setCheckState( ::pPtr, nState )
   METHOD  setData( nRole, pValue )            INLINE  Qt_QTableWidgetItem_setData( ::pPtr, nRole, pValue )
   METHOD  setFlags( nFlags )                  INLINE  Qt_QTableWidgetItem_setFlags( ::pPtr, nFlags )
   METHOD  setFont( pFont )                    INLINE  Qt_QTableWidgetItem_setFont( ::pPtr, pFont )
   METHOD  setForeground( pBrush )             INLINE  Qt_QTableWidgetItem_setForeground( ::pPtr, pBrush )
   METHOD  setIcon( cIcon )                    INLINE  Qt_QTableWidgetItem_setIcon( ::pPtr, cIcon )
   METHOD  setSelected( lSelect )              INLINE  Qt_QTableWidgetItem_setSelected( ::pPtr, lSelect )
   METHOD  setSizeHint( pSize )                INLINE  Qt_QTableWidgetItem_setSizeHint( ::pPtr, pSize )
   METHOD  setStatusTip( cStatusTip )          INLINE  Qt_QTableWidgetItem_setStatusTip( ::pPtr, cStatusTip )
   METHOD  setText( cText )                    INLINE  Qt_QTableWidgetItem_setText( ::pPtr, cText )
   METHOD  setTextAlignment( nAlignment )      INLINE  Qt_QTableWidgetItem_setTextAlignment( ::pPtr, nAlignment )
   METHOD  setToolTip( cToolTip )              INLINE  Qt_QTableWidgetItem_setToolTip( ::pPtr, cToolTip )
   METHOD  setWhatsThis( cWhatsThis )          INLINE  Qt_QTableWidgetItem_setWhatsThis( ::pPtr, cWhatsThis )
   METHOD  sizeHint()                          INLINE  Qt_QTableWidgetItem_sizeHint( ::pPtr )
   METHOD  statusTip()                         INLINE  Qt_QTableWidgetItem_statusTip( ::pPtr )
   METHOD  tableWidget()                       INLINE  Qt_QTableWidgetItem_tableWidget( ::pPtr )
   METHOD  text()                              INLINE  Qt_QTableWidgetItem_text( ::pPtr )
   METHOD  textAlignment()                     INLINE  Qt_QTableWidgetItem_textAlignment( ::pPtr )
   METHOD  toolTip()                           INLINE  Qt_QTableWidgetItem_toolTip( ::pPtr )
   METHOD  type()                              INLINE  Qt_QTableWidgetItem_type( ::pPtr )
   METHOD  whatsThis()                         INLINE  Qt_QTableWidgetItem_whatsThis( ::pPtr )
   METHOD  write( pOut )                       INLINE  Qt_QTableWidgetItem_write( ::pPtr, pOut )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QTableWidgetItem

   ::pParent := pParent

   ::pPtr := Qt_QTableWidgetItem( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QTableWidgetItem

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/


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


CREATE CLASS QToolBox INHERIT QFrame

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QToolBox_destroy( ::pPtr )

   METHOD  addItem( pWidget, cIconSet, cText )  INLINE  Qt_QToolBox_addItem( ::pPtr, pWidget, cIconSet, cText )
   METHOD  addItem_1( pW, cText )              INLINE  Qt_QToolBox_addItem_1( ::pPtr, pW, cText )
   METHOD  count()                             INLINE  Qt_QToolBox_count( ::pPtr )
   METHOD  currentIndex()                      INLINE  Qt_QToolBox_currentIndex( ::pPtr )
   METHOD  currentWidget()                     INLINE  Qt_QToolBox_currentWidget( ::pPtr )
   METHOD  indexOf( pWidget )                  INLINE  Qt_QToolBox_indexOf( ::pPtr, pWidget )
   METHOD  insertItem( nIndex, pWidget, cIcon, cText )  INLINE  Qt_QToolBox_insertItem( ::pPtr, nIndex, pWidget, cIcon, cText )
   METHOD  insertItem_1( nIndex, pWidget, cText )  INLINE  Qt_QToolBox_insertItem_1( ::pPtr, nIndex, pWidget, cText )
   METHOD  isItemEnabled( nIndex )             INLINE  Qt_QToolBox_isItemEnabled( ::pPtr, nIndex )
   METHOD  itemIcon( nIndex )                  INLINE  Qt_QToolBox_itemIcon( ::pPtr, nIndex )
   METHOD  itemText( nIndex )                  INLINE  Qt_QToolBox_itemText( ::pPtr, nIndex )
   METHOD  itemToolTip( nIndex )               INLINE  Qt_QToolBox_itemToolTip( ::pPtr, nIndex )
   METHOD  removeItem( nIndex )                INLINE  Qt_QToolBox_removeItem( ::pPtr, nIndex )
   METHOD  setItemEnabled( nIndex, lEnabled )  INLINE  Qt_QToolBox_setItemEnabled( ::pPtr, nIndex, lEnabled )
   METHOD  setItemIcon( nIndex, cIcon )        INLINE  Qt_QToolBox_setItemIcon( ::pPtr, nIndex, cIcon )
   METHOD  setItemText( nIndex, cText )        INLINE  Qt_QToolBox_setItemText( ::pPtr, nIndex, cText )
   METHOD  setItemToolTip( nIndex, cToolTip )  INLINE  Qt_QToolBox_setItemToolTip( ::pPtr, nIndex, cToolTip )
   METHOD  widget( nIndex )                    INLINE  Qt_QToolBox_widget( ::pPtr, nIndex )
   METHOD  setCurrentIndex( nIndex )           INLINE  Qt_QToolBox_setCurrentIndex( ::pPtr, nIndex )
   METHOD  setCurrentWidget( pWidget )         INLINE  Qt_QToolBox_setCurrentWidget( ::pPtr, pWidget )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QToolBox

   ::pParent := pParent

   ::pPtr := Qt_QToolBox( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QToolBox

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

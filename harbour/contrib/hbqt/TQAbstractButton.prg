/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include 'hbclass.ch'


CLASS QAbstractButton INHERIT QWidget

   DATA    pPtr

   METHOD  New()

   METHOD  autoExclusive()                     INLINE  Qt_QAbstractButton_autoExclusive( ::pPtr )
   METHOD  autoRepeat()                        INLINE  Qt_QAbstractButton_autoRepeat( ::pPtr )
   METHOD  autoRepeatDelay()                   INLINE  Qt_QAbstractButton_autoRepeatDelay( ::pPtr )
   METHOD  autoRepeatInterval()                INLINE  Qt_QAbstractButton_autoRepeatInterval( ::pPtr )
   METHOD  group()                             INLINE  Qt_QAbstractButton_group( ::pPtr )
   METHOD  iconSize()                          INLINE  Qt_QAbstractButton_iconSize( ::pPtr )
   METHOD  isCheckable()                       INLINE  Qt_QAbstractButton_isCheckable( ::pPtr )
   METHOD  isChecked()                         INLINE  Qt_QAbstractButton_isChecked( ::pPtr )
   METHOD  isDown()                            INLINE  Qt_QAbstractButton_isDown( ::pPtr )
   METHOD  setAutoExclusive( lBool )           INLINE  Qt_QAbstractButton_setAutoExclusive( ::pPtr, lBool )
   METHOD  setAutoRepeat( lBool )              INLINE  Qt_QAbstractButton_setAutoRepeat( ::pPtr, lBool )
   METHOD  setAutoRepeatDelay( nInt )          INLINE  Qt_QAbstractButton_setAutoRepeatDelay( ::pPtr, nInt )
   METHOD  setAutoRepeatInterval( nInt )       INLINE  Qt_QAbstractButton_setAutoRepeatInterval( ::pPtr, nInt )
   METHOD  setCheckable( lBool )               INLINE  Qt_QAbstractButton_setCheckable( ::pPtr, lBool )
   METHOD  setDown( lBool )                    INLINE  Qt_QAbstractButton_setDown( ::pPtr, lBool )
   METHOD  setIcon( cIcon )                    INLINE  Qt_QAbstractButton_setIcon( ::pPtr, cIcon )
   METHOD  setText( cText )                    INLINE  Qt_QAbstractButton_setText( ::pPtr, cText )
   METHOD  text()                              INLINE  Qt_QAbstractButton_text( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QAbstractButton

   ::pPtr := Qt_QAbstractButton( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


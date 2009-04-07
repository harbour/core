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


#include "hbclass.ch"


CREATE CLASS QProgressDialog INHERIT QDialog

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  autoClose()                         INLINE  Qt_QProgressDialog_autoClose( ::pPtr )
   METHOD  autoReset()                         INLINE  Qt_QProgressDialog_autoReset( ::pPtr )
   METHOD  labelText()                         INLINE  Qt_QProgressDialog_labelText( ::pPtr )
   METHOD  maximum()                           INLINE  Qt_QProgressDialog_maximum( ::pPtr )
   METHOD  minimum()                           INLINE  Qt_QProgressDialog_minimum( ::pPtr )
   METHOD  minimumDuration()                   INLINE  Qt_QProgressDialog_minimumDuration( ::pPtr )
   METHOD  open( pReceiver, pMember )          INLINE  Qt_QProgressDialog_open( ::pPtr, pReceiver, pMember )
   METHOD  setAutoClose( lClose )              INLINE  Qt_QProgressDialog_setAutoClose( ::pPtr, lClose )
   METHOD  setAutoReset( lReset )              INLINE  Qt_QProgressDialog_setAutoReset( ::pPtr, lReset )
   METHOD  setBar( pBar )                      INLINE  Qt_QProgressDialog_setBar( ::pPtr, pBar )
   METHOD  setCancelButton( pCancelButton )    INLINE  Qt_QProgressDialog_setCancelButton( ::pPtr, pCancelButton )
   METHOD  setLabel( pLabel )                  INLINE  Qt_QProgressDialog_setLabel( ::pPtr, pLabel )
   METHOD  sizeHint()                          INLINE  Qt_QProgressDialog_sizeHint( ::pPtr )
   METHOD  value()                             INLINE  Qt_QProgressDialog_value( ::pPtr )
   METHOD  wasCanceled()                       INLINE  Qt_QProgressDialog_wasCanceled( ::pPtr )
   METHOD  cancel()                            INLINE  Qt_QProgressDialog_cancel( ::pPtr )
   METHOD  reset()                             INLINE  Qt_QProgressDialog_reset( ::pPtr )
   METHOD  setCancelButtonText( cCancelButtonText )  INLINE  Qt_QProgressDialog_setCancelButtonText( ::pPtr, cCancelButtonText )
   METHOD  setLabelText( cText )               INLINE  Qt_QProgressDialog_setLabelText( ::pPtr, cText )
   METHOD  setMaximum( nMaximum )              INLINE  Qt_QProgressDialog_setMaximum( ::pPtr, nMaximum )
   METHOD  setMinimum( nMinimum )              INLINE  Qt_QProgressDialog_setMinimum( ::pPtr, nMinimum )
   METHOD  setMinimumDuration( nMs )           INLINE  Qt_QProgressDialog_setMinimumDuration( ::pPtr, nMs )
   METHOD  setRange( nMinimum, nMaximum )      INLINE  Qt_QProgressDialog_setRange( ::pPtr, nMinimum, nMaximum )
   METHOD  setValue( nProgress )               INLINE  Qt_QProgressDialog_setValue( ::pPtr, nProgress )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QProgressDialog

   ::pParent := pParent

   ::pPtr := Qt_QProgressDialog( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


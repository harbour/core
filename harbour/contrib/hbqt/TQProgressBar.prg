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


CLASS QProgressBar INHERIT QWidget

   DATA    pPtr

   METHOD  New()

   METHOD  alignment()                         INLINE  Qt_QProgressBar_alignment( ::pPtr )
   METHOD  format()                            INLINE  Qt_QProgressBar_format( ::pPtr )
   METHOD  invertedAppearance()                INLINE  Qt_QProgressBar_invertedAppearance( ::pPtr )
   METHOD  isTextVisible()                     INLINE  Qt_QProgressBar_isTextVisible( ::pPtr )
   METHOD  maximum()                           INLINE  Qt_QProgressBar_maximum( ::pPtr )
   METHOD  minimum()                           INLINE  Qt_QProgressBar_minimum( ::pPtr )
   METHOD  orientation()                       INLINE  Qt_QProgressBar_orientation( ::pPtr )
   METHOD  setAlignment( nAlignment )          INLINE  Qt_QProgressBar_setAlignment( ::pPtr, nAlignment )
   METHOD  setFormat( cFormat )                INLINE  Qt_QProgressBar_setFormat( ::pPtr, cFormat )
   METHOD  setInvertedAppearance( lInvert )    INLINE  Qt_QProgressBar_setInvertedAppearance( ::pPtr, lInvert )
   METHOD  setTextDirection( nTextDirection )  INLINE  Qt_QProgressBar_setTextDirection( ::pPtr, nTextDirection )
   METHOD  setTextVisible( lVisible )          INLINE  Qt_QProgressBar_setTextVisible( ::pPtr, lVisible )
   METHOD  text()                              INLINE  Qt_QProgressBar_text( ::pPtr )
   METHOD  textDirection()                     INLINE  Qt_QProgressBar_textDirection( ::pPtr )
   METHOD  value()                             INLINE  Qt_QProgressBar_value( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QProgressBar

   ::pPtr := Qt_QProgressBar( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


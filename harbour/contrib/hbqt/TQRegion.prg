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


CREATE CLASS QRegion

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  boundingRect()                      INLINE  Qt_QRegion_boundingRect( ::pPtr )
   METHOD  contains( pP )                      INLINE  Qt_QRegion_contains( ::pPtr, pP )
   METHOD  contains_1( pR )                    INLINE  Qt_QRegion_contains_1( ::pPtr, pR )
   METHOD  intersected( pR )                   INLINE  Qt_QRegion_intersected( ::pPtr, pR )
   METHOD  intersected_1( pRect )              INLINE  Qt_QRegion_intersected_1( ::pPtr, pRect )
   METHOD  intersects( pRegion )               INLINE  Qt_QRegion_intersects( ::pPtr, pRegion )
   METHOD  intersects_1( pRect )               INLINE  Qt_QRegion_intersects_1( ::pPtr, pRect )
   METHOD  isEmpty()                           INLINE  Qt_QRegion_isEmpty( ::pPtr )
   METHOD  numRects()                          INLINE  Qt_QRegion_numRects( ::pPtr )
   METHOD  setRects( pRects, nNumber )         INLINE  Qt_QRegion_setRects( ::pPtr, pRects, nNumber )
   METHOD  subtracted( pR )                    INLINE  Qt_QRegion_subtracted( ::pPtr, pR )
   METHOD  translate( nDx, nDy )               INLINE  Qt_QRegion_translate( ::pPtr, nDx, nDy )
   METHOD  translate_1( pPoint )               INLINE  Qt_QRegion_translate_1( ::pPtr, pPoint )
   METHOD  translated( nDx, nDy )              INLINE  Qt_QRegion_translated( ::pPtr, nDx, nDy )
   METHOD  translated_1( pP )                  INLINE  Qt_QRegion_translated_1( ::pPtr, pP )
   METHOD  united( pR )                        INLINE  Qt_QRegion_united( ::pPtr, pR )
   METHOD  united_1( pRect )                   INLINE  Qt_QRegion_united_1( ::pPtr, pRect )
   METHOD  xored( pR )                         INLINE  Qt_QRegion_xored( ::pPtr, pR )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QRegion

   ::pParent := pParent

   ::pPtr := Qt_QRegion( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/


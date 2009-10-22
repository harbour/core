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


CREATE CLASS QPolygon

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  boundingRect()                      INLINE  Qt_QPolygon_boundingRect( ::pPtr )
   METHOD  containsPoint( pPoint, nFillRule )  INLINE  Qt_QPolygon_containsPoint( ::pPtr, pPoint, nFillRule )
   METHOD  intersected( pR )                   INLINE  Qt_QPolygon_intersected( ::pPtr, pR )
   METHOD  point( nIndex, nX, nY )             INLINE  Qt_QPolygon_point( ::pPtr, nIndex, nX, nY )
   METHOD  point_1( nIndex )                   INLINE  Qt_QPolygon_point_1( ::pPtr, nIndex )
   METHOD  putPoints( nIndex, nNPoints, pFromPolygon, nFromIndex )  INLINE  Qt_QPolygon_putPoints( ::pPtr, nIndex, nNPoints, pFromPolygon, nFromIndex )
   METHOD  setPoint( nIndex, nX, nY )          INLINE  Qt_QPolygon_setPoint( ::pPtr, nIndex, nX, nY )
   METHOD  setPoint_1( nIndex, pPoint )        INLINE  Qt_QPolygon_setPoint_1( ::pPtr, nIndex, pPoint )
   METHOD  setPoints( nNPoints, nPoints )      INLINE  Qt_QPolygon_setPoints( ::pPtr, nNPoints, nPoints )
   METHOD  subtracted( pR )                    INLINE  Qt_QPolygon_subtracted( ::pPtr, pR )
   METHOD  translate( nDx, nDy )               INLINE  Qt_QPolygon_translate( ::pPtr, nDx, nDy )
   METHOD  translate_1( pOffset )              INLINE  Qt_QPolygon_translate_1( ::pPtr, pOffset )
   METHOD  united( pR )                        INLINE  Qt_QPolygon_united( ::pPtr, pR )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QPolygon

   ::pParent := pParent

   ::pPtr := Qt_QPolygon( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QPolygon

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

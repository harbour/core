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


CREATE CLASS QRect

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QRect_destroy( ::pPtr )

   METHOD  adjust( nDx1, nDy1, nDx2, nDy2 )    INLINE  Qt_QRect_adjust( ::pPtr, nDx1, nDy1, nDx2, nDy2 )
   METHOD  adjusted( nDx1, nDy1, nDx2, nDy2 )  INLINE  Qt_QRect_adjusted( ::pPtr, nDx1, nDy1, nDx2, nDy2 )
   METHOD  bottom()                            INLINE  Qt_QRect_bottom( ::pPtr )
   METHOD  bottomLeft()                        INLINE  Qt_QRect_bottomLeft( ::pPtr )
   METHOD  bottomRight()                       INLINE  Qt_QRect_bottomRight( ::pPtr )
   METHOD  center()                            INLINE  Qt_QRect_center( ::pPtr )
   METHOD  contains( pPoint, lProper )         INLINE  Qt_QRect_contains( ::pPtr, pPoint, lProper )
   METHOD  contains_1( nX, nY, lProper )       INLINE  Qt_QRect_contains_1( ::pPtr, nX, nY, lProper )
   METHOD  contains_2( nX, nY )                INLINE  Qt_QRect_contains_2( ::pPtr, nX, nY )
   METHOD  contains_3( pRectangle, lProper )   INLINE  Qt_QRect_contains_3( ::pPtr, pRectangle, lProper )
   METHOD  getCoords( nX1, nY1, nX2, nY2 )     INLINE  Qt_QRect_getCoords( ::pPtr, nX1, nY1, nX2, nY2 )
   METHOD  getRect( nX, nY, nWidth, nHeight )  INLINE  Qt_QRect_getRect( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  height()                            INLINE  Qt_QRect_height( ::pPtr )
   METHOD  intersected( pRectangle )           INLINE  Qt_QRect_intersected( ::pPtr, pRectangle )
   METHOD  intersects( pRectangle )            INLINE  Qt_QRect_intersects( ::pPtr, pRectangle )
   METHOD  isEmpty()                           INLINE  Qt_QRect_isEmpty( ::pPtr )
   METHOD  isNull()                            INLINE  Qt_QRect_isNull( ::pPtr )
   METHOD  isValid()                           INLINE  Qt_QRect_isValid( ::pPtr )
   METHOD  left()                              INLINE  Qt_QRect_left( ::pPtr )
   METHOD  moveBottom( nY )                    INLINE  Qt_QRect_moveBottom( ::pPtr, nY )
   METHOD  moveBottomLeft( pPosition )         INLINE  Qt_QRect_moveBottomLeft( ::pPtr, pPosition )
   METHOD  moveBottomRight( pPosition )        INLINE  Qt_QRect_moveBottomRight( ::pPtr, pPosition )
   METHOD  moveCenter( pPosition )             INLINE  Qt_QRect_moveCenter( ::pPtr, pPosition )
   METHOD  moveLeft( nX )                      INLINE  Qt_QRect_moveLeft( ::pPtr, nX )
   METHOD  moveRight( nX )                     INLINE  Qt_QRect_moveRight( ::pPtr, nX )
   METHOD  moveTo( nX, nY )                    INLINE  Qt_QRect_moveTo( ::pPtr, nX, nY )
   METHOD  moveTo_1( pPosition )               INLINE  Qt_QRect_moveTo_1( ::pPtr, pPosition )
   METHOD  moveTop( nY )                       INLINE  Qt_QRect_moveTop( ::pPtr, nY )
   METHOD  moveTopLeft( pPosition )            INLINE  Qt_QRect_moveTopLeft( ::pPtr, pPosition )
   METHOD  moveTopRight( pPosition )           INLINE  Qt_QRect_moveTopRight( ::pPtr, pPosition )
   METHOD  normalized()                        INLINE  Qt_QRect_normalized( ::pPtr )
   METHOD  right()                             INLINE  Qt_QRect_right( ::pPtr )
   METHOD  setBottom( nY )                     INLINE  Qt_QRect_setBottom( ::pPtr, nY )
   METHOD  setBottomLeft( pPosition )          INLINE  Qt_QRect_setBottomLeft( ::pPtr, pPosition )
   METHOD  setBottomRight( pPosition )         INLINE  Qt_QRect_setBottomRight( ::pPtr, pPosition )
   METHOD  setCoords( nX1, nY1, nX2, nY2 )     INLINE  Qt_QRect_setCoords( ::pPtr, nX1, nY1, nX2, nY2 )
   METHOD  setHeight( nHeight )                INLINE  Qt_QRect_setHeight( ::pPtr, nHeight )
   METHOD  setLeft( nX )                       INLINE  Qt_QRect_setLeft( ::pPtr, nX )
   METHOD  setRect( nX, nY, nWidth, nHeight )  INLINE  Qt_QRect_setRect( ::pPtr, nX, nY, nWidth, nHeight )
   METHOD  setRight( nX )                      INLINE  Qt_QRect_setRight( ::pPtr, nX )
   METHOD  setSize( pSize )                    INLINE  Qt_QRect_setSize( ::pPtr, pSize )
   METHOD  setTop( nY )                        INLINE  Qt_QRect_setTop( ::pPtr, nY )
   METHOD  setTopLeft( pPosition )             INLINE  Qt_QRect_setTopLeft( ::pPtr, pPosition )
   METHOD  setTopRight( pPosition )            INLINE  Qt_QRect_setTopRight( ::pPtr, pPosition )
   METHOD  setWidth( nWidth )                  INLINE  Qt_QRect_setWidth( ::pPtr, nWidth )
   METHOD  setX( nX )                          INLINE  Qt_QRect_setX( ::pPtr, nX )
   METHOD  setY( nY )                          INLINE  Qt_QRect_setY( ::pPtr, nY )
   METHOD  size()                              INLINE  Qt_QRect_size( ::pPtr )
   METHOD  top()                               INLINE  Qt_QRect_top( ::pPtr )
   METHOD  topLeft()                           INLINE  Qt_QRect_topLeft( ::pPtr )
   METHOD  topRight()                          INLINE  Qt_QRect_topRight( ::pPtr )
   METHOD  translate( nDx, nDy )               INLINE  Qt_QRect_translate( ::pPtr, nDx, nDy )
   METHOD  translate_1( pOffset )              INLINE  Qt_QRect_translate_1( ::pPtr, pOffset )
   METHOD  translated( nDx, nDy )              INLINE  Qt_QRect_translated( ::pPtr, nDx, nDy )
   METHOD  translated_1( pOffset )             INLINE  Qt_QRect_translated_1( ::pPtr, pOffset )
   METHOD  united( pRectangle )                INLINE  Qt_QRect_united( ::pPtr, pRectangle )
   METHOD  width()                             INLINE  Qt_QRect_width( ::pPtr )
   METHOD  x()                                 INLINE  Qt_QRect_x( ::pPtr )
   METHOD  y()                                 INLINE  Qt_QRect_y( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QRect

   ::pPtr := Qt_QRect( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QRect

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

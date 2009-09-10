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


CREATE CLASS QPen

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QPen_destroy( ::pPtr )

   METHOD  brush()                             INLINE  Qt_QPen_brush( ::pPtr )
   METHOD  capStyle()                          INLINE  Qt_QPen_capStyle( ::pPtr )
   METHOD  color()                             INLINE  Qt_QPen_color( ::pPtr )
   METHOD  dashOffset()                        INLINE  Qt_QPen_dashOffset( ::pPtr )
   METHOD  isCosmetic()                        INLINE  Qt_QPen_isCosmetic( ::pPtr )
   METHOD  isSolid()                           INLINE  Qt_QPen_isSolid( ::pPtr )
   METHOD  joinStyle()                         INLINE  Qt_QPen_joinStyle( ::pPtr )
   METHOD  miterLimit()                        INLINE  Qt_QPen_miterLimit( ::pPtr )
   METHOD  setBrush( pBrush )                  INLINE  Qt_QPen_setBrush( ::pPtr, pBrush )
   METHOD  setCapStyle( nStyle )               INLINE  Qt_QPen_setCapStyle( ::pPtr, nStyle )
   METHOD  setColor( pColor )                  INLINE  Qt_QPen_setColor( ::pPtr, pColor )
   METHOD  setCosmetic( lCosmetic )            INLINE  Qt_QPen_setCosmetic( ::pPtr, lCosmetic )
   METHOD  setDashOffset( nOffset )            INLINE  Qt_QPen_setDashOffset( ::pPtr, nOffset )
   METHOD  setJoinStyle( nStyle )              INLINE  Qt_QPen_setJoinStyle( ::pPtr, nStyle )
   METHOD  setMiterLimit( nLimit )             INLINE  Qt_QPen_setMiterLimit( ::pPtr, nLimit )
   METHOD  setStyle( nStyle )                  INLINE  Qt_QPen_setStyle( ::pPtr, nStyle )
   METHOD  setWidth( nWidth )                  INLINE  Qt_QPen_setWidth( ::pPtr, nWidth )
   METHOD  setWidthF( nWidth )                 INLINE  Qt_QPen_setWidthF( ::pPtr, nWidth )
   METHOD  style()                             INLINE  Qt_QPen_style( ::pPtr )
   METHOD  width()                             INLINE  Qt_QPen_width( ::pPtr )
   METHOD  widthF()                            INLINE  Qt_QPen_widthF( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QPen

   ::pPtr := Qt_QPen( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QPen

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

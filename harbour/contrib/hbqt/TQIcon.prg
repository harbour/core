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


CREATE CLASS QIcon

   VAR     pParent
   VAR     pPtr

   METHOD  New()

   METHOD  actualSize( pSize, nMode, nState )  INLINE  Qt_QIcon_actualSize( ::pPtr, pSize, nMode, nState )
   METHOD  addFile( cFileName, pSize, nMode, nState )  INLINE  Qt_QIcon_addFile( ::pPtr, cFileName, pSize, nMode, nState )
   METHOD  addPixmap( pPixmap, nMode, nState )  INLINE  Qt_QIcon_addPixmap( ::pPtr, pPixmap, nMode, nState )
   METHOD  cacheKey()                          INLINE  Qt_QIcon_cacheKey( ::pPtr )
   METHOD  isNull()                            INLINE  Qt_QIcon_isNull( ::pPtr )
   METHOD  paint( pPainter, pRect, nAlignment, nMode, nState )  INLINE  Qt_QIcon_paint( ::pPtr, pPainter, pRect, nAlignment, nMode, nState )
   METHOD  paint_1( pPainter, nX, nY, nW, nH, nAlignment, nMode, nState )  INLINE  Qt_QIcon_paint_1( ::pPtr, pPainter, nX, nY, nW, nH, nAlignment, nMode, nState )
   METHOD  pixmap( pSize, nMode, nState )      INLINE  Qt_QIcon_pixmap( ::pPtr, pSize, nMode, nState )
   METHOD  pixmap_1( nW, nH, nMode, nState )   INLINE  Qt_QIcon_pixmap_1( ::pPtr, nW, nH, nMode, nState )
   METHOD  pixmap_2( nExtent, nMode, nState )  INLINE  Qt_QIcon_pixmap_2( ::pPtr, nExtent, nMode, nState )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QIcon
   LOCAL aP, nParams

   aP := hb_aParams()
   nParams := len( aP )

   DO CASE
   CASE nParams == 0
      ::pPtr := Qt_QIcon()

   CASE nParams == 1
      ::pPtr := Qt_QIcon( aP[ 1 ] )

   CASE nParams == 2
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ] )

   CASE nParams == 3
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ] )

   CASE nParams == 4
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ] )

   CASE nParams == 5
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ] )

   CASE nParams == 6
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ], aP[ 6 ] )

   CASE nParams == 7
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ], aP[ 6 ], aP[ 7 ] )

   CASE nParams == 8
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ], aP[ 6 ], aP[ 7 ], aP[ 8 ] )

   CASE nParams == 9
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ], aP[ 6 ], aP[ 7 ], aP[ 8 ], aP[ 9 ] )

   CASE nParams ==10
      ::pPtr := Qt_QIcon( aP[ 1 ], aP[ 2 ], aP[ 3 ], aP[ 4 ], aP[ 5 ], aP[ 6 ], aP[ 7 ], aP[ 8 ], aP[ 9 ], aP[10 ] )

   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/


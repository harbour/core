/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * WIN_BMP() class
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software & Systems Ltd
 * www - http://harbour-project.org
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

#include "hbclass.ch"

#include "hbwin.ch"

CREATE CLASS WIN_BMP

   EXPORTED:

   METHOD New()
   METHOD LoadFile( cFileName, aDimXY )
   METHOD Create()
   METHOD Destroy()
   METHOD IsSupported( oPrn, /* @ */ nError )
   METHOD Draw( oPrn, aRectangle, /* @ */ nError )

   VAR Type     INIT 0                  // Type BitMap: 1 == BM, 2 == JPEG, 3 == PNG
   VAR DimXY    INIT { 0, 0 }           // Image Dimensions X Y pixels
   VAR Rect     INIT { 0, 0, 0, 0 }     // Coordinates to print BitMap
                                        //   XDest,                    : x-coord of destination upper-left corner
                                        //   YDest,                    : y-coord of destination upper-left corner
                                        //   nDestWidth,               : width of destination rectangle
                                        //   nDestHeight,              : height of destination rectangle
                                        // See WinApi StretchDIBits()
   VAR BitMap   INIT ""
   VAR FileName INIT ""
ENDCLASS

METHOD New() CLASS WIN_BMP
   RETURN Self

METHOD LoadFile( cFileName, aDimXY ) CLASS WIN_BMP
   ::FileName := cFileName
   ::Bitmap := win_LoadBitMapFile( ::FileName )
   IF Empty( ::Bitmap )
      ::Type := 0
      ::DimXY := { 0, 0 }
   ELSE
      ::Type := win_bitmapType( ::Bitmap )
      IF HB_ISARRAY( aDimXY )
        ::DimXY := aDimXY
      ELSEIF ! win_BitMapDimensions( ::Bitmap, @::DimXY[ 1 ], @::DimXY[ 2 ] )
        ::DimXY := { 1, 1 } // Driver may use the original dimensions
      ENDIF
   ENDIF
   RETURN ::Type != HB_WIN_BITMAP_UNKNOWN

METHOD Create() CLASS WIN_BMP  // Compatibility function for Alaska Xbase++
   RETURN Self

METHOD Destroy() CLASS WIN_BMP  // Compatibility function for Alaska Xbase++
   RETURN NIL

METHOD IsSupported( oPrn, /* @ */ nError ) CLASS WIN_BMP
   RETURN ( nError := win_BitmapIsSupported( oPrn:hPrinterDc, ::Bitmap ) ) == 0

METHOD Draw( oPrn, aRectangle, /* @ */ nError ) CLASS WIN_BMP // Pass a WIN_PRN object reference & Rectangle array
   IF HB_ISARRAY( aRectangle )
      ::Rect := aRectangle
   ENDIF
   RETURN iif( ::IsSupported( oPrn, @nError ), oPrn:DrawBitMap( Self ), .F. )

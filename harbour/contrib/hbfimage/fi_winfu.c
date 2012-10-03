/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FreeImage windows specific functions.
 *
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * www - http://www.xharbour.org http://harbour-project.org
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

/* TOFIX: To use GC collected pointers. */

#include "hbapi.h"
#include "hbapierr.h"

#if defined( HB_OS_WIN )

#include <windows.h>

#if !defined( _WINDOWS_ )
#  define _WINDOWS_
#endif

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "FreeImage.h"

/* Convert from FreeImage to HBITMAP */

/* implementation: HBITMAP bitmap = FI_FiToBitmap( FIBITMAP *dib ); */
HB_FUNC( FI_FITOBITMAP )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
#if ! defined( HB_OS_WIN_CE )
      FIBITMAP *dib;
      HBITMAP bitmap;
      HDC hDC;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function */
      hDC = GetDC( NULL );
      bitmap = CreateDIBitmap( hDC, FreeImage_GetInfoHeader( dib ),
                               CBM_INIT, FreeImage_GetBits( dib ),
                               FreeImage_GetInfo( dib ), DIB_RGB_COLORS );
      ReleaseDC( NULL, hDC );

      /* return value */
      if( bitmap )
         hb_retptr( bitmap );
#endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* Convert from HBITMAP to FreeImage */

/* implementation:  FIBITMAP *dib = FI_BitmapToFi( HBITMAP bitmap ); */
HB_FUNC( FI_BITMAPTOFI )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
#if ! defined( HB_OS_WIN_CE )
      FIBITMAP * dib;
      HBITMAP bitmap;

      /* Retrieve parameters */
      bitmap = ( HBITMAP ) hb_parptr( 1 );

      /* run function */
      dib = NULL;

      if( bitmap )
      {
         BITMAP bm;
         HDC hDC;

         GetObject( bitmap, sizeof( BITMAP ), ( LPSTR ) &bm );
         dib = FreeImage_Allocate( bm.bmWidth, bm.bmHeight, bm.bmBitsPixel, 0, 0, 0 );
         hDC = GetDC( NULL );
         GetDIBits( hDC, bitmap, 0, FreeImage_GetHeight( dib ),
                    FreeImage_GetBits( dib ), FreeImage_GetInfo( dib ), DIB_RGB_COLORS );
         ReleaseDC( NULL, hDC );
      }

      /* return value */
      if( dib )
         hb_retptr( dib );
#endif
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* Draw an image in a window Box */

/* implementation: int scanlines = FI_WinDraw( FIBITMAP *dib, HDC hDC, nTop, nLeft, nBottom, nRight ); */
HB_FUNC( FI_WINDRAW )
{
   if( hb_pcount() == 6 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 )
     )
   {
      FIBITMAP * dib;
      HDC hDC;
      RECT rcDest;
      int scanlines;

      /* Retrieve parameters */
      dib  = ( FIBITMAP * ) hb_parptr( 1 );
      hDC  = HB_ISNUM( 2 ) ? ( HDC ) ( HB_PTRUINT ) hb_parnint( 2 ) : ( HDC ) hb_parptr( 2 );
      rcDest.top    = hb_parni( 3 );
      rcDest.left   = hb_parni( 4 );
      rcDest.bottom = hb_parni( 5 );
      rcDest.right  = hb_parni( 6 );

      /* run function */
#if ! defined( HB_OS_WIN_CE )
      SetStretchBltMode( hDC, COLORONCOLOR );
#endif

      scanlines = StretchDIBits( hDC, rcDest.left,
                                      rcDest.top,
                                      rcDest.right-rcDest.left,
                                      rcDest.bottom-rcDest.top,
                                      0, 0, FreeImage_GetWidth( dib ), FreeImage_GetHeight( dib ),
                                      FreeImage_GetBits( dib ), FreeImage_GetInfo( dib ),
                                      DIB_RGB_COLORS, SRCCOPY );

      /* return value */
      hb_retni( scanlines );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ),
         hb_paramError( 4 ), hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

#else

HB_FUNC( FI_FITOBITMAP )
{
   hb_ret();
}

HB_FUNC( FI_BITMAPTOFI )
{
   hb_ret();
}

HB_FUNC( FI_WINDRAW )
{
   hb_retni( 0 );
}

#endif /* defined( HB_OS_WIN ) */

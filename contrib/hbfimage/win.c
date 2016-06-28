/*
 * FreeImage graphic library low level (client api) interface code.
 *
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 * Copyright 2012 Viktor Szakats (vszakats.net/harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbwapi.h"
#include "hbfiint.h"

/* Convert from FreeImage to HBITMAP */

HB_FUNC( FI_WINCONVTODIB )
{
#if defined( HB_OS_WIN )
   if( hb_FIBITMAP_is( 1 ) )
   {
#if ! defined( HB_OS_WIN_CE )
      FIBITMAP * dib = hb_FIBITMAP_par( 1 );
      HDC        hDC = GetDC( NULL );

      /* run function */
      hb_retptr( CreateDIBitmap( hDC,
                                 FreeImage_GetInfoHeader( dib ),
                                 CBM_INIT,
                                 FreeImage_GetBits( dib ),
                                 FreeImage_GetInfo( dib ),
                                 DIB_RGB_COLORS ) );

      ReleaseDC( NULL, hDC );
#else
      hb_retptr( NULL );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

/* Convert from HBITMAP to FreeImage */

HB_FUNC( FI_WINCONVFROMDIB )
{
#if defined( HB_OS_WIN )
   if( HB_ISPOINTER( 1 ) )
   {
#if ! defined( HB_OS_WIN_CE )
      HBITMAP bitmap = ( HBITMAP ) hb_parptr( 1 );

      if( bitmap )
      {
         FIBITMAP * dib;

         BITMAP bm;
         HDC    hDC;

         GetObject( bitmap, sizeof( BITMAP ), ( LPSTR ) &bm );
         dib = FreeImage_Allocate( bm.bmWidth, bm.bmHeight, bm.bmBitsPixel, 0, 0, 0 );
         hDC = GetDC( NULL );
         GetDIBits( hDC,
                    bitmap,
                    0,
                    FreeImage_GetHeight( dib ),
                    FreeImage_GetBits( dib ),
                    FreeImage_GetInfo( dib ),
                    DIB_RGB_COLORS );
         ReleaseDC( NULL, hDC );

         if( dib )
         {
            hb_FIBITMAP_ret( dib, HB_TRUE );
            return;
         }
      }
#endif
      hb_retptr( NULL );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#endif
}

/* Draw an image in a window Box */

HB_FUNC( FI_WINDRAW )
{
#if defined( HB_OS_WIN )
   if( hb_FIBITMAP_is( 1 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) )
   {
      HDC hDC = hbwapi_par_HDC( 2 );

      if( hDC )
      {
         FIBITMAP * dib = hb_FIBITMAP_par( 1 );
         RECT       rcDest;

         rcDest.top    = hb_parni( 3 );
         rcDest.left   = hb_parni( 4 );
         rcDest.bottom = hb_parni( 5 );
         rcDest.right  = hb_parni( 6 );

         /* run function */
#if ! defined( HB_OS_WIN_CE )
         SetStretchBltMode( hDC, COLORONCOLOR );
#endif

         /* return scanlines */
         hb_retni( StretchDIBits( hDC,
                                  rcDest.left,
                                  rcDest.top,
                                  rcDest.right - rcDest.left,
                                  rcDest.bottom - rcDest.top,
                                  0,
                                  0,
                                  FreeImage_GetWidth( dib ),
                                  FreeImage_GetHeight( dib ),
                                  FreeImage_GetBits( dib ),
                                  FreeImage_GetInfo( dib ),
                                  DIB_RGB_COLORS,
                                  SRCCOPY ) );
      }
      else
         hb_retni( 0 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
#else
   hb_retni( 0 );
#endif
}

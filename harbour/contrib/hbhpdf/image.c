/*
 * $Id$
 */

/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbhpdf.h"

/* HPDF_LoadPngImageFromFile( hDoc, cPNGFileName ) -> hImage
 */
HB_FUNC( HPDF_LOADPNGIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadPngImageFromFile( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_LoadPngImageFromFile2( hDoc, cPNGFileName ) -> hImage
 */
HB_FUNC( HPDF_LOADPNGIMAGEFROMFILE2 )
{
   hb_retptr( ( void * ) HPDF_LoadPngImageFromFile2( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}

/* HPDF_LoadRawImageFromFile( hDoc, cImageFileName, nWidth, nHeight, nColorSpace ) -> hImage
       nColorSpace
   HPDF_CS_DEVICE_GRAY
   HPDF_CS_DEVICE_RGB
   HPDF_CS_DEVICE_CMYK
 */
HB_FUNC( HPDF_LOADRAWIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadRawImageFromFile( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HPDF_ColorSpace ) hb_parni( 5 ) ) );
}

/* HPDF_LoadRawImageFromMem( hDoc, cBuffer, nWidth, nHeight, nColorSpace, nBitsPerComponents ) -> hImage
 */
HB_FUNC( HPDF_LOADRAWIMAGEFROMMEM )
{
   hb_retptr( ( void * ) HPDF_LoadRawImageFromMem( hb_HPDF_Doc_par( 1 ), ( HPDF_BYTE * ) hb_parc( 2 ), hb_parni( 3 ), hb_parni( 4 ), ( HPDF_ColorSpace ) hb_parni( 5 ), hb_parni( 6 ) ) );
}

/* HPDF_LoadJPEGImageFromFile( hDoc, cHPEGFileName ) -> hImage
 */
HB_FUNC( HPDF_LOADJPEGIMAGEFROMFILE )
{
   hb_retptr( ( void * ) HPDF_LoadJpegImageFromFile( hb_HPDF_Doc_par( 1 ), hb_parc( 2 ) ) );
}

/*
   HPDF_EXPORT(HPDF_Image)
   HPDF_LoadPngImageFromMem  (HPDF_Doc     pdf,
                    const HPDF_BYTE    *buffer,
                          HPDF_UINT     size);
 */
HB_FUNC( HPDF_LOADPNGIMAGEFROMMEM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT   size = ( HPDF_UINT ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retptr( ( HPDF_Image ) HPDF_LoadPngImageFromMem( hb_HPDF_Doc_par( 1 ), buffer, size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retptr( NULL );
#endif
}
/*
   HPDF_EXPORT(HPDF_Image)
   HPDF_LoadJpegImageFromMem   (HPDF_Doc      pdf,
                      const HPDF_BYTE     *buffer,
                            HPDF_UINT      size);
 */
HB_FUNC( HPDF_LOADJPEGIMAGEFROMMEM )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   HPDF_UINT   size = ( HPDF_UINT ) hb_parclen( 2 );
   HPDF_BYTE * buffer;

   buffer = ( HPDF_BYTE * ) hb_xgrab( size + 1 );

   hb_retptr( ( HPDF_Image ) HPDF_LoadJpegImageFromMem( hb_HPDF_Doc_par( 1 ), buffer, size ) );

   if( ! hb_storclen_buffer( ( char * ) buffer, size, 2 ) )
      hb_xfree( buffer );
#else
   hb_storc( NULL, 2 );
   hb_retptr( NULL );
#endif
}

/* HPDF_Image_GetSize( hImage ) -> aSize[ nW, nH ]
 */
HB_FUNC( HPDF_IMAGE_GETSIZE )
{
   HPDF_Point pt;
   PHB_ITEM   info = hb_itemArrayNew( 2 );

   pt = HPDF_Image_GetSize( ( HPDF_Image ) hb_parptr( 1 ) );

   hb_arraySetND( info, 1, pt.x );
   hb_arraySetND( info, 2, pt.y );

   hb_itemReturnRelease( info );
}

/* HPDF_Image_GetWidth( hImage ) -> nWidth
 */
HB_FUNC( HPDF_IMAGE_GETWIDTH )
{
   hb_retni( HPDF_Image_GetWidth( ( HPDF_Image ) hb_parptr( 1 ) ) );
}

/* HPDF_Image_GetHeight( hImage ) -> nHeight
 */
HB_FUNC( HPDF_IMAGE_GETHEIGHT )
{
   hb_retni( HPDF_Image_GetHeight( ( HPDF_Image ) hb_parptr( 1 ) ) );
}

/* HPDF_Image_GetBitsPerComponent( hImage ) -> nBitsPerComponent
 */
HB_FUNC( HPDF_IMAGE_GETBITSPERCOMPONENT )
{
   hb_retni( HPDF_Image_GetBitsPerComponent( ( HPDF_Image ) hb_parptr( 1 ) ) );
}

/* HPDF_Image_GetColorSpace( hImage ) -> nColorSpace
 */
HB_FUNC( HPDF_IMAGE_GETCOLORSPACE )
{
   hb_retc( HPDF_Image_GetColorSpace( ( HPDF_Image ) hb_parptr( 1 ) ) );
}

/* HPDF_Image_SetColorMask( hImage, nRGB_R_Min, nRGB_R_Max, nRGB_G_Min, nRGB_G_Max, nRGB_B_Min, nRGB_B_Max )
 */
HB_FUNC( HPDF_IMAGE_SETCOLORMASK )
{
   hb_retnl( ( long ) HPDF_Image_SetColorMask( ( HPDF_Image ) hb_parptr( 1 ),
                                               hb_parni( 2 ),
                                               hb_parni( 3 ),
                                               hb_parni( 4 ),
                                               hb_parni( 5 ),
                                               hb_parni( 6 ),
                                               hb_parni( 7 ) ) );
}

/* HPDF_Image_SetMaskImage( hImage, hImageMask ) -> hStatus
 */
HB_FUNC( HPDF_IMAGE_SETMASKIMAGE )
{
   hb_retnl( ( long ) HPDF_Image_SetMaskImage( ( HPDF_Image ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
}

/*
   HPDF_EXPORT(HPDF_STATUS)
   HPDF_Image_AddSMask  (HPDF_Image    image,
                      HPDF_Image    smask);
 */
HB_FUNC( HPDF_IMAGE_ADDSMASK )
{
#if HB_HPDF_VERS( 2, 2, 0 )
   hb_retnl( ( long ) HPDF_Image_AddSMask( ( HPDF_Image ) hb_parptr( 1 ), ( HPDF_Image ) hb_parptr( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}

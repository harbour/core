/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GD graphic library low level (client api) interface code.
 *
 * Copyright 2004-2005 Francesco Saverio Giudice <info@fsgiudice.com>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"

/* NOTE: Do some initialization required by the GD headers. */
#if defined( HB_OS_WIN )
#  if !defined( WIN32 )
#     define WIN32
#endif
#  if !defined( BGDWIN32 )
#     define BGDWIN32
#  endif
#endif

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "gd.h"
#include "gdfontt.h"
#include "gdfonts.h"
#include "gdfontmb.h"
#include "gdfontl.h"
#include "gdfontg.h"

#define HB_GD_VERS( ma, mi, re )   ( GD_MAJOR_VERSION >= ma && GD_MINOR_VERSION >= mi && GD_RELEASE_VERSION >= re )

#define IMAGE_JPEG     1
#define IMAGE_GIF      2
#define IMAGE_PNG      3
#define IMAGE_WBMP     4
#define IMAGE_GD       5


/* ---------------------------------------------------------------------------*/
/* Internal functions                                                         */
/* ---------------------------------------------------------------------------*/

/*
  *******************
  ** internal function for handling pointers
  **
  ** Code of Przemyslaw Czerpak for gdImagePtr
  ** adapted also to gdFontPtr
  *******************
*/

/*
  gdImage -----------------------

*/

/* gdImage destructor, it's executed automatically
 */
static HB_GARBAGE_FUNC( hb_gdImage_Destructor )
{
   /* Retrieve image pointer holder */
   gdImagePtr * imPtr = ( gdImagePtr * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( * imPtr )
   {
      /* Destroy the image */
      gdImageDestroy( * imPtr );

      /* set pointer to NULL to avoid multiple freeing */
      * imPtr = NULL;
   }
}

static const HB_GC_FUNCS s_gcGDimageFuncs =
{
   hb_gdImage_Destructor,
   hb_gcDummyMark
};


/* ---------------------------------------------------------------------------*/

/* function returns gdImage pointer or NULL when wrong variable is
 * passed or gdImage was freed before
 */
static gdImagePtr hb_parGdImage( int iParam )
{
   gdImagePtr * imPtr =
               ( gdImagePtr * ) hb_parptrGC( &s_gcGDimageFuncs, iParam );

   if( imPtr )
      return * imPtr;
   else
      return NULL;
}

/* ---------------------------------------------------------------------------*/

/* function create in HVM stack return value item with gdImage pointer
 */
static void hb_retGdImage( gdImagePtr im )
{
   gdImagePtr * imPtr;

   imPtr = ( gdImagePtr * ) hb_gcAllocate( sizeof( gdImagePtr ),
                                           &s_gcGDimageFuncs );
   * imPtr = im;
   hb_retptrGC( ( void * ) imPtr );
}

/* ---------------------------------------------------------------------------*/

#if 0
/* function returns PHB_ITEM with gdImage pointer
 */
static PHB_ITEM hb_gdImageItemNew( gdImagePtr im )
{
   gdImagePtr * imPtr;

   imPtr = ( gdImagePtr * ) hb_gcAllocate( sizeof( gdImagePtr ),
                                           &s_gcGDimageFuncs );
   * imPtr = im;
   return hb_itemPutPtrGC( NULL, ( void * ) imPtr );
}
#endif

/*
  gdFont -----------------------

*/

/* gdFont destructor, it's executed automatically
 */
static HB_GARBAGE_FUNC( hb_gdFont_Destructor )
{
   /* Retrieve Font pointer holder */
   gdFontPtr * fontPtr = ( gdFontPtr * ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( * fontPtr )
   {
      /* Destroy the Font */
#if 0
      /* do nothing, GD handles it directly and gdFontDestroy() not exists */
      gdFontDestroy( * fontPtr );
#endif

      /* set pointer to NULL to avoid multiple freeing */
      * fontPtr = NULL;
   }
}

static const HB_GC_FUNCS s_gcGDfontFuncs =
{
   hb_gdFont_Destructor,
   hb_gcDummyMark
};

/* ---------------------------------------------------------------------------*/

/* function returns gdFont pointer or NULL when wrong variable is
 * passed or gdFont was freed before
 */
static gdFontPtr hb_parGdFont( int iParam )
{
   gdFontPtr * fontPtr =
               ( gdFontPtr * ) hb_parptrGC( &s_gcGDfontFuncs, iParam );

   if( fontPtr )
      return * fontPtr;
   else
      return NULL;
}

/* ---------------------------------------------------------------------------*/

/* function create in HVM stack return value item with gdFont pointer
 */
static void hb_retGdFont( gdFontPtr font )
{
   gdFontPtr * fontPtr;

   fontPtr = ( gdFontPtr * ) hb_gcAllocate( sizeof( gdFontPtr ),
                                            &s_gcGDfontFuncs );
   * fontPtr = font;
   hb_retptrGC( ( void * ) fontPtr );
}

/* ---------------------------------------------------------------------------*/

#if 0
/* function returns PHB_ITEM with gdFont pointer
 */
static PHB_ITEM hb_gdFontItemNew( gdFontPtr font )
{
   gdFontPtr * fontPtr;

   fontPtr = ( gdFontPtr * ) hb_gcAllocate( sizeof( gdFontPtr ),
                                            &s_gcGDfontFuncs );
   * fontPtr = font;
   return hb_itemPutPtrGC( NULL, ( void * ) fontPtr );
}
#endif

/* ---------------------------------------------------------------------------*/

static void * LoadImageFromHandle( HB_FHANDLE fhandle, int sz )
{
   void * iptr;

   if( ! fhandle )
      fhandle = 0; /* 0 = std input */

   /* Read file */
   iptr = hb_xgrab( sz );
   hb_fsReadLarge( fhandle, iptr, ( HB_SIZE ) sz );
   /*   TraceLog( NULL, "Error dim %i, read %i", sz, iRead ); */

   return iptr;

}

/* ---------------------------------------------------------------------------*/

static void * LoadImageFromFile( const char *szFile, int *sz )
{
   void * iptr;
   HB_FHANDLE fhandle;

   if( ( fhandle = hb_fsOpen( szFile, FO_READ ) ) != FS_ERROR )
   {
      /* get lenght */
      *sz = ( int ) hb_fsSeek( fhandle, 0, FS_END );
      /* rewind */
      hb_fsSeek( fhandle, 0, FS_SET );

      /* Read file */
      iptr = hb_xgrab( *sz );
      hb_fsReadLarge( fhandle, iptr, ( HB_SIZE ) *sz );
      /*   TraceLog( NULL, "Error dim %i, read %i", sz, iRead ); */

      /* Close file */
      hb_fsClose( fhandle );
   }
   else
   {
      /* File error */
      iptr = NULL;
      *sz  = 0;
   }

   return iptr;

}

/* ---------------------------------------------------------------------------*/

static void SaveImageToHandle( HB_FHANDLE fhandle, const void * iptr, int sz )
{
   if( ! fhandle )
      fhandle = 1; /* 1 = std output */

   /* Write Image */
   hb_fsWriteLarge( fhandle, iptr, ( HB_SIZE ) sz );
}

/* ---------------------------------------------------------------------------*/

static void SaveImageToFile( const char *szFile, const void * iptr, int sz )
{
   HB_FHANDLE fhandle;

   if( ( fhandle = hb_fsCreate( szFile, FC_NORMAL ) ) != FS_ERROR )
   {
      /* Write Image */
      SaveImageToHandle( fhandle, iptr, sz );

      /* Close file */
      hb_fsClose( fhandle );
   }
}

/* ---------------------------------------------------------------------------*/

static void GDImageCreateFrom( int nType )
{
   gdImagePtr im = NULL;
   const char *szFile;
   int sz;
   void * iptr;

   /*TraceLog( NULL, "Params = %i, 1 = %i, 2 = %i \n\r", hb_pcount(), hb_parinfo( 1 ), hb_parinfo( 2 ) );*/

   if( hb_pcount() == 1 &&
        ( HB_ISCHAR( 1 ) )
     )
   {
      /* Retrieve file name */
      szFile = hb_parcx( 1 );
      /* Retrieve image */
      iptr   = LoadImageFromFile( szFile, &sz );
   }

   /* From Image Pointer + size */
   else if( hb_pcount() == 2 &&
        ( HB_ISPOINTER( 1 ) ) &&
        ( HB_ISNUM( 2 ) )
     )
   {

      /* Retrieve image pointer */
      iptr = hb_parGdImage( 1 );
      /* Retrieve image size */
      sz     = hb_parni( 2 );
   }

   /* From file handle */
   else if( hb_pcount() == 2 &&
        ( HB_ISNUM( 1 ) ) &&
        ( HB_ISNUM( 2 ) )
     )
   {
      /* Retrieve file handle */
      HB_FHANDLE fhandle = HB_ISNUM( 1 ) ? hb_numToHandle( hb_parnint( 1 ) ) : 0; /* 0 = std input */

      /* Retrieve image size */
      sz     = hb_parni( 2 );

      /* retrieve image from handle */
      iptr = LoadImageFromHandle( fhandle, sz );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
      return;
   }

   if( iptr && sz )
   {
      /* Create Image */
      switch ( nType )
      {
         case IMAGE_JPEG :
              im = gdImageCreateFromJpegPtr( sz, iptr );
              break;
         case IMAGE_GIF  :
              im = gdImageCreateFromGifPtr( sz, iptr );
              break;
         case IMAGE_PNG  :
              im = gdImageCreateFromPngPtr( sz, iptr );
              break;
         case IMAGE_WBMP :
              im = gdImageCreateFromWBMPPtr( sz, iptr );
              break;
         case IMAGE_GD   :
              im = gdImageCreateFromGdPtr( sz, iptr );
              break;
      }

      /* Return image pointer */
      hb_retGdImage( im );

      /* Free memory */
      hb_xfree( iptr );

   }
}

/* ---------------------------------------------------------------------------*/

static void GDImageSaveTo( int nType )
{
   if( hb_pcount() >= 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;
      int sz = 0;
      void * iptr = NULL;
      int level = 0, fg = 0;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Get file name or an output handler or NIL it I want a return string */
      if( !( HB_ISNIL( 2 ) ||
             HB_ISCHAR( 2 ) ||
             HB_ISNUM( 2 ) ) )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0,
            "Second argument must be NIL or numeric or a string.",
            HB_ERR_FUNCNAME, 2,
            hb_paramError( 2 ) );
         return;
      }

      /* Retrieve compression level */
      /*TraceLog( NULL, "Count = %i\n\r", hb_pcount() ); */
      /* check if is numeric */
      if( !( HB_ISNIL( 3 ) || HB_ISNUM( 3 ) ) )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0,
            "Tirdh argument must be NIL or numeric.",
            HB_ERR_FUNCNAME, 1,
            hb_paramError( 3 ) );
         return;
      }

      if( nType == IMAGE_JPEG )
      {
         /* check range */
         level = hb_parnidef( 3, -1 );
         if( !( level >= -1 && level <= 95 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
               "Compression level must be -1 (default) or a value between 0 and 95.",
               HB_ERR_FUNCNAME, 1,
               hb_paramError( 3 ) );
            return;
         }
      }
      else if( nType == IMAGE_PNG )
      {
         /* check range */
         level = hb_parnidef( 3, -1 );
         if( !( level >= -1 && level <= 9 ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
               "Compression level must be -1 (default) or a value between 0 and 9.",
               HB_ERR_FUNCNAME, 1,
               hb_paramError( 3 ) );
            return;
         }
      }
      else if( nType == IMAGE_WBMP )
      {
         if( !( HB_ISNUM( 3 ) ) )
         {
            hb_errRT_BASE_SubstR( EG_ARG, 0,
               "Foreground color nedeed",
               HB_ERR_FUNCNAME, 1,
               hb_paramError( 3 ) );
            return;
         }
         fg = hb_parni( 3 );
      }

      switch( nType )
      {
      case IMAGE_JPEG  :
           /* Get image Ptr */
           iptr = gdImageJpegPtr( im, &sz, level );
           break;
      case IMAGE_GIF   :
           /* Get image Ptr */
           iptr = gdImageGifPtr( im, &sz );
           break;
      case IMAGE_PNG   :
           /* Get image Ptr */
           iptr = gdImagePngPtrEx( im, &sz, level );
           break;
      case IMAGE_WBMP  :
           /* Get image Ptr */
           iptr = gdImageWBMPPtr( im, &sz, fg );
           break;
      case IMAGE_GD    :
           /* Get image Ptr */
           iptr = gdImageGdPtr( im, &sz );
           break;
      }

      /* If i get a file name */
      if( HB_ISCHAR( 2 ) )
         SaveImageToFile( hb_parcx( 2 ), iptr, sz );

      /* Write to file handle (1 = std output) */
      else if( HB_ISNUM( 2 ) )
      {
         /* Write to std output or to a passed file */
         HB_FHANDLE fhandle = hb_numToHandle( hb_parnint( 2 ) );

         if( fhandle == FS_ERROR )
            fhandle = 1; /* 1 = std output */

         /* Write Image */
         SaveImageToHandle( fhandle, iptr, sz );
      }

      /* Return image as string) */
      else
      {
         /* Return as string */
         hb_retclen( ( const char * ) iptr, ( HB_SIZE ) sz );
      }

      /* Free memory */
      gdFree( iptr );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ************************* WRAPPED FUNCTIONS ****************************** */

HB_FUNC( GDVERSION )
{
#if HB_GD_VERS( 2, 0, 34 )
   char szVer[ 30 ];
   hb_snprintf( szVer, sizeof( szVer ), "GD Version %s", GD_VERSION_STRING );
   hb_retc( szVer );
#elif HB_GD_VERS( 2, 0, 33 )
   hb_retc_const( "GD Version 2.0.33" );
#else
   hb_retc_const( "GD Version 2.0.28" );
#endif
}

HB_FUNC( GDVERSIONNUMBER )
{
#if HB_GD_VERS( 2, 0, 34 )
   hb_retni( GD_MAJOR_VERSION * 1000 + GD_MINOR_VERSION * 100 + GD_RELEASE_VERSION );
#elif HB_GD_VERS( 2, 0, 33 )
   hb_retni( 2033 );
#else
   hb_retni( 2028 );
#endif
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* IMAGE CREATION, DESTRUCTION, LOADING AND SAVING                            */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATE ) /* gdImagePtr gdImageCreate(sx, sy) */
{
   if( hb_pcount() == 2 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im;
      int sx, sy;

      /* Retrieve dimensions */
      sx = hb_parni( 1 );
      sy = hb_parni( 2 );

      /* Create the image */
      im = gdImageCreate( sx, sy );

      /* Return image pointer */
      hb_retGdImage( im );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEPALETTE ) /* gdImagePtr gdImageCreatePalette(sx, sy) */
{
   /* Alias of GDCreate() */
   HB_FUNC_EXEC( GDIMAGECREATE );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATETRUECOLOR ) /* gdImageCreateTrueColor(sx, sy) */
{
   if( hb_pcount() == 2 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) )
   {
      gdImagePtr im;
      int sx, sy;

      /* Retrieve dimensions */
      sx = hb_parni( 1 );
      sy = hb_parni( 2 );

      /* Create the image */
      im = gdImageCreateTrueColor( sx, sy );

      /* Return image pointer */
      hb_retGdImage( im );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEFROMJPEG ) /* gdImageCreateFromJpegPtr(int size, void *data) */
                                 /* implementation: gdImagePtr gdImageCreateFromJpeg( char *szFile ) */
{
   GDImageCreateFrom( IMAGE_JPEG );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEFROMGIF ) /* gdImageCreateFromGifPtr(int size, void *data) */
                                /* implementation: gdImagePtr gdImageCreateFromGif( char *szFile ) */
{
   GDImageCreateFrom( IMAGE_GIF );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEFROMPNG ) /* gdImageCreateFromPngPtr(int size, void *data) */
                                /* implementation: gdImagePtr gdImageCreateFromPng( char *szFile ) */
{
   GDImageCreateFrom( IMAGE_PNG );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEFROMWBMP ) /* gdImagePtr gdImageCreateFromWBMPPtr (int size, void *data) */
                                 /* implementation: gdImagePtr gdImageCreateFromWBMP ( char *szFile ) */
{
   GDImageCreateFrom( IMAGE_WBMP );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEFROMGD ) /* gdImagePtr gdImageCreateFromGdPtr (int size, void *data) */
                               /* implementation: gdImagePtr gdImageCreateFromGd ( char *szFile ) */
{
   GDImageCreateFrom( IMAGE_GD );
}


/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEJPEG ) /* original: void gdImageJpeg(gdImagePtr im, FILE *out) */
                       /* implementation: void gdImageJpeg(gdImagePtr im, char *szFile) */
{
   GDImageSaveTo( IMAGE_JPEG );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGIF ) /* original: void gdImageGif(gdImagePtr im, FILE *out) */
                      /* implementation: void gdImageGif(gdImagePtr im, char *szFile) */
{
   GDImageSaveTo( IMAGE_GIF );
}

HB_FUNC( GDIMAGEPNG ) /* original: void gdImagePngEx(gdImagePtr im, FILE *out, int level) */
                      /* implementation: void gdImagePng(gdImagePtr im, char *szFile [, int level] ) */
{
   GDImageSaveTo( IMAGE_PNG );
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEWBMP ) /* original: void gdImageWBMP(gdImagePtr im, FILE *out) */
                       /* implementation: void gdImageWBMP(gdImagePtr im, char *szFile, int fg) */
{
   GDImageSaveTo( IMAGE_WBMP );
}


/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGD ) /* original: void gdImageGD(gdImagePtr im, FILE *out) */
                     /* implementation: void gdImageGD(gdImagePtr im, char *szFile) */
{
   GDImageSaveTo( IMAGE_GD );
}


/* ---------------------------------------------------------------------------*/

/*
  After Przemek changes on hb_*ptr() functions, this is a void function holded only
  for compatibility with GD library.
*/
HB_FUNC( GDIMAGEDESTROY ) /* gdImageDestroy(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 ) )
   {
      /*
      gdImagePtr im;
      */

      /* Retrieve image pointer */
      /*
      im = (gdImagePtr)hb_parptr( 1 );
      */

      /* Destroy the image */
      /*
      gdImageDestroy( im );
      */

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }

}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* DRAWING FUNCTIONS                                                          */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETPIXEL ) /* void gdImageSetPixel(gdImagePtr im, int x, int y, int color) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int x, y;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord values */
      x = hb_parni( 2 );
      y = hb_parni( 3 );

      /* Retrieve color value */
      color = hb_parni( 4 );

      /* Draw a rectangle */
      gdImageSetPixel( im, x, y, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGELINE ) /* void gdImageLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
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
      gdImagePtr im;
      int x1, y1, x2, y2;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord values */
      x1 = hb_parni( 2 );
      y1 = hb_parni( 3 );
      x2 = hb_parni( 4 );
      y2 = hb_parni( 5 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a rectangle */
      gdImageLine( im, x1, y1, x2, y2, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEDASHEDLINE ) /* void gdImageDashedLine(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
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
      gdImagePtr im;
      int x1, y1, x2, y2;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord values */
      x1 = hb_parni( 2 );
      y1 = hb_parni( 3 );
      x2 = hb_parni( 4 );
      y2 = hb_parni( 5 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a rectangle */
      gdImageDashedLine( im, x1, y1, x2, y2, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEPOLYGON ) /* original: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
                          /* implementation: void gdImagePolygon(gdImagePtr im, gdPointPtr points, int color) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      /*gdPointPtr points; */
      int pointsTotal;
      int color;
      PHB_ITEM pPoints;
      int i;

      /* Max Points of polygon */
      gdPoint * points;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point array */
      pPoints     = hb_param( 2, HB_IT_ARRAY );
      pointsTotal = ( int ) hb_arrayLen( pPoints );
      points = ( gdPoint * ) hb_xgrab( sizeof( gdPoint ) * pointsTotal );

      for( i = 0; i < pointsTotal; i ++ )
      {
         PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i+1 );
         if( HB_IS_ARRAY( pPoint ) )
         {
            points[ i ].x = hb_arrayGetNI( pPoint, 1 );
            points[ i ].y = hb_arrayGetNI( pPoint, 2 );
         }
      }

      /* Retrieve color value */
      color = hb_parni( 3 );

      /* Draw a polygon */
      gdImagePolygon( im, ( gdPointPtr ) points, pointsTotal, color );

      hb_xfree( points );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/
HB_FUNC( GDIMAGEOPENPOLYGON ) /* original: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
                              /* implementation: void gdImageOpenPolygon(gdImagePtr im, gdPointPtr points, int color) */
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      /*gdPointPtr points; */
      int pointsTotal;
      int color;
      PHB_ITEM pPoints;
      int i;

      /* Max Points of polygon */
      gdPoint * points;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point array */
      pPoints     = hb_param( 2, HB_IT_ARRAY );
      pointsTotal = ( int ) hb_arrayLen( pPoints );
      points = ( gdPoint * ) hb_xgrab( sizeof( gdPoint ) * pointsTotal );

      for( i = 0; i < pointsTotal; i ++ )
      {
         PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i+1 );
         if( HB_IS_ARRAY( pPoint ) )
         {
            points[ i ].x = hb_arrayGetNI( pPoint, 1 );
            points[ i ].y = hb_arrayGetNI( pPoint, 2 );
         }
      }

      /* Retrieve color value */
      color = hb_parni( 3 );

      /* Draw a polygon */
      gdImageOpenPolygon( im, ( gdPointPtr ) points, pointsTotal, color );

      hb_xfree( points );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
#endif /* ( GD_VERS >= 2033 ) */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGERECTANGLE ) /* void gdImageRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
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
      gdImagePtr im;
      int x1, y1, x2, y2;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord values */
      x1 = hb_parni( 2 );
      y1 = hb_parni( 3 );
      x2 = hb_parni( 4 );
      y2 = hb_parni( 5 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a rectangle */
      gdImageRectangle( im, x1, y1, x2, y2, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILLEDPOLYGON ) /* original: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int pointsTotal, int color) */
                                /* implementation: void gdImageFilledPolygon(gdImagePtr im, gdPointPtr points, int color) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      /*gdPointPtr points; */
      int pointsTotal;
      int color;
      PHB_ITEM pPoints;
      int i;

      /* Max Points of polygon */
      gdPoint points[50];

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point array */
      pPoints     = hb_param( 2, HB_IT_ARRAY );
      pointsTotal = ( int ) hb_arrayLen( pPoints );

      for( i = 0; i < pointsTotal; i ++ )
      {
         PHB_ITEM pPoint = hb_arrayGetItemPtr( pPoints, i+1 );
         if( HB_IS_ARRAY( pPoint ) )
         {
            points[ i ].x = hb_arrayGetNI( pPoint, 1 );
            points[ i ].y = hb_arrayGetNI( pPoint, 2 );
         }
      }

      /* Retrieve color value */
      color = hb_parni( 3 );

      /* Draw a filled polygon */
      gdImageFilledPolygon( im, ( gdPointPtr ) points, pointsTotal, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILLEDRECTANGLE ) /* void gdImageFilledRectangle(gdImagePtr im, int x1, int y1, int x2, int y2, int color) */
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
      gdImagePtr im;
      int x1, y1, x2, y2;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord values */
      x1 = hb_parni( 2 );
      y1 = hb_parni( 3 );
      x2 = hb_parni( 4 );
      y2 = hb_parni( 5 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a filled rectangle */
      gdImageFilledRectangle( im, x1, y1, x2, y2, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEARC ) /* void gdImageArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color) */
{
   if( hb_pcount() == 8 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 )
     )
   {
      gdImagePtr im;
      int cx, cy, w, h, s, e, color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      cx = hb_parni( 2 );
      cy = hb_parni( 3 );
      /* Retrieve width and height values */
      w  = hb_parni( 4 );
      h  = hb_parni( 5 );
      /* Retrieve starting and ending degree values */
      s  = hb_parni( 6 );
      e  = hb_parni( 7 );
      /* Retrieve color value */
      color = hb_parni( 8 );

      /* Draw an arc */
      gdImageArc( im, cx, cy, w, h, s, e, color );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 8,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILLEDARC ) /* void gdImageFilledArc(gdImagePtr im, int cx, int cy, int w, int h, int s, int e, int color[, int style]) */
{
   if( hb_pcount() >= 8 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 )
     )
   {
      gdImagePtr im;
      int cx, cy, w, h, s, e, color, style;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      cx = hb_parni( 2 );
      cy = hb_parni( 3 );
      /* Retrieve width and height values */
      w  = hb_parni( 4 );
      h  = hb_parni( 5 );
      /* Retrieve starting and ending degree values */
      s  = hb_parni( 6 );
      e  = hb_parni( 7 );
      /* Retrieve color value */
      color = hb_parni( 8 );

      /* Retrieve style value */
      style = hb_parnidef( 9, gdNoFill );

      /* Draw a filled arc */
      gdImageFilledArc( im, cx, cy, w, h, s, e, color, style);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 9,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILLEDELLIPSE ) /* void gdImageFilledEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
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
      gdImagePtr im;
      int cx, cy, w, h, color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      cx = hb_parni( 2 );
      cy = hb_parni( 3 );
      /* Retrieve width and height values */
      w  = hb_parni( 4 );
      h  = hb_parni( 5 );
      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a filled ellipse */
      gdImageFilledEllipse( im, cx, cy, w, h, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILLTOBORDER ) /* void gdImageFillToBorder(gdImagePtr im, int x, int y, int border, int color) */
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
     )
   {
      gdImagePtr im;
      int x, y, border, color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      x = hb_parni( 2 );
      y = hb_parni( 3 );
      /* Retrieve color to use as border */
      border = hb_parni( 4 );
      /* Retrieve color value */
      color = hb_parni( 5 );

      /* Fill image to border */
      gdImageFillToBorder( im, x, y, border, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ) );
   }
}

/* ---------------------------------------------------------------------------*/
/* Disabled, because there is a .prg implementation which
   works with all gd lib versions. [vszakats] */
#if 0
HB_FUNC( GDIMAGEELLIPSE ) /* void gdImageEllipse(gdImagePtr im, int cx, int cy, int w, int h, int color) */
{
#if HB_GD_VERS( 2, 0, 35 )
   if( hb_pcount() == 6 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 )
     )
   {
      gdImagePtr im;
      int cx, cy, w, h, color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      cx = hb_parni( 2 );
      cy = hb_parni( 3 );
      /* Retrieve width and height values */
      w  = hb_parni( 4 );
      h  = hb_parni( 5 );
      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Draw a filled ellipse */
      gdImageEllipse( im, cx, cy, w, h, color );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
#endif /* ( GD_VERS >= 2035 ) */
}
#endif

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEFILL ) /* void gdImageFill(gdImagePtr im, int x, int y, int color) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int x, y, color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve point values */
      x = hb_parni( 2 );
      y = hb_parni( 3 );
      /* Retrieve color value */
      color = hb_parni( 4 );

      /* Fill image */
      gdImageFill( im, x, y, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETANTIALIASED ) /* void gdImageSetAntiAliased(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve color value */
      color = hb_parni( 2 );

      /* Set Antialias */
      gdImageSetAntiAliased( im, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETANTIALIASEDDONTBLEND ) /* void gdImageSetAntiAliasedDontBlend(gdImagePtr im, int c, int dont_blend) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int color;
      int dont_blend;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve color value */
      color = hb_parni( 2 );

      /* Retrieve dont_blend value */
      dont_blend = hb_parni( 3 );

      /* Set Antialias but don't blend */
      gdImageSetAntiAliasedDontBlend( im, color, dont_blend);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETBRUSH ) /* void gdImageSetBrush(gdImagePtr im, gdImagePtr brush) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 )
     )
   {
      gdImagePtr im;
      gdImagePtr brush;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve brush pointer */
      brush = hb_parGdImage( 2 );

      /* Set Brush */
      gdImageSetBrush( im, brush);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETTILE ) /* void gdImageSetTile(gdImagePtr im, gdImagePtr tile) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 )
     )
   {
      gdImagePtr im;
      gdImagePtr tile;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve tile pointer */
      tile = hb_parGdImage( 2 );

      /* Set Tile */
      gdImageSetTile( im, tile);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETSTYLE ) /* original: void gdImageSetStyle(gdImagePtr im, int *style, int styleLength) */
                           /* implementation: void gdImageSetStyle(gdImagePtr im, int *style) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISARRAY( 2 )
     )
   {
      gdImagePtr im;
      PHB_ITEM pStyles;
      int styleLength;
      int i;

      /* Max numbery of Styles */
      int * styles;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve style array */
      pStyles     = hb_param( 2, HB_IT_ARRAY );
      styleLength = ( int ) hb_arrayLen( pStyles );
      styles = ( int * ) hb_xgrab( sizeof( int ) * styleLength );

      for( i = 0; i < styleLength; i ++ )
      {
         styles[ i ] = hb_arrayGetNI( pStyles, i + 1 );
      }

      /* Set style */
      gdImageSetStyle( im, ( int * ) styles, styleLength);

      hb_xfree( styles );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETTHICKNESS ) /* void gdImageSetThickness(gdImagePtr im, int thickness) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int thickness;
      int oldthick;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve thickness value */
      thickness = hb_parni( 2 );

      /* Previous value */
      oldthick = im->thick;

      /* Set thickness */
      gdImageSetThickness( im, thickness);

      /* Return previous */
      hb_retni( oldthick );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEALPHABLENDING ) /* void gdImageAlphaBlending(gdImagePtr im, int blending) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 )
     )
   {
      gdImagePtr im;
      int blending;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve blending value - logical */
      blending = hb_parl( 2 );

      /* Set blending */
      gdImageAlphaBlending( im, blending);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESAVEALPHA ) /* void gdImageSaveAlpha(gdImagePtr im, int saveFlag) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 )
     )
   {
      gdImagePtr im;
      int saveFlag;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value - logical */
      saveFlag = hb_parl( 2 );

      /* Set saveFlag */
      gdImageSaveAlpha( im, saveFlag);
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESETCLIP ) /* void gdImageSetClip(gdImagePtr im, int x1, int y1, int x2, int y2) */
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
     )
   {
      gdImagePtr im;
      int x1, y1, x2, y2;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coords value */
      x1 = hb_parni( 2 );
      y1 = hb_parni( 3 );
      x2 = hb_parni( 4 );
      y2 = hb_parni( 5 );

      /* Set clipping rectangle */
      gdImageSetClip( im, x1, y1, x2, y2);
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGETCLIP ) /* original: void gdImageGetClip(gdImagePtr im, int *x1P, int *y1P, int *x2P, int *y2P) */
                          /* implementation: array gdImageGetClip(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;
      int x1, y1, x2, y2;
      PHB_ITEM pClipArray;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Get clipping rectangle */
      gdImageGetClip( im, &x1, &y1, &x2, &y2 );

      /* Return clipping rectangle value in an array */
      pClipArray = hb_itemArrayNew( 4 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 1 ), x1 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 2 ), y1 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 3 ), x2 );
      hb_itemPutNI( hb_arrayGetItemPtr( pClipArray, 4 ), y2 );

      /* return array */
      hb_itemReturnRelease( pClipArray );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* QUERY FUNCTIONS                                                            */
/* ---------------------------------------------------------------------------*/


HB_FUNC( GDIMAGECOLORSTOTAL ) /* int gdImageColorsTotal(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Get Colors total */
      hb_retni( gdImageColorsTotal( im ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEALPHA ) /* int gdImageAlpha(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Get Alpha Level */
      hb_retni( gdImageAlpha( im, color ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGERED ) /* int gdImageRed(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Get Red Level */
      hb_retni( gdImageRed( im, color ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGREEN ) /* int gdImageGreen(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Get Green Level */
      hb_retni( gdImageGreen( im, color ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEBLUE ) /* int gdImageBlue(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Get Blue Level */
      hb_retni( gdImageBlue( im, color ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESX ) /* int gdImageSX(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Get Image Width in pixels */
      hb_retni( gdImageSX( im ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESY ) /* int gdImageSX(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Get Image Height in pixels */
      hb_retni( gdImageSY( im ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGETPIXEL ) /* int gdImageGetPixel(gdImagePtr im, int x, int y) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int x, y;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord value */
      x = hb_parni( 2 );
      y = hb_parni( 3 );

      /* Get Color of a pixel */
      hb_retni( gdImageGetPixel( im, x, y ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEBOUNDSSAFE ) /* int gdImageBoundsSafe(gdImagePtr im, int x, int y) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int x, y;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord value */
      x = hb_parni( 2 );
      y = hb_parni( 3 );

      /* Get if pixel in Clipping region */
      hb_retl( gdImageBoundsSafe( im, x, y ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGETINTERLACED ) /* int gdImageGetInterlaced(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Image is interlaced ? */
      hb_retl( gdImageGetInterlaced( im ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGETTRANSPARENT ) /* int gdImageGetTransparent(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Image is trasparent ? */
      hb_retl( gdImageGetTransparent( im ) );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGETRUECOLOR ) /* int gdImageTrueColor(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Is TrueColor ? */
      hb_retl( gdImageTrueColor( im ) );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGETRUECOLORTOPALETTE ) /* void gdImageTrueColorToPalette (gdImagePtr im, int ditherFlag, int colorsWanted) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int ditherFlag, colorsWanted;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve ditherFlag - logical */
      ditherFlag = hb_parl( 2 );

      /* Retrieve number of color wanted */
      colorsWanted = hb_parni( 3 );

      /* Converts a truecolor image to a palette-based image */
      gdImageTrueColorToPalette( im, ditherFlag, colorsWanted);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECREATEPALETTEFROMTRUECOLOR ) /* gdImagePtr gdImageCreatePaletteFromTrueColor(gdImagePtr im, int ditherFlag, int colorsWanted) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      gdImagePtr imNew;
      int ditherFlag, colorsWanted;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve ditherFlag - logical */
      ditherFlag = hb_parl( 2 );

      /* Retrieve number of color wanted */
      colorsWanted = hb_parni( 3 );

      /* Converts a truecolor image to a palette-based image and return the image */
      imNew = gdImageCreatePaletteFromTrueColor( im, ditherFlag, colorsWanted);

      /* Return image pointer */
      hb_retGdImage( imNew );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEPALETTEPIXEL ) /* int gdImagePalettePixel(gdImagePtr im, int x, int y) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int x, y;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord value */
      x = hb_parni( 2 );
      y = hb_parni( 3 );

      /* Get Color of a pixel */
      hb_retni( gdImagePalettePixel( im, x, y) );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGETRUECOLORPIXEL ) /* int gdImageTrueColorPixel(gdImagePtr im, int x, int y) */
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      gdImagePtr im;
      int x, y;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve coord value */
      x = hb_parni( 2 );
      y = hb_parni( 3 );

      /* Get Color of a pixel */
      hb_retni( gdImageTrueColorPixel( im, x, y) );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEGETTHICKNESS ) /* void gdImageGetThickness(gdImagePtr im) */
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdImagePtr im;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Return previous */
      hb_retni( im->thick );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* FONTS AND TEXT-HANDLING FUNCTIONS                                          */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETSMALL ) /* gdFontPtr gdFontGetSmall(void) */
{
   gdFontPtr font;

   /* Get font pointer */
   font = gdFontGetSmall();

   /* Return font pointer */
   hb_retGdFont( font );
   /*hb_retptr( font ); */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETLARGE ) /* gdFontPtr gdFontGetLarge(void) */
{
   gdFontPtr font;

   /* Get font pointer */
   font = gdFontGetLarge();

   /* Return font pointer */
   hb_retGdFont( font );
   /*hb_retptr( font ); */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETMEDIUMBOLD ) /* gdFontPtr gdFontGetMediumBold(void) */
{
   gdFontPtr font;

   /* Get font pointer */
   font = gdFontGetMediumBold();

   /* Return font pointer */
   hb_retGdFont( font );
   /*hb_retptr( font ); */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETGIANT ) /* gdFontPtr gdFontGetGiant(void) */
{
   gdFontPtr font;

   /* Get font pointer */
   font = gdFontGetGiant();

   /* Return font pointer */
   hb_retGdFont( font );
   /*hb_retptr( font ); */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETTINY ) /* gdFontPtr gdFontGetTiny(void) */
{
   gdFontPtr font;

   /* Get font pointer */
   font = gdFontGetTiny();

   /* Return font pointer */
   hb_retGdFont( font );
   /*hb_retptr( font ); */
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESTRING ) /* void gdImageChar(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
                         /* void gdImageString(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
{
   if( hb_pcount() == 6 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISCHAR( 5 ) &&
       HB_ISNUM( 6 )
     )
   {
      gdImagePtr im;
      gdFontPtr font;
      int x, y, c, color;
      unsigned char * s;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve font pointer */
      font = hb_parGdFont( 2 );

      /* Retrieve coord value */
      x = hb_parni( 3 );
      y = hb_parni( 4 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Write char */
      if( hb_parclen(5) == 1 )
      {
         /* Retrieve char value */
         c = hb_parni( 5 );
         gdImageChar( im, font, x, y, c, color );
      }
      else
      {
         /* Retrieve string value */
         s = ( unsigned char * )hb_parcx( 5 );
         gdImageString( im, font, x, y, s, color );
      }

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESTRINGUP ) /* void gdImageCharUp(gdImagePtr im, gdFontPtr font, int x, int y, int c, int color) */
                           /* void gdImageStringUp(gdImagePtr im, gdFontPtr font, int x, int y, unsigned char *s, int color) */
{
   if( hb_pcount() == 6 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISCHAR( 5 ) &&
       HB_ISNUM( 6 )
     )
   {
      gdImagePtr im;
      gdFontPtr font;
      int x, y, c, color;
      unsigned char * s;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve font pointer */
      font = hb_parGdFont( 2 );

      /* Retrieve coord value */
      x = hb_parni( 3 );
      y = hb_parni( 4 );

      /* Retrieve color value */
      color = hb_parni( 6 );

      /* Write char */
      if( hb_parclen(5) == 1 )
      {
         /* Retrieve char value */
         c = hb_parni( 5 );
         gdImageCharUp( im, font, x, y, c, color );
      }
      else
      {
         /* Retrieve string value */
         s = ( unsigned char * )hb_parcx( 5 );
         gdImageStringUp( im, font, x, y, s, color );
      }

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 6,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ) );
   }
}

/* ---------------------------------------------------------------------------*/
/* char *gdImageStringFTEx(gdImagePtr im, int *brect, int fg, char *fontname, double ptsize, double angle, int x, int y, char *string, gdFTStringExtraPtr strex) */
/* implementation: cError := gdImageStringFTEx( im, aRect, int fg, cFontname, nPtSize, nAngle, x, y, cString, nLinespacing, nCharmap, nResolution ) */
HB_FUNC( GDIMAGESTRINGFTEX )
{
   /* TraceLog( NULL, "Parameters: %i, Type 1 =%i=\n\r", hb_pcount(), hb_parinfo( 1 ) ); */

   if( hb_pcount() >= 9 &&
       ( HB_ISNIL( 1 ) || HB_ISPOINTER( 1 ) ) &&
       HB_ISARRAY( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISCHAR( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISCHAR( 9 )
     )
   {
      gdImagePtr im;
      gdFTStringExtra extra;
      int fg;
      const char *fontname;
      double ptsize, angle;
      int x, y, i;
      const char *string;
      PHB_ITEM pRect;
      int aRect[8];
      char *err;
      int flags;
      double linespacing;
      int charmap;
      int resolution;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );
      /*TraceLog( NULL, "Image pointer: %p\n\r", im ); */

      /* Retrieve rectangle array */
      pRect = hb_param( 2, HB_IT_ARRAY );
      for( i = 0; i < 8; i ++ )
      {
         aRect[ i ] = hb_arrayGetNI( pRect, i+1 );
      }

      /* Retrieve foreground color value */
      fg = hb_parni( 3 );
      /*TraceLog( NULL, "Forecolor: %i\n\r", fg ); */

      /* Retrieve fontname value */
      fontname = hb_parc( 4 );
      /*TraceLog( NULL, "Font: %s\n\r", fontname ); */

      /* Retrieve point size value */
      ptsize = hb_parni( 5 );
      /*TraceLog( NULL, "PTSize: %lf\n\r", ptsize ); */

      /* Retrieve angle value in radians */
      angle = hb_parnd( 6 );
      /*TraceLog( NULL, "Angle: %lf\n\r", angle ); */

      /* Retrieve pos value */
      x = hb_parni( 7 );
      y = hb_parni( 8 );
      /*TraceLog( NULL, "Pos: %i, %i\n\r", x, y ); */

      /* Retrieve string value */
      string = hb_parcx( 9 );
      /*TraceLog( NULL, "String: %s\n\r", string ); */

      /* EXTENDED FLAGS */
      flags       = 0;

      /* defaults */
      linespacing = 1.05;
      charmap     = gdFTEX_Unicode;
      resolution  = 96;

      /* Retrieve line spacing */
      if( HB_ISNUM( 10 ) )
      {
         linespacing = hb_parnd( 10 );
         flags |= gdFTEX_LINESPACE;
      }

      /* Retrieve charmap */
      if( HB_ISNUM( 11 ) )
      {
         charmap = hb_parni( 11 );
         flags |= gdFTEX_CHARMAP;
      }

      /* Retrieve resolution */
      if( HB_ISNUM( 12 ) )
      {
         resolution = hb_parni( 12 );
         flags |= gdFTEX_RESOLUTION;
      }

      if( !( flags == 0 ) )
      {
         extra.flags       = flags;
         extra.linespacing = ( flags & gdFTEX_LINESPACE  ? linespacing : 1.05 );
         extra.charmap     = ( flags & gdFTEX_CHARMAP    ? charmap : gdFTEX_Unicode );
         extra.hdpi        = ( flags & gdFTEX_RESOLUTION ? resolution : 96 );
         extra.vdpi        = ( flags & gdFTEX_RESOLUTION ? resolution : 96 );
      }

      /* Write string */
      err = gdImageStringFTEx( im, &aRect[0], fg, ( char * ) fontname, ptsize, angle, x, y, ( char * ) string, ( !( flags == 0 ) ? &extra : 0 ));
      if( !( err ) )
      {
         /* Save in array the correct text rectangle dimensions */
         PHB_ITEM pArray;
         pArray = hb_itemArrayNew( 8 );
         for( i = 0; i < 8; i ++ )
         {
            hb_itemPutNI( hb_arrayGetItemPtr( pArray, i+1 ), aRect[i] );
         }
         hb_itemCopy( pRect, pArray );
         hb_itemRelease( pArray );
      }
      hb_retc( err );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 12,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ), hb_paramError( 10 ), hb_paramError( 11 ), hb_paramError( 12 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESTRINGFTCIRCLE ) /* char *gdImageStringFTCircle(gdImagePtr im, int cx, int cy, double radius, double textRadius, double fillPortion, char *font, double points, char *top, char *bottom, int fgcolor) */
{
   /*TraceLog( NULL, "Parameters: %i, Type 9 =%i=\n\r", hb_pcount(), hb_parinfo( 10 ) ); */

   if( hb_pcount() == 11 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISCHAR( 7 ) &&
       HB_ISNUM( 8 ) &&
       ( HB_ISNIL( 9 ) || HB_ISCHAR( 9 ) ) &&
       ( HB_ISNIL( 10 ) || HB_ISCHAR( 10 ) ) &&
       HB_ISNUM( 11 )
     )
   {
      gdImagePtr im;
      int cx, cy;
      double radius, textRadius, fillPortion, points;
      const char *top;
      const char *bottom;
      int fgcolor;
      const char *font;
      char *err;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve pos value */
      cx = hb_parni( 2 );
      cy = hb_parni( 3 );

      /* Retrieve radius value */
      radius = hb_parnd( 4 );

      /* Retrieve textRadius value */
      textRadius = hb_parnd( 5 );

      /* Retrieve textRadius value */
      fillPortion = hb_parnd( 6 );

      /* Retrieve fontname value */
      font = hb_parcx( 7 );

      /* Retrieve points value */
      points = hb_parnd( 8 );

      /* Retrieve top string value */
      top = hb_parcx( 9 );

      /* Retrieve top string value */
      bottom = hb_parcx( 10 );

      /* Retrieve foreground color value */
      fgcolor = hb_parni( 11 );

      /* Write string */
      err = gdImageStringFTCircle( im, cx, cy, radius, textRadius, fillPortion, ( char * ) font, points, ( char * ) top, ( char * ) bottom, fgcolor );
      hb_retc( err );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 11,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ), hb_paramError( 10 ), hb_paramError( 11 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTCACHESETUP ) /* int gdFontCacheSetup (void) */
{
   /* This function initializes the font cache for freetype text output functions */
   hb_retl( gdFontCacheSetup() );
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTCACHESHUTDOWN ) /* void gdFontCacheShutdown (void) */
{
   /* This function initializes the font cache for freetype text output functions */
   gdFontCacheShutdown();
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETWIDTH )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdFontPtr font;

      /* Retrieve font pointer */
      font = hb_parGdFont( 1 );

      /* Return value */
      hb_retni( font->w );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDFONTGETHEIGHT )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
     )
   {
      gdFontPtr font;

      /* Retrieve font pointer */
      font = hb_parGdFont( 1 );

      /* Return value */
      hb_retni( font->h );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* COLOR HANDLING FUNCTIONS                                                   */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORALLOCATE ) /* int gdImageColorAllocate(gdImagePtr im, int r, int g, int b) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Allocate color */
      color = gdImageColorAllocate( im, r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORDEALLOCATE ) /* void gdImageColorDeallocate(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Deallocate color */
      gdImageColorDeallocate( im, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORALLOCATEALPHA ) /* int gdImageColorAllocateAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;
      int a;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Retrieve Alpha value */
      a = hb_parni( 5 );

      /* Allocate color */
      color = gdImageColorAllocateAlpha( im, r, g, b, a);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORCLOSEST ) /* int gdImageColorClosest(gdImagePtr im, int r, int g, int b) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Search color closest */
      color = gdImageColorClosest( im, r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORCLOSESTALPHA ) /* int gdImageColorClosestAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;
      int a;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Retrieve Alpha value */
      a = hb_parni( 5 );

      /* Allocate color */
      color = gdImageColorClosestAlpha( im, r, g, b, a);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORCLOSESTHWB ) /*  gdImageColorClosestHWB(gdImagePtr im, int r, int g, int b) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Search color closest */
      color = gdImageColorClosestHWB( im, r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLOREXACT ) /* int gdImageColorExact(gdImagePtr im, int r, int g, int b) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Search if there is the color */
      color = gdImageColorExact( im, r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORRESOLVE ) /* int gdImageColorResolve(gdImagePtr im, int r, int g, int b) */
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Search if there is the color or similar, if not it creates */
      color = gdImageColorResolve( im, r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORRESOLVEALPHA ) /* int gdImageColorResolveAlpha(gdImagePtr im, int r, int g, int b, int a) */
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
     )
   {
      gdImagePtr im;
      int r, g, b;
      int color;
      int a;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve RGB values */
      r = hb_parni( 2 );
      g = hb_parni( 3 );
      b = hb_parni( 4 );

      /* Retrieve Alpha value */
      a = hb_parni( 5 );

      /* Allocate color */
      color = gdImageColorResolveAlpha( im, r, g, b, a);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 5,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOLORTRANSPARENT ) /* void gdImageColorTransparent(gdImagePtr im, int color) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int color;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve saveFlag value */
      color = hb_parni( 2 );

      /* Set transparent color (to define no transparent color set -1) */
      gdImageColorTransparent( im, color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDTRUECOLOR ) /* int gdTrueColor(int red, int green, int blue) */
{
   if( hb_pcount() == 3 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
     )
   {
      int r, g, b;
      int color;

      /* Retrieve RGB values */
      r = hb_parni( 1 );
      g = hb_parni( 2 );
      b = hb_parni( 3 );

      /* Allocate color */
      color = gdTrueColor(r, g, b);

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDTRUECOLORALPHA ) /* int gdTrueColorAlpha(int red, int green, int blue, int alpha) */
{
   if( hb_pcount() == 4 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      int r, g, b, a;
      int color;

      /* Retrieve RGB values */
      r = hb_parni( 1 );
      g = hb_parni( 2 );
      b = hb_parni( 3 );
      a = hb_parni( 4 );

      /* Allocate color */
      color = gdTrueColorAlpha( r, g, b, a );

      /* return color */
      hb_retni( color );

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 4,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* COPY AND RESIZING FUNCTIONS                                                */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPY ) /* void gdImageCopy(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h) */
{
   if( hb_pcount() == 8 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 )
     )
   {
      gdImagePtr dst, src;
      int dstX, dstY, srcX, srcY, w, h;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parni( 3 );
      dstY = hb_parni( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve width value */
      w = hb_parni( 7 );

      /* Retrieve height value */
      h = hb_parni( 8 );

      /* Perform copy */
      gdImageCopy( dst, src, dstX, dstY, srcX, srcY, w, h );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 8,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPYRESIZED ) /* void gdImageCopyResized(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH, int srcW, int srcH) */
{
   if( hb_pcount() == 10 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) &&
       HB_ISNUM( 10 )
     )
   {
      gdImagePtr dst, src;
      int dstX, dstY, srcX, srcY;
      int dstW, dstH, srcW, srcH;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parni( 3 );
      dstY = hb_parni( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve dest width value */
      dstW = hb_parni( 7 );

      /* Retrieve dest height value */
      dstH = hb_parni( 8 );

      /* Retrieve source width value */
      srcW = hb_parni( 9 );

      /* Retrieve source height value */
      srcH = hb_parni( 10 );

      /* Perform copy */
      gdImageCopyResized( dst, src, dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 10,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ), hb_paramError( 10 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPYRESAMPLED ) /* void gdImageCopyResampled(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int dstW, int dstH, int srcW, int srcH) */
{
   if( hb_pcount() == 10 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 ) &&
       HB_ISNUM( 10 )
     )
   {
      gdImagePtr dst, src;
      int dstX, dstY, srcX, srcY;
      int dstW, dstH, srcW, srcH;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parni( 3 );
      dstY = hb_parni( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve dest width value */
      dstW = hb_parni( 7 );

      /* Retrieve dest height value */
      dstH = hb_parni( 8 );

      /* Retrieve source width value */
      srcW = hb_parni( 9 );

      /* Retrieve source height value */
      srcH = hb_parni( 10 );

      /* Perform copy */
      gdImageCopyResampled( dst, src, dstX, dstY, srcX, srcY, dstW, dstH, srcW, srcH );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 10,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ), hb_paramError( 10 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPYROTATED ) /* void gdImageCopyRotated(gdImagePtr dst, gdImagePtr src, double dstX, double dstY, int srcX, int srcY, int srcW, int srcH, int angle) */
{
   if( hb_pcount() == 9 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 )
     )
   {
      gdImagePtr dst, src;
      double dstX, dstY;
      int srcX, srcY, srcW, srcH, angle;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parnd( 3 );
      dstY = hb_parnd( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve source width value */
      srcW = hb_parni( 7 );

      /* Retrieve source height value */
      srcH = hb_parni( 8 );

      /* Retrieve angle value */
      angle = hb_parni( 9 );

      /* Perform rotation */
      gdImageCopyRotated( dst, src, dstX, dstY, srcX, srcY, srcW, srcH, angle );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 9,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPYMERGE ) /* void gdImageCopyMerge(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int pct) */
{
   if( hb_pcount() == 9 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 )
     )
   {
      gdImagePtr dst, src;
      int dstX, dstY, srcX, srcY, w, h, pct;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parni( 3 );
      dstY = hb_parni( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve width value */
      w = hb_parni( 7 );

      /* Retrieve height value */
      h = hb_parni( 8 );

      /* Retrieve percentual value */
      pct = hb_parni( 9 );

      /* Perform copy */
      gdImageCopyMerge( dst, src, dstX, dstY, srcX, srcY, w, h, pct );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 9,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOPYMERGEGRAY ) /* void gdImageCopyMergeGray(gdImagePtr dst, gdImagePtr src, int dstX, int dstY, int srcX, int srcY, int w, int h, int pct) */
{
   if( hb_pcount() == 9 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       HB_ISNUM( 8 ) &&
       HB_ISNUM( 9 )
     )
   {
      gdImagePtr dst, src;
      int dstX, dstY, srcX, srcY, w, h, pct;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Retrieve destination pos value */
      dstX = hb_parni( 3 );
      dstY = hb_parni( 4 );

      /* Retrieve source pos value */
      srcX = hb_parni( 5 );
      srcY = hb_parni( 6 );

      /* Retrieve width value */
      w = hb_parni( 7 );

      /* Retrieve height value */
      h = hb_parni( 8 );

      /* Retrieve percentual value */
      pct = hb_parni( 9 );

      /* Perform copy */
      gdImageCopyMergeGray( dst, src, dstX, dstY, srcX, srcY, w, h, pct );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 9,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ),
         hb_paramError( 9 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEPALETTECOPY ) /* void gdImagePaletteCopy(gdImagePtr dst, gdImagePtr src) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 )
     )
   {
      gdImagePtr dst, src;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Perform copy */
      gdImagePaletteCopy(dst, src);
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESQUARETOCIRCLE ) /* void gdImageSquareToCircle(gdImagePtr im, int radius) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int radius;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve radius value */
      radius = hb_parni( 2 );

      hb_retGdImage( gdImageSquareToCircle( im, radius ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGESHARPEN ) /* void gdImageSharpen(gdImagePtr im, int pct) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
     )
   {
      gdImagePtr im;
      int pct;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve percentual value */
      pct = hb_parni( 2 );

      /* Sharpens the specified image */
      gdImageSharpen( im, pct);
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

/* ---------------------------------------------------------------------------*/
/* MISCELLANEOUS FUNCTIONS                                                    */
/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGECOMPARE ) /* int gdImageCompare(gdImagePtr im1, gdImagePtr im2) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 )
     )
   {
      gdImagePtr dst, src;

      /* Retrieve destination image pointer */
      dst = hb_parGdImage( 1 );

      /* Retrieve source image pointer */
      src = hb_parGdImage( 2 );

      /* Compare images - if return <> 0 check value for infos */
      hb_retni( gdImageCompare(dst, src) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

HB_FUNC( GDIMAGEINTERLACE ) /* void gdImageInterlace(gdImagePtr im, int interlace) */
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 )
     )
   {
      gdImagePtr im;
      int interlace;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve interlace value */
      interlace = hb_parl( 2 );

      /* Set interlace */
      gdImageInterlace( im, interlace);

   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 2,
         hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ---------------------------------------------------------------------------*/

#if HB_GD_VERS( 2, 0, 33 )

static void AddImageToFile( const char *szFile, const void * iptr, int sz )
{
   HB_FHANDLE fhandle;

   if( ( fhandle = hb_fsOpen( szFile, FO_READWRITE ) ) != FS_ERROR )
   {
      /* move to end of file */
      hb_fsSeek( fhandle, 0, FS_END );

      /* Write Image */
      SaveImageToHandle( fhandle, iptr, sz );

      /* Close file */
      hb_fsClose( fhandle );
   }
}

#endif

/*BGD_DECLARE(void *) gdImageGifAnimBeginPtr(gdImagePtr im, int *size, int GlobalCM, int Loops); */
/* implementation: (void *) gdImageGifAnimBegin( gdImagePtr im, cFile | nHandle, int GlobalCM, int Loops); */
HB_FUNC( GDIMAGEGIFANIMBEGIN )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
        ( HB_ISCHAR( 2 ) || HB_ISNUM( 2 ) || HB_ISNIL( 2 ) ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
     )
   {
      gdImagePtr im;
      void * iptr;
      int size;
      int GlobalCM, Loops;

      /* Retrieve image pointer */
      im = hb_parGdImage( 1 );

      /* Retrieve global color map value */
      GlobalCM = hb_parni( 3 );

      /* Retrieve Loops value */
      Loops = hb_parni( 4 );

      /* run function */
      iptr = gdImageGifAnimBeginPtr( im, &size, GlobalCM, Loops);

      /* Check if 2nd parameter is a file name or an handle */
      if( HB_ISCHAR( 2 ) )
      {
         SaveImageToFile( hb_parcx( 2 ), iptr, size );
      }
      else if( HB_ISNUM( 2 ) || HB_ISNIL( 2 ) )
      {
         /* Retrieve file handle */
         HB_FHANDLE fhandle = HB_ISNUM( 2 ) ? hb_numToHandle( hb_parnint( 2 ) ) : 1; /* 1 = std output */

         SaveImageToHandle( fhandle, iptr, size );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 3,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
#endif
}

/* ---------------------------------------------------------------------------*/

/*BGD_DECLARE(void *) gdImageGifAnimAddPtr(gdImagePtr im, int *size, int LocalCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImagePtr previm); */
/* implementation: (void *) gdImageGifAnimAdd( gdImagePtr im, cFile | nHandle, int LocalCM, int LeftOfs, int TopOfs, int Delay, int Disposal, gdImagePtr previm); */
HB_FUNC( GDIMAGEGIFANIMADD )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_pcount() == 8 &&
       HB_ISPOINTER( 1 ) &&
       ( HB_ISCHAR( 2 ) || HB_ISNUM( 2 ) || HB_ISNIL( 2 ) ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISNUM( 7 ) &&
       ( HB_ISPOINTER( 8 ) || HB_ISNIL( 8 ) )
     )
   {
      gdImagePtr im, previm;
      void * iptr;
      int size;
      int LocalCM, LeftOfs, TopOfs, Delay, Disposal;

      /* Retrieve parameters */
      im       = hb_parGdImage( 1 );

      LocalCM  = hb_parni( 3 );
      LeftOfs  = hb_parni( 4 );
      TopOfs   = hb_parni( 5 );
      Delay    = hb_parni( 6 );
      Disposal = hb_parni( 7 );
      previm   = hb_parGdImage( 8 );

      /* Run function and return value */
      iptr = gdImageGifAnimAddPtr( im, &size, LocalCM, LeftOfs, TopOfs, Delay, Disposal, previm );

      /* Check if 2nd parameter is a file name or an handle */
      if( HB_ISCHAR( 2 ) )
      {
         AddImageToFile( hb_parcx( 2 ), iptr, size );
      }
      else if( HB_ISNUM( 2 ) || HB_ISNIL( 2 ) )
      {
         /* Retrieve file handle */
         HB_FHANDLE fhandle = HB_ISNUM( 2 ) ? hb_numToHandle( hb_parnint( 2 ) ) : 1; /* 1 = std output */

         SaveImageToHandle( fhandle, iptr, size );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 8,
         hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
         hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 ), hb_paramError( 8 ) );
   }
#endif
}

/* ---------------------------------------------------------------------------*/

/*BGD_DECLARE(void *) gdImageGifAnimEndPtr(int *size); */
/* implementation: gdImageGifAnimEnd( cFile | nHandle ); */
HB_FUNC( GDIMAGEGIFANIMEND )
{
#if HB_GD_VERS( 2, 0, 33 )
   if( hb_pcount() == 1 &&
       ( HB_ISCHAR( 1 ) || HB_ISNUM( 1 ) || HB_ISNIL( 1 ) )
     )
   {
      void * iptr;
      int size;

      /* Run function and return value */
      iptr = gdImageGifAnimEndPtr( &size );

      /* Check if 1st parameter is a file name or an handle */
      if( HB_ISCHAR( 1 ) )
      {
         AddImageToFile( hb_parcx( 1 ), iptr, size );
      }
      else if( HB_ISNUM( 2 ) || HB_ISNIL( 2 ) )
      {
         /* Retrieve file handle */
         HB_FHANDLE fhandle = HB_ISNUM( 1 ) ? hb_numToHandle( hb_parnint( 1 ) ) : 1; /* 1 = std output */

         SaveImageToHandle( fhandle, iptr, size );
      }
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
         HB_ERR_FUNCNAME, 1,
         hb_paramError( 1 ) );
   }
#endif
}

/* ---------------------------------------------------------------------------*/

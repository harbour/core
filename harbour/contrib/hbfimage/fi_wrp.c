/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * FreeImage graphic library low level (client api) interface code.
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
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#  if ! defined( _WINDOWS_ )
#     define _WINDOWS_
#  endif
#endif

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "FreeImage.h"

#define hb_fi_retl( x )  hb_retl( x ? HB_TRUE : HB_FALSE )
#define hb_fi_parl( x )  ( hb_parl( x ) ? TRUE : FALSE )

/* ************************* WRAPPED FUNCTIONS ****************************** */

/* static for error handler (see below FI_SETOUTPUTMESSAGE ) */
static void * s_pErrorHandler = NULL;

/* Init / Error routines */
/* --------------------- */

/* DLL_API void DLL_CALLCONV FreeImage_Initialise(BOOL load_local_plugins_only FI_DEFAULT(FALSE)); */
HB_FUNC( FI_INITIALISE )
{
   /* Run function */
   FreeImage_Initialise( hb_fi_parl( 1 ) );
}

/* DLL_API void DLL_CALLCONV FreeImage_DeInitialise(void); */
HB_FUNC( FI_DEINITIALISE )
{
   /* Run function */
   FreeImage_DeInitialise();
}

/* Version routines */
/* ---------------- */

/* DLL_API const char *DLL_CALLCONV FreeImage_GetVersion(void); */
HB_FUNC( FI_GETVERSION )
{
   /* Run function & return value */
   hb_retc( FreeImage_GetVersion() );
}

/* DLL_API const char *DLL_CALLCONV FreeImage_GetCopyrightMessage(void); */
HB_FUNC( FI_GETCOPYRIGHTMESSAGE )
{
   /* Run function & return value */
   hb_retc( FreeImage_GetCopyrightMessage() );
}

/* Message output functions */
/* ------------------------ */

/* DLL_API void DLL_CALLCONV FreeImage_OutputMessageProc(int fif, const char *fmt, ...); */

/* typedef void (*FreeImage_OutputMessageFunction)(FREE_IMAGE_FORMAT fif, const char *msg); */
/* DLL_API void DLL_CALLCONV FreeImage_SetOutputMessage(FreeImage_OutputMessageFunction omf); */

/* implementation: void FreeImage_SetOutputMessage( pFunctionPointer ) */

/**
   FreeImage error handler
   @param fif Format / Plugin responsible for the error
   @param message Error message
 */
static void FreeImageErrorHandler( FREE_IMAGE_FORMAT fif, const char * message )
{
   if( s_pErrorHandler )
   {
      /*TraceLog( NULL, "ErrorHandle %p\n\r", s_pErrorHandler );*/

      if( hb_vmRequestReenter() )
      {
         const char * format = FreeImage_GetFormatFromFIF( fif );

         /* launch error function at prg level */
         hb_vmPushSymbol( ( PHB_SYMB ) s_pErrorHandler );
         hb_vmPushNil();
         hb_vmPushString( format, strlen( format ) );
         hb_vmPushString( message, strlen( message ) );
         hb_vmDo( 2 );

         hb_vmRequestRestore();
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 0, NULL, "FreeImageErrorHandler", 1, hb_paramError( 1 ) );
   }
}

HB_FUNC( FI_SETOUTPUTMESSAGE )
{
   s_pErrorHandler = NULL;
   FreeImage_SetOutputMessage( FreeImageErrorHandler );

/* TraceLog( NULL, "PCount = %i\n\r", hb_pcount() ); */

   if( hb_pcount() == 1 )
   {
      if( HB_ISPOINTER( 1 ) )
      {
         /* Set the pointer */
         s_pErrorHandler = hb_parptr( 1 );
      }
      else if( HB_ISNIL( 1 ) )
      {
         /* do nothing */
      }
      else
      {
         hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                               HB_ERR_FUNCNAME, 1,
                               hb_paramError( 1 )
                               );
      }
   }
}

/* Allocate / Clone / Unload routines */
/* ---------------------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Allocate(int width, int height, int bpp, unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0)); */
HB_FUNC( FI_ALLOCATE )
{
   if( hb_pcount() >= 3 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      int      width, height, bpp;
      unsigned red_mask, green_mask, blue_mask;

      /* Retrieve parameters */
      width      = hb_parni( 1 );
      height     = hb_parni( 2 );
      bpp        = hb_parni( 3 );
      red_mask   = ( unsigned ) hb_parni( 4 );
      green_mask = ( unsigned ) hb_parni( 5 );
      blue_mask  = ( unsigned ) hb_parni( 6 );

      /* run function & return value */
      hb_retptr( FreeImage_Allocate( width, height, bpp, red_mask, green_mask, blue_mask ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_AllocateT(FREE_IMAGE_TYPE type, int width, int height, int bpp FI_DEFAULT(8), unsigned red_mask FI_DEFAULT(0), unsigned green_mask FI_DEFAULT(0), unsigned blue_mask FI_DEFAULT(0)); */
HB_FUNC( FI_ALLOCATET )
{
   if( hb_pcount() >= 3 &&
       HB_ISNUM( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FREE_IMAGE_TYPE type;
      int      width, height, bpp;
      unsigned red_mask, green_mask, blue_mask;

      /* Retrieve parameters */
      type       = ( FREE_IMAGE_TYPE ) hb_parni( 1 );
      width      = hb_parni( 2 );
      height     = hb_parni( 3 );
      bpp        = hb_parni( 3 );
      red_mask   = ( unsigned ) hb_parni( 4 );
      green_mask = ( unsigned ) hb_parni( 5 );
      blue_mask  = ( unsigned ) hb_parni( 6 );

      /* run function & return value */
      hb_retptr( FreeImage_AllocateT( type, width, height, bpp, red_mask, green_mask, blue_mask ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 )
                            );
   }
}

/* DLL_API FIBITMAP * DLL_CALLCONV FreeImage_Clone(FIBITMAP *dib); */
HB_FUNC( FI_CLONE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;
      FIBITMAP * fiClonePtr;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function */
      fiClonePtr = FreeImage_Clone( dib );

      /* return value */
      if( fiClonePtr )
         hb_retptr( fiClonePtr );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_Unload(FIBITMAP *dib); */
HB_FUNC( FI_UNLOAD )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function */
      FreeImage_Unload( dib );

      /* return value */
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* Load / Save routines */
/* -------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromMemory(FREE_IMAGE_FORMAT fif, FIMEMORY *stream, int flags FI_DEFAULT(0)); */
/* DLL_API FIMEMORY *DLL_CALLCONV FreeImage_OpenMemory(BYTE *data FI_DEFAULT(0), DWORD size_in_bytes FI_DEFAULT(0)); */
/* DLL_API void DLL_CALLCONV FreeImage_CloseMemory(FIMEMORY *stream); */
HB_FUNC( FI_LOADFROMMEMORY )
{
   if( hb_pcount() == 3 &&
       HB_ISNUM( 1 ) &&
       HB_ISCHAR( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FIBITMAP *        dib;
      FREE_IMAGE_FORMAT fif;
      FIMEMORY *        stream;
      const char *      szImage;
      int flags;

      /* Retrieve parameters */
      fif     = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
      szImage = hb_parcx( 2 );
      flags   = hb_parni( 3 );

      /* run function */
      stream = FreeImage_OpenMemory( ( BYTE * ) szImage, ( DWORD ) hb_parclen( 2 ) );
      dib    = FreeImage_LoadFromMemory( fif, stream, flags );
      FreeImage_CloseMemory( stream );

      /* return value */
      if( dib )
         hb_retptr( dib );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Load(FREE_IMAGE_FORMAT fif, const char *filename, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_LOAD )
{
   if( hb_pcount() == 3 &&
       HB_ISNUM( 1 ) &&
       HB_ISCHAR( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FIBITMAP *        dib;
      FREE_IMAGE_FORMAT fif;
      const char *      filename;
      int flags;

      /* Retrieve parameters */
      fif      = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
      filename = hb_parcx( 2 );
      flags    = hb_parni( 3 );

      /* run function */
      dib = FreeImage_Load( fif, filename, flags );

      /* return value */
      if( dib )
         hb_retptr( dib );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadU(FREE_IMAGE_FORMAT fif, const wchar_t *filename, int flags FI_DEFAULT(0)); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromHandle(FREE_IMAGE_FORMAT fif, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0)); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_Save(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const char *filename, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_SAVE )
{
   if( hb_pcount() == 4 &&
       HB_ISNUM( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISCHAR( 3 ) &&
       HB_ISNUM( 4 )
       )
   {
      FREE_IMAGE_FORMAT fif;
      FIBITMAP *        dib;
      const char *      filename;
      int flags;

      /* Retrieve parameters */
      fif      = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
      dib      = ( FIBITMAP * ) hb_parptr( 2 );
      filename = hb_parcx( 3 );
      flags    = hb_parni( 4 );

      /* run function & return value */
      hb_fi_retl( FreeImage_Save( fif, dib, filename, flags ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SaveU(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, const wchar_t *filename, int flags FI_DEFAULT(0)); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SaveToHandle(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FreeImageIO *io, fi_handle handle, int flags FI_DEFAULT(0)); */

/* Memory I/O stream routines */
/* -------------------------- */

/*
   DLL_API FIMEMORY *DLL_CALLCONV FreeImage_OpenMemory(BYTE *data FI_DEFAULT(0), DWORD size_in_bytes FI_DEFAULT(0));
   DLL_API void DLL_CALLCONV FreeImage_CloseMemory(FIMEMORY *stream);
   DLL_API FIBITMAP *DLL_CALLCONV FreeImage_LoadFromMemory(FREE_IMAGE_FORMAT fif, FIMEMORY *stream, int flags FI_DEFAULT(0));
   DLL_API BOOL DLL_CALLCONV FreeImage_SaveToMemory(FREE_IMAGE_FORMAT fif, FIBITMAP *dib, FIMEMORY *stream, int flags FI_DEFAULT(0));
   DLL_API long DLL_CALLCONV FreeImage_TellMemory(FIMEMORY *stream);
   DLL_API BOOL DLL_CALLCONV FreeImage_SeekMemory(FIMEMORY *stream, long offset, int origin);
   DLL_API BOOL DLL_CALLCONV FreeImage_AcquireMemory(FIMEMORY *stream, BYTE **data, DWORD *size_in_bytes);
 */

/* Plugin Interface */
/* ---------------- */

/*
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterLocalPlugin(FI_InitProc proc_address, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_RegisterExternalPlugin(const char *path, const char *format FI_DEFAULT(0), const char *description FI_DEFAULT(0), const char *extension FI_DEFAULT(0), const char *regexpr FI_DEFAULT(0));
   DLL_API int DLL_CALLCONV FreeImage_GetFIFCount(void);
   DLL_API int DLL_CALLCONV FreeImage_SetPluginEnabled(FREE_IMAGE_FORMAT fif, BOOL enable);
   DLL_API int DLL_CALLCONV FreeImage_IsPluginEnabled(FREE_IMAGE_FORMAT fif);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFormat(const char *format);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromMime(const char *mime);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFormatFromFIF(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFExtensionList(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFDescription(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFRegExpr(FREE_IMAGE_FORMAT fif);
   DLL_API const char *DLL_CALLCONV FreeImage_GetFIFMimeType(FREE_IMAGE_FORMAT fif);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilename(const char *filename);
   DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFIFFromFilenameU(const wchar_t *filename);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsReading(FREE_IMAGE_FORMAT fif);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsWriting(FREE_IMAGE_FORMAT fif);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportBPP(FREE_IMAGE_FORMAT fif, int bpp);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsExportType(FREE_IMAGE_FORMAT fif, FREE_IMAGE_TYPE type);
   DLL_API BOOL DLL_CALLCONV FreeImage_FIFSupportsICCProfiles(FREE_IMAGE_FORMAT fif);
 */

/* Multipaging interface */
/* --------------------- */

/* DLL_API FIMULTIBITMAP * DLL_CALLCONV FreeImage_OpenMultiBitmap(FREE_IMAGE_FORMAT fif, const char *filename, BOOL create_new, BOOL read_only, BOOL keep_cache_in_memory FI_DEFAULT(FALSE), int flags FI_DEFAULT(0)); */
HB_FUNC( FI_OPENMULTIBITMAP )
{
   if( hb_pcount() >= 4 &&
       HB_ISNUM( 1 ) &&
       HB_ISCHAR( 2 ) &&
       HB_ISLOG( 3 ) &&
       HB_ISLOG( 4 )
       )
   {
      FIMULTIBITMAP *   dib;
      FREE_IMAGE_FORMAT fif;
      const char *      filename;
      BOOL create_new;
      BOOL read_only;
      BOOL keep_cache_in_memory;
      int  flags;

      /* Retrieve parameters */
      fif        = ( FREE_IMAGE_FORMAT ) hb_parni( 1 );
      filename   = hb_parcx( 2 );
      create_new = hb_fi_parl( 3 );
      read_only  = hb_fi_parl( 4 );
      keep_cache_in_memory = hb_fi_parl( 5 );
      flags = hb_parni( 6 );

      /* run function */
      dib = FreeImage_OpenMultiBitmap( fif, filename, create_new, read_only, keep_cache_in_memory, flags );

      /* return value */
      if( dib )
         hb_retptr( dib );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_CloseMultiBitmap(FIMULTIBITMAP *bitmap, int flags FI_DEFAULT(0)); */
HB_FUNC( FI_CLOSEMULTIBITMAP )
{
   if( hb_pcount() >= 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIMULTIBITMAP * bitmap;
      int flags;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      flags  = hb_parni( 2 );

      /* run function & return value */
      hb_fi_retl( FreeImage_CloseMultiBitmap( bitmap, flags ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API int DLL_CALLCONV FreeImage_GetPageCount(FIMULTIBITMAP *bitmap); */
HB_FUNC( FI_GETPAGECOUNT )
{
   if( hb_pcount() >= 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIMULTIBITMAP * bitmap;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetPageCount( bitmap ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_AppendPage(FIMULTIBITMAP *bitmap, FIBITMAP *data); */
HB_FUNC( FI_APPENDPAGE )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 )
       )
   {
      FIMULTIBITMAP * bitmap;
      FIBITMAP *      data;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      data   = ( FIBITMAP * ) hb_parptr( 2 );

      /* run function & return value */
      FreeImage_AppendPage( bitmap, data );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_InsertPage(FIMULTIBITMAP *bitmap, int page, FIBITMAP *data); */
HB_FUNC( FI_INSERTPAGE )
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISPOINTER( 3 )
       )
   {
      FIMULTIBITMAP * bitmap;
      int        page;
      FIBITMAP * data;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      page   = hb_parni( 2 ) - 1; /* 0-based index */
      data   = ( FIBITMAP * ) hb_parptr( 3 );

      /* run function & return value */
      FreeImage_InsertPage( bitmap, page, data );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_DeletePage(FIMULTIBITMAP *bitmap, int page); */
HB_FUNC( FI_DELETEPAGE )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIMULTIBITMAP * bitmap;
      int page;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      page   = hb_parni( 2 ) - 1; /* 0-based index */

      /* run function & return value */
      FreeImage_DeletePage( bitmap, page );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API FIBITMAP * DLL_CALLCONV FreeImage_LockPage(FIMULTIBITMAP *bitmap, int page); */
HB_FUNC( FI_LOCKPAGE )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIMULTIBITMAP * bitmap;
      int page;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      page   = hb_parni( 2 ) - 1; /* 0-based index */

      /* run function & return value */
      hb_retptr( FreeImage_LockPage( bitmap, page ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_UnlockPage(FIMULTIBITMAP *bitmap, FIBITMAP *page, BOOL changed); */
HB_FUNC( FI_UNLOCKPAGE )
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISLOG( 3 )
       )
   {
      FIMULTIBITMAP * bitmap;
      FIBITMAP *      page;
      BOOL changed;

      /* Retrieve parameters */
      bitmap  = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      page    = ( FIBITMAP * ) hb_parptr( 2 );
      changed = hb_fi_parl( 3 );

      /* run function & return value */
      FreeImage_UnlockPage( bitmap, page, changed );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_MovePage(FIMULTIBITMAP *bitmap, int target, int source); */
HB_FUNC( FI_MOVEPAGE )
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FIMULTIBITMAP * bitmap;
      int target, source;

      /* Retrieve parameters */
      bitmap = ( FIMULTIBITMAP * ) hb_parptr( 1 );
      target = hb_parni( 2 );
      source = hb_parni( 3 );

      /* run function & return value */
      hb_fi_retl( FreeImage_MovePage( bitmap, target, source ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetLockedPageNumbers(FIMULTIBITMAP *bitmap, int *pages, int *count); */

/* Filetype request routines */
/* ------------------------- */

/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileType(const char *filename, int size FI_DEFAULT(0)); */
HB_FUNC( FI_GETFILETYPE )
{
   if( hb_pcount() >= 1 &&
       HB_ISCHAR( 1 )
       )
   {
      const char * filename;
      int          size;

      /* Retrieve parameters */
      filename = hb_parcx( 1 );
      size     = ( int ) hb_parclen( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetFileType( filename, size ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeU(const wchar_t *filename, int size FI_DEFAULT(0)); */
/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromHandle(FreeImageIO *io, fi_handle handle, int size FI_DEFAULT(0)); */
/* DLL_API FREE_IMAGE_FORMAT DLL_CALLCONV FreeImage_GetFileTypeFromMemory(FIMEMORY *stream, int size FI_DEFAULT(0)); */
HB_FUNC( FI_GETFILETYPEFROMMEMORY )
{
   if( hb_pcount() >= 1 &&
       HB_ISCHAR( 1 )
       )
   {
      const char * szImage;
      FIMEMORY *   stream;
      int          size;

      /* Retrieve parameters */
      szImage = hb_parcx( 1 );
      stream  = FreeImage_OpenMemory( ( BYTE * ) szImage, ( int ) hb_parclen( 1 ) );
      size    = hb_parni( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetFileTypeFromMemory( stream, size ) );
      FreeImage_CloseMemory( stream );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* Image type request routine */
/* -------------------------- */

/* DLL_API FREE_IMAGE_TYPE DLL_CALLCONV FreeImage_GetImageType(FIBITMAP *dib); */
HB_FUNC( FI_GETIMAGETYPE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetImageType( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* FreeImage helper routines */
/* ------------------------- */

/* DLL_API BOOL DLL_CALLCONV FreeImage_IsLittleEndian(void); */
HB_FUNC( FI_ISLITTLEENDIAN )
{
   /* run function & return value */
   hb_fi_retl( FreeImage_IsLittleEndian() );
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_LookupX11Color(const char *szColor, BYTE *nRed, BYTE *nGreen, BYTE *nBlue); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_LookupSVGColor(const char *szColor, BYTE *nRed, BYTE *nGreen, BYTE *nBlue); */


/* Pixel access routines */
/* --------------------- */

/* DLL_API BYTE *DLL_CALLCONV FreeImage_GetBits(FIBITMAP *dib); */
HB_FUNC( FI_GETBITS )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_GetBits( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BYTE *DLL_CALLCONV FreeImage_GetScanLine(FIBITMAP *dib, int scanline); */
HB_FUNC( FI_GETSCANLINE )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      int        scanline;

      /* Retrieve parameters */
      dib      = ( FIBITMAP * ) hb_parptr( 1 );
      scanline = hb_parni( 2 );

      /* run function & return value */
      hb_retptr( FreeImage_GetScanLine( dib, scanline ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/*
   DLL_API BOOL DLL_CALLCONV FreeImage_GetPixelIndex(FIBITMAP *dib, unsigned x, unsigned y, BYTE *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_GetPixelColor(FIBITMAP *dib, unsigned x, unsigned y, RGBQUAD *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_SetPixelIndex(FIBITMAP *dib, unsigned x, unsigned y, BYTE *value);
   DLL_API BOOL DLL_CALLCONV FreeImage_SetPixelColor(FIBITMAP *dib, unsigned x, unsigned y, RGBQUAD *value);
 */

/* DIB info routines */
/* ----------------- */

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetColorsUsed(FIBITMAP *dib); */
HB_FUNC( FI_GETCOLORSUSED )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetColorsUsed( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetBPP(FIBITMAP *dib); */
HB_FUNC( FI_GETBPP )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetBPP( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetWidth(FIBITMAP *dib); */
HB_FUNC( FI_GETWIDTH )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetWidth( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetHeight(FIBITMAP *dib); */
HB_FUNC( FI_GETHEIGHT )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetHeight( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetLine(FIBITMAP *dib); */
HB_FUNC( FI_GETLINE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetLine( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetPitch(FIBITMAP *dib); */
HB_FUNC( FI_GETPITCH )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetPitch( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetDIBSize(FIBITMAP *dib); */
HB_FUNC( FI_GETDIBSIZE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetDIBSize( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API RGBQUAD *DLL_CALLCONV FreeImage_GetPalette(FIBITMAP *dib); */
HB_FUNC( FI_GETPALETTE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_GetPalette( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterX(FIBITMAP *dib); */
HB_FUNC( FI_GETDOTSPERMETERX )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetDotsPerMeterX( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetDotsPerMeterY(FIBITMAP *dib); */
HB_FUNC( FI_GETDOTSPERMETERY )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retnl( FreeImage_GetDotsPerMeterY( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_SetDotsPerMeterX(FIBITMAP *dib, unsigned res); */
HB_FUNC( FI_SETDOTSPERMETERX )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      unsigned   res;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );
      res = ( unsigned ) hb_parni( 2 );

      /* run function & return value */
      FreeImage_SetDotsPerMeterX( dib, res );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_SetDotsPerMeterY(FIBITMAP *dib, unsigned res); */
HB_FUNC( FI_SETDOTSPERMETERY )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      unsigned   res;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );
      res = ( unsigned ) hb_parni( 2 );

      /* run function & return value */
      FreeImage_SetDotsPerMeterY( dib, res );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API BITMAPINFOHEADER *DLL_CALLCONV FreeImage_GetInfoHeader(FIBITMAP *dib); */
HB_FUNC( FI_GETINFOHEADER )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;
      /* We need not worry about Memory Management - will be automatically released! */
      /*BITMAPINFOHEADER * bmpinfoheader = (BITMAPINFOHEADER *) hb_xgrab( sizeof( BITMAPINFOHEADER ) );*/
      BITMAPINFOHEADER * bmpinfoheader;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      bmpinfoheader = FreeImage_GetInfoHeader( dib );

      /*hb_retclenAdoptRaw( (char *) bmpinfoheader, sizeof( BITMAPINFOHEADER ) );*/
      hb_retptr( bmpinfoheader );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BITMAPINFO *DLL_CALLCONV FreeImage_GetInfo(FIBITMAP *dib); */
HB_FUNC( FI_GETINFO )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP *   dib;
      BITMAPINFO * bmpinfo;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      bmpinfo = FreeImage_GetInfo( dib );

      hb_retptr( bmpinfo );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API FREE_IMAGE_COLOR_TYPE DLL_CALLCONV FreeImage_GetColorType(FIBITMAP *dib); */
HB_FUNC( FI_GETCOLORTYPE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetColorType( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetRedMask(FIBITMAP *dib); */
HB_FUNC( FI_GETREDMASK )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetRedMask( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetGreenMask(FIBITMAP *dib); */
HB_FUNC( FI_GETGREENMASK )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetGreenMask( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetBlueMask(FIBITMAP *dib); */
HB_FUNC( FI_GETBLUEMASK )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetBlueMask( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API unsigned DLL_CALLCONV FreeImage_GetTransparencyCount(FIBITMAP *dib); */
HB_FUNC( FI_GETTRANSPARENCYCOUNT )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retni( FreeImage_GetTransparencyCount( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BYTE * DLL_CALLCONV FreeImage_GetTransparencyTable(FIBITMAP *dib); */
HB_FUNC( FI_GETTRANSPARENCYTABLE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_GetTransparencyTable( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_SetTransparent(FIBITMAP *dib, BOOL enabled); */
HB_FUNC( FI_SETTRANSPARENT )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISLOG( 2 )
       )
   {
      FIBITMAP * dib;
      BOOL       enabled;

      /* Retrieve parameters */
      dib     = ( FIBITMAP * ) hb_parptr( 1 );
      enabled = hb_fi_parl( 2 );

      /* run function & return value */
      FreeImage_SetTransparent( dib, enabled );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_SetTransparencyTable(FIBITMAP *dib, BYTE *table, int count); */
HB_FUNC( FI_SETTRANSPARENCYTABLE )
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FIBITMAP * dib;
      BYTE *     table;
      int        count;

      /* Retrieve parameters */
      dib   = ( FIBITMAP * ) hb_parptr( 1 );
      table = ( BYTE * ) hb_parptr( 2 );
      count = hb_parni( 3 );

      /* run function & return value */
      FreeImage_SetTransparencyTable( dib, table, count );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_IsTransparent(FIBITMAP *dib); */
HB_FUNC( FI_ISTRANSPARENT )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_fi_retl( FreeImage_IsTransparent( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_HasBackgroundColor(FIBITMAP *dib); */
HB_FUNC( FI_HASBACKGROUNDCOLOR )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_fi_retl( FreeImage_HasBackgroundColor( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetBackgroundColor(FIBITMAP *dib, RGBQUAD *bkcolor); */
HB_FUNC( FI_GETBACKGROUNDCOLOR )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 ) /*&& HB_ISCHAR( 2 )*/
       )
   {
      FIBITMAP * dib;
      RGBQUAD *  bkcolor = NULL;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );
      /*bkcolor = ( RGBQUAD * ) hb_param( 2, HB_IT_STRING )->item.asString.value;*/
      /*bkcolor = ( RGBQUAD * ) hb_parptr( 2 );*/

      /* run function & return value */
      /*hb_fi_retl( FreeImage_GetBackgroundColor(dib, bkcolor) );*/
      FreeImage_GetBackgroundColor( dib, bkcolor );
      /*hb_storptr( bkcolor, 2 );*/
      hb_retptr( bkcolor );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetBackgroundColor(FIBITMAP *dib, RGBQUAD *bkcolor); */
HB_FUNC( FI_SETBACKGROUNDCOLOR )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       /*HB_ISPOINTER( 2 )*/
       HB_ISCHAR( 2 )
       )
   {
      FIBITMAP * dib;
      RGBQUAD *  bkcolor;

      /* Retrieve parameters */
      dib     = ( FIBITMAP * ) hb_parptr( 1 );
      bkcolor = ( RGBQUAD * ) hb_itemGetCPtr( hb_param( 2, HB_IT_STRING ) );
      /*bkcolor = ( RGBQUAD * ) hb_parptr( 2 );*/

      /* run function & return value */
      hb_fi_retl( FreeImage_SetBackgroundColor( dib, bkcolor ) );
      /* FreeImage_GetBackgroundColor(dib, bkcolor); */
      /* hb_retptr( bkcolor ); */
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* ICC profile routines */
/* -------------------- */

/* DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_GetICCProfile(FIBITMAP *dib); */
HB_FUNC( FI_GETICCPROFILE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_GetICCProfile( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API FIICCPROFILE *DLL_CALLCONV FreeImage_CreateICCProfile(FIBITMAP *dib, void *data, long size); */
HB_FUNC( FI_CREATEICCPROFILE )
{
   if( hb_pcount() == 3 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 )
       )
   {
      FIBITMAP * dib;
      void *     data;
      long       size;

      /* Retrieve parameters */
      dib  = ( FIBITMAP * ) hb_parptr( 1 );
      data = hb_parptr( 2 );
      size = hb_parnl( 3 );

      /* run function & return value */
      hb_retptr( FreeImage_CreateICCProfile( dib, data, size ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 3,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 )
                            );
   }
}

/* DLL_API void DLL_CALLCONV FreeImage_DestroyICCProfile(FIBITMAP *dib); */
HB_FUNC( FI_DESTROYICCPROFILE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      FreeImage_DestroyICCProfile( dib );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* Line conversion routines */
/* ------------------------ */

/*
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To4(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To4_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To4_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To4(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To8_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To8(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_555(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_565_To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To16_565(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16_555_To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To16_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To24(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To24_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine32To24(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine1To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine4To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine8To32(BYTE *target, BYTE *source, int width_in_pixels, RGBQUAD *palette);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_555(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine16To32_565(BYTE *target, BYTE *source, int width_in_pixels);
   DLL_API void DLL_CALLCONV FreeImage_ConvertLine24To32(BYTE *target, BYTE *source, int width_in_pixels);
 */

/* Smart conversion routines */
/* ------------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo4Bits(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO4BITS )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo4Bits( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo8Bits(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO8BITS )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo8Bits( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToGreyscale(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTOGREYSCALE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertToGreyscale( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits555(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO16BITS555 )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo16Bits555( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo16Bits565(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO16BITS565 )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo16Bits565( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo24Bits(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO24BITS )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo24Bits( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertTo32Bits(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTO32BITS )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertTo32Bits( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantize(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize); */
HB_FUNC( FI_COLORQUANTIZE )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      FREE_IMAGE_QUANTIZE quantize;

      /* Retrieve parameters */
      dib      = ( FIBITMAP * ) hb_parptr( 1 );
      quantize = ( FREE_IMAGE_QUANTIZE ) hb_parni( 2 );

      /* run function & return value */
      hb_retptr( FreeImage_ColorQuantize( dib, quantize ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ColorQuantizeEx(FIBITMAP *dib, FREE_IMAGE_QUANTIZE quantize FI_DEFAULT(FIQ_WUQUANT), int PaletteSize FI_DEFAULT(256), int ReserveSize FI_DEFAULT(0), RGBQUAD *ReservePalette FI_DEFAULT(NULL)); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Threshold(FIBITMAP *dib, BYTE T); */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Dither(FIBITMAP *dib, FREE_IMAGE_DITHER algorithm); */
HB_FUNC( FI_DITHER )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP *        dib;
      FREE_IMAGE_DITHER algorithm;

      /* Retrieve parameters */
      dib       = ( FIBITMAP * ) hb_parptr( 1 );
      algorithm = ( FREE_IMAGE_DITHER ) hb_parni( 2 );

      /* run function & return value */
      hb_retptr( FreeImage_Dither( dib, algorithm ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertFromRawBits(BYTE *bits, int width, int height, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE)); */
/* DLL_API void DLL_CALLCONV FreeImage_ConvertToRawBits(BYTE *bits, FIBITMAP *dib, int pitch, unsigned bpp, unsigned red_mask, unsigned green_mask, unsigned blue_mask, BOOL topdown FI_DEFAULT(FALSE)); */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToRGBF(FIBITMAP *dib); */
HB_FUNC( FI_CONVERTTORGBF )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_retptr( FreeImage_ConvertToRGBF( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToStandardType(FIBITMAP *src, BOOL scale_linear FI_DEFAULT(TRUE)); */
HB_FUNC( FI_CONVERTTOSTANDARDTYPE )
{
   if( hb_pcount() >= 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;
      BOOL       scale_linear;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );
      scale_linear = ( HB_ISLOG( 2 ) ) ? hb_fi_parl( 2 ) : TRUE;

      /* run function & return value */
      hb_retptr( FreeImage_ConvertToStandardType( dib, scale_linear ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ConvertToType(FIBITMAP *src, FREE_IMAGE_TYPE dst_type, BOOL scale_linear FI_DEFAULT(TRUE)); */
HB_FUNC( FI_CONVERTTOTYPE )
{
   if( hb_pcount() >= 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP *      dib;
      FREE_IMAGE_TYPE dst_type;
      BOOL scale_linear;

      /* Retrieve parameters */
      dib          = ( FIBITMAP * ) hb_parptr( 1 );
      dst_type     = ( FREE_IMAGE_TYPE ) hb_parni( 2 );
      scale_linear = ( HB_ISLOG( 3 ) ) ? hb_fi_parl( 3 ) : TRUE;

      /* run function & return value */
      hb_retptr( FreeImage_ConvertToType( dib, dst_type, scale_linear ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* tone mapping operators */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_ToneMapping(FIBITMAP *dib, FREE_IMAGE_TMO tmo, double first_param FI_DEFAULT(0), double second_param FI_DEFAULT(0)); */
/* DLL_API FIBITMAP* DLL_CALLCONV FreeImage_TmoDrago03(FIBITMAP *src, double gamma FI_DEFAULT(2.2), double exposure FI_DEFAULT(0)); */
/* DLL_API FIBITMAP* DLL_CALLCONV FreeImage_TmoReinhard05(FIBITMAP *src, double intensity FI_DEFAULT(0), double contrast FI_DEFAULT(0)); */

/* ZLib interface */
/* -------------- */

/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibCompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibUncompress(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibGZip(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibGUnzip(BYTE *target, DWORD target_size, BYTE *source, DWORD source_size); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_ZLibCRC32(DWORD crc, BYTE *source, DWORD source_size); */

/* Metadata routines */
/* ----------------- */

/* tag creation / destruction */
/* DLL_API FITAG *DLL_CALLCONV FreeImage_CreateTag(); */
/* DLL_API void DLL_CALLCONV FreeImage_DeleteTag(FITAG *tag); */
/* DLL_API FITAG *DLL_CALLCONV FreeImage_CloneTag(FITAG *tag); */

/* tag getters and setters */
/* DLL_API const char *DLL_CALLCONV FreeImage_GetTagKey(FITAG *tag); */
/* DLL_API const char *DLL_CALLCONV FreeImage_GetTagDescription(FITAG *tag); */
/* DLL_API WORD DLL_CALLCONV FreeImage_GetTagID(FITAG *tag); */
/* DLL_API FREE_IMAGE_MDTYPE DLL_CALLCONV FreeImage_GetTagType(FITAG *tag); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_GetTagCount(FITAG *tag); */
/* DLL_API DWORD DLL_CALLCONV FreeImage_GetTagLength(FITAG *tag); */
/* DLL_API const void *DLL_CALLCONV FreeImage_GetTagValue(FITAG *tag); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagKey(FITAG *tag, const char *key); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagDescription(FITAG *tag, const char *description); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagID(FITAG *tag, WORD id); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagType(FITAG *tag, FREE_IMAGE_MDTYPE type); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagCount(FITAG *tag, DWORD count); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagLength(FITAG *tag, DWORD length) */;
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetTagValue(FITAG *tag, const void *value); */

/* iterator */
/* DLL_API FIMETADATA *DLL_CALLCONV FreeImage_FindFirstMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, FITAG **tag); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_FindNextMetadata(FIMETADATA *mdhandle, FITAG **tag); */
/* DLL_API void DLL_CALLCONV FreeImage_FindCloseMetadata(FIMETADATA *mdhandle); */

/* metadata setter and getter */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, const char *key, FITAG *tag); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_GetMetadata(FREE_IMAGE_MDMODEL model, FIBITMAP *dib, const char *key, FITAG **tag); */

/* helpers */
/* DLL_API unsigned DLL_CALLCONV FreeImage_GetMetadataCount(FREE_IMAGE_MDMODEL model, FIBITMAP *dib); */

/* tag to C string conversion */
/* DLL_API const char* DLL_CALLCONV FreeImage_TagToString(FREE_IMAGE_MDMODEL model, FITAG *tag, char *Make FI_DEFAULT(NULL)); */

/* Image manipulation toolkit */
/* -------------------------- */

/* rotation and flipping */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateClassic(FIBITMAP *dib, double angle); */
HB_FUNC( FI_ROTATECLASSIC )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      double     angle;

      /* Retrieve parameters */
      dib   = ( FIBITMAP * ) hb_parptr( 1 );
      angle = hb_parnd( 2 );

      /* run function & return value */
      hb_retptr( FreeImage_RotateClassic( dib, angle ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 ) );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_RotateEx(FIBITMAP *dib, double angle, double x_shift, double y_shift, double x_origin, double y_origin, BOOL use_mask); */
HB_FUNC( FI_ROTATEEX )
{
   if( hb_pcount() == 7 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 ) &&
       HB_ISNUM( 6 ) &&
       HB_ISLOG( 7 )
       )
   {
      FIBITMAP * dib;
      double     angle, x_shift, y_shift, x_origin, y_origin;
      BOOL       use_mask;

      /* Retrieve parameters */
      dib      = ( FIBITMAP * ) hb_parptr( 1 );
      angle    = hb_parnd( 2 );
      x_shift  = hb_parnd( 3 );
      y_shift  = hb_parnd( 4 );
      x_origin = hb_parnd( 5 );
      y_origin = hb_parnd( 6 );
      use_mask = hb_fi_parl( 7 );

      /* run function & return value */
      hb_retptr( FreeImage_RotateEx( dib, angle, x_shift, y_shift, x_origin, y_origin, use_mask ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 7,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
                            hb_paramError( 5 ), hb_paramError( 6 ), hb_paramError( 7 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_FlipHorizontal(FIBITMAP *dib); */
HB_FUNC( FI_FLIPHORIZONTAL )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_fi_retl( FreeImage_FlipHorizontal( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_FlipVertical(FIBITMAP *dib); */
HB_FUNC( FI_FLIPVERTICAL )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_fi_retl( FreeImage_FlipVertical( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 ) );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_JPEGTransform(const char *src_file, const char *dst_file, FREE_IMAGE_JPEG_OPERATION operation, BOOL perfect FI_DEFAULT(FALSE)); */

/* upsampling / downsampling */
/* ------------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Rescale(FIBITMAP *dib, int dst_width, int dst_height, FREE_IMAGE_FILTER filter); */
HB_FUNC( FI_RESCALE )
{
   if( hb_pcount() == 4 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 )
       )
   {
      FIBITMAP *        dib;
      int               dst_width, dst_height;
      FREE_IMAGE_FILTER filter;

      /* Retrieve parameters */
      dib        = ( FIBITMAP * ) hb_parptr( 1 );
      dst_width  = hb_parni( 2 );
      dst_height = hb_parni( 3 );
      filter     = ( FREE_IMAGE_FILTER ) hb_parni( 4 );

      /* run function & return value */
      hb_retptr( FreeImage_Rescale( dib, dst_width, dst_height, filter ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 4,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 )
                            );
   }
}

/* color manipulation routines (point operations) */
/* ---------------------------------------------- */

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustCurve(FIBITMAP *dib, BYTE *LUT, FREE_IMAGE_COLOR_CHANNEL channel); */

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustGamma(FIBITMAP *dib, double gamma); */
HB_FUNC( FI_ADJUSTGAMMA )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      double     gamma;

      /* Retrieve parameters */
      dib   = ( FIBITMAP * ) hb_parptr( 1 );
      gamma = hb_parnd( 2 );

      /* run function & return value */
      hb_fi_retl( FreeImage_AdjustGamma( dib, gamma ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustBrightness(FIBITMAP *dib, double percentage); */
HB_FUNC( FI_ADJUSTBRIGHTNESS )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      double     percentage;

      /* Retrieve parameters */
      dib        = ( FIBITMAP * ) hb_parptr( 1 );
      percentage = hb_parnd( 2 );

      /* run function & return value */
      hb_fi_retl( FreeImage_AdjustBrightness( dib, percentage ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_AdjustContrast(FIBITMAP *dib, double percentage); */
HB_FUNC( FI_ADJUSTCONTRAST )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      double     percentage;

      /* Retrieve parameters */
      dib        = ( FIBITMAP * ) hb_parptr( 1 );
      percentage = hb_parnd( 2 );

      /* run function & return value */
      hb_fi_retl( FreeImage_AdjustContrast( dib, percentage ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_Invert(FIBITMAP *dib); */
HB_FUNC( FI_INVERT )
{
   if( hb_pcount() == 1 &&
       HB_ISPOINTER( 1 )
       )
   {
      FIBITMAP * dib;

      /* Retrieve parameters */
      dib = ( FIBITMAP * ) hb_parptr( 1 );

      /* run function & return value */
      hb_fi_retl( FreeImage_Invert( dib ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 1,
                            hb_paramError( 1 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_GetHistogram(FIBITMAP *dib, DWORD *histo, FREE_IMAGE_COLOR_CHANNEL channel FI_DEFAULT(FICC_BLACK)); */

/* channel processing routines */
/* --------------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_GetChannel(FIBITMAP *dib, FREE_IMAGE_COLOR_CHANNEL channel); */
HB_FUNC( FI_GETCHANNEL )
{
   if( hb_pcount() == 2 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 )
       )
   {
      FIBITMAP * dib;
      FREE_IMAGE_COLOR_CHANNEL channel;

      /* Retrieve parameters */
      dib     = ( FIBITMAP * ) hb_parptr( 1 );
      channel = ( FREE_IMAGE_COLOR_CHANNEL ) hb_parni( 2 );

      /* run function & return value */
      hb_retptr( FreeImage_GetChannel( dib, channel ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 2,
                            hb_paramError( 1 ), hb_paramError( 2 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_SetChannel(FIBITMAP *dib, FIBITMAP *dib8, FREE_IMAGE_COLOR_CHANNEL channel); */
/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_GetComplexChannel(FIBITMAP *src, FREE_IMAGE_COLOR_CHANNEL channel); */
/* DLL_API BOOL DLL_CALLCONV FreeImage_SetComplexChannel(FIBITMAP *dst, FIBITMAP *src, FREE_IMAGE_COLOR_CHANNEL channel); */

/* copy / paste / composite routines */
/* --------------------------------- */

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Copy(FIBITMAP *dib, int left, int top, int right, int bottom); */
HB_FUNC( FI_COPY )
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISNUM( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
       )
   {
      FIBITMAP * dib;
      int        left, top, right, bottom;

      /* Retrieve parameters */
      dib    = ( FIBITMAP * ) hb_parptr( 1 );
      left   = hb_parni( 2 );
      top    = hb_parni( 3 );
      right  = hb_parni( 4 );
      bottom = hb_parni( 5 );

      /* run function & return value */
      hb_retptr( FreeImage_Copy( dib, left, top, right, bottom ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 5,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
                            hb_paramError( 5 )
                            );
   }
}

/* DLL_API BOOL DLL_CALLCONV FreeImage_Paste(FIBITMAP *dst, FIBITMAP *src, int left, int top, int alpha); */
HB_FUNC( FI_PASTE )
{
   if( hb_pcount() == 5 &&
       HB_ISPOINTER( 1 ) &&
       HB_ISPOINTER( 2 ) &&
       HB_ISNUM( 3 ) &&
       HB_ISNUM( 4 ) &&
       HB_ISNUM( 5 )
       )
   {
      FIBITMAP * dst;
      FIBITMAP * src;
      int        left, top, alpha;

      /* Retrieve parameters */
      dst   = ( FIBITMAP * ) hb_parptr( 1 );
      src   = ( FIBITMAP * ) hb_parptr( 2 );
      left  = hb_parni( 3 );
      top   = hb_parni( 4 );
      alpha = hb_parni( 5 );

      /* run function & return value */
      hb_fi_retl( FreeImage_Paste( dst, src, left, top, alpha ) );
   }
   else
   {
      hb_errRT_BASE_SubstR( EG_ARG, 0, NULL,
                            HB_ERR_FUNCNAME, 5,
                            hb_paramError( 1 ), hb_paramError( 2 ), hb_paramError( 3 ), hb_paramError( 4 ),
                            hb_paramError( 5 )
                            );
   }
}

/* DLL_API FIBITMAP *DLL_CALLCONV FreeImage_Composite(FIBITMAP *fg, BOOL useFileBkg FI_DEFAULT(FALSE), RGBQUAD *appBkColor FI_DEFAULT(NULL), FIBITMAP *bg FI_DEFAULT(NULL)); */

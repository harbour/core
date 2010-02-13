/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows DLL handling function (Xbase++ compatible)
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2006 Paul Tucker <ptucker@sympatico.ca>
 * Copyright 2002 Vic McClung <vicmcclung@vicmcclung.com>
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

#include "hbwin.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"

#ifndef HB_WIN_NO_LEGACY
#define HB_WIN_NO_LEGACY
#endif
#undef HB_LEGACY_LEVEL3
#include "hbwin.ch"

#ifdef HB_COMPAT_XPP

/* NOTE: I'm not totally familiar with how Xbase++ works. This functionality
         was derived from the context in which the functions are used. [pt] */

typedef struct
{
   HMODULE  hDLL;       /* Handle */
   HB_BOOL  bFreeDLL;   /* Free library handle on destroy? */
   int      iFuncFlags;
   FARPROC  lpFunction; /* Function Address */
} HB_DLLEXEC, * PHB_DLLEXEC;

static HB_GARBAGE_FUNC( _DLLUnload )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) Cargo;

   if( xec->hDLL && xec->bFreeDLL )
   {
      FreeLibrary( xec->hDLL );
      xec->hDLL = NULL;
   }
}

static const HB_GC_FUNCS s_gcDllFuncs =
{
   _DLLUnload,
   hb_gcDummyMark
};

HB_FUNC( DLLLOAD )
{
   void * hFileName;

   hb_retnint( ( HB_PTRDIFF ) LoadLibrary( HB_PARSTRDEF( 1, &hFileName, NULL ) ) );

   hb_strfree( hFileName );
}

HB_FUNC( DLLUNLOAD )
{
   if( HB_ISPOINTER( 1 ) )
      hb_retl( FreeLibrary( ( HMODULE ) hb_parptr( 1 ) ) ? HB_TRUE : HB_FALSE );
   else if( HB_ISNUM( 1 ) )
      hb_retl( FreeLibrary( ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 ) ) ? HB_TRUE : HB_FALSE );
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( DLLCALL )
{
   HMODULE hDLL;

   if( HB_ISPOINTER( 1 ) )
      hDLL = ( HMODULE ) hb_parptr( 1 );
   else if( HB_ISNUM( 1 ) )
      hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );
   else if( HB_ISCHAR( 1 ) )
   {
      void * hFileName;
      hDLL = LoadLibrary( HB_PARSTR( 1, &hFileName, NULL ) );
      hb_strfree( hFileName );
   }
   else
      hDLL = NULL;

   if( hDLL && ( HB_PTRDIFF ) hDLL >= 32 )
   {
      HB_BOOL bUNICODE;
      FARPROC lpFunction = hbwin_getprocaddress( hDLL, hb_param( 3, HB_IT_ANY ), &bUNICODE );

      hbwin_dllCall( ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : HB_WIN_DLL_CALLCONV_STDCALL ) | ( bUNICODE ? HB_WIN_DLL_ENC_UTF16 : 0 ),
                     lpFunction,
                     hb_pcount(),
                     4,
                     NULL );

      if( HB_ISCHAR( 1 ) )
         FreeLibrary( hDLL );
   }
}

/* TODO: Add support for UNICODE (*W()) calls. */

HB_FUNC( DLLPREPARECALL )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) hb_gcAllocate( sizeof( HB_DLLEXEC ), &s_gcDllFuncs );
   const char * pszErrorText;

   memset( xec, 0, sizeof( HB_DLLEXEC ) );

   if( HB_ISCHAR( 1 ) )
   {
      void * hFileName;
      xec->hDLL = LoadLibrary( HB_PARSTR( 1, &hFileName, NULL ) );
      hb_strfree( hFileName );
      if( xec->hDLL )
         xec->bFreeDLL = HB_TRUE;
   }
   else if( HB_ISPOINTER( 1 ) )
      xec->hDLL = ( HMODULE ) hb_parptr( 1 );
   else if( HB_ISNUM( 1 ) )
      xec->hDLL = ( HMODULE ) ( HB_PTRDIFF ) hb_parnint( 1 );

   if( xec->hDLL )
   {
      HB_BOOL bUNICODE;
      xec->lpFunction = hbwin_getprocaddress( xec->hDLL, hb_param( 3, HB_IT_ANY ), &bUNICODE );
      if( xec->lpFunction )
      {
         xec->iFuncFlags = ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : HB_WIN_DLL_CALLCONV_STDCALL ) | ( bUNICODE ? HB_WIN_DLL_ENC_UTF16 : 0 );

         hb_retptrGC( xec );
         return;
      }
      pszErrorText = HB_ISCHAR( 3 ) ? "Invalid function name" : "Invalid function ordinal";
   }
   else
      pszErrorText = HB_ISCHAR( 1 ) ? "Invalid library name" : "Invalid library handle";

   hb_gcFree( xec );

   hb_errRT_BASE( EG_ARG, 2010, pszErrorText, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DLLEXECUTECALL )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) hb_parptrGC( &s_gcDllFuncs, 1 );

   if( xec && xec->hDLL && xec->lpFunction )
   {
      hbwin_dllCall( xec->iFuncFlags,
                     xec->lpFunction,
                     hb_pcount(),
                     2,
                     NULL );
   }
}

#endif /* HB_COMPAT_XPP */

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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN )

#include "hbwin.h"
#include "hbdyn.h"

/* NOTE: I'm not totally familiar with how Xbase++ works. This functionality
         was derived from the context in which the functions are used. [pt] */

typedef struct
{
   PHB_ITEM pLibraryHandle;
   HB_BOOL  bFreeLibrary;   /* Free library handle on destroy? */
   int      iFuncFlags;
   void *   pFunctionPtr; /* Function Address */
} HB_DLLEXEC, * PHB_DLLEXEC;

static HB_GARBAGE_FUNC( _DLLUnload )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) Cargo;

   if( xec->pLibraryHandle && xec->bFreeLibrary )
   {
      hb_gcRefFree( xec->pLibraryHandle );
      xec->pLibraryHandle = NULL;
   }
}

static HB_GARBAGE_FUNC( _DLLMark )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) Cargo;

   if( xec->pLibraryHandle )
      hb_gcMark( xec->pLibraryHandle );
}

static const HB_GC_FUNCS s_gcDllFuncs =
{
   _DLLUnload,
   _DLLMark
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
   PHB_ITEM pLibrary = hb_param( 1, HB_IT_ANY );
   PHB_ITEM pLibraryHandle = NULL;
   HB_BOOL bFreeLibrary = HB_FALSE;

   if( HB_IS_STRING( pLibrary ) )
   {
      pLibraryHandle = hb_libLoad( pLibrary, NULL );
      if( pLibraryHandle )
         bFreeLibrary = HB_TRUE;
   }
   else if( hb_libHandle( pLibrary ) )
      pLibraryHandle = pLibrary;

   if( pLibraryHandle )
   {
      void * pFunctionPtr = hb_libSymAddr( pLibraryHandle, hb_parcx( 3 ) );

      hb_dynCall( HB_ISNUM( 2 ) ? hb_parni( 2 ) : HB_DYN_CALLCONV_STDCALL,
                  pFunctionPtr,
                  hb_pcount(),
                  4,
                  NULL );

      if( bFreeLibrary )
         hb_libFree( pLibraryHandle );
   }
}

HB_FUNC( DLLPREPARECALL )
{
   PHB_DLLEXEC xec = ( PHB_DLLEXEC ) hb_gcAllocate( sizeof( HB_DLLEXEC ), &s_gcDllFuncs );
   PHB_ITEM pLibrary = hb_param( 1, HB_IT_ANY );
   const char * pszErrorText;

   memset( xec, 0, sizeof( HB_DLLEXEC ) );

   if( HB_IS_STRING( pLibrary ) )
   {
      xec->pLibraryHandle = hb_libLoad( pLibrary, NULL );
      if( xec->pLibraryHandle )
      {
         hb_gcRefInc( xec->pLibraryHandle );
         xec->bFreeLibrary = HB_TRUE;
      }
   }
   else if( hb_libHandle( pLibrary ) )
   {
      xec->pLibraryHandle = pLibrary;
      hb_gcRefInc( xec->pLibraryHandle );
   }

   if( xec->pLibraryHandle )
   {
      xec->pFunctionPtr = hb_libSymAddr( xec->pLibraryHandle, hb_parcx( 3 ) );
      if( xec->pFunctionPtr )
      {
         xec->iFuncFlags = HB_ISNUM( 2 ) ? hb_parni( 2 ) : HB_DYN_CALLCONV_STDCALL;

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

   if( xec && xec->pLibraryHandle && xec->pFunctionPtr )
      hb_dynCall( xec->iFuncFlags,
                  xec->pFunctionPtr,
                  hb_pcount(),
                  2,
                  NULL );
}

#else

HB_FUNC( DLLLOAD ) { hb_retnint( 0 ); }
HB_FUNC( DLLUNLOAD ) { hb_retl( HB_FALSE ); }
HB_FUNC( DLLCALL ) {}
HB_FUNC( DLLPREPARECALL ) {}
HB_FUNC( DLLEXECUTECALL ) {}

#endif

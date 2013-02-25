/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Dynamic link libraries management functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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

/* NOTE: Need to have these before Harbour headers,
         because in MT mode, they will automatically #include <os2.h>. */
#define INCL_DOSMODULEMGR
#define INCL_ERRORS

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbwinuni.h"

#if defined( HB_OS_WIN )
#  include <windows.h>
#  if defined( HB_OS_WIN_CE )
#     include "hbwince.h"
#  endif
#elif defined( HB_OS_OS2 )
#  include <os2.h>
#endif

/* NOTE: VxWorks supports dlopen() functionality only in shared
         executables. [vszakats] */
#if !defined( HB_HAS_DLFCN ) && \
    ( ( defined( HB_OS_LINUX ) && !defined( __WATCOMC__ ) ) || \
      defined( HB_OS_SUNOS ) || defined( HB_OS_DARWIN ) || \
      defined( HB_OS_BSD ) || defined( HB_OS_BEOS ) || \
      defined( HB_OS_QNX ) || defined( HB_OS_CYGWIN )  || \
      defined( HB_OS_MINIX ) || ( defined( __DJGPP__ ) && \
        ( __DJGPP__ > 2 || ( __DJGPP__ == 2 && __DJGPP_MINOR__ >= 4 ) ) ) )
#  define HB_HAS_DLFCN
#endif

#if defined( HB_HAS_DLFCN )
#  include <dlfcn.h>
#endif

static HB_GARBAGE_FUNC( hb_libRelease )
{
   /* do nothing */
   HB_SYMBOL_UNUSED( Cargo );
}

static const HB_GC_FUNCS s_gcDynlibFuncs =
{
   hb_libRelease,
   hb_gcDummyMark
};

PHB_ITEM hb_libLoad( PHB_ITEM pLibName, PHB_ITEM pArgs )
{
   void * hDynLib = NULL;

   if( hb_itemGetCLen( pLibName ) > 0 )
   {
      int argc = pArgs ? ( int ) hb_arrayLen( pArgs ) : 0, i;
      const char ** argv = NULL;

      if( argc > 0 )
      {
         argv = ( const char ** ) hb_xgrab( sizeof( char * ) * argc );
         for( i = 0; i < argc; ++i )
            argv[ i ] = hb_arrayGetCPtr( pArgs, i + 1 );
      }

      if( hb_vmLockModuleSymbols() )
      {
         /* use stack address as first level marker */
         hb_vmBeginSymbolGroup( ( void * ) hb_stackId(), HB_TRUE );
#if defined( HB_OS_WIN )
         {
            void * hFileName;

            hDynLib = ( void * ) LoadLibrary( HB_ITEMGETSTR( pLibName, &hFileName, NULL ) );

            hb_strfree( hFileName );
         }
#elif defined( HB_OS_OS2 )
         {
            HB_UCHAR LoadError[ 256 ] = "";  /* Area for load failure information */
            HMODULE hDynModule;
            if( DosLoadModule( ( PSZ ) LoadError, sizeof( LoadError ),
                               ( PCSZ ) hb_itemGetCPtr( pLibName ), &hDynModule ) == NO_ERROR )
               hDynLib = ( void * ) hDynModule;
         }
#elif defined( HB_HAS_DLFCN )
         hDynLib = ( void * ) dlopen( hb_itemGetCPtr( pLibName ), RTLD_LAZY | RTLD_GLOBAL );
#else
         {
            int iTODO;
         }
#endif
         /* set real marker */
         hb_vmInitSymbolGroup( hDynLib, argc, argv );

         hb_vmUnlockModuleSymbols();
      }

      if( argv )
         hb_xfree( ( void * ) argv );
   }

   if( hDynLib )
   {
      void ** pLibPtr = ( void ** ) hb_gcAllocate( sizeof( void * ), &s_gcDynlibFuncs );
      *pLibPtr = hDynLib;
      return hb_itemPutPtrGC( NULL, pLibPtr );
   }

   return NULL;
}

HB_BOOL hb_libFree( PHB_ITEM pDynLib )
{
   HB_BOOL fResult = HB_FALSE;
   void ** pDynLibPtr = ( void ** ) hb_itemGetPtrGC( pDynLib, &s_gcDynlibFuncs );

   if( pDynLibPtr && *pDynLibPtr &&
       hb_vmLockModuleSymbols() )
   {
      void * hDynLib = *pDynLibPtr;
      if( hDynLib )
      {
         *pDynLibPtr = NULL;
         hb_vmExitSymbolGroup( hDynLib );
#if defined( HB_OS_WIN )
         fResult = FreeLibrary( ( HMODULE ) hDynLib );
#elif defined( HB_OS_OS2 )
         fResult = DosFreeModule( ( HMODULE ) hDynLib ) == NO_ERROR;
#elif defined( HB_HAS_DLFCN )
         fResult = dlclose( hDynLib ) == 0;
#endif
      }
      hb_vmUnlockModuleSymbols();
   }

   return fResult;
}

void * hb_libHandle( PHB_ITEM pDynLib )
{
   void ** pDynLibPtr = ( void ** ) hb_itemGetPtrGC( pDynLib, &s_gcDynlibFuncs );

   return pDynLibPtr ? *pDynLibPtr : NULL;
}

void * hb_libSymAddr( PHB_ITEM pDynLib, const char * pszSymbol )
{
   void * hDynLib = hb_libHandle( pDynLib );

   if( hDynLib )
   {
#if defined( HB_OS_WIN )
      return ( void * ) GetProcAddress( ( HMODULE ) hDynLib, pszSymbol );
#elif defined( HB_OS_OS2 )
      PFN pProcAddr = NULL;
      if( DosQueryProcAddr( ( HMODULE ) hDynLib, 0, ( PCSZ ) pszSymbol, &pProcAddr ) == NO_ERROR )
         return ( void * ) pProcAddr;
#elif defined( HB_HAS_DLFCN )
      return dlsym( hDynLib, pszSymbol );
#else
      HB_SYMBOL_UNUSED( pszSymbol );
#endif
   }
   return NULL;
}

HB_FUNC( HB_LIBLOAD )
{
   int iPCount = hb_pcount(), i;
   PHB_ITEM pArgs = NULL;

   if( iPCount > 1 )
   {
      pArgs = hb_itemArrayNew( iPCount - 1 );
      for( i = 2; i <= iPCount; ++i )
         hb_arraySet( pArgs, i, hb_param( i, HB_IT_ANY ) );
   }

   hb_itemReturnRelease( hb_libLoad( hb_param( 1, HB_IT_ANY ), pArgs ) );

   if( pArgs )
      hb_itemRelease( pArgs );
}

HB_FUNC( HB_LIBFREE )
{
   hb_retl( hb_libFree( hb_param( 1, HB_IT_ANY ) ) );
}

HB_FUNC( HB_LIBERROR )
{
#if defined( HB_HAS_DLFCN )
   hb_retc( dlerror() );
#else
   hb_retc_null();
#endif
}

/* Get FUNCTION or PROCEDURE symbol from given library.
 *    hb_libGetFunSym( <pLibHandle>, <cFuncName> ) -> <sFuncSym> | NIL
 */
HB_FUNC( HB_LIBGETFUNSYM )
{
   const char * szFuncName = hb_parc( 2 );

   if( szFuncName )
   {
      void * hDynLib = hb_libHandle( hb_param( 1, HB_IT_ANY ) );

      if( hDynLib )
      {
         PHB_SYMB pSym = hb_vmFindFuncSym( szFuncName, hDynLib );

         if( pSym )
            hb_itemPutSymbol( hb_stackReturnItem(), pSym );
      }
   }
}

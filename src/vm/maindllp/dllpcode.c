/*
 * Harbour Project source code:
 *    Import library for PCODE DLLs
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * This code uses HB_DLL_NAME* macros defined by
 *    Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbvm.h"
#include "hbtypes.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #if defined( HB_OS_WIN_CE )
      #include "hbwince.h"
   #endif
#endif

#define HB_DLL_PREF      TEXT( "harbour" )
#define HB_DLL_VER       TEXT( "-" ) TEXT( HB_MACRO2STRING( HB_VER_MAJOR ) ) TEXT( HB_MACRO2STRING( HB_VER_MINOR ) )
#define HB_DLL_EXT       TEXT( ".dll" )

#define HB_DLL_NAME      HB_DLL_PREF HB_DLL_EXT

#if   defined( HB_OS_WIN_CE ) && defined( HB_CPU_ARM )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-wce-arm" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_MIPS )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-wce-mips" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_SH )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-wce-sh" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE ) && defined( HB_CPU_X86 )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-wce-x86" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_CE )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-wce" ) HB_DLL_EXT
#elif defined( __BORLANDC__ )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-bcc" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_X86_64 )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-x64" ) HB_DLL_EXT
#elif defined( HB_OS_WIN_64 ) && defined( HB_CPU_IA_64 )
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER TEXT( "-ia64" ) HB_DLL_EXT
#else
   #define HB_DLL_NAME2  HB_DLL_PREF HB_DLL_VER HB_DLL_EXT
#endif

#if defined( HB_OS_WIN )

HB_EXTERN_BEGIN

#define HB_DLL_MSG_NO_FUNC( func )  \
   do \
   { \
      MessageBox( NULL, \
                  TEXT( "Function '" ) TEXT( func ) TEXT( "' not found!" ), \
                  TEXT( func ), \
                  MB_OK | MB_ICONERROR ); \
   } while( 0 )

typedef PHB_FUNC ( *HB_PROC_GET )( const char * szFuncName );

/* hb_vmProcessSymbols() */
typedef PHB_SYMB ( *HB_VM_PROCESS_SYMBOLS )
   ( PHB_SYMB pModuleSymbols, HB_USHORT uiModuleSymbols,
   const char * szModuleName, HB_ULONG ulID,
   HB_USHORT uiPcodeVer );
static PHB_SYMB s_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiSymbols,
                                    const char * szModuleName, HB_ULONG ulID,
                                    HB_USHORT uiPcodeVer );
static HB_VM_PROCESS_SYMBOLS s_pProcessSymbols = s_vmProcessSymbols;


/* hb_vmExecute() */
typedef void ( *HB_VM_EXECUTE )( const HB_BYTE * pCode, PHB_SYMB pSymbols );
static void s_vmExecute( const HB_BYTE * pCode, PHB_SYMB pSymbols );
static HB_VM_EXECUTE s_pExecute = s_vmExecute;


PHB_FUNC hb_dllGetProcAddress( const char * szProcName )
{
   static HB_PROC_GET s_pProcGet = NULL;
   static HMODULE     s_hModule  = NULL;

   if( s_hModule == NULL )
   {
      s_hModule = GetModuleHandle( HB_DLL_NAME );
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( HB_DLL_NAME2 );
      if( s_hModule == NULL )
         s_hModule = GetModuleHandle( NULL );

      if( s_hModule != NULL )
      {
         static const char * s_szGetProcAddr = "_dll_hb_vmProcAddress";
         int i = 6;

         do
         {
            i -= i == 4 ? 3 : 1;
            s_pProcGet = ( HB_PROC_GET ) GetProcAddress( s_hModule, s_szGetProcAddr + i );
         }
         while( s_pProcGet == NULL && i > 0 );
         if( s_pProcGet == NULL )
            HB_DLL_MSG_NO_FUNC( "hb_vmProcAddress" );
      }
   }

   return s_pProcGet ? s_pProcGet( szProcName ) : NULL;
}


#if defined( HB_OS_WIN_CE ) && ( defined( _MSC_VER ) || defined( __POCC__ ) )
BOOL WINAPI HB_DLL_ENTRY_POINT( HANDLE hInstance, DWORD dwReason, PVOID pvReserved )
#else
BOOL WINAPI HB_DLL_ENTRY_POINT( HINSTANCE hInstance, DWORD dwReason, PVOID pvReserved )
#endif
{
   HB_TRACE( HB_TR_DEBUG, ( "DllEntryPoint(%p, %lu, %p)", hInstance, dwReason, pvReserved ) );

   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( dwReason );
   HB_SYMBOL_UNUSED( pvReserved );

   return TRUE;
}


static PHB_SYMB s_dummy_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiSymbols,
                                          const char * szModuleName, HB_ULONG ulID,
                                          HB_USHORT uiPcodeVer )
{
   HB_SYMBOL_UNUSED( uiSymbols );
   HB_SYMBOL_UNUSED( szModuleName );
   HB_SYMBOL_UNUSED( ulID );
   HB_SYMBOL_UNUSED( uiPcodeVer );

   return pSymbols;
}

static PHB_SYMB s_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiSymbols,
                                    const char * szModuleName, HB_ULONG ulID,
                                    HB_USHORT uiPcodeVer )
{
   HB_VM_PROCESS_SYMBOLS pProcessSymbols = ( HB_VM_PROCESS_SYMBOLS )
                                           hb_dllGetProcAddress( "hb_vmProcessSymbols" );

   if( pProcessSymbols )
   {
      s_pProcessSymbols = pProcessSymbols;
      return s_pProcessSymbols( pSymbols, uiSymbols, szModuleName, ulID, uiPcodeVer );
   }
   else
   {
      s_pProcessSymbols = s_dummy_vmProcessSymbols;
      HB_DLL_MSG_NO_FUNC( "hb_vmProcessSymbols" );
      return pSymbols;
   }
}

PHB_SYMB hb_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiSymbols,
                              const char * szModuleName, HB_ULONG ulID,
                              HB_USHORT uiPcodeVer )
{
   return s_pProcessSymbols( pSymbols, uiSymbols, szModuleName, ulID, uiPcodeVer );
}

static void s_dummy_vmExecute( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_SYMBOL_UNUSED( pCode );
   HB_SYMBOL_UNUSED( pSymbols );
}

static void s_vmExecute( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_VM_EXECUTE pExecute = ( HB_VM_EXECUTE )
                            hb_dllGetProcAddress( "hb_vmExecute" );

   if( pExecute )
   {
      s_pExecute = pExecute;
      s_pExecute( pCode, pSymbols );
   }
   else
   {
      s_pExecute = s_dummy_vmExecute;
      HB_DLL_MSG_NO_FUNC( "hb_vmExecute" );
   }
}

void hb_vmExecute( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   s_pExecute( pCode, pSymbols );
}

HB_EXTERN_END

#endif /* HB_OS_WIN */

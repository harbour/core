/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows pcode DLL entry point and VM/RTL routing functions
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

#define HB_OS_WIN_32_USED

#include "hbvm.h"
#include "hbapiitm.h"

typedef void ( * VM_PROCESS_DLL_SYMBOLS ) ( PHB_SYMB pModuleSymbols,
                                            USHORT uiModuleSymbols );

typedef void ( * VM_EXECUTE ) ( const BYTE * pCode, PHB_SYMB pSymbols );


#if defined(HB_OS_WIN_32)

#if defined(__BORLANDC__)
BOOL WINAPI _export
#else
__declspec(dllexport) BOOL
#endif
WINAPI DllEntryPoint( HINSTANCE hInstance, DWORD fdwReason, PVOID pvReserved )
{
   HB_TRACE( HB_TR_DEBUG, ("DllEntryPoint(%p, %p, %d)", hInstance, fdwReason,
             pvReserved ) );

   HB_SYMBOL_UNUSED( hInstance );
   HB_SYMBOL_UNUSED( fdwReason );
   HB_SYMBOL_UNUSED( pvReserved );

   switch( fdwReason )
   {
      case DLL_PROCESS_ATTACH:
           break;

      case DLL_PROCESS_DETACH:
           break;
   }

   return TRUE;
}

/* module symbols initialization */
void hb_vmProcessSymbols( PHB_SYMB pModuleSymbols, USHORT uiModuleSymbols )
{
   FARPROC pProcessSymbols = GetProcAddress( GetModuleHandle( NULL ),
                                             "_hb_vmProcessDllSymbols" );
   if( pProcessSymbols )
      ( ( VM_PROCESS_DLL_SYMBOLS ) pProcessSymbols ) ( pModuleSymbols,
                                                       uiModuleSymbols );
   /* else
    *    may we issue an error ? */
}

void hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
   FARPROC pExecute = GetProcAddress( GetModuleHandle( NULL ), "_hb_vmExecute" );

   if( pExecute )
      ( ( VM_EXECUTE ) pExecute ) ( pCode, pSymbols );

   /* else
    *    may we issue an error ? */
}

#endif
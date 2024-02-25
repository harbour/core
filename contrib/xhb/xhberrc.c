/*
 * The default error handler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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
#include "hbvm.h"
#include "hbstack.h"
#include "hbthread.h"

#if defined( HB_OS_WIN )

#include <windows.h>

static PHB_SYMB s_pFuncSymbol = NULL;
static PHB_DYNS s_pHbCStructDyn = NULL;
static PHB_DYNS s_pBufferMsg = NULL;

LONG WINAPI PRGUnhandledExceptionFilter( EXCEPTION_POINTERS * ExceptionInfo )
{
   LONG lResult = EXCEPTION_CONTINUE_SEARCH;

   if( s_pFuncSymbol && hb_vmRequestReenter() )
   {
      PHB_ITEM pException = NULL;
      HB_USHORT uiParams = 0;

      if( s_pHbCStructDyn )
      {
         hb_vmPushSymbol( hb_dynsymSymbol( s_pHbCStructDyn ) );
         hb_vmPushNil();
         hb_vmPushStringPcode( "EXCEPTION_POINTERS", 18 );
         hb_vmPushLong( 8 );
         hb_vmDo( 2 );
         if( HB_IS_OBJECT( hb_stackReturnItem() ) )
         {
            pException = hb_itemNew( hb_stackReturnItem() );
            if( s_pBufferMsg )
            {
               hb_vmPushDynSym( s_pBufferMsg );
               hb_vmPush( pException );
               if( ( ( const char * ) ExceptionInfo )[ sizeof( EXCEPTION_POINTERS ) ] == '\0' )
                  hb_itemPutCLConst( hb_stackAllocItem(), ( const char * ) ExceptionInfo, sizeof( EXCEPTION_POINTERS ) );
               else
                  hb_itemPutCL( hb_stackAllocItem(), ( const char * ) ExceptionInfo, sizeof( EXCEPTION_POINTERS ) );
               hb_vmPushLogical( HB_FALSE );
               hb_vmSend( 2 );
            }
         }
      }

      hb_vmPushSymbol( s_pFuncSymbol );
      hb_vmPushNil();
      if( pException )
      {
         hb_vmPush( pException );
         hb_itemRelease( pException );
         uiParams = 1;
      }
      hb_vmDo( uiParams );
      lResult = hb_parnldef( -1, EXCEPTION_CONTINUE_SEARCH );

      hb_vmRequestRestore();
   }

   return lResult;
}
#endif

HB_FUNC( SETUNHANDLEDEXCEPTIONFILTER )
{
#if defined( HB_OS_WIN )
   LPTOP_LEVEL_EXCEPTION_FILTER pDefaultHandler = NULL;
   PHB_ITEM pFuncItm = hb_param( 1, HB_IT_ANY );
   PHB_SYMB pFuncSym = s_pFuncSymbol;

   if( pFuncItm && HB_IS_SYMBOL( pFuncItm ) )
   {
      s_pFuncSymbol = hb_itemGetSymbol( pFuncItm );
      pDefaultHandler = PRGUnhandledExceptionFilter;
      /* intentionally no protection for repeated initialization,
       * it's possible that HB_CSTRUCTURE() has been loaded from
       * dynamic library [druzus]
       */
      if( s_pHbCStructDyn == NULL )
      {
         PHB_DYNS pDyn = hb_dynsymFind( "HB_CSTRUCTURE" );
         if( pDyn && hb_dynsymIsFunction( pDyn ) )
         {
            s_pHbCStructDyn = pDyn;
            if( s_pBufferMsg == NULL )
               s_pBufferMsg = hb_dynsymFind( "BUFFER" );
         }
      }
   }
   else
   {
      s_pFuncSymbol = NULL;
      if( pFuncItm && HB_IS_POINTER( pFuncItm ) )
      {
         pDefaultHandler = ( LPTOP_LEVEL_EXCEPTION_FILTER ) hb_itemGetPtr( pFuncItm );
         if( pDefaultHandler == PRGUnhandledExceptionFilter )
            pDefaultHandler = NULL;
      }
   }

   pDefaultHandler = SetUnhandledExceptionFilter( pDefaultHandler );

   if( pFuncSym )
      hb_itemPutSymbol( hb_stackReturnItem(), pFuncSym );
   else if( pDefaultHandler )
      hb_retptr( ( void * ) pDefaultHandler );
   /* else hb_ret(); -> NIL is default */
#endif
}

HB_FUNC( SETERRORMODE )
{
#if defined( HB_OS_WIN )
   hb_retni( SetErrorMode( hb_parni( 1 ) ) );
#else
   hb_retni( 0 );
#endif
}

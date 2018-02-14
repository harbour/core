/*
 * Windows Service API
 *
 * Copyright 2010 Jose Luis Capel <jlcapel at hotmail . com>
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

#include "hbwapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"

#if ! defined( HB_OS_WIN_CE )

#if defined( __POCC__ ) || defined( __XCC__ )
   #include <winsvc.h>   /* it's disabled by WIN32_LEAN_AND_MEAN */
#endif

static SERVICE_STATUS        s_ServiceStatus;
static SERVICE_STATUS_HANDLE s_hStatus;
static PHB_ITEM              s_pHarbourEntryFunc = NULL;
static PHB_ITEM              s_pHarbourControlFunc = NULL;
static TCHAR                 s_lpServiceName[ 256 ];

/* Control handler function */
static VOID WINAPI hbwin_SvcControlHandler( DWORD fdwControl )
{
   if( s_pHarbourControlFunc )
   {
      if( hb_vmRequestReenterExt() )
      {
         hb_vmPushEvalSym();
         hb_vmPush( s_pHarbourControlFunc );
         hb_vmPushNumInt( ( HB_MAXINT ) fdwControl );
         hb_vmSend( 1 );
         hb_vmRequestRestore();
      }
      else
         HB_TRACE( HB_TR_DEBUG, ( "HVM stack not available" ) );
      return;
   }

   switch( fdwControl )
   {
      case SERVICE_CONTROL_STOP:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         break;

      case SERVICE_CONTROL_SHUTDOWN:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         break;

      default:
         return;
   }

   SetServiceStatus( s_hStatus, &s_ServiceStatus );  /* Report current status */
}

static VOID WINAPI hbwin_SvcMainFunction( DWORD dwArgc, LPTSTR * lpszArgv )
{
   s_ServiceStatus.dwServiceType             = SERVICE_WIN32;
   s_ServiceStatus.dwCurrentState            = SERVICE_START_PENDING;
   s_ServiceStatus.dwControlsAccepted        = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
   s_ServiceStatus.dwWin32ExitCode           = 0;
   s_ServiceStatus.dwServiceSpecificExitCode = 0;
   s_ServiceStatus.dwCheckPoint              = 0;
   s_ServiceStatus.dwWaitHint                = 0;

   s_hStatus = RegisterServiceCtrlHandler( s_lpServiceName, ( LPHANDLER_FUNCTION ) hbwin_SvcControlHandler );

   if( s_hStatus != ( SERVICE_STATUS_HANDLE ) 0 )
   {
      if( s_pHarbourEntryFunc != NULL )
      {
         if( hb_vmRequestReenterExt() )
         {
            DWORD i;
            int iArgCount = 0;

            if( ! s_pHarbourControlFunc )
            {
               /* We report the running status to SCM. */
               s_ServiceStatus.dwCurrentState = SERVICE_RUNNING;
               SetServiceStatus( s_hStatus, &s_ServiceStatus );
            }

            hb_vmPushEvalSym();
            hb_vmPush( s_pHarbourEntryFunc );

            for( i = 1; i < dwArgc; ++i )
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               HB_ITEMPUTSTR( pItem, lpszArgv[ i ] );
               if( hb_cmdargIsInternal( hb_itemGetCPtr( pItem ), NULL ) )
                  hb_stackPop();
               else
                  ++iArgCount;
            }

            hb_vmSend( ( HB_USHORT ) iArgCount );

            hb_vmRequestRestore();
         }
         else
            HB_TRACE( HB_TR_DEBUG, ( "HVM stack not available" ) );
      }
      else
         HB_TRACE( HB_TR_DEBUG, ( "Harbour service entry function not found" ) );
   }
   else
      HB_TRACE( HB_TR_DEBUG, ( "Error registering service" ) );
}

#endif

HB_FUNC( WIN_SERVICEGETSTATUS )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retnint( s_ServiceStatus.dwCurrentState );
#else
   hb_retnint( 0 );
#endif
}

HB_FUNC( WIN_SERVICESETSTATUS )
{
#if ! defined( HB_OS_WIN_CE )
   HB_BOOL bRetVal;
   s_ServiceStatus.dwCurrentState = ( DWORD ) hb_parnl( 1 );
   bRetVal = ( HB_BOOL ) SetServiceStatus( s_hStatus, &s_ServiceStatus );
   hbwapi_SetLastError( GetLastError() );
   hb_retl( bRetVal );
#else
   hbwapi_SetLastError( ERROR_NOT_SUPPORTED );
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESETEXITCODE )
{
#if ! defined( HB_OS_WIN_CE )
   HB_BOOL bRetVal;
   s_ServiceStatus.dwWin32ExitCode = ( DWORD ) hb_parnl( 1 );
   bRetVal = ( HB_BOOL ) SetServiceStatus( s_hStatus, &s_ServiceStatus );
   hbwapi_SetLastError( GetLastError() );
   hb_retl( bRetVal );
#else
   hbwapi_SetLastError( ERROR_NOT_SUPPORTED );
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESTOP )
{
#if ! defined( HB_OS_WIN_CE )
   HB_BOOL bRetVal;
   s_ServiceStatus.dwCurrentState = SERVICE_STOPPED;
   bRetVal = ( HB_BOOL ) SetServiceStatus( s_hStatus, &s_ServiceStatus );
   hbwapi_SetLastError( GetLastError() );
   hb_retl( bRetVal );
#else
   hbwapi_SetLastError( ERROR_NOT_SUPPORTED );
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESTART )
{
   HB_BOOL bRetVal;

#if ! defined( HB_OS_WIN_CE )
   PHB_ITEM pEntryFunc;

   SERVICE_TABLE_ENTRY lpServiceTable[ 2 ];

   HB_ITEMCOPYSTR( hb_param( 1, HB_IT_STRING ), s_lpServiceName, HB_SIZEOFARRAY( s_lpServiceName ) );

   if( s_pHarbourEntryFunc )
   {
      hb_itemRelease( s_pHarbourEntryFunc );
      s_pHarbourEntryFunc = NULL;
   }

   pEntryFunc = hb_param( 2, HB_IT_EVALITEM );

   if( pEntryFunc )
      s_pHarbourEntryFunc = hb_itemNew( pEntryFunc );

   if( s_pHarbourControlFunc )
   {
      hb_itemRelease( s_pHarbourControlFunc );
      s_pHarbourControlFunc = NULL;
   }

   pEntryFunc = hb_param( 3, HB_IT_EVALITEM );

   if( pEntryFunc )
      s_pHarbourControlFunc = hb_itemNew( pEntryFunc );

   lpServiceTable[ 0 ].lpServiceName = s_lpServiceName;
   lpServiceTable[ 0 ].lpServiceProc = ( LPSERVICE_MAIN_FUNCTION ) hbwin_SvcMainFunction;

   lpServiceTable[ 1 ].lpServiceName = NULL;
   lpServiceTable[ 1 ].lpServiceProc = NULL;

   bRetVal = ( HB_BOOL ) StartServiceCtrlDispatcher( lpServiceTable );
   hbwapi_SetLastError( GetLastError() );
#else
   bRetVal = HB_FALSE;
   hbwapi_SetLastError( ERROR_NOT_SUPPORTED );
#endif
   hb_retl( bRetVal );
}

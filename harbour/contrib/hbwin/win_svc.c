/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows Service API
 *
 * Copyright 2010 Jose Luis Capel - <jlcapel at hotmail . com>
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

#include "hbwapi.h"

#include "hbvm.h"
#include "hbapiitm.h"

#if ! defined( HB_OS_WIN_CE )

#if defined( __POCC__ ) || defined( __XCC__ )
#  include <winsvc.h>   /* it's disabled by WIN32_LEAN_AND_MEAN */
#endif

static SERVICE_STATUS        s_ServiceStatus;
static SERVICE_STATUS_HANDLE s_hStatus;
static PHB_ITEM              s_pHarbourEntryFunc = NULL;
static TCHAR                 s_lpServiceName[ 256 ];

/* Control handler function */
static VOID WINAPI hbwin_SvcControlHandler( DWORD fdwControl )
{
   switch( fdwControl )
   {
      case SERVICE_CONTROL_STOP:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         return;

      case SERVICE_CONTROL_SHUTDOWN:
         s_ServiceStatus.dwWin32ExitCode = 0;
         s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
         return;
   }

   SetServiceStatus( s_hStatus, &s_ServiceStatus ); /* Report current status */
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

            /* We report the running status to SCM. */
            s_ServiceStatus.dwCurrentState = SERVICE_RUNNING;
            SetServiceStatus( s_hStatus, &s_ServiceStatus );

            hb_vmPushEvalSym();
            hb_vmPush( s_pHarbourEntryFunc );

            for( i = 1; i < dwArgc; ++i )
            {
               char * pszArg = HB_TCHAR_CONVFROM( lpszArgv[ i ] );

               if( ! hb_cmdargIsInternal( pszArg, NULL ) )
               {
                  hb_vmPushString( pszArg, strlen( pszArg ) );
                  ++iArgCount;
               }

               HB_TCHAR_FREE( pszArg );
            }

            hb_vmSend( ( HB_USHORT ) iArgCount );

            hb_vmRequestRestore();
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ("HVM stack not available") );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ("Harbour service entry function not found") );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ("Error registering service") );
   }
}

#endif

HB_FUNC( WIN_SERVICEGETSTATUS )
{
#if ! defined( HB_OS_WIN_CE )
   hb_retnl( s_ServiceStatus.dwCurrentState );
#else
   hb_retnl( 0 );
#endif
}

HB_FUNC( WIN_SERVICESETSTATUS )
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwCurrentState = ( DWORD ) hb_parnl( 1 );
   hb_retl( SetServiceStatus( s_hStatus, &s_ServiceStatus ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESETEXITCODE )
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwWin32ExitCode = ( DWORD ) hb_parnl( 1 );
   hb_retl( SetServiceStatus( s_hStatus, &s_ServiceStatus ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESTOP )
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwCurrentState = SERVICE_STOPPED;
   SetServiceStatus( s_hStatus, &s_ServiceStatus );
#endif
}

HB_FUNC( WIN_SERVICEINSTALL )
{
   HB_BOOL bRetVal = HB_FALSE;

#if ! defined( HB_OS_WIN_CE )

   void * hPath;
   LPCTSTR lpPath = HB_PARSTR( 3, &hPath, NULL );

   TCHAR lpPathBuffer[ MAX_PATH ];

   if( lpPath == NULL )
   {
      if( GetModuleFileName( NULL, lpPathBuffer, HB_SIZEOFARRAY( lpPathBuffer ) ) )
         lpPath = lpPathBuffer;
      else
         hbwapi_SetLastError( GetLastError() );
   }

   if( lpPath )
   {
      SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

      if( schSCM )
      {
         SC_HANDLE schSrv;

         void * hServiceName;
         void * hDisplayName;

         LPCTSTR lpServiceName = HB_PARSTRDEF( 1, &hServiceName, NULL );
         LPCTSTR lpDisplayName = HB_PARSTRDEF( 2, &hDisplayName, NULL );

         schSrv = CreateService( schSCM,                       /* SCM database */
                                 lpServiceName,                /* name of service */
                                 lpDisplayName,                /* service name to display */
                                 SERVICE_ALL_ACCESS,           /* desired access */
                                 SERVICE_WIN32_OWN_PROCESS,    /* service type */
                                 SERVICE_DEMAND_START,         /* start type */
                                 SERVICE_ERROR_NORMAL,         /* error control type */
                                 lpPath,                       /* path to service's binary */
                                 NULL,                         /* no load ordering group */
                                 NULL,                         /* no tag identifier */
                                 NULL,                         /* no dependencies */
                                 NULL,                         /* LocalSystem account */
                                 NULL );                       /* no password */

         if( schSrv )
         {
            bRetVal = HB_TRUE;

            CloseServiceHandle( schSrv );
         }
         else
            hbwapi_SetLastError( GetLastError() );

         hb_strfree( hServiceName );
         hb_strfree( hDisplayName );

         CloseServiceHandle( schSCM );
      }
      else
         hbwapi_SetLastError( GetLastError() );
   }

   hb_strfree( hPath );

#endif
   hb_retl( bRetVal );
}

HB_FUNC( WIN_SERVICEDELETE )
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )
   SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

   if( schSCM )
   {
      void * hServiceName;

      SC_HANDLE schSrv = OpenService( schSCM,
                                      HB_PARSTRDEF( 1, &hServiceName, NULL ),
                                      SERVICE_ALL_ACCESS );

      if( schSrv )
      {
         /* TODO: check if service is up and then stop it */

         bRetVal = ( HB_BOOL ) DeleteService( schSrv );

         CloseServiceHandle( schSrv );
      }
      else
         hbwapi_SetLastError( GetLastError() );

      hb_strfree( hServiceName );

      CloseServiceHandle( schSCM );
   }
   else
      hbwapi_SetLastError( GetLastError() );
#endif
   hb_retl( bRetVal );
}

HB_FUNC( WIN_SERVICESTART )
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )
   PHB_ITEM pEntryFunc;

   SERVICE_TABLE_ENTRY lpServiceTable[ 2 ];

   HB_ITEMCOPYSTR( hb_param( 1, HB_IT_STRING ), s_lpServiceName, HB_SIZEOFARRAY( s_lpServiceName ) );

   if( s_pHarbourEntryFunc )
   {
      hb_itemRelease( s_pHarbourEntryFunc );
      s_pHarbourEntryFunc = NULL;
   }

   pEntryFunc = hb_param( 2, HB_IT_BLOCK | HB_IT_SYMBOL );

   if( pEntryFunc )
      s_pHarbourEntryFunc = hb_itemNew( pEntryFunc );

   lpServiceTable[ 0 ].lpServiceName = s_lpServiceName;
   lpServiceTable[ 0 ].lpServiceProc = ( LPSERVICE_MAIN_FUNCTION ) hbwin_SvcMainFunction;

   lpServiceTable[ 1 ].lpServiceName = NULL;
   lpServiceTable[ 1 ].lpServiceProc = NULL;

   if( StartServiceCtrlDispatcher( lpServiceTable ) )
      bRetVal = HB_TRUE;
   else
      hbwapi_SetLastError( GetLastError() );

#endif
   hb_retl( bRetVal );
}

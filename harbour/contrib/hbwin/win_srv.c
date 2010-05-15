/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Windows Service
 *
 * Copyright 2010 Jose Luis Capel - <jlcapel at hotmail . com>
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
#include "hbvm.h"
#include "hbwinuni.h"

#include "hbwapi.h"

#if ! defined( HB_OS_WIN_CE )

static SERVICE_STATUS        s_ServiceStatus;
static SERVICE_STATUS_HANDLE s_hStatus;
static char *                s_pszPrgFunction;
static char *                s_pszSrvName;

static void hbwin_ControlHandler( DWORD request );
static void hbwin_SrvFunction( int argc, char ** argv );

/* Control handler function */
static void hbwin_ControlHandler( DWORD request )
{
   switch( request )
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

   /* Report current status */
   Sets_ServiceStatus( s_hStatus, &s_ServiceStatus );
}

static void hbwin_SrvFunction( int argc, char** argv )
{
   HB_SYMBOL_UNUSED( argc );
   HB_SYMBOL_UNUSED( argv );

   s_ServiceStatus.dwServiceType             = SERVICE_WIN32;
   s_ServiceStatus.dwCurrentState            = SERVICE_START_PENDING;
   s_ServiceStatus.dwControlsAccepted        = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
   s_ServiceStatus.dwWin32ExitCode           = 0;
   s_ServiceStatus.dwServiceSpecificExitCode = 0;
   s_ServiceStatus.dwCheckPoint              = 0;
   s_ServiceStatus.dwWaitHint                = 0;

   s_hStatus = RegisterServiceCtrlHandler( s_pszSrvName, ( LPHANDLER_FUNCTION ) hbwin_ControlHandler );

   if( s_hStatus == ( SERVICE_STATUS_HANDLE ) 0 )
   {
      HB_TRACE( HB_TR_DEBUG, ( "Error registering service\n" ) );
   }
   else
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( s_pszPrgFunction );

      /* We report the running status to SCM. */
      s_ServiceStatus.dwCurrentState = SERVICE_RUNNING;
      Sets_ServiceStatus( s_hStatus, &s_ServiceStatus );

      if( pDynSym )
      {
         if( hb_vmRequestReenter() )
         {
            hb_vmPushSymbol( hb_dynsymSymbol( pDynSym ) );
            hb_vmPushNil();
            hb_vmDo( 0 );
            hb_vmRequestRestore();
         }
      }
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

HB_FUNC( WIN_SERVICESETSTATUS ) /* dwStatus */
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwCurrentState = ( DWORD ) hb_parnl( 1 );
   hb_retl( Sets_ServiceStatus( s_hStatus, &s_ServiceStatus ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESETEXITCODE ) /* dwExitCode */
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwWin32ExitCode = ( DWORD ) hb_parnl( 1 );
   hb_retl( Sets_ServiceStatus( s_hStatus, &s_ServiceStatus ) );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( WIN_SERVICESTOP )
{
#if ! defined( HB_OS_WIN_CE )
   s_ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
   Sets_ServiceStatus( s_hStatus, &s_ServiceStatus );
#endif
}

HB_FUNC( WIN_SERVICEINSTALL )
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )
   TCHAR szPath[ MAX_PATH ];

   if( GetModuleFileName( NULL, szPath, MAX_PATH ) )
   {
      SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

      if( schSCM )
      {
         SC_HANDLE schSrv;

         void * hServiceName;
         void * hDisplayName;

         LPCTSTR lpServiceName = HB_PARSTR( 1, &hServiceName, NULL );
         LPCTSTR lpDisplayName = HB_PARSTR( 2, &hDisplayName, NULL );

         schSrv = CreateService( schSCM,                       /* SCM database  */
                                 lpServiceName,                /* name of service */
                                 lpDisplayName,                /* service name to display */
                                 SERVICE_ALL_ACCESS,           /* desired access */
                                 SERVICE_WIN32_OWN_PROCESS,    /* service type */
                                 SERVICE_DEMAND_START,         /* start type */
                                 SERVICE_ERROR_NORMAL,         /* error control type */
                                 szPath,                       /* path to service's binary */
                                 NULL,                         /* no load ordering group */
                                 NULL,                         /* no tag identifier */
                                 NULL,                         /* no dependencies */
                                 NULL,                         /* LocalSystem account */
                                 NULL );                       /* no password */

         hb_strfree( hServiceName );
         hb_strfree( hDisplayName );

         if( schSrv )
         {
            bRetVal = HB_TRUE;

            CloseServiceHandle( schSrv );
         }
         else
            hbwapi_SetLastError( GetLastError() );

         CloseServiceHandle( schSCM );
      }
      else
         hbwapi_SetLastError( GetLastError() );
   }
   else
      hbwapi_SetLastError( GetLastError() );

#endif
   hb_retl( bRetVal );
}

HB_FUNC( WIN_SERVICEDELETE ) /* sServiceName */
{
   HB_BOOL bRetVal = HB_FALSE;
#if ! defined( HB_OS_WIN_CE )
   SC_HANDLE schSCM = OpenSCManager( NULL, NULL, SC_MANAGER_ALL_ACCESS );

   if( schSCM )
   {
      void * hschSCM;

      SC_HANDLE schSrv = OpenService( schSCM,
                                      HB_PARSTR( 1, &hschSCM, NULL ),
                                      SERVICE_ALL_ACCESS );

      if( schSrv )
      {
         /* TODO: check if service is up and then stop it */

         bRetVal = ( HB_BOOL ) DeleteService( schSrv );

         CloseServiceHandle( schSrv );
      }
      else
         hbwapi_SetLastError( GetLastError() );

      hb_strfree( hschSCM );

      CloseServiceHandle( schSCM );
   }
   else
      hbwapi_SetLastError( GetLastError() );
#endif
   hb_retl( bRetVal );
}

HB_FUNC( WIN_SERVICESTART ) /* pszServiceName, pszPrgFunction */
{
#if ! defined( HB_OS_WIN_CE )
   SERVICE_TABLE_ENTRY ServiceTable[ 2 ];
   LPTSTR lpServiceName;
   LPTSTR lpPrgFunction;
   void * hServiceName;
   void * hPrgFunction;

   lpServiceName = ( LPTSTR ) HB_PARSTR( 1, &hServiceName, NULL );
   lpPrgFunction = ( LPTSTR ) HB_PARSTR( 2, &hPrgFunction, NULL );

   ServiceTable[ 0 ].lpServiceName = lpServiceName;
   ServiceTable[ 0 ].lpServiceProc = ( LPSERVICE_MAIN_FUNCTION ) hbwin_SrvFunction;

   ServiceTable[ 1 ].lpServiceName = NULL;
   ServiceTable[ 1 ].lpServiceProc = NULL;

   s_pszPrgFunction = lpPrgFunction;
   s_pszSrvName     = lpServiceName;

   hb_strfree( hServiceName );
   hb_strfree( hPrgFunction );

   hb_retl( ( HB_BOOL ) StartServiceCtrlDispatcher( ServiceTable ) );
#else
   hb_retl( HB_FALSE );
#endif
}

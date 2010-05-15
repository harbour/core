/*
 * Chr(36) + "Id" + Chr(36)
 */

/*
 * Harbour Project source code:
 *    Windows Service
 *
 * Copyright 2010 José Luis Capel - <jlcapel at hotmail . com>
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

#include <windows.h>
#include "hbapi.h"
#include "hbvm.h"
#include "hbwinuni.h"
#include "hbwapi.h"

#if ! defined( HB_OS_WIN_CE )


static SERVICE_STATUS ServiceStatus;
static SERVICE_STATUS_HANDLE hStatus;
static char *sPrgFunction;
static char *sSrvName;

void hbwin_ControlHandler(DWORD request);
void hbwin_SrvFunction(int argc, char** argv);


HB_FUNC( WIN_SERVICEGETSTATUS )
{
   hb_retnl( ServiceStatus.dwCurrentState );
}


HB_FUNC( WIN_SERVICESETSTATUS ) /* dwStatus */
{
   ServiceStatus.dwCurrentState = (DWORD) hb_parnl(1);
   hb_retl( SetServiceStatus( hStatus, &ServiceStatus ) );
}

HB_FUNC( WIN_SERVICESETEXITCODE ) /* dwExitCode */
{
   ServiceStatus.dwWin32ExitCode = (DWORD) hb_parnl(1);
   hb_retl( SetServiceStatus( hStatus, &ServiceStatus ) );
}

HB_FUNC( WIN_SERVICESTOP )
{
   ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
   SetServiceStatus( hStatus, &ServiceStatus );
}


HB_FUNC( WIN_SERVICEINSTALL ) /* sServiceName, sServiceNameDisplayed */
{
   SC_HANDLE schSCM;
   SC_HANDLE schSrv;
   TCHAR szPath[MAX_PATH];
   LPCTSTR lpServiceName,lpDisplayName;
   void * hServiceName;
   void * hDisplayName;


   if( !GetModuleFileName( NULL, szPath, MAX_PATH ) )
   {
      hbwapi_SetLastError( GetLastError() ) ;
      hb_retl( HB_FALSE );  /* Check GetLastError   */
      return;
   }


   schSCM = OpenSCManager(NULL, NULL,SC_MANAGER_ALL_ACCESS);
   if( schSCM == NULL )
   {
      hbwapi_SetLastError( GetLastError() ) ;
      hb_retl( HB_FALSE );  /* Check GetLastError */
      return;
   }


   lpServiceName = HB_PARSTR( 1, &hServiceName, NULL );
   lpDisplayName = HB_PARSTR( 2, &hDisplayName, NULL );

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

   if( schSrv == NULL )
   {
      CloseServiceHandle( schSCM );
      hbwapi_SetLastError( GetLastError() ) ;
      hb_retl( HB_FALSE );  /* Check GetLastError */
      return;
   }

   CloseServiceHandle( schSrv );
   CloseServiceHandle( schSCM );
   hb_retl( HB_TRUE );
}


HB_FUNC( WIN_SERVICEDELETE ) /* sServiceName */
{
   SC_HANDLE schSCM;
   SC_HANDLE schSrv;
   void * hschSCM;

   schSCM = OpenSCManager( NULL, NULL,SC_MANAGER_ALL_ACCESS );
   if( schSCM == NULL )
   {
      hbwapi_SetLastError( GetLastError() ) ;
      hb_retl( HB_FALSE );  /* Check GetLastError */
      return;
   }

   schSrv = OpenService( schSCM, HB_PARSTR( 1, &hschSCM, NULL ), SERVICE_ALL_ACCESS );
   if( schSrv== NULL )
   {
      CloseServiceHandle( schSCM );
      hbwapi_SetLastError( GetLastError() ) ;
      hb_retl( HB_FALSE );  /* Check GetLastError */
      return;
   }

   /*
    TODO: check if service is up and then stop it
   */

   hb_strfree( hschSCM );
   hb_retl( DeleteService( schSrv ) );
	CloseServiceHandle( schSrv );
	CloseServiceHandle( schSCM );
}



HB_FUNC( WIN_SERVICESTART ) /* sServiceName, sPrgFunction */
{
   SERVICE_TABLE_ENTRY ServiceTable[2];
   LPTSTR lpServiceName, lpPrgFunction;
   void * hServiceName;
   void * hPrgFunction;

   lpServiceName = (LPTSTR) HB_PARSTR( 1, &hServiceName, NULL );
   lpPrgFunction = (LPTSTR) HB_PARSTR( 2, &hPrgFunction, NULL );


   ServiceTable[0].lpServiceName =  lpServiceName;
   ServiceTable[0].lpServiceProc = (LPSERVICE_MAIN_FUNCTION)hbwin_SrvFunction;

   ServiceTable[1].lpServiceName = NULL;
   ServiceTable[1].lpServiceProc = NULL;

   sPrgFunction =  lpPrgFunction;
   sSrvName     =  lpServiceName;

   hb_strfree( hServiceName );
   hb_strfree( hPrgFunction );

   hb_retl( StartServiceCtrlDispatcher( ServiceTable ) );

}


/* Control handler function */
void hbwin_ControlHandler( DWORD request )
{
    switch( request )
    {
        case SERVICE_CONTROL_STOP:
            ServiceStatus.dwWin32ExitCode = 0;
            ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
            return;

        case SERVICE_CONTROL_SHUTDOWN:
            ServiceStatus.dwWin32ExitCode = 0;
            ServiceStatus.dwCurrentState  = SERVICE_STOPPED;
            return;

        default:
            break;
    }

    /* Report current status */
    SetServiceStatus( hStatus,  &ServiceStatus );
}


void hbwin_SrvFunction( int argc, char** argv )
{
   HB_SYMBOL_UNUSED( argc );
   HB_SYMBOL_UNUSED( argv );

   ServiceStatus.dwServiceType        = SERVICE_WIN32;
   ServiceStatus.dwCurrentState       = SERVICE_START_PENDING;
   ServiceStatus.dwControlsAccepted   = SERVICE_ACCEPT_STOP | SERVICE_ACCEPT_SHUTDOWN;
   ServiceStatus.dwWin32ExitCode      = 0;
   ServiceStatus.dwServiceSpecificExitCode = 0;
   ServiceStatus.dwCheckPoint         = 0;
   ServiceStatus.dwWaitHint           = 0;

   hStatus = RegisterServiceCtrlHandler( sSrvName, (LPHANDLER_FUNCTION) hbwin_ControlHandler );

   if( hStatus == (SERVICE_STATUS_HANDLE) 0 )
   {
      HB_TRACE( HB_TR_DEBUG, ( "Error registering service\n" ) );
      return;
   }

   /* We report the running status to SCM. */
   ServiceStatus.dwCurrentState = SERVICE_RUNNING;
   SetServiceStatus( hStatus, &ServiceStatus );


	PHB_DYNS pDynSym;
	pDynSym = hb_dynsymFindName( sPrgFunction );
	if( pDynSym )
	{
      if( hb_vmRequestReenter() )
      {
   	   hb_vmPushSymbol( hb_dynsymSymbol( pDynSym ) );
   	   hb_vmPushNil();
   	   hb_vmDo(0);
  	      hb_vmRequestRestore();
	   }
	}

   return;

}


#endif

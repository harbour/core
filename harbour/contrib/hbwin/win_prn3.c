/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SETDEFAULTPRINTER()
 *
 * Copyright 2009 Viktor Szakats (harbour.01 syenar.hu) (based on MS sample code)
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

#include "hbsetup.h"

#if defined( HB_OS_WIN ) && \
    !( defined( __RSXNT__ ) || defined( __CYGWIN__ ) || defined( HB_OS_WIN_CE ) )

#include <windows.h>

#if defined( __LCC__ )
#   include <winspool.h>
#endif

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbwinuni.h"

static HB_BOOL hb_SetDefaultPrinter( LPCTSTR lpPrinterName )
{
   BOOL bFlag;
   OSVERSIONINFO osv;
   DWORD dwNeeded = 0;
   HANDLE hPrinter = NULL;
   PRINTER_INFO_2 * ppi2 = NULL;
   LPTSTR pBuffer = NULL;

   /* What version of Windows are you running? */
   osv.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osv );

   /* If Windows 95 or 98, use SetPrinter. */
   if( osv.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
      /* Open this printer so you can get information about it. */
      bFlag = OpenPrinter( ( LPTSTR ) lpPrinterName, &hPrinter, NULL );
      if( ! bFlag || ! hPrinter )
         return HB_FALSE;

      /* The first GetPrinter() tells you how big our buffer must
         be to hold ALL of PRINTER_INFO_2. Note that this will
         typically return FALSE. This only means that the buffer (the 3rd
         parameter) was not filled in. You do not want it filled in here. */
      SetLastError( 0 );
      bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );
      if( ! bFlag )
      {
         if( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) )
         {
            ClosePrinter( hPrinter );
            return HB_FALSE;
         }
      }

      /* Allocate enough space for PRINTER_INFO_2. */
      ppi2 = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );
      if( ! ppi2 )
      {
         ClosePrinter( hPrinter );
         return HB_FALSE;
      }

      /* The second GetPrinter() will fill in all the current information
         so that all you have to do is modify what you are interested in. */
      bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded );
      if( ! bFlag )
      {
         ClosePrinter( hPrinter );
         hb_xfree( ppi2 );
         return HB_FALSE;
      }

      /* Set default printer attribute for this printer. */
      ppi2->Attributes |= PRINTER_ATTRIBUTE_DEFAULT;
      bFlag = SetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, 0 );
      if( ! bFlag )
      {
         ClosePrinter( hPrinter );
         hb_xfree( ppi2 );
         return HB_FALSE;
      }

     /* Tell all open programs that this change occurred.
        Allow each program 1 second to handle this message. */
      SendMessageTimeout( HWND_BROADCAST, WM_SETTINGCHANGE, 0L, ( LPARAM ) ( LPCTSTR ) TEXT( "windows" ), SMTO_NORMAL, 1000, NULL );
   }
   /* If Windows NT, use the SetDefaultPrinter API for Windows 2000,
      or WriteProfileString for version 4.0 and earlier. */
   else if( osv.dwPlatformId == VER_PLATFORM_WIN32_NT )
   {
      if( osv.dwMajorVersion >= 5 ) /* Windows 2000 or later (use explicit call) */
      {
         HMODULE hWinSpool;
         typedef BOOL ( WINAPI * DEFPRINTER )( LPCTSTR ); /* stops warnings */
         DEFPRINTER fnSetDefaultPrinter;

         hWinSpool = LoadLibrary( TEXT( "winspool.drv" ) );
         if( ! hWinSpool )
            return HB_FALSE;
         fnSetDefaultPrinter = ( DEFPRINTER ) GetProcAddress( hWinSpool,
#if defined( UNICODE )
            "SetDefaultPrinterW" );
#else
            "SetDefaultPrinterA" );
#endif
         if( ! fnSetDefaultPrinter )
         {
            FreeLibrary( hWinSpool );
            return HB_FALSE;
         }

         bFlag = ( * fnSetDefaultPrinter )( lpPrinterName );
         FreeLibrary( hWinSpool );
         if( ! bFlag )
            return HB_FALSE;
      }
      else /* NT4.0 or earlier */
      {
         /* Open this printer so you can get information about it. */
         bFlag = OpenPrinter( ( LPTSTR ) lpPrinterName, &hPrinter, NULL );
         if( ! bFlag || ! hPrinter )
            return HB_FALSE;

         /* The first GetPrinter() tells you how big our buffer must
            be to hold ALL of PRINTER_INFO_2. Note that this will
            typically return FALSE. This only means that the buffer (the 3rd
            parameter) was not filled in. You do not want it filled in here. */
         SetLastError( 0 );
         bFlag = GetPrinter( hPrinter, 2, 0, 0, &dwNeeded );
         if( ! bFlag )
         {
            if( ( GetLastError() != ERROR_INSUFFICIENT_BUFFER ) || ( dwNeeded == 0 ) )
            {
               ClosePrinter( hPrinter );
               return HB_FALSE;
            }
         }

         /* Allocate enough space for PRINTER_INFO_2. */
         ppi2 = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );
         if( ! ppi2 )
         {
            ClosePrinter( hPrinter );
            return HB_FALSE;
         }

         /* The second GetPrinter() fills in all the current
            information. */
         bFlag = GetPrinter( hPrinter, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded );
         if( ( ! bFlag ) || ( ! ppi2->pDriverName ) || ( ! ppi2->pPortName ) )
         {
            ClosePrinter( hPrinter );
            hb_xfree( ppi2 );
            return HB_FALSE;
         }

         /* Allocate buffer big enough for concatenated string.
            String will be in form "printername,drivername,portname". */
         pBuffer = ( LPTSTR ) hb_xgrab( ( lstrlen( lpPrinterName ) +
                                          lstrlen( ppi2->pDriverName ) +
                                          lstrlen( ppi2->pPortName ) + 3 ) * sizeof( TCHAR ) );
         if( ! pBuffer )
         {
            ClosePrinter( hPrinter );
            hb_xfree( ppi2 );
            return HB_FALSE;
         }

         /* Build string in form "printername,drivername,portname". */
         lstrcpy( pBuffer, lpPrinterName );  lstrcat( pBuffer, TEXT( "," ) );
         lstrcat( pBuffer, ppi2->pDriverName );  lstrcat( pBuffer, TEXT( "," ) );
         lstrcat( pBuffer, ppi2->pPortName );

         /* Set the default printer in Win.ini and registry. */
         bFlag = WriteProfileString( TEXT( "windows" ), TEXT( "device" ), pBuffer );
         if( ! bFlag )
         {
            ClosePrinter( hPrinter );
            hb_xfree( ppi2 );
            hb_xfree( pBuffer );
            return HB_FALSE;
         }
      }

      /* Tell all open programs that this change occurred.
         Allow each app 1 second to handle this message. */
      SendMessageTimeout( HWND_BROADCAST, WM_SETTINGCHANGE, 0L, 0L, SMTO_NORMAL, 1000, NULL );
   }

   /* Clean up. */
   if( hPrinter )
      ClosePrinter( hPrinter );
   if( ppi2 )
      hb_xfree( ppi2 );
   if( pBuffer )
      hb_xfree( pBuffer );

   return HB_TRUE;
}

HB_FUNC( WIN_PRINTERSETDEFAULT )
{
   void * hPrinterName;
   HB_SIZE nLen;
   TCHAR pszPrinterName = HB_PARSTR( 1, &hPrinterName, &ulLen );

   hb_retl( ulLen > 0 ? hb_SetDefaultPrinter( pszPrinterName ) : HB_FALSE );
   hb_strfree( hPrinterName );
}

#endif

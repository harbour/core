/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour THarbourPrinter C++ Class for Harbour print support
 *
 * Copyright 2002 Luiz Rafael Culik<culikr@uol.com.br>
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

#if defined(HB_OS_WIN) && \
    !( defined(__RSXNT__) || defined(__CYGWIN__) || defined(HB_OS_WIN_CE) )

#include <windows.h>

#if defined(__LCC__)
#   include <winspool.h>
#endif

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapiitm.h"

#define MAXBUFFERSIZE 255

BOOL hb_isLegacyDevice( LPSTR pPrinterName )
{
   BOOL bLegacyDev = FALSE;
   int n = 0;
   LPSTR pszPrnDev[] =
      { "lpt1", "lpt2", "lpt3", "lpt4", "lpt5", "lpt6", "com1", "com2", "com3", "com4", NULL };
   while( pszPrnDev[ n ] && !bLegacyDev )
   {
      bLegacyDev = ( hb_strnicmp( pPrinterName, pszPrnDev[n], strlen( pszPrnDev[n] ) ) == 0 );
      n++;
   }
   return bLegacyDev;
}

BOOL hb_PrinterExists( LPSTR pPrinterName )
{
   BOOL Result = FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "hb_PrinterExists(%s)", pPrinterName ) );

   if( !strchr( pPrinterName, HB_OS_PATH_LIST_SEP_CHR ) && !hb_isLegacyDevice( pPrinterName ) )

   {                            /* Don't bother with test if '\' in string */
      if( hb_iswinnt() )
      {                         /* Use EnumPrinter() here because much faster than OpenPrinter() */
         DWORD Flags = PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS;
         ULONG needed = 0, returned = 0;

         EnumPrinters( Flags, NULL, 4, ( LPBYTE ) NULL, 0, &needed, &returned );
         if( needed )
         {
            PRINTER_INFO_4 * buffer4;
            PRINTER_INFO_4 * pPrinterEnum4;

            pPrinterEnum4 = buffer4 = ( PRINTER_INFO_4 * ) hb_xgrab( needed );

            if( EnumPrinters
                ( Flags, NULL, 4, ( LPBYTE ) pPrinterEnum4, needed, &needed, &returned ) )
            {
               ULONG a;

               for( a = 0; !Result && a < returned; a++, pPrinterEnum4++ )
               {
                  Result = strcmp( ( const char * ) pPrinterName,
                                   ( const char * ) pPrinterEnum4->pPrinterName ) == 0;
               }
            }
            hb_xfree( buffer4 );
         }
      }
      else
      {
         LPTSTR lpPrinterName = HB_TCHAR_CONVTO( pPrinterName );
         HANDLE hPrinter;

         if( OpenPrinter( lpPrinterName, &hPrinter, NULL ) )
         {
            ClosePrinter( hPrinter );
            Result = TRUE;
         }
         HB_TCHAR_FREE( lpPrinterName );
      }
   }
   return Result;
}

HB_FUNC( PRINTEREXISTS )
{
   hb_retl( ISCHAR( 1 ) && hb_PrinterExists( hb_parc( 1 ) ) );
}

BOOL hb_GetDefaultPrinter( char * pPrinterName, LPDWORD pdwBufferSize )
{
   BOOL Result = FALSE;
   OSVERSIONINFO osvi;

   osvi.dwOSVersionInfoSize = sizeof( OSVERSIONINFO );
   GetVersionEx( &osvi );

   if( osvi.dwPlatformId == VER_PLATFORM_WIN32_NT && osvi.dwMajorVersion >= 5 ) /* Windows 2000 or later */
   {
      typedef BOOL( WINAPI * DEFPRINTER ) ( LPSTR, LPDWORD );  /* stops warnings */
      DEFPRINTER fnGetDefaultPrinter;
      HMODULE hWinSpool = LoadLibrary( TEXT( "winspool.drv" ) );

      if( hWinSpool )
      {
         fnGetDefaultPrinter = ( DEFPRINTER ) GetProcAddress( hWinSpool, "GetDefaultPrinterA" );

         if( fnGetDefaultPrinter )
            Result = ( *fnGetDefaultPrinter )( pPrinterName, pdwBufferSize );

         FreeLibrary( hWinSpool );
      }
   }

   if( !Result )                /* Win9X and Windows NT 4.0 or earlier & 2000+ if necessary for some reason i.e. dll could not load!!!! */
   {
      DWORD dwSize = GetProfileStringA( "windows", "device", "", pPrinterName, *pdwBufferSize );

      if( dwSize && dwSize < *pdwBufferSize )
      {
         dwSize = 0;
         while( pPrinterName[ dwSize ] != '\0' && pPrinterName[ dwSize ] != ',' )
            dwSize++;

         pPrinterName[ dwSize ] = '\0';
         *pdwBufferSize = dwSize + 1;
         Result = TRUE;
      }
      else
         *pdwBufferSize = dwSize + 1;
   }

   if( !Result && osvi.dwPlatformId == VER_PLATFORM_WIN32_WINDOWS )
   {
/*
      This option should never be required but is included because of this article

          http://support.microsoft.com/kb/246772/en-us

      This option will not enumerate any network printers.

      From the SDK technical reference for EnumPrinters();

      If Level is 2 or 5, Name is a pointer to a null-terminated string that specifies
      the name of a server whose printers are to be enumerated.
      If this string is NULL, then the function enumerates the printers installed on the local machine.
*/

      DWORD dwNeeded, dwReturned;

      if( EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, NULL, 0, &dwNeeded, &dwReturned ) )
      {
         if( dwNeeded )
         {
            PRINTER_INFO_2 * ppi2 = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );

            if( EnumPrinters
                ( PRINTER_ENUM_DEFAULT, NULL, 2, ( LPBYTE ) ppi2, dwNeeded, &dwNeeded,
                  &dwReturned ) && dwReturned )
            {
               DWORD dwSize = ( DWORD ) lstrlen( ppi2->pPrinterName );

               if( dwSize && dwSize < *pdwBufferSize )
               {
                  HB_TCHAR_GETFROM( pPrinterName, ppi2->pPrinterName,
                                    lstrlen( ppi2->pPrinterName ) );
                  *pdwBufferSize = dwSize + 1;
                  Result = TRUE;
               }
            }
            hb_xfree( ppi2 );
         }
      }
   }
   return Result;
}

HB_FUNC( GETDEFAULTPRINTER )
{
   char szDefaultPrinter[ MAXBUFFERSIZE ];
   DWORD pdwBufferSize = sizeof( szDefaultPrinter );

   if( hb_GetDefaultPrinter( szDefaultPrinter, &pdwBufferSize ) )
      hb_retclen( szDefaultPrinter, pdwBufferSize - 1 );
   else
      hb_retc( NULL );
}

static DWORD IsPrinterError( HANDLE hPrinter )
{
   BOOL Result = -1;
   DWORD cByteNeeded;

   GetPrinter( hPrinter, 2, NULL, 0, &cByteNeeded );

   if( cByteNeeded )
   {
      PRINTER_INFO_2 * pPrinterInfo = ( PRINTER_INFO_2 * ) hb_xgrab( cByteNeeded );

      if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo, cByteNeeded, &cByteNeeded ) )
         Result = pPrinterInfo->Status;

      hb_xfree( pPrinterInfo );
   }

   return Result;
}

static BOOL GetJobs( HANDLE hPrinter, JOB_INFO_2 ** ppJobInfo, int * pcJobs )
{
   DWORD Result = FALSE;
   DWORD cByteNeeded;

   GetPrinter( hPrinter, 2, NULL, 0, &cByteNeeded );
   if( cByteNeeded )
   {
      PRINTER_INFO_2 * pPrinterInfo = ( PRINTER_INFO_2 * ) hb_xgrab( cByteNeeded );
      DWORD cByteUsed;

      if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo, cByteNeeded, &cByteUsed ) )
      {
         DWORD nReturned;

         EnumJobs( hPrinter, 0, pPrinterInfo->cJobs, 2, NULL, 0, ( LPDWORD ) &cByteNeeded, ( LPDWORD ) &nReturned );

         if( cByteNeeded )
         {
            JOB_INFO_2 * pJobStorage = ( JOB_INFO_2 * ) hb_xgrab( cByteNeeded );

            if( EnumJobs( hPrinter, 0, nReturned, 2, ( LPBYTE ) pJobStorage, cByteNeeded, ( LPDWORD ) &cByteUsed, ( LPDWORD ) &nReturned ) )
            {
               *pcJobs = nReturned;
               *ppJobInfo = pJobStorage;
               Result = TRUE;
            }
            else
               hb_xfree( pJobStorage );
         }
      }
      hb_xfree( pPrinterInfo );
   }
   return Result;
}

static DWORD IsPrinterErrorn( HANDLE hPrinter )
{
   DWORD dwError = IsPrinterError( hPrinter );  /* Just return the PrinterStatus */

   if( !dwError )
   {
      JOB_INFO_2 * pJobs;
      int cJobs;

      if( GetJobs( hPrinter, &pJobs, &cJobs ) )
      {
         int i;

         for( i = 0; ! dwError && i < cJobs; i++ )
         {
            if( pJobs[ i ].Status & JOB_STATUS_ERROR )
               dwError = ( DWORD ) -20;
            else if( pJobs[ i ].Status & JOB_STATUS_OFFLINE )
               dwError = ( DWORD ) -21;
            else if( pJobs[ i ].Status & JOB_STATUS_PAPEROUT )
               dwError = ( DWORD ) -22;
            else if( pJobs[ i ].Status & JOB_STATUS_BLOCKED_DEVQ )
               dwError = ( DWORD ) -23;
         }
         hb_xfree( pJobs );
      }
   }
   return dwError;
}

static DWORD hb_printerIsReadyn( char * pszPrinterName )
{
   DWORD dwPrinter = ( DWORD ) -1;
   HANDLE hPrinter;

   if( *pszPrinterName )
   {
      LPTSTR lpPrinterName = HB_TCHAR_CONVTO( pszPrinterName );
      if( OpenPrinter( lpPrinterName, &hPrinter, NULL ) )
      {
         dwPrinter = IsPrinterErrorn( hPrinter );
         CloseHandle( hPrinter );
      }
      HB_TCHAR_FREE( lpPrinterName );
   }
   return dwPrinter;
}

HB_FUNC( XISPRINTER )
{
   char szDefaultPrinter[ MAXBUFFERSIZE ], * pszPrinterName;
   DWORD pdwBufferSize = sizeof( szDefaultPrinter );

   pszPrinterName = hb_parc( 1 );
   if( !pszPrinterName )
   {
      hb_GetDefaultPrinter( szDefaultPrinter, &pdwBufferSize );
      pszPrinterName = szDefaultPrinter;
   }
   hb_retnl( hb_printerIsReadyn( pszPrinterName ) );
}

BOOL hb_GetPrinterNameByPort( char * pPrinterName, LPDWORD pdwBufferSize,
                              char * pPortName, BOOL bSubStr )
{
   BOOL Result = FALSE, bFound = FALSE;
   ULONG needed, returned;

   HB_TRACE( HB_TR_DEBUG, ( "hb_GetPrinterNameByPort(%p,%p)", pPrinterName, pPortName ) );

   EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 5, ( LPBYTE ) NULL, 0,
                 &needed, &returned );
   if( needed )
   {
      PRINTER_INFO_5 * pPrinterEnum, * buffer;
      pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed );

      if( EnumPrinters( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS, NULL, 5,
                        ( LPBYTE ) buffer, needed, &needed, &returned ) )
      {
         ULONG a;

         for( a = 0; a < returned && !bFound; a++, pPrinterEnum++ )
         {
            char * szPortName = HB_TCHAR_CONVFROM( pPrinterEnum->pPortName );

            if( bSubStr )
               bFound = ( hb_strnicmp( szPortName, pPortName, strlen( pPortName ) ) == 0 );
            else
               bFound = ( hb_stricmp( szPortName, pPortName ) == 0 );
            HB_TCHAR_FREE( szPortName );

            if( bFound )
            {
               char * szPrinterName = HB_TCHAR_CONVFROM( pPrinterEnum->pPrinterName );
               if( *pdwBufferSize >= strlen( szPrinterName ) + 1 )
               {
                  hb_strncpy( pPrinterName, szPrinterName, *pdwBufferSize );
                  Result = TRUE;
               }
               /* Store name length + \0 char for return */
               *pdwBufferSize = ( DWORD ) strlen( szPrinterName ) + 1;
               HB_TCHAR_FREE( szPrinterName );
            }
         }
      }
      hb_xfree( buffer );
   }
   return Result;
}

HB_FUNC( PRINTERPORTTONAME )
{
   char szDefaultPrinter[ MAXBUFFERSIZE ];
   DWORD pdwBufferSize = sizeof( szDefaultPrinter );

   if( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 &&
       hb_GetPrinterNameByPort( szDefaultPrinter, &pdwBufferSize, hb_parcx( 1 ),
                                ISLOG( 2 ) ? hb_parl( 2 ) : FALSE ) )
      hb_retc( szDefaultPrinter );
   else
      hb_retc_null();
}

#   define BIG_PRINT_BUFFER (1024*32)

LONG hb_PrintFileRaw( UCHAR * cPrinterName, UCHAR * cFileName, UCHAR * cDocName )
{
   UCHAR printBuffer[ BIG_PRINT_BUFFER ];
   HANDLE hPrinter, hFile;
   DOC_INFO_1 DocInfo;
   DWORD nRead, nWritten;
   LONG Result;
   LPTSTR lpPrinterName = HB_TCHAR_CONVTO( ( char * ) cPrinterName );

   if( OpenPrinter( lpPrinterName, &hPrinter, NULL ) != 0 )
   {
      LPTSTR lpDocName = HB_TCHAR_CONVTO( ( char * ) cDocName );
      DocInfo.pDocName = lpDocName;
      DocInfo.pOutputFile = NULL;
      DocInfo.pDatatype = TEXT( "RAW" );
      if( StartDocPrinter( hPrinter, 1, ( UCHAR * ) & DocInfo ) != 0 )
      {
         if( StartPagePrinter( hPrinter ) != 0 )
         {
            hFile =
               CreateFileA( ( char * ) cFileName, GENERIC_READ, 0, NULL, OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL, NULL );
            if( hFile != INVALID_HANDLE_VALUE )
            {
               while( ReadFile( hFile, printBuffer, BIG_PRINT_BUFFER, &nRead, NULL )
                      && ( nRead > 0 ) )
               {
                  if( printBuffer[ nRead - 1 ] == 26 )
                     nRead--;   /* Skip the EOF() character */

                  WritePrinter( hPrinter, printBuffer, nRead, &nWritten );
               }
               Result = 1;
               CloseHandle( hFile );
            }
            else
               Result = -6;
            EndPagePrinter( hPrinter );
         }
         else
            Result = -4;
         EndDocPrinter( hPrinter );
      }
      else
         Result = -3;
      HB_TCHAR_FREE( lpDocName );
      ClosePrinter( hPrinter );
   }
   else
      Result = -2;

   HB_TCHAR_FREE( lpPrinterName );

   return Result;
}

HB_FUNC( PRINTFILERAW )
{
   LONG Result = -1;

   if( ISCHAR( 1 ) && ISCHAR( 2 ) )
      Result = hb_PrintFileRaw( ( UCHAR * ) hb_parcx( 1 ) /* cPrinterName */,
                                ( UCHAR * ) hb_parcx( 2 ) /* cFileName */,
                                ISCHAR( 3 ) ? ( UCHAR * ) hb_parcx( 3 ) : ( UCHAR * ) hb_parcx( 2 ) /* cDocName */ );

   hb_retnl( Result );
}

HB_FUNC( GETPRINTERS )
{
   HANDLE hPrinter;
   DWORD Flags = PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS;
   BOOL bPrinterNamesOnly = TRUE;
   BOOL bLocalPrintersOnly;
   PRINTER_INFO_4 * buffer4, * pPrinterEnum4;
   PRINTER_INFO_5 * buffer, * pPrinterEnum;
   PRINTER_INFO_2 * pPrinterInfo2;
   ULONG needed = 0, returned = 0, a;
   PHB_ITEM SubItems, File, Port, Net, Driver, ArrayPrinter;
   char * pszData;

   ArrayPrinter = hb_itemNew( NULL );
   SubItems = hb_itemNew( NULL );
   File = hb_itemNew( NULL );
   Port = hb_itemNew( NULL );
   Net = hb_itemNew( NULL );
   Driver = hb_itemNew( NULL );

   hb_arrayNew( ArrayPrinter, 0 );

   buffer = NULL;
   HB_TRACE( HB_TR_DEBUG, ( "GETPRINTERS()" ) );

   if( ISLOG( 1 ) )
      bPrinterNamesOnly = ! hb_parl( 1 );

   bLocalPrintersOnly = ISLOG( 2 ) ? hb_parl( 2 ) : FALSE;

   if( hb_iswinnt() )
   {
      EnumPrinters( Flags, NULL, 4, ( LPBYTE ) NULL, 0, &needed, &returned );

      if( needed )
      {
         pPrinterEnum4 = buffer4 = ( PRINTER_INFO_4 * ) hb_xgrab( needed );

         if( EnumPrinters
             ( Flags, NULL, 4, ( LPBYTE ) pPrinterEnum4, needed, &needed, &returned ) )
         {
            if( bPrinterNamesOnly )
            {
               for( a = 0; a < returned; a++, pPrinterEnum4++ )
               {
                  if( ! bLocalPrintersOnly
                      || pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                  {
                     pszData = HB_TCHAR_CONVFROM( pPrinterEnum4->pPrinterName );
                     hb_itemPutC( File, pszData );
                     HB_TCHAR_FREE( pszData );
                     hb_arrayAddForward( ArrayPrinter, File );
                  }
               }
            }
            else
            {
               for( a = 0; a < returned; a++, pPrinterEnum4++ )
               {
                  if( ! bLocalPrintersOnly
                      || pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                  {
                     if( OpenPrinter( pPrinterEnum4->pPrinterName, &hPrinter, NULL ) )
                     {
                        GetPrinter( hPrinter, 2, NULL, 0, &needed );
                        if( needed )
                        {
                           pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( needed );
                           if( pPrinterInfo2 )
                           {
                              pszData = HB_TCHAR_CONVFROM( pPrinterEnum4->pPrinterName );
                              hb_itemPutC( File, pszData );
                              HB_TCHAR_FREE( pszData );

                              hb_arrayNew( SubItems, 0 );

                              if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo2, needed, &needed ) )
                              {
                                 pszData = HB_TCHAR_CONVFROM( pPrinterInfo2->pPortName );
                                 hb_itemPutC( Port, pszData );
                                 HB_TCHAR_FREE( pszData );
                                 pszData = HB_TCHAR_CONVFROM( pPrinterInfo2->pDriverName );
                                 hb_itemPutC( Driver, pszData );
                                 HB_TCHAR_FREE( pszData );
                              }
                              else
                              {
                                 hb_itemPutC( Port, "Error" );
                                 hb_itemPutC( Driver, "Error" );
                              }

                              if( pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                                 hb_itemPutC( Net, "LOCAL" );
                              else if( pPrinterEnum4->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                                 hb_itemPutC( Net, "NETWORK" );
                              else
                                 hb_itemPutC( Net, "ERROR" );

                              hb_arrayAddForward( SubItems, File );
                              hb_arrayAddForward( SubItems, Port );
                              hb_arrayAddForward( SubItems, Net );
                              hb_arrayAddForward( SubItems, Driver );
                              hb_arrayAddForward( ArrayPrinter, SubItems );
                              hb_xfree( pPrinterInfo2 );
                           }
                        }
                     }
                     CloseHandle( hPrinter );
                  }
               }
            }
         }
         hb_xfree( buffer4 );
      }
   }
   else
   {
      EnumPrinters( Flags, NULL, 5, ( LPBYTE ) buffer, 0, &needed, &returned );

      if( needed )
      {
         pPrinterEnum = buffer = ( PRINTER_INFO_5 * ) hb_xgrab( needed );

         if( EnumPrinters( Flags, NULL, 5, ( LPBYTE ) buffer, needed, &needed, &returned ) )
         {
            for( a = 0; a < returned; a++, pPrinterEnum++ )
            {
               if( !bLocalPrintersOnly || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
               {
                  if( bPrinterNamesOnly )
                  {
                     pszData = HB_TCHAR_CONVFROM( pPrinterEnum->pPrinterName );
                     hb_itemPutC( File, pszData );
                     HB_TCHAR_FREE( pszData );
                     hb_arrayAddForward( ArrayPrinter, File );
                  }
                  else
                  {
                     /* Tony (ABC)   11/1/2005        1:40PM. */
                     for( a = 0; a < returned; a++, pPrinterEnum++ )
                     {
                        if( ! bLocalPrintersOnly
                            || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                        {
                           if( OpenPrinter( pPrinterEnum->pPrinterName, &hPrinter, NULL ) )
                           {
                              GetPrinter( hPrinter, 2, NULL, 0, &needed );
                              if( needed )
                              {
                                 pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( needed );
                                 if( pPrinterInfo2 )
                                 {
                                    hb_arrayNew( SubItems, 0 );
                                    pszData = HB_TCHAR_CONVFROM( pPrinterEnum->pPrinterName );
                                    hb_itemPutC( File, pszData );
                                    HB_TCHAR_FREE( pszData );

                                    if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo2, needed, &needed ) )
                                    {
                                       pszData = HB_TCHAR_CONVFROM( pPrinterInfo2->pPortName );
                                       hb_itemPutC( Port, pszData );
                                       HB_TCHAR_FREE( pszData );
                                       pszData = HB_TCHAR_CONVFROM( pPrinterInfo2->pDriverName );
                                       hb_itemPutC( Driver, pszData );
                                       HB_TCHAR_FREE( pszData );
                                    }
                                    else
                                    {
                                       hb_itemPutC( Port, "Error" );
                                       hb_itemPutC( Driver, "Error" );
                                    }

                                    if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                                       hb_itemPutC( Net, "LOCAL" );
                                    else if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                                       hb_itemPutC( Net, "NETWORK" );
                                    else
                                       hb_itemPutC( Net, "ERROR" );

                                    hb_arrayAddForward( SubItems, File );
                                    hb_arrayAddForward( SubItems, Port );
                                    hb_arrayAddForward( SubItems, Net );
                                    hb_arrayAddForward( SubItems, Driver );
                                    hb_arrayAddForward( ArrayPrinter, SubItems );
                                    hb_xfree( pPrinterInfo2 );
                                 }
                              }
                           }
                           CloseHandle( hPrinter );
                        }
                     }
                     /* Tony (ABC)   11/1/2005        1:40PM. Old Code... Justo in case. */
#if 0
                       hb_arrayNew( SubItems, 0 );
                       hb_itemPutC( File, pPrinterEnum->pPrinterName );
                       hb_itemPutC( Port, pPrinterEnum->pPortName );

                       if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL)
                          hb_itemPutC( Net,"LOCAL" );
                       else if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK)
                          hb_itemPutC( Net, "NETWORK" );
                       else
                          hb_itemPutC( Net, "ERROR" );

                       hb_arrayAddForward( SubItems, File );
                       hb_arrayAddForward( SubItems, Port );
                       hb_arrayAddForward( SubItems, Net );
                       hb_arrayAddForward( ArrayPrinter, SubItems );
#endif
                  }
               }
            }
         }
         hb_xfree( buffer );
      }
   }
   hb_itemReturnForward( ArrayPrinter );

   hb_itemRelease( ArrayPrinter );
   hb_itemRelease( SubItems );
   hb_itemRelease( File );
   hb_itemRelease( Port );
   hb_itemRelease( Net );
   hb_itemRelease( Driver );
}

#endif

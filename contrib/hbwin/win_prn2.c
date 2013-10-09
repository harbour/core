/*
 * Harbour Project source code:
 * Harbour Windows Printing support functions
 *
 * Copyright 2009 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2002 Luiz Rafael Culik <culikr@uol.com.br>
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

#include "hbwin.h"
#include "hbwapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#if ! defined( HB_OS_WIN_CE )
   #include <winspool.h>
#endif

#define _ENUMPRN_FLAGS_  ( PRINTER_ENUM_LOCAL | PRINTER_ENUM_CONNECTIONS )

#if ! defined( HB_OS_WIN_CE )
static HB_BOOL hb_IsLegacyDevice( const char * pszPrinterName )
{
   static const char * s_pszPrnDev[] = { "lpt1", "lpt2", "lpt3", "lpt4", "lpt5", "lpt6", "com1", "com2", "com3", "com4", NULL };
   int i;

   for( i = 0; s_pszPrnDev[ i ]; ++i )
   {
      if( hb_strnicmp( pszPrinterName, s_pszPrnDev[ i ], strlen( s_pszPrnDev[ i ] ) ) == 0 )
         return HB_TRUE;
   }

   return HB_FALSE;
}
#endif

HB_FUNC( WIN_PRINTEREXISTS )
{
   HB_BOOL bResult = HB_FALSE;

#if ! defined( HB_OS_WIN_CE )
   if( HB_ISCHAR( 1 ) )
   {
      const char * pszPrinterName = hb_parc( 1 );

      /* Don't bother with test if '\' in string */
      if( ! strchr( pszPrinterName, HB_OS_PATH_LIST_SEP_CHR ) && ! hb_IsLegacyDevice( pszPrinterName ) )
      {
         DWORD dwNeeded = 0, dwReturned = 0;

         EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) NULL, 0, &dwNeeded, &dwReturned );
         if( dwNeeded )
         {
            PRINTER_INFO_5 * pPrinterEnumBak;
            PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = ( PRINTER_INFO_5 * ) hb_xgrab( dwNeeded );

            if( EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) pPrinterEnum, dwNeeded, &dwNeeded, &dwReturned ) )
            {
               PHB_ITEM pTemp = hb_itemNew( NULL );
               DWORD i;

               for( i = 0; ! bResult && i < dwReturned; ++i, ++pPrinterEnum )
               {
                  HB_ITEMPUTSTR( pTemp, pPrinterEnum->pPrinterName );
                  bResult = ( strcmp( pszPrinterName, hb_itemGetCPtr( pTemp ) ) == 0 );
               }

               hb_itemRelease( pTemp );
            }

            hb_xfree( pPrinterEnumBak );
         }
      }
   }
#endif

   hb_retl( bResult );
}

static void hb_GetDefaultPrinter( PHB_ITEM pPrinterName )
{
#if ! defined( HB_OS_WIN_CE )
   HB_BOOL bResult = HB_FALSE;

   hb_itemPutC( pPrinterName, NULL );

   if( hb_iswin2k() ) /* Windows 2000 or later */
   {
      typedef BOOL( WINAPI * DEFPRINTER ) ( LPTSTR, LPDWORD );
      DEFPRINTER fnGetDefaultPrinter;
      HMODULE hWinSpool = hbwapi_LoadLibrarySystem( TEXT( "winspool.drv" ) );

      if( hWinSpool )
      {
         fnGetDefaultPrinter = ( DEFPRINTER ) GetProcAddress( hWinSpool,
             HB_WINAPI_FUNCTION_NAME( "GetDefaultPrinter" ) );

         if( fnGetDefaultPrinter )
         {
            TCHAR lpPrinterName[ 256 ];
            DWORD dwSize = HB_SIZEOFARRAY( lpPrinterName ) - 1;

            bResult = ( *fnGetDefaultPrinter )( lpPrinterName, &dwSize );

            HB_ITEMPUTSTR( pPrinterName, lpPrinterName );
         }

         FreeLibrary( hWinSpool );
      }
   }

   if( ! bResult ) /* Win9x and Windows NT 4.0 or earlier & 2000+ if necessary for some reason i.e. dll could not load! */
   {
      TCHAR lpPrinterName[ 256 ];

      DWORD dwSize = GetProfileString( TEXT( "windows" ), TEXT( "device" ), TEXT( "" ), lpPrinterName, ( DWORD ) HB_SIZEOFARRAY( lpPrinterName ) - 1 );

      if( dwSize && dwSize < HB_SIZEOFARRAY( lpPrinterName ) )
      {
         dwSize = 0;
         while( lpPrinterName[ dwSize ] != '\0' && lpPrinterName[ dwSize ] != ',' )
            dwSize++;
         lpPrinterName[ dwSize ] = '\0';

         bResult = HB_TRUE;

         HB_ITEMPUTSTRLEN( pPrinterName, lpPrinterName, dwSize );
      }
   }

   if( ! bResult && hb_iswin9x() )
   {
      /* This option should never be required but is included because of this article
            https://support.microsoft.com/kb/246772/en-us

         This option will not enumerate any network printers.

         From the SDK technical reference for EnumPrinters();
            If Level is 2 or 5, Name is a pointer to a null-terminated string that specifies
            the name of a server whose printers are to be enumerated.
            If this string is NULL, then the function enumerates the printers installed on the local machine.
       */
      DWORD dwNeeded = 0, dwReturned = 0;

      if( EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, NULL, 0, &dwNeeded, &dwReturned ) )
      {
         if( dwNeeded )
         {
            PRINTER_INFO_2 * pPrinterInfo = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );

            if( EnumPrinters( PRINTER_ENUM_DEFAULT, NULL, 2, ( LPBYTE ) pPrinterInfo, dwNeeded, &dwNeeded, &dwReturned ) && dwReturned )
               HB_ITEMPUTSTR( pPrinterName, pPrinterInfo->pPrinterName );

            hb_xfree( pPrinterInfo );
         }
      }
   }
#else
   hb_itemPutC( pPrinterName, NULL );
#endif
}

HB_FUNC( WIN_PRINTERGETDEFAULT )
{
   PHB_ITEM pPrinterName = hb_itemNew( NULL );

   hb_GetDefaultPrinter( pPrinterName );

   hb_itemReturnRelease( pPrinterName );
}

#if ! defined( HB_OS_WIN_CE )
static HB_BOOL hb_GetJobs( HANDLE hPrinter, JOB_INFO_2 ** ppJobInfo, long * plJobs )
{
   HB_BOOL bResult = HB_FALSE;
   DWORD dwByteNeeded = 0;

   GetPrinter( hPrinter, 2, NULL, 0, &dwByteNeeded );
   if( dwByteNeeded )
   {
      PRINTER_INFO_2 * pPrinterInfo = ( PRINTER_INFO_2 * ) hb_xgrab( dwByteNeeded );
      DWORD dwByteUsed = 0;

      if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo, dwByteNeeded, &dwByteUsed ) )
      {
         DWORD dwReturned = 0;

         EnumJobs( hPrinter, 0, pPrinterInfo->cJobs, 2, NULL, 0, &dwByteNeeded, &dwReturned );

         if( dwByteNeeded )
         {
            JOB_INFO_2 * pJobStorage = ( JOB_INFO_2 * ) hb_xgrab( dwByteNeeded );

            if( EnumJobs( hPrinter, 0, dwReturned, 2, ( LPBYTE ) pJobStorage, dwByteNeeded, &dwByteUsed, &dwReturned ) )
            {
               *plJobs = ( long ) dwReturned;
               *ppJobInfo = pJobStorage;
               bResult = HB_TRUE;
            }
            else
               hb_xfree( pJobStorage );
         }
      }
      hb_xfree( pPrinterInfo );
   }

   return bResult;
}
#endif

HB_FUNC( WIN_PRINTERSTATUS )
{
   long nStatus = -1;

#if ! defined( HB_OS_WIN_CE )
   PHB_ITEM pPrinterName = hb_itemParam( 1 );

   if( hb_itemGetCLen( pPrinterName ) == 0 )
      hb_GetDefaultPrinter( pPrinterName );

   if( hb_itemGetCLen( pPrinterName ) > 0 )
   {
      void * hPrinterName;
      LPCTSTR lpPrinterName = HB_ITEMGETSTR( pPrinterName, &hPrinterName, NULL );
      HANDLE hPrinter;

      if( OpenPrinter( ( LPTSTR ) lpPrinterName, &hPrinter, NULL ) )
      {
         DWORD dwByteNeeded = 0;

         GetPrinter( hPrinter, 2, NULL, 0, &dwByteNeeded );

         if( dwByteNeeded )
         {
            PRINTER_INFO_2 * pPrinterInfo = ( PRINTER_INFO_2 * ) hb_xgrab( dwByteNeeded );

            if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo, dwByteNeeded, &dwByteNeeded ) )
               nStatus = ( long ) pPrinterInfo->Status;

            hb_xfree( pPrinterInfo );
         }

         if( nStatus == 0 )
         {
            JOB_INFO_2 * pJobs;
            long lJobs = 0;

            if( hb_GetJobs( hPrinter, &pJobs, &lJobs ) )
            {
               long i;

               for( i = 0; nStatus == 0 && i < lJobs; ++i )
               {
                  if( pJobs[ i ].Status & JOB_STATUS_ERROR )
                     nStatus = -20;
                  else if( pJobs[ i ].Status & JOB_STATUS_OFFLINE )
                     nStatus = -21;
                  else if( pJobs[ i ].Status & JOB_STATUS_PAPEROUT )
                     nStatus = -22;
                  else if( pJobs[ i ].Status & JOB_STATUS_BLOCKED_DEVQ )
                     nStatus = -23;
               }
               hb_xfree( pJobs );
            }
         }

         CloseHandle( hPrinter );
      }

      hb_strfree( hPrinterName );
   }

   hb_itemRelease( pPrinterName );
#endif

   hb_retnl( nStatus );
}

HB_FUNC( WIN_PRINTERPORTTONAME )
{
   /* Set default return value */
   hb_retc_null();

#if ! defined( HB_OS_WIN_CE )
   if( hb_parclen( 1 ) > 0 )
   {
      DWORD dwNeeded = 0, dwReturned = 0;

      EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) NULL, 0, &dwNeeded, &dwReturned );
      if( dwNeeded )
      {
         PRINTER_INFO_5 * pPrinterEnumBak;
         PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = ( PRINTER_INFO_5 * ) hb_xgrab( dwNeeded );

         if( EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) pPrinterEnum, dwNeeded, &dwNeeded, &dwReturned ) )
         {
            const char * pszPortNameFind = hb_parc( 1 );
            HB_BOOL bSubStr = hb_parl( 2 );
            HB_BOOL bFound = HB_FALSE;
            PHB_ITEM pTemp = hb_itemNew( NULL );
            DWORD i;

            for( i = 0; i < dwReturned && ! bFound; ++i, ++pPrinterEnum )
            {
               HB_ITEMPUTSTR( pTemp, pPrinterEnum->pPortName );

               if( bSubStr )
                  bFound = ( hb_strnicmp( hb_itemGetCPtr( pTemp ), pszPortNameFind, strlen( pszPortNameFind ) ) == 0 );
               else
                  bFound = ( hb_stricmp( hb_itemGetCPtr( pTemp ), pszPortNameFind ) == 0 );

               if( bFound )
                  HB_RETSTR( pPrinterEnum->pPrinterName );
            }

            hb_itemRelease( pTemp );
         }

         hb_xfree( pPrinterEnumBak );
      }
   }
#endif
}

#define HB_PRINT_BUFFER_SIZE  ( 32 * 1024 )

HB_FUNC( WIN_PRINTFILERAW )
{
   int iResult = -1;

#if ! defined( HB_OS_WIN_CE )
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * pszFileName = hb_parc( 2 );

      HANDLE hPrinter;
      void * hDeviceName;
      LPCTSTR lpDeviceName = HB_PARSTR( 1, &hDeviceName, NULL );

      if( OpenPrinter( ( LPTSTR ) lpDeviceName, &hPrinter, NULL ) != 0 )
      {
         void * hDocName;

         DOC_INFO_1 DocInfo;
         DocInfo.pDocName = ( LPTSTR ) HB_PARSTR( HB_ISCHAR( 3 ) ? 3 : 2, &hDocName, NULL );
         DocInfo.pOutputFile = NULL;
         DocInfo.pDatatype = ( LPTSTR ) TEXT( "RAW" );
         if( StartDocPrinter( hPrinter, 1, ( LPBYTE ) &DocInfo ) != 0 )
         {
            if( StartPagePrinter( hPrinter ) != 0 )
            {
               HB_FHANDLE fhnd = hb_fsOpen( pszFileName, FO_READ | FO_SHARED );

               if( fhnd != FS_ERROR )
               {
                  HB_BYTE * pbyBuffer = ( HB_BYTE * ) hb_xgrab( HB_PRINT_BUFFER_SIZE );
                  HB_SIZE nRead;

                  iResult = 1;

                  while( ( nRead = hb_fsReadLarge( fhnd, pbyBuffer, HB_PRINT_BUFFER_SIZE ) ) > 0 )
                  {
                     DWORD nWritten = 0;

#if 0
                     /* TOFIX: This check seems wrong for any input files
                               larger than our read buffer, in such case it
                               will strip Chr( 26 ) from inside the file, which
                               means it will corrupt it. [vszakats] */
                     if( pbyBuffer[ nRead - 1 ] == 26 )
                        nRead--;   /* Skip the Eof() character */
#endif

                     while( nWritten < nRead )
                     {
                        DWORD dwWritten = 0;
                        if( ! WritePrinter( hPrinter, &pbyBuffer[ nWritten ],
                                            ( DWORD ) ( nRead - nWritten ),
                                            &dwWritten ) )
                        {
                           iResult = -7;
                           break;
                        }
                        else if( dwWritten == 0 )
                        {
                           iResult = -8;
                           break;
                        }
                        nWritten += dwWritten;
                     }
                     if( nWritten < nRead )
                        break;
                  }

                  hb_fsClose( fhnd );
                  hb_xfree( pbyBuffer );
               }
               else
                  iResult = -6;
               EndPagePrinter( hPrinter );
            }
            else
               iResult = -4;
            EndDocPrinter( hPrinter );
         }
         else
            iResult = -3;

         ClosePrinter( hPrinter );

         hb_strfree( hDocName );
      }
      else
         iResult = -2;

      hb_strfree( hDeviceName );
   }
#endif

   hb_retni( iResult );
}

/* Positions for win_printerList() array */

#define HB_WINPRN_NAME    1
#define HB_WINPRN_PORT    2
#define HB_WINPRN_TYPE    3
#define HB_WINPRN_DRIVER  4
#define HB_WINPRN_SHARE   5
#define HB_WINPRN_SERVER  6
#define HB_WINPRN_LEN_    6

HB_FUNC( WIN_PRINTERLIST )
{
   PHB_ITEM pPrinterArray = hb_itemArrayNew( 0 );

#if ! defined( HB_OS_WIN_CE )
   HB_BOOL bPrinterNamesOnly = ! hb_parl( 1 );
   HB_BOOL bLocalPrintersOnly = hb_parl( 2 );
   DWORD dwNeeded = 0, dwReturned = 0, i;

   EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) NULL, 0, &dwNeeded, &dwReturned );
   if( dwNeeded )
   {
      PRINTER_INFO_5 * pPrinterEnumBak;
      PRINTER_INFO_5 * pPrinterEnum = pPrinterEnumBak = ( PRINTER_INFO_5 * ) hb_xgrab( dwNeeded );

      if( EnumPrinters( _ENUMPRN_FLAGS_, NULL, 5, ( LPBYTE ) pPrinterEnum, dwNeeded, &dwNeeded, &dwReturned ) )
      {
         PHB_ITEM pTempItem = hb_itemNew( NULL );

         for( i = 0; i < dwReturned; ++i, ++pPrinterEnum )
         {
            if( ! bLocalPrintersOnly || pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
            {
               if( bPrinterNamesOnly )
                  hb_arrayAddForward( pPrinterArray, HB_ITEMPUTSTR( pTempItem, pPrinterEnum->pPrinterName ) );
               else
               {
                  HANDLE hPrinter;

                  if( OpenPrinter( pPrinterEnum->pPrinterName, &hPrinter, NULL ) )
                  {
                     GetPrinter( hPrinter, 2, NULL, 0, &dwNeeded );
                     if( dwNeeded )
                     {
                        hb_arrayNew( pTempItem, HB_WINPRN_LEN_ );

                        HB_ARRAYSETSTR( pTempItem, HB_WINPRN_NAME, pPrinterEnum->pPrinterName );

                        {
                           PRINTER_INFO_2 * pPrinterInfo2 = ( PRINTER_INFO_2 * ) hb_xgrab( dwNeeded );

                           if( GetPrinter( hPrinter, 2, ( LPBYTE ) pPrinterInfo2, dwNeeded, &dwNeeded ) )
                           {
                              HB_ARRAYSETSTR( pTempItem, HB_WINPRN_PORT, pPrinterInfo2->pPortName );
                              HB_ARRAYSETSTR( pTempItem, HB_WINPRN_DRIVER, pPrinterInfo2->pDriverName );
                              HB_ARRAYSETSTR( pTempItem, HB_WINPRN_SHARE, pPrinterInfo2->pShareName );
                              HB_ARRAYSETSTR( pTempItem, HB_WINPRN_SERVER, pPrinterInfo2->pServerName );
                           }
                           else
                           {
                              hb_arraySetC( pTempItem, HB_WINPRN_PORT, NULL );
                              hb_arraySetC( pTempItem, HB_WINPRN_DRIVER, NULL );
                              hb_arraySetC( pTempItem, HB_WINPRN_SHARE, NULL );
                              hb_arraySetC( pTempItem, HB_WINPRN_SERVER, NULL );
                           }

                           hb_xfree( pPrinterInfo2 );
                        }

                        if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_LOCAL )
                           hb_arraySetC( pTempItem, HB_WINPRN_TYPE, "LOCAL" );
                        else if( pPrinterEnum->Attributes & PRINTER_ATTRIBUTE_NETWORK )
                           hb_arraySetC( pTempItem, HB_WINPRN_TYPE, "NETWORK" );
                        else
                           hb_arraySetC( pTempItem, HB_WINPRN_TYPE, NULL );

                        hb_arrayAddForward( pPrinterArray, pTempItem );
                     }
                  }
                  CloseHandle( hPrinter );
               }
            }
         }

         hb_itemRelease( pTempItem );
      }
      hb_xfree( pPrinterEnumBak );
   }
#endif

   hb_itemReturnRelease( pPrinterArray );
}

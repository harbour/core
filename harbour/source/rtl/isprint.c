/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ISPRINTER() function
 *
 * Copyright 1999-2002 Viktor Szakats <viktor.szakats@syenar.hu>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *    ISPRINTER() support for win32
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"

#if defined(HB_OS_WIN_32) && !defined(__RSXNT__)

   #include <stdio.h>
   #include <malloc.h>
   #include <winspool.h>

   static BOOL IsPrinterError(HANDLE hPrinter);

   static BOOL GetJobs(HANDLE hPrinter,
                       JOB_INFO_2 **ppJobInfo,
                       int *pcJobs,
                       DWORD *pStatus);

#endif

BOOL hb_printerIsReady( char * pszPrinterName )
{
   BOOL bIsPrinter;

#if defined(HB_OS_DOS)

   /* NOTE: DOS specific solution, using BIOS interrupt */

   {
      USHORT uiPort;
      
      if( hb_strnicmp( pszPrinterName, "PRN", 3 ) == 0 )
      {
         union REGS regs;
      
         regs.h.ah = 2;
         regs.HB_XREGS.dx = 0; /* LPT1 */
      
         HB_DOS_INT86( 0x17, &regs, &regs );
      
         bIsPrinter = ( regs.h.ah == 0x90 );
      }
      else if( strlen( pszPrinterName ) >= 4 && 
               hb_strnicmp( pszPrinterName, "LPT", 3 ) == 0 && 
               ( uiPort = atoi( pszPrinterName + 3 ) ) > 0 )
      {
         union REGS regs;
      
         regs.h.ah = 2;
         regs.HB_XREGS.dx = uiPort - 1;
      
         HB_DOS_INT86( 0x17, &regs, &regs );
      
         bIsPrinter = ( regs.h.ah == 0x90 );
      }
      else
         bIsPrinter = FALSE;
   }

#elif defined(HB_OS_WIN_32) && !defined(__RSXNT__)

   {
      HANDLE hPrinter;

      OpenPrinter( pszPrinterName, &hPrinter, NULL );

      bIsPrinter = ! IsPrinterError( hPrinter );
   }

#else

   /* NOTE: Platform independent method, at least it will compile and run
            on any platform, but the result may not be the expected one,
            since Unix/Linux doesn't support LPT/COM by nature, other OSs
            may not reflect the actual physical presence of the printer when
            trying to open it, since we are talking to the spooler.
            [vszakats] */

   {
      FHANDLE fhnd = hb_fsOpen( ( BYTE * ) pszPrinterName, FO_WRITE | FO_SHARED | FO_PRIVATE );
      bIsPrinter = ( fhnd != FS_ERROR );
      hb_fsClose( fhnd );
   }

#endif

   return bIsPrinter;
}

/* NOTE: The parameter is an extension over CA-Cl*pper, it's also supported
         by Xbase++. [vszakats] */

HB_FUNC( ISPRINTER )
{
   hb_retl( hb_printerIsReady( ISCHAR( 1 ) ? hb_parc( 1 ) : "LPT1" ) );
}

/* The code below does the check for the printer under Win32 */

#if defined(HB_OS_WIN_32) && !defined(__RSXNT__)

static BOOL IsPrinterError( HANDLE hPrinter )
{
    JOB_INFO_2  *pJobs;
    int         cJobs,
                i;
    DWORD       dwPrinterStatus;

    /*
     *  Get the state information for the Printer Queue and
     *  the jobs in the Printer Queue.
     */
    if (!GetJobs(hPrinter, &pJobs, &cJobs, &dwPrinterStatus))
        return FALSE;

    /*
     *  If the Printer reports an error, believe it.
     */
    if (dwPrinterStatus &
        (PRINTER_STATUS_ERROR |
         PRINTER_STATUS_PAPER_JAM |
         PRINTER_STATUS_PAPER_OUT |
         PRINTER_STATUS_PAPER_PROBLEM |
         PRINTER_STATUS_OUTPUT_BIN_FULL |
         PRINTER_STATUS_NOT_AVAILABLE |
         PRINTER_STATUS_NO_TONER |
         PRINTER_STATUS_OUT_OF_MEMORY |
         PRINTER_STATUS_OFFLINE |
         PRINTER_STATUS_DOOR_OPEN))
    {
        return TRUE;
    }

    /*
     *  Find the Job in the Queue that is printing.
     */
    for (i=0; i < cJobs; i++)
    {
        if (pJobs[i].Status & JOB_STATUS_PRINTING)
        {
            /*
             *  If the job is in an error state,
             *  report an error for the printer.
             *  Code could be inserted here to
             *  attempt an interpretation of the
             *  pStatus member as well.
             */
            if (pJobs[i].Status &
                (JOB_STATUS_ERROR |
                JOB_STATUS_OFFLINE |
                JOB_STATUS_PAPEROUT |
                JOB_STATUS_BLOCKED_DEVQ))
            {
                return TRUE;
            }
        }
    }

    /*
     *  No error condition.
     */
    return FALSE;

}

static BOOL GetJobs(HANDLE hPrinter,        /* Handle to the printer. */
                JOB_INFO_2 **ppJobInfo, /* Pointer to be filled.  */
                int *pcJobs,            /* Count of jobs filled.  */
                DWORD *pStatus)         /* Print Queue status.    */
{

   DWORD cByteNeeded;
   DWORD nReturned;
   DWORD cByteUsed;
   JOB_INFO_2 * pJobStorage;
   PRINTER_INFO_2 * pPrinterInfo;

/* Get the buffer size needed. */
    if (!GetPrinter(hPrinter, 2, NULL, 0, &cByteNeeded))
    {
        if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
            return FALSE;
    }

    pPrinterInfo = (PRINTER_INFO_2 *)malloc(cByteNeeded);
    if (!(pPrinterInfo))
        /* Failure to allocate memory. */
        return FALSE;

    /* Get the printer information. */
    if (!GetPrinter(hPrinter,
            2,
            (LPBYTE)pPrinterInfo,
            cByteNeeded,
            &cByteUsed))
    {
        /* Failure to access the printer. */
        free(pPrinterInfo);
        return FALSE;
    }

    /* Get job storage space. */
    if (!EnumJobs(hPrinter,
            0,
            pPrinterInfo->cJobs,
            2,
            NULL,
            0,
            (LPDWORD)&cByteNeeded,
            (LPDWORD)&nReturned))
    {
        if (GetLastError() != ERROR_INSUFFICIENT_BUFFER)
        {
            free(pPrinterInfo);
            return FALSE;
        }
    }

    pJobStorage = (JOB_INFO_2 *)malloc(cByteNeeded);
    if (!pJobStorage)
    {
        /* Failure to allocate Job storage space. */
        free(pPrinterInfo);
        return FALSE;
    }

    ZeroMemory(pJobStorage, cByteNeeded);

    /* Get the list of jobs. */
    if (!EnumJobs(hPrinter,
            0,
            pPrinterInfo->cJobs,
            2,
            (LPBYTE)pJobStorage,
            cByteNeeded,
            (LPDWORD)&cByteUsed,
            (LPDWORD)&nReturned))
    {
        free(pPrinterInfo);
        free(pJobStorage);
        return FALSE;
    }

    /*
     *  Return the information.
     */
    *pcJobs = nReturned;
    *pStatus = pPrinterInfo->Status;
    *ppJobInfo = pJobStorage;
    free(pPrinterInfo);

    return TRUE;
}

#endif

/*
 * IsPrinter() function
 *
 * Copyright 1999-2016 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapifs.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif

HB_BOOL hb_printerIsReady( const char * pszPrinterName )
{
   HB_BOOL bIsPrinter;

#if defined( HB_OS_DOS )

   /* NOTE: MS-DOS specific solution, using BIOS interrupt */

   {
      int iPort;

      if( pszPrinterName == NULL )
         pszPrinterName = "LPT1";

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
               ( iPort = atoi( pszPrinterName + 3 ) ) > 0 )
      {
         union REGS regs;

         regs.h.ah = 2;
         regs.HB_XREGS.dx = iPort - 1;

         HB_DOS_INT86( 0x17, &regs, &regs );

         bIsPrinter = ( regs.h.ah == 0x90 );
      }
      else
         bIsPrinter = HB_FALSE;
   }

#else

   /* NOTE: Platform independent method, at least it will compile and run
            on any platform, but the result may not be the expected one,
            since Unix/Linux doesn't support LPT/COM by nature, other OSs
            may not reflect the actual physical presence of the printer when
            trying to open it, since we are talking to the spooler.
            [vszakats] */

   {
      PHB_FILE pFile;

      if( pszPrinterName == NULL )
#if defined( HB_OS_UNIX )
         pszPrinterName = "/dev/lp0";
#else
         pszPrinterName = "LPT1";
#endif

      pFile = hb_fileExtOpen( pszPrinterName, NULL,
                              FXO_APPEND | FO_WRITE | FO_SHARED | FO_PRIVATE,
                              NULL, NULL );
      bIsPrinter = ( pFile != NULL );
      if( bIsPrinter )
         hb_fileClose( pFile );
   }

#endif

   return bIsPrinter;
}

/* Contrary to popular beliefs and such (mis)feature implemented
   in a Harbour derivative xHarbour, [hb_]IsPrinter() functions
   are only meant to work on direct _devices_ (f.e. LPT1:, \\server\queue,
   tcp:localhost:9100, /dev/lp0, etc...). Those that are writable
   just like a stream. Of these two functions, IsPrinter() will
   always use the same, OS-specific predefined device name, just
   like in Cl*pper, for compatiblity. IOW, it doesn't accept
   a parameter. See source code above what the predefined device
   names are. hb_IsPrinter() _will_ accept such device name as its
   1st and only parameter. However, because devices are typically
   virtualized by modern OSes, they may not be offering the exact
   Cl*pper behavior experienced under MS-DOS. To get the latter,
   an MS-DOS build of Harbour is required. Because direct device
   names don't have anything to do with high-level graphical
   printers (and their names may even collide with valid device
   names), such printer names are _not_ handled by these functions.
   For such feature, explore the appropriate OS/desktop specific
   printing libraries/APIs (or create a portable, printable document,
   like a .pdf). As for these ones, regard them as legacy functions
   for compatibility. [vszakats] */

HB_FUNC( HB_ISPRINTER )
{
   hb_retl( hb_printerIsReady( hb_parc( 1 ) ) );
}

HB_FUNC( ISPRINTER )
{
   hb_retl( hb_printerIsReady( NULL ) );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ISPRINTER() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapifs.h"

/* NOTE: The parameter is an extension over CA-Cl*pper, it's also supported
         by Xbase++. [vszakats] */

HB_FUNC( ISPRINTER )
{
   char * pszDOSPort = ( ISCHAR( 1 ) && hb_parclen( 1 ) >= 4 ) ? hb_parc( 1 ) : "LPT1";
   USHORT uiPort = atoi( pszDOSPort + 3 );
   BOOL bIsPrinter = FALSE;

#if defined(HB_OS_DOS)

   /* NOTE: DOS specific solution, using BIOS interrupt */

   if( hb_strnicmp( pszDOSPort, "LPT", 3 ) == 0 && uiPort > 0 )
   {
      union REGS regs;

      regs.h.ah = 2;
      regs.HB_XREGS.dx = uiPort - 1;

      HB_DOS_INT86( 0x17, &regs, &regs );

      bIsPrinter = ( regs.h.ah == 0x90 );
   }
   else if( hb_strnicmp( pszDOSPort, "COM", 3 ) == 0 && uiPort > 0 )
   {
      /* TODO: Proper COM port checking */
      bIsPrinter = TRUE;
   }

#else

   /* NOTE: Platform independent method, at least it will compile and run
            on any platform, but the result may not be the expected one,
            since Unix/Linux doesn't support LPT/COM by nature, other OSs
            may not reflect the actual physical presence of the printer when
            trying to open it, since we are talking to the spooler.
            [vszakats] */

   if( ( hb_strnicmp( pszDOSPort, "LPT", 3 ) == 0 ||
         hb_strnicmp( pszDOSPort, "COM", 3 ) == 0 ) && uiPort > 0 )
   {
      FHANDLE fhnd = hb_fsOpen( ( BYTE * ) pszDOSPort, FO_WRITE | FO_SHARED | FO_PRIVATE );
      bIsPrinter = ( fhnd != FS_ERROR );
      hb_fsClose( fhnd );
   }

#endif

   hb_retl( bIsPrinter );
}

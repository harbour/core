/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * NETNAME() function
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#include "extend.h"

/* TODO: Implement NETNAME() for other platforms */
/* NOTE: Clipper will only return a maximum of 15 bytes from this function. */

/* NOTE: DOS instructions:

       On entry:      AH         5Eh
                      AL         00h
                      DS:DX      Pointer to a memory buffer (16 bytes) where
                                 computer name will be returned

       Returns:       CH         0        name not defined
                                 not 0    name is defined
                      CL         NETBIOS name number (if CH not 0)
                      DS:DX      Pointer to computer name (ASCIIZ string)
                      AX         Error code, if CF is set
*/

HARBOUR HB_NETNAME( void )
{
#if defined(_Windows) || defined(WINNT)
   {
      DWORD ulLen = MAX_COMPUTERNAME_LENGTH + 1;
      char * pszValue = ( char * ) hb_xgrab( ulLen );

      pszValue[ 0 ] = '\0';

      GetComputerName( pszValue, &ulLen );

      hb_retc( pszValue );
      hb_xfree( pszValue );
   }
#else
   hb_retc( "" )
#endif
}


/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DISKSPACE() function
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Luiz Rafael Culik <culik@sl.conex.net>
 *    Parts of DOS support
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include <ctype.h>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined(DOS) || defined(__WATCOMC__)
   #include <dos.h>
#endif

HARBOUR HB_DISKSPACE( void )
{
   ULONG ulSpaceFree = 0;
   USHORT uiDrive = ( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 ) ?
                     ( USHORT )( toupper( *hb_parc( 1 ) ) - 'A' + 1 ) : 0;

#if defined(DOS) || defined(__WATCOMC__)

   struct diskfree_t disk;
   unsigned uiResult;

   while( ( uiResult = _dos_getdiskfree( uiDrive, &disk ) ) != 0 )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( uiResult != 0 )
      ulSpaceFree = ( ULONG ) disk.avail_clusters *
                    ( ULONG ) disk.sectors_per_cluster *
                    ( ULONG ) disk.bytes_per_sector;

#elif defined(HB_OS_WIN_32)

   {
      char szPath[ 4 ];

      DWORD dwSectorsPerCluster;
      DWORD dwBytesPerSector;
      DWORD dwNumberOfFreeClusters;
      DWORD dwTotalNumberOfClusters;

      if( uiDrive == 0 )
         uiDrive = hb_fsCurDrv() + 1;

      szPath[ 0 ] = uiDrive + 'A' - 1;
      szPath[ 1 ] = ':';
      szPath[ 2 ] = '\\';
      szPath[ 3 ] = '\0';

      if( GetDiskFreeSpace( szPath,
                            &dwSectorsPerCluster,
                            &dwBytesPerSector,
                            &dwNumberOfFreeClusters,
                            &dwTotalNumberOfClusters ) )
      {
         ulSpaceFree = dwNumberOfFreeClusters *
                       dwSectorsPerCluster *
                       dwBytesPerSector;
      }
   }

#else

   HB_SYMBOL_UNUSED( uiDrive );

#endif

   hb_retnl( ( long ) ulSpaceFree );
}


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

/* NOTE: DISKSPACE() supports larger disks than 2GB. CA-Cl*pper will always 
         return a (long) value, Harbour may return a (double) for large
         values, the decimal places are always set to zero, though. */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapiitm.h"

#if defined(HB_OS_DOS) || defined(__WATCOMC__)
   #include <dos.h>
#endif

HB_FUNC( DISKSPACE )
{
   double dSpaceFree = 0.0;
   USHORT uiDrive = ISNUM( 1 ) ? hb_parni( 1 ) : 0;

#if defined(HB_OS_DOS) || defined(__WATCOMC__)

   struct diskfree_t disk;
   unsigned uiResult;

   while( ( uiResult = _dos_getdiskfree( uiDrive, &disk ) ) != 0 )
   {
      USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT );

      if( uiAction == E_DEFAULT || uiAction == E_BREAK )
         break;
   }

   if( uiResult != 0 )
      dSpaceFree = ( double ) disk.avail_clusters *
                   ( double ) disk.sectors_per_cluster *
                   ( double ) disk.bytes_per_sector;

#elif defined(HB_OS_WIN_32)
   
   {

      typedef BOOL (WINAPI *P_GDFSE)(LPCTSTR, PULARGE_INTEGER, 
                                     PULARGE_INTEGER, PULARGE_INTEGER);

      char szPath[ 4 ];
      P_GDFSE pGetDiskFreeSpaceEx;

      /* Get the default drive */

      if( uiDrive == 0 )
      {
         USHORT uiErrorOld = hb_fsError();

         uiDrive = hb_fsCurDrv() + 1;

         hb_fsSetError( uiErrorOld );
      }

      szPath[ 0 ] = uiDrive + 'A' - 1;
      szPath[ 1 ] = ':';
      szPath[ 2 ] = '\\';
      szPath[ 3 ] = '\0';

      pGetDiskFreeSpaceEx = ( P_GDFSE ) GetProcAddress( GetModuleHandle( "kernel32.dll" ),
                                                        "GetDiskFreeSpaceExA");

      if( pGetDiskFreeSpaceEx )
      {
         ULARGE_INTEGER i64FreeBytesToCaller;
         ULARGE_INTEGER i64TotalBytes;
         ULARGE_INTEGER i64FreeBytes;

         if( pGetDiskFreeSpaceEx( szPath,
                                  ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                  ( PULARGE_INTEGER ) &i64TotalBytes,
                                  ( PULARGE_INTEGER ) &i64FreeBytes ) )

            dSpaceFree = ( double ) (unsigned)i64FreeBytesToCaller.QuadPart ;
      }
      else 
      {
         DWORD dwSectorsPerCluster;
         DWORD dwBytesPerSector;
         DWORD dwNumberOfFreeClusters;
         DWORD dwTotalNumberOfClusters;

         if( GetDiskFreeSpace( szPath,
                               &dwSectorsPerCluster,
                               &dwBytesPerSector,
                               &dwNumberOfFreeClusters,
                               &dwTotalNumberOfClusters ) )
         {
            dSpaceFree = ( double ) dwNumberOfFreeClusters *
                         ( double ) dwSectorsPerCluster *
                         ( double ) dwBytesPerSector;
         }
      }
   }

#else

   HB_SYMBOL_UNUSED( uiDrive );

#endif

   {
      PHB_ITEM pRetVal;

      pRetVal = hb_itemNew( NULL );
      hb_itemPutNLen( pRetVal, dSpaceFree, -1, 0 );
      hb_itemReturn( pRetVal );
      hb_itemRelease( pRetVal );
   }
}


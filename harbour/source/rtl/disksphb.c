/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_DISKSPACE() function
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
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

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSERRORS

#define HB_OS_WIN_32_USED

#include <ctype.h>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX )
   #include <sys/vfs.h>
#endif

#ifdef HB_EXTENSION

HB_FUNC( HB_DISKSPACE )
{
   char * szPath = ISCHAR( 1 ) ? hb_parc( 1 ) : NULL;
   USHORT uiType = ISNUM( 2 ) ? hb_parni( 2 ) : HB_DISK_AVAIL;
   double dSpace = 0.0;

   if( uiType > HB_DISK_TOTAL )
      uiType = HB_DISK_AVAIL;

#if defined(HB_OS_DOS)

   {
      USHORT uiDrive = ( szPath[ 0 ] == '\0' ) ? 0 : ( toupper( szPath[ 0 ] ) - 'A' + 1 );

      while( TRUE )
      {
         union REGS regs;

         regs.HB_XREGS.dx = uiDrive;
         regs.h.ah = 0x36;
         HB_DOS_INT86( 0x21, &regs, &regs );

         if( regs.HB_XREGS.ax != 0xFFFF )
         {
            USHORT uiClusterTotal  = regs.HB_XREGS.dx;
            USHORT uiClusterFree   = regs.HB_XREGS.bx;
            USHORT uiSecPerCluster = regs.HB_XREGS.ax;
            USHORT uiSectorSize    = regs.HB_XREGS.cx;

            switch( uiType )
            {
               case HB_DISK_AVAIL:
               case HB_DISK_FREE:
                  dSpace = ( double ) uiClusterFree *
                           ( double ) uiSecPerCluster *
                           ( double ) uiSectorSize;
                  break;

               case HB_DISK_USED:
               case HB_DISK_TOTAL:
                  dSpace = ( double ) uiClusterTotal *
                           ( double ) uiSecPerCluster *
                           ( double ) uiSectorSize;

                  if( uiType == HB_DISK_USED )
                     dSpace -= ( double ) uiClusterFree *
                               ( double ) uiSecPerCluster *
                               ( double ) uiSectorSize;
                  break;
            }
         }
         else
         {
            if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, ( EF_CANDEFAULT | EF_CANRETRY ), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) == E_RETRY )
               continue;
         }
         break;
      }
   }

#elif defined(HB_OS_WIN_32)

   {
      while( TRUE )
      {
         typedef BOOL ( WINAPI * P_GDFSE )( LPCTSTR, PULARGE_INTEGER,
                                            PULARGE_INTEGER, PULARGE_INTEGER );

         P_GDFSE pGetDiskFreeSpaceEx;
         UINT uiErrMode;

         uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );

         SetLastError( 0 );

         pGetDiskFreeSpaceEx = ( P_GDFSE ) GetProcAddress( GetModuleHandle( "kernel32.dll" ),
                                                           "GetDiskFreeSpaceExA");

         if( pGetDiskFreeSpaceEx )
         {
            ULARGE_INTEGER i64FreeBytesToCaller,
                           i64TotalBytes,
                           i64FreeBytes,
                           i64RetVal;

            if( pGetDiskFreeSpaceEx( szPath,
                                     ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                     ( PULARGE_INTEGER ) &i64TotalBytes,
                                     ( PULARGE_INTEGER ) &i64FreeBytes ) )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                     memcpy( &i64RetVal, &i64FreeBytesToCaller, sizeof( ULARGE_INTEGER ) );
                     break;

                  case HB_DISK_FREE:
                     memcpy( &i64RetVal, &i64FreeBytes, sizeof( ULARGE_INTEGER ) );
                     break;

                  case HB_DISK_USED:
                  case HB_DISK_TOTAL:
                     memcpy( &i64RetVal, &i64TotalBytes, sizeof( ULARGE_INTEGER ) );
               }

               #if (defined(__GNUC__) || defined(_MSC_VER)) && !defined(__RSXNT__)

                  dSpace  = ( double ) i64RetVal.LowPart +
                            ( double ) i64RetVal.HighPart +
                            ( double ) i64RetVal.HighPart *
                            ( double ) 0xFFFFFFFF;

                  if( uiType == HB_DISK_USED )
                  {
                     dSpace -= ( double ) i64FreeBytes.LowPart +
                               ( double ) i64FreeBytes.HighPart +
                               ( double ) i64FreeBytes.HighPart *
                               ( double ) 0xFFFFFFFF;
                  }

               #else

                  /* NOTE: Borland doesn't seem to deal with the un-named
                           struct that is part of ULARGE_INTEGER
                           [pt] */

                  dSpace  = ( double ) i64RetVal.u.LowPart +
                            ( double ) i64RetVal.u.HighPart +
                            ( double ) i64RetVal.u.HighPart *
                            ( double ) 0xFFFFFFFF;

                  if( uiType == HB_DISK_USED )
                  {
                     dSpace -= ( double ) i64FreeBytes.u.LowPart +
                               ( double ) i64FreeBytes.u.HighPart +
                               ( double ) i64FreeBytes.u.HighPart *
                               ( double ) 0xFFFFFFFF;
                  }

               #endif
            }
         }
         else
         {
            DWORD dwSectorsPerCluster;
            DWORD dwBytesPerSector;
            DWORD dwNumberOfFreeClusters;
            DWORD dwTotalNumberOfClusters;

            SetLastError( 0 );

            if( GetDiskFreeSpace( szPath,
                                  &dwSectorsPerCluster,
                                  &dwBytesPerSector,
                                  &dwNumberOfFreeClusters,
                                  &dwTotalNumberOfClusters ) )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                  case HB_DISK_FREE:
                     dSpace = ( double ) dwNumberOfFreeClusters *
                              ( double ) dwSectorsPerCluster *
                              ( double ) dwBytesPerSector;
                     break;

                  case HB_DISK_USED:
                  case HB_DISK_TOTAL:
                     dSpace  = ( double ) dwTotalNumberOfClusters *
                               ( double ) dwSectorsPerCluster *
                               ( double ) dwBytesPerSector;

                     if( uiType == HB_DISK_USED )
                        dSpace -= ( double ) dwNumberOfFreeClusters *
                                  ( double ) dwSectorsPerCluster *
                                  ( double ) dwBytesPerSector;
                     break;

               }
            }
         }

         SetErrorMode( uiErrMode );

         if( GetLastError() != 0 )
         {
            if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, (EF_CANDEFAULT | EF_CANRETRY), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) == E_RETRY )
               continue;
         }
         break;
      }
   }

#elif defined(HB_OS_OS2)

   {
      struct _FSALLOCATE fsa;
      USHORT rc;
      USHORT uiDrive = ( szPath[ 0 ] == '\0' ) ? 0 : ( toupper( szPath[ 0 ] ) - 'A' + 1 );

      /* Query level 1 info from filesystem */
      while( ( rc = DosQueryFSInfo( uiDrive, 1, &fsa, sizeof( fsa ) ) ) != 0 )
      {
         if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, (EF_CANDEFAULT | EF_CANRETRY), 2, hb_paramError( 1 ), hb_paramError( 2 ) ) != E_RETRY )
            break;
      }

      if( rc == 0 )
      {
         switch( uiType )
         {
            case HB_DISK_AVAIL:
            case HB_DISK_FREE:
               dSpace = ( double ) fsa.cUnitAvail *
                        ( double ) fsa.cSectorUnit *
                        ( double ) fsa.cbSector;
               break;

            case HB_DISK_USED:
            case HB_DISK_TOTAL:
               dSpace = ( double ) fsa.cUnit *
                        ( double ) fsa.cSectorUnit *
                        ( double ) fsa.cbSector;

               if( uiType == HB_DISK_USED )
                  dSpace -= ( double ) fsa.cUnitAvail *
                            ( double ) fsa.cSectorUnit *
                            ( double ) fsa.cbSector;
               break;
         }
      }
   }

#elif defined(HB_OS_UNIX)

   {
      struct statfs sf;

      statfs( szPath, &sf );

      switch( uiType )
      {
         case HB_DISK_AVAIL:
            dSpace = ( double ) sf.f_bavail * ( double ) sf.f_bsize;
            break;

         case HB_DISK_FREE:
            dSpace = ( double ) sf.f_bfree * ( double ) sf.f_bsize;
            break;

         case HB_DISK_USED:
             dSpace = ( double ) ( sf.f_blocks - sf.f_bfree ) *
                      ( double ) sf.f_bsize;
             break;

         case HB_DISK_TOTAL:
            dSpace = ( double ) sf.f_blocks * ( double ) sf.f_bsize;
            break;
      }
   }

#else

   {
      HB_SYMBOL_UNUSED( uiDrive );
      HB_SYMBOL_UNUSED( uiType );
   }

#endif

   hb_retnlen( dSpace, -1, 0 );
}

#endif

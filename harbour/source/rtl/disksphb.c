/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_DISKSPACE() function
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
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

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSERRORS

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined( HB_OS_DARWIN )
   #include <sys/param.h>
   #include <sys/mount.h>
#elif defined( HB_OS_UNIX ) && !( defined( __WATCOMC__ ) || defined( __CEGCC__ ) )
   #include <sys/statvfs.h>
#endif

HB_FUNC( HB_DISKSPACE )
{
   char szPathBuf[ 4 ];
   char * szPath = hb_parc( 1 );
   USHORT uiType = HB_ISNUM( 2 ) ? ( USHORT ) hb_parni( 2 ) : HB_DISK_AVAIL;
   double dSpace = 0.0;

   if( uiType > HB_DISK_TOTAL )
      uiType = HB_DISK_AVAIL;

   if( !szPath )
   {
#ifdef HB_OS_HAS_DRIVE_LETTER
      if( HB_ISNUM( 1 ) )
      {
         szPathBuf[ 0 ] = ( char ) hb_parni( 1 ) + 'A' - 1;
         szPathBuf[ 1 ] = HB_OS_DRIVE_DELIM_CHR;
         szPathBuf[ 2 ] = HB_OS_PATH_DELIM_CHR;
         szPathBuf[ 3 ] = '\0';
      }
      else
#endif
      {
         szPathBuf[ 0 ] = HB_OS_PATH_DELIM_CHR;
         szPathBuf[ 1 ] = '\0';
      }
      szPath = szPathBuf;
   }

#if defined(HB_OS_DOS)
   {
      USHORT uiDrive = szPath[ 1 ] != HB_OS_DRIVE_DELIM_CHR ? 0 :
                       ( szPath[ 0 ] >= 'A' && szPath[ 0 ] <= 'Z' ?
                         szPath[ 0 ] - 'A' + 1 :
                       ( szPath[ 0 ] >= 'a' && szPath[ 0 ] <= 'z' ?
                         szPath[ 0 ] - 'a' + 1 : 0 ) );
      for( ;; )
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
            if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, ( EF_CANDEFAULT | EF_CANRETRY ), HB_ERR_ARGS_BASEPARAMS ) == E_RETRY )
               continue;
         }
         break;
      }
   }
#elif defined(HB_OS_WIN)
   {
#if defined(_MSC_VER) || defined(__LCC__) || \
    ( defined(__GNUC__) && !defined(__RSXNT__) )

#  define HB_GET_LARGE_UINT( v )  ( ( double ) (v).LowPart + \
                                    ( double ) (v).HighPart * \
                                    ( ( ( double ) 0xFFFFFFFF ) + 1 ) )

#else
   /* NOTE: Borland doesn't seem to deal with the un-named
            struct that is part of ULARGE_INTEGER
            [pt] */
#  define HB_GET_LARGE_UINT( v )  ( ( double ) (v).u.LowPart + \
                                    ( double ) (v).u.HighPart * \
                                    ( ( ( double ) 0xFFFFFFFF ) + 1 ) )
#endif

      typedef BOOL ( WINAPI * P_GDFSE )( LPCSTR, PULARGE_INTEGER,
                                         PULARGE_INTEGER, PULARGE_INTEGER );

      for( ;; )
      {
         ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
         UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );
         BOOL fResult;

#if defined(HB_OS_WIN_CE)
         LPTSTR lpPath = HB_TCHAR_CONVTO( szPath );

         fResult = GetDiskFreeSpaceEx( lpPath,
                                       ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                       ( PULARGE_INTEGER ) &i64TotalBytes,
                                       ( PULARGE_INTEGER ) &i64FreeBytes );
         hb_fsSetIOError( fResult, 0 );
         HB_TCHAR_FREE( lpPath );
         if( fResult )
         {
            switch( uiType )
            {
               case HB_DISK_AVAIL:
                  dSpace = HB_GET_LARGE_UINT( i64FreeBytesToCaller );
                  break;

               case HB_DISK_FREE:
                  dSpace = HB_GET_LARGE_UINT( i64FreeBytes );
                  break;

               case HB_DISK_TOTAL:
                  dSpace = HB_GET_LARGE_UINT( i64TotalBytes );
                  break;

               case HB_DISK_USED:
                  dSpace = HB_GET_LARGE_UINT( i64TotalBytes ) -
                           HB_GET_LARGE_UINT( i64FreeBytes );
                  break;
            }
         }
#else
         P_GDFSE pGetDiskFreeSpaceEx = ( P_GDFSE )
                           GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ),
                                           "GetDiskFreeSpaceExA" );
         if( pGetDiskFreeSpaceEx )
         {
            fResult = pGetDiskFreeSpaceEx( szPath,
                                           ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                           ( PULARGE_INTEGER ) &i64TotalBytes,
                                           ( PULARGE_INTEGER ) &i64FreeBytes );
            hb_fsSetIOError( fResult, 0 );
            if( fResult )
            {
               switch( uiType )
               {
                  case HB_DISK_AVAIL:
                     dSpace = HB_GET_LARGE_UINT( i64FreeBytesToCaller );
                     break;

                  case HB_DISK_FREE:
                     dSpace = HB_GET_LARGE_UINT( i64FreeBytes );
                     break;

                  case HB_DISK_TOTAL:
                     dSpace = HB_GET_LARGE_UINT( i64TotalBytes );
                     break;

                  case HB_DISK_USED:
                     dSpace = HB_GET_LARGE_UINT( i64TotalBytes ) -
                              HB_GET_LARGE_UINT( i64FreeBytes );
                     break;
               }
            }
         }
         else
         {
            DWORD dwSectorsPerCluster;
            DWORD dwBytesPerSector;
            DWORD dwNumberOfFreeClusters;
            DWORD dwTotalNumberOfClusters;

            fResult = GetDiskFreeSpaceA( szPath,
                                         &dwSectorsPerCluster,
                                         &dwBytesPerSector,
                                         &dwNumberOfFreeClusters,
                                         &dwTotalNumberOfClusters );
            hb_fsSetIOError( fResult, 0 );
            if( fResult )
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
#endif
         SetErrorMode( uiErrMode );
         break;
      }
   }
#elif defined(HB_OS_OS2)
   {
      struct _FSALLOCATE fsa;
      USHORT rc;
      USHORT uiDrive = szPath[ 1 ] != HB_OS_DRIVE_DELIM_CHR ? 0 :
                       ( szPath[ 0 ] >= 'A' && szPath[ 0 ] <= 'Z' ?
                         szPath[ 0 ] - 'A' + 1 :
                       ( szPath[ 0 ] >= 'a' && szPath[ 0 ] <= 'z' ?
                         szPath[ 0 ] - 'a' + 1 : 0 ) );

      /* Query level 1 info from filesystem */
      while( ( rc = DosQueryFSInfo( uiDrive, 1, &fsa, sizeof( fsa ) ) ) != 0 )
      {
         if( hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, (EF_CANDEFAULT | EF_CANRETRY), HB_ERR_ARGS_BASEPARAMS ) != E_RETRY )
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
         hb_fsSetIOError( TRUE, 0 );
      }
      else
         hb_fsSetIOError( FALSE, 0 );
   }
#elif defined(HB_OS_UNIX) && !( defined(__WATCOMC__) || defined(__CEGCC__) )
   {
#if defined(HB_OS_DARWIN)
      struct statfs sf;
#else
      struct statvfs sf;
#endif
      BOOL fFree = FALSE;

      szPath = ( char * ) hb_fsNameConv( ( BYTE * ) szPath, &fFree );

#if defined(HB_OS_DARWIN)
      if( statfs( szPath, &sf ) == 0 )
#else
      if( statvfs( szPath, &sf ) == 0 )
#endif
      {
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
         hb_fsSetIOError( TRUE, 0 );
      }
      else
         hb_fsSetIOError( FALSE, 0 );

      if( fFree )
         hb_xfree( szPath );
   }
#else
   {
      int iTODO;

      HB_SYMBOL_UNUSED( szPath );
      HB_SYMBOL_UNUSED( uiType );
   }
#endif

   hb_retnlen( dSpace, -1, 0 );
}

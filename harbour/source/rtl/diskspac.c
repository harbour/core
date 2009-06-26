/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DISKSPACE() function
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

/* NOTE: DISKSPACE() supports larger disks than 2GB. CA-Cl*pper will always
         return a (long) value, Harbour may return a (double) for large
         values, the decimal places are always set to zero, though. */

/* NOTE: For OS/2. Must be ahead of any and all #include statements */
#define INCL_BASE
#define INCL_DOSERRORS

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapifs.h"

#if defined( HB_OS_UNIX )
#  include <unistd.h>
#  include <sys/types.h>
#  if defined(__WATCOMC__) || defined(__CEGCC__)
#     include <sys/stat.h>
#  elif defined( HB_OS_DARWIN )
#     include <sys/param.h>
#     include <sys/mount.h>
#  else
#     include <sys/statvfs.h>
#  endif
#endif

HB_FUNC( DISKSPACE )
{
   double dSpace = 0.0;
   BOOL bError;

#if defined(HB_OS_DOS)
   {
      USHORT uiDrive = HB_ISNUM( 1 ) ? hb_parni( 1 ) : 0;
      union REGS regs;

      regs.HB_XREGS.dx = uiDrive;
      regs.h.ah = 0x36;
      HB_DOS_INT86( 0x21, &regs, &regs );

      bError = regs.HB_XREGS.ax == 0xFFFF;
      if( !bError )
         dSpace = ( double ) regs.HB_XREGS.bx *
                  ( double ) regs.HB_XREGS.ax *
                  ( double ) regs.HB_XREGS.cx;
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
      ULARGE_INTEGER i64FreeBytesToCaller, i64TotalBytes, i64FreeBytes;
      USHORT uiParam = ( USHORT ) hb_parni( 1 );
      USHORT uiDrive = uiParam == 0 ? hb_fsCurDrv() + 1 : uiParam;
      UINT uiErrMode = SetErrorMode( SEM_FAILCRITICALERRORS );

#if defined(HB_OS_WIN_CE)
      TCHAR lpPath[ 4 ];

      lpPath[ 0 ] = ( TCHAR ) uiDrive + 'A' - 1;
      lpPath[ 1 ] = ':';
      lpPath[ 2 ] = '\\';
      lpPath[ 3 ] = '\0';

      bError = !GetDiskFreeSpaceEx( lpPath,
                                    ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                    ( PULARGE_INTEGER ) &i64TotalBytes,
                                    ( PULARGE_INTEGER ) &i64FreeBytes );
      if( !bError )
         dSpace = HB_GET_LARGE_UINT( i64FreeBytesToCaller );
#else
      char szPath[ 4 ];
      P_GDFSE pGetDiskFreeSpaceEx = ( P_GDFSE )
                           GetProcAddress( GetModuleHandle( TEXT( "kernel32.dll" ) ),
                                           "GetDiskFreeSpaceExA" );
      szPath[ 0 ] = ( char ) uiDrive + 'A' - 1;
      szPath[ 1 ] = ':';
      szPath[ 2 ] = '\\';
      szPath[ 3 ] = '\0';

      if( pGetDiskFreeSpaceEx )
      {
         bError = !pGetDiskFreeSpaceEx( szPath,
                                        ( PULARGE_INTEGER ) &i64FreeBytesToCaller,
                                        ( PULARGE_INTEGER ) &i64TotalBytes,
                                        ( PULARGE_INTEGER ) &i64FreeBytes );
         if( !bError )
            dSpace = HB_GET_LARGE_UINT( i64FreeBytesToCaller );
      }
      else
      {
         DWORD dwSectorsPerCluster;
         DWORD dwBytesPerSector;
         DWORD dwNumberOfFreeClusters;
         DWORD dwTotalNumberOfClusters;

         bError = !GetDiskFreeSpaceA( szPath,
                                      &dwSectorsPerCluster,
                                      &dwBytesPerSector,
                                      &dwNumberOfFreeClusters,
                                      &dwTotalNumberOfClusters );
         if( !bError )
            dSpace = ( double ) dwNumberOfFreeClusters *
                     ( double ) dwSectorsPerCluster *
                     ( double ) dwBytesPerSector;
      }
#endif
      SetErrorMode( uiErrMode );
   }
#elif defined(HB_OS_OS2)
   {
      USHORT uiDrive = HB_ISNUM( 1 ) ? hb_parni( 1 ) : 0;
      struct _FSALLOCATE fsa;

      /* Query level 1 info from filesystem */
      bError = DosQueryFSInfo( uiDrive, 1, &fsa, sizeof( fsa ) ) != 0;
      if( !bError )
         dSpace = ( double ) fsa.cUnitAvail *
                  ( double ) fsa.cSectorUnit *
                  ( double ) fsa.cbSector;
   }
#elif defined(HB_OS_UNIX)
   {
      const char * szName = hb_parc( 1 );
      char * pszFree = NULL;

      if( !szName )
         szName = "/";
      else
         szName = hb_fsNameConv( szName, &pszFree );

      {
#if defined(__WATCOMC__) || defined(__CEGCC__)
         struct stat st;
         bError = stat( szName, &st) != 0;
         if( !bError )
            dSpace = ( double ) st.st_blocks * ( double ) st.st_blksize;
#else
#if defined( HB_OS_DARWIN )
         struct statfs st;
         bError = statfs( szName, &st ) != 0;
#else
         struct statvfs st;
         bError = statvfs( szName, &st ) != 0;
#endif
         if( !bError )
         {
            if( getuid() == 0 )
               dSpace = ( double ) st.f_bfree * ( double ) st.f_bsize;
            else
               dSpace = ( double ) st.f_bavail * ( double ) st.f_bsize;
         }
#endif
      }

      if( pszFree )
         hb_xfree( pszFree );
   }
#else
   bError = FALSE;
#endif

   if( bError )
      hb_errRT_BASE_Ext1( EG_OPEN, 2018, NULL, NULL, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );

   hb_retnlen( dSpace, -1, 0 );
}

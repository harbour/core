/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * CT (Clipper Tools) Disk, File and Directory management.
 *
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 *
 * DirMake()     - Ready. Already exist a MakeDir() function in xHarbour RTL Lib,
 *                        but DirMake returns a more compatible error codes.
 * DirName()     - Ready.
 * DriveType()   - Ready.  corrected <ptucker@sympatico.ca>
 * Volume()      - Ready.
 * GetVolInfo()  - Ready.  This function is new.
 * VolSerial()   - Ready.
 *
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 * NUMDISKL()
 *
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru>
 * TrueName()
 *
 * www - http://www.xharbour.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or ( at your option )
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
 * Boston, MA 02111-1307 USA ( or visit the web site http://www.gnu.org/ ).
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
#include "hbapierr.h"
#include "hbapifs.h"
#include "ctstrfil.h"

#if defined(HB_OS_WIN)

#   include <windows.h>
#   include <winbase.h>
#   include <shellapi.h>

#   define HB_OS_WIN_USED

#elif defined(HB_OS_DOS)

#   include <dos.h>

#endif


HB_FUNC( DIRMAKE )
{
   BYTE *pFileName = ( BYTE * ) hb_parcx( 1 );

   if( hb_fsMkDir( pFileName ) )
   {
      hb_retni( 0 );
   }
   else
   {
      hb_retni( -hb_fsOsError() );
   }
}

HB_FUNC( DIRNAME )
{
   BYTE *pbyBuffer = ( BYTE * ) hb_xgrab( HB_PATH_MAX );
   unsigned char *pszDrive = ( unsigned char * ) hb_parc( 1 );
   USHORT uiDrive = 0;

   if( pszDrive )
   {
      /* some network drivers (f.e. NETX from Novel Netware) allow
       * to create drives after 'Z' letter.
       */
      if( *pszDrive >= 'A' && *pszDrive < 'A' + 32 )
         uiDrive = *pszDrive - ( 'A' - 1 );
      else if( *pszDrive >= 'a' && *pszDrive < 'a' + 32 )
         uiDrive = *pszDrive - ( 'a' - 1 );
   }
   pbyBuffer[0] = HB_OS_PATH_DELIM_CHR;
   hb_fsCurDirBuff( uiDrive, pbyBuffer + 1, HB_PATH_MAX - 1 );

   hb_retc_buffer( ( char * ) pbyBuffer );
}


HB_FUNC( DRIVETYPE )
{
#if defined(HB_OS_WIN) && ! defined(HB_OS_WIN_CE)
   ULONG ulSize = hb_parclen( 1 ) + 2;  /* allow space for '\0' & ":\" */
   char *pszDrive = ( char * ) hb_xgrab( ulSize + 1 );
   LPTSTR lpDrive;
   int iType;

   hb_strncpy( pszDrive, ( char * ) hb_parcx( 1 ), ulSize );

   if( strstr( pszDrive, ":" ) == NULL )
      hb_strncat( pszDrive, ":", ulSize );

   if( strstr( pszDrive, "\\" ) == NULL )
      hb_strncat( pszDrive, "\\", ulSize );

   lpDrive = HB_TCHAR_CONVTO( pszDrive );
   switch( GetDriveType( lpDrive ) )
   {
      case DRIVE_RAMDISK:
         iType = 0;           /* RAM Drive - Clipper compatible */
         break;
      case DRIVE_REMOVABLE:
         iType = 2;           /* Floppy Drive - Clipper compatible */
         break;
      case DRIVE_FIXED:
         iType = 3;           /* Hard Drive  - Clipper compatible */
         break;
      case DRIVE_CDROM:
         iType = 4;           /* CD-Rom Drive - xHarbour extension */
         break;
      case DRIVE_REMOTE:
         iType = 5;           /* Network Drive - xHarbour extension */
         break;
      default:
         iType = 9;           /* Unknow Drive - xHarbour extension */
         break;
   }
   hb_retni( iType );
   hb_xfree( pszDrive );
   HB_TCHAR_FREE( lpDrive );
#else
   hb_retni( 9 );
#endif

}


HB_FUNC( NUMDISKL )
{
#if defined( HB_OS_DOS )
#if defined( __DJGPP__ )
   unsigned cur_drive, n_drives;

   _dos_getdrive( &cur_drive );
   _dos_setdrive( cur_drive, &n_drives );
   hb_retni( n_drives );
#else
   /* should be easily implementable somehow similar to DJGPP */
   hb_retni( 26 );
#endif
#elif defined( HB_OS_WIN )
   /* LASTDRIVE does not affect Windows apps, they always have 26 letters avail */
   hb_retni( 26 );
#else
   /* For Unix, return the most harmless value... or not? */
   hb_retni( 1 );
#endif
}


/*
 * Volume() depends of the CSETSAFETY() setting and, if is true, does not
 * overwrite an existing label.
 *
 * Syntax is: Volume("X:test") or Volume("X:\test"), where "x" is the
 * any drive letter and "test" will be the new volume name.
 *
 * Notes:
 * 1) if the drive letter is not suplied, then the current drive will
 *    be used to change voloume name.
 * 2) if Volume("X:") or Volume("X:\") then the volume name of the drive
 *    "X:" will be erased.
 * 3) if Volume("") or Volume() then the volume name of the current drive
 *   will be erased.
 */

HB_FUNC( VOLUME )
{
   BOOL bReturn = FALSE;

   if( !ct_getsafety() )
   {
      PHB_FNAME fname;
      BYTE *sDiskName;
      char *sRoot = NULL;
      char *sVolName = NULL;
      char sRootBuf[4], sVolNameBuf[12];
      BOOL fFree;

      if( HB_ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
      {
         sDiskName = hb_fsNameConv( ( BYTE * ) hb_parc( 1 ), &fFree );

         if( ( fname = hb_fsFNameSplit( ( char * ) sDiskName ) ) != NULL )
         {
            if( fname->szPath )
            {
               hb_strncpy( sRootBuf, fname->szPath, sizeof( sRootBuf ) - 1 );
               sRoot = sRootBuf;
            }
            if( fname->szName )
            {
               hb_strncpy( sVolNameBuf, fname->szName, sizeof( sVolNameBuf ) - 1 );
               sVolName = sVolNameBuf;
            }

            hb_xfree( fname );
         }
         else
         {
            hb_strncpy( sVolNameBuf, ( char * ) sDiskName, sizeof( sVolNameBuf ) - 1 );
            sVolName = sVolNameBuf;
         }
         if( fFree )
            hb_xfree( sDiskName );
      }
#if defined(HB_OS_WIN) && ! defined(HB_OS_WIN_CE)
      {
         LPTSTR lpRoot, lpVolName;
         lpRoot = sRoot ? HB_TCHAR_CONVTO( sRoot ) : NULL;
         lpVolName = sVolName ? HB_TCHAR_CONVTO( sVolName ) : NULL;
         bReturn = SetVolumeLabel( lpRoot, lpVolName );
         if( lpRoot )
            HB_TCHAR_FREE( lpRoot );
         if( lpVolName )
            HB_TCHAR_FREE( lpVolName );
      }
#endif
   }
   hb_retl( bReturn );
}

/*
 * GetVolInfo() is a new function. It returns the volume name of a Floppy, CD,
 * Hard-disk or mapped network drive.
 * Syntax is: GetVolInfo("X:\")
 * Note that the trailing backslash is required.
 */
HB_FUNC( GETVOLINFO )
{
#if defined(HB_OS_WIN) && ! defined(HB_OS_WIN_CE)
   int iretval;
   char *sDrive = hb_parcx( 1 ), *sVolName;
   TCHAR lpVolName[256];
   LPTSTR lpDrive;

   lpDrive = sDrive[0] ? HB_TCHAR_CONVTO( sDrive ) : NULL;
   iretval = GetVolumeInformation( lpDrive, lpVolName, 256, NULL, NULL, NULL, NULL, 0 );
   if( lpDrive )
      HB_TCHAR_FREE( lpDrive );

   if( iretval != 0 )
   {
      sVolName = HB_TCHAR_CONVFROM( lpVolName );
      hb_retc( sVolName );
      HB_TCHAR_FREE( sVolName );
   }
   else
      hb_retc_null();
#endif
}

/*
 * VolSerial() function returns the volume serial number of an drive letter like
 * floppy, Hard-disk, CD or mapped network drive. The return value is a dword
 * type. If the drive is not available, volserial() returns -1.
 *
 * Sintax is: VolSerial("X:\")
 * Note that the trailing backslash is required.
 *
 * To convert in the hex format, call numtohex() function.
 * Example: numtohex( volserial("C:\")).
 * See volser.prg in xharbour\tests\cttest folder.
 */

HB_FUNC( VOLSERIAL )
{
#if defined(HB_OS_WIN) && ! defined(HB_OS_WIN_CE)
   int retval;
   char *sDrive = hb_parcx( 1 );
   LPTSTR lpDrive;
   DWORD dSerial;

   lpDrive = sDrive[0] ? HB_TCHAR_CONVTO( sDrive ) : NULL;
   retval = GetVolumeInformation( lpDrive,      /* RootPathName */
                                  NULL,         /* VolumeName */
                                  0,            /* VolumeNameSize */
                                  &dSerial,     /* VolumeSerialNumber */
                                  NULL,         /* MaxComponentLength */
                                  NULL,         /* FileSystemFlags */
                                  NULL,         /* FileSystemName */
                                  0 );          /* FileSystemSize */
   if( lpDrive )
      HB_TCHAR_FREE( lpDrive );

   if( retval != 0 )
      hb_retnint( dSerial );
   else
      hb_retni( -1 );
#endif
}

HB_FUNC( TRUENAME )
{
   char * szFile = hb_parc( 1 );

   if( szFile )
   {
#if defined(HB_OS_WIN) && ! defined(HB_OS_WIN_CE)
      char *szBuffRet;
      TCHAR buffer[MAX_PATH + 1] = { 0 };
      LPTSTR lpFile;

      lpFile = HB_TCHAR_CONVTO( szFile );
      GetFullPathName( lpFile, MAX_PATH, buffer, NULL );
      HB_TCHAR_FREE( lpFile );

      szBuffRet = HB_TCHAR_CONVFROM( buffer );
      hb_retc( szBuffRet );
      HB_TCHAR_FREE( szBuffRet );
#else
      hb_retc( szFile );
#endif
   }
   else
      hb_retc_null();
}

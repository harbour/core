/*
 * CT (Clipper Tools) Disk, File and Directory management.
 *
 * Copyright 2004-2005 Eduardo Fernandes <modalsist@yahoo.com.br>
 *    DirMake(), DirName(), DriveType(), Volume(), VolSerial()
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru> (NumDiskL())
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru> (TrueName())
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA ( or visit the web site https://www.gnu.org/ ).
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
#include "hbvm.h"
#include "ctstrfil.h"
#include "hbwinuni.h"

#if defined( HB_OS_DOS )
   #include <dos.h>
#endif
#if defined( HB_OS_WIN )
   #include <windows.h>
#endif

HB_FUNC( DIRMAKE )
{
   if( hb_fileDirMake( hb_parcx( 1 ) ) )
      hb_retni( 0 );
   else
      hb_retnint( -( HB_MAXINT ) hb_fsOsError() );
}

HB_FUNC( DIRNAME )
{
   char * pbyBuffer = ( char * ) hb_xgrab( HB_PATH_MAX );
   const char * pszDrive = hb_parc( 1 );
   int iDrive = 0;

   if( pszDrive )
   {
      HB_UCHAR uc = ( HB_UCHAR ) *pszDrive;
      /* some network drivers (f.e. NETX from Novell NetWare) allow
       * to create drives after 'Z' letter.
       */
      if( uc >= 'A' && uc < 'A' + 32 )
         iDrive = uc - ( 'A' - 1 );
      else if( uc >= 'a' && uc < 'a' + 32 )
         iDrive = uc - ( 'a' - 1 );
   }
   pbyBuffer[ 0 ] = HB_OS_PATH_DELIM_CHR;
   hb_fsCurDirBuff( iDrive, pbyBuffer + 1, HB_PATH_MAX - 1 );

   hb_retc_buffer( pbyBuffer );
}

HB_FUNC( DRIVETYPE )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   HB_SIZE nSize = hb_parclen( 1 ) + 2;  /* allow space for '\0' & ":\" */
   char * pszDrive = ( char * ) hb_xgrab( nSize + 1 );
   LPCTSTR lpDrive;
   LPTSTR lpFree;
   UINT uiType;

   hb_strncpy( pszDrive, hb_parcx( 1 ), nSize );

   if( strstr( pszDrive, ":" ) == NULL )
      hb_strncat( pszDrive, ":", nSize );

   if( strstr( pszDrive, "\\" ) == NULL )
      hb_strncat( pszDrive, "\\", nSize );

   lpDrive = HB_FSNAMECONV( pszDrive, &lpFree );
   hb_vmUnlock();
   uiType = GetDriveType( lpDrive );
   hb_vmLock();
   if( lpFree )
      hb_xfree( lpFree );
   hb_xfree( pszDrive );

   switch( uiType )
   {
      case DRIVE_RAMDISK:
         uiType = 0;           /* RAM Drive - Clipper compatible */
         break;
      case DRIVE_REMOVABLE:
         uiType = 2;           /* Floppy Drive - Clipper compatible */
         break;
      case DRIVE_FIXED:
         uiType = 3;           /* Hard Drive  - Clipper compatible */
         break;
      case DRIVE_CDROM:
         uiType = 4;           /* CD-Rom Drive - xHarbour extension */ /* HB_EXTENSION */
         break;
      case DRIVE_REMOTE:
         uiType = 5;           /* Network Drive - xHarbour extension */ /* HB_EXTENSION */
         break;
      default:
         uiType = 9;           /* Unknown Drive - xHarbour extension */ /* HB_EXTENSION */
         break;
   }
   hb_retni( uiType );
#else
   hb_retni( 9 );
#endif

}

HB_FUNC( NUMDISKL )
{
#if defined( HB_OS_DOS )
#if defined( __DJGPP__ ) || defined( __WATCOMC__ )
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
 * Volume() depends of the CSetSafety() setting and, if is true, does not
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
   HB_BOOL bReturn = HB_FALSE;

#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   if( ! ct_getsafety() )
   {
      const char * pszRoot = NULL;
      const char * pszVolName = NULL;
      char szRootBuf[ 4 ], szVolNameBuf[ 12 ];
      LPCTSTR lpRoot, lpVolName;
      LPTSTR lpRootFree = NULL, lpVolNameFree = NULL;

      if( hb_parclen( 1 ) > 0 )
      {
         PHB_FNAME fname = hb_fsFNameSplit( hb_parc( 1 ) );

         if( fname->szPath )
            pszRoot = hb_strncpy( szRootBuf, fname->szPath, sizeof( szRootBuf ) - 1 );
         if( fname->szName )
            pszVolName = hb_strncpy( szVolNameBuf, fname->szName, sizeof( szVolNameBuf ) - 1 );
         hb_xfree( fname );
      }

      lpRoot = pszRoot ? HB_FSNAMECONV( pszRoot, &lpRootFree ) : NULL;
      lpVolName = pszVolName ? HB_FSNAMECONV( pszVolName, &lpVolNameFree ) : NULL;
      hb_vmUnlock();
      bReturn = SetVolumeLabel( lpRoot, lpVolName ) != 0;
      hb_vmLock();
      if( lpRootFree )
         hb_xfree( lpRootFree );
      if( lpVolNameFree )
         hb_xfree( lpVolNameFree );
   }
#endif
   hb_retl( bReturn );
}

/*
 * VolSerial() function returns the volume serial number of an drive letter like
 * floppy, Hard-disk, CD or mapped network drive. The return value is a dword
 * type. If the drive is not available, VolSerial() returns -1.
 *
 * Syntax is: VolSerial( "X:\" )
 * Note that the trailing backslash is required.
 *
 * To convert in the hex format, call hb_NumToHex() function.
 * Example: hb_NumToHex( VolSerial( "C:\" ) ).
 */

HB_FUNC( VOLSERIAL )
{
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
   DWORD dwSerial = 0;
   void * hDrive;
   HB_SIZE nLen;
   LPCTSTR lpRootPath = HB_PARSTR( 1, &hDrive, &nLen );

   if( GetVolumeInformation( nLen > 0 ? lpRootPath : NULL, /* RootPathName */
                             NULL,      /* VolumeName */
                             0,         /* VolumeNameSize */
                             &dwSerial, /* VolumeSerialNumber */
                             NULL,      /* MaxComponentLength */
                             NULL,      /* FileSystemFlags */
                             NULL,      /* FileSystemName */
                             0 ) )      /* FileSystemSize */
      hb_retnint( dwSerial );
   else
      hb_retni( -1 );

   hb_strfree( hDrive );
#else
   hb_retni( -1 );
#endif
}

HB_FUNC( TRUENAME )
{
   if( HB_ISCHAR( 1 ) )
   {
#if defined( HB_OS_WIN ) && ! defined( HB_OS_WIN_CE )
      void * hFile;
      TCHAR buffer[ MAX_PATH + 1 ];

      buffer[ 0 ] = buffer[ MAX_PATH ] = TEXT( '\0' );

      GetFullPathName( HB_PARSTR( 1, &hFile, NULL ),
                       HB_SIZEOFARRAY( buffer ) - 1,
                       buffer, NULL );

      HB_RETSTR( buffer );
      hb_strfree( hFile );
#else
      hb_retc( hb_parc( 1 ) );
#endif
   }
   else
      hb_retc_null();
}

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

#if defined(HB_OS_WIN_32)

#   include <windows.h>
#   include <winbase.h>
#   include <shellapi.h>

#   define HB_OS_WIN_32_USED

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
   BYTE *pbyBuffer = ( BYTE * ) hb_xgrab( _POSIX_PATH_MAX + 1 );

   pbyBuffer[0] = OS_PATH_DELIMITER;
   hb_fsCurDirBuff( hb_fsCurDrv(), pbyBuffer + 1, _POSIX_PATH_MAX );

   hb_retc_buffer( ( char * ) pbyBuffer );
}


HB_FUNC( DRIVETYPE )
{
#if defined(HB_OS_WIN_32)
   unsigned int uiType;
   ULONG ulSize = hb_parclen( 1 ) + 2;  /* allow space for '\0' & ":\" */
   char *pDrive = ( char * ) hb_xgrab( ulSize + 1 );

   hb_strncpy( pDrive, ( char * ) hb_parcx( 1 ), ulSize );

   if( strstr( pDrive, ":" ) == NULL )
   {
      hb_strncat( pDrive, ":", ulSize );
   }

   if( strstr( pDrive, "\\" ) == NULL )
   {
      hb_strncat( pDrive, "\\", ulSize );
   }

   uiType = GetDriveType( pDrive );

   if( uiType == DRIVE_RAMDISK )
   {
      hb_retni( 0 );            /* RAM Drive - Clipper compatible */
   }
   else if( uiType == DRIVE_REMOVABLE )
   {
      hb_retni( 2 );            /* Floppy Drive - Clipper compatible */
   }
   else if( uiType == DRIVE_FIXED )
   {
      hb_retni( 3 );            /* Hard Drive  - Clipper compatible */
   }
   else if( uiType == DRIVE_CDROM )
   {
      hb_retni( 4 );            /* CD-Rom Drive - xHarbour extension */
   }
   else if( uiType == DRIVE_REMOTE )
   {
      hb_retni( 5 );            /* Network Drive - xHarbour extension */
   }
   else
   {
      hb_retni( 9 );            /* Unknow Drive - xHarbour extension */
   }
   hb_xfree( pDrive );
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
#elif defined( HB_OS_WIN_32 )
   /* LASTDRIVE does not affect Win32 apps, they always have 26 letters avail */
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
 * Syntax is: Volume("x:test") or Volume("x:\test"), where "x" is the
 * any drive letter and "test" will be the new volume name. 
 *
 * Notes:
 * 1) if the drive letter is not suplied, then the current drive will 
 *    be used to change voloume name.
 * 2) if Volume("x:") or Volume("x:\") then the volume name of the drive
 *    "x:" will be erased.
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
      char sRootBuf[3], sVolNameBuf[12];
      BOOL fFree;

      if( ISCHAR( 1 ) && hb_parclen( 1 ) > 0 )
      {
         sDiskName = hb_fsNameConv( ( BYTE * ) hb_parc( 1 ), &fFree );

         if( ( fname = hb_fsFNameSplit( ( char * ) sDiskName ) ) != NULL )
         {
            if( fname->szPath )
            {
               strncpy( sRootBuf, fname->szPath, 3 );
               sRoot = sRootBuf;
            }
            if( fname->szName )
            {
               strncpy( sVolNameBuf, fname->szName, 11 );
               sVolName = sVolNameBuf;
            }

            hb_xfree( fname );
         }
         else
         {
            strncpy( sVolNameBuf, ( char * ) sDiskName, 11 );
            sVolName = sVolNameBuf;
         }
         if( fFree )
            hb_xfree( sDiskName );
      }
#if defined(HB_OS_WIN_32)
      bReturn = SetVolumeLabel( sRoot, sVolName );
#endif
   }
   hb_retl( bReturn );
}

/*
 * GetVolInfo() is a new function. It returns the volume name of a Floppy, CD,
 * Hard-disk or mapped network drive.
 * Sintax is: GetVolInfo("x:\")
 * Note that the trailing backslash is required.
 */
HB_FUNC( GETVOLINFO )
{
#if defined(HB_OS_WIN_32)
   int iretval;
   char *sDrive = hb_parcx( 1 );
   char sVolName[255];

   if( sDrive[0] == 0 )
   {
      sDrive = NULL;
   }
   iretval = GetVolumeInformation( sDrive, sVolName, 256, NULL, NULL, NULL, NULL, 0 );

   if( iretval != 0 )
      hb_retc( sVolName );
   else
      hb_retc( NULL );
#endif
}

/*
 * VolSerial() function returns the volume serial number of an drive letter like
 * floppy, Hard-disk, CD or mapped network drive. The return value is a dword
 * type. If the drive is not available, volserial() returns -1.
 *
 * Sintax is: VolSerial("x:\")
 * Note that the trailing backslash is required.
 *
 * To convert in the hex format, call numtohex() function. 
 * Example: numtohex( volserial("c:\")). 
 * See volser.prg in xharbour\tests\cttest folder.
 */

HB_FUNC( VOLSERIAL )
{
#if defined(HB_OS_WIN_32)
   int retval;
   char *sDrive = hb_parcx( 1 );
   DWORD dSerial;

   if( sDrive[0] == 0 )
   {
      sDrive = NULL;
   }
   retval = GetVolumeInformation( sDrive,       /* RootPathName */
                                  NULL,         /* VolumeName */
                                  0,            /* VolumeNameSize */
                                  &dSerial,     /* VolumeSerialNumber */
                                  NULL,         /* MaxComponentLength */
                                  NULL,         /* FileSystemFlags */
                                  NULL,         /* FileSystemName */
                                  0 );          /* FileSystemSize */

   if( retval != 0 )
      hb_retnd( dSerial );
   else
      hb_retni( -1 );
#endif
}

HB_FUNC( TRUENAME )
{
   if( ISCHAR( 1 ) )
   {
      char *szFile = hb_parc( 1 );

#ifdef HB_OS_WIN_32
      char *szBuffRet = NULL;
      char buffer[MAX_PATH + 1] = { 0 };

      GetFullPathName( ( LPCSTR ) szFile, MAX_PATH, ( LPSTR ) buffer, &szBuffRet );
      hb_retc( buffer );
#else
      hb_retc( szFile );
#endif
   }
   else
   {
      hb_retc( NULL );
   }
}

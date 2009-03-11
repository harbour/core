/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 files functions
 *
 * SETFATTR
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *
 * SETFDATI, FILESMAX, FILEDELETE
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 *
 * FILESEEK, FILESIZE, FILEATTR, FILETIME, FILEDATE
 * FILEMOVE, FILESMAX,
 * DELETEFILE, RENAMEFILE
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
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

/* OS2 */
#define INCL_DOSFILEMGR   /* File Manager values */
#define INCL_DOSERRORS    /* DOS error values    */
#define INCL_DOSDATETIME  /* DATETIME functions  */

/* Windows */
#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"
#include "hb_io.h"
#include "ctdisk.ch"

#if defined( __DJGPP__ )
#  include <dpmi.h>
#  include <go32.h>
#  include <sys/farptr.h>
#  include <sys/param.h>
#endif
#if defined( HB_OS_UNIX_COMPATIBLE ) || defined( __DJGPP__ )
#  include <sys/types.h>
#  include <utime.h>
#  include <unistd.h>
#  include <time.h>
#elif defined( HB_OS_OS2 )
#  include <os2.h>
#  include <stdio.h>
#endif


typedef struct
{
   PHB_FFIND   ffind;
   ULONG       ulAttr;
} HB_FFDATA, * PHB_FFDATA;

static void hb_fileFindRelease( void * cargo )
{
   PHB_FFDATA pFFData = ( PHB_FFDATA ) cargo;

   if( pFFData->ffind )
      hb_fsFindClose( pFFData->ffind );
}

static HB_TSD_NEW( s_FFData, sizeof( HB_FFDATA ), NULL, hb_fileFindRelease );

#define HB_GET_FFDATA() ( ( PHB_FFDATA ) hb_stackGetTSD( &s_FFData ) )


static PHB_FFIND _hb_fileStart( BOOL fNext, ULONG ulAttr )
{
   PHB_FFDATA pFFData = HB_GET_FFDATA();

   if( hb_pcount() > 0 )
   {
      char * szFile = hb_parc( 1 );
      BOOL fFree;

      if( pFFData->ffind )
      {
         hb_fsFindClose( pFFData->ffind );
         pFFData->ffind = NULL;
      }

      if( szFile )
      {
         szFile = ( char * ) hb_fsNameConv( ( BYTE * ) szFile, &fFree );
         if( ISNUM( 2 ) )
            ulAttr = ( ULONG ) hb_parnl( 2 );
         pFFData->ulAttr = ISLOG( 3 ) && hb_parl( 3 ) ? ulAttr : 0;
         pFFData->ffind = hb_fsFindFirst( szFile, ulAttr );
         if( fFree )
            hb_xfree( szFile );
         while( pFFData->ffind && pFFData->ulAttr &&
                pFFData->ffind->attr != pFFData->ulAttr )
         {
            if( !hb_fsFindNext( pFFData->ffind ) )
            {
               hb_fsFindClose( pFFData->ffind );
               pFFData->ffind = NULL;
            }
         }
      }
   }
   else if( fNext && pFFData->ffind )
   {
      do
      {
         if( !hb_fsFindNext( pFFData->ffind ) )
         {
            hb_fsFindClose( pFFData->ffind );
            pFFData->ffind = NULL;
            break;
         }
      }
      while( pFFData->ulAttr && pFFData->ffind->attr != pFFData->ulAttr );
   }

   return pFFData->ffind;
}

HB_FUNC( FILESEEK )
{
   PHB_FFIND ffind = _hb_fileStart( TRUE, HB_FA_ALL );

   hb_retc( ffind ? ffind->szName : NULL );
}

HB_FUNC( FILEATTR )
{
   /* CT3 uses 63 as attribute mask but the idea was setting ALL
    * attributes and because we are supporting more attributes
    * then I decided to use 0xffff value. [druzus]
    */
   PHB_FFIND ffind = _hb_fileStart( FALSE, 0xffff );

   hb_retni( ffind ? ffind->attr : 0 );
}

HB_FUNC( FILESIZE )
{
   PHB_FFIND ffind = _hb_fileStart( FALSE, HB_FA_ALL );

   hb_retnint( ffind ? ffind->size : -1 );
}

HB_FUNC( FILEDATE )
{
   PHB_FFIND ffind = _hb_fileStart( FALSE, HB_FA_ALL );

   hb_retdl( ffind ? ffind->lDate : 0 );
}

HB_FUNC( FILETIME )
{
   PHB_FFIND ffind = _hb_fileStart( FALSE, HB_FA_ALL );

   hb_retc( ffind ? ffind->szTime : NULL );
}


HB_FUNC( SETFATTR )
{
   int iResult;

   if( hb_fsSetAttr( ( BYTE * ) hb_parcx( 1 ),
                        ISNUM( 2 ) ? hb_parnl( 2 ) : HB_FA_ARCHIVE ) )
      iResult = 0;
   else
      iResult = -1;

   hb_retni( iResult );
}


HB_FUNC( SETFDATI )
{
   if( hb_pcount() >= 1 )
   {
      PHB_ITEM pDate, pTime;
      char *szFile = hb_parcx( 1 );
      int year = 0, month = 0, day = 0, hour = 0, minute = 0, second = 0;

      pDate = hb_param( 2, HB_IT_DATE );
      if( !pDate )
         pDate = hb_param( 3, HB_IT_DATE );
      if( pDate )
         hb_dateDecode( hb_itemGetDL( pDate ), &year, &month, &day );

      pTime = hb_param( 2, HB_IT_STRING );
      if( !pTime )
         pTime = hb_param( 3, HB_IT_STRING );
      if( pTime )
         hb_timeStrGet( hb_itemGetCPtr( pTime ), &hour, &minute, &second, NULL );

#if defined( HB_OS_WIN ) && !defined( __CYGWIN__ )
      {
         FILETIME ft, local_ft;
         SYSTEMTIME st;
         HANDLE f = ( HANDLE ) _lopen( szFile, OF_READWRITE | OF_SHARE_COMPAT );

         if( f != ( HANDLE ) HFILE_ERROR )
         {
            if( !pDate || !pTime )
               GetLocalTime( &st );
            if( pDate )
            {
               st.wYear = ( WORD ) year;
               st.wMonth = ( WORD ) month;
               st.wDay = ( WORD ) day;
            }
            if( pTime )
            {
               st.wHour = ( WORD ) hour;
               st.wMinute = ( WORD ) minute;
               st.wSecond = ( WORD ) second;
            }
            SystemTimeToFileTime( &st, &local_ft );
            LocalFileTimeToFileTime( &local_ft, &ft );
            hb_retl( SetFileTime( f, NULL, &ft, &ft ) != 0 );
            _lclose( ( HFILE ) f );
            return;
         }
      }
#elif defined( HB_OS_OS2 )
      {
         FILESTATUS3 fs3;
         APIRET ulrc;

         ulrc = DosQueryPathInfo( ( PCSZ ) szFile, FIL_STANDARD, &fs3, sizeof( fs3 ) );
         if( ulrc == NO_ERROR )
         {
            FDATE fdate;
            FTIME ftime;

            if( !pDate || !pTime )
            {
               DATETIME dt;

               DosGetDateTime( &dt );

               fdate.year = dt.year - 1980;
               fdate.month = dt.month;
               fdate.day = dt.day;
               ftime.hours = dt.hours;
               ftime.minutes = dt.minutes;
               ftime.twosecs = dt.seconds / 2;
            }

            if( pDate )
            {
               fdate.year = year - 1980;
               fdate.month = month;
               fdate.day = day;
            }
            if( pTime )
            {
               ftime.hours = hour;
               ftime.minutes = minute;
               ftime.twosecs = second / 2;
            }
            fs3.fdateCreation = fs3.fdateLastAccess = fs3.fdateLastWrite = fdate;
            fs3.ftimeCreation = fs3.ftimeLastAccess = fs3.ftimeLastWrite = ftime;
            ulrc = DosSetPathInfo( ( PCSZ ) szFile, FIL_STANDARD,
                                   &fs3, sizeof( fs3 ), DSPI_WRTTHRU );
         }
         hb_retl( ulrc == NO_ERROR );
         return;
      }
#elif defined( HB_OS_UNIX_COMPATIBLE ) || defined( __DJGPP__ )

      if( !pDate && !pTime )
      {
         hb_retl( utime( szFile, NULL ) == 0 );
         return;
      }
      else
      {
         struct utimbuf buf;
         struct tm new_value;

         if( !pDate || !pTime )
         {
            time_t current_time;

            current_time = time( NULL );
#if defined( HB_HAS_LOCALTIME_R )
            localtime_r( &current_time, &new_value );
#   else
            new_value = *localtime( &current_time );
#   endif
         }
         else
            memset( &new_value, 0, sizeof( new_value ) );

         if( pDate )
         {
            new_value.tm_year = year - 1900;
            new_value.tm_mon = month - 1;
            new_value.tm_mday = day;
         }
         if( pTime )
         {
            new_value.tm_hour = hour;
            new_value.tm_min = minute;
            new_value.tm_sec = second;
         }
         buf.actime = buf.modtime = mktime( &new_value );
         hb_retl( utime( szFile, &buf ) == 0 );
         return;
      }
#else
      {
         LONG lJulian, lMillisec;

         lJulian = pDate ? hb_dateEncode( year, month, day ) : -1;
         lMillisec = pTime ? hb_timeStampEncode( hour, minute, second, 0 ) : -1;

         hb_retl( hb_fsSetFileTime( ( BYTE * ) szFile, lJulian, lMillisec ) );
         return;
      }
#endif
   }

   hb_retl( FALSE );
}


HB_FUNC( FILEDELETE )
{
   BOOL bReturn = FALSE;

   if( ISCHAR( 1 ) )
   {
      BYTE * pDirSpec;
      PHB_FFIND ffind;
      ULONG ulAttr = HB_FA_ALL;
      BOOL fFree;

      pDirSpec = hb_fsNameConv( ( BYTE * ) hb_parc( 1 ), &fFree );
      if( ISNUM( 2 ) )
         ulAttr = hb_parnl( 2 );

      if( ( ffind = hb_fsFindFirst( ( char * ) pDirSpec, ulAttr ) ) != NULL )
      {
         PHB_FNAME pFilepath;

         pFilepath = hb_fsFNameSplit( ( char * ) pDirSpec );
         pFilepath->szExtension = NULL;

         do
         {
            char szPath[ _POSIX_PATH_MAX + 1 ];

            pFilepath->szName = ffind->szName;
            hb_fsFNameMerge( szPath, pFilepath );

            if( hb_fsDelete( ( BYTE * ) szPath ) )
               bReturn = TRUE;
         }
         while( hb_fsFindNext( ffind ) );

         hb_xfree( pFilepath );
         hb_fsFindClose( ffind );
      }
      if( fFree )
         hb_xfree( pDirSpec );
   }

   hb_retl( bReturn );
}


HB_FUNC( FILEMOVE )
{
   hb_retni( hb_fsRename( ( BYTE * ) hb_parcx( 1 ),
                          ( BYTE * ) hb_parcx( 2 ) ) ? 0 : -hb_fsOsError() );
}


HB_FUNC( RENAMEFILE )
{
   HB_FUNC_EXEC( FILEMOVE );
}


HB_FUNC( DELETEFILE )
{
   hb_retni( hb_fsDelete( ( BYTE * ) hb_parcx( 1 ) ) ? 0 : -hb_fsOsError() );
}


HB_FUNC( FILESMAX )
{
#if defined( __DJGPP__ )
   __dpmi_regs r;
   unsigned handles;
   ULONG psp;

   r.h.ah = 0x62;               /* Get PSP address */
   __dpmi_int( 0x21, &r );
   psp = ( ( ( ULONG ) r.x.bx ) << 4 ) & 0xFFFFF;

   handles = _farpeekw( _dos_ds, psp + 0x32 );
   hb_retni( handles );
#elif defined( _SC_OPEN_MAX )
   hb_retnl( sysconf( _SC_OPEN_MAX ) );
#else
   hb_retni( -1 );
#endif
}

/*
 * Harbour Project source code:
 *   CT3 files functions
 *
 * SetFattr()
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
 *
 * SetFDaTi(), FileSMax(), FileDelete()
 * Copyright 2004 Phil Krylov <phil@newstar.rinet.ru>
 *
 * FileSeek(), FileSize(), FileAttr(), FileTime(), FileDate()
 * FileMove(), FileSMax(),
 * DeleteFile(), RenameFile()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#if defined( HB_OS_UNIX ) || defined( __DJGPP__ )
#  include <sys/types.h>
#  include <utime.h>
#  include <unistd.h>
#  include <time.h>
#endif

typedef struct
{
   PHB_FFIND ffind;
   HB_FATTR  ulAttr;
} HB_FFDATA, * PHB_FFDATA;

static void hb_fileFindRelease( void * cargo )
{
   PHB_FFDATA pFFData = ( PHB_FFDATA ) cargo;

   if( pFFData->ffind )
      hb_fsFindClose( pFFData->ffind );
}

static HB_TSD_NEW( s_FFData, sizeof( HB_FFDATA ), NULL, hb_fileFindRelease );

#define HB_GET_FFDATA()  ( ( PHB_FFDATA ) hb_stackGetTSD( &s_FFData ) )

/* limit attributes to DOS ones for code portability */
#define HB_FF_ATTR( ff ) ( ( ff )->attr & 0xFF )


static PHB_FFIND _hb_fileStart( HB_BOOL fNext, HB_BOOL fAny )
{
   PHB_FFDATA pFFData = HB_GET_FFDATA();

   if( hb_pcount() > 0 )
   {
      const char * szFile = hb_parc( 1 );

      if( pFFData->ffind )
      {
         hb_fsFindClose( pFFData->ffind );
         pFFData->ffind = NULL;
      }

      if( szFile )
      {
         HB_FATTR ulAttr;

         ulAttr = ( HB_FATTR ) hb_parnldef( 2, fAny ? HB_FA_ANY : HB_FA_ALL );
         pFFData->ulAttr = hb_parl( 3 ) ? ulAttr : 0;
         pFFData->ffind  = hb_fsFindFirst( szFile, ulAttr );
         while( pFFData->ffind && pFFData->ulAttr &&
                HB_FF_ATTR( pFFData->ffind ) != pFFData->ulAttr )
         {
            if( ! hb_fsFindNext( pFFData->ffind ) )
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
         if( ! hb_fsFindNext( pFFData->ffind ) )
         {
            hb_fsFindClose( pFFData->ffind );
            pFFData->ffind = NULL;
            break;
         }
      }
      while( pFFData->ulAttr && HB_FF_ATTR( pFFData->ffind ) != pFFData->ulAttr );
   }

   return pFFData->ffind;
}

HB_FUNC( FILESEEK )
{
   PHB_FFIND ffind = _hb_fileStart( HB_TRUE, HB_FALSE );

   hb_retc( ffind ? ffind->szName : NULL );
}

HB_FUNC( FILEATTR )
{
   PHB_FFIND ffind = _hb_fileStart( HB_FALSE, HB_TRUE );

   hb_retni( ffind ? HB_FF_ATTR( ffind ) : 0 );
}

HB_FUNC( FILESIZE )
{
   PHB_FFIND ffind = _hb_fileStart( HB_FALSE, HB_FALSE );

   hb_retnint( ffind ? ffind->size : -1 );
}

HB_FUNC( FILEDATE )
{
   PHB_FFIND ffind = _hb_fileStart( HB_FALSE, HB_FALSE );

   hb_retdl( ffind ? ffind->lDate : 0 );
}

HB_FUNC( FILETIME )
{
   PHB_FFIND ffind = _hb_fileStart( HB_FALSE, HB_FALSE );

   hb_retc( ffind ? ffind->szTime : NULL );
}

HB_FUNC( SETFATTR )
{
   int iResult;

   if( hb_fsSetAttr( hb_parcx( 1 ), hb_parnldef( 2, HB_FA_ARCHIVE ) ) )
      iResult = 0;
   else
      iResult = -1;

   hb_retni( iResult );
}

HB_FUNC( SETFDATI )
{
   const char * szFile = hb_parc( 1 );
   HB_BOOL fResult = HB_FALSE;

   if( szFile && *szFile )
   {
      PHB_ITEM pDate, pTime;
      long lJulian, lMillisec;

      if( HB_ISTIMESTAMP( 1 ) )
         hb_partdt( &lJulian, &lMillisec, 1 );
      else
      {
         pDate = hb_param( 2, HB_IT_DATE );
         if( pDate )
            pTime = hb_param( 3, HB_IT_STRING );
         else
         {
            pTime = hb_param( 2, HB_IT_STRING );
            pDate = hb_param( 3, HB_IT_DATE );
         }
         lJulian = pDate ? hb_itemGetDL( pDate ) : -1;
         if( pTime )
         {
            int hour = 0, minute = 0, second = 0, msec = 0;
            hb_timeStrGet( hb_itemGetCPtr( pTime ), &hour, &minute, &second, &msec );
            lMillisec = hb_timeEncode( hour, minute, second, msec );
         }
         else
            lMillisec = -1;
      }
      fResult = hb_fsSetFileTime( szFile, lJulian, lMillisec );
   }

   hb_retl( fResult );
}

HB_FUNC( FILEDELETE )
{
   HB_BOOL bReturn = HB_FALSE;

   if( HB_ISCHAR( 1 ) )
   {
      const char * pszDirSpec = hb_parc( 1 );
      HB_FATTR ulAttr = hb_parnldef( 2, HB_FA_ALL );
      PHB_FFIND ffind;

      if( ( ffind = hb_fsFindFirst( pszDirSpec, ulAttr ) ) != NULL )
      {
         PHB_FNAME pFilepath;

         pFilepath = hb_fsFNameSplit( pszDirSpec );
         pFilepath->szExtension = NULL;

         do
         {
            char szPath[ HB_PATH_MAX ];

            pFilepath->szName = ffind->szName;
            hb_fsFNameMerge( szPath, pFilepath );

            if( hb_fsDelete( szPath ) )
               bReturn = HB_TRUE;
         }
         while( hb_fsFindNext( ffind ) );

         hb_xfree( pFilepath );
         hb_fsFindClose( ffind );
      }
   }

   hb_retl( bReturn );
}

HB_FUNC( FILEMOVE )
{
   hb_retnint( hb_fsRename( hb_parcx( 1 ),
                            hb_parcx( 2 ) ) ? 0 : -( HB_MAXINT ) hb_fsOsError() );
}

HB_FUNC_TRANSLATE( RENAMEFILE, FILEMOVE )

HB_FUNC( DELETEFILE )
{
   hb_retnint( hb_fsDelete( hb_parcx( 1 ) ) ? 0 : -( HB_MAXINT ) hb_fsOsError() );
}

HB_FUNC( FILESMAX )
{
#if defined( __DJGPP__ )
   __dpmi_regs r;
   unsigned handles;
   HB_ULONG psp;

   r.h.ah = 0x62;               /* Get PSP address */
   __dpmi_int( 0x21, &r );
   psp = ( ( ( HB_ULONG ) r.x.bx ) << 4 ) & 0xFFFFF;

   handles = _farpeekw( _dos_ds, psp + 0x32 );
   hb_retni( handles );
#elif defined( _SC_OPEN_MAX )
   hb_retnl( sysconf( _SC_OPEN_MAX ) );
#else
   hb_retni( -1 );
#endif
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API (Harbour level)
 *
 * Copyright 1999-2009 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbdate.h"

HB_FUNC( FOPEN )
{
   const char * szFile = hb_parc( 1 );
   if( szFile )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsOpen( szFile,
                  HB_ISNUM( 2 ) ? ( USHORT ) hb_parni( 2 ) : FO_READ | FO_COMPAT ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_fsSetFError( 0 );
      /* NOTE: Undocumented but existing Clipper Run-time error */
      hb_errRT_BASE( EG_ARG, 2021, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

HB_FUNC( FCREATE )
{
   const char * szFile = hb_parc( 1 );
   if( szFile )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsCreate( szFile,
                  HB_ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( F_ERROR );
      hb_fsSetFError( 0 );
   }
}

HB_FUNC( HB_FCREATE )
{
   const char * szFile = hb_parc( 1 );
   if( szFile )
   {
      hb_retnint( ( HB_NHANDLE ) hb_fsCreateEx( szFile,
                  HB_ISNUM( 2 ) ? hb_parni( 2 ) : FC_NORMAL,
                  HB_ISNUM( 3 ) ? ( USHORT ) hb_parni( 3 ) : FO_COMPAT ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
   {
      hb_retni( F_ERROR );
      hb_fsSetFError( 0 );
   }
}

HB_FUNC( FREAD )
{
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   USHORT uiError = 0;
   ULONG ulRead = 0, ulSize;
   char * buffer;

   if( HB_ISNUM( 1 ) && pBuffer && HB_ISBYREF( 2 ) && HB_ISNUM( 3 ) )
   {
      ulRead = hb_parnl( 3 );

      /* NOTE: CA-Cl*pper determines the maximum size by calling _parcsiz()
               instead of _parclen(), this means that the maximum read length
               will be one more than the length of the passed buffer, because
               the terminating zero could be used if needed. [vszakats] */

      if( ulRead <= hb_parcsiz( 2 ) &&
          hb_itemGetWriteCL( pBuffer, &buffer, &ulSize ) )
      {
         ulRead = hb_fsReadLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                  ( BYTE * ) buffer, ulRead );
         uiError = hb_fsError();
      }
      else
         ulRead = 0;
   }

   hb_retnint( ulRead );
   hb_fsSetFError( uiError );
}

HB_FUNC( FWRITE )
{
   USHORT uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) )
   {
      ULONG nLen = hb_parclen( 2 );

      if( HB_ISNUM( 3 ) )
      {
         ULONG nWrite = ( ULONG ) hb_parnl( 3 );
         if( nWrite < nLen )
            nLen = nWrite;
      }

      hb_retnl( hb_fsWriteLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                 ( const BYTE * ) hb_parc( 2 ), nLen ) );
      uiError = hb_fsError();
   }
   else
      hb_retni( 0 );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERROR )
{
   hb_retni( hb_fsGetFError() );
}

HB_FUNC( FCLOSE )
{
   USHORT uiError = 0;
   if( HB_ISNUM( 1 ) )
   {
      hb_fsClose( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
      hb_retl( uiError == 0 );
   }
   else
      hb_retl( FALSE );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERASE )
{
   USHORT uiError = 3;
   const char * szFile = hb_parc( 1 );

   if( szFile )
   {
      hb_retni( hb_fsDelete( szFile ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( FRENAME )
{
   USHORT uiError = 2;
   const char * szFileOld = hb_parc( 1 ),
              * szFileNew = hb_parc( 1 );

   if( szFileOld && szFileNew )
   {
      hb_retni( hb_fsRename( szFileOld, szFileNew ) ? 0 : F_ERROR );
      uiError = hb_fsError();
   }
   else
      hb_retni( F_ERROR );
   hb_fsSetFError( uiError );
}

HB_FUNC( FSEEK )
{
   USHORT uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      hb_retnint( hb_fsSeekLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                  hb_parnint( 2 ),
                                  HB_ISNUM( 3 ) ? ( USHORT ) hb_parni( 3 ) : FS_SET ) );
      uiError = hb_fsError();
   }
   else
      hb_retni( 0 );

   hb_fsSetFError( uiError );
}

HB_FUNC( FREADSTR )
{
   USHORT uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      ULONG ulToRead = ( ULONG ) hb_parnl( 2 );

      if( ulToRead > 0 )
      {
         HB_FHANDLE fhnd = ( HB_FHANDLE ) hb_parni( 1 );
         char * buffer = ( char * ) hb_xgrab( ulToRead + 1 );
         ULONG ulRead;

         ulRead = hb_fsReadLarge( fhnd, ( BYTE * ) buffer, ulToRead );
         uiError = hb_fsError();
         buffer[ ulRead ] = '\0';

         /* NOTE: Clipper will not return zero chars from this functions. */
         hb_retc_buffer( buffer );
      }
      else
         hb_retc_null();
   }
   else
      hb_retc_null();
   hb_fsSetFError( uiError );
}

/* NOTE: This function should not return the leading and trailing */
/*       (back)slashes. [vszakats] */

/* TODO: Xbase++ is able to change to the specified directory. */

HB_FUNC( CURDIR )
{
   char szBuffer[ HB_PATH_MAX ];
   USHORT uiDrive = 0;
   const char * szDrive;

   szDrive = hb_parc( 1 );
   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         uiDrive = *szDrive - ( 'A' - 1 );
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         uiDrive = *szDrive - ( 'a' - 1 );
   }
   hb_fsCurDirBuff( uiDrive, szBuffer, sizeof( szBuffer ) );

   hb_retc( szBuffer );
}

HB_FUNC( HB_PROGNAME )
{
   const char * szBaseName = hb_cmdargARGVN( 0 );

   if( szBaseName )
   {
      /* Convert from OS codepage */
      char * pszFree;

      szBaseName = hb_osDecode( szBaseName, &pszFree );

      if( pszFree )
         hb_retc_buffer( pszFree );
      else
         hb_retc( szBaseName );
   }
   else
      hb_retc_null();
}

HB_FUNC( HB_DIRBASE )
{
   char szBuffer[ HB_PATH_MAX ];

   hb_fsBaseDirBuff( szBuffer );

   hb_retc( szBuffer );
}

HB_FUNC( HB_FEOF )
{
   USHORT uiError = 6;

   if( HB_ISNUM( 1 ) )
   {
      hb_retl( hb_fsEof( hb_numToHandle( hb_parnint( 1 ) ) ) );
      uiError = hb_fsError();
   }
   else
      hb_retl( TRUE );
   hb_fsSetFError( uiError );
}

HB_FUNC( HB_FCOMMIT )
{
   USHORT uiError = 6;

   if( HB_ISNUM( 1 ) )
   {
      hb_fsCommit( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
}

HB_FUNC( HB_FLOCK )
{
   USHORT uiError = 0;
   BOOL fResult = FALSE;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      fResult = hb_fsLockLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_LOCK | ( ( USHORT ) hb_parni( 4 ) & ~FL_MASK ) );
      uiError = hb_fsError();
   }
   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

HB_FUNC( HB_FUNLOCK )
{
   USHORT uiError = 0;
   BOOL fResult = FALSE;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      fResult = hb_fsLockLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_UNLOCK );
      uiError = hb_fsError();
   }
   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

HB_FUNC( HB_FGETATTR )
{
   ULONG nAttr;

   hb_retl( hb_fsGetAttr( hb_parcx( 1 ), &nAttr ) );

   hb_stornl( nAttr, 2 );
}

HB_FUNC( HB_FSETATTR )
{
   hb_retl( hb_fsSetAttr( hb_parcx( 1 ), hb_parnl( 2 ) ) );
}

HB_FUNC( HB_FSETDATETIME )
{
   LONG lDate = -1, lTime = -1;

   if( HB_ISTIMESTAMP( 2 ) )
      hb_partdt( &lDate, &lTime, 2 );
   else
   {
      if( HB_ISDATE( 2 ) )
         lDate = hb_pardl( 2 );
      if( HB_ISCHAR( 3 ) )
      {
         int iHour, iMinutes, iSeconds, iMSec;
         if( hb_timeStrGet( hb_parc( 3 ), &iHour, &iMinutes, &iSeconds, &iMSec ) )
            lTime = hb_timeEncode( iHour, iMinutes, iSeconds, iMSec );
      }
   }

   hb_retl( hb_fsSetFileTime( hb_parcx( 1 ), lDate, lTime ) );
}

HB_FUNC( HB_FGETDATETIME )
{
   LONG lJulian, lMillisec;

   if( hb_fsGetFileTime( hb_parcx( 1 ), &lJulian, &lMillisec ) )
   {
      if( HB_ISBYREF( 3 ) )
      {
         char buf[ 13 ];
         hb_timeStr( buf, lMillisec );
         if( lMillisec % 1000 == 0 )
            buf[ 8 ] = '\0';
         hb_storc( buf, 3 );
         hb_stordl( lJulian, 2 );
      }
      else
         hb_stortdt( lJulian, lMillisec, 2 );

      hb_retl( TRUE );
   }
   else
      hb_retl( FALSE );
}

HB_FUNC( HB_OSERROR )
{
   hb_retni( hb_fsOsError() );
}

HB_FUNC( HB_OSPATHSEPARATOR )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_STRING );
}

HB_FUNC( HB_OSPATHLISTSEPARATOR )
{
   static const char s_ret[ 2 ] = { HB_OS_PATH_LIST_SEP_CHR, '\0' };
   hb_retc_const( s_ret );
}

HB_FUNC( HB_OSPATHDELIMITERS )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_LIST );
}

HB_FUNC( HB_OSDRIVESEPARATOR )
{
#ifdef HB_OS_HAS_DRIVE_LETTER
   static const char s_ret[ 2 ] = { HB_OS_DRIVE_DELIM_CHR, '\0' };
   hb_retc_const( s_ret );
#else
   hb_retc_null();
#endif
}

HB_FUNC( HB_OSFILEMASK )
{
   hb_retc_const( HB_OS_ALLFILE_MASK );
}

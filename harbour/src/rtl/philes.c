/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The FileSys API (Harbour level)
 *
 * Copyright 1999-2009 Viktor Szakats (harbour syenar.net)
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * Copyright 2000 David G. Holm <dholm@jsd-llc.com>
 * Copyright 1999 Manuel Ruiz <mrt@joca.es>
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
                  ( HB_USHORT ) hb_parnidef( 2, FO_READ | FO_COMPAT ) ) );
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
                  hb_parnidef( 2, FC_NORMAL ) ) );
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
                  hb_parnidef( 2, FC_NORMAL ),
                  ( HB_USHORT ) hb_parnidef( 3, FO_COMPAT ) ) );
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
   HB_ERRCODE uiError = 0;
   HB_SIZE nRead = 0;

   if( HB_ISNUM( 1 ) && pBuffer && HB_ISBYREF( 2 ) && HB_ISNUM( 3 ) )
   {
      char * buffer;
      HB_SIZE nSize;

      nRead = hb_parns( 3 );

      /* NOTE: CA-Cl*pper determines the maximum size by calling _parcsiz()
               instead of _parclen(), this means that the maximum read length
               will be one more than the length of the passed buffer, because
               the terminating zero could be used if needed. [vszakats] */

      if( nRead <= hb_parcsiz( 2 ) &&
          hb_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
      {
         nRead = hb_fsReadLarge( hb_numToHandle( hb_parnint( 1 ) ), buffer, nRead );
         uiError = hb_fsError();
      }
      else
         nRead = 0;
   }

   hb_retns( nRead );
   hb_fsSetFError( uiError );
}

HB_FUNC( FWRITE )
{
   HB_ERRCODE uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISCHAR( 2 ) )
   {
      HB_SIZE nLen = hb_parclen( 2 );

      if( HB_ISNUM( 3 ) )
      {
         HB_SIZE nWrite = hb_parns( 3 );
         if( nWrite < nLen )
            nLen = nWrite;
      }

      hb_retns( hb_fsWriteLarge( hb_numToHandle( hb_parnint( 1 ) ), hb_parc( 2 ), nLen ) );
      uiError = hb_fsError();
   }
   else
      hb_retns( 0 );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERROR )
{
   hb_retni( hb_fsGetFError() );
}

HB_FUNC( FCLOSE )
{
   HB_ERRCODE uiError = 0;

   if( HB_ISNUM( 1 ) )
   {
      hb_fsClose( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
      hb_retl( uiError == 0 );
   }
   else
      hb_retl( HB_FALSE );
   hb_fsSetFError( uiError );
}

HB_FUNC( FERASE )
{
   HB_ERRCODE uiError = 3;
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
   HB_ERRCODE uiError = 2;
   const char * szFileOld = hb_parc( 1 ),
              * szFileNew = hb_parc( 2 );

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
   HB_ERRCODE uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      hb_retnint( hb_fsSeekLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                  hb_parnint( 2 ),
                                  ( HB_USHORT ) hb_parnidef( 3, FS_SET ) ) );
      uiError = hb_fsError();
   }
   else
      hb_retni( 0 );

   hb_fsSetFError( uiError );
}

HB_FUNC( FREADSTR )
{
   HB_ERRCODE uiError = 0;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      HB_SIZE nToRead = hb_parns( 2 );

      if( nToRead > 0 )
      {
         HB_FHANDLE fhnd = ( HB_FHANDLE ) hb_parni( 1 );
         char * buffer = ( char * ) hb_xgrab( nToRead + 1 );
         HB_SIZE nRead;

         nRead = hb_fsReadLarge( fhnd, buffer, nToRead );
         uiError = hb_fsError();
         buffer[ nRead ] = '\0';

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
   int iDrive = 0;
   const char * szDrive;

   szDrive = hb_parc( 1 );
   if( szDrive )
   {
      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         iDrive = *szDrive - ( 'A' - 1 );
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         iDrive = *szDrive - ( 'a' - 1 );
   }
   hb_fsCurDirBuff( iDrive, szBuffer, sizeof( szBuffer ) );

   hb_retc( szBuffer );
}

HB_FUNC( HB_CURDRIVE )
{
#if defined( HB_OS_HAS_DRIVE_LETTER )
   char szCurDrive[ 1 ];
   const char * szDrive;

   szCurDrive[ 0 ] = ( ( char ) hb_fsCurDrv() ) + 'A';
   hb_retclen( szCurDrive, 1 );

   szDrive = hb_parc( 1 );
   if( szDrive )
   {
      int iDrive = -1;

      if( *szDrive >= 'A' && *szDrive <= 'Z' )
         iDrive = *szDrive - 'A';
      else if( *szDrive >= 'a' && *szDrive <= 'z' )
         iDrive = *szDrive - 'a';

      if( iDrive >= 0 )
      {
         while( hb_fsChDrv( iDrive ) != 0 )
         {
            HB_USHORT uiAction = hb_errRT_BASE_Ext1( EG_OPEN, 6001, NULL,
                                                     HB_ERR_FUNCNAME, 0, EF_CANDEFAULT | EF_CANRETRY,
                                                     HB_ERR_ARGS_BASEPARAMS );
            if( uiAction != E_RETRY )
               break;
         }
      }
   }
#else
   hb_retc_null();
#endif
}

HB_FUNC( HB_CWD )
{
   char szBuffer[ HB_PATH_MAX ];
   const char * szNewWD;

   if( hb_fsGetCWD( szBuffer, sizeof( szBuffer ) ) )
      hb_retc( szBuffer );
   else
      hb_retc_null();

   szNewWD = hb_parc( 1 );
   if( szNewWD )
      hb_fsSetCWD( szNewWD );

   hb_fsSetFError( hb_fsError() );
}

HB_FUNC( HB_PROGNAME )
{
   char * pszBaseName = hb_cmdargProgName();

   if( pszBaseName )
      hb_retc_buffer( pszBaseName );
   else
      hb_retc_null();
}

HB_FUNC( HB_DIRBASE )
{
   char szBuffer[ HB_PATH_MAX ];

   hb_fsBaseDirBuff( szBuffer );

   hb_retc( szBuffer );
}

HB_FUNC( HB_FCOMMIT )
{
   HB_ERRCODE uiError = 6;

   if( HB_ISNUM( 1 ) )
   {
      hb_fsCommit( hb_numToHandle( hb_parnint( 1 ) ) );
      uiError = hb_fsError();
   }

   hb_fsSetFError( uiError );
}

HB_FUNC( HB_FLOCK )
{
   HB_ERRCODE uiError = 0;
   HB_BOOL fResult = HB_FALSE;

   if( HB_ISNUM( 1 ) && HB_ISNUM( 2 ) && HB_ISNUM( 3 ) )
   {
      fResult = hb_fsLockLarge( hb_numToHandle( hb_parnint( 1 ) ),
                                ( HB_FOFFSET ) hb_parnint( 2 ),
                                ( HB_FOFFSET ) hb_parnint( 3 ),
                                FL_LOCK | ( ( HB_USHORT ) hb_parni( 4 ) & ~FL_MASK ) );
      uiError = hb_fsError();
   }
   hb_fsSetFError( uiError );
   hb_retl( fResult );
}

HB_FUNC( HB_FUNLOCK )
{
   HB_ERRCODE uiError = 0;
   HB_BOOL fResult = HB_FALSE;

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
   HB_FATTR nAttr;

   hb_retl( hb_fsGetAttr( hb_parcx( 1 ), &nAttr ) );
   hb_fsSetFError( hb_fsError() );

   hb_stornl( nAttr, 2 );
}

HB_FUNC( HB_FSETATTR )
{
   hb_retl( hb_fsSetAttr( hb_parcx( 1 ), hb_parnl( 2 ) ) );
   hb_fsSetFError( hb_fsError() );
}

HB_FUNC( HB_FSETDATETIME )
{
   long lDate = -1, lTime = -1;

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
   hb_fsSetFError( hb_fsError() );
}

HB_FUNC( HB_FGETDATETIME )
{
   long lJulian, lMillisec;
   HB_BOOL fOK;

   fOK = hb_fsGetFileTime( hb_parcx( 1 ), &lJulian, &lMillisec );
   hb_fsSetFError( hb_fsError() );

   if( fOK )
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

      hb_retl( HB_TRUE );
   }
   else
   {
      if( HB_ISBYREF( 3 ) )
      {
         hb_storc( "", 3 );
         hb_stordl( 0, 2 );
      }
      else
         hb_stortdt( 0, 0, 2 );

      hb_retl( HB_FALSE );
   }
}

HB_FUNC( HB_FSETDEVMODE )
{
   int iRet = -1;

   if( HB_ISNUM( 1 ) )
   {
      iRet = hb_fsSetDevMode( hb_numToHandle( hb_parnint( 1 ) ), hb_parni( 2 ) );
      hb_fsSetFError( hb_fsError() );
   }
   else
      hb_fsSetFError( 6 );  /* ERROR_INVALID_HANDLE */

   hb_retni( iRet );
}

HB_FUNC( HB_FISDEVICE )
{
   hb_retl( hb_fsIsDevice( hb_numToHandle( hb_parnint( 1 ) ) ) );
   hb_fsSetFError( hb_fsError() );
}

/* HB_PREAD( <nPipeHandle>, <@cBuffer>, [<nBytes>], [<nTimeOut>] )
      -> <nBytesRead> */
HB_FUNC( HB_PREAD )
{
   HB_FHANDLE hStdHandle = hb_numToHandle( hb_parnintdef( 1, FS_ERROR ) );
   PHB_ITEM pBuffer = hb_param( 2, HB_IT_STRING );
   char * buffer;
   HB_SIZE nSize;

   if( hStdHandle != FS_ERROR && pBuffer && HB_ISBYREF( 2 ) &&
       hb_itemGetWriteCL( pBuffer, &buffer, &nSize ) )
   {
      HB_ERRCODE uiError = 0;

      if( HB_ISNUM( 3 ) )
      {
         HB_ISIZ nToRead = hb_parns( 3 );

         if( nToRead >= 0 && ( HB_SIZE ) nToRead < nSize )
            nSize = nToRead;
      }

      if( nSize > 0 )
      {
         nSize = hb_fsPipeRead( hStdHandle, buffer, nSize, hb_parnint( 4 ) );
         uiError = hb_fsError();
      }
      else
         nSize = 0;

      if( nSize == ( HB_SIZE ) -1 )
         hb_retni( -1 );
      else
         hb_retns( nSize );
      hb_fsSetFError( uiError );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 4001, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( HB_OSERROR )
{
   hb_retni( hb_fsOsError() );
}

HB_FUNC( HB_PS )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_STRING );
}

#if defined( HB_LEGACY_LEVEL4 )

/* Deprecated */
HB_FUNC( HB_OSPATHSEPARATOR )
{
   hb_retc_const( HB_OS_PATH_DELIM_CHR_STRING );
}

#endif

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

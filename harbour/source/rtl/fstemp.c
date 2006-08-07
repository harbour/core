/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FTEMPCREATE() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 *                     Viktor Szakats <viktor.szakats@syenar.hu>
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

#ifndef HB_OS_WIN_32_USED
   #define HB_OS_WIN_32_USED
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbmath.h"

#if defined( HB_OS_UNIX )
#include <stdlib.h>
#include <unistd.h>  /* We need for mkstemp() on BSD */
#endif

/* NOTE: The buffer must be at least _POSIX_PATH_MAX chars long */
#if !defined( HB_OS_UNIX )

static BOOL hb_fsTempName( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix )
{
   BOOL fResult;

#if defined(HB_WIN32_IO)

   char cTempDir[ _POSIX_PATH_MAX + 1 ];

   if ( pszDir != NULL && pszDir[0] != '\0' )
   {
      strncpy( (char *) cTempDir, (const char *) pszDir, _POSIX_PATH_MAX );
   }
   else
   {
      if ( ! GetTempPath( ( DWORD ) _POSIX_PATH_MAX, cTempDir ) )
      {
         hb_fsSetIOError( FALSE, 0 );
         return FALSE;
      }
   }
   cTempDir[ _POSIX_PATH_MAX ] = '\0';

   fResult = GetTempFileName( ( LPCSTR ) cTempDir, ( ( pszPrefix == NULL ) ? ( LPCSTR ) "hb" : ( LPCSTR ) pszPrefix ), 0, ( LPSTR ) pszBuffer );

#else

   /* TODO: Implement these: */
   HB_SYMBOL_UNUSED( pszDir );
   HB_SYMBOL_UNUSED( pszPrefix );

   /* TOFIX: The spec says to reserve L_tmpnam number of characters for the
             passed buffer. It will be needed to fix _POSIX_PATH_MAX to be
             at least this large. */

   pszBuffer[ 0 ] = '\0';
   fResult = ( tmpnam( ( char * ) pszBuffer ) != NULL );

#endif

   hb_fsSetIOError( fResult, 0 );
   return fResult;
}

/* NOTE: The buffer must be at least _POSIX_PATH_MAX chars long */

HB_EXPORT FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttr, BYTE * pszName )
{
   USHORT nAttemptLeft = 999;

   while( --nAttemptLeft )
   {
      if( hb_fsTempName( pszName, pszDir, pszPrefix ) )
      {
          FHANDLE fhnd = hb_fsCreateEx( pszName, uiAttr, FO_EXCLUSIVE | FO_EXCL );

          /* This function may fail, if the generated filename got
             used between generation and the file creation. */

          if( fhnd != FS_ERROR )
          {
             return fhnd;
          }
      }
      else
      {
         /* Don't attempt to retry if the filename generator is
            failing for some reason. */
         break;
      }
   }

   return FS_ERROR;
}
#else

#include <ctype.h> /* isupper()/islower() */
#include "hbset.h"

static BOOL fsGetTempDirByCase( BYTE *pszName, const char *pszTempDir )
{
   BOOL bOk= FALSE;
   if ( pszTempDir != NULL && *pszTempDir != '\0' )
   {
      bOk = TRUE;
      strcpy( ( char * ) pszName, ( char * ) pszTempDir );
      if ( hb_set.HB_SET_DIRCASE == HB_SET_CASE_LOWER || hb_set.HB_SET_DIRCASE == HB_SET_CASE_UPPER )
      {
         /* check to see if temp directory already upper or lower. If not use current directory ( "." ) */
         char *psZ = ( char * ) pszName ;
         int iChar ;
         BOOL bLower =  hb_set.HB_SET_DIRCASE == HB_SET_CASE_LOWER  ;
         while( *psZ )
         {
            iChar = ( int ) *psZ;
            if( isalpha( iChar ) && !( bLower ? islower( iChar ) : isupper( iChar ) ) )
            {
               bOk = FALSE;
               break;
            }
            psZ++ ;
         }
      }
   }
   return( bOk ) ;
}

HB_EXPORT FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, USHORT uiAttr, BYTE * pszName )
{
   /* less attemps */
   int iAttemptLeft = 99, iLen;
   FHANDLE fd;

   HB_SYMBOL_UNUSED( uiAttr );

   do
   {
      pszName[0] = '\0';

      if( pszDir != NULL && pszDir[0] != '\0' )
      {
         strcpy( ( char * ) pszName, ( char * ) pszDir );
      }
      else if( !fsGetTempDirByCase( pszName, getenv( "TMPDIR" ) ) &&
               !fsGetTempDirByCase( pszName, P_tmpdir ) )
      {
         strcpy( ( char * ) pszName, "." );
      }
      if( pszName[0] != '\0' )
      {
         int len;
         len = strlen( ( char * ) pszName );
         pszName[ len ] = hb_set.HB_SET_DIRSEPARATOR;
         pszName[ len + 1 ] = '\0';
      }

      if ( pszPrefix != NULL )
      {
         strcat( ( char * ) pszName, ( char * ) pszPrefix );
      }

      iLen = strlen( ( char * ) pszName );
      if ( iLen > _POSIX_PATH_MAX - 6 )
         return FS_ERROR;

#if !defined(__WATCOMC__) && ( defined( HB_OS_LINUX ) || defined( HB_OS_BSD ) )
      if( hb_set.HB_SET_FILECASE == HB_SET_CASE_LOWER ||
          hb_set.HB_SET_FILECASE == HB_SET_CASE_UPPER ||
          hb_set.HB_SET_DIRCASE == HB_SET_CASE_LOWER ||
          hb_set.HB_SET_DIRCASE == HB_SET_CASE_UPPER )
#endif
      {
         int i, n;
         double d = hb_random_num(), x;

         for ( i = 0; i < 6; i++ )
         {
            d = d * 36;
            n = ( int ) d;
            d = modf( d, &x );
            pszName[ iLen++ ] = n + ( n > 9 ? 'a' - 10 : '0' );
         }
         hb_fileNameConv( ( char * ) pszName );
         fd = hb_fsCreateEx( pszName, uiAttr, FO_EXCLUSIVE | FO_EXCL );
      }
#if !defined(__WATCOMC__) && ( defined( HB_OS_LINUX ) || defined( HB_OS_BSD ) )
      else
      {
         strcat( ( char * ) pszName, "XXXXXX" );
         fd = (FHANDLE) mkstemp( ( char * ) pszName );
         hb_fsSetIOError( fd != (FHANDLE) -1, 0 );
      }
#endif
      if ( fd != (FHANDLE) -1 )
      {
         return fd;
      }
   }
   while( --iAttemptLeft );

   return FS_ERROR;
}

#endif

#ifdef HB_EXTENSION

HB_FUNC( HB_FTEMPCREATE )
{
   BYTE szName[ _POSIX_PATH_MAX + 1 ];

   hb_retni( hb_fsCreateTemp( ( BYTE * ) hb_parc( 1 ),
                              ( BYTE * ) hb_parc( 2 ),
                              ( USHORT ) ( ISNUM( 3 ) ? ( USHORT ) hb_parni( 3 ) : FC_NORMAL ),
                              szName ) );

   hb_storc( ( char *) szName, 4 );
}

#endif

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HB_FTEMPCREATE() function
 *
 * Copyright 2000-2001 Jose Lalin <dezac@corevia.com>
 *                     Viktor Szakats (harbour.01 syenar.hu)
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

#define HB_OS_WIN_USED

#include "hbapi.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbmath.h"
#include "hbset.h"

#if defined( HB_OS_UNIX )
#include <stdlib.h>
#include <unistd.h>  /* We need for mkstemp() on BSD */
#endif

#if !defined(HB_OS_WIN)
static BOOL fsGetTempDirByCase( BYTE * pszName, const char * pszTempDir )
{
   BOOL fOK = FALSE;

   if( pszTempDir && *pszTempDir != '\0' )
   {
      hb_strncpy( ( char * ) pszName, ( char * ) pszTempDir, HB_PATH_MAX - 1 );
      switch( hb_setGetDirCase() )
      {
         case HB_SET_CASE_LOWER:
            hb_strLower( ( char * ) pszName, strlen( ( char * ) pszName ) );
            fOK = strcmp( ( char * ) pszName, pszTempDir ) == 0;
            break;
         case HB_SET_CASE_UPPER:
            hb_strUpper( ( char * ) pszName, strlen( ( char * ) pszName ) );
            fOK = strcmp( ( char * ) pszName, pszTempDir ) == 0;
            break;
         default:
            fOK = TRUE;
            break;
      }
   }

   return fOK;
}
#endif

static HB_FHANDLE hb_fsCreateTempLow( const BYTE * pszDir, const BYTE * pszPrefix, ULONG ulAttr, BYTE * pszName, const UCHAR * pszExt )
{
   /* less attemps */
   int iAttemptLeft = 99, iLen;
   HB_FHANDLE fd;

   do
   {
      pszName[ 0 ] = '\0';

      if( pszDir && pszDir[ 0 ] != '\0' )
      {
         hb_strncpy( ( char * ) pszName, ( char * ) pszDir, HB_PATH_MAX - 1 );
      }
      else
      {
#if defined(HB_OS_WIN)
         if( ! GetTempPathA( ( DWORD ) ( HB_PATH_MAX - 1 ), ( LPSTR ) pszName ) )
         {
            pszName[ 0 ] = '.';
            pszName[ 1 ] = '\0';
         }
#else
         char * pszTmpDir = hb_getenv( "TMPDIR" );

         if( !fsGetTempDirByCase( pszName, pszTmpDir ) )
         {
#ifdef P_tmpdir
            if( !fsGetTempDirByCase( pszName, P_tmpdir ) )
#endif
            {
               pszName[ 0 ] = '.';
               pszName[ 1 ] = '\0';
            }
         }
         if( pszTmpDir )
            hb_xfree( pszTmpDir );
#endif
      }

      if( pszName[0] != '\0' )
      {
         int len = strlen( ( char * ) pszName );
         if( pszName[ len - 1 ] != ( BYTE ) HB_OS_PATH_DELIM_CHR )
         {
            pszName[ len ] = ( BYTE ) HB_OS_PATH_DELIM_CHR;
            pszName[ len + 1 ] = '\0';
         }
      }

      if( pszPrefix )
         hb_strncat( ( char * ) pszName, ( char * ) pszPrefix, HB_PATH_MAX - 1 );

      iLen = ( int ) strlen( ( char * ) pszName );
      if( iLen > ( HB_PATH_MAX - 1 ) - 6 )
         return FS_ERROR;

#if !defined(__WATCOMC__) && ( defined( HB_OS_LINUX ) || defined( HB_OS_BSD ) )
      if( hb_setGetFileCase() != HB_SET_CASE_LOWER &&
          hb_setGetFileCase() != HB_SET_CASE_UPPER &&
          hb_setGetDirCase() != HB_SET_CASE_LOWER &&
          hb_setGetDirCase() != HB_SET_CASE_UPPER &&
          pszExt == NULL )
      {
         hb_strncat( ( char * ) pszName, "XXXXXX", HB_PATH_MAX - 1 );
         hb_vmUnlock();
         fd = ( HB_FHANDLE ) mkstemp( ( char * ) pszName );
         hb_fsSetIOError( fd != ( HB_FHANDLE ) -1, 0 );
         hb_vmLock();
      }
      else
#endif
      {
         int i, n;
         double d = hb_random_num(), x;

         for( i = 0; i < 6; i++ )
         {
            d = d * 36;
            n = ( int ) d;
            d = modf( d, &x );
            pszName[ iLen++ ] = ( BYTE ) ( n + ( n > 9 ? 'a' - 10 : '0' ) );
         }
         pszName[ iLen ] = '\0';
         if( pszExt )
            hb_strncat( ( char * ) pszName, ( char * ) pszExt, HB_PATH_MAX - 1 );
         hb_fsNameConv( pszName, NULL );
         fd = hb_fsCreateEx( pszName, ulAttr, FO_EXCLUSIVE | FO_EXCL );
      }

      if( fd != ( HB_FHANDLE ) FS_ERROR )
         return fd;
   }
   while( --iAttemptLeft );

   return FS_ERROR;
}

/* NOTE: The buffer must be at least HB_PATH_MAX chars long */
#if !defined( HB_OS_UNIX )

static BOOL hb_fsTempName( BYTE * pszBuffer, const BYTE * pszDir, const BYTE * pszPrefix )
{
   BOOL fResult;

   hb_vmUnlock();

#if defined(HB_IO_WIN)
   {
      char cTempDir[ HB_PATH_MAX ];

      if( pszDir && pszDir[ 0 ] != '\0' )
         hb_strncpy( ( char * ) cTempDir, ( const char * ) pszDir, sizeof( cTempDir ) - 1 );
      else
      {
         if( ! GetTempPathA( ( DWORD ) HB_PATH_MAX, cTempDir ) )
         {
            hb_fsSetIOError( FALSE, 0 );
            return FALSE;
         }
      }
      cTempDir[ HB_PATH_MAX - 1 ] = '\0';

      fResult = GetTempFileNameA( ( LPCSTR ) cTempDir, pszPrefix ? ( LPCSTR ) pszPrefix : ( LPCSTR ) "hb", 0, ( LPSTR ) pszBuffer );
   }
#else

   /* TODO: Implement these: */
   HB_SYMBOL_UNUSED( pszDir );
   HB_SYMBOL_UNUSED( pszPrefix );

   /* TOFIX: The spec says to reserve L_tmpnam number of characters for the
             passed buffer. It will be needed to fix HB_PATH_MAX - 1 to be
             at least this large. */

   pszBuffer[ 0 ] = '\0';
   fResult = ( tmpnam( ( char * ) pszBuffer ) != NULL );

#endif

   hb_fsSetIOError( fResult, 0 );
   hb_vmLock();

   return fResult;
}

/* NOTE: The pszName buffer must be at least HB_PATH_MAX chars long */

HB_FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, ULONG ulAttr, BYTE * pszName )
{
   USHORT nAttemptLeft = 999;

   while( --nAttemptLeft )
   {
      if( hb_fsTempName( pszName, pszDir, pszPrefix ) )
      {
          HB_FHANDLE fhnd = hb_fsCreateEx( pszName, ulAttr, FO_EXCLUSIVE | FO_EXCL );

          /* This function may fail, if the generated filename got
             used between generation and the file creation. */

          if( fhnd != FS_ERROR )
             return fhnd;
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

HB_FHANDLE hb_fsCreateTemp( const BYTE * pszDir, const BYTE * pszPrefix, ULONG ulAttr, BYTE * pszName )
{
   return hb_fsCreateTempLow( pszDir, pszPrefix, ulAttr, pszName, NULL );
}

#endif

HB_FUNC( HB_FTEMPCREATE )
{
   BYTE szName[ HB_PATH_MAX ];

   hb_retnint( ( HB_NHANDLE ) hb_fsCreateTemp( ( BYTE * ) hb_parc( 1 ),
                                               ( BYTE * ) hb_parc( 2 ),
                                               ( ULONG ) ( HB_ISNUM( 3 ) ? ( ULONG ) hb_parnl( 3 ) : FC_NORMAL ),
                                               szName ) );

   hb_storc( ( char * ) szName, 4 );
}

HB_FHANDLE hb_fsCreateTempEx( BYTE * pszName, const BYTE * pszDir, const BYTE * pszPrefix, const BYTE * pszExt, ULONG ulAttr )
{
   return hb_fsCreateTempLow( pszDir, pszPrefix, ulAttr, pszName, pszExt );
}

HB_FUNC( HB_FTEMPCREATEEX )
{
   BYTE szName[ HB_PATH_MAX ];

   hb_retnint( ( HB_NHANDLE ) hb_fsCreateTempEx( szName,
                                                 ( BYTE * ) hb_parc( 2 ),
                                                 ( BYTE * ) hb_parc( 3 ),
                                                 ( BYTE * ) hb_parc( 4 ),
                                                 ( ULONG ) ( HB_ISNUM( 5 ) ? ( ULONG ) hb_parnl( 5 ) : FC_NORMAL ) ) );

   hb_storc( ( char * ) szName, 1 );
}

/*
 * Harbour Project source code:
 * Windows Mutex functions
 *
 * Copyright 2009 Xavi <jarabal/at/gmail.com>
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

#include "hbwapi.h"

static HB_GARBAGE_FUNC( hbwapi_mutex_release )
{
   void ** ph = ( void ** ) Cargo;

   if( ph && *ph )
   {
      CloseHandle( ( HANDLE ) *ph );
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_hbwapi_mutex_funcs =
{
   hbwapi_mutex_release,
   hb_gcDummyMark
};

static void hbwapi_mutex_ret( HANDLE hMutex )
{
   if( hMutex )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HANDLE * ), &s_gc_hbwapi_mutex_funcs );

      *ph = hMutex;
      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

static HANDLE hbwapi_mutex_par( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_hbwapi_mutex_funcs, iParam );

   return ph ? ( HANDLE ) *ph : NULL;
}

/* HANDLE WINAPI CreateMutex( LPSECURITY_ATTRIBUTES lpMutexAttributes, BOOL bInitialOwner, LPCTSTR lpName ) */
HB_FUNC( WAPI_CREATEMUTEX )
{
   void * hName;
   HANDLE hMutex = CreateMutex( ( LPSECURITY_ATTRIBUTES ) hb_parptr( 1 ), hb_parl( 2 ), HB_PARSTR( 3, &hName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_mutex_ret( hMutex );

   hb_strfree( hName );
}

/* HANDLE WINAPI OpenMutex( DWORD dwDesiredAccess, BOOL bInheritHandle, LPCTSTR lpName ) */
HB_FUNC( WAPI_OPENMUTEX )
{
#if ! defined( HB_OS_WIN_CE )
   void * hName;
   HANDLE hMutex = OpenMutex( hb_parnl( 1 ), hb_parl( 2 ), HB_PARSTR( 3, &hName, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_mutex_ret( hMutex );

   hb_strfree( hName );
#else
   /* WinCE (WinMobile6) does not support OpenMutex() */
   hb_retptr( NULL );
#endif
}

/* BOOL WINAPI ReleaseMutex( HANDLE hMutex ) */
HB_FUNC( WAPI_RELEASEMUTEX )
{
   HANDLE hMutex = hbwapi_mutex_par( 1 );

   if( hMutex )
   {
      BOOL bResult = ReleaseMutex( hMutex );
      hbwapi_SetLastError( GetLastError() );
      hbwapi_ret_L( bResult );
   }
   else
      hb_retl( HB_FALSE );
}

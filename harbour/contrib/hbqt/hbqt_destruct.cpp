/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/

#if defined( __HB_DEBUG__ )
   #define HB_OS_WIN_USED
#endif

#include "hbapi.h"

#include "hbqt.h"

#if QT_VERSION >= 0x040500

#include <QtCore/QObject>

static int s_iObjectReleaseMethod = HBQT_RELEASE_WITH_DELETE_LATER;

/*----------------------------------------------------------------------*/

HB_GARBAGE_FUNC( Q_release )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;
   if( p && p->ph )
   {
      p->func( p );
   }
}

const HB_GC_FUNCS QT_gcFuncs =
{
   Q_release,
   hb_gcDummyMark
};

const HB_GC_FUNCS * gcFuncs( void )
{
   return &QT_gcFuncs;
}

void * hbqt_gcpointer( int iParam )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( gcFuncs(), iParam );

   if( p && p->ph )
   {
      return p->ph;
   }
   else
   {
      /* TOFIX: This is dangerous. */
      return hb_parptr( iParam );
   }
}

int hbqt_get_object_release_method()
{
   return s_iObjectReleaseMethod;
}

HB_FUNC( HBQT_SET_RELEASE_METHOD )
{
   hb_retni( s_iObjectReleaseMethod );

   if( HB_ISNUM( 1 ) && hb_parni( 1 ) >= 0 && hb_parni( 1 ) <= HBQT_RELEASE_WITH_DELETE_LATER )
      s_iObjectReleaseMethod = hb_parni( 1 );
}

HB_FUNC( ISEQUALGCQTPOINTER )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_parptrGC( gcFuncs(), 1 );

   if( p && p->ph )
   {
      hb_retl( p->ph == hb_parptr( 2 ) );
   }
   else
   {
      hb_retl( false );
   }
}

HB_FUNC( QT_FINDCHILD )
{
   hb_retptr( ( QObject * ) hbqt_par_QObject( 1 )->findChild< QObject * >( hbqt_par_QString( 2 ) ) );
}

#if defined( __HB_DEBUG__ )

#if defined( HB_OS_WIN )
   #include <psapi.h>
#endif

void hbqt_debug( const char * sTraceMsg, ... )
{
#if defined( HB_OS_WIN )
   if( sTraceMsg )
   {
      char buffer[ 1024 ];
      va_list ap;

      va_start( ap, sTraceMsg );
      hb_vsnprintf( buffer, sizeof( buffer ), sTraceMsg, ap );
      va_end( ap );

      #if defined( UNICODE )
      {
         LPTSTR lpOutputString = HB_TCHAR_CONVTO( buffer );
         OutputDebugString( lpOutputString );
         HB_TCHAR_FREE( lpOutputString );
      }
      #else
         OutputDebugString( buffer );
      #endif
   }
#endif
}

#endif

#if defined( __HB_DEBUG__ )

int hbqt_getmemused( void )
{
#if defined( HB_OS_WIN )
#if (_WIN32_WINNT >= 0x0501)
#ifdef __GNUC__
// MingW32 doesn't have this struct in psapi.h
typedef struct _PROCESS_MEMORY_COUNTERS_EX
{
   DWORD  cb;
   DWORD  PageFaultCount;
   SIZE_T PeakWorkingSetSize;
   SIZE_T WorkingSetSize;
   SIZE_T QuotaPeakPagedPoolUsage;
   SIZE_T QuotaPagedPoolUsage;
   SIZE_T QuotaPeakNonPagedPoolUsage;
   SIZE_T QuotaNonPagedPoolUsage;
   SIZE_T PagefileUsage;
   SIZE_T PeakPagefileUsage;
   SIZE_T PrivateUsage;
}PROCESS_MEMORY_COUNTERS_EX, *PPROCESS_MEMORY_COUNTERS_EX;
#endif
#endif
#endif
   int size = 0;
#if defined( HB_OS_WIN )
   HANDLE hProcess;
#if (_WIN32_WINNT >= 0x0501)
   PROCESS_MEMORY_COUNTERS_EX pmc;
#else
   PROCESS_MEMORY_COUNTERS pmc;
#endif
   hProcess = OpenProcess( PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, GetCurrentProcessId() );
   if( hProcess == NULL )
      return 0;

   pmc.cb = sizeof(pmc);
   if( GetProcessMemoryInfo( hProcess, (PROCESS_MEMORY_COUNTERS*)&pmc, sizeof( pmc ) ) )
#if (_WIN32_WINNT >= 0x0501)
      size = ( int ) pmc.PrivateUsage / 1024;
#else
      size = ( int ) pmc.WorkingSetSize / 1024;
#endif
   CloseHandle( hProcess );
#endif
   return size;
}

#endif

HB_FUNC( HBQT_GETMEMUSED )
{
#if defined( __HB_DEBUG__ )
   hb_retni( hbqt_getmemused() );
#else
   hb_retni( 0 );
#endif
}

/*----------------------------------------------------------------------*/

#endif                  // #if QT_VERSION >= 0x040500

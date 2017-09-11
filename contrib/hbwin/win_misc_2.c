/*
 * Misc Windows API functions
 *
 * Copyright 2008-2009 Viktor Szakats (vszakats.net/harbour)
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

HB_FUNC( WIN_RUNDETACHED )
{
   void * hCommandName;
   void * hCommandLine;

   HB_SIZE nLen;
   LPCTSTR lpCommandRO = HB_PARSTR( 2, &hCommandLine, &nLen );

#if ! defined( HB_OS_WIN_CE )
   STARTUPINFO si;
   PROCESS_INFORMATION pi;

   memset( &si, 0, sizeof( si ) );
   si.cb = sizeof( si );
   memset( &pi, 0, sizeof( pi ) );
#endif

   if( CreateProcess(
          HB_PARSTR( 1, &hCommandName, NULL ),                  /* Command name */
          HB_STRUNSHARE( &hCommandLine, lpCommandRO, nLen ),    /* Command-line (Unicode version needs an non-const buffer) */
          NULL,                                                 /* Process handle not inheritable */
          NULL,                                                 /* Thread handle not inheritable */
          FALSE,                                                /* Set handle inheritance to FALSE */
#if ! defined( HB_OS_WIN_CE )
          hb_parl( 4 ) ? CREATE_NO_WINDOW : CREATE_NEW_CONSOLE, /* Creation flags */
#else
          CREATE_NEW_CONSOLE,                                   /* Creation flags */
#endif
          NULL,                                                 /* Use parent's environment block */
          NULL,                                                 /* Use parent's starting directory */
#if ! defined( HB_OS_WIN_CE )
          &si,                                                  /* Pointer to STARTUPINFO structure */
          &pi )                                                 /* Pointer to PROCESS_INFORMATION structure */
#else
          NULL,                                                 /* Pointer to STARTUPINFO structure */
          NULL )                                                /* Pointer to PROCESS_INFORMATION structure */
#endif
       )
   {
      hb_retl( HB_TRUE );

#if ! defined( HB_OS_WIN_CE )
      hb_stornl( pi.dwProcessId, 3 );

      /* Close process and thread handles. */
      CloseHandle( pi.hProcess );
      CloseHandle( pi.hThread );
#endif
   }
   else
   {
      hb_stornl( -1, 3 );
      hb_retl( HB_FALSE );
   }

   hb_strfree( hCommandName );
   hb_strfree( hCommandLine );
}

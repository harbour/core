/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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

#include "gtwvg.h"

#include <shellapi.h>

// CreateProcess( cExe, cCmdLineArgs, nFlags, cEnvPair, cInitDirectory, @aProcessInfo )
//
HB_FUNC( CREATEPROCESS )
{
  LPCTSTR               lpApplicationName;    // name of executable module
  LPTSTR                lpCommandLine;        // command line string
  LPSECURITY_ATTRIBUTES lpProcessAttributes;  // SD
  LPSECURITY_ATTRIBUTES lpThreadAttributes;   // SD
  BOOL                  bInheritHandles;      // handle inheritance option
  DWORD                 dwCreationFlags;      // creation flags
  LPVOID                lpEnvironment;        // new environment block
  LPCTSTR               lpCurrentDirectory;   // current directory name
  LPSTARTUPINFO         lpStartupInfo;        // startup information
  LPPROCESS_INFORMATION lpProcessInformation; // process information

  lpApplicationName   = NULL;//hb_parc( 1 );
  lpCommandLine       = hb_parc( 1 );
  lpProcessAttributes = NULL;
  lpThreadAttributes  = NULL;
  bInheritHandles     = TRUE;
  dwCreationFlags     = 0;    //ISNUM( 3 ) ? hb_parnl( 3 ) : CREATE_NEW_CONSOLE;
  lpEnvironment       = NULL; //ISCHAR( 4 ) ? hb_parc( 4 ) : NULL;
  lpCurrentDirectory  = NULL; //ISCHAR( 5 ) ? hb_parc( 5 ) : NULL;
  lpStartupInfo       = NULL;

  hb_retl(
     CreateProcess(
        lpApplicationName,
        lpCommandLine,
        lpProcessAttributes,
        lpThreadAttributes,
        bInheritHandles,
        dwCreationFlags,
        lpEnvironment,
        lpCurrentDirectory,
        lpStartupInfo,
        NULL ) );
}

HB_FUNC( SHELLEXECUTE )
{
   ShellExecute( NULL,
                "OPEN",
                 hb_parc( 1 ),
                 hb_parc( 2 ),
                 hb_parc( 3 ),
                 SW_SHOWNORMAL );
}

HB_FUNC( OUTPUTDEBUGSTRING )
{
   LPTSTR text = HB_TCHAR_CONVTO( hb_parc( 1 ) );
   OutputDebugString( text );
   HB_TCHAR_FREE( text );
}

HB_FUNC( GETSCREENATTRIB )
{
   ULONG uiSize;
   void * pBuffer;
   void * qBuffer;

   hb_gtRectSize( hb_parnl( 1 ),hb_parnl( 2 ),hb_parnl( 3 ),hb_parnl( 4 ),&uiSize );
   pBuffer = hb_xgrab( (uiSize/2)+1 );
   qBuffer = hb_xgrab( (uiSize/2)+1 );

   hb_wvt_GetStringAttrib( hb_parnl( 1 ),hb_parnl( 2 ),hb_parnl( 3 ),hb_parnl( 4 ), pBuffer, qBuffer );
   hb_storclen( pBuffer, (uiSize/2), 5 );
   hb_storclen( qBuffer, (uiSize/2), 6 );

   hb_xfree( pBuffer );
   hb_xfree( qBuffer );
}

HB_FUNC( PUTSCREENATTRIB )
{
   hb_wvt_PutStringAttrib( hb_parnl( 1 ), hb_parnl( 2 ),
                           hb_parnl( 3 ), hb_parnl( 4 ),
                           ( BYTE* ) hb_parc( 5 ),
                           ( BYTE* ) hb_parc( 6 ) );
}

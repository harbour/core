/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Windows API functions (rpc.h)
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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
#include "hbwinuni.h"

#include "hbwin.ch"

#if ! defined( HB_OS_WIN_CE )
#  include <rpc.h>
#endif

HB_FUNC( WIN_UUIDCREATESTRING )
{
   RPC_STATUS lRPCStatus = HB_RPC_S_ERROR;

#if ! defined( HB_OS_WIN_CE )
   typedef RPC_STATUS ( RPC_ENTRY * _HB_UUIDCREATE )( UUID * );
   typedef RPC_STATUS ( RPC_ENTRY * _HB_UUIDTOSTRING )( UUID *, unsigned char ** );
   typedef RPC_STATUS ( RPC_ENTRY * _HB_RPCSTRINGFREE )( unsigned char ** );

   static _HB_UUIDCREATE    s_pUuidCreate    = NULL;
   static _HB_UUIDTOSTRING  s_pUuidToString  = NULL;
   static _HB_RPCSTRINGFREE s_pRpcStringFree = NULL;

   if( ! s_pUuidCreate )
   {
      HMODULE hRpcrt4 = GetModuleHandle( TEXT( "rpcrt4.dll" ) );

      s_pUuidCreate = ( _HB_UUIDCREATE ) GetProcAddress( hRpcrt4, "UuidCreate" );

      s_pUuidToString = ( _HB_UUIDTOSTRING ) GetProcAddress( hRpcrt4,
#if defined( UNICODE )
                                                             "UuidToStringW" );
#else
                                                             "UuidToStringA" );
#endif

      s_pRpcStringFree = ( _HB_RPCSTRINGFREE ) GetProcAddress( hRpcrt4,
#if defined( UNICODE )
                                                               "RpcStringFreeW" );
#else
                                                               "RpcStringFreeA" );
#endif
   }

   if( s_pUuidCreate &&
       s_pUuidToString &&
       s_pRpcStringFree )
   {
      TCHAR * tszUuid = NULL;
      UUID    uuid;

      memset( &uuid, 0, sizeof( UUID ) );

      lRPCStatus = s_pUuidCreate( &uuid );

      s_pUuidToString( &uuid, ( unsigned char ** ) ( void * ) &tszUuid );

      if( tszUuid != NULL )
      {
         HB_RETSTR( tszUuid );

         s_pRpcStringFree( ( unsigned char ** ) ( void * ) &tszUuid );
      }
      else
         hb_retc_null();
   }
   else
      hb_retc_null();
#else
   hb_retc_null();
#endif

   hb_stornl( lRPCStatus, 1 );
}

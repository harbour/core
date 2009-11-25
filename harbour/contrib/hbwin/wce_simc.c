/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SIM interface code (low level)
 *
 * Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
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
#include "hbapiitm.h"

#if defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ )

#include <simmgr.h>

HB_FUNC( WCE_SIMINITIALIZE ) /* hSim by reference, lNotifications */
{
   HSIM hSim = 0;
   HRESULT hResult = SimInitialize( hb_parl( 2 ) ? SIM_INIT_SIMCARD_NOTIFICATIONS : 0, NULL, 0, &hSim );

   hb_storptr( hResult == S_OK ? hSim : 0, 1 );
   hb_retnl( hResult );
}

HB_FUNC( WCE_SIMDEINITIALIZE ) /* hSim */
{
   hb_retnl( SimDeinitialize( ( HSIM ) hb_parptr( 1 ) ) );
}

HB_FUNC( WCE_SIMPHONEBOOKSTATUS ) /* hSim, nLocation, @nTotal, @nUsed */
{
   DWORD dwUsed = 0, dwTotal = 0;
   HRESULT hResult = SimGetPhonebookStatus( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) /* dwLocation */, &dwUsed, &dwTotal );

   hb_stornl( hResult == S_OK ? ( long ) dwTotal : 0, 3 );
   hb_stornl( hResult == S_OK ? ( long ) dwUsed : 0, 4 );

   hb_retnl( hResult );
}

HB_FUNC( WCE_SIMREADPHONEBOOKENTRY ) /* hSim, nLocation, nPos, @aEntry */
{
   HSIM hSim = ( HSIM ) hb_parptr( 1 );
   DWORD dwIndex = ( DWORD ) hb_parnl( 3 );
   SIMPHONEBOOKENTRY PhoneEntry;
   PHB_ITEM pArray;
   char * szAddress;
   char * szText;

   PhoneEntry.cbSize = sizeof( SIMPHONEBOOKENTRY );
   hb_retnl( SimReadPhonebookEntry( hSim, ( DWORD ) hb_parnl( 2 ) /* dwLocation */, dwIndex, &PhoneEntry ) );

   szAddress = HB_TCHAR_CONVFROM( PhoneEntry.lpszAddress );
   szText = HB_TCHAR_CONVFROM( PhoneEntry.lpszText );

   pArray = hb_itemArrayNew( 5 );

   hb_arraySetC( pArray, 1, szAddress );
   hb_arraySetC( pArray, 2, szText );
   hb_arraySetNL( pArray, 3, PhoneEntry.dwAddressType );
   hb_arraySetNL( pArray, 4, PhoneEntry.dwNumPlan );
   hb_arraySetNI( pArray, 5, dwIndex );

   hb_itemCopy( hb_param( 4, HB_IT_ANY ), pArray );
   hb_itemRelease( pArray );

   HB_TCHAR_FREE( szAddress );
   HB_TCHAR_FREE( szText );
}

HB_FUNC( WCE_SIMWRITEPHONEBOOKENTRY ) /* hSim, nLocation, nPos, cNumber, cName, nPlan, nAddrType */
{
   SIMPHONEBOOKENTRY PhoneEntry;
   wchar_t * lpwszAddress = HB_TCHAR_CONVTO( hb_parcx( 4 ) );
   wchar_t * lpwszText = HB_TCHAR_CONVTO( hb_parcx( 5 ) );

   PhoneEntry.cbSize        = sizeof( SIMPHONEBOOKENTRY );
   PhoneEntry.dwParams      = SIM_PARAM_PBE_ALL;
   wcsncpy( PhoneEntry.lpszAddress, lpwszAddress, MAX_LENGTH_ADDRESS );
   wcsncpy( PhoneEntry.lpszText   , lpwszText   , MAX_LENGTH_PHONEBOOKENTRYTEXT );
   PhoneEntry.dwAddressType = ( DWORD ) hb_parnl( 7 );
   PhoneEntry.dwNumPlan     = ( DWORD ) hb_parnl( 6 );

   hb_retnl( SimWritePhonebookEntry( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ), ( DWORD ) hb_parnl( 3 ), &PhoneEntry ) );

   HB_TCHAR_FREE( lpwszAddress );
   HB_TCHAR_FREE( lpwszText );
}

HB_FUNC( WCE_SIMDELETEPHONEBOOKENTRY ) /* hSim, nLocation, nPos */
{
   hb_retnl( SimDeletePhonebookEntry( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
}

#endif

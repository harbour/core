/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SIM interface code (low level)
 *
 * Copyright 2009 Jose Luis Capel <jlcapel@hotmail.com>
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

#include "hbwin.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ )
#  include <simmgr.h>
#  define __HB_COMPONENT_SUPPORTED__
#endif

HB_FUNC( WCE_SIMINITIALIZE ) /* hSim by reference, lNotifications */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   HSIM hSim = 0;
   HRESULT hResult = SimInitialize( hb_parl( 2 ) ? SIM_INIT_SIMCARD_NOTIFICATIONS : 0, NULL, 0, &hSim );

   hb_storptr( hResult == S_OK ? hSim : 0, 1 );
   hb_retnl( hResult );
#else
   hb_storptr( 0, 1 );
   hb_retnl( -1 );
#endif
}

HB_FUNC( WCE_SIMDEINITIALIZE ) /* hSim */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   hb_retnl( SimDeinitialize( ( HSIM ) hb_parptr( 1 ) ) );
#else
   hb_retnl( -1 );
#endif
}

HB_FUNC( WCE_SIMPHONEBOOKSTATUS ) /* hSim, nLocation, @nTotal, @nUsed */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   DWORD dwUsed = 0, dwTotal = 0;
   HRESULT hResult = SimGetPhonebookStatus( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ) /* dwLocation */, &dwUsed, &dwTotal );

   hb_stornl( hResult == S_OK ? ( long ) dwTotal : 0, 3 );
   hb_stornl( hResult == S_OK ? ( long ) dwUsed : 0, 4 );

   hb_retnl( hResult );
#else
   hb_stornl( 0, 3 );
   hb_stornl( 0, 4 );
   hb_retnl( -1 );
#endif
}

HB_FUNC( WCE_SIMREADPHONEBOOKENTRY ) /* hSim, nLocation, nPos, @aEntry */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   HSIM hSim = ( HSIM ) hb_parptr( 1 );
   DWORD dwIndex = ( DWORD ) hb_parnl( 3 );
   SIMPHONEBOOKENTRY PhoneEntry;
   PHB_ITEM pArray;

   PhoneEntry.cbSize = sizeof( SIMPHONEBOOKENTRY );
   hb_retnl( SimReadPhonebookEntry( hSim, ( DWORD ) hb_parnl( 2 ) /* dwLocation */, dwIndex, &PhoneEntry ) );

   pArray = hb_itemArrayNew( 5 );

   HB_ARRAYSETSTR( pArray, 1, PhoneEntry.lpszAddress );
   HB_ARRAYSETSTR( pArray, 2, PhoneEntry.lpszText );
   hb_arraySetNL( pArray, 3, PhoneEntry.dwAddressType );
   hb_arraySetNL( pArray, 4, PhoneEntry.dwNumPlan );
   hb_arraySetNI( pArray, 5, dwIndex );

   hb_itemParamStoreForward( 4, pArray );
   hb_itemRelease( pArray );
#else
   hb_reta( 0 );
#endif
}

HB_FUNC( WCE_SIMWRITEPHONEBOOKENTRY ) /* hSim, nLocation, nPos, cNumber, cName, nPlan, nAddrType */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   SIMPHONEBOOKENTRY PhoneEntry;

   void * hAddress;
   void * hText;

   PhoneEntry.cbSize   = sizeof( SIMPHONEBOOKENTRY );
   PhoneEntry.dwParams = SIM_PARAM_PBE_ALL;
   wcsncpy( PhoneEntry.lpszAddress, HB_PARSTRDEF( 4, &hAddress, NULL ), MAX_LENGTH_ADDRESS );
   wcsncpy( PhoneEntry.lpszText   , HB_PARSTRDEF( 5, &hText   , NULL ), MAX_LENGTH_PHONEBOOKENTRYTEXT );
   PhoneEntry.dwAddressType = ( DWORD ) hb_parnl( 7 );
   PhoneEntry.dwNumPlan     = ( DWORD ) hb_parnl( 6 );

   hb_retnl( SimWritePhonebookEntry( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ), ( DWORD ) hb_parnl( 3 ), &PhoneEntry ) );

   hb_strfree( hAddress );
   hb_strfree( hText );
#else
   hb_retnl( -1 );
#endif
}

HB_FUNC( WCE_SIMDELETEPHONEBOOKENTRY ) /* hSim, nLocation, nPos */
{
#ifdef __HB_COMPONENT_SUPPORTED__
   hb_retnl( SimDeletePhonebookEntry( ( HSIM ) hb_parptr( 1 ), ( DWORD ) hb_parnl( 2 ), ( DWORD ) hb_parnl( 3 ) ) );
#else
   hb_retnl( -1 );
#endif
}

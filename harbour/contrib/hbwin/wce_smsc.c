/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SMS interface code
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
#include "hbwinuni.h"

#if defined( HB_OS_WIN_CE ) && ! defined( __MINGW32__ )

#include <sms.h>

HB_FUNC( WCE_SMSSENDMESSAGE ) /* cMessage, cNumber */
{
   SMS_HANDLE smshHandle = 0;
   HRESULT hr = SmsOpen( SMS_MSGTYPE_TEXT, SMS_MODE_SEND, &smshHandle, NULL ); /* try to open an SMS Handle */

   if( hr == ERROR_SUCCESS && hb_parclen( 2 ) <= SMS_MAX_ADDRESS_LENGTH )
   {
      SMS_ADDRESS smsaDestination;
      TEXT_PROVIDER_SPECIFIC_DATA tpsd;
      SMS_MESSAGE_ID smsmidMessageID = 0;

      void * hMessage;
      void * hPhoneNumber;

      LPCTSTR sztMessage     = HB_PARSTRDEF( 1, &hMessage    , NULL );
      LPCTSTR sztPhoneNumber = HB_PARSTRDEF( 2, &hPhoneNumber, NULL );

      /* Create the destination address */
      memset( &smsaDestination, 0, sizeof( smsaDestination ) );
      smsaDestination.smsatAddressType = ( *sztPhoneNumber == _T( '+' ) ) ? SMSAT_INTERNATIONAL : SMSAT_NATIONAL;
      /* TOFIX: lstrcpy() unsafe and may cause buffer overrun.
                Worked around using hb_parclen( 2 ) check against SMS_MAX_ADDRESS_LENGTH.
                [vszakats]. */
      lstrcpy( smsaDestination.ptsAddress, sztPhoneNumber );

      /* Set up provider specific data */
      tpsd.dwMessageOptions = PS_MESSAGE_OPTION_NONE;
      tpsd.psMessageClass = PS_MESSAGE_CLASS0;
      tpsd.psReplaceOption = PSRO_NONE;

      /* Send the message, indicating success or failure */
      hb_retnl( SmsSendMessage( smshHandle,
                                NULL,
                                &smsaDestination,
                                NULL,
                                ( PBYTE ) sztMessage,
                                _tcslen( sztMessage ) * sizeof( wchar_t ),
                                ( PBYTE ) &tpsd, 12,
                                SMSDE_OPTIMAL,
                                SMS_OPTION_DELIVERY_NONE,
                                &smsmidMessageID ) );

      SmsClose( smshHandle );

      hb_strfree( hMessage );
      hb_strfree( hPhoneNumber );
   }
   else
      hb_retnl( -1 );
}

#endif

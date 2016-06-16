/*
 * The Date API (Harbour level)
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2004 Giancarlo Niccolai <gc -at- niccolai -dot- ws>
 * Copyright 2006 Pavel Tsarenko <tpe2@mail.ru> (DaysInMonth(), DaysToMonth())
 * Copyright 2007 Walter Negro <anegro@overnet.com.ar>
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

#include "hbapi.h"
#include "hbapierr.h"
#include "hbdate.h"

/* NOTE: szTime must be 9 chars large. */

static HB_ULONG hb_TimeStrToSec( const char * pszTime )
{
   HB_SIZE  nLen;
   HB_ULONG ulTime = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_TimeStrToSec(%s)", pszTime ) );

   nLen = strlen( pszTime );

   if( nLen >= 1 )
      ulTime += ( HB_ULONG ) hb_strVal( pszTime, nLen ) * 3600;

   if( nLen >= 4 )
      ulTime += ( HB_ULONG ) hb_strVal( pszTime + 3, nLen - 3 ) * 60;

   if( nLen >= 7 )
      ulTime += ( HB_ULONG ) hb_strVal( pszTime + 6, nLen - 6 );

   return ulTime;
}

HB_FUNC( TSSECS )
{
   hb_retnl( hb_TimeStrToSec( hb_parcx( 1 ) ) );
}

HB_FUNC( TIMEOFDAY )
{
   char szResult[ 9 ];

   if( hb_pcount() == 0 )
      hb_dateTimeStr( szResult );
   else
   {
      int iSeconds = hb_parni( 1 );
      iSeconds %= 3600 * 24;
      hb_snprintf( szResult, sizeof( szResult ), "%02d:%02d:%02d",
                   iSeconds / 3600, ( iSeconds % 3600 ) / 60, iSeconds % 60 );
   }

   hb_retclen( szResult, 8 );
}

HB_FUNC( HMS2D )
{
   int    iHour = hb_parni( 1 );
   int    iMin  = hb_parni( 2 );
   double dSec  = hb_parnd( 3 );

   hb_retnd( hb_timeEncode( iHour, iMin, ( int ) dSec, ( int ) ( ( ( double ) ( dSec - ( double ) ( ( int ) dSec ) ) ) * 1000 ) ) );
}

HB_FUNC( TTOD )
{
   if( HB_ISDATE( 1 ) )
      hb_retdl( hb_pardl( 1 ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static int s_daysinmonth( int iMonth, HB_BOOL bLeap )
{
   if( iMonth == 2 )
      return bLeap ? 29 : 28;
   else if( iMonth == 4 || iMonth == 6 || iMonth == 9 || iMonth == 11 )
      return 30;
   else
      return 31;
}

static int s_daystomonth( int iMonth, HB_BOOL bLeap )
{
   static const int sc_iMonths[] = {
      0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

   return ( iMonth < 1 || iMonth > 12 ) ? 0 : sc_iMonths[ iMonth - 1 ] +
          ( ( bLeap && iMonth > 2 ) ? 1 : 0 );
}

HB_FUNC( DAYSTOMONTH )
{
   hb_retni( s_daystomonth( hb_parni( 1 ), hb_parl( 2 ) ) );
}

HB_FUNC( DAYSINMONTH )
{
   hb_retni( s_daysinmonth( hb_parni( 1 ), hb_parl( 2 ) ) );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API (Harbour level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    DAY()
 *    MONTH()
 *    YEAR()
 *    DOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    CTOD()
 *    DATE()
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    HB_STOD()
 *
 * See COPYING for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

HB_FUNC( CTOD )
{
   if( HB_ISCHAR( 1 ) )
      hb_retdl( hb_dateUnformat( hb_parc( 1 ), hb_setGetDateFormat() ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CTOD )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * szFormat = hb_parc( 2 );

      if( ! szFormat )
         szFormat = hb_setGetDateFormat();
      hb_retdl( hb_dateUnformat( hb_parc( 1 ), szFormat ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DTOC )
{
   if( HB_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, hb_setGetDateFormat() ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_DTOC )
{
   if( HB_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];
      const char * szFormat = hb_parc( 2 );

      if( ! szFormat )
         szFormat = hb_setGetDateFormat();
      hb_retc( hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, szFormat ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DTOS )
{
   if( HB_ISDATETIME( 1 ) )
   {
      char szDate[ 9 ];

      hb_retc( hb_pardsbuff( szDate, 1 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* NOTE: Harbour extension, exactly the same as STOD(). */

HB_FUNC( HB_STOD )
{
   PHB_ITEM pDateString = hb_param( 1, HB_IT_STRING );

   hb_retds( hb_itemGetCLen( pDateString ) >= 7 ? hb_itemGetCPtr( pDateString ) : NULL );
}

HB_FUNC( YEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iYear, 5 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( MONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iMonth, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1113, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DAY )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iDay, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1114, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
      hb_retnilen( hb_dateJulianDOW( hb_itemGetDL( pDate ) ), 3 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1115, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( TIME )
{
   char szResult[ 9 ];

   hb_dateTimeStr( szResult );
   hb_retclen( szResult, 8 );
}

HB_FUNC( DATE )
{
   int iYear, iMonth, iDay;

   hb_dateToday( &iYear, &iMonth, &iDay );
   hb_retd( iYear, iMonth, iDay );
}

HB_FUNC( HB_DATE )
{
   if( hb_pcount() == 0 )
   {
      int iYear, iMonth, iDay;
      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_retd( iYear, iMonth, iDay );
   }
   else
      hb_retd( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) );
}

HB_FUNC( HB_DATETIME )
{
   if( hb_pcount() == 0 )
   {
      long lDate, lTime;
      hb_timeStampGet( &lDate, &lTime );
      hb_rettdt( lDate, lTime );
   }
   else
      hb_rettdt( hb_dateEncode( hb_parni( 1 ), hb_parni( 2 ), hb_parni( 3 ) ),
                 hb_timeEncode( hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), hb_parni( 7 ) ) );
}

HB_FUNC( HB_DTOT )
{
   long lDate, lTime, lDate2;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      const char * szTime = hb_parc( 2 );
      if( szTime )
         hb_timeStampStrGetDT( szTime, &lDate2, &lTime );
      else if( HB_ISNUM( 2 ) )
      {
         lTime = ( long ) ( hb_parnd( 2 ) * 1000 );
         if( lTime < 0 )
            lTime = 0;
      }
      else
         lTime = 0;
      hb_rettdt( lDate, lTime );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_TTOD )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      hb_retdl( lDate );
      if( HB_ISBYREF( 2 ) )
      {
         const char * szTimeFormat = hb_parc( 3 );
         if( szTimeFormat )
         {
            char szBuffer[ 27 ];
            if( *szTimeFormat == '\0' )
               szTimeFormat = hb_setGetTimeFormat();
            hb_storc( hb_timeFormat( szBuffer, szTimeFormat, lTime ), 2 );
         }
         else
            hb_stornd( ( double ) lTime / 1000, 2 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_NTOT )
{
   PHB_ITEM pNum = hb_param( 1, HB_IT_NUMERIC );

   if( pNum )
      hb_rettd( hb_itemGetND( pNum ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_TTON )
{
   PHB_ITEM pTime = hb_param( 1, HB_IT_DATETIME );

   if( pTime )
      hb_retnd( hb_itemGetTD( pTime ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_TTOC )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      const char * szDateFormat = hb_parc( 2 );
      const char * szTimeFormat = hb_parc( 3 );
      char szBuffer[ 27 ];

      if( ! szDateFormat )
         szDateFormat = hb_setGetDateFormat();
      if( ! szTimeFormat )
         szTimeFormat = hb_setGetTimeFormat();

      hb_retc( hb_timeStampFormat( szBuffer, szDateFormat, szTimeFormat,
                                   lDate, lTime ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_CTOT )
{
   const char * szDateTime = hb_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;
      const char * szDateFormat = hb_parc( 2 );
      const char * szTimeFormat = hb_parc( 3 );

      if( ! szDateFormat )
         szDateFormat = hb_setGetDateFormat();
      if( ! szTimeFormat )
         szTimeFormat = hb_setGetTimeFormat();

      hb_timeStampUnformat( szDateTime, szDateFormat, szTimeFormat, &lDate, &lTime );
      hb_rettdt( lDate, lTime );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_TTOS )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      char szBuffer[ 18 ];

      hb_retc( hb_timeStampStrRawPut( szBuffer, lDate, lTime ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STOT )
{
   const char * szDateTime = hb_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;

      hb_timeStampStrRawGet( szDateTime, &lDate, &lTime );
      hb_rettdt( lDate, lTime );
   }
   else
      hb_rettdt( 0, 0 );
}

HB_FUNC( HB_HOUR )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      hb_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      hb_retnilen( iHour, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_MINUTE )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      hb_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      hb_retnilen( iMinutes, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_SEC )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      int iHour, iMinutes, iSeconds, iMSec;

      hb_timeDecode( lTime, &iHour, &iMinutes, &iSeconds, &iMSec );
      hb_retndlen( ( double ) ( iSeconds * 1000 + iMSec ) / 1000, 3, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_TSTOSTR )
{
   long lDate, lTime;

   if( hb_partdt( &lDate, &lTime, 1 ) )
   {
      char szBuffer[ 24 ];

      hb_timeStampStr( szBuffer, lDate, lTime );
      if( hb_parl( 2 ) )
      {
         if( lTime == 0 )
         {
            if( lDate == 0 )
               hb_retc_const( "00:00" );
            else
            {
               szBuffer[ 10 ] = '\0';
               hb_retc( szBuffer );
            }
         }
         else
         {
            int i = 23;
            while( szBuffer[ i - 1 ] == '0' )
               --i;
            if( szBuffer[ i - 1 ] == '.' )
            {
               --i;
               if( szBuffer[ i - 1 ] == '0' && szBuffer[ i - 2 ] == '0' )
                  i -= 3;
            }
            szBuffer[ i ] = '\0';
            if( lDate == 0 )
               hb_retc( szBuffer + 11 );
            else
               hb_retc( szBuffer );
         }
      }
      else
         hb_retc( szBuffer );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_STRTOTS )
{
   const char * szDateTime = hb_parc( 1 );

   if( szDateTime )
   {
      long lDate, lTime;

      hb_timeStampStrGetDT( szDateTime, &lDate, &lTime );
      hb_rettdt( lDate, lTime );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_UTCOFFSET )
{
   hb_retnl( hb_timeUTCOffset() );
}

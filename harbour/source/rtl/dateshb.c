/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Date API (Harbour level)
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *    HB_DAY()
 *    HB_MONTH()
 *    HB_YEAR()
 *    HB_DOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    HB_CTOD()
 *    HB_DATE()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    HB_HB_STOD()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#define HB_OS_WIN_32_USED

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

#include <ctype.h>
#include <time.h>
#if defined( OS_UNIX_COMPATIBLE )
   #include <sys/timeb.h>
#else
   #include <sys\timeb.h>
#endif
#if defined(__TURBOC__) || defined(__BORLANDC__)  || defined(__DJGPP__)
   #include <dos.h>
#endif

HARBOUR HB_CTOD( void )
{
   if( ISCHAR( 1 ) )
   {
      char * szDate = hb_parc( 1 );
      int d_value = 0, m_value = 0, y_value = 0;
      char szDateFormat[ 9 ];

      if( szDate )
      {
         int d_pos = 0, m_pos = 0, y_pos = 0;
         int count, digit, size = strlen( hb_set.HB_SET_DATEFORMAT );

         for( count = 0; count < size; count++ )
         {
            switch( hb_set.HB_SET_DATEFORMAT[ count ] )
            {
               case 'D':
               case 'd':
                  if( d_pos == 0 )
                  {
                     if( m_pos == 0 && y_pos == 0 ) d_pos = 1;
                     else if( m_pos == 0 || y_pos == 0 ) d_pos = 2;
                     else d_pos = 3;
                  }
                  break;
               case 'M':
               case 'm':
                  if( m_pos == 0 )
                  {
                     if( d_pos == 0 && y_pos == 0 ) m_pos = 1;
                     else if( d_pos == 0 || y_pos == 0 ) m_pos = 2;
                     else m_pos = 3;
                  }
                  break;
               case 'Y':
               case 'y':
                  if( y_pos == 0 )
                  {
                     if( m_pos == 0 && d_pos == 0 ) y_pos = 1;
                     else if( m_pos == 0 || d_pos == 0 ) y_pos = 2;
                     else y_pos = 3;
                  }
            }
         }

         size = strlen( szDate );

         for( count = 0; count < size; count++ )
         {
            digit = szDate[ count ];
            if( isdigit( digit ) )
            {
               if( d_pos == 1 )
                  d_value = ( d_value * 10 ) + digit - '0';
               else if( m_pos == 1 )
                  m_value = ( m_value * 10 ) + digit - '0';
               else if( y_pos == 1 )
                  y_value = ( y_value * 10 ) + digit - '0';
            }
            else if( digit != ' ' )
            {
               d_pos--;
               m_pos--;
               y_pos--;
            }
         }

         if( y_value >= 0 && y_value < 100 )
         {
            count = hb_set.HB_SET_EPOCH % 100;
            digit = hb_set.HB_SET_EPOCH / 100;

            if( y_value >= count )
               y_value += ( digit * 100 );
            else
               y_value += ( ( digit * 100 ) + 100 );
         }
      }

      sprintf( szDateFormat, "%04i%02i%02i", y_value, m_value, d_value );

      hb_retds( szDateFormat );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1119, NULL, "CTOD" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_DTOC( void )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dtoc( hb_pardsbuff( szDate, 1 ), szFormatted, hb_set.HB_SET_DATEFORMAT ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1118, NULL, "DTOC" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_DTOS( void )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];

      hb_retc( hb_pardsbuff( szDate, 1 ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1120, NULL, "DTOS" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* NOTE: Harbour extension, exactly the same as STOD(). */

HARBOUR HB_HB_STOD( void )
{
   hb_retds( hb_parc( 1 ) );
}

HARBOUR HB_YEAR( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( hb_itemGetDL( pDate ), &lDay, &lMonth, &lYear );

      hb_retnllen( lYear, 5 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1112, NULL, "YEAR" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_MONTH( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( hb_itemGetDL( pDate ), &lDay, &lMonth, &lYear );

      hb_retnllen( lMonth, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1113, NULL, "MONTH" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_DAY( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( hb_itemGetDL( pDate ), &lDay, &lMonth, &lYear );

      hb_retnllen( lDay, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1114, NULL, "DAY" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HARBOUR HB_TIME( void )
{
   char szResult[ 9 ];

   #if defined(HB_OS_WIN_32)
      SYSTEMTIME st;
      GetLocalTime( &st );
      sprintf( szResult, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond );
   #else
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      sprintf( szResult, "%02d:%02d:%02d", oTime->tm_hour, oTime->tm_min, oTime->tm_sec );
   #endif

   hb_retclen( szResult, 8 );
}

HARBOUR HB_DATE( void )
{
   char szResult[ 9 ];

   #if defined(HB_OS_WIN_32)
      SYSTEMTIME st;
      GetLocalTime( &st );
      sprintf( szResult, "%04d%02d%02d", st.wYear, st.wMonth, st.wDay );
   #else
      time_t t;
      struct tm * oTime;

      time( &t );
      oTime = localtime( &t );
      sprintf( szResult, "%04d%02d%02d", oTime->tm_year + 1900, oTime->tm_mon + 1, oTime->tm_mday );
   #endif

   hb_retds( szResult );
}

HARBOUR HB_DOW( void )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( lDate, &lDay, &lMonth, &lYear );

         hb_retnllen( hb_dow( lDay, lMonth, lYear ), 3 );
      }
      else
         hb_retnllen( 0, 3 );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1115, NULL, "DOW" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}


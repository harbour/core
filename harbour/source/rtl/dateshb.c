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
 *    DAY()
 *    MONTH()
 *    YEAR()
 *    DOW()
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    CTOD()
 *    DATE()
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    HB_STOD()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <ctype.h>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

HB_FUNC( CTOD )
{
   if( ISCHAR( 1 ) )
   {
      char * szDate = hb_parc( 1 );
      int d_value = 0, m_value = 0, y_value = 0;
      char szDateFormat[ 9 ];

      if( szDate )
      {
         int d_pos = 0, m_pos = 0, y_pos = 0;
         int count, digit, non_digit, size = strlen( hb_set.HB_SET_DATEFORMAT );

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

         /* If there are non-digits at the start of the date field,
            they are not to be treated as date field separators */
         non_digit = 1;
         size = strlen( szDate );
         for( count = 0; count < size; count++ )
         {
            digit = szDate[ count ];
            if( isdigit( digit ) )
            {
               /* Process the digit for the current date field */
               if( d_pos == 1 )
                  d_value = ( d_value * 10 ) + digit - '0';
               else if( m_pos == 1 )
                  m_value = ( m_value * 10 ) + digit - '0';
               else if( y_pos == 1 )
                  y_value = ( y_value * 10 ) + digit - '0';
               /* Treat the next non-digit as a date field separator */
               non_digit = 0;
            }
            else if( digit != ' ' )
            {
               /* Process the non-digit */
               if( non_digit++ == 0 )
               {
                  /* Only move to the next date field on the first
                     consecutive non-digit that is encountered */
                  d_pos--;
                  m_pos--;
                  y_pos--;
               }
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
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, "CTOD", 1, hb_paramError( 1 ) );
}

HB_FUNC( DTOC )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, hb_set.HB_SET_DATEFORMAT ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, "DTOC", 1, hb_paramError( 1 ) );
}

HB_FUNC( DTOS )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];

      hb_retc( hb_pardsbuff( szDate, 1 ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1120, NULL, "DTOS", 1, hb_paramError( 1 ) );
}

/* NOTE: Harbour extension, exactly the same as STOD(). */

HB_FUNC( HB_STOD )
{
#ifdef HB_FAST_STOD
   hb_retds( hb_parc( 1 ) );
#else
   hb_retds( ( ISCHAR( 1 ) && hb_parclen( 1 ) == 8 ) ? hb_parc( 1 ) : "        " );
#endif
}

HB_FUNC( YEAR )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      hb_retnllen( lYear, 5 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1112, NULL, "YEAR", 1, hb_paramError( 1 ) );
}

HB_FUNC( MONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      hb_retnllen( lMonth, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1113, NULL, "MONTH", 1, hb_paramError( 1 ) );
}

HB_FUNC( DAY )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );

      hb_retnllen( lDay, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1114, NULL, "DAY", 1, hb_paramError( 1 ) );
}

HB_FUNC( TIME )
{
   char szResult[ 9 ];
   hb_dateTimeStr( szResult );
   hb_retclen( szResult, 8 );
}

HB_FUNC( DATE )
{
   long lYear, lMonth, lDay;
   hb_dateToday( &lYear, &lMonth, &lDay );
   hb_retd( lYear, lMonth, lDay );
}

HB_FUNC( DOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         long lYear, lMonth, lDay;

         hb_dateDecode( lDate, &lYear, &lMonth, &lDay );

         hb_retnllen( hb_dateDOW( lYear, lMonth, lDay ), 3 );
      }
      else
         hb_retnllen( 0, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1115, NULL, "DOW", 1, hb_paramError( 1 ) );
}


/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CMONTH(), CDOW() functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbdate.h"

char * hb_cmonth( int iMonth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cmonth(%d)", iMonth));

   return ( iMonth >= 1 && iMonth <= 12 ) ? hb_monthsname[ iMonth - 1 ] : "";
}

char * hb_cdow( int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_cdow(%d)", iDay));

   return ( iDay >= 1 && iDay <= 7 ) ? hb_daysname[ iDay - 1 ] : "";
}

HB_FUNC( CMONTH )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDay, lMonth, lYear;

      hb_dateDecode( hb_itemGetDL( pDate ), &lDay, &lMonth, &lYear );
      hb_retc( hb_cmonth( lMonth ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1116, NULL, "CMONTH" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

HB_FUNC( CDOW )
{
   PHB_ITEM pDate = hb_param( 1, IT_DATE );

   if( pDate )
   {
      long lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         long lDay, lMonth, lYear;

         hb_dateDecode( lDate, &lDay, &lMonth, &lYear );
         hb_retc( hb_cdow( hb_dow( lDay, lMonth, lYear ) ) );
      }
      else
         hb_retc( "" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1117, NULL, "CDOW" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}


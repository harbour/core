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
#include "hbapilng.h"
#include "hbdate.h"

char * hb_dateCMonth( int iMonth )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateCMonth(%d)", iMonth));

   return ( iMonth >= 1 && iMonth <= 12 ) ? ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 ) : "";
}

char * hb_dateCDOW( int iDay )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_dateCDOW(%d)", iDay));

   return ( iDay >= 1 && iDay <= 7 ) ? ( char * ) hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + iDay - 1 ) : "";
}

HB_FUNC( CMONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lYear, lMonth, lDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &lYear, &lMonth, &lDay );
      hb_retc( hb_dateCMonth( lMonth ) );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1116, NULL, "CMONTH" );

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}

HB_FUNC( CDOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      long lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         long lYear, lMonth, lDay;

         hb_dateDecode( lDate, &lYear, &lMonth, &lDay );
         hb_retc( hb_dateCDOW( hb_dateDOW( lYear, lMonth, lDay ) ) );
      }
      else
         hb_retc( "" );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1117, NULL, "CDOW" );

      if( pResult )
         hb_itemRelease( hb_itemReturn( pResult ) );
   }
}


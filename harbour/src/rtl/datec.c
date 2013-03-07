/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CMonth(), CDoW() functions
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapilng.h"
#include "hbdate.h"

#if defined( __CODEGUARD__ )
   static const char s_nullStr[ 4 ] = { 0 };
#else
#  define s_nullStr     ""
#endif

const char * hb_dateCMonth( int iMonth )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateCMonth(%d)", iMonth ) );

   return ( iMonth >= 1 && iMonth <= 12 ) ? hb_langDGetItem( HB_LANG_ITEM_BASE_MONTH + iMonth - 1 ) : s_nullStr;
}

const char * hb_dateCDOW( int iDay )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_dateCDOW(%d)", iDay ) );

   return ( iDay >= 1 && iDay <= 7 ) ? hb_langDGetItem( HB_LANG_ITEM_BASE_DAY + iDay - 1 ) : s_nullStr;
}

HB_FUNC( CMONTH )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );
      hb_retc_const( hb_dateCMonth( iMonth ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1116, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( CDOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATETIME );

   if( pDate )
   {
      long lDate = hb_itemGetDL( pDate );

      if( lDate )
      {
         int iYear, iMonth, iDay;

         hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
         hb_retc_const( hb_dateCDOW( hb_dateDOW( iYear, iMonth, iDay ) ) );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1117, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

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
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    HB_STOD()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbset.h"
#include "hbdate.h"

HB_FUNC( CTOD )
{
   if( ISCHAR( 1 ) )
      hb_retdl( hb_dateUnformat( hb_parc( 1 ), hb_setGetDateFormat() ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1119, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DTOC )
{
   if( ISDATE( 1 ) )
   {
      char szDate[ 9 ];
      char szFormatted[ 11 ];

      hb_retc( hb_dateFormat( hb_pardsbuff( szDate, 1 ), szFormatted, hb_setGetDateFormat() ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1118, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( DTOS )
{
   if( ISDATE( 1 ) )
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
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

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
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

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
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
   {
      int iYear, iMonth, iDay;

      hb_dateDecode( hb_itemGetDL( pDate ), &iYear, &iMonth, &iDay );

      hb_retnilen( iDay, 3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1114, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
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

HB_FUNC( DOW )
{
   PHB_ITEM pDate = hb_param( 1, HB_IT_DATE );

   if( pDate )
      hb_retnilen( hb_dateJulianDOW( hb_itemGetDL( pDate ) ), 3 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1115, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

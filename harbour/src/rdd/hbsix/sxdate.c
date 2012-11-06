/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible functions:
 *          hb_sxDtoP()
 *          hb_sxPtoD()
 *
 *          SX_DTOP()
 *          SX_PTOD()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbsxfunc.h"

char * hb_sxDtoP( char * pDate, long lJulian )
{
   int iYear, iMonth, iDay;
   long lPDate;

   HB_TRACE( HB_TR_DEBUG, ( "hb_sxDtoP(%p, %ld)", pDate, lJulian ) );

   hb_dateDecode( lJulian, &iYear, &iMonth, &iDay );
   lPDate = ( ( ( iYear << 1 ) | ( iMonth >> 3 ) ) << 8 ) |
            ( ( iMonth & 7 ) << 5 ) | iDay;
   HB_PUT_BE_UINT24( pDate, lPDate );

   return pDate;
}

long hb_sxPtoD( const char * pDate )
{
   int iYear, iMonth, iDay;
   long lPDate;

   HB_TRACE( HB_TR_DEBUG, ( "hb_sxPtoD(%p)", pDate ) );

   if( pDate )
   {
      lPDate = HB_GET_BE_UINT24( pDate );
      iDay = lPDate & 0x1f;
      iMonth = ( lPDate >> 5 ) & 0x0f;
      iYear = ( lPDate >> 9 );

      return hb_dateEncode( iYear, iMonth, iDay );
   }
   return 0;
}

HB_FUNC( SX_DTOP )
{
   char pDate[ 3 ];

   hb_retclen( hb_sxDtoP( pDate, hb_pardl( 1 ) ), 3 );
}

HB_FUNC( SX_PTOD )
{
   hb_retdl( hb_sxPtoD( hb_parclen( 1 ) < 3 ? NULL : hb_parc( 1 ) ) );
}

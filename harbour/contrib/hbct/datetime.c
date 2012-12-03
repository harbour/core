/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions: - BOM() / EOM()
 *                              - BOQ() / EOQ()
 *                              - BOY() / EOY()
 *                              - WOM()
 *
 * Copyright 2005 Pavel Tsarenko <tpe2@mail.ru>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
 *   Wom()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbdate.h"

HB_FUNC( BOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      hb_retd( iYear, iMonth, 1 );
   }
   else
      hb_retdl( 0 );
}

HB_FUNC( EOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth++;
      if( iMonth > 12 )
      {
         iMonth = 1;
         iYear++;
      }
      hb_retdl( hb_dateEncode( iYear, iMonth, 1 ) - 1 );
   }
   else
      hb_retdl( 0 );
}

HB_FUNC( BOQ )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth -= ( iMonth - 1 ) % 3;

      hb_retd( iYear, iMonth, 1 );
   }
   else
      hb_retdl( 0 );
}

HB_FUNC( EOQ )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      iMonth += 3 - ( ( iMonth - 1 ) % 3 );
      if( iMonth > 12 )
      {
         iMonth = 1;
         iYear++;
      }
      hb_retdl( hb_dateEncode( iYear, iMonth, 1 ) - 1 );
   }
   else
      hb_retdl( 0 );
}

HB_FUNC( BOY )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      hb_retd( iYear, 1, 1 );
   }
   else
      hb_retdl( 0 );
}

HB_FUNC( EOY )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      hb_retdl( hb_dateEncode( iYear + 1, 1, 1 ) - 1 );
   }
   else
      hb_retdl( 0 );
}

static int hb_wom( int iYear, int iMonth, int iDay )
{
   int iWom;

   HB_TRACE( HB_TR_DEBUG, ( "hb_wom(%d, %d, %d)", iYear, iMonth, iDay ) );

   iWom = iDay + hb_dateDOW( iYear, iMonth, 1 ) - 1;
   if( iWom > 0 )
      return ( iWom - hb_dateDOW( iYear, iMonth, iDay ) ) / 7 + 1;
   else
      return 0;
}

HB_FUNC( WOM )
{
   long lDate;
   int iYear, iMonth, iDay;

   if( HB_ISNIL( 1 ) )
   {
      hb_dateToday( &iYear, &iMonth, &iDay );
      lDate = hb_dateEncode( iYear, iMonth, iDay );
   }
   else
      lDate = hb_pardl( 1 );

   if( lDate != 0 )
   {
      hb_dateDecode( lDate, &iYear, &iMonth, &iDay );
      hb_retni( hb_wom( iYear, iMonth, iDay ) );
   }
   else
      hb_retni( 0 );
}

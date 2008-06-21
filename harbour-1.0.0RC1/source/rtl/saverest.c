/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * SAVESCREEN(), RESTSCREEN() functions
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

#include "hbapi.h"
#include "hbapigt.h"

static void hb_getScreenRange( USHORT * pusMin, USHORT * pusMax,
                               BOOL fNoCheck, BOOL fVertical )
{
   int iFrom, iTo, iMax;

   if( fVertical )
   {
      iMax  = hb_gtMaxRow();
      iFrom = hb_parni( 1 );
      iTo   = ISNUM( 3 ) ? hb_parni( 3 ) : iMax;
   }
   else
   {
      iMax = hb_gtMaxCol();
      iFrom = hb_parni( 2 );
      iTo   = ISNUM( 4 ) ? hb_parni( 4 ) : iMax;
   }

   if( iFrom < 0 )
      iFrom = 0;
   else if( iFrom > iMax && !fNoCheck )
      iFrom = iMax;

   if( iTo < 0 )
      iTo = 0;
   else if( iTo > iMax && !fNoCheck )
      iTo = iMax;

   if( iFrom > iTo )
   {
      *pusMin = ( USHORT ) iTo;
      *pusMax = ( USHORT ) iFrom;
   }
   else
   {
      *pusMin = ( USHORT ) iFrom;
      *pusMax = ( USHORT ) iTo;
   }
}

HB_FUNC( SAVESCREEN )
{
   USHORT uiTop, uiLeft, uiBottom, uiRight;
   ULONG  ulSize;
   void * pBuffer;
#if defined( HB_EXTENSION )
   BOOL fNoCheck = hb_parl( 5 );
#else
   BOOL fNoCheck = FALSE;
#endif

   hb_getScreenRange( &uiTop, &uiBottom, fNoCheck, TRUE );
   hb_getScreenRange( &uiLeft, &uiRight, fNoCheck, FALSE );

   hb_gtRectSize( uiTop, uiLeft, uiBottom, uiRight, &ulSize );
   pBuffer = hb_xgrab( ulSize + 1 );

   hb_gtSave( uiTop, uiLeft, uiBottom, uiRight, pBuffer );
   hb_retclen_buffer( ( char * ) pBuffer, ulSize );
}

HB_FUNC( RESTSCREEN )
{
   if( ISCHAR( 5 ) )
   {
      USHORT uiTop, uiLeft, uiBottom, uiRight;
#if defined( HB_EXTENSION )
      BOOL fNoCheck = hb_parl( 6 );
#else
      BOOL fNoCheck = FALSE;
#endif

      hb_getScreenRange( &uiTop, &uiBottom, fNoCheck, TRUE );
      hb_getScreenRange( &uiLeft, &uiRight, fNoCheck, FALSE );

      hb_gtRest( uiTop, uiLeft, uiBottom, uiRight, ( void * ) hb_parc( 5 ) );
   }
}

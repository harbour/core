/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PAD*() functions
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbset.h"
#include "hbdate.h"

/* This function is used by all of the PAD functions to prepare the argument
   being padded. If date, convert to string using hb_dateFormat(). If numeric,
   convert to unpadded string. Return pointer to string and set string length */

static char * hb_itemPadConv( PHB_ITEM pItem, char * buffer, ULONG * pulSize )
{
   char * szText;

   HB_TRACE(HB_TR_DEBUG, ("hb_itemPadCond(%p, %p, %p)", pItem, buffer, pulSize));

   if( pItem )
   {
      if( IS_STRING( pItem ) )
      {
         szText = hb_itemGetCPtr( pItem );
         *pulSize = hb_itemGetCLen( pItem );
      }
      else if( IS_DATE( pItem ) )
      {
         char szDate[ 9 ];

         szText = hb_dateFormat( hb_pardsbuff( szDate, 1 ), buffer, hb_set.HB_SET_DATEFORMAT );
         *pulSize = strlen( szText );
      }
      else if( IS_INTEGER( pItem ) )
      {
         sprintf( buffer, "%d", hb_itemGetNI( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else if( IS_LONG( pItem ) )
      {
         sprintf( buffer, "%ld", hb_itemGetNL( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else if( IS_DOUBLE( pItem ) )
      {
         int iDecimal;

         hb_itemGetNLen( pItem, NULL, &iDecimal );
         sprintf( buffer, "%.*f", iDecimal, hb_itemGetND( pItem ) );
         szText = buffer;
         *pulSize = strlen( szText );
      }
      else
         szText = NULL;
   }
   else
      szText = NULL;

   return szText;
}

/* right-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padr( 'hello', 10 ) = '" + padr( 'hello', 10 ) + "'" ) */
HB_FUNC( PADR )
{
   ULONG ulSize;
   char buffer[ 128 ];
   char * szText = hb_itemPadConv( hb_param( 1, IT_ANY ), buffer, &ulSize );

   if( szText && ISNUM( 2 ) )
   {
      long lLen = hb_parnl( 2 );

      if( lLen > ( long ) ulSize )
      {
         char * szResult = ( char * ) hb_xgrab( lLen + 1 );
         long lPos;
         char cPad;

         hb_xmemcpy( szResult, szText, ( long ) ulSize );

         cPad = ( ISCHAR( 3 ) ? *( hb_parc( 3 ) ) : ' ' );

         for( lPos = ( long ) ulSize; lPos < lLen; lPos++ )
            szResult[ lPos ] = cPad;

         hb_retclen( szResult, ( ULONG ) lLen );
         hb_xfree( szResult );
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen( szText, lLen );
      }
   }
   else
      hb_retc( "" );
}

/* synonymn for PADR */
HB_FUNC( PAD )
{
   HB_FUNCNAME( PADR )();
}

/* left-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padl( 'hello', 10 ) = '" + padl( 'hello', 10 ) + "'" ) */
HB_FUNC( PADL )
{
   ULONG ulSize;
   char buffer[ 128 ];
   char * szText = hb_itemPadConv( hb_param( 1, IT_ANY ), buffer, &ulSize );

   if( szText && ISNUM( 2 ) )
   {
      long lLen = hb_parnl( 2 );

      if( lLen > ( long ) ulSize )
      {
         char * szResult = ( char * ) hb_xgrab( lLen + 1 );
         long lPos = lLen - ( long ) ulSize;
         char cPad;

         hb_xmemcpy( szResult + lPos, szText, ( long ) ulSize );

         cPad = ( ISCHAR( 3 ) ? *( hb_parc( 3 ) ) : ' ');

         for(; lPos > 0; lPos-- )
         {
            szResult[ lPos - 1 ] = cPad;
         }

         hb_retclen( szResult, lLen );
         hb_xfree( szResult );
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen( szText, lLen );
      }
   }
   else
      hb_retc( "" );
}

/* centre-pads a date, number, or string with spaces or supplied character */
/* TEST: QOUT( "padc( 'hello', 10 ) = '" + padc( 'hello', 10 ) + "'" ) */
HB_FUNC( PADC )
{
   ULONG ulSize;
   char buffer[ 128 ];
   char * szText = hb_itemPadConv( hb_param( 1, IT_ANY ), buffer, &ulSize );

   if( szText && ISNUM( 2 ) )
   {
      long lLen = hb_parnl( 2 );

      if( lLen > ( long ) ulSize )
      {
         char * szResult = ( char * ) hb_xgrab( lLen + 1 );
         char cPad;
         long w, lPos = ( lLen - ( long ) ulSize ) / 2;

         hb_xmemcpy( szResult + lPos, szText, ( long ) ulSize + 1 );

         cPad = ( ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ' );

         for( w = 0; w < lPos; w++ )
            szResult[ w ] = cPad;

         for( w = ( long ) ulSize + lPos; w < lLen; w++ )
            szResult[ w ] = cPad;

         szResult[ lLen ] = '\0';

         hb_retclen( szResult, lLen );
         hb_xfree( szResult );
      }
      else
      {
         if( lLen < 0 )
            lLen = 0;

         hb_retclen( szText, lLen );
      }
   }
   else
      hb_retc( "" );
}


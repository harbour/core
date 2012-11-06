/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    SIX compatible function:
 *          Sx_SlimFast()
 *          Sx_WildMatch()
 *          Sx_Version()
 *          Sx_Error()
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

#include "hbapi.h"
#include "hbapicdp.h"

HB_FUNC( SX_SLIMFAST )
{
   const char * szExp = hb_parc( 1 );

   if( szExp && *szExp )
   {
      char * szDst, cQuote = 0, c;
      HB_SIZE nDst;

      szDst = hb_cdpnDupUpper( hb_vmCDP(), szExp, NULL );
      szExp = szDst;
      nDst = 0;

      while( ( c = *szExp++ ) != 0 )
      {
         if( c == cQuote )
            cQuote = 0;
         else if( ! cQuote )
         {
            if( c == '"' || c == '\'' )
               cQuote = c;
            else if( c == ' ' && nDst && szDst[ nDst - 1 ] == ' ' )
               continue;
         }
         szDst[ nDst++ ] = c;
      }

      hb_retclen_buffer( szDst, nDst );
   }
   else
      hb_retc_null();
}

HB_FUNC( SX_WILDMATCH )
{
   const char * szPattern = hb_parc( 1 ), * szValue = hb_parc( 2 );
   HB_BOOL fMatch = HB_FALSE;

   if( szPattern && szPattern[ 0 ] && szValue )
      fMatch = hb_strMatchWild( szValue, szPattern );

   hb_retl( fMatch );
}

#define HB_SX_VER   "1.00.00"
#define HB_SX_DAY   "20070530"
#define HB_SX_TIME  "01:00"
#define HB_SX_FULL  "Harbour SIx3 compatible library, 1.00.00 2007/05/30 01:00"

HB_FUNC( SX_VERSION )
{
   switch( hb_parni( 1 ) )
   {
      case 1:
         hb_retds( HB_SX_DAY );
         break;
      case 2:
         hb_retc( HB_SX_TIME );
         break;
      case 3:
         hb_retc( HB_SX_FULL );
         break;
      default:
         hb_retc( HB_SX_VER );
         break;
   }
}

HB_FUNC( SX_ERROR )
{
   /* not use by Harbour */
   hb_retni( 0 );
}

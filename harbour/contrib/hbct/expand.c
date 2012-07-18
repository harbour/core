/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string function:  EXPAND()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
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

HB_FUNC( EXPAND )
{
   HB_SIZE nLen = hb_parclen( 1 ), ulSize, ul;

   if( nLen > 0 )
   {
      const char * szText = hb_parc( 1 );
      if( nLen == 1 )
         hb_retclen( szText, 1 );
      else
      {
         char * szDest, *szPtr, cRepl;
         int iRepl, i;

         iRepl = hb_parni( 2 );
         i = hb_pcount();
         if( i == 2 && HB_ISCHAR( 2 ) )
         {
            iRepl = 1;
            cRepl = hb_parc( 2 )[ 0 ];
         }
         else if( i == 2 && iRepl == 0 && HB_ISNUM( 2 ) )
         {
            iRepl = 1;
            cRepl = 0;
         }
         else
         {
            if( iRepl < 1 )
               iRepl = 1;
            if( HB_ISNUM( 3 ) )
               cRepl = ( char ) hb_parni( 3 );
            else if( HB_ISCHAR( 3 ) )
               cRepl = hb_parc( 3 )[ 0 ];
            else
               cRepl = ' ';
         }
         ulSize = ( nLen - 1 ) * ( iRepl + 1 ) + 1;
         szPtr = szDest = ( char * ) hb_xgrab( ulSize + 1 );
         *szPtr++ = szText[ 0 ];
         for( ul = 1; ul < nLen; ++ul )
         {
            for( i = 0; i < iRepl; ++i )
               *szPtr++ = cRepl;
            *szPtr++ = szText[ ul ];
         }
         hb_retclen_buffer( szDest, ulSize );
      }
   }
   else
      hb_retc_null();
}

/*
 * Harbour Project source code:
 * Replicate() function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* returns n copies of given string */

HB_FUNC( REPLICATE )
{
   if( HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      HB_ISIZ nTimes = hb_parns( 2 );

      if( nTimes > 0 )
      {
         HB_SIZE nLen = hb_parclen( 1 );

         if( ( double ) ( ( double ) nLen * ( double ) nTimes ) < ( double ) ULONG_MAX )
         {
            const char * szText = hb_parc( 1 );
            char * szResult = ( char * ) hb_xgrab( ( nLen * nTimes ) + 1 );
            char * szPtr = szResult;
            HB_ISIZ n;

            for( n = 0; n < nTimes; ++n )
            {
               hb_xmemcpy( szPtr, szText, nLen );
               szPtr += nLen;
            }

            hb_retclen_buffer( szResult, nLen * nTimes );
         }
         else
            hb_errRT_BASE_SubstR( EG_STROVERFLOW, 1234, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      }
      else
         hb_retc_null();
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1106, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

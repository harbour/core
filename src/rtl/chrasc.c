/*
 * Harbour Project source code:
 * Chr(), Asc() functions
 *
 * Copyright 2012 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "hbapierr.h"

/* converts an ASCII code to a character value */
HB_FUNC( CHR )
{
   if( HB_ISNUM( 1 ) )
   {
      /* NOTE: CA-Cl*pper's compiler optimizer will be wrong for those
               Chr() cases where the passed parameter is a constant which
               can be divided by 256 but it's not zero, in this case it
               will return an empty string instead of a Chr( 0 ). [vszakats] */

      /* Believe it or not, clipper does this! */
#ifdef HB_CLP_STRICT
      char szChar[ 2 ];
      szChar[ 0 ] = hb_parnl( 1 ) % 256;
      szChar[ 1 ] = '\0';
      hb_retclen( szChar, 1 );
#else
      PHB_CODEPAGE cdp = hb_vmCDP();
      if( HB_CDP_ISCHARUNI( cdp ) )
      {
         char szChar[ HB_MAX_CHAR_LEN ];
         HB_SIZE nLen;

         nLen = hb_cdpTextPutU16( hb_vmCDP(), szChar, sizeof( szChar ),
                                           ( HB_WCHAR ) hb_parni( 1 ) );
         hb_retclen( szChar, nLen );
      }
      else
         hb_retclen( hb_szAscii[ hb_parni( 1 ) & 0xFF ], 1 );
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1104, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* converts a character value to an ASCII code */
HB_FUNC( ASC )
{
   const char * szValue = hb_parc( 1 );

   if( szValue )
   {
      int iChar;
      PHB_CODEPAGE cdp = hb_vmCDP();
      if( HB_CDP_ISCHARUNI( cdp ) )
         iChar = hb_cdpTextGetU16( cdp, szValue, hb_parclen( 1 ) );
      else
         iChar = ( HB_UCHAR ) szValue[ 0 ];

      hb_retni( iChar );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1107, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

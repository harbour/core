/*
 * Harbour Project source code:
 *    hb_byte*() functions
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
#include "hbapierr.h"

static HB_BOOL hb_numParam( int iParam, HB_MAXINT * plNum )
{
   if( HB_ISNUM( iParam ) )
   {
      *plNum = hb_parnint( iParam );
      return HB_TRUE;
   }
   hb_errRT_BASE_SubstR( EG_ARG, 1089, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   *plNum = 0;
   return HB_FALSE;
}

HB_FUNC( HB_BYTESWAPI )
{
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      HB_I16 iVal = ( HB_I16 ) HB_SWAP_UINT16( lValue );
      hb_retnint( iVal );
   }
}

HB_FUNC( HB_BYTESWAPW )
{
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      HB_U16 uiVal = ( HB_U16 ) HB_SWAP_UINT16( lValue );
      hb_retnint( uiVal );
   }
}

HB_FUNC( HB_BYTESWAPL )
{
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      HB_I32 iVal = ( HB_I32 ) HB_SWAP_UINT32( lValue );
      hb_retnint( iVal );
   }
}

HB_FUNC( HB_BYTESWAPU )
{
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      HB_U32 uiVal = ( HB_U32 ) HB_SWAP_UINT32( lValue );
      hb_retnint( uiVal );
   }
}

HB_FUNC( HB_BYTESWAPLL )
{
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
#if defined( HB_LONG_LONG_OFF )
      HB_MAXINT iVal = ( HB_MAXINT ) HB_SWAP_UINT32( lValue );
#else
      HB_MAXINT iVal = ( HB_MAXINT ) HB_SWAP_UINT64( lValue );
#endif
      hb_retnint( iVal );
   }
}

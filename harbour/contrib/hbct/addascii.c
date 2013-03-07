/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   AddAscii() CT3 string function
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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

#include "ct.h"

HB_FUNC( ADDASCII )
{
   /* suppressing return value ? */
   int iNoRet = ct_getref() && HB_ISBYREF( 1 );

   if( HB_ISCHAR( 1 ) )
   {
      const char * pcSource = hb_parc( 1 );
      HB_SIZE sLen = hb_parclen( 1 );
      char * pcResult;
      HB_SIZE sPos = hb_parnsdef( 3, sLen );
      HB_LONG lValue;
      int iCarryOver;

      if( sPos > sLen || ! HB_ISNUM( 2 ) || sLen == 0 )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII, NULL,
                      HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );

         /* return string unchanged */
         if( iNoRet )
            hb_retl( HB_FALSE );
         else
            hb_retclen( pcSource, sLen );

         return;
      }

      pcResult = ( char * ) hb_xgrab( sLen + 1 );
      hb_xmemcpy( pcResult, pcSource, sLen );

      lValue = hb_parnl( 2 );
      iCarryOver = hb_parldef( 4, 0 );

      if( iCarryOver )
      {
         HB_SIZE sCurrent;
         HB_LONG lResult;

         for( sCurrent = sPos; sCurrent > 0 && lValue != 0; sCurrent-- )
         {
            lResult = ( HB_LONG ) pcSource[ sCurrent - 1 ] + ( lValue % 256 );

            lValue /= 256;
            if( lResult > 255 )
               lValue++;
            else if( lResult < 0 )
               lValue--;

            pcResult[ sCurrent - 1 ] = ( char ) ( lResult % 256 );
         }
      }
      else
         pcResult[ sPos - 1 ] = ( char ) ( ( ( HB_LONG ) pcResult[ sPos - 1 ] + lValue ) % 256 );

      hb_storclen( pcResult, sLen, 1 );

      if( iNoRet )
      {
         hb_retl( HB_FALSE );
         hb_xfree( pcResult );
      }
      else
         hb_retclen_buffer( pcResult, sLen );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ADDASCII,
                                  NULL, HB_ERR_FUNCNAME, 0, EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRet )
         hb_retl( HB_FALSE );
      else
         hb_retc_null();
   }
}

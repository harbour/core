/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string function CHARREPL()
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

#include "ct.h"

HB_FUNC( CHARREPL )
{
   int iNoRet;
   HB_SIZE sSearchLen, sReplaceLen;

   /* suppressing return value ? */
   iNoRet = ct_getref() && HB_ISBYREF( 2 );

   /* param check */
   if( ( sSearchLen = hb_parclen( 1 ) ) > 0 && HB_ISCHAR( 2 ) &&
       ( sReplaceLen = hb_parclen( 3 ) ) > 0 )
   {
      /* get parameters */
      const char * pcSearch = hb_parc( 1 );
      const char * pcString = hb_parc( 2 );
      HB_SIZE sStrLen = hb_parclen( 2 );
      const char * pcReplace = hb_parc( 3 );
      int iMode = hb_parldef( 4, 0 );
      char * pcRet;
      HB_SIZE sIndex;

      /* if sStrLen == 0, we can return immediately */
      if( sStrLen == 0 )
      {
         if( iNoRet )
            hb_retl( HB_FALSE );
         else
            hb_retc_null();
         return;
      }

      pcRet = ( char * ) hb_xgrab( sStrLen + 1 );
      hb_xmemcpy( pcRet, pcString, sStrLen );

      for( sIndex = 0; sIndex < sSearchLen; sIndex++ )
      {
         HB_SIZE sMatchStrLen;
         const char * pc;
         HB_SIZE sReplIndex = sIndex;

         if( sReplIndex > sReplaceLen - 1 )
            sReplIndex = sReplaceLen - 1;

         if( iMode )
         {
            /* no multiple replacements: searching in pcString,
               replacing in pcRet */
            pc = pcString;

            while( ( pc = ct_at_exact_forward( pc, sStrLen - ( pc - pcString ),
                                               pcSearch + sIndex, 1,
                                               &sMatchStrLen ) ) != NULL )
            {
               *( pcRet + ( pc - pcString ) ) = *( pcReplace + sReplIndex );
               pc++;
            }
         }
         else
         {
            /* multiple replacements: searching & replacing in pcRet */
            char * pcw = pcRet;
            while( ( pcw = ( char * ) ct_at_exact_forward( pcw, sStrLen - ( pcw - pcRet ),
                                                           pcSearch + sIndex, 1,
                                                           &sMatchStrLen ) ) != NULL )
            {
               *pcw++ = *( pcReplace + sReplIndex );
            }
         }
      }

      /* return string */
      hb_storclen( pcRet, sStrLen, 2 );

      if( iNoRet )
      {
         hb_retl( HB_FALSE );
         hb_xfree( pcRet );
      }
      else
         hb_retclen_buffer( pcRet, sStrLen );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CHARREPL, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRet )
         hb_retl( HB_FALSE );
      else if( HB_ISCHAR( 2 ) )
         hb_retclen( hb_parc( 2 ), hb_parclen( 2 ) );
      else
         hb_retc_null();
   }
}

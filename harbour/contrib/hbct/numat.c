/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   NUMAT() CT3 string function
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

HB_FUNC( NUMAT )
{
   if( ( HB_ISCHAR( 1 ) ) && ( HB_ISCHAR( 2 ) ) )
   {
      const char * pcStringToMatch = hb_parc( 1 );
      HB_SIZE sStrToMatchLen = hb_parclen( 1 );
      const char * pcString = hb_parc( 2 );
      HB_SIZE sStrLen = hb_parclen( 2 );
      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      HB_SIZE sIgnore, sMatchStrLen = 0, sSubStrLen;
      HB_SIZE nCounter;
      const char * pc, * pcSubStr;

      /* eventually ignore some characters */
      if( HB_ISNUM( 3 ) )
         sIgnore = hb_parns( 3 );
      else
         sIgnore = 0;

      if( sIgnore >= sStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_NUMAT, NULL,
                      HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
         }
         hb_retni( 0 );
         return;
      }
      else
      {
         pcString += sIgnore;
         sStrLen  -= sIgnore;
      }

      nCounter = 0;
      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      do
      {
         switch( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ct_at_exact_forward( pcSubStr, sSubStrLen, pcStringToMatch,
                                         sStrToMatchLen, &sMatchStrLen );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ct_at_wildcard_forward( pcSubStr, sSubStrLen,
                                            pcStringToMatch, sStrToMatchLen,
                                            cAtLike, &sMatchStrLen );
               break;

            default:
               pc = NULL;
         }
         nCounter++;
         if( iMultiPass )
            pcSubStr = pc + 1;
         else
            pcSubStr = pc + sMatchStrLen;
         sSubStrLen = sStrLen - ( pcSubStr - pcString );
      }
      while( pc != NULL );

      hb_retns( nCounter - 1 );
   }
   else  /* ( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_NUMAT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retni( 0 );
   }
}

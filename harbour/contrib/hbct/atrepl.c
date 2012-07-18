/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   ATREPL() CT3 string function
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

HB_FUNC( ATREPL )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * pcStringToMatch = hb_parc( 1 );
      HB_SIZE nStrToMatchLen = hb_parclen( 1 );
      const char * pcString = hb_parc( 2 );
      HB_SIZE nStrLen = hb_parclen( 2 );
      int iMultiPass = ct_getatmupa();
      int iAtLike = ct_getatlike();
      char cAtLike = ct_getatlikechar();
      HB_SIZE nIgnore, nMatchStrLen = 0;
      HB_SIZE nCounter;
      char * pc;

      const char * pcReplacement;
      HB_SIZE nReplaceLen;
      int iReplaceMode;
      char * pcRetStr;
      HB_SIZE nRetStrLen;

      /* eventually ignore some characters */
      nIgnore = hb_parns( 6 );

      if( nIgnore >= nStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_ATREPL, NULL,
                      HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
         }
         hb_retclen( pcString, nStrLen );
         return;
      }

      /* replacement */
      pcReplacement = hb_parc( 3 );
      nReplaceLen = pcReplacement ? hb_parclen( 3 ) : 0;

      /* replace mode */
      iReplaceMode = hb_parl( 5 );

      /* n-th match or last match ? */
      nCounter = hb_parns( 4 );

      /* little trick: */
      if( iReplaceMode == 0 && nCounter == 0 )
         nCounter = HB_SIZE_MAX;

      if( nCounter != 0 )
      {
         /* depending on iReplaceMode: replace all occurences including the nth one
            or only the nth occurence
            NOTE: if iReplaceMode = false and the nth occurence does not exist,
            all occurences are replaced */
         char * pcRetSubStr;
         HB_SIZE sRetSubStrLen;
         HB_SIZE nMatchCounter = 0;

         nRetStrLen = nStrLen;
         pcRetStr = ( char * ) hb_xgrab( nRetStrLen + 1 );
         hb_xmemcpy( pcRetStr, pcString, nRetStrLen );

         pcRetSubStr = pcRetStr + nIgnore;
         sRetSubStrLen = nRetStrLen - nIgnore;

         while( nMatchCounter < nCounter )
         {
            switch ( iAtLike )
            {
               case CT_SETATLIKE_EXACT:
                  pc = ( char * ) ct_at_exact_forward( pcRetSubStr, sRetSubStrLen, pcStringToMatch,
                                                       nStrToMatchLen, &nMatchStrLen );
                  break;

               case CT_SETATLIKE_WILDCARD:
                  pc = ( char * ) ct_at_wildcard_forward( pcRetSubStr, sRetSubStrLen, pcStringToMatch,
                                                          nStrToMatchLen, cAtLike, &nMatchStrLen );
                  break;

               default:
                  pc = NULL;
            }

            if( pc == NULL )
            {
               hb_retclen_buffer( pcRetStr, nRetStrLen );
               return;
            }

            nMatchCounter++;

            /* replace match ? */
            if( ( iReplaceMode == 0 ) || ( nMatchCounter == nCounter ) )
            {
               if( nMatchStrLen < nReplaceLen )
               {
                  /* pcRetStr grows, so realloc memory */
                  /* save pc pointer */
                  HB_SIZE sPCPos = pc - pcRetStr;

                  pcRetStr = ( char * ) hb_xrealloc( pcRetStr,
                                 nRetStrLen + ( nReplaceLen - nMatchStrLen ) + 1 );
                  pc = pcRetStr + sPCPos;
               }

               if( nReplaceLen != nMatchStrLen )
                  memmove( pc + nReplaceLen, pc + nMatchStrLen,
                           nRetStrLen - ( ( pc + nMatchStrLen ) - pcRetStr ) );
               if( nReplaceLen > 0 )
                  hb_xmemcpy( pc, pcReplacement, nReplaceLen );

               if( iMultiPass )
                  pcRetSubStr = pc + 1;
               else
                  pcRetSubStr = pc + nReplaceLen;

               nRetStrLen += nReplaceLen - nMatchStrLen;
            }
            else
            {
               if( iMultiPass )
                  pcRetSubStr = pc + 1;
               else
                  pcRetSubStr = pc + nMatchStrLen;
            }
            sRetSubStrLen = nRetStrLen - ( pcRetSubStr - pcRetStr );
         }
      }
      else /* ( nCounter != 0 ) */
      {
         /* find and replace last match */
         nRetStrLen = nStrLen;
         pcRetStr = ( char * ) hb_xgrab( nRetStrLen + 1 );
         hb_xmemcpy( pcRetStr, pcString, nRetStrLen );

         /* we have to find the last match and replace it */
         switch ( iAtLike )
         {
            case CT_SETATLIKE_EXACT:
               pc = ( char * ) ct_at_exact_backward( pcRetStr + nIgnore, nRetStrLen - nIgnore,
                                                     pcStringToMatch, nStrToMatchLen, &nMatchStrLen );
               break;

            case CT_SETATLIKE_WILDCARD:
               pc = ( char * ) ct_at_wildcard_backward( pcRetStr + nIgnore, nRetStrLen - nIgnore,
                                                        pcStringToMatch, nStrToMatchLen,
                                                        cAtLike, &nMatchStrLen );
               break;

            default:
               pc = NULL;
         }

         if( pc == NULL )
         {
            hb_retclen_buffer( pcRetStr, nRetStrLen );
            return;
         }

         /* replace match */
         if( nMatchStrLen < nReplaceLen )
         {
            /* pcRetStr grows, so realloc memory */
            /* save pc pointer */
            HB_SIZE sPCPos = pc - pcRetStr;

            pcRetStr = ( char * ) hb_xrealloc( pcRetStr,
                                 nRetStrLen + ( nReplaceLen - nMatchStrLen ) + 1 );
            pc = pcRetStr + sPCPos;
         }

         if( nReplaceLen != nMatchStrLen )
            memmove( pc + nReplaceLen, pc + nMatchStrLen,
                     nRetStrLen - ( ( pc + nMatchStrLen ) - pcRetStr ) );
         if( nReplaceLen > 0 )
            hb_xmemcpy( pc, pcReplacement, nReplaceLen );

         nRetStrLen += ( nReplaceLen - nMatchStrLen );
      }

      hb_retclen_buffer( pcRetStr, nRetStrLen );
   }
   else /* ( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_ATREPL, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retclen( hb_parc( 2 ), hb_parclen( 2 ) );
   }
}

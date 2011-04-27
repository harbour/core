/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   STRDIFF() CT3 string function
 *
 * Copyright 2002 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
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
#include <limits.h>

#define MATRIXELEMENT( __row, __col ) *( piPenalty + ( ( __row ) * ( sStrLen2 + 1 ) ) + ( __col ) )

static int min3( int a, int b, int c )
{
   if( a < b )
      return a < c ? a : c;

   return b < c ? b : c;
}

HB_FUNC( STRDIFF )
{
   /* param check */
   if( HB_ISCHAR( 1 ) || HB_ISCHAR( 2 ) )
   {
      /* get parameters */
      const char *   pcStr1, * pcStr2;
      HB_SIZE        sStrLen1, sStrLen2;
      int            iReplace, iDelete, iInsert;
      int            iAtLike = ct_getatlike();
      char           cAtLike = ct_getatlikechar();
      int *          piPenalty;
      HB_SIZE        sRowCnt, sColCnt;

      if( HB_ISCHAR( 1 ) )
      {
         pcStr1     = hb_parc( 1 );
         sStrLen1   = hb_parclen( 1 );
      }
      else
      {
         pcStr1     = "";
         sStrLen1   = 0;
      }

      if( HB_ISCHAR( 2 ) )
      {
         pcStr2     = hb_parc( 2 );
         sStrLen2   = hb_parclen( 2 );
      }
      else
      {
         pcStr2     = "";
         sStrLen2   = 0;
      }

      /* check for memory consumption */
      if( ( ( double ) sStrLen1 + 1.0 ) *
          ( ( double ) sStrLen2 + 1.0 ) *
          ( ( double ) sizeof( int ) ) >= ( double ) UINT_MAX )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_STRDIFF, NULL,
                      HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );
         }
         hb_retni( -1 );
         return;
      }

      /* get penalty points */
      if( HB_ISNUM( 3 ) )
         iReplace = hb_parni( 3 );
      else
         iReplace = 3;

      if( HB_ISNUM( 4 ) )
         iDelete = hb_parni( 4 );
      else
         iDelete = 6;

      if( HB_ISNUM( 5 ) )
         iInsert = hb_parni( 5 );
      else
         iInsert = 1;

      piPenalty              = ( int * ) hb_xgrab( ( sStrLen1 + 1 ) *
                                                   ( sStrLen2 + 1 ) * sizeof( int ) );

      MATRIXELEMENT( 0, 0 )  = 0;
      for( sColCnt = 0; sColCnt <= sStrLen2 - 1; sColCnt++ )
      {
         MATRIXELEMENT( 0, sColCnt + 1 ) = MATRIXELEMENT( 0, sColCnt ) + iInsert;
      }

      for( sRowCnt = 0; sRowCnt <= sStrLen1 - 1; sRowCnt++ )
      {
         MATRIXELEMENT( sRowCnt + 1, 0 ) = MATRIXELEMENT( sRowCnt, 0 ) + iDelete;
         for( sColCnt = 0; sColCnt <= sStrLen2 - 1; sColCnt++ )
         {
            int iReplaceCost;

            if( pcStr1[ sRowCnt ] == pcStr2[ sColCnt ] ||
                ( iAtLike == CT_SETATLIKE_WILDCARD &&
                  ( pcStr1[ sRowCnt ] == cAtLike ||
                    pcStr2[ sColCnt ] == cAtLike ) ) )
               iReplaceCost = 0;
            else
               iReplaceCost = iReplace;

            MATRIXELEMENT( sRowCnt + 1, sColCnt + 1 ) =
               min3( MATRIXELEMENT( sRowCnt, sColCnt ) + iReplaceCost,
                     MATRIXELEMENT( sRowCnt, sColCnt + 1 ) + iDelete,
                     MATRIXELEMENT( sRowCnt + 1, sColCnt ) + iInsert );
         }
      }

      hb_retni( MATRIXELEMENT( sStrLen1, sStrLen2 ) );
      hb_xfree( piPenalty );
   }
   else  /* HB_ISCHAR( 1 ) || HB_ISCHAR( 2 ) */
   {
      PHB_ITEM pSubst        = NULL;
      int      iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_STRDIFF, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retni( 0 );
   }
}

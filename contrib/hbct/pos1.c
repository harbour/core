/*
 * Harbour Project source code:
 *   PosAlpha(), PosLower(), PosRange() and PosUpper() CT3 string functions
 *
 * PosUpper() Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *
 * PosAlpha(), PosLower(), PosRange()
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

#include "ct.h"

/* defines */
#define DO_POS1_POSALPHA  0
#define DO_POS1_POSLOWER  1
#define DO_POS1_POSRANGE  2
#define DO_POS1_POSUPPER  3

/* helper function for the posxxx() functions */
static void do_pos1( int iSwitch )
{
   if( HB_ISCHAR( 1 ) &&                  /* all functions need string as 1st param */
       ( iSwitch != DO_POS1_POSRANGE ||   /* that's the only condition for all funcs _except_ POSRANGE */
         ( iSwitch == DO_POS1_POSRANGE && /* In addition, POSRANGE needs .. */
           HB_ISCHAR( 2 ) &&              /* .. string as 2nd .. */
           HB_ISCHAR( 3 ) ) ) )           /* .. and 3rd param */
   {
      const unsigned char * pcString, * puc;
      HB_SIZE sStrLen;
      unsigned char ucChar1 = ' ', ucChar2 = ' ';
      int iMode;
      HB_SIZE sIgnore;
      int iParamShift = 0;

      if( iSwitch == DO_POS1_POSRANGE )
      {
         if( hb_parclen( 1 ) == 0 )
         {
            hb_retns( 0 );
            return;
         }
         else
            ucChar1 = *( hb_parc( 1 ) );

         if( hb_parclen( 2 ) == 0 )
         {
            hb_retns( 0 );
            return;
         }
         else
            ucChar2 = *( hb_parc( 2 ) );

         iParamShift += 2;
      }

      pcString = ( const unsigned char * ) hb_parc( iParamShift + 1 );
      sStrLen = hb_parclen( iParamShift + 1 );

      iMode = hb_parldef( iParamShift + 2, 0 );
      sIgnore = hb_parnsdef( iParamShift + 3, 0 );

      for( puc = pcString + sIgnore; puc < pcString + sStrLen; puc++ )
      {
         int iDoRet = 0;

         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
               iDoRet = hb_charIsAlpha( ( HB_UCHAR ) *puc );
               break;

            case DO_POS1_POSLOWER:
               iDoRet = hb_charIsLower( ( HB_UCHAR ) *puc );
               break;

            case DO_POS1_POSRANGE:
               iDoRet = ( ucChar1 <= *puc && ucChar2 >= *puc );
               break;

            case DO_POS1_POSUPPER:
               iDoRet = hb_charIsUpper( ( HB_UCHAR ) *puc );
               break;
         }

         if( ( iMode && ! iDoRet ) || ( ! iMode && iDoRet ) )
         {
            hb_retns( puc - pcString + 1 );
            return;
         }
      }
      hb_retns( 0 );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      HB_ERRCODE iError = 0;

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         switch( iSwitch )
         {
            case DO_POS1_POSALPHA:
               iError = CT_ERROR_POSALPHA;
               break;

            case DO_POS1_POSLOWER:
               iError = CT_ERROR_POSLOWER;
               break;

            case DO_POS1_POSRANGE:
               iError = CT_ERROR_POSRANGE;
               break;

            case DO_POS1_POSUPPER:
               iError = CT_ERROR_POSUPPER;
               break;
         }
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG, iError,
                                  NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retns( 0 );
   }
}

HB_FUNC( POSALPHA )
{
   do_pos1( DO_POS1_POSALPHA );
}

HB_FUNC( POSLOWER )
{
   do_pos1( DO_POS1_POSLOWER );
}

HB_FUNC( POSRANGE )
{
   do_pos1( DO_POS1_POSRANGE );
}

HB_FUNC( POSUPPER )
{
   do_pos1( DO_POS1_POSUPPER );
}

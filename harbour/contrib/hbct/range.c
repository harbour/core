/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   RANGEREM() and RANGEREPL() CT3 string functions
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

HB_FUNC( RANGEREM )
{
   if( ( hb_parclen( 1 ) > 0 || HB_ISNUM( 1 ) ) &&
       ( hb_parclen( 2 ) > 0 || HB_ISNUM( 2 ) ) && HB_ISCHAR( 3 ) )
   {
      const char * pcString = hb_parc( 3 );
      HB_SIZE sStrLen = hb_parclen( 3 );
      char * pcRet;
      const unsigned char * pc;
      unsigned char ucChar1, ucChar2;
      HB_SIZE sRetIndex;
      int iMode, iBool;

      if( HB_ISCHAR( 1 ) )
         ucChar1 = *( ( const unsigned char * ) hb_parc( 1 ) );
      else
         ucChar1 = ( unsigned char ) ( hb_parni( 1 ) % 256 );

      if( HB_ISCHAR( 2 ) )
         ucChar2 = *( ( const unsigned char * ) hb_parc( 2 ) );
      else
         ucChar2 = ( unsigned char ) ( hb_parni( 2 ) % 256 );

      iMode = ( ucChar2 < ucChar1 );

      pcRet = ( char * ) hb_xgrab( sStrLen + 1 );
      sRetIndex = 0;
      for( pc = ( const unsigned char * ) pcString; pc < ( const unsigned char * ) pcString + sStrLen; pc++ )
      {
         iBool = ( ( *pc ) >= ucChar1 );
         if( iMode )
            iBool |= ( ( *pc ) <= ucChar2 );
         else
            iBool &= ( ( *pc ) <= ucChar2 );

         if( !iBool )
         {
            *( pcRet + sRetIndex ) = *pc;
            sRetIndex++;
         }
      }

      hb_retclen( pcRet, sRetIndex );
      hb_xfree( pcRet );
   }
   else  /* ( hb_parclen( 1 ) > 0 || HB_ISNUM( 1 ) ) &&
            ( hb_parclen( 2 ) > 0 || HB_ISNUM( 2 ) ) && HB_ISCHAR( 3 ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RANGEREM, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( HB_ISCHAR( 3 ) )
         hb_retclen( hb_parc( 3 ), hb_parclen( 3 ) );
      else
         hb_retc_null();
   }
}

HB_FUNC( RANGEREPL )
{
   int iNoRef = ct_getref() && HB_ISBYREF( 3 );

   if( ( hb_parclen( 1 ) > 0 || HB_ISNUM( 1 ) ) &&
       ( hb_parclen( 2 ) > 0 || HB_ISNUM( 2 ) ) &&
       HB_ISCHAR( 3 ) && ( hb_parclen( 4 ) > 0 || HB_ISNUM( 4 ) ) )
   {
      const char * pcString = hb_parc( 3 );
      HB_SIZE sStrLen = hb_parclen( 3 );
      char * pcRet;
      const unsigned char * pc;
      unsigned char ucChar1, ucChar2, ucReplace;
      HB_SIZE sRetIndex;
      int iMode, iBool;

      if( HB_ISCHAR( 1 ) )
         ucChar1 = *( ( const unsigned char * ) hb_parc( 1 ) );
      else
         ucChar1 = ( unsigned char ) ( hb_parni( 1 ) % 256 );

      if( HB_ISCHAR( 2 ) )
         ucChar2 = *( ( const unsigned char * ) hb_parc( 2 ) );
      else
         ucChar2 = ( unsigned char ) ( hb_parni( 2 ) % 256 );

      if( HB_ISCHAR( 4 ) )
         ucReplace = *( ( const unsigned char * ) hb_parc( 4 ) );
      else
         ucReplace = ( unsigned char ) ( hb_parni( 4 ) % 256 );

      iMode = ( ucChar2 < ucChar1 );

      pcRet = ( char * ) hb_xgrab( sStrLen + 1 );
      sRetIndex = 0;
      for( pc = ( const unsigned char * ) pcString; pc < ( const unsigned char * ) pcString + sStrLen; pc++ )
      {
         iBool = ( ( *pc ) >= ucChar1 );
         if( iMode )
            iBool |= ( ( *pc ) <= ucChar2 );
         else
            iBool &= ( ( *pc ) <= ucChar2 );

         if( iBool )
         {
            *( pcRet + sRetIndex ) = ucReplace;
            sRetIndex++;
         }
         else
         {
            *( pcRet + sRetIndex ) = *pc;
            sRetIndex++;
         }
      }

      if( HB_ISBYREF( 3 ) )
         hb_storclen( pcRet, sStrLen, 3 );

      if( iNoRef )
         /* Contrary to the official documentation, RANGREPL() returns NIL instead of .F.
          * in this situation. If the string is not passed by reference, it returns the
          * string regardless of iNoRef. */
         hb_ret();
      else
         hb_retclen( pcRet, sStrLen );

      hb_xfree( pcRet );
   }
   else  /* ( hb_parclen( 1 ) > 0 || HB_ISNUM( 1 ) ) &&
            ( hb_parclen( 2 ) > 0 || HB_ISNUM( 2 ) ) &&
            HB_ISCHAR( 3 ) && ( hb_parclen( 4 ) > 0 || HB_ISNUM( 4 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RANGEREPL, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRef )
         hb_ret();
      else if( HB_ISCHAR( 3 ) )
         hb_retclen( hb_parc( 3 ), hb_parclen( 3 ) );
      else
         hb_retc_null();
   }
}

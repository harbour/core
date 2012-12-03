/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   REPLALL(), REPLLEFT() and REPLRIGHT() CT3 string functions
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

/* defines */
#define DO_REPLACE_REPLALL    0
#define DO_REPLACE_REPLLEFT   1
#define DO_REPLACE_REPLRIGHT  2

static const HB_ERRCODE sulErrorSubcodes[] =
{
   CT_ERROR_REPLALL,
   CT_ERROR_REPLLEFT,
   CT_ERROR_REPLRIGHT
};

/* helper function for the replxxx functions */
static void do_replace( int iSwitch )
{
   /* suppressing return value ? */
   int iNoRet = ct_getref() && HB_ISBYREF( 1 );

   /* param check */
   if( HB_ISCHAR( 1 ) && ( hb_parclen( 2 ) > 0 || HB_ISNUM( 2 ) ) )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE sStrLen = hb_parclen( 1 );
      char * pcRet, * pc;
      char cSearch, cReplace;

      if( sStrLen == 0 )
      {
         if( iNoRet )
            hb_ret();
         else
            hb_retc_null();
         return;
      }

      if( HB_ISNUM( 2 ) )
         cReplace = ( char ) ( hb_parnl( 2 ) % 256 );
      else
         cReplace = *( ( const char * ) hb_parc( 2 ) );

      if( hb_parclen( 3 ) > 0 )
         cSearch = *( ( const char * ) hb_parc( 3 ) );
      else if( HB_ISNUM( 3 ) )
         cSearch = ( char ) ( hb_parnl( 3 ) % 256 );
      else
         cSearch = 0x20;

      pcRet = ( char * ) hb_xgrab( sStrLen + 1 );
      hb_xmemcpy( pcRet, pcString, sStrLen );

      if( iSwitch != DO_REPLACE_REPLRIGHT )
      {
         pc = pcRet;
         while( *pc == cSearch && pc < pcRet + sStrLen )
         {
            *pc = cReplace;
            pc++;
         }
      }

      if( iSwitch != DO_REPLACE_REPLLEFT )
      {
         pc = pcRet + sStrLen - 1;
         while( *pc == cSearch && pc >= pcRet )
         {
            *pc = cReplace;
            pc--;
         }
      }

      hb_storclen( pcRet, sStrLen, 1 );

      if( iNoRet )
      {
         hb_xfree( pcRet );
         hb_ret();
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
                                  sulErrorSubcodes[ iSwitch ],
                                  NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRet )
         hb_ret();
      else
         hb_retc_null();
   }
}

HB_FUNC( REPLALL )
{
   do_replace( DO_REPLACE_REPLALL );
}

HB_FUNC( REPLLEFT )
{
   do_replace( DO_REPLACE_REPLLEFT );
}

HB_FUNC( REPLRIGHT )
{
   do_replace( DO_REPLACE_REPLRIGHT );
}

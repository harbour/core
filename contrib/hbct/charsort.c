/*
 * Harbour Project source code:
 *   CharSort() CT3 string functions
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

#include "hbstack.h"

/* statics */

typedef struct
{
   HB_SIZE sCompareLen;
   HB_SIZE sElementPos;
} CT_CHARSORT, * PCT_CHARSORT;

static HB_TSD_NEW( s_charsort, sizeof( CT_CHARSORT ), NULL, NULL );

/* qsort function */
#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
_hb_do_sortascend( const void * p1, const void * p2 )
{
   PCT_CHARSORT charsort = ( PCT_CHARSORT ) hb_stackGetTSD( &s_charsort );

   return strncmp( ( const char * ) p1 + charsort->sElementPos,
                   ( const char * ) p2 + charsort->sElementPos,
                   charsort->sCompareLen );
}

#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
_hb_do_sortdescend( const void * p1, const void * p2 )
{
   PCT_CHARSORT charsort = ( PCT_CHARSORT ) hb_stackGetTSD( &s_charsort );

   return -strncmp( ( const char * ) p1 + charsort->sElementPos,
                    ( const char * ) p2 + charsort->sElementPos,
                    charsort->sCompareLen );
}

HB_FUNC( CHARSORT )
{
   /* suppressing return value ? */
   int iNoRet = ct_getref() && HB_ISBYREF( 1 );

   /* param check I */
   if( HB_ISCHAR( 1 ) )
   {
      PCT_CHARSORT charsort = ( PCT_CHARSORT ) hb_stackGetTSD( &s_charsort );

      /* get parameters */
      const char * pcString = hb_parc( 1 );

      char *  pcRet;
      HB_SIZE sStrLen     = hb_parclen( 1 );
      HB_SIZE sElementLen = hb_parnsdef( 2, 1 );
      HB_SIZE sIgnore     = hb_parnsdef( 4, 0 );
      HB_SIZE sSortLen    = hb_parnsdef( 6, sStrLen - sIgnore );
      int     iDescend    = hb_parldef( 7, 0 );

      charsort->sCompareLen = hb_parnsdef( 3, sElementLen );
      charsort->sElementPos = hb_parnsdef( 5, 0 );

      /* param check II */
      if( sElementLen == 0 || charsort->sCompareLen > sElementLen ||
          sIgnore + sElementLen > sStrLen ||
          charsort->sElementPos + charsort->sCompareLen > sElementLen ||
          sSortLen + sIgnore > sStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                      NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      HB_ERR_ARGS_BASEPARAMS );

         if( iNoRet )
            hb_retl( HB_FALSE );
         else
            hb_retc_null();
         return;
      }

      pcRet = ( char * ) hb_xgrab( sStrLen + 1 );
      hb_xmemcpy( pcRet, pcString, sStrLen );

      if( iDescend )
         qsort( pcRet + sIgnore, ( sSortLen / sElementLen ), sElementLen, _hb_do_sortdescend );
      else
         qsort( pcRet + sIgnore, ( sSortLen / sElementLen ), sElementLen, _hb_do_sortascend );

      /* return string */
      hb_storclen( pcRet, sStrLen, 1 );

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
      PHB_ITEM pSubst        = NULL;
      int      iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CHARSORT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRet )
         hb_retl( HB_FALSE );
      else
         hb_retc_null();
   }
}

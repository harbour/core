/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CHARSORT() CT3 string functions
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

/* statics */
static HB_SIZE s_sCompareLen;     /* TODO: make this thread safe */
static HB_SIZE s_sElementPos;     /* TODO: make this thread safe */

/* qsort function */
#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
_hb_do_sortascend( const void * p1, const void * p2 )
{
   const char * pc1 = ( const char * ) p1;
   const char * pc2 = ( const char * ) p2;

   pc1 += s_sElementPos;
   pc2 += s_sElementPos;

   return strncmp( pc1, pc2, s_sCompareLen );
}

#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
_hb_do_sortdescend( const void * p1, const void * p2 )
{
   const char * pc1 = ( const char * ) p1;
   const char * pc2 = ( const char * ) p2;

   pc1 += s_sElementPos;
   pc2 += s_sElementPos;

   return -strncmp( pc1, pc2, s_sCompareLen );
}

HB_FUNC( CHARSORT )
{
   int iNoRet;

   /* suppressing return value ? */
   iNoRet = ct_getref() && HB_ISBYREF( 1 );

   /* param check I */
   if( HB_ISCHAR( 1 ) )
   {
      /* get parameters */
      const char * pcString = hb_parc( 1 );
      char * pcRet;
      HB_SIZE sStrLen = hb_parclen( 1 );
      HB_SIZE sElementLen, sIgnore, sSortLen;
      int iDescend;

      if( HB_ISNUM( 2 ) )
         sElementLen = hb_parns( 2 );
      else
         sElementLen = 1;

      if( HB_ISNUM( 3 ) )
         s_sCompareLen = hb_parns( 3 );
      else
         s_sCompareLen = sElementLen;

      if( HB_ISNUM( 4 ) )
         sIgnore = hb_parns( 4 );
      else
         sIgnore = 0;

      if( HB_ISNUM( 5 ) )
         s_sElementPos = hb_parns( 5 );
      else
         s_sElementPos = 0;

      if( HB_ISNUM( 6 ) )
         sSortLen = hb_parns( 6 );
      else
         sSortLen = sStrLen - sIgnore;

      if( HB_ISLOG( 7 ) )
         iDescend = hb_parl( 7 );
      else
         iDescend = 0;

      /* param check II */
      if( sElementLen == 0 || s_sCompareLen > sElementLen ||
          sIgnore + sElementLen > sStrLen ||
          s_sElementPos + s_sCompareLen > sElementLen ||
          sSortLen + sIgnore > sStrLen )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
         {
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                      NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      HB_ERR_ARGS_BASEPARAMS );
         }
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
      if( HB_ISBYREF( 1 ) )
         hb_storclen( pcRet, sStrLen, 1 );

      if( iNoRet )
      {
         hb_retl( HB_FALSE );
         hb_xfree( pcRet );
      }
      else
         hb_retclen_buffer( pcRet, sStrLen );
   }
   else  /* if( HB_ISCHAR( 1 ) ) */
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
      {
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_CHARSORT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
      }

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else if( iNoRet )
         hb_retl( HB_FALSE );
      else
         hb_retc_null();
   }
}

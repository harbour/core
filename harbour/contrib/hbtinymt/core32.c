/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour interface to TinyMT, Pseudo Random Generator
 *
 * Copyright 2012 Andi Jahja <xharbour@telkom.net.id>
 * www - http://www.harbour-project.org http://www.xharbour.org
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

#include "hbapi.h"
#include "hbstack.h"

#include "tinymt32.h"

typedef struct
{
   tinymt32_t tinymt;
} HB_TINYMTDATA, * PHB_TINYMTDATA;

static HB_TSD_NEW( s_tinymtData, sizeof( HB_TINYMTDATA ), NULL, NULL );

/* Syntax: HB_TINYMT32_INIT( <nVector1>, <nVector2>, <nVector3>, [<nSeed>] ) -> <lSuccess> */
HB_FUNC( HB_TINYMT32_INIT )
{
   if( hb_pcount() >= 3 && HB_ISNUM( 1 ) && HB_ISNUM( 3 ) && HB_ISNUM( 3 ) )
   {
      PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

      HB_U32 seed = ( HB_U32 ) hb_parnldef( 4, 1 );

      tinymtData->tinymt.mat1 = hb_parnl( 1 );
      tinymtData->tinymt.mat2 = hb_parnl( 2 );
      tinymtData->tinymt.tmat = hb_parnl( 3 );

      tinymt32_init( &tinymtData->tinymt, seed );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

/* Syntax: HB_TINYMT32_INIT_BY_ARRAY( { <nVector1>, <nVector2>, <nVector3> }, [<nSeed>], [<nKeyLength>] ) -> <lSuccess> */
HB_FUNC( HB_TINYMT32_INIT_BY_ARRAY )
{
   PHB_ITEM pArray = hb_param( 1, HB_IT_ARRAY );

   if( pArray && hb_arrayLen( pArray ) == 3 )
   {
      PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

      HB_U32 seed_array[ 5 ];

      seed_array[ 0 ] = ( HB_U32 ) hb_parnldef( 2, 1 );

      tinymtData->tinymt.mat1 = hb_arrayGetNL( pArray, 1 );
      tinymtData->tinymt.mat2 = hb_arrayGetNL( pArray, 2 );
      tinymtData->tinymt.tmat = hb_arrayGetNL( pArray, 3 );

      tinymt32_init_by_array( &tinymtData->tinymt, seed_array, hb_parnldef( 3, 1 ) );

      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( HB_TINYMT32_GENERATE_UINT32 )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnint( tinymt32_generate_uint32( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOAT )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_float( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOAT01 )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_float01( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOAT12 )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_float12( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOATOC )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_floatOC( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_FLOATOO )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_floatOO( &tinymtData->tinymt ) );
}

HB_FUNC( HB_TINYMT32_GENERATE_32DOUBLE )
{
   PHB_TINYMTDATA tinymtData = ( PHB_TINYMTDATA ) hb_stackGetTSD( &s_tinymtData );

   hb_retnd( tinymt32_generate_32double( &tinymtData->tinymt ) );
}

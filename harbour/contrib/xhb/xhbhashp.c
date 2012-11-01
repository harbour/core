/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * xhb compatibility functions HSETPARTITION()/HGETPARTITION()
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * Copyright 2003 Giancarlo Niccolai
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

#include "hbapiitm.h"
#include "hbapierr.h"

/* Dummy compatibility functions mimicing RTE behavior only.
   The rest is not needed in Harbour */

HB_FUNC( HSETPARTITION )
{
   PHB_ITEM pHash   = hb_param( 1, HB_IT_HASH );
   PHB_ITEM pLevel  = hb_param( 3, HB_IT_NUMERIC );
   HB_UINT  uiLevel = pLevel ? hb_itemGetNI( pLevel ) : 1;

   if( ! pHash )
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   else if( hb_hashLen( pHash ) > 0 )
      hb_errRT_BASE( EG_ARG, 2017, "Can't change partitioning in a non-empty hash", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   else if( ( hb_hashGetFlags( pHash ) & HB_HASH_KEEPORDER ) != 0 )
      hb_errRT_BASE( EG_ARG, 2017, "Can't set partitioning in a hash with associative array compatibility", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   else if( uiLevel < 1 || uiLevel > 8 )
      hb_errRT_BASE( EG_ARG, 2017, "Pagination level must be between 1 and 8", HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HGETPARTITION )
{
   if( hb_param( 1, HB_IT_HASH ) )
      hb_retl( HB_FALSE );
   else
      hb_errRT_BASE( EG_ARG, 2017, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

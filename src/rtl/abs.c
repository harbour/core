/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Abs() function
 *
 * Copyright 1999 Matthew Hamilton <mhamilton@bunge.com.au>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

HB_FUNC( ABS )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );

   if( pNumber )
   {
      int iWidth;
      int iDec;

      hb_itemGetNLen( pNumber, &iWidth, &iDec );

      if( HB_IS_INTEGER( pNumber ) )
      {
         int iNumber = hb_itemGetNI( pNumber );

         if( iNumber >= 0 )
            hb_retnilen( iNumber, iWidth );
#if -HB_VMINT_MAX > HB_VMINT_MIN
         else if( iNumber < -INT_MAX )
#if HB_VMLONG_MAX > HB_VMINT_MAX
            hb_retnint( -( HB_MAXINT ) iNumber );
#else
            hb_retndlen( -( double ) iNumber, 0, iDec );
#endif
#endif
         else
            hb_retni( -iNumber );
      }
      else if( HB_IS_LONG( pNumber ) )
      {
         HB_MAXINT lNumber = hb_itemGetNInt( pNumber );

         if( lNumber >= 0 )
            hb_retnintlen( lNumber, iWidth );
#if -HB_VMLONG_MAX > HB_VMLONG_MIN
         else if( lNumber < -HB_VMLONG_MAX )
            hb_retndlen( -( double ) lNumber, 0, iDec );
#endif
         else
            hb_retnint( -lNumber );
      }
      else
      {
         double dNumber = hb_itemGetND( pNumber );

         hb_retndlen( dNumber >= 0.0 ? dNumber : -dNumber, 0, iDec );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1089, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MOD() function
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
#include "hbmath.h"

/* NOTE: In CA-Cl*pper this is written in Clipper, see the source below,
         and the error handling is NOT made here, but in the % operator.
         [vszakats] */

/* NOTE: CA-Cl*pper is buggy since it relies on the fact that the errorhandler
         will silently handle zero division errors. [vszakats] */

/* NOTE: This C version fully emulates the behaviour of the original
         CA-Cl*pper version, including bugs/side-effects. [vszakats] */

HB_FUNC( MOD )
{
   PHB_ITEM pNumber = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pBase   = hb_param( 2, HB_IT_NUMERIC );

   if( pNumber && pBase )
   {
      double dNumber = hb_itemGetND( pNumber );
      double dBase   = hb_itemGetND( pBase ); /* dBase! Cool! */

      if( dBase )
      {
         double dResult = fmod( dNumber, dBase );

         if( dResult && ( dNumber > 0 ? dBase < 0 : dBase > 0 ) )
            dResult += dBase;
         hb_retnd( dResult );
      }
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", HB_ERR_ARGS_BASEPARAMS );

         /* In CA-Cl*pper MOD() function ignores substitution result
          * and return original numeric item keeping its internal
          * representation: integer or double, size and number of
          * decimal places, it can be seen in code like:
          *    PROCEDURE Main()
          *       SET FIXED ON
          *       ? Transform( Mod( 12345, 0 ),"" )
          *       RETURN
          *
          * [druzus]
          */
         if( pResult )
         {
            hb_itemReturn( pNumber );
            hb_itemRelease( pResult );
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1085, NULL, "%", 2, hb_param( 1, HB_IT_ANY ), hb_param( 2, HB_IT_ANY ) );
}

/*
   FUNCTION Mod( cl_num, cl_base )
      LOCAL cl_result := cl_num % cl_base

      RETURN iif( cl_base = 0, cl_num, iif( cl_result * cl_base < 0, cl_result + cl_base, cl_result ) )
 */

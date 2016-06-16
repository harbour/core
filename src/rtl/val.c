/*
 * Val() function
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* returns the numeric value of a character string representation of a number  */
static void hb_val( HB_BOOL fExt )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      const char * szText = hb_itemGetCPtr( pText );
      int iWidth, iDec, iLen = ( int ) hb_itemGetCLen( pText );
      HB_BOOL fDbl;
      HB_MAXINT lValue;
      double dValue;

      fDbl = hb_valStrnToNum( szText, iLen, &lValue, &dValue, &iDec, &iWidth );

      if( fExt )
      {
         if( HB_ISNUM( 2 ) )
            iLen = hb_parni( 2 );

         if( fDbl && iDec > 0 )
            iLen -= iDec + 1;

         if( iLen > iWidth )
            iWidth = iLen;
         else if( iLen > 0 )
         {
            while( iWidth > iLen && *szText == ' ' )
            {
               iWidth--;
               szText++;
            }
         }
      }

      if( fDbl )
         hb_retndlen( dValue, iWidth, iDec );
      else
         hb_retnintlen( lValue, iWidth );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1098, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( VAL )
{
   hb_val( HB_FALSE );
}

HB_FUNC( HB_VAL )
{
   hb_val( HB_TRUE );
}

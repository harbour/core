/*
 * xHarbour Project source code:
 * StrDel() function
 *
 * Copyright 2003 Walter Negro <anegro@overnet.com.ar>
 * www - http://www.xharbour.org
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
 * As a special exception, the xHarbour Project gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the xHarbour
 * Project under the name xHarbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * xHarbour, as the General Public License permits, the exception does
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

/* replaces characters in a string */
HB_FUNC( STRDEL )
{
   if( HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      const char * szText = hb_parcx( 1 );
      HB_SIZE      nText  = hb_parclen( 1 );
      HB_SIZE      nDel   = hb_parclen( 2 );

      if( nDel > 0 && nText > 0 )
      {
         const char * szDel    = hb_parcx( 2 );
         HB_SIZE      nPosTxt  = 0;
         HB_SIZE      nResult  = 0;
         HB_SIZE      nPosDel  = 0;
         char *       szResult = ( char * ) hb_xgrab( nText + 1 );

         for(; ( nPosDel < nText && nPosDel < nDel ); nPosDel++ )
         {
            if( szDel[ nPosDel ] != ' ' )
            {
               hb_xmemcpy( szResult + nResult, szText + nPosTxt, nPosDel - nPosTxt );
               nResult += nPosDel - nPosTxt;
               nPosTxt  = nPosDel + 1;
            }
         }
         hb_xmemcpy( szResult + nResult, szText + nPosTxt, nText - nPosTxt );
         nResult += nText - nPosTxt;

         szResult[ nResult ] = '\0';
         hb_retclen_buffer( szResult, nResult );
      }
      else
         hb_retc( szText );
   }
   else
      hb_retc_null();
}

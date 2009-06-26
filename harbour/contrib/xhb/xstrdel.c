/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 * STRDEL() function
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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
      ULONG ulText = hb_parclen( 1 );
      ULONG ulDel = hb_parclen( 2 );

      if( ulDel > 0 && ulText > 0 )
      {
         const char * szDel = hb_parcx( 2 );
         ULONG ulPosTxt = 0;
         ULONG ulResult = 0;
         ULONG ulPosDel = 0;
         char * szResult = ( char * ) hb_xgrab( ulText + 1 );

         for( ; ( ulPosDel < ulText && ulPosDel < ulDel ); ulPosDel++ )
         {
            if( szDel[ ulPosDel ] != ' ' )
            {
               hb_xmemcpy( szResult + ulResult, szText + ulPosTxt, ulPosDel - ulPosTxt );
               ulResult += ulPosDel - ulPosTxt;
               ulPosTxt = ulPosDel + 1;
            }
         }
         hb_xmemcpy( szResult + ulResult, szText + ulPosTxt, ulText - ulPosTxt );
         ulResult += ulText - ulPosTxt;

         szResult[ ulResult ] = '\0';
         hb_retclenAdopt( szResult, ulResult );
      }
      else
         hb_retc( szText );
   }
   else
      hb_retc( NULL );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * RAT() function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbapi.h"

HB_FUNC( RAT )
{
   HB_SIZE nSubLen = hb_parclen( 1 );

   if( nSubLen )
   {
      HB_ISIZ nPos = hb_parclen( 2 ) - nSubLen;

      if( nPos >= 0 )
      {
         const char * pszSub = hb_parc( 1 );
         const char * pszText = hb_parc( 2 );
         HB_BOOL bFound = HB_FALSE;

         while( nPos >= 0 && !bFound )
         {
            if( *( pszText + nPos ) == *pszSub )
               bFound = ( memcmp( pszSub, pszText + nPos, nSubLen ) == 0 );
            nPos--;
         }

         hb_retns( bFound ? nPos + 2 : 0 );
      }
      else
         hb_retns( 0 );
   }
   else
      /* This function never seems to raise an error */
      hb_retns( 0 );
}

HB_FUNC( HB_RAT )
{
   HB_SIZE nSubLen = hb_parclen( 1 );

   if( nSubLen )
   {
      HB_ISIZ nPos = hb_parclen( 2 ) - nSubLen;

      if( nPos >= 0 )
      {
         const char * pszSub = hb_parc( 1 );
         const char * pszText = hb_parc( 2 );
         HB_BOOL bFound = HB_FALSE;
         HB_ISIZ nStart;

         if( HB_ISNUM( 3 ) )
         {
            nStart = hb_parns( 3 );
            if( nStart >= 1 )
               --nStart;
            else
               nStart = 0;
         }
         else
            nStart = 0;

         if( HB_ISNUM( 4 ) )
         {
            HB_ISIZ nEnd = hb_parns( 4 ) - 1;

            if( nEnd < nPos )
               nPos = nEnd;
         }

         while( nPos >= nStart && !bFound )
         {
            if( *( pszText + nPos ) == *pszSub )
               bFound = ( memcmp( pszSub, pszText + nPos, nSubLen ) == 0 );
            nPos--;
         }

         hb_retns( bFound ? nPos + 2 : 0 );
      }
      else
         hb_retns( 0 );
   }
   else
      /* This function never seems to raise an error */
      hb_retns( 0 );
}

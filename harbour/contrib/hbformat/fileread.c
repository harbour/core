/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour source code formatter
 *
 * Copyright 2009 Alexander S.Kresin <alex@belacy.belgorod.su>
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
#include "hbapiitm.h"

HB_FUNC( __HBFORMAT_FILEREAD ) /* ( cText, @cEol ) */
{
   const char *   szText = hb_parcx( 1 );
   const char *   ptr;
   const char *   ptr1;
   HB_SIZE        n, nLines = 0;
   PHB_ITEM       arr;

   ptr = szText;
   while( *ptr )
   {
      if( *ptr == '\r' || *ptr == '\n' )
      {
         if( ! nLines )
            hb_storclen( ptr, ( *( ptr + 1 ) == '\r' || *( ptr + 1 ) == '\n' ) ? 2 : 1, 2 );

         if( *( ptr + 1 ) == '\r' || *( ptr + 1 ) == '\n' )
            ptr++;

         nLines++;
      }
      else if( ( unsigned int ) ( *ptr ) < 9 )
      {
         hb_ret();
         return;
      }
      ptr++;
   }

   ptr--;

   if( *ptr != '\r' && *ptr != '\n' )
      nLines++;

   arr = hb_itemArrayNew( nLines );

   ptr = ptr1 = szText;
   n = 1;
   while( *ptr )
   {
      if( *ptr == '\r' || *ptr == '\n' )
      {
         hb_arraySetCL( arr, n, ptr1, ptr - ptr1 );

         if( *( ptr + 1 ) == '\r' || *( ptr + 1 ) == '\n' )
            ptr++;

         ptr1 = ptr + 1;
         n++;
      }
      ptr++;
   }

   if( n == nLines )
   {
      if( *( ptr - 1 ) == 0x1A )
         ptr--;

      hb_arraySetCL( arr, n, ptr1, ptr - ptr1 );
   }

   hb_itemReturnRelease( arr );
}

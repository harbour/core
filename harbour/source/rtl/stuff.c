/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * STUFF() function
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

/* replaces characters in a string */
HB_FUNC( STUFF )
{
   if( ISCHAR( 1 ) && ISNUM( 2 ) && ISNUM( 3 ) && ISCHAR( 4 ) )
   {
      char * szText = hb_parc( 1 );
      ULONG ulText = hb_parclen( 1 );
      ULONG ulPos = hb_parnl( 2 );
      ULONG ulDel = hb_parnl( 3 );
      ULONG ulInsert = hb_parclen( 4 );

      ULONG ulTotalLen;

      if( ulPos > 0 )
         ulPos--;

      if( ulPos > ulText )
         ulPos = ulText;

      if( ulDel > ulText - ulPos )
         ulDel = ulText - ulPos;

      if( ( ulTotalLen = ulText + ulInsert - ulDel ) > 0 )
      {
         char * szResult = ( char * ) hb_xgrab( ulTotalLen + 1 );

         hb_xmemcpy( szResult, szText, ulPos );
         hb_xmemcpy( szResult + ulPos, hb_parc( 4 ), ulInsert );
         hb_xmemcpy( szResult + ulPos + ulInsert, szText + ulPos + ulDel, ulText - ( ulPos + ulDel ) );

         szResult[ ulTotalLen ] = '\0';
         hb_retclen( szResult, ulTotalLen );
         hb_xfree( szResult );
      }
      else
         hb_retc( "" );
   }
   else
      hb_retc( "" );
}


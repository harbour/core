/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MEMOTRAN() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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
#include "hbapiitm.h"

/* NOTE: pszResult must have an allocated buffer of at least ulStringLen */

char * hb_strMemotran( char * pszResult, ULONG * ulResultLen, const char * pszString, ULONG ulStringLen, char cHardcr, char cSoftcr )
{
   ULONG ulStringPos = 0;
   ULONG ulResultPos = 0;

   HB_TRACE(HB_TR_DEBUG, ("hb_strMemotran(%s, %p, %s, %lu, %x, %x)", pszResult, ulResultLen, pszString, ulStringLen, cHardcr, cSoftcr));

   while( ulStringPos < ulStringLen )
   {
      if(      pszString[ ulStringPos ]     == HB_CHAR_HARD1 &&
               pszString[ ulStringPos + 1 ] == HB_CHAR_HARD2 )
      {
         pszResult[ ulResultPos++ ] = cHardcr;
         ulStringPos += 2;
      }
      else if( pszString[ ulStringPos ]     == HB_CHAR_SOFT1 &&
               pszString[ ulStringPos + 1 ] == HB_CHAR_SOFT2 )
      {
         pszResult[ ulResultPos++ ] = cSoftcr;
         ulStringPos += 2;
      }
      else
         pszResult[ ulResultPos++ ] = pszString[ ulStringPos++ ];
   }

   pszResult[ ulResultPos ] = '\0';

   *ulResultLen = ulResultPos;

   return pszResult;
}

HB_FUNC( MEMOTRAN )
{
   PHB_ITEM pString = hb_param( 1, HB_IT_STRING );

   if( pString )
   {
      char * pszResult = ( char * ) hb_xgrab( hb_itemGetCLen( pString ) + 1 );
      char cHardcr = ISCHAR( 2 ) ? *hb_parc( 2 ) : ';';
      char cSoftcr = ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ';
      ULONG ulResultLen;

      hb_strMemotran( pszResult, &ulResultLen, hb_itemGetCPtr( pString ), hb_itemGetCLen( pString ), cHardcr, cSoftcr );
      hb_retclen( pszResult, ulResultLen );

      hb_xfree( pszResult );
   }
   else
      hb_retc( "" );
}


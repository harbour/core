/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CT_CHARMIX() CA-Tools compatible function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "hbapi.h"
#include "hbapiitm.h"

/* NOTE: CA-Tools will hang if the second parameter is an empty string */

HB_FUNC( CT_CHARMIX )
{
   PHB_ITEM pStr1 = hb_param( 1, IT_STRING );
   PHB_ITEM pStr2 = hb_param( 2, IT_STRING );
   ULONG ulLen2;

   if( pStr1 && pStr2 && ( ulLen2 = hb_itemGetCLen( pStr2 ) ) > 0 )
   {
      ULONG ulLen1 = hb_itemGetCLen( pStr1 );
      char * pszStr1 = hb_itemGetCPtr( pStr1 );
      char * pszStr2 = hb_itemGetCPtr( pStr2 );
      char * pszResult = ( char * ) hb_xgrab( 2 * ulLen1 );
      ULONG ulPos1 = 0;
      ULONG ulPos2 = 0;
      ULONG ulPosResult = 0;

      while( ulPos1 < ulLen1 )
      {
         pszResult[ ulPosResult++ ] = pszStr1[ ulPos1++ ];
         pszResult[ ulPosResult++ ] = pszStr2[ ulPos2++ ];

         if( ulPos2 == ulLen2 )
            ulPos2 = 0;
      }

      hb_retclen( pszResult, 2 * ulLen1 );
      hb_xfree( pszResult );
   }
   else
      hb_retc( "" );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OEM <-> ANSI string conversion functions (Win32 specific, XBase++ ext.)
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

/* NOTE: These are Win32 specific, for other platforms it will return the
         passed parameter unchanged. */

/* NOTE: The following #include "hbwinapi.h" must
         be ahead of any other #include statements! */
#include "hbwinapi.h"

#include "hbapi.h"
#include "hbapiitm.h"

HARBOUR HB_HB_ANSITOOEM( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );

   if( pString )
#if defined(_Windows) || defined(WINNT)
   {
      DWORD ulLen = hb_itemGetCLen( pString );
      char * pszDst = ( char * ) hb_xgrab( ulLen + 1 );

      CharToOemBuff( ( LPCSTR ) hb_itemGetCPtr( pString ), ( LPSTR ) pszDst, ulLen );

      hb_retclen( pszDst, ulLen );
      hb_xfree( pszDst );
   }
#else
      hb_itemReturn( pString );
#endif
   else
      hb_retc( "" );
}
           
HARBOUR HB_HB_OEMTOANSI( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );

   if( pString )
#if defined(_Windows) || defined(WINNT)
   {
      DWORD ulLen = hb_itemGetCLen( pString );
      char * pszDst = ( char * ) hb_xgrab( ulLen + 1 );

      OemToCharBuff( ( LPCSTR ) hb_itemGetCPtr( pString ), ( LPSTR ) pszDst, ulLen );

      hb_retclen( pszDst, ulLen );
      hb_xfree( pszDst );
   }
#else
      hb_itemReturn( pString );
#endif
   else
      hb_retc( "" );
}

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */

HARBOUR HB_CONVTOOEMCP( void )
{
   HB_HB_ANSITOOEM();
}

/* NOTE: XBase++ compatible function */

HARBOUR HB_CONVTOANSICP( void )
{
   HB_HB_OEMTOANSI();
}

#endif

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Uppercase/lowercase string functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include <ctype.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* converts szText to lower case. Does not create a new string! */
char * hb_strLower( char * szText, ULONG ulLen )
{
   ULONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_strLower(%s, %lu)", szText, ulLen));

   for( i = 0; i < ulLen; i++ )
      szText[ i ] = tolower( szText[ i ] );

   return szText;
}

/* converts szText to upper case. Does not create a new string! */
char * hb_strUpper( char * szText, ULONG ulLen )
{
   ULONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_strUpper(%s, %lu)", szText, ulLen));

   for( i = 0; i < ulLen; i++ )
      szText[ i ] = toupper( szText[ i ] );

   return szText;
}

/* This function copies and converts szText to upper case.
 */
char * hb_strncpyUpper( char * pDest, const char * pSource, ULONG ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strncpyUpper(%p, %s, %lu)", pDest, pSource, ulLen));

   pDest[ ulLen ] ='\0';
   while( ulLen-- )
   {
      /* some compilers impliment toupper as a macro, and this has side effects! */
      /* *pDest++ = toupper( *pSource++ ); */
      pDest[ulLen] = toupper( pSource[ulLen] );
   }

   return pDest;
}

/* converts string to lower case */
HB_FUNC( LOWER )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * pszBuffer = hb_itemGetC( pText );
      ULONG ulLen = hb_itemGetCLen( pText );

      hb_retclen( hb_strLower( pszBuffer, ulLen ), ulLen );

      hb_itemFreeC( pszBuffer );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1103, NULL, "LOWER" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* converts string to upper case */
HB_FUNC( UPPER )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * pszBuffer = hb_itemGetC( pText );
      ULONG ulLen = hb_itemGetCLen( pText );

      hb_retclen( hb_strUpper( pszBuffer, ulLen ), ulLen );

      hb_itemFreeC( pszBuffer );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1102, NULL, "UPPER" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

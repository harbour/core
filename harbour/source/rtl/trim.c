/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * *TRIM() functions
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* trims from the left, and returns a new pointer to szText */
/* also returns the new length in lLen */
char * hb_strLTrim( const char * szText, ULONG * ulLen )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strLTrim(%s, %p)", szText, ulLen));

   while( *ulLen && HB_ISSPACE( *szText ) )
   {
      szText++;
      ( *ulLen )--;
   }

   return ( char * ) szText;
}

/* returns szText and the new length in lLen */
ULONG hb_strRTrimLen( const char * szText, ULONG ulLen, BOOL bAnySpace )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_strRTrimLen(%s, %lu. %d)", szText, ulLen, (int) bAnySpace));

   if( bAnySpace )
   {
      while( ulLen && HB_ISSPACE( szText[ ulLen - 1 ] ) )
         ulLen--;
   }
   else
   {
      while( ulLen && szText[ ulLen - 1 ] == ' ' )
         ulLen--;
   }

   return ulLen;
}

/* trims leading spaces from a string */

HB_FUNC( LTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      ULONG ulLen = hb_itemGetCLen( pText );
      char * szText = hb_strLTrim( hb_itemGetCPtr( pText ), &ulLen );

      hb_retclen( szText, ulLen );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1101, NULL, "LTRIM" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* trims trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( RTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * pszText = hb_itemGetCPtr( pText );

      hb_retclen( pszText, hb_strRTrimLen( pszText, hb_itemGetCLen( pText ),
         ISLOG( 2 ) ? hb_parl( 2 ) : FALSE ) );
   }
   else
   {
      /* NOTE: "TRIM" is right here [vszakats] */
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1100, NULL, "TRIM" );

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* synonymn for RTRIM */
HB_FUNC( TRIM )
{
   HB_FUNCNAME( RTRIM )();
}

/* trims leading and trailing spaces from a string */

/* NOTE: The second parameter is a Harbour extension. */

HB_FUNC( ALLTRIM )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * pszText = hb_itemGetCPtr( pText );
      ULONG ulLen = hb_strRTrimLen( pszText, hb_itemGetCLen( pText ),
         ISLOG( 2 ) ? hb_parl( 2 ) : FALSE );

      pszText = hb_strLTrim( pszText, &ulLen );
      hb_retclen( pszText, ulLen );
   }
   else
#ifdef HB_COMPAT_C53
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 2022, NULL, "ALLTRIM" ); /* NOTE: This appeared in CA-Cl*pper 5.3 [vszakats] */

      if( pResult )
      {
         hb_itemReturn( pResult );
         hb_itemRelease( pResult );
      }
   }
#else
      hb_retc( "" );
#endif
}


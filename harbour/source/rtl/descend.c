/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DESCEND() function
 *
 * Copyright 1999 Jose Lalin <dezac@corevia.com>
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

/*  $DOC$
 *  $FUNCNAME$
 *      DESCEND
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Inverts an expression of string, logical, date or numeric type.
 *  $SYNTAX$
 *      DESCEND( <xExp> ) --> <xExpInverted>
 *  $ARGUMENTS$
 *      <xExp> is any valid expression.
 *  $RETURNS$
 *      Inverted value of the same type as passed.
 *  $DESCRIPTION$
 *      This function converts an expression in his inverted form. It is
 *      useful to build descending indexes.
 *  $EXAMPLES$
 *      // Seek for Smith in a descending index
 *      SEEK DESCEND( "SMITH" )
 *  $TESTS$
 *      DATA->( DBSEEK( DESCEND( "SMITH" ) ) )
 *      will seek "SMITH" into a descending index.
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      DESCEND is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      INDEX, SEEK
 *  $END$
 */

#include "extend.h"
#include "itemapi.h"

void hb_strDescend( char * szStringTo, const char * szStringFrom, ULONG ulLen )
{
   HB_TRACE(("hb_strDescend(%s, %s, %lu)", szStringTo, szStringFrom, ulLen));

   if( ulLen == 1 && szStringFrom[ 0 ] == '\0' )
      szStringTo[ 0 ] = '\0';
   else
   {
      for(; ulLen--; ++szStringTo, ++szStringFrom )
         *szStringTo = 256 - *szStringFrom;
   }
}

HARBOUR HB_DESCEND( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   if( pItem )
   {
      if( IS_STRING( pItem ) )
      {
         ULONG ulLen = hb_itemGetCLen( pItem );
         char * szBuffer = ( char * ) hb_xgrab( ulLen );
         hb_strDescend( szBuffer, hb_itemGetCPtr( pItem ), ulLen );
         hb_retclen( szBuffer, ulLen );
         hb_xfree( szBuffer );
      }
      else if( IS_DATE( pItem ) )
         hb_retnl( 5231808 - hb_itemGetNL( pItem ) );
      else if( IS_NUMERIC( pItem ) )
         hb_retnd( -1 * hb_itemGetND( pItem ) );
      else if( IS_LOGICAL( pItem ) )
         hb_retl( ! hb_itemGetL( pItem ) );
   }
}

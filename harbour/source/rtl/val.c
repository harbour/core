/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * VAL() function
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

#include <math.h>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"

/* returns the numeric value of a character string representation of a number */
double hb_strVal( const char * szText, ULONG ulLen )
{
   double dValue = 0.0;
   ULONG ulPos;
   ULONG ulDecPos = 0;
   BOOL bNegative = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_strVal(%s, %d)", szText, ulLen));

   /* Look for sign */

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
   {
      if( szText[ ulPos ] == '-' )
      {
         bNegative = TRUE;
         ulPos++;
         break;
      }
      else if( szText[ ulPos ] == '+' )
      {
         ulPos++;
         break;
      }
      else if( ! HB_ISSPACE( szText[ ulPos ] ) )
         break;
   }

   /* Build the number */

   for(; ulPos < ulLen; ulPos++ )
   {
      if( szText[ ulPos ] == '.' && ulDecPos == 0 )
      {
         ulDecPos++;
      }
      else if( szText[ ulPos ] >= '0' && szText[ ulPos ] <= '9' )
      {
         if( ulDecPos )
            dValue += ( ( double ) ( szText[ ulPos ] - '0' ) ) / pow( 10.0, ( double ) ulDecPos++ );
         else
            dValue = ( dValue * 10 ) + ( ( double ) ( szText[ ulPos ] - '0' ) );
      }
      else
         break;
   }

   return bNegative && dValue != 0.0 ? -dValue : dValue;
}

/* returns the numeric value of a character string representation of a number  */
HB_FUNC( VAL )
{
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );

   if( pText )
   {
      char * szText = hb_itemGetCPtr( pText );
      int iWidth = ( int ) hb_itemGetCLen( pText );
      int iDec;
      double dValue = hb_strVal( szText, hb_itemGetCLen( pText ) );

      for( iDec = 0; iDec < iWidth && szText[ iDec ] != '.'; iDec++ );

      if( iDec >= iWidth - 1 )
         hb_retnlen( dValue, iWidth, 0 );
      else
      {
         /* NOTE: Kludge Warning! This condition:
                  "|| ( iDec == 1 && szText[ 0 ] == '-' && dValue != 0.0 )" 
                  may not be the generic way to handle the width of this "-.1" 
                  string. I could not find a matching case which
                  fails for the same reason, nor a better way to handle it.
                  The problem is that in this case only, the width is 
                  calculated upon conditions which can only be discovered by 
                  parsing the string, but the parsing is made by a lower level
                  generic function. [vszakats] */

         hb_retnlen( dValue, iDec + ( iDec == 0 || ( iDec == 1 && szText[ 0 ] == '-' && dValue != 0.0 ) ? 1 : 0 ), iWidth - iDec - 1 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 1098, NULL, "VAL" );
}

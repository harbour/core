/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MLCOUNT() function
 *
 * Copyright 1999 Ignacio Ortiz de Z£niga <ignacio@fivetech.com>
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

HB_FUNC( MLCOUNT )
{
   char * pszString    = ISCHAR( 1 ) ? hb_parc( 1 ) : "";
   ULONG  ulLineLength = ISNUM( 2 ) ? hb_parni( 2 ) : 79;
   ULONG  ulTabLength  = ISNUM( 3 ) ? hb_parni( 3 ) : 4;
   ULONG  ulLastSpace  = 0;
   ULONG  ulCurLength  = 0;
   BOOL   bWordWrap    = ISLOG( 4 ) ? hb_parl( 4 ) : TRUE;
   ULONG  ulLen        = hb_parclen( 1 );
   ULONG  ulLines      = 0;
   ULONG  ulPos;

   if( ulLineLength < 4 || ulLineLength > 254 )
      ulLineLength = 79;

   if( ulTabLength > ulLineLength )
      ulTabLength = ulLineLength - 1;

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
   {
      switch( pszString[ ulPos ] )
      {
         case HB_CHAR_HT:
            ulCurLength = ( ( ULONG ) ( ulCurLength / ulTabLength ) * ulTabLength ) + ulTabLength;
            break;

         case HB_CHAR_LF:
            ulCurLength = 0;
            ulLastSpace = 0;
            ulLines++;
            break;

         case HB_CHAR_CR:
            break;

         case ' ':
            ulCurLength++;
            ulLastSpace = ulCurLength;
            break;

         default:
            ulCurLength++;
      }

      if( ulCurLength > ulLineLength )
      {
         if( bWordWrap )
         {
            if( ulLastSpace == 0 )
               ulCurLength = 1;
            else
               ulCurLength = ulCurLength - ulLastSpace;
         }
         else
            ulCurLength = 1;

         ulLines++;
         ulLastSpace = 0;
      }
   }

   if( ulCurLength > 0 )
      ulLines++;

   hb_retnl( ulLines );
}

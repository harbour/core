/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MLCOUNT() function
 *
 * Copyright 1999 Ignacio Ortiz de Zúniga <ignacio@fivetech.com>
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

#include "extend.h"

HARBOUR HB_MLCOUNT( void )
{
   char * pString     = hb_parc( 1 );
   BYTE   bLineLength = ISNUM( 2 ) ? hb_parni( 2 ) : 79;
   BYTE   bTabLength  = ISNUM( 3 ) ? hb_parni( 3 ) : 4;
   BYTE   bLastSpace  = 0;
   BYTE   bCurLength  = 0;
   BOOL   bWordWrap   = ISLOG( 4 ) ? hb_parl( 4 ) : TRUE;
   ULONG  ulLen       = hb_parclen( 1 );
   ULONG  ulLines     = 0;
   ULONG  ulPos;

   if( bLineLength < 4 || bLineLength > 254 ) 
      bLineLength = 79;

   if( bTabLength > bLineLength ) 
      bTabLength = bLineLength - 1;

   for( ulPos = 0; ulPos < ulLen; ulPos++ )
   {
      switch( pString[ ulPos ] ) 
      {
         case HB_CHAR_HT:
            bCurLength += bTabLength;
            break;

         case HB_CHAR_LF:
            bCurLength = 0;
            bLastSpace = 0;
            ulLines++;
            break;

         case HB_CHAR_CR:
            break;

         case ' ':
            bCurLength++;
            bLastSpace = bCurLength;
            break;

         default:
            bCurLength++;
      }

      if( bCurLength > bLineLength ) 
      {
         if( bWordWrap ) 
         {
            if( bLastSpace == 0 ) 
               bCurLength = 1;
            else
               bCurLength = bCurLength - bLastSpace;
         }
         else
            bCurLength = 1;

         ulLines++;
         bLastSpace = 0;
      }
   }

   if( bCurLength > 0 ) 
      ulLines++;

   hb_retnl( ulLines );
}

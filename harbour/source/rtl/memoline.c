/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * MEMOLINE() function
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

HARBOUR HB_MEMOLINE( void )
{
   char * pszString    = hb_parc( 1 );
   USHORT uiLineLength = ISNUM( 2 ) ? hb_parni( 2 ) : 79;
   ULONG  ulLineNumber = ISNUM( 3 ) ? hb_parni( 3 ) : 1;
   USHORT uiTabLength  = ISNUM( 4 ) ? hb_parni( 4 ) : 4;
   ULONG  uiLastSpace  = 0;
   ULONG  uiCurLength  = 0;
   BOOL   bWordWrap    = ISLOG( 5 ) ? hb_parl( 5 ) : TRUE;
   ULONG  ulLen        = hb_parclen( 1 );
   ULONG  ulLines      = 0;
   ULONG  ulPos        = 0;
   ULONG  ulLineBegin;
   ULONG  ulLineEnd;

   if( uiLineLength < 4 || uiLineLength > 254 )
      uiLineLength = 79;

   if( uiTabLength >  uiLineLength )
      uiTabLength = uiLineLength - 1;

   ulLineBegin = ulPos;
   ulLineEnd   = 0;

   while( ulPos < ulLen && ulLines < ulLineNumber )
   {
      switch( pszString[ ulPos ] )
      {
         case HB_CHAR_HT:
            uiCurLength += uiTabLength;
            break;

         case HB_CHAR_LF:
            uiCurLength = 0;
            uiLastSpace = 0;
            ulLineEnd   = ulPos - 2;
            ulLines++;
            if( ulLines < ulLineNumber )
            {
               ulLineBegin = ulPos + 1;
               ulLineEnd   = 0;
            }
            break;

         case HB_CHAR_CR:
            break;

         case ' ':
            uiCurLength++;
            uiLastSpace = uiCurLength;
            break;

         default:
            uiCurLength++;
      }

      if( uiCurLength > uiLineLength )
      {
         if( bWordWrap )
         {
            if( uiLastSpace == 0 )
            {
               uiCurLength = 1;
               ulLineEnd   = ulPos - 1;
            }
            else
            {
               uiCurLength = uiCurLength - uiLastSpace;
               ulLineEnd   = ulPos - uiCurLength ;
            }
         }
         else
         {
            uiCurLength = 1;
            ulLineEnd   = ulPos - 1;
         }

         ulLines++;
         uiLastSpace = 0;

         if( ulLines < ulLineNumber )
         {
            ulLineBegin = ulPos - uiCurLength + 1;
            ulLineEnd   = 0;
         }
      }

      ulPos++;
   }

   if( ulLineEnd == 0 )
   {
      ulLines++;
      ulLineEnd = ulPos - 1;
   }

   if( ulLineNumber == ulLines )
   {
      if( (ulLineEnd - ulLineBegin + 1 ) == uiLineLength )
      {
         hb_retclen( pszString + ulLineBegin, ( ulLineEnd - ulLineBegin + 1 ) );
      }
      else
      {
         char * pszLine = (char *)hb_xgrab( uiLineLength );
         memset( pszLine, ' ', uiLineLength );
         memcpy( pszLine, ( pszString + ulLineBegin ), ( ulLineEnd - ulLineBegin + 1 ) );
         hb_retclen( pszLine, uiLineLength );
         hb_xfree( pszLine );
      }
   }
   else
      hb_retc( "" );
}

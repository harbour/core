/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * HARDCR() function
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
 *      HARDCR
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Converts soft carriage pair chr(141)/chr(10) into hard carriage chr(13)
 *  $SYNTAX$
 *      HARDCR( <cString> ) --> <cConvertedString>
 *  $ARGUMENTS$
 *      <cString> is a string of chars to convert.
 *  $RETURNS$
 *      Trasformed string.
 *  $DESCRIPTION$
 *      Returns a string/memo with soft carriages chars converted to
 *      hard carraige char.
 *  $EXAMPLES$
 *      ? HARDCR( Data->CNOTES )
 *  $TESTS$
 *      @ 1, 1 SAY HARDCR( Data->CNOTES )
 *      will display converted string starting on row two, column two of the
 *      current device.
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      HARDCR is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      MEMOTRAN(), STRTRAN()
 *  $END$
 */

#include "extend.h"
#include "itemapi.h"

#define CHR_HARD1   ( ( char ) 13 )

#define CHR_SOFT1   ( ( char ) 141 )
#define CHR_SOFT2   ( ( char ) 10 )

char * hb_strHardCR( char * pszString, ULONG ulStringLen )
{
   ULONG ulStringPos;

   for( ulStringPos = 0; ulStringPos < ulStringLen; ulStringPos++ )
   {
      if( pszString[ ulStringPos ]     == CHR_SOFT1 &&
          pszString[ ulStringPos + 1 ] == CHR_SOFT2 )
      {
         pszString[ ulStringPos ] = CHR_HARD1;
      }
   }

   return pszString;
}

HARBOUR HB_HARDCR( void )
{
   PHB_ITEM pString = hb_param( 1, IT_STRING );

   if( pString )
   {
      char * pszBuffer = hb_itemGetC( pString );
      ULONG ulStringLen = hb_itemGetCLen( pString );

      hb_retclen( hb_strHardCR( pszBuffer, ulStringLen ), ulStringLen );

      hb_itemFreeC( pszBuffer );
   }
   else
      hb_retc( "" );
}


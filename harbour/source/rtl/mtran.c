/*
 * $Id$
   Harbour Project source code

   Copyright(C) 1999 by Jose Lalin.
   http://www.Harbour-Project.org/

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   You can contact me at: dezac@corevia.com
*/

/*  $DOC$
 *  $FUNCNAME$
 *      MEMOTRAN
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Converts hard and soft carriages within strings.
 *  $SYNTAX$
 *      MEMOTRAN( <cString>, <cHard>, <cSoft> ) --> <cConvertedString>
 *  $ARGUMENTS$
 *      <cString> is a string of chars to convert.
 *      <cHard> is the character to replace hard carriages with. If not
 *      specified defaults to semicolon.
 *      <cSoft> is the character to replace soft carriages with. If not
 *      specified defaults to single space.
 *  $RETURNS$
 *      Trasformed string.
 *  $DESCRIPTION$
 *      Returns a string/memo with carriage chars converted to specified
 *      chars.
 *  $EXAMPLES$
 *      ? MEMOTRAN( DATA->CNOTES )
 *  $TESTS$
 *      @ 1, 1 SAY MEMOTRAN( Data->CNOTES )
 *      will display converted string starting on row two, column two of the
 *      current device.
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      MEMOTRAN is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      HARDCR(), STRTRAN()
 *  $END$
 */

#include "extend.h"
#include "init.h"

#define CHR_HARD1   ((char)13)
#define CHR_HARD2   ((char)10)

#define CHR_SOFT1   ((char)141)
#define CHR_SOFT2   ((char)10)

HARBOUR HB_MEMOTRAN(void);

HB_INIT_SYMBOLS_BEGIN( Memotran__InitSymbols )
{ "MEMOTRAN", FS_PUBLIC, HB_MEMOTRAN, 0 }
HB_INIT_SYMBOLS_END( Memotran__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup Memotran__InitSymbols
#endif

/* NOTE: pszResult must have an allocated buffer of at least */
/*       ulStringLen */

void hb_memotran( char *pszResult, ULONG *ulResultLen, char *pszString, ULONG ulStringLen, char cHardcr, char cSoftcr )
{
   ULONG ulStringPos = 0;
   ULONG ulResultPos = 0;

   while (ulStringPos < ulStringLen)
   {
      if      ( pszString[ ulStringPos     ] == CHR_HARD1 &&
                pszString[ ulStringPos + 1 ] == CHR_HARD2 )
      {
         pszResult[ ulResultPos++ ] = cHardcr;
         ulStringPos += 2;
      }
      else if ( pszString[ ulStringPos     ] == CHR_SOFT1 &&
                pszString[ ulStringPos + 1 ] == CHR_SOFT2 )
      {
         pszResult[ ulResultPos++ ] = cSoftcr;
         ulStringPos += 2;
      }
      else
      {
         pszResult[ ulResultPos++ ] = pszString[ ulStringPos++ ];
      }
   }

   pszResult[ ulResultPos ] = '\0';

   *ulResultLen = ulResultPos;
}

HARBOUR HB_MEMOTRAN( void )
{
   if( ISCHAR( 1 ) )
   {
      char *pszResult = (char *) hb_xgrab ( hb_parclen( 1 ) + 1 );
      char cHardcr = ISCHAR( 2 ) ? *hb_parc( 2 ) : ';';
      char cSoftcr = ISCHAR( 3 ) ? *hb_parc( 3 ) : ' ';
      ULONG ulResultLen;

      hb_memotran( pszResult, &ulResultLen, hb_parc( 1 ), hb_parclen( 1 ), cHardcr, cSoftcr );
      hb_retclen( pszResult, ulResultLen );

      hb_xfree( pszResult );
   }
   else
      hb_retc( "" );
}


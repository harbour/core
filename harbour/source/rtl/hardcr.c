/*
 * $Id$
 *
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

#include "extend.h"
#include <ctype.h>
#include "init.h"

#define CHR_HARD1   (char)141
#define CHR_HARD2   (char)10

HARBOUR HB_HARDCR(void);


HB_INIT_SYMBOLS_BEGIN( HardCR__InitSymbols )
{ "HARDCR", FS_PUBLIC, HB_HARDCR, 0 }
HB_INIT_SYMBOLS_END( HardCR__InitSymbols );
#if ! defined(__GNUC__)
#pragma startup HardCR__InitSymbols
#endif

char *hb_hardcr( char *string )
{
   char *s;

   if( string )
   {
      for( s = string; *s; ++s )
         if( *s == CHR_HARD1 && *(s+1) == CHR_HARD2 )
            *s++ = '\n';
      *s = '\0';
   }
   return string;
}

HARBOUR HB_HARDCR( void )
{
   if( ISCHAR( 1 ) )
      hb_retc( hb_hardcr( hb_parc( 1 ) ) );
   else
      hb_retc( "" );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * COLORTON() CA-Tools function
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
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

static BYTE StrToColor( char * pszColor )
{
   BYTE color = 0;
   BOOL bFore = TRUE;

   while( *pszColor && *pszColor != ',' )
   {
      switch( *pszColor++ )
      {
      case '*':
         color |= 0x80;
         break;
 
      case '+':
         color |= 0x08;
         break;

      case '/':
         bFore = FALSE;
         break;

      case 'W':
      case 'w':
         color |= bFore ? 0x07 : 0x70;
         break;

      case 'R':
      case 'r':
         if( *pszColor == 'B' || *pszColor == 'b' )
         {
            color |= bFore ? 0x05 : 0x50;
            pszColor++;
         }
         else
            color |= bFore ? 0x04 : 0x40;
         break;

      case 'G':
      case 'g':
         if( *pszColor == 'R' || *pszColor == 'r' )
         {
            color |= bFore ? 0x06 : 0x60;
            pszColor++;
         }
         else
            color |= bFore ? 0x02 : 0x20;
         break;

      case 'B':
      case 'b':
         if( *pszColor == 'G' || *pszColor == 'g' )
         {
            color |= bFore ? 0x03 : 0x30;
            pszColor++;
         }
         else
            color |= bFore ? 0x01 : 0x10;
         break;
      }
   }

   return color;
}

HB_FUNC( COLORTON )
{
   hb_retni( StrToColor( hb_parc( 1 ) ) );
}


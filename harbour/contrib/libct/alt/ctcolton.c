/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * COLORTON() CA-Tools function
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
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


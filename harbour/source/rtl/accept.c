/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * ACCEPT command related functions
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    __ACCEPTSTR()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbapi.h"
#include "hbapigt.h"
#include "inkey.ch"

extern HB_FUNC( QOUT );

#define ACCEPT_BUFFER_LEN 256 /* length of input buffer for ACCEPT command */

static char s_szAcceptResult[ ACCEPT_BUFFER_LEN ] = { '\0' };

HB_FUNC( __ACCEPT )
{
   int input;
   ULONG ulLen;

   /* cPrompt(s) passed ? */
   if( hb_pcount() >= 1 )
      HB_FUNCNAME( QOUT )();

   ulLen = 0;
   input = 0;

   while( input != K_ENTER )
   {
      /* Wait forever, for keyboard events only */
      input = hb_inkey( 0.0, ( HB_inkey_enum ) INKEY_KEYBOARD, 1, 1 );
      switch( input )
      {
         case K_BS:
         case K_LEFT:
            if( ulLen > 0 )
            {
               hb_gtWriteCon( ( BYTE * ) "\x8 \x8", 3 ); /* Erase it from the screen. */
               ulLen--; /* Adjust input count to get rid of last character */
            }
            break;

         default:
            if( ulLen < ( ACCEPT_BUFFER_LEN - 1 ) && input >= 32 )
            {
               s_szAcceptResult[ ulLen ] = input; /* Accept the input */
               hb_gtWriteCon( ( BYTE * ) &s_szAcceptResult[ ulLen ], sizeof( char ) ); /* Then display it */
               ulLen++;  /* Then adjust the input count */
            }
      }
   }

   s_szAcceptResult[ ulLen ] = '\0';

   hb_retc( s_szAcceptResult );
}

HB_FUNC( __ACCEPTSTR )
{
   hb_retc( s_szAcceptResult );
}


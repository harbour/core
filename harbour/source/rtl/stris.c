/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * IS*() string functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include <ctype.h>

#include "hbapi.h"

/* determines if first char of string is letter */
/* TEST: QOUT( "isalpha( 'hello' ) = ", isalpha( 'hello' ) ) */
/* TEST: QOUT( "isalpha( '12345' ) = ", isalpha( '12345' ) ) */

HARBOUR HB_ISALPHA( void )
{
   hb_retl( isalpha( *hb_parc( 1 ) ) );
}

/* determines if first char of string is digit */
/* TEST: QOUT( "isdigit( '12345' ) = ", isdigit( '12345' ) ) */
/* TEST: QOUT( "isdigit( 'abcde' ) = ", isdigit( 'abcde' ) ) */

HARBOUR HB_ISDIGIT( void )
{
   hb_retl( isdigit( *hb_parc( 1 ) ) );
}

/* determines if first char of string is upper-case */
/* TEST: QOUT( "isupper( 'Abcde' ) = ", isupper( 'Abcde' ) ) */
/* TEST: QOUT( "isupper( 'abcde' ) = ", isupper( 'abcde' ) ) */

HARBOUR HB_ISUPPER( void )
{
   hb_retl( isupper( *hb_parc( 1 ) ) );
}

/* determines if first char of string is lower-case */
/* TEST: QOUT( "islower( 'abcde' ) = ", islower( 'abcde' ) ) */
/* TEST: QOUT( "islower( 'Abcde' ) = ", islower( 'Abcde' ) ) */

HARBOUR HB_ISLOWER( void )
{
   hb_retl( islower( *hb_parc( 1 ) ) );
}


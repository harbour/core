/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * WORD() function
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
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
#include "errorapi.h"
#include "itemapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      WORD()
 *  $CATEGORY$
 *      Conversion
 *  $ONELINER$
 *      Converts double to integer values.
 *  $SYNTAX$
 *      WORD( <nDouble> ) --> <nInteger>
 *  $ARGUMENTS$
 *      <nDouble> is a numeric double value.
 *  $RETURNS$
 *      An integer in the range +-32767
 *  $DESCRIPTION$
 *      This function converts double values to integers to use
 *      within the CALL command
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      The Clipper NG states that WORD() will only work when used in CALL
 *      commands parameter list, otherwise it will return NIL, in Harbour
 *      it will work anywhere.
 *  $SEEALSO$
 *       CALL
 *  $END$
 */

/* NOTE: The Clipper NG states that WORD() will only work when used
         in CALL commands parameter list, otherwise it will return
         NIL, in Harbour it will work anywhere. */

HARBOUR HB_WORD( void )
{
   if( ISNUM( 1 ) )
      hb_retni( hb_parni( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 1091, NULL, "WORD" );
}

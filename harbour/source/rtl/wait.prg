/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __WAIT() undocumented function
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
 *      __WAIT
 *  $CATEGORY$
 *
 *  $ONELINER$
 *      Stops the application until a key is pressed.
 *  $SYNTAX$
 *      __WAIT( <cMessage> ) --> <cKey>
 *  $ARGUMENTS$
 *      <cMessage> is a string.
 *  $RETURNS$
 *      Pressed key.
 *  $DESCRIPTION$
 *      This function stops the application until a key is pressed. The key
 *      must be in the range 32..255. Control keys are not processed.
 *  $EXAMPLES$
 *      // Wait for a key stroke
 *      __Wait( "Press a key to continue" )
 *  $TESTS$
 *      do while cKey != "Q"
 *        cKey := __Wait( "Press 'Q' to continue" )
 *      end do
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      __WAIT() is fully CA-Clipper compliant.
 *  $SEEALSO$
 *      __ACCEPT, __INPUT
 *  $END$
 */

#include "inkey.ch"

FUNCTION __Wait( cString )
   LOCAL nKey
   LOCAL bBlock

   IF cString == NIL
      /* TODO: Here we can use LangApi to localize default message
      */
      ? "Press any key to continue..."
   ELSE
      ? cString
   ENDIF

   DO WHILE .T.

      nKey := Inkey( 0 )

      IF ( bBlock := SetKey( nKey ) ) != NIL
         Eval( bBlock, ProcName( 1 ), ProcLine( 1 ), "" )
         LOOP
      ENDIF

      IF nKey >= 32 .and. nKey <= 255
         ?? Chr( nKey )
      ELSE
         nKey := 0
      ENDIF

      EXIT

   ENDDO

   RETURN Chr( nKey )

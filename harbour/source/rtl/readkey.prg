/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * READKEY() function
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
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
 * Copyright 1999 Chen Kedem <niki@actcom.co.il>
 *    READKEY() documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "inkey.ch"

/*  $DOC$
 *  $FUNCNAME$
 *      READKEY()*
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Find out which key terminate a READ
 *  $SYNTAX$
 *      READKEY() --> nKeyCode
 *  $ARGUMENTS$
 *      none.
 *  $RETURNS$
 *      READKEY() return a numeric code representing the key that cause READ
 *      to terminate.
 *  $DESCRIPTION$
 *      READKEY() is used after a READ was terminated to determine the exit
 *      key pressed. If the get buffer was updated during READ, 256 is added
 *      to the return code.
 *
 *      Exit key        Return code    Return code
 *                      (not updated)  (updated)
 *      --------------  -------------  -----------
 *      Up                    4            260
 *      Down                  5            261
 *      Page-Up               6            262
 *      Page-Down             7            263
 *      Ctrl Page-Up         34            290
 *      Ctrl Page-Down       35            291
 *      Esc                  12            268
 *      Ctrl End             14            270
 *      Enter                15            271
 *
 *      Key >= 32            15            271
 *      otherwise             0              0
 *
 *      READKEY() is a compatibility function so try not use it. READKEY()
 *      is superseded by LASTKEY() which return INKEY() code for that key,
 *      UPDATED() could be use to find if the get buffer was changed during
 *      READ.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      READKEY() works exactly like CA-Clipper's READKEY().
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *       @...GET,INKEY(),LASTKEY(),READ,READEXIT(),UPDATED()
 *  $END$
 */

FUNCTION ReadKey()
   LOCAL nKey := LastKey()

   DO CASE
   CASE nKey == K_UP        ; nKey :=  4 // NOTE: NG says 5 incorrectly
   CASE nKey == K_DOWN      ; nKey :=  5 // NOTE: NG says 2 incorrectly
   CASE nKey == K_PGUP      ; nKey :=  6
   CASE nKey == K_PGDN      ; nKey :=  7
   CASE nKey == K_CTRL_PGUP ; nKey := 34 // NOTE: NG says 31 incorrectly
   CASE nKey == K_CTRL_PGDN ; nKey := 35 // NOTE: NG says 30 incorrectly
   CASE nKey == K_ESC       ; nKey := 12
   CASE nKey == K_CTRL_W    ; nKey := 14
   CASE nKey == K_ENTER     ; nKey := 15
   CASE nKey >= K_SPACE     ; nKey := 15
   OTHERWISE                ; RETURN 0
   ENDCASE

   IF Updated()
      nKey += 256
   ENDIF

   RETURN nKey

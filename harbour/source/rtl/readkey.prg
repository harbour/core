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

#include "inkey.ch"

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

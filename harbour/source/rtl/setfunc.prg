/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __SETFUNCTION() function
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
/*  $DOC$
 *  $FUNCNAME$
 *      __SetFunction() 
 *  $CATEGORY$
 *      Environment
 *  $ONELINER$
 *      Assign a character string to a function key
 *  $SYNTAX$
 *      __SetFunction( <nFunctionKey>, [<cString>] ) --> NIL
 *  $ARGUMENTS$
 *      <nFunctionKey> is a number in the range 1..40 that represent the
 *      function key to be assigned.
 *
 *      <cString> is a character string to set. If <cString> is not
 *      specified, the function key is going to be set to NIL releasing by
 *      that any previous __SetFunction() or SETKEY() for that function.
 *  $RETURNS$
 *      __SetFunction() always return NIL.
 *  $DESCRIPTION$
 *      __SetFunction() assign a character string with a function key, when
 *      this function key is pressed, the keyboard is stuffed with this
 *      character string. __SetFunction() has the effect of clearing any
 *      SETKEY() previously set to the same function number and vice versa.
 *
 *      nFunctionKey    Key to be set
 *      ------------    -------------
 *         1 .. 12      F1 .. F12
 *        13 .. 20      Shift-F3 .. Shift-F10
 *        21 .. 30      Ctrl-F1 .. Ctrl-F10
 *        31 .. 40      Alt-F1 .. Alt-F10
 *
 *      SET FUNCTION command is preprocessed into __SetFunction() function
 *      during compile time.
 *  $EXAMPLES$
 *      // Set F1 with a string
 *      CLS
 *      __SetFunction( 1, "I Am Lazy" + CHR( 13 ) )
 *      cTest := SPACE( 20 )
 *      @ 10, 0 SAY "type something or F1 for lazy mode " GET cTest
 *      READ
 *      ? cTest
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      Harbour use 11 and 12 to represent F11 and F12, while CA-Clipper use
 *      11 and 12 to represent Shift-F1 and Shift-F2.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      INKEY(),SETKEY(),__Keyboard(),SET KEY
 *  $END$
 */

PROCEDURE __SetFunction( nFunctionKey, cString )

   /* NOTE: CA-Cl*pper will not handle F11 and F12 here. */

   DO CASE
   CASE nFunctionKey == 1  ; nFunctionKey := K_F1
   CASE nFunctionKey == 11 ; nFunctionKey := K_F11
   CASE nFunctionKey == 12 ; nFunctionKey := K_F12
   OTHERWISE               ; nFunctionKey := -nFunctionKey + 1
   ENDCASE

   IF ISCHARACTER( cString )
      SetKey( nFunctionKey, {|| __Keyboard( cString ) } )
   ELSE
      SetKey( nFunctionKey, NIL )
   ENDIF

   RETURN
/*  $DOC$
 *  $FUNCNAME$
 *     SET FUNCTION 
 *  $CATEGORY$
 *     Command
 *  $ONELINER$
 *      Assign a character string to a function key
 *  $SYNTAX$
 *      SET FUNCTION <nFunctionKey> TO [<cString>]
 *  $ARGUMENTS$
 *      <nFunctionKey> is a number in the range 1..40 that represent the
 *      function key to be assigned.
 *
 *      <cString> is a character string to set. If <cString> is not
 *      specified, the function key is going to be set to NIL releasing by
 *      that any previous  Set Function or SETKEY() for that function.
 *  $RETURNS$
 *  $DESCRIPTION$
 *        Set Function assign a character string with a function key, when
 *      this function key is pressed, the keyboard is stuffed with this
 *      character string. Set Function has the effect of clearing any
 *      SETKEY() previously set to the same function number and vice versa.
 *
 *      nFunctionKey    Key to be set
 *      ------------    -------------
 *         1 .. 12      F1 .. F12
 *        13 .. 20      Shift-F3 .. Shift-F10
 *        21 .. 30      Ctrl-F1 .. Ctrl-F10
 *        31 .. 40      Alt-F1 .. Alt-F10
 *
 *      SET FUNCTION command is preprocessed into __SetFunction() function
 *      during compile time.
 *  $EXAMPLES$
 *      // Set F1 with a string
 *      CLS
 *      Set Function  1 to  "I Am Lazy" + CHR( 13 ) 
 *      cTest := SPACE( 20 )
 *      @ 10, 0 SAY "type something or F1 for lazy mode " GET cTest
 *      READ
 *      ? cTest
 *  $TESTS$
 *  $STATUS$
 *  $COMPLIANCE$
 *      Harbour use 11 and 12 to represent F11 and F12, while CA-Clipper use
 *      11 and 12 to represent Shift-F1 and Shift-F2.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      INKEY(),SETKEY(),__Keyboard()
 *  $END$
 */

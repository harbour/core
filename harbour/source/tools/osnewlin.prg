/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Operating System related functions
 *
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
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

/* ------------------------------------------------------------------------- */

/*
 * ChangeLog:
 * V 1.1    David G. Holm               Committed to CVS.
 *
 */

/* ------------------------------------------------------------------------- */

/*  $DOC$
 *  $FUNCNAME$
 *      OS_NewLine
 *  $CATEGORY$
 *      Operating System Specific
 *  $ONELINER$
 *      Returns the newline character(s) to use with the current OS
 *  $SYNTAX$
 *      OS_NewLine( aCrLf, aLf ) --> cString
 *  $ARGUMENTS$
 *      aCrLf can be used to add to the keyword list that is used to detect
 *      operating systems that require both CR and LF. The aCrLf list will
 *      be scanned before the default list is scanned.
 *      aLf can be used to force an OS that would normally be set up to
 *      require both CR and LF to use LF only.
 *      For both arguments, each element of the array must be a text string.
 *      Matches are made by using the $ operator with an array element as
 *      the left operand and the OS string as the right argument, after both
 *      strings have been converted to upper case.
 *  $RETURNS$
 *      A character string containing the character or characters required
 *      to move the screen cursor or print head to the start of a new line.
 *      The string will hold either CHR( 10 ) or CHR( 13 ) + CHR( 10 ).
 *  $DESCRIPTION$
 *      Returns a character string containing the character or characters
 *      required to move the screen cursor or print head to the start of a
 *      new line for the operating system that the program is running on
 *      (or thinks it is running on, if an OS emulator is being used).
 *  $EXAMPLES$
 *      // Get the newline character(s) for the current OS using defaults.
 *      STATIC s_cNewLine
 *      ...
 *      s_cNewLine := OS_NewLine()
 *      ...
 *  $TESTS$
 *      valtype( OS_NewLine() ) == "C"
 *      LEN( OS_NewLine( { "ANOTHERDOS" }, { "" } ) ) == 1
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      This is an add-on Operating System Tool function.
 *  $SEEALSO$
 *      OS()
 *  $END$
 */

STATIC aDefault

FUNCTION OS_NewLine( aCrLf, aLf )
LOCAL lCrLf, i, cOS := UPPER( OS() )

   // Only create the default array once.
   IF EMPTY( aDefault )
      // NOTE: Don't use overly permissive strings like "DOS", "NT", or "WIN"
      //       All alpha characters must be in UPPER CASE only!
      aDefault := { "OS/2", "MS-DOS", "PC DOS", "DR DOS", "DOS 5", ;
                    "WINDOWS 9", "WINDOWS NT", "WINDOWS 2", "CYGWIN" }
   END IF

   // First check the default CR/LF list.
   lCrLf := ASCAN( aDefault, {|cEntry| cEntry $ cOS } ) > 0

   IF ! lCrLf .AND. ! EMPTY( aCrLf )
      // Only check the extra CR/LF list if the OS wasn't already
      // identified as a CR/LF OS and an extra list was provided.
      lCrLf := ASCAN( aCrLf, {|cEntry| UPPER( cEntry ) $ cOS } ) > 0
   END IF
   IF lCrLf .AND. ! EMPTY( aLf )
      // Only check the force LF list if it was previously determined
      // that the OS would normally use CR/LF.
      lCrLf := ! ASCAN( aLf, {|cEntry| UPPER( cEntry ) $ cOS } ) > 0
   END IF

   // Finally, assign and return the appropriate newline string.
   IF lCrLf
      // It's an OS that requires both CR and LF.
      cNewLine := CHR( 13 ) + CHR( 10 )
   ELSE
      // Everything else gets just LF.
      cNewLine := CHR( 10 )
   END IF
RETURN cNewLine

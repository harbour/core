/* $Id$

   Harbour Project source code

   This file contains the Harbour function that outputs expressions
   using picture transformations

   Copyright 1999  David G. Holm
   www - http://www.Harbour-Project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).

   V 1.2    David G. Holm               Removed "(C)" from Copyright and
                                        updated previous version comments.
   V 1.1    David G. Holm               Submitted to Harbour Project.
   V 1.0    David G. Holm               Initial version.
*/

/*  $DOC$
 *  $FUNCNAME$
 *      DEVOUTPICT
 *  $CATEGORY$
 *      Terminal
 *  $ONELINER$
 *      Outputs the result of any expression using a picture transformation
 *  $SYNTAX$
 *      DEVOUTPICT(<xExp>,<cPicture>[,<cColorString>]) --> NIL
 *  $ARGUMENTS$
 *      <xExp> is any valid expression.
 *      <cPicture> is any picture transformation that TRANSFORM() can use.
 *      <cColorString> is an optional string that specifies a screen color to
 *      use in place of the default color when the output goes to the screen.
 *  $RETURNS$
 *      NIL
 *  $DESCRIPTION$
 *      Outputs any expression using a picture transformation instead of using
 *      the default transformation for the type of expression.
 *  $EXAMPLES$
 *      // Output a negative dollar amount using debit notation.
 *      DEVOUTPICT( -1.25, "@D$ 99,999.99 )
 *  $TESTS$
 *      @ 3,1 SAY -1.25 PICTURE "@D$ 99,999.99"
 *      will display "$(     1.25)" starting on row four, column two of the
 *      current device (without the double quotation marks, of course).
 *  $STATUS$
 *      C
 *  $COMPLIANCE$
 *      DEVOUTPICT() is mostly CA-Clipper compliant. Any differences are due
 *      to enhancements in the Harbour TRANSFORM() over CA-Clipper.
 *  $SEEALSO$
 *      DEVOUT(), TRANSFORM().
 *  $END$
 */

FUNCTION DEVOUTPICT( xValue, cPicture, cColor )
   LOCAL cText := TRANSFORM( xValue, cPicture )
   DEVOUT( cText, cPicture )
RETURN NIL

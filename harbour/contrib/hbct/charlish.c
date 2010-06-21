/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - CHARSLIST()  (Harbour extension)
 *     - CHARHIST()  (Harbour extension)
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://harbour-project.org
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

#include "ct.h"

/*  $DOC$
 *  $FUNCNAME$
 *      CHARSLIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a sorted list of all characters in a string
 *  $SYNTAX$
 *      CHARSLIST ([<cString>]) -> cSortedCharacterList
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a
 *                        sorted list of all characters
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <cSortedCharacterList>  a sorted list of the characters in <cString>
 *  $DESCRIPTION$
 *      The CHARLIST() function generates a sorted list of those characters that
 *      are contained in <cString>. This list can contain each character
 *      only once, so that its maximum length is 256. The function
 *      gives the same result as CHARSORT(CHARLIST(<cString>))
 *  $EXAMPLES$
 *      ? charslist ("Hello World !") --> " !HWdelor"
 *  $TESTS$
 *      charslist ("Hello World !") == " !HWdelor"
 *      charslist ("Hello World !") == charsort (charlist ("Hello World !"))
 *      charslist (nil) == ""
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSLIST() is only available in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARNOLIST(),CHARLIST(),CHARHIST()
 *  $END$
 */

HB_FUNC( CHARSLIST )
{
   ct_charlist( CT_CHARLIST_CHARSLIST );
}

/*  $DOC$
 *  $FUNCNAME$
 *      CHARHIST()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Generates a character histogram of a string
 *  $SYNTAX$
 *      CHARHIST ([<cString>]) -> aCharacterCount
 *  $ARGUMENTS$
 *      [<cString>]       is the string for whom the function generates a
 *                        character histogram
 *                        Default: "" (empty string)
 *  $RETURNS$
 *      <aCharacterCount> an array with 256 elements where the nth element
 *                        contains the count of character #(n-1) in cString
 *  $DESCRIPTION$
 *      The CHARHIST() function generates a character histogram of those
 *      characters that are contained in <cString>. This histogram is stored
 *      in an 256-element array where the nth element contains the count
 *      of ASCII character #(n-1) in <cString>.
 *  $EXAMPLES$
 *      ? charhist ("Hello World !")[109] --> 3  // chr(108)=="l"
 *  $TESTS$
 *      charhist ("Hello World !")[109] == 3
 *      eval ({||aeval (charhist ("Hello World !"),{|x|nTotal+=x}),nTotal==len("Hello World !")}
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARHIST() is only available in Harbour's CT3 library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charlist.c, library is libct.
 *  $SEEALSO$
 *      CHARLIST(),CHARNOLIST(),CHARSLIST()
 *  $END$
 */

HB_FUNC( CHARHIST )
{
   ct_charlist( CT_CHARLIST_CHARHIST );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   ASCIISUM CT3 string function
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
 *
 * www - http://www.harbour-project.org
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
 *      ASCIISUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      calculate the sum of the ASCII values of the characters in a string
 *  $SYNTAX$
 *      ASCIISUM (<cString>) --> nAsciiSum
 *  $ARGUMENTS$
 *      <cString>      the string to be processed
 *  $RETURNS$
 *      <nAsciiSum>    sum of the ASCII values in <cString>
 *  $DESCRIPTION$
 *      The ASCIISUM() function sums up the ASCII values of the characters
 *      in <cString>. Be aware that the function is not position sensitive,
 *      i.e. a change of position of a certain character in the string does
 *      not change the ascii sum.
 *  $EXAMPLES$
 *      ? asciisum ("ABC")  -->  197
 *      ? asciisum ("ACB")  -->  197
 *  $TESTS$
 *      asciisum (replicate ("A", 10000)) == 650000 
 *      asciisum ("0123456789") == 525              
 *      asciisum (nil) == 0                         
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ASCIISUM() is compatible with CT3's ASCIISUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is asciisum.c, library is ct3.
 *  $SEEALSO$
 *      CHECKSUM()
 *  $END$
 */

HB_FUNC (ASCIISUM)
{

  if (ISCHAR (1))
  {
    size_t sStrSize = hb_parclen (1);
    char *pcString  = hb_parc (1);
    size_t sPos;
    ULONG ulResult = 0;

    for (sPos = 0; sPos < sStrSize; sPos++)
      ulResult += (ULONG)pcString[sPos];

    hb_retnl (ulResult);

  }
  else
  {
    hb_retnl (0);
  }

  return;

}




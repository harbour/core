/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CHARMIX() CT3 function
 *
 * Initial code: Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 * 
 * CT3 conformity: Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *                 Author: Martin Vogel <vogel@inttec.de>
 *
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

#include "ct.h"


/*  $DOC$
 *  $FUNCNAME$
 *      CHARMIX()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Mix two strings
 *  $SYNTAX$
 *      CHARMIX (<cString1>[, <cString2>]) --> cMixedString
 *  $ARGUMENTS$
 *      <cString1>     String that will be mixed with the characters from <cString2>
 *      [<cString2>]   String whose characters will be mixed with the one from
 *                     <cString1>.
 *                     Default: " " (string with one space char)
 *  $RETURNS$
 *      <cMixedString> Mixed string
 *  $DESCRIPTION$
 *      The CHARMIX() function mixes the strings <cString1> and <cString2>. To
 *      do this it takes one character after the other alternatively from
 *      <cString1> and <cString2> and puts them in the output string.
 *      This procedure is stopped when the end of <cString1> is reached. If
 *      <cString2> is shorter than <cString1>, the function will start at
 *      the begin of <cString2> again. If on the other hand <cString2> is
 *      longer than <cString1>, the surplus characters will be omitted.
 *  $EXAMPLES$
 *      ? CHARMIX("ABC", "123")    // "A1B2C3"
 *      ? CHARMIX("ABCDE", "12")   // "A1B2C1D2E1"
 *      ? CHARMIX("AB", "12345")   // "A1B2"
 *      ? CHARMIX("HELLO", " ")    //  "H E L L O "
 *      ? CHARMIX("HELLO", "")     //  "HELLO"
 *  $TESTS$
 *      CHARMIX("ABC", "123")  == "A1B2C3"
 *      CHARMIX("ABCDE", "12") == "A1B2C1D2E1"
 *      CHARMIX("AB", "12345") == "A1B2"
 *      CHARMIX("HELLO", " ")  == "H E L L O "
 *      CHARMIX("HELLO", "")   == "HELLO"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARMIX() is compatible with CT3's CHARMIX().
 *      NOTE: CA-Tools version of CHARMIX() will hang
 *            if the second parameter is an empty string, this version will not.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charmix.c, library is ct3.
 *  $SEEALSO$
 *      CHAREVEN()   CHARODD()
 *  $END$
 */

HB_FUNC (CHARMIX)
{
  
  if (ISCHAR (1))
  {
    char *pcString1 = hb_parc (1);
    char *pcString2, *pcResult;
    size_t sLen1 = hb_parclen (1);
    size_t sLen2, sPos1, sPos2, sResultPos;

    if (sLen1 == 0)
    {
      hb_retc ("");
      return;
    }
    
    if (ISCHAR (2))
    {
      pcString2 = hb_parc (2);
      sLen2 = hb_parclen (2);
      if (sLen2 == 0)
      {
        hb_retclen (pcString1, sLen1);
        return;
      }
    }
    else
    {
      pcString2 = " ";  /* NOTE: The original CT3 uses " " as 2nd string
                           if the 2nd param is not a string ! */
      sLen2 = 1;
    }

    pcResult = hb_xgrab (sLen1 * 2);
    sPos2 = sResultPos = 0;
    for (sPos1 = 0; sPos1 < sLen1;)
    {
      pcResult[sResultPos++] = pcString1[sPos1++];
      pcResult[sResultPos++] = pcString2[sPos2++];
      sPos2 %= sLen2;
    }

    hb_retclen (pcResult, sLen1 * 2);
    hb_xfree (pcResult);

  }
  else
  {
    hb_retc ("");
  }

}
  

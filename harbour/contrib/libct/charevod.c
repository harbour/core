/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - CHAREVEN()
 *     - CHARODD()
 *
 * Copyright 2000 Victor Szakats <info@szelvesz.hu>
 *
 * Merged into one file for optimization and some small corrections:
 * (c) 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *          Author: Martin Vogel <vogel@inttec.de>
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


#define DO_CHAREVOD_CHAREVEN   0
#define DO_CHAREVOD_CHARODD    1

/* helper function */
static void do_charevod (int iSwitch)
{
  
  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sLen = hb_parclen (1);
    char *pcResult;
    size_t sPos, sResultPos;

    if (sLen == 0)
    {
      hb_retc ("");
      return;
    }
    
    pcResult = hb_xgrab ((sLen+1)/2);

    if (iSwitch == DO_CHAREVOD_CHAREVEN)
      sPos = 1;
    else
      sPos = 0;

    sResultPos = 0;
    for (; sPos < sLen; sPos+=2)
      pcResult[sResultPos++] = pcString[sPos];

    if (sResultPos == 0)
      hb_retc ("");
    else
      hb_retclen (pcResult, sResultPos);
  
    hb_xfree (pcResult);

  }
  else
  {
    hb_retc ("");
  }

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHAREVEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns the characters on the even positions in a string
 *  $SYNTAX$
 *      CHAREVEN (<cString>) --> cEvenString
 *  $ARGUMENTS$
 *      <cString>      processed string
 *  $RETURNS$
 *      <cEvenString>  a string containing all character from even positions
 *                     in <cString>
 *  $DESCRIPTION$
 *      The CHAREVEN() function looks for the characters on the even positions
 *      in a given string, collects them and returns them as a string.
 *  $EXAMPLES$
 *      ? CHAREVEN (" H E L L O !") -> "HELLO!"
 *  $TESTS$
 *      CHAREVEN (" 1 2 3 4 5") == "12345"
 *      CHAREVEN (" 1 2 3 4 ") == "1234"
 *      CHAREVEN (" ") == ""
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHAREVEN() is compatible with CT3's CHAREVEN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charevod.c, library is ct3.
 *  $SEEALSO$
 *      CHARODD()  CHARMIX()
 *  $END$
 */

HB_FUNC (CHAREVEN)
{

  do_charevod (DO_CHAREVOD_CHAREVEN);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      CHARODD()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns the characters on the odd positions in a string
 *  $SYNTAX$
 *      CHARODD (<cString>) --> cOddString
 *  $ARGUMENTS$
 *      <cString>      processed string
 *  $RETURNS$
 *      <cOddString>  a string containing all character from odd positions
 *                    in <cString>
 *  $DESCRIPTION$
 *      The CHARODD() function looks for the characters on the odd positions
 *      in a given string, collects them and returns them as a string.
 *  $EXAMPLES$
 *      ? CHARODD ("H E L L O ! ") -> "HELLO!"
 *  $TESTS$
 *      CHARODD ("1A2B3C4D5E") == "12345"
 *      CHARODD ("1A2B3C4D5")  == "12345"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARODD() is compatible with CT3's CHARODD().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charevod.c, library is ct3.
 *  $SEEALSO$
 *      CHAREVEN()  CHARMIX()
 *  $END$
 */

HB_FUNC (CHARODD)
{

  do_charevod (DO_CHAREVOD_CHARODD);
  return;

}




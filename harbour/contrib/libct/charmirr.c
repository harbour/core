/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string function CHARMIRR()
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
 *      CHARMIRR()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Mirror a string
 *  $SYNTAX$
 *      CHARMIRR (<[@]cString>, [<lDontMirrorSpaces>]) -> cMirroredString
 *  $ARGUMENTS$
 *      <[@]cString>            is the string that should be mirrored
 *      [<lDontMirrorSpaces>]   if set to .T., spaces at the end of
 *                              <cString> will not be mirrored but kept at the end
 *                              Default: .F., mirror the whole string
 *  $RETURNS$
 *      <cMirroredString>       the mirrored string
 *  $DESCRIPTION$
 *      The CHARMIRR() function mirrors a string, i.e. the first character
 *      will be put at the end, the second at the last but one position etc..
 *      One can use this function for index searches, but then, the spaces
 *      at the end of the string should not be mirrored.
 *      One can omit the return value of the function by setting the CSETREF()
 *      switch to .T., but <cString> must then be passed by reference to get
 *      a result.
 *  $EXAMPLES$
 *      ? charmirr ("racecar")        // "racecar" 
 *      ? charmirr ("racecar  ", .T.) // "racecar  "
 *      ? charmirr ("racecar  ", .F.) // "  racecar"
 *  $TESTS$
 *      charmirr ("racecar") == "racecar" 
 *      charmirr ("racecar  ", .T.) == "racecar  "
 *      charmirr ("racecar  ", .F.) == "  racecar"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARMIRR() is compatible with CT3's CHARMIRR().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charmirr.c, library is ct3.
 *  $SEEALSO$
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARMIRR)
{

  int iNoRet;

  /* suppressing return value ? */
  iNoRet = ct_getref();

  /* param check */
  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    char *pcRet, *pc1, *pc2;
    int iDontMirrorSpaces;

    if (ISLOG (2))
      iDontMirrorSpaces = hb_parl (2);
    else
      iDontMirrorSpaces = 0;

    if (sStrLen == 0)
    {
      if (iNoRet)
        hb_retl (0);
      else
        hb_retc ("");
      return;
    }

    pcRet = hb_xgrab (sStrLen);

    pc1 = pcString+sStrLen-1;
    if (iDontMirrorSpaces)
    {
      pc2 = pcRet+sStrLen-1;
      while ((pc1 >= pcString) && (*pc1 == 0x20))
      {
        *pc2 = 0x20;
        pc1--;
        pc2--;
      }
    }

    pc2 = pcRet;
    for (; pc1 >= pcString; pc1--)
    {
      *pc2 = *pc1;
      pc2++;
    }

    /* return string */
    if (ISBYREF (1))
      hb_storclen (pcRet, sStrLen, 1);

    if (iNoRet)
      hb_retl (0);
    else
      hb_retclen (pcRet, sStrLen);

    hb_xfree (pcRet);

  }
  else /* if (ISCHAR (1)) */
  {
    if (iNoRet)
      hb_retl (0);
    else
      hb_retc ("");
  }

  return;

}




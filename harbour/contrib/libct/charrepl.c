/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string function CHARREPL()
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
 *      CHARREPL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Replacement of characters
 *  $SYNTAX$
 *      CHARREPL (<cSearchString>, <[@]cString>,
 *                <cReplaceString>, [<lMode>]) -> cString
 *  $ARGUMENTS$
 *      <cSearchString>    is a string of characters that should be replaced
 *      <[@]cString>       is the processed string
 *      <cReplaceString>   is a string of characters that replace the one
 *                         of <cSearchString>
 *      [<lMode>]          sets the replacement method (see description)
 *                         Default: .F.
 *  $RETURNS$
 *      <cString>          the processed string
 *  $DESCRIPTION$
 *      The CHARREPL() function replaces certain characters in <cString>
 *      with others depending on the setting of <lMode>.
 *      If <lMode> is set to .F., the function takes the characters of
 *      <cSearchString> one after the other, searches for them in <cString>
 *      and, if successful, replaces them with the corresponding character
 *      of <cReplaceString>. Be aware that if the same characters occur
 *      in both <cSearchString> and <cReplaceString>, the character on a
 *      certain position in <cString> can be replaced multiple times. 
 *      if <lMode> is set to .T., the function takes the characters in <cString>
 *      one after the other, searches for them in <cSearchString> and, if
 *      successful, replaces them with the corresponding character of
 *      <cReplaceString>. Note that no multiple replacements are possible
 *      in this mode.
 *      If <cReplaceString> is shorter than <cSearchString>, the last
 *      character of <cReplaceString> is used as corresponding character
 *      for the the "rest" of <cSearchString>.
 *      One can omit the return value by setting the CSETREF() switch to .T.,
 *      but then one must pass <cString> by reference to get the result.
 *  $EXAMPLES$
 *      ? charrepl ("1234", "1x2y3z", "abcd")            // "axbycz" 
 *      ? charrepl ("abcdefghij", "jhfdb", "1234567890") // "08642"  
 *      ? charrepl ("abcdefghij", "jhfdb", "12345")      // "55542"  
 *      ? charrepl ("1234", "1234", "234A")              // "AAAA"   
 *      ? charrepl ("1234", "1234", "234A", .T.)         // "234A"   
 *  $TESTS$
 *      charrepl ("1234", "1x2y3z", "abcd") == "axbycz" 
 *      charrepl ("abcdefghij", "jhfdb", "1234567890") == "08642"  
 *      charrepl ("abcdefghij", "jhfdb", "12345") == "55542"  
 *      charrepl ("1234", "1234", "234A") == "AAAA"   
 *      charrepl ("1234", "1234", "234A", .T.) == "234A"   
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARREPL() is compatible with CT3's CHARREPL().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charrepl.c, library is ct3.
 *  $SEEALSO$
 *      WORDREPL()   POSREPL()   RANGEREPL()
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARREPL)
{

  int iNoRet;

  size_t sSearchLen, sReplaceLen;

  /* suppressing return value ? */
  iNoRet = ct_getref();

  /* param check */
  if (((sSearchLen = (size_t)hb_parclen (1)) > 0) &&
      (ISCHAR (2)) &&
      ((sReplaceLen = (size_t)hb_parclen (3)) > 0))
  {

    /* get parameters */
    char *pcSearch = hb_parc (1);
    char *pcString = hb_parc (2);
    size_t sStrLen = (size_t)hb_parclen (2);
    char *pcReplace = hb_parc (3);
    int iMode;
    char *pcRet;
    size_t sIndex;

    if (ISLOG (4))
    {
      iMode = hb_parl (4);
    }
    else
    {
      iMode = 0;
    }

    pcRet = ( char * ) hb_xgrab (sStrLen);
    hb_xmemcpy (pcRet, pcString, sStrLen);

    for (sIndex = 0; sIndex < sSearchLen; sIndex++)
    {
    
      size_t sMatchStrLen;
      char *pc;
      size_t sReplIndex = sIndex;

      if (sReplIndex > sReplaceLen-1)
      {
        sReplIndex = sReplaceLen-1;
      }

      if (iMode)
      {
        /* no multiple replacements: searching in pcString,
                                     replacing in pcRet     */
        pc = pcString;

        while ((pc = ct_at_exact_forward (pc, sStrLen-(pc-pcString),
                                          pcSearch+sIndex, 1,
                                          &sMatchStrLen)) != NULL)
        {
          *(pcRet+(pc-pcString)) = *(pcReplace+sReplIndex);
          pc++;
        }
      }
      else
      {
        /* multiple replacements: searching & replacing in pcRet */
        pc = pcRet;
        while ((pc = ct_at_exact_forward (pc, sStrLen-(pc-pcRet),
                                          pcSearch+sIndex, 1,
                                          &sMatchStrLen)) != NULL)
        {
          *pc = *(pcReplace+sReplIndex);
          pc++;
        }
      }

    }

    /* return string */
    if (ISBYREF (2))
    {
      hb_storclen (pcRet, sStrLen, 2);
    }

    if (iNoRet)
    {
      hb_retl (0);
    }
    else
    {
      hb_retclen (pcRet, sStrLen);
    }

    hb_xfree (pcRet);

  }
  else /* ((sSearchLen = (size_t)hb_parclen (1)) > 0) &&
          (ISCHAR (2)) &&
          ((sReplaceLen = (size_t)hb_parclen (3)) > 0))   */
  {
    if (iNoRet)
    {
      hb_retl (0);
    }
    else
    {
      if (ISCHAR (2))
      {
        hb_retclen (hb_parc (2), hb_parclen (2));
      }
      else
      {
        hb_retc ("");
      }
    }
  }

  return;

}





/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 string functions
 *
 *   - AFTERATNUM()
 *   - BEFORATNUM()
 *   - ATNUM() 
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


#define DO_ATNUM_AFTERATNUM      0
#define DO_ATNUM_BEFORATNUM      1
#define DO_ATNUM_ATNUM           2


/* helper function */
static void do_atnum (int iSwitch)
{

  if ((ISCHAR (1)) && (ISCHAR (2)))
  {
    
    char *pcStringToMatch = hb_parc (1);
    size_t sStrToMatchLen = (size_t)hb_parclen (1);
    char *pcString = hb_parc (2);
    size_t sStrLen = (size_t)hb_parclen (2);
    int iMultiPass = ct_getatmupa();
    int iAtLike    = ct_getatlike();
    char cAtLike   = ct_getatlikechar();
    size_t sIgnore, sMatchStrLen;
    ULONG ulCounter;
    char *pc = NULL;

    /* eventually ignore some characters */
    if (ISNUM (4))
      sIgnore = (size_t)hb_parnl (4);
    else
      sIgnore = 0;

    if (sIgnore >= sStrLen)
    {
      switch (iSwitch)
      {
        case DO_ATNUM_AFTERATNUM:
        {
          /* AFTERATNUM */
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_AFTERATNUM,
                      NULL, "AFTERATNUM", 0, EF_CANDEFAULT, 4,
                      hb_paramError (1), hb_paramError (2),
                      hb_paramError (3), hb_paramError (4));
          };
          hb_retc ("");
        }; break;

        case DO_ATNUM_BEFORATNUM:
        {
          /* BEFORATNUM */
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_BEFORATNUM,
                      NULL, "BEFORATNUM", 0, EF_CANDEFAULT, 4,
                      hb_paramError (1), hb_paramError (2),
                      hb_paramError (3), hb_paramError (4));
          };
          hb_retc ("");
        }; break;
        
        case DO_ATNUM_ATNUM:
        {
          /* ATNUM */
          int iArgErrorMode = ct_getargerrormode();
          if (iArgErrorMode != CT_ARGERR_IGNORE)
          {
            ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                      NULL, "ATNUM", 0, EF_CANDEFAULT, 4,
                      hb_paramError (1), hb_paramError (2),
                      hb_paramError (3), hb_paramError (4));
          };
          hb_retnl (0);
        }; break;
      }
      
      return;
    }
    else
    {
      pcString += sIgnore;
      sStrLen -= sIgnore;
    }

    /* nth match or last match ? */
    if (ISNUM (3) && ((ulCounter = hb_parnl (3)) != 0))
    {

      /* find the <ulCounter>th match */
      char *pcSubStr;
      size_t sSubStrLen;
      ULONG ulMatchCounter = 0;

      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      while (ulMatchCounter < ulCounter) 
      {
        switch (iAtLike)
        {
          case CT_SETATLIKE_EXACT:
          {
            pc = ct_at_exact_forward (pcSubStr, sSubStrLen,
                                      pcStringToMatch, sStrToMatchLen,
                                      &sMatchStrLen);
          }; break;

          case CT_SETATLIKE_WILDCARD:
          {
            pc = ct_at_wildcard_forward (pcSubStr, sSubStrLen,
                                         pcStringToMatch, sStrToMatchLen,
                                         cAtLike, &sMatchStrLen);
          }; break;

          default:
          {
            pc = NULL;
          };
        }

        if (pc == NULL)
        {
          /* no match found; if this happens at this point,
             there are no <ulCounter> matches, so return an empty string */
          switch (iSwitch)
          {
            case DO_ATNUM_AFTERATNUM:
            case DO_ATNUM_BEFORATNUM:
            {
              /* AFTERATNUM */
              /* BEFORATNUM */
              hb_retc ("");
            }; break;
            
            case DO_ATNUM_ATNUM:
            {
              /* ATNUM */
              hb_retnl (0);
            }; break;
          }
          
          return;
        }

        ulMatchCounter++;

        if (iMultiPass)
          pcSubStr = pc+1;
        else
          pcSubStr = pc+sMatchStrLen;
        sSubStrLen = sStrLen-(pcSubStr-pcString);
      }

    }
    else /* (ISNUM (3) && ((ulCounter = hb_parnl (3)) != 0) */
    {
        
      /* we have to find the last match and return the
         string after that last match */

      switch (iAtLike)
      {
        case CT_SETATLIKE_EXACT:
        {
          pc = ct_at_exact_backward (pcString, sStrLen,
                                     pcStringToMatch, sStrToMatchLen,
                                     &sMatchStrLen);
        }; break;

        case CT_SETATLIKE_WILDCARD:
        {
          pc = ct_at_wildcard_backward (pcString, sStrLen,
                                        pcStringToMatch, sStrToMatchLen,
                                        cAtLike, &sMatchStrLen);
        }; break;

        default:
        {
          pc = NULL;
        };
      }

      if (pc == NULL)
      {
        /* no matches found */
        switch (iSwitch)
        {
          case DO_ATNUM_AFTERATNUM:
          case DO_ATNUM_BEFORATNUM:
          {
            /* AFTERATNUM */
            /* BEFORATNUM */
            hb_retc ("");
          }; break;

          case DO_ATNUM_ATNUM:
          {
            /* ATNUM */
            hb_retnl (0);
          }; break;
        }
        
        return;
      }

    }

    switch (iSwitch)
    {
      case DO_ATNUM_AFTERATNUM:
      {
        /* AFTERATNUM */
        if (pc+sMatchStrLen >= pcString+sStrLen)
          hb_retc ("");
        else
          hb_retclen (pc+sMatchStrLen, sStrLen-(pc+sMatchStrLen-pcString));
      }; break;

      case DO_ATNUM_BEFORATNUM:
      {
        /* BEFORATNUM */
        hb_retclen (pcString-sIgnore, pc-(pcString-sIgnore));
      }; break;

      case DO_ATNUM_ATNUM:
      {
        /* ATNUM */
        hb_retnl (pc-(pcString-sIgnore)+1);
      }; break;
    }
    
  }
  else /* ((ISCHAR (1)) && (ISCHAR (2))) */
  {
    switch (iSwitch)
    {
      case DO_ATNUM_AFTERATNUM:
      case DO_ATNUM_BEFORATNUM:
      {
        /* AFTERATNUM */
        PHB_ITEM pSubst = NULL;
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                                   (iSwitch == DO_ATNUM_AFTERATNUM ? CT_ERROR_AFTERATNUM : CT_ERROR_BEFORATNUM),
                                   NULL,
                                   (iSwitch == DO_ATNUM_AFTERATNUM ? "AFTERATNUM" : "BEFORATNUM"),
                                   0, EF_CANSUBSTITUTE, 4,
                                   hb_paramError (1), hb_paramError (2),
                                   hb_paramError (3), hb_paramError (4));
        }
        
        if (pSubst != NULL)
        {
          hb_itemReturn (pSubst);
          hb_itemRelease (pSubst);
        }
        else
        {
          hb_retc ("");
        }
      }; break;
        
      case DO_ATNUM_ATNUM:
      {
        /* ATNUM */
        PHB_ITEM pSubst = NULL;
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_ATNUM,
                                   NULL, "ATNUM", 0, EF_CANSUBSTITUTE, 4,
                                   hb_paramError (1), hb_paramError (2),
                                   hb_paramError (3), hb_paramError (4));
        }
        
        if (pSubst != NULL)
        {
          hb_itemReturn (pSubst);
          hb_itemRelease (pSubst);
        }
        else
        {
          hb_retnl (0);
        }
      }; break;
    }
    
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      AFTERATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns string portion after nth occurence of substring
 *  $SYNTAX$
 *      AFTERATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *                  [<nIgnore>] ) --> cRestString
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <cRestString>       the portion of <cString> after the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>
 *                          If such a rest does not exist, an empty string
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the portion of
 *      <cString> after that match will be returned. If there aren't enough
 *      matches or the last match is identical to the end of <cString>, an
 *      empty string will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? AFTERATNUM ("!", "What is the answer ? 4 ! 5 !") -> ""
 *      ? AFTERATNUM ("!", "What is the answer ? 4 ! 5 ?") -> " 5 ?"
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      AFTERATNUM ("..", "..This..is..a..test!") == "test!"
 *      AFTERATNUM ("..", "..This..is..a..test!", 2) == "is..a..test!"
 *      AFTERATNUM ("..", "..This..is..a..test!", 2, 2) == "a..test!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      AFTERATNUM() is compatible with CT3's AFTERATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is libct.
 *  $SEEALSO$
 *      ATNUM(),BEFORATNUM(),CSETATMUPA(),SETATLIKE()
 *  $END$
 */

HB_FUNC (AFTERATNUM)
{

  do_atnum (DO_ATNUM_AFTERATNUM);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      BEFORATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns string portion before nth occurence of substring
 *  $SYNTAX$
 *      BEFORATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *                  [<nIgnore>] ) --> cRestString
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <cRestString>       the portion of <cString> before the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>
 *                          If such a string does not exist, an empty string
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the portion of
 *      <cString> before that match will be returned. If there aren't enough
 *      matches or the last match is identical to the start of <cString>
 *      (i.e. the last match is the first match), an empty string will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? BEFORATNUM ("!", "What is the answer ? 4 ! 5 !") -> "What is the answer ? 4 ! 5 "
 *      ? BEFORATNUM ("!", "What is the answer ? 4 ! 5 ?") -> "What is the answer ? 4 "
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      BEFORATNUM ("..", "..This..is..a..test!") == "..This..is..a"
 *      BEFORATNUM ("..", "..This..is..a..test!", 2) == "..This"
 *      BEFORATNUM ("..", "..This..is..a..test!", 2, 2) == "..This..is"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      BEFORATNUM() is compatible with CT3's BEFORATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is ct3.
 *  $SEEALSO$
 *      ATNUM()  AFTERATNUM()  CSETATMUPA()  SETATLIKE()
 *  $END$
 */

HB_FUNC (BEFORATNUM)
{

  do_atnum (DO_ATNUM_BEFORATNUM);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      ATNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Returns the start position of the nth occurence of a substring in a string
 *  $SYNTAX$
 *      ATNUM (<cStringToMatch>, <cString>, [<nCounter>],
 *             [<nIgnore>] ) --> nPosition
 *  $ARGUMENTS$
 *      <cStringToMatch>    is the substring scanned for
 *      <cString>           is the scanned string
 *      [<nCounter>]        determines how many occurences are of
 *                          <cStringToMatch> in <cString> are searched
 *                          Default: search last occurence
 *      [<nIgnore>]         determines how many character from the start
 *                          should be ignored in the search
 *                          Default: 0
 *  $RETURNS$
 *      <nPosition>         the position of the <nCounter>th
 *                          occurence of <cStringToMatch> in <cString>.
 *                          If such an occurence does not exist, 0
 *                          is returned.
 *  $DESCRIPTION$
 *      This function scans <cString> for <cStringToMatch>. After the
 *      <nCounter>th match (or the last one, depending on the value of
 *      <nCounter>) has been found, the position of
 *      that match will be returned. If there aren't enough
 *      matches or there is no last match, 0 will be returned.
 *      After a match has been found, the function continues to scan after
 *      that match if the CSETATMUPA() switch is turned off, with the
 *      second character of the matched substring otherwise.
 *      The function will also consider the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? ATNUM ("!", "What is the answer ? 4 ! 5 !") -> 28
 *      ? ATNUM ("!", "What is the answer ? 4 ! 5 ?") -> 24
 *      <TODO: add some examples here with csetatmupa() and setatlike()>
 *  $TESTS$
 *      ATNUM ("..", "..This..is..a..test!") == 14
 *      ATNUM ("..", "..This..is..a..test!", 2) == 7
 *      ATNUM ("..", "..This..is..a..test!", 2, 2) == 11
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATNUM() is compatible with CT3's ATNUM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atnum.c, library is libct.
 *  $SEEALSO$
 *      ATNUM()  AFTERATNUM()  CSETATMUPA()  SETATLIKE()
 *  $END$
 */

HB_FUNC (ATNUM)
{

  do_atnum (DO_ATNUM_ATNUM);
  return;

}





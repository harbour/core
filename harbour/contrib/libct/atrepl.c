/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   ATREPL() CT3 string function
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
 *      ATREPL()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Search and replace sequences in a string
 *  $SYNTAX$
 *      ATREPL (<cStringToMatch>, <cString>, <cReplacement>, [<nCounter>],
 *              [<lMode>], [<nIgnore>]) --> cString
 *  $ARGUMENTS$
 *      <cStringToMatch>   is the substring searched for in <cString>
 *      <cString>          is the processed string
 *      <cReplacement>     is the replacement for sequences found 
 *      [<nCounter>]       specifies the number of replacements
 *                         Default: last occurence
 *      [<lMode>]          if set to .T., only the <nCounter>th sequence
 *                         of <cStringToMatch> will be replaced, else
 *                         all sequences will be replaced.
 *                         Default: .F.
 *      [<nIgnore>])       specifies how many characters in <cString> from
 *                         the beginning should be ignored by the function
 *                         Default: 0
 *  $RETURNS$
 *      <cString>
 *  $DESCRIPTION$
 *      The ATREPL() function searches and replaces sequences in a string.
 *      First, the function ignores the first <nIgnore> characters of <cString>.
 *      Then, if <lMode> is set to .T., it searches for the <nCounter>th
 *      occurence of <cStringToMatch> in <cString>. If successful, the
 *      sequence will be replaced with <cReplacement>.
 *      If <lMode> is set to .F., the same search is performed, but EVERY
 *      occurence of <cStringToMatch> till the <nCounter>th (inclusive) will
 *      be replaced with <cReplacement>. Note that, in this case,
 *      the replacements are performed even if the <nCounter>th occurence
 *      does not exist.
 *      By using the CSETATMUPA() switch you can decide whether the
 *      function restarts searching after a found sequence of after
 *      the first character of that sequence.
 *      The function allows the use of wildcards in <cStringToMatch>
 *      and looks for the settings of SETATLIKE().
 *  $EXAMPLES$
 *      ? ATREPL("ABC", "ABCDABCDABC", "xx")    --> "xxDxxDxx"
 *      ? ATREPL("ABC", "ABCDABC", "ZYXW")      --> "ZYXWDZYXW"
 *      ? ATREPL("ABC", "ABCDABCDABC", "xx", 2) --> "xxDxxDABC"
 *      ? ATREPL("ABC", "ABCDABCDABC", "xx", 2, .T.)  --> "ABCDxxDABC"
 *  $TESTS$
 *      ATREPL("ABC", "ABCDABCDABC", "xx") == "xxDxxDxx"
 *      ATREPL("ABC", "ABCDABC", "ZYXW") == "ZYXWDZYXW"
 *      ATREPL("ABC", "ABCDABCDABC", "xx", 2) == "xxDxxDABC"
 *      ATREPL("ABC", "ABCDABCDABC", "xx", 2, .T.) == "ABCDxxDABC"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATREPL() is compatible with CT3's ATREPL().
 *      Note the new, 6th parameter !
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is atrepl.c, library is ct3.
 *  $SEEALSO$
 *      CSETATMUPA()   SETATLIKE()
 *  $END$
 */

HB_FUNC (ATREPL)
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
    char *pc;

    char *pcReplacement;
    size_t sReplaceLen;
    int iReplaceMode;
    char *pcRetStr;
    size_t sRetStrLen;

    /* eventually ignore some characters */
    if (ISNUM (6))
      sIgnore = (size_t)hb_parnl (6);
    else
      sIgnore = 0;

    if (sIgnore >= sStrLen)
    {
      hb_retclen (pcString, sStrLen);
      return;
    }

    /* replacement */
    if (ISCHAR (3))
    {
      pcReplacement = hb_parc (3);
      sReplaceLen = hb_parclen (3);
    }
    else
    {
      pcReplacement = "";
      sReplaceLen = 0;
    }
  
    /* replace mode */
    if (ISLOG (5))
      iReplaceMode = hb_parl (5);
    else
      iReplaceMode = 0;

    /* nth match or last match ? */
    if (ISNUM (4))
      ulCounter = hb_parnl (4);
    else
      ulCounter = 0;

    /* little trick: */
    if ((iReplaceMode==0) && (ulCounter == 0))
      ulCounter = HB_MKULONG (255,255,255,255); /* ULONG_MAX */

    if (ulCounter != 0)
    {
     
      /* depending on iReplaceMode: replace all occurences including the nth one
         or only the nth occurence
         NOTE: if iReplaceMode = false and the nth occurence does not exist,
               all occurences are replaced */

      char *pcRetSubStr;
      size_t sRetSubStrLen;
      ULONG ulMatchCounter = 0;

      sRetStrLen = sStrLen;
      pcRetStr = hb_xgrab (sRetStrLen);
      hb_xmemcpy (pcRetStr, pcString, sRetStrLen);

      pcRetSubStr = pcRetStr+sIgnore;
      sRetSubStrLen = sRetStrLen-sIgnore;

      while (ulMatchCounter < ulCounter) 
      {
        switch (iAtLike)
        {
          case CT_SETATLIKE_EXACT:
          {
            pc = ct_at_exact_forward (pcRetSubStr, sRetSubStrLen,
                                      pcStringToMatch, sStrToMatchLen,
                                      &sMatchStrLen);
          }; break;

          case CT_SETATLIKE_WILDCARD:
          {
            pc = ct_at_wildcard_forward (pcRetSubStr, sRetSubStrLen,
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
          hb_retclen (pcRetStr, sRetStrLen);
          hb_xfree (pcRetStr);

          return;
        }

        ulMatchCounter++;

        /* replace match ? */
        if ((iReplaceMode == 0) || (ulMatchCounter == ulCounter))
        {

          if (sMatchStrLen < sReplaceLen)
          {
            /* pcRetStr grows, so realloc memory */
            /* save pc pointer */
            size_t sPCPos = pc-pcRetStr;

            pcRetStr = hb_xrealloc (pcRetStr, sRetStrLen+(sReplaceLen-sMatchStrLen));
            pc = pcRetStr+sPCPos;
          }

          if (sReplaceLen != sMatchStrLen)
            memmove (pc+sReplaceLen, pc+sMatchStrLen,
                     sRetStrLen-((pc+sMatchStrLen)-pcRetStr));
          if (sReplaceLen > 0)
            hb_xmemcpy (pc, pcReplacement, sReplaceLen);

          if (iMultiPass)
            pcRetSubStr = pc+1;
          else
            pcRetSubStr = pc+sReplaceLen;

          sRetStrLen += (sReplaceLen-sMatchStrLen);
      
        }
        else
        {
          if (iMultiPass)
            pcRetSubStr = pc+1;
          else
            pcRetSubStr = pc+sMatchStrLen;
        }
         
        sRetSubStrLen = sRetStrLen-(pcRetSubStr-pcRetStr);

      }

    }
    else /* (ulCounter != 0) */
    {
        
      /* find and replace last match */

      sRetStrLen = sStrLen;
      pcRetStr = hb_xgrab (sRetStrLen);
      hb_xmemcpy (pcRetStr, pcString, sRetStrLen);

      /* we have to find the last match and replace it */

      switch (iAtLike)
      {
        case CT_SETATLIKE_EXACT:
        {
          pc = ct_at_exact_backward (pcRetStr+sIgnore, sRetStrLen-sIgnore,
                                     pcStringToMatch, sStrToMatchLen,
                                     &sMatchStrLen);
        }; break;

        case CT_SETATLIKE_WILDCARD:
        {
          pc = ct_at_wildcard_backward (pcRetStr+sIgnore, sRetStrLen-sIgnore,
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
        hb_retclen (pcRetStr, sRetStrLen);
        hb_xfree (pcRetStr);

        return;
      }

      /* replace match */
      if (sMatchStrLen < sReplaceLen)
      {
        /* pcRetStr grows, so realloc memory */
        /* save pc pointer */
        size_t sPCPos = pc-pcRetStr;

        pcRetStr = hb_xrealloc (pcRetStr, sRetStrLen+(sReplaceLen-sMatchStrLen));
        pc = pcRetStr+sPCPos;
      }

      if (sReplaceLen != sMatchStrLen)
        memmove (pc+sReplaceLen, pc+sMatchStrLen,
                 sRetStrLen-((pc+sMatchStrLen)-pcRetStr));
      if (sReplaceLen > 0)
        hb_xmemcpy (pc, pcReplacement, sReplaceLen);

      sRetStrLen += (sReplaceLen-sMatchStrLen);

    }

    hb_retclen (pcRetStr, sRetStrLen);
    hb_xfree (pcRetStr);

  }
  else /* ((ISCHAR (1)) && (ISCHAR (2))) */
  {
    hb_retclen (hb_parc (2), hb_parclen (2));
  }

  return;

}



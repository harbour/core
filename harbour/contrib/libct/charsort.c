/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CHARSORT() CT3 string functions
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


/* statics */
static size_t ssCompareLen;  /* TODO: make this thread safe */
static size_t ssElementPos;  /* TODO: make this thread safe */
static int siDescend;        /* TODO: make this thread safe */

/* qsort function */
#ifdef __IBMCPP__
int extern _LNK_CONV
#else
static int
#endif
do_charsort (const void *p1, const void *p2)
{

  char *pc1 = (char *)p1;
  char *pc2 = (char *)p2;
  int iCmp;

  pc1 += ssElementPos;
  pc2 += ssElementPos;

  iCmp = strncmp (pc1, pc2, ssCompareLen);
  iCmp *= (siDescend ? -1 : 1);
  
  return (iCmp);

}

/*  $DOC$
 *  $FUNCNAME$
 *      CHARSORT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Sort sequences within a string.
 *  $SYNTAX$
 *      CHARSORT (<[@]cString>, [<nElementLength>], [<nCompareLength>],
 *                [<nIgnoreCharacters>], [<nElemenOffset>], [<nSortLength>],
 *                [<lDescending>]) -> cSortedString
 *  $ARGUMENTS$
 *      <[@]cString>             is the string that should be processed
 *      [<nElementLength>]       specifies the length of the elements that
 *                               should be sorted
 *                               Default: 1
 *      [<nCompareLength>]       specifies how many characters within one
 *                               element should be used for comparison
 *                               Default: <nElementLength>
 *      [<nIgnoreCharacters>]    specifies the number of characters at the
 *                               beginning of <cString> that should be ignored
 *                               in the sort process
 *                               Default: 0
 *      [<nElementOffset>]       specifies the offset of the comparison string
 *                               within a element
 *                               Default: 0
 *      [<nSortLength>]          specifies how many characters in <cString>,
 *                               starting from the <nIgnoreCharacters> position,
 *                               should be sorted
 *                               Default: len(cString)-nIgnoreCharacters
 *      [<lDescending>])         specifies whether the process should
 *                               sort descending or not
 *  $RETURNS$
 *      <cSortedString>          the string resulting from the sort process
 *  $DESCRIPTION$
 *      The CHARSORT function sorts the characters within a string <cString>.
 *      With the parameters <nIgnoreCharacters> and <nSortLength>, you can
 *      determine that only the substring from position <nIgnoreCharacters>+1
 *      to position <nIgnoreCharacters>+<nSortLength> within <cString> should
 *      be sorted.
 *      The sorting algorithm is determined with the other parameters.
 *      <nElementLength> specifies the length of one element, i.e. there are
 *      <nSortLength>/<nElementLength> elements that are sorted. Note that
 *      surplus characters are not sorted but stay at their position.
 *      To do the sorting, the function uses the Quicksort algorithm implemented
 *      in the C-lib qsort() function. This algorithm needs to know how to compare
 *      and order two elements. This is done by comparing the ASCII values of
 *      a substring within each element. This substring is determined by the
 *      parameters <nElementOffset> and <nCompareLength> and the order
 *      by <lDescending>.
 *      By setting the CSETREF() switch to .T., one can omit the return value
 *      of the function, but one must then pass <cString> by reference.
 *  $EXAMPLES$
 *      ? CHARSORT("qwert")                     // "eqrtw"
 *      ? CHARSORT("qwert", 2)                  // "erqwt"
 *      ? CHARSORT("b1a4a3a2a1", 2, 1)          // "a2a1a3a4b1"
 *      ? CHARSORT("XXXqwert", 1, 1, 3)         // "XXXeqrtw"
 *      ? CHARSORT("b1a4a3a2a1", 2, 1, 0, 1)    // "a1b1a2a3a4"
 *      ? CHARSORT("384172852", 1, 1, 0, 0, 4)  // "134872852"
 *      ? CHARSORT("qwert", .T.)                // "wtrqe"
 *  $TESTS$
 *      CHARSORT("qwert")                     == "eqrtw"
 *      CHARSORT("qwert", 2)                  == "erqwt"
 *      CHARSORT("b1a4a3a2a1", 2, 1)          == "a2a1a3a4b1"
 *      CHARSORT("XXXqwert", 1, 1, 3)         == "XXXeqrtw"
 *      CHARSORT("b1a4a3a2a1", 2, 1, 0, 1)    == "a1b1a2a3a4"
 *      CHARSORT("384172852", 1, 1, 0, 0, 4)  == "134872852"
 *      CHARSORT("qwert", .T.)                == "wtrqe"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      CHARSORT() is compatible with CT3's CHARSORT().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is charsort.c, library is ct3.
 *  $SEEALSO$
 *      CSETREF()
 *  $END$
 */

HB_FUNC (CHARSORT)
{

  int iNoRet;

  /* suppressing return value ? */
  iNoRet = ct_getref();

  /* param check I */
  if (ISCHAR (1))
  {

    /* get parameters */
    char *pcString = hb_parc (1);
    char *pcRet;
    size_t sStrLen = (size_t)hb_parclen (1);
    size_t sElementLen, sIgnore, sSortLen;

    if (ISNUM (2))
      sElementLen = hb_parnl (2);
    else
      sElementLen = 1;
    
    if (ISNUM (3))
      ssCompareLen = hb_parnl (3);
    else
      ssCompareLen = sElementLen;

    if (ISNUM (4))
      sIgnore = hb_parnl (4);
    else
      sIgnore = 0;

    if (ISNUM (5))
      ssElementPos = hb_parnl (5);
    else
      ssElementPos = 0;

    if (ISNUM (6))
      sSortLen = hb_parnl (6);
    else
      sSortLen = sStrLen-sIgnore;
    
    if (ISLOG (7))
      siDescend = hb_parl (7);
    else
      siDescend = 0;

    /* param check II */
    if ((sElementLen == 0) ||
        (ssCompareLen > sElementLen) || 
        (sIgnore+sElementLen > sStrLen) || 
        ((ssElementPos+ssCompareLen) > sElementLen) || 
        (sSortLen+sIgnore > sStrLen))
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                  NULL, "CHARSORT", 0, EF_CANDEFAULT, 7,
                  hb_paramError (1), hb_paramError (2),
                  hb_paramError (3), hb_paramError (4),
                  hb_paramError (5), hb_paramError (6),
                  hb_paramError (7));
      }
      if (iNoRet)
        hb_retl (0);
      else
        hb_retc ("");
      return;
    }

    pcRet = ( char * ) hb_xgrab (sStrLen);
    hb_xmemcpy (pcRet, pcString, sStrLen);

    qsort (pcRet+sIgnore, (sSortLen/sElementLen), sElementLen, do_charsort);

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
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_CHARSORT,
                               NULL, "CHARSORT", 0, EF_CANSUBSTITUTE, 7,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3), hb_paramError (4),
                               hb_paramError (5), hb_paramError (6),
                               hb_paramError (7));
    }

    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      if (iNoRet)
        hb_retl (0);
      else
        hb_retc ("");
    }
  }

}



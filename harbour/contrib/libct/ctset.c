/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 switch functions
 *
 *   - CSETREF()
 *   - CSETATMUPA()
 *   - SETATLIKE() 
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


/*  
 *  CSETREF() stuff
 */

static int siRefSwitch = 0; /* TODO: make this tread safe */

void ct_setref (int iNewSwitch)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setref(%i)",iNewSwitch));
  siRefSwitch = iNewSwitch;
  return;
}


int ct_getref (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getref()"));
  return (siRefSwitch);
}


/*  $DOC$
 *  $FUNCNAME$
 *      CSETREF()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Determine return value of reference sensitive CT3 string functions
 *  $SYNTAX$
 *      CSETREF ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *      [<lNewSwitch>]  .T. -> suppress return value
 *                      .F. -> do not suppress return value
 *  $RETURNS$
 *      lOldSwitch      old (if lNewSwitch is a logical value) or
 *                      current state of the switch
 *  $DESCRIPTION$
 *      Within the CT3 functions, the following functions do not
 *      change the length of a string passed as parameter while
 *      transforming this string:
 *
 *      ADDASCII()   BLANK()       CHARADD()
 *      CHARAND()    CHARMIRR()    CHARNOT()
 *      CHAROR()     CHARRELREP()  CHARREPL()
 *      CHARSORT()   CHARSWAP()    CHARXOR()
 *      CRYPT()      JUSTLEFT()    JUSTRIGHT()
 *      POSCHAR()    POSREPL()     RANGEREPL()
 *      REPLALL()    REPLLEFT()    REPLRIGHT()
 *      TOKENLOWER() TOKENUPPER()  WORDREPL()
 *      WORDSWAP()
 *
 *      Thus, these functions allow to pass the string by reference [@] to
 *      the function so that it may not be necessary to return the transformed
 *      string. By calling CSETREF (.T.), the above mentioned functions return
 *      the value .F. instead of the transformed string if the string is
 *      passed by reference to the function.
 *      The switch is turned off (.F.) by default.
 *
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is fully CT3 compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctset.c.
 *  $SEEALSO$
 *      ADDASCII()   BLANK()       CHARADD()
 *      CHARAND()    CHARMIRR()    CHARNOT()
 *      CHAROR()     CHARRELREP()  CHARREPL()
 *      CHARSORT()   CHARSWAP()    CHARXOR()
 *      CRYPT()      JUSTLEFT()    JUSTRIGHT()
 *      POSCHAR()    POSREPL()     RANGEREPL()
 *      REPLALL()    REPLLEFT()    REPLRIGHT()
 *      TOKENLOWER() TOKENUPPER()  WORDREPL()
 *      WORDSWAP()
 *  $END$
 */

HB_FUNC (CSETREF)
{

  hb_retl (ct_getref());

  if (ISLOG (1))
    ct_setref (hb_parl (1));

  return;

}


/*
 * CSETATMUPA() stuff
 */

static int siAtMupaSwitch = 0; /* TODO: make this tread safe */

void ct_setatmupa (int iNewSwitch)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setatmupa(%i)",iNewSwitch));
  siAtMupaSwitch = iNewSwitch;
  return;
}


int ct_getatmupa (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getatmupa()"));
  return (siAtMupaSwitch);
}


/*  $DOC$
 *  $FUNCNAME$
 *      CSETATMUPA()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Determine "multi-pass" behaviour in some string functions
 *  $SYNTAX$
 *      CSETATMUPA ([<lNewSwitch>]) -> lOldSwitch
 *  $ARGUMENTS$
 *      [<lNewSwitch>]  .T. -> turn "multi-pass" on
 *                      .F. -> turn "multi-pass" off
 *  $RETURNS$
 *      lOldSwitch      old (if lNewSwitch is a logical value) or
 *                      current state of the switch
 *  $DESCRIPTION$
 *      CSETATMUPA determines how the following CT3 string functions
 *      
 *      ATNUM()       AFTERATNUM()  BEFORATNUM()
 *      ATREPL()      NUMAT()       ATADJUST()
 *      WORDTOCHAR()  WORDREPL()
 *
 *      perform their work. See the respective function documentation for a
 *      further description how the switch influences these functions.
 *
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is fully CT3 compatible.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctset.c.
 *  $SEEALSO$
 *      ATNUM()       AFTERATNUM()  BEFORATNUM()
 *      ATREPL()      NUMAT()       ATADJUST()
 *      WORDTOCHAR()  WORDREPL()
 *  $END$
 */


HB_FUNC (CSETATMUPA)
{

  hb_retl (ct_getatmupa());

  if (ISLOG (1))
    ct_setatmupa (hb_parl (1));

  return;

}


/*
 * SETATLIKE() stuff
 */

static int siAtLikeMode = 0;   /* TODO: make this tread safe */
static int scAtLikeChar = '?'; /* TODO: make this tread safe */

void ct_setatlike (int iNewMode)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setatlike(%i)",iNewMode));
  siAtLikeMode = iNewMode;
  return;
}


int ct_getatlike (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getatlike()"));
  return (siAtLikeMode);
}


void ct_setatlikechar (char cNewChar)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_setatlikechar(\'%c\')",cNewChar));
  scAtLikeChar = cNewChar;
  return;
}


char ct_getatlikechar (void)
{
  HB_TRACE(HB_TR_DEBUG, ("ct_getatlikechar()"));
  return (scAtLikeChar);
}


/*  $DOC$
 *  $FUNCNAME$
 *      SETATLIKE()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Determine scan behaviour in some string functions
 *  $SYNTAX$
 *      SETATLIKE ([<nMode>] [, <[@]cWildcard>]) --> nOldMode
 *  $ARGUMENTS$
 *      [<nMode>]   CT_SETATLIKE_EXACT    -> characters are compared exactly
 *                  CT_SETATLIKE_WILDCARD -> characters are compared using
 *                                           a wildcard character
 *                  The default value is CT_SETATLIKE_EXACT.
 *      [<[@]cWildcard>]  determines the character that is subsequently used
 *                        as a wildcard character for substring scanning.
 *                        The default value is "?".
 *                        NEW: If this parameter is passed by reference [@],
 *                        the current wildcard character is stored in
 *                        <cWildcard>.
 *  $RETURNS$
 *      nOldMode          old (if nMode is a numeric value) or
 *                        current state of the switch
 *  $DESCRIPTION$
 *      In the following CT3 functions, strings are compared on a character
 *      base:
 *
 *      ATADJUST()    ATNUM()    AFTERATNUM()
 *      BEFOREATNUM() ATREPL()   NUMAT()
 *      STRDIFF()
 *
 *      With the SETATLIKE function, one can determine when characters are
 *      considered to match within these functions. If CT_SETATLIKE_WILDCARD
 *      is set (e.g. "?"), then "?" matches every other character.
 *
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      This function is fully CT3 compatible, but allows to pass the
 *      second parameter by reference so that the current wildcard character
 *      can be determined.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is ctset.c, header is ct.ch.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC (SETATLIKE)
{

  hb_retni (ct_getatlike());

  /* set new mode if first parameter is CT_SETATLIKE_EXACT (==0)
                                     or CT_SETATLIKE_WILDCARD (==1) */
  if (ISNUM (1))
  {
    int iNewMode = hb_parni (1);
    if ((iNewMode == CT_SETATLIKE_EXACT) ||
        (iNewMode == CT_SETATLIKE_WILDCARD))
      ct_setatlike (iNewMode);
  }

  /* set new wildcard character, if ISCHAR(2) but !ISBYREF(2) */
  if (ISCHAR (2))
  {
    if (ISBYREF (2))
    {
      /* new behaviour: store the current wildcard char in second parameter */
      char cResult;
      cResult = ct_getatlikechar();
      hb_storclen (&cResult, 1, 2);
    }
    else
    {
      char *pcNewChar = hb_parc (2);
      if (hb_parclen (2) > 0)
        ct_setatlikechar (*pcNewChar);
    }
  }

  return;

}

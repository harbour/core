/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - TOKENINIT()
 *     - TOKENEXIT()
 *     - TOKENNEXT()
 *     - TOKENNUM()
 *     - TOKENAT()
 *     - SAVETOKEN()
 *     - RESTTOKEN()
 *     - TOKENEND()
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


/* ==================================================================== */
/* static functions for token environment management                    */
/* ==================================================================== */


#define TOKEN_ENVIRONMENT_STEP 100

typedef struct _TOKEN_POSITION
{
  size_t sStartPos;  /* relative 0-based index of first char of token */
  size_t sEndPos;    /* relative 0-based index of first char BEHIND token,
                        so that length = sEndPos-sStartPos */
} TOKEN_POSITION;
typedef TOKEN_POSITION * TOKEN_ENVIRONMENT;


/* -------------------------------------------------------------------- */
/* alloc new token environment                                          */
/* -------------------------------------------------------------------- */
static TOKEN_ENVIRONMENT sTokEnvNew (void)
{
  
  TOKEN_ENVIRONMENT env = (TOKEN_ENVIRONMENT)hb_xalloc (sizeof (TOKEN_POSITION)*(2+TOKEN_ENVIRONMENT_STEP));

  if (env == NULL)
  {
    return (NULL);
  }

  /* use the first element to store current length and use of token env */
  env[0].sStartPos = 0;  /* 0-based index to next free, unused element */
  env[0].sEndPos = 100;  /* but there are 100 elements ready for use */

  /* use second element to store actual index with tokennext() */
  env[1].sStartPos = 0;  /* 0-based index value that is to be used NEXT */

  return (env);

}

/* -------------------------------------------------------------------- */
/* add a tokenizing position to a token environment
/* -------------------------------------------------------------------- */

static int sTokEnvAddPos (TOKEN_ENVIRONMENT env, TOKEN_POSITION *pPos)
{

  size_t index;

  /* new memory needed ? */
  if (env[0].sStartPos == env[0].sEndPos)
  {
    env = (TOKEN_ENVIRONMENT)hb_xrealloc (env,
                                          sizeof (TOKEN_POSITION)*
                                          (2+env[0].sEndPos+TOKEN_ENVIRONMENT_STEP));
    if (env == NULL)
    {
      return (0);
    }

    env[0].sEndPos += TOKEN_ENVIRONMENT_STEP;
  }

  index = env[0].sStartPos+2;  /* +2  because of extra elements */
  env[index].sStartPos = pPos->sStartPos;
  env[index].sEndPos   = pPos->sEndPos;
  env[0].sStartPos++;

  return (1);

}

/* -------------------------------------------------------------------- */
/* check to see if token pointer is at end of environment               */
/* -------------------------------------------------------------------- */

static int sTokEnvEnd (TOKEN_ENVIRONMENT env)
{
  return (env[1].sStartPos>=env[0].sStartPos);
}

/* -------------------------------------------------------------------- */
/* get size of token environment in memory                              */
/* -------------------------------------------------------------------- */

static size_t sTokEnvGetSize (TOKEN_ENVIRONMENT env)
{

  return (sizeof (TOKEN_POSITION)*
          (2+env[0].sEndPos));

}

/* -------------------------------------------------------------------- */
/* get position element pointed to by tokenizing pointer                */
/* -------------------------------------------------------------------- */

static TOKEN_POSITION *sTokEnvGetPos (TOKEN_ENVIRONMENT env)
{

  if (env[1].sStartPos>=env[0].sStartPos)
  {
    return (NULL);
  }

  return (env+2+(env[1].sStartPos));  /* "+2" because of extra elements */

}

/* -------------------------------------------------------------------- */
/* get position element pointed to by given 0-based index               */
/* -------------------------------------------------------------------- */

static TOKEN_POSITION *sTokEnvGetPosIndex (TOKEN_ENVIRONMENT env, size_t index)
{

  if (index>=env[0].sStartPos)
  {
    return (NULL);
  }

  return (env+2+index); /* "+2" because of extra elements */

}

/* -------------------------------------------------------------------- */
/* increment tokenizing pointer by one                                  */
/* -------------------------------------------------------------------- */

static int sTokEnvIncPtr (TOKEN_ENVIRONMENT env)
{

  if (env[1].sStartPos>=env[0].sStartPos)
  {
    return (0);
  }
  else
  {
    env[1].sStartPos++;
    return (1);
  }

}

/* -------------------------------------------------------------------- */
/* set tokenizing pointer to 0-based value                              */
/* -------------------------------------------------------------------- */

static int sTokEnvSetPtr (TOKEN_ENVIRONMENT env, size_t sCnt)
{
  
  if (sCnt >= env[0].sStartPos)
  {
    return (0);
  }
  else
  {
    env[1].sStartPos = sCnt;
    return (1);
  }

}

/* -------------------------------------------------------------------- */
/* decrement tokenizing pointer by one                                  */
/* -------------------------------------------------------------------- */

static int sTokEnvDecPtr (TOKEN_ENVIRONMENT env)
{

  if (env[1].sStartPos <= 0)
  {
    return (0);
  }
  else
  {
    env[1].sStartPos--;
    return (1);
  }

}

/* -------------------------------------------------------------------- */
/* get value of tokenizing pointer                                      */
/* -------------------------------------------------------------------- */

static size_t sTokEnvGetPtr (TOKEN_ENVIRONMENT env)
{

  return (env[1].sStartPos);

}

/* -------------------------------------------------------------------- */
/* get token count                                                      */
/* -------------------------------------------------------------------- */

static size_t sTokEnvGetCnt (TOKEN_ENVIRONMENT env)
{

  return (env[0].sStartPos);

}

/* -------------------------------------------------------------------- */
/* free token environment                                               */
/* -------------------------------------------------------------------- */

static void sTokEnvDel (TOKEN_ENVIRONMENT env)
{

  hb_xfree (env);

}

/* ==================================================================== */
/* HARBOUR functions                                                    */
/* ==================================================================== */

/* static data */
/* TODO: make thread safe */
static const char *spcSeparatorStr = "\x00""\x09""\x0A""\x0C""\x1A""\x20""\x8A""\x8C"",.;:!\?/\\<>()#&%+-*";
static const size_t ssSeparatorStrLen = 26;

static TOKEN_ENVIRONMENT ssTokenEnvironment = NULL;


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENINIT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Initializes a token environment
 *  $SYNTAX$
 *      TOKENINIT (<[@]cString>], [<cTokenizer>], [<nSkipWidth>],
 *                 [<@cTokenEnvironment>]) -> lState
 *  $ARGUMENTS$
 *      <[@]cString>          is the processed string
 *      <cTokenizer>          is a list of characters separating the tokens
 *                            in <cString>
 *                            Default: chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+
 *                                     chr(32)+chr(32)+chr(138)+chr(141)+
 *                                     ",.;:!\?/\\<>()#&%+-*"
 *      <nSkipWidth>          specifies the maximum number of successive
 *                            tokenizing characters that are combined as
 *                            ONE token stop, e.g. specifying 1 can
 *                            yield to empty token
 *                            Default: 0, any number of successive tokenizing
 *                            characters are combined as ONE token stop
 *      <@cTokenEnvironment>  is a token environment stored in a binary
 *                            encoded string
 *  $RETURNS$
 *      <lState>              success of the initialization
 *  $DESCRIPTION$
 *      The TOKENINIT() function initializes a token environment. A token
 *      environment is the information about how a string is to be tokenized.
 *      This information is created in the process of tokenization of the
 *      string <cString> - equal to the one used in the TOKEN() function
 *      with the help of the <cTokenizer> and <nSkipWidth> parameters.
 *
 *      This token environment can be very useful when large strings have
 *      to be tokenized since the tokenization has to take place only once
 *      whereas the TOKEN() function must always start the tokenizing process
 *      from scratch.
 * 
 *      Unlike CTIII, this function provides two mechanisms of storing the
 *      resulting token environment. If a variable is passed by reference
 *      as 4th parameter, the token environment is stored in this variable,
 *      otherwise the global token environment is used. Do not modify the
 *      token environment string directly !
 *
 *      Additionally, a counter is stored in the token environment, so that
 *      the tokens can successivly be obtained. This counter is first set to 1.
 *      When the TOKENINIT() function is called without a string a tokenize,
 *      the counter of either the global environment or the environment given
 *      by reference in the 4th parameter is rewind to 1.
 *
 *      Additionally, unlike CTIII, tokeninit() does not need the string
 *      <cString> to be passed by reference, since one must provide the
 *      string in calls to TOKENNEXT() again.
 *  $EXAMPLES$
 *      tokeninit (cString)   // tokenize the string <cString> with default
 *                            // rules and store the token environment globally
 *                            // and eventually delete an old global TE
 *      tokeninit (@cString)  // no difference in result, but eventually faster,
 *                            // since the string must not be copied
 *      tokeninit()           // rewind counter of global TE to 1
 *      tokeninit ("1,2,3",",",1) // tokenize constant string, store in global TE
 *      tokeninit (cString,,1,@cTE1)  // tokenize cString and store TE in
 *                                    // cTE1 only without overriding global TE
 *      tokeninit (cString,,1,cTE1)  // tokenize cString and store TE in
 *                                   // GLOBAL TE since 4th parameter is
 *                                   // not given by reference !!!
 *      tokeninit (,,,@cTE1)         // set counter in TE stored in cTE1 to 1
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENINIT() is compatible with CTIII's TOKENINIT(),
 *      but there is an additional parameter featuring local token environments.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),TOKENAT(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (TOKENINIT)
{

  if (ISCHAR (1))
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    char *pcSeparatorStr;
    size_t sSeparatorStrLen;
    ULONG ulSkipCnt, ulSkip;

    char *pcSubStr, *pc;
    size_t sSubStrLen;
    
    TOKEN_ENVIRONMENT sTokenEnvironment;
    TOKEN_POSITION sTokenPosition;

    /* separator string */
    if (ISCHAR (2) && ((sSeparatorStrLen = hb_parclen (2)) != 0))
    {
      pcSeparatorStr = hb_parc (2);
    }
    else
    {
      pcSeparatorStr = (char *)spcSeparatorStr;
      sSeparatorStrLen = ssSeparatorStrLen;
    }

    /* skip width */
    if (ISNUM (3))
    {
      ulSkip = hb_parnl (3);
    }
    else
    {
      ulSkip = HB_MKULONG (255,255,255,255);
    }
    if (ulSkip == 0)
    {
      ulSkip = HB_MKULONG (255,255,255,255);
    }

    /* allocate new token environment */
    if ((sTokenEnvironment = sTokEnvNew()) == NULL)
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                  NULL, "TOKENINIT", 0, EF_CANDEFAULT, 4,
                  hb_paramError (1), hb_paramError (2),
                  hb_paramError (3), hb_paramError (4));
      }

      hb_retl (0);
      return;
    }
      
    pcSubStr = pcString;
    sSubStrLen = sStrLen;

    /* scan start condition */
    pc = pcSubStr-1;

    while (1)
    {
    
      size_t sMatchedPos = sSeparatorStrLen;

      /* ulSkip */
      ulSkipCnt = 0;
      do
      {
        sSubStrLen -= (pc-pcSubStr)+1;
        pcSubStr = pc+1;
        pc = ct_at_charset_forward (pcSubStr, sSubStrLen,
                                    pcSeparatorStr, sSeparatorStrLen,
                                    &sMatchedPos);
        ulSkipCnt++;
      } while ((ulSkipCnt < ulSkip) && (pc == pcSubStr));

      if (sSubStrLen == 0)
        break;
      
      sTokenPosition.sStartPos = pcSubStr-pcString;
      if (pc == NULL)
      {
        sTokenPosition.sEndPos = pcSubStr-pcString+sSubStrLen;
      }
      else
      {
        sTokenPosition.sEndPos = pc-pcString;
      }

      if (!sTokEnvAddPos (sTokenEnvironment, &sTokenPosition))
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                    NULL, "TOKENINIT", 0, EF_CANDEFAULT, 4,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3), hb_paramError (4));
        }

        sTokEnvDel (sTokenEnvironment);
        hb_retl (0);
        return;
      }
        
      if (pc == NULL)
        break;
 
    } /* while (1);

    /* save token environment to 4th parameter OR to the static */
    if (ISBYREF (4))
    {
      hb_storclen ((char *)sTokenEnvironment,
                   sTokEnvGetSize (sTokenEnvironment), 4);
      sTokEnvDel (sTokenEnvironment);
    }
    else
    {
      if (ssTokenEnvironment != NULL)
        sTokEnvDel (ssTokenEnvironment);
      ssTokenEnvironment = sTokenEnvironment;
    }

    hb_retl (1);

  }
  else /* ISCHAR (1) */
  {
    
    /* if there is a token environment stored in either the 4th parameter or
       in the static variable -> rewind to first token */
    TOKEN_ENVIRONMENT sTokenEnvironment;

    if (ISCHAR (4) && ISBYREF (4))
    {
      sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_parc (4);
    }
    else
    {
      sTokenEnvironment = ssTokenEnvironment;
    }
    
    if (sTokenEnvironment != NULL)
    {
      /* rewind to first token */
      hb_retl (sTokEnvSetPtr (sTokenEnvironment, 0));
      if (ISCHAR (4) && ISBYREF (4))
      {
        hb_storclen ((char *)sTokenEnvironment,sTokEnvGetSize(sTokenEnvironment),4);
      }
    }
    else
    {
      /* nothing to rewind -> return .f. */ 
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                                 CT_ERROR_TOKENINIT, NULL, "TOKENINIT",
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
        hb_retl (0);
      }
    }

  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENNEXT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Successivly obtains tokens from a string
 *  $SYNTAX$
 *      TOKENNEXT (<[@]cString>, [<nToken>],
 *                 [<@cTokenEnvironment>]) -> cToken
 *  $ARGUMENTS$
 *      <[@]cString>             the processed string
 *      <nToken>                 a token number
 *      <@cTokenEnvironment>     a token environment
 *  $RETURNS$
 *      <cToken>                 a token from <cString>
 *  $DESCRIPTION$
 *      With TOKENNEXT(), the tokens determined with the TOKENINIT() functions
 *      can be retrieved. To do this, TOKENNEXT() uses the information stored
 *      in either the global token environment or the local one supplied by
 *      <cTokenEnvironment>. Note that, is supplied, this 3rd parameter has
 *      always to be passed by reference.
 *
 *      If the 2nd parameter, <nToken> is given, TOKENNEXT() simply returns
 *      the <nToken>th token without manipulating the TE counter. Otherwise
 *      the token pointed to by the TE counter is returned and the counter
 *      is incremented by one. Like this, a simple loop with TOKENEND() can
 *      be used to retrieve all tokens of a string successivly.
 *
 *      Note that <cString> does not have to be the same used in TOKENINIT(),
 *      so that one can do a "correlational tokenization", i.e. tokenize a string
 *      as if it was another! E.G. using TOKENINIT() with the string
 *      "AA,BBB" but calling TOKENNEXT() with "CCCEE" would
 *      give first "CC" and then "EE" (because "CCCEE" is not long enough).
 *  $EXAMPLES$
 *      // default behavhiour
 *      tokeninit (cString) // initialize a TE
 *      do while (!tokenend())
 *        ? tokennext (cString)  // get all tokens successivly
 *      enddo
 *      ? tokennext (cString, 3)  // get the 3rd token, counter will remain the same
 *      tokenexit()              // free the memory used for the global TE
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENNEXT() is compatible with CTIII's TOKENNEXT(),
 *      but there are two additional parameters featuring local token
 *      environments and optional access to tokens.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNUM(),TOKENAT(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (TOKENNEXT)
{

  if (ISCHAR (1))
  {
    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);

    TOKEN_ENVIRONMENT sTokenEnvironment;
    TOKEN_POSITION *psTokenPosition;

    /* token environment by parameter ... */
    if (ISCHAR (3) && ISBYREF(3))
    {
      size_t sStrLen3 = (size_t)hb_parclen (3);
      
      if (sStrLen3 < sizeof (TOKEN_POSITION)*2)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                    NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        }

        hb_retc ("");
        return;
      }
      sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_xgrab (sStrLen3);
      hb_xmemcpy ((char *)sTokenEnvironment, hb_parc (3), sStrLen3);

    }
    else
    {

      /* ... or static  ? */
      if (ssTokenEnvironment == NULL)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                    NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                    hb_paramError (1), hb_paramError (2),
                    hb_paramError (3));
        }

        hb_retc ("");
        return;
      }
      sTokenEnvironment = ssTokenEnvironment;
    
    }

    /* nth token or next token ?  */
    if (ISNUM (2))
    {
      psTokenPosition = sTokEnvGetPosIndex (sTokenEnvironment,
                                            hb_parnl (2)-1);
      /* no increment here */
    }
    else
    {
      psTokenPosition = sTokEnvGetPos (sTokenEnvironment);
      /* increment counter */
      sTokEnvIncPtr (sTokenEnvironment);
    }

    if ((psTokenPosition == NULL) ||
        (sStrLen <= psTokenPosition->sStartPos)) 
    {
      int iArgErrorMode = ct_getargerrormode();
      if (iArgErrorMode != CT_ARGERR_IGNORE)
      {
        ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                  NULL, "TOKENNEXT", 0, EF_CANDEFAULT, 3,
                  hb_paramError (1), hb_paramError (2),
                  hb_paramError (3));
      }

      if (ISCHAR (3) && ISBYREF (3))
      {
        hb_storclen ((char *)sTokenEnvironment,sTokEnvGetSize(sTokenEnvironment),3);
        hb_xfree ((char *)sTokenEnvironment);
      }
      hb_retc ("");
      return;
    }

    if (sStrLen < psTokenPosition->sEndPos)
    {
      hb_retclen (pcString+psTokenPosition->sStartPos,
                  sStrLen-(psTokenPosition->sStartPos));
    }
    else
    {
      hb_retclen (pcString+psTokenPosition->sStartPos,
                  (psTokenPosition->sEndPos)-(psTokenPosition->sStartPos));
    }
    
    if (ISCHAR (3) && ISBYREF (3))
    {
      hb_storclen ((char *)sTokenEnvironment,sTokEnvGetSize(sTokenEnvironment),3);
      hb_xfree ((char *)sTokenEnvironment);
    }

  }
  else
  {
    /* no string given, no token returns */
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               CT_ERROR_TOKENNEXT, NULL, "TOKENNEXT",
                               0, EF_CANSUBSTITUTE, 3,
                               hb_paramError (1), hb_paramError (2),
                               hb_paramError (3));
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
  }
        
}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENNUM()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Get the total number of tokens in a token environment
 *  $SYNTAX$
 *      TOKENNUM ([<@cTokenEnvironment>]) -> nNumberofTokens
 *  $ARGUMENTS$
 *      <@cTokenEnvironment>     a token environment
 *  $RETURNS$
 *      <nNumberofTokens>        number of tokens in the token environment
 *  $DESCRIPTION$
 *      The TOKENNUM() function can be used to retrieve the total number
 *      of tokens in a token environment.
 *      If the parameter <@cTokenEnvironment> is supplied (must be by
 *      reference), the information from this token environment is used,
 *      otherwise the global TE is used.
 *  $EXAMPLES$
 *      tokeninit ("a.b.c.d", ".", 1)  // initialize global TE
 *      ? tokennum()  // --> 4   
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENNUM() is a new function in Harbour's CTIII library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENAT(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (TOKENNUM)
{

  TOKEN_ENVIRONMENT sTokenEnvironment;

  if (ISCHAR (1) && ISBYREF (1))
  {
    sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_parc (1);
  }
  else
  {
    sTokenEnvironment = ssTokenEnvironment;
  }

  if ((void *)sTokenEnvironment != NULL)
  {
    hb_retnl (sTokEnvGetCnt (sTokenEnvironment));
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               CT_ERROR_TOKENNUM, NULL, "TOKENNUM",
                               0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
  }
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENEND()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Check whether additional tokens are available with TOKENNEXT()
 *  $SYNTAX$
 *      TOKENEND ([<@cTokenEnvironment>]) -> lTokenEnd
 *  $ARGUMENTS$
 *      <@cTokenEnvironment>     a token environment
 *  $RETURNS$
 *      <lTokenEnd>              .T., if additional tokens are available
 *  $DESCRIPTION$
 *      The TOKENEND() function can be used to check whether the next
 *      call to TOKENNEXT() would return a new token. This can not be
 *      decided with TOKENNEXT() alone, since an empty token cannot be
 *      distinguished from a "no more" tokens.
 *      If the parameter <@cTokenEnvironment> is supplied (must be by
 *      reference), the information from this token environment is used,
 *      otherwise the global TE is used.
 *      With a combination of TOKENEND() and TOKENNEXT(), all tokens from a
 *      string can be retrieved successivly (see example).
 *  $EXAMPLES$
 *      tokeninit ("a.b.c.d", ".", 1)  // initialize global TE
 *      do while (!tokenend())
 *        ? tokennext ("a.b.c.d")  // get all tokens successivly
 *      enddo
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENEND() is compatible with CTIII's TOKENEND(),
 *      but there are is an additional parameter featuring local token environments.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),TOKENAT(),SAVETOKEN(),RESTTOKEN()
 *  $END$
 */

HB_FUNC (TOKENEND)
{
  
  TOKEN_ENVIRONMENT sTokenEnvironment;

  if (ISCHAR (1) && ISBYREF (1))
    sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_parc (1);
  else
    sTokenEnvironment = ssTokenEnvironment;

  if ((void *)sTokenEnvironment != NULL)
  {
    hb_retl (sTokEnvEnd (sTokenEnvironment));
  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               CT_ERROR_TOKENEND, NULL, "TOKENEND",
                               0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
    }

    if (pSubst != NULL)
    {
      hb_itemReturn (pSubst);
      hb_itemRelease (pSubst);
    }
    else
    {
      /* it is CTIII behaviour to return .T. if there's no string TOKENINIT'ed */
      hb_retl (1);
    }
  }
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENEXIT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Release global token environment
 *  $SYNTAX$
 *      TOKENEXIT () -> lStaticEnvironmentReleased
 *  $ARGUMENTS$
 *  $RETURNS$
 *      <lStaticEnvironmentReleased>   .T., if global token environment is successfully released
 *  $DESCRIPTION$
 *      The TOKENEXIT() function releases the memory associated with the
 *      global token environment. One should use it for every tokeninit()
 *      using the global TE. Additionally, TOKENEXIT() is implicitly called
 *      from CTEXIT() to free the memory at library shutdown.
 *  $EXAMPLES$
 *      tokeninit (cString) // initialize a TE
 *      do while (!tokenend())
 *        ? tokennext (cString)  // get all tokens successivly
 *      enddo
 *      ? tokennext (cString, 3)  // get the 3rd token, counter will remain the same
 *      tokenexit()              // free the memory used for the global TE
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENEXIT() is a new function in Harbour's CTIII library.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENNEXT(),TOKENNUM(),TOKENAT(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (TOKENEXIT)
{

  if (ssTokenEnvironment != NULL)
  {
    sTokEnvDel (ssTokenEnvironment);
    ssTokenEnvironment = NULL;
    hb_retl (1);
  }
  else
  {
    hb_retl (0);
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENAT()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Get start and end positions of tokens in a token environment
 *  $SYNTAX$
 *      TOKENAT ([<lSeparatorPositionBehindToken>], [<nToken>],
 *               [<@cTokenEnvironment>]) -> nPosition
 *  $ARGUMENTS$
 *      <lSeparatorPositionBehindToken>   .T., if TOKENAT() should return
 *                                        the position of the separator character
 *                                        BEHIND the token.
 *                                        Default: .F., return start position of a token.
 *      <nToken>                          a token number
 *      <@cTokenEnvironment>              a token environment
 *  $RETURNS$
 *      <nPosition>
 *  $DESCRIPTION$
 *      The TOKENAT() function is used to retrieve the start and end position
 *      of the tokens in a token environment. Note however that the position of
 *      last character of a token is given by tokenat (.T.)-1 !!
 *
 *      If the 2nd parameter, <nToken> is given, TOKENAT() returns the
 *      positions of the <nToken>th token. Otherwise
 *      the token pointed to by the TE counter, i.e. the token that will
 *      be retrieved by TOKENNEXT() _NEXT_ is used.
 *
 *      If the parameter <@cTokenEnvironment> is supplied (must be by
 *      reference), the information from this token environment is used,
 *      otherwise the global TE is used.
 *  $EXAMPLES$
 *  $TESTS$
 *      tokeninit (cString) // initialize a TE
 *      do while (!tokenend())
 *        ? "From", tokenat(), "to", tokenat(.T.)-1
 *        ? tokennext (cString)  // get all tokens successivly
 *      enddo
 *      ? tokennext (cString, 3)  // get the 3rd token, counter will remain the same
 *      tokenexit()              // free the memory used for the global TE
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENAT() is compatible with CTIII's TOKENAT(),
 *      but there are two additional parameters featuring local token
 *      environments and optional access to tokens.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),SAVETOKEN(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (TOKENAT)
{

  int iSeparatorPos = 0;
  size_t sCurrentIndex;
  TOKEN_ENVIRONMENT sTokenEnvironment;
  TOKEN_POSITION *psTokenPosition;

  if (ISLOG (1))
    iSeparatorPos = hb_parl (1);

  if (ISCHAR (3) && ISBYREF(3))
    sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_parc (3);
  else
    sTokenEnvironment = ssTokenEnvironment;

  if ((void *)sTokenEnvironment == NULL) 
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                NULL, "TOKENAT", 0, EF_CANDEFAULT, 3,
                hb_paramError (1), hb_paramError (2),
                hb_paramError (3));
    }

    hb_retnl (0);
    return;
  }

  if (ISNUM (2))
    sCurrentIndex = hb_parnl (2)-1;
  else
    sCurrentIndex = sTokEnvGetPtr (sTokenEnvironment);

  psTokenPosition = sTokEnvGetPosIndex (sTokenEnvironment, sCurrentIndex);
  if (psTokenPosition == NULL)
  {
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      ct_error ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                NULL, "TOKENAT", 0, EF_CANDEFAULT, 3,
                hb_paramError (1), hb_paramError (2),
                hb_paramError (3));
    }
    hb_retnl (0);
    return;
  }

  if (iSeparatorPos)
    hb_retnl (psTokenPosition->sEndPos+1);
  else
    hb_retnl (psTokenPosition->sStartPos+1);

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      SAVETOKEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Save the global token environment
 *  $SYNTAX$
 *      SAVETOKEN () -> cStaticTokenEnvironment
 *  $ARGUMENTS$
 *  $RETURNS$
 *      <cStaticTokenEnvironment>   a binary string encoding the global TE
 *  $DESCRIPTION$
 *      The SAVETOKEN() function can be used to store the global TE for future
 *      use or when two or more incremental tokenizers must the nested.
 *      Note however that the latter can now be solved with locally stored
 *      token environments.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      SAVETOKEN() is compatible with CTIII's SAVETOKEN(),
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),TOKENAT(),RESTTOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (SAVETOKEN)
{

  if (ssTokenEnvironment != NULL)
  {
    hb_retclen ((char *)ssTokenEnvironment, sTokEnvGetSize (ssTokenEnvironment));
  }
  else
  {
    hb_retc ("");
  }

  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      RESTTOKEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Restore global token environment
 *  $SYNTAX$
 *      RESTTOKEN (<cStaticTokenEnvironment>) -> cOldStaticEnvironment
 *  $ARGUMENTS$
 *      <cStaticTokenEnvironment>     a binary string encoding a TE
 *  $RETURNS$
 *      <cOldStaticEnvironment>       a string encoding the old global TE
 *  $DESCRIPTION$
 *      The RESTTOKEN() function restores the global TE to the one encoded
 *      in <cStaticTokenEnvironment>. This can either be the return value
 *      of SAVETOKEN() or the value stored in the 4th parameter in a
 *      TOKENINIT() call.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      RESTTOKEN() is compatible with CTIII's RESTTOKEN(),
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token2.c, library is libct.
 *  $SEEALSO$
 *      TOKENINIT(),TOKENEXIT(),TOKENNEXT(),TOKENNUM(),TOKENAT(),SAVETOKEN(),TOKENEND()
 *  $END$
 */

HB_FUNC (RESTTOKEN)
{

  if (ISCHAR (1))
  {
    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    
    TOKEN_ENVIRONMENT sTokenEnvironment;

    if (sStrLen != 0)
    {
      /* alloc memory for new environment */
      sTokenEnvironment = (TOKEN_ENVIRONMENT)hb_xalloc (sStrLen);
      if (sTokenEnvironment == NULL)
      {
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          ct_error ((USHORT)iArgErrorMode, EG_MEM, CT_ERROR_RESTTOKEN,
                    NULL, "RESTTOKEN", 0, EF_CANDEFAULT, 1,
                    hb_paramError (1));
        }
        hb_retc ("");
        return;
      }
      hb_xmemcpy (sTokenEnvironment, pcString, sStrLen);
    }
    else
    {
      /* restored env has length 0 */
      sTokenEnvironment = NULL;
    }

    /* return current environment, then delete it */
    if (ssTokenEnvironment != NULL)
    {
      hb_retclen ((char *)ssTokenEnvironment, sTokEnvGetSize (ssTokenEnvironment));
      sTokEnvDel (ssTokenEnvironment);
    }
    else
    {
      hb_retc ("");
    }

    ssTokenEnvironment = sTokenEnvironment;

  }
  else
  {
    PHB_ITEM pSubst = NULL;
    int iArgErrorMode = ct_getargerrormode();
    if (iArgErrorMode != CT_ARGERR_IGNORE)
    {
      pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                               CT_ERROR_RESTTOKEN, NULL, "RESTTOKEN",
                               0, EF_CANSUBSTITUTE, 1, hb_paramError (1));
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
  }

  return;

}




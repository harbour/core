/*
 * $Id$
 */

/*
 * Harbour Project source code: 
 *   CT3 string functions
 *     - TOKEN()
 *     - NUMTOKEN()
 *     - ATTOKEN()
 *     - TOKENLOWER()
 *     - TOKENUPPER() 
 *     - TOKENSEP()
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
#include <ctype.h>


/* static const data */
static const char *spcSeparatorStr = "\x00""\x09""\x0A""\x0C""\x1A""\x20""\x8A""\x8C"",.;:!\?/\\<>()#&%+-*";
static const size_t ssSeparatorStrLen = 26;

/* static data */
/* even if these are chars, variable must be int, since we need an extra -1 */
static int siPreSeparator = -1;    /* TODO: make this threadsafe */
static int siPostSeparator = -1;   /* TODO: make this threadsafe */

/* defines */
#define DO_TOKEN1_TOKEN         0
#define DO_TOKEN1_NUMTOKEN      1
#define DO_TOKEN1_ATTOKEN       2
#define DO_TOKEN1_TOKENLOWER    3
#define DO_TOKEN1_TOKENUPPER    4

/* helper function for the token function group I */
static void do_token1 (int iSwitch)
{

  int iParamCheck = 0;
  int iNoRef = ct_getref();

  switch (iSwitch)
  {
    case DO_TOKEN1_TOKEN:
    {
      siPreSeparator = siPostSeparator = -1;
    };   /* no "break" here !! */

    case DO_TOKEN1_ATTOKEN:
    case DO_TOKEN1_NUMTOKEN:
    case DO_TOKEN1_TOKENLOWER:
    case DO_TOKEN1_TOKENUPPER:
    {
      iParamCheck = (ISCHAR (1));
    }; break;

  }

  if (iParamCheck)
  {

    char *pcString = hb_parc (1);
    size_t sStrLen = (size_t)hb_parclen (1);
    char *pcSeparatorStr;
    size_t sSeparatorStrLen;
    ULONG ulTokenCounter = 0;
    ULONG ulSkip;

    char *pcSubStr;
    char *pcRet = NULL;
    size_t sSubStrLen;
    size_t sRetStrLen = 0;

    ULONG ulToken = 0;
    ULONG ulSkipCnt;
    char *pc;
    
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

    /* token counter */
    if (iSwitch != DO_TOKEN1_NUMTOKEN)
    {
      if (ISNUM (3))
        ulTokenCounter = hb_parnl (3);
      else
        ulTokenCounter = 0;
    }
    if (ulTokenCounter == 0)
      ulTokenCounter = HB_MKULONG (255,255,255,255); 

    /* skip width */
    if (iSwitch == DO_TOKEN1_NUMTOKEN)
    {
      if (ISNUM (3))
        ulSkip = hb_parnl (3);
      else
        ulSkip = HB_MKULONG (255,255,255,255);
    }
    else
    {
      if (ISNUM (4))
        ulSkip = hb_parnl (4);
      else
        ulSkip = HB_MKULONG (255,255,255,255);
    }
    if (ulSkip == 0)
      ulSkip = HB_MKULONG (255,255,255,255);

    /* prepare return value for TOKENUPPER/TOKENLOWER */
    if ((iSwitch == DO_TOKEN1_TOKENLOWER) || (iSwitch == DO_TOKEN1_TOKENUPPER))
    {
      pcRet = ( char * ) hb_xgrab (sRetStrLen = sStrLen);
      hb_xmemcpy (pcRet, pcString, sRetStrLen);
    }

    /* find the <ulTokenCounter>th token */
    pcSubStr = pcString;
    sSubStrLen = sStrLen;

    /* scan start condition */
    pc = pcSubStr-1;

    while (ulToken < ulTokenCounter)
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
        if (iSwitch == DO_TOKEN1_TOKEN)
        {
          siPreSeparator = siPostSeparator;
          if (sMatchedPos < sSeparatorStrLen)
            siPostSeparator = pcSeparatorStr[sMatchedPos];
          else
            siPostSeparator = -1;
        }

        ulSkipCnt++;
      } while ((ulSkipCnt < ulSkip) && (pc == pcSubStr));

      if (sSubStrLen == 0)
      {
        /* string ends with tokenizer (null string after tokenizer at 
           end of string is not a token) */
        switch (iSwitch)
        {
          case DO_TOKEN1_TOKEN:
          {
            char cRet;
            hb_retc ("");
            if (ISBYREF (5))
            {
              cRet = (char)siPreSeparator;
              hb_storclen (&cRet, (siPreSeparator != -1 ? 1 : 0), 5);
            }
            if (ISBYREF (6))
            {
              cRet = (char)siPostSeparator;
              hb_storclen (&cRet, (siPostSeparator != -1 ? 1 : 0), 6);
            }
          }; break;

          case DO_TOKEN1_NUMTOKEN:
          {
            hb_retnl (ulToken);
          }; break;
    
          case DO_TOKEN1_ATTOKEN:
          {
            hb_retnl (0);
          }; break;

          case DO_TOKEN1_TOKENLOWER:
          case DO_TOKEN1_TOKENUPPER:
          {
            if (!iNoRef)
            {
              hb_retclen (pcRet, sRetStrLen);
            }
            else
            {
              hb_retl (0);
            }
            if (ISBYREF (1))
            {
              hb_storclen (pcRet, sRetStrLen, 1);
            }
            hb_xfree (pcRet);
          }; break;

        }
        return;
      }
        
      switch (iSwitch)
      {
        case DO_TOKEN1_TOKEN:
        case DO_TOKEN1_NUMTOKEN:
        case DO_TOKEN1_ATTOKEN:
          break;
       
        case DO_TOKEN1_TOKENLOWER:
        {
          if (pcSubStr != pc)  /* letters can be tokenizers, too,
                                     but they should not be lowercase'd */
            *(pcRet+(pcSubStr-pcString)) = tolower (*pcSubStr);
        }; break;

        case DO_TOKEN1_TOKENUPPER:
        {
          if (pcSubStr != pc)  /* letters can be tokenizers, too, 
                                     but they should not be uppercase'd */
            *(pcRet+(pcSubStr-pcString)) = toupper (*pcSubStr);
        }; break;

        default:
          break;
      }
    
      ulToken++;

      /* should we find the last token, but string ends with tokenizer, i.e.
         pc points to a the last character at the moment ?
         -> break here ! */
      if ((ulTokenCounter == HB_MKULONG (255,255,255,255)) &&
          (pc+1==pcString+sStrLen))
      {
        break;
      }

      if (pc == NULL)
      {
        pc = pcSubStr+sSubStrLen;  /* little trick for return values */
        break;  /* we must leave the while loop even if we have not
                   yet found the <ulTokenCounter>th token */
      }
 
    } /* while (ulToken < ulTokenCounter) */

    switch (iSwitch)
    {
      case DO_TOKEN1_TOKEN:
      {
        char cRet;
      
        if ((ulTokenCounter == HB_MKULONG (255,255,255,255)) ||
            (ulToken == ulTokenCounter))
          hb_retclen (pcSubStr, pc-pcSubStr);
        else
          hb_retc ("");
        
        if (ISBYREF (5))
        {
          cRet = (char)siPreSeparator;
          hb_storclen (&cRet, (siPreSeparator != -1 ? 1 : 0), 5);
        }
        if (ISBYREF (6))
        {
          cRet = (char)siPostSeparator;
          hb_storclen (&cRet, (siPostSeparator != -1 ? 1 : 0), 6);
        }

      }; break;
      
      case DO_TOKEN1_NUMTOKEN:
      {
        hb_retnl (ulToken);
      }; break;

      case DO_TOKEN1_ATTOKEN:
      {
        if ((ulTokenCounter == HB_MKULONG (255,255,255,255)) ||
            (ulToken == ulTokenCounter))
          hb_retnl (pcSubStr-pcString+1);
        else
          hb_retnl (0);
      }; break;
    
      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
      {
        if (!iNoRef)
        {
          hb_retclen (pcRet, sRetStrLen);
        }
        else
        {
          hb_retl (0);
        }
        if (ISBYREF (1))
        {
          hb_storclen (pcRet, sRetStrLen, 1);
        }
        hb_xfree (pcRet);
      }; break;

    }

  }
  else /* iParamCheck */
  {
    switch (iSwitch)
    {
      case DO_TOKEN1_TOKEN:
      {
        PHB_ITEM pSubst  = NULL;
        int iArgErrorMode = ct_getargerrormode();
        char cRet;
        
        if (ISBYREF (5))
        {
          cRet = (char)siPreSeparator;
          hb_storclen (&cRet, (siPreSeparator != -1 ? 1 : 0),
                       5);
        }
        if (ISBYREF (6))
        {
          cRet = (char)siPostSeparator;
          hb_storclen (&cRet, (siPostSeparator != -1 ? 1 : 0),
                       6);
        }

        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG, CT_ERROR_TOKEN,
                                   NULL, "TOKEN", 0, EF_CANSUBSTITUTE, 6,
                                   hb_paramError (1), hb_paramError (2),
                                   hb_paramError (3), hb_paramError (4),
                                   hb_paramError (5), hb_paramError (6));
        }

        if (pSubst != NULL)
        {
          hb_itemReturn (pSubst);
          hb_itemRelease (pSubst);
        }
        else
        {
          if (!iNoRef)
          {
            hb_retc ("");
          }
          else
          {
            hb_retl (0);
          }
        }
      }; break;  

      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
      {
        PHB_ITEM pSubst = NULL;
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                                   (iSwitch == DO_TOKEN1_TOKENLOWER ? CT_ERROR_TOKENLOWER : CT_ERROR_TOKENUPPER),
                                   NULL,
                                   (iSwitch == DO_TOKEN1_TOKENLOWER ? "TOKENLOWER" : "TOKENUPPER"),
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
          if (!iNoRef)
          {
            hb_retc ("");
          }
          else
          {
            hb_retl (0);
          }
        }
      }; break;

      case DO_TOKEN1_NUMTOKEN:
      case DO_TOKEN1_ATTOKEN:
      {
        PHB_ITEM pSubst = NULL;
        int iArgErrorMode = ct_getargerrormode();
        if (iArgErrorMode != CT_ARGERR_IGNORE)
        {
          pSubst = ct_error_subst ((USHORT)iArgErrorMode, EG_ARG,
                                   (iSwitch == DO_TOKEN1_NUMTOKEN ? CT_ERROR_NUMTOKEN : CT_ERROR_ATTOKEN),
                                   NULL,
                                   (iSwitch == DO_TOKEN1_NUMTOKEN ? "NUMTOKEN" : "ATTOKEN"),
                                   0, EF_CANSUBSTITUTE,
                                   (iSwitch == DO_TOKEN1_NUMTOKEN ? 3 : 4),
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
 *      ATTOKEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Position of a token in a string
 *  $SYNTAX$
 *      ATTOKEN (<cString>, [<cTokenizer>],
 *               [<nTokenCount>], [<nSkipWidth>]) -> nPosition
 *  $ARGUMENTS$
 *      <cString>          is the processed string
 *      [<cTokenizer>]     is a list of characters separating the tokens
 *                         in <cString>
 *                         Default: chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+
 *                                  chr(32)+chr(32)+chr(138)+chr(141)+
 *                                  ",.;:!\?/\\<>()#&%+-*"
 *      [<nTokenCount>]    specifies the count of the token whose
 *                         position should be calculated
 *                         Default: last token
 *      [<nSkipWidth>]     specifies the maximum number of successive
 *                         tokenizing characters that are combined as
 *                         ONE token stop, e.g. specifying 1 can
 *                         yield to empty tokens
 *                         Default: 0, any number of successive tokenizing
 *                         characters are combined as ONE token stop
 *  $RETURNS$
 *      <nPosition>        The start position of the specified token or
 *                         0 if such a token does not exist in <cString>.
 *  $DESCRIPTION$
 *      The ATTOKEN() function calculates the start position of tne
 *      <nTokenCount>th token in <cString>. By setting the new <nSkipWidth>
 *      parameter to a value different than 0, you can specify how many tokenizing
 *      characters are combined at most to one token stop. Be aware that
 *      this can result to empty tokens there the start position is not
 *      defined clearly. Then, ATTOKEN() returns the position there the 
 *      token WOULD start if its length is larger than 0. To check for
 *      empty tokens, simply look if the character at the returned position
 *      is within the tokenizer list.
 *  $EXAMPLES$
 *      attoken ("Hello, World!")  --> 8  // empty strings after tokenizer
 *                                        // are not a token !
 *  $TESTS$
 *      attoken ("Hello, World!") == 8
 *      attoken ("Hello, World!",,2) == 8
 *      attoken ("Hello, World!",,2,1) == 7
 *      attoken ("Hello, World!"," ",2,1) == 8
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      ATTOKEN() is compatible with CT3's ATTOKEN, but has an additional
 *      4th parameter to let you specify a skip width equal to that in the
 *      TOKEN() function.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),NUMTOKEN(),TOKENLOWER(),TOKENUPPER(),TOKENSEP()
 *  $END$
 */

HB_FUNC (ATTOKEN)
{

  do_token1 (DO_TOKEN1_ATTOKEN);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Tokens of a string
 *  $SYNTAX$
 *      TOKEN (<cString>, [<cTokenizer>],
 *             [<nTokenCount], [<nSkipWidth>],
 *             [<@cPreTokenSep>], [<@cPostTokenSep>]) -> cToken
 *  $ARGUMENTS$
 *      <cString>          is the processed string
 *      [<cTokenizer>]     is a list of characters separating the tokens
 *                         in <cString>
 *                         Default: chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+
 *                                  chr(32)+chr(32)+chr(138)+chr(141)+
 *                                  ",.;:!\?/\\<>()#&%+-*"
 *      [<nTokenCount>]    specifies the count of the token that
 *                         should be extracted
 *                         Default: last token
 *      [<nSkipWidth>]     specifies the maximum number of successive
 *                         tokenizing characters that are combined as
 *                         ONE token stop, e.g. specifying 1 can
 *                         yield to empty token
 *                         Default: 0, any number of successive tokenizing
 *                         characters are combined as ONE token stop
 *      [<@cPreTokenSep>]  If given by reference, the tokenizer before
 *                         the actual token will be stored
 *      [<@cPostTokenSep>] If given by reference, the tokenizer after
 *                         the actual token will be stored
 *  $RETURNS$
 *      <cToken>           the token specified by the parameters given above
 *  $DESCRIPTION$
 *      The TOKEN() function extracts the <nTokenCount>th token from the
 *      string <cString>. In the course of this, the tokens in the
 *      string are separated by the character(s) specified in <cTokenizer>.
 *      The function may also extract empty tokens, if you specify a skip
 *      width other than zero.
 *      Be aware of the new 5th and 6th parameter there the TOKEN() function
 *      stores the tokenizing character before and after the extracted token.
 *      Therefore, additional calls to the TOKENSEP() function are not
 *      necessary.
 *  $EXAMPLES$
 *      ? token ("Hello, World!")         -->  "World" 
 *      ? token ("Hello, World!",,2,1)    --> "" 
 *      ? token ("Hello, World!",",",2,1) --> " World!"
 *      ? token ("Hello, World!"," ",2,1) --> "World!" 
 *  $TESTS$
 *      token ("Hello, World!") == "World" 
 *      token ("Hello, World!",,2,1) == "" 
 *      token ("Hello, World!",",",2,1) == " World!"
 *      token ("Hello, World!"," ",2,1) == "World!" 
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKEN() is compatible with CT3's TOKEN, but two additional
 *      parameters have been added there the TOKEN() function can store
 *      the tokenizers before and after the current token.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      NUMTOKEN(),ATTOKEN(),TOKENLOWER(),TOKENUPPER(),TOKENSEP()
 *  $END$
 */

HB_FUNC (TOKEN)
{

  do_token1 (DO_TOKEN1_TOKEN);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      NUMTOKEN()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Retrieves the number of tokens in a string
 *  $SYNTAX$
 *      NUMTOKEN (<cString>, [<cTokenizer>], [<nSkipWidth>]) -> nTokenCount
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *  $EXAMPLES$
 *  $TESTS$
 *      numtoken ("Hello, World!") ==  2 
 *      numtoken ("This is good. See you! How do you do?",".!?") == 3
 *      numtoken ("one,,three,four,,six",",",1) ==  6 
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      NUMTOKEN() is compatible with CT3's NUMTOKEN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),ATTOKEN(),TOKENLOWER(),TOKENUPPER(),TOKENSEP()
 *  $END$
 */

HB_FUNC (NUMTOKEN)
{

  do_token1 (DO_TOKEN1_NUMTOKEN);
  return;

}

/*  $DOC$
 *  $FUNCNAME$
 *      TOKENLOWER()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Change the first letter of tokens to lower case
 *  $SYNTAX$
 *      TOKENLOWER (<[@]cString>, [<cTokenizer>], [<nTokenCount>],
 *                  [<nSkipWidth>]) -> cString
 *  $ARGUMENTS$
 *      <[@]cString>      is the processed string
 *      [<cTokenizer>]    is a list of characters separating the tokens
 *                        in <cString>
 *                        Default: chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+
 *                                 chr(32)+chr(32)+chr(138)+chr(141)+
 *                                 ",.;:!\?/\\<>()#&%+-*"
 *      [<nTokenCount>]   specifies the number of tokens that
 *                        should be processed
 *                        Default: all tokens
 *      [<nSkipWidth>]    specifies the maximum number of successive
 *                        tokenizing characters that are combined as
 *                        ONE token stop, e.g. specifying 1 can
 *                        yield to empty token
 *                        Default: 0, any number of successive tokenizing
 *                        characters are combined as ONE token stop
 *  $RETURNS$
 *      <cString>         the string with the lowercased tokens
 *  $DESCRIPTION$
 *      The TOKENLOWER() function changes the first letter of tokens in <cString>
 *      to lower case. To do this, it uses the same tokenizing mechanism
 *      as the token() function. If TOKENLOWER() extracts a token that starts
 *      with a letter, this letter will be changed to lower case.
 *      You can omit the return value of this function by setting the CSETREF()
 *      switch to .T., but you must then pass <cString> by reference to get
 *      the result.
 *  $EXAMPLES$
 *      ? tokenlower("Hello, World, here I am!")       // "hello, world, here i am!"
 *      ? tokenlower("Hello, World, here I am!",,3)    // "hello, world, here I am!" 
 *      ? tokenlower("Hello, World, here I am!",",",3) // "hello, World, here I am!" 
 *      ? tokenlower("Hello, World, here I am!"," W")  // "hello, World, here i am!"
 *  $TESTS$
 *      tokenlower("Hello, World, here I am!") == "hello, world, here i am!"
 *      tokenlower("Hello, World, here I am!",,3)    == "hello, world, here I am!" 
 *      tokenlower("Hello, World, here I am!",",",3) == "hello, World, here I am!" 
 *      tokenlower("Hello, World, here I am!"," W")  == "hello, World, here i am!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENLOWER() is compatible with CT3's TOKENLOWER(),
 *      but a new 4th parameter, <nSkipWidth> has been added for
 *      synchronization with the the other token functions.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),NUMTOKEN(),ATTOKEN(),TOKENUPPER(),TOKENSEP(),CSETREF()
 *  $END$
 */

HB_FUNC (TOKENLOWER)
{

  do_token1 (DO_TOKEN1_TOKENLOWER);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENUPPER()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Change the first letter of tokens to upper case
 *  $SYNTAX$
 *      TOKENUPPER (<[@]cString>, [<cTokenizer>], [<nTokenCount>],
 *                  [<nSkipWidth>]) -> cString
 *  $ARGUMENTS$
 *      <[@]cString>      is the processed string
 *      [<cTokenizer>]    is a list of characters separating the tokens
 *                        in <cString>
 *                        Default: chr(0)+chr(9)+chr(10)+chr(13)+chr(26)+
 *                                 chr(32)+chr(32)+chr(138)+chr(141)+
 *                                 ",.;:!\?/\\<>()#&%+-*"
 *      [<nTokenCount>]   specifies the number of tokens that
 *                        should be processed
 *                        Default: all tokens
 *      [<nSkipWidth>]    specifies the maximum number of successive
 *                        tokenizing characters that are combined as
 *                        ONE token stop, e.g. specifying 1 can
 *                        yield to empty token
 *                        Default: 0, any number of successive tokenizing
 *                        characters are combined as ONE token stop
 *  $RETURNS$
 *      <cString>         the string with the uppercased tokens
 *  $DESCRIPTION$
 *      The TOKENUPPER() function changes the first letter of tokens in <cString>
 *      to upper case. To do this, it uses the same tokenizing mechanism
 *      as the token() function. If TOKENUPPER() extracts a token that starts
 *      with a letter, this letter will be changed to upper case.
 *      You can omit the return value of this function by setting the CSETREF()
 *      switch to .T., but you must then pass <cString> by reference to get
 *      the result.
 *  $EXAMPLES$
 *      ? tokenupper("Hello, world, here I am!")       // "Hello, World, Here I Am!"
 *      ? tokenupper("Hello, world, here I am!",,3)    // "Hello, World, Here I am!"
 *      ? tokenupper("Hello, world, here I am!",",",3) // "Hello, world, here I am!"
 *      ? tokenupper("Hello, world, here I am!"," w")  // "Hello, wOrld, Here I Am!"
 *  $TESTS$
 *      tokenupper("Hello, world, here I am!")       == "Hello, World, Here I Am!"
 *      tokenupper("Hello, world, here I am!",,3)    == "Hello, World, Here I am!"
 *      tokenupper("Hello, world, here I am!",",",3) == "Hello, world, here I am!"
 *      tokenupper("Hello, world, here I am!"," w")  == "Hello, wOrld, Here I Am!"
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENUPPER() is compatible with CT3's TOKENUPPER(),
 *      but a new 4th parameter, <nSkipWidth> has been added for
 *      synchronization with the the other token functions.
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),NUMTOKEN(),ATTOKEN(),TOKENLOWER(),TOKENSEP(),CSETREF()
 *  $END$
 */

HB_FUNC (TOKENUPPER)
{

  do_token1 (DO_TOKEN1_TOKENUPPER);
  return;

}


/*  $DOC$
 *  $FUNCNAME$
 *      TOKENSEP()
 *  $CATEGORY$
 *      CT3 string functions
 *  $ONELINER$
 *      Retrieves the token separators of the last token() call
 *  $SYNTAX$
 *      TOKENSEP ([<lMode>]) -> cSeparator
 *  $ARGUMENTS$
 *      [<lMode>]   if set to .T., the token separator BEHIND the token
 *                  retrieved from the token() call will be returned.
 *                  Default: .F., returns the separator BEFORE the token
 *  $RETURNS$
 *      Depending on the setting of <lMode>, the separating character of the
 *      the token retrieved from the last token() call will be returned.
 *      These separating characters can now also be retrieved with the token()
 *      function.
 *  $DESCRIPTION$
 *      When one does extract tokens from a string with the token() function,
 *      one might be interested in the separator characters that have been
 *      used to extract a specific token. To get this information you can
 *      either use the TOKENSEP() function after each token() call, or
 *      use the new 5th and 6th parameter of the token() function.
 *  $EXAMPLES$
 *      see TOKEN() function
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *      TOKENSEP() is compatible with CT3's TOKENSEP().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is token1.c, library is libct.
 *  $SEEALSO$
 *      TOKEN(),NUMTOKEN(),ATTOKEN(),TOKENLOWER(),TOKENUPPER()
 *  $END$
 */

HB_FUNC (TOKENSEP)
{

  char cRet;

  if (ISLOG (1) && hb_parl (1))
  {
    /* return the separator char BEHIND the last token */
    if (siPostSeparator != -1)
    {
      cRet = (char)siPostSeparator;
      hb_retclen (&cRet, 1);
    }
    else
    {
      hb_retc ("");
    }
  }
  else
  {
    /* return the separator char BEFORE the last token */
    if (siPreSeparator != -1)
    {
      cRet = (char)siPreSeparator;
      hb_retclen (&cRet, 1);
    }
    else
    {
      hb_retc ("");
    }
  }

  return;

}





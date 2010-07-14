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
 * www - http://harbour-project.org
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


/* static const data */
static const char *s_pcSeparatorStr =
   "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";
static const HB_SIZE s_sSeparatorStrLen = 26;

/* static data */
/* even if these are chars, variable must be int, since we need an extra -1 */
static int s_iPreSeparator = -1;     /* TODO: make this threadsafe */
static int s_iPostSeparator = -1;    /* TODO: make this threadsafe */

/* defines */
#define DO_TOKEN1_TOKEN         0
#define DO_TOKEN1_NUMTOKEN      1
#define DO_TOKEN1_ATTOKEN       2
#define DO_TOKEN1_TOKENLOWER    3
#define DO_TOKEN1_TOKENUPPER    4

/* helper function for the token function group I */
static void do_token1( int iSwitch )
{
   int iParamCheck = 0;
   int iNoRef = ct_getref() && HB_ISBYREF( 1 );

   switch ( iSwitch )
   {
      case DO_TOKEN1_TOKEN:
         s_iPreSeparator = s_iPostSeparator = -1;
         /* no "break" here !! */
      case DO_TOKEN1_ATTOKEN:
      case DO_TOKEN1_NUMTOKEN:
      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
         iParamCheck = ( HB_ISCHAR( 1 ) );
         break;
   }

   if( iParamCheck )
   {
      const char *pcString = hb_parc( 1 );
      HB_SIZE sStrLen = hb_parclen( 1 );
      const char *pcSeparatorStr;
      HB_SIZE sSeparatorStrLen;
      HB_SIZE nTokenCounter = 0;
      HB_SIZE nSkip;
      const char *pcSubStr;
      char *pcRet = NULL;
      HB_SIZE sSubStrLen;
      HB_SIZE sRetStrLen = 0;
      HB_SIZE nToken = 0;
      HB_SIZE nSkipCnt;
      const char *pc;

      /* separator string */
      sSeparatorStrLen = hb_parclen( 2 );
      if( sSeparatorStrLen != 0 )
         pcSeparatorStr = hb_parc( 2 );
      else
      {
         pcSeparatorStr = ( char * ) s_pcSeparatorStr;
         sSeparatorStrLen = s_sSeparatorStrLen;
      }

      /* token counter */
      if( iSwitch != DO_TOKEN1_NUMTOKEN )
         nTokenCounter = hb_parns( 3 );
      if( nTokenCounter == 0 )
         nTokenCounter = HB_SIZE_MAX;

      /* skip width */
      if( iSwitch == DO_TOKEN1_NUMTOKEN )
      {
         if( HB_ISNUM( 3 ) )
            nSkip = hb_parns( 3 );
         else
            nSkip = HB_SIZE_MAX;
      }
      else
      {
         if( HB_ISNUM( 4 ) )
            nSkip = hb_parns( 4 );
         else
            nSkip = HB_SIZE_MAX;
      }
      if( nSkip == 0 )
         nSkip = HB_SIZE_MAX;

      /* prepare return value for TOKENUPPER/TOKENLOWER */
      if( iSwitch == DO_TOKEN1_TOKENLOWER || iSwitch == DO_TOKEN1_TOKENUPPER )
      {
         if( sStrLen == 0 )
         {
            if( iNoRef )
               hb_retl( HB_FALSE );
            else
               hb_retc_null();
            return;
         }
         sRetStrLen = sStrLen;
         pcRet = ( char * ) hb_xgrab( sRetStrLen + 1 );
         hb_xmemcpy( pcRet, pcString, sRetStrLen );
      }

      /* find the <nTokenCounter>th token */
      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      /* scan start condition */
      pc = pcSubStr - 1;

      while( nToken < nTokenCounter )
      {
         HB_SIZE sMatchedPos = sSeparatorStrLen;

         /* Skip the left nSkip successive separators */
         nSkipCnt = 0;
         do
         {
            sSubStrLen -= ( pc - pcSubStr ) + 1;
            pcSubStr = pc + 1;
            pc = ct_at_charset_forward( pcSubStr, sSubStrLen,
                                        pcSeparatorStr, sSeparatorStrLen, &sMatchedPos );
            if( iSwitch == DO_TOKEN1_TOKEN )
            {
               s_iPreSeparator = s_iPostSeparator;
               if( sMatchedPos < sSeparatorStrLen )
                  s_iPostSeparator = pcSeparatorStr[sMatchedPos];
               else
                  s_iPostSeparator = -1;
            }
            nSkipCnt++;
         }
         while( nSkipCnt < nSkip && pc == pcSubStr );

         if( sSubStrLen == 0 )
         {
            /* string ends with tokenizer (null string after tokenizer at
               end of string is not a token) */
            switch ( iSwitch )
            {
               case DO_TOKEN1_TOKEN:
               {
                  char cRet;

                  hb_retc_null();
                  if( HB_ISBYREF( 5 ) )
                  {
                     cRet = ( char ) s_iPreSeparator;
                     hb_storclen( &cRet, ( s_iPreSeparator != -1 ? 1 : 0 ), 5 );
                  }
                  if( HB_ISBYREF( 6 ) )
                  {
                     cRet = ( char ) s_iPostSeparator;
                     hb_storclen( &cRet, ( s_iPostSeparator != -1 ? 1 : 0 ), 6 );
                  }
                  break;
               }
               case DO_TOKEN1_NUMTOKEN:
                  hb_retns( nToken );
                  break;

               case DO_TOKEN1_ATTOKEN:
                  hb_retns( 0 );
                  break;

               case DO_TOKEN1_TOKENLOWER:
               case DO_TOKEN1_TOKENUPPER:
                  if( HB_ISBYREF( 1 ) )
                     hb_storclen( pcRet, sRetStrLen, 1 );

                  if( iNoRef )
                  {
                     hb_xfree( pcRet );
                     hb_retl( HB_FALSE );
                  }
                  else
                     hb_retclen_buffer( pcRet, sRetStrLen );
                  break;
            }
            return;
         }

         switch ( iSwitch )
         {
            case DO_TOKEN1_TOKEN:
            case DO_TOKEN1_NUMTOKEN:
            case DO_TOKEN1_ATTOKEN:
               break;

            case DO_TOKEN1_TOKENLOWER:
               if( pcSubStr != pc )     /* letters can be tokenizers, too,
                                           but they should not be lowercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = ( char ) hb_charLower( ( HB_UCHAR ) *pcSubStr );
               break;

            case DO_TOKEN1_TOKENUPPER:
               if( pcSubStr != pc )     /* letters can be tokenizers, too,
                                           but they should not be uppercase'd */
                  *( pcRet + ( pcSubStr - pcString ) ) = ( char ) hb_charUpper( ( HB_UCHAR ) *pcSubStr );
               break;

            default:
               break;
         }

         nToken++;

         if( pc == NULL )
         {
            pc = pcSubStr + sSubStrLen; /* little trick for return values */
            break;              /* we must leave the while loop even if we have not
                                   yet found the <nTokenCounter>th token */
         }

         /* should we find the last token, but string ends with tokenizer, i.e.
            pc points to the last character at the moment ?
            -> break here ! */
         if( nTokenCounter == HB_SIZE_MAX )
         {
            if( nSkip == HB_SIZE_MAX )
            {
               const char *t;
               HB_BOOL bLast = HB_TRUE;

               for( t = pc + 1; t < pcString + sStrLen; t++ )
               {
                  if( !memchr( pcSeparatorStr, *t, sSeparatorStrLen ) )
                  {
                     bLast = HB_FALSE;
                     break;
                  }
               }
               if( bLast )
                  break;
            }
            else if( pc + 1 == pcString + sStrLen )
               break;
         }
      }  /* while( nToken < nTokenCounter ) */

      switch ( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            char cRet;

            if( ( nTokenCounter == HB_SIZE_MAX ) ||
                ( nToken == nTokenCounter ) )
               hb_retclen( pcSubStr, pc - pcSubStr );
            else
               hb_retc_null();

            if( HB_ISBYREF( 5 ) )
            {
               cRet = ( char ) s_iPreSeparator;
               hb_storclen( &cRet, ( s_iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( HB_ISBYREF( 6 ) )
            {
               cRet = ( char ) s_iPostSeparator;
               hb_storclen( &cRet, ( s_iPostSeparator != -1 ? 1 : 0 ), 6 );
            }
            break;
         }
         case DO_TOKEN1_NUMTOKEN:
            hb_retns( nToken );
            break;

         case DO_TOKEN1_ATTOKEN:
            if( ( nTokenCounter == HB_SIZE_MAX ) ||
                ( nToken == nTokenCounter ) )
               hb_retns( pcSubStr - pcString + 1 );
            else
               hb_retns( 0 );
            break;

         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
            if( HB_ISBYREF( 1 ) )
               hb_storclen( pcRet, sRetStrLen, 1 );

            if( iNoRef )
            {
               hb_xfree( pcRet );
               hb_retl( HB_FALSE );
            }
            else
               hb_retclen_buffer( pcRet, sRetStrLen );
            break;
      }
   }
   else  /* iParamCheck */
   {
      switch ( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();
            char cRet;

            if( HB_ISBYREF( 5 ) )
            {
               cRet = ( char ) s_iPreSeparator;
               hb_storclen( &cRet, ( s_iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( HB_ISBYREF( 6 ) )
            {
               cRet = ( char ) s_iPostSeparator;
               hb_storclen( &cRet, ( s_iPostSeparator != -1 ? 1 : 0 ), 6 );
            }

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        CT_ERROR_TOKEN, NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        HB_ERR_ARGS_BASEPARAMS );
            }

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else if( !iNoRef )
               hb_retc_null();
            else
               hb_retl( HB_FALSE );
            break;
         }
         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
         {
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_TOKENLOWER ?
                                        CT_ERROR_TOKENLOWER : CT_ERROR_TOKENUPPER,
                                        NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        HB_ERR_ARGS_BASEPARAMS );
            }
            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else if( !iNoRef )
               hb_retc_null();
            else
               hb_retl( HB_FALSE );
            break;
         }
         case DO_TOKEN1_NUMTOKEN:
         case DO_TOKEN1_ATTOKEN:
         {
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
            {
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_NUMTOKEN ?
                                        CT_ERROR_NUMTOKEN : CT_ERROR_ATTOKEN,
                                        NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );
            }
            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else
               hb_retns( 0 );
            break;
         }
      }
   }
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

HB_FUNC( ATTOKEN )
{
   do_token1( DO_TOKEN1_ATTOKEN );
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

HB_FUNC( TOKEN )
{
   do_token1( DO_TOKEN1_TOKEN );
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

HB_FUNC( NUMTOKEN )
{
   do_token1( DO_TOKEN1_NUMTOKEN );
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

HB_FUNC( TOKENLOWER )
{
   do_token1( DO_TOKEN1_TOKENLOWER );
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

HB_FUNC( TOKENUPPER )
{
   do_token1( DO_TOKEN1_TOKENUPPER );
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

HB_FUNC( TOKENSEP )
{
   char cRet;

   if( hb_parl( 1 ) )
   {
      /* return the separator char BEHIND the last token */
      if( s_iPostSeparator != -1 )
      {
         cRet = ( char ) s_iPostSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
         hb_retc_null();
   }
   else
   {
      /* return the separator char BEFORE the last token */
      if( s_iPreSeparator != -1 )
      {
         cRet = ( char ) s_iPreSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
         hb_retc_null();
   }
}

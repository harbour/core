/*
 * Harbour Project source code:
 *   CT3 string functions
 *     - Token()
 *     - NumToken()
 *     - AtToken()
 *     - TokenLower()
 *     - TokenUpper()
 *     - TokenSep()
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbstack.h"

/* static const data */
static const char * sc_pcSeparatorStr =
   "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";
static const HB_SIZE sc_sSeparatorStrLen = 26;

/* static data */

typedef struct
{
   /* even if these are chars, variable must be int, since we need an extra -1 */
   int iPreSeparator;
   int iPostSeparator;
} CT_TOKEN, * PCT_TOKEN;

static void s_ct_token_init( void * cargo )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) cargo;

   ct_token->iPreSeparator  = -1;
   ct_token->iPostSeparator = -1;
}

static HB_TSD_NEW( s_ct_token, sizeof( CT_TOKEN ), s_ct_token_init, NULL );

/* defines */
#define DO_TOKEN1_TOKEN       0
#define DO_TOKEN1_NUMTOKEN    1
#define DO_TOKEN1_ATTOKEN     2
#define DO_TOKEN1_TOKENLOWER  3
#define DO_TOKEN1_TOKENUPPER  4

/* helper function for the token function group I */
static void do_token1( int iSwitch )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) hb_stackGetTSD( &s_ct_token );

   int iParamCheck = 0;
   int iNoRef = ct_getref() && HB_ISBYREF( 1 );

   switch( iSwitch )
   {
      case DO_TOKEN1_TOKEN:
         ct_token->iPreSeparator = ct_token->iPostSeparator = -1;
         /* no "break" here !! */
      case DO_TOKEN1_ATTOKEN:
      case DO_TOKEN1_NUMTOKEN:
      case DO_TOKEN1_TOKENLOWER:
      case DO_TOKEN1_TOKENUPPER:
         iParamCheck = HB_ISCHAR( 1 );
         break;
   }

   if( iParamCheck )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE sStrLen = hb_parclen( 1 );
      const char * pcSeparatorStr;
      HB_SIZE sSeparatorStrLen;
      HB_SIZE nTokenCounter;
      HB_SIZE nSkip;
      const char * pcSubStr;
      char * pcRet = NULL;
      HB_SIZE sSubStrLen;
      HB_SIZE sRetStrLen = 0;
      HB_SIZE nToken = 0;
      HB_SIZE nSkipCnt;
      const char * pc;

      /* separator string */
      sSeparatorStrLen = hb_parclen( 2 );
      if( sSeparatorStrLen != 0 )
         pcSeparatorStr = hb_parc( 2 );
      else
      {
         pcSeparatorStr = sc_pcSeparatorStr;
         sSeparatorStrLen = sc_sSeparatorStrLen;
      }

      if( iSwitch == DO_TOKEN1_NUMTOKEN )
      {
         /* token counter */
         nTokenCounter = HB_SIZE_MAX;
         /* skip width */
         nSkip = hb_parns( 3 );
      }
      else
      {
         /* token counter */
         nTokenCounter = hb_parns( 3 );
         /* skip width */
         nSkip = hb_parns( 4 ); /* HB_EXTENSION for AtToken()/TokenLower()/TokenUpper() */
      }

      if( nTokenCounter == 0 )
         nTokenCounter = HB_SIZE_MAX;
      if( nSkip == 0 )
         nSkip = HB_SIZE_MAX;

      /* prepare return value for TokenUpper()/TokenLower() */
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
               ct_token->iPreSeparator = ct_token->iPostSeparator;
               if( sMatchedPos < sSeparatorStrLen )
                  ct_token->iPostSeparator = pcSeparatorStr[ sMatchedPos ];
               else
                  ct_token->iPostSeparator = -1;
            }
            nSkipCnt++;
         }
         while( nSkipCnt < nSkip && pc == pcSubStr );

         if( sSubStrLen == 0 )
         {
            /* string ends with tokenizer (null string after tokenizer at
               end of string is not a token) */
            switch( iSwitch )
            {
               case DO_TOKEN1_TOKEN:
               {
                  char cRet;

                  hb_retc_null();
                  if( HB_ISBYREF( 5 ) ) /* HB_EXTENSION */
                  {
                     cRet = ( char ) ct_token->iPreSeparator;
                     hb_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
                  }
                  if( HB_ISBYREF( 6 ) ) /* HB_EXTENSION */
                  {
                     cRet = ( char ) ct_token->iPostSeparator;
                     hb_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
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

         switch( iSwitch )
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
            /* little trick for return values */
            pc = pcSubStr + sSubStrLen;
            /* we must leave the while loop even if we have not
               yet found the <nTokenCounter>th token */
            break;
         }

         /* should we find the last token, but string ends with tokenizer, i.e.
            pc points to the last character at the moment ?
            -> break here ! */
         if( nTokenCounter == HB_SIZE_MAX )
         {
            if( nSkip == HB_SIZE_MAX )
            {
               const char * t;
               HB_BOOL bLast = HB_TRUE;

               for( t = pc + 1; t < pcString + sStrLen; t++ )
               {
                  if( ! memchr( pcSeparatorStr, *t, sSeparatorStrLen ) )
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
      }

      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            char cRet;

            if( nTokenCounter == HB_SIZE_MAX ||
                nToken == nTokenCounter )
               hb_retclen( pcSubStr, pc - pcSubStr );
            else
               hb_retc_null();

            if( HB_ISBYREF( 5 ) ) /* HB_EXTENSION */
            {
               cRet = ( char ) ct_token->iPreSeparator;
               hb_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( HB_ISBYREF( 6 ) ) /* HB_EXTENSION */
            {
               cRet = ( char ) ct_token->iPostSeparator;
               hb_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
            }
            break;
         }
         case DO_TOKEN1_NUMTOKEN:
            hb_retns( nToken );
            break;

         case DO_TOKEN1_ATTOKEN:
            if( nTokenCounter == HB_SIZE_MAX ||
                nToken == nTokenCounter )
               hb_retns( pcSubStr - pcString + 1 );
            else
               hb_retns( 0 );
            break;

         case DO_TOKEN1_TOKENLOWER:
         case DO_TOKEN1_TOKENUPPER:
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
   else
   {
      switch( iSwitch )
      {
         case DO_TOKEN1_TOKEN:
         {
            PHB_ITEM pSubst = NULL;
            int iArgErrorMode = ct_getargerrormode();
            char cRet;

            if( HB_ISBYREF( 5 ) ) /* HB_EXTENSION */
            {
               cRet = ( char ) ct_token->iPreSeparator;
               hb_storclen( &cRet, ( ct_token->iPreSeparator != -1 ? 1 : 0 ), 5 );
            }
            if( HB_ISBYREF( 6 ) ) /* HB_EXTENSION */
            {
               cRet = ( char ) ct_token->iPostSeparator;
               hb_storclen( &cRet, ( ct_token->iPostSeparator != -1 ? 1 : 0 ), 6 );
            }

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        CT_ERROR_TOKEN, NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        HB_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else if( ! iNoRef )
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
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_TOKENLOWER ?
                                        CT_ERROR_TOKENLOWER : CT_ERROR_TOKENUPPER,
                                        NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE,
                                        HB_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else if( ! iNoRef )
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
               pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                        iSwitch == DO_TOKEN1_NUMTOKEN ?
                                        CT_ERROR_NUMTOKEN : CT_ERROR_ATTOKEN,
                                        NULL, HB_ERR_FUNCNAME, 0,
                                        EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

            if( pSubst != NULL )
               hb_itemReturnRelease( pSubst );
            else
               hb_retns( 0 );
            break;
         }
      }
   }
}

HB_FUNC( ATTOKEN )
{
   do_token1( DO_TOKEN1_ATTOKEN );
}

HB_FUNC( TOKEN )
{
   do_token1( DO_TOKEN1_TOKEN );
}

HB_FUNC( NUMTOKEN )
{
   do_token1( DO_TOKEN1_NUMTOKEN );
}

HB_FUNC( TOKENLOWER )
{
   do_token1( DO_TOKEN1_TOKENLOWER );
}

HB_FUNC( TOKENUPPER )
{
   do_token1( DO_TOKEN1_TOKENUPPER );
}

HB_FUNC( TOKENSEP )
{
   PCT_TOKEN ct_token = ( PCT_TOKEN ) hb_stackGetTSD( &s_ct_token );

   char cRet;

   if( hb_parl( 1 ) )
   {
      /* return the separator char BEHIND the last token */
      if( ct_token->iPostSeparator != -1 )
      {
         cRet = ( char ) ct_token->iPostSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
         hb_retc_null();
   }
   else
   {
      /* return the separator char BEFORE the last token */
      if( ct_token->iPreSeparator != -1 )
      {
         cRet = ( char ) ct_token->iPreSeparator;
         hb_retclen( &cRet, 1 );
      }
      else
         hb_retc_null();
   }
}

/*
 * CT3 string functions
 *     - TokenInit()
 *     - TokenExit()
 *     - TokenNext()
 *     - TokenNum()
 *     - TokenAt()
 *     - SaveToken()
 *     - RestToken()
 *     - TokenEnd()
 *
 * Copyright 2001 IntTec GmbH, Neunlindenstr 32, 79106 Freiburg, Germany
 *        Author: Martin Vogel <vogel@inttec.de>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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
#include "hbvm.h"

#include "hbstack.h"

/* static functions for token environment management */

#define TOKEN_ENVIRONMENT_STEP  100

typedef struct
{
   HB_SIZE sStartPos;            /* relative 0-based index of first char of token */
   HB_SIZE sEndPos;              /* relative 0-based index of first char BEHIND token,
                                    so that length = sEndPos-sStartPos */
} TOKEN_POSITION, * TOKEN_ENVIRONMENT;

/* alloc new token environment */
static TOKEN_ENVIRONMENT sTokEnvNew( void )
{
   TOKEN_ENVIRONMENT env = ( TOKEN_ENVIRONMENT )
         hb_xalloc( sizeof( TOKEN_POSITION ) * ( 2 + TOKEN_ENVIRONMENT_STEP ) + 1 );

   if( env == NULL )
      return NULL;

   /* use the first element to store current length and use of token env */
   env[ 0 ].sStartPos = 0;                    /* 0-based index to next free, unused element */
   env[ 0 ].sEndPos = TOKEN_ENVIRONMENT_STEP; /* but there are 100 elements ready for use */

   /* use second element to store actual index with TokenNext() */
   env[ 1 ].sStartPos = 0;        /* 0-based index value that is to be used NEXT */

   return env;
}

/* add a tokenizing position to a token environment */
static int sTokEnvAddPos( TOKEN_ENVIRONMENT * pEnv, TOKEN_POSITION * pPos )
{
   HB_SIZE nIndex;
   TOKEN_ENVIRONMENT env = *pEnv;

   /* new memory needed? */
   if( env[ 0 ].sStartPos == env[ 0 ].sEndPos )
   {
      env = *pEnv = ( TOKEN_ENVIRONMENT )
               hb_xrealloc( env, sizeof( TOKEN_POSITION ) *
                            ( 2 + env[ 0 ].sEndPos + TOKEN_ENVIRONMENT_STEP ) + 1 );

      env[ 0 ].sEndPos += TOKEN_ENVIRONMENT_STEP;
   }

   nIndex = env[ 0 ].sStartPos + 2;        /* +2  because of extra elements */
   env[ nIndex ].sStartPos = pPos->sStartPos;
   env[ nIndex ].sEndPos = pPos->sEndPos;
   env[ 0 ].sStartPos++;

   return 1;
}

/* check to see if token pointer is at end of environment */
static int sTokEnvEnd( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos >= env[ 0 ].sStartPos;
}

/* get size of token environment in memory */
static HB_SIZE sTokEnvGetSize( TOKEN_ENVIRONMENT env )
{
   return sizeof( TOKEN_POSITION ) * ( 2 + env[ 0 ].sEndPos );
}

/* get position element pointed to by tokenizing pointer */
static TOKEN_POSITION * sTokEnvGetPos( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
      return NULL;

   return env + 2 + ( env[ 1 ].sStartPos ); /* "+2" because of extra elements */
}

/* get position element pointed to by given 0-based index */
static TOKEN_POSITION * sTokEnvGetPosIndex( TOKEN_ENVIRONMENT env, HB_SIZE nIndex )
{
   if( nIndex >= env[ 0 ].sStartPos )
      return NULL;

   return env + 2 + nIndex; /* "+2" because of extra elements */
}

/* increment tokenizing pointer by one */
static int sTokEnvIncPtr( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos >= env[ 0 ].sStartPos )
      return 0;
   else
   {
      env[ 1 ].sStartPos++;
      return 1;
   }
}

/* set tokenizing pointer to 0-based value */
static int sTokEnvSetPtr( TOKEN_ENVIRONMENT env, HB_SIZE sCnt )
{
   if( sCnt >= env[ 0 ].sStartPos )
      return 0;
   else
   {
      env[ 1 ].sStartPos = sCnt;
      return 1;
   }
}

/* decrement tokenizing pointer by one */

/* sTokEnvDecPtr currently not used ! */
#if 0
static int sTokEnvDecPtr( TOKEN_ENVIRONMENT env )
{
   if( env[ 1 ].sStartPos <= 0 )
      return 0;
   else
   {
      env[ 1 ].sStartPos--;
      return 1;
   }
}
#endif

/* get value of tokenizing pointer */
static HB_SIZE sTokEnvGetPtr( TOKEN_ENVIRONMENT env )
{
   return env[ 1 ].sStartPos;
}

/* get token count */
static HB_SIZE sTokEnvGetCnt( TOKEN_ENVIRONMENT env )
{
   return env[ 0 ].sStartPos;
}

/* free token environment */
static void sTokEnvDel( TOKEN_ENVIRONMENT env )
{
   hb_xfree( env );
}

/* Harbour functions */

/* static data */
static const char sc_spcSeparatorStr[] =
   "\x00" "\x09" "\x0A" "\x0C" "\x1A" "\x20" "\x8A" "\x8C" ",.;:!\?/\\<>()#&%+-*";

static void s_token_exit( void * cargo )
{
   TOKEN_ENVIRONMENT * penv = ( TOKEN_ENVIRONMENT * ) cargo;

   if( *penv )
   {
      sTokEnvDel( *penv );
      *penv = NULL;
   }
}

static HB_TSD_NEW( s_token, sizeof( TOKEN_ENVIRONMENT * ), NULL, s_token_exit );

static void sTokSet( TOKEN_ENVIRONMENT env )
{
   TOKEN_ENVIRONMENT * penv = ( TOKEN_ENVIRONMENT * ) hb_stackGetTSD( &s_token );

   if( *penv != env )
   {
      if( *penv )
         sTokEnvDel( *penv );
      *penv = env;
   }
}

static TOKEN_ENVIRONMENT sTokGet( int iParam, HB_BOOL fReadOnly )
{
   if( iParam < 0 || ( iParam > 0 && HB_ISCHAR( iParam ) ) )
   {
      if( iParam < 0 || fReadOnly || HB_ISBYREF( iParam ) )
      {
         HB_SIZE nLen;

         if( iParam < 0 )
            iParam = -iParam;

         nLen = hb_parclen( iParam );
         if( nLen >= sizeof( TOKEN_POSITION ) * 2 )
         {
            TOKEN_ENVIRONMENT env = ( TOKEN_ENVIRONMENT ) HB_UNCONST( hb_parc( iParam ) );

            if( sTokEnvGetSize( env ) == nLen )
               return fReadOnly ? env : ( TOKEN_ENVIRONMENT ) hb_xmemdup( env, nLen + 1 );
         }
      }
      return NULL;
   }
   else
      return * ( TOKEN_ENVIRONMENT * ) hb_stackGetTSD( &s_token );
}

static int sTokSave( TOKEN_ENVIRONMENT sTokenEnvironment, int iParam )
{
   if( iParam != 0 && HB_ISBYREF( iParam ) )
   {
      if( ! hb_storclen_buffer( ( char * ) sTokenEnvironment,
                                sTokEnvGetSize( sTokenEnvironment ), iParam ) )
      {
         sTokEnvDel( sTokenEnvironment );
         return 0;
      }
   }
   else
      sTokSet( sTokenEnvironment );
   return 1;
}

HB_FUNC( TOKENINIT )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE sStrLen = hb_parclen( 1 );
      const char * pcSeparatorStr;
      HB_SIZE sSeparatorStrLen;
      HB_SIZE nSkip;
      const char * pcSubStr, * pc;
      HB_SIZE sSubStrLen;
      TOKEN_ENVIRONMENT sTokenEnvironment;
      TOKEN_POSITION sTokenPosition;

      /* separator string */
      sSeparatorStrLen = hb_parclen( 2 );
      if( sSeparatorStrLen > 0 )
         pcSeparatorStr = hb_parc( 2 );
      else
      {
         pcSeparatorStr = sc_spcSeparatorStr;
         sSeparatorStrLen = sizeof( sc_spcSeparatorStr ) - 1;
      }

      /* skip width */
      if( HB_ISNUM( 3 ) )
         nSkip = hb_parns( 3 );
      else
         nSkip = HB_SIZE_MAX;
      if( nSkip == 0 )
         nSkip = HB_SIZE_MAX;

      /* allocate new token environment */
      if( ( sTokenEnvironment = sTokEnvNew() ) == NULL )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                      NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      HB_ERR_ARGS_BASEPARAMS );

         hb_retl( HB_FALSE );
         return;
      }

      pcSubStr = pcString;
      sSubStrLen = sStrLen;

      /* scan start condition */
      pc = pcSubStr - 1;

      for( ;; )
      {
         HB_SIZE sMatchedPos = sSeparatorStrLen;
         HB_SIZE nSkipCnt;

         /* nSkip */
         nSkipCnt = 0;
         do
         {
            sSubStrLen -= ( pc - pcSubStr ) + 1;
            pcSubStr = pc + 1;
            pc = ct_at_charset_forward( pcSubStr, sSubStrLen, pcSeparatorStr,
                                        sSeparatorStrLen, &sMatchedPos );
            nSkipCnt++;
         }
         while( nSkipCnt < nSkip && pc == pcSubStr );

         if( sSubStrLen == 0 )
            break;

         sTokenPosition.sStartPos = pcSubStr - pcString;
         if( pc == NULL )
            sTokenPosition.sEndPos = pcSubStr - pcString + sSubStrLen;
         else
            sTokenPosition.sEndPos = pc - pcString;

         if( ! sTokEnvAddPos( &sTokenEnvironment, &sTokenPosition ) )
         {
            int iArgErrorMode = ct_getargerrormode();

            if( iArgErrorMode != CT_ARGERR_IGNORE )
               ct_error( ( HB_USHORT ) iArgErrorMode, EG_MEM, CT_ERROR_TOKENINIT,
                         NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                         HB_ERR_ARGS_BASEPARAMS );

            sTokEnvDel( sTokenEnvironment );
            hb_retl( HB_FALSE );
            return;
         }

         if( pc == NULL )
            break;
      }

      /* save token environment to 4th parameter OR to the static */
      hb_retl( sTokSave( sTokenEnvironment, 4 ) );
   }
   else
   {
      /* if there is a token environment stored in either the 4th parameter or
         in the static variable -> rewind to first token */
      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 4, HB_FALSE );

      if( sTokenEnvironment != NULL )
      {
         /* rewind to first token */
         int iResult = sTokEnvSetPtr( sTokenEnvironment, 0 );

         if( ! sTokSave( sTokenEnvironment, 4 ) )
            iResult = HB_FALSE;
         hb_retl( iResult );
      }
      else
      {
         /* nothing to rewind -> return .F. */
         PHB_ITEM pSubst = NULL;
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                     CT_ERROR_TOKENINIT, NULL, HB_ERR_FUNCNAME, 0,
                                     EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

         if( pSubst != NULL )
            hb_itemReturnRelease( pSubst );
         else
            hb_retl( HB_FALSE );
      }
   }
}

HB_FUNC( TOKENNEXT )
{
   if( HB_ISCHAR( 1 ) )
   {
      const char * pcString = hb_parc( 1 );
      HB_SIZE sStrLen = hb_parclen( 1 );

      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 3, HB_FALSE );
      TOKEN_POSITION * psTokenPosition;

      /* token environment by parameter ... */
      if( sTokenEnvironment == NULL )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT,
                      NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT,
                      HB_ERR_ARGS_BASEPARAMS );
         hb_retc_null();
         return;
      }

      /* nth token or next token ?  */
      if( HB_ISNUM( 2 ) )
      {
         psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment, hb_parns( 2 ) - 1 );
         /* no increment here */
      }
      else
      {
         psTokenPosition = sTokEnvGetPos( sTokenEnvironment );
         /* increment counter */
         sTokEnvIncPtr( sTokenEnvironment );
      }

      if( psTokenPosition == NULL || sStrLen <= psTokenPosition->sStartPos )
      {
         int iArgErrorMode = ct_getargerrormode();

         if( iArgErrorMode != CT_ARGERR_IGNORE )
            ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENNEXT, NULL,
                      HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );

         sTokSave( sTokenEnvironment, 3 );
         hb_retc_null();
         return;
      }

      if( sStrLen < psTokenPosition->sEndPos )
         hb_retclen( pcString + psTokenPosition->sStartPos,
                     sStrLen - ( psTokenPosition->sStartPos ) );
      else
         hb_retclen( pcString + psTokenPosition->sStartPos,
                     ( psTokenPosition->sEndPos ) - ( psTokenPosition->sStartPos ) );

      sTokSave( sTokenEnvironment, 3 );
   }
   else
   {
      /* no string given, no token returns */
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNEXT, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retc_null();
   }
}

HB_FUNC( TOKENNUM )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 1, HB_TRUE );

   if( sTokenEnvironment != NULL )
      hb_retns( sTokEnvGetCnt( sTokenEnvironment ) );
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENNUM, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retns( 0 );
   }
}

HB_FUNC( TOKENEND )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 1, HB_TRUE );

   if( sTokenEnvironment != NULL )
      hb_retl( sTokEnvEnd( sTokenEnvironment ) );
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_TOKENEND, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         /* it is CT3 behaviour to return .T. if there's no string TokenInit()'ed */
         hb_retl( HB_TRUE );
   }
}

HB_FUNC( TOKENEXIT )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, HB_TRUE );

   if( sTokenEnvironment != NULL )
   {
      sTokSet( NULL );
      hb_retl( HB_TRUE );
   }
   else
      hb_retl( HB_FALSE );
}

HB_FUNC( TOKENAT )
{
   int iSeparatorPos = 0;
   HB_SIZE sCurrentIndex;
   TOKEN_ENVIRONMENT sTokenEnvironment;
   TOKEN_POSITION *psTokenPosition;

   if( HB_ISLOG( 1 ) )
      iSeparatorPos = hb_parl( 1 );

   sTokenEnvironment = sTokGet( 3, HB_TRUE );
   if( sTokenEnvironment == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT,
                   NULL, HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );

      hb_retns( 0 );
      return;
   }

   if( HB_ISNUM( 2 ) )
      sCurrentIndex = hb_parns( 2 ) - 1;
   else
      sCurrentIndex = sTokEnvGetPtr( sTokenEnvironment );

   psTokenPosition = sTokEnvGetPosIndex( sTokenEnvironment, sCurrentIndex );
   if( psTokenPosition == NULL )
   {
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         ct_error( ( HB_USHORT ) iArgErrorMode, EG_ARG, CT_ERROR_TOKENAT, NULL,
                   HB_ERR_FUNCNAME, 0, EF_CANDEFAULT, HB_ERR_ARGS_BASEPARAMS );

      hb_retns( 0 );
      return;
   }

   if( iSeparatorPos )
      hb_retns( psTokenPosition->sEndPos + 1 );
   else
      hb_retns( psTokenPosition->sStartPos + 1 );
}

HB_FUNC( SAVETOKEN )
{
   TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, HB_TRUE );

   if( sTokenEnvironment != NULL )
      hb_retclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ) );
   else
      hb_retc_null();
}

HB_FUNC( RESTTOKEN )
{
   TOKEN_ENVIRONMENT sNewTokEnv = sTokGet( 1, HB_FALSE );

   if( sNewTokEnv != NULL || ( HB_ISCHAR( 1 ) && hb_parclen( 1 ) == 0 ) )
   {
      TOKEN_ENVIRONMENT sTokenEnvironment = sTokGet( 0, HB_FALSE );

      if( sTokenEnvironment != NULL )
         hb_retclen( ( char * ) sTokenEnvironment, sTokEnvGetSize( sTokenEnvironment ) );
      else
         hb_retc_null();

      sTokSet( sNewTokEnv );
   }
   else
   {
      PHB_ITEM pSubst = NULL;
      int iArgErrorMode = ct_getargerrormode();

      if( iArgErrorMode != CT_ARGERR_IGNORE )
         pSubst = ct_error_subst( ( HB_USHORT ) iArgErrorMode, EG_ARG,
                                  CT_ERROR_RESTTOKEN, NULL, HB_ERR_FUNCNAME, 0,
                                  EF_CANSUBSTITUTE, HB_ERR_ARGS_BASEPARAMS );

      if( pSubst != NULL )
         hb_itemReturnRelease( pSubst );
      else
         hb_retc_null();
   }
}

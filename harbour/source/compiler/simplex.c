/*
 * Copyright 2000 Ron Pinkas <ron@profit-master.com>
 * www - http://www.Profit-Master.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link this file with other files to produce
 * an executable, this does not by itself causes the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking this file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

/* These are NOT overidable (yet). */
#define MAX_MATCH 4
#define TOKEN_SIZE 64

/* Language Definitions Readability. */
#define SELF_CONTAINED_WORDS_ARE LEX_WORD const aSelfs[] =
#define LANGUAGE_KEY_WORDS_ARE LEX_WORD const aKeys[] =
#define LANGUAGE_WORDS_ARE LEX_WORD const aWords[] =
#define LANGUAGE_RULES_ARE int const aiRules[][ MAX_MATCH + 2 ] =
#define ACCEPT_TOKEN_AND_DROP_DELIMITER_IF_ONE_OF_THESE(x) char const * szOmmit = x
#define ACCEPT_TOKEN_AND_RETURN_DELIMITER_IF_ONE_OF_THESE(x) char const * szReturn = x
#define DELIMITER_BELONGS_TO_TOKEN_IF_ONE_OF_THESE(x) char const * szAppend = x
#define START_NEW_LINE_IF_ONE_OF_THESE(x) char const * szNewLine = x
#define IF_SEQUENCE_IS(a, b, c, d) {a, b, c, d
#define REDUCE_TO(x, y) ,x, y }
#define PASS_THROUGH() ,0, 0 }
#define LEX_WORD(x) {x
#define AS_TOKEN(x) ,x }

/* Streams ("Pairs"). */
#define DEFINE_STREAM_AS_ONE_OF_THESE LEX_PAIR aPairs[] =
#define START_WITH(x) { x,
#define END_WITH(x) x,
#define STOP_IF_ONE_OF_THESE(x) x,
#define AND_IGNORE_DELIMITERS(x) x,
#define AS_PAIR_TOKEN(x) x }
#define STREAM_EXCEPTION( sPair, chrPair) \
        if( chrPair ) \
           printf(  "Exception: %c for stream at: \"%s\"\n", chrPair, sPair ); \
        else \
           printf(  "Exception: <EOF> for stream at: \"%s\"\n", chrPair, sPair ); \

/* Pairs. */
static char sPair[ 2048 ];
static char cTerm;
static BOOL bExclusive;
static char * sExclude;
static int iPairToken;

/* Self Contained Words. */
static char sSelf[ TOKEN_SIZE ];

typedef struct _LEX_WORD
{
   char  sWord[ TOKEN_SIZE ];
   int   iToken;
} LEX_WORD;                    /* support structure for KEYS and WORDS */

typedef struct _LEX_PAIR
{
   char  cStart;
   char  cTerm;
   char  sExclude[4];
   BOOL  bExclusive;
   int   iToken;
} LEX_PAIR;                    /* support structure for KEYS and WORDS */

#ifdef __cplusplus
   typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

/* Above are NOT overidable !!! Need to precede the Language Definitions. */

/* --------------------------------------------------------------------------------- */

/* Overidables. */
#define LEX_CUSTOM_ACTION    -65
#define ERR_TOO_COMPLEX_RULE -66
#define YY_BUF_SIZE 16384
#define YY_INPUT( a, b, c )

#define INIT_ACTION()
#define INTERCEPT_ACTION(x)
#define CUSTOM_ACTION(x)
#define NEW_LINE_ACTION()
#define ELEMENT_TOKEN(x) -1
#define DEBUG_INFO(x)
#define LEX_CASE(x)

/*
 * Can be used to indicate how many chars needed to qualify as an abbreviation for Key Words.
 *
#define LEX_ABBREVIATE_KEYS
#define LEX_ABBREVIATE_WORDS
*/

#include SLX_RULES

/* Declarations. */

FILE *yyin;      /* currently yacc parsed file */


extern void yyerror( char * ); /* parsing error management function */

#ifdef __cplusplus
   extern "C" int yywrap( void );
#else
   extern int yywrap( void );     /* manages the EOF of current processed file */
#endif

extern YYSTYPE yylval;

/* Use prototypes in function declarations. */
#define YY_USE_PROTOS

#ifdef YY_USE_PROTOS
   #define YY_PROTO(proto) proto
#else
   #define YY_PROTO(proto) ()
#endif

/* ---------------------------------------------------------------------------------------------- */

#define LEX_RULE_SIZE ( sizeof( (int) iRet ) * ( MAX_MATCH + 2 ) )
#define LEX_WORD_SIZE ( sizeof( LEX_WORD ) )
#define LEX_PAIR_SIZE ( sizeof( LEX_PAIR ) )

/* Using statics when we could use locals to eliminate Allocations and Deallocations each time the yylex is called and returns. */

/* Look ahead Tokens. */
static int  iHold = 0;
static int  aiHold[4];
static char asHold[4][ TOKEN_SIZE ];

/* Pre-Checked Tokens. */
static int  iReturn = 0;
static int  aiReturn[4];

/* Rules Support */
static int  aiMatched[ MAX_MATCH ];
static int  iMatched = 0;
static int  aiTentative[2] = { 0, 0 };
static int  iTentative = 0;
static int  aiProspects[ 256 ];
static int  iProspects = 0;
static int  iFound = 0;
static int  iReduce = 0;

/* yylex */
static char * tmpPtr;
static char sToken[TOKEN_SIZE];
static int  iLen = 0;
static char chr, cPrev = 0;
static int  iKey, iWord, iMatch, iRemove, iWordLen, iPush;
static char szLexBuffer[ YY_BUF_SIZE ];
static char * s_szBuffer;
static int  iSize = 0;
static int  iRet;
static BOOL bTmp;

/* Lex emulation */
char * yytext;
int yyleng;

/* NewLine Support. */
static BOOL bNewLine = TRUE, bStart = TRUE;

static int iSelfs = (int) ( sizeof( aSelfs  ) / LEX_WORD_SIZE );
static int iKeys  = (int) ( sizeof( aKeys   ) / LEX_WORD_SIZE );
static int iWords = (int) ( sizeof( aWords  ) / LEX_WORD_SIZE );
static int iRules = (int) ( sizeof( aiRules ) / LEX_RULE_SIZE );
static int iPairs = (int) ( sizeof( aPairs  ) / LEX_PAIR_SIZE );

int Reduce( int iToken, BOOL bReal );

/* --------------------------------------------------------------------------------- */

/* MACROS. */

/* Readability Macros. */
#define LEX_RULE_SIZE ( sizeof( (int) iRet ) * ( MAX_MATCH + 2 ) )
#define LEX_WORD_SIZE ( sizeof( LEX_WORD ) )
#define LEX_PAIR_SIZE ( sizeof( LEX_PAIR ) )
#define IF_TOKEN_READY()  if( iReturn )
#define IF_TOKEN_ON_HOLD()  if( iHold )
#define REDUCE( x ) Reduce( (x), TRUE )
#define RESET_LEX() { iLen = 0; iMatched = 0; iHold = 0; iReturn = 0; bNewLine = TRUE; bStart = TRUE; }
#define FORCE_REDUCE() Reduce( 0, TRUE )

#define HOLD_TOKEN(x, y) \
                      /* Last In, First Out */ \
                      { iRet = x ; aiHold[ iHold++ ] = iRet; \
                        \
                        if( y ) \
                        { \
                           strcpy( asHold[ iHold - 1 ], y ); \
                        } \
                        else \
                        { \
                           asHold[ iHold - 1 ][0] = '\0'; \
                        } \
                        \
                        DEBUG_INFO( printf( "Placed on hold: %i Position %i Text: >%s<\n", iRet, iHold, asHold[ iHold - 1 ] ) ); \
                        DEBUG_INFO( printf("Now Holding %i Tokens: %i %i %i %i\n", iHold, aiHold[0], aiHold[1], aiHold[2], aiHold[3] ) ); \
                      }

#define RETURN_TOKEN(x, y) \
           \
           iRet = (x); \
           if( (iRet) ) \
           { \
               LEX_RETURN(iRet, y); \
           } \
           else \
           { \
              goto Start; \
           }

#define IF_NEWLINE(chr) \
            \
            tmpPtr = (char*) szNewLine; \
            while ( *tmpPtr && chr != *tmpPtr ) \
            { \
               tmpPtr++; \
            } \
            /* NewLine Found?. */\
            if( *tmpPtr )

#define IF_BEGIN_PAIR(chr) \
         {  register int iPair = 0;\
            \
            while( iPair < iPairs ) \
            { \
               if( chr == aPairs[iPair].cStart ) \
               { \
                  /* Terminator to look for. */ \
                  cTerm      =          aPairs[iPair].cTerm; \
                  bExclusive =          aPairs[iPair].bExclusive; \
                  sExclude   = (char *) aPairs[iPair].sExclude; \
                  iPairToken =          aPairs[iPair].iToken; \
                  break; \
               } \
               iPair++; \
            } \
            bTmp = ( iPair < iPairs ); \
            \
         } \
         /* Begin New Pair. */ \
         if( bTmp )

#define CHECK_SELF_CONTAINED(chr) \
            \
            DEBUG_INFO( printf( "Checking %i Selfs for %c At: >%s<\n", iSelfs, chr, szBuffer - 1 ) ); \
            \
         {\
            register int iSelf = 0, iSelfLen; \
            register char chrSelf; \
            \
            while( iSelf < iSelfs ) \
            { \
               chrSelf = LEX_CASE(chr);\
               \
               if( chrSelf == aSelfs[iSelf].sWord[0] ) \
               { \
                  sSelf[0] = chrSelf; \
                  iSelfLen = 1; \
                  chrSelf = LEX_CASE( *szBuffer ); \
                  \
                  while( aSelfs[iSelf].sWord[iSelfLen] ) \
                  { \
                     if( chrSelf == aSelfs[iSelf].sWord[iSelfLen] ) \
                     { \
                        sSelf[ iSelfLen ] = chrSelf; \
                     } \
                     else \
                     { \
                        break; \
                     } \
                     \
                     iSelfLen++; \
                     \
                     /* TODO: worry about End of Buffer. */ \
                     \
                     /* Peek at Next Character. */ \
                     chrSelf = LEX_CASE( *( szBuffer + iSelfLen - 1 ) ); \
                  } \
                  \
                  /* Match */ \
                  if( aSelfs[iSelf].sWord[iSelfLen] == '\0' ) \
                  { \
                     /* Moving to next postion after the Self Contained Word. */ \
                     szBuffer += ( iSelfLen - 1 ); \
                     \
                     sSelf[ iSelfLen ] = '\0'; \
                     \
                     if( iLen ) \
                     { \
                        DEBUG_INFO( printf( "Holding Self >%s<\n", sSelf ) ); \
                        \
                        HOLD_TOKEN( aSelfs[iSelf].iToken, sSelf ); \
                        \
                        /* Terminate current token and check it. */ \
                        sToken[ iLen ] = '\0'; \
                        \
                        /* Last charcter read. */\
                        chr = chrSelf;\
                        \
                        goto CheckToken; \
                     } \
                     else \
                     { \
                        DEBUG_INFO( printf( "Reducing Self >%s<\n", sSelf ) ); \
                        \
                        if( bNewLine )\
                        {\
                           bNewLine = FALSE;\
                           NEW_LINE_ACTION();\
                        }\
                        \
                        RETURN_TOKEN( REDUCE( aSelfs[iSelf].iToken ), sSelf ); \
                     } \
                  } \
               } \
               \
               iSelf++; \
            } \
         }

#define IF_ABORT_PAIR(chrPair) \
                        tmpPtr = sExclude; \
                        while ( *tmpPtr && chrPair != *tmpPtr ) \
                        { \
                            tmpPtr++; \
                        } \
                        \
                        /* Exception. */ \
                        if( *tmpPtr )

#define IF_OMMIT(chr) \
            /* Delimiter to Ommit? */ \
            tmpPtr = (char*) szOmmit; \
            while ( *tmpPtr && chr != *tmpPtr ) \
            { \
                tmpPtr++; \
            } \
            \
            /* Delimiter to Ommit found. */ \
            if ( *tmpPtr )

#define IF_APPEND_DELIMITER(chr) \
             /* Delimiter to Append? */ \
            tmpPtr = (char*) szAppend; \
            while ( *tmpPtr && chr != *tmpPtr ) \
            { \
                 tmpPtr++; \
            } \
            \
            /* Delimiter to Append found. */ \
            if( *tmpPtr )

#define IF_RETURN_DELIMITER(chr) \
             /* Returnable Delimiter? */ \
            tmpPtr = (char*) szReturn; \
            while ( *tmpPtr && chr != *tmpPtr ) \
            { \
                 tmpPtr++; \
            } \
            \
            /* Returnable Delimiter found. */ \
            if( *tmpPtr )

#define IF_BELONG_LEFT(chr) \
            /* Give precedence to associate rules */ \
            DEBUG_INFO( printf(  "Checking Left for: '%c' cPrev: '%c'\n", chr, cPrev ) ); \
            \
            if( iMatched && Reduce( chr, FALSE ) )

#define RETURN_READY_TOKEN() \
            \
            /* Ready from previous call. */ \
            iReturn--; \
            iRet = aiReturn[iReturn]; \
            \
            DEBUG_INFO( printf(  "Returning Ready: %i\n", iRet ) ); \
            \
            LEX_RETURN(iRet, NULL);

#define RELEASE_TOKEN() \
            \
            /* Ready from previous call. */ \
            /* Last in First Out. */ \
            iHold--; \
            iRet = aiHold[iHold]; \
            if( asHold[iHold] ) \
            { \
               strcpy( (char*) sToken, asHold[iHold] );\
            } \
            \
            DEBUG_INFO( printf(  "Released %i Now Holding %i Tokens: %i %i %i %i\n", iRet, iHold, aiHold[0], aiHold[1], aiHold[2], aiHold[3] ) ); \
            \
            if( iRet == -1 && iMatched ) \
            { \
               DEBUG_INFO( printf(  "Reducing Held: <EOF>\n" ) ); \
               \
               /* Returning Pending Rule Match Tokens without further tests. */ \
               iMatch = 0; \
               while( iMatch < iMatched ) \
               { \
                  DEBUG_INFO( printf(  "Returning Pending Match Tokens at: <EOF>\n" ) ); \
                  \
                  aiReturn[ iReturn++ ] = aiMatched[ iMatch ]; \
                  iMatch++; \
               } \
               iMatched = 0;\
               goto Start;\
            } \
            else if( iRet == -1 ) \
            { \
               RESET_LEX();\
               return -1; \
            }\
            else if( iRet < 256 )\
            {\
               IF_NEWLINE( (char) iRet )\
               {\
                  bNewLine = TRUE;\
               }\
               else\
               {\
                  bNewLine = FALSE;\
               }\
            }\
            \
            DEBUG_INFO( printf(  "Reducing Held: %i Pos: %i\n", iRet, iHold ) ); \
            \
            RETURN_TOKEN( REDUCE( iRet ), sToken );

#define LEX_RETURN(x, y) \
        \
        if( x <= LEX_CUSTOM_ACTION ) \
        { \
            CUSTOM_ACTION(x); \
            \
            if( x ) \
            { \
               INTERCEPT_ACTION(x)\
               \
               s_szBuffer = szBuffer;\
               \
               return x; \
            } \
            else \
            { \
               goto Start; \
            } \
        } \
        else \
        { \
            DEBUG_INFO( printf(  "Returning: %i\n", x ) ); \
            \
            INTERCEPT_ACTION(x)\
            \
            s_szBuffer = szBuffer;\
            \
            return x; \
        }

#define ACCEPT_TOKEN()\
{\
   DEBUG_INFO( printf(  "Accepting Token %i After Tokens: %i %i %i\n", iToken, aiMatched[0], aiMatched[1], aiMatched[2] ) );\
   \
   if( iMatched == MAX_MATCH )\
   {\
      printf(  "Maximum depth reached! Multiple Rules for same Tokens: %i %i %i %i - Please correct the rules.\n", aiMatched[0], aiMatched[1], aiMatched[2], aiMatched[3] );\
      \
      GIVE_UP();\
   }\
   else\
   {\
      aiMatched[ iMatched++ ] = iToken;\
   }\
}

#define FIND_MATCH()\
{\
   if( iProspects )\
   {\
      SCAN_PROSPECTS();\
   }\
   else\
   {\
      SCAN_RULES();\
   }\
}

#define SCAN_PROSPECTS()\
{\
   register int iScan = 0;\
   \
   DEBUG_INFO( printf(  "Scaning %i Prospects for %i at Pos: %i\n", iProspects, iToken, iMatched ) ); \
   \
   iFound = 0;\
   while( iScan < iProspects )\
   {\
      if( aiRules[ aiProspects[iScan] ][ iMatched ] == iToken )\
      {\
         /* No more Tokens - Rule Match. */\
\
         if( iMatched == MAX_MATCH - 1 || aiRules[ aiProspects[iScan] ][ iMatched + 1 ] == 0 )\
         {\
            iFound++;\
            iReduce = aiProspects[iScan];\
         }\
         else\
         {\
            /* Already in prospect list - still a prospect, do nothing. */\
         }\
      }\
      else\
      {\
         /* No longer a prospect. */\
         REMOVE_PROSPECT(iScan);\
\
         /* Has to continue without increasing the counter, because of side effect of REMOVE_PROSPECT(). */\
         continue;\
      }\
\
      iScan++;\
   }\
\
   if( iProspects == 1 && iFound )\
   {\
      /* Already counted as a FOUND. */\
      if( bReal )\
      {\
         iProspects = 0;\
      }\
   }\
}

#define SCAN_RULES()\
{\
   register int iScan = 0;\
   \
   DEBUG_INFO( printf(  "Scaning %i Rules for %i at Pos: %i\n", iRules, iToken, iMatched ) ); \
   \
   iFound = 0;\
   iProspects = 0;\
   while( iScan < iRules )\
   {\
      /* No prospects means we only search 1st Token of Rules. */\
      if( aiRules[iScan][0] == iToken )\
      {\
\
         /* No more Tokens - Rule Match. */\
         if( aiRules[iScan][1] == 0 )\
         {\
            iReduce = iScan;\
            iFound++;\
         }\
         else\
         {\
            /* Adding this Rule to the Prospects List. */\
            ADD_PROSPECT(iScan);\
         }\
      }\
\
      iScan++;\
   }\
}

#define ADD_PROSPECT(iScan)\
{\
   aiProspects[ iProspects++ ] = iScan;\
}

#define REMOVE_PROSPECT(iScan)\
{\
   DEBUG_INFO( printf(  "Removing Prospect: %i Of: %i\n", iScan, iProspects ) );\
\
   aiProspects[iProspects--] = 0;\
   iRemove = iScan;\
   while( iRemove < iProspects )\
   {\
      /* iProspectNo is already Zero Based. */\
      aiProspects[ iRemove ] = aiProspects[ iRemove + 1 ];\
\
      iRemove++;\
   }\
}

#define REVERT_OR_GIVEUP()\
{\
    DEBUG_INFO( printf(  "Revert Or Giveup for %i After %i %i %i\n", iToken, aiMatched[0], aiMatched[1], aiMatched[2] ) );\
\
   /* Have to push the unmatched Token first, so it will Pop LAST after the Left Shift of reverted Tokens. */\
\
   /* Avoid infinite loop, don't push if this is the only Token, it will be returned instead. */\
\
   /* iToken maybe ZERO indicating REDUCTION to be FORCED. */\
   if( iMatched && iToken )\
   {\
      PUSH_TOKEN( iToken );\
   }\
\
   if( iTentative )\
   {\
      if( iMatched > iTentative )\
      {\
         /* Unused Tokens must be pushed before the reduction, so the Reductions will be Poped First. */\
\
         DEBUG_INFO( printf(  "Reverting from %i to %i Shifting to Reductions: %i %i\n", iMatched, iTentative, aiTentative[0], aiTentative[1] ) );\
\
         /* iTentative is Zero Based, iMatched is 1 Based, but iMatched not increased yet for the unmatched Token. */\
\
         /* Shift unused Token[s] Left. */\
         PUSH_UN_MATCHED( iMatched - iTentative );\
      }\
\
      /* Will put the Reductions back into the Hold Stack to support Recursive Rules. */\
      REDUCE_TENTATIVE();\
\
      CLEAN_UP();\
   }\
   else\
   {\
      /* Will get rid of the 1st (Left) Token and will shift the unmatched Tokens 1 position to the left.\
         So we will start again with the First unmatched Token. */\
\
      GIVE_UP();\
   }\
}

#define REDUCE_TENTATIVE()\
{\
   /* If Associate Left than don't reduce. */\
   if( aiTentative[0] )\
   {\
      DEBUG_INFO( printf(  "Reducing Tentative for %i Matches to %i %i\n", iTentative, aiTentative[0], aiTentative[1] ) );\
\
      /* Have to push from Right to Left so Last pushed will be First poped. */\
      if( aiTentative[1] )\
      {\
         PUSH_TOKEN( aiTentative[1] );\
         aiTentative[1] = 0;\
      }\
\
      PUSH_TOKEN( aiTentative[0] );\
      aiTentative[0] = 0;\
   }\
   else\
   {\
      DEBUG_INFO( printf(  "Returning Tentative for %i Matches, Associate Left of %i %i %i\n", iTentative, aiMatched[0], aiMatched[1], aiMatched[2] ) );\
\
      /* Can't have the last Item becuase we just failed a rule. */ \
\
      if( aiMatched[ 2 ] )\
      {\
         aiReturn[ iReturn++ ] = aiMatched[ 2 ];\
      }\
      if( aiMatched[ 1 ] )\
      {\
         aiReturn[ iReturn++ ] = aiMatched[ 1 ];\
      }\
      aiReturn[ iReturn++ ] = aiMatched[ 0 ];\
   }\
}

#define PUSH_UN_MATCHED( iHowMany ) /* Shift Left. */\
{\
   iPush = iHowMany;\
   while( iPush )\
   {\
      iPush--;\
      iMatched--;\
      PUSH_TOKEN( aiMatched[ iMatched ] );\
   }\
}

#define PUSH_TOKEN( iPushToken )\
{\
   aiHold[ iHold++ ] = iPushToken;\
\
   /* We don't know what was the text value of this Token any more. */\
   asHold[ iHold - 1 ][0] = '\0'; \
}

#define SAVE_TENTATIVE()\
{\
   iTentative = iMatched;\
   aiTentative[1] = aiRules[ iReduce ][ MAX_MATCH + 1 ];\
   aiTentative[0] = aiRules[ iReduce ][ MAX_MATCH ];\
}

#define GIVE_UP()\
{\
   /* We may have Tokens to shift Left. */\
   if( iMatched == 0)\
   {\
      CLEAN_UP();\
\
      return iToken;\
   }\
   else if( iMatched == 1 )\
   {\
      iRet = aiMatched[0];\
\
      CLEAN_UP();\
\
      return iRet;\
   }\
   else\
   {\
      DEBUG_INFO( printf(  "Shifting Left %i Tokens\n", iMatched - 1 ) );\
\
      if( iMatched > 1 )\
      {\
         PUSH_UN_MATCHED( iMatched - 1 );\
      }\
\
      iRet = aiMatched[0];\
\
      CLEAN_UP();\
\
      return iRet;\
   }\
}

#define CLEAN_UP()\
{\
   iMatched    = 0;\
   iTentative  = 0;\
   aiMatched[0] = 0;\
   aiMatched[1] = 0;\
   aiMatched[2] = 0;\
   aiMatched[3] = 0;\
}

#define REDUCE_RULE()\
{\
   /* If Associate Left than don't push reduce, pass through. */\
\
   if( aiRules[ iReduce ][ MAX_MATCH ] )\
   {\
      DEBUG_INFO( printf(  "Reducing: %i %i %i %i Shifting To: %i %i\n", aiMatched[0], aiMatched[1], aiMatched[2], aiMatched[3], aiRules[ iReduce ][ MAX_MATCH ], aiRules[ iReduce ][ MAX_MATCH + 1 ] ) );\
\
      /* Have to push from Right to Left so Last pushed will be First poped. */\
      if( aiRules[ iReduce ][ MAX_MATCH + 1 ] )\
      {\
         PUSH_TOKEN( aiRules[ iReduce ][ MAX_MATCH + 1 ] );\
      }\
      PUSH_TOKEN( aiRules[ iReduce ][ MAX_MATCH ] );\
\
      /* Dispose of tokens and reset. */ \
      CLEAN_UP();\
   }\
   else\
   {\
      DEBUG_INFO( printf(  "Associating Left: %i %i %i %i Shifting To: New Token %i\n", aiMatched[0], aiMatched[1], aiMatched[2], aiMatched[3], iToken ) );\
\
      /* Have to push from Right to Left so Last pushed will be First poped. */\
      if( aiMatched[ 3 ] )\
      {\
         aiReturn[ iReturn++ ] = aiMatched[ 3 ];\
      }\
      if( aiMatched[ 2 ] )\
      {\
         aiReturn[ iReturn++ ] = aiMatched[ 2 ];\
      }\
      if( aiMatched[ 1 ] )\
      {\
         aiReturn[ iReturn++ ] = aiMatched[ 1 ];\
      }\
      aiReturn[ iReturn++ ] = aiMatched[ 0 ];\
\
      /* Dispose of tokens and reset. */ \
      CLEAN_UP();\
   }\
}

#ifndef YY_DECL
   #define YY_DECL int yylex YY_PROTO(( void ))
#endif

YY_DECL
{
 register char * szBuffer = s_szBuffer;

 Start:
    IF_TOKEN_READY()
    {
       RETURN_READY_TOKEN();
    }

    IF_TOKEN_ON_HOLD()
    {
       RELEASE_TOKEN();
    }

    if( iSize == 0 )
    {
       /*
       if( szLexBuffer == NULL )
       {
          szLexBuffer = malloc( YY_BUF_SIZE );
       }

       if( yytext == NULL )
       {
          yytext = malloc( YY_BUF_SIZE );
       }
       */

       if( bStart )
       {
         bStart = FALSE;
         INIT_ACTION();
       }

       YY_INPUT( (char*) szLexBuffer, iSize, YY_BUF_SIZE );

       if( iSize )
       {
          szBuffer = (char*) szLexBuffer;

          DEBUG_INFO( printf(  "New Buffer: >%s<\n", szBuffer ) );
       }
       else
       {
          if( iMatched )
          {
              /* Returning Pending Rule Match Tokens without further tests. */ \
              iMatch = 0; \
              while( iMatch < iMatched ) \
              { \
                 DEBUG_INFO( printf(  "Returning Pending Match Tokens at: <EOF>\n" ) ); \
                 \
                 aiReturn[ iReturn++ ] = aiMatched[ iMatch ]; \
                 iMatch++; \
              } \
              iMatched = 0;\
              goto Start;\
          }

          DEBUG_INFO( printf(  "Returning: <EOF>\n" ) );

          return -1;
       }
    }

    iLen = 0;
    yyleng = 0;

    while ( 1 )
    {
        if ( iSize && *szBuffer )
        {
            cPrev = chr;

            /* Get next character. */
            iSize--;
            chr = LEX_CASE(*szBuffer++) ;

            //DEBUG_INFO( printf(  "Char: %c\n", chr ) );

            IF_OMMIT(chr)
            {
                if ( iLen )
                {
                   /* Terminate current token and check it. */
                   sToken[ iLen ] = '\0';

                   DEBUG_INFO( printf(  "Token: \"%s\" Ommited: \'%c\'\n", sToken, chr ) );

                   goto CheckToken;
                }
                else
                {
                   continue;
                }
            }

            CHECK_SELF_CONTAINED(chr);

            /* Soft Pair Terminator ? */
            if ( cTerm && chr == cTerm )
            {
                /* Reset. */
                cTerm = 0;

                /* Terminate current token and check it. */
                sToken[ iLen++ ] = chr;
                sToken[ iLen ] = '\0';

                DEBUG_INFO( printf(  "Pair at %c = %s\n", chr, sToken ) );

                goto CheckToken;
            }

            //DEBUG_INFO( printf(  "Scaning Pairs >%s< for %c\n", szPairs1, chr ) );

            /* New Pair ? */
            IF_BEGIN_PAIR( chr )
            {
                if( iLen )
                {
                    /* Resume here on next call. */
                    szBuffer--;
                    iSize++;
                    cTerm = 0;
                    /* So cPrev will remain intact. */
                    chr = cPrev;

                    DEBUG_INFO( printf(  "Pushed back: '%c' Buffer = >%s<\n", chr, szBuffer ) );

                    /* Terminate and Check Token to the left. */
                    sToken[ iLen ] = '\0';

                    DEBUG_INFO( printf(  "Token: \"%s\" before New Pair at: \'%c\'\n", sToken, chr ) );

                    goto CheckToken ;
                }

                IF_BELONG_LEFT( chr )
                {
                    DEBUG_INFO( printf(  "Reducing Left '%c'\n", chr ) );
                    cTerm = 0;
                    RETURN_TOKEN( REDUCE( (int) chr ), NULL );
                }

                /* Soft ? */
                if ( bExclusive )
                {
                    {
                        register int iPairLen = 0;
                        register char chrPair;

                        /* Look for the terminator. */
                        while ( *szBuffer )
                        {
                            /* Next Character. */
                            chrPair = *szBuffer++ ;

                            //DEBUG_INFO( printf(  "Checking: '%c' in Exclusive Pair\n", chrPair ) );

                            /* Check if exception. */
                            IF_ABORT_PAIR( chrPair )
                            {
                               sPair[ iPairLen ] = '\0';

                               STREAM_EXCEPTION( sPair, chrPair);

                               /* Resetting. */
                               cTerm = 0;

                               /* Last charcter read. */
                               chr = chrPair;

                               goto on_error;
                            }
                            /* Terminator found. */
                            else if( chrPair == cTerm )
                            {
                               sPair[ iPairLen ] = '\0';

                               DEBUG_INFO( printf(  "Returning Pair = >%s<\n", sPair ) );

                               /* Resetting. */
                               cTerm = 0;

                               /* Last charcter read. */
                               chr = chrPair;

                               if( bNewLine )
                               {
                                  bNewLine = FALSE;
                                  NEW_LINE_ACTION();
                               }

                               /* LITERAL */
                               RETURN_TOKEN( REDUCE( iPairToken ), NULL );
                            }
                            else
                            {
                               //DEBUG_INFO( printf(  "Adding %c to Pair Pos: %i\n", chrPair, iPairLen ) );

                               /* Accumulating. */
                               sPair[ iPairLen++ ] = chrPair;
                            }
                        }
                    }

                    /* EOF */
                    STREAM_EXCEPTION( sPair, NULL );

                    /* Resetting. */
                    cTerm = 0;

                    goto on_error;
                }
                else
                {
                    DEBUG_INFO( printf(  "Opened Pair, looking for: %c\n", cTerm ) );

                    sToken[iLen++] = chr;

                    /* Scan next charcter. */
                    continue;
                }
            }
            /* End Pairs. */

            /* NewLine ? */
            IF_NEWLINE( chr )
            {
                if( iLen )
                {
                    /* Will return NewLine on next call. */
                    HOLD_TOKEN(chr, NULL);

                    /* Terminate current token and check it. */
                    sToken[ iLen ] = '\0';

                    DEBUG_INFO( printf(  "Token: \"%s\" at <NewLine> Holding: \'%c\'\n", sToken, chr ) );

                    goto CheckToken;
                }
                else
                {
                    DEBUG_INFO( printf(  "Reducing NewLine '%c'\n", chr ) );
                    bNewLine = TRUE;
                    RETURN_TOKEN( REDUCE( (int) chr ), NULL );
                }
            }

            IF_APPEND_DELIMITER( chr )
            {
                /* Append and Terminate current token and check it. */
                sToken[ iLen++ ] = chr;
                sToken[ iLen ] = '\0';

                DEBUG_INFO( printf(  "Token: \"%s\" Appended: \'%c\'\n", sToken, chr ) );

                goto CheckToken;
            }

            IF_RETURN_DELIMITER( chr )
            {
                if( iLen )
                {
                    /* Will be rturned on next cycle. */
                    HOLD_TOKEN(chr, NULL);

                    /* Terminate current token and check it. */
                    sToken[ iLen ] = '\0';

                    DEBUG_INFO( printf(  "Token: \"%s\" Holding: \'%c\'\n", sToken, chr ) );

                    goto CheckToken;
                }
                else
                {
                    DEBUG_INFO( printf(  "Reducing Delimiter: '%c'\n", chr ) );

                    if( bNewLine )
                    {
                       bNewLine = FALSE;
                       NEW_LINE_ACTION();
                    }

                    RETURN_TOKEN( REDUCE( (int) chr ), NULL );
                }
            }

            /* Acumulate and scan next Charcter. */
            sToken[ iLen++ ] = chr;

            //DEBUG_INFO( printf(  "Added: %c\n", chr ) );

            continue;
        }
        else
        {
            YY_INPUT( (char*) szLexBuffer, iSize, YY_BUF_SIZE );

            if( iSize )
            {
               szBuffer = (char*) szLexBuffer;
               continue;
            }
            else
            {
                if( iLen )
                {
                   /* <EOF> */
                   HOLD_TOKEN(-1, NULL);

                   /* Terminate current token and check it. */
                   sToken[ iLen ] = '\0';

                   DEBUG_INFO( printf(  "Token: \"%s\" at: \'<EOF>\'\n", sToken ) );

                   goto CheckToken;
                }
                else
                {
                   goto Start;
                }
            }
        }

    on_error:
        continue;

    CheckToken:
        {
            #ifdef LEX_ABBREVIATE_KEYS
               iWordLen = strlen( (char*) sToken );

               if( iWordLen < LEX_ABBREVIATE_KEYS )
               {
                  iWordLen = LEX_ABBREVIATE_KEYS;
               }
            #endif

            iKey = 0;
            while ( bNewLine && iKey < iKeys )
            {
                //DEBUG_INFO( printf(  "Comparing: \"%s\" and \"%s\"\n", sToken, aTokens[iKey] ) );

                #ifdef LEX_ABBREVIATE_KEYS
                   if( strncmp( (char*) sToken, (char*)( aKeys[ iKey++ ].sWord ), iWordLen ) == 0 )
                #else
                   if( strcmp( (char*) sToken, (char*) ( aKeys[ iKey++ ].sWord ) ) == 0 )
                #endif
                {
                    DEBUG_INFO( printf(  "Reducing Key Word: %s\n", (char*) sToken ) );

                    bNewLine = FALSE;
                    NEW_LINE_ACTION();

                    RETURN_TOKEN( REDUCE( aKeys[ iKey - 1 ].iToken ), (char*) sToken );
                }
            }

            if( bNewLine )
            {
               bNewLine = FALSE;
               NEW_LINE_ACTION();
            }

            #ifdef LEX_ABBREVIATE_WORDS
               iWordLen = strlen( (char*) sToken );

               if( iWordLen < LEX_ABBREVIATE_WORDS )
               {
                  iWordLen = LEX_ABBREVIATE_WORDS;
               }
            #endif

            iWord = 0;
            while ( iWord < iWords )
            {
                #ifdef LEX_ABBREVIATE_WORDS
                   if( strncmp( (char*) sToken, (char*) ( aWords[ iWord++ ].sWord ), iWordLen ) == 0 )
                #else
                   if( strcmp( (char*) sToken, (char*) ( aWords[ iWord++ ].sWord ) ) == 0 )
                #endif
                {
                    DEBUG_INFO( printf(  "Reducing Word: %s\n", (char*) sToken ) );

                    RETURN_TOKEN( REDUCE( aWords[ iWord - 1 ].iToken ), (char*) sToken );
                }
            }

            DEBUG_INFO( printf(  "Reducing Element: \"%s\"\n", (char*) sToken ) );

            /* "Returns" the Token as iRet. */
            ELEMENT_TOKEN( sToken )

            RETURN_TOKEN( REDUCE( iRet ), sToken );
        }
    }
}

int Reduce( int iToken, BOOL bReal )
{
   /* The search rutine will "return" the number of Matches in iFound, number of Prospects in iProspects, and the last
      (and hopefuly only) Matched Rule No in iReduce. */

   if( iToken )
   {
      DEBUG_INFO( printf(  "Checking Token: %i After %i %i %i at Pos: %i\n", iToken, aiMatched[0], aiMatched[1], aiMatched[2], iMatched ) );

      FIND_MATCH();
   }
   else
   {
      DEBUG_INFO( printf(  "Reduced Forced After %i %i %i at Pos: %i\n", aiMatched[0], aiMatched[1], aiMatched[2], iMatched ) );

      /* Force Reduce was requested. */
      iFound = 0;
      iProspects = 0;
   }

   if( ! bReal )
   {
      return ( iFound || iProspects );
   }

   DEBUG_INFO( printf(  "Found %i Rules and %i Prospects for Token %i After %i %i %i\n", iFound, iProspects, iToken, aiMatched[0], aiMatched[1], aiMatched[2] ) );

   /* Pass through. */
   if( iMatched == 0 && iFound == 0 && iProspects == 0 )
   {
      DEBUG_INFO( printf(  "Passed Through Token %i After %i %i %i\n", iToken, aiMatched[0], aiMatched[1], aiMatched[2] ) );
      return iToken;
   }

   if( iFound || iProspects )
   {
      /* Adding to Matched List. */
      ACCEPT_TOKEN();
   }

   switch( iFound )
   {
       /* Match Failed. */
       case 0 :
          if( iProspects )
          {
             /* Can't reduce yet, can't "bookmark" possible reduction either, must continue checking. */

             return 0;
          }
          else
          {
             REVERT_OR_GIVEUP();
             return 0;
          }

       case 1 :
          if( iProspects )
          {
             /* Can't reduce yet, "bookmark" possible reduction here, and continue checking. */

             SAVE_TENTATIVE();

             return 0;
          }
          else
          {
             /* One Match and no additional Prospects - we can reduce. */

             REDUCE_RULE();

             return 0;
          }

       default :
          if( iProspects )
          {
             /* Can't reduce yet, and can't "bookmark" possible reduction either, must continue checking. */

             return 0;
          }
          else
          {
             printf(  "Multiple Rules for same Tokens: %i %i %i %i - Please correct the rules.\n", aiMatched[0], aiMatched[1], aiMatched[2], aiMatched[3] );

             return 0;
          }
   }
}

#ifdef __cplusplus
   YY_BUFFER_STATE yy_create_buffer( FILE * pFile, int iBufSize )
#else
   void * yy_create_buffer( FILE * pFile, int iBufSize )
#endif
{
   HB_SYMBOL_UNUSED( pFile );
   HB_SYMBOL_UNUSED( iBufSize );
   iSize = 0;

   #ifdef __cplusplus
      return (YY_BUFFER_STATE) szLexBuffer;
   #else
      return (void*) szLexBuffer;
   #endif
}

#ifdef __cplusplus
   void yy_switch_to_buffer( YY_BUFFER_STATE pBuffer )
#else
   void yy_switch_to_buffer( void * pBuffer )
#endif
{
   HB_SYMBOL_UNUSED( pBuffer );
   FORCE_REDUCE();
   iSize = 0;
}

#ifdef __cplusplus
   void yy_delete_buffer( YY_BUFFER_STATE pBuffer )
#else
   void yy_delete_buffer( void * pBuffer )
#endif
{
   HB_SYMBOL_UNUSED( pBuffer );
   FORCE_REDUCE();
   iSize = 0;
}

void * yy_bytes_buffer( char * pBuffer, int iBufSize )
{
   s_szBuffer = pBuffer;
   iSize = iBufSize;
   return s_szBuffer;
}

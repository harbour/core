/*
 * $Id$
 */

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
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

/* NOT overidable (yet). */
#define MAX_MATCH 4

#ifndef TOKEN_SIZE
   #define TOKEN_SIZE 64
#endif

/* Language Definitions Readability. */
#define SELF_CONTAINED_WORDS_ARE LEX_WORD static aSelfs[] =
#define LANGUAGE_KEY_WORDS_ARE LEX_WORD static aKeys[] =
#define LANGUAGE_WORDS_ARE LEX_WORD static aWords[] =
#define LANGUAGE_RULES_ARE static int aiRules[][ MAX_MATCH + 2 ] =
#define ACCEPT_TOKEN_AND_DROP_DELIMITER_IF_ONE_OF_THESE(x) static char *szOmmit = x
#define ACCEPT_TOKEN_AND_RETURN_DELIMITERS static LEX_DELIMITER aDelimiters[] =
#define DELIMITER_BELONGS_TO_TOKEN_IF_ONE_OF_THESE(x) static char *szAppend = x
#define START_NEW_LINE_IF_ONE_OF_THESE(x) static char *szNewLine = x
#define IF_SEQUENCE_IS(a, b, c, d) {a, b, c, d
#define REDUCE_TO(x, y) ,x, y }
#define PASS_THROUGH() ,0, 0 }
#define LEX_DELIMITER(x) {x
#define LEX_WORD(x) {x
#define AS_TOKEN(x) ,x }

/* Streams ("Pairs"). */
#define DEFINE_STREAM_AS_ONE_OF_THESE LEX_PAIR aPairs[] =
#define START_WITH(x) { x,
#define END_WITH(x) x,
#define STOP_IF_ONE_OF_THESE(x) x,
#define TEST_LEFT(x) x,
#define AS_PAIR_TOKEN(x) x }
#define STREAM_EXCEPTION( sPair, chrPair) \
        if( chrPair ) \
        { \
           printf(  "Exception: %c for stream at: \"%s\"\n", chrPair, sPair ); \
        } \
        else \
        { \
           printf(  "Exception: <EOF> for stream at: \"%s\"\n", chrPair, sPair ); \
        } \

/* Pairs. */
#ifndef MAX_STREAM
   #define MAX_STREAM 2048
#endif
#ifndef MAX_STREAM_STARTER
   #define MAX_STREAM_STARTER 2
#endif
#ifndef MAX_STREAM_TERMINATOR
   #define MAX_STREAM_TERMINATOR 2
#endif
#ifndef MAX_STREAM_EXCLUSIONS
   #define MAX_STREAM_EXCLUSIONS 2
#endif

static char sPair[ MAX_STREAM ];
static char * sStart, * sTerm;
static char * sExclude;
static BOOL   bTestLeft;
static int iPairToken = 0;

/* Self Contained Words. */
static char sSelf[ TOKEN_SIZE ];

typedef struct _LEX_DELIMITER
{
   char  cDelimiter;
   int   iToken;
} LEX_DELIMITER;                    /* support structure for KEYS and WORDS. */

typedef struct _LEX_WORD
{
   char  sWord[ TOKEN_SIZE ];
   int   iToken;
} LEX_WORD;                    /* support structure for KEYS and WORDS. */

typedef struct _LEX_PAIR
{
   char  sStart[MAX_STREAM_STARTER];
   char  sTerm[MAX_STREAM_TERMINATOR];
   char  sExclude[MAX_STREAM_EXCLUSIONS];
   BOOL  bTestLeft;
   int   iToken;
} LEX_PAIR;                    /* support structure for Streams (Pairs). */

#ifdef __cplusplus
   typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

/* Above are NOT overidable !!! Need to precede the Language Definitions. */

/* --------------------------------------------------------------------------------- */

/* Overidables. */
#define LEX_CUSTOM_ACTION -65
#define DONT_REDUCE 1024
#define YY_BUF_SIZE 16384

#define YY_INPUT( a, b, c )

/* Optional User Macros. */
#define LEX_USER_SETUP()
#define INIT_ACTION()
#define INTERCEPT_ACTION(x)
#define CUSTOM_ACTION(x)
#define NEW_LINE_ACTION()
#define ELEMENT_TOKEN(x,y) -1
#define DEBUG_INFO(x)
#define LEX_CASE(x)
#define STREAM_OPEN(x)
#define STREAM_APPEND(x) sPair[ iPairLen++ ] = x
#define KEYWORD_ACTION()
#define WORD_ACTION()

#include SLX_RULES

/* Declarations. */

FILE *yyin;      /* currently yacc parsed file */

extern void yyerror( char * ); /* parsing error management function */

#ifdef __cplusplus
   extern "C" int yywrap( void );
#else
   extern int yywrap( void );     /* manages the EOF of current processed file */
#endif

/* Use prototypes in function declarations. */
#define YY_USE_PROTOS

#ifdef YY_USE_PROTOS
   #define YY_PROTO(proto) proto
#else
   #define YY_PROTO(proto) ()
#endif

/* ---------------------------------------------------------------------------------------------- */

#define LEX_RULE_SIZE      ( sizeof( (int) iRet ) * ( MAX_MATCH + 2 ) )
#define LEX_WORD_SIZE      ( sizeof( LEX_WORD ) )
#define LEX_PAIR_SIZE      ( sizeof( LEX_PAIR ) )
#define LEX_DELIMITER_SIZE ( sizeof( LEX_DELIMITER ) )

/* Using statics when we could use locals to eliminate Allocations and Deallocations each time yylex is called and returns. */

/* Look ahead Tokens. */
static int  iHold = 0;
static int  aiHold[4];

/* Pre-Checked Tokens. */
static int  iReturn = 0;
static int  aiReturn[4];

/* yylex */
static char * tmpPtr;
static char sToken[TOKEN_SIZE];
static unsigned int iLen = 0;
static char chr, cPrev = 0;
static unsigned int iLastToken = 0;
static char szLexBuffer[ YY_BUF_SIZE ];
static char * s_szBuffer;
static unsigned int iSize = 0;
static int  iRet;
static BOOL bTmp, bIgnoreWords = FALSE, bRecursive = FALSE;

/* Lex emulation */
char * yytext;
int yyleng;

/* NewLine Support. */
static BOOL bNewLine = TRUE, bStart = TRUE;

#ifdef USE_KEYWORDS
   static unsigned int iKeys  = (int) ( sizeof( aKeys   ) / LEX_WORD_SIZE );
#endif

static unsigned int iWords      = (int) ( sizeof( aWords      ) / LEX_WORD_SIZE      );
static unsigned int iSelfs      = (int) ( sizeof( aSelfs      ) / LEX_WORD_SIZE      );
static unsigned int iPairs      = (int) ( sizeof( aPairs      ) / LEX_PAIR_SIZE      );
static unsigned int iDelimiters = (int) ( sizeof( aDelimiters ) / LEX_DELIMITER_SIZE );
static unsigned int iRules      = (int) ( sizeof( aiRules     ) / LEX_RULE_SIZE      );

typedef struct _TREE_NODE
{
   int iMin;
   int iMax;
} TREE_NODE;                    /* support structure for Streams (Pairs). */

/* Indexing System. */
static TREE_NODE aPairNodes[256], aSelfNodes[256], aKeyNodes[256], aWordNodes[256], aRuleNodes[1024];
static char acOmmit[256], acNewLine[256];
static int acReturn[256];

static int Reduce( int iToken );
int SimpLex_GetNextToken( void );
int SimpLex_CheckToken( void );
void SimpLex_CheckWords( void );

/* Indexing System. */
static void GenTrees( void );
static int rulecmp( const void * pLeft, const void * pRight );

/* --------------------------------------------------------------------------------- */

/* MACROS. */

/* Readability Macros. */
#define LEX_RULE_SIZE ( sizeof( (int) iRet ) * ( MAX_MATCH + 2 ) )
#define LEX_WORD_SIZE ( sizeof( LEX_WORD ) )
#define LEX_PAIR_SIZE ( sizeof( LEX_PAIR ) )
#define IF_TOKEN_READY()  if( iReturn )
#define IF_TOKEN_ON_HOLD()  if( iHold )
#define RESET_LEX() { iLen = 0; iHold = 0; iReturn = 0; bNewLine = TRUE; bStart = TRUE; }
#define FORCE_REDUCE() Reduce( 0 )

#define HOLD_TOKEN(x) PUSH_TOKEN(x)

#define IF_BEGIN_PAIR(chr) \
         if( aPairNodes[(int)chr].iMin == -1 ) \
         { \
            bTmp = FALSE; \
         } \
         else \
         { \
            register unsigned int i = aPairNodes[(int)chr].iMin, iMax = aPairNodes[(int)chr].iMax + 1, iStartLen; \
            register unsigned char chrStart; \
            unsigned int iLastPair = 0, iLastLen = 0; \
            \
            DEBUG_INFO( printf( "Checking %i Streams for %c At: >%s<\n", iPairs, chr, szBuffer - 1 ) ); \
            \
            while( i < iMax ) \
            { \
               iStartLen = 1; \
               \
               chrStart = LEX_CASE( *szBuffer ); \
               \
               while( aPairs[i].sStart[iStartLen] ) \
               { \
                  if( chrStart != aPairs[i].sStart[iStartLen] ) \
                  { \
                     break; \
                  } \
                  \
                  iStartLen++; \
                  \
                  chrStart = LEX_CASE( *( szBuffer + iStartLen - 1 ) ); \
               } \
               \
               /* Match */ \
               if( aPairs[i].sStart[iStartLen] == '\0' ) \
               { \
                  if( iStartLen > iLastLen ) \
                  { \
                     iLastPair = i + 1; \
                     iLastLen  = iStartLen; \
                  } \
               } \
               i++; \
            } \
            \
            bTmp = FALSE; \
            \
            if( iLastPair ) \
            { \
               iLastPair--; \
               STREAM_OPEN( aPairs[iLastPair].sStart )\
               { \
                  bTmp = TRUE; \
                  \
                  /* Last charcter read. */\
                  if( iStartLen > 1 ) chr = chrStart; \
                  \
                  /* Moving to next postion after the Stream Start position. */ \
                  szBuffer += ( iLastLen - 1 ); \
                  \
                  sStart     = (char *) aPairs[iLastPair].sStart; \
                  sTerm      = (char *) aPairs[iLastPair].sTerm; \
                  sExclude   = (char *) aPairs[iLastPair].sExclude; \
                  bTestLeft  =          aPairs[iLastPair].bTestLeft; \
                  iPairToken =          aPairs[iLastPair].iToken; \
                  \
                  DEBUG_INFO( printf( "Looking for Stream Terminator: >%s< Exclusions >%s<\n", sTerm, sExclude ) ); \
               } \
            } \
         } \
         /* Begin New Pair. */ \
         if( bTmp )

#define CHECK_SELF_CONTAINED(chr) \
         if( aSelfNodes[(int)chr].iMin != -1 ) \
         { \
            register unsigned int i = aSelfNodes[(int)chr].iMin, iMax = aSelfNodes[(int)chr].iMax + 1, iSelfLen; \
            register unsigned char chrSelf; \
            \
            DEBUG_INFO( printf( "Checking %i Selfs for %c At: >%s<\n", iSelfs, chr, szBuffer - 1 ) ); \
            \
            while( i < iMax ) \
            { \
               sSelf[0] = chr; \
               iSelfLen = 1; \
               chrSelf = LEX_CASE( *szBuffer ); \
               \
               while( aSelfs[i].sWord[iSelfLen] ) \
               { \
                  if( aSelfs[i].sWord[iSelfLen] == chrSelf ) \
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
                  chrSelf = LEX_CASE( *( szBuffer + iSelfLen - 1 ) ); \
               } \
               \
               /* Match */ \
               if( aSelfs[i].sWord[iSelfLen] == '\0' ) \
               { \
                  /* Moving to next postion after the Self Contained Word. */ \
                  szBuffer += ( iSelfLen - 1 ); \
                  s_szBuffer = szBuffer; \
                  \
                  sSelf[ iSelfLen ] = '\0'; \
                  iRet = aSelfs[i].iToken; \
                  \
                  if( iLen ) \
                  { \
                     DEBUG_INFO( printf( "Holding Self >%s<\n", sSelf ) ); \
                     \
                     HOLD_TOKEN( iRet ); \
                     \
                     /* Terminate current token and check it. */ \
                     sToken[ iLen ] = '\0'; \
                     \
                     /* Last charcter read. */\
                     chr = chrSelf;\
                     \
                     return SimpLex_CheckToken(); \
                  } \
                  else \
                  { \
                     DEBUG_INFO( printf( "Reducing Self >%s<\n", sSelf ) ); \
                     bIgnoreWords = FALSE;\
                     \
                     if( bNewLine )\
                     {\
                        bNewLine = FALSE;\
                        NEW_LINE_ACTION();\
                     }\
                     \
                     /* Last charcter read. */\
                     if( iSelfLen > 1 ) chr = chrSelf;\
                     \
                     return iRet; \
                  } \
               } \
               \
               i++; \
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

#define IF_APPEND_DELIMITER(chr) \
            /* Delimiter to Append? */ \
            tmpPtr = (char*) szAppend; \
            while ( *tmpPtr && chr != *tmpPtr ) tmpPtr++; \
            \
            /* Delimiter to Append found. */ \
            if( *tmpPtr )

#ifndef IF_BELONG_LEFT
   #define IF_BELONG_LEFT(chr) \
            /* Give precedence to associate rules */ \
            DEBUG_INFO( printf(  "Checking Left for: '%c' cPrev: '%c'\n", chr, cPrev ) ); \
            \
            if( 0 )
#endif

#define RETURN_READY_TOKEN() \
            \
            iReturn--; \
            iRet = aiReturn[iReturn]; \
            \
            DEBUG_INFO( printf(  "Returning Ready: %i\n", iRet ) ); \
            \
            INTERCEPT_ACTION(iRet); \
            return iRet; \

#define RELEASE_TOKEN() \
            \
            /* Last in First Out. */ \
            iHold--; \
            iRet = aiHold[iHold]; \
            \
            DEBUG_INFO( printf(  "Released %i Now Holding %i Tokens: %i %i %i %i\n", iRet, iHold, aiHold[0], aiHold[1], aiHold[2], aiHold[3] ) ); \
            bIgnoreWords = FALSE;\
            \
            if( iRet < 256 ) \
            { \
               if( acNewLine[iRet] ) bNewLine = TRUE; \
            } \
            \
            DEBUG_INFO( printf(  "Reducing Held: %i Pos: %i\n", iRet, iHold ) ); \
            LEX_RETURN( Reduce( iRet ) );

#define LEX_RETURN(x) \
        \
        iRet = x;\
        \
        if( iRet < LEX_CUSTOM_ACTION ) \
        { \
           iRet = CUSTOM_ACTION(iRet); \
        } \
        \
        if( iRet ) \
        { \
           INTERCEPT_ACTION(iRet); \
           DEBUG_INFO( printf(  "Returning: %i\n", iRet ) ); \
           return iRet; \
        } \
        else \
        { \
           goto Start; \
        }

#define PUSH_TOKEN( iPushToken )\
{\
   aiHold[ iHold++ ] = iPushToken;\
   DEBUG_INFO( printf("Now Holding %i Tokens: %i %i %i %i\n", iHold, aiHold[0], aiHold[1], aiHold[2], aiHold[3] ) ); \
}

#ifndef YY_DECL
   #define YY_DECL int yylex YY_PROTO(( void ))
#endif

YY_DECL
{

 LEX_USER_SETUP();

 Start :
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
          GenTrees();
          INIT_ACTION();
       }

       YY_INPUT( (char*) szLexBuffer, iSize, YY_BUF_SIZE );

       if( iSize )
       {
          s_szBuffer = (char*) szLexBuffer;
          DEBUG_INFO( printf(  "New Buffer: >%s<\n", szLexBuffer ) );
       }
       else
       {
          RESET_LEX();
          DEBUG_INFO( printf(  "Returning: <EOF>\n" ) );
          return -1; \
       }
    }

    LEX_RETURN( Reduce( SimpLex_GetNextToken() ) )
}

int SimpLex_GetNextToken( void )
{
    register char * szBuffer = s_szBuffer;

    iLen = 0;

    while ( 1 )
    {
        if ( iSize && *szBuffer )
        {
            if( iPairToken )
            {
               goto ProcessStream;
            }

            cPrev = chr;

            /* Get next character. */
            iSize--;
            chr = (*szBuffer++);

            /* Not using LEX_CASE() yet (white space)!!! */

            if( acOmmit[(int)chr] )
            {
               while( acOmmit[(int)(*szBuffer)] )
               {
                  iSize--; szBuffer++;
               }

               if ( iLen )
               {
                  /* Terminate current token and check it. */
                  sToken[ iLen ] = '\0';

                  s_szBuffer = szBuffer;

                  DEBUG_INFO( printf(  "Token: \"%s\" Ommited: \'%c\'\n", sToken, chr ) );
                  return SimpLex_CheckToken();
               }
               else
               {
                  continue;
               }
            }

            chr = LEX_CASE(chr);

            CHECK_SELF_CONTAINED(chr);

            /* New Pair ? */
            IF_BEGIN_PAIR( chr )
            {
               if( iLen )
               {
                  DEBUG_INFO( printf( "Holding Stream Mode: '%c' Buffer = >%s<\n", chr, szBuffer ) );

                  /* Terminate and Check Token to the left. */
                  sToken[ iLen ] = '\0';

                  s_szBuffer = szBuffer;

                  DEBUG_INFO( printf(  "Token: \"%s\" before New Pair at: \'%c\'\n", sToken, chr ) );
                  return SimpLex_CheckToken();
               }

               ProcessStream :

               bIgnoreWords = FALSE;

               if( bTestLeft )
               {
                  IF_BELONG_LEFT( chr )
                  {
                     /* Resetting. */
                     iPairToken = 0;

                     s_szBuffer = szBuffer;

                     DEBUG_INFO( printf(  "Reducing Left '%c'\n", chr ) );
                     return (int) chr ;
                  }
               }

               {  register int iPairLen = 0;
                  register char chrPair;

                  /* Look for the terminator. */
                  while ( *szBuffer )
                  {
                     /* Next Character. */
                     chrPair = *szBuffer++ ;

                     /* Terminator ? */
                     if( chrPair == sTerm[0] )
                     {
                        register int iTermLen = 1;

                        if( sTerm[1] )
                        {
                           register char chrTerm = *szBuffer; /* Not using LEX_CASE() here !!! */

                           while( sTerm[iTermLen] )
                           {
                              if( chrTerm != sTerm[iTermLen] )
                              {
                                 /* Last charcter read. */
                                 chr = chrTerm;
                                 break;
                              }

                              iTermLen++;

                              chrTerm = *( szBuffer + iTermLen - 1 ); /* Not using LEX_CASE() here !!! */
                           }
                        }

                        /* Match */ \
                        if( sTerm[iTermLen] == '\0' ) \
                        { \
                           /* Moving to next postion after the Stream Terminator. */ \
                           szBuffer += ( iTermLen - 1 ); \

                           sPair[ iPairLen ] = '\0';

                           if( bNewLine )
                           {
                              bNewLine = FALSE;
                              NEW_LINE_ACTION();
                           }

                           iRet = iPairToken;

                           /* Resetting. */
                           iPairToken = 0;

                           s_szBuffer = szBuffer;

                           DEBUG_INFO( printf(  "Returning Pair = >%s<\n", sPair ) );
                           return iRet;
                        }
                     }

                     /* Check if exception. */
                     IF_ABORT_PAIR( chrPair )
                     {
                        sPair[ iPairLen ] = '\0';

                        /* Resetting. */
                        iPairToken = 0;

                        /* Last charcter read. */
                        chr = chrPair;

                        STREAM_EXCEPTION( sPair, chrPair );

                        s_szBuffer = szBuffer;

                        return iPairToken;
                     }
                     else
                     {
                        STREAM_APPEND( chrPair );
                     }
                  }
               }

               /* Resetting. */
               iPairToken = 0;

               /* EOF */
               STREAM_EXCEPTION( sPair, NULL );

               s_szBuffer = szBuffer;

               return iPairToken;
            }
            /* End Pairs. */

            /* NewLine ? */
            if( acNewLine[(int)chr] )
            {
               while( acNewLine[(int)(*szBuffer)] )
               {
                  iSize--; szBuffer++;
               }
               s_szBuffer = szBuffer;

               if( iLen )
               {
                   /* Will return NewLine on next call. */
                   HOLD_TOKEN( chr );

                   /* Terminate current token and check it. */
                   sToken[ iLen ] = '\0';

                   DEBUG_INFO( printf(  "Token: \"%s\" at <NewLine> Holding: \'%c\'\n", sToken, chr ) );
                   return SimpLex_CheckToken();
               }
               else
               {
                   DEBUG_INFO( printf(  "Reducing NewLine '%c'\n", chr ) );
                   bIgnoreWords = FALSE;
                   bNewLine = TRUE;
                   return (int) chr;
               }
            }

            #ifdef USE_BELONGS
               IF_APPEND_DELIMITER( chr )
               {
                  /* Append and Terminate current token and check it. */
                  sToken[ iLen++ ] = chr;
                  sToken[ iLen ] = '\0';

                  s_szBuffer = szBuffer;

                  DEBUG_INFO( printf(  "Token: \"%s\" Appended: \'%c\'\n", sToken, chr ) );
                  return SimpLex_CheckToken();
               }
            #endif

            if( acReturn[(int)chr] )
            {
                s_szBuffer = szBuffer;

                if( iLen )
                {
                    /* Will be returned on next cycle. */

                    HOLD_TOKEN( acReturn[(int)chr] );

                    /* Terminate current token and check it. */
                    sToken[ iLen ] = '\0';

                    DEBUG_INFO( printf(  "Token: \"%s\" Holding: \'%c\' As: %i \n", sToken, chr, iRet ) );
                    return SimpLex_CheckToken();
                }
                else
                {
                    bIgnoreWords = FALSE;

                    if( bNewLine )
                    {
                       bNewLine = FALSE;
                       NEW_LINE_ACTION();
                    }

                    DEBUG_INFO( printf(  "Reducing Delimiter: '%c' As: %i\n", chr, iRet ) );
                    return acReturn[(int)chr];
                }
            }

            /* Acumulate and scan next Charcter. */
            sToken[ iLen++ ] = chr;

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
                   HOLD_TOKEN( -1 );

                   /* Terminate current token and check it. */
                   sToken[ iLen ] = '\0';

                   s_szBuffer = szBuffer;

                   DEBUG_INFO( printf(  "Token: \"%s\" at: \'<EOF>\'\n", sToken ) );
                   return SimpLex_CheckToken();
                }
                else
                {
                   s_szBuffer = szBuffer;
                   DEBUG_INFO( printf(  "Returning: <EOF>\n", iRet ) ); \
                   return -1; \
                }
            }
        }
    }
}

int SimpLex_CheckToken( void )
{
    if( bRecursive )
    {
       return 0;
    }
    else
    {
       bRecursive = TRUE;
    }

    if( bNewLine )
    {
       bIgnoreWords = FALSE;

       NEW_LINE_ACTION();

       #ifdef USE_KEYWORDS
          SimpLex_CheckWords();
          if( iRet )
          {
              bRecursive = FALSE;
              /* bIgnoreWords and bNewLine were handled by SimpLex_CheckWords(). */
              return iRet;
          }
       #endif
    }

    if( bIgnoreWords )
    {
       DEBUG_INFO( printf(  "Skiped Words for Word: %s\n", (char*) sToken ) );
       bIgnoreWords = FALSE;
    }
    else
    {
       SimpLex_CheckWords();
       if( iRet )
       {
          bRecursive = FALSE;
          return iRet;
       }
    }

    DEBUG_INFO( printf(  "Reducing Element: \"%s\"\n", (char*) sToken ) );

    iRet = ELEMENT_TOKEN( (char*)sToken, iLen );

    bRecursive = FALSE;
    return iRet;
}

int Reduce( int iToken )
{
  BeginReduce :

   if( iToken < LEX_CUSTOM_ACTION )
   {
      iToken = CUSTOM_ACTION( iToken ); \
   }

   if( iToken > DONT_REDUCE )
   {
      iLastToken = ( iToken - DONT_REDUCE );
      DEBUG_INFO( printf(  "Returning Dont Reduce %i\n", iLastToken ) );
      return iLastToken;
   }
   else if( iToken == 0 )
   {
      DEBUG_INFO( printf(  "Returning 0\n" ) );
      return 0;
   }

   iLastToken = iToken;

   if( aRuleNodes[ iToken ].iMin == -1 )
   {
      DEBUG_INFO( printf( "Passing through: %i\n", iToken ) );
      return iToken;
   }
   else
   {
      register unsigned int i = (unsigned int)(aRuleNodes[ iToken ].iMin), iMax = (unsigned int)(aRuleNodes[ iToken ].iMax);
      register unsigned int iTentative = 0, iMatched = 1;

      DEBUG_INFO( printf(  "Scaning Prospects %i-%i at Pos: 0 for Token: %i\n", i, iMax -1, iToken ) );

      {
        FoundProspect :

         DEBUG_INFO( printf( "Prospect of %i Tokens - Testing Token: %i\n", iMatched, iToken ) );

         if( iMatched == MAX_MATCH || aiRules[i][iMatched] == 0 )
         {
            DEBUG_INFO( printf( "Saving Tentative %i - Found match of %i Tokens at Token: %i\n", i, iMatched, iToken ) );
            iTentative = i;
         }
         else
         {
            DEBUG_INFO( printf( "Partial Match - Get next Token after Token: %i\n", iToken ) );

            /* Partial Match, try more...*/
            if( iHold )
            {
                iHold--;
                iToken = aiHold[ iHold ];
                bIgnoreWords = FALSE;
                if( iToken < 256 )
                {
                   if( acNewLine[iToken] ) bNewLine = TRUE;
                }
            }
            else
            {
               iToken = SimpLex_GetNextToken();
            }

            if( iToken < LEX_CUSTOM_ACTION )
            {
               iToken = CUSTOM_ACTION( iToken ); \
            }

            if( iToken > DONT_REDUCE )
            {
               DEBUG_INFO( printf( "Reduce Forced for Token: %i\n", iToken - DONT_REDUCE ) );
               aiHold[iHold++] = iToken;
               goto AfterScanRules;
            }
            else
            {
               iLastToken = iToken;
            }

            if( aiRules[i][iMatched] == iToken )
            {
               /* Continue... Still a prospect. */
               DEBUG_INFO( printf( "Accepted Token: %i - Continue with this Rule...\n", iToken ) );
               iMatched++;
               goto FoundProspect;
            }
            else if( aiRules[i][iMatched] > iToken )
            {
               DEBUG_INFO( printf( "Rejected Token: %i - Giving up...\n", iToken ) );
               aiHold[iHold++] = iToken;
               goto AfterScanRules;
            }
            else
            {
               DEBUG_INFO( printf( "Rejected Token: %i - Continue with next Rule...\n", iToken ) );
               aiHold[iHold++] = iToken;
            }
         }

         if( i < iMax )
         {
            register unsigned int j = 1;

            DEBUG_INFO( printf( "Checking if next rule is extension of last Rule\n" ) );

            while( j < iMatched )
            {
               if( aiRules[i][j] != aiRules[i+1][j] )
               {
                  break;
               }
               j++;
            }
            if( j < iMatched )
            {
               DEBUG_INFO( printf( "Rejected Rule - Not an extension or previous - Giving up...\n" ) );
            }
            else
            {
               DEBUG_INFO( printf( "Accepted Next Rule...\n" ) );
               i++;
               goto FoundProspect;
            }
         }
         else
         {
            DEBUG_INFO( printf( "No More prospects...\n" ) );
         }
      }

     AfterScanRules :

      if( iTentative )
      {
         DEBUG_INFO( printf( "Processing Tentative: %i\n", iTentative ) );

         if( aiRules[iTentative][MAX_MATCH] )
         {
            DEBUG_INFO( printf( "Reducing Rule: %i Found %i Tokens\n", iTentative, iMatched ) );

            if( aiRules[iTentative][MAX_MATCH + 1] )
            {
               DEBUG_INFO( printf( "Pushing Reduction: %i\n", aiRules[iTentative][MAX_MATCH + 1] ) );
               aiHold[iHold++] = aiRules[iTentative][MAX_MATCH + 1];
            }

            DEBUG_INFO( printf( "Recycling Reduction: %i\n", aiRules[iTentative][MAX_MATCH] ) );
            iToken = aiRules[iTentative][MAX_MATCH];
            goto BeginReduce;
         }
         else
         {
            DEBUG_INFO( printf( "Passing Through %i Tokens\n", iMatched ) );
            while( iMatched > 1 )
            {
               iMatched--;
               DEBUG_INFO( printf( "Stacking Return: %i\n", aiRules[iTentative][iMatched] ) );
               aiReturn[iReturn++] = aiRules[iTentative][iMatched];
            }
            DEBUG_INFO( printf( "Returning: %i\n", aiRules[iTentative][0] ) );
            return aiRules[iTentative][0];
         }
      }
      else
      {
         while( iMatched > 1 )
         {
            iMatched--;
            DEBUG_INFO( printf( "Pushing: %i\n", aiRules[i][iMatched] ) );
            aiHold[iHold++] = aiRules[i][iMatched];
         }

         DEBUG_INFO( printf( "Returning Shifted Left: %i\n", aiRules[i][0] ) );
         return aiRules[i][0];
      }
   }
}

void SimpLex_CheckWords( void )
{
   int iTentative = -1, iCompare;
   unsigned int i, iMax, iLenMatched, iBaseSize, iKeyLen;
   char *pNextSpacer, *sKeys2Match = NULL, *szBaseBuffer = s_szBuffer, cSpacer = chr;
   LEX_WORD *aCheck;

  #ifdef DEBUG_LEX
   char sKeyDesc[] = "Key", sWordDesc[] = "Word", *sDesc;
  #endif
  #ifdef LEX_ABBREVIATE
   unsigned int iLen2Match;
  #endif

  #ifdef USE_KEYWORDS
   if( bNewLine )
   {
      i      = aKeyNodes[ (int)(sToken[0]) ].iMin;
      iMax   = aKeyNodes[ (int)(sToken[0]) ].iMax + 1;
      aCheck = (LEX_WORD*) ( &(aKeys[0]) );
     #ifdef DEBUG_LEX
      sDesc  = (char*) sKeyDesc;
     #endif
   }
   else
  #endif
   {
      i      = aWordNodes[ (int)(sToken[0]) ].iMin;
      iMax   = aWordNodes[ (int)(sToken[0]) ].iMax + 1;
      aCheck = (LEX_WORD*) ( &( aWords[0] ) );
     #ifdef DEBUG_LEX
      sDesc  = (char*) sWordDesc;
     #endif
   }

   bNewLine = FALSE;

   DEBUG_INFO( printf( "Pre-Scaning %ss for Token: %s at Positions: %i-%i\n", sDesc, (char*) sToken, i, iMax -1 ) );

   while( i < iMax )
   {
      if( sToken[1] < aCheck[i].sWord[1] )
      {
         DEBUG_INFO( printf( "Gave-Up! Token [%s] < Pattern [%s]\n", sToken, aCheck[i].sWord ) );
         iRet = 0;
         return;
      }
      else if( sToken[1] > aCheck[i].sWord[1] )
      {
         DEBUG_INFO( printf( "Skip... %s [%s] < [%s]\n", sDesc, aCheck[i].sWord, sToken ) );
         i++;
         DEBUG_INFO( printf( "Continue with larger: [%s]\n", aCheck[i].sWord ) );
         continue;
      }
      else
      {
         break;
      }
   }

   while( i < iMax )
   {
      if( sKeys2Match )
      {
         pNextSpacer = strstr( sKeys2Match, "{WS}" );
      }
      else
      {
         sKeys2Match = aCheck[ i ].sWord;
         pNextSpacer = strstr( sKeys2Match, "{WS}" );
      }

      if( sToken[0] < sKeys2Match[0] )
      {
         DEBUG_INFO( printf( "Gave-Up! Token [%s] < Pattern [%s]\n", sToken, sKeys2Match ) );
         break;
      }
      else if( sToken[0] > sKeys2Match[0] )
      {
         DEBUG_INFO( printf( "Skip... %s [%s] < [%s]\n", sDesc, sKeys2Match, sToken ) );
         i++;
         if( ( iLenMatched = ( sKeys2Match - aCheck[i - 1].sWord ) ) == 0 )
         {
            sKeys2Match = NULL;
            DEBUG_INFO( printf( "Continue with larger: [%s]\n", aCheck[i].sWord ) );
            continue;
         }

         /* Is there a next potential Pattern. */
         if( i < iMax && strncmp( aCheck[i - 1].sWord, aCheck[i].sWord, iLenMatched ) == 0  )
         {
            /* Same relative position, in the next Pattern. */
            sKeys2Match = aCheck[i].sWord + iLenMatched;
            DEBUG_INFO( printf( "Continue with larger: [%s] at: [%s]\n", aCheck[i].sWord, sKeys2Match ) );
            continue;
         }
         else
         {
            DEBUG_INFO( printf( "Gave-Up! %i !< %i or Pattern [%s] not compatible with last match\n", i, iMax, aCheck[i].sWord ) );
            break;
         }
      }

      if( pNextSpacer )
      {
         /* Token not followed by white space - can't match this [or any latter] pattern! */
         if( ! acOmmit[(int)cSpacer] )
         {
            DEBUG_INFO( printf( "Skip... Pattern [%s] requires {WS}, cSpacer: %c\n", sKeys2Match, cSpacer ) );

            i++;
            if( ( iLenMatched = ( sKeys2Match - aCheck[i - 1].sWord ) ) == 0 )
            {
               sKeys2Match = NULL;
               DEBUG_INFO( printf( "Continue with: [%s]\n", aCheck[i].sWord ) );
               continue;
            }

            /* Is there a next potential Pattern. */
            if( i < iMax && strncmp( aCheck[i - 1].sWord, aCheck[i].sWord, iLenMatched ) == 0  )
            {
               /* Same relative position, in the next Pattern. */
               sKeys2Match = aCheck[i].sWord + iLenMatched;
               DEBUG_INFO( printf( "Continue with: [%s] at: [%s]\n", aCheck[i].sWord, sKeys2Match ) );
               continue;
            }
            else
            {
               DEBUG_INFO( printf( "Gave-Up! %i !< %i or Pattern [%s] not compatible with last match\n", i, iMax, aCheck[i].sWord ) );
               break;
            }
         }

         iKeyLen = pNextSpacer - sKeys2Match;
      }
      else
      {
         iKeyLen = strlen( sKeys2Match );
      }

     #ifdef LEX_ABBREVIATE
      iLen2Match = iLen;
      if( iLen2Match < LEX_ABBREVIATE && iLen2Match < iKeyLen )
      {
         iLen2Match = ( LEX_ABBREVIATE < iKeyLen ) ? LEX_ABBREVIATE : iKeyLen ;
      }

      if( iLen2Match > iKeyLen && i < iMax - 1 )
      {
         DEBUG_INFO( printf( "Trying Next... length mismatch - iKeyLen: %i iLen2Match: %i comparing: [%s] with: [%s]\n", iKeyLen, iLen2Match, sToken, sKeys2Match ) );
         i++;

         if( ( iLenMatched = ( sKeys2Match - aCheck[i - 1].sWord ) ) == 0 )
         {
            sKeys2Match = NULL;
            DEBUG_INFO( printf( "Continue with: [%s]\n", aCheck[i].sWord ) );
            continue;
         }

         /* Is there a next potential Pattern. */
         if( i < iMax && strncmp( aCheck[i - 1].sWord, aCheck[i].sWord, iLenMatched ) == 0  )
         {
            /* Same relative position, in the next Pattern. */
            sKeys2Match = aCheck[i].sWord + iLenMatched;
            DEBUG_INFO( printf( "Continue with: [%s] at: [%s]\n", aCheck[i].sWord, sKeys2Match ) );
            continue;
         }
         else
         {
            DEBUG_INFO( printf( "Gave-Up! %i !< %i or Pattern [%s] not compatible with last match\n", i, iMax, aCheck[i].sWord ) );
            break;
         }
      }

      DEBUG_INFO( printf( "iKeyLen: %i iLen2Match: %i comparing: [%s] with: [%s]\n", iKeyLen, iLen2Match, sToken, sKeys2Match ) );

      iCompare = strncmp( (char*) sToken, sKeys2Match, iLen2Match );
     #else
      iCompare = strcmp( (char*) sToken, sKeys2Match );
     #endif

      if( iCompare == 0 ) /* Match found */
      {
         if( pNextSpacer == NULL ) /* Full Match! */
         {
            DEBUG_INFO( printf( "Saving Tentative %s [%s] == [%s]\n", sDesc, sToken, sKeys2Match ) );

            iTentative  = i;
            iLenMatched = strlen( aCheck[i].sWord );

            /* Saving this pointer of the input stream, we might have to get here again. */
            szBaseBuffer = s_szBuffer; iBaseSize = iSize;
            DEBUG_INFO( printf( "Saved Buffer Postion: %i at: [%s]\n", iBaseSize, szBaseBuffer ) );

            /* No White Space after last Token! */
            if( iHold || iPairToken )
            {
               DEBUG_INFO( printf( "No White space after [%s] Holding: %i\n", sToken, aiHold[0] ) );
               break;
            }

         IsExtendedMatch :
            i++;

            /* Is there a next potential Pattern, that is an extended version of the current Pattern. */
            if( i < iMax && strncmp( aCheck[i - 1].sWord, aCheck[i].sWord, iLenMatched ) == 0 )
            {
               if( strlen( aCheck[i].sWord ) > ( iLenMatched + 4 ) && ( pNextSpacer = strstr( aCheck[i].sWord + iLenMatched, "{WS}" ) ) != NULL )
               {
                  /* Same relative position, in the next Pattern. */
                  sKeys2Match = pNextSpacer + 4;
                  DEBUG_INFO( printf( "Continue with: [%s] at: [%s]\n", aCheck[i].sWord, sKeys2Match ) );
               }
               else
               {
                  DEBUG_INFO( printf( "Skip... - Not Extended: [%s]\n", aCheck[i].sWord ) );
                  goto IsExtendedMatch;
               }
            }
            else
            {
               DEBUG_INFO( printf( "Gave-Up! %i !< %i or Pattern [%s] not extension of Pattern [%s]\n", i, iMax, aCheck[i].sWord, aCheck[iTentative].sWord ) );
               break;
            }
         }
         else
         {
            sKeys2Match = pNextSpacer + 4;
            DEBUG_INFO( printf( "Partial %s Match! [%s] == [%s] - Looking for: [%s]\n", sDesc, sToken, aCheck[i].sWord, sKeys2Match ) );

            /* Saving this pointer of the input stream, we might have to get here again. */
            szBaseBuffer = s_szBuffer; iBaseSize = iSize;
            DEBUG_INFO( printf( "Saved Buffer Postion: %i at: [%s]\n", iBaseSize, szBaseBuffer ) );
         }

         /* i may have been increased above - don't want to read next token if it won't get used! */
         if( i < iMax )
         {
            bRecursive = TRUE;
            cSpacer = chr;

            DEBUG_INFO( printf( "Getting next Token...\n" ) );
            SimpLex_GetNextToken();
            continue;
         }
      }
      else if( iCompare > 0 )
      {
         DEBUG_INFO( printf( "Trying Next %s Pattern... [%s] > [%s]\n", sDesc, sToken, sKeys2Match ) );
         i++;

         if( ( iLenMatched = ( sKeys2Match - aCheck[i - 1].sWord ) ) == 0 )
         {
            sKeys2Match = NULL;
            DEBUG_INFO( printf( "Continue with: [%s]\n", aCheck[i].sWord ) );
            continue;
         }

         /* Is there a next potential Pattern. */
         if( i < iMax && strncmp( aCheck[i - 1].sWord, aCheck[i].sWord, iLenMatched ) == 0 )
         {
            /* Same relative position, in the next Pattern. */
            sKeys2Match = aCheck[i].sWord + iLenMatched;
            DEBUG_INFO( printf( "Continue with: [%s] at: [%s]\n", aCheck[i].sWord, sKeys2Match ) );
            continue;
         }
         else
         {
            DEBUG_INFO( printf( "Gave-Up! %i !< %i or Pattern [%s] not compatible with previous match.\n", i, iMax, aCheck[i].sWord ) );
            break;
         }
      }
      else
      {
         DEBUG_INFO( printf( "Gave-Up! [%s] < [%s]\n", sToken, sKeys2Match ) );
         break;
      }
   }

   if( s_szBuffer != szBaseBuffer )
   {
      s_szBuffer = szBaseBuffer; iSize = iBaseSize; iHold = 0; iReturn = 0;
      DEBUG_INFO( printf( "Partial Match - Restored position: %i at: [%s]\n", iSize, s_szBuffer ) );
   }

   if( iTentative > -1 )
   {
      DEBUG_INFO( printf(  "Reducing %s Pattern: %i [%s]\n", sDesc, iTentative, aCheck[iTentative].sWord ) );

      bIgnoreWords = TRUE;

      KEYWORD_ACTION()

      iRet = aCheck[ iTentative ].iToken;
      if( iRet < LEX_CUSTOM_ACTION )
      {
         iRet = CUSTOM_ACTION( iRet );
      }
   }
   else
   {
      iRet = 0;
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

   if( bStart )
   {
      bStart = FALSE;
      GenTrees();
      INIT_ACTION();
   }

   return s_szBuffer;
}

static void GenTrees( void )
{
   register unsigned int i;
   register unsigned int iIndex;

   i = 0;
   while( i < 256 )
   {
      acOmmit[i]            = 0;
      acNewLine[i]          = 0;
      acReturn[i]           = 0;
      aPairNodes[i].iMin    = -1;
      aPairNodes[i].iMax    = -1;
      aSelfNodes[i].iMin    = -1;
      aSelfNodes[i].iMax    = -1;
      aKeyNodes[i].iMin     = -1;
      aKeyNodes[i].iMax     = -1;
      aWordNodes[i].iMin    = -1;
      aWordNodes[i].iMax    = -1;
      aRuleNodes[i].iMin    = -1;
      aRuleNodes[i].iMax    = -1;
      i++;
   }

   while( i < 1024 )
   {
      aRuleNodes[i].iMin = -1;
      aRuleNodes[i].iMax = -1;
      i++;
   }

   i = 0;
   while ( szOmmit[i] )
   {
      acOmmit[ (int)(szOmmit[i]) ] = 1;
      i++;
   }

   i = 0;
   while ( szNewLine[i] )
   {
      acNewLine[ (int)(szNewLine[i]) ] = 1;
      i++;
   }

   i = 0;
   while ( i < iDelimiters )
   {
      acReturn[ (int)(aDelimiters[i].cDelimiter) ] = aDelimiters[i].iToken;
      i++;
   }

   i = 0;
   while ( i < iPairs )
   {
      iIndex = aPairs[i].sStart[0];
      if( aPairNodes[ iIndex ].iMin == -1 )
      {
         aPairNodes[ iIndex ].iMin = i;
      }
      aPairNodes[ iIndex ].iMax = i;
      i++;
   }

   i = 0;
   while ( i < iSelfs )
   {
      iIndex = aSelfs[i].sWord[0];
      if( aSelfNodes[ iIndex ].iMin == -1 )
      {
         aSelfNodes[ iIndex ].iMin = i;
      }
      aSelfNodes[ iIndex ].iMax = i;
      i++;
   }

   #ifdef USE_KEYWORDS
      i = 0;
      while ( i < iKeys )
      {
         iIndex = aKeys[i].sWord[0];
         if( aKeyNodes[ iIndex ].iMin == -1 )
         {
            aKeyNodes[ iIndex ].iMin = i;
         }
         aKeyNodes[ iIndex ].iMax = i;
         i++;
      }
   #endif

   i = 0;
   while ( i < iWords )
   {
      iIndex = aWords[i].sWord[0];
      if( aWordNodes[ iIndex ].iMin == -1 )
      {
         aWordNodes[ iIndex ].iMin = i;
      }
      aWordNodes[ iIndex ].iMax = i;
      i++;
   }

   /* Reduce logic excpects the Rules to be sorted.  */
   qsort( ( void * ) aiRules, iRules, LEX_RULE_SIZE, rulecmp );

   i = 0;
   while ( i < iRules )
   {
      iIndex = (unsigned int) aiRules[i][0];
      if( iIndex > 1023 )
      {
         printf( "ERROR! Primary Token: %i out of range.\n", (int) iIndex );
         exit( EXIT_FAILURE );
      }
      if( aRuleNodes[ iIndex ].iMin == -1 )
      {
         aRuleNodes[ iIndex ].iMin = i;
      }
      aRuleNodes[ iIndex ].iMax = i;
      i++;
   }
}

static int rulecmp( const void * pLeft, const void * pRight )
{
   int *iLeftRule  = (int*)( pLeft );
   int *iRightRule = (int*)( pRight );
   register unsigned int i = 0;

   while( iLeftRule[i] == iRightRule[i] )
   {
      i++;
   }

   if( iLeftRule[i] < iRightRule[i] )
   {
      return -1;
   }
   else
   {
      return 1;
   }
}

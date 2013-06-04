/*
 * Harbour Project source code:
 *    preprocessor static rules generator.
 *    It creates .c file with tables for defines/[x]translates/[x]commands
 *    found in given .ch or .prg file
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"

int hb_verRevision( void )
{
   return 0;
}

#include "ppcore.c"

/*
 * functions to create .c files with rules defined in given PP context
 */
static int hb_pp_writeTokenCount( PHB_PP_TOKEN pToken )
{
   int iToken = 0;

   while( pToken )
   {
      iToken += hb_pp_writeTokenCount( pToken->pMTokens ) + 1;
      pToken  = pToken->pNext;
   }
   return iToken;
}

static void hb_pp_writeToken( FILE * fout, PHB_PP_TOKEN pToken,
                              const char * szName, int iToken, HB_BOOL fLast )
{
   while( pToken )
   {
      int iOptional = hb_pp_writeTokenCount( pToken->pMTokens ), i;

      i = ( int ) strlen( szName );
      if( pToken->pNext )
         fprintf( fout, "   { %s +%2d", szName, iToken + iOptional + 1 );
      else
         fprintf( fout, "   { NULL%*s", i, "" );
      if( iOptional )
         fprintf( fout, ", %s +%2d", szName, iToken + 1 );
      else
         fprintf( fout, ", NULL%*s", i, "" );

      i = 16 - ( int ) strlen( pToken->value );
      fprintf( fout, ", \"%s\", %*s %2d,%2d, 0x%04x, %d }%s\n",
               pToken->value,
               i < 0 ? 0 : i, "",
               ( int ) pToken->len, ( int ) pToken->spaces,
               pToken->type | HB_PP_TOKEN_STATIC | HB_PP_TOKEN_PREDEFINED,
               pToken->index,
               fLast && ! pToken->pNext && iOptional == 0 ? "" : "," );

      if( iOptional )
         hb_pp_writeToken( fout, pToken->pMTokens, szName, iToken + 1,
                           pToken->pNext == NULL && fLast );

      iToken += iOptional + 1;
      pToken  = pToken->pNext;
   }
}

static void hb_pp_writeTokenList( FILE * fout, PHB_PP_TOKEN pTokenLst, const char * szName )
{
   int iTokens;

   iTokens = hb_pp_writeTokenCount( pTokenLst );
   if( iTokens )
   {
      fprintf( fout, "static HB_PP_TOKEN %s[ %d ] = {\n",
               szName, iTokens );
      hb_pp_writeToken( fout, pTokenLst, szName, 0, HB_TRUE );
      fprintf( fout, "};\n" );
   }
}

static int hb_pp_writeRules( FILE * fout, PHB_PP_RULE pFirst, const char * szName )
{
   char szMatch[ 16 ], szResult[ 16 ];
   HB_ULONG ulRepeatBits, ulBit;
   PHB_PP_RULE pRule;
   int iRule;
   HB_USHORT u;

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;
      if( pRule->pMatch )
      {
         hb_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
         hb_pp_writeTokenList( fout, pRule->pMatch, szMatch );
      }

      if( pRule->pResult )
      {
         hb_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
         hb_pp_writeTokenList( fout, pRule->pResult, szResult );
      }
      pRule = pRule->pPrev;
   }

   fprintf( fout, "static const HB_PP_DEFRULE s_%s[ %d ] = {\n",
            szName, iRule );

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;
      if( pRule->pMatch )
         hb_snprintf( szMatch, sizeof( szMatch ), "s_%cm%03d", szName[ 0 ], iRule );
      else
         hb_strncpy( szMatch, "NULL   ", sizeof( szMatch ) - 1 );
      if( pRule->pResult )
         hb_snprintf( szResult, sizeof( szResult ), "s_%cr%03d", szName[ 0 ], iRule );
      else
         hb_strncpy( szResult, "NULL   ", sizeof( szResult ) - 1 );

      ulRepeatBits = 0;
      for( u = 0, ulBit = 1; u < pRule->markers; ++u, ulBit <<= 1 )
      {
         if( pRule->pMarkers[ u ].canrepeat )
            ulRepeatBits |= ulBit;
      }
      fprintf( fout, "   { %s, %s, %d,%2d, 0x%04lx }%s\n",
               szMatch, szResult, HB_PP_CMP_MODE( pRule->mode ),
               pRule->markers, ulRepeatBits, pRule->pPrev ? "," : "" );
      pRule = pRule->pPrev;
   }
   fprintf( fout, "};\n\n" );
   return iRule;
}

static void hb_pp_generateInitFunc( FILE * fout, int iRules,
                                    const char * szVar, const char * szRule )
{
   fprintf( fout, "   hb_pp_initRules( &pState->p%s, &pState->i%s, ",
            szVar, szVar );
   if( iRules )
      fprintf( fout, "s_%s, %d );\n", szRule, iRules );
   else
      fprintf( fout, "NULL, 0 );\n" );
}

static void hb_pp_generateRules( FILE * fout, PHB_PP_STATE pState, const char * szPPRuleFuncName )
{
   int iDefs = 0, iTrans = 0, iCmds = 0;

   fprintf( fout, "/*\n * $" "Id" "$\n */\n\n/*\n"
            " * Harbour Project source code:\n"
            " *    Build in preprocessor rules.\n"
            " *\n"
            " * Copyright 2006-2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>\n"
            " * www - http://harbour-project.org\n"
            " *\n"
            " * This file is generated automatically by Harbour preprocessor\n"
            " * and is covered by the same license as Harbour PP\n"
            " */\n\n#define _HB_PP_INTERNAL\n#include \"hbpp.h\"\n\n" );

   if( pState->pDefinitions )
      iDefs = hb_pp_writeRules( fout, pState->pDefinitions, "def" );
   if( pState->pTranslations )
      iTrans = hb_pp_writeRules( fout, pState->pTranslations, "trs" );
   if( pState->pCommands )
      iCmds = hb_pp_writeRules( fout, pState->pCommands, "cmd" );

   fprintf( fout, "\nvoid %s( PHB_PP_STATE pState )\n{\n", szPPRuleFuncName ? szPPRuleFuncName : "hb_pp_setStdRules" );
   hb_pp_generateInitFunc( fout, iDefs,  "Definitions",  "def" );
   hb_pp_generateInitFunc( fout, iTrans, "Translations", "trs" );
   hb_pp_generateInitFunc( fout, iCmds,  "Commands",     "cmd" );
   fprintf( fout, "}\n" );
}

static void hb_pp_undefCompilerRules( PHB_PP_STATE pState )
{
   int i;
   PHB_PP_RULE * pRulePtr, pRule;
   const char * szRules[] = { "__HARBOUR__",
                              "__DATE__",
                              "__TIME__",
                              "__FILE__",
                              "__LINE__",
                              "__HB_MAIN__",
                              "__ARCH16BIT__",
                              "__ARCH32BIT__",
                              "__ARCH64BIT__",
                              "__LITTLE_ENDIAN__",
                              "__BIG_ENDIAN__",
                              "__PDP_ENDIAN__",
                              NULL };

   for( i = 0; szRules[ i ]; ++i )
      hb_pp_delDefine( pState, szRules[ i ] );

   pRulePtr = &pState->pDefinitions;
   while( *pRulePtr )
   {
      pRule = *pRulePtr;
      if( ! pRule->pMatch->pNext &&
          strncmp( pRule->pMatch->value, "__PLATFORM__", 12 ) == 0 )
      {
         *pRulePtr = pRule->pPrev;
         hb_pp_ruleFree( pRule );
         pState->iDefinitions--;
      }
      else
         pRulePtr = &pRule->pPrev;
   }
}

static int hb_pp_preprocesfile( PHB_PP_STATE pState, const char * szRuleFile, const char * szPPRuleFuncName )
{
   int iResult = 0;
   HB_SIZE nLen;

   while( hb_pp_nextLine( pState, &nLen ) != NULL && nLen )
      ;

   if( szRuleFile )
   {
      FILE * foutr;

      foutr = hb_fopen( szRuleFile, "w" );
      if( ! foutr )
      {
#if ! defined( HB_OS_WIN_CE )
         perror( szRuleFile );
#endif
         iResult = 1;
      }
      else
      {
         hb_pp_undefCompilerRules( pState );
         hb_pp_generateRules( foutr, pState, szPPRuleFuncName );
         fclose( foutr );
      }
   }

   return iResult;
}

/* NOTE: Caller should free the pointer. */
static char * hb_pp_escapeString( char * szString )
{
   char * szResult, ch;
   int iLen;

   szResult = szString;
   iLen = 0;
   do
   {
      ch = *szResult++;
      /* NOTE: ? is escaped to avoid conflicts with trigraph sequences which
       *       are part of ANSI C standard
       */
      if( ch == '"' || ch == '\\' || ch == '?' )
         ++iLen;
      ++iLen;
   }
   while( ch );

   szResult = ( char * ) hb_xgrab( iLen );
   iLen = 0;
   do
   {
      ch = *szString++;
      if( ch == '"' || ch == '\\' || ch == '?' )
         szResult[ iLen++ ] = '\\';
      szResult[ iLen++ ] = ch;
   }
   while( ch );

   return szResult;
}

static int hb_pp_generateVerInfo( char * szVerFile, int iRevID, char * szChangeLogID, char * szLastEntry )
{
   int iResult = 0;
   char * pszEnv;
   FILE * fout;

   fout = hb_fopen( szVerFile, "w" );
   if( ! fout )
   {
#if ! defined( HB_OS_WIN_CE )
      perror( szVerFile );
#endif
      iResult = 1;
   }
   else
   {
      char * pszEscaped;

      fprintf( fout, "/*\n"
               " * Harbour Project source code:\n"
               " *    Version information and build time switches.\n"
               " *\n"
               " * Copyright 2008-2013 Przemyslaw Czerpak <druzus / at / priv.onet.pl>\n"
               " * www - http://harbour-project.org\n"
               " *\n"
               " * This file is generated automatically by Harbour preprocessor\n"
               " * and is covered by the same license as Harbour PP\n"
               " */\n\n" );

      fprintf( fout, "#define HB_VER_REVID             %d\n", iRevID );

      if( szChangeLogID )
      {
         pszEscaped = hb_pp_escapeString( szChangeLogID );
         fprintf( fout, "#define HB_VER_CHLID             \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
      }

      if( szLastEntry )
      {
         pszEscaped = hb_pp_escapeString( szLastEntry );
         fprintf( fout, "#define HB_VER_LENTRY            \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
      }

      pszEnv = hb_getenv( "HB_USER_CFLAGS" );
      if( pszEnv )
      {
         pszEscaped = hb_pp_escapeString( pszEnv );
         fprintf( fout, "#define HB_VER_HB_USER_CFLAGS    \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "HB_USER_LDFLAGS" );
      if( pszEnv )
      {
         pszEscaped = hb_pp_escapeString( pszEnv );
         fprintf( fout, "#define HB_VER_HB_USER_LDFLAGS   \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "HB_USER_PRGFLAGS" );
      if( pszEnv )
      {
         pszEscaped = hb_pp_escapeString( pszEnv );
         fprintf( fout, "#define HB_VER_HB_USER_PRGFLAGS  \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "HB_PLATFORM" );
      if( pszEnv )
      {
         pszEscaped = hb_pp_escapeString( pszEnv );
         fprintf( fout, "#define HB_PLATFORM              \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
         hb_xfree( pszEnv );
      }

      pszEnv = hb_getenv( "HB_COMPILER" );
      if( pszEnv )
      {
         pszEscaped = hb_pp_escapeString( pszEnv );
         fprintf( fout, "#define HB_COMPILER              \"%s\"\n", pszEscaped );
         hb_xfree( pszEscaped );
         hb_xfree( pszEnv );
      }

      fclose( fout );
   }

   return iResult;
}

static char * hb_fsFileFind( const char * pszFileMask )
{
   PHB_FFIND ffind;

   if( ( ffind = hb_fsFindFirst( pszFileMask, HB_FA_ALL ) ) != NULL )
   {
      char pszFileName[ HB_PATH_MAX ];
      PHB_FNAME pFileName = hb_fsFNameSplit( pszFileMask );
      pFileName->szName = ffind->szName;
      pFileName->szExtension = NULL;
      hb_fsFNameMerge( pszFileName, pFileName );
      hb_fsFindClose( ffind );
      hb_xfree( pFileName );
      return hb_strdup( pszFileName );
   }
   return NULL;
}

static int hb_pp_parseChangelog( PHB_PP_STATE pState, const char * pszFileName,
                                 int iQuiet, int * piRevID,
                                 char ** pszChangeLogID, char ** pszLastEntry )
{
   char * pszFree = NULL;
   int iResult = 0;
   FILE * file_in;

   char szToCheck[ HB_PATH_MAX ];
   PHB_FNAME pFileName = hb_fsFNameSplit( pszFileName );

   if( ! pFileName->szName )
   {
      static const char * s_szNames[] = {
         "ChangeLog.txt",
         "CHANGES.txt",
#if defined( HB_OS_DOS )
         "ChangeLo.txt",
         "Change~1.txt",
         "Change~?.txt",
         "Chang~??.txt",
#endif
         NULL
      };
      int i = 0;

      if( ! pFileName->szPath )
         pFileName->szPath = "../../../../..";

      pszFileName = s_szNames[ i++ ];
      while( pszFileName )
      {
         pFileName->szName = pszFileName;
         hb_fsFNameMerge( szToCheck, pFileName );

         if( hb_fsFileExists( szToCheck ) )
         {
            pszFileName = szToCheck;
            break;
         }

         if( strchr( szToCheck, '?' ) != NULL )
         {
            pszFree = hb_fsFileFind( szToCheck );
            if( pszFree )
            {
               pszFileName = pszFree;
               break;
            }
         }

         pszFileName = s_szNames[ i++ ];
      }

      if( ! pszFileName )
         pszFileName = s_szNames[ 0 ];
   }

   hb_xfree( pFileName );

   file_in = hb_fopen( pszFileName, "r" );
   if( ! file_in )
   {
      if( iQuiet < 2 )
      {
#if ! defined( HB_OS_WIN_CE )
         perror( pszFileName );
#else
         fprintf( stderr, "Cannot open the %s file.\n", pszFileName );
#endif
      }
      iResult = 1;
   }
   else
   {
      char szLine[ 256 ];
      char szId[ 128 ];
      char szLog[ 128 ];
      char * szFrom, * szTo;
      int iLen;

      if( iQuiet == 0 )
         fprintf( stdout, "Reading ChangeLog file: %s\n", pszFileName );

      *szId = *szLog = '\0';

      do
      {
         if( ! fgets( szLine, sizeof( szLine ), file_in ) )
            break;

         if( ! *szId )
         {
            szFrom = strstr( szLine, "$" "Id" );
            if( szFrom )
            {
               szFrom += 3;
               szTo = strchr( szFrom, '$' );
               if( szTo )
               {
                  /* Is it tarball source package? */
                  if( szTo == szFrom )
                  {
                     /* we do not have revision number :-( */
                     hb_strncpy( szId, "unknown -1 (source tarball without keyword expanding)", sizeof( szId ) - 1 );
                  }
                  else if( szTo - szFrom > 3 && szTo[ -1 ] == ' ' &&
                           szFrom[ 0 ] == ':' && szFrom[ 1 ] == ' ' )
                  {
                     szTo[ -1 ] = '\0';
                     hb_strncpy( szId, szFrom + 2, sizeof( szId ) - 1 );
                  }
               }
            }
         }
         else if( ! *szLog )
         {
            if( szLine[ 4 ] == '-' && szLine[ 7 ] == '-' &&
                szLine[ 10 ] == ' ' && szLine[ 13 ] == ':' )
            {
               hb_strncpy( szLog, szLine, sizeof( szLog ) - 1 );
               iLen = ( int ) strlen( szLog );
               while( iLen-- && HB_ISSPACE( szLog[ iLen ] ) )
                  szLog[ iLen ] = '\0';
            }
         }
      }
      while( ! *szLog );

      fclose( file_in );

      if( ! *szLog )
      {
         if( iQuiet < 2 )
            fprintf( stderr, "Cannot find valid $" "Id entry in the %s file.\n", pszFileName );
         iResult = 1;
      }
      else
      {
         char szRevID[ 18 ];

         *szLine = '"';
         hb_strncpy( szLine + 1, szLog, sizeof( szLine ) - 3 );
         iLen = ( int ) strlen( szLine );
         szLine[ iLen ] = '"';
         szLine[ ++iLen ] = '\0';
         hb_pp_addDefine( pState, "HB_VER_LENTRY", szLine );
         *pszLastEntry = hb_strdup( szLog );

         hb_strncpy( szLine + 1, szId, sizeof( szLine ) - 3 );
         iLen = ( int ) strlen( szLine );
         szLine[ iLen ] = '"';
         szLine[ ++iLen ] = '\0';
         hb_pp_addDefine( pState, "HB_VER_CHLID", szLine );
         *pszChangeLogID = hb_strdup( szId );

         if( strlen( szLog ) >= 16 )
         {
            long lJulian = 0, lMilliSec = 0;
            int iUTC = 0;

            if( strlen( szLog ) >= 25 && szLog[ 17 ] == 'U' &&
                szLog[ 18 ] == 'T' && szLog[ 19 ] == 'C' &&
                ( szLog[ 20 ] == '+' || szLog[ 20 ] == '-' ) &&
                HB_ISDIGIT( szLog[ 21 ] ) && HB_ISDIGIT( szLog[ 22 ] ) &&
                HB_ISDIGIT( szLog[ 23 ] ) && HB_ISDIGIT( szLog[ 24 ] ) )
            {
               iUTC = ( ( int ) ( szLog[ 21 ] - '0' ) * 10 +
                        ( int ) ( szLog[ 22 ] - '0' ) ) * 60 +
                        ( int ) ( szLog[ 23 ] - '0' ) * 10 +
                        ( int ) ( szLog[ 24 ] - '0' );
            }
            szLog[ 16 ] = '\0';
            if( iUTC != 0 && hb_timeStampStrGetDT( szLog, &lJulian, &lMilliSec ) )
            {
               hb_timeStampUnpackDT( hb_timeStampPackDT( lJulian, lMilliSec ) -
                                     ( double ) iUTC / ( 24 * 60 ),
                                     &lJulian, &lMilliSec );
            }
            if( lJulian && lMilliSec )
            {
               hb_timeStampStrRawPut( szRevID, lJulian, lMilliSec );
               memmove( szRevID, szRevID + 2, 10 );
            }
            else
            {
               szRevID[ 0 ] = szLog[ 2 ];
               szRevID[ 1 ] = szLog[ 3 ];
               szRevID[ 2 ] = szLog[ 5 ];
               szRevID[ 3 ] = szLog[ 6 ];
               szRevID[ 4 ] = szLog[ 8 ];
               szRevID[ 5 ] = szLog[ 9 ];
               szRevID[ 6 ] = szLog[ 11 ];
               szRevID[ 7 ] = szLog[ 12 ];
               szRevID[ 8 ] = szLog[ 14 ];
               szRevID[ 9 ] = szLog[ 15 ];
            }
            szRevID[ 10 ] = '\0';

         }
         else
            szRevID[ 0 ] = '\0';

         *piRevID = ( int ) hb_strValInt( szRevID, &iLen );

         hb_pp_addDefine( pState, "HB_VER_REVID", szRevID );
#ifdef HB_LEGACY_LEVEL4
         hb_pp_addDefine( pState, "HB_VER_SVNID", szRevID );
#endif
      }
   }

   if( pszFree )
      hb_xfree( pszFree );

   return iResult;
}

/*
 * ppgen only functions
 */
static void hb_pp_usage( char * szName )
{
   printf( "\n" );
   printf( "Syntax:  %s <file[.prg]> [options]\n\n", szName );
   printf( "Options:  -d<id>[=<val>]\t#define <id>\n"
           "          -e[<func>]    \tuse <func> as entry function in generated .c\n"
           "          -i<path>      \tadd #include file search path\n"
           "          -u[<file>]    \tuse command def set in <file> (or none)\n"
           "          -c[<file>]    \tlook for ChangeLog file\n"
           "          -o<file>      \tcreates .c file with PP rules\n"
           "          -v<file>      \tcreates .h file with version information\n"
           "          -w            \twrite preprocessed (.ppo) file\n"
           "          -q[012]       \tdisable information messages\n"
           "\n"
           "Note:  if neither -o nor -v is specified then -w is default action\n\n" );
}

int main( int argc, char * argv[] )
{
   char * szFile = NULL, * szRuleFile = NULL, * szVerFile = NULL;
   char * szStdCh = NULL, * szLogFile = NULL, * szInclude;
   HB_BOOL fWrite = HB_FALSE, fChgLog = HB_FALSE;
   char * szChangeLogID = NULL, * szLastEntry = NULL;
   int iRevID = 0, iResult = 0, iQuiet = 0, i;
   char * szPPRuleFuncName = NULL;
   PHB_PP_STATE pState;

   pState = hb_pp_new();

   if( argc >= 2 )
   {
      szFile = argv[ 1 ];
      for( i = 2; szFile && i < argc; i++ )
      {
         if( ! HB_ISOPTSEP( argv[ i ][ 0 ] ) )
            szFile = NULL;
         else
         {
            switch( argv[ i ][ 1 ] )
            {
               case 'q':
               case 'Q':
                  if( ! argv[ i ][ 2 ] )
                     iQuiet = 1;
                  else if( argv[ i ][ 2 ] == '-' && ! argv[ i ][ 3 ] )
                     iQuiet = 0;
                  else if( argv[ i ][ 2 ] >= '0' && argv[ i ][ 2 ] <= '2' && ! argv[ i ][ 3 ] )
                     iQuiet = argv[ i ][ 2 ] - '0';
                  else
                     szFile = NULL;
                  break;

               case 'd':
               case 'D':
                  if( ! argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                  {
                     char * szDefText = hb_strdup( argv[ i ] + 2 ), * szAssign;

                     szAssign = strchr( szDefText, '=' );
                     if( szAssign )
                        *szAssign++ = '\0';
                     hb_pp_addDefine( pState, szDefText, szAssign );
                     hb_xfree( szDefText );
                  }
                  break;

               case 'e':
               case 'E':
                  if( argv[ i ][ 2 ] )
                     szPPRuleFuncName = argv[ i ] + 2;
                  else
                     szPPRuleFuncName = NULL;
                  break;

               case 'w':
               case 'W':
                  if( argv[ i ][ 2 ] )
                     szFile = NULL;
                  else
                     fWrite = HB_TRUE;
                  break;

               case 'c':
               case 'C':
                  fChgLog = HB_TRUE;
                  if( argv[ i ][ 2 ] )
                     szLogFile = argv[ i ] + 2;
                  break;

               case 'i':
               case 'I':
                  if( argv[ i ][ 2 ] )
                     hb_pp_addSearchPath( pState, argv[ i ] + 2, HB_FALSE );
                  else
                     szFile = NULL;
                  break;

               case 'o':
               case 'O':
                  if( argv[ i ][ 2 ] )
                     szRuleFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               case 'v':
               case 'V':
                  if( argv[ i ][ 2 ] )
                     szVerFile = argv[ i ] + 2;
                  else
                     szFile = NULL;
                  break;

               case 'u':
               case 'U':
                  if( argv[ i ][ 2 ] )
                     szStdCh = argv[ i ] + 2;
                  else
                     szStdCh = NULL;
                  break;

               default:
                  szFile = NULL;
                  break;
            }
         }
      }
   }

   if( iQuiet < 2 )
   {
      printf( "Harbour Preprocessor %d.%d.%d%s\n",
              HB_VER_MAJOR, HB_VER_MINOR, HB_VER_RELEASE, HB_VER_STATUS );
      printf( "Copyright (c) 1999-2013, http://harbour-project.org/\n" );
   }

   if( szFile )
   {
      if( ! szRuleFile && ! szVerFile )
         fWrite = HB_TRUE;

      hb_pp_init( pState, iQuiet != 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL );

      szInclude = hb_getenv( "INCLUDE" );
      if( szInclude )
      {
         if( szInclude[ 0 ] )
            hb_pp_addSearchPath( pState, szInclude, HB_FALSE );
         hb_xfree( szInclude );
      }

      if( szStdCh )
         hb_pp_readRules( pState, szStdCh );

      if( hb_pp_inFile( pState, szFile, HB_TRUE, NULL, HB_TRUE ) )
      {
         if( fWrite )
         {
            char szFileName[ HB_PATH_MAX ];
            PHB_FNAME pFileName;

            pFileName = hb_fsFNameSplit( szFile );
            pFileName->szExtension = ".ppo";
            hb_fsFNameMerge( szFileName, pFileName );
            hb_xfree( pFileName );

            hb_pp_outFile( pState, szFileName, NULL );
         }

         if( fChgLog )
            iResult = hb_pp_parseChangelog( pState, szLogFile, iQuiet,
                                            &iRevID, &szChangeLogID, &szLastEntry );

         if( iResult == 0 )
            iResult = hb_pp_preprocesfile( pState, szRuleFile, szPPRuleFuncName );

         if( iResult == 0 && szVerFile )
            iResult = hb_pp_generateVerInfo( szVerFile, iRevID,
                                             szChangeLogID, szLastEntry );
         if( iResult == 0 && hb_pp_errorCount( pState ) > 0 )
            iResult = 1;
      }
      else
         iResult = 1;
   }
   else
   {
      hb_pp_usage( argv[ 0 ] );
      iResult = 1;
   }

   if( szChangeLogID )
      hb_xfree( szChangeLogID );
   if( szLastEntry )
      hb_xfree( szLastEntry );

   hb_pp_free( pState );

   return iResult;
}

#if defined( HB_OS_WIN_CE ) && ! defined( __CEGCC__ )
#  include "hbwmain.c"
#endif

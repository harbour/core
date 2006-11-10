/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * 
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "ppcore.c"


/*
 * library functions used by PP core code
 * necessary to create stand alone binries
 */
void * hb_xgrab( ULONG ulSize ) { return malloc( ulSize ); }
void * hb_xrealloc( void * pMem, ULONG ulSize ) { return realloc( pMem, ulSize ); }
void hb_xfree( void * pMem ) { free( pMem ); }


void hb_pp_initStaticRules( PHB_PP_STATE pState )
{
   HB_SYMBOL_UNUSED( pState );
}


/*
 * functions to create .c files with rules defined in given PP context
 */
static int hb_pp_writeTokenCount( PHB_PP_TOKEN pToken )
{
   int iToken = 0;
   while( pToken )
   {
      iToken += hb_pp_writeTokenCount( pToken->pMTokens ) + 1;
      pToken = pToken->pNext;
   }
   return iToken;
}

static void hb_pp_writeToken( FILE * fout, PHB_PP_TOKEN pToken,
                              char * szName, int iToken, BOOL fLast )
{
   while( pToken )
   {
      int iOptional = hb_pp_writeTokenCount( pToken->pMTokens ), i;

      i = strlen( szName );
      if( pToken->pNext )
         fprintf( fout, "   { %s +%2d", szName, iToken + iOptional + 1 );
      else
         fprintf( fout, "   { NULL%*s", i, "" );
      if( iOptional )
         fprintf( fout, ", %s +%2d", szName, iToken + 1 );
      else
         fprintf( fout, ", NULL%*s", i, "" );

      i = 16 - strlen( pToken->value );
      fprintf( fout, ", \"%s\", %*s %2d,%2d, 0x%04x, %d }%s\n",
               pToken->value,
               i < 0 ? 0 : i, "",
               pToken->len, pToken->spaces,
               pToken->type | HB_PP_TOKEN_STATIC | HB_PP_TOKEN_PREDEFINED,
               pToken->index,
               fLast && !pToken->pNext && iOptional == 0 ? "" : "," );

      if( iOptional )
         hb_pp_writeToken( fout, pToken->pMTokens, szName, iToken + 1,
                           pToken->pNext == NULL && fLast );

      iToken += iOptional + 1;
      pToken = pToken->pNext;
   }
}

static void hb_pp_writeTokenList( FILE * fout, PHB_PP_TOKEN pTokenLst, char * szName )
{
   int iTokens;

   iTokens = hb_pp_writeTokenCount( pTokenLst );
   if( iTokens )
   {
      fprintf( fout, "static HB_PP_TOKEN %s[ %d ] = {\n",
               szName, iTokens );
      hb_pp_writeToken( fout, pTokenLst, szName, 0, TRUE );
      fprintf( fout, "};\n" );
   }
}

static int hb_pp_writeRules( FILE * fout, PHB_PP_RULE pFirst, char * szName )
{
   char szMatch[ 16 ], szResult[ 16 ];
   ULONG ulRepeatBits, ulBit;
   PHB_PP_RULE pRule;
   int iRule;
   USHORT u;

   iRule = 0;
   pRule = pFirst;
   while( pRule )
   {
      ++iRule;
      if( pRule->pMatch )
      {
         sprintf( szMatch, "s_%cm%03d", szName[0], iRule );
         hb_pp_writeTokenList( fout, pRule->pMatch, szMatch );
      }

      if( pRule->pResult )
      {
         sprintf( szResult, "s_%cr%03d", szName[0], iRule );
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
         sprintf( szMatch, "s_%cm%03d", szName[0], iRule );
      else
         strcpy( szMatch, "NULL   " );
      if( pRule->pResult )
         sprintf( szResult, "s_%cr%03d", szName[0], iRule );
      else
         strcpy( szResult, "NULL   " );

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
                                    char * szVar, char * szRule )
{
   fprintf( fout, "   hb_pp_initRules( &pState->p%s, &pState->i%s, ",
            szVar, szVar );
   if( iRules )
      fprintf( fout, "s_%s, %d );\n", szRule, iRules );
   else
      fprintf( fout, "NULL, 0 );\n" );
}

static void hb_pp_generateRules( FILE * fout, PHB_PP_STATE pState )
{
   int iDefs = 0, iTrans = 0, iCmds = 0;

   fprintf( fout, "/*\n * $Id$\n */\n\n/*\n"
         " * Harbour Project source code:\n"
         " *    Build in preprocessor rules.\n"
         " *\n"
         " * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>\n"
         " * www - http://www.harbour-project.org\n"
         " *\n"
         " * This file is generate automatically by Harbour preprocessor\n"
         " * and is covered by the same license as Harbour PP\n"
         " */\n\n#define _HB_PP_INTERNAL\n#include \"hbpp.h\"\n\n" );

   if( pState->pDefinitions )
      iDefs = hb_pp_writeRules( fout, pState->pDefinitions, "def" );
   if( pState->pTranslations )
      iTrans = hb_pp_writeRules( fout, pState->pTranslations, "trs" );
   if( pState->pCommands )
      iCmds = hb_pp_writeRules( fout, pState->pCommands, "cmd" );

   fprintf( fout, "\nvoid hb_pp_initStaticRules( PHB_PP_STATE pState )\n{\n" );
   hb_pp_generateInitFunc( fout, iDefs,  "Definitions",  "def" );
   hb_pp_generateInitFunc( fout, iTrans, "Translations", "trs" );
   hb_pp_generateInitFunc( fout, iCmds,  "Commands",     "cmd" );
   fprintf( fout, "}\n" );
}

static void hb_pp_undefCompilerRules( PHB_PP_STATE pState )
{
   int i;
   PHB_PP_RULE * pRulePtr, pRule;
   char * szRules[] = { "__HARBOUR__",
                        "__DATE__",
                        "__TIME__",
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
   while( * pRulePtr )
   {
      pRule = *pRulePtr;
      if( !pRule->pMatch->pNext &&
          strncmp( pRule->pMatch->value, "__PLATFORM__", 12 ) == 0 )
      {
         * pRulePtr = pRule->pPrev;
         hb_pp_ruleFree( pRule );
         pState->iDefinitions--;
      }
      else
         pRulePtr = &pRule->pPrev;
   }
}

static int hb_pp_preprocesfile( PHB_PP_STATE pState, char * szRuleFile )
{
   int iResult = 0;
   ULONG ulLen;

   while( hb_pp_nextLine( pState, &ulLen ) != NULL && ulLen );

   if( szRuleFile )
   {
      FILE * foutr;

      foutr = fopen( szRuleFile, "w" );
      if( !foutr )
      {
         perror( szRuleFile );
         iResult = 1;
      }
      else
      {
         hb_pp_undefCompilerRules( pState );
         hb_pp_generateRules( foutr, pState );
         fclose( foutr );
      }
   }

   return iResult;
}


/*
 * ppgen only functions
 */
static void hb_pp_usage( char * szName )
{
   printf( "Syntax:  %s <file>[.prg] [options]\n\n", szName );
   printf( "Options: -i<path>  \tadd #include file search path\n"
           "         -o[<file>]\tcreates .c file with PP rules\n"
           "         -w        \twrite preprocessed (.ppo) input file\n"
           "         -q        \tdisable information messages\n" );
}

int main( int argc, char * argv[] )
{
   char * szFile = NULL, * szRuleFile = NULL;
   BOOL fQuiet = FALSE, fWrite = FALSE;
   PHB_PP_STATE pState;
   int iResult, i;

   pState = hb_pp_new();

   if( argc >= 2 )
   {
      szFile = argv[1];
      for( i = 2; szFile && i < argc; i++ )
      {
         if( !HB_ISOPTSEP( argv[i][0] ) )
            szFile = NULL;
         else
         {
            switch( argv[i][1] )
            {
               case 'q':
               case 'Q':
                  if( argv[i][2] )
                     szFile = NULL;
                  else
                     fQuiet = TRUE;
                  break;

               case 'w':
               case 'W':
                  if( argv[i][2] )
                     szFile = NULL;
                  else
                     fWrite = TRUE;
                  break;

               case 'i':
               case 'I':
                  if( argv[i][2] )
                     hb_pp_addSearchPath( pState, argv[i] + 2, FALSE );
                  else
                     szFile = NULL;
                  break;

               case 'o':
               case 'O':
                  if( argv[i][2] )
                     szRuleFile = argv[i] + 2;
                  else
                     szFile = NULL;
                  break;

               default:
                  szFile = NULL;
                  break;
            }
         }
      }
   }

   if( szFile )
   {
      hb_pp_init( pState, NULL, fQuiet, NULL, NULL, NULL, NULL, NULL, NULL, NULL );
      hb_pp_inFile( pState, szFile, NULL );
      if( fWrite )
      {
         char szFileName[ _POSIX_PATH_MAX + 1 ];
         PHB_FNAME pFileName;

         pFileName = hb_fsFNameSplit( szFile );
         pFileName->szExtension = ".ppo";
         hb_fsFNameMerge( szFileName, pFileName );
         hb_xfree( pFileName );

         hb_pp_outFile( pState, szFileName, NULL );
      }
      iResult = hb_pp_preprocesfile( pState, szRuleFile );
   }
   else
   {
      hb_pp_usage( argv[0] );
      iResult = 1;
   }

   hb_pp_free( pState );

   return iResult;
}

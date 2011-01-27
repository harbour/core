/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Preprocessor core module
 *
 * Copyright 1999 Alexander S.Kresin <alex@belacy.belgorod.su>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour.01 syenar.hu)
 *    __DATE__, __TIME__, __HB_MAIN__ support
 *
 * Copyright 2000 Ron Pinkas <Ron@Profit-Master.com>
 *
 * hb_pp_SetRules_() and related code for supportting
 * replaceable rules with -w switch
 *
 * See COPYING for licensing terms.
 *
 */

/*
 * Avoid tracing in preprocessor/compiler.
 */
#if ! defined( HB_TRACE_UTILS )
#if defined( HB_TRACE_LEVEL )
#undef HB_TRACE_LEVEL
#endif
#endif

#include <ctype.h>

#include "hbppdef.h"
#include "hbcomp.h"
#include "hbdate.h"

#if ! defined( __MINGW32CE__ ) && ! ( defined( _MSC_VER ) && defined( HB_OS_WIN_CE ) )
#  include <errno.h>
#endif

int hb_pp_ParseDefine_( char * );         /* Process #define directive */

static COMMANDS * AddCommand( char * );   /* Add new #command to an array  */
static COMMANDS * AddTranslate( char * ); /* Add new #translate to an array  */
static DEFINES * DefSearch( char *, int, HB_BOOL * );
static COMMANDS * ComSearch( char *, COMMANDS * );
static COMMANDS * TraSearch( char *, COMMANDS * );

static int ParseUndef( char * );                         /* Process #undef directive */
static int ParseIfdef( char *, int );                    /* Process #ifdef directive */
static void ParseCommand( char *, HB_BOOL, HB_BOOL );    /* Process #command or #translate directive */
static void ConvertPatterns( char *, int, char *, int ); /* Converting result pattern in #command and #translate */
static int WorkDefine( char **, char *, DEFINES * );     /* Replace fragment of code with a #defined result text */
static int WorkPseudoF( char **, char *, DEFINES * );    /* Replace pseudofunction with a #defined result text */
static int WorkCommand( char *, char *, COMMANDS * );
static int WorkTranslate( char *, char *, COMMANDS *, int * );
static int CommandStuff( char *, char *, char *, int *, HB_BOOL, HB_BOOL );
static int RemoveSlash( char * );
static int WorkMarkers( char **, char **, char *, int *, HB_BOOL, HB_BOOL );
static int getExpReal( char *, char **, HB_BOOL, int, HB_BOOL, HB_BOOL );
static HB_BOOL isExpres( char *, HB_BOOL );
static HB_BOOL TestOptional( char *, char * );
static HB_BOOL CheckOptional( char *, char *, char *, int *, HB_BOOL, HB_BOOL );
static void SkipOptional( char ** );

static void SearnRep( char *, char *, int, char *, int * );
static int ReplacePattern( char, char *, int, char *, int );
static void pp_rQuotes( char *, char * );
static int md_strAt( char *, int, char *, HB_BOOL, HB_BOOL, HB_BOOL, int );

#define MD_STR_AT_IGNORECASE  0  /* search ignoring case */
#define MD_STR_AT_USESUBCASE  1  /* use case specified in search string (old) */
static char * PrevSquare( char *, char *, int * );
static int IsInStr( char, char * );
static int stroncpy( char *, char *, int );
static int strincpy( char *, char * );
static HB_BOOL truncmp( char **, char **, HB_BOOL );
static HB_BOOL strincmp( char *, char **, HB_BOOL );
static int strotrim( char *, HB_BOOL );    /* Ron Pinkas 2001-02-14 added 2nd parameter */
static int NextWord( char **, char *, HB_BOOL );
static int NextName( char **, char * );
static int NextParm( char **, char * );
static HB_BOOL OpenInclude( char *, HB_PATHNAMES *, PHB_FNAME, HB_BOOL bStandardOnly, char * );
static HB_BOOL IsIdentifier( char * szProspect );
static int IsMacroVar( char * szText, HB_BOOL isCommand );
static void RemoveOptional( char * cpatt );
static int ConvertOptional( char * cpatt, int len, HB_BOOL bLeft );

#define ISNAME( c ) ( isalnum( c ) || ( c ) == '_' || ( c ) > 0x7E )
#define MAX_NAME           255
#define MAX_EXP            2048
#define PATTERN_SIZE       2048

#define STATE_INIT         0
#define STATE_NORMAL       1
#define STATE_COMMENT      2
#define STATE_QUOTE1       3
#define STATE_QUOTE2       4
#define STATE_QUOTE3       5
#define STATE_ID_END       6
#define STATE_ID           7
#define STATE_EXPRES       8
#define STATE_EXPRES_ID    9
#define STATE_BRACKET      10

#define IT_EXPR            1
#define IT_ID              2
#define IT_COMMA           3
#define IT_ID_OR_EXPR      4

#define HB_PP_MAX_INCLUDES FOPEN_MAX - 5 - 1

#define HB_PP_MATCH_MARK   '\1'
#define HB_PP_OPT_START    '\2'
#define HB_PP_OPT_END      '\3'

/* Ron Pinkas added 2000-01-24 */
#define IS_2CHAR_OPERATOR( p ) ( p[ 0 ] && p[ 1 ] && ( strncmp( p, ":=", 2 ) == 0 || \
                                                       strncmp( p, "+=", 2 ) == 0 || \
                                                       strncmp( p, "-=", 2 ) == 0 || \
                                                       strncmp( p, "*=", 2 ) == 0 || \
                                                       strncmp( p, "/=", 2 ) == 0 || \
                                                       strncmp( p, "^=", 2 ) == 0 || \
                                                       strncmp( p, "==", 2 ) == 0 || \
                                                       strncmp( p, "<>", 2 ) == 0 || \
                                                       strncmp( p, "<=", 2 ) == 0 || \
                                                       strncmp( p, ">=", 2 ) == 0 || \
                                                       strncmp( p, "++", 2 ) == 0 || \
                                                       strncmp( p, "--", 2 ) == 0 || \
                                                       strncmp( p, "->", 2 ) == 0 ) )
/* END, Ron Pinkas added 2000-01-24 */


static int     s_kolAddDefs  = 0;
static int     s_kolAddComs  = 0;
static int     s_kolAddTras  = 0;
static int     s_ParseState;
static int     s_maxCondCompile;
static int     s_aIsRepeate[ 5 ];
static int     s_Repeate;
static HB_BOOL s_bReplacePat = HB_TRUE;
static int     s_numBrackets;
static char    s_groupchar;
static char    s_prevchar;
/* additional buffers for expressions */
static char *  s_expreal                 = NULL;   /* allocation inside WorkMarkers */
static char *  s_expcopy                 = NULL;   /* allocation inside SearnExp */

/* global variables */
int *          hb_pp_aCondCompile        = NULL;
int            hb_pp_nCondCompile        = 0;
HB_BOOL        hb_pp_NestedLiteralString = HB_FALSE;
HB_BOOL        hb_pp_LiteralEscSeq       = HB_FALSE;
unsigned int   hb_pp_MaxTranslateCycles  = 1024;
int            hb_pp_StreamBlock         = 0;
char *         hb_pp_STD_CH              = NULL;

/* Ron Pinkas added 2000-11-21 */
static HB_BOOL s_bArray                  = HB_FALSE;

/* Table with parse errors */
const char *   hb_pp_szErrors[]          = {
   "Can\'t open #include file: \'%s\'; %s",
   "#else does not match #ifdef",
   "#endif does not match #ifdef",
   "Bad filename in #include",
   "#define without parameters",
   "Missing => in #translate/#command \'%s\' [%s]'",
   "Error in pattern definition",
   "Cycled #define",
   "Invalid name follows #: \'%s\'",
   "\'%s\'",
   "Memory allocation error",
   "Memory reallocation error",
   "Freeing a NULL memory pointer",
   "Value out of range in #pragma directive",
   "Can\'t open command definitions file: \'%s\'",
   "Invalid command definitions file name: \'%s\'",
   "Too many nested #includes, can\'t open: \'%s\'",
   "Input buffer overflow",
   "Label missing in #define '%s'",
   "Comma or right parenthesis missing in #define '%s'",
   "Label duplicated in #define '%s(%s)'",
};

/* Table with warnings */
const char *   hb_pp_szWarnings[] = {
   "1Redefinition or duplicate definition of #define %s",
   "1No directives in command definitions file"
};

void hb_pp_SetRules_( HB_INCLUDE_FUNC_PTR pIncludeFunc, HB_BOOL bQuiet )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_SetRules_()" ) );

   if( hb_pp_STD_CH )
   {
      if( *hb_pp_STD_CH > ' ' )
      {
         hb_comp_pFileName = hb_fsFNameSplit( hb_pp_STD_CH );

         if( hb_comp_pFileName->szName )
         {
            char szFileName[ HB_PATH_MAX ];

            if( ! hb_comp_pFileName->szExtension )
               hb_comp_pFileName->szExtension = ".ch";

            hb_fsFNameMerge( szFileName, hb_comp_pFileName );

            if( ( *pIncludeFunc )( szFileName, hb_comp_pIncludePath ) )
            {
               hb_pp_Init();

               hb_pp_ReadRules();

               if( s_kolAddComs || s_kolAddTras || s_kolAddDefs > 3 )
               {
                  if( ! bQuiet )
                     printf( "Loaded: %i Commands, %i Translates, %i Defines from: %s\n", s_kolAddComs, s_kolAddTras, s_kolAddDefs - 3, szFileName );
               }
               else
               {
                  hb_compGenWarning( NULL, hb_pp_szWarnings, 'I', HB_PP_WARN_NO_DIRECTIVES, NULL /*szFileName */, NULL );
               }

               fclose( hb_comp_files.pLast->handle );
               hb_xfree( hb_comp_files.pLast->pBuffer );
               hb_xfree( hb_comp_files.pLast->szFileName );
               hb_xfree( hb_comp_files.pLast );
               hb_comp_files.pLast    = NULL;
               hb_comp_files.iFiles   = 0;

               hb_xfree( hb_comp_pFileName );
               hb_comp_pFileName      = NULL;
            }
            else
            {
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_CANNOT_OPEN_RULES, szFileName, NULL );
            }
         }
         else
         {
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_BAD_RULES_FILE_NAME, hb_pp_STD_CH, NULL );
         }
      }
      else
      {
         if( ! bQuiet )
            printf( "Standard command definitions excluded.\n" );

         hb_pp_Init();
      }
      hb_xfree( hb_pp_STD_CH );
   }
   else
   {
      hb_pp_Table();
      hb_pp_Init();
   }
}

void hb_pp_Free( void )
{
   DEFINES *   stdef;
   COMMANDS *  stcmd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_Free()" ) );

   while( s_kolAddDefs )
   {
      stdef = hb_pp_topDefine;
      if( stdef->pars )
         hb_xfree( stdef->pars );
      if( stdef->value )
         hb_xfree( stdef->value );
      if( stdef->name )
         hb_xfree( stdef->name );
      hb_pp_topDefine = stdef->last;
      hb_xfree( stdef );
      s_kolAddDefs--;
   }
   while( s_kolAddComs )
   {
      stcmd = hb_pp_topCommand;
      if( stcmd->mpatt )
         hb_xfree( stcmd->mpatt );
      if( stcmd->value )
         hb_xfree( stcmd->value );
      hb_xfree( stcmd->name );
      hb_pp_topCommand = stcmd->last;
      hb_xfree( stcmd );
      s_kolAddComs--;
   }
   while( s_kolAddTras )
   {
      stcmd = hb_pp_topTranslate;
      if( stcmd->mpatt )
         hb_xfree( stcmd->mpatt );
      if( stcmd->value )
         hb_xfree( stcmd->value );
      hb_xfree( stcmd->name );
      hb_pp_topTranslate = stcmd->last;
      hb_xfree( stcmd );
      s_kolAddTras--;
   }
   if( hb_pp_aCondCompile )
   {
      hb_xfree( hb_pp_aCondCompile );
      hb_pp_aCondCompile = NULL;
   }
   hb_pp_InternalFree();

   if( s_expreal )
   {
      hb_xfree( ( void * ) s_expreal );
      s_expreal = NULL;
   }
   if( s_expcopy )
   {
      hb_xfree( ( void * ) s_expcopy );
      s_expcopy = NULL;
   }
}

void hb_pp_Init( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_Init()" ) );

   hb_pp_Free();

   s_ParseState     = 0;
   s_maxCondCompile = 5;
   s_bReplacePat    = HB_TRUE;
   s_prevchar       = 'A';

   if( ! hb_pp_aCondCompile )
      hb_pp_aCondCompile = ( int * ) hb_xgrab( sizeof( int ) * 5 );

   hb_pp_nCondCompile = 0;

   {
      char     sOS[ 64 ];
      char     sVer[ 64 ];
      char *   pSrc, * pDst;
      char *   szPlatform = hb_verPlatform();
      int      n;

      hb_strncpy( sOS, "__PLATFORM__", sizeof( sOS ) - 1 );

      pSrc = szPlatform;
      n    = strlen( sOS );
      pDst = sOS;

      while( *pSrc && *pSrc != ' ' && n < ( int ) sizeof( sOS ) - 1 )
      {
         if( *pSrc == '_' || ( *pSrc >= 'A' && *pSrc <= 'Z' ) || ( *pSrc >= 'a' && *pSrc <= 'z' ) || ( *pSrc >= '0' && *pSrc <= '9' ) )
         {
            pDst[ n++ ] = *pSrc;
         }
         pSrc++;
      }
      pDst[ n ]     = 0;

      n             = 0;
      pDst          = sVer;
      pDst[ n++ ]   = '"';
      if( *pSrc == ' ' )
      {
         while( *( ++pSrc ) && n < ( int ) sizeof( sVer ) - 2 )
            pDst[ n++ ] = *pSrc;
      }
      pDst[ n++ ]   = '"';
      pDst[ n ]     = 0;

      hb_pp_AddDefine_( sOS, sVer );
#ifdef HB_OS_UNIX
      hb_strncpy( &sOS[ 12 ], "UNIX", sizeof( sOS ) - 13 );
      hb_pp_AddDefine_( sOS, sVer );
#endif
      hb_xfree( szPlatform );
   }

   {
      char     szResult[ 6 ];
      USHORT   usHarbour = ( 256 * HB_VER_MAJOR ) + HB_VER_MINOR;

      /*
         This updates __HARBOUR__ on every change of HB_VER_MAJOR / HB_VER_MINOR
         HIBYTE is the HB_VER_MAJOR value and the LOBYTE is the HB_VER_MINOR value.

         The check below is to ensure that __HARBOUR__ gets the
         value of 1 by default
       */
      hb_snprintf( szResult, sizeof( szResult ), "%05d", ( usHarbour ? usHarbour : 1 ) );
      hb_pp_AddDefine_( "__HARBOUR__", szResult );
   }

   {
      int   iYear, iMonth, iDay;
      char  szResult[ 11 ];

      hb_dateToday( &iYear, &iMonth, &iDay );
      hb_dateStrPut( szResult + 1, iYear, iMonth, iDay );
      szResult[ 0 ]    = '"';
      szResult[ 9 ]    = '"';
      szResult[ 10 ]   = '\0';
      hb_pp_AddDefine_( "__DATE__", szResult );

      hb_dateTimeStr( szResult + 1 );
      szResult[ 0 ]    = '"';
      szResult[ 9 ]    = '"';
      szResult[ 10 ]   = '\0';
      hb_pp_AddDefine_( "__TIME__", szResult );
   }

   {
      char szResult[ 11 ];

      hb_snprintf( szResult, sizeof( szResult ), "%d", ( int ) sizeof( void * ) );
#if defined( HB_ARCH_16BIT )
      hb_pp_AddDefine_( "__ARCH16BIT__", szResult );
#elif defined( HB_ARCH_32BIT )
      hb_pp_AddDefine_( "__ARCH32BIT__", szResult );
#elif defined( HB_ARCH_64BIT )
      hb_pp_AddDefine_( "__ARCH64BIT__", szResult );
#endif

#if defined( HB_LITTLE_ENDIAN )
      hb_pp_AddDefine_( "__LITTLE_ENDIAN__", szResult );
#elif defined( HB_BIG_ENDIAN )
      hb_pp_AddDefine_( "__BIG_ENDIAN__", szResult );
#elif defined( HB_PDP_ENDIAN )
      hb_pp_AddDefine_( "__PDP_ENDIAN__", szResult );
#endif
   }

#ifdef HB_START_PROCEDURE
   hb_pp_AddDefine_( "__HB_MAIN__", HB_START_PROCEDURE );
#endif
}

/* Table with parse warnings */
/* NOTE: The first character stores the warning's level that triggers this
 * warning. The warning's level is set by -w<n> command line option.
 */

int hb_pp_ParseDirective_( char * sLine )
{
   char     sDirective[ MAX_NAME ];
   char     szInclude[ HB_PATH_MAX ];
   int      i;
   int      bIgnore = 1;
   char *   sParse;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_ParseDirective_(%s)", sLine ) );

   strotrim( sLine, HB_TRUE );
   hb_pp_strocpy( sLine, sLine + 1 );
   sParse  = sLine;

   i       = NextName( &sLine, sDirective );
   hb_strupr( sDirective );

   HB_SKIPTABSPACES( sLine );

   if( i == 4 && memcmp( sDirective, "ELSE", 4 ) == 0 ) /* ---  #else  --- */
   {
      if( hb_pp_nCondCompile == 0 )
         hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_DIRECTIVE_ELSE, NULL, NULL );
      else if( hb_pp_nCondCompile == 1 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 2 ] )
         hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] = 1 - hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ];
   }
   else if( i >= 4 && i <= 5 && memcmp( sDirective, "ENDIF", i ) == 0 ) /* --- #endif  --- */
   {
      if( hb_pp_nCondCompile == 0 )
         hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_DIRECTIVE_ENDIF, NULL, NULL );
      else
         hb_pp_nCondCompile--;
   }
   else if( i >= 4 && i <= 5 && memcmp( sDirective, "IFDEF", i ) == 0 )
      ParseIfdef( sLine, HB_TRUE );        /* --- #ifdef  --- */

   else if( i >= 4 && i <= 6 && memcmp( sDirective, "IFNDEF", i ) == 0 )
      ParseIfdef( sLine, HB_FALSE );       /* --- #ifndef  --- */

   else if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
   {
      if( i >= 4 && i <= 7 && memcmp( sDirective, "INCLUDE", i ) == 0 ) /* --- #include --- */
      {
         char cDelimChar;

         if( *sLine != '\"' && *sLine != '\'' && *sLine != '<' )
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_WRONG_NAME, NULL, NULL );

         cDelimChar = *sLine;
         if( cDelimChar == '<' )
            cDelimChar = '>';
         else if( cDelimChar == '`' )
            cDelimChar = '\'';

         sLine++;
         i = 0;
         while( *( sLine + i ) != '\0' && *( sLine + i ) != cDelimChar )
            i++;
         if( *( sLine + i ) != cDelimChar )
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_WRONG_NAME, NULL, NULL );
         *( sLine + i ) = '\0';

         if( ! OpenInclude( sLine, hb_comp_pIncludePath, hb_comp_pFileName, ( cDelimChar == '>' ), szInclude ) )
         {
            if( hb_fsMaxFilesError() )
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_TOO_MANY_INCLUDES, sLine, NULL );
            else
            {
#if defined( __CYGWIN__ ) || defined( __IBMCPP__ ) || defined( __LCC__ ) || defined( HB_OS_WIN_CE )
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_CANNOT_OPEN, sLine, "" );
#else
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_CANNOT_OPEN, sLine, strerror( errno ) );
#endif
            }
         }
      }
      else if( i >= 4 && i <= 6 && memcmp( sDirective, "DEFINE", i ) == 0 )
         hb_pp_ParseDefine_( sLine );    /* --- #define  --- */

      else if( i >= 4 && i <= 5 && memcmp( sDirective, "UNDEF", i ) == 0 )
         ParseUndef( sLine );   /* --- #undef  --- */

      else if( ( i >= 4 && i <= 7 && memcmp( sDirective, "COMMAND", i ) == 0 ) || ( i >= 4 && i <= 8 && memcmp( sDirective, "XCOMMAND", i ) == 0 ) )
         /* --- #command  --- */
         ParseCommand( sLine, ( i == 7 ) ? HB_FALSE : HB_TRUE, HB_TRUE );

      else
      if( ( i >= 4 && i <= 9 && memcmp( sDirective, "TRANSLATE", i ) == 0 )
          || ( i >= 4 && i <= 10 && memcmp( sDirective, "XTRANSLATE", i ) == 0 ) )
         /* --- #translate  --- */
         ParseCommand( sLine, ( i == 9 ) ? HB_FALSE : HB_TRUE, HB_FALSE );

      else if( i >= 4 && i <= 6 && memcmp( sDirective, "STDOUT", i ) == 0 )
         printf( "%s\n", sLine );       /* --- #stdout  --- */

      else if( i >= 4 && i <= 5 && memcmp( sDirective, "ERROR", i ) == 0 )
         /* --- #error  --- */
         hb_compGenError( NULL, hb_pp_szErrors, 'E', HB_PP_ERR_EXPLICIT, sLine, NULL );

      else if( i == 4 && memcmp( sDirective, "LINE", 4 ) == 0 )
         return -1;

      else if( i == 6 && memcmp( sDirective, "PRAGMA", 6 ) == 0 )
      {
         hb_pp_strocpy( sParse, sParse + 6 );
         bIgnore = hb_pp_ParsePragma( sParse ); /* --- #pragma  --- */
      }
      else
         hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_WRONG_DIRECTIVE, sDirective, NULL );
   }
   return bIgnore;
}

int hb_pp_ParseDefine_( char * sLine )
{
   char        defname[ MAX_NAME ], pars[ MAX_NAME + 1 ];
   int         i, npars = -1;
   DEFINES *   lastdef;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_ParseDefine_(%s)", sLine ) );

   HB_SKIPTABSPACES( sLine );
   if( ISNAME( ( BYTE ) *sLine ) )
   {
      char * cParams = NULL;

      NextName( &sLine, defname );
      if( *sLine == '(' )       /* If pseudofunction was found */
      {
         int   iParLen = 0;
         int   iLen;

         sLine++;
         HB_SKIPTABSPACES( sLine );

         npars = 0;
         while( *sLine && *sLine != ')' )
         {
            if( ISNAME( ( BYTE ) *sLine ) )
            {
               NextName( &sLine, pars );
               iLen = strlen( pars );
               if( cParams == NULL )
               {
                  /* 'xy0' -> '~xy0' */
                  cParams = ( char * ) hb_xgrab( iLen + 2 );
               }
               else
               {
                  /* '~xy0' -> '~xy,~ab0' */
                  char * cPos;

                  cPos = strstr( cParams, pars );
                  if( cPos && ( cPos[ iLen ] == ',' || cPos[ iLen ] == '\0' ) )
                  {
                     cPos--;
                     if( *cPos == '\001' )
                        hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_LABEL_DUPL_IN_DEFINE, defname, pars );
                  }
                  cParams                = ( char * ) hb_xrealloc( cParams, iParLen + iLen + 3 );
                  cParams[ iParLen++ ]   = ',';
                  cParams[ iParLen ]     = '\0';
               }
               cParams[ iParLen ]  = '\001';
               memcpy( cParams + iParLen + 1, pars, iLen + 1 );
               iParLen            += iLen + 1;
               npars++;
               HB_SKIPTABSPACES( sLine );
            }
            else
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_LABEL_MISSING_IN_DEFINE, defname, NULL );

            if( *sLine == ',' )
            {
               sLine++;
               HB_SKIPTABSPACES( sLine );
               if( *sLine == ')' )
                  hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_LABEL_MISSING_IN_DEFINE, defname, NULL );
            }
         }
         HB_SKIPTABSPACES( sLine );
         if( *sLine == '\0' )
            hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PARE_MISSING_IN_DEFINE, defname, NULL );

         sLine++;
      }

      HB_SKIPTABSPACES( sLine );

      if( cParams )
      {
         char *   tmp = cParams;
         char *   cPos;
         int      iPar, iLen, iPos, iOldPos;

         iLen = strlen( sLine );
         for( i = 0; i < npars; i++ )
         {
            /*1z,1y */
            cPos = strchr( tmp, ',' );
            if( cPos )
               iPar = cPos - tmp;
            else
               iPar = strlen( tmp );
            memcpy( pars, tmp, iPar );
            pars[ iPar ]  = '\0';
            iOldPos       = 0;
            while( ( iPos = md_strAt( pars + 1, iPar - 1, sLine + iOldPos, HB_TRUE, HB_FALSE, HB_FALSE, MD_STR_AT_IGNORECASE ) ) != 0 )
            {
               if( sLine[ iOldPos + iPos ] != '\001' )
               {
                  hb_pp_Stuff( pars, sLine + iOldPos + iPos - 1, iPar, iPar - 1, iLen - iPos - iOldPos );
                  iLen++;
               }
               iOldPos += iPos + iPar;
            }
            if( cPos )
               tmp = cPos + 1;
         }
      }
      lastdef = hb_pp_AddDefine_( defname, ( *sLine == '\0' ) ? NULL : sLine );

      if( lastdef )
      {
         lastdef->npars   = npars;
         lastdef->pars    = cParams;
      }
      else if( cParams )
         hb_xfree( cParams );
   }
   else
      hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_DEFINE_ABSENT, NULL, NULL );

   return 0;
}

DEFINES * hb_pp_AddDefine_( char * defname, char * value )
{
   HB_BOOL     isNew;
   DEFINES *   stdef;
   int         len = strlen( defname );

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_AddDefine_(%s, %s)", defname, value ) );

   stdef = DefSearch( defname, len, &isNew );

   if( stdef != NULL )
   {
      hb_compGenWarning( NULL, hb_pp_szWarnings, 'I', HB_PP_WARN_DEFINE_REDEF, defname, NULL );

      if( isNew )
      {
         if( stdef->pars )
            hb_xfree( stdef->pars );
         if( stdef->value )
            hb_xfree( stdef->value );
      }
      else
         return NULL;
   }
   else
   {
      stdef            = ( DEFINES * ) hb_xgrab( sizeof( DEFINES ) );
      stdef->last      = hb_pp_topDefine;
      hb_pp_topDefine  = stdef;
      stdef->name      = hb_strdup( defname );
      stdef->namelen   = len;
      stdef->npars     = -1;

      s_kolAddDefs++;
   }

   stdef->value  = ( value == NULL ) ? NULL : hb_strdup( value );
   stdef->pars   = NULL;

   return stdef;
}

static int ParseUndef( char * sLine )
{
   char        defname[ MAX_NAME ];
   DEFINES *   stdef;
   HB_BOOL     isNew;
   int         len;

   HB_TRACE( HB_TR_DEBUG, ( "ParseUndef(%s)", sLine ) );

   NextWord( &sLine, defname, HB_FALSE );

   len = strlen( defname );
   if( ( stdef = DefSearch( defname, len, &isNew ) ) != NULL )
   {
      if( isNew )
      {
         if( stdef->pars )
            hb_xfree( stdef->pars );
         if( stdef->value )
            hb_xfree( stdef->value );
         hb_xfree( stdef->name );
      }
      stdef->pars      = NULL;
      stdef->value     = NULL;
      stdef->name      = NULL;
      stdef->namelen   = 0;
   }

   return 0;
}

static int ParseIfdef( char * sLine, int usl )
{
   char        defname[ MAX_NAME ];
   DEFINES *   stdef;
   int         len = 0;

   HB_TRACE( HB_TR_DEBUG, ( "ParseIfdef(%s, %d)", sLine, usl ) );

   if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
   {
      len = NextWord( &sLine, defname, HB_FALSE );
      if( *defname == '\0' )
         hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_DEFINE_ABSENT, NULL, NULL );
   }
   if( hb_pp_nCondCompile == s_maxCondCompile )
   {
      s_maxCondCompile   += 5;
      hb_pp_aCondCompile  = ( int * ) hb_xrealloc( hb_pp_aCondCompile, sizeof( int ) * s_maxCondCompile );
   }
   if( hb_pp_nCondCompile == 0 || hb_pp_aCondCompile[ hb_pp_nCondCompile - 1 ] )
   {
      if( ( ( stdef = DefSearch( defname, len, NULL ) ) != NULL && usl ) || ( stdef == NULL && ! usl ) )
         hb_pp_aCondCompile[ hb_pp_nCondCompile ] = 1;
      else
         hb_pp_aCondCompile[ hb_pp_nCondCompile ] = 0;
   }
   else
      hb_pp_aCondCompile[ hb_pp_nCondCompile ] = 0;

   hb_pp_nCondCompile++;

   return 0;
}

static DEFINES * DefSearch( char * defname, int len, HB_BOOL * isNew )
{
   int         kol     = 0, j;
   DEFINES *   stdef   = hb_pp_topDefine;

   HB_TRACE( HB_TR_DEBUG, ( "DefSearch(%s)", defname ) );

   while( stdef != NULL )
   {
      kol++;
      if( stdef->name != NULL && stdef->namelen == len )
      {
         for( j = 0; *( stdef->name + j ) == *( defname + j ) && *( stdef->name + j ) != '\0'; j++ )
            ;
         if( *( stdef->name + j ) == *( defname + j ) )
         {
            if( isNew )
               *isNew = ( s_kolAddDefs >= kol );
            return stdef;
         }
      }
      stdef = stdef->last;
   }
   return NULL;
}

static COMMANDS * ComSearch( char * cmdname, COMMANDS * stcmdStart )
{
   COMMANDS * stcmd = ( stcmdStart ) ? stcmdStart : hb_pp_topCommand;

   HB_TRACE( HB_TR_DEBUG, ( "ComSearch(%s, %p)", cmdname, stcmdStart ) );

   while( stcmd != NULL )
   {
      int j;

      for( j = 0; ( *( stcmd->name + j ) == HB_TOUPPER( *( cmdname + j ) ) ) &&
           ( *( stcmd->name + j ) != '\0' ) && ( ( stcmd->com_or_xcom ) ? 1 : ( j < 4 || ISNAME( ( BYTE ) *( cmdname + j + 1 ) ) ) ); j++ )
         ;
      if( ( *( stcmd->name + j ) == HB_TOUPPER( *( cmdname + j ) ) )
          || ( ! stcmd->com_or_xcom && j >= 4 && *( stcmd->name + j ) != '\0' && *( cmdname + j ) == '\0' ) )
         break;

      stcmd = stcmd->last;
   }

   return stcmd;
}

static COMMANDS * TraSearch( char * cmdname, COMMANDS * sttraStart )
{
   int         j;
   COMMANDS *  sttra = ( sttraStart ) ? sttraStart : hb_pp_topTranslate;

   HB_TRACE( HB_TR_DEBUG, ( "TraSearch(%s, %p)", cmdname, sttraStart ) );

   while( sttra != NULL )
   {
      for( j = 0; *( sttra->name + j ) == HB_TOUPPER( *( cmdname + j ) ) &&
           *( sttra->name + j ) != '\0' && ( ( sttra->com_or_xcom ) ? 1 : ( j < 4 || ISNAME( ( BYTE ) *( cmdname + j + 1 ) ) ) ); j++ )
         ;
      if( *( sttra->name + j ) == HB_TOUPPER( *( cmdname + j ) )
          || ( ! sttra->com_or_xcom && j >= 4 && *( sttra->name + j ) != '\0' && *( cmdname + j ) == '\0' ) )
         break;
      sttra = sttra->last;
   }
   return sttra;
}

static void ParseCommand( char * sLine, HB_BOOL com_or_xcom, HB_BOOL com_or_tra )
{
#if ! defined( HB_PP_DEBUG_MEMORY )
   static char mpatt[ PATTERN_SIZE ];
#else
   char *      mpatt = ( char * ) hb_xgrab( PATTERN_SIZE );
#endif
   char *      rpatt;

   char        cmdname[ MAX_NAME ];
   COMMANDS *  stcmd;
   int         mlen, rlen;
   int         ipos;

   /* Ron Pinkas added 2000-12-03 */
   HB_BOOL     bOk = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "ParseCommand(%s, %d, %d)", sLine, com_or_xcom, com_or_tra ) );

   HB_SKIPTABSPACES( sLine );
   ipos = 0;

   /* JFL 2000-09-19 */
   /* This was the original line as Alexander wrote it */
   /* while( *sLine != '\0' && *sLine != ' ' && *sLine != '\t' && *sLine != '<' && *sLine != '=' && ( *sLine != '(' || ipos == 0 ) ) */
   /* Now the line #xtranslate = name(.. => will be allowed */

   /* I changed it to the following to allow < and = to be the first char within a translate or xtranslate */
   while( *sLine != '\0' && *sLine != ' ' && *sLine != '\t'
          && ( *sLine != '<' || ipos == 0 ) && ( *sLine != '=' || ipos == 0 ) && ( *sLine != '(' || ipos == 0 ) )
   {
      /* Ron Pinkas added 2000-01-24 */
      if( ! ISNAME( ( BYTE ) *sLine ) )
      {
         if( *sLine == '[' && ipos )
            break;

         if( IS_2CHAR_OPERATOR( sLine ) )
         {
            *( cmdname + ipos++ )  = *sLine++;
            *( cmdname + ipos++ )  = *sLine++;
            break;
         }
         else
         {
            *( cmdname + ipos++ ) = *sLine++;
            break;
         }
      }
      /* END, Ron Pinkas added 2000-01-24 */

      *( cmdname + ipos++ ) = *sLine++;
   }
   *( cmdname + ipos ) = '\0';

   if( ! ipos )
   {
#if defined( HB_PP_DEBUG_MEMORY )
      hb_xfree( mpatt );
#endif
      return;
   }

   hb_strupr( cmdname );
   HB_SKIPTABSPACES( sLine );

   /* Ron Pinkas added 2000-12-03 */
   ipos = 0;
   while( *sLine )
   {
      mpatt[ ipos++ ] = *sLine;

      if( *sLine == '=' )
      {
         int i = ipos;

         sLine++;
         mpatt[ i++ ] = *sLine;

         while( *sLine && ( *sLine == ' ' || *sLine == '\t' ) )
         {
            sLine++;
            mpatt[ i++ ] = *sLine;
         }

         if( *sLine == '>' )
         {
            ipos = ipos - 2;
            while( mpatt[ ipos ] == ' ' || mpatt[ ipos ] == '\t' )
            {
               ipos--;
            }

            mpatt[ ipos + 1 ]   = '\0';
            sLine++;
            bOk                 = HB_TRUE;
            break;
         }

         ipos = i;
      }

      sLine++;
   }
   /* End - Ron Pinkas added 2000-12-03 */

   /* Ron Pinkas modified 2000-12-03
      if( (ipos = hb_strAt( "=>", 2, sLine, strlen(sLine) )) > 0 ) */
   if( bOk )
   {
      /* Ron Pinkas removed 2000-12-03
         stroncpy( mpatt, sLine, ipos-1 ); */

      mlen = strotrim( mpatt, HB_TRUE );

      /* Ron Pinkas removed 2000-12-03
         sLine += ipos + 1; */

      HB_SKIPTABSPACES( sLine );
      /* hb_pp_strocpy( rpatt, sLine ); */
      rpatt   = sLine;
      rlen    = strotrim( rpatt, HB_TRUE );

      ConvertPatterns( mpatt, mlen, rpatt, rlen );
      RemoveSlash( mpatt );
      rlen = RemoveSlash( rpatt );

      if( com_or_tra )
         stcmd = AddCommand( cmdname );
      else
         stcmd = AddTranslate( cmdname );

      stcmd->com_or_xcom  = com_or_xcom;
      stcmd->mpatt        = hb_strdup( mpatt );
      stcmd->value        = ( rlen > 0 ) ? hb_strdup( rpatt ) : NULL;
   }
   else
   {
      sLine -= ( ipos + 1 );
      hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_COMMAND_DEFINITION, cmdname, sLine );
   }
#if defined( HB_PP_DEBUG_MEMORY )
   hb_xfree( mpatt );
#endif
}

/* Remove escape characters and check '[' optional markers
 */
static int ConvertOptional( char * cpatt, int len, HB_BOOL bLeft )
{
   int i = 0;

   while( cpatt[ i ] != '\0' )
   {
      if( cpatt[ i ] == '"' || cpatt[ i ] == '\'' )
      {
         char c = cpatt[ i ];

         i++;
         while( cpatt[ i ] && cpatt[ i ] != c )
         {
            i++;
         }
         i++;
         continue;              /* skip "strings" */
      }

      if( cpatt[ i ] == '[' )
      {
         if( i && cpatt[ i - 1 ] == '\\' )
         {
            hb_pp_Stuff( "", cpatt + i - 1, 0, 1, len - i + 1 );
            len--;
            continue;
         }
         else
         {
            int      j             = i + 1;
            int      iOpenBrackets = 1;
            HB_BOOL  bOption       = HB_FALSE;

            while( cpatt[ j ] && iOpenBrackets )
            {
               if( cpatt[ j ] == '[' && cpatt[ j - 1 ] != '\\' )
                  iOpenBrackets++;
               else if( cpatt[ j ] == ']' && cpatt[ j - 1 ] != '\\' )
               {
                  if( --iOpenBrackets == 0 && ( bOption || bLeft ) )
                  {
                     cpatt[ i ] = HB_PP_OPT_START;
                     cpatt[ j ] = HB_PP_OPT_END;
                  }
               }
               else if( cpatt[ j ] == '<' )
               {
                  j++;
                  while( cpatt[ j ] == ' ' || cpatt[ j ] == '\t' )
                     j++;
                  if( strchr( "*(!-{.\"", cpatt[ j ] ) || ISNAME( ( BYTE ) cpatt[ j ] ) )
                  {
                     bOption = HB_TRUE;
                     continue;
                  }
               }
               else if( cpatt[ j ] == '"' || cpatt[ j ] == '\'' )
               {
                  char c = cpatt[ j ];

                  j++;
                  while( cpatt[ j ] && cpatt[ j ] != c )
                  {
                     j++;
                  }
               }
               j++;
            }

            if( iOpenBrackets )
            {
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PATTERN_DEFINITION, cpatt + i, NULL );
            }
         }
      }
      else if( cpatt[ i ] == ']' )
      {
         if( i && cpatt[ i - 1 ] == '\\' )
         {
            hb_pp_Stuff( "", cpatt + i - 1, 0, 1, len - i + 1 );
            len--;
            continue;
         }
      }
      i++;
   }

   return len;
}

static void RemoveOptional( char * cpatt )
{
   int   i          = 0;
   int   len        = strlen( cpatt );
   int   iOpenBra   = 0;

   while( cpatt[ i ] != '\0' )
   {
      if( cpatt[ i ] == '"' || cpatt[ i ] == '\'' )
      {
         char c = cpatt[ i++ ];

         while( cpatt[ i ] && cpatt[ i ] != c )
         {
            i++;
         }
         if( cpatt[ i ] )
            i++;
         continue;              /* skip "strings" */
      }
      if( cpatt[ i ] == '[' )
      {
         i++;
         iOpenBra++;
         while( cpatt[ i ] && iOpenBra )
         {
            if( cpatt[ i ] == '[' )
               iOpenBra++;
            else if( cpatt[ i ] == ']' )
               iOpenBra--;
            i++;
         }
         continue;              /* skip [strings] */
      }

      if( cpatt[ i ] == HB_PP_OPT_START || cpatt[ i ] == HB_PP_OPT_END )
      {
         hb_pp_Stuff( "", cpatt + i, 0, 1, len - i + 1 );
         len--;
      }
      else
         i++;
   }
}


/* ConvertPatterns()
 * Converts result pattern in #command and #translate to inner format
 */

static void ConvertPatterns( char * mpatt, int mlen, char * rpatt, int rlen )
{
   int      i          = 0, ipos, ifou;
   int      explen, rmlen;
   char     exppatt[ MAX_NAME ], expreal[ 5 ] = "   0";
   char     lastchar   = '@', exptype;
   char *   ptr, * ptrtmp;

   HB_TRACE( HB_TR_DEBUG, ( "ConvertPatterns(%s, %d, %s, %d)", mpatt, mlen, rpatt, rlen ) );

   expreal[ 0 ]  = HB_PP_MATCH_MARK;
   mlen          = ConvertOptional( mpatt, mlen, HB_TRUE );    /* left pattern */
   rlen          = ConvertOptional( rpatt, rlen, HB_FALSE );   /* right pattern */

   while( *( mpatt + i ) != '\0' )
   {
      if( mpatt[ i ] == '"' || mpatt[ i ] == '\'' )
      {
         char c = mpatt[ i ];

         i++;
         while( mpatt[ i ] && mpatt[ i ] != c )
         {
            i++;
         }
         i++;
         continue;              /* skip "strings" */
      }

      if( *( mpatt + i ) == '<' )
      {
         if( i && mpatt[ i - 1 ] == '\\' )
         {
            i++;
            continue;
         }

         /* Drag match marker, determine it type */
         explen  = 0;
         ipos    = i;
         i++;
         exptype = '0';
         while( *( mpatt + i ) == ' ' || *( mpatt + i ) == '\t' )
            i++;
         if( *( mpatt + i ) == '*' )    /* Wild match marker */
         {
            exptype = '3';
            i++;
         }
         else if( *( mpatt + i ) == '(' )       /* Extended expression match marker */
         {
            exptype = '4';
            i++;
         }
         else if( *( mpatt + i ) == '!' )       /* Minimal expression match marker */
         {
            exptype = '5';
            i++;
         }
         ptr = mpatt + i;
         while( *ptr != '>' )
         {
            if( *ptr == '\0' || *ptr == '<' || *ptr == '[' || *ptr == ']' )
            {
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PATTERN_DEFINITION, NULL, NULL );
               return;
            }
            ptr++;
         }

         while( *( mpatt + i ) != '>' )
         {
            if( *( mpatt + i ) == ',' ) /* List match marker */
            {
               exptype = '1';
               while( *( mpatt + i ) != '>' )
                  i++;
               break;
            }
            else if( *( mpatt + i ) == ':' )    /* Restricted match marker */
            {
               exptype          = '2';
               *( mpatt + i-- ) = ' ';
               break;
            }
            if( *( mpatt + i ) != ' ' && *( mpatt + i ) != '\t' )
               *( exppatt + explen++ ) = *( mpatt + i );
            i++;
         }

         if( exptype == '3' )
         {
            if( *( exppatt + explen - 1 ) == '*' )
               explen--;
            else
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PATTERN_DEFINITION, NULL, NULL );
         }
         else if( exptype == '4' )
         {
            if( *( exppatt + explen - 1 ) == ')' )
               explen--;
            else
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PATTERN_DEFINITION, NULL, NULL );
         }
         else if( exptype == '5' )
         {
            if( *( exppatt + explen - 1 ) == '!' )
               explen--;
            else
               hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_PATTERN_DEFINITION, NULL, NULL );
         }

         rmlen         = i - ipos + 1;
         /* Convert match marker into inner format */
         lastchar      = ( lastchar != 'Z' ) ? ( ( char ) ( ( unsigned int ) lastchar + 1 ) ) : 'a';
         expreal[ 1 ]  = lastchar;
         expreal[ 2 ]  = exptype;
         hb_pp_Stuff( expreal, mpatt + ipos, 4, rmlen, mlen - ipos );
         mlen         += 4 - rmlen;
         i            += 4 - rmlen;

         /* Look for appropriate result markers */
         ptr           = rpatt;
         while( ( ifou = hb_strAt( exppatt, explen, ptr, rlen - ( ptr - rpatt ) ) ) > 0 )
         {
            /* Convert result marker into inner format */
            ifou--;
            ptr    += ifou;
            ptrtmp  = ptr + 1;
            rmlen   = explen;
            exptype = '0';      /* regular result marker */
            do
            {
               ptr--;
               rmlen++;
               ifou--;
               if( *ptr == '<' )
                  continue;
               else if( *ptr == '\"' )
                  exptype = '2';          /* normal stringify result marker */
               else if( *ptr == '(' )
                  exptype = '3';          /* Smart stringify result marker */
               else if( *ptr == '{' )
                  exptype = '4';          /* Blockify result marker */
               else if( *ptr == '.' )
                  exptype = '5';          /* Logify result marker */
               else if( *ptr == '-' )
                  exptype = '6';          /* ommit (remove) result marker */
               else if( *ptr == ' ' || *ptr == '\t' )
                  continue;
               else
                  ifou = -1;
            }
            while( ifou >= 0 && *ptr != '<' && *( ptr - 1 ) != '\\' );

            if( ifou >= 0 && *ptr == '<' )
            {
               ptr += rmlen++;
               while( *ptr != '\0' && *ptr != '>' && *( ptr - 1 ) != '\\' )
               {
                  if( *ptr != ' ' && *ptr != '\t' && *ptr != '\"' && *ptr != ')' && *ptr != '}' && *ptr != '.' && *ptr != '-' )
                  {
                     ifou = -1;
                     break;
                  }
                  rmlen++;
                  ptr++;
               }
               if( ifou >= 0 && *ptr == '>' )
               {
                  ptr -= rmlen;
                  ptr++;
                  if( exptype == '0' && *( ptr - 1 ) == '#' && *( ptr - 2 ) != '\\' )
                  {
                     exptype = '1';     /* dumb stringify result marker */
                     ptr--;
                     rmlen++;
                  }
                  expreal[ 2 ]  = exptype;
                  hb_pp_Stuff( expreal, ptr, 4, rmlen, rlen + ( rpatt - ptr ) );
                  rlen         += 4 - rmlen;
               }
               else
                  ptr = ptrtmp;
            }
            else
               ptr = ptrtmp;
         }
      }
      i++;
   }
}

static COMMANDS * AddCommand( char * cmdname )
{
   COMMANDS * stcmd;

   HB_TRACE( HB_TR_DEBUG, ( "AddCommand(%s)", cmdname ) );

   stcmd            = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
   stcmd->last      = hb_pp_topCommand;
   hb_pp_topCommand = stcmd;
   stcmd->name      = hb_strdup( cmdname );
   stcmd->namelen   = strlen( cmdname );
   s_kolAddComs++;
   return stcmd;
}

static COMMANDS * AddTranslate( char * traname )
{
   COMMANDS * sttra;

   HB_TRACE( HB_TR_DEBUG, ( "AddTranslate(%s)", traname ) );

   sttra               = ( COMMANDS * ) hb_xgrab( sizeof( COMMANDS ) );
   sttra->last         = hb_pp_topTranslate;
   hb_pp_topTranslate  = sttra;
   sttra->name         = hb_strdup( traname );
   sttra->namelen      = strlen( traname );
   s_kolAddTras++;
   return sttra;
}

int hb_pp_ParseExpression( char * sLine, char * sOutLine, HB_BOOL bSplitLines )
{
#if ! defined( HB_PP_DEBUG_MEMORY )
   static char    rpatt[ PATTERN_SIZE ];
#else
   char *         rpatt = ( char * ) hb_xgrab( PATTERN_SIZE );
#endif
   char           sToken[ MAX_NAME ];
   char *         ptri, * ptro, * ptrb;
   int            lenToken, i, ipos, isdvig, lens;
   int            ifou;
   int            rezDef, rezTra, rezCom;
   unsigned int   kolpass = 0;
   DEFINES *      stdef;
   COMMANDS *     stcmd;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_ParseExpression(%s, %s)", sLine, sOutLine ) );

   do
   {
      strotrim( sLine, HB_FALSE );

      rezDef  = 0;
      rezTra  = 0;
      rezCom  = 0;
      isdvig  = 0;

      do
      {
         ptro = sOutLine;
         ptri = sLine + isdvig;
         if( bSplitLines )
            ipos = md_strAt( ";", 1, ptri, HB_TRUE, HB_FALSE, HB_FALSE, MD_STR_AT_IGNORECASE );
         else
            ipos = 0;

         if( ipos > 0 )
         {
            *( ptri + ipos - 1 ) = '\0';
         }

         HB_SKIPTABSPACES( ptri );

         if( *ptri == '#' )
         {
            int bIgnore;

            hb_strncpy( rpatt, ptri, PATTERN_SIZE - 1 );
            bIgnore = hb_pp_ParseDirective_( rpatt );

            if( ipos > 0 )
            {
               ipos--;
               *( sLine + isdvig + ipos - 1 )  = ';';
               *( sLine + isdvig + ipos )      = ' ';
            }

            lens = strlen( sLine + isdvig );
            if( bIgnore )
               hb_pp_Stuff( " ", sLine + isdvig, 0, ( ipos ) ? ipos : lens, lens );
            else
               hb_pp_Stuff( rpatt, sLine + isdvig, strlen( rpatt ), ( ipos ) ? ipos : lens, lens );

            if( ipos > 0 )
            {
               ipos = 1;
            }
         }
         else                   /* Look for macros from #define      */
         {
            while( ( lenToken = NextName( &ptri, sToken ) ) > 0 )
            {
#if 0
               printf( "Token: >%s< Line: >%s<\n", sToken, sLine );
#endif

               if( ( stdef = DefSearch( sToken, lenToken, NULL ) ) != NULL )
               {
                  ptrb = ptri - lenToken;

                  if( ( i = WorkDefine( &ptri, ptro, stdef ) ) >= 0 )
                  {
                     rezDef++;
                     lens = strlen( ptrb );

                     if( ipos > 0 )
                     {
                        *( ptrb + lens ) = ';';
                        lens            += strlen( ptrb + lens + 1 );
                     }

                     hb_pp_Stuff( ptro, ptrb, i, ptri - ptrb, lens + 1 );
                     if( ipos > 0 )
                     {
                        ipos                           += i - ( ptri - ptrb );
                        *( sLine + isdvig + ipos - 1 )  = '\0';
                     }
                     ptri += i - ( ptri - ptrb );
                  }
               }
            }

            if( rezDef == 0 )
            {
               /* Look for definitions from #translate    */
               stcmd = hb_pp_topTranslate;
               while( stcmd != NULL )
               {
                  ptri       = sLine + isdvig;
                  lenToken   = stcmd->namelen;

                  while( ( ifou = md_strAt( stcmd->name, lenToken, ptri, HB_TRUE, HB_FALSE, HB_FALSE, MD_STR_AT_USESUBCASE ) ) > 0 )
                  {
                     ptri += ifou - 1;

                     if( ( i = WorkTranslate( ptri + lenToken, ptro, stcmd, &lens ) ) >= 0 )
                     {
                        lens += lenToken;
                        while( lens > 0 && ( *( ptri + lens - 1 ) == ' ' || *( ptri + lens - 1 ) == '\t' ) )
                        {
                           lens--;
                        }

                        if( ipos > 0 )
                        {
                           *( sLine + isdvig + ipos - 1 ) = ';';
                        }

                        hb_pp_Stuff( ptro, ptri, i, lens, strlen( ptri ) );
                        rezTra = 1;

                        if( ipos > 0 )
                        {
                           ipos                           += i - lens;
                           *( sLine + isdvig + ipos - 1 )  = '\0';
                        }

                        ptri += i;
                     }
                     else
                     {
                        ptri += lenToken;
                     }
                  }

                  stcmd = stcmd->last;
               }
            }                   /* rezDef == 0 */

            /* Look for definitions from #command      */
            /* JFL ! Was 3 but insufficient in most cases */
            /* I know this is a new hardcoded limit ... any better idea's welcome */
            if( rezDef == 0 && rezTra == 0 && kolpass < 20 )
            {
               ptri = sLine + isdvig;
               HB_SKIPTABSPACES( ptri );

               if( ISNAME( ( BYTE ) *ptri ) )
               {
                  NextName( &ptri, sToken );
               }
               else
               {
                  /* Ron Pinkas commented 2000-01-24
                     i = 0;
                     while( *ptri != ' ' && *ptri != '\t' && *ptri != '\0' && *ptri != '\"' && *ptri != '\'' && *ptri != '('  && !ISNAME( ( BYTE ) *ptri ) )
                     {
                     *(sToken+i) = *ptri++;
                     i++;
                     }
                     *(sToken+i) = '\0';
                   */

                  /* Ron Pinkas added 2000-01-24 */
                  if( IS_2CHAR_OPERATOR( ptri ) )
                  {
                     sToken[ 0 ]   = *ptri++;
                     sToken[ 1 ]   = *ptri++;
                     sToken[ 2 ]   = '\0';
                  }
                  else
                  {
                     sToken[ 0 ]   = *ptri++;
                     sToken[ 1 ]   = '\0';
                  }
                  /* END, Ron Pinkas added 2000-01-24 */
               }

               HB_SKIPTABSPACES( ptri );

               if( ( *ptri == '\0'
                     || ( *ptri != '='
                          && ( ! IsInStr( *ptri, ":/+*-%^" )
                               || *( ptri + 1 ) != '=' ) && ( *ptri != '-'
                                                              || *( ptri + 1 ) != '>' ) ) ) && ( stcmd = ComSearch( sToken, NULL ) ) != NULL )
               {
                  ptro = sOutLine;

                  i    = WorkCommand( ptri, ptro, stcmd );
                  ptri = sLine + isdvig;

                  if( ipos > 0 )
                  {
                     *( ptri + ipos - 1 ) = ';';
                  }

                  if( i >= 0 )
                  {
                     if( isdvig + ipos > 0 )
                     {
                        lens = strlen( sLine + isdvig );
                        hb_pp_Stuff( ptro, sLine + isdvig, i, ( ipos ) ? ipos - 1 : lens, lens );

                        if( ipos > 0 )
                        {
                           ipos = i + 1;
                        }
                     }
                     else
                     {
                        memcpy( sLine, sOutLine, i + 1 );
                     }
                  }

                  rezCom = 1;
               }
               else if( ipos > 0 )
               {
                  *( sLine + isdvig + ipos - 1 ) = ';';
               }
            }
            else if( ipos > 0 )
            {
               *( sLine + isdvig + ipos - 1 ) = ';';
            }
         }

         isdvig += ipos;
      }
      while( ipos != 0 );

      kolpass++;

      if( kolpass > hb_pp_MaxTranslateCycles && ( rezDef || rezTra || rezCom ) )
      {
         hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_RECURSE, NULL, NULL );
         break;
      }
   }
   while( rezDef || rezTra || rezCom );

   RemoveOptional( sLine );
   if( *sOutLine )
   {
      RemoveOptional( sOutLine );
   }

#if defined( HB_PP_DEBUG_MEMORY )
   hb_xfree( rpatt );
#endif
   return 0;
}

static int WorkDefine( char ** ptri, char * ptro, DEFINES * stdef )
{
   int      npars, lens;
   char *   ptr;

   HB_TRACE( HB_TR_DEBUG, ( "WorkDefine(%p, %s, %p)", ptri, ptro, stdef ) );

   if( stdef->npars < 0 )
   {
      lens = hb_pp_strocpy( ptro, stdef->value );
   }
   else
   {
      HB_SKIPTABSPACES( *ptri );

      if( **ptri == '(' )
      {
         npars   = 0;
         ptr     = *ptri;

         do
         {
            ptr++;

            if( NextParm( &ptr, NULL ) > 0 )
            {
               npars++;
            }
         }
         while( *ptr != ')' && *ptr != '\0' );

         if( *ptr == ')' && stdef->npars == npars )
         {
            /* Ron Pinkas added 2000-11-21 */
            char * pTmp = ptr + 1;

            while( *pTmp && ( *pTmp == ' ' || *pTmp == '\t' ) )
            {
               pTmp++;
            }
            if( *pTmp == '[' )
            {
               s_bArray = HB_TRUE;
            }
            /* END - Ron Pinkas added 2000-11-21 */

            lens = WorkPseudoF( ptri, ptro, stdef );
         }
         else
         {
            return -1;
         }
      }
      else
      {
         return -1;
      }
   }

   return lens;
}

static int WorkPseudoF( char ** ptri, char * ptro, DEFINES * stdef )
{
   char     parfict[ MAX_NAME ], * ptrreal;
   char *   ptrb;
   int      ipos, ifou, ibeg;
   int      lenfict, lenreal, lenres;

   HB_TRACE( HB_TR_DEBUG, ( "WorkPseudoF(%p, %s, %p)", ptri, ptro, stdef ) );

   lenres = hb_pp_strocpy( ptro, stdef->value );        /* Copying value of macro to destination string  */

   if( stdef->pars )
   {
      ipos = 0;
      ibeg = 0;

      for(;; )                  /* Parsing through parameters */
      {                         /* in macro definition        */
         if( *( stdef->pars + ipos ) == ',' || *( stdef->pars + ipos ) == '\0' )
         {
            *( parfict + ipos - ibeg )   = '\0';
            lenfict                      = ipos - ibeg;

            if( **ptri != ')' )
            {
               ( *ptri )++;     /* Get next real parameter */
               HB_SKIPTABSPACES( *ptri );
               ptrreal = *ptri;
               lenreal = NextParm( ptri, NULL );

               ptrb    = ptro;
               while( ( ifou = hb_strAt( parfict, lenfict, ptrb, lenres - ( ptrb - ptro ) ) ) > 0 )
               {
                  ptrb = ptrb + ifou - 1;
                  if( ! ISNAME( ( BYTE ) *( ptrb - 1 ) ) && ! ISNAME( ( BYTE ) *( ptrb + lenfict ) ) )
                  {
                     hb_pp_Stuff( ptrreal, ptrb, lenreal, lenfict, lenres + ( ptro - ptrb ) );
                     lenres += lenreal - lenfict;
                     ptrb   += lenreal;
                  }
                  else
                  {
                     ptrb++;
                  }
               }

               ibeg = ipos + 1;
            }
         }
         else
         {
            *( parfict + ipos - ibeg ) = *( stdef->pars + ipos );
         }

         if( *( stdef->pars + ipos ) == '\0' )
         {
            break;
         }

         ipos++;
      }
   }
   else
   {
      while( **ptri != ')' )
      {
         ( *ptri )++;
      }
   }

   ( *ptri )++;

   return lenres;
}

static int WorkCommand( char * ptri, char * ptro, COMMANDS * stcmd )
{
   int      rez;
   int      lenres;
   char *   ptrmp;
   char *   sToken = stcmd->name;

   HB_TRACE( HB_TR_DEBUG, ( "WorkCommand(%s, %s, %p)", ptri, ptro, stcmd ) );

   do
   {
      lenres        = hb_pp_strocpy( ptro, stcmd->value );  /* Copying result pattern */
      ptrmp         = stcmd->mpatt;                         /* Pointer to a match pattern */
      s_Repeate     = 0;
      s_groupchar   = '@';
      rez           = CommandStuff( ptrmp, ptri, ptro, &lenres, HB_TRUE, stcmd->com_or_xcom );

      stcmd         = stcmd->last;
      if( rez < 0 && stcmd != NULL )
         stcmd = ComSearch( sToken, stcmd );
   }
   while( rez < 0 && stcmd != NULL );

   *( ptro + lenres ) = '\0';
   if( rez >= 0 )
   {
      return lenres;
   }
   return -1;
}

static int WorkTranslate( char * ptri, char * ptro, COMMANDS * sttra, int * lens )
{
   int      rez;
   int      lenres;
   char *   ptrmp;
   char *   sToken = sttra->name;

   HB_TRACE( HB_TR_DEBUG, ( "WorkTranslate(%s, %s, %p, %p)", ptri, ptro, sttra, lens ) );

   do
   {
      lenres        = hb_pp_strocpy( ptro, sttra->value );
      ptrmp         = sttra->mpatt;
      s_Repeate     = 0;
      s_groupchar   = '@';
      rez           = CommandStuff( ptrmp, ptri, ptro, &lenres, HB_FALSE, sttra->com_or_xcom );

      sttra         = sttra->last;

      if( rez < 0 && sttra != NULL )
      {
         sttra = TraSearch( sToken, sttra );
      }
   }
   while( rez < 0 && sttra != NULL );

   *( ptro + lenres ) = '\0';

   if( rez >= 0 )
   {
      *lens = rez;
      return lenres;
   }

   return -1;
}

#define MAX_OPTIONALS 64

static int CommandStuff( char * ptrmp, char * inputLine, char * ptro, int * lenres, HB_BOOL com_or_tra, HB_BOOL com_or_xcom )
{
   HB_BOOL  endTranslation   = HB_FALSE;
   int      ipos;
   char *   lastopti[ MAX_OPTIONALS ], * strtopti = NULL, * strtptri = NULL;
   char *   ptri             = inputLine, * ptr, tmpname[ MAX_NAME ];
   int      isWordInside     = 0;
   char     szMatch[ 2 ];
   char *   cSkipped[ MAX_OPTIONALS ];
   int      iSkipped         = 0;

   /*
      printf( "MP: >%s<\nIn: >%s<\n", ptrmp, ptri );
    */

   HB_TRACE( HB_TR_DEBUG, ( "CommandStuff(%s, %s, %s, %p, %d, %d)", ptrmp, inputLine, ptro, lenres, com_or_tra, com_or_xcom ) );

   s_numBrackets = 0;
   HB_SKIPTABSPACES( ptri );
   if( ptrmp == NULL )
   {
      if( *ptri != '\0' )
         return -1;
   }
   else
   {
      while( *ptri != '\0' && ! endTranslation )
      {
         HB_SKIPTABSPACES( ptrmp );
         if( *ptrmp == HB_PP_OPT_START && ! s_numBrackets && ! strtopti )
         {
            /* Store start position of outermost optional in pattern */
            strtopti = ptrmp;
         }
         if( ! s_numBrackets && strtopti && strtptri != ptri &&
             ( ISNAME( ( BYTE ) *ptri ) || *ptri == '&' ) )
         {
            /* Input stream starts with a word or macro -store the position
             * which matches the outermost optional in pattern
             */
            strtptri   = ptri;
            ptrmp      = strtopti;
            ptr        = ptri;
            ipos       = NextName( &ptr, tmpname ); /* get starting keyword */
            ipos       = md_strAt( tmpname, ipos, strtopti, HB_TRUE, HB_TRUE, HB_TRUE, MD_STR_AT_USESUBCASE );
            if( ipos && TestOptional( strtopti, strtopti + ipos - 2 ) )
            {
               /* the keyword from input is found in the pattern */
               ptr = PrevSquare( strtopti + ipos - 2, strtopti, NULL );
               if( ptr )
                  ptrmp = ptr;
               if( ptr != strtopti )
               {
                  cSkipped[ iSkipped++ ] = strtopti;
               }
            }
         }

         switch( *ptrmp )
         {
            case HB_PP_OPT_START:
               if( ! s_numBrackets )
                  isWordInside = 0;
               s_numBrackets++;
               s_aIsRepeate[ s_Repeate ] = 0;
               lastopti[ s_Repeate++ ]   = ptrmp;
               ptrmp++;
               if( ! CheckOptional( ptrmp, ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
               {
                  SkipOptional( &ptrmp );
               }
               break;

            case HB_PP_OPT_END:
               if( s_Repeate )
               {
                  s_Repeate--;
                  if( s_aIsRepeate[ s_Repeate ] )
                  {
                     if( ISNAME( ( BYTE ) *ptri ) )
                     {
                        ptr  = ptri;
                        ipos = NextName( &ptr, tmpname );
                        ipos = md_strAt( tmpname, ipos, ptrmp, HB_TRUE, HB_TRUE, HB_TRUE, MD_STR_AT_USESUBCASE );
                        if( ipos && TestOptional( ptrmp + 1, ptrmp + ipos - 2 ) )
                        {
                           ptr = PrevSquare( ptrmp + ipos - 2, ptrmp + 1, NULL );
                           if( ! ptr || CheckOptional( ptrmp + 1, ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
                           {
                              ptrmp = lastopti[ s_Repeate ];
                              ptrmp++;
                              s_Repeate++;
                              SkipOptional( &ptrmp );
                              s_numBrackets++;
                              ptrmp++;
                              strtptri = ptri;
                           }
                           else
                              ptrmp = lastopti[ s_Repeate ];
                        }
                        else
                        {
                           ptrmp = lastopti[ s_Repeate ];
                        }
                     }
                     else
                     {
                        ptrmp = lastopti[ s_Repeate ];
                     }
                  }
                  else
                  {
                     if( ! isWordInside )
                        strtopti = NULL;
                     ptrmp++;
                  }
                  s_numBrackets--;
               }
               else
               {
                  if( ! isWordInside )
                     strtopti = NULL;
                  s_numBrackets--;
                  ptrmp++;
               }
               break;

            case ',':
               if( s_numBrackets == 1 )
                  isWordInside = 1;
               if( ! s_numBrackets )
                  strtopti = NULL;
               if( *ptri == ',' )
               {
                  ptrmp++;
                  ptri++;
               }
               else
               {
                  if( s_numBrackets )
                  {
                     SkipOptional( &ptrmp );
                  }
                  else
                     return -1;
               }
               break;

            case HB_PP_MATCH_MARK:     /*  Match marker */
               if( ! s_numBrackets )
                  strtopti = NULL;
               if( s_numBrackets == 1 && *( ptrmp + 2 ) == '2' )
                  isWordInside = 1;     /*  restricted match marker  */
               if( ! WorkMarkers( &ptrmp, &ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
               {
                  if( s_numBrackets )
                  {
                     SkipOptional( &ptrmp );
                  }
                  else
                     return -1;
               }
               break;

            case '\0':
               if( iSkipped )
               {
                  ptrmp = cSkipped[ --iSkipped ];
                  break;
               }
               if( com_or_tra )
                  return -1;
               else
                  endTranslation = HB_TRUE;
               break;

            default:           /*   Key word    */
               if( s_numBrackets == 1 )
                  isWordInside = 1;
               if( ! s_numBrackets )
                  strtopti = NULL;
               ptr = ptri;
               if( *ptri == ',' || truncmp( &ptri, &ptrmp, ! com_or_xcom ) )
               {
                  ptri = ptr;
                  if( s_numBrackets )
                  {
                     SkipOptional( &ptrmp );
                  }
                  else
                     return -1;
               }
         }
         HB_SKIPTABSPACES( ptri );
      }
      ;
   }

   if( *ptrmp != '\0' )
   {
      if( s_Repeate )
      {
         s_Repeate  = 0;
         ptrmp      = lastopti[ 0 ];
      }
      s_numBrackets = 0;
      do
      {
         HB_SKIPTABSPACES( ptrmp );
         if( *ptrmp != '\0' )
            switch( *ptrmp )
            {
               case HB_PP_OPT_START:
                  ptrmp++;
                  SkipOptional( &ptrmp );
                  ptrmp++;
                  break;
               case HB_PP_OPT_END:
                  ptrmp++;
                  break;
               default:
                  return -1;
            }
      }
      while( *ptrmp != '\0' );
   }

   szMatch[ 0 ]        = HB_PP_MATCH_MARK;
   szMatch[ 1 ]        = '\0';
   SearnRep( szMatch, "", 0, ptro, lenres );
   *( ptro + *lenres ) = '\0';

   if( com_or_tra )
      return 1;
   else
      return ptri - inputLine;
}

static int RemoveSlash( char * cpatt )
{
   int   i       = 0;
   int   lenres  = strlen( cpatt );

   while( cpatt[ i ] != '\0' )
   {
      if( cpatt[ i ] == '"' || cpatt[ i ] == '\'' )
      {
         char c = cpatt[ i ];

         i++;
         while( cpatt[ i ] && cpatt[ i ] != c )
         {
            i++;
         }
         i++;
         continue;              /* skip "strings" */
      }
      if( cpatt[ i ] == '[' )
      {
         i++;
         while( cpatt[ i ] && cpatt[ i ] != ']' )
         {
            i++;
         }
         i++;
         continue;              /* skip [strings] */
      }

      if( cpatt[ i ] == '\\' )
      {
         hb_pp_Stuff( "", cpatt + i, 0, 1, lenres - i + 1 );
         lenres--;
         i++;
      }
      else
         i++;
   }
   return lenres;
}

static int WorkMarkers( char ** ptrmp, char ** ptri, char * ptro, int * lenres, HB_BOOL com_or_tra, HB_BOOL com_or_xcom )
{
#if ! defined( HB_PP_DEBUG_MEMORY )
   char     exppatt[ MAX_NAME ];
#else
   char *   exppatt = ( char * ) hb_xgrab( MAX_NAME );
#endif
   int      lenreal = 0, maxlenreal, lenpatt;
   int      rezrestr, ipos, nBra;
   char *   ptr, * ptrtemp;

   HB_TRACE( HB_TR_DEBUG, ( "WorkMarkers(%p, %p, %s, %p)", ptrmp, ptri, ptro, lenres ) );


   maxlenreal = HB_PP_STR_SIZE;
   if( s_expreal == NULL )
      s_expreal = ( char * ) hb_xgrab( maxlenreal + 1 );

   /* Copying a match pattern to 'exppatt' */
   lenpatt = stroncpy( exppatt, *ptrmp, 4 );
   *ptrmp += 4;

   HB_SKIPTABSPACES( *ptrmp );

   /* JFL removed 12/11/2001 to allow param like (,,3) as allowed by clipper */
   /*
      if( **ptri == ',' )
      {
      if( s_numBrackets )
      {
      return 0;
      }
      } */

   ptrtemp = *ptrmp;

   if( *( exppatt + 2 ) != '2' && *ptrtemp == HB_PP_OPT_END )
   {
      ptrtemp++;
      HB_SKIPTABSPACES( ptrtemp );

      while( *ptrtemp == HB_PP_OPT_START )
      {
         nBra = 0;
         ptrtemp++;

         while( ( *ptrtemp != HB_PP_OPT_END || nBra ) && *ptrtemp != '\0' )
         {
            if( *ptrtemp == HB_PP_OPT_START )
            {
               nBra++;
            }
            else if( *ptrtemp == HB_PP_OPT_END )
            {
               nBra--;
            }
            ptrtemp++;
         }
         ptrtemp++;

         HB_SKIPTABSPACES( ptrtemp );
      }
   }

   if( *( exppatt + 2 ) != '2' && *ptrtemp != HB_PP_MATCH_MARK
       && *ptrtemp != ',' && *ptrtemp != HB_PP_OPT_START && *ptrtemp != HB_PP_OPT_END && *ptrtemp != '\0' )
   {
      lenreal = strincpy( s_expreal, ptrtemp );

      if( ( ipos = md_strAt( s_expreal, lenreal, *ptri, HB_TRUE, HB_TRUE, HB_FALSE, MD_STR_AT_USESUBCASE ) ) > 0 )
      {
         if( ptrtemp > *ptrmp )
         {
            if( ipos == 1 )
            {
               if( s_numBrackets )
               {
#if defined( HB_PP_DEBUG_MEMORY )
                  hb_xfree( exppatt );
#endif
                  return 0;
               }
            }
            else
            {
               maxlenreal = ipos;
               lenreal    = 0;
            }
         }
         else
         {
            /*
               printf( "\nFound: '%s' Len: %i In: '%s' At: %i \n", s_expreal, lenreal, *ptri, ipos );
             */

            lenreal = stroncpy( s_expreal, *ptri, ipos - 1 );

            if( ipos > 1 )
            {
               if( *( exppatt + 2 ) == '5' )    /*  ----  Minimal match marker  */
               {
                  if( IsIdentifier( s_expreal ) )
                  {
                     *ptri += lenreal;
                  }
               }
               else if( isExpres( s_expreal, *( exppatt + 2 ) == '1' ) )
               {
                  *ptri += lenreal;
               }
            }
            else
            {
               if( s_numBrackets )
               {
#if defined( HB_PP_DEBUG_MEMORY )
                  hb_xfree( exppatt );
#endif
                  return 0;
               }
               else
               {
                  lenreal = 0;
               }
            }
         }
      }
      else
      {
         if( s_numBrackets )
         {
#if defined( HB_PP_DEBUG_MEMORY )
            hb_xfree( exppatt );
#endif
            return 0;
         }
         else
         {
            lenreal = 0;
         }
      }
   }

   if( *( exppatt + 2 ) == '4' )        /*  ----  extended match marker  */
   {
      if( ! lenreal )
         lenreal = getExpReal( s_expreal, ptri, HB_FALSE, maxlenreal, HB_FALSE, HB_FALSE );
      {
         SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
      }
   }
   else if( *( exppatt + 2 ) == '3' )   /*  ----  wild match marker  */
   {
      lenreal = hb_pp_strocpy( s_expreal, *ptri );
      *ptri  += lenreal;
      SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
   }
   else if( *( exppatt + 2 ) == '2' )   /*  ---- restricted match marker  */
   {
      while( **ptrmp != '>' )
      {
         *( exppatt + lenpatt++ ) = *( ( *ptrmp )++ );
      }
      *( exppatt + lenpatt ) = '\0';
      ( *ptrmp )++;

      ptr                    = exppatt + 4;
      rezrestr               = 0;
      while( *ptr != '\0' )
      {
         if( *ptr == '&' )
         {
            /* rglab: Thu Sep  2 21:30:07 2004
             * Special Clipper undocumented restricted match marker:
             * <x:&>
             * Clipper accepts the macro variable only here.
             * eg.
             * SET FILTER TO &var.
             * SET FILTER TO &var.foo
             * SET FILTER TO &(var)
             *
             * Notice that any expression that starts from the macro
             * variable is not valid for this marker
             * eg.
             * SET FILTER TO &var+1
             *  the above command is preprocessed by a general rule
             * that is using smart match marker <(x)>
             */
            if( **ptri == '&' )
            {
               char * ptrmacro = *ptri; /* save current position */

               *ptri += 1;
               HB_SKIPTABSPACES( *ptri );
               if( **ptri == '(' )      /* macro expression &( expr )  */
               {
                  lenreal                   = getExpReal( s_expreal + 2, ptri, HB_TRUE, maxlenreal, HB_FALSE, HB_TRUE );
                  if( ! ( **ptri == '\0' || **ptri == ' ' ) && com_or_tra )
                     break;
                  s_expreal[ 0 ]            = '&';
                  s_expreal[ 1 ]            = '(';
                  lenreal                  += 3;
                  s_expreal[ lenreal - 1 ]  = ')';
                  SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
                  rezrestr                  = 1;
               }
               else
               {
                  lenreal = IsMacroVar( *ptri, com_or_tra );
                  if( lenreal > 0 )
                  {
                     if( ! com_or_tra )
                     {
                        /* translate */
                        hb_strncpy( s_expreal + 1, *ptri, HB_PP_STR_SIZE - 1 );
                        s_expreal[ 0 ]            = '&';
                        s_expreal[ lenreal + 1 ]  = '\0';
                        *ptri                    += lenreal;
                        SearnRep( exppatt, s_expreal, lenreal + 1, ptro, lenres );
                        rezrestr                  = 1;
                        break;
                     }
                     else
                     {
                        char * ptmp = *ptri + lenreal;
                        HB_SKIPTABSPACES( ptmp );
                        if( ! IsInStr( *ptmp, ":/+*-%^=<>[{.," ) ||
                            ( *ptmp && ptmp[ 0 ] == '+' && ptmp[ 1 ] == '+' ) ||
                            ( *ptmp && ptmp[ 0 ] == '-' && ptmp[ 1 ] == '-' ) )
                        {
                           /* NOTE:
                            * Clipper usually bounds to left the '++' and '--'
                            * operators which means that the following code:
                            * #command XCALL <x> <y> => <x>( <y> )
                            *  XCALL &a ++b
                            * is preprocessed into:
                            * &a( ++b )
                            * However if used with restricted macro match marker:
                            * #command MCALL <x:&> <y> => <x>( <y> )
                            * then
                            *  MCALL &a ++b
                            * is preprocessed into:
                            * &a ++( b )
                            */
                           hb_strncpy( s_expreal + 1, *ptri, HB_PP_STR_SIZE - 1 );
                           s_expreal[ 0 ]            = '&';
                           s_expreal[ lenreal + 1 ]  = '\0';
                           *ptri                    += lenreal;
                           SearnRep( exppatt, s_expreal, lenreal + 1, ptro, lenres );
                           rezrestr                  = 1;
                           break;
                        }
                        else
                        {
#if defined( HB_PP_DEBUG_MEMORY )
                           hb_xfree( exppatt );
#endif
                           *ptri = ptrmacro; /* restore '&' char */
                           return 0;
                        }
                     }
                  }
                  else
                  {
#if defined( HB_PP_DEBUG_MEMORY )
                     hb_xfree( exppatt );
#endif
                     *ptri -= 1; /* restore '&' char */
                     return 0;
                  }
               }
               break;
            }
            else
            {
               ptr++;
            }
         }
         else
         {
            HB_SKIPTABSPACES( ptr );
            /* Comparing real parameter and restriction value */
            ptrtemp = ptr;
            if( ! strincmp( *ptri, &ptr, ! com_or_xcom ) )
            {
               lenreal    = stroncpy( s_expreal, *ptri, ( ptr - ptrtemp ) );
               *ptri     += lenreal;
               SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
               rezrestr   = 1;
               break;
            }
            else
            {
               while( *ptr != ',' && *ptr != '\0' )
               {
                  ptr++;
               }
               if( *ptr == ',' )
               {
                  ptr++;
               }
            }
         }
      }
      if( rezrestr == 0 )
      {
         /* If restricted match marker doesn't correspond to real parameter */
#if defined( HB_PP_DEBUG_MEMORY )
         hb_xfree( exppatt );
#endif
         return 0;
      }
   }
   else if( *( exppatt + 2 ) == '1' )   /*  ---- list match marker  */
   {
      if( ! lenreal )
      {
         lenreal = getExpReal( s_expreal, ptri, HB_TRUE, maxlenreal, HB_FALSE, HB_FALSE );
      }

      if( lenreal )
      {
         SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
      }
      else
      {
#if defined( HB_PP_DEBUG_MEMORY )
         hb_xfree( exppatt );
#endif
         return 0;
      }
   }
   else                         /*  ---- regular match marker  */
   {
      /* Copying a real expression to 's_expreal' */
      if( ! lenreal )
      {
         lenreal = getExpReal( s_expreal, ptri, HB_FALSE, maxlenreal, HB_FALSE, HB_FALSE );
      }

      /*
         printf("Len: %i Pat: %s Exp: %s\n", lenreal, exppatt, s_expreal );
       */

      if( lenreal )
      {
         SearnRep( exppatt, s_expreal, lenreal, ptro, lenres );
      }
      else
      {
#if defined( HB_PP_DEBUG_MEMORY )
         hb_xfree( exppatt );
#endif
         return 0;
      }
   }

#if defined( HB_PP_DEBUG_MEMORY )
   hb_xfree( exppatt );
#endif
   return 1;
}

static int getExpReal( char * expreal, char ** ptri, HB_BOOL prlist, int maxrez, HB_BOOL bStrict, HB_BOOL bInBrackets )
{
   int      lens       = 0;
   char *   sZnaki     = "+-=><*/$.:#%!^";
   int      State;
   int      StBr1      = 0, StBr2 = 0, StBr3 = 0;
   HB_BOOL  rez        = HB_FALSE;
   HB_BOOL  bMacro     = HB_FALSE;
   HB_BOOL  bBrackets  = HB_FALSE;
   char *   cStart     = expreal;
   char     cLastSep   = '\0';
   char     cLastChar  = '\0';

   HB_TRACE( HB_TR_DEBUG, ( "getExpReal(%s, %p, %d, %d, %d, %d)", expreal, ptri, prlist, maxrez, bStrict, bInBrackets ) );

   HB_SKIPTABSPACES( *ptri );

   if( **ptri == '(' && bInBrackets )
   {
      /* scan expression including start and end brackets */
      bBrackets  = HB_TRUE;
      ( *ptri )++;
      prlist     = HB_TRUE;
   }
   State = ( **ptri == '\'' || **ptri == '\"' || **ptri == '[' ) ? STATE_EXPRES : STATE_ID;

   while( **ptri != '\0' && ! rez && lens < maxrez )
   {
      if( State == STATE_EXPRES || ( cLastChar && strchr( "({[.|,$!#=<>^%*/+-", cLastChar ) ) )
      {
         /* Ron Pinkas added if on State 2001-05-02 to avoid
            multiple strings concatination.
          */
         if( **ptri == '"' )
         {
            if( expreal != NULL )
            {
               *expreal++ = **ptri;
            }

            ( *ptri )++;
            lens++;

            while( **ptri != '\0' && lens < maxrez )
            {
               if( expreal != NULL )
               {
                  *expreal++ = **ptri;
               }

               if( **ptri == '"' )
               {
                  break;
               }

               ( *ptri )++;
               lens++;
            }

            ( *ptri )++;
            lens++;

            cLastChar  = '"';
            State      = ( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 ) ? STATE_ID_END : STATE_BRACKET;
            continue;
         }
         else if( **ptri == '\'' )
         {
            char * pString;

            if( expreal != NULL )
            {
               *expreal++ = **ptri;
            }

            ( *ptri )++;
            lens++;

            pString = expreal;

            while( **ptri != '\0' && lens < maxrez )
            {
               if( expreal != NULL )
               {
                  *expreal++ = **ptri;
               }

               if( **ptri == '\'' )
               {
                  break;
               }

               ( *ptri )++;
               lens++;
            }

            if( expreal != NULL )
            {
               *( expreal - 1 ) = '\0';
               if( strchr( pString, '"' ) == NULL )
               {
                  *( pString - 1 ) = '"';
                  *( expreal - 1 ) = '"';
               }
               else
               {
                  *( expreal - 1 ) = '\'';
               }
            }

            ( *ptri )++;
            lens++;

            cLastChar  = '\'';
            State      = ( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 ) ? STATE_ID_END : STATE_BRACKET;
            continue;
         }
         else if( **ptri == '[' )
         {
            /* ( see below 5-2-2001
               && ( State == STATE_EXPRES || ( strchr( ")]}.", cLastChar ) == NULL
               && ! ISNAME( ( BYTE ) cLastChar ) ) )
             */
            char * pString;

            if( expreal != NULL )
            {
               *expreal++ = **ptri;
            }

            ( *ptri )++;
            lens++;

            pString = expreal;

            while( **ptri != '\0' && lens < maxrez )
            {
               if( expreal != NULL )
               {
                  *expreal++ = **ptri;
               }

               if( **ptri == ']' )
               {
                  break;
               }

               ( *ptri )++;
               lens++;
            }

            if( expreal != NULL )
            {
               *( expreal - 1 ) = '\0';
               if( strchr( pString, '"' ) == NULL )
               {
                  *( pString - 1 ) = '"';
                  *( expreal - 1 ) = '"';
               }
               else if( strchr( pString, '\'' ) == NULL )
               {
                  *( pString - 1 ) = '\'';
                  *( expreal - 1 ) = '\'';
               }
               else
               {
                  *( expreal - 1 ) = ']';
               }
            }

            ( *ptri )++;
            lens++;

            cLastChar  = ']';
            State      = ( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 ) ? STATE_ID_END : STATE_BRACKET;
            continue;
         }
         /* Added by Ron Pinkas 2001-05-02
            ( removed lots of related scattered logic below!
          */
      }
      else if( strchr( "'\"", **ptri ) )
      {
         /* New String, can't belong to extracted expression. */
         break;
      }
      else if( **ptri == '[' && ( strchr( ")]}.", cLastChar ) == NULL && ! ISNAME( ( BYTE ) cLastChar ) ) )     /* New String, can't belong to extracted expression. */
      {
         break;
      }
      /* End - END - Added by Ron Pinkas 2000-11-05 */

#if 0
      printf( "State: %i Char:%c\n", State, **ptri );
#endif

      switch( State )
      {
         case STATE_BRACKET:
            if( **ptri == '(' )
            {
               StBr1++;
            }
            else if( **ptri == '[' )
            {
               StBr2++;
            }
            else if( **ptri == '{' )
            {
               StBr3++;
            }
            else if( **ptri == ')' )
            {
               StBr1--;
               if( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 )
               {
                  State = STATE_ID_END;
               }
            }
            else if( **ptri == ']' )
            {
               StBr2--;
               if( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 )
               {
                  State = STATE_ID_END;
               }
            }
            else if( **ptri == '}' )
            {
               StBr3--;
               if( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 )
               {
                  State = STATE_ID_END;
               }
            }

            break;

         case STATE_ID:
         case STATE_ID_END:
            if( ( ( ISNAME( ( BYTE ) **ptri ) || **ptri == '\\' || **ptri == '&' ) && State == STATE_ID_END ) || **ptri == ',' )
            {
               if( **ptri == ',' )
               {
                  if( ! prlist )
                  {
                     rez = HB_TRUE;
                  }
                  else
                  {
                     State = STATE_EXPRES;
                  }
                  cLastSep = ',';
               }
               else
               {
                  rez = HB_TRUE;
               }
            }
            else if( ( **ptri == '+' && *( *ptri + 1 ) == '+' ) || ( **ptri == '-' && *( *ptri + 1 ) == '-' ) )
            {
               cLastChar = **ptri;

               if( expreal )
               {
                  *expreal++ = **ptri;
               }
               ( *ptri )++;
               lens++;

               if( expreal )
               {
                  *expreal++ = **ptri;
               }
               ( *ptri )++;
               lens++;

               if( State == STATE_ID )
               {
                  /* Prefix ONLY when lens == 0 (2) oterwise MUST be a postfix. */
                  if( lens == 2 )
                  {
                     while( **ptri == ' ' && lens < maxrez )
                     {
                        if( expreal )
                        {
                           *expreal++ = **ptri;
                        }
                        ( *ptri )++;
                        lens++;
                     }
                  }
                  else
                  {
                     State = ( StBr1 == 0 && StBr2 == 0 && StBr3 == 0 ) ? STATE_ID_END : STATE_BRACKET;
                  }
               }
               continue;
            }
            else if( IsInStr( **ptri, sZnaki ) )
            {
               cLastSep = ',';
               /* Ron Pinkas added 2000-06-02 */
               if( **ptri == '.' && bMacro )
               {
                  /* Macro terminator '.' */
                  if( *( *ptri + 1 ) == ' ' )
                  {
                     State = STATE_ID_END;
                  }

                  bMacro = HB_FALSE;

                  /* Ron Pinkas added 2000-05-03 */
                  /* Macro terminator is NOT a coninutation char unlike '.' of logical operators, so we don't want it recorded as cLastChar! */
                  if( expreal != NULL )
                  {
                     *expreal++ = **ptri;
                  }
                  ( *ptri )++;
                  lens++;
                  continue;
                  /* END - Ron Pinkas added 2000-05-03 */
               }
               else if( **ptri == '*' && *( *ptri + 1 ) == '*' )
               {
                  /* Clipper replaces ** with ^ operator */
                  if( expreal != NULL )
                  {
                     *expreal++ = '^';
                  }
                  ( *ptri ) += 2;
                  lens++;
                  cLastChar  = '^';
                  State      = STATE_EXPRES;
                  continue;
               }
               else
                  /* Ron Pinkas end 2000-06-02 */
                  State = STATE_EXPRES;
            }
            else if( **ptri == '(' )
            {
               State   = STATE_BRACKET;
               StBr1   = 1;
            }
            else if( **ptri == '[' )
            {
               StBr2++;
               State = STATE_BRACKET;
            }
            else if( **ptri == '{' )
            {
               State   = STATE_BRACKET;
               StBr3   = 1;
            }
            else if( **ptri == ')' && StBr1 == 0 )
            {
               if( bBrackets )
               {
                  ( *ptri )++;
               }
               rez = HB_TRUE;
            }
            else if( **ptri == ']' && StBr2 == 0 )
            {
               rez = HB_TRUE;
            }
            else if( **ptri == '}' && StBr3 == 0 )
            {
               rez = HB_TRUE;
            }
            else if( **ptri == '&' )
            {
               bMacro = HB_TRUE;
            }
            else if( **ptri == ' ' )
            {
               State   = STATE_ID_END;
               bMacro  = HB_FALSE;
            }

            break;

         case STATE_EXPRES:
         case STATE_EXPRES_ID:
            if( **ptri == '[' )
            {
               StBr2++;
               State = STATE_BRACKET;
            }
            else if( ISNAME( ( BYTE ) **ptri ) )
            {
               if( prlist && cLastSep == ' ' )
               {
                  State   = STATE_ID_END;
                  rez     = HB_TRUE;
               }
               else
                  State = STATE_EXPRES_ID;
               cLastSep = **ptri;
            }
            else if( **ptri == ' ' )
            {
               if( ! prlist )
               {
                  if( State == STATE_EXPRES_ID )
                  {
                     State = STATE_ID_END;
                  }
                  else if( lens > 2 && ( ( *( *ptri - 2 ) == '+' && *( *ptri - 1 ) == '+' ) || ( *( *ptri - 2 ) == '-' && *( *ptri - 1 ) == '-' ) ) )
                  {
                     State = STATE_ID_END;
                  }
               }
               if( cLastSep != ',' )
                  cLastSep = ' ';
            }
            /* Ron Pinkas added 2000-06-14 */
            else if( **ptri == ')' && StBr1 == 0 )
            {
               rez = HB_TRUE;
            }
            /* Ron Pinkas end 2000-06-14 */
            else if( **ptri == '(' )
            {
               StBr1++;
               State = STATE_BRACKET;
            }
            else if( **ptri == '{' )
            {
               StBr3++;
               State = STATE_BRACKET;
            }
            else if( **ptri == '}' && StBr3 == 0 )
            {
               rez = HB_TRUE;
            }
            else if( **ptri == ',' )
            {
               if( ! prlist )
               {
                  rez     = HB_TRUE;
                  State   = STATE_EXPRES;
               }
               cLastSep = ',';
            }
            else if( **ptri == '.' && *( *ptri - 2 ) == '.' &&
                     ( *( *ptri - 1 ) == 'T' || *( *ptri - 1 ) == 'F' || *( *ptri - 1 ) == 't' || *( *ptri - 1 ) == 'f' ) )
            {
               State = STATE_ID_END;
            }
            else if( **ptri == '&' )
            {
               State = STATE_ID;
               continue;
            }
            else
            {
               State      = STATE_EXPRES;
               cLastSep   = ',';
            }

            break;
      }

      if( ! rez )
      {
         /* Ron Pinkas added 2000-06-17 */
         if( **ptri != ' ' && **ptri != '\t' )
         {
            cLastChar = **ptri;
         }
         /* Ron Pinkas end 2000-06-17 */

         if( expreal != NULL )
            *expreal++ = **ptri;

         ( *ptri )++;
         lens++;
      }
   }
   if( ! rez && lens >= maxrez )
   {
      hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_BUFFER_OVERFLOW, NULL, NULL );
   }

   if( expreal != NULL && expreal > cStart )
   {
      while( *( expreal - 1 ) == ' ' && expreal > cStart )
      {
         expreal--;
         lens--;
      }

      *expreal = '\0';
   }

   /* Ron Pinkas added 2000-06-21 */
   if( bStrict )
   {
      if( State == STATE_QUOTE1 || State == STATE_QUOTE2 || State == STATE_QUOTE3 || State == STATE_BRACKET || StBr1 || StBr2 || StBr3 )
      {
         /* Alexander should we include this???
            expreal = NULL;
          */
         lens = 0;
      }
   }
   /* Ron Pinkas end 2000-06-21 */

   return lens;
}

static HB_BOOL isExpres( char * stroka, HB_BOOL prlist )
{
   int l1, l2;

   HB_TRACE( HB_TR_DEBUG, ( "isExpres(%s)", stroka ) );

#if 0
   printf( "Exp: >%s<\n", stroka );
#endif

   l1   = strlen( stroka );
   l2   = getExpReal( NULL, &stroka, prlist, HB_PP_STR_SIZE, HB_TRUE, HB_FALSE );

#if 0
   printf( "Len1: %i Len2: %i RealExp: >%s< Last: %c\n", l1, l2, stroka - l2, ( stroka - l2 )[ l1 - 1 ] );
#endif

   /* Ron Pinkas modified 2000-06-17 Expression can't be valid if last charcter is one of these: ":/+*-%^=(<>"
      return ( l1 <= l2 );
    */

   return l1 <= l2 /*&& ! IsInStr( ( stroka - l2 )[l1-1], ":/+*-%^=(<>[{" ) */;
}

static HB_BOOL TestOptional( char * ptr1, char * ptr2 )
{
   int      nbr        = 0;
   HB_BOOL  flagname   = HB_FALSE;
   int      statevar   = 0;

   HB_TRACE( HB_TR_DEBUG, ( "TestOptional(%s, %s)", ptr1, ptr2 ) );

   while( ptr1 <= ptr2 )
   {
      if( *ptr1 == HB_PP_OPT_START )
      {
         nbr++;
      }
      else if( *ptr1 == HB_PP_OPT_END )
      {
         if( nbr )
         {
            nbr--;
            flagname = HB_FALSE;
         }
         else
            return 0;
      }
      else if( *ptr1 == HB_PP_MATCH_MARK && *( ptr1 + 2 ) == '2' && nbr )
         statevar = 1;
      else if( *ptr1 == '>' && statevar )
         statevar = 0;
      else if( *ptr1 != ' ' && *ptr1 != '\t' && ! statevar )
      {
         if( nbr )
            flagname = HB_TRUE;
         else
            return 0;
      }
      ptr1++;
   }
   /*   if( !flagname )
      while( *ptr1 != ']' )
      {
      if( *ptr1 == '[' || *ptr1 == '\0' ) return 0;
      ptr1++;
      } */
   return ! flagname;
}

static HB_BOOL CheckOptional( char * ptrmp, char * ptri, char * ptro, int * lenres, HB_BOOL com_or_tra, HB_BOOL com_or_xcom )
{
   int      save_numBr       = s_numBrackets, save_Repeate = s_Repeate;
   HB_BOOL  endTranslation   = HB_FALSE;
   HB_BOOL  bResult          = HB_TRUE;
   char *   lastInputptr[ 5 ];
   char *   lastopti[ 3 ], * ptr;

   HB_SYMBOL_UNUSED( com_or_tra );

   HB_TRACE( HB_TR_DEBUG, ( "CheckOptional(%s, %s, %s, %p, %d, %d)", ptrmp, ptri, ptro, lenres, com_or_tra, com_or_xcom ) );

   s_bReplacePat             = HB_FALSE;
   lastInputptr[ s_Repeate ] = ptri;
   while( *ptri != '\0' && ! endTranslation && bResult )
   {
      HB_SKIPTABSPACES( ptrmp );
      switch( *ptrmp )
      {
         case HB_PP_OPT_START:
            s_numBrackets++;
            s_aIsRepeate[ s_Repeate ] = 0;
            lastInputptr[ s_Repeate ] = ptri;
            lastopti[ s_Repeate++ ]   = ptrmp;
            ptrmp++;
            break;

         case HB_PP_OPT_END:
            if( s_numBrackets == save_numBr )
               endTranslation = HB_TRUE;
            else
            {
               if( s_Repeate )
               {
                  s_Repeate--;
                  ptrmp = lastopti[ s_Repeate ];
               }
               else
                  ptrmp++;
               s_numBrackets--;
            }
            break;

         case ',':
            if( *ptri == ',' )
            {
               ptrmp++;
               ptri++;
            }
            else
            {
               if( s_numBrackets - save_numBr > 0 )
               {
                  SkipOptional( &ptrmp );
                  ptri = lastInputptr[ s_Repeate ];
               }
               else
                  bResult = HB_FALSE;
            }
            break;

         case HB_PP_MATCH_MARK:        /*  Match marker */
            if( ! WorkMarkers( &ptrmp, &ptri, ptro, lenres, com_or_tra, com_or_xcom ) )
            {
               if( s_numBrackets - save_numBr > 0 )
               {
                  SkipOptional( &ptrmp );
                  ptri = lastInputptr[ s_Repeate ];
               }
               else
                  bResult = HB_FALSE;
            }
            break;

         case '\0':
            bResult = HB_FALSE;

         default:              /*   Key word    */
            ptr     = ptri;
            if( *ptri == ',' || truncmp( &ptri, &ptrmp, ! com_or_xcom ) )
            {
               ptri = ptr;
               if( s_numBrackets - save_numBr > 0 )
               {
                  SkipOptional( &ptrmp );
                  ptri = lastInputptr[ s_Repeate ];
               }
               else
                  bResult = HB_FALSE;
            }
      }
      HB_SKIPTABSPACES( ptri );
   }
   ;

   if( *ptri == '\0' )
   {
      for(;; )
      {
         HB_SKIPTABSPACES( ptrmp );
         if( *ptrmp == HB_PP_OPT_START )
         {
            ptrmp++;
            SkipOptional( &ptrmp );
         }
         else if( *ptrmp == HB_PP_OPT_END )
            break;
         else
         {
            bResult = 0;
            break;
         }
      }
   }
   s_Repeate     = save_Repeate;
   s_numBrackets = save_numBr;
   s_bReplacePat = HB_TRUE;
   return bResult;
}

static void SkipOptional( char ** ptri )
{
   int nbr = 0;

   HB_TRACE( HB_TR_DEBUG, ( "SkipOptional(%p)", ptri ) );

   while( **ptri != HB_PP_OPT_END || nbr )
   {
      switch( **ptri )
      {
         case HB_PP_OPT_START:
            nbr++;
            break;
         case HB_PP_OPT_END:
            nbr--;
            break;
         case HB_PP_MATCH_MARK:
            ( *ptri ) += 3;
            if( *( *ptri - 1 ) == '2' )
               while( **ptri != '>' )
                  ( *ptri )++;
            break;
      }
      ( *ptri )++;
   }
   if( **ptri == HB_PP_OPT_END && s_numBrackets > 0 )
   {
      if( s_Repeate )
         s_Repeate--;
      s_numBrackets--;
      ( *ptri )++;
   }
}

static void SearnRep( char * exppatt, char * expreal, int lenreal, char * ptro, int * lenres )
{
   int      ifou, isdvig = 0;
   HB_BOOL  rezs, bFound = HB_FALSE;
   int      kolmarkers;
   int      lennew, i;
   char     lastchar = '0';
   char *   ptr, * ptr2, * ptrOut = ptro;

   HB_TRACE( HB_TR_DEBUG, ( "SearnRep(%s, %s, %d, %s, %p)", exppatt, expreal, lenreal, ptro, lenres ) );

   if( s_expcopy == NULL )
      s_expcopy = ( char * ) hb_xgrab( HB_PP_STR_SIZE );

   if( *( exppatt + 1 ) == '\0' )
      *( ptro + *lenres ) = '\0';

   while( ( ifou = md_strAt( exppatt, ( *( exppatt + 1 ) ) ? 2 : 1, ptrOut, HB_FALSE, HB_FALSE, HB_TRUE, MD_STR_AT_USESUBCASE ) ) > 0 )
   {
      bFound     = HB_TRUE;
      rezs       = HB_FALSE;
      ptr        = ptrOut + ifou - 1;
      kolmarkers = 0;
      ptr        = PrevSquare( ptr, ptro, &kolmarkers );
      if( ptr )
      {
         if( s_Repeate )
            s_aIsRepeate[ s_Repeate - 1 ]++;
         if( ! s_bReplacePat )
            return;

         ptr2 = ptrOut + ifou + 3;
         while( *ptr2 != HB_PP_OPT_END || *( ptr2 - 1 ) == '\\' )
         {
            if( *ptr2 == HB_PP_MATCH_MARK )
               kolmarkers++;
            ptr2++;
         }

         if( s_Repeate && lenreal && kolmarkers && lastchar != '0' && *( ptrOut + ifou + 2 ) == '0' )
         {
            isdvig += ifou;
            rezs    = HB_TRUE;
         }
         else if( s_Repeate )
         {
            if( lenreal == 0 )
            {
               if( s_numBrackets >= 2 )
               {
                  isdvig += ifou;
                  continue;
               }
               else
               {
                  hb_pp_Stuff( "", ptr, 0, ptr2 - ptr + 1, *lenres - ( ptr - ptro ) );
                  *lenres   -= ptr2 - ptr + 1;
                  isdvig     = ptr - ptro;
                  rezs       = HB_TRUE;
               }
            }
            else
            {
               lennew = ptr2 - ptr - 1;

               if( lennew < HB_PP_STR_SIZE - 2 )
               {
                  memcpy( s_expcopy, ptr + 1, lennew );
               }
               else
               {
                  hb_compGenError( NULL, hb_pp_szErrors, 'F', HB_PP_ERR_BUFFER_OVERFLOW, NULL, NULL );
                  return;
               }
               *( s_expcopy + lennew++ ) = ' ';
               *( s_expcopy + lennew )   = '\0';
               while( ( i = hb_strAt( exppatt, 2, s_expcopy, lennew ) ) > 0 )
                  lennew += ReplacePattern( exppatt[ 2 ], expreal, lenreal, s_expcopy + i - 1, lennew );
               if( kolmarkers )
               {
                  s_groupchar = ( char ) ( ( unsigned int ) s_groupchar + 1 );
                  for( i = 0; i < lennew; i++ )
                     if( *( s_expcopy + i ) == HB_PP_MATCH_MARK )
                     {
                        *( s_expcopy + i + 3 ) = s_groupchar;
                        i                     += 4;
                     }
               }
               hb_pp_Stuff( s_expcopy, ptr, lennew, 0, *lenres - ( ptr - ptro ) );
               *lenres   += lennew;
               isdvig     = ptr - ptro + ( ptr2 - ptr - 1 ) + lennew;
               rezs       = HB_TRUE;
            }
         }
         else if( exppatt[ 0 ] == '\001' && exppatt[ 1 ] == '\000' )
         {
            /* final pass to remove optional markers */
            hb_pp_Stuff( "", ptr, 0, ptr2 - ptr + 1, *lenres - ( ptr - ptro ) );
            *lenres   -= ptr2 - ptr + 1;
            isdvig     = ptr - ptro;
            rezs       = HB_TRUE;
         }
      }

      if( ! rezs && s_bReplacePat )
      {
         if( *( ptrOut + ifou + 2 ) != '0' && *( exppatt + 1 ) )
         {
            isdvig = ptrOut - ptro + ifou;
            do
            {
               if( lastchar == '0' )
                  lastchar = *( ptrOut + ifou + 2 );
               if( lastchar != *( ptrOut + ifou + 2 ) )
               {
                  ifou   += 3;
                  ptrOut  = ptrOut + ifou;
                  continue;
               }

               *lenres   += ReplacePattern( exppatt[ 2 ], expreal, lenreal, ptrOut + ifou - 1, *lenres - ifou + 1 );
               ptrOut     = ptrOut + ifou;
            }
            while( ( ifou = md_strAt( exppatt, ( *( exppatt + 1 ) ) ? 2 : 1, ptrOut, HB_FALSE, HB_FALSE, HB_TRUE, MD_STR_AT_USESUBCASE ) ) > 0 );
            if( ! s_Repeate )
            {
               lastchar++;
               ptrOut  = ptro + isdvig;
               isdvig  = 0;
               continue;
            }
            return;
         }
         else if( lastchar == '0' )
         {
            *lenres   += ReplacePattern( ( exppatt[ 1 ] ? exppatt[ 2 ] : exppatt[ 1 ] ), expreal, lenreal, ptrOut + ifou - 1, *lenres - isdvig - ifou + 1 );
            isdvig    += ifou - 1;
         }
         else
         {
            ptrOut += ifou + 1;
            continue;
         }
      }
      else if( ! s_bReplacePat )
         isdvig += ifou;
      ptrOut = ptro + isdvig;
   }
   if( ! bFound && s_Repeate )
      s_aIsRepeate[ s_Repeate - 1 ]++;
}

static HB_BOOL ScanMacro( char * expreal, int lenitem, int * pNewLen )
{
   int i;

   HB_TRACE( HB_TR_DEBUG, ( "ScanMacro(%s, %d, %p)", expreal, lenitem, pNewLen ) );

   expreal++;                   /* skip '&' character */
   i = 0;

   while( expreal[ i ] == ' ' || expreal[ i ] == '\t' )
   {
      i++;
   }
   if( expreal[ i ] == '(' )
   {
      *pNewLen = lenitem - 1;
      return HB_TRUE;
   }
   else if( HB_ISALPHA( ( BYTE ) expreal[ i ] ) || expreal[ i ] == '_' )
   {
      i++;
      while( ISNAME( ( BYTE ) expreal[ i ] ) )
      {
         i++;
      }
      *pNewLen = i;
      if( expreal[ i ] == '.' )
      {
         i++;
      }
      while( expreal[ i ] == ' ' || expreal[ i ] == '\t' )
      {
         i++;
      }
      if( expreal[ i ] == '\0' || expreal[ i ] == ',' || expreal[ i ] == ')' )   /* || expreal[i] == ' ' )*/
      {
         return HB_TRUE;
      }
   }
   *pNewLen = lenitem;
   return HB_FALSE;
}

static int pp_Stringify( HB_BOOL bSmart, char ** ptro, int * lenres, char * expr, int lenitem )
{
   int      rmlen         = 0;
   int      lenTrim       = lenitem;
   int      iAdd          = 0;
   char     sQuotes[ 3 ]  = "\"\"";
   HB_BOOL  bComma        = expr[ lenitem ] == ',';

   while( *expr == ' ' && lenTrim )
   {
      expr++;
      lenTrim--;
   }
   while( lenTrim && expr[ lenTrim - 1 ] == ' ' )
   {
      lenTrim--;
   }

   if( ! lenTrim )
   {
      /* empty string - do nothing unless the comma is required */
   }
   else if( *expr == '&' )
   {
      /* macro operator */
      HB_BOOL  bSmartMacro;
      int      lennew;

      lennew        = lenTrim;
      bSmartMacro   = ScanMacro( expr, lenTrim, &lennew );
      if( bSmartMacro )
      {
         /* remove macro operator '&' */
         lenTrim = lennew;
         hb_pp_Stuff( expr + 1, *ptro, lenTrim, 0, *lenres );
      }
      else
      {
         /* enclose expression in string markers */
         iAdd = 2;
         pp_rQuotes( expr, sQuotes );
         hb_pp_Stuff( sQuotes, *ptro, iAdd, 0, *lenres );
         hb_pp_Stuff( expr, *ptro + 1, lenTrim, 0, *lenres + iAdd );
      }
   }
   else if( bSmart && ( *expr == '('
                        || ( *expr == '\"' && *( expr + lenTrim - 1 ) == '\"' )
                        || ( *expr == '[' && *( expr + lenTrim - 1 ) == ']' ) || ( *expr == '\'' && *( expr + lenTrim - 1 ) == '\'' ) ) )
   {
      /* items enclosed in () "" '' [] leave unchanged */
      hb_pp_Stuff( expr, *ptro, lenTrim, 0, *lenres );
   }
   else
   {
      /* enclose expression in string markers */
      iAdd = 2;
      pp_rQuotes( expr, sQuotes );
      hb_pp_Stuff( sQuotes, *ptro, iAdd, 0, *lenres );
      hb_pp_Stuff( expr, *ptro + 1, lenTrim, 0, *lenres + iAdd );
   }
   ( *ptro )    += lenTrim + iAdd;
   ( *lenres )  += lenTrim + iAdd;

   if( bComma )
   {
      hb_pp_Stuff( ",", *ptro, 1, 0, *lenres );
      ( *lenres )++;
      ( *ptro )++;
      rmlen++;
   }
   rmlen += lenTrim + iAdd;

   return rmlen;
}

static int ReplacePattern( char patttype, char * expreal, int lenreal, char * ptro, int lenres )
{
   int   rmlen         = lenreal, ifou, lenitem;
   char  sQuotes[ 4 ]  = "\"\",";

   HB_TRACE( HB_TR_DEBUG, ( "ReplacePattern(%c, %s, %d, %s, %p)", patttype, expreal, lenreal, ptro, lenres ) );

   switch( *( ptro + 2 ) )
   {
      case '0':                /* Regular result marker  */
         hb_pp_Stuff( expreal, ptro, lenreal, 4, lenres );
         break;

      case '1':                /* Dumb stringify result marker  */
         pp_rQuotes( expreal, sQuotes );
         hb_pp_Stuff( sQuotes, ptro, 2, 4, lenres );
         if( lenreal )
            hb_pp_Stuff( expreal, ptro + 1, lenreal, 0, lenres );
         rmlen = lenreal + 2;
         if( *sQuotes == '[' )
            hb_pp_NestedLiteralString = HB_TRUE;
         break;

      case '2':                                 /* Normal stringify result marker  */
         hb_pp_Stuff( "", ptro, 0, 4, lenres ); /* remove match marker */
         lenres -= 4;
         if( patttype == '1' )                  /* list match marker */
         {
            rmlen = 0;
            do
            {
               ifou = md_strAt( ",", 1, expreal, HB_FALSE, HB_TRUE, HB_FALSE, MD_STR_AT_IGNORECASE );
               if( *expreal != '\0' )
               {
                  rmlen += pp_Stringify( HB_FALSE, &ptro, &lenres, expreal, ( ( ifou ) ? ifou - 1 : lenreal ) );
               }
               expreal   += ifou;
               lenreal   -= ifou;
            }
            while( ifou > 0 );
         }
         else
         {
            rmlen = pp_Stringify( HB_FALSE, &ptro, &lenres, expreal, lenreal );
         }
         break;

      case '3':                                 /* Smart stringify result marker  */
         hb_pp_Stuff( "", ptro, 0, 4, lenres ); /* remove match marker */
         lenres -= 4;
         if( patttype == '1' )                  /* list match marker */
         {
            rmlen = 0;
            do
            {
               ifou = md_strAt( ",", 1, expreal, HB_FALSE, HB_TRUE, HB_FALSE, MD_STR_AT_IGNORECASE );
               if( *expreal != '\0' )
               {
                  rmlen += pp_Stringify( HB_TRUE, &ptro, &lenres, expreal, ( ( ifou ) ? ifou - 1 : lenreal ) );
               }
               expreal   += ifou;
               lenreal   -= ifou;
            }
            while( ifou > 0 );
         }
         else
         {
            rmlen = pp_Stringify( HB_TRUE, &ptro, &lenres, expreal, lenreal );
         }
         break;

      case '4':                /* Blockify result marker  */
         if( ! lenreal )
            hb_pp_Stuff( expreal, ptro, lenreal, 4, lenres );
         else if( patttype == '1' )     /* list match marker */
         {
            hb_pp_Stuff( "", ptro, 0, 4, lenres );
            lenres -= 4;
            rmlen   = 0;
            do
            {
               ifou    = md_strAt( ",", 1, expreal, HB_FALSE, HB_TRUE, HB_FALSE, MD_STR_AT_IGNORECASE );
               lenitem = ( ifou ) ? ifou - 1 : lenreal;
               if( *expreal != '\0' )
               {
                  int i;

                  i       = ( ifou ) ? 6 : 5;
                  hb_pp_Stuff( "{|| },", ptro, i, 0, lenres );
                  hb_pp_Stuff( expreal, ptro + 4, lenitem, 0, lenres + i );
                  ptro   += i + lenitem;
                  rmlen  += i + lenitem;
               }
               expreal   += ifou;
               lenreal   -= ifou;
            }
            while( ifou > 0 );
         }
         else if( lenreal && *expreal == '{' )
         {
            hb_pp_Stuff( expreal, ptro, lenreal, 4, lenres );
         }
         else
         {
            hb_pp_Stuff( "{|| }", ptro, 5, 4, lenres );
            hb_pp_Stuff( expreal, ptro + 4, lenreal, 0, lenres );
            rmlen = lenreal + 5;
         }
         break;

      case '5':                /* Logify result marker  */
         rmlen = 3;
         if( ! lenreal )
         {
            hb_pp_Stuff( ".F.", ptro, 3, 4, lenres );
         }
         else
            hb_pp_Stuff( ".T.", ptro, 3, 4, lenres );
         break;

      case '6':                /* Ommit result marker  */
         rmlen = 1;
         hb_pp_Stuff( " ", ptro, 1, 4, lenres );
         break;
   }
   return rmlen - 4;
}

static void pp_rQuotes( char * expreal, char * sQuotes )
{
   HB_BOOL  lQuote1 = HB_FALSE;
   HB_BOOL  lQuote2 = HB_FALSE;

   HB_TRACE( HB_TR_DEBUG, ( "pp_rQuotes(%s, %s)", expreal, sQuotes ) );

   /*
      printf( "String: >%s< Delim: %s\n", expreal, sQuotes );
    */

   while( *expreal != '\0' )
   {
      if( *expreal == '\"' )
         lQuote2 = HB_TRUE;
      else if( *expreal == '\'' )
         lQuote1 = HB_TRUE;
      expreal++;
   }
   if( lQuote2 )
   {
      if( lQuote1 )
      {
         *sQuotes         = '[';
         *( sQuotes + 1 ) = ']';
      }
      else
      {
         *sQuotes         = '\'';
         *( sQuotes + 1 ) = '\'';
      }
   }
   else
   {
      *sQuotes         = '\"';
      *( sQuotes + 1 ) = '\"';
   }
}

int hb_pp_RdStr( FILE * handl_i, char * buffer, int maxlen, HB_BOOL lContinue, char * sBuffer, int * lenBuffer, int * iBuffer )
{
   int      readed        = 0;
   int      State         = 0;
   char     cha, cLast = '\0', symbLast = '\0';
   HB_BOOL  lDropSpaces   = lContinue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_RdStr(%p, %s, %d, %d, %s, %p, %p)", handl_i, buffer, maxlen, lDropSpaces, sBuffer, lenBuffer, iBuffer ) );

   if( *lenBuffer == 0 )
   {
      return -1;
   }

   for(;; )
   {
      if( *iBuffer == *lenBuffer )
      {
         if( ( *lenBuffer = fread( sBuffer, 1, HB_PP_BUFF_SIZE, handl_i ) ) < 1 )
         {
            sBuffer[ 0 ] = '\n';
         }
         *iBuffer = 0;
      }

      cha = sBuffer[ *iBuffer ];
      ( *iBuffer )++;

      if( cha == '\r' )
      {
         cha = ' ';
      }

      if( cha == '\n' )
      {
         if( ( hb_pp_StreamBlock != HB_PP_STREAM_DUMP_C ) && s_ParseState == STATE_COMMENT && symbLast == ';' )
         {
            buffer[ readed++ ] = ';';
         }
         break;
      }
      else
      {
         if( hb_pp_StreamBlock == HB_PP_STREAM_DUMP_C )
         {
            buffer[ readed++ ] = cha;
            continue;
         }
      }

      if( maxlen > 0 )
      {
         switch( s_ParseState )
         {
            case STATE_COMMENT:
               if( cha == '/' && cLast == '*' )
               {
                  s_ParseState  = STATE_NORMAL;
                  cha           = ' ';
               }

               cLast = cha;

               if( cha != ' ' && cha != '\t' )
               {
                  symbLast = cha;
               }
               break;

            case STATE_QUOTE1:
               if( cha == '\'' )
                  s_ParseState = STATE_NORMAL;
               break;

            case STATE_QUOTE2:
               if( cha == '\"' )
                  s_ParseState = STATE_NORMAL;
               break;

            case STATE_QUOTE3:
               if( cha == ']' )
                  s_ParseState = STATE_NORMAL;
               break;

            default:
               switch( cha )
               {
                  case '[':
                     /* Ron Pinkas modified 2000-06-17
                        if( ISNAME(( BYTE ) s_prevchar) || s_prevchar == ']' )
                      */
                     if( ISNAME( ( BYTE ) s_prevchar ) || strchr( ")]}.", s_prevchar ) )
                     {
                        s_ParseState = STATE_BRACKET;
                     }
                     else
                     {
                        s_ParseState = STATE_QUOTE3;
                     }
                     break;

                  case ']':
                     s_ParseState = STATE_NORMAL;
                     break;

                  case '\"':
                     if( s_ParseState != STATE_BRACKET )
                     {
                        s_ParseState = STATE_QUOTE2;
                     }
                     break;

                  case '\'':
                     if( s_ParseState != STATE_BRACKET )
                     {
                        s_ParseState = STATE_QUOTE1;
                     }
                     break;

                  case '&':
                     if( readed > 0 && buffer[ readed - 1 ] == '&' )
                     {
                        maxlen = 0;
                        readed--;
                     }
                     break;

                  case '/':
                     if( readed > 0 && buffer[ readed - 1 ] == '/' && ! hb_pp_StreamBlock )
                     {
                        maxlen = 0;
                        readed--;
                     }
                     break;

                  case '*':
                     if( readed > 0 && buffer[ readed - 1 ] == '/' && ! hb_pp_StreamBlock )
                     {
                        s_ParseState = STATE_COMMENT;
                        readed--;
                     }
                     else if( ! State && ! lContinue )
                     {
                        maxlen = readed = 0;
                     }
                     break;
               }

               if( cha != ' ' && cha != ';' )
               {
                  s_prevchar = cha;
               }
         }

         if( cha != ' ' && cha != '\t' )
         {
            State = 1;
         }

         if( lDropSpaces && State )
         {
            lDropSpaces = 0;
         }

         if( readed < maxlen && ( ! lDropSpaces || readed == 0 ) && s_ParseState != STATE_COMMENT )
         {
            buffer[ readed++ ] = cha;
         }
      }
   }

   while( --readed >= 0 && ( buffer[ readed ] == ' ' || buffer[ readed ] == '\t' ) )
      ;

   if( buffer[ readed ] != ';' && s_ParseState != STATE_COMMENT )
   {
      s_ParseState = STATE_NORMAL;
   }

   if( maxlen )
   {
      if( readed < maxlen )
         readed++;
   }
   else
      readed++;
   buffer[ readed ] = '\0';

#if 0
   printf( "%s\n", buffer );
#endif

   return readed;
}

int hb_pp_WrStr( FILE * handl_o, char * buffer )
{
   int lens = strlen( buffer );

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_WrStr(%p, %s)", handl_o, buffer ) );

   /* Ron Pinkas added 2001-01-20 */
   if( hb_comp_files.iFiles == 1 )
   {
      for(; hb_pp_LastOutLine < hb_comp_iLine - 1; hb_pp_LastOutLine++ )
      {
         fwrite( "\n", 1, 1, handl_o );
      }
      hb_pp_LastOutLine = hb_comp_iLine;
   }
   /* END Ron Pinkas added 2001-01-20 */

   fwrite( buffer, lens, 1, handl_o );

   if( *( buffer + lens - 1 ) != '\n' )
   {
      fwrite( "\n", 1, 1, handl_o );
   }

   return 0;
}

static int md_strAt( char * szSub, int lSubLen, char * szText, HB_BOOL checkword, HB_BOOL checkPrth, HB_BOOL bRule, int iCaseOption )
{
   int   State         = STATE_NORMAL;
   long  lPos          = 0, lSubPos = 0;
   int   kolPrth       = 0, kolSquare = 0, kolFig = 0;
   int   lCase;
   int   iNestedQuote3 = 0;
   char  cLastChar     = '\0';

   HB_TRACE( HB_TR_DEBUG, ( "md_strAt(%s, %d, %s, %d, %d, %d)", szSub, lSubLen, szText, checkword, checkPrth, iCaseOption ) );

   if( iCaseOption == MD_STR_AT_IGNORECASE )
   {
      lCase = 0;
   }
   else
   {
      lCase = ( *szSub == HB_PP_MATCH_MARK ) ? 0 : 1;
   }

   while( *( szText + lPos ) != '\0' && lSubPos < lSubLen )
   {
      if( State == STATE_QUOTE1 )
      {
         if( *( szText + lPos ) == '\'' )
         {
            State = STATE_NORMAL;
         }
         lPos++;
      }
      else if( State == STATE_QUOTE2 )
      {
         if( *( szText + lPos ) == '\"' )
         {
            State = STATE_NORMAL;
         }
         lPos++;
      }
      else if( State == STATE_QUOTE3 )
      {
         if( *( szText + lPos ) == ']' )
         {
            if( --iNestedQuote3 == 0 )
               State = STATE_NORMAL;
         }
         else if( *( szText + lPos ) == '[' )
            iNestedQuote3++;
         lPos++;
      }
      else
      {
         if( State == STATE_BRACKET )
         {
            if( *( szText + lPos ) == ']' && ( lPos == 0 || *( szText + lPos - 1 ) != '\\' ) )
            {
               kolSquare--;
               if( kolSquare == 0 )
               {
                  State = STATE_NORMAL;
               }

               cLastChar = ']';
               lPos++;
               continue;
            }
            else if( *( szText + lPos ) == '[' && ( lPos == 0 || *( szText + lPos - 1 ) != '\\' ) )
            {
               kolSquare++;
               cLastChar = '[';
               lPos++;
               continue;
            }
            else if( *( szText + lPos ) == ',' )
            {
               cLastChar = ',';
               lPos++;
               continue;
            }
         }
         else
         {
            if( ( *( szText + lPos ) == '\'' || *( szText + lPos ) == '`' ) && ( lPos == 0 || *( szText + lPos - 1 ) != '\\' ) )
            {
               State = STATE_QUOTE1;
               lPos++;
               continue;
            }
            else if( *( szText + lPos ) == '\"' && ( lPos == 0 || *( szText + lPos - 1 ) != '\\' ) )
            {
               State = STATE_QUOTE2;
               lPos++;
               continue;
            }
            else if( bRule == HB_FALSE && *( szText + lPos ) == '[' && strchr( ")]}.", cLastChar ) == NULL && ! ISNAME( ( BYTE ) cLastChar ) )
            {
               State = STATE_QUOTE3;
               iNestedQuote3++;
               lPos++;
               continue;
            }
            else if( *( szText + lPos ) == '[' && ( lPos == 0 || *( szText + lPos - 1 ) != '\\' ) )
            {
               State      = STATE_BRACKET;
               kolSquare++;
               cLastChar  = '[';
               lPos++;
               continue;
            }
            else if( *( szText + lPos ) == '(' )
            {
               kolPrth++;
            }
            else if( *( szText + lPos ) == ')' )
            {
               kolPrth--;
            }
            else if( *( szText + lPos ) == '{' )
            {
               kolFig++;
            }
            else if( *( szText + lPos ) == '}' )
            {
               kolFig--;
            }
            else if( szText[ lPos ] == '.' && szSub[ 0 ] != '.' )
            {
               if( HB_TOUPPER( szText[ lPos + 1 ] ) == 'T' && szText[ lPos + 2 ] == '.' )
               {
                  lPos += 3;
               }
               else if( HB_TOUPPER( szText[ lPos + 1 ] ) == 'F' && szText[ lPos + 2 ] == '.' )
               {
                  lPos += 3;
               }
               else if( HB_TOUPPER( szText[ lPos + 1 ] ) == 'O' && HB_TOUPPER( szText[ lPos + 2 ] ) == 'R' && szText[ lPos + 4 ] == '.' )
               {
                  lPos += 4;
               }
               else if( HB_TOUPPER( szText[ lPos + 1 ] ) == 'A'
                        && HB_TOUPPER( szText[ lPos + 2 ] ) == 'N' && HB_TOUPPER( szText[ lPos + 3 ] ) == 'D' && szText[ lPos + 4 ] == '.' )
               {
                  lPos += 5;
               }
               else if( HB_TOUPPER( szText[ lPos + 1 ] ) == 'N'
                        && HB_TOUPPER( szText[ lPos + 2 ] ) == 'O' && HB_TOUPPER( szText[ lPos + 3 ] ) == 'T' && szText[ lPos + 4 ] == '.' )
               {
                  lPos += 5;
               }
               else
               {
                  lPos++;
               }

               lSubPos = 0;
               continue;
            }
         }

         if( ! lSubPos && checkPrth &&
             ( ( ( kolPrth > 1 )
                 || ( kolPrth == 1 && *( szText + lPos ) != '(' )
                 || ( kolPrth == 0 && *( szText + lPos ) == ')' ) )
               || ( ( kolFig > 1 ) || ( kolFig == 1 && *( szText + lPos ) != '{' ) || ( kolFig == 0 && *( szText + lPos ) == '}' ) ) ) )
         {
            cLastChar = *( szText + lPos );
            lPos++;
            continue;
         }

         if( lSubPos && checkPrth && ( kolPrth > 0 || kolFig > 0 ) )
         {
            cLastChar  = *( szText + lPos );
            lPos++;
            lSubPos    = 0;
            continue;
         }

         if( ( lCase
               && HB_TOUPPER( *( szText + lPos ) ) == HB_TOUPPER( *( szSub + lSubPos ) ) ) || ( ! lCase && *( szText + lPos ) == *( szSub + lSubPos ) ) )
         {
            lSubPos++;
            cLastChar = *( szText + lPos );
            lPos++;

            if( lSubPos >= lSubLen && checkword &&
                ( ( ISNAME( ( BYTE ) *szSub ) && lPos > lSubPos
                    && ISNAME( ( BYTE ) *( szText + lPos - lSubPos - 1 ) ) )
                  || ( ISNAME( ( BYTE ) *( szSub + lSubLen - 1 ) ) && ISNAME( ( BYTE ) *( szText + lPos ) ) ) ) )
            {
               lSubPos = 0;
            }
         }
         else if( lSubPos )
         {
            lSubPos = 0;
         }
         else
         {
            cLastChar = *( szText + lPos );
            lPos++;
         }
      }
   }

#if 0
   if( bRule == 0 && szSub[ 0 ] != ';' )
   {
      printf( "Rule: %i Find: >%s< In: >%s<\n", bRule, szSub, szText );
      printf( "Pos: %i Len: %i At: >%s<\n", lPos, lSubLen, ( szText + lPos - lSubLen ) );
   }
#endif

   return lSubPos < lSubLen ? 0 : lPos - lSubLen + 1;
}

static char * PrevSquare( char * ptr, char * bound, int * kolmark )
{
   HB_TRACE( HB_TR_DEBUG, ( "PrevSquare(%s, %s, %d)", ptr, bound, kolmark == NULL ? 0 : *kolmark ) );
   while( ptr > bound )
   {
      if( kolmark && *ptr == HB_PP_MATCH_MARK )
      {
         ( *kolmark )++;
      }
      else if( *ptr == HB_PP_OPT_START || *ptr == HB_PP_OPT_END )
      {
         break;
      }
      ptr--;
   }
   return ( *ptr == HB_PP_OPT_START ) ? ptr : NULL;
}

static int IsInStr( char symb, char * s )
{
   HB_TRACE( HB_TR_DEBUG, ( "IsInStr(%c, %s)", symb, s ) );

   while( *s != '\0' )
      if( *s++ == symb )
         return 1;
   return 0;
}

/* ptri = string for inserting
   ptro = output string
   len1 = length of ptri string
   len2 = length of ptro string that will be replaced
   lenres = length of ptro string
 */
void hb_pp_Stuff( char * ptri, char * ptro, int len1, int len2, int lenres )
{
   char *   ptr1, * ptr2;
   int      i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_Stuff(%s, %s, %d, %d, %d)", ptri, ptro, len1, len2, lenres ) );

   if( len1 > len2 )
   {
      ptr1       = ptro + lenres + 1;
      ptr2       = ptro + lenres + len1 - len2;
      /* This is a static buffer - current inserting can erase the null char
         than we need set it again.
       */
      ptr2[ 1 ]  = '\0';
      for( i = 0; i <= lenres; ptr2--, i++ )
      {
         ptr1--;
         *ptr2 = *ptr1;
      }
   }
   else
   {
      ptr1 = ptro + len2;
      ptr2 = ptro + len1;
      for(; ptr1 <= ptro + lenres; ptr1++, ptr2++ )
         *ptr2 = *ptr1;
   }
   ptr2 = ptro;
   for( i = 0; i < len1; i++ )
      *ptr2++ = *( ptri + i );
}

int hb_pp_strocpy( char * ptro, char * ptri )
{
   int lens = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_pp_strocpy(%s, %s)", ptro, ptri ) );

   if( ptri != NULL )
      while( *ptri != '\0' )
      {
         *ptro++ = *ptri++;
         lens++;
      }
   *ptro = '\0';
   return lens;
}

static int stroncpy( char * ptro, char * ptri, int lens )
{
   int i = 0;

   HB_TRACE( HB_TR_DEBUG, ( "stroncpy(%s, %s, %d)", ptro, ptri, lens ) );

   for(; i < lens; i++ )
      *( ptro + i ) = *ptri++;
   i--;
   while( i > 0 && *( ptro + i ) == ' ' )
      i--;
   i++;
   *( ptro + i ) = '\0';
   return i;
}

static HB_BOOL truncmp( char ** ptro, char ** ptri, HB_BOOL lTrunc )
{
   char * ptrb = *ptro, co, ci;

   HB_TRACE( HB_TR_DEBUG, ( "truncmp(%p, %p, %d)", ptro, ptri, lTrunc ) );


   for(;
       **ptri != ' ' && **ptri != '\t' && **ptri != ','
       && **ptri != HB_PP_OPT_START && **ptri != HB_PP_OPT_END
       && **ptri != HB_PP_MATCH_MARK && **ptri != '\0' && HB_TOUPPER( **ptri ) == HB_TOUPPER( **ptro ); ( *ptro )++, ( *ptri )++ )
      ;
   co   = *( *ptro - 1 );
   ci   = **ptri;

   if( ( ( ci == ' ' || ci == ',' || ci == HB_PP_OPT_START ||
           ci == HB_PP_OPT_END || ci == HB_PP_MATCH_MARK || ci == '\0' ) &&
         ( ( ! ISNAME( ( BYTE ) **ptro ) && ISNAME( ( BYTE ) co ) ) ||
           ( ! ISNAME( ( BYTE ) co ) ) ) ) )
      return HB_FALSE;
   else if( lTrunc && *ptro - ptrb >= 4 && ISNAME( ( BYTE ) ci ) && ! ISNAME( ( BYTE ) **ptro ) && ISNAME( ( BYTE ) co ) )
   {
      while( ISNAME( ( BYTE ) **ptri ) )
         ( *ptri )++;
      return HB_FALSE;
   }
   return HB_TRUE;
}

static HB_BOOL strincmp( char * ptro, char ** ptri, HB_BOOL lTrunc )
{
   char * ptrb = ptro, co, ci;

   HB_TRACE( HB_TR_DEBUG, ( "strincmp(%s, %p)", ptro, ptri ) );

   for(;
       **ptri != ',' && **ptri != HB_PP_OPT_START && **ptri != HB_PP_OPT_END
       && **ptri != HB_PP_MATCH_MARK && **ptri != '\0' && HB_TOUPPER( **ptri ) == HB_TOUPPER( *ptro ); ptro++, ( *ptri )++ )
      ;
   co   = *( ptro - 1 );
   ci   = **ptri;
   if( ( ( ci == ' ' || ci == ',' || ci == HB_PP_OPT_START ||
           ci == HB_PP_OPT_END || ci == HB_PP_MATCH_MARK || ci == '\0' ) &&
         ( ( ! ISNAME( ( BYTE ) *ptro ) && ISNAME( ( BYTE ) co ) ) || ( ! ISNAME( ( BYTE ) co ) ) ) ) )
      return HB_FALSE;
   else if( lTrunc && ptro - ptrb >= 4 && ISNAME( ( BYTE ) ci ) && ! ISNAME( ( BYTE ) *ptro ) && ISNAME( ( BYTE ) co ) )
   {
      /*      while( ISNAME(( BYTE ) **ptri) ) (*ptri)++; */
      return HB_FALSE;
   }
   return HB_TRUE;
}

static int strincpy( char * ptro, char * ptri )
{
   int lens = 0;

   HB_TRACE( HB_TR_DEBUG, ( "strincpy(%s, %s)", ptro, ptri ) );

   for(;
       *ptri != ' ' && *ptri != ',' && *ptri != HB_PP_OPT_START
       && *ptri != HB_PP_OPT_END && *ptri != HB_PP_MATCH_MARK && *ptri != '\0'; ptro++, ptri++, lens++ )
      *ptro = *ptri;
   return lens;
}

static int strotrim( char * stroka, HB_BOOL bRule )
{
   char *   ptr  = stroka, lastc = '0', curc;
   int      lens = 0, State = STATE_NORMAL;

   HB_TRACE( HB_TR_DEBUG, ( "strotrim(%s)", stroka ) );

   while( ( curc = *stroka ) != '\0' )
   {
      if( State == STATE_QUOTE1 )
      {
         if( curc == '\'' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( State == STATE_QUOTE2 )
      {
         if( curc == '\"' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( State == STATE_QUOTE3 )
      {
         if( curc == ']' )
         {
            State = STATE_NORMAL;
         }
      }
      else
      {
         if( curc == '\'' )
         {
            State = STATE_QUOTE1;
         }
         else if( curc == '\"' )
         {
            State = STATE_QUOTE2;
         }
         /* Ron Pinkas added 2000-11-05 */
         /* Ron Pinkas 2001-02-14 added bRule logic  (removed array logic). */
         else if( curc == '[' && bRule == HB_FALSE )       /* && ( strchr( ")]}.", cLastChar ) == NULL && ! ISNAME( ( BYTE ) cLastChar ) ) ) */
         {
            State = STATE_QUOTE3;
         }
         /* END - Ron Pinkas added 2000-11-05 */
         else if( curc == '\t' )
         {
            curc = ' ';
         }
      }

      if( State != STATE_NORMAL || curc != ' ' ||
          ( curc == ' ' && ! bRule ) ||
          ( curc == ' ' && lastc != ' ' && lastc != ',' && lastc != '(' && *( stroka + 1 ) != ',' && *( stroka + 1 ) != ' ' && *( stroka + 1 ) ) )
      {
         *ptr++  = curc;
         lastc   = curc;
         lens++;
      }

      stroka++;
   }

   *ptr = '\0';

   return lens;
}

static int NextWord( char ** sSource, char * sDest, HB_BOOL lLower )
{
   int i = 0;

   HB_TRACE( HB_TR_DEBUG, ( "NextWord(%p, %s, %d)", sSource, sDest, lLower ) );

   HB_SKIPTABSPACES( ( *sSource ) );

   while( **sSource != '\0' && **sSource != ' ' && **sSource != '\t' && **sSource != '(' )
   {
      *sDest++ = ( lLower ) ? HB_TOLOWER( **sSource ) : **sSource;
      ( *sSource )++;
      i++;
   }

   *sDest = '\0';

   return i;
}

static int NextName( char ** sSource, char * sDest )
{
   /* Ron Pinkas added 2000-11-08 */
   char cLastChar = '\0', * pString = NULL, * pTmp;

   /* END - Ron Pinkas added 2000-11-08 */

   int lenName = 0, State = STATE_NORMAL;

   HB_TRACE( HB_TR_DEBUG, ( "NextName(%p, %s)", sSource, sDest ) );

#if 0
   printf( "In: >%s<\n", *sSource );
#endif

   while( **sSource != '\0' && ( ! ISNAME( ( BYTE ) **sSource ) || State != STATE_NORMAL ) )
   {
      if( State == STATE_QUOTE1 )
      {
         if( **sSource == '\'' )
         {
            State      = STATE_NORMAL;

            /* Ron Pinkas added 2000-11-08 */
            **sSource  = '\0';
            if( strchr( pString, '"' ) == NULL )
            {
               *pString   = '"';
               **sSource  = '"';
            }
            else
            {
               **sSource = '\'';
            }
            /* END - Ron Pinkas added 2000-11-08 */
         }
      }
      else if( State == STATE_QUOTE2 )
      {
         if( **sSource == '\"' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( State == STATE_QUOTE3 )
      {
         if( **sSource == ']' )
         {
            State      = STATE_NORMAL;

            /* Ron Pinkas added 2000-11-08 */
            **sSource  = '\0';
            if( strchr( pString, '"' ) == NULL )
            {
               *pString   = '"';
               **sSource  = '"';
            }
            else if( strchr( pString, '\'' ) == NULL )
            {
               *pString   = '\'';
               **sSource  = '\'';
            }
            else
            {
               **sSource = ']';
            }
            /* END - Ron Pinkas added 2000-11-08 */
         }
      }
      else if( ( *sSource )[ 0 ] == HB_PP_OPT_START || ( *sSource )[ 0 ] == HB_PP_OPT_END )
      {
         State = STATE_NORMAL;
      }
      /* Ron Pinkas added 2001-02-21 */
      else if( ( *sSource )[ 0 ] == '.' && HB_TOUPPER( ( *sSource )[ 1 ] ) == 'A'
               && HB_TOUPPER( ( *sSource )[ 2 ] ) == 'N' && HB_TOUPPER( ( *sSource )[ 3 ] ) == 'D' && ( *sSource )[ 4 ] == '.' )
      {
         ( *sSource ) += 5;
         cLastChar     = ' ';
         continue;
      }
      else if( ( *sSource )[ 0 ] == '.' && HB_TOUPPER( ( *sSource )[ 1 ] ) == 'N'
               && HB_TOUPPER( ( *sSource )[ 2 ] ) == 'O' && HB_TOUPPER( ( *sSource )[ 3 ] ) == 'T' && ( *sSource )[ 4 ] == '.' )
      {
         ( *sSource ) += 5;
         cLastChar     = ' ';
         continue;
      }
      else if( ( *sSource )[ 0 ] == '.' && HB_TOUPPER( ( *sSource )[ 1 ] ) == 'O' && HB_TOUPPER( ( *sSource )[ 2 ] ) == 'R' && ( *sSource )[ 3 ] == '.' )
      {
         ( *sSource ) += 4;
         cLastChar     = ' ';
         continue;
      }
      /* End - Ron Pinkas added 2001-02-21 */
      else if( ( *sSource )[ 0 ] == '.' && HB_TOUPPER( ( *sSource )[ 1 ] ) == 'T' && ( *sSource )[ 2 ] == '.' )
      {
         ( *sSource ) += 3;
         cLastChar     = ' ';
         continue;
      }
      else if( ( *sSource )[ 0 ] == '.' && HB_TOUPPER( ( *sSource )[ 1 ] ) == 'F' && ( *sSource )[ 2 ] == '.' )
      {
         ( *sSource ) += 3;
         cLastChar     = ' ';
         continue;
      }
      else if( **sSource == '\'' )
      {
         /* Ron Pinkas added 2000-11-08 */
         pString = *sSource;
         State   = STATE_QUOTE1;
      }
      else if( **sSource == '\"' )
      {
         /* Ron Pinkas added 2000-11-08 */
         pString = *sSource;
         State   = STATE_QUOTE2;
      }
      /* Ron Pinkas added 2000-11-08 */
      else if( **sSource == '[' && s_bArray == HB_FALSE && strchr( ")]}.\"\'", cLastChar ) == NULL && ! ISNAME( ( BYTE ) cLastChar ) )
      {
         /* Ron Pinkas added 2000-11-08 */
         pString = *sSource;
         State   = STATE_QUOTE3;
      }
      /* END - Ron Pinkas added 2000-11-08 */

      /* Ron Pinkas added 2000-11-08 */
      if( State == STATE_NORMAL && **sSource != ' ' && **sSource != '\t' )
      {
         cLastChar = **sSource;
      }
      /* END - Ron Pinkas added 2000-11-08 */

      ( *sSource )++;
   }

   while( **sSource != '\0' && ISNAME( ( BYTE ) **sSource ) )
   {
      *sDest++ = *( *sSource )++;
      lenName++;
   }

   *sDest  = '\0';

   /* Ron Pinkas added 2000-11-08 - Prepare for next run. */
   pTmp    = *sSource;
   while( *pTmp && ( *pTmp == ' ' || *pTmp == '\t' || *pTmp == HB_PP_OPT_END || *pTmp == HB_PP_OPT_START ) )
   {
      pTmp++;
   }

   s_bArray = ( *pTmp == '[' );
   /* END - Ron Pinkas added 2000-11-08 */

#if 0
   printf( "NextName: >%s<\n", sDest - lenName );
   printf( "Rest: >%s<\n", *sSource );
#endif

   return lenName;
}

static int NextParm( char ** sSource, char * sDest )
{
   int   lenName    = 0, State = STATE_NORMAL, StBr = 0;
   char  cLastChar  = '\0';

   HB_TRACE( HB_TR_DEBUG, ( "NextParm(%p, %s)", sSource, sDest ) );

   HB_SKIPTABSPACES( ( *sSource ) );

   while( **sSource != '\0' )
   {
      if( State == STATE_QUOTE1 )
      {
         if( **sSource == '\'' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( State == STATE_QUOTE2 )
      {
         if( **sSource == '\"' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( State == STATE_QUOTE3 )
      {
         if( **sSource == ']' )
         {
            State = STATE_NORMAL;
         }
      }
      else if( **sSource == '\'' )
      {
         State = STATE_QUOTE1;
      }
      else if( **sSource == '\"' )
      {
         State = STATE_QUOTE2;
      }
      else if( **sSource == '[' && strchr( ")]}.", cLastChar ) == NULL && ! ISNAME( ( BYTE ) cLastChar ) )
      {
         State = STATE_QUOTE3;
      }
      /* Ron Pinkas added 2000-11-26 */
      else if( **sSource == '[' )
      {
         StBr++;
      }
      else if( **sSource == ']' )
      {
         StBr--;
      }
      /* END - Ron Pinkas added 2000-11-26 */
      else if( **sSource == '{' )
      {
         StBr++;
      }
      else if( **sSource == '}' )
      {
         StBr--;
      }
      else if( **sSource == '(' )
      {
         StBr++;
      }
      else if( **sSource == ')' || **sSource == ',' )
      {
         if( StBr == 0 )
         {
            break;
         }

         if( **sSource == ')' )
         {
            StBr--;
         }
      }

      if( sDest != NULL )
      {
         *sDest++ = **sSource;
      }

      if( State == STATE_NORMAL && **sSource != ' ' && **sSource != '\t' )
      {
         cLastChar = **sSource;
      }

      ( *sSource )++;
      lenName++;
   }

   if( sDest )
   {
      *sDest = '\0';
   }

#if 0
   if( sDest )
      printf( "NextParm: >%s<\n", sDest - lenName );
   else
      printf( "NextParm Len: %i\n", lenName );
#endif

   return lenName;
}

static HB_BOOL IsIdentifier( char * szProspect )
{
   if( HB_ISALPHA( ( BYTE ) szProspect[ 0 ] ) || szProspect[ 0 ] == '_' )
   {
      int i = 1;

      while( ISNAME( ( BYTE ) szProspect[ i ] ) )
      {
         i++;
      }
      while( szProspect[ i ] == ' ' )
      {
         i++;
      }

      return szProspect[ i ] == '\0';
   }

   return HB_FALSE;
}


static int IsMacroVar( char * szText, HB_BOOL isCommand )
{
   int len = 0;

   if( HB_ISALPHA( ( BYTE ) szText[ 0 ] ) || szText[ 0 ] == '_' )
   {
      int i = 1;

      while( ISNAME( ( BYTE ) szText[ i ] ) || HB_ISDIGIT( ( BYTE ) szText[ i ] ) || szText[ i ] == '&' || szText[ i ] == '.' )
      {
         i++;
      }
/*
      while( ISNAME( ( BYTE ) szText[i] ) || szText[i] == '&' )
      {
         i++;
      }
      if( szText[i] == '.' || szText[i] == '&' )
      {
         i++;
         while( ISNAME(( BYTE ) szText[i]) || HB_ISDIGIT( ( BYTE ) szText[i] ) || szText[i] == '&' || szText[i] == '.' )
         {
            i++;
         }
      }
 */
      len = i;
      if( ! ( szText[ i ] == '\0' || szText[ i ] == ' ' || szText[ i ] == ')' ) && isCommand )
      {
         return 0;
      }
   }
   return len;
}


static HB_BOOL OpenInclude( char * szFileName, HB_PATHNAMES * pSearch, PHB_FNAME pMainFileName, HB_BOOL bStandardOnly, char * szInclude )
{
   FILE *      fptr;
   PHB_FNAME   pFileName;
   PFILE       pFile;

   HB_TRACE( HB_TR_DEBUG, ( "OpenInclude(%s, %p, %p, %d, %s)", szFileName, pSearch, pMainFileName, ( int ) bStandardOnly, szInclude ) );

#if ! defined( __MINGW32CE__ ) && ! ( defined( _MSC_VER ) && defined( HB_OS_WIN_CE ) )
   errno = 0;
#endif
   if( bStandardOnly )
   {
      fptr             = 0;
      szInclude[ 0 ]   = '\0';
   }
   else
   {
      pFileName = hb_fsFNameSplit( szFileName );

      if( ( pFileName->szPath == NULL || *( pFileName->szPath ) == '\0' ) && pMainFileName )
         pFileName->szPath = pMainFileName->szPath;

      hb_fsFNameMerge( szInclude, pFileName );

      fptr = fopen( szInclude, "r" );
      hb_xfree( pFileName );
   }

   if( ! fptr && pSearch && ! hb_fsMaxFilesError() )
   {
      pFileName              = hb_fsFNameSplit( szFileName );
      pFileName->szName      = szFileName;
      pFileName->szExtension = NULL;
      while( pSearch && ! fptr )
      {
         pFileName->szPath   = pSearch->szPath;
         hb_fsFNameMerge( szInclude, pFileName );
         fptr                = fopen( szInclude, "r" );
         pSearch             = pSearch->pNext;
      }
      hb_xfree( pFileName );
   }

   if( fptr )
   {
      pFile               = ( PFILE ) hb_xgrab( sizeof( _FILE ) );
      pFile->handle       = fptr;
      pFile->pBuffer      = hb_xgrab( HB_PP_BUFF_SIZE );
      pFile->iBuffer      = pFile->lenBuffer = 10;
      pFile->yyBuffer     = NULL;
      pFile->szFileName   = hb_strdup( szFileName );

      if( hb_comp_files.pLast )
         hb_comp_files.pLast->iLine = hb_comp_iLine;
      hb_comp_iLine       = 1;

      pFile->iLine        = 1;
      pFile->pPrev        = hb_comp_files.pLast;
      hb_comp_files.pLast = pFile;
      hb_comp_files.iFiles++;
      return HB_TRUE;
   }

   return HB_FALSE;
}


void hb_pp_CloseInclude( void )
{
   PFILE pFile;

   /* we close the currently include file and continue */
   fclose( hb_comp_files.pLast->handle );
   hb_xfree( hb_comp_files.pLast->pBuffer );
   hb_xfree( hb_comp_files.pLast->szFileName );
   pFile               = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
   hb_xfree( hb_comp_files.pLast );
   hb_comp_files.pLast = pFile;
   if( hb_comp_files.pLast )
      hb_comp_iLine = hb_comp_files.pLast->iLine;
   hb_comp_files.iFiles--;
}

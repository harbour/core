/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler main file
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
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

#include "compiler.h"

#if defined(DOS) && defined(__BORLANDC__)
   #include <limits.h>
   extern unsigned _stklen = UINT_MAX;
#endif

/* /ES command line setting types */
#define HB_EXITLEVEL_DEFAULT    0
#define HB_EXITLEVEL_SETEXIT    1
#define HB_EXITLEVEL_DELTARGET  2

typedef enum
{
   LANG_C,                  /* C language (by default) <file.c> */
   LANG_OBJ32,              /* DOS/Windows 32 bits <file.obj> */
   LANG_JAVA,               /* Java <file.java> */
   LANG_PASCAL,             /* Pascal <file.pas> */
   LANG_RESOURCES,          /* Resources <file.rc> */
   LANG_PORT_OBJ            /* Portable objects <file.hrb> */
} LANGUAGES;                /* supported Harbour output languages */



extern int harbour_main( char *szName );
extern int Include( char * szFileName, PATHNAMES * pSearchPath );  /* end #include support */

  /* Following line added for preprocessor */
extern void Hbpp_init ( void );

/* output related functions */
extern void GenCCode( PHB_FNAME );      /* generates the C language output */
extern void GenJava( PHB_FNAME );       /* generates the Java language output */
extern void GenPascal( PHB_FNAME );     /* generates the Pascal language output */
extern void GenRC( PHB_FNAME );         /* generates the RC language output */
extern void GenPortObj( PHB_FNAME );    /* generates the portable objects */
#ifdef HARBOUR_OBJ_GENERATION
extern void GenObj32( PHB_FNAME );      /* generates OBJ 32 bits */
#endif

  /* Following two lines added for preprocessor */
extern BOOL _bPPO;       /* flag indicating, is ppo output needed */
extern FILE *yyppo;     /* output .ppo file */


static void PrintUsage( char * );
static void PrintCredits( void );
static BOOL SwitchCmp( char * szString, char * szSwitch );
static ULONG PackDateTime( void );
static void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */
static char * RESERVED_FUNC( char * );

void EXTERNAL_LINKAGE close_on_exit( void );

extern int iLine;       /* currently parsed file line number */

/* global variables */
FILES hb_comp_files;
FUNCTIONS hb_comp_functions, hb_comp_funcalls;
SYMBOLS hb_comp_symbols;


PFUNCTION hb_comp_pInitFunc;
PHB_FNAME hb_comp_pFileName = NULL;
BOOL hb_comp_bStartProc = TRUE;                 /* holds if we need to create the starting procedure */
BOOL hb_comp_bLineNumbers = TRUE;               /* holds if we need pcodes with line numbers */
BOOL hb_comp_bQuiet = FALSE;                    /* quiet mode */
BOOL hb_comp_bRestrictSymbolLength = FALSE;     /* generate 10 chars max symbols length */
BOOL hb_comp_bShortCuts = TRUE;                 /* .and. & .or. expressions shortcuts */
int  hb_comp_iWarnings = 0;                     /* enable parse warnings */
BOOL hb_comp_bAnyWarning = FALSE;               /* holds if there was any warning during the compilation process */
BOOL hb_comp_bAutoMemvarAssume = FALSE;         /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
BOOL hb_comp_bForceMemvars = FALSE;             /* holds if memvars are assumed when accesing undeclared variable (-v)*/
BOOL hb_comp_bDebugInfo = FALSE;                /* holds if generate debugger required info */
char hb_comp_szPrefix[ 20 ] = { '\0' };         /* holds the prefix added to the generated symbol init function name (in C output currently) */
BOOL hb_comp_bGenCVerbose = TRUE;               /* C code generation should be verbose (use comments) or not */
int  hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
PATHNAMES *hb_comp_pIncludePath = NULL;
int hb_comp_iFunctionCnt = 0;
int hb_comp_iErrorCount = 0;
char hb_comp_cVarType = ' ';               /* current declared variable type */
BOOL hb_comp_bDontGenLineNum = FALSE;   /* suppress line number generation */
LONG hb_comp_lLastPushPos = -1; /* position of last push pcode */
ULONG hb_comp_ulLastLinePos = 0;    /* position of last opcode with line number */
ULONG hb_comp_ulMessageFix = 0;  /* Position of the message which needs to be changed */
int hb_comp_iStaticCnt = 0;       /* number of defined statics variables on the PRG */
BOOL hb_comp_bExternal = FALSE;
PTR_LOOPEXIT hb_comp_pLoops = NULL;
PEXTERN hb_comp_pExterns = NULL;
int hb_comp_iVarScope = VS_LOCAL;   /* holds the scope for next variables to be defined */
                            /* different values for hb_comp_iVarScope */

/* local variables */
static PHB_FNAME s_pOutPath = NULL;
static BOOL s_bCredits = FALSE;                  /* print credits */
static BOOL s_bLogo = TRUE;                      /* print logo */
static BOOL s_bSyntaxCheckOnly = FALSE;          /* syntax check only */
static int  s_iLanguage = LANG_C;                /* default Harbour generated output language */


/* Table with parse errors */
char * hb_comp_szCErrors[] =
{
   "Statement not allowed outside of procedure or function",
   "Redefinition of procedure or function: \'%s\'",
   "Duplicate variable declaration: \'%s\'",
   "%s declaration follows executable statement",
   "Outer codeblock variable is out of reach: \'%s\'",
   "Invalid numeric format '.'",
   "Unterminated string: \'%s\'",
   "Redefinition of predefined function %s: \'%s\'",
   "Illegal variable \'%s\' initializer: \'%s\'",
   "ENDIF does not match IF",
   "ENDDO does not match WHILE",
   "ENDCASE does not match DO CASE",
   "NEXT does not match FOR",
   "ELSE does not match IF",
   "ELSEIF does not match IF",
   "Syntax error: \'%s\'",
   "Unclosed control structures",
   "%s statement with no loop in sight",
   "Syntax error: \'%s\' in: \'%s\'",
   "Incomplete statement: %s",
   "Incorrect number of arguments: %s %s",
   "Invalid lvalue: \'%s\'",
   "Invalid use of \'@\' (pass by reference): \'%s\'",
   "Formal parameters already declared",
   "Invalid %s from within of SEQUENCE code",
   "Unterminated array index",
   "Memory allocation error",
   "Memory reallocation error",
   "Freeing a NULL memory pointer",
   "Syntax error: \"%s at \'%s\'\"",
   "Jump offset too long",
   "Can't create output file: \'%s\'",
   "Can't create preprocessed output file: \'%s\'",
   "Bad command line option: \'%s\'",
   "Bad command line parameter: \'%s\'",
   "Invalid filename: \'%s\'",
   "Mayhem in CASE handler",
   "Operation not supported for this data type: \'%s\'",
   "Invalid alias expression: \'%s\'",
   "Invalid array index expression: \'%s\'",
   "Bound error: \'%s\'"
};

/* Table with parse warnings */
/* NOTE: The first character stores the warning's level that triggers this
 * warning. The warning's level is set by -w<n> command line option.
 */
char * hb_comp_szCWarnings[] =
{
   "1Ambiguous reference: \'%s\'",
   "1Ambiguous reference, assuming memvar: \'%s\'",
   "2Variable: \'%s\' declared but not used in function: \'%s\'",
   "2CodeBlock Parameter: \'%s\' declared but not used in function: \'%s\'",
   "1RETURN statement with no return value in function",
   "1Procedure returns value",
   "1Function \'%s\' does not end with RETURN statement",
   "3Incompatible type in assignment to: \'%s\' expected: \'%s\'",
   "3Incompatible operand type: \'%s\' expected: \'Logical\'",
   "3Incompatible operand type: \'%s\' expected: \'Numeric\'",
   "3Incompatible operand types: \'%s\' and: \'%s\'",
   "3Suspicious type in assignment to: \'%s\' expected: \'%s\'",
   "3Suspicious operand type: \'UnKnown\' expected: \'%s\'",
   "3Suspicious operand type: \'UnKnown\' expected: \'Logical\'",
   "3Suspicious operand type: \'UnKnown\' expected: \'Numeric\'",
   "0Meaningless use of expression: \'%s\'"
};

/* Table with reserved functions names
 * NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
*/
static const char * _szReservedFun[] = {
   "AADD"      ,
   "ABS"       ,
   "ASC"       ,
   "AT"        ,
   "BOF"       ,
   "BREAK"     ,
   "CDOW"      ,
   "CHR"       ,
   "CMONTH"    ,
   "COL"       ,
   "CTOD"      ,
   "DATE"      ,
   "DAY"       ,
   "DELETED"   ,
   "DEVPOS"    ,
   "DOW"       ,
   "DTOC"      ,
   "DTOS"      ,
   "EMPTY"     ,
   "EOF"       ,
   "EXP"       ,
   "FCOUNT"    ,
   "FIELDNAME" ,
   "FLOCK"     ,
   "FOUND"     ,
   "INKEY"     ,
   "INT"       ,
   "LASTREC"   ,
   "LEFT"      ,
   "LEN"       ,
   "LOCK"      ,
   "LOG"       ,
   "LOWER"     ,
   "LTRIM"     ,
   "MAX"       ,
   "MIN"       ,
   "MONTH"     ,
   "PCOL"      ,
   "PCOUNT"    ,
   "PROW"      ,
   "QSELF"     ,
   "RECCOUNT"  ,
   "RECNO"     ,
   "REPLICATE" ,
   "RLOCK"     ,
   "ROUND"     ,
   "ROW"       ,
   "RTRIM"     ,
   "SECONDS"   ,
   "SELECT"    ,
   "SETPOS"    ,
   "SETPOSBS"  ,
   "SPACE"     ,
   "SQRT"      ,
   "STR"       ,
   "SUBSTR"    ,
   "TIME"      ,
   "TRANSFORM" ,
   "TRIM"      ,
   "TYPE"      ,
   "UPPER"     ,
   "VAL"       ,
   "WORD"      ,
   "YEAR"
};
#define RESERVED_FUNCTIONS  sizeof( _szReservedFun ) / sizeof( char * )


int main( int argc, char * argv[] )
{
   int iStatus = 0;
   int iArg;
   BOOL bSkipGen;

   /* Check for the nologo switch /q0 before everything else. */

   for( iArg = 1; iArg < argc; iArg++ )
   {
      if( SwitchCmp( argv[ iArg ], "Q0" ) )
         s_bLogo = FALSE;

      else if( SwitchCmp( argv[ iArg ], "CREDITS" ) ||
               SwitchCmp( argv[ iArg ], "CREDIT" ) ||
               SwitchCmp( argv[ iArg ], "CREDI" ) ||
               SwitchCmp( argv[ iArg ], "CRED" ) )
         s_bCredits = TRUE;
   }

   if( s_bLogo )
   {
      printf( "Harbour Compiler, Build %i%s (%04d.%02d.%02d)\n",
         hb_build, hb_revision, hb_year, hb_month, hb_day );
      printf( "Copyright 1999, http://www.harbour-project.org\n" );
   }

   if( s_bCredits )
   {
      PrintCredits();
      return iStatus;
   }

   if( argc > 1 )
   {
      char szFileName[ _POSIX_PATH_MAX ];    /* filename to parse */
      char szPpoName[ _POSIX_PATH_MAX ];

      hb_comp_pFileName = NULL;
      s_pOutPath = NULL;

      Hbpp_init();  /* Initialization of preprocessor arrays */
      /* Command line options */
      for( iArg = 1; iArg < argc; iArg++ )
      {
         if( IS_OPT_SEP( argv[ iArg ][ 0 ] ) )
         {
            switch( argv[ iArg ][ 1 ] )
            {
               case '1':
                  if( argv[ iArg ][ 2 ] == '0' )
                     hb_comp_bRestrictSymbolLength = TRUE;
                  break;

               case 'a':
               case 'A':
                  /* variables declared by PRIVATE and PUBLIC statement are
                   * automatically assumed as MEMVAR
                   */
                  hb_comp_bAutoMemvarAssume = TRUE;
                  break;

               case 'b':
               case 'B':
                  hb_comp_bDebugInfo = TRUE;
                  hb_comp_bLineNumbers = TRUE;
                  break;

               case 'd':
               case 'D':   /* defines a Lex #define from the command line */
                  {
                     unsigned int i = 0;
                     char * szDefText = yy_strdup( argv[ iArg ] + 2 );
                     while( i < strlen( szDefText ) && szDefText[ i ] != '=' )
                        i++;
                     if( szDefText[ i ] != '=' )
                        AddDefine( szDefText, 0 );
                     else
                     {
                        szDefText[ i ] = '\0';
                        AddDefine( szDefText, szDefText + i + 1 );
                     }
                     free( szDefText );
                  }
                  break;

               case 'e':
               case 'E':

                  if( argv[ iArg ][ 2 ] == 's' ||
                      argv[ iArg ][ 2 ] == 'S' )
                  {
                     switch( argv[ iArg ][ 3 ] )
                     {
                        case '\0':
                        case '0':
                           hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT;
                           break;

                        case '1':
                           hb_comp_iExitLevel = HB_EXITLEVEL_SETEXIT;
                           break;

                        case '2':
                           hb_comp_iExitLevel = HB_EXITLEVEL_DELTARGET;
                           break;

                        default:
                           hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADOPTION, argv[ iArg ], NULL );
                     }
                  }
                  else
                     hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADOPTION, argv[ iArg ], NULL );

                  break;
               case 'g':
               case 'G':
                  switch( argv[ iArg ][ 2 ] )
                  {
                     case 'c':
                     case 'C':
                        s_iLanguage = LANG_C;

                        switch( argv[ iArg ][ 3 ] )
                        {
                           case '\0':
                           case '1':
                              hb_comp_bGenCVerbose = TRUE;
                              break;

                           case '0':
                              hb_comp_bGenCVerbose = FALSE;
                              break;

                           default:
                              hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADOPTION, argv[ iArg ], NULL );
                        }
                        break;

                     case 'f':
                     case 'F':
                        s_iLanguage = LANG_OBJ32;
                        break;

                     case 'j':
                     case 'J':
                        s_iLanguage = LANG_JAVA;
                        break;

                     case 'p':
                     case 'P':
                        s_iLanguage = LANG_PASCAL;
                        break;

                     case 'r':
                     case 'R':
                        s_iLanguage = LANG_RESOURCES;
                        break;

                     case 'h':
                     case 'H':
                        s_iLanguage = LANG_PORT_OBJ;
                        break;

                     default:
                        printf( "\nUnsupported output language option\n" );
                        exit( EXIT_FAILURE );
                  }
                  break;

               case 'i':
               case 'I':
                  {
                     char * pPath;
                     char * pDelim;
                     char * szInclude;

                     pPath = szInclude = yy_strdup( argv[ iArg ] + 2 );
                     while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
                     {
                        * pDelim = '\0';
                        AddSearchPath( pPath, &hb_comp_pIncludePath );
                        pPath = pDelim + 1;
                     }
                     AddSearchPath( pPath, &hb_comp_pIncludePath );
                  }
                  break;

               case 'l':
               case 'L':
                  hb_comp_bLineNumbers = FALSE;
                  break;

               case 'm':
               case 'M':
                  /* TODO: Implement this switch */
                  printf( "Not yet supported command line option: %s\n", argv[ iArg ] );
                  break;

               case 'n':
               case 'N':
                  hb_comp_bStartProc = FALSE;
                  break;

               case 'o':
               case 'O':
                  s_pOutPath = hb_fsFNameSplit( argv[ iArg ] + 2 );
                  break;

               /* Added for preprocessor needs */
               case 'p':
               case 'P':
                  _bPPO = TRUE;
                  break;

               case 'q':
               case 'Q':
                  hb_comp_bQuiet = TRUE;
                  break;

               case 'r':
               case 'R':
                  /* TODO: Implement this switch */
                  printf( "Not yet supported command line option: %s\n", argv[ iArg ] );
                  break;

               case 's':
               case 'S':
                  s_bSyntaxCheckOnly = TRUE;
                  break;

               case 't':
               case 'T':
                  /* TODO: Implement this switch */
                  printf( "Not yet supported command line option: %s\n", argv[ iArg ] );
                  break;

               case 'u':
               case 'U':
                  /* TODO: Implement this switch */
                  printf( "Not yet supported command line option: %s\n", argv[ iArg ] );
                  break;

               case 'v':
               case 'V':
                  /* All undeclared variables are assumed MEMVAR variables
                   */
                  hb_comp_bForceMemvars = TRUE;
                  break;

               case 'w':
               case 'W':
                  hb_comp_iWarnings = 1;
                  if( argv[ iArg ][ 2 ] )
                  {  /*there is -w<0,1,2,3> probably */
                     hb_comp_iWarnings = argv[ iArg ][ 2 ] - '0';
                     if( hb_comp_iWarnings < 0 || hb_comp_iWarnings > 3 )
                        hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADOPTION, argv[ iArg ], NULL );
                  }
                  break;

               case 'x':
               case 'X':
                  {
                     if( strlen( argv[ iArg ] + 2 ) == 0 )
                        sprintf( hb_comp_szPrefix, "%08lX_", PackDateTime() );
                     else
                     {
                        strncpy( hb_comp_szPrefix, argv[ iArg ] + 2, 16 );
                        hb_comp_szPrefix[ 16 ] = '\0';
                        strcat( hb_comp_szPrefix, "_" );
                     }
                  }
                  break;

#ifdef YYDEBUG
               case 'y':
               case 'Y':
                  yydebug = TRUE;
                  break;
#endif

               case 'z':
               case 'Z':
                  hb_comp_bShortCuts = FALSE;
                  break;

               default:
                  hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADOPTION, argv[ iArg ], NULL );
                  break;
            }
         }
         else if( argv[ iArg ][ 0 ] == '@' )
            /* TODO: Implement this switch */
            printf( "Not yet supported command line option: %s\n", argv[ iArg ] );
         else
         {
            if( hb_comp_pFileName )
               hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADPARAM, argv[ iArg ], NULL );
            else
            {
               hb_comp_pFileName = hb_fsFNameSplit( argv[ iArg ] );

               if( ! hb_comp_pFileName->szName )
                  hb_compGenError( hb_comp_szCErrors, 'E', ERR_BADFILENAME, argv[ iArg ], NULL );
            }
         }
      }

      if( hb_comp_pFileName )
      {
         if( !hb_comp_pFileName->szExtension )
            hb_comp_pFileName->szExtension = ".prg";
         hb_fsFNameMerge( szFileName, hb_comp_pFileName );
         if( _bPPO )
         {
            hb_comp_pFileName->szExtension = ".ppo";
            hb_fsFNameMerge( szPpoName, hb_comp_pFileName );
            yyppo = fopen( szPpoName, "w" );
            if( ! yyppo )
            {
               hb_compGenError( hb_comp_szCErrors, 'E', ERR_CREATE_PPO, szPpoName, NULL );
               return iStatus;
            }
         }
      }
      else
      {
         PrintUsage( argv[ 0 ] );
         return iStatus;
      }

      atexit( close_on_exit );

      hb_comp_files.iFiles     = 0;        /* initialize support variables */
      hb_comp_files.pLast      = NULL;
      hb_comp_functions.iCount = 0;
      hb_comp_functions.pFirst = NULL;
      hb_comp_functions.pLast  = NULL;
      hb_comp_funcalls.iCount  = 0;
      hb_comp_funcalls.pFirst  = NULL;
      hb_comp_funcalls.pLast   = NULL;
      hb_comp_symbols.iCount   = 0;
      hb_comp_symbols.pFirst   = NULL;
      hb_comp_symbols.pLast    = NULL;

      hb_comp_pInitFunc = NULL;
      hb_comp_bAnyWarning = FALSE;

      if( Include( szFileName, NULL ) )
      {
         char * szInclude = getenv( "INCLUDE" );

         if( szInclude )
         {
            char * pPath;
            char * pDelim;

            pPath = szInclude = yy_strdup( szInclude );
            while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
            {
               *pDelim = '\0';
               AddSearchPath( pPath, &hb_comp_pIncludePath );
               pPath = pDelim + 1;
            }
            AddSearchPath( pPath, &hb_comp_pIncludePath );
         }

         /*
          * Start processing
          */
         harbour_main( hb_comp_pFileName->szName );

         bSkipGen = FALSE;

         if( hb_comp_bAnyWarning )
         {
            if( hb_comp_iExitLevel == HB_EXITLEVEL_SETEXIT )
               iStatus = 1;
            if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
            {
               iStatus = 1;
               bSkipGen = TRUE;
               printf( "\nNo code generated\n" );
            }
         }

         if( ! s_bSyntaxCheckOnly && ! bSkipGen && ( hb_comp_iErrorCount == 0 ) )
         {
            hb_comp_pFileName->szPath = NULL;
            hb_comp_pFileName->szExtension = NULL;

            /* we create a the output file */
            if( s_pOutPath )
            {
               if( s_pOutPath->szPath )
                  hb_comp_pFileName->szPath = s_pOutPath->szPath;
               if( s_pOutPath->szName )
               {
                  hb_comp_pFileName->szName = s_pOutPath->szName;
                  if( s_pOutPath->szExtension )
                     hb_comp_pFileName->szExtension = s_pOutPath->szExtension;
               }
            }

            switch( s_iLanguage )
            {
               case LANG_C:
                  GenCCode( hb_comp_pFileName );
                  break;

               case LANG_OBJ32:
#ifdef HARBOUR_OBJ_GENERATION
                  GenObj32( hb_comp_pFileName );
#endif
                  break;

               case LANG_JAVA:
                  GenJava( hb_comp_pFileName );
                  break;

               case LANG_PASCAL:
                  GenPascal( hb_comp_pFileName );
                  break;

               case LANG_RESOURCES:
                  GenRC( hb_comp_pFileName );
                  break;

               case LANG_PORT_OBJ:
                  GenPortObj( hb_comp_pFileName );
                  break;
            }
         }

         if( _bPPO )
            fclose( yyppo );
      }
      else
      {
         printf( "Cannot open input file: %s\n", szFileName );
         iStatus = 1;
      }
      hb_xfree( ( void * ) hb_comp_pFileName );
      if( s_pOutPath )
         hb_xfree( s_pOutPath );

      if( hb_comp_iErrorCount > 0 )
         iStatus = EXIT_FAILURE;
   }
   else
      PrintUsage( argv[ 0 ] );

   return iStatus;
}

/*
 * Prints available options
*/
static void PrintUsage( char * szSelf )
{
   printf( "\nSyntax:  %s <file[.prg]> [options]"
           "\n"
           "\nOptions:  /a               automatic memvar declaration"
           "\n          /b               debug info"
           "\n          /d<id>[=<val>]   #define <id>"
           "\n          /es[<level>]     set exit severity"
           "\n          /g<type>         output type generated is <type> (see below)"
           "\n          /gc[<type>]      output type: C source (.c) (default)"
           "\n                           <type>: 0=without comments, 1=normal (default)"
#ifdef HARBOUR_OBJ_GENERATION
           "\n          /gf              output type: Windows/DOS OBJ32 (.obj)"
#endif
           "\n          /gh              output type: Harbour Portable Object (.hrb)"
           "\n          /gj              output type: Java source (.java)"
           "\n          /gp              output type: Pascal source (.pas)"
           "\n          /gr              output type: Windows resource (.rc)"
           "\n          /i<path>         add #include file search path"
           "\n          /l               suppress line number information"
/* TODO:   "\n          /m               compile module only" */
           "\n          /n               no implicit starting procedure"
           "\n          /o<path>         object file drive and/or path"
           "\n          /p               generate pre-processed output (.ppo) file"
           "\n          /q               quiet"
           "\n          /q0              quiet and don't display program header"
/* TODO:   "\n          /r[<lib>]        request linker to search <lib> (or none)" */
           "\n          /s               syntax check only"
/* TODO:   "\n          /t<path>         path for temp file creation" */
/* TODO:   "\n          /u[<file>]       use command def set in <file> (or none)" */
           "\n          /v               variables are assumed M->"
           "\n          /w[<level>]      set warning level number (0..3, default 1)"
           "\n          /x[<prefix>]     set symbol init function name prefix (for .c only)"
#ifdef YYDEBUG
           "\n          /y               trace lex & yacc activity"
#endif
           "\n          /z               suppress shortcutting (.and. & .or.)"
           "\n          /10              restrict symbol length to 10 characters"
/* TODO:   "\n           @<file>         compile list of modules in <file>" */
           "\n"
           , szSelf );
}

/*
 * Prints credits
*/
static void PrintCredits( void )
{
   printf( "\nCredits:  The Harbour Team at www.harbour-project.com"
           );
}


#if defined(__IBMCPP__) || defined(_MSC_VER)
int isatty( int handle )
{
   return ( handle < 4 ) ? 1 : 0;
}
#endif

static BOOL SwitchCmp( char * szString, char * szSwitch )
{
   if( IS_OPT_SEP( *szString ) )
   {
      szString++;

      if( strlen( szString ) == strlen( szSwitch ) )
      {
         while( *szString != '\0' )
         {
            if( toupper( *szString ) != *szSwitch )
               return FALSE;

            szString++;
            szSwitch++;
         }

         return TRUE;
      }
   }

   return FALSE;
}

/* NOTE: Making the date and time info to fit into 32 bits can only be done
         in a "lossy" way, in practice that means it's not possible to unpack
         the exact date/time info from the resulting ULONG. Since the year
         is only stored in 6 bits, 1980 will result in the same bit pattern
         as 2044. The purpose of this value is only used to *differenciate*
         between the dates ( the exact dates are not significant ), so this can
         be used here without problems. */

/* 76543210765432107654321076543210
   |.......|.......|.......|.......
   |____|                               Year    6 bits
         |__|                           Month   4 bits
             |___|                      Day     5 bits
                  |___|                 Hour    5 bits
                       |____|           Minute  6 bits
                             |____|     Second  6 bits */

static ULONG PackDateTime( void )
{
   BYTE szString[ 4 ];
   BYTE nValue;

   time_t t;
   struct tm * oTime;

   time( &t );
   oTime = localtime( &t );

   nValue = ( BYTE ) ( ( ( oTime->tm_year + 1900 ) - 1980 ) & ( 2 ^ 6 ) ) ; /* 6 bits */
   szString[ 0 ]  = nValue << 2;
   nValue = ( BYTE ) ( oTime->tm_mon + 1 ); /* 4 bits */
   szString[ 0 ] |= nValue >> 2;
   szString[ 1 ]  = nValue << 6;
   nValue = ( BYTE ) ( oTime->tm_mday ); /* 5 bits */
   szString[ 1 ] |= nValue << 1;

   nValue = ( BYTE ) oTime->tm_hour; /* 5 bits */
   szString[ 1 ]  = nValue >> 4;
   szString[ 2 ]  = nValue << 4;
   nValue = ( BYTE ) oTime->tm_min; /* 6 bits */
   szString[ 2 ] |= nValue >> 2;
   szString[ 3 ]  = nValue << 6;
   nValue = ( BYTE ) oTime->tm_sec; /* 6 bits */
   szString[ 3 ] |= nValue;

   return HB_MKLONG( szString[ 3 ], szString[ 2 ], szString[ 1 ], szString[ 0 ] );
}

/*
 * Function that adds specified path to the list of pathnames to search
 */
void AddSearchPath( char * szPath, PATHNAMES * * pSearchList )
{
   PATHNAMES * pPath = *pSearchList;

   if( pPath )
   {
      while( pPath->pNext )
      pPath = pPath->pNext;
      pPath->pNext = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
      pPath = pPath->pNext;
   }
   else
   {
      *pSearchList = pPath = ( PATHNAMES * ) hb_xgrab( sizeof( PATHNAMES ) );
   }
   pPath->pNext  = NULL;
   pPath->szPath = szPath;
}

void EXTERNAL_LINKAGE close_on_exit( void )
{
   PFILE pFile = hb_comp_files.pLast;

   while( pFile )
   {
      fclose( pFile->handle );
      pFile = ( PFILE ) pFile->pPrev;
   }
}



/* ------------------------------------------------------------------------- */

#define IS_PATH_SEP( c ) ( strchr( OS_PATH_DELIMITER_LIST, ( c ) ) != NULL )

/* Split given filename into path, name and extension */
PHB_FNAME hb_fsFNameSplit( char * szFileName )
{
   PHB_FNAME pFileName = ( PHB_FNAME ) hb_xgrab( sizeof( HB_FNAME ) );

   int iLen = strlen( szFileName );
   int iSlashPos;
   int iDotPos;
   int iPos;

   pFileName->szPath =
   pFileName->szName =
   pFileName->szExtension = NULL;

   iSlashPos = iLen - 1;
   iPos = 0;

   while( iSlashPos >= 0 && !IS_PATH_SEP( szFileName[ iSlashPos ] ) )
      --iSlashPos;

   if( iSlashPos == 0 )
   {
      /* root path -> \filename */
      pFileName->szBuffer[ 0 ] = OS_PATH_DELIMITER;
      pFileName->szBuffer[ 1 ] = '\0';
      pFileName->szPath = pFileName->szBuffer;
      iPos = 2; /* first free position after the slash */
   }
   else if( iSlashPos > 0 )
   {
      /* If we are after a drive letter let's keep the following backslash */
      if( IS_PATH_SEP( ':' ) &&
         ( szFileName[ iSlashPos ] == ':' || szFileName[ iSlashPos - 1 ] == ':' ) )
      {
         /* path with separator -> d:\path\filename or d:path\filename */
         memcpy( pFileName->szBuffer, szFileName, iSlashPos + 1 );
         pFileName->szBuffer[ iSlashPos + 1 ] = '\0';
         iPos = iSlashPos + 2; /* first free position after the slash */
      }
      else
      {
         /* path with separator -> path\filename */
         memcpy( pFileName->szBuffer, szFileName, iSlashPos );
         pFileName->szBuffer[ iSlashPos ] = '\0';
         iPos = iSlashPos + 1; /* first free position after the slash */
      }

      pFileName->szPath = pFileName->szBuffer;
   }

   iDotPos = iLen - 1;
   while( iDotPos > iSlashPos && szFileName[ iDotPos ] != '.' )
      --iDotPos;

   if( ( iDotPos - iSlashPos ) > 1 )
   {
      /* the dot was found
       * and there is at least one character between a slash and a dot
       */
      if( iDotPos == iLen - 1 )
      {
         /* the dot is the last character - use it as extension name */
         pFileName->szExtension = pFileName->szBuffer + iPos;
         pFileName->szBuffer[ iPos++ ] = '.';
         pFileName->szBuffer[ iPos++ ] = '\0';
      }
      else
      {
         pFileName->szExtension = pFileName->szBuffer + iPos;
         /* copy rest of the string with terminating ZERO character */
         memcpy( pFileName->szExtension, szFileName + iDotPos + 1, iLen - iDotPos );
         iPos += iLen - iDotPos;
      }
   }
   else
      /* there is no dot in the filename or it is  '.filename' */
      iDotPos = iLen;

   if( ( iDotPos - iSlashPos - 1 ) > 0 )
   {
      pFileName->szName = pFileName->szBuffer + iPos;
      memcpy( pFileName->szName, szFileName + iSlashPos + 1, iDotPos - iSlashPos - 1 );
      pFileName->szName[ iDotPos - iSlashPos - 1 ] = '\0';
   }

/* DEBUG
   printf( "\nFilename: %s\n", szFileName );
   printf( "\n  szPath: %s\n", pFileName->szPath );
   printf( "\n  szName: %s\n", pFileName->szName );
   printf( "\n   szExt: %s\n", pFileName->szExtension );
*/

   return pFileName;
}

/* This function joins path, name and extension into a string with a filename */
char * hb_fsFNameMerge( char * szFileName, PHB_FNAME pFileName )
{
   if( pFileName->szPath && pFileName->szPath[ 0 ] )
   {
      /* we have not empty path specified */
      int iLen = strlen( pFileName->szPath );

      strcpy( szFileName, pFileName->szPath );

      /* if the path is a root directory then we don't need to add path separator */
      if( !( IS_PATH_SEP( pFileName->szPath[ 0 ] ) && pFileName->szPath[ 0 ] == '\0' ) )
      {
         /* add the path separator only in cases:
          *  when a name doesn't start with it
          *  when the path doesn't end with it
          */
         if( !( IS_PATH_SEP( pFileName->szName[ 0 ] ) || IS_PATH_SEP( pFileName->szPath[ iLen-1 ] ) ) )
         {
            szFileName[ iLen++ ] = OS_PATH_DELIMITER;
            szFileName[ iLen ] = '\0';
         }
      }
      if( pFileName->szName )
         strcpy( szFileName + iLen, pFileName->szName );
   }
   else
   {
      if( pFileName->szName )
         strcpy( szFileName, pFileName->szName );
   }

   if( pFileName->szExtension )
   {
      int iLen = strlen( szFileName );

      if( !( pFileName->szExtension[ 0 ] == '.' || szFileName[ iLen - 1 ] == '.') )
      {
         /* add extension separator only when extansion doesn't contain it */
         szFileName[ iLen++ ] = '.';
         szFileName[ iLen ] = '\0';
      }
      strcpy( szFileName + iLen, pFileName->szExtension );
   }

/* DEBUG
   printf( "\nMERGE:\n" );
   printf( "\n  szPath: %s\n", pFileName->szPath );
   printf( "\n  szName: %s\n", pFileName->szName );
   printf( "\n   szExt: %s\n", pFileName->szExtension );
   printf( "\nFilename result: %s\n", szFileName );
*/

   return szFileName;
}

/* ------------------------------------------------------------------------- */

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize );

   if( ! pMem )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_MEMALLOC, NULL, NULL );

   return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   void * pResult = realloc( pMem, ulSize );

   if( ! pResult )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_MEMREALLOC, NULL, NULL );

   return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
      free( pMem );
   else
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_MEMFREE, NULL, NULL );
}

char * yy_strupr( char * p )
{
   char * p1;

   for( p1 = p; * p1; p1++ )
      * p1 = toupper( * p1 );

   return p;
}

char * yy_strdup( char * p )
{
   char * pDup;
   int iLen;

   iLen = strlen( p ) + 1;
   pDup = ( char * ) hb_xgrab( iLen );
   memcpy( pDup, p, iLen );

   return pDup;
}

/* ------------------------------------------------------------------------- */

void hb_compGenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
   if( hb_comp_files.pLast != NULL && hb_comp_files.pLast->szFileName != NULL )
      printf( "\r%s(%i) ", hb_comp_files.pLast->szFileName, iLine );

   printf( "Error %c%04i  ", cPrefix, iError );
   printf( _szErrors[ iError - 1 ], szError1, szError2 );
   printf( "\n" );

   hb_comp_iErrorCount++;
/*   exit( EXIT_FAILURE ); */
}

void hb_compGenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2)
{
   char *szText = _szWarnings[ iWarning - 1 ];

   if( (szText[ 0 ] - '0') <= hb_comp_iWarnings )
   {
      printf( "\r%s(%i) ", hb_comp_files.pLast->szFileName, iLine );
      printf( "Warning %c%04i  ", cPrefix, iWarning );
      printf( szText + 1, szWarning1, szWarning2 );
      printf( "\n" );

      hb_comp_bAnyWarning = TRUE;    /* report warnings at exit */
   }
}

/* ------------------------------------------------------------------------- */

/* checks if passed string is a reserved function name
 */
static char * RESERVED_FUNC( char * szName )
{
   USHORT wNum = 0;
   int iFound = 1;

   while( wNum < RESERVED_FUNCTIONS && iFound )
   {
      /* Compare first 4 characters
      * If they are the same then compare the whole name
      * SECO() is not allowed because of Clipper function SECONDS()
      * however SECO32() is a valid name.
      */
      iFound = strncmp( szName, _szReservedFun[ wNum ], 4 );
      if( iFound == 0 )
         iFound = strncmp( szName, _szReservedFun[ wNum ], strlen( szName ) );
      ++wNum;
   }
   if( iFound )
      return NULL;
   else
      return (char *) _szReservedFun[ wNum - 1 ];
}

/* NOTE: iMinParam = -1, means no checking */
/*       iMaxParam = -1, means no upper limit */

typedef struct
{
   char * cFuncName;                /* function name              */
   int    iMinParam;                /* min no of parms it needs   */
   int    iMaxParam;                /* max no of parms need       */
} FUNCINFO, * PFUNCINFO;

static FUNCINFO _StdFun[] =
{
   { "AADD"      , 2,  2 },
   { "ABS"       , 1,  1 },
   { "ASC"       , 1,  1 },
   { "AT"        , 2,  2 },
   { "BOF"       , 0,  0 },
   { "BREAK"     , 0,  1 },
   { "CDOW"      , 1,  1 },
   { "CHR"       , 1,  1 },
   { "CMONTH"    , 1,  1 },
   { "COL"       , 0,  0 },
   { "CTOD"      , 1,  1 },
   { "DATE"      , 0,  0 },
   { "DAY"       , 1,  1 },
   { "DELETED"   , 0,  0 },
   { "DEVPOS"    , 2,  2 },
   { "DOW"       , 1,  1 },
   { "DTOC"      , 1,  1 },
   { "DTOS"      , 1,  1 },
   { "EMPTY"     , 1,  1 },
   { "EOF"       , 0,  0 },
   { "EVAL"      , 1, -1 },
   { "EXP"       , 1,  1 },
   { "FCOUNT"    , 0,  0 },
   { "FIELDNAME" , 1,  1 },
   { "FILE"      , 1,  1 },
   { "FLOCK"     , 0,  0 },
   { "FOUND"     , 0,  0 },
   { "INKEY"     , 0,  2 },
   { "INT"       , 1,  1 },
   { "LASTREC"   , 0,  0 },
   { "LEFT"      , 2,  2 },
   { "LEN"       , 1,  1 },
   { "LOCK"      , 0,  0 },
   { "LOG"       , 1,  1 },
   { "LOWER"     , 1,  1 },
   { "LTRIM"     , 1,  1 },
   { "MAX"       , 2,  2 },
   { "MIN"       , 2,  2 },
   { "MONTH"     , 1,  1 },
   { "PCOL"      , 0,  0 },
   { "PCOUNT"    , 0,  0 },
   { "PROW"      , 0,  0 },
   { "RECCOUNT"  , 0,  0 },
   { "RECNO"     , 0,  0 },
   { "REPLICATE" , 2,  2 },
   { "RLOCK"     , 0,  0 },
   { "ROUND"     , 2,  2 },
   { "ROW"       , 0,  0 },
   { "RTRIM"     , 1,  2 }, /* Second parameter is a Harbour extension */
   { "SECONDS"   , 0,  0 },
   { "SELECT"    , 0,  1 },
   { "SETPOS"    , 2,  2 },
   { "SETPOSBS"  , 0,  0 },
   { "SPACE"     , 1,  1 },
   { "SQRT"      , 1,  1 },
   { "STR"       , 1,  3 },
   { "SUBSTR"    , 2,  3 },
   { "TIME"      , 0,  0 },
   { "TRANSFORM" , 2,  2 },
   { "TRIM"      , 1,  2 }, /* Second parameter is a Harbour extension */
   { "TYPE"      , 1,  1 },
   { "UPPER"     , 1,  1 },
   { "VAL"       , 1,  1 },
   { "VALTYPE"   , 1,  1 },
   { "WORD"      , 1,  1 },
   { "YEAR"      , 1,  1 },
   { 0           , 0,  0 }
};

void hb_compCheckArgs( char * szFuncCall, int iArgs )
{
   FUNCINFO * f = _StdFun;
   int i = 0;
   int iPos = -1;
   int iCmp;

   while( f[ i ].cFuncName )
   {
      iCmp = strncmp( szFuncCall, f[ i ].cFuncName, 4 );
      if( iCmp == 0 )
         iCmp = strncmp( szFuncCall, f[ i ].cFuncName, strlen( szFuncCall ) );
      if( iCmp == 0 )
      {
         iPos = i;
         break;
      }
      else
         ++i;
   }

   if( iPos >= 0 && ( f[ iPos ].iMinParam != -1 ) )
   {
      if( iArgs < f[ iPos ].iMinParam || ( f[ iPos ].iMaxParam != -1 && iArgs > f[ iPos ].iMaxParam ) )
      {
         char szMsg[ 40 ];

         if( f[ iPos ].iMaxParam == -1 )
            sprintf( szMsg, "\nPassed: %i, expected: at least %i", iArgs, f[ iPos ].iMinParam );
         else if( f[ iPos ].iMinParam == f[ iPos ].iMaxParam )
            sprintf( szMsg, "\nPassed: %i, expected: %i", iArgs, f[ iPos ].iMinParam );
         else
            sprintf( szMsg, "\nPassed: %i, expected: %i - %i", iArgs, f[ iPos ].iMinParam, f[ iPos ].iMaxParam );

         hb_compGenError( hb_comp_szCErrors, 'E', ERR_CHECKING_ARGS, szFuncCall, szMsg );

         /* Clipper way */
         /* hb_compGenError( hb_comp_szCErrors, 'E', ERR_CHECKING_ARGS, szFuncCall, NULL ); */
      }
   }
}

/* ------------------------------------------------------------------------- */
/**                          ACTIONS                                        **/
/* ------------------------------------------------------------------------- */

/*
 * This function adds the name of called function into the list
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
PFUNCTION hb_compAddFunCall( char * szFunctionName )
{
   PFUNCTION pFunc = FunctionNew( szFunctionName, 0 );

   if( ! hb_comp_funcalls.iCount )
   {
      hb_comp_funcalls.pFirst = pFunc;
      hb_comp_funcalls.pLast  = pFunc;
   }
   else
   {
      ( ( PFUNCTION ) hb_comp_funcalls.pLast )->pNext = pFunc;
      hb_comp_funcalls.pLast = pFunc;
   }
   hb_comp_funcalls.iCount++;

   return pFunc;
}

/*
 * This function adds the name of external symbol into the list of externals
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
void hb_compAddExtern( char * szExternName ) /* defines a new extern name */
{
   PEXTERN pExtern = ( PEXTERN ) hb_xgrab( sizeof( _EXTERN ) ), pLast;

   pExtern->szName = szExternName;
   pExtern->pNext  = NULL;

   if( hb_comp_pExterns == NULL )
      hb_comp_pExterns = pExtern;
   else
   {
      pLast = hb_comp_pExterns;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pExtern;
   }
}

void hb_compAddVar( char * szVarName, char cValueType )
{
   PVAR pVar, pLastVar;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 && hb_comp_iVarScope == VS_LOCAL )
   {
      /* Variable declaration is outside of function/procedure body.
         In this case only STATIC and PARAMETERS variables are allowed. */
      --iLine;
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_OUTSIDE, NULL, NULL );
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( ( hb_comp_functions.pLast->bFlags & FUN_STATEMENTS ) && !( hb_comp_iVarScope == VS_FIELD || ( hb_comp_iVarScope & VS_MEMVAR ) ) )
   {
      --iLine;
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_FOLLOWS_EXEC, ( hb_comp_iVarScope == VS_LOCAL ? "LOCAL" : "STATIC" ), NULL );
   }

   /* When static variable is added then hb_comp_functions.pLast points to function
    * that will initialise variables. The function where variable is being
    * defined is stored in pOwner member.
    */
   if( hb_comp_iVarScope == VS_STATIC )
      pFunc = pFunc->pOwner;

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      CheckDuplVars( pFunc->pFields, szVarName, hb_comp_iVarScope );
      CheckDuplVars( pFunc->pStatics, szVarName, hb_comp_iVarScope );
      if( !( hb_comp_iVarScope == VS_PRIVATE || hb_comp_iVarScope == VS_PUBLIC ) )
         CheckDuplVars( pFunc->pMemvars, szVarName, hb_comp_iVarScope );
   }
   else
      /* variable defined in a codeblock */
      hb_comp_iVarScope = VS_PARAMETER;
   CheckDuplVars( pFunc->pLocals, szVarName, hb_comp_iVarScope );

   pVar = ( PVAR ) hb_xgrab( sizeof( VAR ) );
   pVar->szName = szVarName;
   pVar->szAlias = NULL;
   pVar->cType = hb_comp_cVarType;
   pVar->iUsed = 0;
   pVar->pNext = NULL;

   if( hb_comp_iVarScope & VS_MEMVAR )
   {
      PCOMSYMBOL pSym;
      USHORT wPos;

      if( hb_comp_bAutoMemvarAssume || hb_comp_iVarScope == VS_MEMVAR )
      {
         /* add this variable to the list of MEMVAR variables
          */
         if( ! pFunc->pMemvars )
            pFunc->pMemvars = pVar;
         else
         {
            pLastVar = pFunc->pMemvars;
            while( pLastVar->pNext )
               pLastVar = pLastVar->pNext;
            pLastVar->pNext = pVar;
         }
      }

      switch( hb_comp_iVarScope )
      {
         case VS_MEMVAR:
            /* variable declared in MEMVAR statement */
            break;
         case ( VS_PARAMETER | VS_PRIVATE ):
            {
               BOOL bNewParameter = FALSE;

               if( ++hb_comp_functions.pLast->wParamNum > hb_comp_functions.pLast->wParamCount )
               {
                  hb_comp_functions.pLast->wParamCount = hb_comp_functions.pLast->wParamNum;
                  bNewParameter = TRUE;
               }

               pSym = hb_compGetSymbol( szVarName, &wPos ); /* check if symbol exists already */
               if( ! pSym )
                  pSym = hb_compAddSymbol( yy_strdup( szVarName ), &wPos );
               pSym->cScope |= VS_MEMVAR;
               hb_compGenPCode3( HB_P_PARAMETER, HB_LOBYTE( wPos ), HB_HIBYTE( wPos ) );
               hb_compGenPCode1( HB_LOBYTE( hb_comp_functions.pLast->wParamNum ) );

               /* Add this variable to the local variables list - this will
                * allow to use the correct positions for real local variables.
                * The name of variable have to be hidden because we should
                * not find this name on the local variables list.
                * We have to use the new structure because it is used in
                * memvars list already.
                */
               if( bNewParameter )
               {
                  pVar = ( PVAR ) hb_xgrab( sizeof( VAR ) );
                  pVar->szName = yy_strdup( szVarName );
                  pVar->szAlias = NULL;
                  pVar->cType = cValueType;
                  pVar->iUsed = 0;
                  pVar->pNext = NULL;
                  pVar->szName[ 0 ] ='!';
                  if( ! pFunc->pLocals )
                     pFunc->pLocals = pVar;
                  else
                  {
                     pLastVar = pFunc->pLocals;
                     while( pLastVar->pNext )
                        pLastVar = pLastVar->pNext;
                     pLastVar->pNext = pVar;
                  }
               }
            }
            break;
         case VS_PRIVATE:
            {
               hb_compPushSymbol( yy_strdup( "__MVPRIVATE" ), 1);
               hb_compGenPushNil();
               hb_compPushSymbol( yy_strdup( szVarName ), 0 );
               hb_compGenDoProc( 1 );
               pSym = hb_compGetSymbol( szVarName, NULL );
               pSym->cScope |= VS_MEMVAR;
            }
            break;
         case VS_PUBLIC:
            {
               hb_compPushSymbol( yy_strdup( "__MVPUBLIC" ), 1);
               hb_compGenPushNil();
               hb_compPushSymbol( yy_strdup( szVarName ), 0 );
               hb_compGenDoProc( 1 );
               pSym = hb_compGetSymbol( szVarName, NULL );
               pSym->cScope |= VS_MEMVAR;
            }
            break;
      }
   }
   else
   {
      switch( hb_comp_iVarScope )
      {
         case VS_LOCAL:
         case VS_PARAMETER:
            {
               USHORT wLocal = 1;

               if( ! pFunc->pLocals )
                  pFunc->pLocals = pVar;
               else
               {
                  pLastVar = pFunc->pLocals;
                  while( pLastVar->pNext )
                  {
                     pLastVar = pLastVar->pNext;
                     wLocal++;
                  }
                  pLastVar->pNext = pVar;
               }
               if( hb_comp_iVarScope == VS_PARAMETER )
               {
                  ++hb_comp_functions.pLast->wParamCount;
                  hb_comp_functions.pLast->bFlags |= FUN_USES_LOCAL_PARAMS;
               }
               if( hb_comp_bDebugInfo )
               {
                  hb_compGenPCode3( HB_P_LOCALNAME, HB_LOBYTE( wLocal ), HB_HIBYTE( wLocal ) );
                  hb_compGenPCodeN( ( BYTE * )szVarName, strlen( szVarName ) );
                  hb_compGenPCode1( 0 );
               }
            }
            break;

         case VS_STATIC:
            if( ! pFunc->pStatics )
               pFunc->pStatics = pVar;
            else
            {
               pLastVar = pFunc->pStatics;
               while( pLastVar->pNext )
                  pLastVar = pLastVar->pNext;
               pLastVar->pNext = pVar;
            }
            break;

         case VS_FIELD:
            if( ! pFunc->pFields )
               pFunc->pFields = pVar;
            else
            {
               pLastVar = pFunc->pFields;
               while( pLastVar->pNext )
                  pLastVar = pLastVar->pNext;
               pLastVar->pNext = pVar;
            }
            break;
      }

   }
}

PCOMSYMBOL hb_compAddSymbol( char * szSymbolName, USHORT * pwPos )
{
   PCOMSYMBOL pSym = ( PCOMSYMBOL ) hb_xgrab( sizeof( COMSYMBOL ) );

   pSym->szName = szSymbolName;
   pSym->cScope = 0;
   pSym->cType = hb_comp_cVarType;
   pSym->pNext = NULL;

   if( ! hb_comp_symbols.iCount )
   {
      hb_comp_symbols.pFirst = pSym;
      hb_comp_symbols.pLast  = pSym;
   }
   else
   {
      ( ( PCOMSYMBOL ) hb_comp_symbols.pLast )->pNext = pSym;
      hb_comp_symbols.pLast = pSym;
   }
   hb_comp_symbols.iCount++;

   if( pwPos )
      *pwPos = hb_comp_symbols.iCount;

   /*if( hb_comp_cVarType != ' ') printf("\nDeclared %s as type %c at symbol %i\n", szSymbolName, hb_comp_cVarType, hb_comp_symbols.iCount );*/
   return pSym;
}

void Duplicate( void )
{
   hb_compGenPCode1( HB_P_DUPLICATE );
}

void DupPCode( ULONG ulStart ) /* duplicates the current generated pcode from an offset */
{
   ULONG w, wEnd = hb_comp_functions.pLast->lPCodePos - ulStart;

   for( w = 0; w < wEnd; w++ )
      hb_compGenPCode1( hb_comp_functions.pLast->pCode[ ulStart + w ] );
}

/*
 * Function generates passed pcode for passed database field
 */
void FieldPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pVar;

   pVar = hb_compGetSymbol( szVarName, &wVar );
   if( ! pVar )
      pVar = hb_compAddSymbol( szVarName, &wVar );
   pVar->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

/*
 * This function creates and initialises the _FUNC structure
 */
PFUNCTION FunctionNew( char * szName, HB_SYMBOLSCOPE cScope )
{
   PFUNCTION pFunc;

   pFunc = ( PFUNCTION ) hb_xgrab( sizeof( _FUNC ) );
   pFunc->szName       = szName;
   pFunc->cScope       = cScope;
   pFunc->pLocals      = NULL;
   pFunc->pStatics     = NULL;
   pFunc->pFields      = NULL;
   pFunc->pMemvars     = NULL;
   pFunc->pCode        = NULL;
   pFunc->lPCodeSize   = 0;
   pFunc->lPCodePos    = 0;
   pFunc->pNext        = NULL;
   pFunc->wParamCount  = 0;
   pFunc->wParamNum    = 0;
   pFunc->iStaticsBase = hb_comp_iStaticCnt;
   pFunc->pOwner       = NULL;
   pFunc->bFlags       = 0;

   hb_comp_lLastPushPos = -1;

   return pFunc;
}

/*
 * Stores a Clipper defined function/procedure
 * szFunName - name of a function
 * cScope    - scope of a function
 * iType     - FUN_PROCEDURE if a procedure or 0
 */
void hb_compFunDef( char * szFunName, HB_SYMBOLSCOPE cScope, int iType )
{
   PCOMSYMBOL   pSym;
   PFUNCTION pFunc;
   char * szFunction;

   FixReturns();    /* fix all previous function returns offsets */

   pFunc = GetFunction( szFunName );
   if( pFunc )
   {
      /* The name of a function/procedure is already defined */
      if( ( pFunc != hb_comp_functions.pFirst ) || hb_comp_bStartProc )
         /* it is not a starting procedure that was automatically created */
         hb_compGenError( hb_comp_szCErrors, 'E', ERR_FUNC_DUPL, szFunName, NULL );
   }

   szFunction = RESERVED_FUNC( szFunName );
   if( szFunction && !( hb_comp_functions.iCount==0 && !hb_comp_bStartProc ) )
   {
      /* We are ignoring it when it is the name of PRG file and we are
       * not creating implicit starting procedure
       */
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_FUNC_RESERVED, szFunction, szFunName );
   }

   hb_comp_iFunctionCnt++;

   pSym = hb_compGetSymbol( szFunName, NULL );
   if( ! pSym )
      /* there is not a symbol on the symbol table for this function name */
      pSym = hb_compAddSymbol( szFunName, NULL );

   if( cScope != FS_PUBLIC )
/*    pSym->cScope = FS_PUBLIC; */
/* else */
      pSym->cScope |= cScope; /* we may have a non public function and a object message */

   pFunc = FunctionNew( szFunName, cScope );
   pFunc->bFlags |= iType;

   if( hb_comp_functions.iCount == 0 )
   {
      hb_comp_functions.pFirst = pFunc;
      hb_comp_functions.pLast  = pFunc;
   }
   else
   {
      hb_comp_functions.pLast->pNext = pFunc;
      hb_comp_functions.pLast = pFunc;
   }
   hb_comp_functions.iCount++;

   hb_comp_ulLastLinePos = 0;   /* optimization of line numbers opcode generation */

   hb_compGenPCode3( HB_P_FRAME, 0, 0 );   /* frame for locals and parameters */
   hb_compGenPCode3( HB_P_SFRAME, 0, 0 );     /* frame for statics variables */

   if( hb_comp_bDebugInfo )
   {
      hb_compGenPCode1( HB_P_MODULENAME );
      hb_compGenPCodeN( ( BYTE * )hb_comp_files.pLast->szFileName, strlen( hb_comp_files.pLast->szFileName ) );
      hb_compGenPCode1( ':' );
      hb_compGenPCodeN( ( BYTE * )szFunName, strlen( szFunName ) );
      hb_compGenPCode1( 0 );
   }
}

void hb_compGenPushFunRef( char * szName )
{
   HB_SYMBOL_UNUSED( szName );   /* TODO: */
}

void * GenElseIf( void * pFirst, ULONG ulOffset )
{
   PELSEIF pElseIf = ( PELSEIF ) hb_xgrab( sizeof( _ELSEIF ) ), pLast;

   pElseIf->ulOffset = ulOffset;
   pElseIf->pNext   = 0;

   if( ! pFirst )
      pFirst = pElseIf;
   else
   {
      pLast = ( PELSEIF ) pFirst;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pElseIf;
   }
   return pFirst;
}


PFUNCTION KillFunction( PFUNCTION pFunc )
{
   PFUNCTION pNext = pFunc->pNext;
   PVAR pVar;

   while( pFunc->pLocals )
   {
      pVar = pFunc->pLocals;
      pFunc->pLocals = pVar->pNext;

      hb_xfree( ( void * ) pVar->szName );
      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pStatics )
   {
      pVar = pFunc->pStatics;
      pFunc->pStatics = pVar->pNext;

      hb_xfree( ( void * ) pVar->szName );
      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pFields )
   {
      pVar = pFunc->pFields;
      pFunc->pFields = pVar->pNext;

      hb_xfree( ( void * ) pVar->szName );
      if( pVar->szAlias )
      {
         hb_xfree( ( void * ) pVar->szAlias );
      }
      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pMemvars )
   {
      pVar = pFunc->pMemvars;
      pFunc->pMemvars = pVar->pNext;

      hb_xfree( ( void * ) pVar->szName );
      if( pVar->szAlias )
      {
         hb_xfree( ( void * ) pVar->szAlias );
      }
      hb_xfree( ( void * ) pVar );
   }

   hb_xfree( ( void * ) pFunc->pCode );
/* hb_xfree( ( void * ) pFunc->szName ); The name will be released in KillSymbol() */
   hb_xfree( ( void * ) pFunc );

   return pNext;
}


PCOMSYMBOL KillSymbol( PCOMSYMBOL pSym )
{
   PCOMSYMBOL pNext = pSym->pNext;

   hb_xfree( ( void * ) pSym->szName );
   hb_xfree( ( void * ) pSym );

   return pNext;
}

void hb_compGenBreak( void )
{
   hb_compPushSymbol( yy_strdup("BREAK"), 1 );
   hb_compGenPushNil();
}

void hb_compGenExterns( void ) /* generates the symbols for the EXTERN names */
{
   PEXTERN pDelete;

   if( hb_comp_bDebugInfo )
      hb_compAddExtern( yy_strdup( "__DBGENTRY" ) );

   while( hb_comp_pExterns )
   {
      if( hb_compGetSymbol( hb_comp_pExterns->szName, NULL ) )
      {
         if( ! GetFuncall( hb_comp_pExterns->szName ) )
            hb_compAddFunCall( hb_comp_pExterns->szName );
      }
      else
      {
         hb_compAddSymbol( hb_comp_pExterns->szName, NULL );
         hb_compAddFunCall( hb_comp_pExterns->szName );
      }
      pDelete  = hb_comp_pExterns;
      hb_comp_pExterns = hb_comp_pExterns->pNext;
      hb_xfree( ( void * ) pDelete );
   }
}

PFUNCTION GetFuncall( char * szFunctionName ) /* returns a previously called defined function */
{
   PFUNCTION pFunc = hb_comp_funcalls.pFirst;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) )
         return pFunc;
      else
      {
         if( pFunc->pNext )
            pFunc = pFunc->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PFUNCTION GetFunction( char * szFunctionName ) /* returns a previously defined function */
{
   PFUNCTION pFunc = hb_comp_functions.pFirst;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) )
         return pFunc;
      else
      {
         if( pFunc->pNext )
            pFunc = pFunc->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PVAR GetVar( PVAR pVars, USHORT wOrder ) /* returns variable if defined or zero */
{
   USHORT w = 1;

   while( pVars->pNext && w++ < wOrder )
      pVars = pVars->pNext;

   return pVars;
}

USHORT GetVarPos( PVAR pVars, char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   USHORT wVar = 1;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
      {
         /* TODO: This is not the best place to push the variable type
          * in some cases it will be called two times for the same variable
          */
         pVars->iUsed = 1;
         return wVar;
      }
      else
      {
         if( pVars->pNext )
         {
            pVars = pVars->pNext;
            wVar++;
         }
         else
            return 0;
      }
   }
   return 0;
}

int GetLocalVarPos( char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   int iVar = 0;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      return GetVarPos( pFunc->pLocals, szVarName );
   else
   {
      /* we are in a codeblock */
      iVar = GetVarPos( pFunc->pLocals, szVarName );
      if( iVar == 0 )
      {
         /* this is not a current codeblock parameter
         * we have to check the list of nested codeblocks up to a function
         * where the codeblock is defined
         */
         PFUNCTION pOutBlock = pFunc;   /* the outermost codeblock */

         pFunc = pFunc->pOwner;
         while( pFunc )
         {
            iVar = GetVarPos( pFunc->pLocals, szVarName );
            if( iVar )
            {
               if( pFunc->pOwner )
               {
                  /* this variable is defined in a parent codeblock
                  * It is not possible to access a parameter of a codeblock in which
                  * the current codeblock is defined
                  */
                  hb_compGenError( hb_comp_szCErrors, 'E', ERR_OUTER_VAR, szVarName, NULL );
               }
               else
               {
                  /* We want to access a local variable defined in a function
                   * that owns this codeblock. We cannot access this variable in
                   * a normal way because at runtime the stack base will point
                   * to local variables of EVAL function.
                   *  The codeblock cannot have static variables then we can
                   * use this structure to store temporarily all referenced
                   * local variables
                   */
                  /* NOTE: The list of local variables defined in a function
                   * and referenced in a codeblock will be stored in a outer
                   * codeblock only. This makes sure that all variables will be
                   * detached properly - the inner codeblock can be created
                   * outside of a function where it was defined when the local
                   * variables are not accessible.
                   */
                  iVar = -GetVarPos( pOutBlock->pStatics, szVarName );
                  if( iVar == 0 )
                  {
                     /* this variable was not referenced yet - add it to the list */
                     PVAR pVar;

                     pVar = ( PVAR ) hb_xgrab( sizeof( VAR ) );
                     pVar->szName = szVarName;
                     pVar->cType = ' ';
                     pVar->iUsed = 0;
                     pVar->pNext  = NULL;

                     /* Use negative order to signal that we are accessing a local
                     * variable from a codeblock
                     */
                     iVar = -1;  /* first variable */
                     if( ! pOutBlock->pStatics )
                        pOutBlock->pStatics = pVar;
                     else
                     {
                        PVAR pLastVar = pOutBlock->pStatics;

                        --iVar;   /* this will be at least second variable */
                        while( pLastVar->pNext )
                        {
                           pLastVar = pLastVar->pNext;
                           --iVar;
                        }
                        pLastVar->pNext = pVar;
                     }
                  }
                  return iVar;
               }
            }
            pOutBlock = pFunc;
            pFunc = pFunc->pOwner;
         }
      }
   }
   return iVar;
}

/*
 * Gets position of passed static variables.
 * All static variables are hold in a single array at runtime then positions
 * are numbered for whole PRG module.
 */
int GetStaticVarPos( char * szVarName )
{
   int iPos;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   /* First we have to check if this name belongs to a static variable
     * defined in current function
     */
   if( pFunc->pOwner )
      pFunc = pFunc->pOwner;  /* we are in the static variable definition state */
   iPos = GetVarPos( pFunc->pStatics, szVarName );
   if( iPos )
      return iPos + pFunc->iStaticsBase;

   /* Next we have to check the list of global static variables
     * Note: It is not possible to have global static variables when
     * implicit starting procedure is defined
     */
   if( !hb_comp_bStartProc )
   {
      iPos = GetVarPos( hb_comp_functions.pFirst->pStatics, szVarName );
      if( iPos )
         return iPos;
   }
   return 0;
}

/* Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 */
int GetFieldVarPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = GetVarPos( pFunc->pFields, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      iVar = GetVarPos( pFunc->pFields, szVarName );
   }
   return iVar;
}

/* Checks if passed variable name is declared as MEMVAR
 * Returns 0 if not found in MEMVAR list or its position in this list if found
 */
int GetMemvarPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = GetVarPos( pFunc->pMemvars, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      iVar = GetVarPos( pFunc->pMemvars, szVarName );
   }
   return iVar;
}

USHORT FixSymbolPos( USHORT wCompilePos )
{
   return ( hb_comp_bStartProc ? wCompilePos - 1 : wCompilePos - 2 );
}


/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table
 */
PCOMSYMBOL hb_compGetSymbol( char * szSymbolName, USHORT * pwPos )
{
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT wCnt = 1;

   if( pwPos )
      *pwPos = 0;
   while( pSym )
   {
      if( ! strcmp( pSym->szName, szSymbolName ) )
      {
         if( pwPos )
            *pwPos = wCnt;
         return pSym;
      }
      else
      {
         if( pSym->pNext )
         {
            pSym = pSym->pNext;
            ++wCnt;
         }
         else
            return NULL;
      }
   }
   return NULL;
}

PCOMSYMBOL hb_compGetSymbolOrd( USHORT wSymbol )   /* returns a symbol based on its index on the symbol table */
{
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT w = 1;

   while( w++ < wSymbol && pSym->pNext )
      pSym = pSym->pNext;

   return pSym;
}

USHORT GetFunctionPos( char * szFunctionName ) /* return 0 if not found or order + 1 */
{
   PFUNCTION pFunc = hb_comp_functions.pFirst;
   USHORT wFunction = hb_comp_bStartProc;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) && pFunc != hb_comp_functions.pFirst )
         return wFunction;
      else
      {
         if( pFunc->pNext )
         {
            pFunc = pFunc->pNext;
            wFunction++;
         }
         else
            return 0;
      }
   }
   return 0;
}

void Inc( void )
{
   hb_compGenPCode1( HB_P_INC );
}

ULONG hb_compGenJump( LONG lOffset )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compGenPCode3( HB_P_JUMP, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ) );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

ULONG hb_compGenJumpFalse( LONG lOffset )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compGenPCode3( HB_P_JUMPFALSE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ) );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

void hb_compGenJumpThere( ULONG ulFrom, ULONG ulTo )
{
   BYTE * pCode = hb_comp_functions.pLast->pCode;
   LONG lOffset = ulTo - ulFrom + 1;

   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_JUMP_TOO_LONG, NULL, NULL );

   pCode[ ( ULONG ) ulFrom ]     = HB_LOBYTE( lOffset );
   pCode[ ( ULONG ) ulFrom + 1 ] = HB_HIBYTE( lOffset );
}

void hb_compGenJumpHere( ULONG ulOffset )
{
   hb_compGenJumpThere( ulOffset, hb_comp_functions.pLast->lPCodePos );
}

ULONG hb_compGenJumpTrue( LONG lOffset )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_JUMP_TOO_LONG, NULL, NULL );
   hb_compGenPCode3( HB_P_JUMPTRUE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ) );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

void Line( void ) /* generates the pcode with the currently compiled source code line */
{
   if( hb_comp_bLineNumbers && ! hb_comp_bDontGenLineNum )
   {
      if( ( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastLinePos ) > 3 ) || hb_comp_bDebugInfo )
      {
         hb_comp_ulLastLinePos = hb_comp_functions.pLast->lPCodePos;
         hb_compGenPCode3( HB_P_LINE, HB_LOBYTE( iLine ), HB_HIBYTE( iLine ) );
      }
      else
      {
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +1 ] = HB_LOBYTE( iLine );
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +2 ] = HB_HIBYTE( iLine );
      }
   }
   hb_comp_bDontGenLineNum = FALSE;
   hb_comp_functions.pLast->bFlags &= ~ FUN_WITH_RETURN;   /* clear RETURN flag */
   hb_comp_lLastPushPos = -1;
}

/* Generates the pcode with the currently compiled source code line
 * if debug code was requested only
 */
void LineDebug( void )
{
   if( hb_comp_bDebugInfo )
      Line();
   else
      hb_comp_functions.pLast->bFlags &= ~ FUN_WITH_RETURN;  /* clear RETURN flag */
}

void LineBody( void ) /* generates the pcode with the currently compiled source code line */
{
   /* This line can be placed inside a procedure or function only */
   /* except EXTERNAL */
   if( ! hb_comp_bExternal )
   {
      hb_comp_bExternal = FALSE;
      if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 )
      {
         hb_compGenError( hb_comp_szCErrors, 'E', ERR_OUTSIDE, NULL, NULL );
      }
   }

   hb_comp_functions.pLast->bFlags |= FUN_STATEMENTS;
   Line();
}

/*
 * Function generates passed pcode for passed variable name
 */
void VariablePCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pSym;
   PFUNCTION pOwnerFunc = NULL;
   int iType = VS_LOCAL; /* not really */

   /* Check if it is a FIELD declared in current function
    */
   wVar = GetFieldVarPos( szVarName, hb_comp_functions.pLast );
   if( wVar == 0 )
   {
      /* Check if it is a MEMVAR declared in current function
       */
      wVar = GetMemvarPos( szVarName, hb_comp_functions.pLast );
      if( wVar )
         iType = VS_MEMVAR;
   }
   else
   {
      iType = VS_FIELD;
      pOwnerFunc = hb_comp_functions.pLast;
   }

   /* if it is not declared in current function then check if it is
    * a symbol with file wide scope
    */
   if( wVar == 0 && ! hb_comp_bStartProc )
   {
      wVar = GetFieldVarPos( szVarName, hb_comp_functions.pFirst );
      if( wVar == 0 )
      {
         wVar = GetMemvarPos( szVarName, hb_comp_functions.pFirst );
         if( wVar )
            iType = VS_MEMVAR;
      }
      else
      {
         iType = VS_FIELD;
         pOwnerFunc = hb_comp_functions.pFirst;
      }
   }

   if( wVar == 0 )
   {
      /* This is undeclared variable  */
      /*
       * NOTE:
       * Clipper always assumes a memvar variable if undeclared variable
       * is popped (a value is asssigned to a variable).
       *
       */
#if defined( HARBOUR_STRICT_CLIPPER_COMPATIBILITY )
      if( hb_comp_bForceMemvars || bPCode == HB_P_POPVARIABLE )
#else
      if( hb_comp_bForceMemvars )
#endif
      {
         /* -v switch was used -> assume it is a memvar variable
          */
         iType = VS_MEMVAR;
         hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_MEMVAR_ASSUMED, szVarName, NULL );
      }
      else
         hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_AMBIGUOUS_VAR, szVarName, NULL );
   }

   if( iType == VS_FIELD )
   {  /* variable is declared using FIELD statement */
      PVAR pField = GetVar( pOwnerFunc->pFields, wVar );

      if( pField->szAlias )
      {  /* the alias was specified in FIELD declaration */
         if( bPCode == HB_P_POPVARIABLE )
            bPCode = HB_P_POPALIASEDFIELD;
         else if( bPCode == HB_P_PUSHVARIABLE )
            bPCode = HB_P_PUSHALIASEDFIELD;
         else
            /* pushing fields by reference is not allowed */
            hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
         /*
          * Push alias symbol before the field symbol
          */
         hb_compPushSymbol( yy_strdup( pField->szAlias ), 0 );
      }
      else
      {  /* this is unaliased field */
         if( bPCode == HB_P_POPVARIABLE )
            bPCode = HB_P_POPFIELD;
         else if( bPCode == HB_P_PUSHVARIABLE )
            bPCode = HB_P_PUSHFIELD;
         else if( bPCode == HB_P_PUSHMEMVARREF )
            /* pushing fields by reference is not allowed */
            hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
      }
   }
   else if( iType == VS_MEMVAR )
   {
      /* variable is declared or assumed MEMVAR */
      if( bPCode == HB_P_POPVARIABLE )
         bPCode = HB_P_POPMEMVAR;
      else if( bPCode == HB_P_PUSHVARIABLE )
         bPCode = HB_P_PUSHMEMVAR;
   }

   /* Check if this variable name is placed into the symbol table
    */
   pSym = hb_compGetSymbol( szVarName, &wVar );
   if( ! pSym )
      pSym = hb_compAddSymbol( szVarName, &wVar );
   pSym->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

/*
 * Function generates passed pcode for passed memvar name
 */
void MemvarPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pSym;

   /* Check if this variable name is placed into the symbol table
    */
   pSym = hb_compGetSymbol( szVarName, &wVar );
   if( ! pSym )
      pSym = hb_compAddSymbol( szVarName, &wVar );
   pSym->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

void hb_compGenMessage( char * szMsgName )       /* sends a message to an object */
{
   USHORT wSym;
   PCOMSYMBOL pSym = hb_compGetSymbol( szMsgName, &wSym );

   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym = hb_compAddSymbol( szMsgName, &wSym );
   pSym->cScope |= FS_MESSAGE;
   hb_compGenPCode3( HB_P_MESSAGE, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ) );
}

void hb_compGenMessageData( char * szMsg ) /* generates an underscore-symbol name for a data assignment */
{
   char * szResult = ( char * ) hb_xgrab( strlen( szMsg ) + 2 );

   strcpy( szResult, "_" );
   strcat( szResult, szMsg );

   hb_compGenMessage( szResult );
}

void hb_compGenPopVar( char * szVarName ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   int iVar;

   iVar = GetLocalVarPos( szVarName );
   if( iVar )
      hb_compGenPCode3( HB_P_POPLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
   else
   {
      iVar = GetStaticVarPos( szVarName );
      if( iVar )
      {
         hb_compGenPCode3( HB_P_POPSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         VariablePCode( HB_P_POPVARIABLE, szVarName );
      }
   }
}

/* generates the pcode to pop a value from the virtual machine stack onto
 * an aliased variable
 */
void hb_compGenPopAliasedVar( char * szVarName,
                              BOOL bPushAliasValue,
                              char * szAlias,
                              long lWorkarea )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' )
         {  /* M->variable */
            MemvarPCode( HB_P_POPMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               MemvarPCode( HB_P_POPMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  FieldPCode( HB_P_POPFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compPushSymbol( yy_strdup( szAlias ), 0 );
                  FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack */
      FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushVar( char * szVarName )
{
   int iVar;

   iVar = GetLocalVarPos( szVarName );
   if( iVar )
      hb_compGenPCode3( HB_P_PUSHLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
   else
   {
      iVar = GetStaticVarPos( szVarName );
      if( iVar )
      {
         hb_compGenPCode3( HB_P_PUSHSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         VariablePCode( HB_P_PUSHVARIABLE, szVarName );
      }
   }
}

void hb_compGenPushVarRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   USHORT iVar;

   iVar = GetLocalVarPos( szVarName );
   if( iVar )
      hb_compGenPCode3( HB_P_PUSHLOCALREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
   else
   {
      iVar = GetStaticVarPos( szVarName );
      if( iVar )
      {
         hb_compGenPCode3( HB_P_PUSHSTATICREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ) );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         /* if undeclared variable is passed by reference then a memvar
          * variable is assumed because fields cannot be passed by
          * a reference
          */
         VariablePCode( HB_P_PUSHMEMVARREF, szVarName );
      }
   }
}

 /* generates the pcode to push an aliased variable value to the virtual
  * machine stack
  */
void hb_compGenPushAliasedVar( char * szVarName,
                               BOOL bPushAliasValue,
                               char * szAlias,
                               long lWorkarea )
{
   if( bPushAliasValue )
   {
      if( szAlias )
      {
         /* myalias->var
         * FIELD->var
         * MEMVAR->var
         */
         if( szAlias[ 0 ] == 'M' && szAlias[ 1 ] == '\0' )
         {  /* M->variable */
            MemvarPCode( HB_P_PUSHMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               MemvarPCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  FieldPCode( HB_P_PUSHFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compPushSymbol( yy_strdup( szAlias ), 0 );
                  FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack */
      FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
}

void hb_compGenPushLogical( int iTrueFalse ) /* pushes a logical value on the virtual machine stack */
{
   if( iTrueFalse )
      hb_compGenPCode1( HB_P_TRUE );
   else
      hb_compGenPCode1( HB_P_FALSE );
}

void hb_compGenPushNil( void )
{
   hb_compGenPCode1( HB_P_PUSHNIL );
}

/* generates the pcode to push a double number on the virtual machine stack */
void hb_compGenPushDouble( double dNumber, BYTE bDec )
{
   hb_compGenPCode1( HB_P_PUSHDOUBLE );
   hb_compGenPCodeN( ( BYTE * ) &dNumber, sizeof( double ) );
   hb_compGenPCode1( bDec );
}

void hb_compGenPushFunCall( char * szFunName )
{
   char * szFunction;

   szFunction = RESERVED_FUNC( szFunName );
   if( szFunction )
   {
      /* Abbreviated function name was used - change it for whole name
       */
      hb_compPushSymbol( yy_strdup( szFunction ), 1 );
   }
   else
      hb_compPushSymbol( szFunName, 1 );
}

/* generates the pcode to push a integer number on the virtual machine stack */
void hb_compGenPushInteger( int iNumber )
{
   if( iNumber )
      hb_compGenPCode3( HB_P_PUSHINT, HB_LOBYTE( ( USHORT ) iNumber ), HB_HIBYTE( ( USHORT ) iNumber ) );
   else
      hb_compGenPCode1( HB_P_ZERO );
}

/* generates the pcode to push a long number on the virtual machine stack */
void hb_compGenPushLong( long lNumber )
{
   if( lNumber )
   {
      hb_compGenPCode1( HB_P_PUSHLONG );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 0 ] );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 1 ] );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 2 ] );
      hb_compGenPCode1( ( ( char * ) &lNumber )[ 3 ] );
   }
   else
      hb_compGenPCode1( HB_P_ZERO );
}

/* generates the pcode to push a string on the virtual machine stack */
void hb_compGenPushString( char * szText, ULONG ulStrLen )
{
   hb_compGenPCode3( HB_P_PUSHSTR, HB_LOBYTE( ulStrLen ), HB_HIBYTE( ulStrLen ) );
   hb_compGenPCodeN( ( BYTE * ) szText, ulStrLen );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void hb_compPushSymbol( char * szSymbolName, int iIsFunction )
{
   USHORT wSym;
   PCOMSYMBOL pSym;

   if( iIsFunction )
   {
      char * pName = RESERVED_FUNC( szSymbolName );
      /* If it is reserved function name then we should truncate
       * the requested name.
       * We have to use passed szSymbolName so we can latter deallocate it
       * (pName points to static data)
       */
      if( pName )
         szSymbolName[ strlen( pName ) ] ='\0';
   }

   pSym = hb_compGetSymbol( szSymbolName, &wSym );
   if( ! pSym )  /* the symbol was not found on the symbol table */
   {
      pSym = hb_compAddSymbol( szSymbolName, &wSym );
      if( iIsFunction )
         hb_compAddFunCall( szSymbolName );
   }
   else
   {
      if( iIsFunction && ! GetFuncall( szSymbolName ) )
         hb_compAddFunCall( szSymbolName );
   }
   hb_compGenPCode3( HB_P_PUSHSYM, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ) );
}


void CheckDuplVars( PVAR pVar, char * szVarName, int iVarScope )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         if( ! ( iVarScope & VS_PARAMETER ) )
            --iLine;
         hb_compErrorDuplVar( szVarName );
      }
      else
         pVar = pVar->pNext;
   }
}

void Dec( void )
{
   hb_compGenPCode1( HB_P_DEC );
}

void hb_compGenDoProc( BYTE bParams )
{
   hb_compGenPCode3( HB_P_DO, bParams, 0 );
}

void FixElseIfs( void * pFixElseIfs )
{
   PELSEIF pFix = ( PELSEIF ) pFixElseIfs;

   while( pFix )
   {
      hb_compGenJumpHere( pFix->ulOffset );
      pFix = pFix->pNext;
   }
}

void FixReturns( void ) /* fixes all last defined function returns jumps offsets */
{
   if( hb_comp_iWarnings && hb_comp_functions.pLast )
   {
      PVAR pVar;

      pVar = hb_comp_functions.pLast->pLocals;
      while( pVar )
      {
         if( pVar->szName && hb_comp_functions.pLast->szName && hb_comp_functions.pLast->szName[0] && ! pVar->iUsed )
            hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, hb_comp_functions.pLast->szName );

         pVar = pVar->pNext;
      }

      pVar = hb_comp_functions.pLast->pStatics;
      while( pVar )
      {
         if( pVar->szName && hb_comp_functions.pLast->szName && hb_comp_functions.pLast->szName[0] && ! pVar->iUsed )
            hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, hb_comp_functions.pLast->szName );

         pVar = pVar->pNext;
      }

      /* Check if the function returned some value
       */
      if( (hb_comp_functions.pLast->bFlags & FUN_WITH_RETURN) == 0 &&
          (hb_comp_functions.pLast->bFlags & FUN_PROCEDURE) == 0 )
         hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_FUN_WITH_NO_RETURN,
                     hb_comp_functions.pLast->szName, NULL );
   }

/* TODO: check why it triggers this error in keywords.prg
   if( hb_comp_pLoops )
   {
      PTR_LOOPEXIT pLoop = hb_comp_pLoops;
      char cLine[ 64 ];

      while( pLoop->pNext )
         pLoop = pLoop->pNext;

      itoa( pLoop->iLine, cLine, 10 );
      hb_compGenError( hb_comp_szCErrors, 'E', ERR_UNCLOSED_STRU, cLine, NULL );
   }
*/
}

void Function( BYTE bParams )
{
   hb_compGenPCode3( HB_P_FUNCTION, bParams, 0 );
}

void hb_compGenArray( int iElements )
{
   hb_compGenPCode3( HB_P_ARRAYGEN, HB_LOBYTE( iElements ), HB_HIBYTE( iElements ) );
}

void hb_compGenPCode1( BYTE byte )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( PCODE_CHUNK );
      pFunc->lPCodeSize = PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
         pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;
}

void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3 )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( BYTE * ) hb_xgrab( PCODE_CHUNK );
      pFunc->lPCodeSize = PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 3 )
         pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize += PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;
}

void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize )
{
   PFUNCTION pFunc = hb_comp_functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->lPCodeSize = ( ( ulSize / PCODE_CHUNK ) + 1 ) * PCODE_CHUNK;
      pFunc->pCode      = ( BYTE * ) hb_xgrab( pFunc->lPCodeSize );
      pFunc->lPCodePos  = 0;
   }
   else if( pFunc->lPCodePos + ulSize > pFunc->lPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->lPCodeSize += ( ( ( ulSize / PCODE_CHUNK ) + 1 ) * PCODE_CHUNK );
      pFunc->pCode = ( BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->lPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->lPCodePos, pBuffer, ulSize );
   pFunc->lPCodePos += ulSize;
}

/* Generate the opcode to open BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * - either the address of HB_P_SEQEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
ULONG SequenceBegin( void )
{
   hb_compGenPCode3( HB_P_SEQBEGIN, 0, 0 );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

/* Generate the opcode to close BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * the address of first line after END SEQUENCE
 * This opcode will be executed if recover code was not requested (as the
 * last statement in code beetwen BEGIN ... RECOVER) or if BREAK was requested
 * and there was no matching RECOVER clause.
 */
ULONG SequenceEnd( void )
{
   hb_compGenPCode3( HB_P_SEQEND, 0, 0 );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

/* Remove unnecessary opcodes in case there were no executable statements
 * beetwen BEGIN and RECOVER sequence
 */
void SequenceFinish( ULONG ulStartPos, int bUsualStmts )
{
   if( ! hb_comp_bDebugInfo ) /* only if no debugger info is required */
   {
      if( ! bUsualStmts )
      {
         hb_comp_functions.pLast->lPCodePos = ulStartPos - 1; /* remove also HB_P_SEQBEGIN */
         hb_comp_ulLastLinePos = ulStartPos - 4;
      }
   }
}


/* Set the name of an alias for the list of previously declared FIELDs
 *
 * szAlias -> name of the alias
 * iField  -> position of the first FIELD name to change
 */
void FieldsSetAlias( char * szAlias, int iField )
{
   PVAR pVar;

   pVar = hb_comp_functions.pLast->pFields;
   while( iField-- && pVar )
      pVar = pVar->pNext;

   while( pVar )
   {
      pVar->szAlias = szAlias;
      pVar = pVar->pNext;
   }
}

/* This functions counts the number of FIELD declaration in a function
 * We will required this information in FieldsSetAlias function
 */
int FieldsCount()
{
   int iFields = 0;
   PVAR pVar = hb_comp_functions.pLast->pFields;

   while( pVar )
   {
      ++iFields;
      pVar = pVar->pNext;
   }

   return iFields;
}

/*
 * Start of definition of static variable
 * We are using here the special function hb_comp_pInitFunc which will store
 * pcode needed to initialize all static variables declared in PRG module.
 * pOwner member will point to a function where the static variable is
 * declared:
 * TODO: support for static variables in codeblock
 */
void hb_compStaticDefStart( void )
{
   hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
   if( ! hb_comp_pInitFunc )
   {
      BYTE pBuffer[ 5 ];

      hb_comp_pInitFunc = FunctionNew( yy_strdup("(_INITSTATICS)"), FS_INIT );
      hb_comp_pInitFunc->pOwner = hb_comp_functions.pLast;
      hb_comp_pInitFunc->bFlags = FUN_USES_STATICS | FUN_PROCEDURE;
      hb_comp_pInitFunc->cScope = FS_INIT | FS_EXIT;
      hb_comp_functions.pLast = hb_comp_pInitFunc;

      pBuffer[ 0 ] = HB_P_STATICS;
      pBuffer[ 1 ] = 0;
      pBuffer[ 2 ] = 0;
      pBuffer[ 3 ] = 1; /* the number of static variables is unknown now */
      pBuffer[ 4 ] = 0;
      hb_compGenPCodeN( pBuffer, 5 );
      hb_compGenPCode3( HB_P_SFRAME, 0, 0 );     /* frame for statics variables */
   }
   else
   {
      hb_comp_pInitFunc->pOwner = hb_comp_functions.pLast;
      hb_comp_functions.pLast = hb_comp_pInitFunc;
   }
}

/*
 * End of definition of static variable
 * Return to previously pcoded function.
 */
void hb_compStaticDefEnd( void )
{
   hb_comp_functions.pLast = hb_comp_pInitFunc->pOwner;
   hb_comp_pInitFunc->pOwner = NULL;
   ++hb_comp_iStaticCnt;
}

/*
 * Start a new fake-function that will hold pcodes for a codeblock
*/
void hb_compCodeBlockStart()
{
   PFUNCTION pFunc = FunctionNew( NULL, FS_STATIC );

   pFunc->pOwner       = hb_comp_functions.pLast;
   pFunc->iStaticsBase = hb_comp_functions.pLast->iStaticsBase;

   hb_comp_functions.pLast = pFunc;
   LineDebug();
}

void hb_compCodeBlockEnd( void )
{
   PFUNCTION pCodeblock;   /* pointer to the current codeblock */
   PFUNCTION pFunc;        /* pointer to a function that owns a codeblock */
   USHORT wSize;
   USHORT wLocals = 0;   /* number of referenced local variables */
   USHORT wPos;
   PVAR pVar, pFree;

   pCodeblock = hb_comp_functions.pLast;

   /* return to pcode buffer of function/codeblock in which the current
    * codeblock was defined
    */
   hb_comp_functions.pLast = pCodeblock->pOwner;

   /* find the function that owns the codeblock */
   pFunc = pCodeblock->pOwner;
   while( pFunc->pOwner )
      pFunc = pFunc->pOwner;
   pFunc->bFlags |= ( pCodeblock->bFlags & FUN_USES_STATICS );

   /* generate a proper codeblock frame with a codeblock size and with
    * a number of expected parameters
    */
   /*QUESTION: would be 64kB enough for a codeblock size?
    * we are assuming now a USHORT for a size of codeblock
    */

   /* Count the number of referenced local variables */
   pVar = pCodeblock->pStatics;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++wLocals;
   }

   /*NOTE:  8 = HB_P_PUSHBLOCK + USHORT( size ) + USHORT( wParams ) + USHORT( wLocals ) + _ENDBLOCK */
   wSize = ( USHORT ) pCodeblock->lPCodePos + 8 + wLocals * 2;

   hb_compGenPCode3( HB_P_PUSHBLOCK, HB_LOBYTE( wSize ), HB_HIBYTE( wSize ) );
   hb_compGenPCode1( HB_LOBYTE( pCodeblock->wParamCount ) );
   hb_compGenPCode1( HB_HIBYTE( pCodeblock->wParamCount ) );
   hb_compGenPCode1( HB_LOBYTE( wLocals ) );
   hb_compGenPCode1( HB_HIBYTE( wLocals ) );

   /* generate the table of referenced local variables */
   pVar = pCodeblock->pStatics;
   while( wLocals-- )
   {
      wPos = GetVarPos( pFunc->pLocals, pVar->szName );
      hb_compGenPCode1( HB_LOBYTE( wPos ) );
      hb_compGenPCode1( HB_HIBYTE( wPos ) );

      pFree = pVar;
      hb_xfree( ( void * ) pFree->szName );
      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }

   hb_compGenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos );
   hb_compGenPCode1( HB_P_ENDBLOCK ); /* finish the codeblock */

   /* this fake-function is no longer needed */
   hb_xfree( ( void * ) pCodeblock->pCode );
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      if( hb_comp_iWarnings && pFunc->szName && pVar->szName && ! pVar->iUsed )
         hb_compGenWarning( hb_comp_szCWarnings, 'W', WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );

      /* free used variables */
      pFree = pVar;
      hb_xfree( ( void * ) pFree->szName );
      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }
   hb_xfree( ( void * ) pCodeblock );
}


/* ************************************************************************* */

HB_EXPR_PTR hb_compErrorLValue( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_LVALUE, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorType( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_TYPE, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorIndex( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_INDEX, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorBound( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_BOUND, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorSyntax( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_SYNTAX, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorAlias(  HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_INVALID_ALIAS, szDesc, NULL );
   return pExpr;
}

HB_EXPR_PTR hb_compErrorStatic( char * szVarName, HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_ILLEGAL_INIT, szVarName, szDesc );
   return pExpr;
}

void hb_compErrorDuplVar( char * szVarName )
{
   hb_compGenError( hb_comp_szCErrors, 'E', ERR_VAR_DUPL, szVarName, NULL );
}

HB_EXPR_PTR hb_compWarnMeaningless( HB_EXPR_PTR pExpr )
{
   char * szDesc = hb_compExprDescription( pExpr );
   hb_compGenWarning(  hb_comp_szCWarnings, 'W', WARN_MEANINGLESS, szDesc, NULL );
   return pExpr;
}

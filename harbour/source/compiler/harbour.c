/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler main file
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 RonPinkas <Ron@Profit-Master.com>
 *    hb_compPrepareOptimize()
 *    hb_compOptimizeJumps()
 *    hb_compOptimizeFrames()
 *    hb_compAutoOpenAdd()
 *    hb_compAutoOpenFind()
 *    hb_compAutoOpen()
 *    hb_compDeclaredParameterAdd()
 *    hb_compClassAdd()
 *    hb_compClassFind()
 *    hb_compMethodAdd()
 *    hb_compMethodFind()
 *    hb_compDeclaredAdd()
 *    hb_compDeclaredInit()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <malloc.h>     /* required for allocating and freeing memory */

#include "hbcomp.h"
#include "hbhash.h"

#if defined(HB_OS_DOS) && defined(__BORLANDC__)
   #include <limits.h>
   extern unsigned _stklen = UINT_MAX;
#endif

static void hb_compInitVars( void );
static void hb_compGenOutput( int );
static void hb_compOutputFile( void );

int hb_compLocalGetPos( char * szVarName );   /* returns the order + 1 of a local variable */
int hb_compStaticGetPos( char *, PFUNCTION ); /* return if passed name is a static variable */
int hb_compFieldGetPos( char *, PFUNCTION );  /* return if passed name is a field variable */
int hb_compMemvarGetPos( char *, PFUNCTION ); /* return if passed name is a memvar variable */

static void hb_compGenFieldPCode( BYTE , int, char *, PFUNCTION );      /* generates the pcode for database field */
static void hb_compGenVariablePCode( BYTE , char * );    /* generates the pcode for undeclared variable */
static void hb_compGenVarPCode( BYTE , char * );    /* generates the pcode for undeclared variable */

static PFUNCTION hb_compFunctionNew( char *, HB_SYMBOLSCOPE );  /* creates and initialises the _FUNC structure */
static PINLINE hb_compInlineNew( char * );  /* creates and initialises the _INLINE structure */
static void hb_compCheckDuplVars( PVAR pVars, char * szVarName ); /*checks for duplicate variables definitions */

/* int hb_compSort_ULONG( ULONG * ulLeft, ULONG * ulRight ); */
static void hb_compOptimizeJumps( void );
static void hb_compPrepareOptimize( void );
static void hb_compOptimizeFrames( PFUNCTION pFunc );

static void hb_compDeclaredInit( void );

/* global variables */
FILES         hb_comp_files;
FUNCTIONS     hb_comp_functions;
FUNCTIONS     hb_comp_funcalls;
SYMBOLS       hb_comp_symbols;
PCOMDECLARED  hb_comp_pFirstDeclared;
PCOMDECLARED  hb_comp_pLastDeclared;
PCOMDECLARED  hb_comp_pReleaseDeclared;

PCOMCLASS     hb_comp_pFirstClass;
PCOMCLASS     hb_comp_pLastClass;
PCOMCLASS     hb_comp_pReleaseClass;
char *        hb_comp_szFromClass;
PCOMDECLARED  hb_comp_pLastMethod;

int           hb_comp_iLine;                             /* currently processed line number (globaly) */
char *        hb_comp_szFile;                            /* File Name of last compiled line */
PFUNCTION     hb_comp_pInitFunc;
PHB_FNAME     hb_comp_pFileName = NULL;

BOOL          hb_comp_bPPO = FALSE;                      /* flag indicating, is ppo output needed */
FILE *        hb_comp_yyppo = NULL;                      /* output .ppo file */
BOOL          hb_comp_bStartProc = TRUE;                 /* holds if we need to create the starting procedure */
BOOL          hb_comp_bLineNumbers = TRUE;               /* holds if we need pcodes with line numbers */
BOOL          hb_comp_bQuiet = FALSE;                    /* quiet mode */
BOOL          hb_comp_bShortCuts = TRUE;                 /* .and. & .or. expressions shortcuts */
int           hb_comp_iWarnings = 0;                     /* enable parse warnings */
BOOL          hb_comp_bAnyWarning = FALSE;               /* holds if there was any warning during the compilation process */
BOOL          hb_comp_bAutoMemvarAssume = FALSE;         /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
BOOL          hb_comp_bForceMemvars = FALSE;             /* holds if memvars are assumed when accesing undeclared variable (-v)*/
BOOL          hb_comp_bDebugInfo = FALSE;                /* holds if generate debugger required info */
char          hb_comp_szPrefix[ 20 ] = { '\0' };         /* holds the prefix added to the generated symbol init function name (in C output currently) */
int           hb_comp_iGenCOutput = HB_COMPGENC_VERBOSE; /* C code generation should be verbose (use comments) or not */
int           hb_comp_iExitLevel = HB_EXITLEVEL_DEFAULT; /* holds if there was any warning during the compilation process */
PATHNAMES *   hb_comp_pIncludePath = NULL;
int           hb_comp_iFunctionCnt;
int           hb_comp_iErrorCount;
char          hb_comp_cVarType;                          /* current declared variable type */
char          hb_comp_cDataListType;                     /* current declared variable list type */
char          hb_comp_cCastType;                         /* current casting type */
BOOL          hb_comp_bDontGenLineNum = FALSE;           /* suppress line number generation */
ULONG         hb_comp_ulLastLinePos;                     /* position of last opcode with line number */
int           hb_comp_iStaticCnt;                        /* number of defined statics variables on the PRG */
int           hb_comp_iVarScope;                         /* holds the scope for next variables to be defined */
PHB_FNAME     hb_comp_pOutPath = NULL;
BOOL          hb_comp_bCredits = FALSE;                  /* print credits */
BOOL          hb_comp_bBuildInfo = FALSE;                /* print build info */
BOOL          hb_comp_bLogo = TRUE;                      /* print logo */
BOOL          hb_comp_bSyntaxCheckOnly = FALSE;          /* syntax check only */
int           hb_comp_iLanguage = LANG_C;                /* default Harbour generated output language */
int           hb_comp_iJumpOptimize = 1;
char *        hb_comp_szDeclaredFun = NULL;

BOOL          hb_comp_bAutoOpen = TRUE;
BOOL          hb_comp_bError = FALSE;
char          hb_comp_cInlineID = '0';

int           hb_comp_iLineINLINE = 0;
int           hb_comp_iLinePRG;
INLINES       hb_comp_inlines;

/* various compatibility flags (-k switch) */
ULONG         hb_comp_Supported;


/* EXTERNAL statement can be placed into any place in a function - this flag is
 * used to suppress error report generation
 */
static BOOL hb_comp_bExternal   = FALSE;
/* linked list with EXTERNAL symbols declarations
 */
static PEXTERN hb_comp_pExterns = NULL;
static PAUTOOPEN hb_comp_pAutoOpen = NULL;
static int hb_compAutoOpen( char * szPrg, BOOL * bSkipGen );

/* -m Support */
static BOOL hb_compAutoOpenFind( char * szName );

extern int yyparse( void );    /* main yacc parsing function */

extern FILE    *yyin   ;
extern FILE    *yyout  ;
extern char    *yytext ;
extern int     yyleng  ;
extern int     yychar  ;
extern void *  yylval  ;
#ifdef YYLSP_NEEDED
   extern void *  yylloc  ;
#endif
extern int     yynerrs ;

extern char * hb_comp_buffer;
extern char * hb_comp_szAnnounce;

extern void yyrestart( FILE * );

/* ************************************************************************* */

int main( int argc, char * argv[] )
{
   int iStatus = EXIT_SUCCESS;
   int i;
   BOOL bAnyFiles;

   hb_comp_pOutPath = NULL;

   /* Activate Harbour extensions by default. */
   hb_comp_Supported  = HB_COMPFLAG_HARBOUR;
   hb_comp_Supported |= HB_COMPFLAG_XBASE;
   hb_comp_Supported |= HB_COMPFLAG_HB_INLINE;

   /* First check the environment variables */
   hb_compChkCompilerSwitch( 0, NULL );

   /* Then check command line arguments
      This will override duplicated environment settings */
   hb_compChkCompilerSwitch( argc, argv );

   if( hb_comp_bLogo )
      hb_compPrintLogo();

   if( hb_comp_bBuildInfo )
   {
      printf( "\n" );
      hb_verBuildInfo();
      return iStatus;
   }

   if( hb_comp_bCredits )
   {
      hb_compPrintCredits();
      return iStatus;
   }

   /* Set Search Path */
   hb_compChkPaths();

   /* Set standard rules */
   hb_pp_SetRules( hb_compInclude, hb_comp_bQuiet );

   /* Prepare the table of identifiers */
   hb_compIdentifierOpen();

   /* Load standard Declarations. */
   if ( hb_comp_iWarnings >= 3 )
   {
	  hb_compDeclaredInit();
   }

   /* Process all files passed via the command line. */

   bAnyFiles = FALSE;

   for( i = 1; i < argc; i++ )
   {
      if( ! HB_ISOPTSEP( argv[ i ][ 0 ] ) )
      {
         if( ! bAnyFiles )
            bAnyFiles = TRUE;

         if( i > 1 )
            hb_pp_Init();

         iStatus = hb_compCompile( argv[ i ], argc, argv );

         if( iStatus != EXIT_SUCCESS )
            break;
      }
   }

   hb_compIdentifierClose();

   if( (! bAnyFiles ) && (! hb_comp_bQuiet) )
   {
      hb_compPrintUsage( argv[ 0 ] );
      iStatus = EXIT_FAILURE;
   }

   if( hb_comp_pOutPath )
      hb_xfree( hb_comp_pOutPath );

   if( hb_comp_iErrorCount > 0 )
      iStatus = EXIT_FAILURE;

   return iStatus;
}

#if defined(__IBMCPP__) || defined(_MSC_VER) || (defined(__BORLANDC__) && defined(__cplusplus))
int isatty( int handle )
{
   return ( handle < 4 ) ? 1 : 0;
}
#endif

/* ------------------------------------------------------------------------- */

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize );

   if( ! pMem )
   {
      char szSize[ 32 ];

      sprintf( szSize, "%li", ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMALLOC, szSize, NULL );
   }

   return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   void * pResult = realloc( pMem, ulSize );
   if( ! pResult )
   {
      char szSize[ 32 ];

      sprintf( szSize, "%li", ulSize );
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMREALLOC, szSize, NULL );
   }

   return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
      free( pMem );
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_MEMFREE, NULL, NULL );
}

void hb_conOutErr( char * pStr, ULONG ulLen )
{
   HB_SYMBOL_UNUSED( ulLen );

   printf( pStr );
}

char * hb_conNewLine( void )
{
   return "\n";
}

/* ------------------------------------------------------------------------- */
/**                          ACTIONS                                        **/
/* ------------------------------------------------------------------------- */


/*
 * This function adds the name of called function into the list
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
PFUNCTION hb_compFunCallAdd( char * szFunctionName )
{
   PFUNCTION pFunc = hb_compFunctionNew( szFunctionName, 0 );

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
void hb_compExternAdd( char * szExternName ) /* defines a new extern name */
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
   hb_comp_bExternal = TRUE;
}

void hb_compDeclaredParameterAdd( char * szVarName, BYTE cValueType )
{
   /* Nothing to do since no warnings requested.*/
   if ( hb_comp_iWarnings < 3 )
   {
      HB_SYMBOL_UNUSED( szVarName );
      return;
   }

   /* Either a Declared Function Parameter or a Declared Method Parameter. */
   if( hb_comp_szDeclaredFun )
   {
      /* Find the Declared Function owner of this parameter. */
      PCOMDECLARED pDeclared = hb_compDeclaredFind( hb_comp_szDeclaredFun );

      if ( pDeclared )
      {
         pDeclared->iParamCount++;

         if ( pDeclared->cParamTypes )
         {
            pDeclared->cParamTypes = ( BYTE * ) hb_xrealloc( pDeclared->cParamTypes, pDeclared->iParamCount );
            pDeclared->pParamClasses = ( PCOMCLASS * ) hb_xrealloc( pDeclared->pParamClasses, pDeclared->iParamCount * sizeof( PCOMCLASS ) );
         }
         else
         {
            pDeclared->cParamTypes = ( BYTE * ) hb_xgrab( 1 );
            pDeclared->pParamClasses = ( PCOMCLASS * ) hb_xgrab( sizeof( PCOMCLASS ) );
         }

         pDeclared->cParamTypes[ pDeclared->iParamCount - 1 ] = cValueType;

         if ( toupper( cValueType ) == 'S' )
         {
            pDeclared->pParamClasses[ pDeclared->iParamCount - 1 ] = hb_compClassFind( hb_comp_szFromClass );

            /* Resetting */
            hb_comp_szFromClass = NULL;
         }
      }
   }
   else /* Declared Method Parameter */
   {
      /*
      printf( "\nAdding parameter: %s Type: %c In Method: %s Class: %s FROM CLASS: %s\n", szVarName, cValueType, hb_comp_pLastMethod->szName, hb_comp_pLastClass->szName, hb_comp_szFromClass );
      */

      hb_comp_pLastMethod->iParamCount++;

      if ( hb_comp_pLastMethod->cParamTypes )
      {
         hb_comp_pLastMethod->cParamTypes = ( BYTE * ) hb_xrealloc( hb_comp_pLastMethod->cParamTypes, hb_comp_pLastMethod->iParamCount );
         hb_comp_pLastMethod->pParamClasses = ( PCOMCLASS * ) hb_xrealloc( hb_comp_pLastMethod->pParamClasses, hb_comp_pLastMethod->iParamCount * sizeof( COMCLASS ) );
      }
      else
      {
         hb_comp_pLastMethod->cParamTypes = ( BYTE * ) hb_xgrab( 1 );
         hb_comp_pLastMethod->pParamClasses = ( PCOMCLASS * ) hb_xgrab( sizeof( COMCLASS ) );
      }

      hb_comp_pLastMethod->cParamTypes[ hb_comp_pLastMethod->iParamCount - 1 ] = cValueType;

      if ( toupper( cValueType ) == 'S' )
      {
         hb_comp_pLastMethod->pParamClasses[ hb_comp_pLastMethod->iParamCount - 1 ] = hb_compClassFind( hb_comp_szFromClass );

         /*
         printf( "\nParameter: %s FROM CLASS: %s\n", szVarName, hb_comp_pLastMethod->pParamClasses[ hb_comp_pLastMethod->iParamCount - 1 ]->szName );
         */

         /* Resetting */
         hb_comp_szFromClass = NULL;
      }
   }
}

void hb_compVariableAdd( char * szVarName, BYTE cValueType )
{
   PVAR pVar, pLastVar;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   HB_SYMBOL_UNUSED( cValueType );

   if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 && hb_comp_iVarScope == VS_LOCAL )
   {
      /* Variable declaration is outside of function/procedure body.
         In this case only STATIC and PARAMETERS variables are allowed. */
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTSIDE, NULL, NULL );
      return;
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( ( hb_comp_functions.pLast->bFlags & FUN_STATEMENTS ) && !( hb_comp_iVarScope == VS_FIELD || ( hb_comp_iVarScope & VS_MEMVAR ) ) )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FOLLOWS_EXEC, ( hb_comp_iVarScope == VS_LOCAL ? "LOCAL" : "STATIC" ), NULL );
   }

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      hb_compCheckDuplVars( pFunc->pFields, szVarName );
      hb_compCheckDuplVars( pFunc->pStatics, szVarName );
      /*NOTE: Clipper warns if PARAMETER variable duplicates the MEMVAR
       * declaration
      */
      if( !( hb_comp_iVarScope == VS_PRIVATE || hb_comp_iVarScope == VS_PUBLIC ) )
         hb_compCheckDuplVars( pFunc->pMemvars, szVarName );
   }
   else
      /* variable defined in a codeblock */
      hb_comp_iVarScope = VS_PARAMETER;

   hb_compCheckDuplVars( pFunc->pLocals, szVarName );

   pVar = ( PVAR ) hb_xgrab( sizeof( VAR ) );
   pVar->szName = szVarName;
   pVar->szAlias = NULL;
   pVar->cType = cValueType;
   pVar->iUsed = VU_NOT_USED;
   pVar->pNext = NULL;
   pVar->iDeclLine = hb_comp_iLine - 1;

   if ( toupper( cValueType ) == 'S' )
   {
      /*
      printf( "\nVariable %s is of Class: %s\n", szVarName, hb_comp_szFromClass );
      */

      pVar->pClass = hb_compClassFind( hb_comp_szFromClass );
      if( ! pVar->pClass )
      {
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, szVarName );
         pVar->cType = 'O';
      }

      /* Resetting */
      hb_comp_szFromClass = NULL;
   }

   if ( hb_comp_iVarScope & VS_PARAMETER )
      pVar->iUsed = VU_INITIALIZED;

   if( hb_comp_iVarScope & VS_MEMVAR )
   {
      PCOMSYMBOL pSym;
      USHORT wPos;

      /*printf( "\nAdding: %s in Function: %s\n", pVar->szName, pFunc->szName );*/

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
               if( ++hb_comp_functions.pLast->wParamNum > hb_comp_functions.pLast->wParamCount )
               {
                  hb_comp_functions.pLast->wParamCount = hb_comp_functions.pLast->wParamNum;
               }

               pSym = hb_compSymbolFind( szVarName, &wPos ); /* check if symbol exists already */
               if( ! pSym )
                  pSym = hb_compSymbolAdd( hb_strdup( szVarName ), &wPos );

               pSym->cScope |= VS_MEMVAR;

               /*printf( "\nAdded Symbol: %s Pos: %i\n", pSym->szName, wPos );*/

               hb_compGenPCode4( HB_P_PARAMETER, HB_LOBYTE( wPos ), HB_HIBYTE( wPos ), HB_LOBYTE( hb_comp_functions.pLast->wParamNum ), ( BOOL ) 0 );
            }

            if ( hb_comp_iWarnings >= 3 )
            {
               PVAR pMemVar = pFunc->pMemvars;

               while( pMemVar )
                  if( strcmp( pMemVar->szName, pVar->szName ) == 0 )
                      break;
                  else
                     pMemVar = pMemVar->pNext;

               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  if( ! pFunc->pPrivates )
                     pFunc->pPrivates = pVar;
                  else
                  {
                     pLastVar = pFunc->pPrivates;

                     while( pLastVar->pNext )
                        pLastVar = pLastVar->pNext;

                     pLastVar->pNext = pVar;
                  }
                  /*printf( "\nAdded Private: %s Type %c\n", pVar->szName, pVar->cType );*/
               }
            }

            break;

         case VS_PRIVATE:
            {
               pSym = hb_compSymbolFind( szVarName, &wPos ); /* check if symbol exists already */
               if( ! pSym )
                  pSym = hb_compSymbolAdd( hb_strdup( szVarName ), &wPos );

               pSym->cScope |= VS_MEMVAR;

               /*printf( "\nAdded Symbol: %s Pos: %i\n", pSym->szName, wPos );*/
            }

            if ( hb_comp_iWarnings >= 3 )
            {
               PVAR pMemVar = pFunc->pMemvars;

               while( pMemVar )
                  if( strcmp( pMemVar->szName, pVar->szName ) == 0 )
                     break;
                  else
                     pMemVar = pMemVar->pNext;

               /* Not declared as memvar. */
               if( pMemVar == NULL )
               {
                  /* add this variable to the list of PRIVATE variables. */
                  if( ! pFunc->pPrivates )
                     pFunc->pPrivates = pVar;
                  else
                  {
                     pLastVar = pFunc->pPrivates;

                     while( pLastVar->pNext )
                        pLastVar = pLastVar->pNext;

                     pLastVar->pNext = pVar;
                  }
                  /*printf( "\nAdded Private: %s Type %c\n", pVar->szName, pVar->cType );*/
               }
            }

            break;

         case VS_PUBLIC:
            {
               pSym = hb_compSymbolFind( szVarName, &wPos ); /* check if symbol exists already */
               if( ! pSym )
                  pSym = hb_compSymbolAdd( hb_strdup( szVarName ), &wPos );
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
                  wLocal++;
               }
               if( hb_comp_iVarScope == VS_PARAMETER )
               {
                  ++hb_comp_functions.pLast->wParamCount;
                  hb_comp_functions.pLast->bFlags |= FUN_USES_LOCAL_PARAMS;
               }
               if( hb_comp_bDebugInfo )
               {
                  BYTE * pBuffer;

                  pBuffer = ( BYTE * ) hb_xgrab( strlen( szVarName ) + 4 );

                  pBuffer[0] = HB_P_LOCALNAME;
                  pBuffer[1] = HB_LOBYTE( wLocal );
                  pBuffer[2] = HB_HIBYTE( wLocal );

                  memcpy( ( BYTE * ) ( & ( pBuffer[3] ) ), szVarName, strlen( szVarName ) + 1 );

                  hb_compGenPCodeN( pBuffer, strlen( szVarName ) + 4 , 0 );

                  hb_xfree( pBuffer );
               }
            }
            break;

         case VS_STATIC:
            {
               if( ! pFunc->pStatics )
                  pFunc->pStatics = pVar;
               else
               {
                  pLastVar = pFunc->pStatics;
                  while( pLastVar->pNext )
                     pLastVar = pLastVar->pNext;

                  pLastVar->pNext = pVar;
               }

               if( hb_comp_bDebugInfo )
               {
                  BYTE * pBuffer = ( BYTE * ) hb_xgrab( strlen( szVarName ) + 4 );
                  int iVar = hb_compStaticGetPos( szVarName, pFunc );

                  pBuffer[0] = HB_P_STATICNAME;
                  pBuffer[1] = HB_LOBYTE( iVar );
                  pBuffer[2] = HB_HIBYTE( iVar );

                  memcpy( ( BYTE * ) ( & ( pBuffer[3] ) ), szVarName, strlen( szVarName ) + 1 );

                  hb_compGenPCodeN( pBuffer, strlen( szVarName ) + 4 , 0 );

                  hb_xfree( pBuffer );
               }
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

/* Generate an error if passed variable name cannot be used in macro
 * expression.
 * Only MEMVAR or undeclared (memvar will be assumed) variables can be used.
 */
BOOL hb_compVariableMacroCheck( char * szVarName )
{
   BOOL bValid = FALSE;

   if( hb_compLocalGetPos( szVarName ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( hb_compStaticGetPos( szVarName, hb_comp_functions.pLast ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( hb_compFieldGetPos( szVarName, hb_comp_functions.pLast ) > 0 )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
   else if( ! hb_comp_bStartProc )
   {
      if( hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast ) == 0 )
      {
         /* This is not a local MEMVAR
          */
         if( hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst ) > 0 )
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
         else if( hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst ) > 0 )
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_BAD_MACRO, szVarName, NULL );
         else
            bValid = TRUE;    /* undeclared variable */
      }
      else
         bValid = TRUE;
   }
   else
      bValid = TRUE;    /* undeclared variable */
   return bValid;
}

PCOMCLASS hb_compClassAdd( char * szClassName )
{
   PCOMCLASS pClass;
   PCOMDECLARED pDeclared;

   /*printf( "Declaring Class: %s\n", szClassName );*/

   if ( hb_comp_iWarnings < 3 )
      return NULL;

   if ( ( pClass = hb_compClassFind( szClassName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Class", szClassName );
      return pClass;
   }

   pClass = ( PCOMCLASS ) hb_xgrab( sizeof( COMCLASS ) );

   pClass->szName = szClassName;
   pClass->pMethod = NULL;
   pClass->pNext = NULL;

   if ( hb_comp_pFirstClass == NULL )
      hb_comp_pFirstClass = pClass;
   else
      hb_comp_pLastClass->pNext = pClass;

   hb_comp_pLastClass = pClass;

   /* Auto declaration for the Class Function. */
   pDeclared = hb_compDeclaredAdd( szClassName );
   pDeclared->cType = 'S';
   pDeclared->pClass = pClass;

   return pClass;
}

PCOMCLASS hb_compClassFind( char * szClassName )
{
   PCOMCLASS pClass = hb_comp_pFirstClass;

   if ( hb_comp_iWarnings < 3 )
      return NULL;

   while( pClass )
   {
      if( ! strcmp( pClass->szName, szClassName ) )
         return pClass;
      else
      {
         if( pClass->pNext )
            pClass = pClass->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PCOMDECLARED hb_compMethodAdd( PCOMCLASS pClass, char * szMethodName )
{
   PCOMDECLARED pMethod;

   /*printf( "\nDeclaring Method: %s of Class: %s Pointer: %li\n", szMethodName, pClass->szName, pClass );*/

   if ( hb_comp_iWarnings < 3 )
      return NULL;

   if ( ( pMethod = hb_compMethodFind( pClass, szMethodName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Method", szMethodName );

      /* Last Declaration override previous declarations */
      pMethod->cParamTypes = NULL;
      pMethod->iParamCount = 0;
      pMethod->pParamClasses = NULL;

      return pMethod;
   }

   pMethod = ( PCOMDECLARED ) hb_xgrab( sizeof( COMDECLARED ) );

   pMethod->szName = szMethodName;
   pMethod->cType = ' '; /* Not known yet */
   pMethod->cParamTypes = NULL;
   pMethod->iParamCount = 0;
   pMethod->pParamClasses = NULL;
   pMethod->pNext = NULL;

   if ( pClass->pMethod == NULL )
      pClass->pMethod = pMethod;
   else
      pClass->pLastMethod->pNext = pMethod;

   pClass->pLastMethod = pMethod;

   hb_comp_pLastMethod = pMethod;

   return pMethod;
}

PCOMDECLARED hb_compMethodFind( PCOMCLASS pClass, char * szMethodName )
{
   PCOMDECLARED pMethod = NULL;

   if ( pClass )
     pMethod = pClass->pMethod;

   while( pMethod )
   {
      if( ! strcmp( pMethod->szName, szMethodName ) )
         return pMethod;
      else
      {
         if( pMethod->pNext )
            pMethod = pMethod->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

void hb_compDeclaredInit( void )
{
  #define _DECL static COMDECLARED

  /*
    \x5c -> ByRef    (+60)             '-'  -> NIL
    \x7a -> Optional (+90)             'U'  -> Undefined

    ' '  -> AnyType                    'A'  -> Array                     'B'  -> Array
    'A'  -> Array of AnyType           'a'  -> Array of Arrays           'b'  -> Array of Blocks
    \x7a -> Optional AnyType           \x9b -> Optional Array            \x9c -> Optional Block
    \x94 -> Optional Array of AnyType  \xb5 -> Optional Array of Arrays  \xb6 -> Optional Array of Blocks

    'C'  -> Character/String           'D'  -> Date                      'L'  -> Logical
    'c'  -> Array of Strings           'd'  -> Array of Dates            'l'  -> Array of Logicals
    \x9d -> Optional Character         \x9e -> Optional Date             \xa6 -> Optional Logical
    \xb7 -> Optional Array of Strings  \xb8 -> Optional Array of Dates   \xc0 -> Optional Array of Logicals

    'N'  -> Numeric                    'O'  -> Object                    'S'  -> Class
    'n'  -> Array of Numerics          'o'  -> Array of Objects          's'  -> Array of Classes
    \xa8 -> Optional Numeric           \xa9 -> Optional Object           \xad -> Optional Class
    \xc2 -> Optional Array of Numerics \xc3 -> Optional Array of Objects \xc7 -> Optional Array of Classes
   */

   /* ------------------------------------------------- Standard Functions -------------------------------------------------- */

   /*              Name        Ret  # of Prams  Param Types                     Ret Class  Param Classes  Next
                   ----------  ---  ----------  ------------------------------  ---------  -------------  ------ */
   _DECL s_001 = { "AADD"    , ' ', 2         , (BYTE*)"A "                   , NULL     , NULL         , NULL   };
   _DECL s_002 = { "ABS"     , 'N', 1         , (BYTE*)"N"                    , NULL     , NULL         , &s_001 };
   _DECL s_003 = { "ACHOICE" , 'N', 9         , (BYTE*)"NNNNc\x7a\x9d\xa8\xa8", NULL     , NULL         , &s_002 };

   /* TODO: Rest of Standard Functions. */

   /* -------------------------------------------------- Standard Classes --------------------------------------------------- */

   static COMCLASS s_ERROR    = { "ERROR"   , NULL, NULL, NULL };
   static COMCLASS s_GET      = { "GET"     , NULL, NULL, &s_ERROR };
   static COMCLASS s_TBCOLUMN = { "TBCOLUMN", NULL, NULL, &s_GET };
   static COMCLASS s_TBROWSE  = { "TBROWSE" , NULL, NULL, &s_TBCOLUMN };

  /*                       Name             Ret  # of Prams  Param Types           Ret Class  Param Classes  Next
                           ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_ERROR_01    = { "ARGS"         , 'A', 0         , (BYTE*)NULL         , NULL     , NULL         , NULL            };
   _DECL s_ERROR_02    = { "CANDEFAULT"   , 'B', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_01     };
   _DECL s_ERROR_03    = { "CANRETRY"     , 'B', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_02     };
   _DECL s_ERROR_04    = { "CANSUBSTITUTE", 'B', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_03     };
   _DECL s_ERROR_05    = { "CARGO"        , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_04     };
   _DECL s_ERROR_06    = { "DESCRIPTION"  , 'S', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_05     };
   _DECL s_ERROR_07    = { "FILENAME"     , 'S', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_06     };
   _DECL s_ERROR_08    = { "GENCODE"      , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_07     };
   _DECL s_ERROR_09    = { "OPERATION"    , 'S', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_08     };
   _DECL s_ERROR_10    = { "OSCODE"       , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_09     };
   _DECL s_ERROR_11    = { "SEVERITY"     , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_10     };
   _DECL s_ERROR_12    = { "SUBCODE"      , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_11     };
   _DECL s_ERROR_13    = { "SUBSYSTEM"    , 'S', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_12     };
   _DECL s_ERROR_14    = { "TRIES"        , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_ERROR_13     };

  /*                       Name             Ret  # of Prams  Param Types           Ret Class  Param Classes  Next
                           ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_GET_01      = { "ASSIGN"       , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , NULL            };
   _DECL s_GET_02      = { "COLORDISP"    , 'S', 1         , (BYTE*)"\x9d"       , &s_GET   , NULL         , &s_GET_01       };
   _DECL s_GET_03      = { "DISPLAY"      , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_02       };
   _DECL s_GET_04      = { "KILLFOCUS"    , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_03       };
   _DECL s_GET_05      = { "PARSEPICT"    , 'C', 1         , (BYTE*)"C"          , NULL     , NULL         , &s_GET_04       };
   _DECL s_GET_06      = { "RESET"        , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_05       };
   _DECL s_GET_07      = { "SETFOCUS"     , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_06       };
   _DECL s_GET_08      = { "UNDO"         , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_07       };
   _DECL s_GET_09      = { "UNTRANSFORM"  , 'S', 1         , (BYTE*)"\x9d"       , &s_GET   , NULL         , &s_GET_08       };
   _DECL s_GET_10      = { "UPDATEBUFFER" , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_09       };
   _DECL s_GET_11      = { "VARGET"       , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_GET_10       };
   _DECL s_GET_12      = { "VARPUT"       , ' ', 1         , (BYTE*)" "          , NULL     , NULL         , &s_GET_11       };

   _DECL s_GET_13      = { "END"          , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_12       };
   _DECL s_GET_14      = { "HOME"         , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_13       };
   _DECL s_GET_15      = { "LEFT"         , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_14       };
   _DECL s_GET_16      = { "RIGHT"        , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_15       };
   _DECL s_GET_17      = { "TODECPOS"     , 'S', 0         , (BYTE*)"L"          , &s_GET   , NULL         , &s_GET_16       };
   _DECL s_GET_18      = { "WORDLEFT"     , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_17       };
   _DECL s_GET_19      = { "WORDRIGHT"    , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_18       };

   _DECL s_GET_20      = { "BACKSPACE"    , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_19       };
   _DECL s_GET_21      = { "DELETE"       , 'S', 1         , (BYTE*)"\xa6"       , &s_GET   , NULL         , &s_GET_20       };
   _DECL s_GET_22      = { "DELEND"       , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_21       };
   _DECL s_GET_23      = { "DELLEFT"      , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_22       };
   _DECL s_GET_24      = { "DELRIGHT"     , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_23       };
   _DECL s_GET_25      = { "DELWORDLEFT"  , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_24       };
   _DECL s_GET_26      = { "DELWORDRIGHT" , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_25       };

   _DECL s_GET_27      = { "INSERT"       , 'S', 1         , (BYTE*)"C"          , &s_GET   , NULL         , &s_GET_26       };
   _DECL s_GET_28      = { "OVERSTRIKE"   , 'S', 1         , (BYTE*)"C"          , &s_GET   , NULL         , &s_GET_27       };

   _DECL s_GET_29      = { "DELETEALL"    , 'S', 0         , (BYTE*)NULL         , &s_GET   , NULL         , &s_GET_28       };
   _DECL s_GET_30      = { "ISEDITABLE"   , 'L', 1         , (BYTE*)"N"          , NULL     , NULL         , &s_GET_29       };
   _DECL s_GET_31      = { "INPUT"        , 'C', 0         , (BYTE*)"C"          , NULL     , NULL         , &s_GET_30       };
   _DECL s_GET_32      = { "PUTMASK"      , 'C', 2         , (BYTE*)"CL"         , NULL     , NULL         , &s_GET_31       };
   _DECL s_GET_33      = { "HASSCROLL"    , 'L', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_GET_32       };

  /*                       Name             Ret  # of Prams  Param Types           Ret Class  Param Classes  Next
                           ---------------  ---  ----------  --------------------  ---------  -------------  --------------- */
   _DECL s_TBCOLUMN_01 = { "BLOCK"        , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , NULL            };
   _DECL s_TBCOLUMN_02 = { "CARGO"        , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_01  };
   _DECL s_TBCOLUMN_03 = { "COLORBLOCK"   , 'A', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_02  };
   _DECL s_TBCOLUMN_04 = { "COLSEP"       , 'C', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_03  };
   _DECL s_TBCOLUMN_05 = { "DEFCOLOR"     , 'A', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_04  };
   _DECL s_TBCOLUMN_06 = { "FOOTING"      , 'C', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_05  };
   _DECL s_TBCOLUMN_07 = { "FOOTSEP"      , 'C', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_06  };
   _DECL s_TBCOLUMN_08 = { "HEADING"      , 'C', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_07  };
   _DECL s_TBCOLUMN_09 = { "PICTURE"      , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_08  };
   _DECL s_TBCOLUMN_10 = { "WIDTH"        , 'N', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_09  };
   _DECL s_TBCOLUMN_11 = { "COLPOS"       , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_10  };
   _DECL s_TBCOLUMN_12 = { "HEADSEP"      , ' ', 0         , (BYTE*)NULL         , NULL     , NULL         , &s_TBCOLUMN_11  };

   /* TODO: Finish definition of GET, and add definitions for TBROWSE. */

   #undef _DECL

   /* ------- */

   /* First (bottom) Method */
   s_ERROR.pMethod     = &s_ERROR_14;
   /* Last (top) Method. */
   s_ERROR.pLastMethod = &s_ERROR_01;

   /* ------- */

   /* First (bottom) Method */
   s_GET.pMethod     = &s_GET_33; /* Change to BOTTOM Method. */
   /* Last (top) Method. */
   s_GET.pLastMethod = &s_GET_01;

   /* ------- */

   /* First (bottom) Method */
   s_TBCOLUMN.pMethod     = &s_TBCOLUMN_12; /* Change to BOTTOM Method. */
   /* Last (top) Method. */
   s_TBCOLUMN.pLastMethod = &s_TBCOLUMN_01;

   /* ------- */

   hb_comp_pFirstDeclared   = &s_003; /* Change to BOTTOM Function. */
   hb_comp_pLastDeclared    = &s_001;
   hb_comp_pReleaseDeclared = &s_001;

   hb_comp_pFirstClass      = &s_TBROWSE;
   hb_comp_pLastClass       = &s_ERROR;
   hb_comp_pReleaseClass    = &s_ERROR;
}

PCOMDECLARED hb_compDeclaredAdd( char * szDeclaredName )
{
   PCOMDECLARED pDeclared;

   if ( hb_comp_iWarnings < 3 )
      return NULL;

   /*printf( "\nDeclaring Function: %s\n", szDeclaredName, NULL );*/

   if ( ( pDeclared = hb_compDeclaredFind( szDeclaredName ) ) != NULL )
   {
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUP_DECLARATION, "Function", szDeclaredName );

      /* Last declaration will take effect. */
      pDeclared->cType = ' '; /* Not known yet */
      pDeclared->cParamTypes = NULL;
      pDeclared->iParamCount = 0;
      pDeclared->pParamClasses = NULL;

      return pDeclared;
   }

   pDeclared = ( PCOMDECLARED ) hb_xgrab( sizeof( COMDECLARED ) );

   pDeclared->szName = szDeclaredName;
   pDeclared->cType = ' '; /* Not known yet */
   pDeclared->cParamTypes = NULL;
   pDeclared->iParamCount = 0;
   pDeclared->pParamClasses = NULL;
   pDeclared->pNext = NULL;

   hb_comp_pLastDeclared->pNext = pDeclared;
   hb_comp_pLastDeclared = pDeclared;

   return pDeclared;
}

PCOMSYMBOL hb_compSymbolAdd( char * szSymbolName, USHORT * pwPos )
{
   PCOMSYMBOL pSym;

   if( szSymbolName[ 0 ] )
   {
      /* Create a symbol for non-empty names only.
       * NOTE: an empty name is passed for a fake starting function when
       * '-n' switch is used
      */
      pSym = ( PCOMSYMBOL ) hb_xgrab( sizeof( COMSYMBOL ) );

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
         *pwPos = hb_comp_symbols.iCount -1; /* position number starts form 0 */
   }
   else
      pSym = NULL;

   return pSym;
}

/*
 * This function creates and initialises the _FUNC structure
 */
static PFUNCTION hb_compFunctionNew( char * szName, HB_SYMBOLSCOPE cScope )
{
   PFUNCTION pFunc;

   pFunc = ( PFUNCTION ) hb_xgrab( sizeof( _FUNC ) );
   pFunc->szName          = szName;
   pFunc->cScope          = cScope;
   pFunc->pLocals         = NULL;
   pFunc->pStatics        = NULL;
   pFunc->pFields         = NULL;
   pFunc->pMemvars        = NULL;
   pFunc->pPrivates       = NULL;
   pFunc->pCode           = NULL;
   pFunc->lPCodeSize      = 0;
   pFunc->lPCodePos       = 0;
   pFunc->pNext           = NULL;
   pFunc->wParamCount     = 0;
   pFunc->wParamNum       = 0;
   pFunc->iStaticsBase    = hb_comp_iStaticCnt;
   pFunc->pOwner          = NULL;
   pFunc->bFlags          = 0;
   pFunc->iNOOPs          = 0;
   pFunc->iJumps          = 0;
   pFunc->pNOOPs          = NULL;
   pFunc->pJumps          = NULL;
   pFunc->pStack          = NULL;
   pFunc->iStackSize      = 0;
   pFunc->iStackIndex     = 0;
   pFunc->iStackFunctions = 0;
   pFunc->iStackClasses   = 0;

   return pFunc;
}

static PINLINE hb_compInlineNew( char * szName )
{
   PINLINE pInline;

   pInline = ( PINLINE ) hb_xgrab( sizeof( _INLINE ) );

   pInline->szName     = szName;
   pInline->pCode      = NULL;
   pInline->lPCodeSize = 0;
   pInline->pNext      = NULL;
   pInline->szFileName = hb_strdup( hb_comp_files.pLast->szFileName );
   pInline->iLine      = hb_comp_iLine - 1;

   return pInline;
}

/*
 * Stores a Clipper defined function/procedure
 * szFunName - name of a function
 * cScope    - scope of a function
 * iType     - FUN_PROCEDURE if a procedure or 0
 */
void hb_compFunctionAdd( char * szFunName, HB_SYMBOLSCOPE cScope, int iType )
{
   PCOMSYMBOL   pSym;
   PFUNCTION pFunc;
   char * szFunction;

   hb_compFinalizeFunction();    /* fix all previous function returns offsets */

   pFunc = hb_compFunctionFind( szFunName );
   if( pFunc )
   {
      /* The name of a function/procedure is already defined */
      if( ( pFunc != hb_comp_functions.pFirst ) || hb_comp_bStartProc )
         /* it is not a starting procedure that was automatically created */
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_FUNC_DUPL, szFunName, NULL );
   }

   szFunction = hb_compReservedName( szFunName );
   if( szFunction && !( hb_comp_functions.iCount==0 && !hb_comp_bStartProc ) )
   {
      /* We are ignoring it when it is the name of PRG file and we are
       * not creating implicit starting procedure
       */
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FUNC_RESERVED, szFunction, szFunName );
   }

   hb_comp_iFunctionCnt++;

   pSym = hb_compSymbolFind( szFunName, NULL );
   if( ! pSym )
      /* there is not a symbol on the symbol table for this function name */
      pSym = hb_compSymbolAdd( szFunName, NULL );

   if( pSym && cScope != HB_FS_PUBLIC )
      pSym->cScope |= cScope; /* we may have a non public function and a object message */

   pFunc = hb_compFunctionNew( szFunName, cScope );
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

   hb_compGenPCode3( HB_P_FRAME, 0, 0, ( BOOL ) 0 );   /* frame for locals and parameters */
   hb_compGenPCode3( HB_P_SFRAME, 0, 0, ( BOOL ) 0 );     /* frame for statics variables */

   if( hb_comp_bDebugInfo )
   {
      BYTE * pBuffer;

      pBuffer = ( BYTE * ) hb_xgrab( 3 + strlen( hb_comp_files.pLast->szFileName ) + strlen( szFunName ) );

      pBuffer[0] = HB_P_MODULENAME;

      memcpy( ( BYTE * ) ( &( pBuffer[1] ) ), ( BYTE * ) hb_comp_files.pLast->szFileName, strlen( hb_comp_files.pLast->szFileName ) );

      pBuffer[ strlen( hb_comp_files.pLast->szFileName ) + 1 ] = ':';

      memcpy( ( BYTE * ) ( &( pBuffer[ strlen( hb_comp_files.pLast->szFileName ) + 2 ] ) ), ( BYTE * ) szFunName, strlen( szFunName ) + 1 );

      hb_compGenPCodeN( pBuffer, 3 + strlen( hb_comp_files.pLast->szFileName ) + strlen( szFunName ), 0 );

      hb_xfree( pBuffer );
   }
   hb_comp_bDontGenLineNum = FALSE; /* reset the flag */
}

PINLINE hb_compInlineAdd( char * szFunName )
{
   PINLINE pInline;
   PCOMSYMBOL   pSym;

   if( szFunName )
   {
      pSym = hb_compSymbolFind( szFunName, NULL );
      if( ! pSym )
      {
         pSym = hb_compSymbolAdd( szFunName, NULL );
      }
      if( pSym )
      {
         pSym->cScope |= HB_FS_STATIC;
      }
   }
   pInline = hb_compInlineNew( szFunName );

   if( hb_comp_inlines.iCount == 0 )
   {
      hb_comp_inlines.pFirst = pInline;
      hb_comp_inlines.pLast  = pInline;
   }
   else
   {
      hb_comp_inlines.pLast->pNext = pInline;
      hb_comp_inlines.pLast = pInline;
   }

   hb_comp_inlines.iCount++;

   return pInline;
}

/* create an ANNOUNCEd procedure
 */
void hb_compAnnounce( char * szFunName )
{
   PFUNCTION pFunc;

   pFunc = hb_compFunctionFind( szFunName );
   if( pFunc )

   {
      /* there is a function/procedure defined already - ANNOUNCEd procedure
       * have to be a public symbol - check if existing symbol is public
       */
      if( pFunc->cScope & HB_FS_STATIC )
         hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_FUNC_ANNOUNCE, szFunName, NULL );
   }
   else
   {
      PCOMSYMBOL pSym;

      /* create a new procedure
       */
      pSym = hb_compSymbolAdd( szFunName, NULL );
      pSym->cScope = HB_FS_PUBLIC;

      pFunc = hb_compFunctionNew( szFunName, HB_FS_PUBLIC );
      pFunc->bFlags |= FUN_PROCEDURE;

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

      /* this function have a very limited functionality
       */
      hb_compGenPCode1( HB_P_ENDPROC );
   }
}

/* NOTE: Names of variables and functions are released in hbident.c on exit */
PFUNCTION hb_compFunctionKill( PFUNCTION pFunc )
{
   PFUNCTION pNext = pFunc->pNext;
   PVAR pVar;

   while( pFunc->pLocals )
   {
      pVar = pFunc->pLocals;
      pFunc->pLocals = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pStatics )
   {
      pVar = pFunc->pStatics;
      pFunc->pStatics = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pFields )
   {
      pVar = pFunc->pFields;
      pFunc->pFields = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pMemvars )
   {
      pVar = pFunc->pMemvars;
      pFunc->pMemvars = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   while( pFunc->pPrivates )
   {
      pVar = pFunc->pPrivates;
      pFunc->pPrivates = pVar->pNext;

      hb_xfree( ( void * ) pVar );
   }

   /* Release the NOOP array. */
   if( pFunc->pNOOPs )
      hb_xfree( ( void * ) pFunc->pNOOPs );

   /* Release the Jumps array. */
   if( pFunc->pJumps )
      hb_xfree( ( void * ) pFunc->pJumps );

   hb_xfree( ( void * ) pFunc->pCode );
/* hb_xfree( ( void * ) pFunc->szName ); The name will be released in hb_compSymbolKill() */
   hb_xfree( ( void * ) pFunc );

   return pNext;
}

/* NOTE: Name of symbols are released in hbident.c  on exit */
PCOMSYMBOL hb_compSymbolKill( PCOMSYMBOL pSym )
{
   PCOMSYMBOL pNext = pSym->pNext;

   hb_xfree( ( void * ) pSym );

   return pNext;
}

void hb_compGenBreak( void )
{
   hb_compGenPushSymbol( hb_strdup("BREAK"), 1 );
   hb_compGenPushNil();
}

void hb_compExternGen( void ) /* generates the symbols for the EXTERN names */
{
   PEXTERN pDelete;

   if( hb_comp_bDebugInfo )
      hb_compExternAdd( hb_strdup( "__DBGENTRY" ) );

   while( hb_comp_pExterns )
   {
      if( hb_compSymbolFind( hb_comp_pExterns->szName, NULL ) )
      {
         if( ! hb_compFunCallFind( hb_comp_pExterns->szName ) )
            hb_compFunCallAdd( hb_comp_pExterns->szName );
      }
      else
      {
         hb_compSymbolAdd( hb_comp_pExterns->szName, NULL );
         hb_compFunCallAdd( hb_comp_pExterns->szName );
      }
      pDelete  = hb_comp_pExterns;
      hb_comp_pExterns = hb_comp_pExterns->pNext;
      hb_xfree( ( void * ) pDelete );
   }
}

PFUNCTION hb_compFunCallFind( char * szFunctionName ) /* returns a previously called defined function */
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

PFUNCTION hb_compFunctionFind( char * szFunctionName ) /* returns a previously defined function */
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

PINLINE hb_compInlineFind( char * szFunctionName )
{
   PINLINE pInline = hb_comp_inlines.pFirst;

   while( pInline )
   {
      if( pInline->szName && strcmp( pInline->szName, szFunctionName ) == 0 )
         return pInline;
      else
      {
         if( pInline->pNext )
            pInline = pInline->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

/* return variable using its order after final fixing */
PVAR hb_compLocalVariableFind( PFUNCTION pFunc, USHORT wVar )
{
   if( pFunc->wParamCount && !(pFunc->bFlags & FUN_USES_LOCAL_PARAMS) )
   {
      wVar -= pFunc->wParamCount;
   }
   return hb_compVariableFind( pFunc->pLocals, wVar );
}

PVAR hb_compVariableFind( PVAR pVars, USHORT wOrder ) /* returns variable if defined or zero */
{
   USHORT w = 1;

   if( pVars )
       while( pVars->pNext && w++ < wOrder )
          pVars = pVars->pNext;

   return pVars;
}

USHORT hb_compVariableGetPos( PVAR pVars, char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   USHORT wVar = 1;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
      {
         if ( hb_comp_iWarnings < 3 )
            pVars->iUsed |= VU_USED;
         /*
         else
            Handled by hb_compStrongType()
         */

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

int hb_compLocalGetPos( char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   int iVar;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( pFunc->szName )
   {
      /* we are in a function/procedure -we don't need any tricks */
      if( pFunc->pOwner )
         pFunc =pFunc->pOwner;
      iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );
   }
   else
   {
      /* we are in a codeblock */
      iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );
      if( iVar == 0 )
      {
         /* this is not a current codeblock parameter
         * we have to check the list of nested codeblocks up to a function
         * where the codeblock is defined
         */
         PFUNCTION pOutBlock = pFunc;   /* the outermost codeblock */
         BOOL bStatic;

         pFunc = pFunc->pOwner;
         while( pFunc )
         {
            bStatic = FALSE;
            if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) == ( HB_FS_INIT | HB_FS_EXIT ) )
            {
               /* we are in a codeblock used to initialize a static variable -
                * skip to a function where this static variable was declared
                */
               pFunc = pFunc->pOwner;
               bStatic = TRUE;
            }

            iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );
            if( iVar )
            {
               if( pFunc->pOwner )
               {
                  /* this variable is defined in a parent codeblock
                  * It is not possible to access a parameter of a codeblock in which
                  * the current codeblock is defined
                  */
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTER_VAR, szVarName, NULL );
                  return iVar;
               }
               else if( bStatic )
               {
                  /* local variable was referenced in a codeblock during
                   * initialization of static variable. This cannot be supported
                   * because static variables are initialized at program
                   * startup when there is no local variables yet - hence we
                   * cannot detach this local variable
                   * For example:
                   * LOCAL locvar
                   * STATIC stavar:={ | x | locvar}
                   *
                   * NOTE: Clipper creates such a codeblock however at the
                   * time of codeblock evaluation it generates a runtime error:
                   * 'bound error: array acccess'
                   * Called from: (b)STATICS$(0)
                   */
                  hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_ILLEGAL_INIT, "(b)", szVarName );
                  return iVar;
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
                  iVar = -hb_compVariableGetPos( pOutBlock->pStatics, szVarName );
                  if( iVar == 0 )
                  {
                     /* this variable was not referenced yet - add it to the list */
                     PVAR pVar;

                     pVar = ( PVAR ) hb_xgrab( sizeof( VAR ) );
                     pVar->szName = szVarName;
                     pVar->cType = ' ';
                     pVar->iUsed = VU_NOT_USED;
                     pVar->pNext  = NULL;
                     pVar->iDeclLine = hb_comp_iLine - 1;

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

/* Checks if passed variable name is declared as STATIC
 * Returns 0 if not found in STATIC list or its position in this list if found
 *
 * All static variables are hold in a single array at runtime then positions
 * are numbered for whole PRG module.
 */
int hb_compStaticGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   while( pFunc->pOwner )     /* pOwner is not NULL if STATIC var := value is used */
      pFunc = pFunc->pOwner;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pStatics, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      iVar = hb_compVariableGetPos( pFunc->pStatics, szVarName );
   }
   if( iVar )
      iVar += pFunc->iStaticsBase;

   return iVar;
}

/* Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 */
int hb_compFieldGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pFields, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      iVar = hb_compVariableGetPos( pFunc->pFields, szVarName );
   }
   return iVar;
}

/* Checks if passed variable name is declared as MEMVAR
 * Returns 0 if not found in MEMVAR list or its position in this list if found
 */
int hb_compMemvarGetPos( char * szVarName, PFUNCTION pFunc )
{
   int iVar;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar = hb_compVariableGetPos( pFunc->pMemvars, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
      iVar = hb_compVariableGetPos( pFunc->pMemvars, szVarName );
   }
   return iVar;
}

/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table.
 * NOTE: symbol's position number starts from 0
 */
PCOMDECLARED hb_compDeclaredFind( char * szDeclaredName )
{
   PCOMDECLARED pSym = hb_comp_pFirstDeclared;

   while( pSym )
   {
      if( ! strcmp( pSym->szName, szDeclaredName ) )
         return pSym;
      else
      {
         if( pSym->pNext )
            pSym = pSym->pNext;
         else
            return NULL;
      }
   }
   return NULL;
}

PCOMSYMBOL hb_compSymbolFind( char * szSymbolName, USHORT * pwPos )
{
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT wCnt = 0;

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

/* returns a symbol based on its index on the symbol table
 * index starts from 0
*/
PCOMSYMBOL hb_compSymbolGetPos( USHORT wSymbol )
{
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT w = 0;

   while( w++ < wSymbol && pSym->pNext )
      pSym = pSym->pNext;

   return pSym;
}

USHORT hb_compFunctionGetPos( char * szFunctionName ) /* return 0 if not found or order + 1 */
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

void hb_compNOOPadd( PFUNCTION pFunc, ULONG ulPos )
{
   pFunc->iNOOPs++;

   if( pFunc->pNOOPs )
   {
      pFunc->pNOOPs = ( ULONG * ) hb_xrealloc( pFunc->pNOOPs, sizeof( ULONG ) * pFunc->iNOOPs );
      pFunc->pNOOPs[ pFunc->iNOOPs - 1 ] = ulPos;
   }
   else
   {
      pFunc->pNOOPs = ( ULONG * ) hb_xgrab( sizeof( ULONG ) );
      pFunc->pNOOPs[ pFunc->iNOOPs - 1 ] = ulPos;
   }
}

/* NOTE: To disable jump optimization, just make this function a dummy one.
         [vszakats] */

static void hb_compPrepareOptimize()
{
   hb_comp_functions.pLast->iJumps++;

   if( hb_comp_functions.pLast->pJumps )
   {
      hb_comp_functions.pLast->pJumps = ( ULONG * ) hb_xrealloc( hb_comp_functions.pLast->pJumps, sizeof( ULONG ) * hb_comp_functions.pLast->iJumps );
      hb_comp_functions.pLast->pJumps[ hb_comp_functions.pLast->iJumps - 1 ] = ( ULONG ) ( hb_comp_functions.pLast->lPCodePos - 4 );
   }
   else
   {
      hb_comp_functions.pLast->pJumps = ( ULONG * ) hb_xgrab( sizeof( ULONG ) );
      hb_comp_functions.pLast->pJumps[ hb_comp_functions.pLast->iJumps - 1 ] = ( LONG ) ( hb_comp_functions.pLast->lPCodePos - 4 );
   }

   /* Only Reserve Request - Don't know if NOOPs will remain yet. */
   if( hb_comp_functions.pLast->pCode[ hb_comp_functions.pLast->lPCodePos - 3 ] == 0 &&
       hb_comp_functions.pLast->pCode[ hb_comp_functions.pLast->lPCodePos - 2 ] == 0 &&
       hb_comp_functions.pLast->pCode[ hb_comp_functions.pLast->lPCodePos - 1 ] == 0 )
      return;

   /* 3rd. Byte might be not used */
   if( hb_comp_functions.pLast->pCode[ hb_comp_functions.pLast->lPCodePos - 1 ] == HB_P_NOOP )
   {
      hb_compNOOPadd( hb_comp_functions.pLast, hb_comp_functions.pLast->lPCodePos - 1 );

      /* 2nd. Byte might be not used */
      if( hb_comp_functions.pLast->pCode[ hb_comp_functions.pLast->lPCodePos - 2 ] == HB_P_NOOP )
      {
         hb_compNOOPadd( hb_comp_functions.pLast, hb_comp_functions.pLast->lPCodePos - 2 );
      }
   }
}

ULONG hb_compGenJump( LONG lOffset )
{
   /* Just a place holder, it might be a far jump...*/
   if( lOffset == 0 )
   {
      hb_compGenPCode4( HB_P_JUMPFAR, 0, 0, 0, ( BOOL ) 1 );
   }
   else if( lOffset >= -128 && lOffset <= 127 )
   {
      hb_compGenPCode4( HB_P_JUMPNEAR, HB_LOBYTE( lOffset ), HB_P_NOOP, HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= SHRT_MIN && lOffset <= SHRT_MAX )
   {
      hb_compGenPCode4( HB_P_JUMP, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= (-8388608L) && lOffset <= 8388607L )
   {
      hb_compGenPCode4( HB_P_JUMPFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), ( BYTE ) ( ( ( USHORT ) ( lOffset ) >> 16 ) & 0xFF ), ( BOOL ) 1 );
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
   }

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

ULONG hb_compGenJumpFalse( LONG lOffset )
{
   /* Just a place holder, it might be a far jump...*/
   if( lOffset == 0 )
   {
      hb_compGenPCode4( HB_P_JUMPFALSEFAR, 0, 0, 0, ( BOOL ) 1 );
   }
   else if( lOffset >= -128 && lOffset <= 127 )
   {
      hb_compGenPCode4( HB_P_JUMPFALSENEAR, HB_LOBYTE( lOffset ), HB_P_NOOP, HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= SHRT_MIN && lOffset <= SHRT_MAX )
   {
      hb_compGenPCode4( HB_P_JUMPFALSE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= (-8388608L) && lOffset <= 8388607L )
   {
      hb_compGenPCode4( HB_P_JUMPFALSEFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), ( BYTE ) ( ( ( USHORT ) ( lOffset ) >> 16 ) & 0xFF ), ( BOOL ) 1 );
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
   }

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

ULONG hb_compGenJumpTrue( LONG lOffset )
{
   /* Just a place holder, it might be a far jump...*/
   if( lOffset == 0 )
   {
      hb_compGenPCode4( HB_P_JUMPTRUEFAR, 0, 0, 0, ( BOOL ) 1 );
   }
   else if( lOffset >= -128 && lOffset <= 127 )
   {
      hb_compGenPCode4( HB_P_JUMPTRUENEAR, HB_LOBYTE( lOffset ), HB_P_NOOP, HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= SHRT_MIN && lOffset <= SHRT_MAX )
   {
      hb_compGenPCode4( HB_P_JUMPTRUE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), HB_P_NOOP, ( BOOL ) 1 );
   }
   else if( lOffset >= (-8388608L) && lOffset <= 8388607L )
   {
      hb_compGenPCode4( HB_P_JUMPTRUEFAR, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ), ( BYTE ) ( ( ( USHORT ) ( lOffset ) >> 16 ) & 0xFF ), ( BOOL ) 1 );
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
   }

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

void hb_compGenJumpThere( ULONG ulFrom, ULONG ulTo )
{
   BYTE * pCode = hb_comp_functions.pLast->pCode;
   LONG lOffset = ulTo - ulFrom + 1;
   BOOL bOptimize = TRUE;

   if( lOffset >= -128 && lOffset <= 127 )
   {
      switch( pCode[ ( ULONG ) ( ulFrom - 1 ) ] )
      {
         /*
         case HB_P_JUMPNEAR :
            break;

         case HB_P_JUMPTRUENEAR :
            break;

         case HB_P_JUMPFALSENEAR :
            break;

         case HB_P_JUMP :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPNEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPTRUE :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPTRUENEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPFALSE :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPFALSENEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;
         */

         case HB_P_JUMPFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPNEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPTRUEFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPTRUENEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPFALSEFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPFALSENEAR;
            pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_P_NOOP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_SEQBEGIN :
            bOptimize = FALSE;
            break;

         case HB_P_SEQEND :
            bOptimize = FALSE;
            break;

         default:
/*            printf( "\rPCode: %i", pCode[ ( ULONG ) ulFrom - 1 ] ); */
            hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_NOT_FOUND, NULL, NULL );
            break;
      }

      pCode[ ( ULONG ) ulFrom ] = HB_LOBYTE( lOffset );

      if( ! bOptimize  )
         return;

      /* Check if 3rd. Byte not used. */
      if( pCode[ ( ULONG ) ( ulFrom + 2 ) ] == HB_P_NOOP )
      {
         hb_compNOOPadd( hb_comp_functions.pLast, ulFrom + 2 );

         /* Check if 2nd. Byte not used. */
         if( pCode[ ( ULONG ) ulFrom + 1 ] == HB_P_NOOP )
            hb_compNOOPadd( hb_comp_functions.pLast, ulFrom + 1 );
      }
   }
   else if( lOffset >= SHRT_MIN && lOffset <= SHRT_MAX )
   {
      switch( pCode[ ( ULONG ) ( ulFrom - 1 ) ] )
      {
         /*
         case HB_P_JUMPNEAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPTRUENEAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPTRUE;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPFALSENEAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPFALSE;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMP :
            break;

         case HB_P_JUMPTRUE :
            break;

         case HB_P_JUMPFALSE :
            break;
         */

         case HB_P_JUMPFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMP;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPTRUEFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPTRUE;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_JUMPFALSEFAR :
            pCode[ ( ULONG ) ( ulFrom - 1 ) ] = HB_P_JUMPFALSE;
            pCode[ ( ULONG ) ( ulFrom + 2 ) ] = HB_P_NOOP;
            break;

         case HB_P_SEQBEGIN :
            bOptimize = FALSE;
            break;

         case HB_P_SEQEND :
            bOptimize = FALSE;
            break;

         default:
/*            printf( "\rPCode: %i", pCode[ ( ULONG ) ulFrom - 1 ] ); */
            hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_NOT_FOUND, NULL, NULL );
            break;
      }

      pCode[ ulFrom ] = HB_LOBYTE( lOffset );
      pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_HIBYTE( lOffset );

      if( ! bOptimize )
         return;

      /* Check if 3rd. Byte not used. */
      if( pCode[ ( ULONG ) ulFrom + 2 ] == HB_P_NOOP )
         hb_compNOOPadd( hb_comp_functions.pLast, ulFrom + 2 );
   }
   else if( lOffset >= ( -8388608L ) && lOffset <= 8388607L )
   {
      pCode[ ulFrom ] = HB_LOBYTE( lOffset );
      pCode[ ( ULONG ) ( ulFrom + 1 ) ] = HB_HIBYTE( lOffset );
      pCode[ ( ULONG ) ( ulFrom + 2 ) ] = ( BYTE ) ( ( ( USHORT ) ( lOffset ) >> 16 ) & 0xFF );
   }
   else
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_JUMP_TOO_LONG, NULL, NULL );
}

void hb_compGenJumpHere( ULONG ulOffset )
{
   hb_compGenJumpThere( ulOffset, hb_comp_functions.pLast->lPCodePos );
}

void hb_compLinePush( void ) /* generates the pcode with the currently compiled source code line */
{
   if( hb_comp_bLineNumbers && ! hb_comp_bDontGenLineNum )
   {
      if( ( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastLinePos ) > 3 ) || hb_comp_bDebugInfo )
      {
         hb_comp_ulLastLinePos = hb_comp_functions.pLast->lPCodePos;
         hb_compGenPCode3( HB_P_LINE, HB_LOBYTE( hb_comp_iLine-1 ), HB_HIBYTE( hb_comp_iLine-1 ), ( BOOL ) 0 );
      }
      else
      {
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +1 ] = HB_LOBYTE( hb_comp_iLine-1 );
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +2 ] = HB_HIBYTE( hb_comp_iLine-1 );
      }
   }

   if( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE )
   {
      /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_UNREACHABLE, NULL, NULL );
   }
   hb_comp_bDontGenLineNum = FALSE;
   /* clear RETURN/BREAK flag */
   hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE );

   /* Resting Compile Time Stack */
   hb_comp_functions.pLast->iStackIndex = 0;
   hb_comp_functions.pLast->iStackFunctions = 0;
   hb_comp_functions.pLast->iStackClasses = 0;
}

/* Generates the pcode with the currently compiled source code line
 * if debug code was requested only
 */
void hb_compLinePushIfDebugger( void )
{
   if( hb_comp_bDebugInfo )
      hb_compLinePush();
   else
   {
      if( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE )
      {
         /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_UNREACHABLE, NULL, NULL );
      }
      hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE );  /* clear RETURN flag */
   }
}

void hb_compLinePushIfInside( void ) /* generates the pcode with the currently compiled source code line */
{
   /* This line can be placed inside a procedure or function only
    * except EXTERNAL
    */
   if( ! hb_comp_bExternal )
   {
      if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 )
      {
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_OUTSIDE, NULL, NULL );
      }
   }

   hb_comp_functions.pLast->bFlags |= FUN_STATEMENTS;
   hb_compLinePush();
}

/*
 * Function generates pcode for undeclared variable
 */
static void hb_compGenVariablePCode( BYTE bPCode, char * szVarName )
{
   BOOL bGenCode;
   /*
   * NOTE:
   * Clipper always assumes a memvar variable if undeclared variable
   * is popped (a value is asssigned to a variable).
   */
   if( HB_COMP_ISSUPPORTED( HB_COMPFLAG_HARBOUR ) )
      bGenCode = hb_comp_bForceMemvars;    /* harbour compatibility */
   else
      bGenCode = ( hb_comp_bForceMemvars || bPCode == HB_P_POPVARIABLE );

   if( bGenCode )
   {
      /* -v switch was used -> assume it is a memvar variable
       */
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_MEMVAR_ASSUMED, szVarName, NULL );

      if( bPCode == HB_P_POPVARIABLE )
         bPCode = HB_P_POPMEMVAR;
      else if( bPCode == HB_P_PUSHVARIABLE )
         bPCode = HB_P_PUSHMEMVAR;
      else
         bPCode = HB_P_PUSHMEMVARREF;
   }
   else
      hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_AMBIGUOUS_VAR, szVarName, NULL );

   hb_compGenVarPCode( bPCode, szVarName );
}

/* Generate a pcode for a field variable
 */
void hb_compGenFieldPCode( BYTE bPCode, int wVar, char * szVarName, PFUNCTION pFunc )
{
   PVAR pField;

   if( ! pFunc->szName )
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc = pFunc->pOwner;
   }

   pField = hb_compVariableFind( pFunc->pFields, wVar );

   if( pField->szAlias )
   {  /* the alias was specified in FIELD declaration
       * Push alias symbol before the field symbol
       */
      if( bPCode == HB_P_POPFIELD )
         bPCode = HB_P_POPALIASEDFIELD;
      else if( bPCode == HB_P_PUSHFIELD )
         bPCode = HB_P_PUSHALIASEDFIELD;

      hb_compGenPushSymbol( hb_strdup( pField->szAlias ), 0 );
   }
   hb_compGenVarPCode( bPCode, szVarName );
}

/*
 * Function generates passed pcode for passed runtime variable
 * (field or memvar)
 */
void hb_compGenVarPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pSym;

   /* Check if this variable name is placed into the symbol table
    */
   pSym = hb_compSymbolFind( szVarName, &wVar );
   if( ! pSym )
      pSym = hb_compSymbolAdd( szVarName, &wVar );
   pSym->cScope |= VS_MEMVAR;

   if( bPCode == HB_P_PUSHALIASEDFIELD && wVar <= 255 )
      hb_compGenPCode2( HB_P_PUSHALIASEDFIELDNEAR, ( BYTE ) wVar, ( BOOL ) 1 );
   else if( bPCode == HB_P_POPALIASEDFIELD && wVar <= 255 )
      hb_compGenPCode2( HB_P_POPALIASEDFIELDNEAR, ( BYTE ) wVar, ( BOOL ) 1 );
   else
      hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ), ( BOOL ) 1 );
}

void hb_compGenMessage( char * szMsgName )       /* sends a message to an object */
{
   USHORT wSym;
   PCOMSYMBOL pSym = hb_compSymbolFind( szMsgName, &wSym );

   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym = hb_compSymbolAdd( szMsgName, &wSym );
   pSym->cScope |= HB_FS_MESSAGE;
   hb_compGenPCode3( HB_P_MESSAGE, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ), ( BOOL ) 1 );
}

void hb_compGenMessageData( char * szMsg ) /* generates an underscore-symbol name for a data assignment */
{
   char * szResult = ( char * ) hb_xgrab( strlen( szMsg ) + 2 );

   strcpy( szResult, "_" );
   strcat( szResult, szMsg );

   hb_compGenMessage( szResult );
}

/* Check variable in the following order:
 * LOCAL variable
 *    local STATIC variable
 *       local FIELD variable
 *          local MEMVAR variable
 * global STATIC variable
 *    global FIELD variable
 *       global MEMVAR variable
 * (if not found - it is an undeclared variable)
 */
void hb_compGenPopVar( char * szVarName ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   int iVar;

   iVar = hb_compLocalGetPos( szVarName );
   if( iVar )
   {
      /* local variable
       */
      if( iVar >= -128 && iVar <= 127 )
      {
         /* local variables used in a coddeblock will not be adjusted
          * if PARAMETERS statement will be used then it is safe to
          * use 2 bytes for LOCALNEAR
         */
         if( hb_comp_functions.pLast->szName )
            hb_compGenPCode3( HB_P_POPLOCALNEAR, ( BYTE ) iVar, 0, ( BOOL ) 1 );
         else
            hb_compGenPCode2( HB_P_POPLOCALNEAR, ( BYTE ) iVar, ( BOOL ) 1 );
      }
      else
         hb_compGenPCode3( HB_P_POPLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
   }
   else
   {
      PFUNCTION pFunc;

      /* Check if we are generating a pop code for static variable
       * initialization function - if YES then we have to switch to a function
       * where the static variable was declared
       */
      if( ( hb_comp_functions.pLast->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) == ( HB_FS_INIT | HB_FS_EXIT ) )
          pFunc = hb_comp_functions.pLast->pOwner;
      else
          pFunc = hb_comp_functions.pLast;
      iVar = hb_compStaticGetPos( szVarName, pFunc );
      if( iVar )
      {
         /* Static variable declared in current function
          */
         hb_compGenPCode3( HB_P_POPSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
         pFunc->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );
         if( iVar )
         {
            /* field declared in current function
             */
            hb_compGenFieldPCode( HB_P_POPFIELD, iVar, szVarName, hb_comp_functions.pLast );
         }
         else
         {
            iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );
            if( iVar )
            {
               /* Memvar variable declared in current functions
                */
               hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
            }
            else
            {
               if( ! hb_comp_bStartProc )
                  iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );
               if( iVar )
               {
                  /* Global static variable
                   */
                  hb_compGenPCode3( HB_P_POPSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
                  hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
               }
               else
               {
                  if( ! hb_comp_bStartProc )
                     iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );
                  if( iVar )
                  {
                     /* Global field declaration
                      */
                     hb_compGenFieldPCode( HB_P_POPFIELD, iVar, szVarName, hb_comp_functions.pFirst );
                  }
                  else
                  {
                     if( ! hb_comp_bStartProc )
                        iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );
                     if( iVar )
                     {
                        /* Global Memvar variable declaration
                         */
                        hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
                     }
                     else
                     {
                        /* undeclared variable
                         */
                        hb_compGenVariablePCode( HB_P_POPVARIABLE, szVarName );
                     }
                  }
               }
            }
         }
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
            hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compGenVarPCode( HB_P_POPMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compGenVarPCode( HB_P_POPFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0 );
                  hb_compGenVarPCode( HB_P_POPALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compGenVarPCode( HB_P_POPALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      hb_compGenVarPCode( HB_P_POPALIASEDVAR, szVarName );
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushVar( char * szVarName )
{
   int iVar;

   iVar = hb_compLocalGetPos( szVarName );
   if( iVar )
   {
      /* local variable
       */
      if( iVar >= -128 && iVar <= 127 )
      {
         /* local variables used in a coddeblock will not be adjusted
          * if PARAMETERS statement will be used then it is safe to
          * use 2 bytes for LOCALNEAR
         */
         if( hb_comp_functions.pLast->szName )
            hb_compGenPCode3( HB_P_PUSHLOCALNEAR, ( BYTE ) iVar, 0, ( BOOL ) 1 );
         else
            hb_compGenPCode2( HB_P_PUSHLOCALNEAR, ( BYTE ) iVar, ( BOOL ) 1 );
      }
      else
         hb_compGenPCode3( HB_P_PUSHLOCAL, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
   }
   else
   {
      iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pLast );
      if( iVar )
      {
         /* Static variable declared in current function
          */
         hb_compGenPCode3( HB_P_PUSHSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );
         if( iVar )
         {
            /* field declared in current function
             */
            hb_compGenFieldPCode( HB_P_PUSHFIELD, iVar, szVarName, hb_comp_functions.pLast );
         }
         else
         {
            iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );
            if( iVar )
            {
               /* Memvar variable declared in current functions
                */
               hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else
            {
               if( ! hb_comp_bStartProc )
                  iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );
               if( iVar )
               {
                  /* Global static variable
                   */
                  hb_compGenPCode3( HB_P_PUSHSTATIC, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
                  hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
               }
               else
               {
                  if( ! hb_comp_bStartProc )
                     iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );
                  if( iVar )
                  {
                     /* Global field declaration
                      */
                     hb_compGenFieldPCode( HB_P_PUSHFIELD, iVar, szVarName, hb_comp_functions.pFirst );
                  }
                  else
                  {
                     if( ! hb_comp_bStartProc )
                        iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );
                     if( iVar )
                     {
                        /* Global Memvar variable declaration
                         */
                        hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
                     }
                     else
                     {
                        /* undeclared variable
                         */
                        hb_compGenVariablePCode( HB_P_PUSHVARIABLE, szVarName );
                     }
                  }
               }
            }
         }
      }
   }
}

void hb_compGenPushVarRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   int iVar;

   iVar = hb_compLocalGetPos( szVarName );
   if( iVar )
   {
      /* local variable
       */
      hb_compGenPCode3( HB_P_PUSHLOCALREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
   }
   else
   {
      iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pLast );
      if( iVar )
      {
         /* Static variable declared in current function
          */
         hb_compGenPCode3( HB_P_PUSHSTATICREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
         hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pLast );
         if( iVar )
         {
            /* pushing fields by reference is not allowed */
            hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_REFER, szVarName, NULL );
         }
         else
         {
            iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );
            if( iVar )
            {
               /* Memvar variable declared in current functions
                */
               hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
            }
            else
            {
               if( ! hb_comp_bStartProc )
                  iVar = hb_compStaticGetPos( szVarName, hb_comp_functions.pFirst );
               if( iVar )
               {
                  /* Global static variable
                   */
                  hb_compGenPCode3( HB_P_PUSHSTATICREF, HB_LOBYTE( iVar ), HB_HIBYTE( iVar ), ( BOOL ) 1 );
                  hb_comp_functions.pLast->bFlags |= FUN_USES_STATICS;
               }
               else
               {
                  if( ! hb_comp_bStartProc )
                     iVar = hb_compFieldGetPos( szVarName, hb_comp_functions.pFirst );
                  if( iVar )
                  {
                     /* pushing fields by reference is not allowed */
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_REFER, szVarName, NULL );
                  }
                  else
                  {
                     if( ! hb_comp_bStartProc )
                        iVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );
                     if( iVar )
                     {
                        /* Global Memvar variable declaration
                         */
                        hb_compGenVarPCode( HB_P_PUSHMEMVARREF, szVarName );
                     }
                     else
                     {
                        /* undeclared variable - field cannot be passed by the
                         * reference - assume the memvar
                         */
                        hb_compGenVariablePCode( HB_P_PUSHMEMVARREF, szVarName );
                     }
                  }
               }
            }
         }
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
            hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compGenVarPCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compGenVarPCode( HB_P_PUSHFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0 );
                  hb_compGenVarPCode( HB_P_PUSHALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compGenVarPCode( HB_P_PUSHALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack
       * NOTE: An alias will be determined at runtime then we cannot decide
       * here if passed name is either a field or a memvar
       */
      hb_compGenVarPCode( HB_P_PUSHALIASEDVAR, szVarName );
}

void hb_compGenPushLogical( int iTrueFalse ) /* pushes a logical value on the virtual machine stack */
{
   hb_compGenPCode1( iTrueFalse ? HB_P_TRUE : HB_P_FALSE );
}

void hb_compGenPushNil( void )
{
   hb_compGenPCode1( HB_P_PUSHNIL );
}

/* generates the pcode to push a double number on the virtual machine stack */
void hb_compGenPushDouble( double dNumber, BYTE bWidth, BYTE bDec )
{
   BYTE pBuffer[ sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1 ];

   pBuffer[ 0 ] = HB_P_PUSHDOUBLE;

   memcpy( ( BYTE * ) &( pBuffer[ 1 ] ), ( BYTE * ) &dNumber, sizeof( double ) );

   pBuffer[ 1 + sizeof( double ) ] = bWidth;
   pBuffer[ 1 + sizeof( double ) + sizeof( BYTE ) ] = bDec;

   hb_compGenPCodeN( pBuffer, 1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ), 1 );
}

void hb_compGenPushFunCall( char * szFunName )
{
   char * szFunction;

   szFunction = hb_compReservedName( szFunName );
   if( szFunction )
   {
      /* Abbreviated function name was used - change it for whole name
       */
      hb_compGenPushSymbol( hb_compIdentifierNew( szFunction, TRUE ), 1 );
   }
   else
      hb_compGenPushSymbol( szFunName, 1 );
}

/* generates the pcode to push a long number on the virtual machine stack */
void hb_compGenPushLong( long lNumber )
{
   if( lNumber == 0 )
      hb_compGenPCode1( HB_P_ZERO );
   else if( lNumber == 1 )
      hb_compGenPCode1( HB_P_ONE );
   else if( ( ( char * ) &lNumber )[ 2 ] == 0 && ( ( char * ) &lNumber )[ 3 ] == 0 )
   {
      if( ( ( char * ) &lNumber )[ 1 ] == 0 )
         hb_compGenPCode2( HB_P_PUSHBYTE, ( ( char * ) &lNumber )[ 0 ], ( BOOL ) 1 );
      else
         hb_compGenPCode3( HB_P_PUSHINT, ( ( char * ) &lNumber )[ 0 ], ( ( char * ) &lNumber )[ 1 ], ( BOOL ) 1 );
   }
   else
   {
      BYTE pBuffer[5];

      pBuffer[0] = HB_P_PUSHLONG;
      pBuffer[1] = ( ( BYTE * ) &lNumber )[0];
      pBuffer[2] = ( ( BYTE * ) &lNumber )[1];
      pBuffer[3] = ( ( BYTE * ) &lNumber )[2];
      pBuffer[4] = ( ( BYTE * ) &lNumber )[3];

      hb_compGenPCodeN( pBuffer, 5, 1 );
   }
}

/* generates the pcode to push a string on the virtual machine stack */
void hb_compGenPushString( char * szText, ULONG ulStrLen )
{
   BYTE * pBuffer;

   if( ulStrLen > 255 )
   {
      pBuffer = ( BYTE * ) hb_xgrab( ulStrLen + 3 );

      pBuffer[0] = HB_P_PUSHSTR;
      pBuffer[1] = HB_LOBYTE( ulStrLen );
      pBuffer[2] = HB_HIBYTE( ulStrLen );

      memcpy( ( BYTE *)( &( pBuffer[3] ) ), ( BYTE * ) szText, ulStrLen );

      hb_compGenPCodeN( pBuffer, ulStrLen + 3, 1 );
   }
   else
   {
      pBuffer = ( BYTE * ) hb_xgrab( ulStrLen + 3 );

      pBuffer[0] = HB_P_PUSHSTRSHORT;
      pBuffer[1] = ( BYTE ) ulStrLen;

      memcpy( ( BYTE *)( &( pBuffer[2] ) ), ( BYTE * ) szText, ulStrLen );

      hb_compGenPCodeN( pBuffer, ulStrLen + 2, 1 );
   }

   hb_xfree( pBuffer );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void hb_compGenPushSymbol( char * szSymbolName, int iIsFunction )
{
   USHORT wSym;

   if( ! hb_compSymbolFind( szSymbolName, &wSym ) )  /* the symbol was not found on the symbol table */
   {
      hb_compSymbolAdd( szSymbolName, &wSym );
      if( iIsFunction )
         hb_compFunCallAdd( szSymbolName );
   }
   else
   {
      if( iIsFunction && ! hb_compFunCallFind( szSymbolName ) )
         hb_compFunCallAdd( szSymbolName );
   }

   if( wSym > 255 )
      hb_compGenPCode3( HB_P_PUSHSYM, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ), ( BOOL ) 1 );
   else
      hb_compGenPCode2( HB_P_PUSHSYMNEAR, ( BYTE ) wSym, ( BOOL ) 1 );
}


static void hb_compCheckDuplVars( PVAR pVar, char * szVarName )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         hb_compErrorDuplVar( szVarName );
         break;
      }
      else
         pVar = pVar->pNext;
   }
}

void hb_compFinalizeFunction( void ) /* fixes all last defined function returns jumps offsets */
{
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( pFunc )
   {
      if( (pFunc->bFlags & FUN_WITH_RETURN) == 0 )
      {
         /* The last statement in a function/procedure was not a RETURN
         * Generate end-of-procedure pcode
         */
         hb_compGenPCode1( HB_P_ENDPROC );
      }

      if( pFunc->bFlags & FUN_USES_LOCAL_PARAMS )
      {
         int PCount = pFunc->wParamCount;

         /* do not adjust if local parameters are used -remove NOOPs only */
         pFunc->wParamCount = 0;
         /* There was a PARAMETERS statement used.
         * NOTE: This fixes local variables references in a case when
         * there is PARAMETERS statement after a LOCAL variable declarations.
         * All local variables are numbered from 1 - which means use first
         * item from the eval stack. However if PARAMETERS statement is used
         * then there are additional items on the eval stack - the
         * function arguments. Then first local variable is at the position
         * (1 + <number of arguments>). We cannot fix this numbering
         * because the PARAMETERS statement can be used even at the end
         * of function body when all local variables are already created.
         */

         hb_compFixFuncPCode( pFunc );
         pFunc->wParamCount = PCount;
      }
      else
         hb_compFixFuncPCode( pFunc );

      if( pFunc->iNOOPs )
         hb_compOptimizeJumps();

      if( hb_comp_iWarnings )
      {
         PVAR pVar;

         pVar = pFunc->pLocals;
         while( pVar )
         {
            if( pVar->szName && pFunc->szName && pFunc->szName[0] && (! ( pVar->iUsed & VU_USED )) )
            {
               char szFun[ 256 ];
               sprintf( szFun, "%s(%i)", pFunc->szName, pVar->iDeclLine );
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
            }

            pVar = pVar->pNext;
         }

         pVar = pFunc->pStatics;
         while( pVar )
         {
            if( pVar->szName && pFunc->szName && pFunc->szName[0] && ! ( pVar->iUsed & VU_USED ) )
            {
               char szFun[ 256 ];
               sprintf( szFun, "%s(%i)", pFunc->szName, pVar->iDeclLine );
               hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAR_NOT_USED, pVar->szName, szFun );
            }

            pVar = pVar->pNext;
         }

         /* Check if the function returned some value
         */
         if( (pFunc->bFlags & FUN_WITH_RETURN) == 0 &&
            (pFunc->bFlags & FUN_PROCEDURE) == 0 )
            hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_FUN_WITH_NO_RETURN,
                        pFunc->szName, NULL );

         /* Compile Time Strong Type Checking is not needed any more. */
         if ( pFunc->pStack )
            hb_xfree( ( void * ) pFunc->pStack );

         pFunc->iStackSize = 0;
         pFunc->iStackIndex = 0;
         pFunc->iStackFunctions = 0;
         pFunc->iStackClasses = 0;
      }
   }
}

static void hb_compOptimizeFrames( PFUNCTION pFunc )
{
   USHORT w;

   if( pFunc == NULL )
      return;

   if( pFunc == hb_comp_pInitFunc )
   {
      if( pFunc->pCode[ 0 ] == HB_P_STATICS &&
          pFunc->pCode[ 5 ] == HB_P_SFRAME )
      {
         hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
         pFunc->pCode[ 1 ] = HB_LOBYTE( w );
         pFunc->pCode[ 2 ] = HB_HIBYTE( w );
         pFunc->pCode[ 6 ] = HB_LOBYTE( w );
         pFunc->pCode[ 7 ] = HB_HIBYTE( w );

         /* Remove the SFRAME pcode if there's no global static
            initialization: */

         /* NOTE: For some reason this will not work for the static init
                  function, so I'm using an ugly hack instead. [vszakats] */
/*       if( !( pFunc->bFlags & FUN_USES_STATICS ) ) */
         if( pFunc->pCode[ 8 ] == HB_P_ENDPROC )
         {
            pFunc->lPCodePos -= 3;
            memmove( pFunc->pCode + 5, pFunc->pCode + 8, pFunc->lPCodePos - 5 );
         }
         else
         /* Check Global Statics. */
         {
            /* PVAR pVar = pFunc->pStatics; */
            PVAR pVar = hb_comp_functions.pFirst->pStatics;

            while( pVar )
            {
               /*printf( "\nChecking: %s Used: %i\n", pVar->szName, pVar->iUsed );*/

               if ( ! ( pVar->iUsed & VU_USED ) && (pVar->iUsed & VU_INITIALIZED) )
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_VAL_NOT_USED, pVar->szName, NULL );

               /* May have been initialized in previous execution of the function.
               else if ( ( pVar->iUsed & VU_USED ) && ! ( pVar->iUsed & VU_INITIALIZED ) )
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NOT_INITIALIZED, pVar->szName, NULL );
               */
               pVar = pVar->pNext;
            }
         }
      }
   }
   else if( pFunc->pCode[ 0 ] == HB_P_FRAME &&
            pFunc->pCode[ 3 ] == HB_P_SFRAME )
   {
      PVAR pLocal;
      int bLocals = 0;
      BOOL bSkipFRAME;
      BOOL bSkipSFRAME;

      pLocal = pFunc->pLocals;
      while( pLocal )
      {
         pLocal = pLocal->pNext;
         bLocals++;
      }

      if( bLocals || pFunc->wParamCount )
      {
         if( pFunc->bFlags & FUN_USES_LOCAL_PARAMS )
         {
            pFunc->pCode[ 1 ] = ( BYTE )( bLocals ) - ( BYTE )( pFunc->wParamCount );
         }
         else
         {
            /* Parameters declared with PARAMETERS statement are not
             * placed in the local variable list.
             */
            pFunc->pCode[ 1 ] = ( BYTE )( bLocals );
         }
         pFunc->pCode[ 2 ] = ( BYTE )( pFunc->wParamCount );
         bSkipFRAME = FALSE;
      }
      else
         bSkipFRAME = TRUE;

      if( pFunc->bFlags & FUN_USES_STATICS )
      {
         hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
         pFunc->pCode[ 4 ] = HB_LOBYTE( w );
         pFunc->pCode[ 5 ] = HB_HIBYTE( w );
         bSkipSFRAME = FALSE;
      }
      else
         bSkipSFRAME = TRUE;

      /* Remove the frame pcodes if they are not needed */

      if( bSkipFRAME && bSkipSFRAME )
      {
         pFunc->lPCodePos -= 6;
         memmove( pFunc->pCode, pFunc->pCode + 6, pFunc->lPCodePos );
      }
      else if( bSkipFRAME )
      {
         pFunc->lPCodePos -= 3;
         memmove( pFunc->pCode, pFunc->pCode + 3, pFunc->lPCodePos );
      }
      else if( bSkipSFRAME )
      {
         pFunc->lPCodePos -= 3;
         memmove( pFunc->pCode + 3, pFunc->pCode + 6, pFunc->lPCodePos - 3 );
      }
   }
}

int
#ifdef __IBMCPP__
extern _LNK_CONV
#endif
hb_compSort_ULONG( const void * pLeft, const void * pRight )
{
    ULONG ulLeft  = *( ( ULONG * ) ( pLeft ) );
    ULONG ulRight = *( ( ULONG * ) ( pRight ) );

    if( ulLeft == ulRight )
       return 0 ;
    else if( ulLeft < ulRight )
       return -1;
    else
       return 1;
}

static void hb_compOptimizeJumps( void )
{
   /* Jump Optimizer */
   BYTE * pCode      = hb_comp_functions.pLast->pCode;
   ULONG * pNOOPs    = hb_comp_functions.pLast->pNOOPs;
   ULONG * pJumps    = hb_comp_functions.pLast->pJumps;
   ULONG ulOptimized = 0;
   ULONG ulNextByte  = 0;
   int * piShifts    = NULL;
   ULONG iNOOP;
   ULONG ulOffset;
   ULONG ulBytes2Copy;
   ULONG iJump;
   BOOL bForward = FALSE;

   /* Needed so the pasting of PCODE pieces below will work correctly  */
   qsort( ( void * ) pNOOPs, hb_comp_functions.pLast->iNOOPs, sizeof( ULONG ), hb_compSort_ULONG );

   if ( hb_comp_functions.pLast->iJumps )
      piShifts = ( int * ) hb_xgrab( sizeof( int ) * hb_comp_functions.pLast->iJumps );

   for( iJump = 0; iJump < hb_comp_functions.pLast->iJumps; iJump++ )
      piShifts[ iJump ] = 0;

   /* First Scan NOOPS - Adjust Jump addresses. */
   for( iNOOP = 0; iNOOP < hb_comp_functions.pLast->iNOOPs; iNOOP++ )
   {
      /* Adjusting preceding jumps that pooint to code beyond the current NOOP or trailing backward jumps pointing to lower address. */
      for( iJump = 0; iJump < hb_comp_functions.pLast->iJumps ; iJump++ )
      {
         switch( pCode[ pJumps[ iJump ] ] )
         {
            case HB_P_JUMPNEAR :
            case HB_P_JUMPFALSENEAR :
            case HB_P_JUMPTRUENEAR :
            {
               ulOffset = pCode[ pJumps[ iJump ] + 1 ];

               if( ulOffset > 127 )
               {
                  ulOffset -= 256;
                  bForward = FALSE;
               }
               else
                  bForward = TRUE;
            }
            break;

            case HB_P_JUMP :
            case HB_P_JUMPFALSE :
            case HB_P_JUMPTRUE :
            {
               ulOffset = ( ULONG ) ( pCode[ pJumps[ iJump ] + 1 ] + ( pCode[ pJumps[ iJump ] + 2 ] * 256 ) );
               if( ulOffset > SHRT_MAX )
               {
                  ulOffset -= 65536;
                  bForward = FALSE;
               }
               else
                  bForward = TRUE;
            }
            break;

            default:
            {
               ulOffset = ( ULONG )( pCode[ pJumps[ iJump ] + 1 ] + ( pCode[ pJumps[ iJump ] + 2 ] * 256 ) + ( pCode[ pJumps[ iJump ] + 3 ] * 65536 ) );
               if( ulOffset > 8388607L )
               {
                  ulOffset -= 16777216L;
                  bForward = FALSE;
               }
               else
                  bForward = TRUE;
            }
            break;
         }

         /* Only interested in forward (positive) jumps. */
         if( bForward )
         {
            /* Only if points to code beyond the current fix. */
            if( pJumps[ iJump ] < pNOOPs[ iNOOP ] && pJumps[ iJump ] + piShifts[ iJump ] + ulOffset > pNOOPs[ iNOOP ] )
            {
               /* Increasing Shift Counter for this Jump. */
               piShifts[ iJump ]++;

               if( pCode[ pJumps[ iJump ] + 1 ] )
                  pCode[ pJumps[ iJump ] + 1 ]--;
               else
               {
                  pCode[ pJumps[ iJump ] + 1 ] = 255;
                  pCode[ pJumps[ iJump ] + 2 ]--;
               }
            }
         }
         else
         {
            /* Adjusting all later jumps (if negative) and target prior the current NOOP. */

            /* Only if points to code beyond the current fix. */
            if(  pJumps[ iJump ] > pNOOPs[ iNOOP ] && pJumps[ iJump ] + piShifts[ iJump ] + ulOffset < pNOOPs[ iNOOP ] )
            {
               /* Decreasing Shift Counter for this Jump. */
               piShifts[ iJump ]--;

               if( pCode[ pJumps[ iJump ] + 1 ] < 255 )
                  pCode[ pJumps[ iJump ] + 1 ]++;
               else
               {
                  pCode[ pJumps[ iJump ] + 1 ] = 0;
                  pCode[ pJumps[ iJump ] + 2 ]++;
               }
            }
         }
      }
   }

   /* Second Scan, after all adjustements been made, we can copy the optimized code. */
   for( iNOOP = 0; iNOOP < hb_comp_functions.pLast->iNOOPs; iNOOP++ )
   {
      ulBytes2Copy = ( pNOOPs[ iNOOP ] - ulNextByte ) ;

      memmove( pCode + ulOptimized, pCode + ulNextByte, ulBytes2Copy );

      ulOptimized += ulBytes2Copy;
      ulNextByte  += ulBytes2Copy;

      /* Skip the NOOP and point to next valid byte */
      ulNextByte++;
   }

   ulBytes2Copy = ( hb_comp_functions.pLast->lPCodePos - ulNextByte ) ;
   memmove( pCode + ulOptimized, pCode + ulNextByte, ulBytes2Copy );
   ulOptimized += ulBytes2Copy;

   hb_comp_functions.pLast->lPCodePos  = ulOptimized;
   hb_comp_functions.pLast->lPCodeSize = ulOptimized;
}

/* Generate the opcode to open BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * - either the address of HB_P_SEQEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
ULONG hb_compSequenceBegin( void )
{
   hb_compGenPCode4( HB_P_SEQBEGIN, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Generate the opcode to close BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * the address of first line after END SEQUENCE
 * This opcode will be executed if recover code was not requested (as the
 * last statement in code beetwen BEGIN ... RECOVER) or if BREAK was requested
 * and there was no matching RECOVER clause.
 */
ULONG hb_compSequenceEnd( void )
{
   hb_compGenPCode4( HB_P_SEQEND, 0, 0, 0, ( BOOL ) 0 );

   hb_compPrepareOptimize();

   return hb_comp_functions.pLast->lPCodePos - 3;
}

/* Remove unnecessary opcodes in case there were no executable statements
 * beetwen BEGIN and RECOVER sequence
 */
void hb_compSequenceFinish( ULONG ulStartPos, int bUsualStmts )
{
   if( ! hb_comp_bDebugInfo ) /* only if no debugger info is required */
   {
      if( ! bUsualStmts )
      {
         hb_comp_functions.pLast->lPCodePos = ulStartPos - 1; /* remove also HB_P_SEQBEGIN */
         hb_comp_ulLastLinePos = ulStartPos - 5;
      }
   }
}


/* Set the name of an alias for the list of previously declared FIELDs
 *
 * szAlias -> name of the alias
 * iField  -> position of the first FIELD name to change
 */
void hb_compFieldSetAlias( char * szAlias, int iField )
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
 * We will required this information in hb_compFieldSetAlias function
 */
int hb_compFieldsCount()
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

      hb_comp_pInitFunc = hb_compFunctionNew( hb_strdup("(_INITSTATICS)"), HB_FS_INIT );
      hb_comp_pInitFunc->pOwner = hb_comp_functions.pLast;
      hb_comp_pInitFunc->bFlags = FUN_USES_STATICS | FUN_PROCEDURE;
      hb_comp_pInitFunc->cScope = HB_FS_INIT | HB_FS_EXIT;
      hb_comp_functions.pLast = hb_comp_pInitFunc;

      pBuffer[ 0 ] = HB_P_STATICS;
      pBuffer[ 1 ] = 0;
      pBuffer[ 2 ] = 0;
      pBuffer[ 3 ] = 1; /* the number of static variables is unknown now */
      pBuffer[ 4 ] = 0;

      hb_compGenPCodeN( pBuffer, 5, 0 );

      hb_compGenPCode3( HB_P_SFRAME, 0, 0, ( BOOL ) 0 );     /* frame for statics variables */
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
   PFUNCTION pBlock;

   #if 0
      if( hb_comp_iWarnings >= 3 )
      {
	     /* Not generating yet - will be generated in hb_compCodeBlockEnd() */
         hb_compGenPCode1( HB_P_PUSHBLOCK );
         pBlock->lPCodePos--;
      }
   #endif

   pBlock               = hb_compFunctionNew( NULL, HB_FS_STATIC );
   pBlock->pOwner       = hb_comp_functions.pLast;
   pBlock->iStaticsBase = hb_comp_functions.pLast->iStaticsBase;

   hb_comp_functions.pLast = pBlock;
   hb_compLinePushIfDebugger();
}

void hb_compCodeBlockEnd( void )
{
   PFUNCTION pCodeblock;   /* pointer to the current codeblock */
   PFUNCTION pFunc;        /* pointer to a function that owns a codeblock */
   USHORT wSize;
   USHORT wLocals = 0;   /* number of referenced local variables */
   USHORT wPos;
   PVAR pVar, pFree;

   if( hb_comp_functions.pLast &&
       hb_comp_functions.pLast->iNOOPs )
      hb_compOptimizeJumps();

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
   /* QUESTION: would be 64kB enough for a codeblock size?
    * we are assuming now a USHORT for a size of codeblock
    */

   /* Count the number of referenced local variables */
   pVar = pCodeblock->pStatics;
   while( pVar )
   {
      pVar = pVar->pNext;
      ++wLocals;
   }

   if( ( pCodeblock->lPCodePos + 3 ) <= 255 &&  pCodeblock->wParamCount == 0 && wLocals == 0 )
   {
      /* NOTE: 3 = HB_P_PUSHBLOCKSHORT + BYTE( size ) + _ENDBLOCK */
      wSize = ( USHORT ) pCodeblock->lPCodePos + 3;

      hb_compGenPCode2( HB_P_PUSHBLOCKSHORT, ( BYTE ) wSize, ( BOOL ) 0 );
   }
   else
   {
      /* NOTE: 8 = HB_P_PUSHBLOCK + USHORT( size ) + USHORT( wParams ) + USHORT( wLocals ) + _ENDBLOCK */
      wSize = ( USHORT ) pCodeblock->lPCodePos + 8 + wLocals * 2;

      hb_compGenPCode3( HB_P_PUSHBLOCK, HB_LOBYTE( wSize ), HB_HIBYTE( wSize ), ( BOOL ) 0 );
      hb_compGenPCode2( HB_LOBYTE( pCodeblock->wParamCount ), HB_HIBYTE( pCodeblock->wParamCount ), ( BOOL ) 0 );
      hb_compGenPCode2( HB_LOBYTE( wLocals ), HB_HIBYTE( wLocals ), ( BOOL ) 0 );
   }

   /* generate the table of referenced local variables */
   pVar = pCodeblock->pStatics;
   while( wLocals-- )
   {
      wPos = hb_compVariableGetPos( pFunc->pLocals, pVar->szName );
      hb_compGenPCode2( HB_LOBYTE( wPos ), HB_HIBYTE( wPos ), ( BOOL ) 0 );

      pFree = pVar;

      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }

   hb_compGenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos, ( BOOL ) 0 );
   hb_compGenPCode1( HB_P_ENDBLOCK ); /* finish the codeblock */

   /* this fake-function is no longer needed */
   hb_xfree( ( void * ) pCodeblock->pCode );
   pVar = pCodeblock->pLocals;
   while( pVar )
   {
      if( hb_comp_iWarnings && pFunc->szName && pVar->szName && ! ( pVar->iUsed & VU_USED ) )
         hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );

      /* free used variables */
      pFree = pVar;

      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }

   /* Release the NOOP array. */
   if( pCodeblock->pNOOPs )
      hb_xfree( ( void * ) pCodeblock->pNOOPs );

   /* Release the Jumps array. */
   if( pCodeblock->pJumps )
      hb_xfree( ( void * ) pCodeblock->pJumps );

   /* Compile Time Strong Type Checking Stack is not needed any more. */
   if ( pCodeblock->pStack )
      hb_xfree( ( void * ) pCodeblock->pStack );

   pCodeblock->iStackSize = 0;
   pCodeblock->iStackIndex = 0;
   pCodeblock->iStackFunctions = 0;
   pCodeblock->iStackClasses = 0;

   hb_xfree( ( void * ) pCodeblock );
}

/* ************************************************************************* */

/* initialize support variables */
static void hb_compInitVars( void )
{
   hb_comp_files.iFiles     = 0;
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
   hb_comp_szAnnounce       = NULL;
   hb_comp_pInitFunc = NULL;
   hb_comp_bAnyWarning = FALSE;

   hb_comp_iLine = 1;
   hb_comp_iFunctionCnt = 0;
   hb_comp_iErrorCount = 0;
   hb_comp_cVarType = ' ';
   hb_comp_ulLastLinePos = 0;
   hb_comp_iStaticCnt = 0;
   hb_comp_iVarScope = VS_LOCAL;

   hb_comp_inlines.iCount   = 0;
   hb_comp_inlines.pFirst   = NULL;
   hb_comp_inlines.pLast    = NULL;
}

static void hb_compGenOutput( int iLanguage )
{

   switch( iLanguage )
   {
      case LANG_C:
         hb_compGenCCode( hb_comp_pFileName );
         break;

      case LANG_OBJ32:
         hb_compGenObj32( hb_comp_pFileName );
         break;

      case LANG_JAVA:
         hb_compGenJava( hb_comp_pFileName );
         break;

      case LANG_PORT_OBJ:
         hb_compGenPortObj( hb_comp_pFileName );
         break;

      case LANG_OBJ_MODULE:
         hb_compGenCObj( hb_comp_pFileName );
         break;
   }
}

static void hb_compOutputFile( void )
{
   hb_comp_pFileName->szPath = NULL;
   hb_comp_pFileName->szExtension = NULL;

   /* we create the output file name */
   if( hb_comp_pOutPath )
   {
      if( hb_comp_pOutPath->szPath )
         hb_comp_pFileName->szPath = hb_comp_pOutPath->szPath;

      if( hb_comp_pOutPath->szName )
      {
         hb_comp_pFileName->szName = hb_comp_pOutPath->szName;
         if( hb_comp_pOutPath->szExtension )
            hb_comp_pFileName->szExtension = hb_comp_pOutPath->szExtension;
      }
   }
}

int hb_compCompile( char * szPrg, int argc, char * argv[] )
{
   int iStatus = EXIT_SUCCESS;
   PHB_FNAME pFileName;

   hb_comp_pFileName = hb_fsFNameSplit( szPrg );

   if( hb_comp_pFileName->szName )
   {
      char szFileName[ _POSIX_PATH_MAX ];    /* filename to parse */
      char szPpoName[ _POSIX_PATH_MAX ];

      if( !hb_comp_pFileName->szExtension )
         hb_comp_pFileName->szExtension = ".prg";

      hb_fsFNameMerge( szFileName, hb_comp_pFileName );

      if( hb_comp_bPPO )
      {
         hb_comp_pFileName->szExtension = ".ppo";
         hb_fsFNameMerge( szPpoName, hb_comp_pFileName );
         hb_comp_yyppo = fopen( szPpoName, "w" );
         if( ! hb_comp_yyppo )
         {
            hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_CREATE_PPO, szPpoName, NULL );
            iStatus = EXIT_FAILURE;
         }
      }

      if( iStatus == EXIT_SUCCESS )
      {
         /* Add /D command line or envvar defines */
         hb_compChkDefines( argc, argv );

         /* Initialize support variables */
         hb_compInitVars();

         if( hb_compInclude( szFileName, NULL ) )
         {
            BOOL bSkipGen = FALSE ;

            hb_comp_szFile = szFileName;

            if( ! hb_comp_bQuiet )
            {
               if( hb_comp_bPPO )
                  printf( "Compiling '%s' and generating preprocessed output to '%s'...\n", szFileName, szPpoName );
               else
                  printf( "Compiling '%s'...\n", szFileName );
            }

            /* Generate the starting procedure frame */
            if( hb_comp_bStartProc )
                hb_compFunctionAdd( hb_strupr( hb_strdup( hb_comp_pFileName->szName ) ), HB_FS_PUBLIC, FUN_PROCEDURE );
            else
            {
                /* Don't pass the name of module if the code for starting procedure
                * will be not generated. The name cannot be placed as first symbol
                * because this symbol can be used as function call or memvar's name.
                */
                hb_compFunctionAdd( hb_compIdentifierNew( "", TRUE ), HB_FS_PUBLIC, FUN_PROCEDURE );
            }

            yyparse();

            /* Close processed file (it is opened in hb_compInclude() function ) */
            fclose( yyin );
            hb_comp_files.pLast  = NULL;

            if( hb_comp_bPPO && hb_comp_yyppo )
            {
               fclose( hb_comp_yyppo );
               hb_comp_yyppo = NULL;
            }

            /* Saving main file. */
            pFileName = hb_comp_pFileName;

            /* Open refernced modules. */
            while( hb_comp_pAutoOpen )
            {
               PAUTOOPEN pAutoOpen = hb_comp_pAutoOpen;

               hb_comp_pAutoOpen = hb_comp_pAutoOpen->pNext;

               if( ! hb_compFunctionFind( pAutoOpen->szName ) )
                  hb_compAutoOpen( pAutoOpen->szName, &bSkipGen );

               hb_xfree( pAutoOpen->szName );
               hb_xfree( pAutoOpen );
            }

            /* Restoring main file. */
            hb_comp_pFileName = pFileName;

         /* Begin of finalization phase. */

            /* fix all previous function returns offsets */
            hb_compFinalizeFunction();

            hb_compExternGen();       /* generates EXTERN symbols names */

            if( hb_comp_pInitFunc )
            {
                PCOMSYMBOL pSym;

                /* Fix the number of static variables */
                hb_comp_pInitFunc->pCode[ 3 ] = HB_LOBYTE( hb_comp_iStaticCnt );
                hb_comp_pInitFunc->pCode[ 4 ] = HB_HIBYTE( hb_comp_iStaticCnt );
                hb_comp_pInitFunc->iStaticsBase = hb_comp_iStaticCnt;

                pSym = hb_compSymbolAdd( hb_comp_pInitFunc->szName, NULL );
                pSym->cScope |= hb_comp_pInitFunc->cScope;
                hb_comp_functions.pLast->pNext = hb_comp_pInitFunc;
                hb_comp_functions.pLast = hb_comp_pInitFunc;
                hb_compGenPCode1( HB_P_ENDPROC );
                ++hb_comp_functions.iCount;
            }

            if( hb_comp_szAnnounce )
                hb_compAnnounce( hb_comp_szAnnounce );

         /* End of finalization phase. */

            if( hb_comp_iErrorCount || hb_comp_bAnyWarning )
            {
               if( hb_comp_iErrorCount )
               {
                  iStatus = EXIT_FAILURE;
                  bSkipGen = TRUE;
                  printf( "\r%i error%s\n\nNo code generated\n", hb_comp_iErrorCount, ( hb_comp_iErrorCount > 1 ? "s" : "" ) );
               }
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_SETEXIT )
               {
                  iStatus = EXIT_FAILURE;
               }
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
               {
                  iStatus = EXIT_FAILURE;
                  bSkipGen = TRUE;
                  printf( "\nNo code generated.\n" );
               }
            }

            if( ! hb_comp_bSyntaxCheckOnly && ! bSkipGen && ( hb_comp_iErrorCount == 0 ) )
            {
               PFUNCTION pFunc;
               char * szFirstFunction = NULL;

               /* we create the output file name */
               hb_compOutputFile();

               if( ! hb_comp_bStartProc )
                  --hb_comp_iFunctionCnt;

               pFunc = hb_comp_functions.pFirst;
               while( pFunc )
               {
                  hb_compOptimizeFrames( pFunc );

                  if( szFirstFunction == NULL && pFunc->szName[0] && ! ( pFunc->cScope & HB_FS_INIT || pFunc->cScope & HB_FS_EXIT ) )
                  {
                     szFirstFunction = pFunc->szName;
                  }

                  pFunc = pFunc->pNext;
               }

               if( szFirstFunction )
               {
                  PCOMSYMBOL pSym = hb_comp_symbols.pFirst;

                  while( pSym )
                  {
                     if( strcmp( pSym->szName, szFirstFunction ) == 0 )
                     {
                        pSym->cScope |= HB_FS_FIRST;
                        break;
                     }
                     pSym = pSym->pNext;
                  }
               }

               if( ! hb_comp_bQuiet )
                  printf( "\rLines %i, Functions/Procedures %i\n", hb_comp_iLine, hb_comp_iFunctionCnt );

               hb_compGenOutput( hb_comp_iLanguage );
            }
         }
         else
         {
            printf( "Cannot open input file: %s\n", szFileName );

            /* printf( "No code generated\n" ); */
            iStatus = EXIT_FAILURE;
         }

         {
            PFILE pFile = hb_comp_files.pLast;

            while( pFile )
            {
               fclose( pFile->handle );
               pFile = ( PFILE ) pFile->pPrev;
            }
         }

/*
         while( hb_comp_pExterns )
         {
            PEXTERN pExtern = hb_comp_pExterns;

            hb_comp_pExterns = hb_comp_pExterns->pNext;

            hb_xfree( pExtern->szName );
            hb_xfree( pExtern );
         }
*/
         hb_comp_bExternal = FALSE;
      }
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADFILENAME, szPrg, NULL );
      iStatus = EXIT_FAILURE;
   }
   hb_xfree( hb_comp_pFileName );

   return iStatus;
}

void hb_compAutoOpenAdd( char * szName )
{
   if( hb_comp_bAutoOpen && ! hb_compAutoOpenFind( szName ) )
   {
      PAUTOOPEN pAutoOpen = ( PAUTOOPEN ) hb_xgrab( sizeof( AUTOOPEN ) ), pLast;

      pAutoOpen->szName = hb_strdup( szName );
      pAutoOpen->pNext  = NULL;

      if( hb_comp_pAutoOpen == NULL )
          hb_comp_pAutoOpen = pAutoOpen;
      else
      {
          pLast = hb_comp_pAutoOpen;
          while( pLast->pNext )
              pLast = pLast->pNext;

          pLast->pNext = pAutoOpen;
      }
   }
}

BOOL hb_compAutoOpenFind( char * szName )
{
   PAUTOOPEN pLast = hb_comp_pAutoOpen;

   if( pLast == NULL )
      return FALSE;

   if( strcmp( pLast->szName, szName ) == 0 )
      return TRUE;
   else
   {
      while( pLast->pNext )
      {
         pLast = pLast->pNext;

         if( strcmp( pLast->szName, szName ) == 0 )
            return TRUE;
      }
   }
   return FALSE;
}

int hb_compAutoOpen( char * szPrg, BOOL * pbSkipGen )
{
   int iStatus = EXIT_SUCCESS;

   hb_comp_pFileName = hb_fsFNameSplit( szPrg );

   if( hb_comp_pFileName->szName )
   {
      char szFileName[ _POSIX_PATH_MAX ];    /* filename to parse */
      char szPpoName[ _POSIX_PATH_MAX ];

      if( !hb_comp_pFileName->szExtension )
         hb_comp_pFileName->szExtension = ".prg";

      hb_fsFNameMerge( szFileName, hb_comp_pFileName );

      if( hb_comp_bPPO )
      {
         hb_comp_pFileName->szExtension = ".ppo";
         hb_fsFNameMerge( szPpoName, hb_comp_pFileName );
         hb_comp_yyppo = fopen( szPpoName, "w" );
         if( ! hb_comp_yyppo )
         {
            hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_CREATE_PPO, szPpoName, NULL );
            iStatus = EXIT_FAILURE;
         }
      }

      if( iStatus == EXIT_SUCCESS )
      {
         /* Minimal Init. */
         hb_comp_files.iFiles = 0;
         hb_comp_iLine        = 1;

         if( hb_compInclude( szFileName, NULL ) )
         {
            if( ! hb_comp_bQuiet )
            {
               if( hb_comp_bPPO )
                  printf( "Compiling module '%s' and generating preprocessed output to '%s'...\n", szFileName, szPpoName );
               else
                  printf( "Compiling module '%s'...\n", szFileName );
            }

            hb_pp_Init();

            /*
            yyrestart( yyin );
            */

            /* Generate the starting procedure frame */
            if( hb_comp_bStartProc )
               hb_compFunctionAdd( hb_strupr( hb_strdup( hb_comp_pFileName->szName ) ), HB_FS_PUBLIC, FUN_PROCEDURE );

            {
               int i = hb_comp_iExitLevel ;
               BOOL b = hb_comp_bAnyWarning;

               yyparse();

               hb_comp_iExitLevel = ( i > hb_comp_iExitLevel ? i : hb_comp_iExitLevel );
               hb_comp_bAnyWarning = ( b ? b : hb_comp_bAnyWarning );
            }

            /* Close processed file (it is opened in hb_compInclude() function ) */
            fclose( yyin );
            hb_comp_files.pLast = NULL;

            if( hb_comp_bAnyWarning )
            {
               if( hb_comp_iExitLevel == HB_EXITLEVEL_SETEXIT )
               {
                  iStatus = EXIT_FAILURE;
               }
               else if( hb_comp_iExitLevel == HB_EXITLEVEL_DELTARGET )
               {
                  iStatus = EXIT_FAILURE;
                  *pbSkipGen = TRUE;
                  printf( "\nNo code generated.\n" );
               }
            }
         }
         else
         {
            printf( "Cannot open %s, assumed external\n", szFileName );
         }
      }
   }
   else
   {
      hb_compGenError( hb_comp_szErrors, 'F', HB_COMP_ERR_BADFILENAME, szPrg, NULL );
      iStatus = EXIT_FAILURE;
   }

   return iStatus;
}

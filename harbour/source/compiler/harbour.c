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

#include <malloc.h>     /* required for allocating and freeing memory */

#include "compiler.h"

#if defined(DOS) && defined(__BORLANDC__)
   #include <limits.h>
   extern unsigned _stklen = UINT_MAX;
#endif

extern int harbour_main( char *szName );
extern BOOL Include( char * szFileName, PATHNAMES * pSearchPath );  /* end #include support */

/* output related functions */
extern void GenCCode( PHB_FNAME );      /* generates the C language output */
extern void GenJava( PHB_FNAME );       /* generates the Java language output */
extern void GenPascal( PHB_FNAME );     /* generates the Pascal language output */
extern void GenRC( PHB_FNAME );         /* generates the RC language output */
extern void GenPortObj( PHB_FNAME );    /* generates the portable objects */
#ifdef HARBOUR_OBJ_GENERATION
extern void GenObj32( PHB_FNAME );      /* generates OBJ 32 bits */
#endif

extern void PrintUsage( char * );
extern void PrintCredits( void );
extern void PrintLogo( void );

void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */
static char * RESERVED_FUNC( char * );

static void hb_compCheckDuplVars( PVAR pVars, char * szVarName, int iVarScope ); /*checks for duplicate variables definitions */
static void hb_compFieldGenPCode( BYTE , char * );      /* generates the pcode for database field */
static int hb_compFieldGetVarPos( char *, PFUNCTION );   /* return if passed name is a field variable */
static void hb_compFixReturns( void ); /* fixes all last defined function returns jumps offsets */
static PFUNCTION hb_compFunctionNew( char *, char );  /* creates and initialises the _FUNC structure */
static int hb_compLocalVarGetPos( char * szVarName ); /* returns the order + 1 of a local variable */
static int hb_compMemvarGetPos( char *, PFUNCTION );   /* return if passed name is a memvar variable */
static void hb_compMemvarGenPCode( BYTE , char * );      /* generates the pcode for memvar variable */
static USHORT hb_compVariableGetPos( PVAR pVars, char * szVarName ); /* returns the order + 1 of a variable if defined or zero */
static void hb_compVariableGenPCode( BYTE , char * );    /* generates the pcode for undeclared variable */

void EXTERNAL_LINKAGE close_on_exit( void );

extern int hb_comp_iLine;       /* currently parsed file line number */

/* global variables */
FILES hb_comp_files;
FUNCTIONS hb_comp_functions, hb_comp_funcalls;
SYMBOLS hb_comp_symbols;

int hb_comp_iLine = 1;     /* currently processed line number */
PFUNCTION hb_comp_pInitFunc;
PHB_FNAME hb_comp_pFileName = NULL;
BOOL hb_comp_bPPO = FALSE;                      /* flag indicating, is ppo output needed */
FILE * hb_comp_yyppo = NULL;                    /* output .ppo file */
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
ULONG hb_comp_ulLastLinePos = 0;    /* position of last opcode with line number */
ULONG hb_comp_ulMessageFix = 0;  /* Position of the message which needs to be changed */
int hb_comp_iStaticCnt = 0;       /* number of defined statics variables on the PRG */
int hb_comp_iVarScope = VS_LOCAL;   /* holds the scope for next variables to be defined */
                            /* different values for hb_comp_iVarScope */

/* local variables */
/* CHANGED to global */
PHB_FNAME hb_comp_pOutPath = NULL;
BOOL hb_comp_bCredits = FALSE;                  /* print credits */
BOOL hb_comp_bLogo = TRUE;                      /* print logo */
BOOL hb_comp_bSyntaxCheckOnly = FALSE;          /* syntax check only */
int  hb_comp_iLanguage = LANG_C;                /* default Harbour generated output language */

typedef struct __EXTERN
{
   char * szName;
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

/* EXTERNAL statement can be placed into any place in a function - this flag is
 * used to suppress error report generation
 */
static BOOL hb_comp_bExternal   = FALSE;
/* linked list with EXTERNAL symbols declarations
 */
static PEXTERN hb_comp_pExterns = NULL;

/* Table with parse warnings */

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

void hb_compInitVars( void );
void hb_compGenOutput( int );
void hb_compCheckPaths( void );
void hb_compOutputFile( void );

extern void hb_ChkCompilerSwitch( int, char * Args [] );
extern void hb_ChkEnvironVar( char * );
extern void hb_ChkCompileFileName( int, char * Args[] );

int main( int argc, char * argv[] )
{
   int iStatus = 0;
   BOOL bSkipGen;

   char szFileName[ _POSIX_PATH_MAX ];    /* filename to parse */
   char szPpoName[ _POSIX_PATH_MAX ];

   hb_comp_pFileName = NULL;
   hb_comp_pOutPath = NULL;

   /* Initializes hb_comp_pFileName with file name to compile */
   hb_ChkCompileFileName( argc, argv );

   /* First check the environment variables */
   hb_ChkCompilerSwitch( 0, NULL );

   /* Then check command line arguments
      This will override duplicated environment settings */
   hb_ChkCompilerSwitch( argc, argv );

   if( hb_comp_bLogo )
      PrintLogo();

   if( hb_comp_bCredits )
   {
      PrintCredits();
      return iStatus;
   }

   if( hb_comp_pFileName )
   {
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
            hb_compGenError( hb_comp_szErrors, 'F', ERR_CREATE_PPO, szPpoName, NULL );
            return iStatus;
         }
      }
   }
   else
   {
      PrintUsage( argv[ 0 ] );
      return iStatus;
   }

   /* Initialization of preprocessor arrays */
   hb_pp_Init();

   /* Initialize support variables */
   hb_compInitVars();

   atexit( close_on_exit );

   if( Include( szFileName, NULL ) )
   {
      hb_compCheckPaths();

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

      if( ! hb_comp_bSyntaxCheckOnly && ! bSkipGen && ( hb_comp_iErrorCount == 0 ) )
      {
         /* we create the output file name */
         hb_compOutputFile();

         /* fix all previous function returns offsets */
         hb_compFixReturns();

         if( ! hb_comp_bQuiet )
         {
            if( ! hb_comp_bStartProc )
               --hb_comp_iFunctionCnt;
            printf( "\rLines %i, Functions/Procedures %i\n", hb_comp_iLine, hb_comp_iFunctionCnt );
         }

         hb_compGenOutput( hb_comp_iLanguage );
      }

      if( hb_comp_bPPO )
         fclose( hb_comp_yyppo );
   }
   else
   {
      printf( "Cannot open input file: %s\n", szFileName );
      /* printf( "No code generated\n" ); */
      iStatus = 1;
   }

   hb_xfree( ( void * ) hb_comp_pFileName );

   if( hb_comp_pOutPath )
      hb_xfree( hb_comp_pOutPath );

   if( hb_comp_iErrorCount > 0 )
      iStatus = EXIT_FAILURE;

   return iStatus;
}

#if defined(__IBMCPP__) || defined(_MSC_VER)
int isatty( int handle )
{
   return ( handle < 4 ) ? 1 : 0;
}
#endif

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

void * hb_xgrab( ULONG ulSize )         /* allocates fixed memory, exits on failure */
{
   void * pMem = malloc( ulSize );

   if( ! pMem )
      hb_compGenError( hb_comp_szErrors, 'F', ERR_MEMALLOC, NULL, NULL );

   return pMem;
}

void * hb_xrealloc( void * pMem, ULONG ulSize )       /* reallocates memory */
{
   void * pResult = realloc( pMem, ulSize );

   if( ! pResult )
      hb_compGenError( hb_comp_szErrors, 'F', ERR_MEMREALLOC, NULL, NULL );

   return pResult;
}

void hb_xfree( void * pMem )            /* frees fixed memory */
{
   if( pMem )
      free( pMem );
   else
      hb_compGenError( hb_comp_szErrors, 'F', ERR_MEMFREE, NULL, NULL );
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

void hb_compVariableAdd( char * szVarName, char cValueType )
{
   PVAR pVar, pLastVar;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 && hb_comp_iVarScope == VS_LOCAL )
   {
      /* Variable declaration is outside of function/procedure body.
         In this case only STATIC and PARAMETERS variables are allowed. */
      --hb_comp_iLine;
      hb_compGenError( hb_comp_szErrors, 'E', ERR_OUTSIDE, NULL, NULL );
      return;
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( ( hb_comp_functions.pLast->bFlags & FUN_STATEMENTS ) && !( hb_comp_iVarScope == VS_FIELD || ( hb_comp_iVarScope & VS_MEMVAR ) ) )
   {
      --hb_comp_iLine;
      hb_compGenError( hb_comp_szErrors, 'E', ERR_FOLLOWS_EXEC, ( hb_comp_iVarScope == VS_LOCAL ? "LOCAL" : "STATIC" ), NULL );
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
      hb_compCheckDuplVars( pFunc->pFields, szVarName, hb_comp_iVarScope );
      hb_compCheckDuplVars( pFunc->pStatics, szVarName, hb_comp_iVarScope );
      if( !( hb_comp_iVarScope == VS_PRIVATE || hb_comp_iVarScope == VS_PUBLIC ) )
         hb_compCheckDuplVars( pFunc->pMemvars, szVarName, hb_comp_iVarScope );
   }
   else
      /* variable defined in a codeblock */
      hb_comp_iVarScope = VS_PARAMETER;
   hb_compCheckDuplVars( pFunc->pLocals, szVarName, hb_comp_iVarScope );

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

               pSym = hb_compSymbolFind( szVarName, &wPos ); /* check if symbol exists already */
               if( ! pSym )
                  pSym = hb_compSymbolAdd( hb_strdup( szVarName ), &wPos );
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
                  pVar->szName = hb_strdup( szVarName );
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
               hb_compGenPushSymbol( hb_strdup( "__MVPRIVATE" ), 1);
               hb_compGenPushNil();
               hb_compGenPushSymbol( hb_strdup( szVarName ), 0 );
               hb_compGenPCode3( HB_P_DO, 1, 0 );
               pSym = hb_compSymbolFind( szVarName, NULL );
               pSym->cScope |= VS_MEMVAR;
            }
            break;
         case VS_PUBLIC:
            {
               hb_compGenPushSymbol( hb_strdup( "__MVPUBLIC" ), 1);
               hb_compGenPushNil();
               hb_compGenPushSymbol( hb_strdup( szVarName ), 0 );
               hb_compGenPCode3( HB_P_DO, 1, 0 );
               pSym = hb_compSymbolFind( szVarName, NULL );
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

PCOMSYMBOL hb_compSymbolAdd( char * szSymbolName, USHORT * pwPos )
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

/*
 * Function generates passed pcode for passed database field
 */
void hb_compFieldGenPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pVar;

   pVar = hb_compSymbolFind( szVarName, &wVar );
   if( ! pVar )
      pVar = hb_compSymbolAdd( szVarName, &wVar );
   pVar->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

/*
 * This function creates and initialises the _FUNC structure
 */
static PFUNCTION hb_compFunctionNew( char * szName, HB_SYMBOLSCOPE cScope )
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

   return pFunc;
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

   hb_compFixReturns();    /* fix all previous function returns offsets */

   pFunc = hb_compFunctionFind( szFunName );
   if( pFunc )
   {
      /* The name of a function/procedure is already defined */
      if( ( pFunc != hb_comp_functions.pFirst ) || hb_comp_bStartProc )
         /* it is not a starting procedure that was automatically created */
         hb_compGenError( hb_comp_szErrors, 'F', ERR_FUNC_DUPL, szFunName, NULL );
   }

   szFunction = RESERVED_FUNC( szFunName );
   if( szFunction && !( hb_comp_functions.iCount==0 && !hb_comp_bStartProc ) )
   {
      /* We are ignoring it when it is the name of PRG file and we are
       * not creating implicit starting procedure
       */
      hb_compGenError( hb_comp_szErrors, 'E', ERR_FUNC_RESERVED, szFunction, szFunName );
   }

   hb_comp_iFunctionCnt++;

   pSym = hb_compSymbolFind( szFunName, NULL );
   if( ! pSym )
      /* there is not a symbol on the symbol table for this function name */
      pSym = hb_compSymbolAdd( szFunName, NULL );

   if( cScope != FS_PUBLIC )
/*    pSym->cScope = FS_PUBLIC; */
/* else */
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


PFUNCTION hb_compFunctionKill( PFUNCTION pFunc )
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
/* hb_xfree( ( void * ) pFunc->szName ); The name will be released in hb_compSymbolKill() */
   hb_xfree( ( void * ) pFunc );

   return pNext;
}


PCOMSYMBOL hb_compSymbolKill( PCOMSYMBOL pSym )
{
   PCOMSYMBOL pNext = pSym->pNext;

   hb_xfree( ( void * ) pSym->szName );
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

PVAR hb_compVariableFind( PVAR pVars, USHORT wOrder ) /* returns variable if defined or zero */
{
   USHORT w = 1;

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

static int hb_compLocalVarGetPos( char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   int iVar = 0;
   PFUNCTION pFunc = hb_comp_functions.pLast;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      return hb_compVariableGetPos( pFunc->pLocals, szVarName );
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

         pFunc = pFunc->pOwner;
         while( pFunc )
         {
            iVar = hb_compVariableGetPos( pFunc->pLocals, szVarName );
            if( iVar )
            {
               if( pFunc->pOwner )
               {
                  /* this variable is defined in a parent codeblock
                  * It is not possible to access a parameter of a codeblock in which
                  * the current codeblock is defined
                  */
                  hb_compGenError( hb_comp_szErrors, 'E', ERR_OUTER_VAR, szVarName, NULL );
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
   iPos = hb_compVariableGetPos( pFunc->pStatics, szVarName );
   if( iPos )
      return iPos + pFunc->iStaticsBase;

   /* Next we have to check the list of global static variables
     * Note: It is not possible to have global static variables when
     * implicit starting procedure is defined
     */
   if( !hb_comp_bStartProc )
   {
      iPos = hb_compVariableGetPos( hb_comp_functions.pFirst->pStatics, szVarName );
      if( iPos )
         return iPos;
   }
   return 0;
}

/* Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 */
static int hb_compFieldGetVarPos( char * szVarName, PFUNCTION pFunc )
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
static int hb_compMemvarGetPos( char * szVarName, PFUNCTION pFunc )
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

USHORT hb_compSymbolFixPos( USHORT wCompilePos )
{
   return ( hb_comp_bStartProc ? wCompilePos - 1 : wCompilePos - 2 );
}


/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table
 */
PCOMSYMBOL hb_compSymbolFind( char * szSymbolName, USHORT * pwPos )
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

PCOMSYMBOL hb_compSymbolGetPos( USHORT wSymbol )   /* returns a symbol based on its index on the symbol table */
{
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT w = 1;

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

ULONG hb_compGenJump( LONG lOffset )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szErrors, 'F', ERR_JUMP_TOO_LONG, NULL, NULL );

   hb_compGenPCode3( HB_P_JUMP, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ) );

   return hb_comp_functions.pLast->lPCodePos - 2;
}

ULONG hb_compGenJumpFalse( LONG lOffset )
{
   /* TODO: We need a longer offset (longer then two bytes)
    */
   if( lOffset < ( LONG ) SHRT_MIN || lOffset > ( LONG ) SHRT_MAX )
      hb_compGenError( hb_comp_szErrors, 'F', ERR_JUMP_TOO_LONG, NULL, NULL );

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
      hb_compGenError( hb_comp_szErrors, 'F', ERR_JUMP_TOO_LONG, NULL, NULL );

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
      hb_compGenError( hb_comp_szErrors, 'F', ERR_JUMP_TOO_LONG, NULL, NULL );
   hb_compGenPCode3( HB_P_JUMPTRUE, HB_LOBYTE( lOffset ), HB_HIBYTE( lOffset ) );

   return hb_comp_functions.pLast->lPCodePos - 2;
}


void hb_compLinePush( void ) /* generates the pcode with the currently compiled source code line */
{
   if( hb_comp_bLineNumbers && ! hb_comp_bDontGenLineNum )
   {
      if( ( ( hb_comp_functions.pLast->lPCodePos - hb_comp_ulLastLinePos ) > 3 ) || hb_comp_bDebugInfo )
      {
         hb_comp_ulLastLinePos = hb_comp_functions.pLast->lPCodePos;
         hb_compGenPCode3( HB_P_LINE, HB_LOBYTE( hb_comp_iLine ), HB_HIBYTE( hb_comp_iLine ) );
      }
      else
      {
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +1 ] = HB_LOBYTE( hb_comp_iLine );
         hb_comp_functions.pLast->pCode[ hb_comp_ulLastLinePos +2 ] = HB_HIBYTE( hb_comp_iLine );
      }
   }
   if( hb_comp_functions.pLast->bFlags & FUN_BREAK_CODE )
   {
      /* previous line contained RETURN/BREAK/LOOP/EXIT statement */
      hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_UNREACHABLE, NULL, NULL );
   }
   hb_comp_bDontGenLineNum = FALSE;
   /* clear RETURN/BREAK flag */
   hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE );
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
         hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_UNREACHABLE, NULL, NULL );
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
      hb_comp_bExternal = FALSE;
      if( ! hb_comp_bStartProc && hb_comp_functions.iCount <= 1 )
      {
         hb_compGenError( hb_comp_szErrors, 'E', ERR_OUTSIDE, NULL, NULL );
      }
   }

   hb_comp_functions.pLast->bFlags |= FUN_STATEMENTS;
   hb_compLinePush();
}

/*
 * Function generates passed pcode for passed variable name
 */
static void hb_compVariableGenPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pSym;
   PFUNCTION pOwnerFunc = NULL;
   int iType = VS_LOCAL; /* not really */

   /* Check if it is a FIELD declared in current function
    */
   wVar = hb_compFieldGetVarPos( szVarName, hb_comp_functions.pLast );
   if( wVar == 0 )
   {
      /* Check if it is a MEMVAR declared in current function
       */
      wVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pLast );
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
      wVar = hb_compFieldGetVarPos( szVarName, hb_comp_functions.pFirst );
      if( wVar == 0 )
      {
         wVar = hb_compMemvarGetPos( szVarName, hb_comp_functions.pFirst );
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
         hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_MEMVAR_ASSUMED, szVarName, NULL );
      }
      else
         hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_AMBIGUOUS_VAR, szVarName, NULL );
   }

   if( iType == VS_FIELD )
   {  /* variable is declared using FIELD statement */
      PVAR pField = hb_compVariableFind( pOwnerFunc->pFields, wVar );

      if( pField->szAlias )
      {  /* the alias was specified in FIELD declaration */
         if( bPCode == HB_P_POPVARIABLE )
            bPCode = HB_P_POPALIASEDFIELD;
         else if( bPCode == HB_P_PUSHVARIABLE )
            bPCode = HB_P_PUSHALIASEDFIELD;
         else
            /* pushing fields by reference is not allowed */
            hb_compGenError( hb_comp_szErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
         /*
          * Push alias symbol before the field symbol
          */
         hb_compGenPushSymbol( hb_strdup( pField->szAlias ), 0 );
      }
      else
      {  /* this is unaliased field */
         if( bPCode == HB_P_POPVARIABLE )
            bPCode = HB_P_POPFIELD;
         else if( bPCode == HB_P_PUSHVARIABLE )
            bPCode = HB_P_PUSHFIELD;
         else if( bPCode == HB_P_PUSHMEMVARREF )
            /* pushing fields by reference is not allowed */
            hb_compGenError( hb_comp_szErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
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
   pSym = hb_compSymbolFind( szVarName, &wVar );
   if( ! pSym )
      pSym = hb_compSymbolAdd( szVarName, &wVar );
   pSym->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

/*
 * Function generates passed pcode for passed memvar name
 */
void hb_compMemvarGenPCode( BYTE bPCode, char * szVarName )
{
   USHORT wVar;
   PCOMSYMBOL pSym;

   /* Check if this variable name is placed into the symbol table
    */
   pSym = hb_compSymbolFind( szVarName, &wVar );
   if( ! pSym )
      pSym = hb_compSymbolAdd( szVarName, &wVar );
   pSym->cScope |= VS_MEMVAR;
   hb_compGenPCode3( bPCode, HB_LOBYTE( wVar ), HB_HIBYTE( wVar ) );
}

void hb_compGenMessage( char * szMsgName )       /* sends a message to an object */
{
   USHORT wSym;
   PCOMSYMBOL pSym = hb_compSymbolFind( szMsgName, &wSym );

   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym = hb_compSymbolAdd( szMsgName, &wSym );
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

   iVar = hb_compLocalVarGetPos( szVarName );
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
         hb_compVariableGenPCode( HB_P_POPVARIABLE, szVarName );
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
            hb_compMemvarGenPCode( HB_P_POPMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compMemvarGenPCode( HB_P_POPMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compFieldGenPCode( HB_P_POPFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0 );
                  hb_compFieldGenPCode( HB_P_POPALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compFieldGenPCode( HB_P_POPALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack */
      hb_compFieldGenPCode( HB_P_POPALIASEDFIELD, szVarName );
}

/* generates the pcode to push a nonaliased variable value to the virtual
 * machine stack
 */
void hb_compGenPushVar( char * szVarName )
{
   int iVar;

   iVar = hb_compLocalVarGetPos( szVarName );
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
         hb_compVariableGenPCode( HB_P_PUSHVARIABLE, szVarName );
      }
   }
}

void hb_compGenPushVarRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   USHORT iVar;

   iVar = hb_compLocalVarGetPos( szVarName );
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
         hb_compVariableGenPCode( HB_P_PUSHMEMVARREF, szVarName );
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
            hb_compMemvarGenPCode( HB_P_PUSHMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "MEMVAR", strlen( szAlias ) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               hb_compMemvarGenPCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( szAlias, "FIELD", strlen( szAlias ) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  hb_compFieldGenPCode( HB_P_PUSHFIELD, szVarName );
               }
               else
               {  /* database alias */
                  hb_compGenPushSymbol( hb_strdup( szAlias ), 0 );
                  hb_compFieldGenPCode( HB_P_PUSHALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else
      {
         hb_compGenPushLong( lWorkarea );
         hb_compFieldGenPCode( HB_P_PUSHALIASEDFIELD, szVarName );
      }
   }
   else
      /* Alias is already placed on stack */
      hb_compFieldGenPCode( HB_P_PUSHALIASEDFIELD, szVarName );
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
void hb_compGenPushDouble( double dNumber, BYTE bDec )
{
   hb_compGenPCode1( HB_P_PUSHDOUBLE );
   hb_compGenPCodeN( ( BYTE * ) &dNumber, sizeof( double ) );
   hb_compGenPCode1( bDec );
}

void hb_compGenPushFunCall( char * szFunName )
{
   char * szFunction = RESERVED_FUNC( szFunName );

   /* If an abbreviated function name was used - change it for whole name */
   hb_compGenPushSymbol( ( szFunction ? hb_strdup( szFunction ) : szFunName ), 1 );
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
void hb_compGenPushSymbol( char * szSymbolName, int iIsFunction )
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

   pSym = hb_compSymbolFind( szSymbolName, &wSym );
   if( ! pSym )  /* the symbol was not found on the symbol table */
   {
      pSym = hb_compSymbolAdd( szSymbolName, &wSym );
      if( iIsFunction )
         hb_compFunCallAdd( szSymbolName );
   }
   else
   {
      if( iIsFunction && ! hb_compFunCallFind( szSymbolName ) )
         hb_compFunCallAdd( szSymbolName );
   }
   hb_compGenPCode3( HB_P_PUSHSYM, HB_LOBYTE( wSym ), HB_HIBYTE( wSym ) );
}


static void hb_compCheckDuplVars( PVAR pVar, char * szVarName, int iVarScope )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         if( ! ( iVarScope & VS_PARAMETER ) )
            --hb_comp_iLine;
         hb_compErrorDuplVar( szVarName );
         break;
      }
      else
         pVar = pVar->pNext;
   }
}

static void hb_compFixReturns( void ) /* fixes all last defined function returns jumps offsets */
{
   if( hb_comp_iWarnings && hb_comp_functions.pLast )
   {
      PVAR pVar;

      pVar = hb_comp_functions.pLast->pLocals;
      while( pVar )
      {
         if( pVar->szName && hb_comp_functions.pLast->szName && hb_comp_functions.pLast->szName[0] && ! pVar->iUsed )
            hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, hb_comp_functions.pLast->szName );

         pVar = pVar->pNext;
      }

      pVar = hb_comp_functions.pLast->pStatics;
      while( pVar )
      {
         if( pVar->szName && hb_comp_functions.pLast->szName && hb_comp_functions.pLast->szName[0] && ! pVar->iUsed )
            hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, hb_comp_functions.pLast->szName );

         pVar = pVar->pNext;
      }

      /* Check if the function returned some value
       */
      if( (hb_comp_functions.pLast->bFlags & FUN_WITH_RETURN) == 0 &&
          (hb_comp_functions.pLast->bFlags & FUN_PROCEDURE) == 0 )
         hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_FUN_WITH_NO_RETURN,
                     hb_comp_functions.pLast->szName, NULL );
   }
}

/* Generate the opcode to open BEGIN/END sequence
 * This code is simmilar to JUMP opcode - the offset will be filled with
 * - either the address of HB_P_SEQEND opcode if there is no RECOVER clause
 * - or the address of RECOVER code
 */
ULONG hb_compSequenceBegin( void )
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
ULONG hb_compSequenceEnd( void )
{
   hb_compGenPCode3( HB_P_SEQEND, 0, 0 );

   return hb_comp_functions.pLast->lPCodePos - 2;
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
         hb_comp_ulLastLinePos = ulStartPos - 4;
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

      hb_comp_pInitFunc = hb_compFunctionNew( hb_strdup("(_INITSTATICS)"), FS_INIT );
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
   PFUNCTION pFunc = hb_compFunctionNew( NULL, FS_STATIC );

   pFunc->pOwner       = hb_comp_functions.pLast;
   pFunc->iStaticsBase = hb_comp_functions.pLast->iStaticsBase;

   hb_comp_functions.pLast = pFunc;
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

   /* NOTE: 8 = HB_P_PUSHBLOCK + USHORT( size ) + USHORT( wParams ) + USHORT( wLocals ) + _ENDBLOCK */
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
      wPos = hb_compVariableGetPos( pFunc->pLocals, pVar->szName );
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
         hb_compGenWarning( hb_comp_szWarnings, 'W', WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );

      /* free used variables */
      pFree = pVar;
      hb_xfree( ( void * ) pFree->szName );
      pVar = pVar->pNext;
      hb_xfree( ( void * ) pFree );
   }
   hb_xfree( ( void * ) pCodeblock );
}

/* ************************************************************************* */

/* initialize support variables */
void hb_compInitVars( void )
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

   hb_comp_pInitFunc = NULL;
   hb_comp_bAnyWarning = FALSE;
}

void hb_compGenOutput( int iLanguage )
{
   switch( iLanguage )
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

void hb_compCheckPaths( void )
{
   char * szInclude = getenv( "INCLUDE" );

   if( szInclude )
   {
      char * pPath;
      char * pDelim;

      pPath = szInclude = hb_strdup( szInclude );
      while( ( pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR ) ) != NULL )
      {
         *pDelim = '\0';
         AddSearchPath( pPath, &hb_comp_pIncludePath );
         pPath = pDelim + 1;
      }
      AddSearchPath( pPath, &hb_comp_pIncludePath );
   }
}

void hb_compOutputFile( void )
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


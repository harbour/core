%{
/*
 * $Id$
 */

/*
 * Harbour compiler (yacc rules and actions)
 * Build 29 summer 1999
 * Usage: bison -d -v harbour.y
 * You may find Bison at www.harbour.project.org
 *
 * Copyright(C) 1999 by Antonio Linares.
 *
 * The generated C output files structure by the function GenCCode()
 * is also under this copyright.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 *
 * Partial Copyright (C) 1999 Eddie Runia <eddie@runia.com>
 *   partial copyright regarding generation portable objects
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <malloc.h>     /* required for allocating and freeing memory */
#include <ctype.h>
#include "extend.h"
#include "pcode.h"      /* pcode values */
#include "compiler.h"
#include "hberrors.h"
#include "hbpp.h"
#include "hbver.h"

#define debug_msg( x, z )

/* TODO: #define this for various platforms */
#define PATH_DELIMITER "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)

#define OPT_DELIMITER  "/-"
#define IS_OPT_SEP( c ) (strchr(OPT_DELIMITER, (c))!=NULL)

extern FILE *yyin;      /* currently yacc parsed file */
extern int iLine;       /* currently parsed file line number */
  /* Following two lines added for preprocessor */
extern int lPpo;        /* flag indicating, is ppo output needed */
extern FILE *yyppo;     /* output .ppo file */

typedef struct          /* #include support */
{
   FILE * handle;       /* handle of the opened file */
   void * pBuffer;      /* buffer used by yacc */
   char * szFileName;   /* name of the file */
   void * pPrev;        /* pointer to the previous opened file */
   void * pNext;        /* pointer to the next opened file */
   int    iLine;        /* currently processed line number */
} _FILE, * PFILE;       /* structure to hold an opened PRG or CH */

typedef struct
{
   PFILE pLast;         /* pointer to the last opened file */
   int   iFiles;        /* number of files currently opened */
} FILES;                /* structure to control several opened PRGs and CHs */

int Include( char * szFileName, PATHNAMES *pSearchPath );  /* end #include support */

/*
 * flags for bFlags member
*/
#define FUN_STATEMENTS    1 /* Function have at least one executable statement */
#define FUN_USES_STATICS  2 /* Function uses static variables */
#define FUN_PROCEDURE     4 /* This is a procedure that shouldn't return value */
#define FUN_ILLEGAL_INIT  8 /* Attempt to initialize static variable with a function call */
#define FUN_USES_LOCAL_PARAMS 16 /* parameters are declared using () */

/* pcode chunks bytes size */
#define PCODE_CHUNK   100

typedef struct __ELSEIF
{
   WORD wOffset;
   struct __ELSEIF * pNext;
} _ELSEIF, * PELSEIF;      /* support structure for else if pcode fixups */

typedef struct _LOOPEXIT
{
  WORD wOffset;
  WORD wLine;
  struct _LOOPEXIT *pLoopList;
  struct _LOOPEXIT *pExitList;
  struct _LOOPEXIT *pNext;
} LOOPEXIT, * PTR_LOOPEXIT;  /* support structure for EXIT and LOOP statements */
static void LoopStart( void );
static void LoopEnd( void );
static void LoopLoop( void );
static void LoopExit( void );
static void LoopHere( void );

typedef struct __EXTERN
{
   char * szName;
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

FILENAME *SplitFilename( char * );  /* splits filename into a path, a name and an extension */
char *MakeFilename( char *, FILENAME *);  /* joins a path, a name an an extension int filename */

/* Support for aliased expressions
 */
typedef struct _ALIASID
{
   char type;
   union {
      int iAlias;
      char *szAlias;
   } alias;
   struct _ALIASID *pPrev;
} ALIASID, *ALIASID_PTR;

#define  ALIAS_NUMBER   1
#define  ALIAS_NAME     2
#define  ALIAS_EVAL     3

void AliasAddInt( int );
void AliasAddExp( void );
void AliasAddStr( char * );
void AliasPush( void );
void AliasPop( void );
void AliasSwap( void );
void AliasAdd( ALIASID_PTR );
void AliasRemove( void );

/* lex & yacc related prototypes */
void yyerror( char * ); /* parsing error management function */
int yylex( void );      /* main lex token function, called by yyparse() */
int yyparse( void );    /* main yacc parsing function */
#ifdef __cplusplus
extern "C" int yywrap( void );
#else
int yywrap( void );     /* manages the EOF of current processed file */
#endif
  /* Following line added for preprocessor */
void Hbpp_init ( void );

#ifdef __cplusplus
typedef struct yy_buffer_state *YY_BUFFER_STATE;
YY_BUFFER_STATE yy_create_buffer( FILE *, int ); /* yacc functions to manage multiple files */
void yy_switch_to_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
void yy_delete_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
#else
void * yy_create_buffer( FILE *, int ); /* yacc functions to manage multiple files */
void yy_switch_to_buffer( void * ); /* yacc functions to manage multiple files */
void yy_delete_buffer( void * ); /* yacc functions to manage multiple files */
#endif

char * yy_strdup( char *p );  /* this will exit if there is not enough memory */
char *yy_strupr( char *p );

#if 0
static void __yy_memcpy( char * from, char * to, int count ); /* Bison prototype */
#endif

/* production related functions */
PFUNCTION AddFunCall( char * szFuntionName );
void AddExtern( char * szExternName ); /* defines a new extern name */
void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */
void AddVar( char * szVarName ); /* add a new param, local, static variable to a function definition or a public or private */
PCOMSYMBOL AddSymbol( char *, WORD * );
void CheckDuplVars( PVAR pVars, char * szVarName, int iVarScope ); /*checks for duplicate variables definitions */
void Dec( void );                  /* generates the pcode to decrement the latest value on the virtual machine stack */
void DimArray( WORD wDimensions ); /* instructs the virtual machine to build an array with wDimensions */
void Do( BYTE bParams );      /* generates the pcode to execute a Clipper function discarding its result */
void Duplicate( void ); /* duplicates the virtual machine latest stack latest value and places it on the stack */
void DupPCode( WORD wStart ); /* duplicates the current generated pcode from an offset */
void FieldPCode( BYTE , char * );      /* generates the pcode for database field */
void FixElseIfs( void * pIfElseIfs ); /* implements the ElseIfs pcode fixups */
void FixReturns( void ); /* fixes all last defined function returns jumps offsets */
WORD FixSymbolPos( WORD );    /* converts symbol's compile-time position into generation-time position */
void Function( BYTE bParams ); /* generates the pcode to execute a Clipper function pushing its result */
PFUNCTION FunctionNew( char *, char );  /* creates and initialises the _FUNC structure */
void FunDef( char * szFunName, SYMBOLSCOPE cScope, int iType ); /* starts a new Clipper language function definition */
void GenArray( WORD wElements ); /* instructs the virtual machine to build an array and load elemnst from the stack */
void * GenElseIf( void * pFirstElseIf, WORD wOffset ); /* generates a support structure for elseifs pcode fixups */
void GenExterns( void ); /* generates the symbols for the EXTERN names */
PFUNCTION GetFuncall( char * szFunName ); /* locates a previously defined called function */
int GetFieldVarPos( char *, PFUNCTION *);   /* return if passed name is a field variable */
PVAR GetVar( PVAR pVars, WORD wOrder ); /* returns a variable if defined or zero */
WORD GetVarPos( PVAR pVars, char * szVarName ); /* returns the order + 1 of a variable if defined or zero */
int GetLocalVarPos( char * szVarName ); /* returns the order + 1 of a local variable */
PCOMSYMBOL GetSymbol( char *, WORD * ); /* returns a symbol pointer from the symbol table */
PCOMSYMBOL GetSymbolOrd( WORD );   /* returns a symbol based on its index on the symbol table */
void Inc( void );                       /* generates the pcode to increment the latest value on the virtual machine stack */
WORD Jump( int iOffset );               /* generates the pcode to jump to a specific offset */
WORD JumpFalse( int iOffset );          /* generates the pcode to jump if false */
void JumpHere( int iOffset );           /* returns the pcode pos where to set a jump offset */
void JumpThere( int iOffset, WORD wTo ); /* sets a jump offset */
WORD JumpTrue( int iOffset );           /* generates the pcode to jump if true */
PFUNCTION KillFunction( PFUNCTION );    /* releases all memory allocated by function and returns the next one */
PCOMSYMBOL KillSymbol( PCOMSYMBOL );    /* releases all memory allocated by symbol and returns the next one */
void Line( void );                      /* generates the pcode with the currently compiled source code line */
void LineBody( void );                  /* generates the pcode with the currently compiled source code line */
void VariablePCode( BYTE , char * );    /* generates the pcode for memvar variable */
void Message( char * szMsgName );       /* sends a message to an object */
void MessageFix( char * szMsgName );    /* fix a generated message to an object */
void MessageDupl( char * szMsgName );   /* fix a one generated message to an object and duplicate */
void PopId( char * szVarName );         /* generates the pcode to pop a value from the virtual machine stack onto a variable */
void PushDouble( double fNumber, BYTE bDec ); /* Pushes a number on the virtual machine stack */
void PushFunCall( char * );             /* generates the pcode to push function's call */
void PushId( char * szVarName );        /* generates the pcode to push a variable value to the virtual machine stack */
void PushIdByRef( char * szVarName );   /* generates the pcode to push a variable by reference to the virtual machine stack */
void PushInteger( int iNumber );        /* Pushes a integer number on the virtual machine stack */
void PushLogical( int iTrueFalse );     /* pushes a logical value on the virtual machine stack */
void PushLong( long lNumber );          /* Pushes a long number on the virtual machine stack */
void PushNil( void );                   /* Pushes nil on the virtual machine stack */
void PushString( char * szText );       /* Pushes a string on the virtual machine stack */
void PushSymbol( char * szSymbolName, int iIsFunction ); /* Pushes a symbol on to the Virtual machine stack */
void GenPCode1( BYTE );             /* generates 1 byte of pcode */
void GenPCode3( BYTE, BYTE, BYTE ); /* generates 3 bytes of pcode */
void GenPCodeN( BYTE * pBuffer, WORD wSize );  /* copy bytes to a pcode buffer */
char * SetData( char * szMsg );     /* generates an underscore-symbol name for a data assignment */

/* support for FIELD declaration */
void FieldsSetAlias( char *, int );
int FieldsCount( void );

/* Codeblocks */
void CodeBlockStart( void );        /* starts a codeblock creation */
void CodeBlockEnd( void );          /* end of codeblock creation */

/* Static variables */
void StaticDefStart( void );
void StaticDefEnd( WORD );
void StaticAssign( void ); /* checks if static variable is initialized with function call */

/* output related functions */
void GenCCode( char *, char * );      /* generates the C language output */
void GenJava( char *, char * );       /* generates the Java language output */
void GenPascal( char *, char * );     /* generates the Pascal language output */
void GenRC( char *, char * );         /* generates the RC language output */
void GenPortObj( char *, char * );    /* generates the portable objects */
#ifdef HARBOUR_OBJ_GENERATION
void GenObj32( char *, char * );      /* generates OBJ 32 bits */
#endif

/* argument checking */
void CheckArgs( char *, int );

void PrintUsage( char * );

#define YYDEBUG        1    /* Parser debug information support */

typedef enum
{
   LANG_C,                  /* C language (by default) <file.c> */
   LANG_JAVA,               /* Java <file.java> */
   LANG_PASCAL,             /* Pascal <file.pas> */
   LANG_RESOURCES,          /* Resources <file.rc> */
   LANG_PORT_OBJ            /* Portable objects <file.hrb> */
} LANGUAGES;                /* supported Harbour output languages */

#define VS_LOCAL      1
#define VS_STATIC     2
#define VS_FIELD      4
#define VS_PARAMETER  8
#define VS_PRIVATE    64
#define VS_PUBLIC     128
#define VS_MEMVAR     (VS_PUBLIC | VS_PRIVATE)
int iVarScope = VS_LOCAL;   /* holds the scope for next variables to be defined */
                            /* different values for iVarScope */

/* Table with parse errors */
char * _szCErrors[] = { "Statement not allowed outside of procedure or function",
                       "Redefinition of procedure or function: \'%s\'",
                       "Duplicate variable declaration: \'%s\'",
                       "%s declaration follows executable statement",
                       "Outer codeblock variable is out of reach: \'%s\'",
                       "Invalid numeric format '.'",
                       "Unterminated string: \'%s\'",
                       "Redefinition of predefined function %s: \'%s\'",
                       "Illegal initializer: \'%s\'",
                       "ENDIF does not match IF",
                       "ENDDO does not match WHILE",
                       "ENDCASE does not match DO CASE",
                       "NEXT does not match FOR",
                       "ELSE does not match IF",
                       "ELSEIF does not match IF",
                       "Syntax error: \'%s\'",
                       "Unclosed control structures at line: %i",
                       "%s statement with no loop in sight",
                       "Syntax error: \'%s\' in: \'%s\'",
                       "Incomplete statement: %s",
                       "Incorrect number of arguments: %s %s",
                       "Invalid lvalue",
                       "Invalid use of \'@\' (pass by reference): \'%s\'",
                        "Formal parameters already declared"
                     };

/* Table with parse warnings */
char * _szCWarnings[] = {
      "Ambiguous reference: \'%s\'",
      "Ambiguous reference, assuming memvar: \'%s\'",
      "Variable: \'%s\' declared but not used in function: \'%s\'",
      "CodeBlock Parameter: \'%s\' declared but not used in function: \'%s\'",
      "Incompatible type in assignment to: \'%s\' expected: \'%s\'",
      "Incompatible operand type: \'%s\' expected: \'Logical\'",
      "Incompatible operand type: \'%s\' expected: \'Numeric\'",
      "Incompatible operand types: \'%s\' and: \'%s\'",
      "Suspicious type in assignment to: \'%s\' expected: \'%s\'",
      "Suspicious operand type: \'UnKnown\' expected: \'%s\'",
      "Suspicious operand type: \'UnKnown\' expected: \'Logical\'",
      "Suspicious operand type: \'UnKnown\' expected: \'Numeric\'"
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
#define RESERVED_FUNCTIONS  sizeof(_szReservedFun) / sizeof(char *)
/* function compares strings upto maximum 4 characters (used in bsearch) */
/* Borland C 3.1 reports error when this forward declaration is used
 * int sz_compare4( const void *, const void * );
 *
 */
/* Compare first 4 characters
 * If they are the same then compare the whole name
 * SECO() is not allowed because of Clipper function SECONDS()
 * however SECO32() is a valid name.
 */
int EXTERNAL_LINKAGE sz_compare4( const void *pLookup, const void *pReserved )
{
  int iCmp;

  iCmp = strncmp( (const char *)pLookup, *((const char * *)pReserved), 4 );
  if( iCmp == 0 )
     iCmp = strncmp( (const char *)pLookup, *((const char * *)pReserved),
                     strlen((const char *)pLookup) );
  return iCmp;
}

#define RESERVED_FUNC(szName) \
 bsearch( (szName), _szReservedFun, RESERVED_FUNCTIONS, sizeof(char*), sz_compare4 )


FILES files;
FUNCTIONS functions, funcalls;
PFUNCTION _pInitFunc;
SYMBOLS symbols;

BOOL _bStartProc = TRUE;             /* holds if we need to create the starting procedure */
BOOL _bLineNumbers = TRUE;           /* holds if we need pcodes with line numbers */
BOOL _bQuiet = FALSE;                /* quiet mode */
BOOL _bSyntaxCheckOnly = FALSE;      /* syntax check only */
int  _iLanguage = LANG_C;            /* default Harbour generated output language */
BOOL _bRestrictSymbolLength = FALSE; /* generate 10 chars max symbols length */
BOOL _bShortCuts = TRUE;             /* .and. & .or. expressions shortcuts */
BOOL _bWarnings = FALSE;             /* enable parse warnings */
BOOL _bAutoMemvarAssume = FALSE;     /* holds if undeclared variables are automatically assumed MEMVAR */
BOOL _bForceMemvars = FALSE;         /* holds if memvars are assumed when accesing undeclared variable */
BOOL _bDebugInfo = FALSE;            /* holds if generate debugger required info */

/* This variable is used to flag if variables have to be passed by reference
 * - it is required in DO <proc> WITH <params> statement
 * For example:
 * DO proces WITH aVar, bVar:=cVar
 *  aVar - have to be passed by reference
 *  bVar and cBar - have to be passed by value
 */
BOOL _bForceByRefer = FALSE;
/* This variable is true if the right value of assignment will be build.
 * It is used to temporarily cancel the above _bForceByRefer
 */
BOOL _bRValue       = FALSE;

WORD _wSeqCounter   = 0;
WORD _wForCounter   = 0;
WORD _wIfCounter    = 0;
WORD _wWhileCounter = 0;
WORD _wCaseCounter  = 0;
LONG _lMessageFix   = 0;  /* Position of the message which needs to be changed */
#ifdef HARBOUR_OBJ_GENERATION
BOOL _bObj32 = FALSE;     /* generate OBJ 32 bits */
#endif
WORD _wStatics = 0;       /* number of defined statics variables on the PRG */
PEXTERN pExterns = 0;
PTR_LOOPEXIT pLoops = 0;
PATHNAMES *_pIncludePath = NULL;
FILENAME *_pFileName =NULL;
ALIASID_PTR pAliasId = NULL;

PSTACK_VAL_TYPE pStackValType = 0; /* compile time stack values linked list */
char cVarType = ' ';               /* current declared variable type */

#define LOOKUP 0
extern int _iState;     /* current parser state (defined in harbour.l */
%}

%union                  /* special structure used by lex and yacc to share info */
{
   char * string;       /* to hold a string returned by lex */
   int    iNumber;      /* to hold a number returned by lex */
   long   lNumber;      /* to hold a long number returned by lex */
   struct
   {
      double dNumber;   /* to hold a double number returned by lex */
      /* NOTE: Intentionally using "unsigned char" instead of "BYTE" */
      unsigned char bDec; /* to hold the number of decimal points in the value */
   } dNum;
   void * pVoid;        /* to hold any memory structure we may need */
};

%token FUNCTION PROCEDURE IDENTIFIER RETURN NIL DOUBLE INASSIGN INTEGER INTLONG
%token LOCAL STATIC IIF IF ELSE ELSEIF END ENDIF LITERAL TRUEVALUE FALSEVALUE
%token EXTERN INIT EXIT AND OR NOT PUBLIC EQ NE1 NE2
%token INC DEC ALIAS DOCASE CASE OTHERWISE ENDCASE ENDDO MEMVAR
%token WHILE EXIT LOOP END FOR NEXT TO STEP LE GE FIELD IN PARAMETERS
%token PLUSEQ MINUSEQ MULTEQ DIVEQ POWER EXPEQ MODEQ EXITLOOP
%token PRIVATE BEGINSEQ BREAK RECOVER USING DO WITH SELF LINE
%token AS_NUMERIC AS_CHARACTER AS_LOGICAL AS_DATE AS_ARRAY AS_BLOCK AS_OBJECT DECLARE_FUN

/*the lowest precedence*/
/*postincrement and postdecrement*/
%left  POST
/*assigment - from right to left*/
%right INASSIGN
%left  PLUSEQ MINUSEQ
%left  MULTEQ DIVEQ MODEQ
%left  EXPEQ
/*logical operators*/
%left  OR
%left  AND
%left  NOT
/*relational operators*/
%left  '<' '>' EQ NE1 NE2 LE GE '$'
/*mathematical operators*/
%left  '+' '-'
%left  '*' '/' '%'
%left  POWER
%left  UNARY
/*preincrement and predecrement*/
%left  PRE
/*special operators*/
%left  ALIAS '&' '@' ')'
%right '\n' ';' ',' '='
/*the highest precedence*/

%type <string>  IDENTIFIER LITERAL FunStart MethStart IdSend ObjectData AliasVar
%type <dNum>    DOUBLE
%type <iNumber> ArgList ElemList PareExpList ExpList FunCall FunScope IncDec
%type <iNumber> Params ParamList Logical
%type <iNumber> INTEGER BlockExpList Argument IfBegin VarId VarList MethParams ObjFunCall
%type <iNumber> MethCall BlockList FieldList DoArgList VarAt
%type <lNumber> INTLONG WhileBegin BlockBegin
%type <pVoid>   IfElseIf Cases

%%

Main       : { Line(); } Source       {
                                         FixReturns();       /* fix all previous function returns offsets */
                                         if( ! _bQuiet ) printf( "\nsyntax ok\n" );
                                      }

Source     : Crlf
           | VarDefs
           | FieldsDef
           | MemvarDef
           | Function
           | Statement
           | Line
           | Source Crlf
           | Source Function
           | Source { LineBody(); } Statement
           | Source VarDefs
           | Source FieldsDef
           | Source MemvarDef
           | Source Line
           ;

Line       : LINE INTEGER LITERAL Crlf
           | LINE INTEGER LITERAL '@' LITERAL Crlf   /* XBase++ style */
           ;

Function   : FunScope FUNCTION  IDENTIFIER { cVarType = ' '; FunDef( $3, $1, 0 ); } Params Crlf {}
           | FunScope PROCEDURE IDENTIFIER { cVarType = ' '; FunDef( $3, $1, FUN_PROCEDURE ); } Params Crlf {}
           | FunScope DECLARE_FUN IDENTIFIER Params              Crlf { cVarType = ' '; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_NUMERIC   Crlf { cVarType = 'N'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_CHARACTER Crlf { cVarType = 'C'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_DATE      Crlf { cVarType = 'D'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_LOGICAL   Crlf { cVarType = 'L'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_ARRAY     Crlf { cVarType = 'A'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_OBJECT    Crlf { cVarType = 'O'; AddSymbol( $3, NULL ); }
           | FunScope DECLARE_FUN IDENTIFIER Params AS_BLOCK     Crlf { cVarType = 'B'; AddSymbol( $3, NULL ); }
           ;

FunScope   :                  { $$ = FS_PUBLIC; }
           | STATIC           { $$ = FS_STATIC; }
           | INIT             { $$ = FS_INIT; }
           | EXIT             { $$ = FS_EXIT; }
           ;

Params     :                                               { $$ = 0; }
           | '(' ')'                                       { $$ = 0; }
           | '(' { iVarScope = VS_PARAMETER; } ParamList ')'   { $$ = $3; }
           ;

ParamList  : IDENTIFIER                    { cVarType = ' '; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_NUMERIC         { cVarType = 'N'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_CHARACTER       { cVarType = 'C'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_DATE            { cVarType = 'D'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_LOGICAL         { cVarType = 'L'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_ARRAY           { cVarType = 'A'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_BLOCK           { cVarType = 'B'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_OBJECT          { cVarType = 'O'; AddVar( $1 ); $$ = 1; }
           | ParamList ',' IDENTIFIER      { AddVar( $3 ); $$++; }
           ;

Statements : Statement
           | Statements { Line(); } Statement
           ;

Statement  : ExecFlow Crlf        {}
           | FunCall Crlf         { Do( $1 ); }
           | AliasFunc Crlf       {}
           | IfInline Crlf        { GenPCode1( HB_P_POP ); }
           | ObjectMethod Crlf    { GenPCode1( HB_P_POP ); }
           | VarUnary Crlf        { GenPCode1( HB_P_POP ); }
           | VarAssign Crlf       { GenPCode1( HB_P_POP ); _bRValue =FALSE; }

           | IDENTIFIER '=' Expression Crlf            { PopId( $1 ); }
           | AliasVar '=' { $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression Crlf  { pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); AliasRemove(); }
           | AliasFunc '=' Expression Crlf             { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | VarAt '=' Expression Crlf                 { GenPCode1( HB_P_ARRAYPUT ); GenPCode1( HB_P_POP ); }
           | FunCallArray '=' Expression Crlf          { GenPCode1( HB_P_ARRAYPUT ); GenPCode1( HB_P_POP ); }
           | ObjectData '=' { MessageFix( SetData( $1 ) ); } Expression Crlf { Function( 1 ); GenPCode1( HB_P_POP ); }
           | ObjectData ArrayIndex '=' Expression Crlf    { GenPCode1( HB_P_ARRAYPUT ); GenPCode1( HB_P_POP ); }
           | ObjectMethod ArrayIndex '=' Expression Crlf  { GenPCode1( HB_P_ARRAYPUT ); GenPCode1( HB_P_POP ); }

           | BREAK Crlf
           | BREAK Expression Crlf
           | RETURN Crlf              { GenPCode1( HB_P_ENDPROC ); }
           | RETURN Expression Crlf   { GenPCode1( HB_P_RETVALUE ); GenPCode1( HB_P_ENDPROC ); }
           | PUBLIC { iVarScope = VS_PUBLIC; } VarList Crlf
           | PRIVATE { iVarScope = VS_PRIVATE; } VarList Crlf

           | EXITLOOP Crlf            { LoopExit(); }
           | LOOP Crlf                { LoopLoop(); }
           | DoProc Crlf
           | EXTERN ExtList Crlf
           ;

ExtList    : IDENTIFIER                               { AddExtern( $1 ); }
           | ExtList ',' IDENTIFIER                   { AddExtern( $3 ); }
           ;

FunCall    : FunStart ')'                { $$=0; CheckArgs( $1, $$ ); }
           | FunStart ArgList ')'        { $$=$2; CheckArgs( $1, $$ ); }
           ;

FunStart   : IDENTIFIER '('              { StaticAssign(); PushFunCall( $1 ); $$ = $1; }
           ;

MethCall   : MethStart ')'               { $$ = 0; }
           | MethStart ArgList ')'       { $$ = $2; }
           ;

MethStart  : IDENTIFIER '('              { StaticAssign(); Message( $1 ); $$ = $1; }
           ;

ArgList    : ','                               { PushNil(); PushNil(); $$ = 2; }
           | Argument                          { $$ = 1; }
           | ArgList ','                       { PushNil(); $$++; }
           | ArgList ',' Argument              { $$++; }
           | ','                               { PushNil(); } Argument { $$ = 2; }
           ;

Argument   : Expression                        {}
           | '@' IDENTIFIER                    { PushIdByRef( $2 ); }
           | '@' IDENTIFIER '(' ')'            { PushSymbol( $2, 1 ); GenPCode1( HB_P_FUNCPTR ); }
           ;

MethParams : /* empty */                       { $$ = 0; }
           | ArgList                           { $$ = $1; }
           ;

ObjectData : IdSend IDENTIFIER                     { $$ = $2; _lMessageFix = functions.pLast->lPCodePos; Message( $2 ); Function( 0 ); }
           | VarAt ':' IDENTIFIER                  { GenPCode1( HB_P_ARRAYAT ); $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjFunCall IDENTIFIER                 { $$ = $2; _lMessageFix = functions.pLast->lPCodePos; Message( $2 ); Function( 0 ); }
           | ObjFunArray  ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectMethod ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectData   ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectData ArrayIndex ':' IDENTIFIER  { GenPCode1( HB_P_ARRAYAT ); $$ = $4; _lMessageFix = functions.pLast->lPCodePos; Message( $4 ); Function( 0 ); }
           ;

ObjectMethod : IdSend IDENTIFIER { Message( $2 ); } '(' MethParams ')' { Function( $5 ); }
           | VarAt ':' MethCall { Function( $3 ); GenPCode1( HB_P_ARRAYAT ); }
           | ObjFunCall MethCall                   { Function( $2 ); }
           | ObjFunArray  ':' MethCall             { Function( $3 ); }
           | ObjectData   ':' MethCall             { Function( $3 ); }
           | ObjectData ArrayIndex ':' MethCall { Function( $4 ); { GenPCode1( HB_P_ARRAYAT ); } }
           | ObjectMethod ':' MethCall             { Function( $3 ); }
           ;

IdSend     : IDENTIFIER ':'                       { PushId( $1 ); $$ = $1; }
           ;

ObjFunCall : FunCall ':'                      { Function( $1 ); $$ = $1; }
           ;

FunCallArray : FunCall { Function( $1 ); } ArrayIndex
           ;

ObjFunArray : FunCallArray ':' { GenPCode1( HB_P_ARRAYAT ); }
           ;

Expression : NIL                              { PushNil(); }
           | DOUBLE                           { PushDouble( $1.dNumber,$1.bDec ); }
           | INTEGER                          { PushInteger( $1 ); }
           | INTLONG                          { PushLong( $1 ); }
           | LITERAL                          { PushString( $1 ); }
           | Variable
           | VarUnary
           | Logical                          { PushLogical( $1 ); }
           | Operators                        {}
           | FunCall                          { Function( $1 ); }
           | IfInline                         {}
           | Array                            {}
           | CodeBlock                        {}
           | ObjectMethod                     {}
           | Macro                            {}
           | AliasVar                         { PushId( $1 ); AliasRemove(); }
           | AliasFunc                        {}
           | PareExpList                      {}
           | SELF                             { GenPCode1( HB_P_PUSHSELF ); }
           ;

IfInline   : IIF '(' Expression ',' { $<iNumber>$ = JumpFalse( 0 ); }
                IfInlExp ',' { $<iNumber>$ = Jump( 0 ); JumpHere( $<iNumber>5 ); }
                IfInlExp ')' { JumpHere( $<iNumber>8 );
                if( _bWarnings )
                {
                  PSTACK_VAL_TYPE pFree;

                  if( pStackValType )
                  {
                    pFree = pStackValType;
                    debug_msg( "\n***---IIF()\n", NULL );

                    pStackValType = pStackValType->pPrev;
                    OurFree( (void *)pFree );
                  }
                  else
                    debug_msg( "\n***IIF() Compile time stack overflow\n", NULL );
                }
             }

           | IF '(' Expression ',' { $<iNumber>$ = JumpFalse( 0 ); }
                IfInlExp ',' { $<iNumber>$ = Jump( 0 ); JumpHere( $<iNumber>5 ); }
                IfInlExp ')' {  JumpHere( $<iNumber>8 );

                if( _bWarnings )
                {
                    PSTACK_VAL_TYPE pFree;

                    if( pStackValType )
                    {
                      pFree = pStackValType;
                      debug_msg( "\n***---IIF()\n", NULL );

                      pStackValType = pStackValType->pPrev;
                      OurFree( (void *)pFree );
                    }
                    else
                      debug_msg( "\n***IIF() Compile time stack overflow\n", NULL );
                }
              }
           ;

IfInlExp   : /* nothing => nil */            { PushNil(); }
           | Expression
           ;

Macro      : '&' Variable
           | '&' '(' Expression ')'
           ;

AliasVar   : INTEGER ALIAS { AliasAddInt( $1 ); } IDENTIFIER  { $$ = $4; }
           | IDENTIFIER ALIAS { AliasAddStr( $1 ); } IDENTIFIER  { $$ = $4; }
           | PareExpList ALIAS { AliasAddExp(); } IDENTIFIER  { $$ = $4; }
           ;

/* NOTE: In the case:
 * alias->( Expression )
 * alias always selects a workarea even if it is MEMVAR or M
 */
AliasFunc  : INTEGER ALIAS { AliasPush(); PushInteger( $1 ); AliasPop(); } PareExpList { AliasSwap(); }
           | IDENTIFIER ALIAS { AliasPush(); PushSymbol( $1, 0 ); AliasPop(); } PareExpList   { AliasSwap(); }
           | PareExpList ALIAS { AliasPush(); AliasSwap(); } PareExpList  { AliasSwap(); }
           ;

VarUnary   : IDENTIFIER IncDec %prec POST    { PushId( $1 ); Duplicate(); $2 ? Inc(): Dec(); PopId( $1 ); }
           | IncDec IDENTIFIER %prec PRE     { PushId( $2 ); $1 ? Inc(): Dec(); Duplicate(); PopId( $2 ); }
           | VarAt IncDec %prec POST { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); $2 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); $2 ? Dec(): Inc(); }
           | IncDec VarAt %prec PRE  { DupPCode( $2 ); GenPCode1( HB_P_ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray IncDec %prec POST { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $2 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); $2 ? Dec(): Inc(); }
           | IncDec FunCallArray %prec PRE  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData IncDec %prec POST   { MessageDupl( SetData( $1 ) ); Function( 0 ); $2 ? Inc(): Dec(); Function( 1 ); $2 ? Dec(): Inc(); }
           | IncDec ObjectData %prec PRE    { MessageDupl( SetData( $2 ) ); Function( 0 ); $1 ? Inc(): Dec(); Function( 1 ); }
           | ObjectData ArrayIndex IncDec %prec POST { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $3 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); $3 ? Dec(): Inc(); }
           | IncDec ObjectData ArrayIndex %prec PRE  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex IncDec %prec POST { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $3 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); $3 ? Dec(): Inc(); }
           | IncDec ObjectMethod ArrayIndex %prec PRE  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( HB_P_ARRAYPUT ); }
           | AliasVar IncDec %prec POST    { PushId( $1 ); Duplicate(); $2 ? Inc(): Dec(); PopId( $1 ); AliasRemove(); }
           | IncDec AliasVar %prec PRE     { PushId( $2 ); $1 ? Inc(): Dec(); Duplicate(); PopId( $2 ); AliasRemove(); }
           ;

IncDec     : INC                             { $$ = 1; }
           | DEC                             { $$ = 0; }
           ;

Variable   : VarId                     {}
           | VarAt                     { GenPCode1( HB_P_ARRAYAT ); }
           | FunCallArray              { GenPCode1( HB_P_ARRAYAT ); }
           | ObjectData                {}
           | ObjectData ArrayIndex     { GenPCode1( HB_P_ARRAYAT ); }
           | ObjectMethod ArrayIndex   { GenPCode1( HB_P_ARRAYAT ); }
           ;

VarId      : IDENTIFIER        { $$ = functions.pLast->lPCodePos;
                                 if( _bForceByRefer && functions.pLast->szName && ! _bRValue )
                                    /* DO .. WITH uses reference to a variable
                                     * if not inside a codeblock
                                     */
                                    PushIdByRef( $1 );
                                 else
                                    PushId( $1 );
                               }
           ;

VarAt      : IDENTIFIER { $<iNumber>$ = functions.pLast->lPCodePos; PushId( $1 ); } ArrayIndex { $$ =$<iNumber>2;  }
           ;

ArrayIndex : '[' IndexList ']'
           | ArrayIndex { GenPCode1( HB_P_ARRAYAT ); } '[' IndexList ']'
           ;

IndexList  : Expression
           | IndexList { GenPCode1( HB_P_ARRAYAT ); } ',' Expression
           ;

/*NOTE: If _bRValue is TRUE then the expression is on the right side of assignment
 * operator (or +=, -= ...) - in this case a variable is not pushed by
 * a reference it is a part of DO <proc> WITH ... statement
 */
VarAssign  : IDENTIFIER INASSIGN { _bRValue = TRUE; } Expression { PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER PLUSEQ   { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_PLUS    ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MINUSEQ  { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MINUS   ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MULTEQ   { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MULT    ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER DIVEQ    { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_DIVIDE  ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER EXPEQ    { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_POWER   ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MODEQ    { PushId( $1 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MODULUS ); PopId( $1 ); PushId( $1 ); }
           | VarAt INASSIGN { _bRValue = TRUE; } Expression { GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt PLUSEQ   { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_PLUS    ); GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt MINUSEQ  { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MINUS   ); GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt MULTEQ   { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MULT    ); GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt DIVEQ    { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_DIVIDE  ); GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt EXPEQ    { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_POWER   ); GenPCode1( HB_P_ARRAYPUT ); }
           | VarAt MODEQ    { DupPCode( $1 ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MODULUS ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray INASSIGN { _bRValue = TRUE; } Expression { GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray PLUSEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_PLUS    ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray MINUSEQ  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_MINUS   ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray MULTEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_MULT    ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray DIVEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_DIVIDE  ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray EXPEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_POWER   ); GenPCode1( HB_P_ARRAYPUT ); }
           | FunCallArray MODEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; }  Expression { GenPCode1( HB_P_MODULUS ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData INASSIGN { MessageFix ( SetData( $1 ) ); _bRValue = TRUE; } Expression { Function( 1 ); }
           | ObjectData PLUSEQ   { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_PLUS );    Function( 1 ); }
           | ObjectData MINUSEQ  { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MINUS );   Function( 1 ); }
           | ObjectData MULTEQ   { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MULT );    Function( 1 ); }
           | ObjectData DIVEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_DIVIDE );  Function( 1 ); }
           | ObjectData EXPEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_POWER );   Function( 1 ); }
           | ObjectData MODEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MODULUS ); Function( 1 ); }
           | ObjectData ArrayIndex INASSIGN { _bRValue = TRUE; } Expression      { GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex PLUSEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_PLUS    ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex MINUSEQ  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MINUS   ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex MULTEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MULT    ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex DIVEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_DIVIDE  ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex EXPEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_POWER   ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectData ArrayIndex MODEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MODULUS ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex INASSIGN { _bRValue = TRUE; } Expression    { GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex PLUSEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_PLUS    ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex MINUSEQ  { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MINUS   ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex MULTEQ   { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MULT    ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex DIVEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_DIVIDE  ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex EXPEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_POWER   ); GenPCode1( HB_P_ARRAYPUT ); }
           | ObjectMethod ArrayIndex MODEQ    { GenPCode1( HB_P_DUPLTWO ); GenPCode1( HB_P_ARRAYAT ); _bRValue = TRUE; } Expression { GenPCode1( HB_P_MODULUS ); GenPCode1( HB_P_ARRAYPUT ); }
           | AliasVar INASSIGN { _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar PLUSEQ   { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_PLUS    ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar MINUSEQ  { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_MINUS   ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar MULTEQ   { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_MULT    ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar DIVEQ    { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_DIVIDE  ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar EXPEQ    { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_POWER   ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasVar MODEQ    { PushId( $1 ); _bRValue = TRUE; $<pVoid>$=(void*)pAliasId; pAliasId=NULL; } Expression { GenPCode1( HB_P_MODULUS ); pAliasId=(ALIASID_PTR) $<pVoid>3; PopId( $1 ); PushId( $1 ); AliasRemove(); }
           | AliasFunc INASSIGN Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc PLUSEQ   Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc MINUSEQ  Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc MULTEQ   Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc DIVEQ    Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc EXPEQ    Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           | AliasFunc MODEQ    Expression { --iLine; GenError( _szCErrors, 'E', ERR_INVALID_LVALUE, NULL, NULL ); }
           ;


Operators  : Expression '='    Expression   { GenPCode1( HB_P_EQUAL ); } /* compare */
           | Expression '+'    Expression   { GenPCode1( HB_P_PLUS ); }
           | Expression '-'    Expression   { GenPCode1( HB_P_MINUS ); }
           | Expression '*'    Expression   { GenPCode1( HB_P_MULT ); }
           | Expression '/'    Expression   { GenPCode1( HB_P_DIVIDE ); }
           | Expression '<'    Expression   { GenPCode1( HB_P_LESS ); }
           | Expression '>'    Expression   { GenPCode1( HB_P_GREATER ); }
           | Expression '$'    Expression   { GenPCode1( HB_P_INSTRING ); }
           | Expression '%'    Expression   { GenPCode1( HB_P_MODULUS ); }
           | Expression LE     Expression   { GenPCode1( HB_P_LESSEQUAL ); }
           | Expression GE     Expression   { GenPCode1( HB_P_GREATEREQUAL ); }
           | Expression AND { if( _bShortCuts ){ Duplicate(); $<iNumber>$ = JumpFalse( 0 ); } }
                       Expression { GenPCode1( HB_P_AND ); if( _bShortCuts ) JumpHere( $<iNumber>3 ); }
           | Expression OR { if( _bShortCuts ){ Duplicate(); $<iNumber>$ = JumpTrue( 0 ); } }
                       Expression { GenPCode1( HB_P_OR ); if( _bShortCuts ) JumpHere( $<iNumber>3 ); }
           | Expression EQ     Expression   { GenPCode1( HB_P_EXACTLYEQUAL ); }
           | Expression NE1    Expression   { GenPCode1( HB_P_NOTEQUAL ); }
           | Expression NE2    Expression   { GenPCode1( HB_P_NOTEQUAL ); }
           | Expression POWER  Expression   { GenPCode1( HB_P_POWER ); }
           | NOT Expression                 { GenPCode1( HB_P_NOT ); }
           | '-' Expression %prec UNARY     { GenPCode1( HB_P_NEGATE ); }
           | '+' Expression %prec UNARY
           | VarAssign                      { _bRValue = FALSE; }
           ;

Logical    : TRUEVALUE                                   { $$ = 1; }
           | FALSEVALUE                                  { $$ = 0; }
           ;

Array      : '{' ElemList '}'                       { GenArray( $2 ); }
           ;

ElemList   : /*empty array*/                        { $$ = 0; }
           | Expression                             { $$ = 1; }
           | ElemList ','                           { if( $$ == 0 ) {
                                                         PushNil();
                                                         PushNil();
                                                         $$ = 2;
                                                       } else {
                                                          PushNil();
                                                          $$++;
                                                       } }
           | ElemList ',' Expression                { if( $$ == 0 )
                                                      {
                                                         PushNil();
                                                         $$ = 2;
                                                       }
                                                       else
                                                          $$++; }
           ;

CodeBlock  : BlockBegin '|' BlockExpList '}'           { CodeBlockEnd(); }
           | BlockBegin BlockList '|' BlockExpList '}' { CodeBlockEnd(); }
           ;

BlockBegin : '{' '|'  { CodeBlockStart(); }
           ;

BlockExpList : Expression                            { $$ = 1; }
           | ','                            { PushNil(); GenPCode1( HB_P_POP ); PushNil(); $$ = 2; }
           | BlockExpList ','                        { GenPCode1( HB_P_POP ); PushNil(); $$++; }
           | BlockExpList ',' { GenPCode1( HB_P_POP ); } Expression  { $$++; }
           ;

BlockList  : IDENTIFIER                            { cVarType = ' '; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_NUMERIC                 { cVarType = 'N'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_CHARACTER               { cVarType = 'C'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_DATE                    { cVarType = 'D'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_LOGICAL                 { cVarType = 'L'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_ARRAY                   { cVarType = 'A'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_BLOCK                   { cVarType = 'B'; AddVar( $1 ); $$ = 1; }
           | IDENTIFIER AS_OBJECT                  { cVarType = 'O'; AddVar( $1 ); $$ = 1; }
           | BlockList ',' IDENTIFIER             { AddVar( $3 ); $$++; }
           ;

PareExpList: '(' ExpList ')'        { $$ = $2; }
           ;

ExpList    : Expression %prec POST                    { $$ = 1; }
           | ExpList { GenPCode1( HB_P_POP ); } ',' Expression %prec POST  { $$++; }
           ;

VarDefs    : LOCAL { iVarScope = VS_LOCAL; Line(); } VarList Crlf { cVarType = ' '; }
           | STATIC { StaticDefStart() } VarList Crlf { StaticDefEnd( $<iNumber>3 ); }
           | PARAMETERS { if( functions.pLast->bFlags & FUN_USES_LOCAL_PARAMS )
                             GenError( _szCErrors, 'E', ERR_PARAMETERS_NOT_ALLOWED, NULL, NULL );
                          else
                             functions.pLast->wParamNum=0; iVarScope = (VS_PRIVATE | VS_PARAMETER); }
                             MemvarList Crlf
           ;

VarList    : VarDef                                  { $$ = 1; }
           | VarList ',' VarDef                      { $$++; }
           ;

VarDef     : IDENTIFIER                                   { cVarType = ' '; AddVar( $1 ); }
           | IDENTIFIER AS_NUMERIC                        { cVarType = 'N'; AddVar( $1 ); }
           | IDENTIFIER AS_CHARACTER                      { cVarType = 'C'; AddVar( $1 ); }
           | IDENTIFIER AS_LOGICAL                        { cVarType = 'L'; AddVar( $1 ); }
           | IDENTIFIER AS_DATE                           { cVarType = 'D'; AddVar( $1 ); }
           | IDENTIFIER AS_ARRAY                          { cVarType = 'A'; AddVar( $1 ); }
           | IDENTIFIER AS_BLOCK                          { cVarType = 'B'; AddVar( $1 ); }
           | IDENTIFIER AS_OBJECT                         { cVarType = 'O'; AddVar( $1 ); }
           | IDENTIFIER INASSIGN Expression               { cVarType = ' '; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_NUMERIC   INASSIGN Expression  { cVarType = 'N'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_CHARACTER INASSIGN Expression  { cVarType = 'C'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_LOGICAL   INASSIGN Expression  { cVarType = 'L'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_DATE      INASSIGN Expression  { cVarType = 'D'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_ARRAY     INASSIGN Expression  { cVarType = 'A'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_BLOCK     INASSIGN Expression  { cVarType = 'B'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER AS_OBJECT    INASSIGN Expression  { cVarType = 'O'; AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER '[' ExpList ']'                   { cVarType = ' '; AddVar( $1 ); DimArray( $3 ); PopId( $1 ); }
           | IDENTIFIER '[' ExpList ']' AS_ARRAY          { cVarType = 'A'; AddVar( $1 ); DimArray( $3 ); PopId( $1 ); }
           ;

FieldsDef  : FIELD { iVarScope =VS_FIELD; } FieldList Crlf
           ;

FieldList  : IDENTIFIER                            { cVarType = ' '; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_NUMERIC                 { cVarType = 'N'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_CHARACTER               { cVarType = 'C'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_DATE                    { cVarType = 'D'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_LOGICAL                 { cVarType = 'L'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_ARRAY                   { cVarType = 'A'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_BLOCK                   { cVarType = 'B'; $$=FieldsCount(); AddVar( $1 ); }
           | IDENTIFIER AS_OBJECT                  { cVarType = 'O'; $$=FieldsCount(); AddVar( $1 ); }
           | FieldList ',' IDENTIFIER                { AddVar( $3 ); }
           | FieldList IN IDENTIFIER { FieldsSetAlias( $3, $<iNumber>1 ); }
           ;

MemvarDef  : MEMVAR { iVarScope = VS_MEMVAR; } MemvarList Crlf
           ;

MemvarList : IDENTIFIER                            { AddVar( $1 ); }
           | MemvarList ',' IDENTIFIER             { AddVar( $3 ); }
           ;

ExecFlow   : IfEndif
           | DoCase
           | DoWhile
           | ForNext
           | BeginSeq
           ;

IfEndif    : IfBegin EndIf                    { JumpHere( $1 ); }
           | IfBegin IfElse EndIf             { JumpHere( $1 ); }
           | IfBegin IfElseIf EndIf           { JumpHere( $1 ); FixElseIfs( $2 ); }
           | IfBegin IfElseIf IfElse EndIf    { JumpHere( $1 ); FixElseIfs( $2 ); }
           ;

IfBegin    : IF Expression { ++_wIfCounter; } Crlf { $$ = JumpFalse( 0 ); }
                IfStats
                { $$ = Jump( 0 ); JumpHere( $<iNumber>5 ); }
           ;

IfElse     : ELSE Crlf IfStats
           ;

IfElseIf   : ELSEIF Expression Crlf { $<iNumber>$ = JumpFalse( 0 ); }
                IfStats { $$ = GenElseIf( 0, Jump( 0 ) ); JumpHere( $<iNumber>4 ); }

           | IfElseIf ELSEIF Expression Crlf { $<iNumber>$ = JumpFalse( 0 ); }
                IfStats { $$ = GenElseIf( $1, Jump( 0 ) ); JumpHere( $<iNumber>5 ); }
           ;

EndIf      : ENDIF                 { --_wIfCounter; }
           | END                   { --_wIfCounter; }
           ;

IfStats    : /* no statements */
           | Statements
           ;

DoCase     : DoCaseBegin
                Cases
             EndCase                  { FixElseIfs( $2 ); }

           | DoCaseBegin
                Otherwise
             EndCase

           | DoCaseBegin
             EndCase

           | DoCaseBegin
                Cases
                Otherwise
             EndCase                   { FixElseIfs( $2 ); }
           ;

EndCase    : ENDCASE              { --_wCaseCounter; }
           | END                  { --_wCaseCounter; }
           ;

DoCaseBegin : DOCASE { ++_wCaseCounter; } Crlf
           ;

Cases      : CASE Expression Crlf { $<iNumber>$ = JumpFalse( 0 ); Line(); } CaseStmts { $$ = GenElseIf( 0, Jump( 0 ) ); JumpHere( $<iNumber>4 ); Line(); }
           | Cases CASE Expression Crlf { $<iNumber>$ = JumpFalse( 0 ); Line(); } CaseStmts { $$ = GenElseIf( $1, Jump( 0 ) ); JumpHere( $<iNumber>5 ); Line(); }
           ;

Otherwise  : OTHERWISE Crlf CaseStmts
           ;

CaseStmts  : /* no statements */
           | Statements
           ;

DoWhile    : WhileBegin Expression Crlf { $<lNumber>$ = JumpFalse( 0 ); }
                { Jump( $1 - functions.pLast->lPCodePos ); }
             EndWhile { JumpHere( $<lNumber>4 ); --_wWhileCounter; }

           | WhileBegin Expression Crlf { $<lNumber>$ = JumpFalse( 0 ); Line(); }
                WhileStatements { LoopHere(); Jump( $1 - functions.pLast->lPCodePos ); }
             EndWhile  { JumpHere( $<lNumber>4 ); --_wWhileCounter; LoopEnd(); }
           ;

WhileBegin : WHILE    { $$ = functions.pLast->lPCodePos; ++_wWhileCounter; LoopStart(); }
           ;

WhileStatements : Statement
           | WhileStatements Statement        { Line(); }
           ;

EndWhile   : END
           | ENDDO
           ;

ForNext    : FOR IDENTIFIER ForAssign Expression { PopId( $2 ); $<iNumber>$ = functions.pLast->lPCodePos; ++_wForCounter; LoopStart(); }
             TO Expression                       { PushId( $2 ); }
             StepExpr Crlf                       { GenPCode1( HB_P_FORTEST ); $<iNumber>$ = JumpTrue( 0 ); }
             ForStatements                       { LoopHere(); PushId( $2 ); GenPCode1( HB_P_PLUS ); PopId( $2 ); Jump( $<iNumber>5 - functions.pLast->lPCodePos ); JumpHere( $<iNumber>11 ); LoopEnd(); }
           ;

ForAssign  : '='
           | INASSIGN
           ;

StepExpr   : /* default step expression */       { PushInteger( 1 ); }
           | STEP Expression
           ;

ForStatements : ForStat NEXT                     { --_wForCounter; }
           | ForStat NEXT IDENTIFIER             { --_wForCounter; }
           | NEXT                                { --_wForCounter; }
           | NEXT IDENTIFIER                     { --_wForCounter; }
           ;

ForStat    : Statements                          { Line(); }
           ;

BeginSeq   : BEGINSEQ { ++_wSeqCounter; } Crlf
                SeqStatms
                RecoverSeq
             END           { --_wSeqCounter; }
           ;

SeqStatms  : /* empty */
           | Statements
           ;

RecoverSeq : /* no recover */
           | RecoverEmpty Crlf
           | RecoverEmpty Crlf Statements
           | RecoverUsing Crlf
           | RecoverUsing Crlf Statements
           ;

RecoverEmpty : RECOVER
           ;

RecoverUsing : RECOVER USING IDENTIFIER
           ;

/* NOTE: In Clipper all variables used in DO .. WITH are passed by reference
 * however if they are part of an expression then they are passed by value
 * for example:
 * DO .. WITH ++variable
 * will pass the value of variable not a reference
 */
DoProc     : DO IDENTIFIER { PushSymbol( $2, 1 ); PushNil(); Do( 0 ); }
           | DO IDENTIFIER { PushSymbol( $2, 1 ); PushNil(); _bForceByRefer=TRUE; } WITH DoArgList { Do( $5 ); _bForceByRefer=FALSE; }
           | WHILE { PushSymbol( yy_strdup("WHILE"), 1 ); PushNil(); _bForceByRefer=TRUE; } WITH DoArgList { Do( $4 ); _bForceByRefer=FALSE; }
           ;

DoArgList  : ','                               { PushNil(); PushNil(); $$ = 2; }
           | DoExpression                      { $$ = 1; }
           | DoArgList ','                     { PushNil(); $$++; }
           | DoArgList ',' DoExpression        { $$++; }
           | ',' { PushNil(); } DoExpression   { $$ = 2; }
           ;

DoExpression: Expression         { _bForceByRefer=TRUE; }
           ;

Crlf       : '\n'
           | ';'
           | '\n' Crlf
           | ';' Crlf
           ;

%%

void yyerror( char * s )
{
   printf( "\n%s at line %i\n", s, iLine );
   exit( 1 );
}

void * GenElseIf( void * pFirst, WORD wOffset )
{
   PELSEIF pElseIf = ( PELSEIF ) OurMalloc( sizeof( _ELSEIF ) ), pLast;

   pElseIf->wOffset = wOffset;
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

void GenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 )
{
  char * szLine = ( char * ) OurMalloc( 160 );      /*2 lines of text */
  printf( "\r%s(%i) ", files.pLast->szFileName, iLine );
  printf( "Error %c%i  ", cPrefix, iError );
  sprintf( szLine, _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "%s\n\n", szLine );
  exit( 1 );
}

void GenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2)
{
    if( _bWarnings && iWarning < WARN_ASSIGN_SUSPECT ) /* TODO: add switch to set level */
    {
        char * szLine = ( char * ) OurMalloc( 160 );      /*2 lines of text */
        printf( "\r%s(%i) ", files.pLast->szFileName, iLine );
        printf( "Warning %c%i  ", cPrefix, iWarning );
        sprintf( szLine, _szWarnings[ iWarning - 1 ], szWarning1, szWarning2 );
        printf( "%s\n", szLine );
    }
}

void EXTERNAL_LINKAGE close_on_exit( void )
{
  PFILE pFile = files.pLast;

  while( pFile )
  {
    printf( "\nClosing file: %s\n", pFile->szFileName );
    fclose( pFile->handle );
    pFile = (PFILE) pFile->pPrev;
  }
}

int harbour_main( int argc, char * argv[] )
{
   int iStatus = 0, iArg = 1;
   char szFileName[ _POSIX_PATH_MAX ];    /* filename to parse */
   char szPpoName[ _POSIX_PATH_MAX ];
   char *szOutPath ="";

   if( argc > 1 )
   {
      Hbpp_init();  /* Initialization of preprocessor arrays */
      /* Command line options */
      while( iArg < argc )
      {
         if( IS_OPT_SEP(argv[ iArg ][ 0 ]))
         {
            switch( argv[ iArg ][ 1 ] )
            {
               case '1':
                    if( argv[ iArg ][ 2 ] == '0' )
                       _bRestrictSymbolLength = TRUE;
                    break;

               case 'a':
               case 'A':
                    _bAutoMemvarAssume = TRUE;
                    break;

               case 'b':
               case 'B':
                    _bDebugInfo = TRUE;
                    _bLineNumbers = TRUE;
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
                          szDefText[ i ] = 0;
                          AddDefine( szDefText, szDefText + i + 1 );
                       }
                       free( szDefText );
                    }
                    break;
#ifdef HARBOUR_OBJ_GENERATION
               case 'f':
               case 'F':
                    {
                       char * szUpper = yy_strupr( yy_strdup( &argv[ iArg ][ 2 ] ) );
                       if( ! strcmp( szUpper, "OBJ32" ) )
                          _bObj32 = TRUE;
                       free( szUpper );
                    }
                    break;
#endif
               case 'g':
               case 'G':
                    switch( argv[ iArg ][ 2 ] )
                    {
                       case 'c':
                       case 'C':
                            _iLanguage = LANG_C;
                            break;

                       case 'j':
                       case 'J':
                            _iLanguage = LANG_JAVA;
                            break;

                       case 'p':
                       case 'P':
                            _iLanguage = LANG_PASCAL;
                            break;

                       case 'r':
                       case 'R':
                            _iLanguage = LANG_RESOURCES;
                            break;

                       case 'h':
                       case 'H':
                            _iLanguage = LANG_PORT_OBJ;
                            break;

                       default:
                            printf( "\nUnsupported output language option\n" );
                            exit( 1 );
                    }
                    break;

               case 'i':
               case 'I':
                    AddSearchPath( argv[ iArg ]+2, &_pIncludePath );
                    break;

               case 'l':
               case 'L':
                    _bLineNumbers = FALSE;
                    break;

               case 'n':
               case 'N':
                    _bStartProc = FALSE;
                    break;

               case 'o':
               case 'O':
                    szOutPath = argv[ iArg ]+2;
                    break;

               /* Added for preprocessor needs */
               case 'p':
               case 'P':
                    lPpo = 1;
                    break;

               case 'q':
               case 'Q':
                    _bQuiet = TRUE;
                    break;

               case 's':
               case 'S':
                    _bSyntaxCheckOnly = TRUE;
                    break;

               case 'v':
               case 'V':
                    _bForceMemvars = TRUE;
                    break;

               case 'w':
               case 'W':
                    _bWarnings = TRUE;
                    break;

               case 'y':
               case 'Y':
                    yydebug = TRUE;
                    break;

               case 'z':
               case 'Z':
                    _bShortCuts = FALSE;
                    break;

               default:
                    printf( "Invalid command line option: %s\n",
                            &argv[ iArg ][ 0 ] );
                    break;
            }
         }
         else
            _pFileName =SplitFilename( argv[ iArg ] );
         iArg++;
      }

      if( !_bQuiet )
         printf( "Harbour compiler build %i%s (%04d.%02d.%02d)\n",
            hb_build, hb_revision, hb_year, hb_month, hb_day );

      if( _pFileName )
      {
        if( !_pFileName->extension )
          _pFileName->extension =".prg";
        MakeFilename( szFileName, _pFileName );
        if ( lPpo )
        {
          _pFileName->extension =".ppo";
          MakeFilename( szPpoName, _pFileName );
          yyppo = fopen ( szPpoName, "w" );
        }
      }
      else
      {
        PrintUsage( argv[ 0 ] );
        return iStatus;
      }

      files.iFiles     = 0;        /* initialize support variables */
      files.pLast      = 0;
      functions.iCount = 0;
      functions.pFirst = 0;
      functions.pLast  = 0;
      funcalls.iCount  = 0;
      funcalls.pFirst  = 0;
      funcalls.pLast   = 0;
      symbols.iCount   = 0;
      symbols.pFirst   = 0;
      symbols.pLast    = 0;

      _pInitFunc   =NULL;

      atexit( close_on_exit );

      if( Include( szFileName, NULL ) )
      {
         char * szInclude = getenv( "INCLUDE" );

         if( szInclude )
         {
          char * pPath;
          char * pDelim;

          pPath = szInclude = yy_strdup( szInclude );
          while( (pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR )) != NULL )
          {
            *pDelim = '\0';
            AddSearchPath( pPath, &_pIncludePath );
            pPath = pDelim + 1;
          }
          AddSearchPath( pPath, &_pIncludePath );
         }

         /* Generate the starting procedure frame
          */
         if( _bStartProc )
            FunDef( yy_strupr( yy_strdup( _pFileName->name ) ), FS_PUBLIC, FUN_PROCEDURE );
         else
             /* Don't pass the name of module if the code for starting procedure
             * will be not generated. The name cannot be placed as first symbol
             * because this symbol can be used as function call or memvar's name.
             */
            FunDef( yy_strupr( yy_strdup( "" ) ), FS_PUBLIC, FUN_PROCEDURE );

         yyparse();

         GenExterns();       /* generates EXTERN symbols names */
         fclose( yyin );
         files.pLast = NULL;

#ifdef HARBOUR_OBJ_GENERATION
         if( ! _bSyntaxCheckOnly && ! _bObj32 )
#else
         if( ! _bSyntaxCheckOnly )
#endif
         {
            if( _pInitFunc )
            {
              PCOMSYMBOL pSym;

              /* Fix the number of static variables */
              _pInitFunc->pCode[ 1 ] =LOBYTE( _wStatics );
              _pInitFunc->pCode[ 2 ] =HIBYTE( _wStatics );
              _pInitFunc->wStaticsBase =_wStatics;

              pSym = AddSymbol( _pInitFunc->szName, NULL );
              pSym->cScope |= _pInitFunc->cScope;
              functions.pLast->pNext = _pInitFunc;
              ++functions.iCount;
            }

            /* we create a the output file */
            _pFileName->path = szOutPath;
            switch( _iLanguage )
            {
               case LANG_C:
                    _pFileName->extension =".c";
                    MakeFilename( szFileName, _pFileName );
                    GenCCode( szFileName, _pFileName->name );
                    break;

               case LANG_JAVA:
                    _pFileName->extension =".java";
                    MakeFilename( szFileName, _pFileName );
                    GenJava( szFileName, _pFileName->name );
                    break;

               case LANG_PASCAL:
                    _pFileName->extension =".pas";
                    MakeFilename( szFileName, _pFileName );
                    GenPascal( szFileName, _pFileName->name );
                    break;

               case LANG_RESOURCES:
                    _pFileName->extension =".rc";
                    MakeFilename( szFileName, _pFileName );
                    GenRC( szFileName, _pFileName->name );
                    break;

               case LANG_PORT_OBJ:
                    _pFileName->extension =".hrb";
                    MakeFilename( szFileName, _pFileName );
                    GenPortObj( szFileName, _pFileName->name );
                    break;
            }
         }
#ifdef HARBOUR_OBJ_GENERATION
         if( _bObj32 )
         {
            _pFileName->extension = ".obj";
            MakeFilename( szFileName, _pFileName );
            GenObj32( szFileName, _pFileName->name );
         }
#endif
         if ( lPpo ) fclose ( yyppo );
      }
      else
      {
         printf( "Can't open input file: %s\n", szFileName );
         iStatus = 1;
      }
      OurFree( (void *) _pFileName );
   }
   else
      PrintUsage( argv[ 0 ] );

   return iStatus;
}

/*
 * Prints available options
*/
void PrintUsage( char * szSelf )
{
  printf( "Syntax: %s <file.prg> [options]\n"
          "\nOptions: \n"
          "\t/a\t\tautomatic memvar declaration\n"
          "\t/b\t\tdebug info\n"
          "\t/d<id>[=<val>]\t#define <id>\n"
#ifdef HARBOUR_OBJ_GENERATION
          "\t/f\t\tgenerated object file\n"
          "\t\t\t /fobj32 --> Windows/Dos 32 bits OBJ\n"
#endif
          "\t/g\t\tgenerated output language\n"
          "\t\t\t /gc (C default) --> <file.c>\n"
          "\t\t\t /gh (HRB file)  --> <file.hrb>\n"
          "\t\t\t /gj (Java)      --> <file.java>\n"
          "\t\t\t /gp (Pascal)    --> <file.pas>\n"
          "\t\t\t /gr (Resources) --> <file.rc>\n"
          "\t/i<path>\tadd #include file search path\n"
          "\t/l\t\tsuppress line number information\n"
          "\t/n\t\tno implicit starting procedure\n"
          "\t/o<path>\tobject file drive and/or path\n"
          "\t/p\t\tgenerate pre-processed output (.ppo) file\n"
          "\t/q\t\tquiet\n"
          "\t/s\t\tsyntax check only\n"
          "\t/v\t\tvariables are assumed M->\n"
          "\t/w\t\tenable warnings\n"
          "\t/y\t\ttrace lex & yacc activity\n"
          "\t/z\t\tsuppress shortcutting (.and. & .or.)\n"
          "\t/10\t\trestrict symbol length to 10 characters\n"
          , szSelf );
}

/*
 * Split given filename into path, name and extension
*/
FILENAME *SplitFilename( char *szFilename )
{
  FILENAME *pName =(FILENAME *)OurMalloc( sizeof(FILENAME) );
  int iLen = strlen(szFilename);
  int iSlashPos, iDotPos;
  int iPos;

  pName->path =pName->name =pName->extension =NULL;

  iSlashPos =iLen-1;
  iPos =0;
  while( iSlashPos >= 0 && !IS_PATH_SEP(szFilename[ iSlashPos ]) )
    --iSlashPos;
  if( iSlashPos == 0 )
  {
    /* root path ->  \filename */
    pName->_buffer[ 0 ] =PATH_DELIMITER[0];
    pName->_buffer[ 1 ] ='\x0';
    pName->path =pName->_buffer;
    iPos =2;  /* first free position after the slash */
  }
  else if( iSlashPos > 0 )
  {
    /* path with separator ->  path\filename */
    memcpy( pName->_buffer, szFilename, iSlashPos );
    pName->_buffer[ iSlashPos ] ='\x0';
    pName->path =pName->_buffer;
    iPos =iSlashPos +1;   /* first free position after the slash */
  }

  iDotPos =iLen-1;
  while( iDotPos > iSlashPos && szFilename[ iDotPos ] != '.' )
    --iDotPos;
  if( (iDotPos-iSlashPos) > 1 )
  {
    /* the dot was found
     * and there is at least one character between a slash and a dot
     */
    if( iDotPos == iLen-1 )
    {
      /* the dot is the last character -use it as extension name */
      pName->extension =pName->_buffer+iPos;
      pName->_buffer[ iPos++ ] ='.';
      pName->_buffer[ iPos++ ] ='\x0';
    }
    else
    {
      pName->extension =pName->_buffer+iPos;
      /* copy rest of the string with terminating ZERO character */
      memcpy( pName->extension, szFilename+iDotPos+1, iLen-iDotPos );
      iPos +=iLen-iDotPos;
    }
  }
  else
    /* there is no dot in the filename or it is  '.filename' */
    iDotPos =iLen;

  pName->name =pName->_buffer+iPos;
  memcpy( pName->name, szFilename+iSlashPos+1, iDotPos-iSlashPos-1 );
  pName->name[ iDotPos-iSlashPos-1 ] ='\x0';

  return pName;
}

/*
 * This function joins path, name and extension into a string with a filename
*/
char *MakeFilename( char *szFileName, FILENAME *pFileName )
{
#if 0
  fprintf(stderr, "path: |%s|\n"
                  "name: |%s|\n"
                  " ext: |%s|\n",
          pFileName->path, pFileName->name, pFileName->extension);
#endif

  if( pFileName->path && pFileName->path[ 0 ] )
  {
    /* we have not empty path specified */
    int iLen =strlen(pFileName->path);
    strcpy( szFileName, pFileName->path );
    /* if the path is a root directory then we don't need to add path separator */
    if( !(IS_PATH_SEP(pFileName->path[ 0 ]) && pFileName->path[ 0 ] == '\x0') )
    {
      /* add the path separator only in cases:
       *  when a name doesn't start with it
       *  when the path doesn't end with it
       */
      if( !( IS_PATH_SEP(pFileName->name[ 0 ]) || IS_PATH_SEP(pFileName->path[ iLen-1 ]) ) )
      {
        szFileName[ iLen++ ] =PATH_DELIMITER[0];
        szFileName[ iLen ] ='\x0';
      }
    }
    strcpy( szFileName+iLen, pFileName->name );
  }
  else
    strcpy( szFileName, pFileName->name );

  if( pFileName->extension )
  {
    int iLen =strlen(szFileName);

    if( !(pFileName->extension[ 0 ] == '.' || szFileName[ iLen-1 ] == '.') )
    {
      /* add extension separator only when extansion doesn't contain it */
      szFileName[ iLen++ ] ='.';
      szFileName[ iLen ]   ='\x0';
    }
    strcpy( szFileName+iLen, pFileName->extension );
  }

  return szFileName;
}

/*
 * Function that adds specified path to the list of pathnames to search
 */
void AddSearchPath( char *szPath, PATHNAMES * *pSearchList )
{
  PATHNAMES *pPath = *pSearchList;

  if( pPath )
  {
    while( pPath->pNext )
      pPath = pPath->pNext;
    pPath->pNext = ( PATHNAMES * ) OurMalloc( sizeof( PATHNAMES ) );
    pPath = pPath->pNext;
  }
  else
  {
    *pSearchList =pPath =(PATHNAMES *)OurMalloc( sizeof(PATHNAMES) );
  }
  pPath->pNext  = NULL;
  pPath->szPath = szPath;
}


/*
 * This function adds the name of called function into the list
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
PFUNCTION AddFunCall( char * szFunctionName )
{
   PFUNCTION pFunc = FunctionNew( szFunctionName, 0 );

   if( ! funcalls.iCount )
   {
      funcalls.pFirst = pFunc;
      funcalls.pLast  = pFunc;
   }
   else
   {
      ( ( PFUNCTION ) funcalls.pLast )->pNext = pFunc;
      funcalls.pLast = pFunc;
   }
   funcalls.iCount++;

   return pFunc;
}

/*
 * This function adds the name of external symbol into the list of externals
 * as they have to be placed on the symbol table later than the first
 * public symbol
 */
void AddExtern( char * szExternName ) /* defines a new extern name */
{
   PEXTERN pExtern = ( PEXTERN ) OurMalloc( sizeof( _EXTERN ) ), pLast;

   pExtern->szName = szExternName;
   pExtern->pNext  = 0;

   if( pExterns == 0 )
      pExterns = pExtern;
   else
   {
      pLast = pExterns;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pExtern;
   }
}

void AddVar( char * szVarName )
{
   PVAR pVar, pLastVar;
   PFUNCTION pFunc =functions.pLast;

   if( ! _bStartProc && functions.iCount <= 1 && iVarScope == VS_LOCAL )
   {
     /* Variable declaration is outside of function/procedure body.
        In this case only STATIC and PARAMETERS variables are allowed. */
      --iLine;
      GenError( _szCErrors, 'E', ERR_OUTSIDE, NULL, NULL );
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( (functions.pLast->bFlags & FUN_STATEMENTS) && !(iVarScope == VS_FIELD || (iVarScope & VS_MEMVAR)) )
   {
      --iLine;
      GenError( _szCErrors, 'E', ERR_FOLLOWS_EXEC, (iVarScope==VS_LOCAL?"LOCAL":"STATIC"), NULL );
   }

   /* When static variable is added then functions.pLast points to function
    * that will initialise variables. The function where variable is being
    * defined is stored in pOwner member.
    */
   if( iVarScope == VS_STATIC )
   {
      pFunc =pFunc->pOwner;
      /* Check if an illegal action was invoked during a static variable
       * value initialization
       */
      if( _pInitFunc->bFlags & FUN_ILLEGAL_INIT )
        GenError( _szCErrors, 'E', ERR_ILLEGAL_INIT, szVarName, pFunc->szName );
   }

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      CheckDuplVars( pFunc->pFields, szVarName, iVarScope );
      CheckDuplVars( pFunc->pStatics, szVarName, iVarScope );
      if( !( iVarScope == VS_PRIVATE || iVarScope == VS_PUBLIC ) )
         CheckDuplVars( pFunc->pMemvars, szVarName, iVarScope );
   }
   else
     /* variable defined in a codeblock */
     iVarScope =VS_PARAMETER;
   CheckDuplVars( pFunc->pLocals, szVarName, iVarScope );

   pVar = ( PVAR ) OurMalloc( sizeof( VAR ) );
   pVar->szName = szVarName;
   pVar->szAlias = NULL;
   pVar->cType = cVarType;
   pVar->iUsed = 0;
   pVar->pNext = NULL;

   if( iVarScope & VS_MEMVAR )
   {
      PCOMSYMBOL pSym;
      WORD wPos;

      if( _bAutoMemvarAssume || iVarScope == VS_MEMVAR )
      {
         /** add this variable to the list of MEMVAR variables
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

      switch( iVarScope )
      {
          case VS_MEMVAR:
            /* variable declared in MEMVAR statement */
            break;
          case (VS_PARAMETER | VS_PRIVATE):
            {
                BOOL bNewParameter = FALSE;

                if( ++functions.pLast->wParamNum > functions.pLast->wParamCount )
                {
                   functions.pLast->wParamCount =functions.pLast->wParamNum;
                   bNewParameter = TRUE;
                }

                pSym =GetSymbol( szVarName, &wPos ); /* check if symbol exists already */
                if( ! pSym )
                   pSym =AddSymbol( yy_strdup(szVarName), &wPos );
                pSym->cScope |=VS_MEMVAR;
                GenPCode3( HB_P_PARAMETER, LOBYTE(wPos), HIBYTE(wPos) );
                GenPCode1( LOBYTE(functions.pLast->wParamNum) );

                /* Add this variable to the local variables list - this will
                 * allow to use the correct positions for real local variables.
                 * The name of variable have to be hidden because we should
                 * not find this name on the local variables list.
                 * We have to use the new structure because it is used in
                 * memvars list already.
                 */
                if( bNewParameter )
                {
                   pVar = ( PVAR ) OurMalloc( sizeof( VAR ) );
                   pVar->szName = yy_strdup( szVarName );
                   pVar->szAlias = NULL;
                   pVar->cType = cVarType;
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
                PushSymbol(yy_strdup("__MVPRIVATE"), 1);
                PushNil();
                PushSymbol( yy_strdup(szVarName), 0 );
                Do( 1 );
                pSym =GetSymbol( szVarName, NULL );
                pSym->cScope |=VS_MEMVAR;
            }
            break;
          case VS_PUBLIC:
            {
                PushSymbol(yy_strdup("__MVPUBLIC"), 1);
                PushNil();
                PushSymbol( yy_strdup(szVarName), 0 );
                Do( 1 );
                pSym =GetSymbol( szVarName, NULL );
                pSym->cScope |=VS_MEMVAR;
            }
            break;
      }
   }
   else
   {
      switch( iVarScope )
      {
          case VS_LOCAL:
          case VS_PARAMETER:
              { WORD wLocal = 1;

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
                 if( iVarScope == VS_PARAMETER )
                 {
                    ++functions.pLast->wParamCount;
                    functions.pLast->bFlags |= FUN_USES_LOCAL_PARAMS;
                 }
                 if( _bDebugInfo )
                 {
                    GenPCode3( HB_P_LOCALNAME, LOBYTE( wLocal ), HIBYTE( wLocal ) );
                    GenPCodeN( (BYTE *)szVarName, strlen( szVarName ) );
                    GenPCode1( 0 );
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

PCOMSYMBOL AddSymbol( char * szSymbolName, WORD *pwPos )
{
   PCOMSYMBOL pSym = ( PCOMSYMBOL ) OurMalloc( sizeof( COMSYMBOL ) );

   pSym->szName = szSymbolName;
   pSym->cScope = 0;
   pSym->cType = cVarType;
   pSym->pNext = 0;

   if( ! symbols.iCount )
   {
      symbols.pFirst = pSym;
      symbols.pLast  = pSym;
   }
   else
   {
      ( ( PCOMSYMBOL ) symbols.pLast )->pNext = pSym;
      symbols.pLast = pSym;
   }
   symbols.iCount++;

   if( pwPos )
      *pwPos =symbols.iCount;

   /*if( cVarType != ' ') printf("\nDeclared %s as type %c at symbol %i\n", szSymbolName, cVarType, symbols.iCount );*/
   return pSym;
}

/* Adds new alias to the alias stack
 */
void AliasAdd( ALIASID_PTR pAlias )
{
   pAlias->pPrev =pAliasId;
   pAliasId =pAlias;
}

/* Restores previously selected alias
 */
void AliasRemove( void )
{
   ALIASID_PTR pAlias = pAliasId;

   pAliasId = pAliasId->pPrev;
   OurFree( pAlias );
}

/* Adds an integer workarea number into alias stack
 */
void AliasAddInt( int iWorkarea )
{
   ALIASID_PTR pAlias = (ALIASID_PTR) OurMalloc( sizeof( ALIASID ) );

   pAlias->type =ALIAS_NUMBER;
   pAlias->alias.iAlias =iWorkarea;
   AliasAdd( pAlias );
}

/* Adds an expression into alias stack
 */
void AliasAddExp( void )
{
   ALIASID_PTR pAlias = (ALIASID_PTR) OurMalloc( sizeof( ALIASID ) );

   pAlias->type =ALIAS_EVAL;
   AliasAdd( pAlias );
}

/* Adds an alias name into alias stack
 */
void AliasAddStr( char * szAlias )
{
   ALIASID_PTR pAlias = (ALIASID_PTR) OurMalloc( sizeof( ALIASID ) );

   pAlias->type =ALIAS_NAME;
   pAlias->alias.szAlias =szAlias;
   AliasAdd( pAlias );
}

/* Generates pcodes to store the current workarea number
 */
void AliasPush( void )
{
   GenPCode1( HB_P_PUSHALIAS );
}

/* Generates pcodes to select the workarea number using current value
 * from the eval stack
 */
void AliasPop( void )
{
   GenPCode1( HB_P_POPALIAS );
}

/* Generates pcodes to swap two last items from the eval stack.
 * Last item (after swaping) is next popped as current workarea
 */
void AliasSwap( void )
{
   GenPCode1( HB_P_SWAPALIAS );
}


int Include( char * szFileName, PATHNAMES *pSearch )
{
  PFILE pFile;

  yyin = fopen( szFileName, "r" );
  if( ! yyin )
  {
    if( pSearch )
    {
      FILENAME *pFileName =SplitFilename( szFileName );
      char szFName[ _POSIX_PATH_MAX ];    /* filename to parse */

      pFileName->name =szFileName;
      pFileName->extension =NULL;
      while( pSearch && !yyin )
      {
        pFileName->path =pSearch->szPath;
        MakeFilename( szFName, pFileName );
        yyin = fopen( szFName, "r" );
        if( ! yyin )
        {
            pSearch = pSearch->pNext;
            if( ! pSearch )
              return 0;
        }
      }
      OurFree( (void *) pFileName );
    }
    else
      return 0;
  }

   if( ! _bQuiet )
      printf( "\nparsing file %s\n", szFileName );

   pFile = ( PFILE ) OurMalloc( sizeof( _FILE ) );
   pFile->handle = yyin;
   pFile->szFileName = szFileName;
   pFile->pPrev = NULL;

   if( ! files.iFiles )
      files.pLast = pFile;
   else
   {
      files.pLast->iLine = iLine;
      iLine = 1;
      pFile->pPrev = files.pLast;
      files.pLast  = pFile;
   }
#ifdef __cplusplus
   yy_switch_to_buffer( (YY_BUFFER_STATE) (pFile->pBuffer = yy_create_buffer( yyin, 8192 * 2 ) ) );
#else
   yy_switch_to_buffer( pFile->pBuffer = yy_create_buffer( yyin, 8192 * 2 ) );
#endif
   files.iFiles++;
   return 1;
}

int yywrap( void )   /* handles the EOF of the currently processed file */
{
   void * pLast;

   if( files.iFiles == 1 )
      return 1;      /* we have reached the main EOF */
   else
   {
      pLast = files.pLast;
      fclose( files.pLast->handle );
      files.pLast = ( PFILE ) ( ( PFILE ) files.pLast )->pPrev;
      iLine = files.pLast->iLine;
      if( ! _bQuiet )
         printf( "\nparsing file %s\n", files.pLast->szFileName );
#ifdef __cplusplus
      yy_delete_buffer( (YY_BUFFER_STATE) ( ( PFILE ) pLast )->pBuffer );
#else
      yy_delete_buffer( ( ( PFILE ) pLast )->pBuffer );
#endif
      free( pLast );
      files.iFiles--;
      yyin = files.pLast->handle;
#ifdef __cplusplus
      yy_switch_to_buffer( (YY_BUFFER_STATE) files.pLast->pBuffer );
#else
      yy_switch_to_buffer( files.pLast->pBuffer );
#endif
      return 0;      /* we close the currently include file and continue */
   }
}

void Duplicate( void )
{
   GenPCode1( HB_P_DUPLICATE );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = pStackValType->cType;
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      /*debug_msg( "\n* *Duplicate()\n ", NULL );*/
   }
}

void DupPCode( WORD wStart ) /* duplicates the current generated pcode from an offset */
{
   WORD w, wEnd = functions.pLast->lPCodePos - wStart;

   for( w = 0; w < wEnd; w++ )
      GenPCode1( functions.pLast->pCode[ wStart + w ] );
}

/*
 * Function generates passed pcode for passed database field
 */
void FieldPCode( BYTE bPCode, char * szVarName )
{
   WORD wVar;
   PCOMSYMBOL pVar;

   pVar = GetSymbol( szVarName, &wVar );
   if( ! pVar )
      pVar =AddSymbol( szVarName, &wVar );
   pVar->cScope |=VS_MEMVAR;
   GenPCode3( bPCode, LOBYTE( wVar ), HIBYTE( wVar ) );
}

/*
 * This function creates and initialises the _FUNC structure
 */
PFUNCTION FunctionNew( char *szName, SYMBOLSCOPE cScope )
{
   PFUNCTION pFunc;

   pFunc = ( PFUNCTION ) OurMalloc( sizeof( _FUNC ) );
   pFunc->szName       = szName;
   pFunc->cScope       = cScope;
   pFunc->pLocals      = 0;
   pFunc->pStatics     = 0;
   pFunc->pFields      = 0;
   pFunc->pMemvars     = 0;
   pFunc->pCode        = 0;
   pFunc->lPCodeSize   = 0;
   pFunc->lPCodePos    = 0;
   pFunc->pNext        = 0;
   pFunc->wParamCount  = 0;
   pFunc->wParamNum    = 0;
   pFunc->wStaticsBase = _wStatics;
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
void FunDef( char * szFunName, SYMBOLSCOPE cScope, int iType )
{
   PCOMSYMBOL   pSym;
   PFUNCTION pFunc;
   char * *pFunction;

   pFunc = GetFunction( szFunName );
   if( pFunc )
   {
      /* The name of a function/procedure is already defined */
      if( ( pFunc != functions.pFirst ) || _bStartProc )
        /* it is not a starting procedure that was automatically created */
        GenError( _szCErrors, 'E', ERR_FUNC_DUPL, szFunName, NULL );
   }

   pFunction = (char * *)RESERVED_FUNC( szFunName );
   if( pFunction && !( functions.iCount==0 && !_bStartProc ) )
   {
      /* We are ignoring it when it is the name of PRG file and we are
       * not creating implicit starting procedure
       */
        GenError( _szCErrors, 'E', ERR_FUNC_RESERVED, *pFunction, szFunName );
   }

   FixReturns();    /* fix all previous function returns offsets */

   pSym = GetSymbol( szFunName, NULL );
   if( ! pSym )
      /* there is not a symbol on the symbol table for this function name */
      pSym = AddSymbol( szFunName, NULL );

   if( cScope != FS_PUBLIC )
/*      pSym->cScope = FS_PUBLIC; */
/*   else */
      pSym->cScope |= cScope; /* we may have a non public function and a object message */

   pFunc = FunctionNew( szFunName, cScope );
   pFunc->bFlags |= iType;

   if( functions.iCount == 0 )
   {
      functions.pFirst = pFunc;
      functions.pLast  = pFunc;
   }
   else
   {
      functions.pLast->pNext = pFunc;
      functions.pLast = pFunc;
   }
   functions.iCount++;

   GenPCode3( HB_P_FRAME, 0, 0 );   /* frame for locals and parameters */
   GenPCode3( HB_P_SFRAME, 0, 0 );     /* frame for statics variables */

   if( _bDebugInfo )
   {
      GenPCode1( HB_P_MODULENAME );
      GenPCodeN( (BYTE *)files.pLast->szFileName, strlen( files.pLast->szFileName ) );
      GenPCode1( ':' );
      GenPCodeN( (BYTE *)szFunName, strlen( szFunName ) );
      GenPCode1( 0 );
   }
}

void GenJava( char *szFileName, char *szName )
{
  printf( "\ngenerating Java language output...\n" );
  printf( "%s -> not implemented yet! %s\n", szFileName, szName );
}

void GenPascal( char *szFileName, char *szName )
{
  printf( "\ngenerating Pascal language output...\n" );
  printf( "%s -> not implemented yet! %s\n", szFileName, szName );
}

void GenRC( char *szFileName, char *szName )
{
  printf( "\ngenerating resources output...\n" );
  printf( "%s -> not implemented yet! %s\n", szFileName, szName );
}

void GenCCode( char *szFileName, char *szName )       /* generates the C language output */
{
   PFUNCTION pFunc = functions.pFirst, pFTemp;
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w, wLen, wSym, wVar;
   WORD iNestedCodeblock = 0;
   LONG lPCodePos;
   char chr;
   BOOL bEndProcRequired;

   FILE * yyc;             /* file handle for C output */

   HB_SYMBOL_UNUSED( szName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
     printf( "Error opening file %s\n", szFileName );
     return;
   }

   if( ! _bQuiet )
      printf( "\nGenerating C language output...\n" );

   fprintf( yyc, "#include \"hb_vmpub.h\"\n" );
   fprintf( yyc, "#include \"init.h\"\n\n\n" );

   if( ! _bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   /* write functions prototypes for PRG defined functions */
   while( pFunc )
   {
      if( pFunc->cScope & FS_STATIC || pFunc->cScope & FS_INIT || pFunc->cScope & FS_EXIT )
         fprintf( yyc, "static " );

      if( pFunc == _pInitFunc )
         fprintf( yyc, "HARBOUR hb_INITSTATICS( void );\n" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( yyc, "HARBOUR HB_%s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }
   /* write functions prototypes for called functions outside this PRG */
   pFunc = funcalls.pFirst;
   while( pFunc )
   {
      pFTemp = GetFunction( pFunc->szName );
      if( ! pFTemp || pFTemp == functions.pFirst )
         fprintf( yyc, "HARBOUR HB_%s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }

   /* writes the symbol table */
   /* Generate the wrapper that will initialize local symbol table
    */
   yy_strupr( _pFileName->name );
   fprintf( yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( %s__InitSymbols )\n", _pFileName->name );

   if( ! _bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   wSym = 0; /* symbols counter */
   while( pSym )
   {
      if( pSym->szName[ 0 ] == '(' )
      {
         /* Since the normal function cannot be INIT and EXIT at the same time
         * we are using these two bits to mark the special function used to
         * initialize static variables
         */
         fprintf( yyc, "{ \"(_INITSTATICS)\", FS_INIT | FS_EXIT, hb_INITSTATICS, 0}" ); /* NOTE: hb_ intentionally in lower case */
      }
      else
      {
         fprintf( yyc, "{ \"%s\", ", pSym->szName );

         if( pSym->cScope & FS_STATIC )
            fprintf( yyc, "FS_STATIC" );

         else if( pSym->cScope & FS_INIT )
            fprintf( yyc, "FS_INIT" );

         else if( pSym->cScope & FS_EXIT )
            fprintf( yyc, "FS_EXIT" );

         else
            fprintf( yyc, "FS_PUBLIC" );

         if( pSym->cScope & VS_MEMVAR )
            fprintf( yyc, " | FS_MEMVAR" );

         if( ( pSym->cScope != FS_MESSAGE ) && ( pSym->cScope & FS_MESSAGE ) ) /* only for non public symbols */
            fprintf( yyc, " | FS_MESSAGE" );

         /* specify the function address if it is a defined function or an
            external called function */
         if( GetFunction( pSym->szName ) ) /* is it a function defined in this module */
            fprintf( yyc, ", HB_%s, 0 }", pSym->szName );
         else if( GetFuncall( pSym->szName ) ) /* is it a function called from this module */
            fprintf( yyc, ", HB_%s, 0 }", pSym->szName );
         else
            fprintf( yyc, ", 0, 0 }" );   /* memvar */
      }
      ++wSym;

      if( pSym != symbols.pLast )
         fprintf( yyc, ",\n" );

      pSym = pSym->pNext;
   }
   fprintf( yyc, "\nHB_INIT_SYMBOLS_END( %s__InitSymbols )\n", _pFileName->name );
   fprintf( yyc, "#if ! defined(__GNUC__)\n#pragma startup %s__InitSymbols\n#endif\n\n\n", _pFileName->name );

   /* Generate functions data
    */
   pFunc = functions.pFirst;
   if( ! _bStartProc )
     pFunc = pFunc->pNext; /* No implicit starting procedure */
   while( pFunc )
   {
      if( pFunc->cScope != FS_PUBLIC )
         fprintf( yyc, "static " );

      if( pFunc == _pInitFunc )        /* Is it (_INITSTATICS) */
         fprintf( yyc, "HARBOUR hb_INITSTATICS( void )\n{\n  static BYTE pcode[] = { \n" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( yyc, "HARBOUR HB_%s( void )\n{\n  static BYTE pcode[] = { \n", pFunc->szName );

      bEndProcRequired =TRUE;
      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case HB_P_AND:
                 fprintf( yyc, "                HB_P_AND,\n" );
                 lPCodePos++;
                 break;

            case HB_P_ARRAYAT:
                 fprintf( yyc, "                HB_P_ARRAYAT,\n" );
                 lPCodePos++;
                 break;

            case HB_P_ARRAYPUT:
                 fprintf( yyc, "                HB_P_ARRAYPUT,\n" );
                 lPCodePos++;
                 break;

            case HB_P_DEC:
                 fprintf( yyc, "                HB_P_DEC,\n" );
                 lPCodePos++;
                 break;

            case HB_P_DIMARRAY:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                HB_P_DIMARRAY, %i, %i,\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case HB_P_DIVIDE:
                 fprintf( yyc, "                HB_P_DIVIDE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_DO:
                 fprintf( yyc, "                HB_P_DO, %i, %i,\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ] );
                 lPCodePos += 3;
                 break;

            case HB_P_DUPLICATE:
                 fprintf( yyc, "                HB_P_DUPLICATE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_DUPLTWO:
                 fprintf( yyc, "                HB_P_DUPLTWO,\n" );
                 lPCodePos++;
                 break;

            case HB_P_EQUAL:
                 fprintf( yyc, "                HB_P_EQUAL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_EXACTLYEQUAL:
                 fprintf( yyc, "                HB_P_EXACTLYEQUAL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_ENDBLOCK:
                 --iNestedCodeblock;
                 fprintf( yyc, "                HB_P_ENDBLOCK,\n" );
                 lPCodePos++;
                 break;

            case HB_P_ENDPROC:
                 lPCodePos++;
                 if( lPCodePos == pFunc->lPCodePos )
                 {
                     bEndProcRequired =FALSE;
                     fprintf( yyc, "                HB_P_ENDPROC\n" );
                 }
                 else
                  fprintf( yyc, "                HB_P_ENDPROC,\n" );
                 break;

            case HB_P_FALSE:
                 fprintf( yyc, "                HB_P_FALSE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_FORTEST:                    /* ER For tests. Step > 0 LESS */
                                              /* Step < 0 GREATER */
                 fprintf( yyc, "                HB_P_FORTEST,\n" );
                 lPCodePos++;
                 break;

            case HB_P_FRAME:
                 {
                    PVAR pLocal  = pFunc->pLocals;
                    BYTE bLocals = 0;

                    while( pLocal )
                    {
                       pLocal = pLocal->pNext;
                       bLocals++;
                    }

                    if( bLocals || pFunc->wParamCount )
                       fprintf( yyc, "                HB_P_FRAME, %i, %i,\t\t/* locals, params */\n",
                                bLocals - pFunc->wParamCount,
                                pFunc->wParamCount );
                    lPCodePos += 3;
                 }
                 break;

            case HB_P_FUNCPTR:
                 fprintf( yyc, "                HB_P_FUNCPTR,\n" );
                 lPCodePos++;
                 break;

            case HB_P_FUNCTION:
                 fprintf( yyc, "                HB_P_FUNCTION, %i, %i,\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ] );
                 lPCodePos += 3;
                 break;

            case HB_P_GENARRAY:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                HB_P_GENARRAY, %i, %i,\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case HB_P_GREATER:
                 fprintf( yyc, "                HB_P_GREATER,\n" );
                 lPCodePos++;
                 break;

            case HB_P_GREATEREQUAL:
                 fprintf( yyc, "                HB_P_GREATEREQUAL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_INC:
                 fprintf( yyc, "                HB_P_INC,\n" );
                 lPCodePos++;
                 break;

            case HB_P_INSTRING:
                 fprintf( yyc, "                HB_P_INSTRING,\n" );
                 lPCodePos++;
                 break;

            case HB_P_JUMP:
                 /*if( 1 ) (lPCodePos + 3) < pFunc->lPCodePos ) */
                 {
                    w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    fprintf( yyc, "                HB_P_JUMP, %i, %i,\t/* %i (abs: %05li) */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 }
                 lPCodePos += 3;
                 break;

            case HB_P_JUMPFALSE:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                HB_P_JUMPFALSE, %i, %i,\t/* %i (abs: %05li) */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 lPCodePos += 3;
                 break;

            case HB_P_JUMPTRUE:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                HB_P_JUMPTRUE, %i, %i,\t/* %i (abs: %05li) */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 lPCodePos += 3;
                 break;

            case HB_P_LESS:
                 fprintf( yyc, "                HB_P_LESS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_LESSEQUAL:
                 fprintf( yyc, "                HB_P_LESSEQUAL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_LINE:
                 fprintf( yyc, "/* %05li */", lPCodePos );
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "  HB_P_LINE, %i, %i,\t\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case HB_P_LOCALNAME:
                 fprintf( yyc, "                HB_P_LOCALNAME, %i, %i,\t/* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          ( char * ) pFunc->pCode + lPCodePos + 3 );
                 lPCodePos += 3;
                 while( pFunc->pCode[ lPCodePos ] )
                 {
                    chr = pFunc->pCode[ lPCodePos++ ];
                    if( chr == '\'' || chr == '\\')
                      fprintf( yyc, " \'\\%c\',", chr );
                    else
                      fprintf( yyc, " \'%c\',", chr );
                 }
                 fprintf( yyc, " 0,\n" );
                 lPCodePos++;
                 break;

            case HB_P_MESSAGE:
                 {
                  WORD wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wSym );
                  fprintf( yyc, "                HB_P_MESSAGE, %i, %i,      /* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wSym )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_MINUS:
                 fprintf( yyc, "                HB_P_MINUS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_MODULENAME:
                 fprintf( yyc, "                HB_P_MODULENAME, /* %s */\n",
                          ( char * ) pFunc->pCode + lPCodePos++ + 1 );
                 while( pFunc->pCode[ lPCodePos ] )
                 {
                    chr = pFunc->pCode[ lPCodePos++ ];
                    if( chr == '\'' || chr == '\\')
                      fprintf( yyc, " \'\\%c\',", chr );
                    else
                      fprintf( yyc, " \'%c\',", chr );
                 }
                 fprintf( yyc, " 0,\n" );
                 lPCodePos++;
                 break;

            case HB_P_MODULUS:
                 fprintf( yyc, "                HB_P_MODULUS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_MULT:
                 fprintf( yyc, "                HB_P_MULT,\n" );
                 lPCodePos++;
                 break;

            case HB_P_NEGATE:
                 fprintf( yyc, "                HB_P_NEGATE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_NOT:
                 fprintf( yyc, "                HB_P_NOT,\n" );
                 lPCodePos++;
                 break;

            case HB_P_NOTEQUAL:
                 fprintf( yyc, "                HB_P_NOTEQUAL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_OR:
                 fprintf( yyc, "                HB_P_OR,\n" );
                 lPCodePos++;
                 break;

            case HB_P_PARAMETER:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_PARAMETER, %i, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           pFunc->pCode[ lPCodePos + 3 ],
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 4;
                 }
                 break;

            case HB_P_PLUS:
                 fprintf( yyc, "                HB_P_PLUS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_POP:
                 fprintf( yyc, "                HB_P_POP,\n" );
                 lPCodePos++;
                 break;

            case HB_P_POPALIAS:
                 fprintf( yyc, "                HB_P_POPALIAS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_POPALIASEDFIELD:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_POPALIASEDFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_POPFIELD:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_POPFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_POPLOCAL:
                 {
                   SHORT wVar = * ( ( SHORT *) &(pFunc->pCode )[ lPCodePos + 1 ] );
                   /* Variable with negative order are local variables
                    * referenced in a codeblock -handle it with care
                    */
                   if( iNestedCodeblock )
                   {
                     /* we are accesing variables within a codeblock */
                     /* the names of codeblock variable are lost     */
                     if( wVar < 0 )
                       fprintf( yyc, "                HB_P_POPLOCAL, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                HB_P_POPLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                HB_P_POPLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case HB_P_POPMEMVAR:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_POPMEMVAR, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_POPSTATIC:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                HB_P_POPSTATIC, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case HB_P_POWER:
                 fprintf( yyc, "                HB_P_POWER,\n" );
                 lPCodePos++;
                 break;

            case HB_P_PUSHALIAS:
                 fprintf( yyc, "                HB_P_PUSHALIAS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_PUSHALIASEDFIELD:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                           pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_PUSHALIASEDFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHBLOCK:
                 ++iNestedCodeblock;
                 fprintf( yyc, "                HB_P_PUSHBLOCK, %i, %i,\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256 );
                 w = * ( ( WORD *) &( pFunc->pCode [ lPCodePos + 3 ] ) );
                 fprintf( yyc, "                %i, %i, \t/* number of local parameters (%i) */\n",
                          pFunc->pCode[ lPCodePos + 3 ],
                          pFunc->pCode[ lPCodePos + 4 ], w );
                 wVar = * ( ( WORD *) &( pFunc->pCode [ lPCodePos + 5 ] ) );
                 fprintf( yyc, "                %i, %i, \t/* number of local variables (%i) */\n",
                          pFunc->pCode[ lPCodePos + 5 ],
                          pFunc->pCode[ lPCodePos + 6 ], wVar );
                 lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
                 /* create the table of referenced local variables */
                 while( wVar-- )
                 {
                   w = * ( ( WORD *) &( pFunc->pCode [ lPCodePos ] ) );
                   fprintf( yyc, "                %i, %i, \t/* %s */\n",
                            pFunc->pCode[ lPCodePos ],
                            pFunc->pCode[ lPCodePos + 1 ],
                            GetVar( pFunc->pLocals, w )->szName );
                   lPCodePos +=2;
                 }
                 break;

            case HB_P_PUSHDOUBLE:
                 {
                    int i;
                    ++lPCodePos;
                    fprintf( yyc, "                HB_P_PUSHDOUBLE, " );
                    for( i = 0; i < sizeof( double ) + sizeof( BYTE ); ++i )
                       fprintf( yyc, "%i, ", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
                    fprintf( yyc, "/* %.*f, %d */\n",
                    *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
                    *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
                    *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ) );
                    lPCodePos += sizeof( double ) + sizeof( BYTE );
                 }
                 break;

            case HB_P_PUSHFIELD:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                           pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_PUSHFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHINT:
                 fprintf( yyc, "                HB_P_PUSHINT, %i, %i,     /* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256 );
                 lPCodePos += 3;
                 break;

            case HB_P_PUSHLOCAL:
                 {
                   SHORT wVar = * ( ( SHORT *) &(pFunc->pCode )[ lPCodePos + 1 ] );
                   /* Variable with negative order are local variables
                    * referenced in a codeblock -handle it with care
                    */
                   if( iNestedCodeblock )
                   {
                     /* we are accesing variables within a codeblock */
                     /* the names of codeblock variable are lost     */
                     if( wVar < 0 )
                       fprintf( yyc, "                HB_P_PUSHLOCAL, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                HB_P_PUSHLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                HB_P_PUSHLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHLOCALREF:
                 {
                   SHORT wVar = * ( ( SHORT *) &(pFunc->pCode )[ lPCodePos + 1 ] );
                   /* Variable with negative order are local variables
                    * referenced in a codeblock -handle it with care
                    */
                   if( iNestedCodeblock )
                   {
                     /* we are accesing variables within a codeblock */
                     /* the names of codeblock variable are lost     */
                     if( wVar < 0 )
                       fprintf( yyc, "                HB_P_PUSHLOCALREF, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                HB_P_PUSHLOCALREF, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                HB_P_PUSHLOCALREF, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHLONG:
                 fprintf( yyc, "                HB_P_PUSHLONG, %i, %i, %i, %i,  /* %li */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          pFunc->pCode[ lPCodePos + 3 ],
                          pFunc->pCode[ lPCodePos + 4 ],
                          *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
                 lPCodePos +=( 1 + sizeof(long) );
                 break;

            case HB_P_PUSHMEMVAR:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                           pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_PUSHMEMVAR, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHMEMVARREF:
                 {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                           pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wVar );
                  fprintf( yyc, "                HB_P_PUSHMEMVARREF, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHNIL:
                 fprintf( yyc, "                HB_P_PUSHNIL,\n" );
                 lPCodePos++;
                 break;

            case HB_P_PUSHSELF:
                 fprintf( yyc, "                HB_P_PUSHSELF,\n" );
                 lPCodePos++;
                 break;

            case HB_P_PUSHSTATIC:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] +pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                HB_P_PUSHSTATIC, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHSTATICREF:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                HB_P_PUSHSTATICREF, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case HB_P_PUSHSTR:
                 wLen = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                HB_P_PUSHSTR, %i, %i,     /* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], wLen );
                 lPCodePos +=3;
                 while( wLen-- )
                 {
                    chr = pFunc->pCode[ lPCodePos++ ];
                    if( chr == '\'' || chr == '\\')
                      fprintf( yyc, " \'\\%c\',", chr );
                    else
                      fprintf( yyc, " \'%c\',", chr );
                 }
                 fprintf( yyc, "\n" );
                 break;

            case HB_P_PUSHSYM:
                 {
                  WORD wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] +
                           pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos =FixSymbolPos( wSym );
                  fprintf( yyc, "                HB_P_PUSHSYM, %i, %i,      /* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wSym )->szName );
                  lPCodePos += 3;
                 }
                 break;

            case HB_P_RETVALUE:
                 fprintf( yyc, "                HB_P_RETVALUE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_SFRAME:
                 /* we only generate it if there are statics used in this function */
                 if( pFunc->bFlags & FUN_USES_STATICS )
                 {
                    GetSymbol( _pInitFunc->szName, &w );
                    w = FixSymbolPos( w );
                    fprintf( yyc, "                HB_P_SFRAME, %i, %i,\t\t/* symbol (_INITSTATICS) */\n",
                             LOBYTE( w ), HIBYTE( w ) );
                 }
                 lPCodePos += 3;
                 break;

            case HB_P_STATICS:
                 {
                    GetSymbol( _pInitFunc->szName, &w );
                    w = FixSymbolPos( w );
                    fprintf( yyc, "                HB_P_STATICS, %i, %i,\t\t/* symbol (_INITSTATICS) */\n",
                             LOBYTE( w ), HIBYTE( w ) );
                    lPCodePos += 3;
                 }
                 break;

            case HB_P_SWAPALIAS:
                 fprintf( yyc, "                HB_P_SWAPALIAS,\n" );
                 lPCodePos++;
                 break;

            case HB_P_TRUE:
                 fprintf( yyc, "                HB_P_TRUE,\n" );
                 lPCodePos++;
                 break;

            case HB_P_ZERO:
                 fprintf( yyc, "                HB_P_ZERO,\n" );
                 lPCodePos++;
                 break;

            default:
                 printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
                 lPCodePos = pFunc->lPCodePos;
                 break;
         }
      }

      fprintf( yyc, "/* %05li */", lPCodePos );
      if( bEndProcRequired )
         fprintf( yyc, "  HB_P_ENDPROC };\n\n" );
      else
         fprintf( yyc, "  };\n\n" );
      fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n}\n\n" );
      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   pFunc =functions.pFirst;
   while( pFunc )
     pFunc = KillFunction( pFunc );

   pFunc =funcalls.pFirst;
   while( pFunc )
   {
     funcalls.pFirst =pFunc->pNext;
     OurFree( (void *) pFunc );  /*NOTE: szName will be released by KillSymbol() */
     pFunc =funcalls.pFirst;
   }

   pSym =symbols.pFirst;
   while( pSym )
      pSym = KillSymbol( pSym );

   if( ! _bQuiet )
      printf( "%s -> done!\n", szFileName );
}

PFUNCTION KillFunction( PFUNCTION pFunc )
{
  PFUNCTION pNext = pFunc->pNext;
  PVAR pVar;

  while( pFunc->pLocals )
  {
    pVar = pFunc->pLocals;
    pFunc->pLocals =pVar->pNext;

    OurFree( (void *) pVar->szName );
    OurFree( (void *) pVar );
  }

  while( pFunc->pStatics )
  {
    pVar = pFunc->pStatics;
    pFunc->pStatics =pVar->pNext;

    OurFree( (void *) pVar->szName );
    OurFree( (void *) pVar );
  }

  while( pFunc->pFields )
  {
    pVar = pFunc->pFields;
    pFunc->pFields =pVar->pNext;

    OurFree( (void *) pVar->szName );
    if( pVar->szAlias )
    {
      OurFree( (void *) pVar->szAlias );
    }
    OurFree( (void *) pVar );
  }

  while( pFunc->pMemvars )
  {
    pVar =pFunc->pMemvars;
    pFunc->pMemvars =pVar->pNext;

    OurFree( (void *) pVar->szName );
    if( pVar->szAlias )
    {
      OurFree( (void *) pVar->szAlias );
    }
    OurFree( (void *) pVar );
  }

  OurFree( (void *) pFunc->pCode );
/*  OurFree( (void *) pFunc->szName ); The name will be released in KillSymbol() */
  OurFree( (void *) pFunc );

  return pNext;
}


PCOMSYMBOL KillSymbol( PCOMSYMBOL pSym )
{
  PCOMSYMBOL pNext = pSym->pNext;

  OurFree( (void *) pSym->szName );
  OurFree( (void *) pSym );

  return pNext;
}


void GenExterns( void ) /* generates the symbols for the EXTERN names */
{
  PEXTERN pDelete;

  if( _bDebugInfo )
     AddExtern( yy_strdup( "DEBUGGER" ) );

  while( pExterns )
  {
    if( GetSymbol( pExterns->szName, NULL ) )
    {
      if( ! GetFuncall( pExterns->szName ) )
        AddFunCall( pExterns->szName );
    }
    else
    {
      AddSymbol( pExterns->szName, NULL );
      AddFunCall( pExterns->szName );
    }
    pDelete  = pExterns;
    pExterns = pExterns->pNext;
    OurFree( (void *) pDelete );
  }
}

PFUNCTION GetFuncall( char * szFunctionName ) /* returns a previously called defined function */
{
   PFUNCTION pFunc = funcalls.pFirst;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) )
         return pFunc;
      else
      {
         if( pFunc->pNext )
            pFunc = pFunc->pNext;
         else
            return 0;
      }
   }
   return 0;
}

PFUNCTION GetFunction( char * szFunctionName ) /* returns a previously defined function */
{
   PFUNCTION pFunc = functions.pFirst;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) )
         return pFunc;
      else
      {
         if( pFunc->pNext )
            pFunc = pFunc->pNext;
         else
            return 0;
      }
   }
   return 0;
}

PVAR GetVar( PVAR pVars, WORD wOrder ) /* returns variable if defined or zero */
{
   WORD w = 1;

   while( pVars->pNext && w++ < wOrder )
      pVars = pVars->pNext;

   return pVars;
}

WORD GetVarPos( PVAR pVars, char * szVarName ) /* returns the order + 1 of a variable if defined or zero */
{
   WORD wVar = 1;

   while( pVars )
   {
      if( pVars->szName && ! strcmp( pVars->szName, szVarName ) )
      {
         if( _bWarnings )
         {
            PSTACK_VAL_TYPE pNewStackType;

            pVars->iUsed = 1;

            pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
            pNewStackType->cType = pVars->cType;
            pNewStackType->pPrev = pStackValType;

            pStackValType = pNewStackType;
            debug_msg( "\n* *GetVarPos()\n", NULL );
         }
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
  int iVar;
  PFUNCTION pFunc =functions.pLast;

  if( pFunc->szName )
    /* we are in a function/procedure -we don't need any tricks */
    return GetVarPos( pFunc->pLocals, szVarName );
  else
  {
    /* we are in a codeblock */
    iVar = GetVarPos( pFunc->pLocals, szVarName );
    if( iVar )
      /* this is a current codeblock parameter */
      return iVar;
    else
    {
      /* we have to check the list of nested codeblock up to a function
      * where the codeblock is defined
      */
      pFunc =pFunc->pOwner;
      while( pFunc )
      {
        iVar =GetVarPos( pFunc->pLocals, szVarName );
        if( iVar )
        {
          if( pFunc->pOwner )
            /* this variable is defined in a parent codeblock
            * It is not possible to access a parameter of a codeblock in which
            * the current codeblock is defined
            */
            GenError( _szCErrors, 'E', ERR_OUTER_VAR, szVarName, NULL );
          else
          {
            /* We want to access a local variable defined in a function that
             * owns this codeblock. We cannot access this variable in a normal
             * way because at runtime the stack base will point to local
             * variables of EVAL function.
             *  The codeblock cannot have static variables then we can use this
             * structure to store temporarily all referenced local variables
             */
            pFunc =functions.pLast;

            iVar =GetVarPos( pFunc->pStatics, szVarName );
            if( !iVar )
            {
              /* this variable was not referenced yet - add it to the list */
              PVAR pVar;

              pVar = (PVAR) OurMalloc( sizeof(VAR) );
              pVar->szName = szVarName;
              pVar->cType = ' ';
              pVar->iUsed = 0;
              pVar->pNext  = NULL;

              iVar = 1;  /* first variable */
              if( ! pFunc->pStatics )
                pFunc->pStatics = pVar;
              else
              {
                PVAR pLastVar = pFunc->pStatics;

                ++iVar;   /* this will be at least second variable */
                while( pLastVar->pNext )
                {
                  pLastVar = pLastVar->pNext;
                  ++iVar;
                }
                pLastVar->pNext = pVar;
              }
            }
            /* Use negative order to signal that we are accessing a local
            * variable from a codeblock
            */
            return (-iVar);
          }
        }
        pFunc =pFunc->pOwner;
      }
    }
  }
  return 0;
}

/*
 * Gets position of passed static variables.
 * All static variables are hold in a single array at runtime then positions
 * are numbered for whole PRG module.
 */
int GetStaticVarPos( char *szVarName )
{
  int iPos;
  PFUNCTION pFunc = functions.pLast;

  /* First we have to check if this name belongs to a static variable
    * defined in current function
    */
  if( pFunc->pOwner )
      pFunc =pFunc->pOwner;  /* we are in the static variable definition state */
  iPos =GetVarPos( pFunc->pStatics, szVarName );
  if( iPos )
      return iPos + pFunc->wStaticsBase;

  /* Next we have to check the list of global static variables
    * Note: It is not possible to have global static variables when
    * implicit starting procedure is defined
    */
  if( !_bStartProc )
  {
    iPos =GetVarPos( functions.pFirst->pStatics, szVarName );
    if( iPos )
      return iPos;
  }
  return 0;
}

/* Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 * It also returns a pointer to the function where this field was declared
 */
int GetFieldVarPos( char *szVarName, PFUNCTION *pOwner )
{
   int iVar;
   PFUNCTION pFunc =functions.pLast;

   *pOwner =NULL;
   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar =GetVarPos( pFunc->pFields, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc =pFunc->pOwner;
      iVar =GetVarPos( pFunc->pFields, szVarName );
   }
   /* If not found on the list declared in current function then check
    * the global list (only if there will be no starting procedure)
    */
   if( ! iVar && ! _bStartProc )
   {
      pFunc =functions.pFirst;
      iVar =GetVarPos( pFunc->pFields, szVarName );
   }
   if( iVar )
      *pOwner =pFunc;

   return iVar;
}

/** Checks if passed variable name is declared as FIELD
 * Returns 0 if not found in FIELD list or its position in this list if found
 */
int GetMemvarPos( char *szVarName )
{
   int iVar;
   PFUNCTION pFunc =functions.pLast;

   if( pFunc->szName )
      /* we are in a function/procedure -we don't need any tricks */
      iVar =GetVarPos( pFunc->pMemvars, szVarName );
   else
   {
      /* we have to check the list of nested codeblock up to a function
       * where the codeblock is defined
       */
      while( pFunc->pOwner )
         pFunc =pFunc->pOwner;
      iVar =GetVarPos( pFunc->pMemvars, szVarName );
   }
   /* if not found on the list declared in current function then check
    * the global list (only if there will be no starting procedure)
    */
   if( ! iVar && ! _bStartProc )
      iVar =GetVarPos( functions.pFirst->pMemvars, szVarName );

   return iVar;
}

WORD FixSymbolPos( WORD wCompilePos )
{
   return (_bStartProc ? wCompilePos-1 : wCompilePos-2);
}


/* returns a symbol pointer from the symbol table
 * and sets its position in the symbol table
 */
PCOMSYMBOL GetSymbol( char * szSymbolName, WORD * pwPos )
{
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD wCnt = 1;

   if( pwPos )
      *pwPos = 0;
   while( pSym )
   {
      if( ! strcmp( pSym->szName, szSymbolName ) )
      {
         if( pwPos )
            *pwPos =wCnt;
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
            return 0;
      }
   }
   return 0;
}

PCOMSYMBOL GetSymbolOrd( WORD wSymbol )   /* returns a symbol based on its index on the symbol table */
{
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w = 1;

   while( w++ < wSymbol && pSym->pNext )
      pSym = pSym->pNext;

   return pSym;
}

WORD GetFunctionPos( char * szFunctionName ) /* return 0 if not found or order + 1 */
{
   PFUNCTION pFunc = functions.pFirst;
   WORD wFunction = _bStartProc;

   while( pFunc )
   {
      if( ! strcmp( pFunc->szName, szFunctionName ) && pFunc != functions.pFirst )
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
   GenPCode1( HB_P_INC );

   if( _bWarnings )
   {
      char sType[2];

      if( pStackValType )
      {
         sType[0] = pStackValType->cType;
         sType[1] = 0;
      }
      else
        debug_msg( "\n* *Inc() Compile time stack overflow\n", NULL );


      if( pStackValType && pStackValType->cType == ' ' )
         GenWarning( _szCWarnings, 'W', WARN_NUMERIC_SUSPECT, NULL, NULL );
      else if( pStackValType->cType != 'N' )
         GenWarning( _szCWarnings, 'W', WARN_NUMERIC_TYPE, sType, NULL );
   }
}

WORD Jump( int iOffset )
{
   GenPCode3( HB_P_JUMP, LOBYTE( iOffset ), HIBYTE( iOffset ) );

   return functions.pLast->lPCodePos - 2;
}

WORD JumpFalse( int iOffset )
{
   GenPCode3( HB_P_JUMPFALSE, LOBYTE( iOffset ), HIBYTE( iOffset ) );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pFree;
      char sType[2];

      if( pStackValType )
      {
         sType[0] = pStackValType->cType;
         sType[1] = 0;
      }
      else
        debug_msg( "\n* *HB_P_JUMPFALSE Compile time stack overflow\n", NULL );

      /* compile time Operand value */
      if( pStackValType && pStackValType->cType == ' ' )
         GenWarning( _szCWarnings, 'W', WARN_LOGICAL_SUSPECT, NULL, NULL );
      else if( pStackValType && pStackValType->cType != 'L')
         GenWarning( _szCWarnings, 'W', WARN_LOGICAL_TYPE, sType, NULL );

      /* compile time assignment value has to be released */
      pFree = pStackValType;
      debug_msg( "\n* *---JampFalse()\n", NULL );

      if( pStackValType )
      {
         pStackValType = pStackValType->pPrev;
      }

      if( pFree )
      {
         OurFree( (void *) pFree );
      }
   }

   return functions.pLast->lPCodePos - 2;
}

void JumpThere( int iOffset, WORD wTo )
{
   BYTE * pCode = functions.pLast->pCode;

   pCode[ ( WORD ) iOffset ]     = LOBYTE( wTo - iOffset + 1 );
   pCode[ ( WORD ) iOffset + 1 ] = HIBYTE( wTo - iOffset + 1 );
}

void JumpHere( int iOffset )
{
   JumpThere( iOffset, functions.pLast->lPCodePos );
}

WORD JumpTrue( int iOffset )
{
   GenPCode3( HB_P_JUMPTRUE, LOBYTE( iOffset ), HIBYTE( iOffset ) );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pFree;
      char sType[2];

      if( pStackValType )
      {
         sType[0] = pStackValType->cType;
         sType[1] = 0;
      }
      else
        debug_msg( "\n* *HB_P_JUMPTRUE Compile time stack overflow\n", NULL );

      /* compile time Operand value */
      if( pStackValType && pStackValType->cType == ' ' )
         GenWarning( _szCWarnings, 'W', WARN_LOGICAL_SUSPECT, NULL, NULL );
      else if( pStackValType && pStackValType->cType != 'L')
         GenWarning( _szCWarnings, 'W', WARN_LOGICAL_TYPE, sType, NULL );

      /* compile time assignment value has to be released */
      pFree = pStackValType;
      debug_msg( "\n* *---JampTrue() \n", NULL );

      if( pStackValType )
      {
         pStackValType = pStackValType->pPrev;
      }

      if( pFree )
      {
         OurFree( (void *) pFree );
      }
   }

   return functions.pLast->lPCodePos - 2;
}

void Line( void ) /* generates the pcode with the currently compiled source code line */
{
  if( _bLineNumbers )
   GenPCode3( HB_P_LINE, LOBYTE( iLine ), HIBYTE( iLine ) );
}

void LineBody( void ) /* generates the pcode with the currently compiled source code line */
{
   /* This line can be placed inside a procedure or function only */
   /* except EXTERNAL */
   if( _iState != EXTERN )
   {
      if( ! _bStartProc && functions.iCount <= 1 )
      {
         GenError( _szCErrors, 'E', ERR_OUTSIDE, NULL, NULL );
      }
   }

   functions.pLast->bFlags |= FUN_STATEMENTS;
   if( _bLineNumbers )
      GenPCode3( HB_P_LINE, LOBYTE( iLine ), HIBYTE( iLine ) );
}

/**
 * Function generates passed pcode for passed variable name
 */
void VariablePCode( BYTE bPCode, char * szVarName )
{
   WORD wVar;
   PCOMSYMBOL pSym;
   PFUNCTION pOwnerFunc = NULL;

   if( _bForceMemvars )
   {  /* -v swith was used -> first check the MEMVARs */
      wVar =GetMemvarPos( szVarName );
      if( ! wVar )
      {
         wVar =GetFieldVarPos( szVarName, &pOwnerFunc );
         if( ! wVar )
            GenWarning( _szCWarnings, 'W', ((bPCode==HB_P_POPMEMVAR) ? WARN_MEMVAR_ASSUMED : WARN_AMBIGUOUS_VAR),
                  szVarName, NULL );
      }
   }
   else
   {  /* -v was not used -> default action is checking FIELDs list */
      wVar =GetFieldVarPos( szVarName, &pOwnerFunc );
      if( wVar == 0 )
      {
         wVar =GetMemvarPos( szVarName );
         if( wVar == 0 )
            GenWarning( _szCWarnings, 'W', ((bPCode==HB_P_POPMEMVAR) ? WARN_MEMVAR_ASSUMED : WARN_AMBIGUOUS_VAR),
                  szVarName, NULL );
      }
   }

   if( wVar && pOwnerFunc )
   {  /* variable is declared using FIELD statement */
      PVAR pField = GetVar( pOwnerFunc->pFields, wVar );

      if( pField->szAlias )
      {  /* the alias was specified too */
         if( bPCode == HB_P_POPMEMVAR )
            bPCode =HB_P_POPALIASEDFIELD;
         else if( bPCode == HB_P_PUSHMEMVAR )
            bPCode =HB_P_PUSHALIASEDFIELD;
         else
            /* pushing fields by reference is not allowed */
            GenError( _szCErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
         PushSymbol( yy_strdup( pField->szAlias ), 0 );
      }
      else
      {  /* this is unaliased field */
         if( bPCode == HB_P_POPMEMVAR )
            bPCode =HB_P_POPFIELD;
         else if( bPCode == HB_P_PUSHMEMVAR )
            bPCode =HB_P_PUSHFIELD;
         else
            /* pushing fields by reference is not allowed */
            GenError( _szCErrors, 'E', ERR_INVALID_REFER, szVarName, NULL );
      }
   }

   pSym = GetSymbol( szVarName, &wVar );
   if( ! pSym )
      pSym =AddSymbol( szVarName, &wVar );
   pSym->cScope |=VS_MEMVAR;
   GenPCode3( bPCode, LOBYTE( wVar ), HIBYTE( wVar ) );
}

void Message( char * szMsgName )       /* sends a message to an object */
{
   WORD wSym;
   PCOMSYMBOL pSym =GetSymbol( szMsgName, &wSym );

   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym =AddSymbol( szMsgName, &wSym );
   pSym->cScope |= FS_MESSAGE;
   GenPCode3( HB_P_MESSAGE, LOBYTE( wSym ), HIBYTE( wSym ) );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;
      char cType;

      cType = pSym->cType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = cType;
      pNewStackType->pPrev = pStackValType;
      pStackValType = pNewStackType;

      pStackValType->cType = cType;
      debug_msg( "\n***Message()\n", NULL );
   }
}

void MessageDupl( char * szMsgName )  /* fix a generated message and duplicate to an object */
{
   WORD wSetSym;
   PCOMSYMBOL pSym;
   BYTE bLoGetSym, bHiGetSym;           /* get symbol */
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   pSym =GetSymbol( szMsgName, &wSetSym );
   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym =AddSymbol( szMsgName, &wSetSym );
   pSym->cScope |= FS_MESSAGE;
                                        /* Get previously generated message */
   bLoGetSym = pFunc->pCode[ _lMessageFix + 1];
   bHiGetSym = pFunc->pCode[ _lMessageFix + 2];

   pFunc->pCode[ _lMessageFix + 1 ] = LOBYTE( wSetSym );
   pFunc->pCode[ _lMessageFix + 2 ] = HIBYTE( wSetSym );

   pFunc->lPCodePos -= 3;               /* Remove unnecessary function call  */
   Duplicate();                         /* Duplicate object                  */
   GenPCode3( HB_P_MESSAGE, bLoGetSym, bHiGetSym );
                                        /* Generate new message              */
}

void MessageFix( char * szMsgName )  /* fix a generated message to an object */
{
   WORD wSym;
   PCOMSYMBOL pSym;
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   pSym =GetSymbol( szMsgName, &wSym );
   if( ! pSym )  /* the symbol was not found on the symbol table */
      pSym =AddSymbol( szMsgName, &wSym );
   pSym->cScope |= FS_MESSAGE;

   pFunc->pCode[ _lMessageFix + 1 ] = LOBYTE( wSym );
   pFunc->pCode[ _lMessageFix + 2 ] = HIBYTE( wSym );
   pFunc->lPCodePos -= 3;        /* Remove unnecessary function call */
}

void PopId( char * szVarName ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   int iVar;

   if( pAliasId == NULL )
   {
      iVar = GetLocalVarPos( szVarName );
      if( iVar )
         GenPCode3( HB_P_POPLOCAL, LOBYTE( iVar ), HIBYTE( iVar ) );
      else
      {
         iVar = GetStaticVarPos( szVarName );
         if( iVar )
         {
            GenPCode3( HB_P_POPSTATIC, LOBYTE( iVar ), HIBYTE( iVar ) );
            functions.pLast->bFlags |= FUN_USES_STATICS;
         }
         else
         {
            VariablePCode( HB_P_POPMEMVAR, szVarName );
         }
      }
   }
   else
   {
      if( pAliasId->type == ALIAS_NAME )
      {
         if( pAliasId->alias.szAlias[0] == 'M' && pAliasId->alias.szAlias[1] == '\x0' )
         {  /* M->variable */
            VariablePCode( HB_P_POPMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( pAliasId->alias.szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( pAliasId->alias.szAlias, "MEMVAR", strlen(pAliasId->alias.szAlias) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               VariablePCode( HB_P_POPMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( pAliasId->alias.szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( pAliasId->alias.szAlias, "FIELD", strlen(pAliasId->alias.szAlias) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  FieldPCode( HB_P_POPFIELD, szVarName );
               }
               else
               {  /* database alias */
                  PushSymbol( yy_strdup( pAliasId->alias.szAlias ), 0 );
                  FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else if( pAliasId->type == ALIAS_NUMBER )
      {
         PushInteger( pAliasId->alias.iAlias );
         FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
      }
      else
         /* Alias is already placed on stack */
         FieldPCode( HB_P_POPALIASEDFIELD, szVarName );
   }


   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pVarType, pFree;
      char sType[2];

      /* Just pushed by Get...Pos() */
      pVarType = pStackValType;

      if( pVarType )
      {
         sType[0] = pVarType->cType;
         sType[1] = 0;

         /* skip back to the assigned value */
         pStackValType = pStackValType->pPrev;
      }
      else
        debug_msg( "\n***PopId() Compile time stack overflow\n", NULL );

      if( pVarType && pStackValType && pVarType->cType != ' ' && pStackValType->cType == ' ' )
         GenWarning( _szCWarnings, 'W', WARN_ASSIGN_SUSPECT, szVarName, sType );
      else if( pVarType && pStackValType && pVarType->cType != ' ' && pVarType->cType != pStackValType->cType )
         GenWarning( _szCWarnings, 'W', WARN_ASSIGN_TYPE, szVarName, sType );

      /* compile time variable has to be released */
      if( pVarType )
      {
         OurFree( (void *) pVarType );
      }

      debug_msg( "\n***--- Var at PopId()\n", NULL );

      /* compile time assignment value has to be released */
      pFree = pStackValType;
      debug_msg( "\n***--- Value at PopId()\n", NULL );

      if( pStackValType )
      {
         pStackValType = pStackValType->pPrev;
      }
      else
      {
        debug_msg( "\n***PopId() Compile time stack overflow\n", NULL );
      }

      if( pFree )
      {
         OurFree( (void *) pFree );
      }
   }
}

void PushId( char * szVarName ) /* generates the pcode to push a variable value to the virtual machine stack */
{
   int iVar;

   if( pAliasId == NULL )
   {
      if( iVarScope == VS_STATIC && functions.pLast->szName )
      {
      /* Reffering to any variable is not allowed during initialization
         * of static variable
         */
         _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
      }

      iVar = GetLocalVarPos( szVarName );
      if( iVar )
         GenPCode3( HB_P_PUSHLOCAL, LOBYTE( iVar ), HIBYTE( iVar ) );
      else
      {
         iVar = GetStaticVarPos( szVarName );
         if( iVar )
         {
            GenPCode3( HB_P_PUSHSTATIC, LOBYTE( iVar ), HIBYTE( iVar ) );
            functions.pLast->bFlags |= FUN_USES_STATICS;
         }
         else
         {
            VariablePCode( HB_P_PUSHMEMVAR, szVarName );
         }
      }
   }
   else
   {
      if( pAliasId->type == ALIAS_NAME )
      {
         if( pAliasId->alias.szAlias[0] == 'M' && pAliasId->alias.szAlias[1] == '\x0' )
         {  /* M->variable */
            VariablePCode( HB_P_PUSHMEMVAR, szVarName );
         }
         else
         {
            int iCmp = strncmp( pAliasId->alias.szAlias, "MEMVAR", 4 );
            if( iCmp == 0 )
                  iCmp = strncmp( pAliasId->alias.szAlias, "MEMVAR", strlen(pAliasId->alias.szAlias) );
            if( iCmp == 0 )
            {  /* MEMVAR-> or MEMVA-> or MEMV-> */
               VariablePCode( HB_P_PUSHMEMVAR, szVarName );
            }
            else
            {  /* field variable */
               iCmp = strncmp( pAliasId->alias.szAlias, "FIELD", 4 );
               if( iCmp == 0 )
                  iCmp = strncmp( pAliasId->alias.szAlias, "FIELD", strlen(pAliasId->alias.szAlias) );
               if( iCmp == 0 )
               {  /* FIELD-> */
                  FieldPCode( HB_P_PUSHFIELD, szVarName );
               }
               else
               {  /* database alias */
                  PushSymbol( yy_strdup( pAliasId->alias.szAlias ), 0 );
                  FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
               }
            }
         }
      }
      else if( pAliasId->type == ALIAS_NUMBER )
      {
         PushInteger( pAliasId->alias.iAlias );
         FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
      }
      else
         /* Alias is already placed on stack */
         FieldPCode( HB_P_PUSHALIASEDFIELD, szVarName );
   }

  if( _bWarnings )
  {
        PSTACK_VAL_TYPE pNewStackType;

        pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
        pNewStackType->cType = cVarType;
        pNewStackType->pPrev = pStackValType;

        pStackValType = pNewStackType;
        debug_msg( "\n***HB_P_PUSHMEMVAR\n ", NULL );
  }
}

void PushIdByRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   WORD iVar;

   if( iVarScope == VS_STATIC && functions.pLast->szName )
   {
     /* Reffering to any variable is not allowed during initialization
      * of static variable
      */
      _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
   }

   iVar = GetLocalVarPos( szVarName );
   if( iVar )
      GenPCode3( HB_P_PUSHLOCALREF, LOBYTE( iVar ), HIBYTE( iVar ) );
   else
   {
      iVar = GetStaticVarPos( szVarName );
      if( iVar )
      {
         GenPCode3( HB_P_PUSHSTATICREF, LOBYTE( iVar ), HIBYTE( iVar ) );
         functions.pLast->bFlags |= FUN_USES_STATICS;
      }
      else
      {
         VariablePCode( HB_P_PUSHMEMVARREF, szVarName );
      }
   }
}

void PushLogical( int iTrueFalse ) /* pushes a logical value on the virtual machine stack */
{
   if( iTrueFalse )
      GenPCode1( HB_P_TRUE );
   else
      GenPCode1( HB_P_FALSE );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = 'L';
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushLogical()\n", NULL );
   }
}

void PushNil( void )
{
   GenPCode1( HB_P_PUSHNIL );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = ' ' /*TODO maybe 'U'*/ ;
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushNil()\n", NULL );
   }
}

/* generates the pcode to push a double number on the virtual machine stack */
void PushDouble( double dNumber, BYTE bDec )
{
   GenPCode1( HB_P_PUSHDOUBLE );
   GenPCodeN( ( BYTE * ) &dNumber, sizeof( double ) );
   GenPCode1( bDec );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = 'N';
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushDouble()\n", NULL );
   }
}

void PushFunCall( char *szFunName )
{
   char * *pFunction;

   pFunction = (char * *)RESERVED_FUNC( szFunName );
   if( pFunction )
   {
      /* Abbreviated function name was used - change it for whole name
       */
       PushSymbol( yy_strdup( *pFunction ), 1 );
   }
   else
       PushSymbol( szFunName, 1 );
   PushNil();
}

/* generates the pcode to push a integer number on the virtual machine stack */
void PushInteger( int iNumber )
{
   if( iNumber )
      GenPCode3( HB_P_PUSHINT, LOBYTE( ( WORD ) iNumber ), HIBYTE( ( WORD ) iNumber ) );
   else
      GenPCode1( HB_P_ZERO );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = 'N';
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushInteger() %i\n ", iNumber );
   }
}

/* generates the pcode to push a long number on the virtual machine stack */
void PushLong( long lNumber )
{
   if( lNumber )
   {
      GenPCode1( HB_P_PUSHLONG );
      GenPCode1( ( ( char * ) &lNumber )[ 0 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 1 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 2 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 3 ] );
   }
   else
      GenPCode1( HB_P_ZERO );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = 'N';
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushLong()\n", NULL );
   }
}

/* generates the pcode to push a string on the virtual machine stack */
void PushString( char * szText )
{
   WORD wStrLen = strlen( szText );

   GenPCode3( HB_P_PUSHSTR, LOBYTE(wStrLen), HIBYTE(wStrLen) );
   GenPCodeN( ( BYTE * ) szText, wStrLen );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = 'C';
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushString()\n", NULL );
   }
}

/* generates the pcode to push a symbol on the virtual machine stack */
void PushSymbol( char * szSymbolName, int iIsFunction )
{
   WORD wSym;
   PCOMSYMBOL pSym;

   if( iIsFunction )
   {
      char * *pName = (char * *)RESERVED_FUNC( szSymbolName );
      /* If it is reserved function name then we should truncate
       * the requested name.
       * We have to use passed szSymbolName so we can latter deallocate it
       * (pName points to static data)
       */
      if( pName )
        szSymbolName[ strlen( *pName ) ] ='\0';
   }

   pSym = GetSymbol( szSymbolName, &wSym );
   if( ! pSym )  /* the symbol was not found on the symbol table */
   {
      pSym =AddSymbol( szSymbolName, &wSym );
      if( iIsFunction )
         AddFunCall( szSymbolName );
   }
   else
   {
      if( iIsFunction && ! GetFuncall( szSymbolName ) )
         AddFunCall( szSymbolName );
   }
   GenPCode3( HB_P_PUSHSYM, LOBYTE( wSym ), HIBYTE( wSym ) );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pNewStackType;
      char cType;

      if( iIsFunction )
        cType = pSym->cType;
      else
        cType = cVarType;

      pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
      pNewStackType->cType = cType;
      pNewStackType->pPrev = pStackValType;

      pStackValType = pNewStackType;
      debug_msg( "\n***PushSymbol()\n", NULL );
   }
}

void CheckDuplVars( PVAR pVar, char * szVarName, int iVarScope )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         if( ! (iVarScope & VS_PARAMETER) )
            --iLine;
         GenError( _szCErrors, 'E', ERR_VAR_DUPL, szVarName, NULL );
      }
      else
         pVar = pVar->pNext;
   }
}

void Dec( void )
{
   GenPCode1( HB_P_DEC );

   if( _bWarnings )
   {
      char sType[2];

      if( pStackValType )
      {
        sType[0] = pStackValType->cType;
        sType[1] = 0;
      }
      else
        debug_msg( "\n***Dec() Compile time stack overflow\n", NULL );

      if( pStackValType && pStackValType->cType == ' ' )
         GenWarning( _szCWarnings, 'W', WARN_NUMERIC_SUSPECT, NULL, NULL );
      else if( pStackValType->cType != 'N' )
         GenWarning( _szCWarnings, 'W', WARN_NUMERIC_TYPE, sType, NULL );
   }
}

void DimArray( WORD wDimensions )
{
   GenPCode3( HB_P_DIMARRAY, LOBYTE( wDimensions ), HIBYTE( wDimensions ) );
}

void Do( BYTE bParams )
{
   GenPCode3( HB_P_DO, bParams, 0 );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pFree;
      int i;

      /* Releasing the compile time stack items used as parameters to the function. */
      for( i = abs( bParams ); i > 0; i-- )
      {
         pFree = pStackValType;
         debug_msg( "\n***---Do() \n", NULL );

         if( pStackValType )
            pStackValType = pStackValType->pPrev;
         else
            debug_msg( "\n***Do() Compile time stack overflow\n", NULL );

         if( pFree )
         {
            OurFree( (void *) pFree );
         }
      }

      /* releasing the compile time Nil symbol terminator */
      pFree = pStackValType;
      debug_msg( "\n***---Do()\n", NULL );
      if( pStackValType )
         pStackValType = pStackValType->pPrev;
      else
          debug_msg( "\n***Do(2) Compile time stack overflow\n", NULL );

      if ( pFree )
      {
         OurFree( (void *) pFree );
      }

      /* releasing the compile time procedure value */
      pFree = pStackValType;
      debug_msg( "\n***---Do() \n", NULL );

      if( pStackValType )
      {
         pStackValType = pStackValType->pPrev;
      }

      if ( pFree )
      {
         OurFree( (void *) pFree );
      }
   }
}

void FixElseIfs( void * pFixElseIfs )
{
   PELSEIF pFix = ( PELSEIF ) pFixElseIfs;

   while( pFix )
   {
      JumpHere( pFix->wOffset );
      pFix = pFix->pNext;
   }
}

void FixReturns( void ) /* fixes all last defined function returns jumps offsets */
{
   if( _bWarnings && functions.pLast )
   {
      PVAR pVar;

      pVar = functions.pLast->pLocals;
      while ( pVar )
      {
         if( pVar->szName && functions.pLast->szName && ! pVar->iUsed )
            GenWarning( _szCWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, functions.pLast->szName );

         pVar = pVar->pNext;
      }

      pVar = functions.pLast->pStatics;
      while ( pVar )
      {
         if( pVar->szName && functions.pLast->szName && ! pVar->iUsed )
            GenWarning( _szCWarnings, 'W', WARN_VAR_NOT_USED, pVar->szName, functions.pLast->szName );

         pVar = pVar->pNext;
      }

      /* Clear the compile time stack values (should be empty at this point) */
      while( pStackValType )
      {
        PSTACK_VAL_TYPE pFree;

        debug_msg( "\n***Compile time stack underflow - type: %c\n", pStackValType->cType );
        pFree = pStackValType;
        pStackValType = pStackValType->pPrev;
        OurFree( (void *) pFree );
      }
      pStackValType = 0;
   }

/* TODO: check why it triggers this error in keywords.prg
   if( pLoops )
   {
     PTR_LOOPEXIT pLoop = pLoops;
     char cLine[ 64 ];

     while( pLoop->pNext )
        pLoop =pLoop->pNext;

     itoa( pLoop->wLine, cLine, 10 );
     GenError( _szCErrors, 'E', ERR_UNCLOSED_STRU, cLine, NULL );
   }
*/
}

void Function( BYTE bParams )
{
   GenPCode3( HB_P_FUNCTION, bParams, 0 );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pFree;
      int i;

      /* Releasing the compile time stack items used as parameters to the function. */
      for( i = abs( bParams ); i > 0; i-- )
      {
         pFree = pStackValType;
         debug_msg( "\n***---Function() parameter %i \n", i );

         if( pStackValType )
          pStackValType = pStackValType->pPrev;
         else
          debug_msg( "\n***Function() parameter %i Compile time stack overflow\n", i );

         if( pFree )
         {
            OurFree( (void *) pFree );
         }
      }

      /* releasing the compile time Nil symbol terminator */
      pFree = pStackValType;
      debug_msg( "\n***---NIL at Function()\n", NULL );

      if( pStackValType )
      {
         pStackValType = pStackValType->pPrev;
      }

      if ( pFree )
      {
         OurFree( (void *) pFree );
      }
   }
}

void GenArray( WORD wElements )
{
   GenPCode3( HB_P_GENARRAY, LOBYTE( wElements ), HIBYTE( wElements ) );

   if( _bWarnings )
   {
      PSTACK_VAL_TYPE pFree;
      WORD wIndex;

      /* Releasing the stack items used by the _GENARRAY (other than the 1st element). */
      for( wIndex = wElements; wIndex > 1; wIndex-- )
      {
         pFree = pStackValType;
         debug_msg( "\n***---element %i at GenArray()\n", wIndex );

          if( pStackValType )
            pStackValType = pStackValType->pPrev;
          else
            debug_msg( "\n***GenArray() Compile time stack overflow\n", NULL );

          if ( pFree )
          {
            OurFree( (void *) pFree );
          }
      }

      if( wElements == 0 )
      {
          PSTACK_VAL_TYPE pNewStackType;

          pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
          pNewStackType->cType = 'A';
          pNewStackType->pPrev = pStackValType;

          pStackValType = pNewStackType;
          debug_msg( "\n***empty array in GenArray()\n ", NULL );
      }

      /* Using the either remaining 1st element place holder or a new item if empty array. */
      if( pStackValType )
         pStackValType->cType = 'A';
      else
         debug_msg( "\n***ArrrayGen() Compile time stack overflow\n", NULL );
   }
}

void GenPCode1( BYTE byte )
{
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   /* Releasing value consumed by HB_P_ARRAYPUT */
   if( _bWarnings )
   {
      if( byte == HB_P_PUSHSELF )
      {
         PSTACK_VAL_TYPE pNewStackType;

         pNewStackType = ( STACK_VAL_TYPE * )OurMalloc( sizeof( STACK_VAL_TYPE ) );
         pNewStackType->cType = 'O';
         pNewStackType->pPrev = pStackValType;

         pStackValType = pNewStackType;
         debug_msg( "\n***HB_P_PUSHSELF\n", NULL );
      }
      else if( byte == HB_P_ARRAYPUT )
      {
         PSTACK_VAL_TYPE pFree;

         /* Releasing compile time assignment value */
         pFree = pStackValType;
         debug_msg( "\n***---ArrayPut()\n", NULL );

         if( pStackValType )
            pStackValType = pStackValType->pPrev;
         else
            debug_msg( "\n***HB_P_ARRAYPUT Compile time stack overflow\n", NULL );

         if( pFree )
         {
            OurFree( (void *) pFree );
         }

         /* Releasing compile time array element index value */
         pFree = pStackValType;
         debug_msg( "\n***---HB_P_ARRAYPUT\n", NULL );

         if( pStackValType )
            pStackValType = pStackValType->pPrev;
         else
            debug_msg( "\n***HB_P_ARRAYPUT2 Compile time stack overflow\n", NULL );

         if( pFree )
         {
            OurFree( (void *) pFree );
         }
      }
      else if( byte == HB_P_POP || byte == HB_P_RETVALUE || byte == HB_P_FORTEST || byte == HB_P_ARRAYAT )
      {
         PSTACK_VAL_TYPE pFree;

         pFree = pStackValType;
         debug_msg( "\n***---HB_P_POP / HB_P_RETVALUE / HB_P_FORTEST / HB_P_ARRAYAT pCode: %i\n", byte );

         if( pStackValType )
            pStackValType = pStackValType->pPrev;
         else
            debug_msg( "\n***pCode: %i Compile time stack overflow\n", byte );

         if( pFree )
         {
            OurFree( (void *) pFree );
         }
      }
      else if( byte == HB_P_MULT || byte == HB_P_DIVIDE || byte == HB_P_MODULUS || byte == HB_P_POWER || byte == HB_P_NEGATE )
      {
        PSTACK_VAL_TYPE pOperand1 = 0, pOperand2;
        char sType1[2], sType2[2];

        /* 2nd. Operand (stack top)*/
        pOperand2 = pStackValType;

        /* skip back to the 1st. operand */
        if( pOperand2 )
        {
            pOperand1 = pOperand2->pPrev;
            sType2[0] = pOperand1->cType;
            sType2[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_MULT pCode: %i Compile time stack overflow\n", byte );

        /* skip back to the 1st. operand */
        if( pOperand1 )
        {
            sType1[0] = pOperand1->cType;
            sType1[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_MULT2 pCode: %i Compile time stack overflow\n", byte );

        if( pOperand1 && pOperand1->cType != 'N' && pOperand1->cType != ' ' )
          GenWarning( _szCWarnings, 'W', WARN_NUMERIC_TYPE, sType1, NULL );
        else if( pOperand1 && pOperand1->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_NUMERIC_SUSPECT, NULL, NULL );

        if( pOperand2 && pOperand2->cType != 'N' && pOperand2->cType != ' ' )
          GenWarning( _szCWarnings, 'W', WARN_NUMERIC_TYPE, sType2, NULL );
        else if( pOperand2 && pOperand2->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_NUMERIC_SUSPECT, NULL, NULL );

         /* compile time 2nd. operand has to be released */
        if( pOperand2 )
        {
          OurFree( (void *) pOperand2 );
        }

        /* compile time 1st. operand has to be released *but* result will be pushed and assumed numeric type */
        pStackValType = pOperand1;
        pStackValType->cType = 'N';
      }
      else if( byte == HB_P_PLUS || byte == HB_P_MINUS )
      {
        PSTACK_VAL_TYPE pOperand1 = 0, pOperand2;
        char sType1[2], sType2[2], cType = ' ';

        /* 2nd. Operand (stack top)*/
        pOperand2 = pStackValType;

        /* skip back to the 1st. operand */
        if( pOperand2 )
        {
            pOperand1 = pOperand2->pPrev;
            sType2[0] = pOperand2->cType;
            sType2[1] = 0;
        }
        else
          debug_msg( "\n***HB_P_PLUS / HB_P_MINUS Compile time stack overflow\n", NULL );

        if( pOperand1 )
        {
            sType1[0] = pOperand1->cType;
            sType1[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_PLUS / HB_P_MINUS2 Compile time stack overflow\n", NULL );

        if( pOperand1 && pOperand2 && pOperand1->cType != ' ' && pOperand2->cType != ' ' && pOperand1->cType != pOperand2->cType )
          GenWarning( _szCWarnings, 'W', WARN_OPERANDS_INCOMPATBLE, sType1, sType2 );
        else if( pOperand1 && pOperand2 && pOperand2->cType != ' ' && pOperand1->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType2, NULL );
        else if( pOperand1 && pOperand2 && pOperand1->cType != ' ' && pOperand2->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType1, NULL );
        else
          cType = pOperand1->cType;

         /* compile time 2nd. operand has to be released */
        if( pOperand2 )
        {
          OurFree( (void *) pOperand2 );
        }

        /* compile time 1st. operand has to be released *but* result will be pushed and type as calculated */
        /* Resetting */
        pStackValType = pOperand1;
        pStackValType->cType = cType;
      }
      else if( byte == HB_P_EQUAL || byte == HB_P_LESS ||  byte == HB_P_GREATER || byte == HB_P_INSTRING || byte == HB_P_LESSEQUAL || byte == HB_P_GREATEREQUAL || byte == HB_P_EXACTLYEQUAL || byte == HB_P_NOTEQUAL )
      {
        PSTACK_VAL_TYPE pOperand1 = 0, pOperand2;
        char sType1[2], sType2[2];

        /* 2nd. Operand (stack top)*/
        pOperand2 = pStackValType;

        /* skip back to the 1st. operand */
        if( pOperand2 )
        {
            pOperand1 = pOperand2->pPrev;
            sType2[0] = pOperand2->cType;
            sType2[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_EQUAL pCode: %i Compile time stack overflow\n", byte );

        if( pOperand1 )
        {
            sType1[0] = pOperand1->cType;
            sType1[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_EQUAL2 pCode: %i Compile time stack overflow\n", byte );

        if( pOperand1 && pOperand2 && pOperand1->cType != ' ' && pOperand2->cType != ' ' && pOperand1->cType != pOperand2->cType )
          GenWarning( _szCWarnings, 'W', WARN_OPERANDS_INCOMPATBLE, sType1, sType2 );
        else if( pOperand1 && pOperand2 && pOperand2->cType != ' ' && pOperand1->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType2, NULL );
        else if( pOperand1 && pOperand2 && pOperand1->cType != ' ' && pOperand2->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_OPERAND_SUSPECT, sType1, NULL );

         /* compile time 2nd. operand has to be released */
        if( pOperand2 )
        {
           OurFree( (void *) pOperand2 );
        }

        /* compile time 1st. operand has to be released *but* result will be pushed and of type logical */
        if( pOperand1 )
            pOperand1->cType = 'L';

        /* Resetting */
        pStackValType = pOperand1;
      }
      else if( byte == HB_P_NOT )
      {
        char sType[2];

        if( pStackValType )
        {
            sType[0] = pStackValType->cType;
            sType[1] = 0;
        }
        else
            debug_msg( "\n***HB_P_NOT Compile time stack overflow\n", NULL );

        if( pStackValType && pStackValType->cType == ' ' )
          GenWarning( _szCWarnings, 'W', WARN_LOGICAL_SUSPECT, NULL, NULL );
        else if( pStackValType && pStackValType->cType != 'L' )
          GenWarning( _szCWarnings, 'W', WARN_LOGICAL_TYPE, sType, NULL );

        /* compile time 1st. operand has to be released *but* result will be pushed and assumed logical */
        if( pStackValType )
            pStackValType->cType = 'L';
      }
   }

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = (BYTE *) OurMalloc( PCODE_CHUNK );
      pFunc->lPCodeSize = PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 1 )
         pFunc->pCode = (BYTE *)OurRealloc( pFunc->pCode, pFunc->lPCodeSize += PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte;
}

void GenPCode3( BYTE byte1, BYTE byte2, BYTE byte3 )
{
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = (BYTE *) OurMalloc( PCODE_CHUNK );
      pFunc->lPCodeSize = PCODE_CHUNK;
      pFunc->lPCodePos  = 0;
   }
   else
      if( ( pFunc->lPCodeSize - pFunc->lPCodePos ) < 3 )
         pFunc->pCode = (BYTE *) OurRealloc( pFunc->pCode, pFunc->lPCodeSize += PCODE_CHUNK );

   pFunc->pCode[ pFunc->lPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->lPCodePos++ ] = byte3;
}

void GenPCodeN( BYTE * pBuffer, WORD wSize )
{
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )   /* has been created the memory block to hold the pcode ? */
   {
      pFunc->lPCodeSize = ((wSize / PCODE_CHUNK) +1) * PCODE_CHUNK;
      pFunc->pCode      = (BYTE *) OurMalloc( pFunc->lPCodeSize );
      pFunc->lPCodePos  = 0;
   }
   else if( pFunc->lPCodePos + wSize > pFunc->lPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->lPCodeSize +=( ((wSize / PCODE_CHUNK) +1) * PCODE_CHUNK );
      pFunc->pCode = (BYTE *) OurRealloc( pFunc->pCode, pFunc->lPCodeSize );
   }

   memcpy( pFunc->pCode+pFunc->lPCodePos, pBuffer, wSize );
   pFunc->lPCodePos +=wSize;
}

char * SetData( char * szMsg ) /* generates an underscore-symbol name for a data assignment */
{
   char * szResult = ( char * ) OurMalloc( strlen( szMsg ) + 2 );

   strcpy( szResult, "_" );
   strcat( szResult, szMsg );

   return szResult;
}

/*
 * Start a new fake-function that will hold pcodes for a codeblock
*/
void CodeBlockStart()
{
   PFUNCTION pFunc = FunctionNew( NULL, FS_STATIC );

   pFunc->pOwner       = functions.pLast;
   pFunc->wStaticsBase = functions.pLast->wStaticsBase;

   functions.pLast = pFunc;
}

void CodeBlockEnd()
{
  PFUNCTION pCodeblock;   /* pointer to the current codeblock */
  PFUNCTION pFunc;        /* poiter to a function that owns a codeblock */
  WORD wSize;
  WORD wLocals = 0;   /* number of referenced local variables */
  WORD wPos;
  PVAR pVar, pFree;

  pCodeblock = functions.pLast;

  /* return to pcode buffer of function/codeblock in which the current
   * codeblock was defined
   */
  functions.pLast = pCodeblock->pOwner;

  /* find the function that owns the codeblock */
  pFunc = pCodeblock->pOwner;
  while( pFunc->pOwner )
    pFunc = pFunc->pOwner;
  pFunc->bFlags |= ( pCodeblock->bFlags & FUN_USES_STATICS );

  /* generate a proper codeblock frame with a codeblock size and with
   * a number of expected parameters
   */
  /*QUESTION: would be 64kB enough for a codeblock size?
   * we are assuming now a WORD for a size of codeblock
   */

  /* Count the number of referenced local variables */
  pVar = pCodeblock->pStatics;
  while( pVar )
  {
    pVar = pVar->pNext;
    ++wLocals;
  }

  /*NOTE:  8 = HB_P_PUSHBLOCK + WORD(size) + WORD(wParams) + WORD(wLocals) +_ENDBLOCK */
  wSize =( WORD ) pCodeblock->lPCodePos +8 +wLocals*2;

  GenPCode3( HB_P_PUSHBLOCK, LOBYTE(wSize), HIBYTE(wSize) );
  GenPCode1( LOBYTE(pCodeblock->wParamCount) );
  GenPCode1( HIBYTE(pCodeblock->wParamCount) );
  GenPCode1( LOBYTE(wLocals) );
  GenPCode1( HIBYTE(wLocals) );

  /* generate the table of referenced local variables */
  pVar = pCodeblock->pStatics;
  while( wLocals-- )
  {
    wPos = GetVarPos( pFunc->pLocals, pVar->szName );
    GenPCode1( LOBYTE(wPos) );
    GenPCode1( HIBYTE(wPos) );

    pFree = pVar;
    OurFree( (void *) pFree->szName );
    pVar = pVar->pNext;
    OurFree( (void *) pFree );
  }

  GenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos );
  GenPCode1( HB_P_ENDBLOCK ); /* finish the codeblock */

  /* this fake-function is no longer needed */
  OurFree( (void *) pCodeblock->pCode );
  pVar = pCodeblock->pLocals;
  while( pVar )
  {
    if( _bWarnings && pFunc->szName && pVar->szName && ! pVar->iUsed )
       GenWarning( _szCWarnings, 'W', WARN_BLOCKVAR_NOT_USED, pVar->szName, pFunc->szName );

    /* free used variables */
    pFree = pVar;
    OurFree( (void *) pFree->szName );
    pVar = pVar->pNext;
    OurFree( (void *) pFree );
  }
  OurFree( (void *) pCodeblock );

  if( _bWarnings )
  {
     if( pStackValType )
        /* reusing the place holder of the result value */
        pStackValType->cType = 'B';
     else
        debug_msg( "\n***CodeBlockEnd() Compile time stack overflow\n", NULL );
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

  pVar = functions.pLast->pFields;
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
  PVAR pVar = functions.pLast->pFields;

  while( pVar )
  {
    ++iFields;
    pVar = pVar->pNext;
  }

  return iFields;
}

/*
 * Start of definition of static variable
 * We are using here the special function _pInitFunc which will store
 * pcode needed to initialize all static variables declared in PRG module.
 * pOwner member will point to a function where the static variable is
 * declared:
 * TODO: support for static variables in codeblock
 */
void StaticDefStart( void )
{
  iVarScope =VS_STATIC;
  Line();

  functions.pLast->bFlags |= FUN_USES_STATICS;
  if( ! _pInitFunc )
  {
      _pInitFunc =FunctionNew( yy_strdup("(_INITSTATICS)"), FS_INIT );
      _pInitFunc->pOwner =functions.pLast;
      _pInitFunc->bFlags =FUN_USES_STATICS | FUN_PROCEDURE;
      _pInitFunc->cScope =FS_INIT | FS_EXIT;
      functions.pLast =_pInitFunc;
      PushInteger( 1 );   /* the number of static variables is unknown now */
      GenPCode3( HB_P_STATICS, 0, 0 );
      GenPCode3( HB_P_SFRAME, 0, 0 );     /* frame for statics variables */
  }
  else
  {
      _pInitFunc->pOwner =functions.pLast;
      functions.pLast =_pInitFunc;
  }

}

/*
 * End of definition of static variable
 * Return to previously pcoded function.
 */
void StaticDefEnd( WORD wCount )
{
  functions.pLast =_pInitFunc->pOwner;
  _pInitFunc->pOwner =NULL;
  _wStatics += wCount;
  iVarScope =VS_LOCAL;

  if( _bWarnings )
  {
     PSTACK_VAL_TYPE pFree;

     if( pStackValType )
     {
        pFree = pStackValType;
        debug_msg( "\n***---%i in StaticeDefEnd()\n", _wStatics );

        pStackValType = pStackValType->pPrev;
        OurFree( (void *) pFree );
     }
     else
        debug_msg( "\n***StaticDefEnd() Compile time stack overflow\n", NULL );
  }
}

/*
 * This function checks if we are initializing a static variable.
 * It should be called only in case when the parser have recognized any
 * function or method invocation.
 */
void StaticAssign( void )
{
   if( iVarScope == VS_STATIC && functions.pLast->szName )
      /* function call is allowed if it is inside a codeblock
       */
      _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
}

/*
 * This function stores the position in pcode buffer where the FOR/WHILE
 * loop starts. It will be used to fix any LOOP/EXIT statements
 */
static void LoopStart( void )
{
  PTR_LOOPEXIT pLoop = ( PTR_LOOPEXIT ) OurMalloc( sizeof(LOOPEXIT) );

  if( pLoops )
  {
    PTR_LOOPEXIT pLast =pLoops;

    while( pLast->pNext )
      pLast =pLast->pNext;
    pLast->pNext =pLoop;
  }
  else
    pLoops = pLoop;

  pLoop->pNext       =NULL;
  pLoop->pExitList   =NULL;
  pLoop->pLoopList   =NULL;
  pLoop->wOffset =functions.pLast->lPCodePos;  /* store the start position */
  pLoop->wLine   =iLine;
}

/*
 * Stores the position of LOOP statement to fix it later at the end of loop
 */
static void LoopLoop( void )
{
  PTR_LOOPEXIT pLast, pLoop = (PTR_LOOPEXIT) OurMalloc( sizeof( LOOPEXIT ) );

  pLoop->pLoopList =NULL;
  pLoop->wOffset =functions.pLast->lPCodePos;  /* store the position to fix */

  pLast =pLoops;
  while( pLast->pNext )
    pLast =pLast->pNext;

  while( pLast->pLoopList )
    pLast =pLast->pLoopList;

  pLast->pLoopList =pLoop;

  Jump( 0 );
}

/*
 * Stores the position of EXIT statement to fix it later at the end of loop
 */
static void LoopExit( void )
{
  PTR_LOOPEXIT pLast, pLoop = (PTR_LOOPEXIT) OurMalloc( sizeof( LOOPEXIT ) );

  pLoop->pExitList =NULL;
  pLoop->wOffset =functions.pLast->lPCodePos;  /* store the position to fix */

  pLast =pLoops;
  while( pLast->pNext )
    pLast =pLast->pNext;

  while( pLast->pExitList )
    pLast =pLast->pExitList;

  pLast->pExitList =pLoop;

  Jump( 0 );
}

/*
 * Fixes the LOOP statement
 */
static void LoopHere( void )
{
  PTR_LOOPEXIT pLoop = pLoops, pFree;

  while( pLoop->pNext )
    pLoop = pLoop->pNext;

  pLoop =pLoop->pLoopList;
  while( pLoop )
  {
    JumpHere( pLoop->wOffset +1 );
    pFree = pLoop;
    pLoop = pLoop->pLoopList;
    OurFree( (void *) pFree );
  }
}

/*
 * Fixes the EXIT statements and releases memory allocated for current loop
 */
static void LoopEnd( void )
{
  PTR_LOOPEXIT pExit, pLoop = pLoops, pLast = pLoops, pFree;

  while( pLoop->pNext )
  {
    pLast = pLoop;
    pLoop = pLoop->pNext;
  }

  pExit =pLoop->pExitList;
  while( pExit )
  {
    JumpHere( pExit->wOffset +1 );
    pFree = pExit;
    pExit = pExit->pExitList;
    OurFree( (void *) pFree );
  }

  pLast->pNext = NULL;
  if( pLoop == pLoops )
    pLoops = NULL;
  OurFree( (void *) pLoop );
}


void * OurMalloc( LONG lSize )
{
   void * pMem = malloc( lSize );

   if( ! pMem )
      yyerror( "\nCan't allocate memory!\n" );

   return pMem;
}

void * OurRealloc( void * p, LONG lSize )
{
   void * pMem = realloc( p, lSize );

   if( ! pMem )
      yyerror( "\nCan't reallocate memory!\n" );

   return pMem;
}

void OurFree( void *ptr )
{
  if( ptr )
    free( ptr );
  else
  {
    printf( "ERROR FREE" );
    exit(4);
  }
}



char * yy_strupr( char * p )
{
   char * p1;

   for ( p1 = p; * p1; p1++ )
      * p1 = toupper( * p1 );
   return( p );
}

char * yy_strdup( char *p )
{
  char *pDup;
  int iLen;

  iLen = strlen( p ) +1;
  pDup = (char *) OurMalloc( iLen );
  memcpy( pDup, p, iLen );

  return pDup;
}


#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

void GenPortObj( char *szFileName, char *szName )
{
   PFUNCTION pFunc /*= functions.pFirst */;
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w, wLen, wVar;
   LONG lPCodePos;
   LONG lPad;
   LONG lSymbols;
   BOOL bEndProcReq;
   ULONG ulCodeLength;
   FILE * yyc;             /* file handle for C output */

   HB_SYMBOL_UNUSED( szName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
     printf( "Error opening file %s\n", szFileName );
     return;
   }

   if( ! _bQuiet )
      printf( "\ngenerating portable object file...\n" );

   /* writes the symbol table */

   if( ! _bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   lSymbols = 0;                /* Count number of symbols */
   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }
   fputc( (BYTE) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( (BYTE) ( ( lSymbols >> 8  ) & 255 ), yyc );
   fputc( (BYTE) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( (BYTE) ( ( lSymbols >> 24 ) & 255 ), yyc );

   pSym = symbols.pFirst;
   if( ! _bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   while( pSym )
   {
      fputs( pSym->szName, yyc );
      fputc( 0, yyc );
      if( pSym->cScope != FS_MESSAGE )
         fputc( pSym->cScope, yyc );
      else
         fputc( 0, yyc );

      /* specify the function address if it is a defined function or a
         external called function */
      if( GetFunction( pSym->szName ) ) /* is it a defined function ? */
      {
         fputc( SYM_FUNC, yyc );
      }
      else
      {
         if( GetFuncall( pSym->szName ) )
         {
            fputc( SYM_EXTERN, yyc );
         }
         else
         {
            fputc( SYM_NOLINK, yyc );
         }
      }
      pSym = pSym->pNext;
   }

   pFunc = functions.pFirst;
   if( ! _bStartProc )
      pFunc = pFunc->pNext;

   lSymbols = 0;                /* Count number of symbols */
   while( pFunc )
   {
      lSymbols++;
      pFunc = pFunc->pNext;
   }
   fputc( (BYTE) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( (BYTE) ( ( lSymbols >> 8  ) & 255 ), yyc );
   fputc( (BYTE) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( (BYTE) ( ( lSymbols >> 24 ) & 255 ), yyc );

   /* Generate functions data
    */
   pFunc = functions.pFirst;
   if( ! _bStartProc )
     pFunc = pFunc->pNext; /* No implicit starting procedure */

   while( pFunc )
   {
      fputs( pFunc->szName, yyc );
      fputc( 0, yyc );
      /* We will have to add HB_P_ENDPROC in cases when RETURN statement
       * was not used in a function/procedure - this is why we have to reserve
       * one additional byte
       */
      ulCodeLength =pFunc->lPCodePos +1;
      fputc( (BYTE) ( ( ulCodeLength       ) & 255 ), yyc ); /* Write size */
      fputc( (BYTE) ( ( ulCodeLength >> 8  ) & 255 ), yyc );
      fputc( (BYTE) ( ( ulCodeLength >> 16 ) & 255 ), yyc );
      fputc( (BYTE) ( ( ulCodeLength >> 24 ) & 255 ), yyc );

/*      printf( "Creating output for %s\n", pFunc->szName ); */

      lPCodePos = 0;
      lPad = 0;                         /* Number of bytes optimized */
      bEndProcReq = TRUE;
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case HB_P_AND:
            case HB_P_ARRAYAT:
            case HB_P_ARRAYPUT:
            case HB_P_DEC:
            case HB_P_DIVIDE:
            case HB_P_DUPLICATE:
            case HB_P_DUPLTWO:
            case HB_P_ENDBLOCK:
            case HB_P_EQUAL:
            case HB_P_EXACTLYEQUAL:
            case HB_P_FALSE:
            case HB_P_FORTEST:
            case HB_P_FUNCPTR:
            case HB_P_GREATER:
            case HB_P_GREATEREQUAL:
            case HB_P_INC:
            case HB_P_INSTRING:
            case HB_P_LESS:
            case HB_P_LESSEQUAL:
            case HB_P_MINUS:
            case HB_P_MODULUS:
            case HB_P_MULT:
            case HB_P_NEGATE:
            case HB_P_NOT:
            case HB_P_NOTEQUAL:
            case HB_P_OR:
            case HB_P_PLUS:
            case HB_P_POP:
            case HB_P_POPALIAS:
            case HB_P_POWER:
            case HB_P_PUSHALIAS:
            case HB_P_PUSHNIL:
            case HB_P_PUSHSELF:
            case HB_P_RETVALUE:
            case HB_P_SWAPALIAS:
            case HB_P_TRUE:
            case HB_P_ZERO:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case HB_P_DIMARRAY:
            case HB_P_DO:
            case HB_P_FUNCTION:
            case HB_P_GENARRAY:
            case HB_P_JUMP:
            case HB_P_JUMPFALSE:
            case HB_P_JUMPTRUE:
            case HB_P_LINE:
            case HB_P_POPLOCAL:
            case HB_P_POPSTATIC:
            case HB_P_PUSHINT:
            case HB_P_PUSHLOCAL:
            case HB_P_PUSHLOCALREF:
            case HB_P_PUSHSTATIC:
            case HB_P_PUSHSTATICREF:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case HB_P_ENDPROC:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 if( lPCodePos == pFunc->lPCodePos )
                     bEndProcReq = FALSE;
                 break;

            case HB_P_FRAME:
                 /* update the number of local variables */
                 {
                    PVAR pLocal  = pFunc->pLocals;
                    BYTE bLocals = 0;

                    while( pLocal )
                    {
                       pLocal = pLocal->pNext;
                       bLocals++;
                    }

                    if( bLocals || pFunc->wParamCount )
                    {
                        fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                        fputc(   (BYTE)(bLocals - pFunc->wParamCount), yyc );
                        fputc(   (BYTE)(pFunc->wParamCount), yyc );
                        lPCodePos += 2;
                    }
                    else
                    {
                       lPad += 3;
                       lPCodePos += 3;
                    }
                 }
                 break;

            case HB_P_PUSHSYM:
            case HB_P_MESSAGE:
            case HB_P_POPMEMVAR:
            case HB_P_PUSHMEMVAR:
            case HB_P_PUSHMEMVARREF:
            case HB_P_POPFIELD:
            case HB_P_PUSHFIELD:
            case HB_P_POPALIASEDFIELD:
            case HB_P_PUSHALIASEDFIELD:
                 fputc( pFunc->pCode[ lPCodePos ], yyc );
                 wVar =FixSymbolPos( pFunc->pCode[ lPCodePos+1 ] + 256 *pFunc->pCode[ lPCodePos+2 ] );
                 fputc( LOBYTE( wVar ), yyc );
                 fputc( HIBYTE( wVar ), yyc );
                 lPCodePos +=3;
                 break;

            case HB_P_PARAMETER:
                 fputc( pFunc->pCode[ lPCodePos ], yyc );
                 wVar = FixSymbolPos( pFunc->pCode[ lPCodePos+1 ] + 256 * pFunc->pCode[ lPCodePos+2 ] );
                 fputc( LOBYTE( wVar ), yyc );
                 fputc( HIBYTE( wVar ), yyc );
                 fputc( pFunc->pCode[ lPCodePos+3 ], yyc );
                 lPCodePos +=4;
                 break;

            case HB_P_PUSHBLOCK:
                 wVar = * ( ( WORD *) &( pFunc->pCode [ lPCodePos + 5 ] ) );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 /* create the table of referenced local variables */
                 while( wVar-- )
                 {
                    fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                    fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 }
                 break;

            case HB_P_PUSHDOUBLE:
                 {
                    int i;
                    fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                    for( i = 0; i < sizeof( double ); ++i )
                       fputc( ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ], yyc );
                    fputc( pFunc->pCode[ lPCodePos + sizeof( double ) ], yyc );
                    lPCodePos += sizeof( double ) + 1;
                 }
                 break;

            case HB_P_PUSHLONG:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case HB_P_PUSHSTR:
                 wLen = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fputc( pFunc->pCode[ lPCodePos     ], yyc );
                 fputc( pFunc->pCode[ lPCodePos + 1 ], yyc );
                 fputc( pFunc->pCode[ lPCodePos + 2 ], yyc );
                 lPCodePos +=3;
                 while( wLen-- )
                 {
                    fputc( pFunc->pCode[ lPCodePos ++ ], yyc );
                 }
                 break;

            case HB_P_SFRAME:
                 /* we only generate it if there are statics used in this function */
                 if( pFunc->bFlags & FUN_USES_STATICS )
                 {
                    GetSymbol( _pInitFunc->szName, &w );
                    w = FixSymbolPos( w );
                    fputc( pFunc->pCode[ lPCodePos ], yyc );
                    fputc( LOBYTE( w ), yyc );
                    fputc( HIBYTE( w ), yyc );
                 }
                 else
                    lPad += 3;
                 lPCodePos += 3;
                 break;

            case HB_P_STATICS:
                 GetSymbol( _pInitFunc->szName, &w );
                 w = FixSymbolPos( w );
                 fputc( pFunc->pCode[ lPCodePos ], yyc );
                 fputc( LOBYTE( w ), yyc );
                 fputc( HIBYTE( w ), yyc );
                 lPCodePos += 3;
                 break;

            default:
                 printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
                 lPCodePos = pFunc->lPCodePos;
                 break;
         }
      }

      if( bEndProcReq )
         fputc( HB_P_ENDPROC, yyc );
      else
      {
         /* HB_P_ENDPROC was the last opcode: we have to fill the byte
          * reserved earlier
          */
         lPad++;
      }
      for( ; lPad; lPad-- )
      {
         /* write additional bytes to agree with stored earlier
          * function/procedure size
          */
         fputc( 0, yyc );
      }
      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   if( ! _bQuiet )
      printf( "%s -> done!\n", szFileName );
}

typedef struct
{
   char * cFuncName;                /* function name              */
   int    iMinParam;                /* min no of parms it needs   */
                                    /* iMinParam = -1, means no checking */
   int    iMaxParam;                /* max no of parms need       */
} FUNCINFO, * PFUNCINFO;

static FUNCINFO _StdFun[] = {
{ "AADD"      , 2, 2 },
{ "ABS"       , 1, 1 },
{ "ASC"       , 1, 1 },
{ "AT"        , 2, 2 },
{ "BOF"       , 0, 0 },
{ "BREAK"     , 0, 1 },
{ "CDOW"      , 1, 1 },
{ "CHR"       , 1, 1 },
{ "CMONTH"    , 1, 1 },
{ "COL"       , 0, 0 },
{ "CTOD"      , 1, 1 },
{ "DATE"      , 0, 0 },
{ "DAY"       , 1, 1 },
{ "DELETED"   , 0, 0 },
{ "DEVPOS"    , 2, 2 },
{ "DOW"       , 1, 1 },
{ "DTOC"      , 1, 1 },
{ "DTOS"      , 1, 1 },
{ "EMPTY"     , 1, 1 },
{ "EOF"       , 0, 0 },
{ "EXP"       , 1, 1 },
{ "FCOUNT"    , 0, 0 },
{ "FIELDNAME" , 1, 1 },
{ "FILE"      , 1, 1 },
{ "FLOCK"     , 0, 0 },
{ "FOUND"     , 0, 0 },
{ "INKEY"     , 0, 2 },
{ "INT"       , 1, 1 },
{ "LASTREC"   , 0, 0 },
{ "LEN"       , 1, 1 },
{ "LOG"       , 1, 1 },
{ "LOWER"     , 1, 1 },
{ "LTRIM"     , 1, 1 },
{ "MAX"       , 2, 2 },
{ "MIN"       , 2, 2 },
{ "MONTH"     , 1, 1 },
{ "PCOL"      , 0, 0 },
{ "PCOUNT"    , 0, 0 },
{ "PROW"      , 0, 0 },
{ "RECCOUNT"  , 0, 0 },
{ "RECNO"     , 0, 0 },
{ "REPLICATE" , 2, 2 },
{ "RLOCK"     , 0, 0 },
{ "ROUND"     , 2, 2 },
{ "ROW"       , 0, 0 },
{ "RTRIM"     , 1, 1 },
{ "SECONDS"   , 0, 0 },
{ "SELECT"    , 0, 1 },
{ "SETPOS"    , 2, 2 },
{ "SPACE"     , 1, 1 },
{ "SQRT"      , 1, 1 },
{ "STR"       , 1, 3 },
{ "SUBSTR"    , 2, 3 },
{ "TIME"      , 0, 0 },
{ "TRANSFORM" , 2, 2 },
{ "TRIM"      , 1, 1 },
{ "TYPE"      , 1, 1 },
{ "UPPER"     , 1, 1 },
{ "VAL"       , 1, 1 },
{ "VALTYPE"   , 1, 1 },
{ "WORD"      , 1, 1 },
{ "YEAR"      , 1, 1 },
{ 0           , 0, 0 }
};

void CheckArgs( char *cFuncCall, int iArgs )
{
   FUNCINFO *f = _StdFun;
   int i = 0;
   int iPos = -1;
   int iCmp;

   while( f[i].cFuncName )
   {
     iCmp = strncmp( cFuncCall, f[i].cFuncName, 4 );
     if( iCmp == 0 )
         iCmp = strncmp( cFuncCall, f[i].cFuncName, strlen(cFuncCall) );
     if( iCmp == 0 )
     {
         iPos = i;
         break;
     }
     else
         ++i;
   }

   if( iPos >= 0 && ( f[iPos].iMinParam != -1 ) )
     if( iArgs < f[iPos].iMinParam || iArgs > f[iPos].iMaxParam )
     {
        char *szMsg = ( char * ) OurMalloc( 30 );

        sprintf( szMsg, " Passed: %i Expected: %i", iArgs, f[iPos].iMinParam );
        GenError( _szCErrors, 'E', ERR_CHECKING_ARGS, cFuncCall, szMsg );

        /* Clipper way */
        /* GenError( _szCErrors, 'E', ERR_CHECKING_ARGS, cFuncCall, NULL ); */
     }
}

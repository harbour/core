%{
/*
 * $Id$
 *
 * Harbour compiler (yacc rules and actions)
 * Build 21 proposal: spring 1999
 * Usage: bison -d -v harbour.y  You may find Bison at www.harbour.project.org
 */

#define BUILD         21    /* current harbour.y build */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <malloc.h>     /* required for allocating and freeing memory */
#include "hbsetup.h"    /* main configuration file */
#include "pcode.h"      /* pcode values */
#include "types.h"      /* our defined types */
#include "compiler.h"
#include "hberrors.h"

#ifdef __BORLANDC__
   #define HAVE_STRUPR
#endif

#ifndef HAVE_STRUPR
   #include <ctype.h>
   char *strupr( char *p );
#endif

/* TODO: #define this for various platforms */
#define PATH_DELIMITER "/\\"
#define IS_PATH_SEP( c ) (strchr(PATH_DELIMITER, (c))!=NULL)

#define OPT_DELIMITER  "/-"
#define IS_OPT_SEP( c ) (strchr(OPT_DELIMITER, (c))!=NULL)

extern FILE * yyin;     /* currently yacc parsed file */
extern int iLine;       /* currently parsed file line number */

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

typedef struct _PATHNAMES { /* the list of pathnames to search with #include */
  char *szPath;
  struct _PATHNAMES *pNext;
} PATHNAMES;

int Include( char * szFileName, PATHNAMES *pSearchPath );  /* end #include support */

/*
 * flags for bFlags member
*/
#define FUN_STATEMENTS    1 /* Function have at least one executable statement */
#define FUN_USES_STATICS  2 /* Function uses static variables */
#define FUN_PROCEDURE     4 /* This is a procedure that shouldn't return value */
#define FUN_ILLEGAL_INIT  8 /* Attempt to initialize static variable with a function call */

/* pcode chunks bytes size */
#define PCODE_CHUNK   100

typedef struct __ELSEIF
{
   WORD wOffset;
   struct __ELSEIF * pNext;
} _ELSEIF, * PELSEIF;      /* support structure for else if pcode fixups */

typedef struct __RETURN
{
   WORD wOffset;
   struct __RETURN * pNext;
} _RETURN, * PRETURN;      /* support structure for multiple returns from a function */

typedef struct _LOOPEXIT
{
  WORD wOffset;
  WORD wLine;
  struct _LOOPEXIT *pLoopList;
  struct _LOOPEXIT *pExitList;
  struct _LOOPEXIT *pNext;
} LOOPEXIT, * PLOOPEXIT;  /* support structure for EXIT and LOOP statements */
static void LoopStart( void );
static void LoopEnd( void );
static void LoopLoop( void );
static void LoopExit( void );
static void LoopHere( void );

typedef struct             /* support for filenames */
{
  char _buffer[ _POSIX_PATH_MAX+3 ];
  char *path;
  char *name;
  char *extension;
} FILENAME;

typedef struct __EXTERN
{
   char * szName;
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

FILENAME *SplitFilename( char * );  /* splits filename into a path, a name and an extension */
char *MakeFilename( char *, FILENAME *);  /* joins a path, a name an an extension int filename */

/* lex & yacc related prototypes */
void yyerror( char * ); /* parsing error management function */
int yylex( void );      /* main lex token function, called by yyparse() */
int yyparse( void );    /* main yacc parsing function */
#ifdef __cplusplus
extern "C" int yywrap( void );
#else
int yywrap( void );     /* manages the EOF of current processed file */
#endif
void AddDefine( char * szDefine, char * szValue ); /* add a new Lex define from the command line */

void * yy_create_buffer( FILE *, int ); /* yacc functions to manage multiple files */
#ifdef __cplusplus
typedef struct yy_buffer_state *YY_BUFFER_STATE;
void yy_switch_to_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
void yy_delete_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
#else
void yy_switch_to_buffer( void * ); /* yacc functions to manage multiple files */
void yy_delete_buffer( void * ); /* yacc functions to manage multiple files */
#endif

#if 0
static void __yy_memcpy( char * from, char * to, int count ); /* Bison prototype */
#endif

/* production related functions */
PFUNCTION AddFunCall( char * szFuntionName );
void AddExtern( char * szExternName ); /* defines a new extern name */
void AddSearchPath( char *, PATHNAMES * * ); /* add pathname to a search list */
void AddVar( char * szVarName ); /* add a new param, local, static variable to a function definition or a public or private */
PCOMSYMBOL AddSymbol( char * szSymbolName );
void CheckDuplVars( PVAR pVars, char * szVarName, int iVarScope ); /*checks for duplicate variables definitions */
void Dec( void );                  /* generates the pcode to decrement the latest value on the virtual machine stack */
void DimArray( WORD wDimensions ); /* instructs the virtual machine to build an array with wDimensions */
void Do( BYTE bParams );      /* generates the pcode to execute a Clipper function discarding its result */
void Duplicate( void ); /* duplicates the virtual machine latest stack latest value and places it on the stack */
void DupPCode( WORD wStart ); /* duplicates the current generated pcode from an offset */
void FixElseIfs( void * pIfElseIfs ); /* implements the ElseIfs pcode fixups */
void FixReturns( void ); /* fixes all last defined function returns jumps offsets */
void Function( BYTE bParams ); /* generates the pcode to execute a Clipper function pushing its result */
PFUNCTION FunctionNew( char *, char );  /* creates and initialises the _FUNC structure */
void FunDef( char * szFunName, char cScope, int iType ); /* starts a new Clipper language function definition */
void GenArray( WORD wElements ); /* instructs the virtual machine to build an array and load elemnst from the stack */
void * GenElseIf( void * pFirstElseIf, WORD wOffset ); /* generates a support structure for elseifs pcode fixups */
void GenExterns( void ); /* generates the symbols for the EXTERN names */
void GenReturn( WORD wOffset );  /* generates a return offset to later on fill it with the proper exiting pcode address */
PFUNCTION GetFuncall( char * szFunName ); /* locates a previously defined called function */
PVAR GetVar( PVAR pVars, WORD wOrder ); /* returns a variable if defined or zero */
WORD GetVarPos( PVAR pVars, char * szVarName ); /* returns the order + 1 of a variable if defined or zero */
int GetLocalVarPos( char * szVarName ); /* returns the order + 1 of a local variable */
PCOMSYMBOL GetSymbol( char * szSymbolName ); /* returns a symbol pointer from the symbol table */
PCOMSYMBOL GetSymbolOrd( WORD wSymbol );   /* returns a symbol based on its index on the symbol table */
WORD GetSymbolPos( char * szSymbolName ); /* returns the index + 1 of a symbol on the symbol table */
void Inc( void );                       /* generates the pcode to increment the latest value on the virtual machine stack */
WORD Jump( int iOffset );               /* generates the pcode to jump to a specific offset */
WORD JumpFalse( int iOffset );          /* generates the pcode to jump if false */
void JumpHere( int iOffset );           /* returns the pcode pos where to set a jump offset */
void JumpThere( int iOffset, WORD wTo ); /* sets a jump offset */
WORD JumpTrue( int iOffset );           /* generates the pcode to jump if true */
void Line( void );                      /* generates the pcode with the currently compiled source code line */
void LineBody( void );                  /* generates the pcode with the currently compiled source code line */
void Message( char * szMsgName );       /* sends a message to an object */
void MessageFix( char * szMsgName );    /* fix a generated message to an object */
void MessageDupl( char * szMsgName );   /* fix a one generated message to an object and duplicate */
void PopId( char * szVarName );         /* generates the pcode to pop a value from the virtual machine stack onto a variable */
void PushDouble( double fNumber, BYTE bDec ); /* Pushes a number on the virtual machine stack */
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
void SetFrame( void );              /* generates the proper _FRAME values */

/* support for FIELD declaration */
void SetAlias( char *, int );
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

extern int iLine;           /* currently compiled source code line */

int iVarScope = 0;          /* holds the scope for next variables to be defined */
#define VS_LOCAL   0        /* different values for iVarScope */
#define VS_STATIC  1
#define VS_PARAMETER 2
#define VS_FIELD      3
#define VS_MEMVAR     4

/* Table with parse errors */
char * _szErrors[] = { "Statement not allowed outside of procedure or function",
                       "Redefinition of procedure or function: \'%s\'",
                       "Duplicate variable declaration: \'%s\'",
                       "%s declaration follows executable statement",
                       "Outer codeblock variable is out of reach: \'%s\'",
                       "Invalid numeric format '.'",
                       "Unterminated string: \'%s\'",
                       "Redefinition of predefined function %s: \'%s\'",
                       "Illegal initializer: \'%s\'",
                       "Can\'t open #include file: \'%s\'",
                       "ENDIF does not match IF",
                       "ENDDO does not match WHILE",
                       "ENDCASE does not match DO CASE",
                       "NEXT does not match FOR",
                       "ELSE does not match IF",
                       "ELSEIF does not match IF",
                       "Syntax error: \'%s\'",
                       "Unclosed control structures at line: %i",
                       "%s statement with no loop in sight",
                       "Syntax error: \'%s\' in: \'%s\'"
                     };

/* Table with reserved functions names
 * NOTE: THIS TABLE MUST BE SORTED ALPHABETICALLY
*/
static const char * _szReservedFun[] = {
  "AADD"    ,
  "ABS"     ,
  "ASC"     ,
  "AT"      ,
  "BOF"     ,
  "BREAK"   ,
  "CDOW"    ,
  "CHR"     ,
  "CMONTH"  ,
  "COL"     ,
  "CTOD"    ,
  "DATE"    ,
  "DAY"     ,
  "DELETED" ,
  "DEVPOS"  ,
  "DO"      ,
  "DOW"     ,
  "DTOC"    ,
  "DTOS"    ,
  "EMPTY"   ,
  "EOF"     ,
  "EXP"     ,
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
int _iStartProc = 1;       /* holds if we need to create the starting procedure */
int _iLineNumbers = 1;     /* holds if we need pcodes with line numbers */
int _iQuiet = 0;           /* quiet mode */
int _iSyntaxCheckOnly = 0; /* syntax check only */
int _iLanguage = LANG_C;   /* default Harbour generated output language */
int _iRestrictSymbolLength = 0; /* generate 10 chars max symbols length */
int _iShortCuts = 1;       /* .and. & .or. expressions shortcuts */
short int _iAltSymbolTableInit = 0; /* alternative method of symbol table initialization */
WORD _wSeqCounter   = 0;
WORD _wForCounter   = 0;
WORD _wIfCounter    = 0;
WORD _wWhileCounter = 0;
WORD _wCaseCounter  = 0;
LONG _lMessageFix   = 0;  /* Position of the message which needs to be changed */
#ifdef HARBOUR_OBJ_GENERATION
int _iObj32 = 0;           /* generate OBJ 32 bits */
#endif
WORD _wStatics = 0;        /* number of defined statics variables on the PRG */
PRETURN pReturns = 0;      /* list of multiple returns from a function */
PEXTERN pExterns = 0;
PLOOPEXIT pLoops = 0;
PATHNAMES *_pIncludePath = NULL;

%}

%union                  /* special structure used by lex and yacc to share info */
{
   char * string;       /* to hold a string returned by lex */
   int    iNumber;      /* to hold a number returned by lex */
   long   lNumber;      /* to hold a long number returned by lex */
   struct
   {
      double dNumber;   /* to hold a double number returned by lex */
      unsigned char bDec; /* to hold the number of decimal points in the value */
   } dNum;
   void * pVoid;        /* to hold any memory structure we may need */
};

%token FUNCTION PROCEDURE IDENTIFIER RETURN NIL DOUBLE INASSIGN INTEGER INTLONG
%token LOCAL STATIC IIF IF ELSE ELSEIF END ENDIF LITERAL TRUEVALUE FALSEVALUE
%token INCLUDE EXTERN INIT EXIT AND OR NOT PUBLIC EQ NE1 NE2
%token INC DEC ALIAS DOCASE CASE OTHERWISE ENDCASE ENDDO MEMVAR
%token WHILE EXIT LOOP END FOR NEXT TO STEP LE GE FIELD IN PARAMETERS
%token PLUSEQ MINUSEQ MULTEQ DIVEQ POWER EXPEQ MODEQ EXITLOOP
%token PRIVATE BEGINSEQ BREAK RECOVER USING DO WITH SELF

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

%type <string>  IDENTIFIER LITERAL FunStart MethStart IdSend ObjectData
%type <dNum>    DOUBLE 
%type <iNumber> ArgList ElemList ExpList FunCall FunScope IncDec Logical Params ParamList
%type <iNumber> INTEGER BlockExpList Argument IfBegin VarId VarList MethParams ObjFunCall
%type <iNumber> MethCall BlockList FieldList 
%type <lNumber> INTLONG WhileBegin BlockBegin
%type <pVoid>   IfElseIf Cases

%%

Main       : { Line(); } Source       { if( ! _iQuiet ) printf( "\nsyntax ok\n" ); }
           ;

Source     : Crlf
           | Extern
           | Include
           | VarDefs
           | FieldsDef
           | MEMVAR IdentList
           | Function
           | Statement
           | Source Crlf
           | Source Extern
           | Source Include
           | Source Function
           | Source { LineBody(); } Statement
           | Source VarDefs
           | Source FieldsDef
           | Source MEMVAR IdentList
           ;

Include    : NE1 INCLUDE LITERAL { if( ! Include( $3, _pIncludePath ) )
                                      GenError( ERR_CANT_OPEN_INCLUDE, $3, NULL );
                                 } Crlf
           ;

Extern     : EXTERN ExtList Crlf
           ;

ExtList    : IDENTIFIER                               { AddExtern( $1 ); }
           | ExtList ',' IDENTIFIER                   { AddExtern( $3 ); }
           ;

Function   : FunScope FUNCTION  IDENTIFIER { FunDef( $3, $1, 0 ); } Params Crlf { SetFrame(); }
           | FunScope PROCEDURE IDENTIFIER { FunDef( $3, $1, FUN_PROCEDURE ); } Params Crlf { SetFrame(); }
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

ParamList  : IDENTIFIER                            { AddVar( $1 ); $$ = 1; }
           | ParamList ',' IDENTIFIER              { AddVar( $3 ); $$++; }
           ;

Statements : Statement
           | Statements { Line(); } Statement
           ;

Statement  : ExecFlow Crlf                             {}
           | FunCall Crlf                              { Do( $1 ); }
           | AliasFunc Crlf                            {}
           | IfInline Crlf                             { GenPCode1( _POP ); }
           | ObjectMethod Crlf                         { GenPCode1( _POP ); }
           | VarUnary Crlf                             { GenPCode1( _POP ); }
           | VarAssign Crlf                            { GenPCode1( _POP ); }

           | IDENTIFIER '=' Expression Crlf            { PopId( $1 ); }
           | VarId ArrayIndex '=' Expression Crlf      { GenPCode1( _ARRAYPUT ); GenPCode1( _POP ); }
           | FunArrayCall '=' Expression Crlf          { GenPCode1( _ARRAYPUT ); GenPCode1( _POP ); }
           | IdSend IDENTIFIER '='      { Message( SetData( $2 ) ); } Expression Crlf  { Function( 1 ); }
           | ObjectData ArrayIndex '=' Expression Crlf    { GenPCode1( _ARRAYPUT ); GenPCode1( _POP ); }
           | ObjectMethod ArrayIndex '=' Expression Crlf  { GenPCode1( _ARRAYPUT ); GenPCode1( _POP ); }

           | BREAK Crlf
           | BREAK Expression Crlf
           | RETURN Crlf              { GenReturn( Jump( 0 ) ); }
           | RETURN Expression Crlf   { GenPCode1( _RETVALUE ); GenReturn( Jump ( 0 ) ); }
           | PUBLIC VarList Crlf
           | PRIVATE VarList Crlf
           | PARAMETERS IdentList Crlf
           | EXITLOOP Crlf            { LoopExit(); }
           | LOOP Crlf                { LoopLoop(); }
           | DoProc Crlf
           ;

FunCall    : FunStart ')'                { $$ = 0; }
           | FunStart ArgList ')'        { $$ = $2; }
           ;

FunStart   : IDENTIFIER '('              { StaticAssign(); PushSymbol( $1, 1 ); PushNil(); $$ = $1; }
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
           ;

Argument   : Expression                        {}
           | '@' IDENTIFIER                    { PushIdByRef( $2 ); }
           | '@' IDENTIFIER '(' ')'            { PushSymbol( $2, 1 ); GenPCode1( _FUNCPTR ); }
           ;

MethParams : /* empty */                       { $$ = 0; }
           | ArgList                           { $$ = $1; }
           ;

ObjectData : IdSend IDENTIFIER                     { $$ = $2; _lMessageFix = functions.pLast->lPCodePos; Message( $2 ); Function( 0 ); }
           | VarId ArrayIndex ':' IDENTIFIER       { $$ = $4; _lMessageFix = functions.pLast->lPCodePos; Message( $4 ); Function( 0 ); }
           | ObjFunCall IDENTIFIER                 { $$ = $2; _lMessageFix = functions.pLast->lPCodePos; Message( $2 ); Function( 0 ); }
           | ObjFunArray  ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectMethod ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectData   ':' IDENTIFIER           { $$ = $3; _lMessageFix = functions.pLast->lPCodePos; Message( $3 ); Function( 0 ); }
           | ObjectData ArrayIndex ':' IDENTIFIER  { $$ = $4; _lMessageFix = functions.pLast->lPCodePos; Message( $4 ); Function( 0 ); }
           ;

ObjectMethod : IdSend IDENTIFIER { Message( $2 ); } '(' MethParams ')' { Function( $5 ); }
           | VarId ArrayIndex ':' MethCall         { Function( $4 ); }
           | ObjFunCall MethCall                   { Function( $2 ); }
           | ObjFunArray  ':' MethCall             { Function( $3 ); }
           | ObjectData   ':' MethCall             { Function( $3 ); }
           | ObjectData ArrayIndex ':' MethCall    { Function( $4 ); }
           | ObjectMethod ':' MethCall             { Function( $3 ); }
           ;

IdSend     : IDENTIFIER ':'                       { PushId( $1 ); $$ = $1; }
           ;

ObjFunCall : FunCall ':'                      { Function( $1 ); $$ = $1; }
           ;

FunArrayCall : FunCall { Function( $1 ); } ArrayIndex
           ;

ObjFunArray : FunArrayCall ':' { GenPCode1( _ARRAYAT ); }
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
           | AliasExp                         {}
           | '(' Expression ')'               {}
           | '(' ExpList ')'                  {}
           | SELF                             { GenPCode1( _PUSHSELF ); }
           ;

IfInline   : IIF '(' Expression ',' { $<iNumber>$ = JumpFalse( 0 ); }
                IfInlExp ',' { $<iNumber>$ = Jump( 0 ); JumpHere( $<iNumber>5 ); }
                IfInlExp ')' { JumpHere( $<iNumber>8 ); }

           | IF '(' Expression ',' { $<iNumber>$ = JumpFalse( 0 ); }
                IfInlExp ',' { $<iNumber>$ = Jump( 0 ); JumpHere( $<iNumber>5 ); }
                IfInlExp ')' { JumpHere( $<iNumber>8 ); }
           ;

IfInlExp   : /* nothing => nil */            { PushNil(); }
           | Expression
           ;

Macro      : '&' Variable
           | '&' '(' Expression ')'
           ;

AliasExp   : IDENTIFIER ALIAS IDENTIFIER                 {}
           | '(' Expression ')' ALIAS IDENTIFIER         {}
           | AliasFunc                                   {}
           ;

AliasFunc  : IDENTIFIER ALIAS '(' ExpList ')'            {}
           | '(' Expression ')' ALIAS '(' ExpList ')'    {}
           ;

VarUnary   : IDENTIFIER IncDec %prec POST    { PushId( $1 ); Duplicate(); $2 ? Inc(): Dec(); PopId( $1 ); }
           | IncDec IDENTIFIER %prec PRE     { PushId( $2 ); $1 ? Inc(): Dec(); Duplicate(); PopId( $2 ); }
           | VarId ArrayIndex IncDec %prec POST { DupPCode( $1 ); GenPCode1( _ARRAYAT ); $3 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); $3 ? Dec(): Inc(); }
           | IncDec VarId ArrayIndex %prec PRE  { DupPCode( $2 ); GenPCode1( _ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall IncDec %prec POST { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $2 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); $2 ? Dec(): Inc(); }
           | IncDec FunArrayCall %prec PRE  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); }
           | ObjectData IncDec %prec POST   { MessageDupl( SetData( $1 ) ); Function( 0 ); $2 ? Inc(): Dec(); Function( 1 ); $2 ? Dec(): Inc(); }
           | IncDec ObjectData %prec PRE    { MessageDupl( SetData( $2 ) ); Function( 0 ); $1 ? Inc(): Dec(); Function( 1 ); }
           | ObjectData ArrayIndex IncDec %prec POST { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $3 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); $3 ? Dec(): Inc(); }
           | IncDec ObjectData ArrayIndex %prec PRE  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex IncDec %prec POST { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $3 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); $3 ? Dec(): Inc(); }
           | IncDec ObjectMethod ArrayIndex %prec PRE  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); $1 ? Inc(): Dec(); GenPCode1( _ARRAYPUT ); }
           ;

IncDec     : INC                             { $$ = 1; }
           | DEC                             { $$ = 0; }
           ;

Variable   : VarId                     {}
           | VarId ArrayIndex          { GenPCode1( _ARRAYAT ); }
           | FunArrayCall              { GenPCode1( _ARRAYAT ); }
           | ObjectData                {}
           | ObjectData ArrayIndex     { GenPCode1( _ARRAYAT ); }
           | ObjectMethod ArrayIndex   { GenPCode1( _ARRAYAT ); }
           ;

VarId      : IDENTIFIER        { $$ = functions.pLast->lPCodePos; PushId( $1 ); }
           ;

ArrayIndex : '[' IndexList ']'
           | ArrayIndex { GenPCode1( _ARRAYAT ); } '[' IndexList ']'
           ;

IndexList  : Expression
           | IndexList { GenPCode1( _ARRAYAT ); } ',' Expression
           ;

VarAssign  : IDENTIFIER INASSIGN Expression { PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER PLUSEQ   { PushId( $1 ); } Expression { GenPCode1( _PLUS ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MINUSEQ  { PushId( $1 ); } Expression { GenPCode1( _MINUS ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MULTEQ   { PushId( $1 ); } Expression { GenPCode1( _MULT ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER DIVEQ    { PushId( $1 ); } Expression { GenPCode1( _DIVIDE ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER EXPEQ    { PushId( $1 ); } Expression { GenPCode1( _POWER ); PopId( $1 ); PushId( $1 ); }
           | IDENTIFIER MODEQ    { PushId( $1 ); } Expression { GenPCode1( _MODULUS ); PopId( $1 ); PushId( $1 ); }
           | VarId ArrayIndex INASSIGN Expression { GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex PLUSEQ   { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _PLUS    ); GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex MINUSEQ  { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MINUS   ); GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex MULTEQ   { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MULT    ); GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex DIVEQ    { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _DIVIDE  ); GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex EXPEQ    { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _POWER   ); GenPCode1( _ARRAYPUT ); }
           | VarId ArrayIndex MODEQ    { DupPCode( $1 ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MODULUS ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall INASSIGN Expression { GenPCode1( _ARRAYPUT ); }
           | FunArrayCall PLUSEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _PLUS    ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall MINUSEQ  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _MINUS   ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall MULTEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _MULT    ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall DIVEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _DIVIDE  ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall EXPEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _POWER   ); GenPCode1( _ARRAYPUT ); }
           | FunArrayCall MODEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); }  Expression { GenPCode1( _MODULUS ); GenPCode1( _ARRAYPUT ); }
           | ObjectData INASSIGN { MessageFix ( SetData( $1 ) ); } Expression { Function( 1 ); }
           | ObjectData PLUSEQ   { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _PLUS );    Function( 1 ); }
           | ObjectData MINUSEQ  { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _MINUS );   Function( 1 ); }
           | ObjectData MULTEQ   { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _MULT );    Function( 1 ); }
           | ObjectData DIVEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _DIVIDE );  Function( 1 ); }
           | ObjectData EXPEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _POWER );   Function( 1 ); }
           | ObjectData MODEQ    { MessageDupl( SetData( $1 ) ); Function( 0 ); } Expression { GenPCode1( _MODULUS ); Function( 1 ); }
           | ObjectData ArrayIndex INASSIGN Expression      { GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex PLUSEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _PLUS    ); GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex MINUSEQ  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MINUS   ); GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex MULTEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MULT    ); GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex DIVEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _DIVIDE  ); GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex EXPEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _POWER   ); GenPCode1( _ARRAYPUT ); }
           | ObjectData ArrayIndex MODEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MODULUS ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex INASSIGN Expression    { GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex PLUSEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _PLUS    ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex MINUSEQ  { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MINUS   ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex MULTEQ   { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MULT    ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex DIVEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _DIVIDE  ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex EXPEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _POWER   ); GenPCode1( _ARRAYPUT ); }
           | ObjectMethod ArrayIndex MODEQ    { GenPCode1( _DUPLTWO ); GenPCode1( _ARRAYAT ); } Expression { GenPCode1( _MODULUS ); GenPCode1( _ARRAYPUT ); }
           | AliasExp INASSIGN Expression                 {}
           ;

Operators  : Expression '='    Expression   { GenPCode1( _EQUAL ); } /* compare */
           | Expression '+'    Expression   { GenPCode1( _PLUS ); }
           | Expression '-'    Expression   { GenPCode1( _MINUS ); }
           | Expression '*'    Expression   { GenPCode1( _MULT ); }
           | Expression '/'    Expression   { GenPCode1( _DIVIDE ); }
           | Expression '<'    Expression   { GenPCode1( _LESS ); }
           | Expression '>'    Expression   { GenPCode1( _GREATER ); }
           | Expression '$'    Expression   { GenPCode1( _INSTRING ); }
           | Expression '%'    Expression   { GenPCode1( _MODULUS ); }
           | Expression LE     Expression   { GenPCode1( _LESSEQUAL ); }
           | Expression GE     Expression   { GenPCode1( _GREATEREQUAL ); }
           | Expression AND { if( _iShortCuts ){ Duplicate(); $<iNumber>$ = JumpFalse( 0 ); } }
                       Expression { GenPCode1( AND_ ); if( _iShortCuts ) JumpHere( $<iNumber>3 ); }
           | Expression OR { if( _iShortCuts ){ Duplicate(); $<iNumber>$ = JumpTrue( 0 ); } }
                       Expression { GenPCode1( OR_ ); if( _iShortCuts ) JumpHere( $<iNumber>3 ); }
           | Expression EQ     Expression   { GenPCode1( _EXACTLYEQUAL ); }
           | Expression NE1    Expression   { GenPCode1( _NOTEQUAL ); }
           | Expression NE2    Expression   { GenPCode1( _NOTEQUAL ); }
           | Expression POWER  Expression   { GenPCode1( _POWER ); }
           | NOT Expression                 { GenPCode1( _NOT ); }
           | '-' Expression %prec UNARY     { GenPCode1( _NEGATE ); }
           | '+' Expression %prec UNARY
           | VarAssign
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
           | ','                            { PushNil(); GenPCode1( _POP ); PushNil(); $$ = 2; }
           | BlockExpList ','                        { GenPCode1( _POP ); PushNil(); $$++; }
           | BlockExpList ',' { GenPCode1( _POP ); } Expression  { $$++; }
           ;

BlockList  : IDENTIFIER                           { AddVar( $1 ); $$ = 1; }
           | BlockList ',' IDENTIFIER             { AddVar( $3 ); $$++; }
           ;

ExpList    : Expression %prec POST                    { $$ = 1; }
           | ExpList { GenPCode1( _POP ); } ',' Expression %prec POST  { $$++; }
           ;

VarDefs    : LOCAL { iVarScope = VS_LOCAL; Line(); } VarList Crlf   { SetFrame(); }
           | STATIC { StaticDefStart() } VarList Crlf { StaticDefEnd( $<iNumber>3 ); }
           ;

VarList    : VarDef                                  { $$ = 1; }
           | VarList ',' VarDef                      { $$++; }
           ;

VarDef     : IDENTIFIER                              { AddVar( $1 ); }
           | IDENTIFIER INASSIGN Expression          { AddVar( $1 ); PopId( $1 ); }
           | IDENTIFIER '[' ExpList ']'              { AddVar( $1 ); DimArray( $3 ); }
           ;

FieldsDef  : FIELD { iVarScope =VS_FIELD; } FieldList Crlf { LineBody(); }
           ;

FieldList  : IDENTIFIER                              { $$=FieldsCount(); AddVar( $1 ); }
           | FieldList ',' IDENTIFIER                { AddVar( $3 ); }
           | FieldList IN IDENTIFIER { SetAlias( $3, $<iNumber>1 ); }
           ;

IdentList  : IDENTIFIER                              {}
           | IdentList ',' IDENTIFIER                {}
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
             StepExpr Crlf                       { GenPCode1( _FORTEST ); $<iNumber>$ = JumpTrue( 0 ); /*PushId( $2 )*/; }
             ForStatements                       { LoopHere(); PushId( $2 ); GenPCode1( _PLUS ); PopId( $2 ); Jump( $<iNumber>5 - functions.pLast->lPCodePos ); JumpHere( $<iNumber>11 ); LoopEnd(); }
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
           | RECOVER Crlf
           | RECOVER Crlf Statements
           | RECOVER USING IDENTIFIER Crlf
           | RECOVER USING IDENTIFIER Crlf Statements
           ;

DoProc     : DO IDENTIFIER { PushSymbol( $2, 1 ); PushNil(); Do( 0 ); }
           | DO IDENTIFIER { PushSymbol( $2, 1 ); PushNil(); } WITH ArgList { Do( $5 ); }
           | WHILE { PushSymbol( "WHILE", 1 ); PushNil(); } WITH ArgList { Do( $4 ); }
           ;

Crlf       : '\n'
           | ';'
           | '\n' Crlf
           | ';' Crlf
           ;

%%

void yyerror( char * s )
{
   printf( "\n%s at line %i\n", s, --iLine );
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

void GenError( int iError, char * szError1, char * szError2 )
{
  char * szLine = ( char * ) OurMalloc( 160 );      /*2 lines of text */
  printf( "\r%s(%i) ", files.pLast->szFileName, iLine );
  printf( "Error C%i  ", iError );
  sprintf( szLine, _szErrors[ iError - 1 ], szError1, szError2 );
  printf( "%s\n\n", szLine );
  exit( 1 );
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
   char *szOutPath ="";
   FILENAME *pFileName =NULL;

   if( argc > 1 )
   {
      /* Command line options */
      while( iArg < argc )
      {
         if( IS_OPT_SEP(argv[ iArg ][ 0 ]))
         {
            switch( argv[ iArg ][ 1 ] )
            {
               case '1':
                    if( argv[ iArg ][ 2 ] == '0' )
                       _iRestrictSymbolLength = 1;
                    break;

               case 'd':
               case 'D':   /* defines a Lex #define from the command line */
                    {
                       unsigned int i = 0;
                       char * szDefText = strdup( argv[ iArg ] + 2 );
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
                       char * szUpper = strupr( strdup( &argv[ iArg ][ 2 ] ) );
                       if( ! strcmp( szUpper, "OBJ32" ) )
                          _iObj32 = 1;
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
                    _iLineNumbers = 0;
                    break;

               case 'n':
               case 'N':
                    _iStartProc = 0;
                    break;

               case 'o':
               case 'O':
                    szOutPath = argv[ iArg ]+2;
                    break;

               case 'q':
               case 'Q':
                    _iQuiet = 1;
                    break;

               case 's':
               case 'S':
                    _iSyntaxCheckOnly = 1;
                    break;

               case 't':
               case 'T':
                    _iAltSymbolTableInit = 1;
                    break;

               case 'y':
               case 'Y':
                    yydebug = TRUE;
                    break;

               case 'z':
               case 'Z':
                    _iShortCuts = 0;
                    break;

               default:
                    printf( "Invalid command line option: %s\n",
                            &argv[ iArg ][ 1 ] );
                    break;
            }
         }
         else
            pFileName =SplitFilename( argv[ iArg ] );
         iArg++;
      }

      if( !_iQuiet )
        printf( "Harbour compiler\nbuild %i Spring 1999\n", BUILD );

      if( pFileName )
      {
        if( !pFileName->extension )
          pFileName->extension =".prg";
        MakeFilename( szFileName, pFileName );
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

          pPath = szInclude = strdup( szInclude );
          while( (pDelim = strchr( pPath, OS_PATH_LIST_SEPARATOR )) != NULL )
          {
            *pDelim ='\0';
            AddSearchPath( pPath, &_pIncludePath );
            pPath =pDelim + 1;
          }
          AddSearchPath( pPath, &_pIncludePath );
         }

         FunDef( strupr( strdup( pFileName->name ) ), FS_PUBLIC, FUN_PROCEDURE );
         yyparse();
         FixReturns();       /* fix all previous function returns offsets */
         GenExterns();       /* generates EXTERN symbols names */
         fclose( yyin );
         files.pLast =NULL;

#ifdef HARBOUR_OBJ_GENERATION
         if( ! _iSyntaxCheckOnly && ! _iObj32 )
#else
         if( ! _iSyntaxCheckOnly )
#endif
         {
            if( _pInitFunc )
            {
              PCOMSYMBOL pSym;

              /* Fix the number of static variables */
              _pInitFunc->pCode[ 1 ] =LOBYTE( _wStatics );
              _pInitFunc->pCode[ 2 ] =HIBYTE( _wStatics );
              _pInitFunc->wStaticsBase =_wStatics;

              pSym =AddSymbol( _pInitFunc->szName );
              pSym->cScope |= _pInitFunc->cScope;
              functions.pLast->pNext = _pInitFunc;
              ++functions.iCount;
            }

            /* we create a the output file */
            pFileName->path = szOutPath;
            switch( _iLanguage )
            {
               case LANG_C:
                    pFileName->extension =".c";
                    MakeFilename( szFileName, pFileName );
                    GenCCode( szFileName, pFileName->name );
                    break;

               case LANG_JAVA:
                    pFileName->extension =".java";
                    MakeFilename( szFileName, pFileName );
                    GenJava( szFileName, pFileName->name );
                    break;

               case LANG_PASCAL:
                    pFileName->extension =".pas";
                    MakeFilename( szFileName, pFileName );
                    GenPascal( szFileName, pFileName->name );
                    break;

               case LANG_RESOURCES:
                    pFileName->extension =".rc";
                    MakeFilename( szFileName, pFileName );
                    GenRC( szFileName, pFileName->name );
                    break;

               case LANG_PORT_OBJ:
                    pFileName->extension =".hrb";
                    MakeFilename( szFileName, pFileName );
                    GenPortObj( szFileName, pFileName->name );
                    break;
            }
         }
#ifdef HARBOUR_OBJ_GENERATION
         if( _iObj32 )
         {
            pFileName->extension = ".obj";
            MakeFilename( szFileName, pFileName );
            GenObj32( szFileName, pFileName->name );
         }
#endif
      }
      else
      {
         printf( "Can't open input file: %s\n", szFileName );
         iStatus = 1;
      }
      OurFree( pFileName );
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
          "\t/d<id>[=<val>]\t#define <id>\n"
#ifdef HARBOUR_OBJ_GENERATION
          "\t/f\t\tgenerated object file\n"
          "\t\t\t /fobj32 --> Windows/Dos 32 bits OBJ\n"
#endif
          "\t/g\t\tgenerated output language\n"
          "\t\t\t /gc (C default) --> <file.c>\n"
          "\t\t\t /gj (Java)      --> <file.java>\n"
          "\t\t\t /gp (Pascal)    --> <file.pas>\n"
          "\t\t\t /gr (Resources) --> <file.rc>\n"
          "\t/i<path>\tadd #include file search path\n"
          "\t/l\t\tsuppress line number information\n"
          "\t/n\t\tno implicit starting procedure\n"
          "\t/o<path>\tobject file drive and/or path\n"
          "\t/q\t\tquiet\n"
          "\t/s\t\tsyntax check only\n"
          "\t/t\t\talternative method of symbol table initialization\n"
          "\t/y\t\ttrace lex & yacc activity\n"
          "\t/z\t\tsupress .and. & .or. shortcutting\n"
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

    if( !(pFileName->extension[ 0 ] == '.' || pFileName->name[ iLen-1 ] == '.') )
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
      pPath =pPath->pNext;
    pPath->pNext =(PATHNAMES *)OurMalloc( sizeof(PATHNAMES) );
    pPath =pPath->pNext;
  }
  else
  {
    *pSearchList =pPath =(PATHNAMES *)OurMalloc( sizeof(PATHNAMES) );
  }
  pPath->pNext  = NULL;
  pPath->szPath = szPath;
}


PFUNCTION AddFunCall( char * szFunctionName )
{
   PFUNCTION pFunc = ( PFUNCTION ) OurMalloc( sizeof( _FUNC ) );

   pFunc->szName = szFunctionName;
   pFunc->cScope = 0;
   pFunc->pNext  = 0;

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

   if( ! _iStartProc && functions.iCount <= 1 && iVarScope == VS_LOCAL )
   {
     /* Variable declaration is outside of function/procedure body.
        In this case only STATIC and PARAMETERS variables are allowed. */
      --iLine;
      GenError( ERR_OUTSIDE, NULL, NULL );
   }

   /* check if we are declaring local/static variable after some
    * executable statements
    * Note: FIELD and MEMVAR are executable statements
    */
   if( (functions.pLast->bFlags & FUN_STATEMENTS) && !(iVarScope == VS_FIELD || iVarScope == VS_MEMVAR) )
   {
      --iLine;
      GenError( ERR_FOLLOWS_EXEC, (iVarScope==VS_LOCAL?"LOCAL":"STATIC"), NULL );
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
        GenError( ERR_ILLEGAL_INIT, szVarName, pFunc->szName );
   }

   /* Check if a declaration of duplicated variable name is requested */
   if( pFunc->szName )
   {
      /* variable defined in a function/procedure */
      CheckDuplVars( pFunc->pFields, szVarName, iVarScope );
      CheckDuplVars( pFunc->pStatics, szVarName, iVarScope );
   }
   else
     /* variable defined in a codeblock */
     iVarScope =VS_PARAMETER;
   CheckDuplVars( pFunc->pLocals, szVarName, iVarScope );

   pVar = ( PVAR ) OurMalloc( sizeof( VAR ) );
   pVar->szName  = szVarName;
   pVar->szAlias = NULL;
   pVar->pNext   = NULL;

   switch( iVarScope )
   {
      case VS_LOCAL:
      case VS_PARAMETER:
           if( ! pFunc->pLocals )
              pFunc->pLocals = pVar;
           else
           {
              pLastVar = pFunc->pLocals;
              while( pLastVar->pNext )
                 pLastVar = pLastVar->pNext;
              pLastVar->pNext = pVar;
           }
           if( iVarScope == VS_PARAMETER )
              ++functions.pLast->wParamCount;
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

PCOMSYMBOL AddSymbol( char * szSymbolName )
{
   PCOMSYMBOL pSym = ( PCOMSYMBOL ) OurMalloc( sizeof( COMSYMBOL ) );

   pSym->szName = szSymbolName;
   pSym->cScope = 0;
   pSym->pNext  = 0;

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

   return pSym;
}

int Include( char * szFileName, PATHNAMES *pSearch )
{
   PFILE pFile;

  if( ! ( yyin = fopen( szFileName, "r" ) ) )
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
        if( ! ( yyin = fopen( szFName, "r" ) ) )
        {
            pSearch = pSearch->pNext;
            if( ! pSearch )
              return 0;
        }
      }
      OurFree( pFileName );
    }
    else
      return 0;
  }

   if( ! _iQuiet )
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
      if( ! _iQuiet )
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
   GenPCode1( _DUPLICATE );
}

void DupPCode( WORD wStart ) /* duplicates the current generated pcode from an offset */
{
   WORD w, wEnd = functions.pLast->lPCodePos - wStart;

   for( w = 0; w < wEnd; w++ )
      GenPCode1( functions.pLast->pCode[ wStart + w ] );
}

/*
 * This function creates and initialises the _FUNC structure
 */
PFUNCTION FunctionNew( char *szName, char cScope )
{
   PFUNCTION pFunc;

   pFunc = ( PFUNCTION ) OurMalloc( sizeof( _FUNC ) );
   pFunc->szName       = szName;
   pFunc->cScope       = cScope;
   pFunc->pLocals      = 0;
   pFunc->pStatics     = 0;
   pFunc->pFields      = 0;
   pFunc->pCode        = 0;
   pFunc->lPCodeSize   = 0;
   pFunc->lPCodePos    = 0;
   pFunc->pNext        = 0;
   pFunc->wParamCount  = 0;
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
void FunDef( char * szFunName, char cScope, int iType )
{
   PCOMSYMBOL   pSym;
   PFUNCTION pFunc;
   char * *pFunction;

   if( ( pFunc = GetFunction( szFunName ) ) )
   {
      /* The name of a function/procedure is already defined */
      if( pFunc != functions.pFirst || _iStartProc )
        /* it is not a starting procedure that was automatically created */
        GenError( ERR_FUNC_DUPL, szFunName, NULL );
   }

   pFunction =(char * *)RESERVED_FUNC( szFunName );
   if( pFunction && !(functions.iCount==0 && !_iStartProc) )
   {
      /* We are ignoring it when it is the name of PRG file and we are
       * not creating implicit starting procedure
       */
        GenError( ERR_FUNC_RESERVED, *pFunction, szFunName );
   }

   FixReturns();    /* fix all previous function returns offsets */

   if( !( pSym = GetSymbol( szFunName ) ) )
      /* there is not a symbol on the symbol table for this function name */
      pSym = AddSymbol( szFunName );

   if( cScope == FS_PUBLIC )
      pSym->cScope = FS_PUBLIC;
   else
      pSym->cScope |= cScope; /* we may have a non public function and a object message */

   pFunc =FunctionNew( szFunName, cScope );
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

   GenPCode3( _FRAME, 0, 0 );   /* frame for locals and parameters */
   GenPCode3( _SFRAME, 0, 0 );     /* frame for statics variables */
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
   FILE * yyc;             /* file handle for C output */

   szName =szName;  /* just to keep compiler silent */

   if( ! ( yyc = fopen( szFileName, "wb" ) ) )
   {
     printf( "Error opening file %s\n", szFileName );
     return;
   }

   if( ! _iQuiet )
      printf( "\ngenerating C language output...\n" );

   fprintf( yyc, "#include \"pcode.h\"\n\n" );

   if( ! _iStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   /* write functions prototypes for PRG defined functions */
   while( pFunc )
   {
      if ( pFunc->cScope & FS_STATIC || pFunc->cScope & FS_INIT ||
           pFunc->cScope & FS_EXIT )
         fprintf( yyc, "static " );

      fprintf( yyc, "HARBOUR %s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }
   /* write functions prototypes for called functions outside this PRG */
   pFunc = funcalls.pFirst;
   while( pFunc )
   {
      if( ! ( pFTemp = GetFunction( pFunc->szName ) ) || pFTemp == functions.pFirst )
         fprintf( yyc, "HARBOUR %s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }

   /* writes the symbol table */
   fprintf( yyc, "\nstatic SYMBOL symbols[] = { " );

   if( ! _iStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   wSym = 0; /* syymbols counter */
   while( pSym )
   {
      fprintf( yyc, "{ \"%s\", ", pSym->szName );
      ++wSym;

      if( pSym->cScope & FS_STATIC )
         fprintf( yyc, "FS_STATIC" );

      else if( pSym->cScope & FS_INIT )
         fprintf( yyc, "FS_INIT" );

      else if( pSym->cScope & FS_EXIT )
         fprintf( yyc, "FS_EXIT" );

      else
         fprintf( yyc, "FS_PUBLIC" );

      if( ( pSym->cScope != FS_MESSAGE ) && ( pSym->cScope & FS_MESSAGE ) ) /* only for non public symbols */
         fprintf( yyc, " | FS_MESSAGE" );

      /* specify the function address if it is a defined function or a
         external called function */
      if( ( pFTemp = GetFunction( pSym->szName ) ) ) /* is it a defined function ? */
        fprintf( yyc, ", %s, 0 }", pFTemp->szName );
      else
      {
         if( ( pFTemp = GetFuncall( pSym->szName ) ) )
            fprintf( yyc, ", %s, 0 }", pFTemp->szName );
         else
            fprintf( yyc, ", 0, 0 }" );
      }

      if( pSym != symbols.pLast )
         fprintf( yyc, ",\n                            " );

      pSym = pSym->pNext;
   }
   fprintf( yyc, " };\n\n" );

   if( _iAltSymbolTableInit )
   {
     fprintf( yyc, "void ProcessSymbols( SYMBOL *, WORD );\n" );
     fprintf( yyc, "/* Add a local symbol table to the global one\n*/\n" );
     fprintf( yyc, "void %s__InitSymbols( void )\n{\n"
                   "  ProcessSymbols( symbols, %i );\n}\n\n", symbols.pFirst->szName, wSym );
   }
   else
     fprintf( yyc, "#include <init.h>\n\n" );

   /* Generate functions data
    */
   pFunc = functions.pFirst;
   if( ! _iStartProc )
     pFunc = pFunc->pNext; /* No implicit starting procedure */
   while( pFunc )
   {
      if( pFunc->cScope != FS_PUBLIC )
         fprintf( yyc, "static " );

      fprintf( yyc, "HARBOUR %s( void )\n{\n  static BYTE pcode[] = { \n", pFunc->szName );

      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case AND_:
                 fprintf( yyc, "                AND_,\n" );
                 lPCodePos++;
                 break;

            case _ARRAYAT:
                 fprintf( yyc, "                _ARRAYAT,\n" );
                 lPCodePos++;
                 break;

            case _ARRAYPUT:
                 fprintf( yyc, "                _ARRAYPUT,\n" );
                 lPCodePos++;
                 break;

            case _DEC:
                 fprintf( yyc, "                _DEC,\n" );
                 lPCodePos++;
                 break;

            case _DIMARRAY:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _DIMARRAY, %i, %i,\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case _DIVIDE:
                 fprintf( yyc, "                _DIVIDE,\n" );
                 lPCodePos++;
                 break;

            case _DO:
                 fprintf( yyc, "                _DO, %i, %i,\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ] );
                 lPCodePos += 3;
                 break;

            case _DUPLICATE:
                 fprintf( yyc, "                _DUPLICATE,\n" );
                 lPCodePos++;
                 break;

            case _DUPLTWO:
                 fprintf( yyc, "                _DUPLTWO,\n" );
                 lPCodePos++;
                 break;

            case _EQUAL:
                 fprintf( yyc, "                _EQUAL,\n" );
                 lPCodePos++;
                 break;

            case _EXACTLYEQUAL:
                 fprintf( yyc, "                _EXACTLYEQUAL,\n" );
                 lPCodePos++;
                 break;

            case _ENDBLOCK:
                 --iNestedCodeblock;
                 fprintf( yyc, "                _ENDBLOCK,\n" );
                 lPCodePos++;
                 break;

            case _FALSE:
                 fprintf( yyc, "                _FALSE,\n" );
                 lPCodePos++;
                 break;

            case _FORTEST:                    /* ER For tests. Step > 0 LESS */
                                              /* Step < 0 GREATER */
                 fprintf( yyc, "                _FORTEST,\n" );
                 lPCodePos++;
                 break;

            case _FRAME:
                 if( pFunc->pCode[ lPCodePos + 1 ] || pFunc->pCode[ lPCodePos + 2 ] )
                    fprintf( yyc, "                _FRAME, %i, %i,\t\t/* locals, params */\n",
                             pFunc->pCode[ lPCodePos + 1 ],
                             pFunc->pCode[ lPCodePos + 2 ] );
                 lPCodePos += 3;
                 break;

            case _FUNCPTR:
                 fprintf( yyc, "                _FUNCPTR,\n" );
                 lPCodePos++;
                 break;

            case _FUNCTION:
                 fprintf( yyc, "                _FUNCTION, %i, %i,\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ] );
                 lPCodePos += 3;
                 break;

            case _GENARRAY:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _GENARRAY, %i, %i,\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case _GREATER:
                 fprintf( yyc, "                _GREATER,\n" );
                 lPCodePos++;
                 break;

            case _GREATEREQUAL:
                 fprintf( yyc, "                _GREATEREQUAL,\n" );
                 lPCodePos++;
                 break;

            case _INC:
                 fprintf( yyc, "                _INC,\n" );
                 lPCodePos++;
                 break;

            case _INSTRING:
                 fprintf( yyc, "                _INSTRING,\n" );
                 lPCodePos++;
                 break;

            case _JUMP:
                 if( 1 ) /* (lPCodePos + 3) < pFunc->lPCodePos ) */
                 {
                    w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    fprintf( yyc, "                _JUMP, %i, %i,\t/* %i (abs: %05li) */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 }
                 lPCodePos += 3;
                 break;

            case _JUMPFALSE:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _JUMPFALSE, %i, %i,\t/* %i (abs: %05li) */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 lPCodePos += 3;
                 break;

            case _JUMPTRUE:
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _JUMPTRUE, %i, %i,\t/* %i (abs: %05li) */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
                 lPCodePos += 3;
                 break;

            case _LESS:
                 fprintf( yyc, "                _LESS,\n" );
                 lPCodePos++;
                 break;

            case _LESSEQUAL:
                 fprintf( yyc, "                _LESSEQUAL,\n" );
                 lPCodePos++;
                 break;

            case _LINE:
                 fprintf( yyc, "/* %05li */", lPCodePos );
                 w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "  _LINE, %i, %i,\t\t/* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ], w );
                 lPCodePos += 3;
                 break;

            case _MESSAGE:
                 wSym = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _MESSAGE, %i, %i,      /* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          GetSymbolOrd( wSym + ! _iStartProc )->szName );
                 lPCodePos += 3;
                 break;

            case _MINUS:
                 fprintf( yyc, "                _MINUS,\n" );
                 lPCodePos++;
                 break;

            case _MODULUS:
                 fprintf( yyc, "                _MODULUS,\n" );
                 lPCodePos++;
                 break;

            case _MULT:
                 fprintf( yyc, "                _MULT,\n" );
                 lPCodePos++;
                 break;

            case _NEGATE:
                 fprintf( yyc, "                _NEGATE,\n" );
                 lPCodePos++;
                 break;

            case _NOT:
                 fprintf( yyc, "                _NOT,\n" );
                 lPCodePos++;
                 break;

            case _NOTEQUAL:
                 fprintf( yyc, "                _NOTEQUAL,\n" );
                 lPCodePos++;
                 break;

            case OR_:
                 fprintf( yyc, "                OR_,\n" );
                 lPCodePos++;
                 break;

            case _PLUS:
                 fprintf( yyc, "                _PLUS,\n" );
                 lPCodePos++;
                 break;

            case _POP:
                 fprintf( yyc, "                _POP,\n" );
                 lPCodePos++;
                 break;

            case _POPLOCAL:
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
                       fprintf( yyc, "                _POPLOCAL, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                _POPLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                _POPLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case _POPMEMVAR:
                 wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _POPMEMVAR, %i, %i,\t/* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          GetSymbolOrd( wVar + ! _iStartProc )->szName );
                 lPCodePos += 3;
                 break;

            case _POPSTATIC:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                _POPSTATIC, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case _POWER:
                 fprintf( yyc, "                _POWER,\n" );
                 lPCodePos++;
                 break;

            case _PUSHBLOCK:
                 ++iNestedCodeblock;
                 fprintf( yyc, "                _PUSHBLOCK, %i, %i,\t/* %i */\n",
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

            case _PUSHDOUBLE:
                 {
                    int i;
                    ++lPCodePos;
                    fprintf( yyc, "                _PUSHDOUBLE, " );
                    for( i = 0; i < sizeof( double ) + sizeof( BYTE ); ++i )
                       fprintf( yyc, "%i, ", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
                    fprintf( yyc, "/* %.*f, %d */\n",
                    *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
                    *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
                    *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ) );
                    lPCodePos += sizeof( double ) + sizeof( BYTE );
                 }
                 break;

            case _PUSHINT:
                 fprintf( yyc, "                _PUSHINT, %i, %i,     /* %i */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256 );
                 lPCodePos += 3;
                 break;

            case _PUSHLOCAL:
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
                       fprintf( yyc, "                _PUSHLOCAL, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                _PUSHLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                _PUSHLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case _PUSHLOCALREF:
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
                       fprintf( yyc, "                _PUSHLOCALREF, %i, %i,\t/* localvar%i */\n",
                                pFunc->pCode[ lPCodePos + 1 ],
                                pFunc->pCode[ lPCodePos + 2 ],
                                -wVar );
                       else
                         fprintf( yyc, "                _PUSHLOCALREF, %i, %i,\t/* codeblockvar%i */\n",
                                  pFunc->pCode[ lPCodePos + 1 ],
                                  pFunc->pCode[ lPCodePos + 2 ],
                                  wVar );
                   }
                   else
                     fprintf( yyc, "                _PUSHLOCALREF, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                   lPCodePos += 3;
                 }
                 break;

            case _PUSHLONG:
                 fprintf( yyc, "                _PUSHLONG, %i, %i, %i, %i,  /* %li */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          pFunc->pCode[ lPCodePos + 3 ],
                          pFunc->pCode[ lPCodePos + 4 ],
                          *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
                 lPCodePos +=( 1 + sizeof(long) );
                 break;

            case _PUSHMEMVAR:
                 wVar = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _PUSHMEMVAR, %i, %i,\t/* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          GetSymbolOrd( wVar + ! _iStartProc )->szName );
                 lPCodePos += 3;
                 break;

            case _PUSHMEMVARREF:
                 wVar = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _PUSHMEMVARREF, %i, %i,\t/* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          GetSymbolOrd( wVar + ! _iStartProc )->szName );
                 lPCodePos += 3;
                 break;

            case _PUSHNIL:
                 fprintf( yyc, "                _PUSHNIL,\n" );
                 lPCodePos++;
                 break;

            case _PUSHSELF:
                 fprintf( yyc, "                _PUSHSELF,\n" );
                 lPCodePos++;
                 break;

            case _PUSHSTATIC:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] +pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                _PUSHSTATIC, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case _PUSHSTATICREF:
                 {
                    PVAR pVar;
                    PFUNCTION pTmp = functions.pFirst;

                    wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                    while( pTmp->pNext && pTmp->pNext->wStaticsBase < wVar )
                        pTmp =pTmp->pNext;
                    pVar = GetVar( pTmp->pStatics, wVar - pTmp->wStaticsBase );
                    fprintf( yyc, "                _PUSHSTATICREF, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              pVar->szName );
                    lPCodePos += 3;
                 }
                 break;

            case _PUSHSTR:
                 wLen = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _PUSHSTR, %i, %i,     /* %i */\n",
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

            case _PUSHSYM:
                 wSym = pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256;
                 fprintf( yyc, "                _PUSHSYM, %i, %i,      /* %s */\n",
                          pFunc->pCode[ lPCodePos + 1 ],
                          pFunc->pCode[ lPCodePos + 2 ],
                          GetSymbolOrd( wSym + ! _iStartProc )->szName );
                 lPCodePos += 3;
                 break;

            case _RETVALUE:
                 fprintf( yyc, "                _RETVALUE,\n" );
                 lPCodePos++;
                 break;

            case _SFRAME:
                 /* we only generate it if there are statics used in this function */
                 if( pFunc->bFlags & FUN_USES_STATICS )
                 {
                    w = GetSymbolPos( _pInitFunc->szName ) - ( _iStartProc ? 1: 2 );
                    fprintf( yyc, "                _SFRAME, %i, %i,\t\t/* symbol _INITSTATICS */\n",
                             LOBYTE( w ), HIBYTE( w ) );
                 }
                 lPCodePos += 3;
                 break;

            case _STATICS:
                 {
                    w = GetSymbolPos( _pInitFunc->szName ) - ( _iStartProc ? 1: 2 );
                    fprintf( yyc, "                _STATICS, %i, %i,\t\t/* symbol _INITSTATICS */\n",
                             LOBYTE( w ), HIBYTE( w ) );
                    lPCodePos += 3;
                 }
                 break;

            case _TRUE:
                 fprintf( yyc, "                _TRUE,\n" );
                 lPCodePos++;
                 break;

            case _ZERO:
                 fprintf( yyc, "                _ZERO,\n" );
                 lPCodePos++;
                 break;

            default:
                 printf( "Incorrect pcode value!\n" );
                 lPCodePos = pFunc->lPCodePos;
                 break;
         }
      }

      fprintf( yyc, "/* %05li */", lPCodePos );
      fprintf( yyc, "  _ENDPROC };\n\n" );
      fprintf( yyc, "   VirtualMachine( pcode, symbols );\n}\n\n" );
      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   if( ! _iQuiet )
      printf( "%s -> done!\n", szFileName );
}

void GenExterns( void ) /* generates the symbols for the EXTERN names */
{
  PEXTERN pDelete;

  while( pExterns )
  {
    if( GetSymbolPos( pExterns->szName ) )
    {
      if( ! GetFuncall( pExterns->szName ) )
        AddFunCall( pExterns->szName );
    }
    else
    {
      AddSymbol( pExterns->szName );
      AddFunCall( pExterns->szName );
    }
    pDelete  = pExterns;
    pExterns = pExterns->pNext;
    OurFree( pDelete );
  }
}

void GenReturn( WORD wOffset ) /* generates a return offset to later on fill it with the proper exiting pcode address */
{
   PRETURN pReturn = ( PRETURN ) OurMalloc( sizeof( _RETURN ) ), pLast;

   pReturn->wOffset = wOffset;
   pReturn->pNext   = 0;

   if( ! pReturns )
      pReturns = pReturn;
   else
   {
      pLast = pReturns;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pReturn;
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
      if( ! strcmp( pVars->szName, szVarName ) )
         return wVar;
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
    if( (iVar = GetVarPos( pFunc->pLocals, szVarName ) ) )
      /* this a current codeblock parameter */
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
            GenError( ERR_OUTER_VAR, szVarName, NULL );
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

              pVar =(PVAR) OurMalloc( sizeof(PVAR) );
              pVar->szName =szVarName;
              pVar->pNext  =NULL;
              iVar =1;  /* first variable */
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
  if( !_iStartProc )
  {
    iPos =GetVarPos( functions.pFirst->pStatics, szVarName );
    if( iPos )
      return iPos;
  }
  return 0;
}


PCOMSYMBOL GetSymbol( char * szSymbolName ) /* returns a symbol pointer from the symbol table */
{
   PCOMSYMBOL pSym = symbols.pFirst;

   while( pSym )
   {
      if( ! strcmp( pSym->szName, szSymbolName ) && pSym != symbols.pFirst )
         return pSym;
      else
      {
         if( pSym->pNext )
            pSym = pSym->pNext;
         else
            return 0;
      }
   }
   return 0;
}

PCOMSYMBOL GetSymbolOrd( WORD wSymbol )   /* returns a symbol based on its index on the symbol table */
{
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w = 0;

   while( w++ < wSymbol && pSym->pNext )
      pSym = pSym->pNext;

   return pSym;
}

WORD GetSymbolPos( char * szSymbolName ) /* return 0 if not found or order + 1 */
{
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD wSymbol = 1;

   while( pSym )
   {
      if( ! strcmp( pSym->szName, szSymbolName ) && pSym != symbols.pFirst )
         return wSymbol;
      else
      {
         if( pSym->pNext )
         {
            pSym = pSym->pNext;
            wSymbol++;
         }
         else
            return 0;
      }
   }
   return 0;
}

WORD GetFunctionPos( char * szFunctionName ) /* return 0 if not found or order + 1 */
{
   PFUNCTION pFunc = functions.pFirst;
   WORD wFunction = _iStartProc;

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
   GenPCode1( _INC );
}

WORD Jump( int iOffset )
{
   GenPCode3( _JUMP, LOBYTE( iOffset ), HIBYTE( iOffset ) );

   return functions.pLast->lPCodePos - 2;
}

WORD JumpFalse( int iOffset )
{
   GenPCode3( _JUMPFALSE, LOBYTE( iOffset ), HIBYTE( iOffset ) );

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
   GenPCode3( _JUMPTRUE, LOBYTE( iOffset ), HIBYTE( iOffset ) );

   return functions.pLast->lPCodePos - 2;
}

void Line( void ) /* generates the pcode with the currently compiled source code line */
{
  if( _iLineNumbers )
   GenPCode3( _LINE, LOBYTE( iLine ), HIBYTE( iLine ) );
}

void LineBody( void ) /* generates the pcode with the currently compiled source code line */
{
   /* This line can be placed inside a procedure or function only */
   if( ! _iStartProc && functions.iCount <= 1 )
   {
     GenError( ERR_OUTSIDE, NULL, NULL );
   }
  functions.pLast->bFlags |= FUN_STATEMENTS;
  if( _iLineNumbers )
   GenPCode3( _LINE, LOBYTE( iLine ), HIBYTE( iLine ) );
}

void Message( char * szMsgName )       /* sends a message to an object */
{
   WORD wSym = GetSymbolPos( szMsgName );

   if( ! wSym )  /* the symbol was not found on the symbol table */
   {
      AddSymbol( szMsgName );
      wSym = symbols.iCount;
   }
   GetSymbolOrd( wSym - 1 )->cScope |= FS_MESSAGE;
   wSym -= _iStartProc ? 1: 2;
   GenPCode3( _MESSAGE, LOBYTE( wSym ), HIBYTE( wSym ) );
}

void MessageDupl( char * szMsgName )  /* fix a generated message and duplicate to an object */
{
   WORD wSetSym = GetSymbolPos( szMsgName );
   BYTE bLoGetSym, bHiGetSym;           /* get symbol */
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   if( ! wSetSym )  /* the symbol was not found on the symbol table */
   {
      AddSymbol( szMsgName );
      wSetSym = symbols.iCount;
   }
   GetSymbolOrd( wSetSym - 1 )->cScope |= FS_MESSAGE;
   wSetSym -= _iStartProc ? 1: 2;
                                        /* Get previously generated message */
   bLoGetSym = pFunc->pCode[ _lMessageFix + 1];
   bHiGetSym = pFunc->pCode[ _lMessageFix + 2];

   pFunc->pCode[ _lMessageFix + 1 ] = LOBYTE( wSetSym );
   pFunc->pCode[ _lMessageFix + 2 ] = HIBYTE( wSetSym );

   pFunc->lPCodePos -= 3;               /* Remove unnecessary function call  */
   Duplicate();                         /* Duplicate object                  */
   GenPCode3( _MESSAGE, bLoGetSym, bHiGetSym );
                                        /* Generate new message              */
}

void MessageFix( char * szMsgName )  /* fix a generated message to an object */
{
   WORD wSym = GetSymbolPos( szMsgName );
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

   if( ! wSym )  /* the symbol was not found on the symbol table */
   {
      AddSymbol( szMsgName );
      wSym = symbols.iCount;
   }
   GetSymbolOrd( wSym - 1 )->cScope |= FS_MESSAGE;
   wSym -= _iStartProc ? 1: 2;
   pFunc->pCode[ _lMessageFix + 1 ] = LOBYTE( wSym );
   pFunc->pCode[ _lMessageFix + 2 ] = HIBYTE( wSym );
   pFunc->lPCodePos -= 3;        /* Remove unnecessary function call */
}

void PopId( char * szVarName ) /* generates the pcode to pop a value from the virtual machine stack onto a variable */
{
   WORD wVar;

   if( ( wVar = GetLocalVarPos( szVarName ) ) )
      GenPCode3( _POPLOCAL, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetStaticVarPos( szVarName ) ) )
      GenPCode3( _POPSTATIC, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2 ) )
      GenPCode3( _POPMEMVAR, LOBYTE( wVar ), HIBYTE( wVar ) );

   else
   {
      AddSymbol( szVarName );
      wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2;
      GenPCode3( _POPMEMVAR, LOBYTE( wVar ), HIBYTE( wVar ) );
   }
}

void PushId( char * szVarName ) /* generates the pcode to push a variable value to the virtual machine stack */
{
   WORD wVar;

   if( iVarScope == VS_STATIC )
   {
     /* Reffering to any variable is not allowed during initialization
      * of static variable
      */
      _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
   }

   if( ( wVar = GetLocalVarPos( szVarName ) ) )
      GenPCode3( _PUSHLOCAL, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetStaticVarPos( szVarName ) ) )
      GenPCode3( _PUSHSTATIC, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2 ) )
      GenPCode3( _PUSHMEMVAR, LOBYTE( wVar ), HIBYTE( wVar ) );

   else
   {
      AddSymbol( szVarName );
      wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2;
      GenPCode3( _PUSHMEMVAR, LOBYTE( wVar ), HIBYTE( wVar ) );
   }
}

void PushIdByRef( char * szVarName ) /* generates the pcode to push a variable by reference to the virtual machine stack */
{
   WORD wVar;

   if( iVarScope == VS_STATIC )
   {
     /* Reffering to any variable is not allowed during initialization
      * of static variable
      */
      _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
   }

   if( ( wVar = GetLocalVarPos( szVarName ) ) )
      GenPCode3( _PUSHLOCALREF, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetStaticVarPos( szVarName ) ) )
      GenPCode3( _PUSHSTATICREF, LOBYTE( wVar ), HIBYTE( wVar ) );

   else if( ( wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2) )
      GenPCode3( _PUSHMEMVARREF, LOBYTE( wVar ), HIBYTE( wVar ) );

   else
   {
      AddSymbol( szVarName );
      wVar = GetSymbolPos( szVarName ) - _iStartProc ? 1: 2;
      GenPCode3( _PUSHMEMVARREF, LOBYTE( wVar ), HIBYTE( wVar ) );
   }
}

void PushLogical( int iTrueFalse ) /* pushes a logical value on the virtual machine stack */
{
   if( iTrueFalse )
      GenPCode1( _TRUE );
   else
      GenPCode1( _FALSE );
}

void PushNil( void )
{
   GenPCode1( _PUSHNIL );
}

/* generates the pcode to push a double number on the virtual machine stack */
void PushDouble( double dNumber, BYTE bDec )
{
   GenPCode1( _PUSHDOUBLE );
   GenPCodeN( ( BYTE * ) &dNumber, sizeof( double ) );
   GenPCode1( bDec );
}

/* generates the pcode to push a integer number on the virtual machine stack */
void PushInteger( int iNumber )
{
   if( iNumber )
      GenPCode3( _PUSHINT, LOBYTE( ( WORD ) iNumber ), HIBYTE( ( WORD ) iNumber ) );
   else
      GenPCode1( _ZERO );
}

/* generates the pcode to push a long number on the virtual machine stack */
void PushLong( long lNumber )
{
   if( lNumber )
   {
      GenPCode1( _PUSHLONG );
      GenPCode1( ( ( char * ) &lNumber )[ 0 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 1 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 2 ] );
      GenPCode1( ( ( char * ) &lNumber )[ 3 ] );
   }
   else
      GenPCode1( _ZERO );
}

/* generates the pcode to push a string on the virtual machine stack */
void PushString( char * szText )
{
   WORD wStrLen = strlen( szText );

   GenPCode3( _PUSHSTR, LOBYTE(wStrLen), HIBYTE(wStrLen) );
   GenPCodeN( ( BYTE * ) szText, wStrLen );
}

/* generates the pcode to push a symbol on the virtual machine stack */
void PushSymbol( char * szSymbolName, int iIsFunction )
{
   WORD wSym;

   if( iIsFunction )
   {
      char * *pName = (char * *)RESERVED_FUNC( szSymbolName );
      if( pName )
        szSymbolName =*pName;
   }

   wSym = GetSymbolPos( szSymbolName ); /* returns 1, 2, ... */

   if( wSym == 1 ) /* default module name procedure */
      wSym = 0;

   if( ! wSym )  /* the symbol was not found on the symbol table */
   {
      AddSymbol( szSymbolName );
      wSym = symbols.iCount;
      if( iIsFunction )
         AddFunCall( szSymbolName );
   }
   else
   {
      if( iIsFunction && ! GetFuncall( szSymbolName ) )
         AddFunCall( szSymbolName );
   }
   wSym -= _iStartProc ? 1: 2;

   if( ! iIsFunction )
      GetSymbolOrd( wSym )->cScope |= FS_MESSAGE;

   GenPCode3( _PUSHSYM, LOBYTE( wSym ), HIBYTE( wSym ) );
}

void CheckDuplVars( PVAR pVar, char * szVarName, int iVarScope )
{
   while( pVar )
   {
      if( ! strcmp( pVar->szName, szVarName ) )
      {
         if( iVarScope != VS_PARAMETER )
            --iLine;
         GenError( ERR_VAR_DUPL, szVarName, NULL );
      }
      else
         pVar =pVar->pNext;
   }
}

void Dec( void )
{
   GenPCode1( _DEC );
}

void DimArray( WORD wDimensions )
{
   GenPCode3( _DIMARRAY, LOBYTE( wDimensions ), HIBYTE( wDimensions ) );
}

void Do( BYTE bParams )
{
   GenPCode3( _DO, bParams, 0 );
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
   PRETURN pLast = pReturns, pDelete;

   if( pReturns )
   {
      while( pLast )
      {
         JumpHere( pLast->wOffset );
         pLast = pLast->pNext;
      }
      pLast   = pReturns;
      pDelete = pReturns;
      while( pLast )
      {
         pLast = pLast->pNext;
         OurFree( pDelete );
         pDelete = pLast;
      }
      pReturns = 0;
   }
/* TODO: check why it triggers this error in keywords.prg
   if( pLoops )
   {
     PLOOPEXIT pLoop = pLoops;
     char cLine[ 64 ];

     while( pLoop->pNext )
        pLoop =pLoop->pNext;

     itoa( pLoop->wLine, cLine, 10 );
     GenError( ERR_UNCLOSED_STRU, cLine, NULL );
   }
*/
}

void Function( BYTE bParams )
{
   GenPCode3( _FUNCTION, bParams, 0 );
}

void GenArray( WORD wElements )
{
   GenPCode3( _GENARRAY, LOBYTE( wElements ), HIBYTE( wElements ) );
}

void GenPCode1( BYTE byte )
{
   PFUNCTION pFunc = functions.pLast;   /* get the currently defined Clipper function */

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

void SetFrame( void ) /* generates the proper _FRAME values */
{
   BYTE * pCode = functions.pLast->pCode;
   PVAR pLocal  = functions.pLast->pLocals;
   BYTE bLocals = 0;

   while( pLocal )
   {
      pLocal = pLocal->pNext;
      bLocals++;
   }

   pCode[ 1 ] = bLocals - functions.pLast->wParamCount;
   pCode[ 2 ] = functions.pLast->wParamCount;
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

  pCodeblock =functions.pLast;

  /* return to pcode buffer of function/codeblock in which the current
   * codeblock was defined
   */
  functions.pLast =pCodeblock->pOwner;

  /* find the function that owns the codeblock */
  pFunc =pCodeblock->pOwner;
  while( pFunc->pOwner )
    pFunc =pFunc->pOwner;

  /* generate a proper codeblock frame with a codeblock size and with
   * a number of expected parameters
   */
  /*QUESTION: would be 64kB enough for a codeblock size?
   * we are assuming now a WORD for a size of codeblock
   */

  /* Count the number of referenced local variables */
  pVar =pCodeblock->pStatics;
  while( pVar )
  {
    pVar =pVar->pNext;
    ++wLocals;
  }

  /*NOTE:  8 = _PUSHBLOCK + WORD(size) + WORD(wParams) + WORD(wLocals) +_ENDBLOCK */
  wSize =( WORD ) pCodeblock->lPCodePos +8 +wLocals*2;
  GenPCode3( _PUSHBLOCK, LOBYTE(wSize), HIBYTE(wSize) );
  GenPCode1( LOBYTE(pCodeblock->wParamCount) );
  GenPCode1( HIBYTE(pCodeblock->wParamCount) );
  GenPCode1( LOBYTE(wLocals) );
  GenPCode1( HIBYTE(wLocals) );

  /* generate the table of referenced local variables */
  pVar =pCodeblock->pStatics;
  while( wLocals-- )
  {
    wPos =GetVarPos( pFunc->pLocals, pVar->szName );
    GenPCode1( LOBYTE(wPos) );
    GenPCode1( HIBYTE(wPos) );
    pFree =pVar;
    pVar =pVar->pNext;
    OurFree( pFree->szName );
    OurFree( pFree );
  }

  GenPCodeN( pCodeblock->pCode, pCodeblock->lPCodePos );
  GenPCode1( _ENDBLOCK ); /* finish the codeblock */

  /* this fake-function is no longer needed */
  OurFree( pCodeblock->pCode )
  pVar =pCodeblock->pLocals;
  while( pVar )
  {
    /* free used variables */
    pFree =pVar;
    pVar =pVar->pNext;
    OurFree( pFree->szName );
    OurFree( pFree );
  }
  OurFree( pCodeblock );
}

/* Set the name of an alias for the list of previously declared FIELDs
 *
 * szAlias -> name of the alias
 * iField  -> position of the first FIELD name to change
 */
void SetAlias( char * szAlias, int iField )
{
  PVAR pVar;

  pVar = functions.pLast->pFields;
  while( iField-- && pVar )
      pVar = pVar->pNext;

  while( pVar )
  {
    pVar->szAlias =szAlias;
    pVar =pVar->pNext;
  }
}

/* This functions counts the number of FIELD declaration in a function
 * We will required this information in SetAlias function
 */
int FieldsCount()
{
  int iFields = 0;
  PVAR pVar = functions.pLast->pFields;

  while( pVar )
  {
    ++iFields;
    pVar =pVar->pNext;
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
      _pInitFunc =FunctionNew( "_INITSTATICS", FS_INIT );
      _pInitFunc->pOwner =functions.pLast;
      _pInitFunc->bFlags =FUN_USES_STATICS | FUN_PROCEDURE;
      functions.pLast =_pInitFunc;
      PushInteger( 1 );   /* the number of static variables is unknown now */
      GenPCode3( _STATICS, 0, 0 );
      GenPCode3( _SFRAME, 0, 0 );     /* frame for statics variables */
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
  iVarScope =0;
}

/*
 * This function checks if we are initializing a static variable.
 * It should be called only in case when the parser have recognized any
 * function or method invocation.
 */
void StaticAssign( void )
{
  if( iVarScope == VS_STATIC )
    _pInitFunc->bFlags |= FUN_ILLEGAL_INIT;
}

/*
 * This function stores the position in pcode buffer where the FOR/WHILE
 * loop starts. It will be used to fix any LOOP/EXIT statements
 */
static void LoopStart( void )
{
  PLOOPEXIT pLoop = OurMalloc( sizeof(LOOPEXIT) );

  if( pLoops )
  {
    PLOOPEXIT pLast =pLoops;

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
  PLOOPEXIT pLast, pLoop = (PLOOPEXIT) OurMalloc( sizeof( LOOPEXIT ) );

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
  PLOOPEXIT pLast, pLoop = (PLOOPEXIT) OurMalloc( sizeof( LOOPEXIT ) );

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
  PLOOPEXIT pLoop = pLoops, pFree;

  while( pLoop->pNext )
    pLoop = pLoop->pNext;

  pLoop =pLoop->pLoopList;
  while( pLoop )
  {
    JumpHere( pLoop->wOffset +1 );
    pFree = pLoop;
    pLoop = pLoop->pLoopList;
    OurFree( pFree );
  }
}

/*
 * Fixes the EXIT statements and releases memory allocated for current loop
 */
static void LoopEnd( void )
{
  PLOOPEXIT pExit, pLoop = pLoops, pLast = pLoops, pFree;

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
    OurFree( pFree );
  }

  pLast->pNext = NULL;
  if( pLoop == pLoops )
    pLoops = NULL;
  OurFree( pLoop );
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

#ifndef HAVE_STRUPR
char * strupr( char * p )
{
   char * p1;

   for ( p1 = p; * p1; p1++ )
      * p1 = toupper( * p1 );
   return( p );
}
#endif

#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

void GenPortObj( char *szFileName, char *szName )
{
   PFUNCTION pFunc = functions.pFirst;
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w, wLen, wVar;
   LONG lPCodePos;
   LONG lPad;
   LONG lSymbols;
   FILE * yyc;             /* file handle for C output */

   szName = szName;
   if( ! ( yyc = fopen( szFileName, "wb" ) ) )
   {
     printf( "Error opening file %s\n", szFileName );
     return;
   }

   szName = szName;

   if( ! _iQuiet )
      printf( "\ngenerating portable object file...\n" );

   /* writes the symbol table */

   if( ! _iStartProc )
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
   if( ! _iStartProc )
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
   if( ! _iStartProc )
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
   if( ! _iStartProc )
     pFunc = pFunc->pNext; /* No implicit starting procedure */

   while( pFunc )
   {
      fputs( pFunc->szName, yyc );
      fputc( 0, yyc );
      fputc( (BYTE) ( ( pFunc->lPCodePos       ) & 255 ), yyc ); /* Write size */
      fputc( (BYTE) ( ( pFunc->lPCodePos >> 8  ) & 255 ), yyc );
      fputc( (BYTE) ( ( pFunc->lPCodePos >> 16 ) & 255 ), yyc );
      fputc( (BYTE) ( ( pFunc->lPCodePos >> 24 ) & 255 ), yyc );

/*      printf( "Creating output for %s\n", pFunc->szName ); */

      lPCodePos = 0;
      lPad = 0;                         /* Number of bytes optimized */
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case AND_:
            case _ARRAYAT:
            case _ARRAYPUT:
            case _DEC:
            case _DIVIDE:
            case _DUPLICATE:
            case _DUPLTWO:
            case _EQUAL:
            case _EXACTLYEQUAL:
            case _FALSE:
            case _FORTEST:
            case _FUNCPTR:
            case _GREATER:
            case _GREATEREQUAL:
            case _INC:
            case _INSTRING:
            case _LESS:
            case _LESSEQUAL:
            case _MINUS:
            case _MODULUS:
            case _MULT:
            case _NEGATE:
            case _NOT:
            case _NOTEQUAL:
            case OR_:
            case _PLUS:
            case _POP:
            case _POWER:
            case _PUSHNIL:
            case _PUSHSELF:
            case _RETVALUE:
            case _TRUE:
            case _ZERO:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case _DIMARRAY:
            case _DO:
            case _ENDBLOCK:
            case _FUNCTION:
            case _GENARRAY:
            case _JUMP:
            case _JUMPFALSE:
            case _JUMPTRUE:
            case _LINE:
            case _MESSAGE:
            case _POPLOCAL:
            case _POPMEMVAR:
            case _POPSTATIC:
            case _PUSHINT:
            case _PUSHLOCAL:
            case _PUSHLOCALREF:
            case _PUSHMEMVAR:
            case _PUSHMEMVARREF:
            case _PUSHSTATIC:
            case _PUSHSTATICREF:
            case _PUSHSYM:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case _FRAME:
                 if( pFunc->pCode[ lPCodePos + 1 ] || pFunc->pCode[ lPCodePos + 2 ] )
                 {
                    fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                    fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                    fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                 }
                 else
                 {
                    lPad += 3;
                    lPCodePos += 3;
                 }
                 break;

            case _PUSHBLOCK:
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

            case _PUSHDOUBLE:
                 {
                    int i;
                    fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                    for( i = 0; i < sizeof( double ); ++i )
                       fputc( ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ], yyc );
                    lPCodePos += sizeof( double );
                 }
                 break;

            case _PUSHLONG:
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                 break;

            case _PUSHSTR:
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

            case _SFRAME:
                 /* we only generate it if there are statics used in this function */
                 if( pFunc->bFlags & FUN_USES_STATICS )
                 {
                    w = GetSymbolPos( _pInitFunc->szName ) - ( _iStartProc ? 1: 2 );
                    fputc( pFunc->pCode[ lPCodePos ], yyc );
                    fputc( LOBYTE( w ), yyc );
                    fputc( HIBYTE( w ), yyc );
                 }
                 else
                    lPad += 3;
                 lPCodePos += 3;
                 break;

            case _STATICS:
                 w = GetSymbolPos( _pInitFunc->szName ) - ( _iStartProc ? 1: 2 );
                 fputc( pFunc->pCode[ lPCodePos ], yyc );
                 fputc( LOBYTE( w ), yyc );
                 fputc( HIBYTE( w ), yyc );
                 lPCodePos += 3;
                 break;

            default:
                 printf( "Incorrect pcode value!\n" );
                 lPCodePos = pFunc->lPCodePos;
                 break;
         }
      }

      fputc( _ENDPROC, yyc );
      for( ; lPad; lPad-- )
         fputc( 0, yyc );                       /* Pad optimalizations */
      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   if( ! _iQuiet )
      printf( "%s -> done!\n", szFileName );
}


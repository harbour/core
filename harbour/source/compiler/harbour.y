%{
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler YACC rules and actions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/* TODO list
 * 1) Support this syntax: nPtr := @Hello()
 */

#include <malloc.h>

#include "hbcomp.h"

/* Compile using: bison -d -v harbour.y */

extern FILE *yyin;      /* currently yacc parsed file */
extern int hb_comp_iLine;       /* currently parsed file line number */
extern char *yytext;

#ifdef __cplusplus
typedef struct yy_buffer_state *YY_BUFFER_STATE;
extern YY_BUFFER_STATE yy_create_buffer( FILE *, int ); /* yacc functions to manage multiple files */
extern void yy_switch_to_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
extern void yy_delete_buffer( YY_BUFFER_STATE ); /* yacc functions to manage multiple files */
#else
extern void * yy_create_buffer( FILE *, int ); /* yacc functions to manage multiple files */
extern void yy_switch_to_buffer( void * ); /* yacc functions to manage multiple files */
extern void yy_delete_buffer( void * ); /* yacc functions to manage multiple files */
#endif

/* lex & yacc related prototypes */
extern int yyparse( void );    /* main yacc parsing function */
extern void yyerror( char * ); /* parsing error management function */
extern int yylex( void );      /* main lex token function, called by yyparse() */
#ifdef __cplusplus
extern "C" int yywrap( void );
#else
extern int yywrap( void );     /* manages the EOF of current processed file */
#endif

static void hb_compLoopStart( void );
static void hb_compLoopEnd( void );
static void hb_compLoopLoop( void );
static void hb_compLoopExit( void );
static void hb_compLoopHere( void );

static void * hb_compElseIfGen( void * pFirstElseIf, ULONG ulOffset ); /* generates a support structure for elseifs pcode fixups */
static void hb_compElseIfFix( void * pIfElseIfs ); /* implements the ElseIfs pcode fixups */

static void hb_compRTVariableAdd( HB_EXPR_PTR, BOOL );
static void hb_compRTVariableGen( char * );

static void hb_compVariableDim( char *, HB_EXPR_PTR );

#ifdef HARBOUR_YYDEBUG
   #define YYDEBUG        1 /* Parser debug information support */
#endif

typedef struct __ELSEIF
{
   ULONG ulOffset;
   struct __ELSEIF * pNext;
} _ELSEIF, * PELSEIF;      /* support structure for else if pcode fixups */

typedef struct _LOOPEXIT
{
   ULONG ulOffset;
   int iLine;
   USHORT wSeqCounter;
   struct _LOOPEXIT * pLoopList;
   struct _LOOPEXIT * pExitList;
   struct _LOOPEXIT * pNext;
} LOOPEXIT, * PTR_LOOPEXIT;  /* support structure for EXIT and LOOP statements */

typedef struct HB_RTVAR_
{
   HB_EXPR_PTR pVar;
   BOOL bPopValue;
   struct HB_RTVAR_ *pNext;
   struct HB_RTVAR_ *pPrev;
} HB_RTVAR, *HB_RTVAR_PTR; /* support structure for PUBLIC and PRIVATE statements */

USHORT hb_comp_wSeqCounter   = 0;
USHORT hb_comp_wForCounter   = 0;
USHORT hb_comp_wIfCounter    = 0;
USHORT hb_comp_wWhileCounter = 0;
USHORT hb_comp_wCaseCounter  = 0;

char * hb_comp_buffer; /* yacc input buffer */

static PTR_LOOPEXIT hb_comp_pLoops = NULL;
static HB_RTVAR_PTR hb_comp_rtvars = NULL;

char * hb_comp_szAnnounce = NULL;    /* ANNOUNCEd procedure */
%}

%union                  /* special structure used by lex and yacc to share info */
{
   char * string;       /* to hold a string returned by lex */
   int    iNumber;      /* to hold a temporary integer number */
   long   lNumber;      /* to hold a temporary long number */
   struct
   {
      int    iNumber;      /* to hold a number returned by lex */
      char * szValue;
   } valInteger;
   struct
   {
      long   lNumber;      /* to hold a long number returned by lex */
      char * szValue;
   } valLong;
   struct
   {
      double dNumber;   /* to hold a double number returned by lex */
      /* NOTE: Intentionally using "unsigned char" instead of "BYTE" */
      unsigned char bWidth; /* to hold the width of the value */
      unsigned char bDec; /* to hold the number of decimal points in the value */
      char * szValue;
   } valDouble;
   HB_EXPR_PTR asExpr;
   void * pVoid;        /* to hold any memory structure we may need */
};

%token FUNCTION PROCEDURE IDENTIFIER RETURN NIL NUM_DOUBLE INASSIGN NUM_INTEGER NUM_LONG
%token LOCAL STATIC IIF IF ELSE ELSEIF END ENDIF LITERAL TRUEVALUE FALSEVALUE
%token ANNOUNCE EXTERN INIT EXIT AND OR NOT PUBLIC EQ NE1 NE2
%token INC DEC ALIASOP DOCASE CASE OTHERWISE ENDCASE ENDDO MEMVAR
%token WHILE EXIT LOOP END FOR NEXT TO STEP LE GE FIELD IN PARAMETERS
%token PLUSEQ MINUSEQ MULTEQ DIVEQ POWER EXPEQ MODEQ EXITLOOP
%token PRIVATE BEGINSEQ BREAK RECOVER RECOVERUSING DO WITH SELF LINE
%token MACROVAR MACROTEXT
%token AS_ARRAY AS_BLOCK AS_CHARACTER AS_CLASS AS_DATE AS_LOGICAL AS_NUMERIC AS_OBJECT AS_VARIANT DECLARE OPTIONAL
%token AS_ARRAY_ARRAY AS_BLOCK_ARRAY AS_CHARACTER_ARRAY AS_CLASS_ARRAY AS_DATE_ARRAY AS_LOGICAL_ARRAY AS_NUMERIC_ARRAY AS_OBJECT_ARRAY

/*the lowest precedence*/
/*postincrement and postdecrement*/
%left  POST
/*assigment - from right to left*/
%right INASSIGN
%right  PLUSEQ MINUSEQ
%right  MULTEQ DIVEQ MODEQ
%right  EXPEQ
/*logical operators*/
%right  OR
%right  AND
%right  NOT
/*relational operators*/
%right  '=' '<' '>' EQ NE1 NE2 LE GE '$'
/*mathematical operators*/
%right  '+' '-'
%right  '*' '/' '%'
%right  POWER
%right UNARY
/*preincrement and predecrement*/
%right  PRE
/*special operators*/
%right  ALIASOP '&' '@'
%right '\n' ';' ','
/*the highest precedence*/

%type <string>  IdentName IDENTIFIER LITERAL SendId MACROVAR MACROTEXT
%type <valDouble>  NUM_DOUBLE
%type <valInteger> NUM_INTEGER
%type <valLong>    NUM_LONG
%type <iNumber> FunScope
%type <iNumber> Params ParamList
%type <iNumber> IfBegin VarList ExtVarList
%type <iNumber> FieldList
%type <lNumber> WhileBegin
%type <pVoid>   IfElseIf Cases
%type <asExpr>  Argument ArgList ElemList BlockExpList BlockVarList BlockNoVar
%type <asExpr>  DoName DoProc DoArgument DoArgList
%type <asExpr>  PareExpList1 PareExpList2 PareExpList3 PareExpListN
%type <asExpr>  ExpList ExpList1 ExpList2 ExpList3
%type <asExpr>  NumValue NumAlias
%type <asExpr>  NilValue NilAlias
%type <asExpr>  LiteralValue LiteralAlias
%type <asExpr>  CodeBlock CodeBlockAlias
%type <asExpr>  Logical LogicalAlias
%type <asExpr>  SelfValue SelfAlias
%type <asExpr>  Array ArrayAlias
%type <asExpr>  ArrayAt ArrayAtAlias
%type <asExpr>  Variable VarAlias
%type <asExpr>  MacroVar MacroVarAlias
%type <asExpr>  MacroExpr MacroExprAlias
%type <asExpr>  AliasId AliasVar AliasExpr
%type <asExpr>  VariableAt VariableAtAlias
%type <asExpr>  FunCall FunCallAlias
%type <asExpr>  ObjectData ObjectDataAlias
%type <asExpr>  ObjectMethod ObjectMethodAlias
%type <asExpr>  IfInline IfInlineAlias
%type <asExpr>  PareExpList PareExpListAlias
%type <asExpr>  Expression SimpleExpression LValue
%type <asExpr>  EmptyExpression
%type <asExpr>  ExprAssign ExprOperEq ExprPreOp ExprPostOp
%type <asExpr>  ExprEqual ExprMath ExprBool ExprRelation ExprUnary
%type <asExpr>  ExprPlusEq ExprMinusEq ExprMultEq ExprDivEq ExprModEq ExprExpEq
%type <asExpr>  ArrayIndex IndexList
%type <asExpr>  DimIndex DimList
%type <asExpr>  FieldAlias FieldVarAlias
%type <asExpr>  PostOp

%%

Main       : { hb_compLinePush(); } Source       { }
           | /* empty file */
           ;

Source     : Crlf         { hb_comp_EOL = FALSE; }
           | VarDefs      { hb_comp_EOL = FALSE; }
           | FieldsDef    { hb_comp_EOL = FALSE; }
           | MemvarDef    { hb_comp_EOL = FALSE; }
           | Declaration  { hb_comp_EOL = FALSE; }
           | Function     { hb_comp_EOL = FALSE; }
           | Statement    { hb_comp_EOL = FALSE; }
           | Line         { hb_comp_EOL = FALSE; }
           | Source Crlf        { hb_comp_EOL = FALSE; }
           | Source Function    { hb_comp_EOL = FALSE; }
           | Source Statement   { hb_comp_EOL = FALSE; }
           | Source VarDefs     { hb_comp_EOL = FALSE; }
           | Source FieldsDef   { hb_comp_EOL = FALSE; }
           | Source MemvarDef   { hb_comp_EOL = FALSE; }
           | Source Declaration { hb_comp_EOL = FALSE; }
           | Source Line        { hb_comp_EOL = FALSE; }
           | Source error Crlf { hb_comp_EOL = FALSE; yyclearin; }
           ;

Line       : LINE NUM_INTEGER LITERAL Crlf
           | LINE NUM_INTEGER LITERAL '@' LITERAL Crlf   /* Xbase++ style */
           ;

Function   : FunScope FUNCTION  IdentName { hb_comp_cVarType = ' '; hb_compFunctionAdd( $3, ( HB_SYMBOLSCOPE ) $1, 0 ); } Params Crlf {}
           | FunScope PROCEDURE IdentName { hb_comp_cVarType = ' '; hb_compFunctionAdd( $3, ( HB_SYMBOLSCOPE ) $1, FUN_PROCEDURE ); } Params Crlf {}
           ;

FunScope   :                  { $$ = HB_FS_PUBLIC; }
           | STATIC           { $$ = HB_FS_STATIC; }
           | INIT             { $$ = HB_FS_INIT; }
           | EXIT             { $$ = HB_FS_EXIT; }
           ;

Params     :                                                         { $$ = 0; }
           | '(' ')'                                                 { $$ = 0; }
           | '(' { hb_comp_iVarScope = VS_PARAMETER; } ParamList ')' { $$ = $3; }
           ;

AsType     : /* not specified */           { hb_comp_cVarType = ' '; }
           | AS_NUMERIC                    { hb_comp_cVarType = 'N'; }
           | AS_CHARACTER                  { hb_comp_cVarType = 'C'; }
           | AS_DATE                       { hb_comp_cVarType = 'D'; }
           | AS_LOGICAL                    { hb_comp_cVarType = 'L'; }
           | AS_ARRAY                      { hb_comp_cVarType = 'A'; }
           | AS_BLOCK                      { hb_comp_cVarType = 'B'; }
           | AS_OBJECT                     { hb_comp_cVarType = 'O'; }
           | AS_CLASS IdentName            { hb_comp_cVarType = 'S'; hb_comp_szFromClass = $2 }
           | AS_VARIANT                    { hb_comp_cVarType = ' '; }
           | AS_NUMERIC_ARRAY              { hb_comp_cVarType = 'n'; }
           | AS_CHARACTER_ARRAY            { hb_comp_cVarType = 'c'; }
           | AS_DATE_ARRAY                 { hb_comp_cVarType = 'd'; }
           | AS_LOGICAL_ARRAY              { hb_comp_cVarType = 'l'; }
           | AS_ARRAY_ARRAY                { hb_comp_cVarType = 'a'; }
           | AS_BLOCK_ARRAY                { hb_comp_cVarType = 'b'; }
           | AS_OBJECT_ARRAY               { hb_comp_cVarType = 'o'; }
           | AS_CLASS_ARRAY IdentName      { hb_comp_cVarType = 's'; hb_comp_szFromClass = $2 }
           ;

AsArray    : AS_ARRAY                      { hb_comp_cVarType = 'A'; }
           | AS_NUMERIC_ARRAY              { hb_comp_cVarType = 'n'; }
           | AS_CHARACTER_ARRAY            { hb_comp_cVarType = 'c'; }
           | AS_DATE_ARRAY                 { hb_comp_cVarType = 'd'; }
           | AS_LOGICAL_ARRAY              { hb_comp_cVarType = 'l'; }
           | AS_ARRAY_ARRAY                { hb_comp_cVarType = 'a'; }
           | AS_BLOCK_ARRAY                { hb_comp_cVarType = 'b'; }
           | AS_OBJECT_ARRAY               { hb_comp_cVarType = 'o'; }
           | AS_CLASS_ARRAY IdentName      { hb_comp_cVarType = 's'; hb_comp_szFromClass = $2 }
           ;

ParamList  : IdentName AsType                { hb_compVariableAdd( $1, hb_comp_cVarType ); $$ = 1; }
           | ParamList ',' IdentName AsType  { hb_compVariableAdd( $3, hb_comp_cVarType ); $$++; }
           ;

/* NOTE: This alllows the use of Expression as a statement.
 *    The Expression is validated later in reduction phase of
 *    hb_compExprGenStatement(). With this solution we don't have to
 *    stop compilation if invalid syntax will be used.
 */
Statement  : ExecFlow   CrlfStmnt   { }
           | IfInline CrlfStmnt     { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | FunCall CrlfStmnt      { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | AliasExpr CrlfStmnt    { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ObjectMethod CrlfStmnt { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | PareExpList CrlfStmnt  { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ExprPreOp CrlfStmnt    { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ExprPostOp CrlfStmnt   { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ExprOperEq CrlfStmnt   { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ExprEqual CrlfStmnt    { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | ExprAssign CrlfStmnt   { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | DoProc CrlfStmnt       { hb_compExprDelete( hb_compExprGenStatement( $1 ) ); }
           | BREAK CrlfStmnt        { hb_compGenBreak(); hb_compGenPCode2( HB_P_DOSHORT, 0, ( BOOL ) 1 );
                                      hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE; }
           | BREAK { hb_compLinePushIfInside(); } Expression Crlf  { hb_compGenBreak(); hb_compExprDelete( hb_compExprGenPush( $3 ) );
                                           hb_compGenPCode2( HB_P_DOSHORT, 1, ( BOOL ) 1 );
                                           hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE;
                                         }
           | RETURN CrlfStmnt {
                     if( hb_comp_wSeqCounter )
                        {
                           hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "RETURN", NULL );
                        }
                        hb_compGenPCode1( HB_P_ENDPROC );
                        if( (hb_comp_functions.pLast->bFlags & FUN_PROCEDURE) == 0 )
                        { /* return from a function without a return value */
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_NO_RETURN_VALUE, NULL, NULL );
                        }
                        hb_comp_functions.pLast->bFlags |= FUN_WITH_RETURN;
                        hb_comp_bDontGenLineNum = TRUE;
                        hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE;
                     }
           | RETURN { hb_compLinePushIfInside(); } Expression Crlf {
                        if( hb_comp_wSeqCounter )
                        {
                           hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "RETURN", NULL );
                        }
                        hb_compExprGenPush( $3 );   /* TODO: check if return value agree with declared value */
                        hb_compGenPCode2( HB_P_RETVALUE, HB_P_ENDPROC, ( BOOL ) 1 );
                        if( hb_comp_functions.pLast->bFlags & FUN_PROCEDURE )
                        { /* procedure returns a value */
                           hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_PROC_RETURN_VALUE, NULL, NULL );
                        }
                        hb_comp_functions.pLast->bFlags |= FUN_WITH_RETURN;
                        hb_comp_bDontGenLineNum = TRUE;
                        hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE;
                     }
           | PUBLIC { hb_compLinePushIfInside(); hb_comp_iVarScope = VS_PUBLIC; }
                     ExtVarList
                    { hb_compRTVariableGen( "__MVPUBLIC" ); hb_comp_cVarType = ' ';  hb_comp_iVarScope = VS_NONE; } CrlfStmnt
           | PRIVATE { hb_compLinePushIfInside(); hb_comp_iVarScope = VS_PRIVATE; }
                     ExtVarList
                    { hb_compRTVariableGen( "__MVPRIVATE" ); hb_comp_cVarType = ' '; hb_comp_iVarScope = VS_NONE; } CrlfStmnt

           | EXITLOOP  { hb_comp_bDontGenLineNum = TRUE; hb_compLoopExit(); } CrlfStmnt { hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE; }
           | LOOP      { hb_comp_bDontGenLineNum = TRUE; hb_compLoopLoop(); } CrlfStmnt { hb_comp_functions.pLast->bFlags |= FUN_BREAK_CODE; }
           | EXTERN ExtList Crlf
           | ANNOUNCE IdentName {
               if( hb_comp_szAnnounce == NULL )
               {
                  /* check for reserved name
                  * NOTE: Clipper doesn't check for it
                  */
                  char * szFunction = hb_compReservedName( $2 );
                  if( szFunction )
                     hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_FUNC_RESERVED, szFunction, $2 );
                  hb_comp_szAnnounce = $2;
               }
               else
                  hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_DUPL_ANNOUNCE, $2, NULL );
             } Crlf

           ;

CrlfStmnt  : { hb_compLinePushIfInside(); } Crlf
;

LineStat   : Crlf          { $<lNumber>$ = 0; hb_comp_bDontGenLineNum = TRUE; }
           | Statement     { $<lNumber>$ = 1; }
           ;

Statements : LineStat                  { $<lNumber>$ = $<lNumber>1; hb_compLinePush(); }
           | Statements LineStat       { $<lNumber>$ += $<lNumber>2; hb_compLinePush();  }
           ;

ExtList    : IdentName                      { hb_compExternAdd( $1 ); }
           | ExtList ',' IdentName          { hb_compExternAdd( $3 ); }
           ;

IdentName  : IDENTIFIER       { $$ = $1; }
           | STEP             { $$ = hb_strdup( "STEP" ); }
           | TO               { $$ = hb_strdup( "TO" ); }
           ;

/* Numeric values
 */
NumValue   : NUM_DOUBLE          { $$ = hb_compExprNewDouble( $1.dNumber, $1.bWidth, $1.bDec ); }
           | NUM_INTEGER         { $$ = hb_compExprNewLong( $1.iNumber ); }
           | NUM_LONG            { $$ = hb_compExprNewLong( $1.lNumber ); }
           ;

NumAlias   : NUM_INTEGER ALIASOP      { $$ = hb_compExprNewLong( $1.iNumber ); }
           | NUM_LONG    ALIASOP      { $$ = hb_compExprNewLong( $1.lNumber ); }
           | NUM_DOUBLE  ALIASOP      { $$ = hb_compErrorAlias( hb_compExprNewDouble( $1.dNumber, $1.bWidth, $1.bDec ) ); }
           ;

/* NIL value
 */
NilValue   : NIL                       { $$ = hb_compExprNewNil(); }
;

NilAlias   : NilValue ALIASOP          { $$ = $1; }
;

/* Literal string value
 */
LiteralValue : LITERAL                    { $$ = hb_compExprNewString( $1 ); }
;

LiteralAlias : LiteralValue ALIASOP       { $$ = $1; }
;

/* Codeblock value
 */
CodeBlockAlias : CodeBlock ALIASOP     { $$ = $1; }
;

/* Logical value
 */
Logical    : TRUEVALUE                    { $$ = hb_compExprNewLogical( TRUE ); }
           | FALSEVALUE                   { $$ = hb_compExprNewLogical( FALSE ); }
           ;

LogicalAlias : Logical ALIASOP      { $$ = $1; }
;

/* SELF value and expressions
 */
SelfValue  : SELF             { $$ = hb_compExprNewSelf(); }
;

SelfAlias  : SelfValue ALIASOP         { $$ = $1; }
;

/* Literal array
 */
Array      : '{' ElemList '}'          { $$ = hb_compExprNewArray( $2 ); }
           ;

ArrayAlias  : Array ALIASOP            { $$ = $1; }
;

/* Literal array access
 */
ArrayAt     : Array ArrayIndex   { $$ = $2; }
;

ArrayAtAlias : ArrayAt ALIASOP      { $$ = $1; }
;

/* Variables
 */
Variable    : IdentName         { $$ = hb_compExprNewVar( $1 ); }
;

VarAlias    : IdentName ALIASOP      { $$ = hb_compExprNewAlias( $1 ); }
;

/* Macro variables
 */
MacroVar    : MACROVAR        { $$ = hb_compExprNewMacro( NULL, '&', $1 ); }
            | MACROTEXT       { $$ = hb_compExprNewMacro( NULL, 0, $1 ); }
;

MacroVarAlias  : MacroVar ALIASOP   { $$ = $1; }
;

/* Macro expressions
 */
MacroExpr  : '&' PareExpList       { $$ = hb_compExprNewMacro( $2, 0, NULL ); }
;

MacroExprAlias : MacroExpr ALIASOP     { $$ = $1; }
;

/* Aliased variables
 */
/* special case: _FIELD-> and FIELD-> can be nested
 */
FieldAlias  : FIELD ALIASOP               { $$ = hb_compExprNewAlias( "FIELD" ); }
            | FIELD ALIASOP FieldAlias    { $$ = $3; }
            ;

/* ignore _FIELD-> or FIELD-> if a real alias is specified
 */
FieldVarAlias  : FieldAlias VarAlias            { hb_compExprDelete( $1 ); $$ = $2; }
               | FieldAlias NumAlias            { hb_compExprDelete( $1 ); $$ = $2; }
               | FieldAlias PareExpListAlias    { hb_compExprDelete( $1 ); $$ = $2; }
               | FieldAlias MacroVarAlias       { hb_compExprDelete( $1 ); $$ = $2; }
               | FieldAlias MacroExprAlias      { hb_compExprDelete( $1 ); $$ = $2; }
               | FieldAlias NilAlias            { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias LiteralAlias        { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias LogicalAlias        { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias CodeBlockAlias      { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias SelfAlias           { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias ArrayAlias          { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias ArrayAtAlias        { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               | FieldAlias IfInlineAlias       { hb_compExprDelete( $1 ); $$ = hb_compErrorAlias( $2 ); }
               ;

AliasId     : IdentName      { $$ = hb_compExprNewVar( $1 ); }
            | MacroVar        { $$ = $1; }
            | MacroExpr       { $$ = $1; }
            ;

AliasVar   : NumAlias AliasId          { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | MacroVarAlias AliasId     { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | MacroExprAlias AliasId    { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | PareExpListAlias AliasId  { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | NilAlias AliasId          { $$ = hb_compErrorAlias( $1 ); }
           | LiteralAlias AliasId      { $$ = hb_compErrorAlias( $1 ); }
           | LogicalAlias AliasId      { $$ = hb_compErrorAlias( $1 ); }
           | CodeBlockAlias AliasId    { $$ = hb_compErrorAlias( $1 ); }
           | SelfAlias AliasId         { $$ = hb_compErrorAlias( $1 ); }
           | ArrayAlias AliasId        { $$ = hb_compErrorAlias( $1 ); }
           | ArrayAtAlias AliasId      { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | VariableAtAlias AliasId   { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | IfInlineAlias AliasId     { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | FunCallAlias AliasId      { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | ObjectDataAlias AliasId   { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | ObjectMethodAlias AliasId { $$ = hb_compErrorAlias( $1 ); }  /* QUESTION: Clipper reports error here - we can handle this */
           | VarAlias AliasId          { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | FieldAlias AliasId        { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           | FieldVarAlias AliasId     { $$ = hb_compExprNewAliasVar( $1, $2 ); }
           ;

/* Aliased expressions
 */
/* NOTE: In the case:
 * alias->( Expression )
 * alias always selects a workarea at runtime
 */
AliasExpr  : NumAlias PareExpList         { $$ = hb_compExprNewAliasExpr( $1, $2 ); }
           | VarAlias PareExpList         { $$ = hb_compExprNewAliasExpr( $1, $2 ); }
           | MacroVarAlias PareExpList    { $$ = hb_compExprNewAliasExpr( $1, $2 ); }
           | MacroExprAlias PareExpList   { $$ = hb_compExprNewAliasExpr( $1, $2 ); }
           | PareExpListAlias PareExpList { $$ = hb_compExprNewAliasExpr( $1, $2 ); }
           | FieldAlias PareExpList       { $$ = hb_compErrorAlias( $2 ); } /* QUESTION: Clipper reports error here - we can handle it */
           ;

/* Array expressions access
 */
VariableAt  : NilValue      ArrayIndex    { $$ = $2; }
            | LiteralValue  ArrayIndex    { $$ = $2; }
            | CodeBlock     ArrayIndex    { $$ = $2; }
            | Logical       ArrayIndex    { $$ = $2; }
            | SelfValue     ArrayIndex    { $$ = $2; }
            | Variable      ArrayIndex    { $$ = $2; }
            | AliasVar      ArrayIndex    { $$ = $2; }
            | AliasExpr     ArrayIndex    { $$ = $2; }
            | MacroVar      ArrayIndex    { $$ = $2; }
            | MacroExpr     ArrayIndex    { $$ = $2; }
            | ObjectData    ArrayIndex    { $$ = $2; }
            | ObjectMethod  ArrayIndex    { $$ = $2; }
            | FunCall       ArrayIndex    { $$ = $2; }
            | IfInline      ArrayIndex    { $$ = $2; }
            | PareExpList   ArrayIndex    { $$ = $2; }
            ;

VariableAtAlias : VariableAt ALIASOP      { $$ = $1; }
;

/* Function call
 */
FunCall    : IdentName '(' ArgList ')'   { $$ = hb_compExprNewFunCall( hb_compExprNewFunName( $1 ), $3 ); }
           | MacroVar '(' ArgList ')'     { $$ = hb_compExprNewFunCall( $1, $3 ); }
;

ArgList    : Argument                     { $$ = hb_compExprNewArgList( $1 ); }
           | ArgList ',' Argument         { $$ = hb_compExprAddListExpr( $1, $3 ); }
           ;

Argument   : EmptyExpression                   { $$ = $1; }
           | '@' IdentName                    { $$ = hb_compExprNewVarRef( $2 ); }
           | '@' IdentName '(' ')'            { $$ = hb_compExprNewFunRef( $2 ); }
           ;

FunCallAlias : FunCall ALIASOP        { $$ = $1; }
;

/* Object's instance variable
 */
SendId      : IdentName      { $$ = $1; }
            | MacroVar        { $$ = "&"; hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_INVALID_SEND, "&", NULL); }
            ;

ObjectData  : NumValue ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | NilValue ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | LiteralValue ':' SendId    { $$ = hb_compExprNewSend( $1, $3 ); }
            | CodeBlock ':' SendId       { $$ = hb_compExprNewSend( $1, $3 ); }
            | Logical ':' SendId         { $$ = hb_compExprNewSend( $1, $3 ); }
            | SelfValue ':' SendId       { $$ = hb_compExprNewSend( $1, $3 ); }
            | Array ':' SendId           { $$ = hb_compExprNewSend( $1, $3 ); }
            | ArrayAt ':' SendId         { $$ = hb_compExprNewSend( $1, $3 ); }
            | Variable ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | AliasVar ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | AliasExpr ':' SendId       { $$ = hb_compExprNewSend( $1, $3 ); }
            | MacroVar ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | MacroExpr ':' SendId       { $$ = hb_compExprNewSend( $1, $3 ); }
            | FunCall ':' SendId         { $$ = hb_compExprNewSend( $1, $3 ); }
            | IfInline ':' SendId        { $$ = hb_compExprNewSend( $1, $3 ); }
            | PareExpList ':' SendId     { $$ = hb_compExprNewSend( $1, $3 ); }
            | VariableAt ':' SendId      { $$ = hb_compExprNewSend( $1, $3 ); }
            | ObjectMethod ':' SendId    { $$ = hb_compExprNewSend( $1, $3 ); }
            | ObjectData ':' SendId      { $$ = hb_compExprNewSend( $1, $3 ); }
            ;

ObjectDataAlias : ObjectData ALIASOP         { $$ = $1; }
;

/* Object's method
 */
ObjectMethod : ObjectData '(' ArgList ')'    { $$ = hb_compExprNewMethodCall( $1, $3 ); }
            ;

ObjectMethodAlias : ObjectMethod ALIASOP        { $$ = $1; }
;

/* NOTE: We have to distinguish IdentName here because it is repeated
 * in DoArgument (a part of DO <proc> WITH .. statement)
 * where it generates different action.
 */
SimpleExpression :
             NumValue
           | NilValue                         { $$ = $1; }
           | LiteralValue                     { $$ = $1; }
           | CodeBlock                        { $$ = $1; }
           | Logical                          { $$ = $1; }
           | SelfValue                        { $$ = $1; }
           | Array                            { $$ = $1; }
           | ArrayAt                          { $$ = $1; }
           | AliasVar                         { $$ = $1; }
           | MacroVar                         { $$ = $1; }
           | MacroExpr                        { $$ = $1; }
           | VariableAt                       { $$ = $1; }
           | FunCall                          { $$ = $1; }
           | IfInline                         { $$ = $1; }
           | ObjectData                       { $$ = $1; }
           | ObjectMethod                     { $$ = $1; }
           | AliasExpr                        { $$ = $1; }
           | ExprAssign                       { $$ = $1; }
           | ExprOperEq                       { $$ = $1; }
           | ExprPostOp                       { $$ = $1; }
           | ExprPreOp                        { $$ = $1; }
           | ExprUnary                        { $$ = $1; }
           | ExprMath                         { $$ = $1; }
           | ExprBool                         { $$ = $1; }
           | ExprRelation                     { $$ = $1; }
;

Expression : Variable                        { $$ = $1; }
           | SimpleExpression                { $$ = $1; }
           | PareExpList                     { $$ = $1; }
;

EmptyExpression: /* nothing => nil */        { $$ = hb_compExprNewEmpty(); }
           | Expression
;

LValue      : IdentName                     { $$ = hb_compExprNewVar( $1 ); }
            | AliasVar
            | MacroVar
            | MacroExpr
            | ObjectData
            | VariableAt
            ;

/* NOTE: PostOp can be used in one context only - it uses $0 rule
 *    (the rule that stands before PostOp)
 */
PostOp      : INC    { $$ = hb_compExprNewPostInc( $<asExpr>0 ); }
            | DEC    { $$ = hb_compExprNewPostDec( $<asExpr>0 ); }
            ;

/* NOTE: The rule: Expression Operator Expression
 * that can be used standalone as a statement have to be written
 * using all possible left values to resolve shift/reduce conflicts
 */
ExprPostOp  : NumValue     PostOp %prec POST  { $$ = $2; }
            | NilValue     PostOp %prec POST  { $$ = $2; }
            | LiteralValue PostOp %prec POST  { $$ = $2; }
            | CodeBlock    PostOp %prec POST  { $$ = $2; }
            | Logical      PostOp %prec POST  { $$ = $2; }
            | SelfValue    PostOp %prec POST  { $$ = $2; }
            | Array        PostOp %prec POST  { $$ = $2; }
            | ArrayAt      PostOp %prec POST  { $$ = $2; }
            | Variable     PostOp %prec POST  { $$ = $2; }
            | MacroVar     PostOp %prec POST  { $$ = $2; }
            | MacroExpr    PostOp %prec POST  { $$ = $2; }
            | AliasVar     PostOp %prec POST  { $$ = $2; }
            | AliasExpr    PostOp %prec POST  { $$ = $2; }
            | VariableAt   PostOp %prec POST  { $$ = $2; }
            | PareExpList  PostOp %prec POST  { $$ = $2; }
            | IfInline     PostOp %prec POST  { $$ = $2; }
            | FunCall      PostOp %prec POST  { $$ = $2; }
            | ObjectData   PostOp %prec POST  { $$ = $2; }
            | ObjectMethod PostOp %prec POST  { $$ = $2; }
            ;

ExprPreOp   : INC Expression  %prec PRE      { $$ = hb_compExprNewPreInc( $2 ); }
            | DEC Expression  %prec PRE      { $$ = hb_compExprNewPreDec( $2 ); }
            ;

ExprUnary   : NOT Expression                 { $$ = hb_compExprNewNot( $2 ); }
            | '-' Expression  %prec UNARY    { $$ = hb_compExprNewNegate( $2 ); }
            | '+' Expression  %prec UNARY    { $$ = $2; }
            ;
/*
ExprAssign  : Expression INASSIGN Expression    { $$ = hb_compExprAssign( $1, $3 ); }
;
*/

ExprAssign  : NumValue     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | NilValue     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | LiteralValue INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | CodeBlock    INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | Logical      INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | SelfValue    INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | Array        INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | ArrayAt      INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | Variable     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | MacroVar     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | MacroExpr    INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | AliasVar     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | AliasExpr    INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | VariableAt   INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | PareExpList  INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | IfInline     INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | FunCall      INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | ObjectData   INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            | ObjectMethod INASSIGN Expression   { $$ = hb_compExprAssign( $1, $3 ); }
            ;

ExprEqual   : NumValue     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | NilValue     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | LiteralValue '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | CodeBlock    '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | Logical      '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | SelfValue    '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | Array        '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | ArrayAt      '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | Variable     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | MacroVar     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | MacroExpr    '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | AliasVar     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | AliasExpr    '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | VariableAt   '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | PareExpList  '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | IfInline     '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | FunCall      '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | ObjectData   '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            | ObjectMethod '=' Expression %prec INASSIGN  { $$ = hb_compExprAssign( $1, $3 ); }
            ;

ExprPlusEq  : NumValue     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | NilValue     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | LiteralValue PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | CodeBlock    PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | Logical      PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | SelfValue    PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | Array        PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | ArrayAt      PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | Variable     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | MacroVar     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | MacroExpr    PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | AliasVar     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | AliasExpr    PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | VariableAt   PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | PareExpList  PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | IfInline     PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | FunCall      PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | ObjectData   PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            | ObjectMethod PLUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPlusEq( $1 ), $3 ); }
            ;

ExprMinusEq : NumValue     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | NilValue     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | LiteralValue MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | CodeBlock    MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | Logical      MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | SelfValue    MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | Array        MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | ArrayAt      MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | Variable     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | MacroVar     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | MacroExpr    MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | AliasVar     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | AliasExpr    MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | VariableAt   MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | PareExpList  MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | IfInline     MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | FunCall      MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | ObjectData   MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            | ObjectMethod MINUSEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMinusEq( $1 ), $3 ); }
            ;

ExprMultEq  : NumValue     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | NilValue     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | LiteralValue MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | CodeBlock    MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | Logical      MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | SelfValue    MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | Array        MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | ArrayAt      MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | Variable     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | MacroVar     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | MacroExpr    MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | AliasVar     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | AliasExpr    MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | VariableAt   MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | PareExpList  MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | IfInline     MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | FunCall      MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | ObjectData   MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            | ObjectMethod MULTEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewMultEq( $1 ), $3 ); }
            ;

ExprDivEq   : NumValue     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | NilValue     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | LiteralValue DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | CodeBlock    DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | Logical      DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | SelfValue    DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | Array        DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | ArrayAt      DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | Variable     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | MacroVar     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | MacroExpr    DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | AliasVar     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | AliasExpr    DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | VariableAt   DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | PareExpList  DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | IfInline     DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | FunCall      DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | ObjectData   DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            | ObjectMethod DIVEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewDivEq( $1 ), $3 ); }
            ;

ExprModEq   : NumValue     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | NilValue     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | LiteralValue MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | CodeBlock    MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | Logical      MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | SelfValue    MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | Array        MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | ArrayAt      MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | Variable     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | MacroVar     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | MacroExpr    MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | AliasVar     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | AliasExpr    MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | VariableAt   MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | PareExpList  MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | IfInline     MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | FunCall      MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | ObjectData   MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            | ObjectMethod MODEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewModEq( $1 ), $3 ); }
            ;

ExprExpEq   : NumValue     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | NilValue     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | LiteralValue EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | CodeBlock    EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | Logical      EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | SelfValue    EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | Array        EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | ArrayAt      EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | Variable     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | MacroVar     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | MacroExpr    EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | AliasVar     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | AliasExpr    EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | VariableAt   EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | PareExpList  EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | IfInline     EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | FunCall      EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | ObjectData   EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            | ObjectMethod EXPEQ Expression   { $$ = hb_compExprSetOperand( hb_compExprNewExpEq( $1 ), $3 ); }
            ;

ExprOperEq  : ExprPlusEq        { $$ = $1; }
            | ExprMinusEq       { $$ = $1; }
            | ExprMultEq        { $$ = $1; }
            | ExprDivEq         { $$ = $1; }
            | ExprModEq         { $$ = $1; }
            | ExprExpEq         { $$ = $1; }
            ;

ExprMath    : Expression '+' Expression     { $$ = hb_compExprSetOperand( hb_compExprNewPlus( $1 ), $3 ); }
            | Expression '-' Expression     { $$ = hb_compExprSetOperand( hb_compExprNewMinus( $1 ), $3 ); }
            | Expression '*' Expression     { $$ = hb_compExprSetOperand( hb_compExprNewMult( $1 ), $3 ); }
            | Expression '/' Expression     { $$ = hb_compExprSetOperand( hb_compExprNewDiv( $1 ), $3 ); }
            | Expression '%' Expression     { $$ = hb_compExprSetOperand( hb_compExprNewMod( $1 ), $3 ); }
            | Expression POWER Expression   { $$ = hb_compExprSetOperand( hb_compExprNewPower( $1 ), $3 ); }
            ;

ExprBool    : Expression AND Expression   { $$ = hb_compExprSetOperand( hb_compExprNewAnd( $1 ), $3 ); }
            | Expression OR  Expression   { $$ = hb_compExprSetOperand( hb_compExprNewOr( $1 ), $3 ); }
            ;

ExprRelation: Expression EQ  Expression   { $$ = hb_compExprSetOperand( hb_compExprNewEQ( $1 ), $3 ); }
            | Expression '<' Expression   { $$ = hb_compExprSetOperand( hb_compExprNewLT( $1 ), $3 ); }
            | Expression '>' Expression   { $$ = hb_compExprSetOperand( hb_compExprNewGT( $1 ), $3 ); }
            | Expression LE  Expression   { $$ = hb_compExprSetOperand( hb_compExprNewLE( $1 ), $3 ); }
            | Expression GE  Expression   { $$ = hb_compExprSetOperand( hb_compExprNewGE( $1 ), $3 ); }
            | Expression NE1 Expression   { $$ = hb_compExprSetOperand( hb_compExprNewNE( $1 ), $3 ); }
            | Expression NE2 Expression   { $$ = hb_compExprSetOperand( hb_compExprNewNE( $1 ), $3 ); }
            | Expression '$' Expression   { $$ = hb_compExprSetOperand( hb_compExprNewIN( $1 ), $3 ); }
            | Expression '=' Expression   { $$ = hb_compExprSetOperand( hb_compExprNewEqual( $1 ), $3 ); }
            ;

ArrayIndex : IndexList ']'                   { $$ = $1; }
           ;

/* NOTE: $0 represents the expression before ArrayIndex
 *    Don't use ArrayIndex in other context than as an array index!
 */
IndexList  : '[' Expression               { $$ = hb_compExprNewArrayAt( $<asExpr>0, $2 ); }
           | IndexList ',' Expression     { $$ = hb_compExprNewArrayAt( $1, $3 ); }
           | IndexList ']' '[' Expression { $$ = hb_compExprNewArrayAt( $1, $4 ); }
           ;

ElemList   : Argument                { $$ = hb_compExprNewList( $1 ); }
           | ElemList ',' Argument   { $$ = hb_compExprAddListExpr( $1, $3 ); }
           ;

CodeBlock  : '{' '|' { $<asExpr>$ = hb_compExprNewCodeBlock(); } BlockNoVar
             '|' BlockExpList '}'   { $$ = $<asExpr>3; }
           | '{' '|' { $<asExpr>$ = hb_compExprNewCodeBlock(); } BlockVarList
             '|' BlockExpList '}'   { $$ = $<asExpr>3; }
           ;

/* NOTE: This uses $-2 then don't use BlockExpList in other context
 */
BlockExpList : Expression                { $$ = hb_compExprAddListExpr( $<asExpr>-2, $1 ); }
           | BlockExpList ',' Expression { $$ = hb_compExprAddListExpr( $<asExpr>-2, $3 ); }
           ;

/* NOTE: This is really not needed however it allows the use of $-2 item
 * in BlockExpList to refer the same rule defined in Codeblock
 */
BlockNoVar : /* empty list */    { $$ = NULL; }
;

BlockVarList : IdentName AsType                 { hb_comp_iVarScope = VS_LOCAL; $$ = hb_compExprCBVarAdd( $<asExpr>0, $1, hb_comp_cVarType ); hb_comp_cVarType = ' '; }
           | BlockVarList ',' IdentName AsType  { hb_comp_iVarScope = VS_LOCAL; $$ = hb_compExprCBVarAdd( $<asExpr>0, $3, hb_comp_cVarType ); hb_comp_cVarType = ' '; }
           ;

/* There is a conflict between the use of IF( Expr1, Expr2, Expr3 )
 * and parenthesized expression ( Expr1, Expr2, Expr3 )
 * To solve this conflict we have to split the definitions into more
 * atomic ones.
 *   Also the generation of pcodes have to be delayed and moved to the
 * end of whole parenthesized expression.
 */
PareExpList1: ExpList1 ')'        { $$ = $1; }
            ;

PareExpList2: ExpList2 ')'        { $$ = $1; }
            ;

PareExpList3: ExpList3 ')'        { $$ = $1; }
            ;

PareExpListN: ExpList ')'         { $$ = $1; }
           ;

PareExpList : PareExpList1        { $$ = $1; }
            | PareExpList2        { $$ = $1; }
            | PareExpList3        { $$ = $1; }
            | PareExpListN        { $$ = $1; }
            ;

PareExpListAlias : PareExpList ALIASOP     { $$ = $1; }
;

ExpList1   : '(' EmptyExpression    { $$ = hb_compExprNewList( $2 ); }
;

ExpList2   : ExpList1 ',' EmptyExpression    { $$ = hb_compExprAddListExpr( $1, $3 ); }
;

ExpList3   : ExpList2 ',' EmptyExpression    { $$ = hb_compExprAddListExpr( $1, $3 ); }
;

ExpList    : ExpList3 ',' EmptyExpression { $$ = hb_compExprAddListExpr( $1, $3 ); }
           | ExpList  ',' EmptyExpression { $$ = hb_compExprAddListExpr( $1, $3 ); }
           ;

IfInline   : IIF PareExpList3          { $$ = hb_compExprNewIIF( $2 ); }
           | IF ExpList1 ',' EmptyExpression ','
             { $<asExpr>$ = hb_compExprAddListExpr( $2, $4 ); }
             EmptyExpression ')'
             { $$ = hb_compExprNewIIF( hb_compExprAddListExpr( $<asExpr>6, $7 ) ); }
           ;

IfInlineAlias : IfInline ALIASOP       { $$ = $1; }
;

VarDefs    : LOCAL { hb_comp_iVarScope = VS_LOCAL; hb_compLinePush(); } VarList Crlf { hb_comp_cVarType = ' '; }
           | STATIC { hb_comp_iVarScope = VS_STATIC; hb_compLinePush(); } VarList Crlf { hb_comp_cVarType = ' '; }
           | PARAMETERS { if( hb_comp_functions.pLast->bFlags & FUN_USES_LOCAL_PARAMS )
                             hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_PARAMETERS_NOT_ALLOWED, NULL, NULL );
                          else
                             hb_comp_functions.pLast->wParamNum=0; hb_comp_iVarScope = ( VS_PRIVATE | VS_PARAMETER ); }
                             MemvarList Crlf { hb_comp_iVarScope = VS_NONE; }
           ;

VarList    : VarDef                                  { $$ = 1; }
           | VarList ',' VarDef                      { $$++; }
           ;

ExtVarList : ExtVarDef                               { $$ = 1; }
           | ExtVarList ',' ExtVarDef                { $$++; }
           ;

/* NOTE: if STATIC or LOCAL variables are declared and initialized then we can
 * assign a value immediately - however for PRIVATE and PUBLIC variables
 * initialization have to be delayed because we have to create these variables
 * first.
 */
ExtVarDef  : VarDef
           | MacroVar AsType
               { hb_compRTVariableAdd( hb_compExprNewRTVar( NULL, $1 ), FALSE ); }
           | MacroVar AsType INASSIGN Expression
               { hb_compExprDelete( hb_compExprGenPush( $4 ) );
                 hb_compRTVariableAdd( hb_compExprNewRTVar( NULL, $1 ), TRUE );
               }
           | MacroVar DimList
               {
                  USHORT uCount = hb_compExprListLen( $2 );
                  hb_compExprDelete( hb_compExprGenPush( $2 ) );
                  hb_compGenPCode3( HB_P_ARRAYDIM, HB_LOBYTE( uCount ), HB_HIBYTE( uCount ), ( BOOL ) 1 );
                  hb_compRTVariableAdd( hb_compExprNewRTVar( NULL, $1 ), TRUE );
               }
           | MacroVar DimList AsArray
               {
                  USHORT uCount = hb_compExprListLen( $2 );
                  hb_compExprDelete( hb_compExprGenPush( $2 ) );
                  hb_compGenPCode3( HB_P_ARRAYDIM, HB_LOBYTE( uCount ), HB_HIBYTE( uCount ), ( BOOL ) 1 );
                  hb_compRTVariableAdd( hb_compExprNewRTVar( NULL, $1 ), TRUE );
               }
           ;

VarDef     : IdentName AsType { hb_compVariableAdd( $1, hb_comp_cVarType ); }
               {
                  if( hb_comp_iVarScope == VS_STATIC )
                  {
                     hb_compStaticDefStart();   /* switch to statics pcode buffer */
                     hb_compStaticDefEnd();
                  }
                  else if( hb_comp_iVarScope == VS_PUBLIC || hb_comp_iVarScope == VS_PRIVATE )
                  {
                     hb_compRTVariableAdd( hb_compExprNewRTVar( $1, NULL ), FALSE );
                  }
               }

           | IdentName AsType { $<iNumber>$ = hb_comp_iVarScope;
                                hb_compVariableAdd( $1, hb_comp_cVarType );
                              }
             INASSIGN Expression
               {
                  hb_comp_iVarScope = $<iNumber>3;
                  if( hb_comp_iVarScope == VS_STATIC )
                  {
                     hb_compStaticDefStart();   /* switch to statics pcode buffer */
                     hb_compExprDelete( hb_compExprGenStatement( hb_compExprAssignStatic( hb_compExprNewVar( $1 ), $5 ) ) );
                     hb_compStaticDefEnd();
                  }
                  else if( hb_comp_iVarScope == VS_PUBLIC || hb_comp_iVarScope == VS_PRIVATE )
                  {
                     hb_compExprDelete( hb_compExprGenPush( $5 ) );
                     hb_compRTVariableAdd( hb_compExprNewRTVar( $1, NULL ), TRUE );
                  }
                  else
                  {
                     hb_compExprDelete( hb_compExprGenStatement( hb_compExprAssign( hb_compExprNewVar( $1 ), $5 ) ) );
                  }
                  hb_comp_iVarScope = $<iNumber>3;
               }

           | IdentName DimList          { hb_compVariableDim( $1, $2 ); }
           | IdentName DimList AsArray  { hb_compVariableDim( $1, $2 ); }
           ;

/* NOTE: DimList and DimIndex is the same as ArrayIndex and IndexList
 *       however we are using quite different actions here
 */
DimList    : DimIndex ']'                   { $$ = $1; }
           ;

DimIndex   : '[' Expression               { $$ = hb_compExprNewArgList( $2 ); }
           | DimIndex ',' Expression      { $$ = hb_compExprAddListExpr( $1, $3 ); }
           | DimIndex ']' '[' Expression  { $$ = hb_compExprAddListExpr( $1, $4 ); }
           ;


FieldsDef  : FIELD { hb_comp_iVarScope = VS_FIELD; } FieldList Crlf { hb_comp_cVarType = ' '; }
           ;

FieldList  : IdentName AsType               { $$=hb_compFieldsCount(); hb_compVariableAdd( $1, hb_comp_cVarType ); }
           | FieldList ',' IdentName AsType { hb_compVariableAdd( $3, hb_comp_cVarType ); }
           | FieldList IN IdentName { hb_compFieldSetAlias( $3, $<iNumber>1 ); }
           ;

MemvarDef  : MEMVAR { hb_comp_iVarScope = VS_MEMVAR; } MemvarList Crlf { hb_comp_cVarType = ' '; }
           ;

MemvarList : IdentName AsType                     { hb_compVariableAdd( $1, hb_comp_cVarType ); }
           | MemvarList ',' IdentName AsType      { hb_compVariableAdd( $3, hb_comp_cVarType ); }
           ;

Declaration: DECLARE IdentName '(' { hb_compDeclaredAdd( $2 ); hb_comp_szDeclaredFun = $2; } DecList ')' AsType Crlf
             {
               if( hb_comp_pLastDeclared )
                 hb_comp_pLastDeclared->cType = hb_comp_cVarType;

               hb_comp_szDeclaredFun = NULL;
               hb_comp_cVarType = ' ';
               hb_comp_iVarScope = VS_NONE;
             }
           | DECLARE IdentName { hb_comp_pLastClass = hb_compClassAdd( $2 ); } ClassInfo Crlf { hb_comp_iVarScope = VS_NONE; }
           ;

ClassInfo  : DecMethod
           | ClassInfo DecMethod
           | DecData
           | ClassInfo DecData
           ;

DecMethod  : IdentName '(' { hb_comp_pLastMethod = hb_compMethodAdd( hb_comp_pLastClass, $1 ); } DecList ')' AsType
             {
               if( hb_comp_pLastMethod )
               {
                 hb_comp_pLastMethod->cType = hb_comp_cVarType;
                 if ( toupper( hb_comp_cVarType ) == 'S' )
                 {
                   hb_comp_pLastMethod->pClass = hb_compClassFind( hb_comp_szFromClass );
                   if( ! hb_comp_pLastMethod->pClass )
                   {
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, hb_comp_pLastMethod->szName );
                     hb_comp_pLastMethod->cType = ( isupper(  ( int ) hb_comp_cVarType ) ? 'O' : 'o' );
                   }

                   /* Resetting */
                   hb_comp_szFromClass = NULL;
                 }
               }
               hb_comp_pLastMethod = NULL;
               hb_comp_cVarType = ' ';
             }
           ;

DecData    : IdentName { hb_comp_pLastMethod = hb_compMethodAdd( hb_comp_pLastClass, $1 ); } AsType
             {
               if( hb_comp_pLastMethod )
               {
                 PCOMCLASS pClass;
                 char * szSetData = ( char * ) hb_xgrab( strlen( $1 ) + 2 );

                 hb_comp_pLastMethod->cType = hb_comp_cVarType;
                 if ( toupper( hb_comp_cVarType ) == 'S' )
                 {
                   pClass = hb_compClassFind( hb_comp_szFromClass );
                   hb_comp_pLastMethod->pClass = pClass;
                   if( ! hb_comp_pLastMethod->pClass )
                   {
                     hb_compGenWarning( hb_comp_szWarnings, 'W', HB_COMP_WARN_CLASS_NOT_FOUND, hb_comp_szFromClass, hb_comp_pLastMethod->szName );
                     hb_comp_pLastMethod->cType = ( isupper(  ( int ) hb_comp_cVarType ) ? 'O' :'o' );
                   }
                 }
                 else
                   pClass = NULL;

                 sprintf( szSetData, "_%s", $1 );

                 hb_comp_pLastMethod = hb_compMethodAdd( hb_comp_pLastClass, szSetData );
                 hb_comp_pLastMethod->cType = hb_comp_cVarType;
                 hb_comp_pLastMethod->iParamCount = 1;

                 hb_comp_pLastMethod->cParamTypes = ( BYTE * ) hb_xgrab( 1 );
                 hb_comp_pLastMethod->cParamTypes[0] = hb_comp_cVarType;

                 hb_comp_pLastMethod->pParamClasses = ( PCOMCLASS * ) hb_xgrab( sizeof( COMCLASS ) );
                 hb_comp_pLastMethod->pParamClasses[0] = pClass;

                 if ( toupper( hb_comp_cVarType ) == 'S' )
                 {
                   hb_comp_pLastMethod->pClass = pClass;

                   /* Resetting */
                   hb_comp_szFromClass = NULL;
                 }
               }
               hb_comp_pLastMethod = NULL;
               hb_comp_cVarType = ' ';
             }
           ;

DecList    :                  {}
           | FormalList
           | FormalList OptList
           ;

FormalList : IdentName AsType                    { hb_compVariableAdd( $1, hb_comp_cVarType ); }
           | '@' IdentName AsType                { hb_compVariableAdd( $2, hb_comp_cVarType + VT_OFFSET_BYREF ); }
           | FormalList ',' IdentName AsType     { hb_compVariableAdd( $3, hb_comp_cVarType ); }
           | FormalList ',' '@' IdentName AsType { hb_compVariableAdd( $4, hb_comp_cVarType + VT_OFFSET_BYREF ); }
           ;

OptList    : ',' OPTIONAL IdentName AsType     { hb_compVariableAdd( $3, hb_comp_cVarType + VT_OFFSET_OPTIONAL ); }
           | ',' OPTIONAL '@' IdentName AsType { hb_compVariableAdd( $4, hb_comp_cVarType + VT_OFFSET_OPTIONAL + VT_OFFSET_BYREF ); }
           | OptList ',' OPTIONAL IdentName AsType     { hb_compVariableAdd( $4, hb_comp_cVarType + VT_OFFSET_OPTIONAL ); }
           | OptList ',' OPTIONAL '@' IdentName AsType { hb_compVariableAdd( $5, hb_comp_cVarType + VT_OFFSET_OPTIONAL + VT_OFFSET_BYREF ); }
           ;

ExecFlow   : IfEndif
           | DoCase
           | DoWhile
           | ForNext
           | BeginSeq
           ;

IfEndif    : IfBegin EndIf                    { hb_compGenJumpHere( $1 ); }
           | IfBegin IfElse EndIf             { hb_compGenJumpHere( $1 ); }
           | IfBegin IfElseIf EndIf           { hb_compGenJumpHere( $1 ); hb_compElseIfFix( $2 ); }
           | IfBegin IfElseIf IfElse EndIf    { hb_compGenJumpHere( $1 ); hb_compElseIfFix( $2 ); }
           ;

EmptyStatements : LineStat                                    { $<lNumber>$ = $<lNumber>1; }
           | EmptyStatements { hb_compLinePush(); } LineStat  { $<lNumber>$ += $<lNumber>3; }
           ;

EmptyStats : /* empty */           { hb_comp_bDontGenLineNum = TRUE; hb_comp_EOL = FALSE; $<lNumber>$ = 0; }
           | EmptyStatements       { hb_comp_EOL = FALSE; $<lNumber>$ = $<lNumber>1; }
           ;

IfBegin    : IF SimpleExpression { ++hb_comp_wIfCounter; hb_compLinePush(); } Crlf { hb_compExprDelete( hb_compExprGenPush( $2 ) ); $$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats
                { $$ = hb_compGenJump( 0 ); hb_compGenJumpHere( $<iNumber>5 ); }

           | IF Variable { ++hb_comp_wIfCounter; hb_compLinePush(); } Crlf { hb_compExprDelete( hb_compExprGenPush( $2 ) ); $$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats
                { $$ = hb_compGenJump( 0 ); hb_compGenJumpHere( $<iNumber>5 ); }

           | IF PareExpList1 { ++hb_comp_wIfCounter; hb_compLinePush(); } Crlf { hb_compExprDelete( hb_compExprGenPush( $2 ) ); $$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats
                { $$ = hb_compGenJump( 0 ); hb_compGenJumpHere( $<iNumber>5 ); }

           | IF PareExpList2 { ++hb_comp_wIfCounter; hb_compLinePush(); } Crlf { hb_compExprDelete( hb_compExprGenPush( $2 ) ); $$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats
                { $$ = hb_compGenJump( 0 ); hb_compGenJumpHere( $<iNumber>5 ); }

           | IF PareExpListN { ++hb_comp_wIfCounter; hb_compLinePush(); } Crlf { hb_compExprDelete( hb_compExprGenPush( $2 ) ); $$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats
                { $$ = hb_compGenJump( 0 ); hb_compGenJumpHere( $<iNumber>5 ); }
           ;

IfElse     : ELSE Crlf { hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE; hb_compLinePush();  }
                EmptyStats
           ;

IfElseIf   : ELSEIF Expression Crlf { hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE; hb_compExprDelete( hb_compExprGenPush( $2 ) ); $<iNumber>$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats { $$ = hb_compElseIfGen( NULL, hb_compGenJump( 0 ) ); hb_compGenJumpHere( $<iNumber>4 ); }

           | IfElseIf ELSEIF Expression Crlf { hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE; hb_compExprDelete( hb_compExprGenPush( $3 ) ); $<iNumber>$ = hb_compGenJumpFalse( 0 ); hb_compLinePush(); }
                EmptyStats { $$ = hb_compElseIfGen( $1, hb_compGenJump( 0 ) ); hb_compGenJumpHere( $<iNumber>5 ); }
           ;

EndIf      : ENDIF                 { --hb_comp_wIfCounter; hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE ); }
           | END                   { --hb_comp_wIfCounter; hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE ); }
           ;

DoCase     : DoCaseBegin
                Cases
             EndCase                  { hb_compElseIfFix( $2 ); }

           | DoCaseBegin
                Otherwise
             EndCase

           | DoCaseBegin
             EndCase

           | DoCaseBegin
                Cases
                Otherwise
             EndCase                   { hb_compElseIfFix( $2 ); }
           ;

EndCase    : ENDCASE
               { --hb_comp_wCaseCounter;
                  hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE );
                }
           | END
               { --hb_comp_wCaseCounter;
                  hb_comp_functions.pLast->bFlags &= ~ ( FUN_WITH_RETURN | FUN_BREAK_CODE );
               }
           ;

DoCaseStart : DOCASE { ++hb_comp_wCaseCounter; } Crlf { hb_compLinePush(); }
            ;

DoCaseBegin : DoCaseStart            { }
            | DoCaseStart Statements {
                        if( $<lNumber>2 > 0 )
                        {
                           hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL );
                        }
                     }
           ;

Cases      : CASE Expression Crlf
               {
                  hb_compExprDelete( hb_compExprGenPush( $2 ) );
                  $<iNumber>$ = hb_compGenJumpFalse( 0 );
                  hb_compLinePush();
               }
             EmptyStats
               {
                  hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE;
                  $$ = hb_compElseIfGen( 0, hb_compGenJump( 0 ) );
                  hb_compGenJumpHere( $<iNumber>4 );
                  hb_compLinePush();
               }

           | Cases CASE Expression Crlf
               {
                  hb_compExprDelete( hb_compExprGenPush( $3 ) );
                  $<iNumber>$ = hb_compGenJumpFalse( 0 );
                  hb_compLinePush();
               }
             EmptyStats
               {
                  hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE;
                  $$ = hb_compElseIfGen( $1, hb_compGenJump( 0 ) );
                  hb_compGenJumpHere( $<iNumber>5 );
                  hb_compLinePush();
               }
           ;

Otherwise  : OTHERWISE Crlf { hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE; hb_compLinePush(); }
                EmptyStats
           | Otherwise OTHERWISE { hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_MAYHEM_IN_CASE, NULL, NULL ); } Crlf
                EmptyStats
           ;

DoWhile    : WhileBegin Expression Crlf
               {
                  hb_compExprDelete( hb_compExprGenPush( $2 ) );
                  $<lNumber>$ = hb_compGenJumpFalse( 0 );
                  hb_compLinePush();
                }
             EmptyStats
               {
                  hb_compLoopHere();
                  hb_compGenJump( $1 - hb_comp_functions.pLast->lPCodePos );
               }
             EndWhile
               {
                  hb_compGenJumpHere( $<lNumber>4 ); --hb_comp_wWhileCounter;
                  hb_compLoopEnd();
                  hb_comp_functions.pLast->bFlags &= ~ FUN_WITH_RETURN;
                }
           ;

WhileBegin : WHILE    { $$ = hb_comp_functions.pLast->lPCodePos; hb_compLinePushIfInside(); ++hb_comp_wWhileCounter; hb_compLoopStart(); }
           ;

EndWhile   : END
           | ENDDO
           ;

ForNext    : FOR LValue ForAssign Expression          /* 1  2  3  4 */
               {
                  hb_compLinePush();
                  ++hb_comp_wForCounter;              /* 5 */
                  $<asExpr>$ = hb_compExprGenStatement( hb_compExprAssign( $2, $4 ) );
               }
             TO Expression StepExpr                   /* 6  7  8 */
               {
                  hb_compLoopStart();
                  $<lNumber>$ = hb_comp_functions.pLast->lPCodePos;  /* 9 */
                  hb_compExprGenPush( $2 );              /* counter */
                  hb_compExprGenPush( $7 );              /* end */
                  if( $<asExpr>8 )
                     hb_compExprGenPush( $<asExpr>8 );   /* step */
               }
             Crlf                                     /* 10 */
               {
                  if( $<asExpr>8 )
                     hb_compGenPCode1( HB_P_FORTEST );
                  else
                     hb_compGenPCode1( HB_P_LESSEQUAL );
                  $<lNumber>$ = hb_compGenJumpFalse( 0 );   /* 11 */
                  hb_compLinePush();
               }
             ForStatements                            /* 12 */
               {
                  hb_compLoopHere();
                  if( $<asExpr>8 )
                     hb_compExprClear( hb_compExprGenStatement( hb_compExprSetOperand( hb_compExprNewPlusEq( $2 ), $<asExpr>8 ) ) );
                  else
                     hb_compExprClear( hb_compExprGenStatement( hb_compExprNewPreInc( $2 ) ) );
                  hb_compGenJump( $<lNumber>9 - hb_comp_functions.pLast->lPCodePos );
                  hb_compGenJumpHere( $<lNumber>11 );
                  hb_compLoopEnd();
                  hb_compExprDelete( $7 );
                  hb_compExprDelete( $<asExpr>5 ); /* deletes $5, $2, $4 */
                  if( $<asExpr>8 )
                     hb_compExprDelete( $<asExpr>8 );
                  hb_comp_functions.pLast->bFlags &= ~ FUN_WITH_RETURN;
               }
           ;

ForAssign  : '='
           | INASSIGN
           ;

StepExpr   : /* default step expression */       { $<asExpr>$ = NULL; }
           | STEP Expression                     { $<asExpr>$ = $2; }
           ;

ForStatements : EmptyStats NEXT                     { --hb_comp_wForCounter; }
           | EmptyStats NEXT IdentName              { --hb_comp_wForCounter; }
           | EmptyStats END                         { --hb_comp_wForCounter; }
           | EmptyStats END IdentName               { --hb_comp_wForCounter; }
           ;

BeginSeq   : BEGINSEQ { ++hb_comp_wSeqCounter; $<lNumber>$ = hb_compSequenceBegin(); } Crlf { hb_compLinePush(); }
                EmptyStats
                {
                  /* Set jump address for HB_P_SEQBEGIN opcode - this address
                   * will be used in BREAK code if there is no RECOVER clause
                   */
                  hb_compGenJumpHere( $<lNumber>2 );
                  $<lNumber>$ = hb_compSequenceEnd();
                  hb_compLinePush();
                }
                RecoverSeq
                {
                   /* Replace END address with RECOVER address in
                    * HB_P_SEQBEGIN opcode if there is RECOVER clause
                    */
                   if( $<lNumber>7 )
                      hb_compGenJumpThere( $<lNumber>2, $<lNumber>7-( hb_comp_bLineNumbers ? 3 : 0 ) );
                }
             END
             {
                /* Fix END address
                 * There is no line number after HB_P_SEQEND in case no
                 * RECOVER clause is used
                 */
                hb_compGenJumpThere( $<lNumber>6, hb_comp_functions.pLast->lPCodePos-((hb_comp_bLineNumbers && !$<lNumber>7)?3:0) );
                if( $<lNumber>7 )   /* only if there is RECOVER clause */
                   hb_compLinePushIfDebugger();
                else
                   --hb_comp_wSeqCounter;  /* RECOVER is also considered as end of sequence */
                hb_compSequenceFinish( $<lNumber>2, $<iNumber>5 );
                hb_comp_functions.pLast->bFlags &= ~ FUN_WITH_RETURN;
             }
           ;

RecoverSeq : /* no recover */  { $<lNumber>$ = 0; }
           | RecoverEmpty Crlf { $<lNumber>$ = $<lNumber>1; hb_compLinePush(); } EmptyStats
           | RecoverUsing Crlf { $<lNumber>$ = $<lNumber>1; hb_compLinePush(); } EmptyStats
           ;

RecoverEmpty : RECOVER
               {
                  hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE;
                  $<lNumber>$ = hb_comp_functions.pLast->lPCodePos;
                  --hb_comp_wSeqCounter;
                  hb_compGenPCode2( HB_P_SEQRECOVER, HB_P_POP, ( BOOL ) 1 );
               }
           ;

RecoverUsing : RECOVERUSING IdentName
               {
                  hb_comp_functions.pLast->bFlags &= ~ FUN_BREAK_CODE;
                  $<lNumber>$ = hb_comp_functions.pLast->lPCodePos;
                  --hb_comp_wSeqCounter;
                  hb_compGenPCode1( HB_P_SEQRECOVER );
                  hb_compGenPopVar( $2 );
               }
           ;

/* NOTE: In Clipper all variables used in DO .. WITH are passed by reference
 * however if they are part of an expression then they are passed by value
 * for example:
 * DO .. WITH ++variable
 * will pass the value of variable not a reference
 */
DoName     : IdentName       { $$ = hb_compExprNewFunName( $1 ); }
           | MacroVar         { $$ = $1; }
           | MacroExpr    { $$ = $1; }
           ;

DoProc     : DO DoName
               { $$ = hb_compExprNewFunCall( $2, NULL );  if( hb_comp_bAutoOpen ) hb_compCompile( $2->value.asSymbol, 0, NULL ); }
           | DO DoName WITH DoArgList
               { $$ = hb_compExprNewFunCall( $2, $4 ); hb_compCompile( $2->value.asSymbol, 0, NULL ); }
           | WHILE WITH DoArgList
               { $$ = hb_compExprNewFunCall( hb_compExprNewFunName( hb_strdup("WHILE") ), $3 ); hb_compCompile( "WHILE", 0, NULL );}
           ;

DoArgList  : ','                       { $$ = hb_compExprAddListExpr( hb_compExprNewArgList( hb_compExprNewNil() ), hb_compExprNewNil() ); }
           | ',' DoArgument            { $$ = hb_compExprAddListExpr( hb_compExprNewArgList( hb_compExprNewNil() ), $2 ); }
           | DoArgument                { $$ = hb_compExprNewArgList( $1 ); }
           | DoArgList ','             { $$ = hb_compExprAddListExpr( $1, hb_compExprNewNil() ); }
           | DoArgList ',' DoArgument  { $$ = hb_compExprAddListExpr( $1, $3 ); }
           ;

DoArgument : IdentName                { $$ = hb_compExprNewVarRef( $1 ); }
           | '@' IdentName '(' ')'    { $$ = hb_compExprNewFunRef( $2 ); }
           | SimpleExpression          { $$ = $1; }
           | PareExpList               { $$ = $1; }
           ;

Crlf       : '\n'          { ++hb_comp_iLine; hb_comp_EOL = TRUE; }
           | ';'           { hb_comp_bDontGenLineNum = TRUE; }
           ;

%%

/*
 ** ------------------------------------------------------------------------ **
 */

int hb_compYACCMain( char * szName )
{
   /* Generate the starting procedure frame
      */
   if( hb_comp_bStartProc )
      hb_compFunctionAdd( hb_strupr( hb_strdup( szName ) ), HB_FS_PUBLIC, FUN_PROCEDURE );
   else
         /* Don't pass the name of module if the code for starting procedure
         * will be not generated. The name cannot be placed as first symbol
         * because this symbol can be used as function call or memvar's name.
         */
      hb_compFunctionAdd( hb_strupr( hb_strdup( "" ) ), HB_FS_PUBLIC, FUN_PROCEDURE );

   yyparse();

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

   /* Close processed file (it is opened in hb_compInclude() function )
   */
   fclose( yyin );

   hb_comp_files.pLast = NULL;

   return 0;
}


/* ------------------------------------------------------------------------ */

void yyerror( char * s )
{
   hb_comp_EOL = FALSE; /* we are in the middle of a line */
   if( yytext[ 0 ] == '\n' )
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_YACC, s, "<eol>" );
   else
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_YACC, s, yytext );
}


BOOL hb_compInclude( char * szFileName, PATHNAMES * pSearch )
{
   PFILE pFile;

   yyin = fopen( szFileName, "r" );
   if( ! yyin )
   {
      if( pSearch )
      {
         PHB_FNAME pFileName = hb_fsFNameSplit( szFileName );

         while( pSearch && !yyin )
         {
            char szFName[ _POSIX_PATH_MAX ];    /* filename to parse */

            pFileName->szPath = pSearch->szPath;
            hb_fsFNameMerge( szFName, pFileName );
            yyin = fopen( szFName, "r" );
            if( ! yyin )
            {
               pSearch = pSearch->pNext;
               if( ! pSearch )
                  return FALSE;
            }
         }

         hb_xfree( ( void * ) pFileName );
      }
      else
         return FALSE;
   }

   pFile = ( PFILE ) hb_xgrab( sizeof( _FILE ) );
   pFile->handle = yyin;
   pFile->pBuffer = hb_xgrab( HB_PP_BUFF_SIZE );
   pFile->iBuffer = pFile->lenBuffer = 10;
   pFile->szFileName = szFileName;
   pFile->pPrev = NULL;

/*
   if( ! hb_comp_files.iFiles )
      hb_comp_files.pLast = pFile;
   else
   {
      hb_comp_files.pLast->iLine = hb_comp_iLine;
      hb_comp_iLine = 1;
      pFile->pPrev = hb_comp_files.pLast;
      hb_comp_files.pLast  = pFile;
   }
#ifdef __cplusplus
   yy_switch_to_buffer( ( YY_BUFFER_STATE ) ( pFile->pBuffer = yy_create_buffer( yyin, 8192 * 2 ) ) );
#else
   yy_switch_to_buffer( pFile->pBuffer = yy_create_buffer( yyin, 8192 * 2 ) );
#endif
*/

   hb_comp_files.pLast = pFile;
#ifdef __cplusplus
   yy_switch_to_buffer( ( YY_BUFFER_STATE ) ( hb_comp_buffer = ( char * ) yy_create_buffer( yyin, 8192 * 2 ) ) );
#else
   yy_switch_to_buffer( hb_comp_buffer = yy_create_buffer( yyin, 8192 * 2 ) );
#endif
   hb_comp_files.iFiles++;

   return TRUE;
}

int yywrap( void )   /* handles the EOF of the currently processed file */
{
   if( hb_comp_files.iFiles == 1 )
   {
      hb_xfree( hb_comp_files.pLast->pBuffer );
      return 1;      /* we have reached the main EOF */
   }

/*
   else
   {
      void * pLast;

      pLast = hb_comp_files.pLast;
      fclose( hb_comp_files.pLast->handle );
      hb_comp_files.pLast = ( PFILE ) ( ( PFILE ) hb_comp_files.pLast )->pPrev;
      hb_comp_iLine = hb_comp_files.pLast->iLine;
#ifdef __cplusplus
      yy_delete_buffer( ( YY_BUFFER_STATE ) ( ( PFILE ) pLast )->pBuffer );
#else
      yy_delete_buffer( ( ( PFILE ) pLast )->pBuffer );
#endif
      free( pLast );
      hb_comp_files.iFiles--;
      yyin = hb_comp_files.pLast->handle;
#ifdef __cplusplus
      yy_switch_to_buffer( ( YY_BUFFER_STATE ) hb_comp_files.pLast->pBuffer );
#else
      yy_switch_to_buffer( hb_comp_files.pLast->pBuffer );
#endif
      return 0;
   }
*/ /* we close the currently include file and continue */

   return 0;
}

/* ************************************************************************* */

/*
 * This function stores the position in pcode buffer where the FOR/WHILE
 * loop starts. It will be used to fix any LOOP/EXIT statements
 */
static void hb_compLoopStart( void )
{
   PTR_LOOPEXIT pLoop = ( PTR_LOOPEXIT ) hb_xgrab( sizeof( LOOPEXIT ) );

   if( hb_comp_pLoops )
   {
      PTR_LOOPEXIT pLast = hb_comp_pLoops;

      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pLoop;
   }
   else
      hb_comp_pLoops = pLoop;

   pLoop->pNext       = NULL;
   pLoop->pExitList   = NULL;
   pLoop->pLoopList   = NULL;
   pLoop->ulOffset    = hb_comp_functions.pLast->lPCodePos;  /* store the start position */
   pLoop->iLine       = hb_comp_iLine;
   pLoop->wSeqCounter = hb_comp_wSeqCounter;  /* store current SEQUENCE counter */
}

/*
 * Stores the position of LOOP statement to fix it later at the end of loop
 */
static void hb_compLoopLoop( void )
{
   if( ! hb_comp_pLoops )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "LOOP", NULL );
   }
   else
   {
      PTR_LOOPEXIT pLast, pLoop;

      pLoop = ( PTR_LOOPEXIT ) hb_xgrab( sizeof( LOOPEXIT ) );

      pLoop->pLoopList = NULL;
      pLoop->ulOffset = hb_comp_functions.pLast->lPCodePos;  /* store the position to fix */

      pLast = hb_comp_pLoops;
      while( pLast->pNext )
         pLast = pLast->pNext;

      if( pLast->wSeqCounter != hb_comp_wSeqCounter )
      {
         /* Attempt to LOOP from BEGIN/END sequence
         * Current SEQUENCE counter is different then at the beginning of loop
         * Notice that LOOP is allowed in RECOVER code.
         */
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "LOOP", NULL );
      }
      else
      {
         while( pLast->pLoopList )
            pLast = pLast->pLoopList;

         pLast->pLoopList = pLoop;

         hb_compGenJump( 0 );
      }
   }
}

/*
 * Stores the position of EXIT statement to fix it later at the end of loop
 */
static void hb_compLoopExit( void )
{
   if( ! hb_comp_pLoops )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "EXIT", NULL );
   }
   else
   {
      PTR_LOOPEXIT pLast, pLoop;

      pLoop = ( PTR_LOOPEXIT ) hb_xgrab( sizeof( LOOPEXIT ) );

      pLoop->pExitList = NULL;
      pLoop->ulOffset = hb_comp_functions.pLast->lPCodePos;  /* store the position to fix */

      pLast = hb_comp_pLoops;
      while( pLast->pNext )
         pLast = pLast->pNext;

      if( pLast->wSeqCounter != hb_comp_wSeqCounter )
      {
         /* Attempt to LOOP from BEGIN/END sequence
         * Current SEQUENCE counter is different then at the beginning of loop
         * Notice that LOOP is allowed in RECOVER code.
         */
         hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_EXIT_IN_SEQUENCE, "EXIT", NULL );
      }
      else
      {
         while( pLast->pExitList )
            pLast = pLast->pExitList;

         pLast->pExitList = pLoop;

         hb_compGenJump( 0 );
      }
   }
}

/*
 * Fixes the LOOP statement
 */
static void hb_compLoopHere( void )
{
   PTR_LOOPEXIT pLoop = hb_comp_pLoops, pFree;

   if( pLoop )
   {
      while( pLoop->pNext )
         pLoop = pLoop->pNext;

      pLoop = pLoop->pLoopList;
      while( pLoop )
      {
         hb_compGenJumpHere( pLoop->ulOffset + 1 );
         pFree = pLoop;
         pLoop = pLoop->pLoopList;
         hb_xfree( ( void * ) pFree );
      }
   }
}

/*
 * Fixes the EXIT statements and releases memory allocated for current loop
 */
static void hb_compLoopEnd( void )
{
   PTR_LOOPEXIT pExit, pLoop = hb_comp_pLoops, pLast = hb_comp_pLoops, pFree;

   if( pLoop )
   {
      while( pLoop->pNext )
      {
         pLast = pLoop;
         pLoop = pLoop->pNext;
      }

      pExit = pLoop->pExitList;
      while( pExit )
      {
         hb_compGenJumpHere( pExit->ulOffset + 1 );
         pFree = pExit;
         pExit = pExit->pExitList;
         hb_xfree( ( void * ) pFree );
      }

      pLast->pNext = NULL;
      if( pLoop == hb_comp_pLoops )
         hb_comp_pLoops = NULL;
      hb_xfree( ( void * ) pLoop );
   }
}

static void * hb_compElseIfGen( void * pFirst, ULONG ulOffset )
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


static void hb_compElseIfFix( void * pFixElseIfs )
{
   PELSEIF pFix = ( PELSEIF ) pFixElseIfs;

   while( pFix )
   {
      hb_compGenJumpHere( pFix->ulOffset );
      pFix = pFix->pNext;
   }
}

static void hb_compRTVariableAdd( HB_EXPR_PTR pVar, BOOL bPopInitValue )
{
   HB_RTVAR_PTR pRTvar = ( HB_RTVAR_PTR ) hb_xgrab( sizeof( HB_RTVAR ) );

   pRTvar->pVar = pVar;
   pRTvar->bPopValue = bPopInitValue;
   pRTvar->pNext = NULL;
   pRTvar->pPrev = NULL;

   if( hb_comp_rtvars )
   {
      HB_RTVAR_PTR pLast = hb_comp_rtvars;
      while( pLast->pNext )
         pLast = pLast->pNext;
      pLast->pNext = pRTvar;
      pRTvar->pPrev = pLast;
   }
   else
      hb_comp_rtvars = pRTvar;
}

static void hb_compRTVariableGen( char * szCreateFun )
{
   USHORT usCount = 0;
   HB_RTVAR_PTR pVar = hb_comp_rtvars;
   HB_RTVAR_PTR pDel;

   /* generate the function call frame */
   hb_compGenPushSymbol( hb_strdup( szCreateFun ), 1);
   hb_compGenPushNil();

   /* push variable names to create */
   while( pVar->pNext )
   {
      hb_compExprGenPush( pVar->pVar );
      pVar = pVar->pNext;
      ++usCount;
   }
   hb_compExprGenPush( pVar->pVar );
   ++usCount;

   /* call function that will create either PUBLIC or PRIVATE variables */
   if( usCount > 255 )
      hb_compGenPCode3( HB_P_DO, HB_LOBYTE( usCount ), HB_HIBYTE( usCount ), ( BOOL ) 1 );
   else
      hb_compGenPCode2( HB_P_DOSHORT, ( BYTE ) usCount, ( BOOL ) 1 );

   /* pop initial values */
   while( pVar )
   {
      if( pVar->bPopValue )
         hb_compExprDelete( hb_compExprGenPop( pVar->pVar ) );
      else
         hb_compExprDelete( pVar->pVar );
      pDel = pVar;
      pVar = pVar->pPrev;
      hb_xfree( pDel );
   }
   hb_comp_rtvars = NULL;
}

static void hb_compVariableDim( char * szName, HB_EXPR_PTR pInitValue )
{
  if( hb_comp_iVarScope == VS_PUBLIC || hb_comp_iVarScope == VS_PRIVATE )
  {
     USHORT uCount = hb_compExprListLen( pInitValue );
     hb_compVariableAdd( szName, 'A' );
     hb_compExprDelete( hb_compExprGenPush( pInitValue ) );
     hb_compGenPCode3( HB_P_ARRAYDIM, HB_LOBYTE( uCount ), HB_HIBYTE( uCount ), ( BOOL ) 1 );
     hb_compRTVariableAdd( hb_compExprNewRTVar( szName, NULL ), TRUE );
  }
  else if( hb_comp_iVarScope == VS_STATIC )
  {
     USHORT uCount = hb_compExprListLen( pInitValue );
     HB_EXPR_PTR pVar = hb_compExprNewVar( szName );
     HB_EXPR_PTR pAssign;

     /* create a static variable */
     hb_compVariableAdd( szName, 'A' );
     hb_compStaticDefStart();   /* switch to statics pcode buffer */
     /* create an array */
     hb_compExprGenPush( pInitValue );
     hb_compGenPCode3( HB_P_ARRAYDIM, HB_LOBYTE( uCount ), HB_HIBYTE( uCount ), ( BOOL ) 1 );
     /* check if valid initializers were used but don't generate any code */
     pAssign = hb_compExprAssignStatic( pVar, pInitValue );
     /* now pop an array */
     hb_compExprGenPop( pVar );
     /* delete all used expressions */
     hb_compExprDelete( pAssign );
     hb_compStaticDefEnd();
  }
  else
  {
     USHORT uCount = hb_compExprListLen( pInitValue );

     hb_compVariableAdd( szName, 'A' );
     hb_compExprDelete( hb_compExprGenPush( pInitValue ) );
     hb_compGenPCode3( HB_P_ARRAYDIM, HB_LOBYTE( uCount ), HB_HIBYTE( uCount ), ( BOOL ) 1 );
     hb_compExprDelete( hb_compExprGenPop( hb_compExprNewVar( szName ) ) );
  }
}

void * hb_compGet_pLoops( void )
{
   return (void *) hb_comp_pLoops;
}

void hb_compSet_pLoops( void * pLoops )
{
   hb_comp_pLoops = (PTR_LOOPEXIT) pLoops;
}

void * hb_compGet_rtvars( void )
{
   return (void *) hb_comp_rtvars;
}

void hb_compSet_rtvars( void * rtvars )
{
   hb_comp_rtvars = (HB_RTVAR_PTR) rtvars;
}

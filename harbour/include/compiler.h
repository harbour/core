/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
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

#ifndef HB_COMPILER_H_
#define HB_COMPILER_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <malloc.h>     /* required for allocating and freeing memory */
#include <ctype.h>
#include <time.h>

#include "hbsetup.h"
#include "extend.h"
#include "pcode.h"      /* pcode values */
#include "hberrors.h"
#include "hbpp.h"
#include "hbver.h"

/* compiler related declarations */

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


/* locals, static, public variables support */
typedef struct _VAR
{
   char * szName;          /* variable name */
   char * szAlias;         /* variable alias namespace */
   int    iUsed;           /* number of times used */
   char   cType;           /* optional strong typing */
   struct _VAR * pNext;    /* pointer to next defined variable */
} VAR, * PVAR;

/* pcode chunks bytes size */
#define PCODE_CHUNK   100

/* structure to hold a Clipper defined function */
typedef struct __FUNC      /* functions definition support */
{
   char * szName;          /* name of a defined Clipper function */
   char   cScope;          /* scope of a defined Clipper function */
   BYTE   bFlags;          /* some flags we may need */
   USHORT wParamCount;     /* number of declared parameters */
   USHORT wParamNum;       /* current parameter number */
   PVAR   pLocals;         /* pointer to local variables list */
   PVAR   pStatics;        /* pointer to static variables list */
   PVAR   pFields;         /* pointer to fields variables list */
   PVAR   pMemvars;        /* pointer to memvar variables list */
   BYTE * pCode;           /* pointer to a memory block where pcode is stored */
   ULONG  lPCodeSize;      /* total memory size for pcode */
   ULONG  lPCodePos;       /* actual pcode offset */
   int    iStaticsBase;    /* base for this function statics */
   struct __FUNC * pOwner; /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;  /* pointer to the next defined function */
} _FUNC, * PFUNCTION;

/* structure to control all Clipper defined functions */
typedef struct
{
   PFUNCTION pFirst;       /* pointer to the first defined funtion */
   PFUNCTION pLast;        /* pointer to the last defined function */
   int       iCount;       /* number of defined functions */
} FUNCTIONS;

/* compiler symbol support structure */
typedef struct _COMSYMBOL
{
   char * szName;              /* the name of the symbol */
   char   cScope;              /* the scope of the symbol */
   char   cType;
   struct _COMSYMBOL * pNext;  /* pointer to the next defined symbol */
} COMSYMBOL, * PCOMSYMBOL;

/* symbol table support structures */
typedef struct
{
   PCOMSYMBOL pFirst;      /* pointer to the first defined symbol */
   PCOMSYMBOL pLast;       /* pointer to the last defined symbol */
   int        iCount;      /* number of defined symbols */
} SYMBOLS;

typedef struct HB_EXPR_
{
   union
   {
      char *asString;      /* literal strings */
      char *asSymbol;      /* variable name */
      BOOL asLogical;      /* logical value */
      struct
      {
         long lVal;           /* long value */
         double dVal;         /* double value */
         unsigned char bDec;  /* unsigned char used intentionally */
         unsigned char NumType;    /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         struct HB_EXPR_ *pVar;        /* macro variable */
         char * szNameExt;             /* text after the macro terminator */
      } asMacro;
      struct
      {
         struct HB_EXPR_ *pExprList;    /* list elements */
         struct HB_EXPR_ *pIndex;       /* array index, others */
      } asList;
      struct
      {
         struct HB_EXPR_ *pAlias;      /* alias expression */
         char * szVarName;             /* aliased variable */
         struct HB_EXPR_ *pExpList;    /* aliased expression list */
      } asAlias;
      struct
      {
         char * szFunName;             /* function name */
         struct HB_EXPR_ *pParms;       /* function call parameters */
      } asFunCall;
      struct
      {
         struct HB_EXPR_ *pObject;     /* object */
         char * szMessage;             /* message */
         struct HB_EXPR_ *pParms;      /* method parameters */
      } asMessage;
      struct
      {
         struct HB_EXPR_ *pLeft;       /* object */
         struct HB_EXPR_ *pRight;      /* object */
      } asOperator;
   } value;
   ULONG ulLength;
   unsigned char ExprType;  /* internal expression type */
   USHORT ValType;          /* language level value type */
   struct HB_EXPR_ *pNext;  /* next expression in the list of expressions */
} HB_EXPR, *HB_EXPR_PTR;


char * yy_strdup( char * p );  /* this will exit if there is not enough memory */
char * yy_strupr( char * p );


#define VS_LOCAL      1
#define VS_STATIC     2
#define VS_FIELD      4
#define VS_PARAMETER  8
#define VS_PRIVATE    64
#define VS_PUBLIC     128
#define VS_MEMVAR     ( VS_PUBLIC | VS_PRIVATE )

/*
 * flags for bFlags member
*/
#define FUN_STATEMENTS        1 /* Function have at least one executable statement */
#define FUN_USES_STATICS      2 /* Function uses static variables */
#define FUN_PROCEDURE         4 /* This is a procedure that shouldn't return value */
#define FUN_BREAK_CODE        8 /* last statement breaks execution flow */
#define FUN_USES_LOCAL_PARAMS 16 /* parameters are declared using () */
#define FUN_WITH_RETURN       32  /* there was RETURN statement in previous line */


void      hb_compFunctionAdd( char * szFunName, HB_SYMBOLSCOPE cScope, int iType ); /* starts a new Clipper language function definition */
PFUNCTION hb_compFunctionFind( char * szFunName ); /* locates a previously defined function */
USHORT    hb_compFunctionGetPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */
PFUNCTION hb_compFunctionKill( PFUNCTION );    /* releases all memory allocated by function and returns the next one */

PFUNCTION hb_compFunCallAdd( char * szFuntionName );
PFUNCTION hb_compFunCallFind( char * szFunName ); /* locates a previously defined called function */
void      hb_compFunCallCheck( char *, int );

void hb_compVariableAdd( char * szVarName, char cType ); /* add a new param, local, static variable to a function definition or a public or private */
PVAR hb_compVariableFind( PVAR pVars, USHORT wOrder ); /* returns a variable if defined or zero */

PCOMSYMBOL hb_compSymbolAdd( char *, USHORT * );
PCOMSYMBOL hb_compSymbolKill( PCOMSYMBOL );    /* releases all memory allocated by symbol and returns the next one */
PCOMSYMBOL hb_compSymbolFind( char *, USHORT * ); /* returns a symbol pointer from the symbol table */
PCOMSYMBOL hb_compSymbolGetPos( USHORT );   /* returns a symbol based on its index on the symbol table */
USHORT     hb_compSymbolFixPos( USHORT );    /* converts symbol's compile-time position into generation-time position */

void hb_compGenBreak( void );  /* generate code for BREAK statement */

void hb_compExternGen( void ); /* generates the symbols for the EXTERN names */
void hb_compExternAdd( char * szExternName ); /* defines a new extern name */

ULONG hb_compGenJump( LONG lOffset );                /* generates the pcode to jump to a specific offset */
ULONG hb_compGenJumpFalse( LONG lOffset );           /* generates the pcode to jump if false */
ULONG hb_compGenJumpTrue( LONG lOffset );            /* generates the pcode to jump if true */
void hb_compGenJumpHere( ULONG ulOffset );             /* returns the pcode pos where to set a jump offset */
void hb_compGenJumpThere( ULONG ulFrom, ULONG ulTo ); /* sets a jump offset */


void hb_compLinePush( void ); /* generates the pcode with the currently compiled source code line */
void hb_compLinePushIfDebugger( void ); /* generates the pcode with the currently compiled source code line */
void hb_compLinePushIfInside( void );   /* generates the pcode with the currently compiled source code line */

void hb_compGenMessage( char * szMsgName );       /* sends a message to an object */
void hb_compGenMessageData( char * szMsg );     /* generates an underscore-symbol name for a data assignment */
void hb_compGenPopVar( char * szVarName );         /* generates the pcode to pop a value from the virtual machine stack onto a variable */
void hb_compGenPushDouble( double fNumber, BYTE bDec ); /* Pushes a number on the virtual machine stack */
void hb_compGenPushFunCall( char * );             /* generates the pcode to push function's call */
void hb_compGenPushVar( char * szVarName );        /* generates the pcode to push a variable value to the virtual machine stack */
void hb_compGenPushVarRef( char * szVarName );    /* generates the pcode to push a variable by reference to the virtual machine stack */
void hb_compGenPushInteger( int iNumber );        /* Pushes a integer number on the virtual machine stack */
void hb_compGenPushLogical( int iTrueFalse );     /* pushes a logical value on the virtual machine stack */
void hb_compGenPushLong( long lNumber );          /* Pushes a long number on the virtual machine stack */
void hb_compGenPushNil( void );                   /* Pushes nil on the virtual machine stack */
void hb_compGenPushString( char * szText, ULONG ulLen );       /* Pushes a string on the virtual machine stack */
void hb_compGenPushSymbol( char * szSymbolName, int iIsFunction ); /* Pushes a symbol on to the Virtual machine stack */
void hb_compGenPushAliasedVar( char *, BOOL, char *, long );
void hb_compGenPopAliasedVar( char *, BOOL, char *, long );
void hb_compGenPushFunRef( char * );
void hb_compGenPCode1( BYTE );             /* generates 1 byte of pcode */
void hb_compGenPCode3( BYTE, BYTE, BYTE ); /* generates 3 bytes of pcode */
void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize );  /* copy bytes to a pcode buffer */

/* Codeblocks */
void hb_compCodeBlockStart( void );        /* starts a codeblock creation */
void hb_compCodeBlockEnd( void );          /* end of codeblock creation */

ULONG hb_compSequenceBegin( void );
ULONG hb_compSequenceEnd( void );
void hb_compSequenceFinish( ULONG, int );

/* support for FIELD declaration */
void hb_compFieldSetAlias( char *, int );
int hb_compFieldsCount( void );

/* Static variables */
void hb_compStaticDefStart( void );
void hb_compStaticDefEnd( void );

HB_EXPR_PTR hb_compErrorStatic( char *, HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorType( HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorIndex( HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorSyntax( HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorLValue( HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorBound( HB_EXPR_PTR );
HB_EXPR_PTR hb_compErrorAlias( HB_EXPR_PTR );
void hb_compErrorDuplVar( char * );
HB_EXPR_PTR hb_compWarnMeaningless( HB_EXPR_PTR );

void hb_compGenError( char* _szErrors[], char cPrefix, int iError, char * szError1, char * szError2 );
void hb_compGenWarning( char* _szWarnings[], char cPrefix, int iWarning, char * szWarning1, char * szWarning2);

/* variable used by compiler
 */
extern int    hb_comp_iLine;
extern FUNCTIONS   hb_comp_functions;
extern FUNCTIONS   hb_comp_funcalls;
extern SYMBOLS     hb_comp_symbols;
extern PATHNAMES * hb_comp_pIncludePath;
extern PFUNCTION   hb_comp_pInitFunc;
extern PHB_FNAME   hb_comp_pFileName;
extern BOOL   hb_comp_bPPO;
extern FILE * hb_comp_yyppo;
extern BOOL   hb_comp_bStartProc;
extern BOOL   hb_comp_bLineNumbers;
extern BOOL   hb_comp_bQuiet;
extern BOOL   hb_comp_bRestrictSymbolLength;
extern BOOL   hb_comp_bShortCuts;
extern int    hb_comp_iWarnings;
extern BOOL   hb_comp_bAnyWarning;
extern BOOL   hb_comp_bAutoMemvarAssume;
extern BOOL   hb_comp_bForceMemvars;
extern BOOL   hb_comp_bDebugInfo;
extern char   hb_comp_szPrefix[ 20 ];
extern BOOL   hb_comp_bGenCVerbose;
extern int    hb_comp_iExitLevel;
extern int    hb_comp_iFunctionCnt;
extern char   hb_comp_cVarType;
extern int    hb_comp_iVarScope;
extern BOOL   hb_comp_bDontGenLineNum;
extern FILES  hb_comp_files;
extern int    hb_comp_iStaticCnt;
extern int    hb_comp_iErrorCount;

extern USHORT hb_comp_wSeqCounter;
extern USHORT hb_comp_wForCounter;
extern USHORT hb_comp_wIfCounter;
extern USHORT hb_comp_wWhileCounter;
extern USHORT hb_comp_wCaseCounter;

extern char * hb_comp_szErrors[];
extern char * hb_comp_szWarnings[];


HB_EXPR_PTR hb_compExprNewEmpty( void );
HB_EXPR_PTR hb_compExprNewNil( void );
HB_EXPR_PTR hb_compExprNewDouble( double, BYTE );
HB_EXPR_PTR hb_compExprNewLong( LONG );
HB_EXPR_PTR hb_compExprNewString( char * );
HB_EXPR_PTR hb_compExprNewLogical( int );
HB_EXPR_PTR hb_compExprNewSelf( void );
HB_EXPR_PTR hb_compExprNewCodeBlock( void );
HB_EXPR_PTR hb_compExprNewArray( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewArrayAt( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewVar( char * );
HB_EXPR_PTR hb_compExprNewAliasVar( HB_EXPR_PTR, char * );
HB_EXPR_PTR hb_compExprNewAliasExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewSymbol( char * );
HB_EXPR_PTR hb_compExprNewEQ( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewLT( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewLE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewGT( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewGE( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewIN( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPlus( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMinus( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMult( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewDiv( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMod( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPower( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAssign( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewEqual( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPlusEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMinusEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMultEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewDivEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewModEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewExpEq( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPostInc( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPostDec( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPreInc( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewPreDec( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewAnd( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewOr( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNot( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewNegate( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewMacro( HB_EXPR_PTR, char * );
HB_EXPR_PTR hb_compExprNewVarRef( char * );
HB_EXPR_PTR hb_compExprNewFunRef( char * );
HB_EXPR_PTR hb_compExprNewCodeblockExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewFunCall( char *, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewFunCallArg( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewSend( HB_EXPR_PTR, char * );
HB_EXPR_PTR hb_compExprNewMethodCall( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprSetOperand( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewList( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewArgList( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAddListExpr( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprNewIIF( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprReduce( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAssign( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprEqual( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprAssignStatic( HB_EXPR_PTR, HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR );
ULONG hb_compExprListLen( HB_EXPR_PTR );
void hb_compExprDelete( HB_EXPR_PTR );
void hb_compExprClear( HB_EXPR_PTR );
char * hb_compExprDescription( HB_EXPR_PTR );
HB_EXPR_PTR hb_compExprCBVarAdd( HB_EXPR_PTR, char *, BYTE );

#endif /* HB_COMPILER_H_ */

/*
 * $Id$
 */

/*
   Harbour Project source code

   Harbour Compiler header

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

#ifndef HB_COMPILER_H_
#define HB_COMPILER_H_

#include "hbpp.h"

/* compiler related declarations */

/* locals, static, public variables support */
typedef struct _VAR
{
   char * szName;          /* variable name */
   char * szAlias;         /* variable alias namespace */
   int    iUsed;           /* number of times used */
   char   cType;           /* optional strong typing */
   struct _VAR * pNext;    /* pointer to next defined variable */
} VAR, * PVAR;

/* structure to hold a Clipper defined function */
typedef struct __FUNC      /* functions definition support */
{
   char * szName;          /* name of a defined Clipper function */
   char   cScope;          /* scope of a defined Clipper function */
   BYTE   bFlags;          /* some flags we may need */
   WORD   wParamCount;     /* number of declared parameters */
   WORD   wParamNum;       /* current parameter number */
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

/* locals, static, public variables support */
typedef struct _STACK_VAL_TYPE
{
   char cType;                      /* type of stack value */
   struct _STACK_VAL_TYPE * pPrev;    /* pointer to previous stack value's type */
} STACK_VAL_TYPE, * PSTACK_VAL_TYPE;

extern PFUNCTION GetFunction( char * szFunName ); /* locates a previously defined function */
extern WORD      GetFunctionPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */

extern void *   hb_xgrab( ULONG lSize );   /* allocates memory, exists on failure */
extern void *   hb_xrealloc( void * pMem, ULONG lSize );   /* reallocates memory */
extern void     hb_xfree( void * pMem );    /* frees memory */

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

char * yy_strdup( char * p );  /* this will exit if there is not enough memory */
char * yy_strupr( char * p );

#if 0
static void __yy_memcpy( char * from, char * to, int count ); /* Bison prototype */
#endif

extern WORD FixSymbolPos( WORD );    /* converts symbol's compile-time position into generation-time position */
extern PFUNCTION GetFuncall( char * szFunName ); /* locates a previously defined called function */
extern PVAR GetVar( PVAR pVars, WORD wOrder ); /* returns a variable if defined or zero */
extern PCOMSYMBOL GetSymbol( char *, WORD * ); /* returns a symbol pointer from the symbol table */
extern PCOMSYMBOL GetSymbolOrd( WORD );   /* returns a symbol based on its index on the symbol table */
extern PFUNCTION KillFunction( PFUNCTION );    /* releases all memory allocated by function and returns the next one */
extern PCOMSYMBOL KillSymbol( PCOMSYMBOL );    /* releases all memory allocated by symbol and returns the next one */

extern FUNCTIONS functions, funcalls;
extern PFUNCTION _pInitFunc;
extern SYMBOLS symbols;
extern PHB_FNAME _pFileName;
extern BOOL _bQuiet;
extern BOOL _bStartProc;
extern char _szPrefix[ 20 ];         /* holds the prefix added to the generated symbol init function name (in C output currently) */

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
#define FUN_STATEMENTS    1 /* Function have at least one executable statement */
#define FUN_USES_STATICS  2 /* Function uses static variables */
#define FUN_PROCEDURE     4 /* This is a procedure that shouldn't return value */
#define FUN_ILLEGAL_INIT  8 /* Attempt to initialize static variable with a function call */
#define FUN_USES_LOCAL_PARAMS 16 /* parameters are declared using () */

#endif /* HB_COMPILER_H_ */

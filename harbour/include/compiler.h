/*
 * $Id$
 */

/*
   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_COMPILER_H_
#define HB_COMPILER_H_

/* compiler related declarations */

typedef struct _VAR        /* locals, static, public variables support */
{
   char *szName;           /* variable name */
   char *szAlias;          /* variable alias namespace */
   int  iUsed;             /* number of times used */
   char cType;             /* optional strong typing */
   struct _VAR * pNext;    /* pointer to next defined variable */
} VAR, * PVAR;

typedef struct __FUNC      /* functions definition support */
{
   char * szName;          /* name of a defined Clipper function */
   char   cScope;          /* scope of a defined Clipper function */
   BYTE   bFlags;          /* some flags we may need */
   WORD   wParamCount;     /* number of declared parameters */
   PVAR   pLocals;         /* pointer to local variables list */
   PVAR   pStatics;        /* pointer to static variables list */
   PVAR   pFields;         /* pointer to fields variables list */
   PVAR   pMemvars;        /* pointer to memvar variables list */
   BYTE * pCode;           /* pointer to a memory block where pcode is stored */
   LONG   lPCodeSize;      /* total memory size for pcode */
   LONG   lPCodePos;       /* actual pcode offset */
   WORD   wStaticsBase;    /* base for this function statics */
   struct __FUNC * pOwner; /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;  /* pointer to the next defined function */
} _FUNC, * PFUNCTION;  /* structure to hold a Clipper defined function */

typedef struct
{
   PFUNCTION pFirst;       /* pointer to the first defined funtion */
   PFUNCTION pLast;        /* pointer to the last defined function */
   int      iCount;        /* number of defined functions */
} FUNCTIONS;               /* structure to control all Clipper defined functions */

typedef struct _COMSYMBOL  /* compiler symbol support structure */
{
   char * szName;              /* the name of the symbol */
   char   cScope;              /* the scope of the symbol */
   char   cType;
   struct _COMSYMBOL * pNext;  /* pointer to the next defined symbol */
} COMSYMBOL, * PCOMSYMBOL;

typedef struct             /* symbol table support structures */
{
   PCOMSYMBOL pFirst;      /* pointer to the first defined symbol */
   PCOMSYMBOL pLast;       /* pointer to the last defined symbol */
   int    iCount;          /* number of defined symbols */
} SYMBOLS;

typedef struct _STACK_VAL_TYPE        /* locals, static, public variables support */
{
   char cType;                      /* type of stack value */
   struct _STACK_VAL_TYPE * pPrev;    /* pointer to previous stack value's type */
} STACK_VAL_TYPE, * PSTACK_VAL_TYPE;

PFUNCTION GetFunction( char * szFunName ); /* locates a previously defined function */
WORD GetFunctionPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */

void * OurMalloc( LONG lSize ); /* our malloc with error control */
void * OurRealloc( void * p, LONG lSize ); /* our malloc with error control */
void OurFree( void * p ); /* releases allocated memory */

#endif /* HB_COMPILER_H_ */

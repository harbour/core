/*
 * $Id$
 */

#ifndef COMPILER_H_
#define COMPILER_H_

/* compiler related declarations */

typedef struct _VAR        /* locals, static, public variables support */
{
   char *szName;           /* variable name */
   char *szAlias;          /* variable alias namespace */
   int  iUsed;             /* number of times used */
   char cType;             /* future optional strong typing */
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
   char * szName;          /* the name of the symbol */
   char   cScope;          /* the scope of the symbol */
   struct _COMSYMBOL * pNext;  /* pointer to the next defined symbol */
} COMSYMBOL, * PCOMSYMBOL;

typedef struct             /* symbol table support structures */
{
   PCOMSYMBOL pFirst;      /* pointer to the first defined symbol */
   PCOMSYMBOL pLast;       /* pointer to the last defined symbol */
   int    iCount;          /* number of defined symbols */
} SYMBOLS;

PFUNCTION GetFunction( char * szFunName ); /* locates a previously defined function */
WORD GetFunctionPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */

void * OurMalloc( LONG lSize ); /* our malloc with error control */
void * OurRealloc( void * p, LONG lSize ); /* our malloc with error control */
#define OurFree( p )    free( (p) );    /* just for symetry -we can expand it later */

#endif  /* COMPILER_H_ */

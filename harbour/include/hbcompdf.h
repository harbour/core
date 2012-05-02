/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    definitions shared by compiler and macro compiler
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#ifndef HB_COMPDF_H_
#define HB_COMPDF_H_

#include "hbapi.h"
#include "hbpp.h"
#include "hbhash.h"

HB_EXTERN_BEGIN

/* compiler related declarations */

/* Output types */
typedef enum
{
   HB_LANG_C,                      /* C language (by default) <file.c> */
   HB_LANG_OBJ32,                  /* DOS/Windows 32 bits <file.obj> */
   HB_LANG_PORT_OBJ,               /* Portable objects <file.hrb> */
   HB_LANG_PORT_OBJ_BUF            /* Portable objects in memory buffer */
} HB_LANGUAGES;                    /* supported Harbour output languages */

/* Error message format modes */
typedef enum
{
   HB_ERRORFMT_CLIPPER,
   HB_ERRORFMT_IDE
} HB_ERRORFMT;

struct _COMCLASS;    /* forward declaration */

/* Declared Function/Method support structure */
typedef struct _COMDECLARED
{
   const char          * szName;              /* the name of the symbol */
   HB_BYTE               cType;
   HB_USHORT             iParamCount;
   HB_BYTE             * cParamTypes;
   struct _COMCLASS    * pClass;
   struct _COMCLASS    * ( * pParamClasses );
   struct _COMDECLARED * pNext;               /* pointer to the next declared function */
} COMDECLARED, * PCOMDECLARED;

/* Declared Class support structure */
typedef struct _COMCLASS
{
   const char       * szName;
   PCOMDECLARED       pMethod;
   PCOMDECLARED       pLastMethod;
   struct _COMCLASS * pNext;
} COMCLASS, * PCOMCLASS;

/* locals, static, public variables support */
typedef struct _VAR
{
   const char *   szName;           /* variable name */
   const char *   szAlias;          /* variable alias namespace */
   int            iUsed;            /* number of times used */
   int            iDeclLine;        /* declaration line number */
   HB_USHORT      uiFlags;          /* optional falgs, f.e. THREAD STATIC */
   HB_BYTE        cType;            /* optional strong typing */
   PCOMCLASS      pClass;
   struct _VAR * pNext;            /* pointer to next defined variable */
} VAR, * PVAR;

/* local variables declared in a codeblock */
typedef struct HB_CBVAR_
{
   const char * szName;
   HB_BYTE bType;
   HB_BOOL bUsed;
   struct HB_CBVAR_ * pNext;
} HB_CBVAR, * HB_CBVAR_PTR;

typedef struct _HB_VARTYPE
{
   struct _HB_VARTYPE *   pNext;
   char                   cVarType;
   const char *           szFromClass;
} HB_VARTYPE, * PHB_VARTYPE;

/* value types seen at language level
 */
#define  HB_EV_UNKNOWN     0x0000
#define  HB_EV_NIL         0x0001
#define  HB_EV_NUMERIC     0x0002
#define  HB_EV_STRING      0x0004
#define  HB_EV_CODEBLOCK   0x0008
#define  HB_EV_LOGICAL     0x0010
#define  HB_EV_OBJECT      0x0020
#define  HB_EV_ARRAY       0x0040
#define  HB_EV_SYMBOL      0x0080
#define  HB_EV_VARREF      0x0100
#define  HB_EV_FUNREF      0x0200
#define  HB_EV_DATE        0x0400
#define  HB_EV_TIMESTAMP   0x0800
#define  HB_EV_HASH        0x1000

/* messages sent to expressions
 */
typedef enum
{
   HB_EA_REDUCE = 0,    /* reduce the expression into optimized one */
   HB_EA_ARRAY_AT,      /* check if the expession can be used as array */
   HB_EA_ARRAY_INDEX,   /* check if the expession can be used as index */
   HB_EA_LVALUE,        /* check if the expression can be used as lvalue (left side of an assigment) */
   HB_EA_PUSH_PCODE,    /* generate the pcodes to push the value of expression */
   HB_EA_POP_PCODE,     /* generate the pcodes to pop the value of expression */
   HB_EA_PUSH_POP,      /* generate the pcodes to push and pop the expression */
   HB_EA_STATEMENT,     /* generate the pcodes for a statement */
   HB_EA_DELETE         /* delete components of the expression */
} HB_EXPR_MESSAGE;

/* additional definitions used to distinguish numeric expressions
 */
#define  HB_ET_LONG     1
#define  HB_ET_DOUBLE   2

/* additional definitions used to distinguish macro expressions
 */
#define  HB_ET_MACRO_VAR      0   /* &variable */
#define  HB_ET_MACRO_SYMBOL   1   /* &fimcall() */
#define  HB_ET_MACRO_ALIASED  2   /* &alias->&variable */
#define  HB_ET_MACRO_EXPR     4   /* &( expr ) */
#define  HB_ET_MACRO_LIST    16   /* &variable used as in literal arrays or function call argument. */
#define  HB_ET_MACRO_PARE    32   /* &variable used as parentesised expressions. */
#define  HB_ET_MACRO_REFER   64   /* &macro used in @ (pass by reference) */
#define  HB_ET_MACRO_ASSIGN 128   /* o:&msgname := value */

/* types of expressions
 * NOTE: the order of these definition is important - change it carefully
 *    All types <= HB_ET_FUNREF are constant values
 *    All types <= HB_ET_VARIABLE are a simple values
 *    All types > HB_ET_VARIABLE are operators
 */
typedef enum
{
   HB_ET_NONE = 0,
   HB_ET_NIL,
   HB_ET_NUMERIC,
   HB_ET_DATE,
   HB_ET_TIMESTAMP,
   HB_ET_STRING,
   HB_ET_CODEBLOCK,
   HB_ET_LOGICAL,
   HB_ET_SELF,
   HB_ET_ARRAY,
   HB_ET_HASH,
   HB_ET_FUNREF,
   HB_ET_VARREF,
   HB_ET_REFERENCE,
   HB_ET_IIF,
   HB_ET_LIST,
   HB_ET_ARGLIST,
   HB_ET_MACROARGLIST,
   HB_ET_ARRAYAT,
   HB_ET_MACRO,
   HB_ET_FUNCALL,
   HB_ET_ALIASVAR,
   HB_ET_ALIASEXPR,
   HB_ET_SETGET,
   HB_ET_SEND,
   HB_ET_FUNNAME,
   HB_ET_ALIAS,
   HB_ET_RTVAR,      /* PRIVATE or PUBLIC declaration of variable */
   HB_ET_VARIABLE,
   HB_EO_POSTINC,    /* post-operators -> lowest precedence */
   HB_EO_POSTDEC,
   HB_EO_ASSIGN,     /* assigments */
   HB_EO_PLUSEQ,
   HB_EO_MINUSEQ,
   HB_EO_MULTEQ,
   HB_EO_DIVEQ,
   HB_EO_MODEQ,
   HB_EO_EXPEQ,
   HB_EO_OR,         /* logical operators */
   HB_EO_AND,
   HB_EO_NOT,
   HB_EO_EQUAL,      /* relational operators */
   HB_EO_EQ,
   HB_EO_NE,
   HB_EO_IN,
   HB_EO_LT,
   HB_EO_GT,
   HB_EO_LE,
   HB_EO_GE,
   HB_EO_PLUS,       /* addition */
   HB_EO_MINUS,
   HB_EO_MULT,       /* multiple */
   HB_EO_DIV,
   HB_EO_MOD,
   HB_EO_POWER,
   HB_EO_NEGATE,     /* sign operator */
   HB_EO_PREINC,
   HB_EO_PREDEC      /* pre-operators -> the highest precedence */
} HB_EXPR_OPERATOR;

#define HB_EXPR_COUNT   ( HB_EO_PREDEC + 1 )

typedef enum
{
   HB_F_UDF = 0,
   HB_F_AADD,
   HB_F_ABS,
   HB_F_ASC,
   HB_F_AT,
   HB_F_BOF,
   HB_F_BREAK,
   HB_F_CDOW,
   HB_F_CHR,
   HB_F_CMONTH,
   HB_F_COL,
   HB_F_CTOD,
   HB_F_DATE,
   HB_F_DAY,
   HB_F_DELETED,
   HB_F_DEVPOS,
   HB_F_DOW,
   HB_F_DTOC,
   HB_F_DTOS,
   HB_F_EMPTY,
   HB_F_EOF,
   HB_F_EVAL,
   HB_F_EXP,
   HB_F_FCOUNT,
   HB_F_FIELDNAME,
   HB_F_FILE,
   HB_F_FLOCK,
   HB_F_FOUND,
   HB_F_INKEY,
   HB_F_INT,
   HB_F_LASTREC,
   HB_F_LEFT,
   HB_F_LEN,
   HB_F_LOCK,
   HB_F_LOG,
   HB_F_LOWER,
   HB_F_LTRIM,
   HB_F_MAX,
   HB_F_MIN,
   HB_F_MONTH,
   HB_F_PCOL,
   HB_F_PCOUNT,
   HB_F_PROW,
   HB_F_QSELF,
   HB_F_RECCOUNT,
   HB_F_RECNO,
   HB_F_REPLICATE,
   HB_F_RLOCK,
   HB_F_ROUND,
   HB_F_ROW,
   HB_F_RTRIM,
   HB_F_SECONDS,
   HB_F_SELECT,
   HB_F_SETPOS,
   HB_F_SETPOSBS,
   HB_F_SPACE,
   HB_F_SQRT,
   HB_F_STOD,
   HB_F_STOT,
   HB_F_STR,
   HB_F_SUBSTR,
   HB_F_TIME,
   HB_F_TRANSFORM,
   HB_F_TRIM,
   HB_F_TYPE,
   HB_F_UPPER,
   HB_F_VAL,
   HB_F_VALTYPE,
   HB_F_WORD,
   HB_F_YEAR,

   HB_F_BCHAR,
   HB_F_BCODE,
   HB_F_BITAND,
   HB_F_BITOR,
   HB_F_BITXOR,
   HB_F_BITSET,
   HB_F_BITRESET,
   HB_F_BITSHIFT,
   HB_F_BITTEST,
   HB_F_BITNOT,
   HB_F_ARRAYTOPARAMS,
   HB_F_I18N_GETTEXT,
   HB_F_I18N_GETTEXT_STRICT,
   HB_F_I18N_GETTEXT_NOOP,
   HB_F_I18N_NGETTEXT,
   HB_F_I18N_NGETTEXT_STRICT,
   HB_F_I18N_NGETTEXT_NOOP,
   HB_F__GET_
} HB_FUNC_ID;

#define HB_FN_UDF       0
#define HB_FN_RESERVED  1
#define HB_FN_MULTIARG  2

typedef HB_USHORT HB_EXPRTYPE;

typedef struct HB_EXPR_
{
   union
   {
      HB_BOOL asLogical;      /* logical value */
      struct
      {
         const char * name;   /* variable/function name */
         HB_FUNC_ID funcid;   /* function ID */
         int flags;           /* function flags */
      } asSymbol;
      struct
      {
         char * string;       /* literal strings */
         HB_BOOL dealloc;     /* automatic deallocate on expresion deletion */
      } asString;
      struct
      {
         struct HB_EXPR_ * pMacro;  /* macro variable */
         const char * szName;       /* variable name  */
      } asRTVar;                 /* PUBLIC or PRIVATE variable declaration */
      struct
      {
         struct HB_EXPR_ * pVar;    /* variable */
         struct HB_EXPR_ * pExpr;   /* expression */
      } asSetGet;                /* IIF( <var>==NIL, <expr>, <expr>:=<var> ) */
      struct
      {
         union {
            HB_MAXINT l;            /* long value */
            double    d;            /* double value */
         } val;
         unsigned char bWidth;   /* unsigned char used intentionally */
         unsigned char bDec;     /* unsigned char used intentionally */
         unsigned char NumType;  /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         long  lDate;            /* julian date */
         long  lTime;            /* time in milliseconds */
      } asDate;
      struct
      {
         const char * szMacro;         /* identifier after the macro operator */
         struct HB_EXPR_ * pExprList;  /* list elements if &(...) was used */
         HB_USHORT SubType;            /* context in which macro is used */
         unsigned char cMacroOp;       /* macro operator */
      } asMacro;
      struct
      {
         struct HB_EXPR_ * pExprList;  /* list elements */
         struct HB_EXPR_ * pIndex;     /* array index, others */
         HB_BOOL  reference;           /* push array item by reference or pass variable parameters to called function or method */
      } asList;
      struct
      {
         struct HB_EXPR_ * pExprList;  /* list elements */
         HB_CBVAR_PTR pLocals;         /* list of local variables */
         char * string;                /* source code of a codeblock */
         HB_USHORT flags;              /* HB_BLOCK_* */
      } asCodeblock;
      struct
      {
         struct HB_EXPR_ * pAlias;     /* alias expression */
         struct HB_EXPR_ * pVar;       /* aliased variable or macro */
         struct HB_EXPR_ * pExpList;   /* aliased expression list */
      } asAlias;
      struct
      {
         struct HB_EXPR_ * pFunName;   /* function name */
         struct HB_EXPR_ * pParms;     /* function call parameters */
      } asFunCall;
      struct
      {
         struct HB_EXPR_ * pObject;    /* object */
         struct HB_EXPR_ * pParms;     /* method parameters */
         const char * szMessage;       /* message as string */
         struct HB_EXPR_ * pMessage;   /* message as macro */
      } asMessage;
      struct
      {
         struct HB_EXPR_ * pLeft;      /* object */
         struct HB_EXPR_ * pRight;     /* object */
      } asOperator;
      struct HB_EXPR_ * asReference;
   } value;
   HB_SIZE     nLength;
   HB_EXPRTYPE ExprType;      /* internal expression type */
   HB_USHORT   ValType;       /* language level value type */
   struct HB_EXPR_ *pNext;    /* next expression in the list of expressions */
} HB_EXPR, *HB_EXPR_PTR;

typedef struct HB_ENUMERATOR_
{
   const char * szName;
   HB_BOOL bForEach;
   struct HB_ENUMERATOR_ *pNext;
} HB_ENUMERATOR, *HB_ENUMERATOR_PTR; /* support structure for FOR EACH statements */

/* support structure for else if pcode fixups */
typedef struct HB_ELSEIF_
{
   HB_SIZE  nOffset;
   struct   HB_ELSEIF_ * pElseif;   /* next ELSEIF in the current IF statement */
   struct   HB_ELSEIF_ * pPrev;     /* previous IF statement */
} HB_ELSEIF, * HB_ELSEIF_PTR;

/* support structure for EXIT and LOOP statements */
typedef struct HB_LOOPEXIT_
{
   HB_SIZE   nOffset;
   HB_BOOL   fCanLoop;
   HB_USHORT wSeqCounter;
   HB_USHORT wAlwaysCounter;
   HB_USHORT wWithObjectCnt;
   struct HB_LOOPEXIT_ * pLoopList;
   struct HB_LOOPEXIT_ * pExitList;
   struct HB_LOOPEXIT_ * pNext;
} HB_LOOPEXIT, * HB_LOOPEXIT_PTR;

/* support structure for SWITCH statement */
typedef struct HB_SWITCHCASE_
{
   HB_SIZE nOffset;
   HB_EXPR_PTR pExpr;
   struct HB_SWITCHCASE_ * pNext;
} HB_SWITCHCASE, * HB_SWITCHCASE_PTR;

typedef struct HB_SWITCHCMD_
{
   HB_SIZE nOffset;
   HB_SWITCHCASE_PTR pCases;
   HB_SWITCHCASE_PTR pLast;
   HB_EXPR_PTR pExpr;
   HB_SIZE nDefault;
   struct HB_SWITCHCMD_ * pPrev;
} HB_SWITCHCMD, *HB_SWITCHCMD_PTR;

/* support structure for PUBLIC and PRIVATE statements */
typedef struct HB_RTVAR_
{
   HB_EXPR_PTR pVar;
   HB_BOOL bPopValue;
   struct HB_RTVAR_ * pNext;
   struct HB_RTVAR_ * pPrev;
} HB_RTVAR, *HB_RTVAR_PTR;

/* structure to hold a Clipper defined function */
typedef struct __FUNC
{
   const char * szName;                   /* name of a defined Clipper function */
   HB_SYMBOLSCOPE cScope;                 /* scope of a defined Clipper function */
   HB_USHORT    funFlags;                 /* some flags we may need */
   HB_USHORT    wParamCount;              /* number of declared parameters */
   HB_USHORT    wParamNum;                /* current parameter number */
   PVAR         pLocals;                  /* pointer to local variables list */
   PVAR         pStatics;                 /* pointer to static variables list */
   PVAR         pFields;                  /* pointer to fields variables list */
   PVAR         pMemvars;                 /* pointer to memvar variables list */
   PVAR         pDetached;                /* pointer to detached local variables list */
   PVAR         pPrivates;                /* pointer to private variables list */
   HB_BYTE *    pCode;                    /* pointer to a memory block where pcode is stored */
   HB_SIZE      nPCodeSize;               /* total memory size for pcode */
   HB_SIZE      nPCodePos;                /* actual pcode offset */
   int          iStaticsBase;             /* base for this function statics */
   int          iFuncSuffix;              /* function suffix for multiple static functions with the same name */
   HB_SIZE *    pNOOPs;                   /* pointer to the NOOP array */
   HB_SIZE *    pJumps;                   /* pointer to the Jumps array */
   HB_SIZE      nNOOPs;                   /* NOOPs Counter */
   HB_SIZE      nJumps;                   /* Jumps Counter */
   HB_BOOL      bLateEval;                /* HB_TRUE if accessing of declared (compile time) variables is allowed */
   HB_BOOL      fVParams;                 /* HB_TRUE if variable number of parameters is used */
   HB_BOOL      bError;                   /* error during function compilation */
   HB_BOOL      bBlock;                   /* HB_TRUE if simple codeblock body is compiled */
   struct __FUNC * pOwner;                /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;                 /* pointer to the next defined function */
   HB_ENUMERATOR_PTR pEnum;               /* pointer to FOR EACH variables */
   HB_LOOPEXIT_PTR   pLoops;
   HB_SWITCHCMD_PTR  pSwitch;
   HB_ELSEIF_PTR     elseif;
   HB_RTVAR_PTR      rtvars;
   HB_USHORT         wSeqCounter;
   HB_USHORT         wAlwaysCounter;
   HB_USHORT         wForCounter;
   HB_USHORT         wIfCounter;
   HB_USHORT         wWhileCounter;
   HB_USHORT         wCaseCounter;
   HB_USHORT         wSwitchCounter;
   HB_USHORT         wWithObjectCnt;
} _FUNC, * PFUNCTION;

/* structure to hold an INLINE block of source */
typedef struct __INLINE
{
   const char * szName;                   /* name of a inline function */
   HB_BYTE *    pCode;                    /* pointer to a memory block where pcode is stored */
   HB_SIZE      nPCodeSize;               /* total memory size for pcode */
   const char * szFileName;               /* Source file name */
   int          iLine;                    /* Source line number */
   struct __INLINE * pNext;               /* pointer to the next defined inline */
} _INLINE, * PINLINE;

/* structure to hold a called functions */
typedef struct __FUNCALL
{
   const char * szName;                   /* name of a called function */
   HB_SYMBOLSCOPE cScope;                 /* the scope of the function */
   struct __FUNCALL * pNext;              /* pointer to the next called function */
} _FUNCALL, * PFUNCALL;

/* structure to control all Clipper defined functions */
typedef struct
{
   PFUNCTION pFirst;            /* pointer to the first defined funtion */
   PFUNCTION pLast;             /* pointer to the last defined function */
   int       iCount;            /* number of defined functions */
} FUNCTIONS;

/* structure to control all Clipper defined functions */
typedef struct
{
   PINLINE pFirst;            /* pointer to the first defined inline */
   PINLINE pLast;             /* pointer to the last defined inline */
   int     iCount;            /* number of defined inlines */
} INLINES;

/* compiler symbol support structure */
typedef struct _COMSYMBOL
{
   const char *   szName;     /* the name of the symbol */
   HB_SYMBOLSCOPE cScope;     /* the scope of the symbol */
   int            iFunc;      /* is it a function name (TRUE) or memvar (FALSE) */
   PFUNCTION      pFunc;
   struct _COMSYMBOL * pNext; /* pointer to the next defined symbol */
} COMSYMBOL, * PCOMSYMBOL;

/* symbol table support structures */
typedef struct
{
   PCOMSYMBOL pFirst;           /* pointer to the first defined symbol */
   PCOMSYMBOL pLast;            /* pointer to the last defined symbol */
   int        iCount;           /* number of defined symbols */
} SYMBOLS;

typedef struct __EXTERN
{
   const char * szName;         /* name of the extern function */
   HB_SYMBOLSCOPE cScope;       /* the scope of the function */
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

typedef struct _HB_MODULE
{
   const char *         szName;
   HB_BOOL              force;  /* force module compilation */
   struct _HB_MODULE *  pNext;
} HB_MODULE, * PHB_MODULE;

typedef struct _HB_DEBUGINFO
{
   char *    pszModuleName;
   HB_ULONG  ulFirstLine;
   HB_ULONG  ulLastLine;
   HB_ULONG  ulAllocated;
   HB_BYTE * pLineMap;
   struct _HB_DEBUGINFO * pNext;
} HB_DEBUGINFO, * PHB_DEBUGINFO;

typedef struct _HB_LABEL_INFO
{
   FILE *    yyc;
   HB_BOOL   fVerbose;
   HB_BOOL   fSetSeqBegin;
   HB_BOOL   fCondJump;
   HB_BOOL   fEndRequest;
   int       iNestedBlock;
   HB_SIZE * pnLabels;
} HB_LABEL_INFO, * PHB_LABEL_INFO;

#define HB_MODE_COMPILER      1
#define HB_MODE_MACRO         2

struct _HB_COMP_FUNCS;

#if defined( HB_COMMON_SUPPORT )

typedef struct _HB_COMMON
{
   /* common to macro compiler members */
   int    mode;               /* HB_MODE_* */
   int    supported;          /* various flags for supported capabilities */
   const struct _HB_COMP_FUNCS * funcs;
}
HB_COMMON, * HB_COMMON_PTR;

#define HB_COMP_PARAM         pCommon
#define HB_COMP_DECL          HB_COMMON_PTR HB_COMP_PARAM

#elif defined( HB_MACRO_SUPPORT )

#define HB_COMP_PARAM         pMacro
#define HB_COMP_DECL          HB_MACRO_PTR HB_COMP_PARAM

typedef struct HB_PCODE_INFO_ /* compiled pcode container for macro compiler */
{
   HB_BYTE * pCode;        /* pointer to a memory block where pcode is stored */
   HB_SIZE nPCodeSize;     /* total memory size for pcode */
   HB_SIZE nPCodePos;      /* actual pcode offset */
   HB_BOOL fVParams;       /* function/codeblock with variable parameters */
   HB_CBVAR_PTR pLocals;
   struct HB_PCODE_INFO_ * pPrev;
} HB_PCODE_INFO, * HB_PCODE_INFO_PTR;

typedef struct HB_MACRO_      /* a macro compiled pcode container */
{
   /* common to compiler members */
   int      mode;             /* HB_MODE_* */
   int      supported;        /* various flags for supported capabilities */
   const struct _HB_COMP_FUNCS * funcs;

   /* macro compiler only members */
   const char * string;       /* compiled string */
   HB_SIZE  length;           /* length of the string */
   int      Flags;            /* some flags we may need */
   int      status;           /* status of compilation */
   HB_ITEM_PTR pError;        /* error object returned from the parser */
   HB_PCODE_INFO_PTR pCodeInfo;  /* pointer to pcode buffer and info */
   void *   pLex;             /* lexer buffer pointer */
   void *   pExprLst;         /* list with allocated expressions */
   void *   pIdentLst;        /* list with allocated identifiers */
   int      exprType;         /* type of successfully compiled expression */
   HB_USHORT uiListElements;  /* number of elements in macro list expression */
   HB_USHORT uiNameLen;       /* the maximum symbol name length */
   HB_PCODE_INFO pCodeInfoBuffer;
} HB_MACRO;

#else

#define HB_COMP_PARAM         pComp
#define HB_COMP_DECL          HB_COMP_PTR HB_COMP_PARAM

#define HB_I18N_PLURAL_MAX    8

typedef struct _HB_I18NPOS
{
   const char *   szFile;
   HB_UINT        uiLine;
} HB_I18NPOS, *PHB_I18NPOS;

typedef struct _HB_I18NSTRING
{
   const char *   szText;
   const char *   szContext;
   const char *   szPlurals[ HB_I18N_PLURAL_MAX ];
   HB_UINT        uiPlurals;
   HB_I18NPOS     pPos;
   HB_I18NPOS *   pPosLst;
   HB_UINT        uiPosCount;
} HB_I18NSTRING, * PHB_I18NSTRING;

typedef struct _HB_I18NTABLE
{
   PHB_I18NSTRING    pString;
   HB_ULONG          uiCount;
   HB_ULONG          uiAllocated;
} HB_I18NTABLE, * PHB_I18NTABLE;

typedef struct _HB_COMP_LEX
{
   PHB_PP_STATE   pPP;
   int            iState;
   HB_BOOL        fEol;
   const char *   lasttok;
}
HB_COMP_LEX, * PHB_COMP_LEX;

typedef struct _HB_EXPRLST
{
   HB_EXPR Expression;
   struct _HB_EXPRLST *pPrev;
   struct _HB_EXPRLST *pNext;
}
HB_EXPRLST, * PHB_EXPRLST;

typedef struct _HB_INCLST
{
   struct _HB_INCLST *pNext;
   char szFileName[ 1 ];
}
HB_INCLST, * PHB_INCLST;

typedef struct _HB_COMP
{
   /* common to macro compiler members */
   int    mode;            /* HB_MODE_* */
   int    supported;       /* various flags for supported capabilities */
   const struct _HB_COMP_FUNCS * funcs;

   /* compiler only members */
   PHB_COMP_LEX      pLex;
   PHB_EXPRLST       pExprLst;

   HB_HASH_TABLE_PTR pIdentifiers;
   FUNCTIONS         functions;
   SYMBOLS           symbols;
   INLINES           inlines;
   PEXTERN           externs;
   PHB_MODULE        modules;
   PHB_VARTYPE       pVarType;
   PHB_INCLST        incfiles;

   PCOMDECLARED      pFirstDeclared;
   PCOMDECLARED      pLastDeclared;
   PCOMDECLARED      pLastMethod;
   PCOMCLASS         pFirstClass;
   PCOMCLASS         pLastClass;

   PFUNCTION         pInitFunc;
   PFUNCTION         pLineFunc;
   PFUNCTION         pDeclFunc;
   PHB_FNAME         pFileName;
   PHB_FNAME         pOutPath;
   PHB_FNAME         pPpoPath;
   PHB_FNAME         pI18nFileName;
   PHB_I18NTABLE     pI18n;
   HB_BOOL           fI18n;

   void              ( * outStdFunc ) ( void *, const char* );
   void              ( * outErrFunc ) ( void *, const char* );
   PHB_PP_MSG_FUNC   outMsgFunc;
   void *            cargo;

   HB_SIZE           nOutBufSize;         /* memory output buffer size */
   HB_BYTE *         pOutBuf;             /* memory output buffer address */

   int               lastLine;            /* last generated in PCODE line number */
   int               currLine;            /* currently compiled line number */
   const char *      lastModule;          /* last generated in PCODE module name */
   const char *      currModule;          /* currently compiled module name */

   const char *      szAnnounce;
   const char *      szDeclaredFun;
   const char *      szFile;              /* Source file name of compiled module */
   char              szPrefix[ 20 ];      /* holds the prefix added to the generated symbol init function name (in C output currently) */
   char *            szDepExt;            /* destination file extension used in decencies list */
   char *            szStdCh;             /* standard definitions file name (-u) */
   char **           szStdChExt;          /* extended definitions file names (-u+<file>) */
   int               iStdChExt;           /* number of extended definition files (-u+<file>) */

   char              cDataListType;       /* current declared variable list type */
   char              cCastType;           /* current casting type */

   int               iErrorCount;
   int               iModulesCount;       /* number of compiled .prg modules */
   int               iStartProc;          /* holds if we need to create the starting procedure */
   int               iMaxTransCycles;     /* maximum translate cycles in PP (-r=<n>) */
   int               iHidden;             /* hide strings */
   int               iWarnings;           /* enable parse warnings */
   int               iExitLevel;          /* holds if there was any warning during the compilation process */
   int               iStaticCnt;          /* number of defined statics variables on the PRG */
   int               iVarScope;           /* holds the scope for next variables to be defined */
   int               iLanguage;           /* default Harbour generated output language */
   int               iGenCOutput;         /* C code generation should be verbose (use comments) or not */
   int               ilastLineErr;        /* line numer with last syntax error */
   int               iTraceInclude;       /* trace included files and generate dependencies list */
   int               iSyntaxCheckOnly;    /* syntax check only */
   int               iErrorFmt;           /* error message formatting mode (default: Clipper) */

   HB_BOOL           fQuiet;              /* be quiet during compilation (-q) */
   HB_BOOL           fFullQuiet;          /* be quiet during compilation disable all messages */
   HB_BOOL           fExit;               /* force breaking compilation process */
   HB_BOOL           fPPO;                /* flag indicating, is ppo output needed */
   HB_BOOL           fPPT;                /* flag indicating, is ppt output needed */
   HB_BOOL           fLineNumbers;        /* holds if we need pcodes with line numbers */
   HB_BOOL           fAnyWarning;         /* holds if there was any warning during the compilation process */
   HB_BOOL           fAutoMemvarAssume;   /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
   HB_BOOL           fForceMemvars;       /* holds if memvars are assumed when accesing undeclared variable (-v)*/
   HB_BOOL           fDebugInfo;          /* holds if generate debugger required info */
   HB_BOOL           fNoStartUp;          /* C code generation embed HB_FS_FIRST or not */
   HB_BOOL           fCredits;            /* print credits */
   HB_BOOL           fBuildInfo;          /* print build info */
   HB_BOOL           fLogo;               /* print logo */
   HB_BOOL           fSwitchCase;         /* generate PCODE for CASE value of SWITCH statement */
   HB_BOOL           fSingleModule;       /* do not automatically compile DO...[WITH...] external modules (-m) */
   HB_BOOL           fError;              /* error appeared during compilation */
   HB_BOOL           fNoArchDefs;         /* do not define architecture dependent macros: __PLATFORM__*, __ARCH??BIT__, __*_ENDIAN__ */
   HB_BOOL           fMeaningful;         /* do not generate warnings about meaningless expression usage */
   HB_BOOL           fINCLUDE;            /* use INCLUDE envvar as header path (default) */
}
HB_COMP, * HB_COMP_PTR;

typedef struct
{
   HB_BOOL  fDebugInfo;
   HB_BOOL  fAutoMemvarAssume;
   HB_BOOL  fI18n;
   HB_BOOL  fLineNumbers;
   HB_BOOL  fPPO;
   HB_BOOL  fPPT;
   HB_BOOL  fQuiet;
   HB_BOOL  fForceMemvars;
   int      iStartProc;
   int      iWarnings;
   int      iExitLevel;
   int      iHidden;
   int      supported;
}
HB_COMP_SWITCHES, * PHB_COMP_SWITCHES;

extern HB_COMP_PTR hb_comp_new( void );
extern void hb_comp_free( HB_COMP_PTR );

#endif /* !HB_MACRO_SUPPORT  */

typedef struct _HB_COMP_FUNCS
{
   HB_EXPR_PTR ( * ExprNew )        ( HB_COMP_DECL, HB_EXPRTYPE iType );
   void        ( * ExprClear )      ( HB_COMP_DECL, HB_EXPR_PTR pExpr );
   void        ( * ExprFree )       ( HB_COMP_DECL, HB_EXPR_PTR pExpr );

   HB_EXPR_PTR ( * ErrorType )      ( HB_COMP_DECL, HB_EXPR_PTR );
   HB_EXPR_PTR ( * ErrorSyntax )    ( HB_COMP_DECL, HB_EXPR_PTR );
   void        ( * ErrorDuplVar )   ( HB_COMP_DECL, const char* );
} HB_COMP_FUNCS, * PHB_COMP_FUNCS;


#define HB_MACRO_DATA         HB_COMP_PARAM
#define HB_PCODE_DATA         ( HB_MACRO_DATA->pCodeInfo )


/* Support for traversing of linked list */
#define HB_COMP_CARGO_FUNC( proc )   void proc( HB_COMP_DECL, void *cargo )
typedef HB_COMP_CARGO_FUNC( ( * HB_COMP_CARGO_FUNC_PTR ) );

#define HB_COMP_CARGO2_FUNC( proc )  void proc( HB_COMP_DECL, void *cargo, void *dummy )
typedef HB_COMP_CARGO2_FUNC( ( * HB_COMP_CARGO2_FUNC_PTR ) );

/* pcode chunks bytes size */
#define HB_PCODE_CHUNK   100


HB_EXTERN_END

#endif /* HB_COMPDF_H_ */

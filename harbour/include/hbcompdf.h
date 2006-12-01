/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    definitions shared by compiler and macro compiler
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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
   LANG_C,                      /* C language (by default) <file.c> */
   LANG_CLI,                    /* .NET IL language */
   LANG_OBJ32,                  /* DOS/Windows 32 bits <file.obj> */
   LANG_JAVA,                   /* Java <file.java> */
   LANG_PORT_OBJ,               /* Portable objects <file.hrb> */
   LANG_OBJ_MODULE              /* Platform dependant object module <file.obj> */
} LANGUAGES;                    /* supported Harbour output languages */

struct _COMCLASS;    /* forward declaration */

/* Declared Function/Method support structure */
typedef struct _COMDECLARED
{
   char                * szName;              /* the name of the symbol */
   BYTE                  cType;
   USHORT                iParamCount;
   BYTE                * cParamTypes;
   struct _COMCLASS    * pClass;
   struct _COMCLASS    * ( * pParamClasses );
   struct _COMDECLARED * pNext;               /* pointer to the next declared function */
} COMDECLARED, * PCOMDECLARED;

/* Declared Class support structure */
typedef struct _COMCLASS
{
   char             * szName;
   PCOMDECLARED       pMethod;
   PCOMDECLARED       pLastMethod;
   struct _COMCLASS * pNext;
} COMCLASS, * PCOMCLASS;

/* locals, static, public variables support */
typedef struct _VAR
{
   char *    szName;               /* variable name */
   char *    szAlias;              /* variable alias namespace */
   int       iUsed;                /* number of times used */
   int       iDeclLine;            /* declaration line number */
   BYTE      cType;                /* optional strong typing */
   PCOMCLASS pClass;
   struct _VAR * pNext;            /* pointer to next defined variable */
} VAR, * PVAR;

/* local variables declared in a codeblock */
typedef struct HB_CBVAR_
{
   char * szName;
   BYTE bType;
   BOOL bUsed;
   struct HB_CBVAR_ * pNext;
} HB_CBVAR, * HB_CBVAR_PTR;

typedef struct HB_ENUMERATOR_
{
   char *szName;
   BOOL bForEach;
   struct HB_ENUMERATOR_ *pNext;
} HB_ENUMERATOR, *HB_ENUMERATOR_PTR; /* support structure for FOR EACH statements */

/* structure to hold a Clipper defined function */
typedef struct __FUNC
{
   char *       szName;                   /* name of a defined Clipper function */
   HB_SYMBOLSCOPE cScope;                 /* scope of a defined Clipper function */
   BYTE         bFlags;                   /* some flags we may need */
   USHORT       wParamCount;              /* number of declared parameters */
   USHORT       wParamNum;                /* current parameter number */
   PVAR         pLocals;                  /* pointer to local variables list */
   PVAR         pStatics;                 /* pointer to static variables list */
   PVAR         pFields;                  /* pointer to fields variables list */
   PVAR         pMemvars;                 /* pointer to memvar variables list */
   PVAR         pPrivates;                /* pointer to private variables list */
   BYTE *       pCode;                    /* pointer to a memory block where pcode is stored */
   ULONG        lPCodeSize;               /* total memory size for pcode */
   ULONG        lPCodePos;                /* actual pcode offset */
   int          iStaticsBase;             /* base for this function statics */
   ULONG *      pNOOPs;                   /* pointer to the NOOP array */
   ULONG *      pJumps;                   /* pointer to the Jumps array */
   ULONG        iNOOPs;                   /* NOOPs Counter */
   ULONG        iJumps;                   /* Jumps Counter */
   BOOL         bLateEval;                /* TRUE if accessing of declared (compile time) variables is allowed */
   BOOL         bError;                   /* error during function compilation */
   struct __FUNC * pOwner;                /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;                 /* pointer to the next defined function */
   HB_ENUMERATOR_PTR pEnum;               /* pointer to FOR EACH variables */
} _FUNC, * PFUNCTION;

/* structure to hold an INLINE block of source */
typedef struct __INLINE
{
   char *       szName;                   /* name of a inline function */
   BYTE *       pCode;                    /* pointer to a memory block where pcode is stored */
   ULONG        lPCodeSize;               /* total memory size for pcode */
   char *       szFileName;               /* Source file name */
   int          iLine;                    /* Source line number */
   struct __INLINE * pNext;               /* pointer to the next defined inline */
} _INLINE, * PINLINE;

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
   char *         szName;               /* the name of the symbol */
   HB_SYMBOLSCOPE cScope;               /* the scope of the symbol */
   BYTE           cType;
   BOOL           bFunc;      /* is it a function name (TRUE) or memvar (FALSE) */
   PCOMCLASS      pClass;
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
   char * szName;
   struct __EXTERN * pNext;
} _EXTERN, * PEXTERN;      /* support structure for extern symbols */
/* as they have to be placed on the symbol table later than the first public symbol */

typedef struct _AUTOOPEN
{
   char * szName;
   struct _AUTOOPEN * pNext;
} AUTOOPEN, * PAUTOOPEN;      /* support structure for extern symbols */

/* value types seen at language level
 */
#define  HB_EV_UNKNOWN     0
#define  HB_EV_NIL         1
#define  HB_EV_NUMERIC     2
#define  HB_EV_STRING      4
#define  HB_EV_CODEBLOCK   8
#define  HB_EV_LOGICAL     16
#define  HB_EV_OBJECT      32
#define  HB_EV_ARRAY       64
#define  HB_EV_SYMBOL      128
#define  HB_EV_VARREF      256
#define  HB_EV_FUNREF      512
#define  HB_EV_DATE        1024

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
#define  HB_ET_MACRO_INDEX   32   /* &variable used as arrays index. */
#define  HB_ET_MACRO_PARE    64   /* &variable used as parentesised expressions. */
#define  HB_ET_MACRO_REFER  128   /* &macro used in @ (pass by reference) */

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
   HB_ET_STRING,
   HB_ET_CODEBLOCK,
   HB_ET_LOGICAL,
   HB_ET_SELF,
   HB_ET_ARRAY,
   HB_ET_VARREF,
   HB_ET_REFERENCE,
   HB_ET_FUNREF,
   HB_ET_IIF,
   HB_ET_LIST,
   HB_ET_ARGLIST,
   HB_ET_MACROARGLIST,
   HB_ET_ARRAYAT,
   HB_ET_MACRO,
   HB_ET_FUNCALL,
   HB_ET_ALIASVAR,
   HB_ET_ALIASEXPR,
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
   HB_EO_LT,
   HB_EO_GT,
   HB_EO_LE,
   HB_EO_GE,
   HB_EO_NE,
   HB_EO_IN,
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

typedef USHORT HB_EXPRTYPE;

typedef struct HB_EXPR_
{
   union
   {
      char *asSymbol;      /* variable name */
      BOOL asLogical;      /* logical value */
      struct
      {
         char *string;        /* literal strings */
         BOOL dealloc;        /* automatic deallocate on expresion deletion */
      } asString;
      struct
      {
         struct HB_EXPR_ *pMacro;   /* macro variable */
         char *szName;              /* variable name  */
      } asRTVar;                 /* PUBLIC or PRIVATE variable declaration */
      struct
      {
         union {
            HB_LONG  l;             /* long value */
            double   d;             /* double value */
         } val;
         unsigned char bWidth;   /* unsigned char used intentionally */
         unsigned char bDec;     /* unsigned char used intentionally */
         unsigned char NumType;  /* used to distinguish LONG and DOUBLE */
      } asNum;
      struct
      {
         char * szMacro;               /* identifier after the macro operator */
         struct HB_EXPR_ *pExprList;   /* list elements if &(...) was used */
         struct HB_EXPR_ *pFunCall;    /* pointer to a function if used as function's call argument */
         unsigned char cMacroOp;       /* macro operator */
         unsigned char SubType;        /* context in which macro is used */
      } asMacro;
      struct
      {
         struct HB_EXPR_ *pExprList;   /* list elements */
         struct HB_EXPR_ *pIndex;      /* array index, others */
      } asList;
      struct
      {
         char     *string;             /* source code of a codeblock */
         USHORT   length;
         USHORT   flags;               /* HB_BLOCK_MACRO, HB_BLOCK_LATEEVAL */
         struct HB_EXPR_ *pExprList;   /* list elements */
         HB_CBVAR_PTR pLocals;         /* list of local variables */
      } asCodeblock;
      struct
      {
         struct HB_EXPR_ *pAlias;      /* alias expression */
         struct HB_EXPR_ *pVar;        /* aliased variable or macro */
         struct HB_EXPR_ *pExpList;    /* aliased expression list */
      } asAlias;
      struct
      {
         struct HB_EXPR_ *pFunName;     /* function name */
         struct HB_EXPR_ *pParms;       /* function call parameters */
      } asFunCall;
      struct
      {
         struct HB_EXPR_ *pObject;     /* object */
         struct HB_EXPR_ *pParms;      /* method parameters */
         char * szMessage;             /* message as string */
         struct HB_EXPR_ *pMessage;    /* message as macro */
      } asMessage;
      struct
      {
         struct HB_EXPR_ *pLeft;       /* object */
         struct HB_EXPR_ *pRight;      /* object */
      } asOperator;
      struct HB_EXPR_ *asReference;
   } value;
   ULONG ulLength;
   ULONG Counter;
   HB_EXPRTYPE ExprType;      /* internal expression type */
   USHORT ValType;            /* language level value type */
   struct HB_EXPR_ *pNext;    /* next expression in the list of expressions */
} HB_EXPR, *HB_EXPR_PTR;

/* support structure for else if pcode fixups */
typedef struct HB_ELSEIF_
{
   ULONG ulOffset;
   struct HB_ELSEIF_ * pElseif; /* next ELSEIF in the current IF statement */
   struct HB_ELSEIF_ * pPrev;   /* previous IF statement */
} HB_ELSEIF, * HB_ELSEIF_PTR;      

/* support structure for EXIT and LOOP statements */
typedef struct HB_LOOPEXIT_
{
   ULONG ulOffset;
   USHORT wSeqCounter;
   struct HB_LOOPEXIT_ * pLoopList;
   struct HB_LOOPEXIT_ * pExitList;
   struct HB_LOOPEXIT_ * pNext;
} HB_LOOPEXIT, * HB_LOOPEXIT_PTR;  

/* support structure for SWITCH statement */
typedef struct HB_SWITCHCASE_
{
   ULONG ulOffset;
   HB_EXPR_PTR pExpr;
   struct HB_SWITCHCASE_ *pNext;
} HB_SWITCHCASE, * HB_SWITCHCASE_PTR;

typedef struct HB_SWITCHCMD_
{
   ULONG ulOffset;
   int iCount;
   HB_SWITCHCASE_PTR pCases;
   HB_SWITCHCASE_PTR pLast;
   ULONG ulDefault;
   struct HB_SWITCHCMD_ *pPrev;
} HB_SWITCHCMD, *HB_SWITCHCMD_PTR;

/* support structure for PUBLIC and PRIVATE statements */
typedef struct HB_RTVAR_
{
   HB_EXPR_PTR pVar;
   BOOL bPopValue;
   struct HB_RTVAR_ *pNext;
   struct HB_RTVAR_ *pPrev;
} HB_RTVAR, *HB_RTVAR_PTR;

typedef struct _HB_LABEL_INFO
{
   FILE *   yyc;
   BOOL     fVerbose;
   BOOL     fSetSeqBegin;
   BOOL     fCondJump;
   ULONG *  pulLabels;
} HB_LABEL_INFO, * PHB_LABEL_INFO;

#define HB_MODE_COMPILER      1
#define HB_MODE_MACRO         2

#if defined( HB_COMMON_SUPPORT )

typedef struct _HB_COMMON
{
   /* common to macro compiler members */
   int    mode;               /* HB_MODE_* */
   ULONG  supported;          /* various flags for supported capabilities */
}
HB_COMMON, * HB_COMMON_PTR;

#define HB_COMP_PARAM         pCommon
#define HB_COMP_DECL          HB_COMMON_PTR HB_COMP_PARAM

#elif defined( HB_MACRO_SUPPORT )

#define HB_COMP_PARAM         pMacro
#define HB_COMP_DECL          HB_MACRO_PTR HB_COMP_PARAM

typedef struct HB_PCODE_INFO_   /* compiled pcode container */
{
   BYTE * pCode;           /* pointer to a memory block where pcode is stored */
   ULONG  lPCodeSize;      /* total memory size for pcode */
   ULONG  lPCodePos;       /* actual pcode offset */
   struct HB_PCODE_INFO_ * pPrev;
   HB_CBVAR_PTR pLocals;
} HB_PCODE_INFO, * HB_PCODE_INFO_PTR;

typedef struct HB_MACRO_    /* a macro compiled pcode container */
{
   /* common to compiler members */
   int    mode;
   ULONG  supported;       /* various flags for supported capabilities */

   /* macro compiler only members */
   char * string;          /* compiled string */
   ULONG  length;          /* length of the string */
   int    Flags;           /* some flags we may need */
   int    status;          /* status of compilation */
   HB_ITEM_PTR pError;     /* error object returned from the parser */
   HB_PCODE_INFO_PTR pCodeInfo;  /* pointer to pcode buffer and info */
   void * pLex;            /* lexer buffer pointer */
   void * pExprLst;        /* list with allocated expressions */
   void * pIdentLst;       /* list with allocated identifiers */
   int    exprType;        /* type of successfully compiled expression */
   USHORT uiListElements;  /* number of elements in macro list expression */
   USHORT uiNameLen;       /* the maximum symbol name length */
} HB_MACRO;

#else

#define HB_COMP_PARAM         pComp
#define HB_COMP_DECL          HB_COMP_PTR HB_COMP_PARAM

typedef struct _HB_COMP_LEX
{
   PHB_PP_STATE   pPP;
   int            iState;
   char *         lasttok;
}
HB_COMP_LEX, * PHB_COMP_LEX;

typedef struct _HB_EXPRLST
{
   HB_EXPR Expression;
   struct _HB_EXPRLST *pPrev;
   struct _HB_EXPRLST *pNext;
}
HB_EXPRLST, * PHB_EXPRLST;

typedef struct _HB_COMP
{
   /* common to macro compiler members */
   int    mode;
   ULONG  supported;       /* various flags for supported capabilities */

   /* compiler only members */
   PHB_COMP_LEX      pLex;
   PHB_EXPRLST       pExprLst;

   HB_HASH_TABLE_PTR pIdentifiers;
   FUNCTIONS         functions;
   FUNCTIONS         funcalls;
   HB_LOOPEXIT_PTR   pLoops;
   HB_SWITCHCMD_PTR  pSwitch;
   HB_ELSEIF_PTR     elseif;
   HB_RTVAR_PTR      rtvars;
   SYMBOLS           symbols;
   INLINES           inlines;
   PEXTERN           externs;
   PAUTOOPEN         autoopen;

   PCOMDECLARED      pFirstDeclared;
   PCOMDECLARED      pLastDeclared;
   PCOMDECLARED      pReleaseDeclared;
   PCOMDECLARED      pLastMethod;
   PCOMCLASS         pFirstClass;
   PCOMCLASS         pLastClass;
   PCOMCLASS         pReleaseClass;
   PFUNCTION         pInitFunc;
   PHB_FNAME         pMainFileName;
   PHB_FNAME         pFileName;
   PHB_FNAME         pFilePpo;
   PHB_FNAME         pOutPath;
   PHB_FNAME         pPpoPath;

   ULONG             lastLinePos;         /* position of last opcode with line number */

   char *            szAnnounce;
   char *            szStdCh;             /* standard definitions file name (-u) */
   char *            szFromClass;
   char *            szDeclaredFun;
   char *            szFile;              /* Source file name of compiled module */
   char              szPrefix[ 20 ];      /* holds the prefix added to the generated symbol init function name (in C output currently) */

   char              cVarType;            /* current declared variable type */
   char              cDataListType;       /* current declared variable list type */
   char              cCastType;           /* current casting type */

   int               iPassByRef;
   int               iErrorCount;
   int               iFunctionCnt;
   int               iMaxTransCycles;     /* maximum translate cycles in PP (-r=<n>) */
   int               iHidden;             /* hide strings */
   int               iWarnings;           /* enable parse warnings */
   int               iExitLevel;          /* holds if there was any warning during the compilation process */
   int               iStaticCnt;          /* number of defined statics variables on the PRG */
   int               iVarScope;           /* holds the scope for next variables to be defined */
   int               iLanguage;           /* default Harbour generated output language */
   int               iGenCOutput;         /* C code generation should be verbose (use comments) or not */

   BOOL              fExit;               /* force breaking compilation process */
   BOOL              fQuiet;              /* be quiet during compilation (-q) */
   BOOL              fPPO;                /* flag indicating, is ppo output needed */
   BOOL              fStartProc;          /* holds if we need to create the starting procedure */
   BOOL              fLineNumbers;        /* holds if we need pcodes with line numbers */
   BOOL              fAnyWarning;         /* holds if there was any warning during the compilation process */
   BOOL              fAutoMemvarAssume;   /* holds if undeclared variables are automatically assumed MEMVAR (-a)*/
   BOOL              fForceMemvars;       /* holds if memvars are assumed when accesing undeclared variable (-v)*/
   BOOL              fDebugInfo;          /* holds if generate debugger required info */
   BOOL              fNoStartUp;          /* C code generation embed HB_FS_FIRST or not */
   BOOL              fDontGenLineNum;     /* suppress line number generation */
   BOOL              fCredits;            /* print credits */
   BOOL              fBuildInfo;          /* print build info */
   BOOL              fLogo;               /* print logo */
   BOOL              fSyntaxCheckOnly;    /* syntax check only */
   BOOL              fTextSubst;          /* check if string variables are macros (&) which needs substitution */
   BOOL              fLongOptimize;       /* optimize PCODEs generated for integers */
   BOOL              fAutoOpen;           /* automatically compile DO...[WITH...] external modules (-m) */
   BOOL              fError;              /* error appeared during compilation */
   BOOL              fExternal;           /* suppress error report generation for EXTERNAL statement */

   USHORT            wSeqCounter;
   USHORT            wForCounter;
   USHORT            wIfCounter;
   USHORT            wWhileCounter;
   USHORT            wCaseCounter;
   USHORT            wSwitchCounter;
   USHORT            wWithObjectCnt;

}
HB_COMP, * HB_COMP_PTR;

extern HB_COMP_PTR hb_comp_new( void );
extern void hb_comp_free( HB_COMP_PTR );

#endif /* !HB_MACRO_SUPPORT  */

#define HB_MACRO_DATA         HB_COMP_PARAM
#define HB_PCODE_DATA         ( HB_MACRO_DATA->pCodeInfo )


/*Support for traversing of linked list */
#define  HB_CARGO_FUNC( proc )   void proc( HB_COMP_DECL, void *cargo )
typedef  HB_CARGO_FUNC( HB_CARGO_FUNC_ );
typedef  HB_CARGO_FUNC_ *HB_CARGO_FUNC_PTR;

#define  HB_CARGO2_FUNC( proc )  void proc( HB_COMP_DECL, void *cargo, void *dummy )
typedef  HB_CARGO2_FUNC( HB_CARGO2_FUNC_ );
typedef  HB_CARGO2_FUNC_ *HB_CARGO2_FUNC_PTR;

/* pcode chunks bytes size */
#define HB_PCODE_CHUNK   100


HB_EXTERN_END

#endif /* HB_COMPDF_H_ */

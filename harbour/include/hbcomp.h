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

#ifndef HB_COMP_H_
#define HB_COMP_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "hbapi.h"
#include "hberrors.h"
#include "hbpp.h"
#include "hbver.h"
#include "hbexprop.h"
#include "hbpcode.h"
#include "hbhash.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* compiler related declarations */

/* Output types */
typedef enum
{
   LANG_C,                      /* C language (by default) <file.c> */
   LANG_OBJ32,                  /* DOS/Windows 32 bits <file.obj> */
   LANG_JAVA,                   /* Java <file.java> */
   LANG_PORT_OBJ,               /* Portable objects <file.hrb> */
   LANG_OBJ_MODULE              /* Platform dependant object module <file.obj> */
} LANGUAGES;                    /* supported Harbour output languages */

/* #include support */
typedef struct
{
   FILE * handle;               /* handle of the opened file */
   void * pBuffer;              /* file buffer */
   int    iBuffer;              /* current position in file buffer */
   int    lenBuffer;            /* current length of data in file buffer */
   char * szFileName;           /* name of the file */
   void * pPrev;                /* pointer to the previous opened file */
   void * pNext;                /* pointer to the next opened file */
   int    iLine;                /* currently processed line number */
} _FILE, * PFILE;               /* structure to hold an opened PRG or CH */

/* structure to control several opened PRGs and CHs */
typedef struct
{
   PFILE pLast;                 /* pointer to the last opened file */
   int   iFiles;                /* number of files currently opened */
} FILES;

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

/* pcode chunks bytes size */
#define HB_PCODE_CHUNK   100

/* structure to hold a Clipper defined function */
typedef struct __FUNC
{
   char *       szName;                   /* name of a defined Clipper function */
   char         cScope;                   /* scope of a defined Clipper function */
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
   BYTE *       pStack;                   /* Compile Time Stack */
   USHORT       iStackSize;               /* Compile Time Stack size */
   int          iStackIndex;              /* Compile Time Stack index */
   PCOMDECLARED pStackFunctions[ 8 ];     /* Declared Functions on the Compile Time Stack */
   int          iStackFunctions;          /* Index into DEclared Functions on Compile Time Stack */
   PCOMCLASS    pStackClasses[ 8 ];       /* Declared Classes on the Compile Time Stack */
   int          iStackClasses;            /* Index into Declared Classes on Compile Time Stack */
   struct __FUNC * pOwner;                /* pointer to the function/procedure that owns the codeblock */
   struct __FUNC * pNext;                 /* pointer to the next defined function */
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
   char *    szName;               /* the name of the symbol */
   char      cScope;               /* the scope of the symbol */
   BYTE      cType;
   PCOMCLASS pClass;
   struct _COMSYMBOL * pNext;   /* pointer to the next defined symbol */
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

/* definitions for hb_compPCodeEval() support */
typedef void * HB_VOID_PTR;
#define HB_PCODE_FUNC( func, type ) USHORT func( PFUNCTION pFunc, ULONG lPCodePos, type cargo )
typedef  HB_PCODE_FUNC( HB_PCODE_FUNC_, HB_VOID_PTR );
typedef  HB_PCODE_FUNC_ * HB_PCODE_FUNC_PTR;

void hb_compPCodeEval( PFUNCTION, HB_PCODE_FUNC_PTR *, void * );

#define VS_NONE       0
#define VS_LOCAL      1
#define VS_STATIC     2
#define VS_FIELD      4
#define VS_PARAMETER  8
#define VS_PRIVATE    64
#define VS_PUBLIC     128
#define VS_MEMVAR     ( VS_PUBLIC | VS_PRIVATE )

#define VU_NOT_USED    0
#define VU_INITIALIZED 1
#define VU_USED        2

#define VT_OFFSET_BYREF             60
#define VT_OFFSET_VARIANT           90
#define VT_OFFSET_OPTIONAL          90

/*
 * flags for bFlags member
*/
#define FUN_STATEMENTS        1   /* Function have at least one executable statement */
#define FUN_USES_STATICS      2   /* Function uses static variables */
#define FUN_PROCEDURE         4   /* This is a procedure that shouldn't return value */
#define FUN_BREAK_CODE        8   /* last statement breaks execution flow */
#define FUN_USES_LOCAL_PARAMS 16  /* parameters are declared using () */
#define FUN_WITH_RETURN       32  /* there was RETURN statement in previous line */

extern void      hb_compFunctionAdd( char * szFunName, HB_SYMBOLSCOPE cScope, int iType ); /* starts a new Clipper language function definition */
extern PFUNCTION hb_compFunctionFind( char * szFunName ); /* locates a previously defined function */
extern PINLINE   hb_compInlineFind( char * szFunName );
extern USHORT    hb_compFunctionGetPos( char * szSymbolName ); /* returns the index + 1 of a function on the functions defined list */
extern PFUNCTION hb_compFunctionKill( PFUNCTION );    /* releases all memory allocated by function and returns the next one */
extern void      hb_compAnnounce( char * );
extern PINLINE   hb_compInlineAdd( char * szFunName );

extern PFUNCTION hb_compFunCallAdd( char * szFuntionName );
extern PFUNCTION hb_compFunCallFind( char * szFunName ); /* locates a previously defined called function */
extern void      hb_compFunCallCheck( char *, int );

extern void hb_compVariableAdd( char * szVarName, BYTE cType ); /* add a new param, local, static variable to a function definition or a public or private */
extern PVAR hb_compVariableFind( PVAR pVars, USHORT wOrder ); /* returns a variable if defined or zero */
extern PVAR hb_compLocalVariableFind( PFUNCTION pFunc, USHORT wVar );
extern USHORT hb_compVariableGetPos( PVAR pVars, char * szVarName ); /* returns the order + 1 of a variable if defined or zero */

extern PCOMSYMBOL hb_compSymbolAdd( char *, USHORT * );
extern PCOMSYMBOL hb_compSymbolKill( PCOMSYMBOL );    /* releases all memory allocated by symbol and returns the next one */
extern PCOMSYMBOL hb_compSymbolFind( char *, USHORT * ); /* returns a symbol pointer from the symbol table */
extern PCOMSYMBOL hb_compSymbolGetPos( USHORT );   /* returns a symbol based on its index on the symbol table */

extern PCOMDECLARED hb_compDeclaredAdd( char * );
extern PCOMDECLARED hb_compDeclaredFind( char * );

extern PCOMCLASS hb_compClassAdd( char * );
extern PCOMCLASS hb_compClassFind( char * );
extern PCOMDECLARED hb_compMethodAdd( PCOMCLASS pClass, char * );
extern PCOMDECLARED hb_compMethodFind( PCOMCLASS pClass, char * );
extern void hb_compDeclaredParameterAdd( char * szVarName, BYTE cValueType );

extern void hb_compGenBreak( void );  /* generate code for BREAK statement */

extern void hb_compExternGen( void ); /* generates the symbols for the EXTERN names */
extern void hb_compExternAdd( char * szExternName ); /* defines a new extern name */

extern void hb_compAutoOpenAdd( char * szName );

#ifdef HB_MACRO_SUPPORT

#define hb_compErrorType( p )    hb_macroError( EG_ARG, HB_MACRO_PARAM )
#define hb_compErrorIndex( p )   hb_macroError( EG_BOUND, HB_MACRO_PARAM )
#define hb_compErrorSyntax( p )  hb_macroError( EG_SYNTAX, HB_MACRO_PARAM )
#define hb_compErrorLValue( p )  hb_macroError( EG_SYNTAX, HB_MACRO_PARAM )
#define hb_compErrorBound( p )   hb_macroError( EG_BOUND, HB_MACRO_PARAM )
#define hb_compErrorAlias( p )   hb_macroError( EG_NOALIAS, HB_MACRO_PARAM )
#define hb_compErrorDuplVar( c ) hb_macroError( EG_SYNTAX, HB_MACRO_PARAM )
#define hb_compWarnMeaningless( p )

#else /* HB_MACRO_SUPPORT */

extern BOOL hb_compVariableMacroCheck( char * ); /* checks if passed variable can be used in macro */

extern ULONG hb_compGenJump( LONG );                /* generates the pcode to jump to a specific offset */
extern ULONG hb_compGenJumpFalse( LONG );           /* generates the pcode to jump if false */
extern ULONG hb_compGenJumpTrue( LONG );            /* generates the pcode to jump if true */
extern void hb_compGenJumpHere( ULONG  );             /* returns the pcode pos where to set a jump offset */
extern void hb_compGenJumpThere( ULONG, ULONG ); /* sets a jump offset */


extern void hb_compLinePush( void ); /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfDebugger( void ); /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfInside( void );   /* generates the pcode with the currently compiled source code line */

extern void hb_compGenMessage( char * szMsgName );       /* sends a message to an object */
extern void hb_compGenMessageData( char * szMsg );     /* generates an underscore-symbol name for a data assignment */
extern void hb_compGenPopVar( char * szVarName );         /* generates the pcode to pop a value from the virtual machine stack onto a variable */
extern void hb_compGenPushDouble( double dNumber, BYTE bWidth, BYTE bDec ); /* Pushes a number on the virtual machine stack */
extern void hb_compGenPushFunCall( char * );             /* generates the pcode to push function's call */
extern void hb_compGenPushVar( char * szVarName );        /* generates the pcode to push a variable value to the virtual machine stack */
extern void hb_compGenPushVarRef( char * szVarName );    /* generates the pcode to push a variable by reference to the virtual machine stack */
extern void hb_compGenPushInteger( int iNumber );        /* Pushes a integer number on the virtual machine stack */
extern void hb_compGenPushLogical( int iTrueFalse );     /* pushes a logical value on the virtual machine stack */
extern void hb_compGenPushLong( long lNumber );          /* Pushes a long number on the virtual machine stack */
extern void hb_compGenPushNil( void );                   /* Pushes nil on the virtual machine stack */
extern void hb_compGenPushString( char * szText, ULONG ulLen );       /* Pushes a string on the virtual machine stack */
extern void hb_compGenPushSymbol( char * szSymbolName, int iIsFunction ); /* Pushes a symbol on to the Virtual machine stack */
extern void hb_compGenPushAliasedVar( char *, BOOL, char *, long );
extern void hb_compGenPopAliasedVar( char *, BOOL, char *, long );
extern void hb_compGenPushFunRef( char * );
extern void hb_compGenPCode1( BYTE );             /* generates 1 byte of pcode */
extern void hb_compGenPData1( BYTE );             /* generates 1 byte of pcode argument */
extern void hb_compGenPCode2( BYTE, BYTE, BOOL );       /* generates 2 bytes of pcode + flag for optional StrongType(). */
extern void hb_compGenPCode3( BYTE, BYTE, BYTE, BOOL ); /* generates 3 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCode4( BYTE, BYTE, BYTE, BYTE, BOOL ); /* generates 4 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize, BOOL );  /* copy bytes to a pcode buffer + flag for optional StrongType() */

extern ULONG hb_compSequenceBegin( void );
extern ULONG hb_compSequenceEnd( void );
extern void hb_compSequenceFinish( ULONG, int );

/* Codeblocks */
extern void hb_compCodeBlockStart( void );        /* starts a codeblock creation */
extern void hb_compCodeBlockEnd( void );          /* end of codeblock creation */

/* support for FIELD declaration */
extern void hb_compFieldSetAlias( char *, int );
extern int hb_compFieldsCount( void );

/* Static variables */
extern void hb_compStaticDefStart( void );
extern void hb_compStaticDefEnd( void );

extern HB_EXPR_PTR hb_compErrorStatic( char *, HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorType( HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorIndex( HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorSyntax( HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorLValue( HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorBound( HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorAlias( HB_EXPR_PTR );
extern void hb_compErrorDuplVar( char * );
extern HB_EXPR_PTR hb_compWarnMeaningless( HB_EXPR_PTR );

extern void hb_compChkCompilerSwitch( int, char * Args[] );
extern void hb_compChkEnvironVar( char * );
extern void hb_compChkPaths( void );
extern void hb_compChkDefines( int iArg, char * Args[] );

extern void hb_compPrintUsage( char * );
extern void hb_compPrintCredits( void );
extern void hb_compPrintLogo( void );
extern void hb_compPrintModes( void );

extern int hb_compCompile( char * szPrg, int argc, char * argv[] );

#endif    /* HB_MACRO_SUPPORT */

/* Misc functions defined in harbour.c */
extern void hb_compFinalizeFunction( void ); /* fixes all last defined function returns jumps offsets */
extern void hb_compNOOPadd( PFUNCTION pFunc, ULONG ulPos );

/* Misc functions defined in hbfix.c */
extern void hb_compFixFuncPCode( PFUNCTION );

/* Misc functions defined in harbour.y */
#if 0
extern int hb_compYACCMain( char * szName );
#endif
extern BOOL hb_compInclude( char * szFileName, PATHNAMES * pSearchPath );  /* end #include support */

extern char * hb_comp_buffer; /* yacc input buffer */

/* output related functions defined in gen*.c */
extern void hb_compGenCCode( PHB_FNAME );      /* generates the C language output */
extern void hb_compGenJava( PHB_FNAME );       /* generates the Java language output */
extern void hb_compGenPortObj( PHB_FNAME );    /* generates the portable objects */
extern void hb_compGenObj32( PHB_FNAME );      /* generates OBJ 32 bits */
extern void hb_compGenCObj( PHB_FNAME );       /* generates platform dependant object module */

/* hbident.c   */
extern char * hb_compIdentifierNew( char * szName, BOOL bCopy ); /* create the reusable identifier */
extern void hb_compIdentifierOpen( void ); /* prepare the table of identifiers */
extern void hb_compIdentifierClose( void ); /* release the table of identifiers */

/* variable used by compiler
 */
extern int           hb_comp_iLine;
extern FUNCTIONS     hb_comp_functions;
extern FUNCTIONS     hb_comp_funcalls;
extern SYMBOLS       hb_comp_symbols;
extern PCOMDECLARED  hb_comp_pFirstDeclared;
extern PCOMDECLARED  hb_comp_pLastDeclared;
extern PCOMDECLARED  hb_comp_pReleaseDeclared;
extern PCOMCLASS     hb_comp_pFirstClass;
extern PCOMCLASS     hb_comp_pLastClass;
extern PCOMCLASS     hb_comp_pReleaseClass;
extern char *        hb_comp_szFromClass;
extern PCOMDECLARED  hb_comp_pLastMethod;
extern PATHNAMES *   hb_comp_pIncludePath;
extern PFUNCTION     hb_comp_pInitFunc;
extern PHB_FNAME     hb_comp_pFileName;
extern BOOL          hb_comp_bPPO;
extern FILE *        hb_comp_yyppo;
extern BOOL          hb_comp_bStartProc;
extern BOOL          hb_comp_bLineNumbers;
extern BOOL          hb_comp_bQuiet;
extern BOOL          hb_comp_bShortCuts;
extern int           hb_comp_iWarnings;
extern BOOL          hb_comp_bAnyWarning;
extern BOOL          hb_comp_bAutoMemvarAssume;
extern BOOL          hb_comp_bForceMemvars;
extern BOOL          hb_comp_bDebugInfo;
extern char          hb_comp_szPrefix[ 20 ];
extern int           hb_comp_iGenCOutput;
extern int           hb_comp_iExitLevel;
extern int           hb_comp_iFunctionCnt;
extern char          hb_comp_cVarType;
extern char          hb_comp_cDataListType;
extern char          hb_comp_cCastType;
extern int           hb_comp_iVarScope;
extern BOOL          hb_comp_bDontGenLineNum;
extern FILES         hb_comp_files;
extern int           hb_comp_iStaticCnt;
extern int           hb_comp_iErrorCount;

extern char *        hb_comp_szAnnounce;

extern PHB_FNAME     hb_comp_pOutPath;
extern BOOL          hb_comp_bCredits;
extern BOOL          hb_comp_bBuildInfo;
extern BOOL          hb_comp_bLogo;
extern BOOL          hb_comp_bSyntaxCheckOnly;
extern int           hb_comp_iLanguage;

extern USHORT        hb_comp_wSeqCounter;
extern USHORT        hb_comp_wForCounter;
extern USHORT        hb_comp_wIfCounter;
extern USHORT        hb_comp_wWhileCounter;
extern USHORT        hb_comp_wCaseCounter;

extern char *        hb_comp_szDeclaredFun;

extern char *        hb_comp_szLastMethod;

extern char *        hb_comp_szErrors[];
extern char *        hb_comp_szWarnings[];

extern char *        hb_pp_STD_CH;
extern BOOL          hb_comp_bAutoOpen;
extern BOOL          hb_comp_bError;
extern char          hb_comp_cInlineID;

extern INLINES       hb_comp_inlines;
extern int           hb_comp_iLineINLINE;
extern int           hb_comp_iLinePRG;

extern ULONG         hb_comp_Supported;

/* /GC command line setting types */
#define HB_COMPGENC_COMPACT     0
#define HB_COMPGENC_NORMAL      1
#define HB_COMPGENC_VERBOSE     2

/* /ES command line setting types */
#define HB_EXITLEVEL_DEFAULT    0
#define HB_EXITLEVEL_SETEXIT    1
#define HB_EXITLEVEL_DELTARGET  2

/* /kx command line setting types - compatibility modes
 * (turn on a bit in ULONG word)
*/
#define HB_COMPFLAG_HARBOUR        1    /* -kh */
#define HB_COMPFLAG_XBASE          2    /* -kx */
#define HB_COMPFLAG_HB_INLINE      4    /* -ki */
#define HB_COMPFLAG_RT_MACRO      64    /* -kr */

#ifdef HB_MACRO_SUPPORT
  #define HB_COMP_ISSUPPORTED(flag)    ( HB_MACRO_DATA->supported & (flag) )
#else
  #define HB_COMP_ISSUPPORTED(flag)    ( hb_comp_Supported & (flag) )
#endif

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_COMP_H_ */


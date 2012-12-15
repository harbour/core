/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
 *
 * Copyright 1999 Antonio Linares <alinares@fivetechsoft.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbmacro.ch"
#include "hbapi.h"
#include "hberrors.h"
#include "hbpp.h"
#include "hbmacro.h"
#include "hbexprop.h"
#include "hbpcode.h"
#include "hbhash.h"

HB_EXTERN_BEGIN

/* definitions for hb_compPCodeEval() support */
typedef void * HB_VOID_PTR;
#define HB_PCODE_FUNC( func, type ) HB_SIZE func( PFUNCTION pFunc, HB_SIZE nPCodePos, type cargo )
typedef HB_PCODE_FUNC( ( * HB_PCODE_FUNC_PTR ), HB_VOID_PTR );

extern HB_ISIZ hb_compPCodeSize( PFUNCTION, HB_SIZE );
extern void hb_compPCodeEval( PFUNCTION, const HB_PCODE_FUNC_PTR *, void * );
extern void hb_compPCodeTrace( PFUNCTION, const HB_PCODE_FUNC_PTR *, void * );

extern void hb_compGenLabelTable( PFUNCTION pFunc, PHB_LABEL_INFO label_info );
extern PHB_DEBUGINFO hb_compGetDebugInfo( HB_COMP_DECL );

extern void hb_compChkFileSwitches( int argc, char * argv[] );

extern void hb_compInitPP( HB_COMP_DECL, int argc, const char * const argv[], PHB_PP_OPEN_FUNC pOpenFunc );
extern void hb_compCompileEnd( HB_COMP_DECL );

extern int  hb_comp_yyparse( HB_COMP_DECL );
extern void hb_compParserStop( HB_COMP_DECL );
extern void hb_compParserRun( HB_COMP_DECL );

#define VS_NONE       0
#define VS_LOCAL      1
#define VS_STATIC     2
#define VS_FIELD      4
#define VS_PARAMETER  8
#define VS_THREAD     16
#define VS_PRIVATE    64
#define VS_PUBLIC     128
#define VS_MEMVAR     ( VS_PUBLIC | VS_PRIVATE )
#define VS_TH_STATIC  ( VS_STATIC | VS_THREAD )

/* return detailed information about a class of variable  */
extern int hb_compVariableScope( HB_COMP_DECL, const char * );
#define HB_VS_UNDECLARED      0
/* variables declared in a current codeblock/function/procedure */
#define HB_VS_CBLOCAL_VAR     1     /* func/proc local variables and parameters used in codeblock (detached) */
#define HB_VS_LOCAL_VAR       2     /* local variables and parameters */
#define HB_VS_LOCAL_MEMVAR    4
#define HB_VS_LOCAL_FIELD     8
#define HB_VS_STATIC_VAR     16
/* variables declared outside of a current function/procedure */
#define HB_VS_FILEWIDE       32
#define HB_VS_GLOBAL_MEMVAR   ( HB_VS_FILEWIDE | HB_VS_LOCAL_MEMVAR )
#define HB_VS_GLOBAL_FIELD    ( HB_VS_FILEWIDE | HB_VS_LOCAL_FIELD )
#define HB_VS_GLOBAL_STATIC   ( HB_VS_FILEWIDE | HB_VS_STATIC_VAR )

#define VU_NOT_USED           0
#define VU_INITIALIZED        1
#define VU_USED               2

#define VT_OFFSET_BYREF       60
#define VT_OFFSET_VARIANT     90
#define VT_OFFSET_OPTIONAL    90

/*
 * flags for funFlags member
 */
#define FUN_STATEMENTS        0x0001   /* Function have at least one executable statement */
#define FUN_USES_STATICS      0x0002   /* Function uses static variables */
#define FUN_PROCEDURE         0x0004   /* This is a procedure that shouldn't return value */
#define FUN_BREAK_CODE        0x0008   /* last statement breaks execution flow */
#define FUN_USES_LOCAL_PARAMS 0x0010   /* parameters are declared using () */
#define FUN_WITH_RETURN       0x0020   /* there was RETURN statement in previous line */
#define FUN_EXTBLOCK          0x0040   /* it's extended codeblock */
#define FUN_FILE_DECL         0x0080   /* pseudo function with file wide declarations */
#define FUN_FILE_FIRST        0x0100   /* 1-st real or pseudo function in compiled .prg module */
#define FUN_ATTACHED          0x0200   /* function attached to function list */

extern               void         hb_compFunctionAdd( HB_COMP_DECL, const char * szFunName, HB_SYMBOLSCOPE cScope, int iType ); /* starts a new Clipper language function definition */
extern               PHB_HINLINE  hb_compInlineAdd( HB_COMP_DECL, const char * szFunName, int iLine );
extern               void         hb_compFunctionMarkStatic( HB_COMP_DECL, const char * szFunName );
extern HB_EXPORT_INT const char * hb_compGetFuncID( const char * szFuncName, HB_FUNC_ID * pFunID, int * piFlags );
extern               HB_BOOL      hb_compFunCallCheck( HB_COMP_DECL, const char *, int );

extern PHB_VARTYPE hb_compVarTypeNew( HB_COMP_DECL, char cVarType, const char * szFromClass );
extern void hb_compVariableAdd( HB_COMP_DECL, const char * szVarName, PHB_VARTYPE pVarType ); /* add a new param, local, static variable to a function definition or a public or private */
extern PVAR hb_compVariableFind( HB_COMP_DECL, const char * szVarName, int * piPos, int * piScope );
extern const char * hb_compLocalVariableName( PFUNCTION pFunc, HB_USHORT wVar );   /* returns the name of local variable */
extern const char * hb_compStaticVariableName( HB_COMP_DECL, HB_USHORT wVar );   /* returns the name of static variable */

#define HB_SYM_MEMVAR   HB_FALSE
#define HB_SYM_ALIAS    HB_FALSE
#define HB_SYM_MSGNAME  HB_FALSE
#define HB_SYM_FUNCNAME HB_TRUE
extern const char * hb_compSymbolName( HB_COMP_DECL, HB_USHORT );   /* returns a symbol name based on its index on the symbol table */

extern PHB_HDECLARED hb_compDeclaredAdd( HB_COMP_DECL, const char * );

extern PHB_HCLASS hb_compClassAdd( HB_COMP_DECL, const char *, const char * );
extern PHB_HCLASS hb_compClassFind( HB_COMP_DECL, const char * );
extern PHB_HDECLARED hb_compMethodAdd( HB_COMP_DECL, PHB_HCLASS pClass, const char * );
extern PHB_HDECLARED hb_compMethodFind( PHB_HCLASS pClass, const char * );
extern void hb_compDeclaredParameterAdd( HB_COMP_DECL, const char * szVarName, PHB_VARTYPE pVarType );

extern void hb_compGenBreak( HB_COMP_DECL );  /* generate code for BREAK statement */

extern void hb_compExternAdd( HB_COMP_DECL, const char * szExternName, HB_SYMBOLSCOPE cScope ); /* defines a new extern name */

extern void hb_compModuleAdd( HB_COMP_DECL, const char * szModuleName, HB_BOOL fForce );

extern void hb_compRTVariableKill( HB_COMP_DECL, PFUNCTION );
extern void hb_compSwitchKill( HB_COMP_DECL, PFUNCTION );
extern void hb_compElseIfKill( PFUNCTION );
extern void hb_compLoopKill( PFUNCTION );

extern void hb_compGenError( HB_COMP_DECL, const char * const szErrors[], char cPrefix, int iError, const char * szError1, const char * szError2 ); /* generic parsing error management function */
extern void hb_compGenWarning( HB_COMP_DECL, const char * const szWarnings[], char cPrefix, int iWarning, const char * szWarning1, const char * szWarning2); /* generic parsing warning management function */

extern HB_SIZE hb_compGenJump( HB_ISIZ nOffset, HB_COMP_DECL );             /* generates the pcode to jump to a specific offset */
extern HB_SIZE hb_compGenJumpFalse( HB_ISIZ nOffset, HB_COMP_DECL );        /* generates the pcode to jump if false */
extern HB_SIZE hb_compGenJumpTrue( HB_ISIZ nOffset, HB_COMP_DECL );         /* generates the pcode to jump if true */
extern void    hb_compGenJumpHere( HB_SIZE nOffset, HB_COMP_DECL );        /* returns the pcode pos where to set a jump offset */
extern void    hb_compGenJumpThere( HB_SIZE nFrom, HB_SIZE nTo, HB_COMP_DECL );   /* sets a jump offset */

extern void hb_compGenModuleName( HB_COMP_DECL, const char * szFunName );  /* generates the pcode with the currently compiled module and function name */
extern void hb_compLinePush( HB_COMP_DECL );             /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfDebugger( HB_COMP_DECL );   /* generates the pcode with the currently compiled source code line */
extern void hb_compLinePushIfInside( HB_COMP_DECL );     /* generates the pcode with the currently compiled source code line */
extern void hb_compStatmentStart( HB_COMP_DECL );        /* Check if we can start statement (without line pushing) */

extern void hb_compGenMessage( const char * szMsgName, HB_BOOL bIsObject, HB_COMP_DECL );    /* sends a message to an object */
extern void hb_compGenMessageData( const char * szMsg, HB_BOOL bIsObject, HB_COMP_DECL );    /* generates an underscore-symbol name for a data assignment */
extern void hb_compGenPopVar( const char * szVarName, HB_COMP_DECL );                        /* generates the pcode to pop a value from the virtual machine stack onto a variable */
extern void hb_compGenPopMemvar( const char * szVarName, HB_COMP_DECL );                     /* generates the pcode to pop a value from the virtual machine stack onto a memvar variable */
extern void hb_compGenPushDouble( double dNumber, HB_BYTE bWidth, HB_BYTE bDec, HB_COMP_DECL );    /* Pushes a number on the virtual machine stack */
extern void hb_compGenPushFunCall( const char *, int, HB_COMP_DECL );                             /* generates the pcode to push function's call */
extern void hb_compGenPushFunSym( const char *, int, HB_COMP_DECL );                              /* generates the pcode to push function's symbol */
extern void hb_compGenPushFunRef( const char *, HB_COMP_DECL );                              /* generates the pcode to push function's reference symbol */
extern void hb_compGenPushVar( const char * szVarName, HB_COMP_DECL );                       /* generates the pcode to push a variable value to the virtual machine stack */
extern void hb_compGenPushVarRef( const char * szVarName, HB_COMP_DECL );                    /* generates the pcode to push a variable by reference to the virtual machine stack */
extern void hb_compGenPushMemvarRef( const char * szVarName, HB_COMP_DECL );                 /* generates the pcode to push memvar variable by reference to the virtual machine stack */
extern void hb_compGenPushInteger( int iNumber, HB_COMP_DECL );                              /* Pushes a integer number on the virtual machine stack */
extern void hb_compGenPushLogical( int iTrueFalse, HB_COMP_DECL );                           /* pushes a logical value on the virtual machine stack */
extern void hb_compGenPushLong( HB_MAXINT nNumber, HB_COMP_DECL );                           /* Pushes a long number on the virtual machine stack */
extern void hb_compGenPushDate( long lDate, HB_COMP_DECL );                                  /* Pushes a date constant on the virtual machine stack */
extern void hb_compGenPushTimeStamp( long lDate, long lTime, HB_COMP_DECL );                 /* Pushes a timestamp constant on the virtual machine stack */
extern void hb_compGenPushNil( HB_COMP_DECL );                                               /* Pushes nil on the virtual machine stack */
extern void hb_compGenPushString( const char * szText, HB_SIZE nLen, HB_COMP_DECL );         /* Pushes a string on the virtual machine stack */
extern void hb_compGenPushSymbol( const char * szSymbolName, HB_BOOL bFunction, HB_COMP_DECL ); /* Pushes a symbol on to the Virtual machine stack */
extern void hb_compGenPushAliasedVar( const char * szVarName, HB_BOOL bPushAliasValue, const char * szAlias, HB_MAXINT nWorkarea, HB_COMP_DECL );
extern void hb_compGenPopAliasedVar( const char * szVarName, HB_BOOL bPushAliasValue, const char * szAlias, HB_MAXINT nWorkarea, HB_COMP_DECL );
extern void hb_compGenPCode1( HB_BYTE, HB_COMP_DECL ); /* generates 1 byte of pcode */
extern void hb_compGenPCode2( HB_BYTE, HB_BYTE, HB_COMP_DECL ); /* generates 2 bytes of pcode + flag for optional StrongType(). */
extern void hb_compGenPCode3( HB_BYTE, HB_BYTE, HB_BYTE, HB_COMP_DECL ); /* generates 3 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCode4( HB_BYTE, HB_BYTE, HB_BYTE, HB_BYTE, HB_COMP_DECL ); /* generates 4 bytes of pcode + flag for optional StrongType() */
extern void hb_compGenPCodeN( const HB_BYTE * pBuffer, HB_SIZE nSize, HB_COMP_DECL ); /* copy bytes to a pcode buffer + flag for optional StrongType() */

extern HB_SIZE hb_compSequenceBegin( HB_COMP_DECL );
extern HB_SIZE hb_compSequenceEnd( HB_COMP_DECL );
extern HB_SIZE hb_compSequenceAlways( HB_COMP_DECL );
extern void hb_compSequenceFinish( HB_COMP_DECL, HB_SIZE nStartPos, HB_SIZE nEndPos,
                                   HB_SIZE nAlways, HB_BOOL fUsualStmts, HB_BOOL fRecover,
                                   HB_BOOL fCanMove );

/* support for FIELD declaration */
extern void hb_compFieldSetAlias( HB_COMP_DECL, const char * szAlias, int iField );
extern int  hb_compFieldsCount( HB_COMP_DECL );

/* Static variables */
extern void hb_compStaticDefStart( HB_COMP_DECL );
extern void hb_compStaticDefEnd( HB_COMP_DECL, const char * szVarName );

extern HB_BOOL hb_compCheckUnclosedStru( HB_COMP_DECL, PFUNCTION );

#define HB_COMP_ERROR_TYPE( x )     HB_COMP_PARAM->funcs->ErrorType( HB_COMP_PARAM, x )
#define HB_COMP_ERROR_SYNTAX( x )   HB_COMP_PARAM->funcs->ErrorSyntax( HB_COMP_PARAM, x )
#define HB_COMP_ERROR_DUPLVAR( s )  HB_COMP_PARAM->funcs->ErrorDuplVar( HB_COMP_PARAM, s )

#define HB_COMP_EXPR_NEW( i )       HB_COMP_PARAM->funcs->ExprNew( HB_COMP_PARAM, i )
#define HB_COMP_EXPR_FREE( x )      HB_COMP_PARAM->funcs->ExprFree( HB_COMP_PARAM, x )
#define HB_COMP_EXPR_CLEAR( x )     HB_COMP_PARAM->funcs->ExprClear( HB_COMP_PARAM, x )

#if defined( HB_MACRO_SUPPORT )

#define HB_GEN_FUNC1( func, p1 )          hb_macroGen##func( p1, HB_COMP_PARAM )
#define HB_GEN_FUNC2( func, p1,p2 )       hb_macroGen##func( p1, p2, HB_COMP_PARAM )
#define HB_GEN_FUNC3( func, p1,p2,p3 )    hb_macroGen##func( p1, p2, p3, HB_COMP_PARAM )
#define HB_GEN_FUNC4( func, p1,p2,p3,p4 ) hb_macroGen##func( p1, p2, p3, p4, HB_COMP_PARAM )

#define hb_compErrorIndex( p, x )         hb_macroError( EG_BOUND, ( p ) )
#define hb_compErrorLValue( p, x )        hb_macroError( EG_SYNTAX, ( p ) )
#define hb_compErrorBound( p, x )         hb_macroError( EG_BOUND, ( p ) )
#define hb_compErrorAlias( p, x )         hb_macroError( EG_NOALIAS, ( p ) )
#define hb_compErrorRefer( p, x, c )      hb_macroError( EG_SYNTAX, ( p ) )
#define hb_compErrorVParams( p, x )       hb_macroError( EG_SYNTAX, ( p ) )
#define hb_compWarnMeaningless( p, x )
#define hb_compErrorMacro( p, x )

#elif !defined( HB_COMMON_SUPPORT )

#define HB_GEN_FUNC1( func, p1 )          hb_compGen##func( p1, HB_COMP_PARAM )
#define HB_GEN_FUNC2( func, p1,p2 )       hb_compGen##func( p1, p2, HB_COMP_PARAM )
#define HB_GEN_FUNC3( func, p1,p2,p3 )    hb_compGen##func( p1, p2, p3, HB_COMP_PARAM )
#define HB_GEN_FUNC4( func, p1,p2,p3,p4 ) hb_compGen##func( p1, p2, p3, p4, HB_COMP_PARAM )

extern int  hb_compMain( int argc, const char * const argv[] );
extern int  hb_compMainExt( int argc, const char * const argv[], HB_BYTE ** pBufPtr, HB_SIZE * pnSize, const char * szSource, int iStartLine, void * cargo, PHB_PP_OPEN_FUNC pOpenFunc, PHB_PP_MSG_FUNC pMsgFunc );
extern void hb_compOutStd( HB_COMP_DECL, const char * szMessage );
extern void hb_compOutErr( HB_COMP_DECL, const char * szMessage );

extern void hb_compExprLstDealloc( HB_COMP_DECL );

extern HB_EXPR_PTR hb_compExprGenStatement( HB_EXPR_PTR, HB_COMP_DECL );
extern HB_EXPR_PTR hb_compExprGenPush( HB_EXPR_PTR, HB_COMP_DECL );
extern HB_EXPR_PTR hb_compExprGenPop( HB_EXPR_PTR, HB_COMP_DECL );
extern HB_EXPR_PTR hb_compExprReduce( HB_EXPR_PTR, HB_COMP_DECL );

extern HB_EXPR_PTR hb_compErrorIndex( HB_COMP_DECL, HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorLValue( HB_COMP_DECL, HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorBound( HB_COMP_DECL, HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorAlias( HB_COMP_DECL, HB_EXPR_PTR );
extern HB_EXPR_PTR hb_compErrorRefer( HB_COMP_DECL, HB_EXPR_PTR, const char * );
extern HB_EXPR_PTR hb_compWarnMeaningless( HB_COMP_DECL, HB_EXPR_PTR );
extern void        hb_compErrorMacro( HB_COMP_DECL, const char * szText );
extern void        hb_compErrorVParams( HB_COMP_DECL, const char * szFuncOrBlock );

extern HB_EXPR_PTR hb_compErrorStatic( HB_COMP_DECL, const char *, HB_EXPR_PTR );
extern void        hb_compErrorCodeblock( HB_COMP_DECL, const char * szBlock );

extern void        hb_compPushMacroText( HB_COMP_DECL, const char * szText, HB_SIZE nLen, HB_BOOL fMacro );

/* Codeblocks */
extern void hb_compCodeBlockStart( HB_COMP_DECL, int iEarlyEvalPass );  /* starts a codeblock creation */
extern void hb_compCodeBlockEnd( HB_COMP_DECL );                        /* end of codeblock creation */
extern void hb_compCodeBlockStop( HB_COMP_DECL );                       /* end of fake codeblock */
extern void hb_compCodeBlockRewind( HB_COMP_DECL );                     /* restart of fake codeblock */

#endif    /* HB_MACRO_SUPPORT */


extern HB_SIZE hb_compExprListEval( HB_COMP_DECL, HB_EXPR_PTR pExpr, HB_COMP_CARGO_FUNC_PTR pEval );
extern HB_SIZE hb_compExprListEval2( HB_COMP_DECL, HB_EXPR_PTR pExpr1, HB_EXPR_PTR pExpr2, HB_COMP_CARGO2_FUNC_PTR pEval );

extern void hb_compChkCompilerSwitch( HB_COMP_DECL, int iArg, const char * const args[] );
extern void hb_compChkPaths( HB_COMP_DECL );
extern void hb_compChkDefines( HB_COMP_DECL, int iArg, const char * const args[] );

extern void hb_compPrintUsage( HB_COMP_DECL, const char * szSelf );
extern void hb_compPrintCredits( HB_COMP_DECL );
extern void hb_compPrintLogo( HB_COMP_DECL );
extern void hb_compPrintModes( HB_COMP_DECL );

/* Misc functions defined in harbour.c */
extern void hb_compNOOPfill( PFUNCTION pFunc, HB_SIZE nFrom, HB_ISIZ nCount, HB_BOOL fPop, HB_BOOL fCheck );
extern HB_BOOL hb_compHasJump( PFUNCTION pFunc, HB_SIZE nPos );

/* Misc functions defined in hbfix.c */
extern void hb_compFixFuncPCode( HB_COMP_DECL, PFUNCTION pFunc );
/* Misc functions defined in hbdead.c */
extern void hb_compCodeTraceMarkDead( HB_COMP_DECL, PFUNCTION pFunc );
/* Misc functions defined in hbopt.c */
extern void hb_compOptimizePCode( HB_COMP_DECL, PFUNCTION pFunc );
extern void hb_compPCodeTraceOptimizer( HB_COMP_DECL );
/* Misc functions defined in hbstripl.c */
extern void hb_compStripFuncLines( HB_COMP_DECL, PFUNCTION pFunc );

/* output related functions defined in gen*.c */
extern void hb_compGenCCode( HB_COMP_DECL, PHB_FNAME );      /* generates the C language output */
extern void hb_compGenPortObj( HB_COMP_DECL, PHB_FNAME );    /* generates the portable objects */

extern void hb_compGenBufPortObj( HB_COMP_DECL, HB_BYTE ** pBufPtr, HB_SIZE * pnSize ); /* generates the portable objects to memory buffer */

extern void hb_compGenCRealCode( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc );
extern void hb_compGenCString( FILE * yyc, const HB_BYTE * pText, HB_SIZE nLen );

/* hbident.c */
extern const char * hb_compIdentifierNew( HB_COMP_DECL, const char * szName, int iType ); /* create the reusable identifier */
extern void hb_compIdentifierOpen( HB_COMP_DECL ); /* prepare the table of identifiers */
extern void hb_compIdentifierClose( HB_COMP_DECL ); /* release the table of identifiers */

/* compi18n.c */
extern void hb_compI18nFree( HB_COMP_DECL );
extern HB_BOOL hb_compI18nSave( HB_COMP_DECL, HB_BOOL fFinal );
extern void hb_compI18nAdd( HB_COMP_DECL, const char * szText, const char * szContext, const char * szModule, HB_UINT uiLine );
extern void hb_compI18nAddPlural( HB_COMP_DECL, const char ** szTexts, HB_ULONG ulCount, const char * szContext, const char * szModule, HB_UINT uiLine );

/* global readonly variables used by compiler
 */

extern const char * const hb_comp_szErrors[];
extern const char * const hb_comp_szWarnings[];

/* table with PCODEs' length */
extern const HB_BYTE hb_comp_pcode_len[];

/* identifier types for hb_compIdentifierNew() */
#define HB_IDENT_STATIC       0
#define HB_IDENT_FREE         1
#define HB_IDENT_COPY         2

/* /GC command line setting types */
#define HB_COMPGENC_COMPACT     0
#define HB_COMPGENC_NORMAL      1
#define HB_COMPGENC_VERBOSE     2
#define HB_COMPGENC_REALCODE    3

/* /ES command line setting types */
#define HB_EXITLEVEL_DEFAULT    0
#define HB_EXITLEVEL_SETEXIT    1
#define HB_EXITLEVEL_DELTARGET  2

/* /kx command line setting types - compatibility modes
 * (turn on a bit in HB_ULONG word)
*/
#define HB_COMPFLAG_HARBOUR      HB_SM_HARBOUR     /* 1 -kh */
#define HB_COMPFLAG_XBASE        HB_SM_XBASE       /* 2 -kx */
#define HB_COMPFLAG_SHORTCUTS    HB_SM_SHORTCUTS   /* 8 -z enable sortcuts for logical operators */
#define HB_COMPFLAG_ARRSTR       HB_SM_ARRSTR      /* 16 -ks strings as array of bytes */
#define HB_COMPFLAG_EXTOPT       HB_SM_EXTOPT      /* 32 -ko clipper incompatible optimizations */
#define HB_COMPFLAG_RT_MACRO     HB_SM_RT_MACRO    /* 64 -kr */
#define HB_COMPFLAG_OPTJUMP      0x0100            /* -kj turn off jump optimalization */
#define HB_COMPFLAG_HB_INLINE    0x0200            /* -ki hb_inLine(...) { ... } support */
#define HB_COMPFLAG_MACROTEXT    0x0400            /* -kM turn off macrotext substitution */
#define HB_COMPFLAG_USERCP       0x0800            /* -ku strings in user encoding */
#define HB_COMPFLAG_MACRODECL    0x1000            /* -kd accept macros with declared symbols */

#define HB_COMP_ISSUPPORTED(flag)   ( HB_COMP_PARAM->supported & (flag) )

#define HB_SUPPORT_XBASE            ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_XBASE) )
#define HB_SUPPORT_HARBOUR          ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_HARBOUR) )
#define HB_SUPPORT_ARRSTR           ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_ARRSTR) )
#define HB_SUPPORT_EXTOPT           ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_EXTOPT) )
#define HB_SUPPORT_MACROTEXT        ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_MACROTEXT) )
#define HB_SUPPORT_USERCP           ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_USERCP) )
#define HB_SUPPORT_MACRODECL        ( HB_COMP_ISSUPPORTED(HB_COMPFLAG_MACRODECL) )

#if defined( HB_MACRO_SUPPORT )
#  define HB_MACRO_GENFLAGS   HB_COMPFLAG_RT_MACRO
#elif ! defined( HB_COMMON_SUPPORT )
#  define HB_MACRO_GENFLAGS   ( ( ( ( HB_BYTE ) HB_COMP_PARAM->supported ) & \
                                  ( HB_COMPFLAG_HARBOUR | \
                                    HB_COMPFLAG_XBASE | \
                                    HB_COMPFLAG_SHORTCUTS | \
                                    HB_COMPFLAG_ARRSTR | \
                                    HB_COMPFLAG_EXTOPT | \
                                    HB_COMPFLAG_RT_MACRO ) ) | \
                                ( ( HB_COMP_PARAM->supported & \
                                    HB_COMPFLAG_HARBOUR ) == 0 ? \
                                  HB_COMPFLAG_SHORTCUTS : 0 ) )
#endif

HB_EXTERN_END

#endif /* HB_COMP_H_ */

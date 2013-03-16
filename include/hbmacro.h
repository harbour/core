/*
 * Harbour Project source code:
 * Header file for the Macro compiler
 *
 * Copyright 1999 Ryszard Glab
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

#ifndef HB_MACRO_H_
#define HB_MACRO_H_

#include "hbcompdf.h"
#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbexprop.h"
#include "hbpcode.h"
#include "hbmacro.ch"

HB_EXTERN_BEGIN

/* flags for compilation process
 */
#define HB_MACRO_GEN_PUSH     1   /* generate PUSH pcodes */
#define HB_MACRO_GEN_POP      2   /* generate POP pcodes */
#define HB_MACRO_GEN_ALIASED  4   /* force aliased variable */
#define HB_MACRO_GEN_TYPE     8   /* check the type of expression (from Type() function) */
#define HB_MACRO_GEN_PARE     16  /* generate parentesized list */
#define HB_MACRO_GEN_LIST     32  /* generate push operation for every comma separated expressions */
#define HB_MACRO_GEN_REFER    64  /* generate PUSH pcodes for reference to given expression */

/* values returned from compilation process
 */
#define HB_MACRO_OK           0   /* macro compiled successfully */
#define HB_MACRO_FAILURE      1   /* syntax error */

/* additional status of compilation
 */
#define HB_MACRO_CONT         1   /* everything is OK so far */
#define HB_MACRO_TOO_COMPLEX  2   /* compiled expression is too complex */
#define HB_MACRO_UDF          4   /* code uses UDF function (info used by Type() function) */
#define HB_MACRO_UNKN_SYM     8   /* requested symbol was not found in runtime symbol table */
#define HB_MACRO_UNKN_VAR     16  /* requested variable doesn't exist */

/* Global functions
 */
extern void hb_macroError( int iError, HB_COMP_DECL );
extern int hb_macroYYParse( PHB_MACRO pMacro );
extern int hb_macroSetMacro( HB_BOOL fSet, int flag );
extern HB_ULONG hb_macroAutoSetMacro( HB_ULONG ulFlag );
extern HB_BOOL hb_macroLexNew( PHB_MACRO pMacro );
extern void hb_macroLexDelete( PHB_MACRO pMacro );
extern char * hb_macroIdentNew( HB_COMP_DECL, char * );

extern PHB_EXPR hb_macroExprGenPush( PHB_EXPR, HB_COMP_DECL );
extern PHB_EXPR hb_macroExprGenPop( PHB_EXPR, HB_COMP_DECL );

extern PHB_EXPR hb_macroExprNewArrayAt( PHB_EXPR pArray, PHB_EXPR pIndex, HB_COMP_DECL );
extern PHB_EXPR hb_macroExprNewFunCall( PHB_EXPR pName, PHB_EXPR pParms, HB_COMP_DECL );

/* Size of pcode buffer incrementation
 */
#define HB_PCODE_SIZE  512

/* Declarations for functions macro.c */
#if defined( HB_MACRO_SUPPORT )

extern void hb_macroGenPCode1( HB_BYTE byte, HB_COMP_DECL );
extern void hb_macroGenPCode2( HB_BYTE byte1, HB_BYTE byte2, HB_COMP_DECL );
extern void hb_macroGenPCode3( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_COMP_DECL );
extern void hb_macroGenPCode4( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_BYTE byte4, HB_COMP_DECL );
extern void hb_macroGenPCodeN( const HB_BYTE * pBuffer, HB_SIZE nSize, HB_COMP_DECL );

extern HB_SIZE hb_macroGenJump( HB_ISIZ nOffset, HB_COMP_DECL );
extern HB_SIZE hb_macroGenJumpFalse( HB_ISIZ nOffset, HB_COMP_DECL );
extern void hb_macroGenJumpThere( HB_SIZE nFrom, HB_SIZE nTo, HB_COMP_DECL );
extern void hb_macroGenJumpHere( HB_SIZE nOffset, HB_COMP_DECL );
extern HB_SIZE hb_macroGenJumpTrue( HB_ISIZ nOffset, HB_COMP_DECL );

extern void hb_macroGenPushSymbol( const char * szSymbolName, HB_BOOL bFunction, HB_COMP_DECL );
extern void hb_macroGenPushLong( HB_MAXINT nNumber, HB_COMP_DECL );
extern void hb_macroGenPushDate( long lDate, HB_COMP_DECL );
extern void hb_macroGenPushTimeStamp( long lDate, long lTime, HB_COMP_DECL );
extern void hb_macroGenMessage( const char * szMsgName, HB_BOOL bIsObject, HB_COMP_DECL );
extern void hb_macroGenMessageData( const char * szMsg, HB_BOOL bIsObject, HB_COMP_DECL );
extern void hb_macroGenPopVar( const char * szVarName, HB_COMP_DECL );
extern void hb_macroGenPopMemvar( const char * szVarName, HB_COMP_DECL );
extern void hb_macroGenPopAliasedVar( const char * szVarName,
                                      HB_BOOL bPushAliasValue,
                                      const char * szAlias,
                                      HB_MAXINT nWorkarea, HB_COMP_DECL );
extern void hb_macroGenPushVar( const char * szVarName, HB_COMP_DECL );
extern void hb_macroGenPushVarRef( const char * szVarName, HB_COMP_DECL );
extern void hb_macroGenPushMemvarRef( const char * szVarName, HB_COMP_DECL );
extern void hb_macroGenPushAliasedVar( const char * szVarName,
                                       HB_BOOL bPushAliasValue,
                                       const char * szAlias,
                                       HB_MAXINT nWorkarea, HB_COMP_DECL );
extern void hb_macroGenPushLogical( int iTrueFalse, HB_COMP_DECL );
extern void hb_macroGenPushDouble( double dNumber, HB_BYTE bWidth, HB_BYTE bDec, HB_COMP_DECL );
extern void hb_macroGenPushFunCall( const char * szFunName, int iFlags, HB_COMP_DECL );
extern void hb_macroGenPushFunSym( const char * szFunName, int iFlags, HB_COMP_DECL );
extern void hb_macroGenPushFunRef( const char * szFunName, HB_COMP_DECL );
extern void hb_macroGenPushString( const char * szText, HB_SIZE nStrLen, HB_COMP_DECL );

extern void hb_macroCodeBlockStart( HB_COMP_DECL );
extern void hb_macroCodeBlockEnd( HB_COMP_DECL );

extern int hb_macroLocalVarGetPos( const char * szVarName, HB_COMP_DECL );
extern HB_BOOL hb_macroIsValidMacroText( const char * szText, HB_SIZE nLen );

#endif /* HB_MACRO_SUPPORT */

HB_EXTERN_END

#endif /* HB_MACRO_H_ */

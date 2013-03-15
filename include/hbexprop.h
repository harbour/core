/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Harbour Compiler
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

#ifndef HB_EXPROP_H_
#define HB_EXPROP_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

/* Definitions of function templates used in expression's message
 * handling
 */
#define  HB_EXPR_FUNC( proc )  PHB_EXPR proc( PHB_EXPR pSelf, HB_EXPR_MESSAGE iMessage, HB_COMP_DECL )
typedef  HB_EXPR_FUNC( ( * PHB_EXPR_FUNC ) );

#if defined( HB_MACRO_SUPPORT )
#define hb_comp_ExprTable     hb_macro_ExprTable
#endif

#if ! defined( HB_COMMON_SUPPORT )
extern const PHB_EXPR_FUNC hb_comp_ExprTable[ HB_EXPR_COUNT ];
#define  HB_EXPR_USE( pSelf, iMessage )  \
         hb_comp_ExprTable[ (pSelf)->ExprType ]( (pSelf), (iMessage), HB_COMP_PARAM )
#endif

extern HB_EXPORT_INT PHB_EXPR hb_compExprNewEmpty( HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewNil( HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewDouble( double, HB_BYTE, HB_BYTE, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewLong( HB_MAXINT nValue, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewDate( long lDate, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewTimeStamp( long lDate, long lTime, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewString( const char * szValue, HB_SIZE nLen, HB_BOOL fDealloc, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewLogical( int iValue, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewSelf( HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewCodeBlock( char * string, HB_SIZE nLen, int iFlags, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewVar( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewAliasVar( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewAliasExpr( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMacro( PHB_EXPR, unsigned char cMacroOp, const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewFunName( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewRTVar( const char * szName, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewAlias( const char * szName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewEQ( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewNE( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewLT( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewLE( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewGT( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewGE( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewIN( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPlus( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMinus( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMult( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewDiv( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMod( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPower( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewAssign( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewEqual( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPlusEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMinusEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMultEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewDivEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewModEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewExpEq( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPostInc( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPostDec( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPreInc( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewPreDec( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewAnd( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewOr( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewNot( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewNegate( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewVarRef( const char * szVarName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewFunRef( const char * szFunName, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewFunCall( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewRef( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewCodeblockExpr( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewSend( const char *, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMacroSend( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMethodObject( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewMethodCall( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewList( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewArgList( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewArgRef( HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewArray( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewHash( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewArrayAt( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprAddListExpr( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprCBVarAdd( PHB_EXPR, const char * szVarName, HB_BYTE bType, HB_COMP_DECL );
extern HB_EXPORT_INT void hb_compExprCBVarDel( PHB_CBVAR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprAddCodeblockExpr( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprSetCodeblockBody( PHB_EXPR pExpr, HB_BYTE * pCode, HB_SIZE nLen );
extern HB_EXPORT_INT PHB_EXPR hb_compExprNewIIF( PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprMacroAsAlias( PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprAssign( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprEqual( PHB_EXPR, PHB_EXPR );
extern HB_EXPORT_INT PHB_EXPR hb_compExprAssignStatic( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprListTypeCheck( PHB_EXPR pExpr, HB_EXPRTYPE ExprType );
extern HB_EXPORT_INT HB_ULONG hb_compExprListLen( PHB_EXPR );
extern HB_EXPORT_INT HB_ULONG hb_compExprParamListLen( PHB_EXPR );
extern HB_EXPORT_INT HB_SIZE hb_compExprParamListCheck( HB_COMP_DECL, PHB_EXPR );

extern HB_EXPORT_INT const char * hb_compExprDescription( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprType( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprIsInteger( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprIsLong( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprAsInteger( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprAsNumSign( PHB_EXPR );
extern HB_EXPORT_INT int hb_compExprIsString( PHB_EXPR );
extern HB_EXPORT_INT HB_SIZE hb_compExprAsStringLen( PHB_EXPR );
extern HB_EXPORT_INT HB_MAXINT hb_compExprAsLongNum( PHB_EXPR );
extern HB_EXPORT_INT const char * hb_compExprAsString( PHB_EXPR );
extern HB_EXPORT_INT const char * hb_compExprAsSymbol( PHB_EXPR );

extern HB_EXPORT_INT PHB_EXPR hb_compExprListStrip( PHB_EXPR, HB_COMP_DECL );

extern HB_EXPORT_INT PHB_EXPR hb_compExprSetOperand( PHB_EXPR, PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprSetGetBlock( PHB_EXPR pExpr, HB_COMP_DECL );

extern HB_EXPORT_INT void hb_compExprDelOperator( PHB_EXPR, HB_COMP_DECL );

extern HB_EXPORT_INT PHB_EXPR hb_compExprReducePower( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceMod( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceDiv( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceMult( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceMinus( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReducePlus( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceNegate( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceIN( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceNE( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceGE( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceLE( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceGT( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceLT( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceEQ( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceAnd( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceOr( PHB_EXPR pSelf, HB_COMP_DECL );
extern HB_EXPORT_INT PHB_EXPR hb_compExprReduceIIF( PHB_EXPR, HB_COMP_DECL );

extern HB_EXPORT_INT HB_BOOL hb_compExprReduceAT( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceCHR( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBCHAR( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceLEN( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceASC( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBCODE( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceINT( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceEMPTY( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceSTOT( PHB_EXPR, HB_USHORT usCount, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceSTOD( PHB_EXPR, HB_USHORT usCount, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceDTOS( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceCTOD( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceUPPER( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceMIN( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceMAX( PHB_EXPR, HB_COMP_DECL );
extern HB_EXPORT_INT HB_BOOL hb_compExprReduceBitFunc( PHB_EXPR, HB_MAXINT nResult, HB_BOOL fBool, HB_COMP_DECL );

HB_EXTERN_END

#endif  /* HB_EXPROP_H_ */

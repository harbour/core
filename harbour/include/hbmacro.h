/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Macro compiler
 *
 * Copyright 1999 Ryszard Glab
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

#ifndef HB_MACRO_H_
#define HB_MACRO_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include <time.h>

/* Standard parameters passed to macro aware functions
 */
#define HB_BISON_PTR void *
#define HB_MACRO_PARAM  pMacro
#define HB_MACRO_DECL   HB_BISON_PTR HB_MACRO_PARAM

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbexprop.h"
#include "hbpcode.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* flags for compilation process
 */
#define HB_MACRO_GEN_PUSH     1   /* generate PUSH pcodes */
#define HB_MACRO_GEN_POP      2   /* generate POP pcodes */
#define HB_MACRO_GEN_ALIASED  4   /* force aliased variable */
#define HB_MACRO_GEN_TYPE     8   /* check the type of expression (from TYPE() function) */
#define HB_MACRO_DEALLOCATE   128 /* macro structure is allocated on the heap */

/* values returned from compilation process
 */
#define HB_MACRO_OK           0   /* macro compiled successfully */
#define HB_MACRO_FAILURE      1   /* syntax error */

/* additional status of compilation
 */
#define HB_MACRO_CONT         1   /* everything is OK so far */
#define HB_MACRO_TOO_COMPLEX  2   /* compiled expression is too complex */
#define HB_MACRO_UDF          4   /* code uses UDF function (info used by TYPE function) */
#define HB_MACRO_UNKN_SYM     8   /* requested symbol was not found in runtime symbol table */
#define HB_MACRO_UNKN_VAR     16  /* requested variable doesn't exist */



/* Global functions
 */
extern void hb_macroError( int iError, HB_BISON_PTR pMacro );
extern int hb_macroYYParse( HB_MACRO_PTR pMacro );

extern void hb_compGenPCode1( BYTE byte, HB_BISON_PTR pMacro );
extern void hb_compGenPCode2( BYTE byte1, BYTE byte2, HB_BISON_PTR pMacro );
extern void hb_compGenPCode3( BYTE byte1, BYTE byte2, BYTE byte3, HB_BISON_PTR pMacro );
extern void hb_compGenPCode4( BYTE byte1, BYTE byte2, BYTE byte3, BYTE byte4, HB_BISON_PTR pMacro );
extern void hb_compGenPCodeN( BYTE * pBuffer, ULONG ulSize, HB_BISON_PTR pMacro );

/* Size of pcode buffer incrementation
 */
#define HB_PCODE_SIZE  512

/* Bison requires (void *) pointer  - some code needs HB_MACRO_PTR pointer
 */
#define HB_MACRO_DATA     ( (HB_MACRO_PTR) HB_MACRO_PARAM )
#define HB_PCODE_DATA     ( HB_MACRO_DATA->pCodeInfo )

/* Declarations for functions macro.c */

extern int hb_compLocalVarGetPos( char * szVarName, HB_BISON_PTR pMacro );
extern ULONG hb_compGenJump( LONG lOffset, HB_BISON_PTR pMacro );
extern ULONG hb_compGenJumpFalse( LONG lOffset, HB_BISON_PTR pMacro );
extern void hb_compGenJumpThere( ULONG ulFrom, ULONG ulTo, HB_BISON_PTR pMacro );
extern void hb_compGenJumpHere( ULONG ulOffset, HB_BISON_PTR pMacro );
extern ULONG hb_compGenJumpTrue( LONG lOffset, HB_BISON_PTR pMacro );
extern void hb_compMemvarGenPCode( BYTE bPCode, char * szVarName, HB_BISON_PTR pMacro );
extern void hb_compGenPushSymbol( char * szSymbolName, int isFunction, HB_BISON_PTR pMacro );
extern void hb_compGenPushLong( long lNumber, HB_BISON_PTR pMacro );
extern void hb_compGenMessage( char * szMsgName, HB_BISON_PTR pMacro );
extern void hb_compGenMessageData( char * szMsg, HB_BISON_PTR pMacro );
extern void hb_compGenPopVar( char * szVarName, HB_BISON_PTR pMacro );
extern void hb_compGenPopAliasedVar( char * szVarName,
                                     BOOL bPushAliasValue,
                                     char * szAlias,
                                     long lWorkarea, HB_BISON_PTR pMacro );
extern void hb_compGenPushVar( char * szVarName, HB_BISON_PTR pMacro );
extern void hb_compGenPushVarRef( char * szVarName, HB_BISON_PTR pMacro );
extern void hb_compGenPushAliasedVar( char * szVarName,
                                      BOOL bPushAliasValue,
                                      char * szAlias,
                                      long lWorkarea, HB_BISON_PTR pMacro );
extern void hb_compGenPushLogical( int iTrueFalse, HB_BISON_PTR pMacro );
extern void hb_compGenPushDouble( double dNumber, BYTE bWidth, BYTE bDec, HB_BISON_PTR pMacro );
extern void hb_compGenPushFunCall( char * szFunName, HB_BISON_PTR pMacro );
extern void hb_compGenPushString( char * szText, ULONG ulStrLen, HB_BISON_PTR pMacro );
extern void hb_compCodeBlockStart( HB_BISON_PTR pMacro );
extern void hb_compCodeBlockEnd( HB_BISON_PTR pMacro );

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_MACRO_H_ */

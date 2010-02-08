/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    debugger C API
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HB_APIDBG_H_
#define HB_APIDBG_H_

#include "hbapi.h"

HB_EXTERN_BEGIN

/*Debugger API */

/* HVM debugger function */
typedef void (*HB_DBGENTRY_FUNC) ( int , int , const char *, int , PHB_ITEM );
extern HB_DBGENTRY_FUNC hb_dbg_SetEntry( HB_DBGENTRY_FUNC pFunDbgEntry );
extern HB_BOOL hb_dbg_InvokeDebug( HB_BOOL bInvoke );
extern HB_ULONG hb_dbg_ProcLevel( void );
extern PHB_ITEM hb_dbg_vmVarSGet( PHB_ITEM pStaticsBase, int nOffset );
extern PHB_ITEM hb_dbg_vmVarLGet( int iLevel, int iLocal );
extern HB_ULONG hb_dbg_vmVarGCount( void );
extern PHB_ITEM hb_dbg_vmVarGGet( int nGlobal, int nOffset );

/* internal debugger function */
extern void hb_dbgEntry( int nMode, int nLine, const char * szName, int nIndex, PHB_ITEM pFrame );
extern void hb_dbgAddBreak( void * handle, const char * cModule, int nLine, const char * szFunction );
extern void hb_dbgAddWatch( void * handle, const char * szExpr, HB_BOOL bTrace );
extern void hb_dbgDelBreak( void * handle, int nBreak );
extern void hb_dbgDelWatch( void * handle, int nWatch );
extern PHB_ITEM hb_dbgGetExpressionValue( void * handle, const char *expression );
extern PHB_ITEM hb_dbgGetSourceFiles( void * handle );
extern PHB_ITEM hb_dbgGetWatchValue( void * handle, int nWatch );
extern HB_BOOL hb_dbgIsValidStopLine( void * handle, const char *szModule, int nLine );
extern void hb_dbgSetCBTrace( void * handle, HB_BOOL bCBTrace );
extern void hb_dbgSetGo( void * handle );
extern void hb_dbgSetInvoke( void * handle, HB_BOOL ( *pFunInvoke )( void ) );
extern void hb_dbgSetNextRoutine( void * handle );
extern void hb_dbgSetQuit( void * handle );
extern void hb_dbgSetToCursor( void * handle, const char * szModule, int nLine );
extern void hb_dbgSetTrace( void * handle );
extern void hb_dbgSetWatch( void * handle, int nWatch, const char * szExpr, HB_BOOL bTrace );

HB_EXTERN_END

#endif /* HB_APIDBG_H_ */

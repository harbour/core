/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Virtual Machine API
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

#ifndef HB_VM_H_
#define HB_VM_H_

#include "hbapi.h"

#if defined(HB_EXTERN_C)
extern "C" {
#endif

/* Harbour virtual machine init/exit functions */
extern void HB_EXPORT hb_vmInit( BOOL bStartMainProc );
extern void HB_EXPORT hb_vmQuit( void );            /* Immediately quits the virtual machine */

/* Harbour virtual machine functions */
extern void HB_EXPORT hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols );  /* invokes the virtual machine */
extern void HB_EXPORT hb_vmProcessSymbols( PHB_SYMB pSymbols, USHORT uiSymbols ); /* statics symbols initialization */
extern void    hb_vmSymbolInit_RT( void );   /* initialization of runtime support symbols */

/* Harbour virtual machine escaping API */
extern void    hb_vmRequestBreak( PHB_ITEM pItem );
extern void    hb_vmRequestCancel( void );
extern void    hb_vmRequestDebug( void );
extern void    hb_vmRequestEndProc( void );
extern USHORT  hb_vmRequestQuery( void );
extern void    hb_vmRequestQuit( void );

/* Return values of hb_vmRequestQuery() */
#define HB_QUIT_REQUESTED       1   /* immediately quit the application */
#define HB_BREAK_REQUESTED      2   /* break to nearest RECOVER/END sequence */
#define HB_ENDPROC_REQUESTED    4   /* immediately return from procedure (error handler in macro evaluation) */

/* Public PCode functions */

/* Object */
extern void    hb_vmMessage( PHB_SYMB pSymMsg ); /* sends a message to an object */

/* Execution */
extern void    hb_vmDo( USHORT uiParams );      /* invoke the virtual machine */
extern void    hb_vmFunction( USHORT uiParams ); /* executes a function saving its result */
extern void    hb_vmSend( USHORT uiParams ); /* sends a message to an object */
extern PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlockItem ); /* executes passed codeblock with no arguments */
/* executes passed codeblock with variable number of arguments */
extern PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlockItem, ULONG ulArgCount, ... );

/* Push */
extern void    hb_vmPush( PHB_ITEM pItem );     /* pushes a generic item onto the stack */
extern void    hb_vmPushNil( void );            /* in this case it places nil at self */
extern void    hb_vmPushNumber( double dNumber, int iDec ); /* pushes a number on to the stack and decides if it is integer, long or double */
extern void    hb_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
extern void    hb_vmPushLong( long lNumber ); /* pushes a long number onto the stack */
extern void    hb_vmPushDouble( double lNumber, int iDec ); /* pushes a double number onto the stack */
extern void    hb_vmPushLogical( BOOL bValue );    /* pushes a logical value onto the stack */
extern void    hb_vmPushString( char * szText, ULONG length );  /* pushes a string on to the stack */
extern void    hb_vmPushDate( long lDate );   /* pushes a long date onto the stack */
extern void    hb_vmPushSymbol( PHB_SYMB pSym ); /* pushes a function pointer onto the stack */
extern void    hb_vmPushPointer( void * ); /* push an item of HB_IT_POINTER type */

#if defined(HB_EXTERN_C)
}
#endif

#endif /* HB_VM_H_ */

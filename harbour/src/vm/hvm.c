/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Virtual Machine
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    hb_vmPushLongConst()
 *    hb_vmPushDoubleConst()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    __dbgVMVarSGet()
 *    __dbgVMVarSList()
 *
 * See COPYING.txt for licensing terms.
 *
 */

#define HB_STACK_PRELOAD

#include "hbvmopt.h"
#include "hbapi.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapicls.h"
#include "hbapidbg.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbapigt.h"
#include "hbapicdp.h"
#include "hbvm.h"
#include "hbxvm.h"
#include "hbpcode.h"
#include "hbset.h"
#include "hbdate.h"
#include "hbmath.h"
#include "hbinkey.ch"
#include "inkey.ch"
#include "hbdebug.ch"
#if defined( HB_MT_VM )
#  include "hbthread.h"
#endif /* HB_MT_VM */
#include "hbmemory.ch"

#ifndef HB_NO_PROFILER
#  include <time.h>
#endif


HB_FUNC_EXTERN( SYSINIT );

/* PCode functions */

/* Operators (mathematical / character / misc) */
static void    hb_vmNegate( void );          /* negates (-) the latest value on the stack */
static void    hb_vmInc( PHB_ITEM pItem );   /* increment the latest numeric value on the stack */
static void    hb_vmDec( PHB_ITEM pItem );   /* decrements the latest numeric value on the stack */
static void    hb_vmFuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the satck */
static void    hb_vmAddInt( PHB_ITEM pResult, HB_LONG lAdd );      /* add integer to given item */
static void    hb_vmPlus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );        /* sums given values */
static void    hb_vmMinus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );       /* substracts given values */
static void    hb_vmMult( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );        /* multiplies given values */
static void    hb_vmDivide( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );      /* divides the given values */
static void    hb_vmModulus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );     /* calculates modulus given values */
static void    hb_vmPower( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 );       /* power given values */

/* Operators (relational) */
static void    hb_vmEqual( void );           /* checks if the two latest values on the stack are equal, removes both and leaves result */
static void    hb_vmExactlyEqual( void );    /* checks if the two latest values on the stack are exactly equal, removes both and leaves result */
static void    hb_vmNotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
static void    hb_vmLess( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
static void    hb_vmLessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
static void    hb_vmGreater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
static void    hb_vmGreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
static void    hb_vmInstring( void );        /* check whether string 1 is contained in string 2 */
static void    hb_vmForTest( void );         /* test for end condition of for */
static void    hb_vmSeqBlock( void );        /* set begin sequence WITH codeblock */
static void    hb_vmWithObjectStart( void ); /* prepare WITH OBJECT block */
static void    hb_vmEnumStart( int nVars, int nDescend ); /* prepare FOR EACH loop */
static void    hb_vmEnumNext( void );        /* increment FOR EACH loop counter */
static void    hb_vmEnumPrev( void );        /* decrement FOR EACH loop counter */
static void    hb_vmEnumEnd( void );         /* rewind the stack after FOR EACH loop counter */
static const HB_BYTE * hb_vmSwitch( const HB_BYTE * pCode, HB_USHORT );  /* make a SWITCH statement */

/* Operators (logical) */
static void    hb_vmNot( void );             /* changes the latest logical value on the stack */
static void    hb_vmAnd( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
static void    hb_vmOr( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */

/* Array */
static void    hb_vmArrayPush( void );       /* pushes an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPushRef( void );    /* pushes a reference to an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPop( void );        /* pops a value from the stack */
static void    hb_vmArrayDim( HB_USHORT uiDimensions ); /* generates an uiDimensions Array and initialize those dimensions from the stack values */
static void    hb_vmArrayGen( HB_SIZE nElements ); /* generates an nElements Array and fills it from the stack values */
static void    hb_vmHashGen( HB_SIZE nElements ); /* generates an nElements Hash and fills it from the stack values */

/* macros */
static void    hb_vmMacroDo( HB_USHORT uiArgSets );         /* execute function passing arguments set(s) on HVM stack func( &var ) */
static void    hb_vmMacroFunc( HB_USHORT uiArgSets );       /* execute procedure passing arguments set(s) on HVM stack func( &var ) */
static void    hb_vmMacroSend( HB_USHORT uiArgSets );       /* execute procedure passing arguments set(s) on HVM stack func( &var ) */
static void    hb_vmMacroArrayGen( HB_USHORT uiArgSets );   /* generate array from arguments set(s) on HVM stack { &var } */
static void    hb_vmMacroPushIndex( void );              /* push macro array index {...}[ &var ] */

/* Database */
static HB_ERRCODE hb_vmSelectWorkarea( PHB_ITEM, PHB_SYMB );  /* select the workarea using a given item or a substituted value */
static void       hb_vmSwapAlias( void );           /* swaps items on the eval stack and pops the workarea number */

/* Execution */
static HARBOUR hb_vmDoBlock( void );             /* executes a codeblock */
static void    hb_vmFrame( HB_USHORT usLocals, unsigned char ucParams ); /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmVFrame( HB_USHORT usLocals, unsigned char ucParams ); /* increases the stack pointer for the amount of locals and variable number of params suplied */
static void    hb_vmSFrame( PHB_SYMB pSym );     /* sets the statics frame for a function */
static void    hb_vmStatics( PHB_SYMB pSym, HB_USHORT uiStatics ); /* increases the global statics array to hold a PRG statics */
static void    hb_vmInitThreadStatics( HB_USHORT uiCount, const HB_BYTE * pCode ); /* mark thread static variables */
static void    hb_vmStaticsClear( void );       /* clear complex static variables */
static void    hb_vmStaticsRelease( void );     /* release arrays with static variables */
/* Push */
static void    hb_vmPushAlias( void );            /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB ); /* pushes an aliased field on the eval stack */
static void    hb_vmPushAliasedVar( PHB_SYMB );   /* pushes an aliased variable on the eval stack */
static void    hb_vmPushBlock( const HB_BYTE * pCode, PHB_SYMB pSymbols, HB_SIZE nLen ); /* creates a codeblock */
static void    hb_vmPushBlockShort( const HB_BYTE * pCode, PHB_SYMB pSymbols, HB_SIZE nLen ); /* creates a codeblock */
static void    hb_vmPushMacroBlock( const HB_BYTE * pCode, HB_SIZE nSize, HB_USHORT usParams ); /* creates a macro-compiled codeblock */
static void    hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec ); /* Pushes a double constant (pcode) */
static void    hb_vmPushLocal( int iLocal );       /* pushes the containts of a local onto the stack */
static void    hb_vmPushLocalByRef( int iLocal );  /* pushes a local by refrence onto the stack */
static void    hb_vmPushHBLong( HB_MAXINT nNumber ); /* pushes a HB_MAXINT number onto the stack */
#if ! defined( HB_LONG_LONG_OFF )
   static void hb_vmPushLongLongConst( HB_LONGLONG lNumber );  /* Pushes a long long constant (pcode) */
#endif
#if HB_VMINT_MAX >= INT32_MAX
static void    hb_vmPushIntegerConst( int iNumber );  /* Pushes a int constant (pcode) */
#else
static void    hb_vmPushLongConst( long lNumber );    /* Pushes a long constant (pcode) */
#endif
static void    hb_vmPushStatic( HB_USHORT uiStatic );     /* pushes the containts of a static onto the stack */
static void    hb_vmPushStaticByRef( HB_USHORT uiStatic ); /* pushes a static by refrence onto the stack */
static void    hb_vmPushVariable( PHB_SYMB pVarSymb ); /* pushes undeclared variable */
static void    hb_vmPushObjectVarRef( void );   /* pushes reference to object variable */
static void    hb_vmPushVParams( void );        /* pushes variable parameters */
static void    hb_vmPushAParams( void );        /* pushes array items */
static void    hb_vmPushUnRef( void );          /* push the unreferenced latest value on the stack */
static void    hb_vmDuplicate( void );          /* duplicates the latest value on the stack */
static void    hb_vmDuplUnRef( void );          /* duplicates the latest value on the stack and unref the source one */
static void    hb_vmSwap( int iCount );        /* swap bCount+1 time two items on HVM stack starting from the most top one */

/* Pop */
static HB_BOOL hb_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
static void    hb_vmPopAlias( void );             /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );  /* pops an aliased field from the eval stack*/
static void    hb_vmPopAliasedVar( PHB_SYMB );    /* pops an aliased variable from the eval stack*/
static void    hb_vmPopLocal( int iLocal );       /* pops the stack latest value onto a local */
static void    hb_vmPopStatic( HB_USHORT uiStatic ); /* pops the stack latest value onto a static */

/* misc */
static void    hb_vmDoInitStatics( void );        /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( HB_BOOL );   /* executes all defined PRGs INIT functions */
static void    hb_vmDoExitFunctions( void );      /* executes all defined PRGs EXIT functions */
static void    hb_vmReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */

static void    hb_vmMsgIndexReference( PHB_ITEM pRefer, PHB_ITEM pObject, PHB_ITEM pIndex ); /* create object index reference */

#ifndef HB_NO_DEBUG
static void    hb_vmLocalName( HB_USHORT uiLocal, const char * szLocalName ); /* locals and parameters index and name information for the debugger */
static void    hb_vmStaticName( HB_BYTE bIsGlobal, HB_USHORT uiStatic, const char * szStaticName ); /* statics vars information for the debugger */
static void    hb_vmModuleName( const char * szModuleName ); /* PRG and function name information for the debugger */

static void    hb_vmDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PHB_ITEM pFrame );
static void    hb_vmDebuggerExit( HB_BOOL fRemove );      /* shuts down the debugger */
static void    hb_vmDebuggerShowLine( HB_USHORT uiLine ); /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void );     /* notifies the debugger for an endproc */

static PHB_DYNS s_pDynsDbgEntry = NULL;   /* Cached __DBGENTRY symbol */
static HB_DBGENTRY_FUNC s_pFunDbgEntry;   /* C level debugger entry */
#endif

static HB_BOOL s_fInternalsEnabled = HB_TRUE;

#if defined( HB_MT_VM )
static int volatile hb_vmThreadRequest = 0;
static void hb_vmRequestTest( void );

static PHB_ITEM s_pSymbolsMtx = NULL;

static HB_CRITICAL_NEW( s_atInitMtx );
#  define HB_ATINIT_LOCK()    hb_threadEnterCriticalSection( &s_atInitMtx )
#  define HB_ATINIT_UNLOCK()  hb_threadLeaveCriticalSection( &s_atInitMtx )
#  define HB_TASK_SHEDULER()  HB_THREAD_SHEDULER()
#else
#  define HB_ATINIT_LOCK()
#  define HB_ATINIT_UNLOCK()
#  define HB_TASK_SHEDULER()
#endif /* HB_MT_VM */

#ifndef HB_NO_PROFILER
static HB_ULONG hb_ulOpcodesCalls[ HB_P_LAST_PCODE ]; /* array to profile opcodes calls */
static HB_ULONG hb_ulOpcodesTime[ HB_P_LAST_PCODE ];  /* array to profile opcodes consumed time */
static HB_BOOL hb_bProfiler = HB_FALSE;                        /* profiler status is off */
#endif

#if defined( HB_PRG_TRACE )
static HB_BOOL hb_bTracePrgCalls = HB_FALSE; /* prg tracing is off */
#  define HB_TRACE_PRG( _TRMSG_ ) if( hb_bTracePrgCalls ) HB_TRACE( HB_TR_ALWAYS, _TRMSG_ )
#else
#  define HB_TRACE_PRG( _TRMSG_ )
#endif

static const char * s_vm_pszLinkedMain = NULL; /* name of startup function set by linker */

/* virtual machine state */

HB_SYMB hb_symEval = { "EVAL", { HB_FS_PUBLIC }, { hb_vmDoBlock }, NULL }; /* symbol to evaluate codeblocks */

static HB_BOOL  s_fHVMActive = HB_FALSE;  /* is HVM ready for PCODE executing */
static HB_BOOL  s_fDoExitProc = HB_TRUE;  /* execute EXIT procedures */
static int      s_nErrorLevel = 0;     /* application exit errorlevel */
static PHB_SYMB s_pSymStart = NULL;    /* start symbol of the application. MAIN() is not required */

static PHB_SYMBOLS s_pSymbols = NULL;  /* to hold a linked list of all different modules symbol tables */
static HB_ULONG    s_ulFreeSymbols = 0;/* number of free module symbols */
static void *      s_hDynLibID = NULL; /* unique identifer to mark symbol tables loaded from dynamic libraries */
static HB_BOOL     s_fCloneSym = HB_FALSE;/* clone registered symbol tables */

/* main VM thread stack ID */
static void * s_main_thread = NULL;

/* Various compatibility flags
 */
static HB_U32 s_VMFlags = HB_VMFLAG_HARBOUR;
#undef hb_vmFlagEnabled
#define hb_vmFlagEnabled( flag )      ( s_VMFlags & ( flag ) )

/* Keycodes to stop virtual machine
 */
static int s_VMCancelKey   = K_ALT_C;
static int s_VMCancelKeyEx = HB_K_ALT_C;

/* SEQUENCE envelope items position from stack top active
 */
#define HB_RECOVER_STATE   -1
#define HB_RECOVER_VALUE   -2

#define HB_SEQ_CANRECOVER  64
#define HB_SEQ_DOALWAYS    128

static PHB_FUNC_LIST s_InitFunctions = NULL;
static PHB_FUNC_LIST s_ExitFunctions = NULL;
static PHB_FUNC_LIST s_QuitFunctions = NULL;

static void hb_vmAddModuleFunction( PHB_FUNC_LIST * pLstPtr, HB_INIT_FUNC pFunc, void * cargo )
{
   PHB_FUNC_LIST pLst = ( PHB_FUNC_LIST ) hb_xgrab( sizeof( HB_FUNC_LIST ) );

   pLst->pFunc = pFunc;
   pLst->cargo = cargo;
   HB_ATINIT_LOCK();
   pLst->pNext = *pLstPtr;
   *pLstPtr = pLst;
   HB_ATINIT_UNLOCK();
}

static void hb_vmDoModuleFunctions( PHB_FUNC_LIST pLst )
{
   while( pLst )
   {
      pLst->pFunc( pLst->cargo );
      pLst = pLst->pNext;
   }
}

static void hb_vmCleanModuleFunctions( void )
{
   PHB_FUNC_LIST pLst;

   while( s_InitFunctions )
   {
      pLst = s_InitFunctions;
      s_InitFunctions = pLst->pNext;
      hb_xfree( pLst );
   }
   while( s_ExitFunctions )
   {
      pLst = s_ExitFunctions;
      s_ExitFunctions = pLst->pNext;
      hb_xfree( pLst );
   }
   while( s_QuitFunctions )
   {
      pLst = s_QuitFunctions;
      s_QuitFunctions = pLst->pNext;
      hb_xfree( pLst );
   }
}

void hb_vmAtInit( HB_INIT_FUNC pFunc, void * cargo )
{
   hb_vmAddModuleFunction( &s_InitFunctions, pFunc, cargo );
}

void hb_vmAtExit( HB_INIT_FUNC pFunc, void * cargo )
{
   hb_vmAddModuleFunction( &s_ExitFunctions, pFunc, cargo );
}

void hb_vmAtQuit( HB_INIT_FUNC pFunc, void * cargo )
{
   hb_vmAddModuleFunction( &s_QuitFunctions, pFunc, cargo );
}

static void hb_vmDoModuleInitFunctions( void )
{
   hb_vmDoModuleFunctions( s_InitFunctions );
}

static void hb_vmDoModuleExitFunctions( void )
{
   hb_vmDoModuleFunctions( s_ExitFunctions );
}

static void hb_vmDoModuleQuitFunctions( void )
{
   hb_vmDoModuleFunctions( s_QuitFunctions );
}


/* call __HBVMINIT function to initialize GETLIST public variable
 * and set ErrorBlock() by ErrorSys() function
 */
static void hb_vmDoInitHVM( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "__HBVMINIT" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmProc( 0 );
   }
}

/* call __SetHelpK() if HELP() function is linked */
static void hb_vmDoInitHelp( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "HELP" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      pDynSym = hb_dynsymFind( "__SETHELPK" );
      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
      {
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         hb_vmProc( 0 );
      }
   }
}

#if ! defined( HB_MT_VM )

HB_BOOL hb_vmIsMt( void ) { return HB_FALSE; }
void hb_vmLock( void ) {}
void hb_vmUnlock( void ) {}
HB_BOOL hb_vmSuspendThreads( HB_BOOL fWait ) { HB_SYMBOL_UNUSED( fWait ); return HB_TRUE; }
void hb_vmResumeThreads( void ) {}
/*
HB_BOOL hb_vmThreadRegister( void * Cargo ) { HB_SYMBOL_UNUSED( Cargo ); return HB_FALSE; }
void hb_vmThreadRelease( void * Cargo )
{
   PHB_THREADSTATE pState = ( PHB_THREADSTATE ) Cargo;
   PHB_ITEM pThItm = pState->pThItm;
   pState->pThItm = NULL;
   if( pThItm )
      hb_itemRelease( pThItm );
}
*/
#else

static HB_CRITICAL_NEW( s_vmMtx );
static HB_COND_NEW( s_vmCond );

/* number of allocated HVM stacks */
static int volatile s_iStackCount = 0;
/* number of running HVM threads */
static int volatile s_iRunningCount = 0;
/* active HVM stacks list */
static PHB_THREADSTATE s_vmStackLst = NULL;
/* thread number */
static HB_THREAD_NO s_threadNo = 0;

#  define HB_THREQUEST_STOP   1
#  define HB_THREQUEST_QUIT   2

#  define HB_VM_LOCK()    hb_threadEnterCriticalSection( &s_vmMtx )
#  define HB_VM_UNLOCK()  hb_threadLeaveCriticalSection( &s_vmMtx )

HB_BOOL hb_vmIsMt( void ) { return HB_TRUE; }

static void hb_vmRequestTest( void )
{
   HB_VM_LOCK();

   s_iRunningCount--;
   for( ;; )
   {
      if( hb_vmThreadRequest & HB_THREQUEST_QUIT )
      {
         HB_STACK_TLS_PRELOAD
         if( ! hb_stackQuitState() )
         {
            hb_stackSetQuitState( HB_TRUE );
            hb_stackSetActionRequest( HB_QUIT_REQUESTED );
         }
      }
      if( hb_vmThreadRequest & HB_THREQUEST_STOP )
      {
         hb_threadCondBroadcast( &s_vmCond );
         hb_threadCondWait( &s_vmCond, &s_vmMtx );
      }
      else
         break;
   }
   s_iRunningCount++;

   HB_VM_UNLOCK();
}

/* unlock VM, allow GC and other exclusive single task code execution */
void hb_vmUnlock( void )
{
   HB_STACK_TLS_PRELOAD

   if( hb_stackId() )   /* check if thread has associated HVM stack */
   {
      if( hb_stackUnlock() == 1 )
      {
         HB_VM_LOCK();
         s_iRunningCount--;
         if( hb_vmThreadRequest )
         {
            if( hb_vmThreadRequest & HB_THREQUEST_QUIT )
            {
               if( ! hb_stackQuitState() )
               {
                  hb_stackSetQuitState( HB_TRUE );
                  hb_stackSetActionRequest( HB_QUIT_REQUESTED );
               }
            }
            hb_threadCondBroadcast( &s_vmCond );
         }
         HB_VM_UNLOCK();
      }
   }

   HB_TASK_SHEDULER();
}

/* lock VM blocking GC and other exclusive single task code execution */
void hb_vmLock( void )
{
   HB_STACK_TLS_PRELOAD

   if( hb_stackId() )   /* check if thread has associated HVM stack */
   {
      if( hb_stackLock() == 0 )
      {
         HB_VM_LOCK();
         for( ;; )
         {
            if( hb_vmThreadRequest & HB_THREQUEST_QUIT )
            {
               if( ! hb_stackQuitState() )
               {
                  hb_stackSetQuitState( HB_TRUE );
                  hb_stackSetActionRequest( HB_QUIT_REQUESTED );
               }
            }
            if( hb_vmThreadRequest & HB_THREQUEST_STOP )
               hb_threadCondWait( &s_vmCond, &s_vmMtx );
            else
               break;
         }
         s_iRunningCount++;
         HB_VM_UNLOCK();
      }
   }
}

/* (try to) stop all threads except current one */
HB_BOOL hb_vmSuspendThreads( HB_BOOL fWait )
{
   HB_VM_LOCK();

   if( ( hb_vmThreadRequest & ( HB_THREQUEST_STOP | HB_THREQUEST_QUIT ) ) == 0 )
   {
      hb_vmThreadRequest |= HB_THREQUEST_STOP;
      --s_iRunningCount;
      for( ;; )
      {
         if( s_iRunningCount <= 0 )
         {
            hb_vmThreadRequest &= ~HB_THREQUEST_STOP;
            ++s_iRunningCount;
            return HB_TRUE;
         }
         if( ! fWait )
            break;
         hb_threadCondWait( &s_vmCond, &s_vmMtx );
         if( hb_vmThreadRequest & HB_THREQUEST_QUIT )
            break;
      }
      hb_vmThreadRequest &= ~HB_THREQUEST_STOP;
      ++s_iRunningCount;
      hb_threadCondBroadcast( &s_vmCond );
   }

   HB_VM_UNLOCK();

   return HB_FALSE;
}

/* unblock execution of threads stopped by hb_vmSuspendThreads() */
void hb_vmResumeThreads( void )
{
   hb_vmThreadRequest &= ~HB_THREQUEST_STOP;
   hb_threadCondBroadcast( &s_vmCond );
   HB_VM_UNLOCK();
}

/* send QUIT request to all threads except current one
 * and wait for their termination,
 * should be called only by main HVM thread
 */
void hb_vmTerminateThreads( void )
{
   HB_STACK_TLS_PRELOAD

   if( s_main_thread == hb_stackId() )
   {
      HB_VM_LOCK();

      hb_vmThreadRequest |= HB_THREQUEST_QUIT;
      --s_iRunningCount;

      hb_threadMutexUnlockAll();
      hb_threadMutexUnsubscribeAll();

      hb_threadCondBroadcast( &s_vmCond );

      while( s_iStackCount > 1 )
         hb_threadCondWait( &s_vmCond, &s_vmMtx );

      ++s_iRunningCount;
      /* hb_vmThreadRequest &= ~HB_THREQUEST_QUIT; */
      hb_vmThreadRequest = 0;

      HB_VM_UNLOCK();
   }
}

/* wait for all threads to terminate
 * should be called only by main HVM thread
 */
void hb_vmWaitForThreads( void )
{
   HB_STACK_TLS_PRELOAD

   if( s_main_thread == hb_stackId() )
   {
      HB_VM_LOCK();

      --s_iRunningCount;
      if( hb_vmThreadRequest )
         hb_threadCondBroadcast( &s_vmCond );

      while( s_iStackCount > 1 )
         hb_threadCondWait( &s_vmCond, &s_vmMtx );

      ++s_iRunningCount;

      HB_VM_UNLOCK();
   }
}

void * hb_vmThreadState( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadState()" ) );

   return hb_stackId() ? hb_stackList() : NULL;
}

static void hb_vmStackAdd( PHB_THREADSTATE pState )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStackAdd(%p)", pState ) );

   if( ! pState->pPrev )
   {
      if( s_vmStackLst )
      {
         pState->pNext = s_vmStackLst;
         pState->pPrev = s_vmStackLst->pPrev;
         pState->pPrev->pNext = pState;
         s_vmStackLst->pPrev = pState;
      }
      else
      {
         s_vmStackLst = pState->pNext = pState->pPrev = pState;
      }
      s_iStackCount++;
   }
   if( pState->th_no == 0 )
      pState->th_no = ++s_threadNo;
}

static PHB_ITEM hb_vmStackDel( PHB_THREADSTATE pState, HB_BOOL fCounter )
{
   PHB_ITEM pThItm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStackDel(%p,%d)", pState, ( int ) fCounter ) );

   pState->fActive = HB_FALSE;
   pState->pStackId = NULL;
   pState->fFinished = HB_TRUE;

   if( pState->pPrev )
   {
      pState->pPrev->pNext = pState->pNext;
      pState->pNext->pPrev = pState->pPrev;
      if( s_vmStackLst == pState )
      {
         s_vmStackLst = pState->pNext;
         if( s_vmStackLst == pState )
            s_vmStackLst = NULL;
      }
      pState->pPrev = pState->pNext = NULL;
      if( fCounter )
         s_iStackCount--;
   }

   /* NOTE: releasing pThItm may force pState freeing if parent
    *       thread does not keep thread pointer item. So it's
    *       important to not access it later. [druzus]
    */
   pThItm = pState->pThItm;
   pState->pThItm = NULL;

   return pThItm;
}

static void hb_vmStackInit( PHB_THREADSTATE pState )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStackInit(%p)", pState ) );

   hb_stackInit();      /* initialize HVM thread stack */

   HB_VM_LOCK();
   {
      HB_STACK_TLS_PRELOAD

      hb_stackUnlock();
      pState->pStackId = hb_stackId();
      hb_stackListSet( ( void * ) ( pState ) );
      pState->fActive = HB_TRUE;
      hb_vmStackAdd( pState );
   }
   HB_VM_UNLOCK();

   hb_vmLock();
}

static void hb_vmStackRelease( void )
{
   HB_STACK_TLS_PRELOAD
   HB_BOOL fLocked;
   PHB_ITEM pThItm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStackRelease()" ) );

   HB_VM_LOCK();

   fLocked = hb_stackUnlock() == 1;
   pThItm = hb_vmStackDel( ( PHB_THREADSTATE ) hb_stackList(), HB_FALSE );

   HB_VM_UNLOCK();

   /* NOTE: releasing pThItm may force pState freeing if parent
    *       thread does not keep thread pointer item. So it's
    *       important to not access it later. [druzus]
    */
   if( pThItm )
      hb_itemRelease( pThItm );

   hb_setRelease( hb_stackSetStruct() );
   hb_stackFree();

   hb_threadMutexUnlockAll();

   HB_VM_LOCK();

   if( fLocked )
      s_iRunningCount--;

   s_iStackCount--;
   hb_threadCondBroadcast( &s_vmCond );

   HB_VM_UNLOCK();
}

HB_BOOL hb_vmThreadRegister( void * Cargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadRegister(%p)", Cargo ) );

   HB_VM_LOCK();

   hb_vmStackAdd( ( PHB_THREADSTATE ) Cargo );

   HB_VM_UNLOCK();

   return HB_TRUE;
}

void hb_vmThreadRelease( void * Cargo )
{
   PHB_ITEM pThItm;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadRelease(%p)", Cargo ) );

   HB_VM_LOCK();

   pThItm = hb_vmStackDel( ( PHB_THREADSTATE ) Cargo, HB_TRUE );
   hb_threadCondBroadcast( &s_vmCond );

   HB_VM_UNLOCK();

   if( pThItm )
      hb_itemRelease( pThItm );
}

/* thread entry point */
void hb_vmThreadInit( void * Cargo )
{
   PHB_THREADSTATE pState;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadInit(%p)", Cargo ) );

   pState = ( PHB_THREADSTATE ) Cargo;
   if( ! pState )
      pState = hb_threadStateNew();

   hb_vmStackInit( pState );  /* initialize HVM thread stack */
   {
      HB_STACK_TLS_PRELOAD

      hb_cdpSelectID( pState->pszCDP );
      hb_langSelectID( pState->pszLang );

      hb_vmSetI18N( pState->pI18N );
      pState->pI18N = NULL;

      if( pState->pSet )
      {
         /* TODO: add set sharing */
         memcpy( hb_stackSetStruct(), pState->pSet, sizeof( HB_SET_STRUCT ) );
         hb_xfree( pState->pSet );
         pState->pSet = NULL;
      }
      else
         hb_setInitialize( hb_stackSetStruct() );

      hb_gtAttach( pState->hGT );
      pState->hGT = NULL;

      if( pState->pszDefRDD )
         hb_stackRDD()->szDefaultRDD = pState->pszDefRDD;

      if( s_fHVMActive )
      {
         /* call __HBVMINIT function to initialize GETLIST public variable
          * and set ErrorBlock() by ErrorSys() function
          */
         hb_vmDoInitHVM();
      }

      if( pState->pMemvars )
      {
         hb_memvarRestoreFromArray( pState->pMemvars );
         hb_itemRelease( pState->pMemvars );
         pState->pMemvars = NULL;
      }
   }
}

/* thread leave point */
void hb_vmThreadQuit( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_THREADSTATE pState;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadQuit()" ) );

   hb_stackSetQuitState( HB_TRUE );
   hb_stackSetActionRequest( 0 );

   pState = ( PHB_THREADSTATE ) hb_stackList();
   {
      PHB_ITEM pReturn = hb_stackReturnItem();

      if( HB_IS_BYREF( pReturn ) )
         pReturn = hb_itemUnRef( pReturn );

      if( ! pState->pResult )
      {
         pState->pResult = hb_itemNew( pReturn );
         hb_gcUnlock( pState->pResult );
      }
      else
         hb_itemCopy( pState->pResult, pReturn );
   }
   hb_itemClear( hb_stackReturnItem() );

   hb_stackSetActionRequest( 0 );
   hb_rddCloseAll();             /* close all workareas */
   hb_stackRemove( 1 );          /* clear stack items, leave only initial symbol item */
   hb_memvarsClear( HB_TRUE );   /* clear all PUBLIC (and PRIVATE if any) variables */
   hb_vmSetI18N( NULL );         /* remove i18n translation table */
#ifndef HB_NO_DEBUG
   hb_vmDebuggerExit( HB_FALSE );   /* deactivate debugger */
#endif
   hb_gtRelease( NULL );
   hb_vmStackRelease();          /* release HVM stack and remove it from linked HVM stacks list */
}

/* send QUIT request to given thread */
void hb_vmThreadQuitRequest( void * Cargo )
{
   PHB_THREADSTATE pState;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadQuitRequest(%p)", Cargo ) );

   pState = ( PHB_THREADSTATE ) Cargo;

   HB_VM_LOCK();

   if( pState->pStackId && pState->fActive )
      hb_stackIdSetActionRequest( pState->pStackId, HB_QUIT_REQUESTED );

   HB_VM_UNLOCK();
}

#endif /* HB_MT_VM */

PHB_ITEM hb_vmThreadStart( HB_ULONG ulAttr, PHB_CARGO_FUNC pFunc, void * cargo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmThreadStart(%lu,%p,%p)", ulAttr, pFunc, cargo ) );

#if defined( HB_MT_VM )
   return hb_threadStart( ulAttr, pFunc, cargo );
#else
   HB_SYMBOL_UNUSED( ulAttr );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( cargo );
   return NULL;
#endif /* HB_MT_VM */
}

void hb_vmSetFunction( PHB_SYMB pOldSym, PHB_SYMB pNewSym )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;
   HB_SYMB SymOldBuf, SymNewBuf;

   /* make copy of symbols to eliminate possible problem with
    * dynamic modification of passed parameters inside the loop
    */
   memcpy( &SymOldBuf, pOldSym, sizeof( SymOldBuf ) );
   pOldSym = &SymOldBuf;
   memcpy( &SymNewBuf, pNewSym, sizeof( SymNewBuf ) );
   pNewSym = &SymNewBuf;

   while( pLastSymbols )
   {
      HB_USHORT ui, uiSymbols = pLastSymbols->uiModuleSymbols;

      for( ui = 0; ui < uiSymbols; ++ui )
      {
         PHB_SYMB pSym = pLastSymbols->pModuleSymbols + ui;

         if( pSym->value.pFunPtr == pOldSym->value.pFunPtr &&
             ( pSym->value.pFunPtr ||
               strcmp( pSym->szName, pOldSym->szName ) == 0 ) )
         {
            pSym->value.pFunPtr = pNewSym->value.pFunPtr;
            pSym->scope.value   = pNewSym->scope.value;
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

void hb_vmSetDynFunc( PHB_DYNS pDynSym )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      HB_USHORT ui, uiSymbols = pLastSymbols->uiModuleSymbols;

      for( ui = 0; ui < uiSymbols; ++ui )
      {
         PHB_SYMB pSym = pLastSymbols->pModuleSymbols + ui;

         if( pSym->pDynSym == pDynSym && pDynSym->pSymbol != pSym )
            pSym->scope.value |= HB_FS_DEFERRED;
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

/* application entry point */

void hb_vmInit( HB_BOOL bStartMainProc )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInit()" ) );

#if defined( HB_OS_WIN )
   hb_winmainArgVBuild();
#endif

   hb_xinit();

   hb_vmSetExceptionHandler();

   hb_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */

#if defined( HB_MT_VM )
   hb_threadInit();
   hb_vmStackInit( hb_threadStateNew() ); /* initialize HVM thread stack */
   s_pSymbolsMtx = hb_threadMutexCreate();
#else
   hb_stackInit();                        /* initialize HVM stack */
#endif /* HB_MT_VM */
   /* Set the language and codepage to the default */
   /* This trick is needed to stringify the macro value */
   hb_langSelectID( HB_MACRO2STRING( HB_LANG_DEFAULT ) );
   hb_cdpSelectID( HB_MACRO2STRING( HB_CODEPAGE_DEFAULT ) );
   {
      HB_STACK_TLS_PRELOAD
      s_main_thread = hb_stackId();
      /* _SET_* initialization */
      hb_setInitialize( hb_stackSetStruct() );
   }

   hb_cmdargUpdate();

   hb_clsInit();              /* initialize Classy/OO system */
   hb_errInit();

   /* initialize dynamic symbol for evaluating codeblocks */
   hb_symEval.pDynSym = hb_dynsymGetCase( hb_symEval.szName );

   hb_conInit();

   /* Check for some internal switches */
   s_VMFlags = hb_cmdargProcessVM( &s_VMCancelKey, &s_VMCancelKeyEx );
   hb_inkeySetCancelKeys( s_VMCancelKey, s_VMCancelKeyEx );

   hb_i18n_init();            /* initialize i18n module */

#ifndef HB_NO_PROFILER
   /* Initialize opcodes profiler support arrays */
   {
      HB_ULONG ul;

      for( ul = 0; ul < HB_P_LAST_PCODE; ul++ )
      {
         hb_ulOpcodesCalls[ ul ] = 0;
         hb_ulOpcodesTime[ ul ] = 0;
      }
   }
#endif

   /* enable executing PCODE (HVM reenter request) */
   s_fHVMActive = HB_TRUE;

#ifndef HB_NO_DEBUG
   s_pDynsDbgEntry = hb_dynsymFind( "__DBGENTRY" );
   if( s_pDynsDbgEntry )
   {
      /* Try to get C dbgEntry() function pointer */
      if( ! s_pFunDbgEntry )
         hb_vmDebugEntry( HB_DBG_GETENTRY, 0, NULL, 0, NULL );
      if( ! s_pFunDbgEntry )
         s_pFunDbgEntry = hb_vmDebugEntry;
   }
#endif

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables
    */
   hb_vmDoInitStatics();

   /* call __HBVMINIT function to initialize GETLIST public variable
    * and set ErrorBlock() by ErrorSys() function.
    */
   hb_vmDoInitHVM();

   hb_clsDoInit();                     /* initialize Class(y) .prg functions */

   hb_vmDoModuleInitFunctions();       /* process AtInit registered functions */
   hb_vmDoInitFunctions( HB_TRUE );    /* process registered CLIPINIT INIT procedures */
   hb_vmDoInitFunctions( HB_FALSE );   /* process registered other INIT procedures */

   /* Call __SetHelpK() function to redirect K_F1 to HELP() function
    * if it is linked. CA-Cl*pper calls it after INIT PROCEDURes and
    * before executing the application entry function.
    */
   hb_vmDoInitHelp();

   /* This is undocumented CA-Cl*pper, if there's a function called _APPMAIN
      it will be executed first. [vszakats] */
   {
      PHB_DYNS pDynSym = hb_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         s_pSymStart = pDynSym->pSymbol;
      else
      {
         /* if first char is '@' then start procedure were set by
          * programmer explicitly and should have the highest priority
          * otherwise it's the name of first public function in
          * first linked module which is used if there is no
          * HB_START_PROCEDURE in code
          */
         const char * pszMain;

         if( s_vm_pszLinkedMain && *s_vm_pszLinkedMain == '@' )
         {
            pszMain = s_vm_pszLinkedMain + 1;
            pDynSym = hb_dynsymFind( pszMain );
         }
         else
         {
#ifndef HB_START_PROCEDURE
            pszMain = NULL;
#else
            pszMain = HB_START_PROCEDURE;
            pDynSym = hb_dynsymFind( pszMain );
            if( ! ( pDynSym && pDynSym->pSymbol->value.pFunPtr ) )
#endif
            {
               if( s_vm_pszLinkedMain )
               {
                  pszMain = s_vm_pszLinkedMain;
                  pDynSym = hb_dynsymFind( pszMain );
               }
            }
         }

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            s_pSymStart = pDynSym->pSymbol;
#ifdef HB_START_PROC_STRICT
         else
            /* clear startup symbol set by initialization code */
            s_pSymStart = NULL;
#endif

#ifndef HB_CLP_STRICT
         if( bStartMainProc && ! s_pSymStart )
         {
            if( pszMain )
               hb_errInternal( HB_EI_VMBADSTARTUP, NULL, pszMain, NULL );
            else
               hb_errInternal( HB_EI_VMNOSTARTUP, NULL, NULL, NULL );
         }
#endif
      }
   }

   if( bStartMainProc && s_pSymStart )
   {
      hb_vmPushSymbol( s_pSymStart ); /* pushes first HB_FS_PUBLIC defined symbol to the stack */
      hb_vmPushNil();                 /* places NIL at self */
      hb_vmProc( ( HB_USHORT ) hb_cmdargPushArgs() ); /* invoke it with number of supplied parameters */
   }
}

int hb_vmQuit( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmQuit()" ) );

#if defined( HB_MT_VM )
   hb_vmTerminateThreads();
#endif

   hb_vmDoExitFunctions();          /* process defined EXIT functions */
   hb_vmDoModuleExitFunctions();    /* process AtExit registered functions */

   /* release all known items stored in subsystems */
   hb_itemClear( hb_stackReturnItem() );
   hb_stackRemove( 1 );          /* clear stack items, leave only initial symbol item */

   /* intentionally here to allow executing object destructors for all
    * cross referenced items before we release classy subsystem
    */
   hb_gcCollectAll( HB_TRUE );

   /* Clear any pending actions so RDD shutdown process
    * can be cleanly executed
    */
   hb_stackSetActionRequest( 0 );
   hb_rddCloseAll();             /* close all workareas */
   hb_rddShutDown();             /* remove all registered RDD drivers */
   hb_memvarsClear( HB_TRUE );   /* clear all PUBLIC (and PRIVATE if any) variables */
   hb_vmSetI18N( NULL );         /* remove i18n translation table */
   hb_i18n_exit();               /* unregister i18n module */

   hb_itemClear( hb_stackReturnItem() );
   hb_gcCollectAll( HB_TRUE );
#ifndef HB_NO_DEBUG
   /* deactivate debugger */
   hb_vmDebuggerExit( HB_TRUE );
#endif

   /* release thread specific data */
   hb_stackDestroyTSD();

   /* stop executing PCODE (HVM reenter request) */
   s_fHVMActive = HB_FALSE;

   hb_vmStaticsClear();

   hb_errExit();
   hb_clsReleaseAll();

   hb_vmStaticsRelease();

   /* release all remaining items */

   hb_conRelease();                 /* releases Console */
   hb_vmReleaseLocalSymbols();      /* releases the local modules linked list */
   hb_dynsymRelease();              /* releases the dynamic symbol table */
   hb_itemClear( hb_stackReturnItem() );
   hb_gcCollectAll( HB_TRUE );

   hb_vmDoModuleQuitFunctions();    /* process AtQuit registered functions */
   hb_vmCleanModuleFunctions();

#if defined( HB_MT_VM )
   hb_vmStackRelease();             /* release HVM stack and remove it from linked HVM stacks list */
   if( s_pSymbolsMtx )
   {
      hb_itemRelease( s_pSymbolsMtx );
      s_pSymbolsMtx = NULL;
   }
   hb_threadExit();
#else
   hb_setRelease( hb_stackSetStruct() );  /* releases Sets */
   hb_stackFree();
#endif /* HB_MT_VM */

   hb_langReleaseAll();             /* release lang modules */
   hb_cdpReleaseAll();              /* releases codepages */

   /* release all known garbage */
   if( hb_xquery( HB_MEM_USEDMAX ) == 0 ) /* check if fmstat is ON */
      hb_gcReleaseAll();

   hb_vmUnsetExceptionHandler();

   hb_xexit();

#if defined( HB_OS_WIN )
   hb_winmainArgVFree();
#endif

   return s_nErrorLevel;
}

void hb_vmExecute( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_STACK_TLS_PRELOAD
   HB_BOOL bCanRecover = HB_FALSE;
   HB_BOOL bDynCode = pSymbols == NULL || ( pSymbols->scope.value & HB_FS_DYNCODE ) != 0;

#ifndef HB_NO_PROFILER
   HB_ULONG ulLastOpcode = 0; /* opcodes profiler support */
   HB_ULONG ulPastClock = 0;  /* opcodes profiler support */
#endif
#if ! defined( HB_GUI )
   int * piKeyPolls = hb_stackKeyPolls();
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmExecute(%p, %p)", pCode, pSymbols ) );

#ifndef HB_NO_PROFILER
   if( hb_bProfiler )
      ulPastClock = ( HB_ULONG ) clock();
#endif

   for( ;; )
   {
#ifndef HB_NO_PROFILER
      if( hb_bProfiler )
      {
         HB_ULONG ulActualClock = ( HB_ULONG ) clock();

         hb_ulOpcodesTime[ ulLastOpcode ] += ( ulActualClock - ulPastClock );
         ulPastClock = ulActualClock;
         ulLastOpcode = pCode[ 0 ];
         hb_ulOpcodesCalls[ ulLastOpcode ]++;
      }
#endif

#if ! defined( HB_GUI )
      if( ! --( *piKeyPolls ) )
      {
         hb_inkeyPoll();
         *piKeyPolls = 65536;

         /* IMHO we should have a _SET_ controlled by user
          * sth like:

         if( hb_stackSetStruct()->HB_SET_KEYPOLL )
         {
            hb_inkeyPoll();
            *piKeyPolls = hb_stackSetStruct()->HB_SET_KEYPOLL;
         }

         for some GTs which can work in assynchrous mode user may
         set it to 0 (or if he doesn't need any inkey poll) and
         when ALT+C/ALT+D is pressed (or any other platform dependent
         key combination) they should set proper flags in
         ActionRequest so we can serve it in main VM loop without
         performance decrease or ignore depending on
         hb_stackSetStruct()->HB_SET_CANCEL,
         hb_stackSetStruct()->HB_SET_DEBUG flags
         */
      }
#endif
#if defined( HB_MT_VM )
      if( hb_vmThreadRequest )
         hb_vmRequestTest();
#endif

      switch( pCode[ 0 ] )
      {
         /* Operators ( mathematical / character / misc ) */

         case HB_P_NEGATE:
            hb_vmNegate();
            pCode++;
            break;

         case HB_P_PLUS:
            hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_PLUSEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmPlus( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_PLUSEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmPlus( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_MINUS:
            hb_vmMinus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_MINUSEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmMinus( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_MINUSEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmMinus( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_MULT:
            hb_vmMult( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_MULTEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmMult( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_MULTEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmMult( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_DIVIDE:
            hb_vmDivide( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_DIVEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmDivide( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_DIVEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmDivide( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_MODULUS:
            hb_vmModulus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_MODEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmModulus( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_MODEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmModulus( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_POWER:
            hb_vmPower( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_EXPEQ:
            {
               PHB_ITEM pResult, pValue;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               pValue = hb_stackItemFromTop( -1 );
               hb_vmPower( pResult, pResult, pValue );
               hb_itemCopy( pValue, pResult );
               hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
               hb_stackDec();
            }
            pCode++;
            break;

         case HB_P_EXPEQPOP:
            {
               PHB_ITEM pResult;
               pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
               hb_vmPower( pResult, pResult, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               hb_stackPop();
            }
            pCode++;
            break;

         case HB_P_INC:
            hb_vmInc( hb_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case HB_P_INCEQ:
            {
               PHB_ITEM pResult, pValue, pTemp;
               pResult = hb_stackItemFromTop( -1 );
               pValue = hb_itemUnRef( pResult );
               hb_vmInc( pValue );
               pTemp = hb_stackAllocItem();
               hb_itemCopy( pTemp, pValue );
               hb_itemMove( pResult, pTemp );
               hb_stackDec();
               pCode++;
            }
            break;

         case HB_P_INCEQPOP:
            hb_vmInc( hb_itemUnRef( hb_stackItemFromTop( -1 ) ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_DEC:
            hb_vmDec( hb_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case HB_P_DECEQ:
            {
               PHB_ITEM pResult, pValue, pTemp;
               pResult = hb_stackItemFromTop( -1 );
               pValue = hb_itemUnRef( pResult );
               hb_vmDec( pValue );
               pTemp = hb_stackAllocItem();
               hb_itemCopy( pTemp, pValue );
               hb_itemMove( pResult, pTemp );
               hb_stackDec();
               pCode++;
            }
            break;

         case HB_P_DECEQPOP:
            hb_vmDec( hb_itemUnRef( hb_stackItemFromTop( -1 ) ) );
            hb_stackPop();
            pCode++;
            break;

         case HB_P_FUNCPTR:
            hb_vmFuncPtr();
            pCode++;
            break;

         /* Operators (relational) */

         case HB_P_EQUAL:
            hb_vmEqual();
            pCode++;
            break;

         case HB_P_EXACTLYEQUAL:
            hb_vmExactlyEqual();
            pCode++;
            break;

         case HB_P_NOTEQUAL:
            hb_vmNotEqual();
            pCode++;
            break;

         case HB_P_LESS:
            hb_vmLess();
            pCode++;
            break;

         case HB_P_LESSEQUAL:
            hb_vmLessEqual();
            pCode++;
            break;

         case HB_P_GREATER:
            hb_vmGreater();
            pCode++;
            break;

         case HB_P_GREATEREQUAL:
            hb_vmGreaterEqual();
            pCode++;
            break;

         case HB_P_INSTRING:
            hb_vmInstring();
            pCode++;
            break;

         case HB_P_FORTEST:
            hb_vmForTest();
            pCode++;
            break;

         case HB_P_ENUMSTART:
            hb_vmEnumStart( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case HB_P_ENUMNEXT:
            hb_vmEnumNext();
            pCode++;
            break;

         case HB_P_ENUMPREV:
            hb_vmEnumPrev();
            pCode++;
            break;

         case HB_P_ENUMEND:
            hb_vmEnumEnd();
            pCode++;
            break;

         case HB_P_SWITCH:
            pCode = hb_vmSwitch( pCode + 3, HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            break;

         /* Operators (logical) */

         case HB_P_NOT:
            hb_vmNot();
            pCode++;
            break;

         case HB_P_AND:
            hb_vmAnd();
            pCode++;
            break;

         case HB_P_OR:
            hb_vmOr();
            pCode++;
            break;

         /* Array */

         case HB_P_ARRAYPUSH:
            hb_vmArrayPush();
            pCode++;
            break;

         case HB_P_ARRAYPUSHREF:
            hb_vmArrayPushRef();
            pCode++;
            break;

         case HB_P_ARRAYPOP:
            hb_vmArrayPop();
            pCode++;
            break;

         case HB_P_ARRAYDIM:
            hb_vmArrayDim( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_ARRAYGEN:
            hb_vmArrayGen( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_HASHGEN:
            hb_vmHashGen( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         /* Object */

         case HB_P_MESSAGE:
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         /* Database */

         case HB_P_SWAPALIAS:
            hb_vmSwapAlias();
            pCode++;
            break;

         /* Execution */

         case HB_P_DO:
            hb_vmProc( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_DOSHORT:
            hb_vmProc( pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_FUNCTION:
            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmProc( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            hb_stackPushReturn();
            pCode += 3;
            break;

         case HB_P_FUNCTIONSHORT:
            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmProc( pCode[ 1 ] );
            hb_stackPushReturn();
            pCode += 2;
            break;

         case HB_P_SEND:
            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmSend( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;

            /* Small opt */
            if( pCode[ 0 ] == HB_P_POP )
               pCode++;
            else
               hb_stackPushReturn();
            break;

         case HB_P_SENDSHORT:
            hb_itemSetNil( hb_stackReturnItem() );
            hb_vmSend( pCode[ 1 ] );
            pCode += 2;

            /* Small opt */
            if( pCode[ 0 ] == HB_P_POP )
               pCode++;
            else
               hb_stackPushReturn();
            break;

         case HB_P_PUSHOVARREF:
            hb_vmPushObjectVarRef();
            pCode++;
            break;

         case HB_P_LINE:
            HB_TRACE( HB_TR_INFO, ( "Opcode: HB_P_LINE: %s (%i)",
                                    hb_stackBaseItem()->item.asSymbol.value->szName,
                                    hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo ) );

            hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
#ifndef HB_NO_DEBUG
            if( hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
               hb_vmDebuggerShowLine( hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo );
#endif
            pCode += 3;
            break;

         case HB_P_PARAMETER:
            hb_memvarNewParameter( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ), hb_stackItemFromBase( pCode[ 3 ] ) );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPopParameter)" ) );
            pCode += 4;
            break;

         case HB_P_FRAME:
            hb_vmFrame( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case HB_P_VFRAME:
            hb_vmVFrame( ( unsigned char ) pCode[ 1 ], ( unsigned char ) pCode[ 2 ] );
            pCode += 3;
            break;

         case HB_P_LARGEFRAME:
            hb_vmFrame( HB_PCODE_MKUSHORT( &pCode[ 1 ] ), ( unsigned char ) pCode[ 3 ] );
            pCode += 4;
            break;

         case HB_P_LARGEVFRAME:
            hb_vmVFrame( HB_PCODE_MKUSHORT( &pCode[ 1 ] ), ( unsigned char ) pCode[ 3 ] );
            pCode += 4;
            break;

         case HB_P_SFRAME:
            hb_vmSFrame( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_STATICS:
            hb_vmStatics( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ), HB_PCODE_MKUSHORT( &pCode[ 3 ] ) );
            pCode += 5;
            break;

         case HB_P_THREADSTATICS:
         {
            HB_USHORT uiCount = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            hb_vmInitThreadStatics( uiCount, &pCode[ 3 ] );
            pCode += 3 + ( ( HB_ULONG ) uiCount << 1 );
            break;
         }

         case HB_P_LOCALNAME:
#ifndef HB_NO_DEBUG
            hb_vmLocalName( HB_PCODE_MKUSHORT( &pCode[ 1 ] ),
                            ( const char * ) pCode + 3 );
#endif
            pCode += 3;
            while( *pCode++ )
               ;
            break;

         case HB_P_STATICNAME:
#ifndef HB_NO_DEBUG
            hb_vmStaticName( pCode[ 1 ], HB_PCODE_MKUSHORT( &pCode[ 2 ] ),
                             ( const char * ) pCode + 4 );
#endif
            pCode += 4;
            while( *pCode++ )
               ;
            break;

         case HB_P_MODULENAME:
#ifndef HB_NO_DEBUG
            hb_vmModuleName( ( const char * ) pCode + 1 );
#endif
            pCode++;
            while( *pCode++ )
               ;
            break;

         case HB_P_RETVALUE:
            hb_stackPopReturn();
            hb_stackReturnItem()->type &= ~HB_IT_MEMOFLAG;
            pCode++;
            break;

         case HB_P_ENDBLOCK:
            HB_TRACE( HB_TR_INFO, ( "HB_P_ENDBLOCK" ) );
            hb_stackPopReturn();
            /* manually inlined hb_vmRequestEndProc() for some C compilers
             * which does not make such optimisation
             */
            hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
            break;

         case HB_P_ENDPROC:
            HB_TRACE( HB_TR_INFO, ( "HB_P_ENDPROC" ) );
            /* manually inlined hb_vmRequestEndProc() for some C compilers
             * which does not make such optimisation
             */
            hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
            break;

         /* BEGIN SEQUENCE/RECOVER/ALWAYS/END SEQUENCE */

         case HB_P_SEQBLOCK:
            hb_vmSeqBlock();
            pCode++;
            break;

         case HB_P_SEQALWAYS:
         {
            /*
             * Create the SEQUENCE envelope
             * [ break return value ]  -2
             * [ recover envelope   ]  -1
             * [                    ] <- new recover base
             */

            PHB_ITEM pItem;

            /*
             * 1) clear the storage for value returned by BREAK statement
             */
            hb_stackAllocItem()->type = HB_IT_NIL;

            /*
             * 2) recover data
             */
            pItem = hb_stackAllocItem();
            /* mark type as NIL - it's not real item */
            pItem->type = HB_IT_RECOVER;
            /* store the address of RECOVER or END opcode */
            pItem->item.asRecover.recover = pCode + HB_PCODE_MKINT24( &pCode[ 1 ] );
            /* store current RECOVER base */
            pItem->item.asRecover.base = hb_stackGetRecoverBase();
            /* store current bCanRecover flag - in a case of nested sequences */
            pItem->item.asRecover.flags = HB_SEQ_DOALWAYS | ( bCanRecover ? HB_SEQ_CANRECOVER : 0 );
            /* clear new recovery state */
            pItem->item.asRecover.request = 0;

            /*
             * set new recover base
             */
            hb_stackSetRecoverBase( hb_stackTopOffset() );
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = HB_TRUE;
            pCode += 4;
            break;
         }

         case HB_P_ALWAYSBEGIN:
#if defined( _HB_RECOVER_DEBUG )
            if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
               hb_errInternal( HB_EI_ERRUNRECOV, "HB_P_ALWAYSBEGIN", NULL, NULL );
#endif
            /* change the recover address to ALWAYSEND opcode */
            hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.recover =
               pCode + HB_PCODE_MKINT24( &pCode[ 1 ] );
            /* store and reset action */
            hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags |=
               hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request;
            hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request = 0;
            /* store RETURN value */
            if( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_ENDPROC_REQUESTED )
               hb_itemMove( hb_stackItemFromTop( HB_RECOVER_VALUE ), hb_stackReturnItem() );
            pCode += 4;
            break;

         case HB_P_ALWAYSEND:
         {
            HB_USHORT uiPrevAction, uiCurrAction;

#if defined( _HB_RECOVER_DEBUG )
            if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
               hb_errInternal( HB_EI_ERRUNRECOV, "HB_P_ALWAYSEND", NULL, NULL );
#endif
            uiPrevAction = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags;
            uiCurrAction = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request;

            /* restore previous recovery base */
            bCanRecover = ( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_CANRECOVER ) != 0;
            hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );

            /* restore requested action */
            if( ( uiCurrAction | uiPrevAction ) & HB_QUIT_REQUESTED )
               hb_stackSetActionRequest( HB_QUIT_REQUESTED );
            else if( ( uiCurrAction | uiPrevAction ) & HB_BREAK_REQUESTED )
               hb_stackSetActionRequest( HB_BREAK_REQUESTED );
            else if( ( uiCurrAction | uiPrevAction ) & HB_ENDPROC_REQUESTED )
               hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
            else
               hb_stackSetActionRequest( 0 );

            /* Remove the ALWAYS envelope */
            hb_stackDec();

            /* restore RETURN value if not overloaded inside ALWAYS code */
            if( ! ( uiCurrAction & HB_ENDPROC_REQUESTED ) &&
                  ( uiPrevAction & HB_ENDPROC_REQUESTED ) )
               hb_stackPopReturn();
            else
               hb_stackPop();
            pCode++;
            break;
         }

         case HB_P_SEQBEGIN:
         {
            /*
             * Create the SEQUENCE envelope
             * [ break return value ]  -2
             * [ recover envelope   ]  -1
             * [                    ] <- new recover base
             */

            PHB_ITEM pItem;

            /*
             * 1) clear the storage for value returned by BREAK statement
             */
            hb_stackAllocItem()->type = HB_IT_NIL;

            /*
             * 2) recover data
             */
            pItem = hb_stackAllocItem();
            /* mark type as NIL - it's not real item */
            pItem->type = HB_IT_RECOVER;
            /* store the address of RECOVER or END opcode */
            pItem->item.asRecover.recover = pCode + HB_PCODE_MKINT24( &pCode[ 1 ] );
            /* store current RECOVER base */
            pItem->item.asRecover.base = hb_stackGetRecoverBase();
            /* store current bCanRecover flag - in a case of nested sequences */
            pItem->item.asRecover.flags = bCanRecover ? HB_SEQ_CANRECOVER : 0;
            /* clear new recovery state */
            pItem->item.asRecover.request = 0;

            /*
             * set new recover base
             */
            hb_stackSetRecoverBase( hb_stackTopOffset() );
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = HB_TRUE;
            pCode += 4;
            break;
         }

         case HB_P_SEQEND:
            /*
             * Remove the SEQUENCE envelope
             * This is executed either at the end of sequence or as the
             * response to the break statement if there is no RECOVER clause
             */
#if defined( _HB_RECOVER_DEBUG )
            if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
               hb_errInternal( HB_EI_ERRUNRECOV, "HB_P_SEQEND", NULL, NULL );
#endif
            /*
             * 2) Restore previous recovery state
             */
            bCanRecover = ( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_CANRECOVER ) != 0;
            hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
            hb_stackDec();

            /*
             * 1) Discard the value returned by BREAK statement - there
             * was no RECOVER clause or there was no BREAK statement
             */
            hb_stackPop();
            /*
             * skip outside of SEQUENCE structure
             */
            pCode += HB_PCODE_MKINT24( &pCode[ 1 ] );
            break;

         case HB_P_SEQRECOVER:
            /*
             * Execute the RECOVER code
             */
#if defined( _HB_RECOVER_DEBUG )
            if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
               hb_errInternal( HB_EI_ERRUNRECOV, "HB_P_SEQRECOVER", NULL, NULL );
#endif
            /*
             * 2) Restore previous recovery state
             */
            bCanRecover = ( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_CANRECOVER ) != 0;
            hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
            hb_stackDec();
            /*
             * 1) Leave the value returned from BREAK  - it will be popped
             * in next executed opcode
             */
            pCode++;
            break;

         /* Jumps */

         case HB_P_JUMPNEAR:
            pCode += ( signed char ) pCode[ 1 ];
            break;

         case HB_P_JUMP:
            pCode += HB_PCODE_MKSHORT( &pCode[ 1 ] );
            break;

         case HB_P_JUMPFAR:
            pCode += HB_PCODE_MKINT24( &pCode[ 1 ] );
            break;

         case HB_P_JUMPFALSENEAR:
            if( ! hb_vmPopLogical() )
               pCode += ( signed char ) pCode[ 1 ];
            else
               pCode += 2;
            break;

         case HB_P_JUMPFALSE:
            if( ! hb_vmPopLogical() )
               pCode += HB_PCODE_MKSHORT( &pCode[ 1 ] );
            else
               pCode += 3;
            break;

         case HB_P_JUMPFALSEFAR:
            if( ! hb_vmPopLogical() )
               pCode += HB_PCODE_MKINT24( &pCode[ 1 ] );
            else
               pCode += 4;
            break;

         case HB_P_JUMPTRUENEAR:
            if( hb_vmPopLogical() )
               pCode += ( signed char ) pCode[ 1 ];
            else
               pCode += 2;
            break;

         case HB_P_JUMPTRUE:
            if( hb_vmPopLogical() )
               pCode += HB_PCODE_MKSHORT( &pCode[ 1 ] );
            else
               pCode += 3;
            break;

         case HB_P_JUMPTRUEFAR:
            if( hb_vmPopLogical() )
               pCode += HB_PCODE_MKINT24( &pCode[ 1 ] );
            else
               pCode += 4;
            break;

         /* Push */

         case HB_P_TRUE:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_LOGICAL;
               pItem->item.asLogical.value = HB_TRUE;
               pCode++;
            }
            break;

         case HB_P_FALSE:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_LOGICAL;
               pItem->item.asLogical.value = HB_FALSE;
               pCode++;
            }
            break;

         case HB_P_ONE:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_INTEGER;
               pItem->item.asInteger.value = 1;
               pItem->item.asInteger.length = 10;
               HB_TRACE( HB_TR_INFO, ( "(HB_P_ONE)" ) );
               pCode++;
            }
            break;

         case HB_P_ZERO:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_INTEGER;
               pItem->item.asInteger.value = 0;
               pItem->item.asInteger.length = 10;
               HB_TRACE( HB_TR_INFO, ( "(HB_P_ZERO)" ) );
               pCode++;
            }
            break;

         case HB_P_PUSHNIL:
            hb_stackAllocItem()->type = HB_IT_NIL;
            HB_TRACE( HB_TR_INFO, ( "(HB_P_PUSHNIL)" ) );
            pCode++;
            break;

         case HB_P_PUSHBYTE:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_INTEGER;
               pItem->item.asInteger.value = ( signed char ) pCode[ 1 ];
               pItem->item.asInteger.length = 10;
               HB_TRACE( HB_TR_INFO, ( "(HB_P_PUSHBYTE)" ) );
               pCode += 2;
            }
            break;

         case HB_P_PUSHINT:
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_INTEGER;
               pItem->item.asInteger.value = HB_PCODE_MKSHORT( &pCode[ 1 ] );
               pItem->item.asInteger.length = 10;
               HB_TRACE( HB_TR_INFO, ( "(HB_P_PUSHINT)" ) );
               pCode += 3;
            }
            break;

         case HB_P_PUSHLONG:
            HB_TRACE( HB_TR_DEBUG, ( "(HB_P_PUSHLONG)" ) );
#if HB_VMINT_MAX >= INT32_MAX
            hb_vmPushIntegerConst( ( int ) HB_PCODE_MKLONG( &pCode[ 1 ] ) );
#else
            hb_vmPushLongConst( ( long ) HB_PCODE_MKLONG( &pCode[ 1 ] ) );
#endif
            pCode += 5;
            break;

         case HB_P_PUSHLONGLONG:
            HB_TRACE( HB_TR_DEBUG, ( "(HB_P_PUSHLONGLONG)" ) );
#if ! defined( HB_LONG_LONG_OFF )
            hb_vmPushLongLongConst( HB_PCODE_MKLONGLONG( &pCode[ 1 ] ) );
#else
            hb_vmPushDoubleConst( HB_PCODE_MKLONGLONG( &pCode[ 1 ] ),
                                  HB_DEFAULT_WIDTH, HB_DEFAULT_DECIMALS );
#endif
            pCode += 9;

            break;

         case HB_P_PUSHDOUBLE:
            hb_vmPushDoubleConst( HB_PCODE_MKDOUBLE( &pCode[ 1 ] ),
                                  ( int ) *( const unsigned char * ) &pCode[ 1 + sizeof( double ) ],
                                  ( int ) *( const unsigned char * ) &pCode[ 2 + sizeof( double ) ] );
            pCode += 3 + sizeof( double );
            break;

         case HB_P_PUSHSTRSHORT:
            if( bDynCode )
               hb_vmPushString( ( const char * ) pCode + 2, ( HB_SIZE ) pCode[ 1 ] - 1 );
            else
               hb_vmPushStringPcode( ( const char * ) pCode + 2, ( HB_SIZE ) pCode[ 1 ] - 1 );
            pCode += 2 + pCode[ 1 ];
            break;

         case HB_P_PUSHSTR:
         {
            HB_USHORT uiSize = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            if( bDynCode )
               hb_vmPushString( ( const char * ) pCode + 3, uiSize - 1 );
            else
               hb_vmPushStringPcode( ( const char * ) pCode + 3, uiSize - 1 );
            pCode += 3 + uiSize;
            break;
         }

         case HB_P_PUSHSTRLARGE:
         {
            HB_SIZE nSize = HB_PCODE_MKUINT24( &pCode[ 1 ] );
            if( bDynCode )
               hb_vmPushString( ( const char * ) pCode + 4, nSize - 1 );
            else
               hb_vmPushStringPcode( ( const char * ) pCode + 4, nSize - 1 );
            pCode += 4 + nSize;
            break;
         }

         case HB_P_PUSHSTRHIDDEN:
         {
            HB_SIZE nSize = ( HB_SIZE ) HB_PCODE_MKUSHORT( &pCode[ 2 ] );
            char * szText = hb_compDecodeString( pCode[ 1 ], ( const char * ) pCode + 4, &nSize );
            hb_itemPutCLPtr( hb_stackAllocItem(), szText, nSize );
            pCode += ( 4 + nSize );
            break;
         }

         case HB_P_PUSHDATE:
            HB_TRACE( HB_TR_DEBUG, ( "(HB_P_PUSHDATE)" ) );
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_DATE;
               pItem->item.asDateTime.julian = ( long ) HB_PCODE_MKLONG( &pCode[ 1 ] );
               pItem->item.asDateTime.time = 0;
               pCode += 5;
            }
            break;

         case HB_P_PUSHTIMESTAMP:
            HB_TRACE( HB_TR_DEBUG, ( "(HB_P_PUSHTIMESTAMP)" ) );
            {
               PHB_ITEM pItem = hb_stackAllocItem();

               pItem->type = HB_IT_TIMESTAMP;
               pItem->item.asDateTime.julian = ( long ) HB_PCODE_MKLONG( &pCode[ 1 ] );;
               pItem->item.asDateTime.time = ( long ) HB_PCODE_MKLONG( &pCode[ 5 ] );;
               pCode += 9;
            }
            break;

         case HB_P_PUSHBLOCK:
         {
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            HB_SIZE nSize = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            hb_vmPushBlock( pCode + 3, pSymbols, bDynCode ? nSize - 7 : 0 );
            pCode += nSize;
            break;
         }
         case HB_P_PUSHBLOCKLARGE:
         {
            /* +0       -> _pushblock
             * +1 +2 +3 -> size of codeblock
             * +4 +5    -> number of expected parameters
             * +6 +7    -> number of referenced local variables
             * +8       -> start of table with referenced local variables
             */
            HB_SIZE nSize = HB_PCODE_MKUINT24( &pCode[ 1 ] );
            hb_vmPushBlock( pCode + 4, pSymbols, bDynCode ? nSize - 8 : 0 );
            pCode += nSize;
            break;
         }
         case HB_P_PUSHBLOCKSHORT:
         {
            /* +0    -> _pushblock
             * +1    -> size of codeblock
             */
            HB_SIZE nSize = pCode[ 1 ];
            hb_vmPushBlockShort( pCode + 2, pSymbols, bDynCode ? nSize - 2 : 0 );
            pCode += nSize;
            break;
         }

         case HB_P_PUSHSELF:
            hb_vmPush( hb_stackSelfItem() );
            pCode++;
            break;

         case HB_P_PUSHSYM:
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHSYMNEAR:
            hb_vmPushSymbol( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_PUSHFUNCSYM:
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            hb_stackAllocItem()->type = HB_IT_NIL;
            pCode += 3;
            break;

         case HB_P_PUSHALIAS:
            hb_vmPushAlias();
            pCode++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            hb_vmPushAliasedField( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHALIASEDFIELDNEAR:
            hb_vmPushAliasedField( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_PUSHALIASEDVAR:
            hb_vmPushAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHFIELD:
            /* It pushes the current value of the given field onto the eval stack
             */
            hb_rddGetFieldValue( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPushField)" ) );
            pCode += 3;
            break;

         case HB_P_PUSHLOCAL:
            hb_vmPushLocal( HB_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHLOCALNEAR:
            hb_vmPushLocal( ( signed char ) pCode[ 1 ] );
            pCode += 2;  /* only first two bytes are used */
            break;

         case HB_P_PUSHLOCALREF:
            hb_vmPushLocalByRef( HB_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHSTATIC:
            hb_vmPushStatic( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHSTATICREF:
            hb_vmPushStaticByRef( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_PUSHMEMVAR:
            hb_memvarGetValue( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPushMemvar)" ) );
            pCode += 3;
            break;

         case HB_P_PUSHMEMVARREF:
            hb_memvarGetRefer( hb_stackAllocItem(), pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPushMemvarRef)" ) );
            pCode += 3;
            break;

         case HB_P_PUSHVARIABLE:
            /* Push a value of variable of unknown type onto the eval stack
             */
            hb_vmPushVariable( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_DUPLICATE:
            hb_vmDuplicate();
            pCode++;
            break;

         case HB_P_DUPLUNREF:
            hb_vmDuplUnRef();
            pCode++;
            break;

         case HB_P_PUSHUNREF:
            hb_vmPushUnRef();
            pCode++;
            break;

         case HB_P_PUSHVPARAMS:
            hb_vmPushVParams();
            pCode++;
            break;

         case HB_P_PUSHAPARAMS:
            hb_vmPushAParams();
            pCode++;
            break;

         case HB_P_SWAP:
            hb_vmSwap( ( unsigned char ) pCode[ 1 ] );
            pCode += 2;
            break;

         /* Pop */

         case HB_P_POP:
            hb_stackPop();
            pCode++;
            break;

         case HB_P_POPALIAS:
            hb_vmPopAlias();
            pCode++;
            break;

         case HB_P_POPALIASEDFIELD:
            hb_vmPopAliasedField( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_POPALIASEDFIELDNEAR:
            hb_vmPopAliasedField( pSymbols + pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_POPALIASEDVAR:
            hb_vmPopAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_POPFIELD:
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            hb_stackPop();
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPopField)" ) );
            pCode += 3;
            break;

         case HB_P_POPLOCAL:
            hb_vmPopLocal( HB_PCODE_MKSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_POPLOCALNEAR:
            hb_vmPopLocal( ( signed char ) pCode[ 1 ] );
            pCode += 2;  /* only first two bytes are used */
            break;

         case HB_P_POPSTATIC:
            hb_vmPopStatic( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_POPMEMVAR:
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ),
                               hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPopMemvar)" ) );
            pCode += 3;
            break;

         case HB_P_POPVARIABLE:
         {
            /*
               2004-03-19 Ron Pinkas
               Test with Clipper shows that for assignment, MEMVAR context
               is always used even if MEMVAR does NOT exists, and a FIELD
               with this name exists!!!
               Here is the Test Ueed - Clipper produced NO R/T Error -
               indicating MEMVAR was created.
                 PROCEDURE Main()
                    USE Test
                    First := First
                    CLOSE
                    ? First
                 RETURN
             */
#if 0
            /* Pops a value from the eval stack and uses it to set
             * a new value of a variable of unknown type.
             */
            PHB_SYMB pSymbol = pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] );

            if( pSymbol->pDynSym && hb_dynsymGetMemvar( pSymbol->pDynSym ) )
               /* If exist a memory symbol with this name use it */
               hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
            else if( hb_rddFieldPut( hb_stackItemFromTop( -1 ), pSymbol ) == HB_FAILURE )
               /* Try with a field and after create a memvar */
               hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
#else
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &pCode[ 1 ] ),
                               hb_stackItemFromTop( -1 ) );
#endif
            hb_stackPop();
            HB_TRACE( HB_TR_INFO, ( "(hb_vmPopVariable)" ) );
            pCode += 3;
            break;
         }

         /* macro creation */

         case HB_P_MACROPOP:
            /* compile and run - pop a value from the stack */
            hb_macroSetValue( hb_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPOPALIASED:
            /* compile and run - pop an aliased variable from the stack */
            hb_macroPopAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPUSH:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPUSHLIST:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPUSHINDEX:
            hb_vmMacroPushIndex();
            pCode++;
            break;

         case HB_P_MACROARRAYGEN:
            hb_vmMacroArrayGen( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_MACRODO:
            hb_vmMacroDo( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_MACROFUNC:
            hb_vmMacroFunc( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_MACROSEND:
            hb_vmMacroSend( HB_PCODE_MKUSHORT( &pCode[ 1 ] ) );
            pCode += 3;
            break;

         case HB_P_MACROPUSHPARE:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPUSHALIASED:
            /* compile and run - leave an aliased variable on the stack */
            hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), pCode[ 1 ] );
            pCode += 2;
            break;

         case HB_P_MACROPUSHREF:
            {
               PHB_ITEM pMacro = hb_stackItemFromTop( -1 );
               hb_macroPushReference( pMacro );
               pCode++;
            }
            break;

         case HB_P_MACROSYMBOL:
            /* compile into a symbol name (used in function calls) */
            hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );
            pCode++;
            break;

         case HB_P_MACROTEXT:
            /* macro text substitution
             * "text &macro.other text"
             */
            hb_macroTextValue( hb_stackItemFromTop( -1 ) );
            pCode++;
            break;

         /* macro compiled opcodes - we are using symbol address here */

         case HB_P_MMESSAGE:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDFIELD:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPopAliasedField( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDVAR:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPopAliasedVar( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPOPFIELD:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            hb_rddPutFieldValue( ( hb_stackItemFromTop( -1 ) ), pDynSym->pSymbol );
            hb_stackPop();
            HB_TRACE( HB_TR_INFO, ( "(hb_vmMPopField)" ) );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPOPMEMVAR:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_memvarSetValue( pDynSym->pSymbol, hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            HB_TRACE( HB_TR_INFO, ( "(hb_vmMPopMemvar)" ) );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDFIELD:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPushAliasedField( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDVAR:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPushAliasedVar( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHBLOCK:
         {
            /*NOTE: the pcode is stored in dynamically allocated memory
             * We need to handle it with more care than compile-time
             * codeblocks
             */
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5    -> pcode bytes
             */
            HB_SIZE nSize = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            hb_vmPushMacroBlock( pCode + 5, nSize - 5,
                                 HB_PCODE_MKUSHORT( &pCode[ 3 ] ) );
            pCode += nSize;
            break;
         }

         case HB_P_MPUSHBLOCKLARGE:
         {
            /*NOTE: the pcode is stored in dynamically allocated memory
             * We need to handle it with more care than compile-time
             * codeblocks
             */
            /* +0       -> _pushblock
             * +1 +2 +3 -> size of codeblock
             * +4 +5    -> number of expected parameters
             * +6       -> pcode bytes
             */
            HB_SIZE nSize = HB_PCODE_MKUINT24( &pCode[ 1 ] );
            hb_vmPushMacroBlock( pCode + 6, nSize - 6,
                                 HB_PCODE_MKUSHORT( &pCode[ 4 ] ) );
            pCode += nSize;
            break;
         }

         case HB_P_MPUSHFIELD:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            /* It pushes the current value of the given field onto the eval stack
             */
            hb_rddGetFieldValue( hb_stackAllocItem(), pDynSym->pSymbol );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushField)" ) );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVAR:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_memvarGetValue( hb_stackAllocItem(), pDynSym->pSymbol );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushMemvar)" ) );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVARREF:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_memvarGetRefer( hb_stackAllocItem(), pDynSym->pSymbol );
            HB_TRACE( HB_TR_INFO, ( "(hb_vmMPushMemvarRef)" ) );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHSYM:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHVARIABLE:
         {
            PHB_DYNS pDynSym = ( PHB_DYNS ) HB_GET_PTR( pCode + 1 );
            hb_vmPushVariable( pDynSym->pSymbol );
            pCode += sizeof( PHB_DYNS ) + 1;
            break;
         }

         case HB_P_MPUSHSTR:
         {
            HB_USHORT uiSize = HB_PCODE_MKUSHORT( &pCode[ 1 ] );

            hb_vmPushString( ( const char * ) ( pCode + 3 ), uiSize - 1 );
            pCode += 3 + uiSize;
            break;
         }

         case HB_P_MPUSHSTRLARGE:
         {
            HB_SIZE nSize = HB_PCODE_MKUINT24( &pCode[ 1 ] );

            hb_vmPushString( ( const char * ) ( pCode + 3 ), nSize - 1 );
            pCode += 4 + nSize;
            break;
         }

         case HB_P_LOCALNEARADDINT:
         {
            int iLocal = pCode[ 1 ];
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALNEARADDINT" ) );

            hb_vmAddInt( hb_stackLocalVariable( iLocal ),
                         HB_PCODE_MKSHORT( &pCode[ 2 ] ) );
            pCode += 4;
            break;
         }

         case HB_P_LOCALADDINT:
         {
            int iLocal = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            HB_TRACE( HB_TR_DEBUG, ( "HB_P_LOCALADDINT" ) );

            hb_vmAddInt( hb_stackLocalVariable( iLocal ),
                         HB_PCODE_MKSHORT( &pCode[ 3 ] ) );
            pCode += 5;
            break;
         }

         case HB_P_LOCALINC:
         {
            int      iLocal = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            PHB_ITEM pLocal = hb_stackLocalVariable( iLocal );
            hb_vmInc( HB_IS_BYREF( pLocal ) ? hb_itemUnRef( pLocal ) : pLocal );
            pCode += 3;
            break;
         }

         case HB_P_LOCALDEC:
         {
            int iLocal = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            PHB_ITEM pLocal = hb_stackLocalVariable( iLocal );
            hb_vmDec( HB_IS_BYREF( pLocal ) ? hb_itemUnRef( pLocal ) : pLocal );
            pCode += 3;
            break;
         }

         case HB_P_LOCALINCPUSH:
         {
            int iLocal = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            PHB_ITEM pLocal = hb_stackLocalVariable( iLocal );
            if( HB_IS_BYREF( pLocal ) )
               pLocal = hb_itemUnRef( pLocal );
            hb_vmInc( pLocal );
            hb_itemCopy( hb_stackAllocItem(), pLocal );
            pCode += 3;
            break;
         }

         /* WITH OBJECT */

         case HB_P_WITHOBJECTMESSAGE:
         {
            PHB_ITEM pWith;
            HB_USHORT wSymPos = HB_PCODE_MKUSHORT( &pCode[ 1 ] );
            if( wSymPos != 0xFFFF )
            {
               /* NOTE: 0xFFFF is passed when ':&varmacro' syntax is used.
                * In this case symbol is already pushed on the stack
                * using HB_P_MACROSYMBOL.
                */
               hb_vmPushSymbol( pSymbols + wSymPos );
            }
            pWith = hb_stackWithObjectItem();
            if( pWith )
               hb_vmPush( pWith );
            else
               hb_stackAllocItem()->type = HB_IT_NIL;
            pCode += 3;
            break;
         }

         case HB_P_WITHOBJECTSTART:
            hb_vmWithObjectStart();
            pCode++;
            break;

         case HB_P_WITHOBJECTEND:
            hb_stackPop();    /* remove with object envelope */
            hb_stackPop();    /* remove implicit object */
            pCode++;
            break;

         /* misc */

         case HB_P_NOOP:
            /* Intentionally do nothing */
            pCode++;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            hb_errInternal( HB_EI_VMBADOPCODE, NULL, NULL, NULL );
            break;
      }

      if( hb_stackGetActionRequest() )
      {
         if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
         {
            /* request to stop current procedure was issued
             * (from macro evaluation)
             */

            /* This code allow to use RETURN inside BEGIN/END sequence
             * or in RECOVER code when ALWAYS clause is used
             */
            if( bCanRecover )
            {
               do
               {
                  hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
                     hb_errInternal( HB_EI_ERRUNRECOV, "ENDPROC", NULL, NULL );
#endif
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_DOALWAYS )
                     break;
                  /* Restore previous recovery state */
                  bCanRecover = ( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_CANRECOVER ) != 0;
                  hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
               }
               while( bCanRecover );

               /* ALWAYS found? */
               if( bCanRecover )
               {
#if defined( _HB_RECOVER_DEBUG )
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
                     hb_errInternal( HB_EI_ERRUNRECOV, "ENDPROC ALWAYS", NULL, NULL );
#endif
                  /* reload the address of ALWAYS code */
                  pCode = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.recover;
                  /* store and reset action */
                  hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request = hb_stackGetActionRequest();
                  hb_stackSetActionRequest( 0 );
                  continue;
               }
            }
            hb_stackSetActionRequest( 0 );
            break;
         }
         else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
         {
            if( bCanRecover )
            {
               /*
                * There is the BEGIN/END sequence defined in current
                * procedure/function - use it to continue opcodes execution
                */
               /*
                * remove all items placed on the stack after BEGIN code
                */
               hb_stackRemove( hb_stackGetRecoverBase() );
               /*
                * reload the address of recovery code
                */
#if defined( _HB_RECOVER_DEBUG )
               if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
                  hb_errInternal( HB_EI_ERRUNRECOV, "BREAK", NULL, NULL );
#endif
               pCode = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.recover;
               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */

               /* store and reset action */
               hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request = hb_stackGetActionRequest();
               hb_stackSetActionRequest( 0 );
            }
            else
               break;
         }
         else if( hb_stackGetActionRequest() & HB_QUIT_REQUESTED )
         {
            if( bCanRecover )
            {
               do
               {
                  hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
                     hb_errInternal( HB_EI_ERRUNRECOV, "QUIT", NULL, NULL );
#endif
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_DOALWAYS )
                     break;
                  /* Restore previous recovery state */
                  bCanRecover = ( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_CANRECOVER ) != 0;
                  hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
                  /* skip other steps */
               }
               while( bCanRecover );

               /* ALWAYS found? */
               if( bCanRecover )
               {
#if defined( _HB_RECOVER_DEBUG )
                  if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
                     hb_errInternal( HB_EI_ERRUNRECOV, "QUIT ALWAYS", NULL, NULL );
#endif
                  /* reload the address of ALWAYS code */
                  pCode = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.recover;
                  /* store and reset action */
                  hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request = hb_stackGetActionRequest();
                  hb_stackSetActionRequest( 0 );
                  continue;
               }
            }
            break;
         }
      }
   }
}

/* ------------------------------- */
/* Operators ( mathematical        */
/*             character / misc )  */
/* ------------------------------- */

static void hb_vmAddInt( PHB_ITEM pResult, HB_LONG lAdd )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmAddInt(%p,%ld)", pResult, lAdd ) );

   if( HB_IS_BYREF( pResult ) )
   {
      pResult = hb_itemUnRef( pResult );
   }

   if( HB_IS_NUMINT( pResult ) )
   {
      HB_MAXINT nVal = HB_ITEM_GET_NUMINTRAW( pResult ), nResult;

      nResult = nVal + lAdd;

      if( lAdd >= 0 ? nResult >= nVal : nResult < nVal )
      {
         HB_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         pResult->type = HB_IT_DOUBLE;
         pResult->item.asDouble.value = ( double ) nVal + lAdd;;
         pResult->item.asDouble.length = HB_DBL_LENGTH( pResult->item.asDouble.value );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( HB_IS_DOUBLE( pResult ) )
   {
      pResult->item.asDouble.value += lAdd;
      pResult->item.asDouble.length = HB_DBL_LENGTH( pResult->item.asDouble.value );
   }
   else if( HB_IS_DATETIME( pResult ) )
   {
      pResult->type &= ~HB_IT_DEFAULT;
      pResult->item.asDateTime.julian += lAdd;
   }
   else if( hb_objHasOperator( pResult, HB_OO_OP_PLUS ) )
   {
      HB_STACK_TLS_PRELOAD
      hb_vmPushLong( lAdd );
      hb_objOperatorCall( HB_OO_OP_PLUS, pResult, pResult, hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pSubst;

      hb_vmPushLong( lAdd );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pResult, hb_stackItemFromTop( -1 ) );
      if( pSubst )
      {
         hb_stackPop();
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNegate()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pItem ) )
   {
#if -HB_VMINT_MAX > HB_VMINT_MIN
      if( pItem->item.asInteger.value < -HB_VMINT_MAX )
      {
#if HB_VMLONG_MAX > HB_VMINT_MAX
         HB_MAXINT nValue = ( HB_MAXINT ) pItem->item.asInteger.value;
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value = -nValue;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( -nValue );
#else
         double dValue = ( double ) pItem->item.asInteger.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
         pItem->item.asDouble.decimal = 0;
#endif
      }
      else
#endif
      {
         pItem->type = HB_IT_INTEGER;
         pItem->item.asInteger.value = -pItem->item.asInteger.value;
         pItem->item.asInteger.length = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
      }
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if -HB_VMLONG_MAX > HB_VMLONG_MIN
      if( pItem->item.asLong.value < -HB_VMLONG_MAX )
      {
         double dValue = ( double ) pItem->item.asLong.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
         pItem->item.asDouble.decimal = 0;
      }
      else
#endif
      {
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value = -pItem->item.asLong.value;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->type = HB_IT_DOUBLE;
      pItem->item.asDouble.value = -pItem->item.asDouble.value;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1080, NULL, "-", 1, pItem );

      if( pResult )
      {
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmTimeStampPut( PHB_ITEM pItem, long lJulian, long lMilliSec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmTimeStampPut(%p,%ld,%ld)", pItem, lJulian, lMilliSec ) );

   /* timestamp normalization */
   if( lJulian < 0 )
   {
      if( lMilliSec <= -HB_MILLISECS_PER_DAY )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
      }
      else if( lMilliSec > 0 )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
         if( lMilliSec > 0 )
         {
            lMilliSec -= HB_MILLISECS_PER_DAY;
            ++lJulian;
         }
      }
   }
   else
   {
      if( lMilliSec >= HB_MILLISECS_PER_DAY )
      {
         lMilliSec -= HB_MILLISECS_PER_DAY;
         ++lJulian;
      }
      else if( lMilliSec < 0 )
      {
         lMilliSec += HB_MILLISECS_PER_DAY;
         --lJulian;
         if( lMilliSec < 0 )
         {
            lMilliSec += HB_MILLISECS_PER_DAY;
            --lJulian;
         }
      }
   }

   hb_itemPutTDT( pItem, lJulian, lMilliSec );
}

static void hb_vmTimeStampAdd( PHB_ITEM pResult, PHB_ITEM pItem, double dValue )
{
   long lJulian, lMilliSec;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmTimeStampAdd(%p,%p,%lf)", pResult, pItem, dValue ) );

   hb_timeStampUnpackDT( dValue, &lJulian, &lMilliSec );

   lJulian += pItem->item.asDateTime.julian;
   lMilliSec += pItem->item.asDateTime.time;

   hb_vmTimeStampPut( pResult, lJulian, lMilliSec );
}

static void hb_vmPlus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPlus(%p,%p,%p)", pResult, pItem1, pItem2 ) );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW( pItem1 );
      HB_MAXINT nNumber2 = HB_ITEM_GET_NUMINTRAW( pItem2 );
      HB_MAXINT nResult = nNumber1 + nNumber2;

      if( HB_IS_COMPLEX( pResult ) )
         hb_itemClear( pResult );

      if( nNumber2 >= 0 ? nResult >= nNumber1 : nResult < nNumber1 )
      {
         HB_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         double dResult = ( double ) nNumber1 + ( double ) nNumber2;
         pResult->type = HB_IT_DOUBLE;
         pResult->item.asDouble.value = dResult;
         pResult->item.asDouble.length = HB_DBL_LENGTH( dResult );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = hb_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = hb_itemGetNDDec( pItem2, &iDec2 );

      hb_itemPutNDDec( pResult, dNumber1 + dNumber2, HB_MAX( iDec1, iDec2 ) );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE nLen1 = pItem1->item.asString.length;
      HB_SIZE nLen2 = pItem2->item.asString.length;

      if( nLen2 )
      {
         if( nLen1 )
         {
            if( nLen1 < HB_SIZE_MAX - nLen2 )
            {
               if( pResult != pItem1 )
               {
                  hb_itemMove( pResult, pItem1 );
                  pItem1 = pResult;
               }
               hb_itemReSizeString( pItem1, nLen1 + nLen2 );
               hb_xmemcpy( pItem1->item.asString.value + nLen1,
                           pItem2->item.asString.value, nLen2 );
            }
            else
               hb_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+", 2, pItem1, pItem2 );
         }
         else
            hb_itemCopy( pResult, pItem2 );
      }
      else if( pResult != pItem1 )
         hb_itemCopy( pResult, pItem1 );
      pResult->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) || HB_IS_TIMESTAMP( pItem2 ) )
         hb_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian +
                                     pItem2->item.asDateTime.julian,
                                     pItem1->item.asDateTime.time +
                                     pItem2->item.asDateTime.time );
      else
         /* NOTE: This is not a bug. CA-Cl*pper does exactly that for DATEs. */
         hb_itemPutDL( pResult, pItem1->item.asDateTime.julian +
                                pItem2->item.asDateTime.julian );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) )
      {
         if( HB_IS_NUMINT( pItem2 ) )
            hb_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian +
                                        ( long ) HB_ITEM_GET_NUMINTRAW( pItem2 ),
                                        pItem1->item.asDateTime.time );
         else
            hb_vmTimeStampAdd( pResult, pItem1, pItem2->item.asDouble.value );
      }
      else
         hb_itemPutDL( pResult, hb_itemGetDL( pItem1 ) + hb_itemGetNL( pItem2 ) );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem2 ) )
      {
         if( HB_IS_NUMINT( pItem1 ) )
            hb_vmTimeStampPut( pResult, ( long ) HB_ITEM_GET_NUMINTRAW( pItem1 ) +
                                        pItem2->item.asDateTime.julian,
                                        pItem2->item.asDateTime.time );
         else
            hb_vmTimeStampAdd( pResult, pItem2, pItem1->item.asDouble.value );
      }
      else
         hb_itemPutDL( pResult, hb_itemGetNL( pItem1 ) + hb_itemGetDL( pItem2 ) );
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_PLUS, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmMinus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMinus(%p,%p,%p)", pResult, pItem1, pItem2 ) );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW( pItem1 );
      HB_MAXINT nNumber2 = HB_ITEM_GET_NUMINTRAW( pItem2 );
      HB_MAXINT nResult = nNumber1 - nNumber2;

      if( HB_IS_COMPLEX( pResult ) )
         hb_itemClear( pResult );

      if( nNumber2 <= 0 ? nResult >= nNumber1 : nResult < nNumber1 )
      {
         HB_ITEM_PUT_NUMINTRAW( pResult, nResult );
      }
      else
      {
         double dResult = ( double ) nNumber1 - ( double ) nNumber2;
         pResult->type = HB_IT_DOUBLE;
         pResult->item.asDouble.value = dResult;
         pResult->item.asDouble.length = HB_DBL_LENGTH( dResult );
         pResult->item.asDouble.decimal = 0;
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = hb_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = hb_itemGetNDDec( pItem2, &iDec2 );

      hb_itemPutNDDec( pResult, dNumber1 - dNumber2, HB_MAX( iDec1, iDec2 ) );
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      long lTime = pItem1->item.asDateTime.time -
                   pItem2->item.asDateTime.time,
           lJulian = pItem1->item.asDateTime.julian -
                     pItem2->item.asDateTime.julian;
      if( lTime != 0 )
         hb_itemPutNDDec( pResult, hb_timeStampPackDT( lJulian, lTime ), HB_TIMEDIFF_DEC );
      else
      {
         if( HB_IS_COMPLEX( pResult ) )
            hb_itemClear( pResult );
         HB_ITEM_PUT_LONGRAW( pResult, lJulian );
      }
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) )
      {
         if( HB_IS_NUMINT( pItem2 ) )
            hb_vmTimeStampPut( pResult, pItem1->item.asDateTime.julian -
                                        ( long ) HB_ITEM_GET_NUMINTRAW( pItem2 ),
                                        pItem1->item.asDateTime.time );
         else
            hb_vmTimeStampAdd( pResult, pItem1, - pItem2->item.asDouble.value );
      }
      else
         hb_itemPutDL( pResult, hb_itemGetDL( pItem1 ) - hb_itemGetNL( pItem2 ) );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_SIZE nLen1 = pItem1->item.asString.length;
      HB_SIZE nLen2 = pItem2->item.asString.length;

      if( nLen1 == 0 )
      {
         hb_itemCopy( pResult, pItem2 );
         pResult->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
      }
      else if( nLen2 == 0 )
      {
         if( pResult != pItem1 )
            hb_itemCopy( pResult, pItem1 );
         pResult->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
      }
      else if( nLen1 < HB_SIZE_MAX - nLen2 )
      {
         if( pResult != pItem1 )
         {
            hb_itemMove( pResult, pItem1 );
            pItem1 = pResult;
         }
         hb_itemReSizeString( pItem1, nLen1 + nLen2 );
         while( nLen1 && pItem1->item.asString.value[ nLen1 - 1 ] == ' ' )
            nLen1--;
         hb_xmemcpy( pItem1->item.asString.value + nLen1,
                     pItem2->item.asString.value, nLen2 );
         hb_xmemset( pItem1->item.asString.value + nLen1 + nLen2, ' ',
                     pItem1->item.asString.length - nLen1 - nLen2 );
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-", 2, pItem1, pItem2 );
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_MINUS, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmMult( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMult(%p,%p,%p)", pResult, pItem1, pItem2 ) );

#if -( HB_VMLONG_MAX / HB_VMINT_MIN ) >= HB_VMINT_MAX && 1
   if( HB_IS_INTEGER( pItem1 ) && HB_IS_INTEGER( pItem2 ) )
   {
      HB_MAXINT nResult = ( HB_MAXINT ) pItem1->item.asInteger.value *
                          ( HB_MAXINT ) pItem2->item.asInteger.value;
      if( HB_IS_COMPLEX( pResult ) )
         hb_itemClear( pResult );
      HB_ITEM_PUT_NUMINTRAW( pResult, nResult );
   }
   else
#endif
   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec1, iDec2;
      double dNumber1 = hb_itemGetNDDec( pItem1, &iDec1 );
      double dNumber2 = hb_itemGetNDDec( pItem2, &iDec2 );

      hb_itemPutNumType( pResult, dNumber1 * dNumber2, iDec1 + iDec2,
                         HB_ITEM_TYPERAW( pItem1 ), HB_ITEM_TYPERAW( pItem2 ) );
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_MULT, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmDivide( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDivide(%p,%p,%p)", pResult, pItem1, pItem2 ) );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_MAXINT nDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if( nDivisor == 0 )
      {
         PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pSubst )
         {
            hb_itemMove( pResult, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
      {
         HB_MAXINT nNumber1 = HB_ITEM_GET_NUMINTRAW( pItem1 );
         hb_itemPutND( pResult, ( double ) nNumber1 / ( double ) nDivisor );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pSubst )
         {
            hb_itemMove( pResult, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
      {
         /* If all both operand was integer and the result is an integer, too,
            push the number without decimals. Clipper compatible. Actually,
            this is not Clipper compatible. The only time Clipper returns 0
            decimal places is for compiler optimized integer division with an
            integer result. Therefore this code is not needed and has been
            removed - David G. Holm <dholm@jsd-llc.com>
          */
         hb_itemPutND( pResult, hb_itemGetND( pItem1 ) / dDivisor );
      }
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_DIVIDE, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmModulus( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmModulus(%p,%p,%p)", pResult, pItem1, pItem2 ) );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_MAXINT nDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if( nDivisor == 0 )
      {
         PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pSubst )
         {
            hb_itemMove( pResult, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
      {
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         hb_itemPutND( pResult, ( double ) ( HB_ITEM_GET_NUMINTRAW( pItem1 ) % nDivisor ) );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_itemGetND( pItem2 );

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pSubst )
         {
            hb_itemMove( pResult, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
      {
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         hb_itemPutND( pResult, fmod( hb_itemGetND( pItem1 ), dDivisor ) );
      }
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_MOD, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmPower( PHB_ITEM pResult, PHB_ITEM pItem1, PHB_ITEM pItem2 )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPower(%p,%p,%p)", pResult, pItem1, pItem2 ) );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_itemPutND( pResult, pow( hb_itemGetND( pItem1 ), hb_itemGetND( pItem2 ) ) );
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_POWER, pResult, pItem1, pItem2, NULL ) )
   {
      PHB_ITEM pSubst = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^", 2, pItem1, pItem2 );

      if( pSubst )
      {
         hb_itemMove( pResult, pSubst );
         hb_itemRelease( pSubst );
      }
   }
}

static void hb_vmInc( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInc(%p)", pItem ) );

   if( HB_IS_NUMINT( pItem ) )
   {
      if( HB_IS_INTEGER( pItem ) )
      {
         if( pItem->item.asInteger.value < HB_VMINT_MAX )
         {
            pItem->type = HB_IT_INTEGER;
            pItem->item.asInteger.value++;
            pItem->item.asInteger.length = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
         }
         else
         {
#if HB_VMINT_MAX < HB_VMLONG_MAX
            pItem->type = HB_IT_LONG;
            pItem->item.asLong.value = ( HB_MAXINT ) pItem->item.asInteger.value + 1;
            pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
#else
            pItem->type = HB_IT_DOUBLE;
            pItem->item.asDouble.value = ( double ) pItem->item.asInteger.value + 1;
            pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
            pItem->item.asDouble.decimal = 0;
#endif
         }
      }
      else if( pItem->item.asLong.value < HB_VMLONG_MAX )
      {
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value++;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = ( double ) pItem->item.asLong.value + 1;
         pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
         pItem->item.asDouble.decimal = 0;
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->type = HB_IT_DOUBLE;
      pItem->item.asDouble.value++;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( HB_IS_DATETIME( pItem ) )
   {
      pItem->type &= ~HB_IT_DEFAULT;
      pItem->item.asDateTime.julian++;
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_INC, pItem, pItem, NULL, NULL ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++", 1, pItem );

      if( pResult )
      {
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmDec( PHB_ITEM pItem )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDec(%p)", pItem ) );

   if( HB_IS_NUMINT( pItem ) )
   {
      if( HB_IS_INTEGER( pItem ) )
      {
         if( pItem->item.asInteger.value > HB_VMINT_MIN )
         {
            pItem->type = HB_IT_INTEGER;
            pItem->item.asInteger.value--;
            pItem->item.asInteger.length = HB_INT_EXPLENGTH( pItem->item.asInteger.value );
         }
         else
         {
#if HB_VMINT_MIN > HB_VMLONG_MIN
            pItem->type = HB_IT_LONG;
            pItem->item.asLong.value = ( HB_MAXINT ) pItem->item.asInteger.value - 1;
            pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
#else
            pItem->type = HB_IT_DOUBLE;
            pItem->item.asDouble.value = ( double ) pItem->item.asInteger.value - 1;
            pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
            pItem->item.asDouble.decimal = 0;
#endif
         }
      }
      else if( pItem->item.asLong.value > HB_VMLONG_MIN )
      {
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value--;
         pItem->item.asLong.length = HB_LONG_EXPLENGTH( pItem->item.asLong.value );
      }
      else
      {
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = ( double ) pItem->item.asLong.value - 1;
         pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
         pItem->item.asDouble.decimal = 0;
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->type = HB_IT_DOUBLE;
      pItem->item.asDouble.value--;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( HB_IS_DATETIME( pItem ) )
   {
      pItem->type &= ~HB_IT_DEFAULT;
      pItem->item.asDateTime.julian--;
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_DEC, pItem, pItem, NULL, NULL ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--", 1, pItem );

      if( pResult )
      {
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmFuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the satck */
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFuncPtr()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_SYMBOL( pItem ) )
   {
      /* do nothing - now we are using HB_IT_SYMBOL */
#if 0
      hb_stackPop();
      hb_vmPushPointer( ( void * ) pItem->item.asSymbol.value->value.pFunPtr );
#endif
   }
   else
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmFuncPtr()", NULL );
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmExactlyEqual( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmExactlyEqual()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_IS_NIL( pItem2 );
      hb_stackPop();    /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec();    /* pItem2 is already NIL */
      if( HB_IS_COMPLEX( pItem1 ) )
         hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_FALSE;
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asString.length == pItem2->item.asString.length &&
                        memcmp( pItem1->item.asString.value,
                                pItem2->item.asString.value,
                                pItem1->item.asString.length ) == 0;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) ==
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                       pItem2->item.asDateTime.julian &&
                                       pItem1->item.asDateTime.time ==
                                       pItem2->item.asDateTime.time );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     pItem2->item.asLogical.value :
                                     ! pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;

      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;

      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) &&
            ! hb_objHasOperator( pItem1, HB_OO_OP_EXACTEQUAL ) )
   {
      HB_BOOL fResult = pItem1->item.asArray.value == pItem2->item.asArray.value;

      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( hb_objOperatorCall( HB_OO_OP_EXACTEQUAL, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==", 2, pItem1, pItem2 );
      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmEqual( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEqual()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_IS_NIL( pItem2 );
      hb_stackPop();    /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec();    /* pItem2 is already NIL */
      if( HB_IS_COMPLEX( pItem1 ) )
         hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_FALSE;
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_BOOL fResult = hb_itemStrCmp( pItem1, pItem2, HB_FALSE ) == 0;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) ==
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) ==
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian ) &&
                                        ( pItem1->item.asDateTime.time ==
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     pItem2->item.asLogical.value :
                                     ! pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asPointer.value == pItem2->item.asPointer.value;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
/*
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asHash.value == pItem2->item.asHash.value;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
 */
   else if( hb_objOperatorCall( HB_OO_OP_EQUAL, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem1, pItem2 );
      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmNotEqual( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNotEqual()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_NIL( pItem1 ) )
   {
      /* pItem1 is NIL so this is safe */
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = ! HB_IS_NIL( pItem2 );
      hb_stackPop();    /* clear the pItem2 */
   }
   else if( HB_IS_NIL( pItem2 ) )
   {
      hb_stackDec();    /* pItem2 is already NIL */
      if( HB_IS_COMPLEX( pItem1 ) )
         hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = HB_TRUE;
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, HB_FALSE );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i != 0;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) !=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) !=
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian !=
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.time !=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian !=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ?
                                     ! pItem2->item.asLogical.value :
                                     pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( HB_IS_POINTER( pItem1 ) && HB_IS_POINTER( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asPointer.value !=
                        pItem2->item.asPointer.value;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
/*
   else if( HB_IS_HASH( pItem1 ) && HB_IS_HASH( pItem2 ) )
   {
      HB_BOOL fResult = pItem1->item.asHash.value != pItem2->item.asHash.value;
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
 */
   else if( hb_objOperatorCall( HB_OO_OP_NOTEQUAL, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmLess( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLess()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, HB_FALSE );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i < 0;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) <
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time <
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ! pItem1->item.asLogical.value &&
                                     pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_LESS, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmLessEqual( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLessEqual()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, HB_FALSE );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i <= 0;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) <=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) <=
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time <=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian <=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = ! pItem1->item.asLogical.value ||
                                     pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_LESSEQUAL, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreater( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmGreater()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, HB_FALSE );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i > 0;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) >
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time >
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value &&
                                     ! pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_GREATER, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreaterEqual( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmGreaterEqual()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, HB_FALSE );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = i >= 0;
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMINTRAW( pItem1 ) >=
                                       HB_ITEM_GET_NUMINTRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      pItem1->item.asLogical.value = ( HB_ITEM_GET_NUMDBLRAW( pItem1 ) >=
                                       HB_ITEM_GET_NUMDBLRAW( pItem2 ) );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_DATETIME( pItem1 ) && HB_IS_DATETIME( pItem2 ) )
   {
      if( HB_IS_TIMESTAMP( pItem1 ) && HB_IS_TIMESTAMP( pItem2 ) )
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >
                                          pItem2->item.asDateTime.julian ) ||
                                        ( pItem1->item.asDateTime.julian ==
                                          pItem2->item.asDateTime.julian &&
                                          pItem1->item.asDateTime.time >=
                                          pItem2->item.asDateTime.time );
      else
         pItem1->item.asLogical.value = ( pItem1->item.asDateTime.julian >=
                                          pItem2->item.asDateTime.julian );
      pItem1->type = HB_IT_LOGICAL;
      hb_stackDec();
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value ||
                                     ! pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_GREATEREQUAL, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmInstring( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInstring()" ) );

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      HB_BOOL fResult = ( hb_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                                    pItem2->item.asString.value, pItem2->item.asString.length ) != 0 );
      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( HB_IS_HASH( pItem2 ) &&
            ( HB_IS_HASHKEY( pItem1 ) || hb_hashLen( pItem1 ) == 1 ) )
   {
      HB_BOOL fResult = hb_hashScan( pItem2, pItem1, NULL );

      hb_stackPop();
      hb_itemClear( pItem1 );
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = fResult;
   }
   else if( hb_objOperatorCall( HB_OO_OP_INCLUDE, pItem1, pItem2, pItem1, NULL ) )
      hb_stackPop();

   else if( hb_objOperatorCall( HB_OO_OP_INSTRING, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* At this moment the eval stack should store:
 * -3 -> <current counter value>
 * -2 -> <end value>
 * -1 -> <step value>
 */
static void hb_vmForTest( void )        /* Test to check the end point of the FOR */
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pStep;
   HB_BOOL fBack;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmForTest()" ) );

   pStep = hb_stackItemFromTop( -1 );
   if( HB_IS_NUMERIC( pStep ) )
   {
      fBack = HB_ITEM_GET_NUMDBLRAW( pStep ) < 0.0;
      hb_stackDec();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushInteger( 0 );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pStep, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         if( HB_IS_LOGICAL( pResult ) )
         {
            fBack = pResult->item.asLogical.value;
            hb_itemRelease( pResult );
            hb_stackPop();
            hb_stackPop();
         }
         else
         {
            hb_itemMove( hb_stackItemFromTop( -1 ), pResult );
            hb_itemRelease( pResult );
            hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ), 1, hb_stackItemFromTop( -1 ) );
            return;
         }
      }
      else
         return;
   }

   if( fBack )
      hb_vmLess();
   else
      hb_vmGreater();
}

/* Begin Sequence WITH block auto destructor */
static HB_GARBAGE_FUNC( hb_SeqBlockDestructor )
{
   hb_itemMove( hb_errorBlock(), ( PHB_ITEM ) Cargo );
}

static const HB_GC_FUNCS s_gcSeqBlockFuncs =
{
   hb_SeqBlockDestructor,
   hb_gcGripMark
};

static void hb_vmSeqBlock( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSeqBlock()" ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_BLOCK( pItem ) )
   {
      PHB_ITEM pBlockCopy, pBlock;

      pBlock = hb_errorBlock();
      pBlockCopy = ( PHB_ITEM ) hb_gcAllocRaw( sizeof( HB_ITEM ),
                                               &s_gcSeqBlockFuncs );
      hb_itemRawCpy( pBlockCopy, pBlock );
      hb_itemRawCpy( pBlock, pItem );
      pItem->type = HB_IT_POINTER;
      pItem->item.asPointer.value = pBlockCopy;
      pItem->item.asPointer.collect = pItem->item.asPointer.single = HB_TRUE;
   }
}

/* With object auto destructor */
static HB_GARBAGE_FUNC( hb_withObjectDestructor )
{
   HB_STACK_TLS_PRELOAD
   HB_ISIZ * pnWithObjectBase = ( HB_ISIZ * ) Cargo;

   hb_stackWithObjectSetOffset( *pnWithObjectBase );
}

static const HB_GC_FUNCS s_gcWithObjectFuncs =
{
   hb_withObjectDestructor,
   hb_gcDummyMark
};


static void hb_vmWithObjectStart( void )
{
   HB_STACK_TLS_PRELOAD
   HB_ISIZ * pnWithObjectBase;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmWithObjectStart()" ) );

   pItem = hb_stackAllocItem();
   pnWithObjectBase = ( HB_ISIZ * ) hb_gcAllocRaw( sizeof( HB_ISIZ ),
                                                &s_gcWithObjectFuncs );
   * pnWithObjectBase = hb_stackWithObjectOffset();
   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pnWithObjectBase;
   pItem->item.asPointer.collect = pItem->item.asPointer.single = HB_TRUE;
   /* The object is pushed directly before this pcode */
   /* store position of current WITH OBJECT frame */
   hb_stackWithObjectSetOffset( hb_stackTopOffset() - 2 );
}

/*
 * Relase enumerator items - called from hb_itemClear()
 */
void hb_vmEnumRelease( PHB_ITEM pBase, PHB_ITEM pValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEnumRelease(%p,%p)", pBase, pValue ) );

   if( pValue )
      hb_itemRelease( pValue );

   if( HB_IS_OBJECT( pBase ) && hb_vmRequestQuery() == 0 &&
       hb_objHasOperator( pBase, HB_OO_OP_ENUMSTOP ) )
   {
      hb_stackPushReturn();
      hb_vmPushNil();
      hb_objOperatorCall( HB_OO_OP_ENUMSTOP, hb_stackItemFromTop( -1 ),
                          pBase, NULL, NULL );
      hb_stackPop();
      hb_stackPopReturn();
   }
}

/*
 * extended reference used as enumerator destructor
 */
typedef struct
{
   HB_ITEM basevalue;
   HB_ITEM oldvalue;
   HB_ITEM enumref;
} HB_ENUMREF, * PHB_ENUMREF;

static PHB_ITEM hb_vmEnumRefRead( PHB_ITEM pRefer )
{
   return &( ( PHB_ENUMREF ) pRefer->item.asExtRef.value )->oldvalue;
}

static PHB_ITEM hb_vmEnumRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   HB_SYMBOL_UNUSED( pRefer );
   HB_SYMBOL_UNUSED( pSource );
   return NULL;
}

static void hb_vmEnumRefCopy( PHB_ITEM pDest )
{
   pDest->type = HB_IT_NIL;
}

static void hb_vmEnumRefClear( void * value )
{
   hb_itemMove( hb_itemUnRefOnce( &( ( PHB_ENUMREF ) value )->enumref ),
                &( ( PHB_ENUMREF ) value )->oldvalue );
   if( HB_IS_COMPLEX( &( ( PHB_ENUMREF ) value )->basevalue ) )
      hb_itemClear( &( ( PHB_ENUMREF ) value )->basevalue );
   if( HB_IS_COMPLEX( &( ( PHB_ENUMREF ) value )->enumref ) )
      hb_itemClear( &( ( PHB_ENUMREF ) value )->enumref );

   hb_xfree( value );
}

static void hb_vmEnumRefMark( void * value )
{
   if( HB_IS_GCITEM( &( ( PHB_ENUMREF ) value )->basevalue ) )
      hb_gcItemRef( &( ( PHB_ENUMREF ) value )->basevalue );
   if( HB_IS_GCITEM( &( ( PHB_ENUMREF ) value )->oldvalue ) )
      hb_gcItemRef( &( ( PHB_ENUMREF ) value )->oldvalue );
   if( HB_IS_GCITEM( &( ( PHB_ENUMREF ) value )->enumref ) )
      hb_gcItemRef( &( ( PHB_ENUMREF ) value )->enumref );
}

/*
 * create extended reference for enumerator destructor
 */
static void hb_vmEnumReference( PHB_ITEM pBase )
{
   static const HB_EXTREF s_EnumExtRef = {
      hb_vmEnumRefRead,
      hb_vmEnumRefWrite,
      hb_vmEnumRefCopy,
      hb_vmEnumRefClear,
      hb_vmEnumRefMark
   };

   PHB_ENUMREF pEnumExtRef;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEnumReference(%p)", pBase ) );

   pEnumExtRef = ( PHB_ENUMREF ) hb_xgrab( sizeof( HB_ENUMREF ) );
   pEnumExtRef->oldvalue.type = HB_IT_NIL;
   pEnumExtRef->enumref.type = HB_IT_NIL;
   hb_itemRawCpy( &pEnumExtRef->basevalue, pBase );
   pBase->type = HB_IT_BYREF | HB_IT_EXTREF;
   pBase->item.asExtRef.value = ( void * ) pEnumExtRef;
   pBase->item.asExtRef.func = &s_EnumExtRef;
}

/* At this moment the eval stack should store:
 * -2 -> <array for traverse>
 * -1 -> <the reference to enumerate variable>
 */
/* Test to check the start point of the FOR EACH loop */
static void hb_vmEnumStart( int nVars, int nDescend )
{
   HB_STACK_TLS_PRELOAD
   HB_BOOL fStart = HB_TRUE;
   int i;

/*
   pItem = hb_itemUnRef( hb_stackItemFromTop( -( ( int ) nVars << 1 ) ) );
   if( ( pItem->type & ( HB_IT_ARRAY | HB_IT_HASH | HB_IT_STRING ) ) == 0 )
   {
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 1, pItem );
      return;
   }
 */

   for( i = ( int ) nVars << 1; i > 0 && fStart; i -= 2 )
   {
      PHB_ITEM pBase, pValue, pEnumRef, pEnum;

      pValue = hb_stackItemFromTop( -i );
      /* create extended reference for enumerator destructor */
      hb_vmEnumReference( pValue );
      /* store the reference to control variable */
      pEnumRef = hb_stackItemFromTop( -i + 1 );
      hb_itemCopy( &( ( PHB_ENUMREF ) pValue->item.asExtRef.value )->enumref,
                   pEnumRef );
      /* the control variable */
      pEnum = hb_itemUnRefOnce( pEnumRef );
      /* store the old value of control variable and clear it */
      hb_itemMove( &( ( PHB_ENUMREF ) pValue->item.asExtRef.value )->oldvalue,
                   pEnum );

      pBase = &( ( PHB_ENUMREF ) pValue->item.asExtRef.value )->basevalue;
      if( HB_IS_BYREF( pBase ) )
         pBase = hb_itemUnRef( pBase );

      if( HB_IS_COMPLEX( pEnum ) )
         hb_itemClear( pEnum );

      /* set the iterator value */
      pEnum->type = HB_IT_BYREF | HB_IT_ENUM;
      pEnum->item.asEnum.basePtr = &( ( PHB_ENUMREF ) pValue->item.asExtRef.value )->basevalue;
      pEnum->item.asEnum.valuePtr = NULL;

      if( HB_IS_OBJECT( pBase ) && hb_objHasOperator( pBase, HB_OO_OP_ENUMSTART ) )
      {
         pEnum->item.asEnum.offset = 0;
         pEnum->item.asEnum.valuePtr = hb_itemNew( NULL );
         hb_vmPushNil();
         hb_vmPushLogical( nDescend == 0 );
         hb_objOperatorCall( HB_OO_OP_ENUMSTART, hb_stackItemFromTop( -2 ),
                             pBase, pEnumRef, hb_stackItemFromTop( -1 ) );
         hb_stackPop();
         if( hb_vmRequestQuery() != 0 || ! hb_vmPopLogical() )
         {
            fStart = HB_FALSE;
            break;
         }
         else if( hb_objHasOperator( pBase, HB_OO_OP_ENUMSKIP ) )
            continue;
         hb_itemRelease( pEnum->item.asEnum.valuePtr );
         pEnum->item.asEnum.valuePtr = NULL;
      }

      if( HB_IS_ARRAY( pBase ) )
      {
         /* the index into an array */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 :
                                       pBase->item.asArray.value->nLen;
         if( pBase->item.asArray.value->nLen == 0 )
            fStart = HB_FALSE;
      }
      else if( HB_IS_HASH( pBase ) )
      {
         HB_SIZE nLen = hb_hashLen( pBase );
         /* the index into a hash */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 : nLen;
         if( nLen == 0 )
            fStart = HB_FALSE;
      }
      else if( HB_IS_STRING( pBase ) )
      {
         /* storage item for single characters */
         pEnum->item.asEnum.offset = ( nDescend > 0 ) ? 1 :
                                       pBase->item.asString.length;
         if( pBase->item.asString.length )
            pEnum->item.asEnum.valuePtr =
                        hb_itemPutCL( NULL, pBase->item.asString.value +
                                            pEnum->item.asEnum.offset - 1, 1 );
         else
            fStart = HB_FALSE;
      }
      else if( hb_vmRequestQuery() == 0 )
      {
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }

   hb_vmPushInteger( nVars );    /* number of iterators */
   /* empty array/string - do not start enumerations loop */
   hb_vmPushLogical( fStart );
}


/* Enumeration in ascending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumNext( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pEnumRef, pEnum, pBase;
   int i;

   for( i = ( int ) hb_stackItemFromTop( -1 )->item.asInteger.value; i > 0; --i )
   {
      pEnumRef = hb_stackItemFromTop( -( i << 1 ) );
      pEnum = hb_itemUnRefOnce( pEnumRef );
      pBase = pEnum->item.asEnum.basePtr;
      if( HB_IS_BYREF( pBase ) )
         pBase = hb_itemUnRef( pBase );
      if( HB_IS_ARRAY( pBase ) )
      {
         if( HB_IS_OBJECT( pBase ) &&
             hb_objHasOperator( pBase, HB_OO_OP_ENUMSKIP ) )
         {
            ++pEnum->item.asEnum.offset;
            hb_vmPushNil();
            hb_vmPushLogical( HB_FALSE );
            hb_objOperatorCall( HB_OO_OP_ENUMSKIP, hb_stackItemFromTop( -2 ),
                                pBase, pEnumRef, hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            if( hb_vmRequestQuery() != 0 || ! hb_vmPopLogical() )
               break;
         }
         else
         {
            /* Clear the item value which can be set with RT error
               when enumerator was out of array size during unreferencing
             */
            if( pEnum->item.asEnum.valuePtr )
            {
               hb_itemRelease( pEnum->item.asEnum.valuePtr );
               pEnum->item.asEnum.valuePtr = NULL;
            }
            if( ( HB_SIZE ) ++pEnum->item.asEnum.offset >
                pBase->item.asArray.value->nLen )
               break;
         }
      }
      else if( HB_IS_HASH( pBase ) )
      {
         /* Clear the item value which can be set with RT error
            when enumerator was out of array size during unreferencing
          */
         if( pEnum->item.asEnum.valuePtr )
         {
            hb_itemRelease( pEnum->item.asEnum.valuePtr );
            pEnum->item.asEnum.valuePtr = NULL;
         }
         if( ( HB_SIZE ) ++pEnum->item.asEnum.offset > hb_hashLen( pBase ) )
            break;
      }
      else if( HB_IS_STRING( pBase ) )
      {
         if( ( HB_SIZE ) ++pEnum->item.asEnum.offset >
             pBase->item.asString.length )
            break;
         pEnum->item.asEnum.valuePtr = hb_itemPutCL(
                                          pEnum->item.asEnum.valuePtr,
                                          pBase->item.asString.value +
                                          pEnum->item.asEnum.offset - 1, 1 );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }
   hb_vmPushLogical( i == 0 );
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumPrev( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pEnumRef, pEnum, pBase;
   int i;

   for( i = hb_stackItemFromTop( -1 )->item.asInteger.value; i > 0; --i )
   {
      pEnumRef = hb_stackItemFromTop( -( i << 1 ) );
      pEnum = hb_itemUnRefOnce( pEnumRef );
      pBase = pEnum->item.asEnum.basePtr;
      if( HB_IS_BYREF( pBase ) )
         pBase = hb_itemUnRef( pBase );
      if( HB_IS_ARRAY( pBase ) )
      {
         if( HB_IS_OBJECT( pBase ) &&
             hb_objHasOperator( pBase, HB_OO_OP_ENUMSKIP ) )
         {
            --pEnum->item.asEnum.offset;
            hb_vmPushNil();
            hb_vmPushLogical( HB_TRUE );
            hb_objOperatorCall( HB_OO_OP_ENUMSKIP, hb_stackItemFromTop( -2 ),
                                pBase, pEnumRef, hb_stackItemFromTop( -1 ) );
            hb_stackPop();
            if( hb_vmRequestQuery() != 0 || ! hb_vmPopLogical() )
               break;
         }
         else
         {
            /* Clear the item value which can be set with RT error
               when enumerator was out of array size during unreferencing
             */
            if( pEnum->item.asEnum.valuePtr )
            {
               hb_itemRelease( pEnum->item.asEnum.valuePtr );
               pEnum->item.asEnum.valuePtr = NULL;
            }
            if( --pEnum->item.asEnum.offset == 0 )
               break;
         }
      }
      else if( HB_IS_HASH( pBase ) )
      {
         /* Clear the item value which can be set with RT error
            when enumerator was out of array size during unreferencing
          */
         if( pEnum->item.asEnum.valuePtr )
         {
            hb_itemRelease( pEnum->item.asEnum.valuePtr );
            pEnum->item.asEnum.valuePtr = NULL;
         }
         if( --pEnum->item.asEnum.offset == 0 )
            break;
      }
      else if( HB_IS_STRING( pBase ) )
      {
         if( --pEnum->item.asEnum.offset == 0 )
            break;
         pEnum->item.asEnum.valuePtr = hb_itemPutCL(
                                          pEnum->item.asEnum.valuePtr,
                                          pBase->item.asString.value +
                                          pEnum->item.asEnum.offset - 1, 1 );
      }
      else
      {
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 1, pBase );
         return;
      }
   }
   hb_vmPushLogical( i == 0 );
}

/* Enumeration in descending order
 * At this moment the eval stack should store:
 * -3 -> <old value of enumerator variable>
 * -2 -> <the reference to enumerate variable>
 * -1 -> <number of iterators>
 */
static void hb_vmEnumEnd( void )
{
   HB_STACK_TLS_PRELOAD
   int iVars;

   /* remove number of iterators */
   iVars = hb_stackItemFromTop( -1 )->item.asInteger.value;
   hb_stackDec();

   while( --iVars >= 0 )
   {
      hb_stackPop();
      hb_stackPop();
   }
}

static PHB_ITEM hb_vmSwitchGet( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pSwitch = hb_stackItemFromTop( -1 );

   if( ! ( HB_IS_NUMINT( pSwitch ) || HB_IS_STRING( pSwitch ) ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 3104, NULL, "SWITCH", 1, pSwitch );

      if( ! pResult )
         return NULL;

      hb_itemMove( pSwitch, pResult );
      hb_itemRelease( pResult );
   }

   return pSwitch;
}

static const HB_BYTE * hb_vmSwitch( const HB_BYTE * pCode, HB_USHORT casesCnt )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pSwitch = hb_vmSwitchGet();

   if( pSwitch )
   {
      HB_BOOL fFound = HB_FALSE;

      while( ! fFound && casesCnt-- )
      {
         switch( pCode[ 0 ] )
         {
            case HB_P_PUSHLONG:
               if( HB_IS_NUMINT( pSwitch ) )
               {
                  fFound = HB_ITEM_GET_NUMINTRAW( pSwitch ) == HB_PCODE_MKLONG( &pCode[ 1 ] );
               }
               pCode += 5;
               break;

            case HB_P_PUSHSTRSHORT:
               if( HB_IS_STRING( pSwitch ) )
               {
                  /*fFound = hb_itemStrCmp( pItem1, pItem2, bExact );*/
                  fFound = ( HB_SIZE ) pCode[ 1 ] - 1 == pSwitch->item.asString.length &&
                           memcmp( pSwitch->item.asString.value, &pCode[ 2 ],
                                   pSwitch->item.asString.length ) == 0;
               }
               pCode += 2 + pCode[ 1 ];
               break;

            case HB_P_PUSHNIL:
               /* default clause */
               fFound = HB_TRUE;
               pCode++;
               break;
         }

         switch( pCode[ 0 ] )
         {
            case HB_P_JUMPNEAR:
               if( fFound )
                  pCode += ( signed char ) pCode[ 1 ];
               else
                  pCode += 2;
               break;
            case HB_P_JUMP:
               if( fFound )
                  pCode += HB_PCODE_MKSHORT( &pCode[ 1 ] );
               else
                  pCode += 3;
               break;
            case HB_P_JUMPFAR:
               if( fFound )
                  pCode += HB_PCODE_MKINT24( &pCode[ 1 ] );
               else
                  pCode += 4;
               break;
         }
      }
   }
   hb_stackPop();
   return pCode;
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmNot()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_LOGICAL( pItem ) )
   {
      pItem->type = HB_IT_LOGICAL;
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;
   }
   else if( ! hb_objOperatorCall( HB_OO_OP_NOT, pItem, pItem, NULL, NULL ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1077, NULL, ".NOT.", 1, pItem );

      if( pResult )
      {
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmAnd( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmAnd()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_AND, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1078, NULL, ".AND.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmOr( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmOr()" ) );

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->type = HB_IT_LOGICAL;
      pItem1->item.asLogical.value = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
      hb_stackDec();
   }
   else if( hb_objOperatorCall( HB_OO_OP_OR, pItem1, pItem1, pItem2, NULL ) )
      hb_stackPop();

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1079, NULL, ".OR.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem1, pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* ------------------------------- */
/* Array                           */
/* ------------------------------- */

static void hb_vmArrayPush( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   HB_SIZE nIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPush()" ) );

   pIndex = hb_stackItemFromTop( -1 );
   pArray = hb_stackItemFromTop( -2 );

   if( HB_IS_HASH( pArray ) && HB_IS_HASHKEY( pIndex ) )
   {
      PHB_ITEM pValue = hb_hashGetItemPtr( pArray, pIndex, HB_HASH_AUTOADD_ACCESS );
      if( pValue )
      {
         hb_itemCopy( pIndex, pValue );
         hb_itemMove( pArray, pIndex );
         hb_stackDec();
      }
      else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         hb_stackPop();
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      return;
   }
   else if( HB_IS_INTEGER( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asDouble.value;
   else
   {
      if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         hb_stackPop();
      else
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
         if( pResult )
         {
            hb_stackPop();
            hb_itemMove( pArray, pResult );
            hb_itemRelease( pResult );
         }
      }
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_OBJECT( pArray ) &&
          hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
      {
         hb_stackPop();
         return;
      }

      if( HB_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         hb_itemCopy( pIndex, pArray->item.asArray.value->pItems + nIndex - 1 );
         hb_itemMove( pArray, pIndex );
         hb_stackDec();
      }
      else if( ! HB_IS_OBJECT( pArray ) &&
               hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
         hb_stackPop();
      else
#ifdef HB_CLP_STRICT
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 0 );
#else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
#endif
   }
   else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, NULL ) )
      hb_stackPop();

   else
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void hb_vmArrayPushRef( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   PHB_ITEM pRefer;
   HB_SIZE  nIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPushRef()" ) );

   pIndex = hb_stackItemFromTop( -1 );
   pRefer = hb_stackItemFromTop( -2 );
   pArray = HB_IS_BYREF( pRefer ) ? hb_itemUnRef( pRefer ) : pRefer;

   if( HB_IS_HASH( pArray ) && HB_IS_HASHKEY( pIndex ) )
   {
      PHB_ITEM pValue = hb_hashGetItemRefPtr( pArray, pIndex );
      if( pValue )
      {
         hb_itemCopy( pIndex, pValue );
         hb_itemMove( pRefer, pIndex );
         hb_stackDec();
      }
      else if( hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         hb_vmMsgIndexReference( pRefer, pArray, pIndex );
         hb_stackPop();
         return;
      }
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
      return;
   }
   else if( HB_IS_INTEGER( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asDouble.value;
   else if( hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
   {
      /* create extended object index reference */
      hb_vmMsgIndexReference( pRefer, pArray, pIndex );
      hb_stackPop();
      return;
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pRefer, pResult );
         hb_itemRelease( pResult );
      }
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_OBJECT( pArray ) && hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         hb_vmMsgIndexReference( pRefer, pArray, pIndex );
         hb_stackPop();
         return;
      }
      else if( HB_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         /* This function is safe for overwriting passed array, [druzus] */
         hb_arrayGetItemRef( pArray, nIndex, pRefer );
         hb_stackDec();
      }
      else if( ! HB_IS_OBJECT( pArray ) && hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
      {
         /* create extended object index reference */
         hb_vmMsgIndexReference( pRefer, pArray, pIndex );
         hb_stackPop();
         return;
      }
      else
#ifdef HB_CLP_STRICT
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 0 );
#else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
#endif
   }
   else if( hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
   {
      /* create extended object index reference */
      hb_vmMsgIndexReference( pRefer, pArray, pIndex );
      hb_stackPop();
      return;
   }
   else
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void hb_vmArrayPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pValue;
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   HB_SIZE  nIndex;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayPop()" ) );

   pValue = hb_stackItemFromTop( -3 );
   pArray = hb_stackItemFromTop( -2 );
   pIndex = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( HB_IS_HASH( pArray ) && HB_IS_HASHKEY( pIndex ) )
   {
      PHB_ITEM pDest = hb_hashGetItemPtr( pArray, pIndex, HB_HASH_AUTOADD_ASSIGN );
      if( pDest )
      {
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemMoveFromRef( pDest, pValue );
         hb_stackPop();
         hb_stackPop();
         hb_stackDec();    /* value was moved above hb_stackDec() is enough */
      }
      else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, pIndex, pValue );
      return;
   }
   else if( HB_IS_INTEGER( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      nIndex = ( HB_SIZE ) pIndex->item.asDouble.value;
   else
   {
      if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_OBJECT( pArray ) &&
          hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
         return;
      }

      if( HB_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemMoveRef( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
         hb_stackPop();
         hb_stackPop();
         hb_stackDec();    /* value was moved above hb_stackDec() is enough */
      }
      else if( ! HB_IS_OBJECT( pArray ) &&
               hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      }
      else
#ifdef HB_CLP_STRICT
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 0 );
#else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
#endif
   }
   else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray, pIndex, pValue ) )
   {
      hb_stackPop();
      hb_stackPop();
      hb_stackPop();
      return;
   }
   else
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
}

static void hb_vmArrayGen( HB_SIZE nElements ) /* generates an nElements Array and fills it from the stack values */
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray;
   HB_SIZE  nPos;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayGen(%" HB_PFS "u)", nElements ) );

   /* create new array on HVM stack */
   pArray = hb_stackAllocItem();
   hb_arrayNew( pArray, nElements );

   if( nElements )
   {
      /* move items from HVM stack to created array */
      for( nPos = 0; nPos < nElements; nPos++ )
      {
         PHB_ITEM pValue = hb_stackItemFromTop( ( int ) ( nPos - nElements - 1 ) );
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemMove( pArray->item.asArray.value->pItems + nPos, pValue );
      }
      /* move the new array to position of first parameter */
      hb_itemMove( hb_stackItemFromTop( -1 - ( int ) nElements ), pArray );

      /* decrease the stack counter - all items are NIL */
      hb_stackDecrease( nElements );
   }
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static void hb_vmArrayNew( PHB_ITEM pArray, HB_USHORT uiDimension )
{
   HB_STACK_TLS_PRELOAD
   HB_SIZE  nElements;
   PHB_ITEM pDim;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayNew(%p, %hu)", pArray, uiDimension ) );

   pDim = hb_stackItemFromTop( ( int ) ( -1 - uiDimension ) );

   /* use the proper type of number of elements */
   if( HB_IS_INTEGER( pDim ) )
      nElements = ( HB_SIZE ) pDim->item.asInteger.value;
   else if( HB_IS_LONG( pDim ) )
      nElements = ( HB_SIZE ) pDim->item.asLong.value;
   else if( HB_IS_DOUBLE( pDim ) )
      nElements = ( HB_SIZE ) pDim->item.asDouble.value;
   else
      /* NOTE: Clipper creates empty array if non-numeric value is
       * specified as dimension and stops further processing.
       * There is no runtime error generated.
       */
      nElements = 0;

   /* create an array */
   hb_arrayNew( pArray, nElements );

   if( --uiDimension )
   {
      /* call self recursively to create next dimensions
       */
      while( nElements-- )
         hb_vmArrayNew( pArray->item.asArray.value->pItems + nElements, uiDimension );
   }
}

static void hb_vmArrayDim( HB_USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayDim(%hu)", uiDimensions ) );

   hb_vmArrayNew( hb_stackAllocItem(), uiDimensions );

   hb_itemMove( hb_stackItemFromTop( ( int ) ( -1 - uiDimensions ) ),
                hb_stackItemFromTop( -1 ) );
   do
   {
      hb_stackPop();
   }
   while( --uiDimensions );
}

static void hb_vmHashGen( HB_SIZE nElements ) /* generates an nElements Hash and fills it from the stack values */
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pHash, pKey, pVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmHashGen(%" HB_PFS "u)", nElements ) );

   /* create new hash item */
   pHash = hb_hashNew( NULL );
   hb_hashPreallocate( pHash, nElements );
   while( nElements-- )
   {
      pKey = hb_stackItemFromTop( -2 );
      pVal = hb_stackItemFromTop( -1 );
      if( HB_IS_HASHKEY( pKey ) )
      {
         hb_hashAddNew( pHash, pKey, pVal );
         hb_stackPop();
         hb_stackPop();
      }
      else
      {
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pHash, pKey, pVal );
         break;
      }
   }
   hb_itemMove( hb_stackAllocItem(), pHash );
   hb_itemRelease( pHash );
}


/* ------------------------------- */
/* Macros                          */
/* ------------------------------- */

static void hb_vmMacroPushIndex( void )
{
   HB_STACK_TLS_PRELOAD
   HB_SIZE nIndexes;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMacroPushIndex()" ) );

   /*
    * Now the top most element on the stack points to number of
    * additional indexes to generated array
    */
   nIndexes = hb_itemGetNS( hb_stackItemFromTop( -1 ) );
   hb_stackDec();

   if( nIndexes > 1 )
   {
      PHB_ITEM pIndexArray;
      HB_SIZE n = 1;

      hb_vmArrayGen( nIndexes - 1 );
      pIndexArray = hb_itemNew( hb_stackItemFromTop( -1 ) );
      hb_stackPop();

      /* First index is still on stack.*/
      do
      {
         PHB_ITEM pArray = hb_stackItemFromTop( -2 );
         if( HB_IS_BYREF( pArray ) )
            hb_vmArrayPushRef();
         else
            hb_vmArrayPush();
         /* RT error? */
         if( hb_stackGetActionRequest() != 0 )
            break;
         hb_vmPush( hb_arrayGetItemPtr( pIndexArray, n ) );
      }
      while( ++n < nIndexes );

      hb_itemRelease( pIndexArray );
   }
   else if( nIndexes == 0 )
      hb_vmPushNil();   /* It will force RT error later in array push or pop */
}

/*
 * On HVM stack we have sets with arguments
 *    offset   value
 *    (-9)     6
 *    (-8)     7
 *    (-7)     2 // number of arguments
 *    (-6)     1
 *    (-5)     2
 *    (-4)     2 // number of arguments
 *    (-3)     1
 *    (-2)     2
 *    (-1)     2 // number of arguments
 * we should join them into one continuous list
 */
static HB_LONG hb_vmArgsJoin( HB_LONG lLevel, HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lArgs, lRestArgs, lOffset;
   PHB_ITEM pArgs = hb_stackItemFromTop( lLevel ) ;

   lArgs = hb_itemGetNL( pArgs );
   if( HB_IS_COMPLEX( pArgs ) )
      hb_itemClear( pArgs );

   if( --uiArgSets )
   {
      lRestArgs = lArgs;
      lArgs += hb_vmArgsJoin( lLevel - lArgs - 1, uiArgSets );
      lOffset = lLevel - lRestArgs - uiArgSets;
      while( lRestArgs-- )
      {
         hb_itemMove( hb_stackItemFromTop( lOffset ),
                      hb_stackItemFromTop( lOffset + uiArgSets ) );
         ++lOffset;
      }
   }

   return lArgs;
}

static void hb_vmMacroDo( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lArgs;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMacroDo(%hu)", uiArgSets ) );

   lArgs = hb_vmArgsJoin( -1, uiArgSets );
   hb_stackDecrease( uiArgSets );
   hb_vmProc( ( HB_USHORT ) lArgs );
}

static void hb_vmMacroFunc( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lArgs;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMacroFunc(%hu)", uiArgSets ) );

   lArgs = hb_vmArgsJoin( -1, uiArgSets );
   hb_stackDecrease( uiArgSets );
   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmProc( ( HB_USHORT ) lArgs );
   hb_stackPushReturn();
}

static void hb_vmMacroSend( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lArgs;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMacroSend(%hu)", uiArgSets ) );

   lArgs = hb_vmArgsJoin( -1, uiArgSets );
   hb_stackDecrease( uiArgSets );
   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmSend( ( HB_USHORT ) lArgs );
   hb_stackPushReturn();
}

static void hb_vmMacroArrayGen( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD
   HB_LONG lArgs;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMacroArrayGen(%hu)", uiArgSets ) );

   lArgs = hb_vmArgsJoin( -1, uiArgSets );
   hb_stackDecrease( uiArgSets );
   hb_vmArrayGen( lArgs );
}

static void hb_vmPushVParams( void )
{
   HB_STACK_TLS_PRELOAD
   int iPCount, iFirst, i = 0;
   PHB_ITEM pBase;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushVParams()" ) );

   pBase = hb_stackBaseItem();
   iFirst = pBase->item.asSymbol.paramdeclcnt;
   iPCount = pBase->item.asSymbol.paramcnt;
   while( ++iFirst <= iPCount )
   {
      hb_vmPush( hb_stackItemFromBase( iFirst ) );
      i++;
   }
   hb_vmPushInteger( i );
}

static void hb_vmPushAParams( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray, pCount;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAParams()" ) );

   pArray = hb_stackItemFromTop( -1 );
   if( HB_IS_ARRAY( pArray ) )
   {
      HB_SIZE nLen = pArray->item.asArray.value->nLen, ul;

      if( nLen )
      {
         for( ul = 1; ul < nLen; ++ul )
            hb_vmPush( pArray->item.asArray.value->pItems + ul );
         pCount = hb_stackAllocItem();
         hb_itemCopy( pCount, pArray->item.asArray.value->pItems );
         hb_itemMove( pArray, pCount );
         hb_itemPutNS( pCount, nLen );
      }
      else
         hb_itemPutNL( pArray, 0 );
   }
   else
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 1, pArray );
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static HB_ERRCODE hb_vmSelectWorkarea( PHB_ITEM pAlias, PHB_SYMB pField )
{
   HB_STACK_TLS_PRELOAD
   HB_ERRCODE errCode;
   HB_BOOL fRepeat;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSelectWorkArea(%p,%p)", pAlias, pField ) );

   /* NOTE: Clipper doesn't generate an error if an workarea specified
    * as numeric value cannot be selected
    */
   do
   {
      fRepeat = HB_FALSE;
      errCode = HB_SUCCESS;

      switch( HB_ITEM_TYPE( pAlias ) )
      {
         case HB_IT_INTEGER:
            /* Alias was used as integer value, for example: 4->field
             * or it was saved on the stack using hb_vmPushAlias()
             * or was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( pAlias->item.asInteger.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_LONG:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asLong.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_DOUBLE:
            /* Alias was evaluated from an expression, (nWorkArea)->field
             */
            hb_rddSelectWorkAreaNumber( ( int ) pAlias->item.asDouble.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_SYMBOL:
            /* Alias was specified using alias identifier, for example: al->field
             */
            errCode = hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
            pAlias->type = HB_IT_NIL;
            break;

         case HB_IT_STRING:
         {
            /* Alias was evaluated from an expression, for example: (cVar)->field
             */
            /* expand '&' operator if exists */
            char * szAlias;
            HB_BOOL bNewString;

            szAlias = hb_macroExpandString( pAlias->item.asString.value, pAlias->item.asString.length, &bNewString );
            if( pField )
            {
               errCode = hb_rddSelectWorkAreaAlias( szAlias );
            }
            else
            {
               int iArea;
               hb_rddGetAliasNumber( szAlias, &iArea );
               hb_rddSelectWorkAreaNumber( iArea );
            }

            if( bNewString )
               hb_xfree( szAlias );
            hb_itemClear( pAlias );
            break;
         }

         default:
            if( pField )
            {
               PHB_ITEM pSubstVal;

               hb_vmPushString( pField->szName, strlen( pField->szName ) );
               pSubstVal = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&",
                                       2, pAlias, hb_stackItemFromTop( -1 ) );
               hb_stackPop();
               if( pSubstVal )
               {
                  hb_itemMove( pAlias, pSubstVal );
                  hb_itemRelease( pSubstVal );
                  fRepeat = HB_TRUE;
               }
               else
               {
                  hb_itemSetNil( pAlias );
                  errCode = HB_FAILURE;
               }
            }
            else
            {
               hb_rddSelectWorkAreaNumber( -1 );
               hb_itemSetNil( pAlias );
            }
            break;
      }
   }
   while( fRepeat );

   return errCode;
}

/* Swaps two last items on the eval stack - the last item after swaping
 * is popped as current workarea number
 */
static void hb_vmSwapAlias( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;
   PHB_ITEM pWorkArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSwapAlias()" ) );

   pItem = hb_stackItemFromTop( -1 );
   pWorkArea = hb_stackItemFromTop( -2 );

   hb_vmSelectWorkarea( pWorkArea, NULL );

   hb_itemMove( pWorkArea, pItem );
   hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void hb_vmProc( HB_USHORT uiParams )
{
   HB_STACK_STATE sStackState;
   PHB_SYMB pSym;

#ifndef HB_NO_PROFILER
   HB_ULONG ulClock = 0;
   HB_BOOL bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmProc(%hu)", uiParams ) );

   HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( HB_ULONG ) clock();
#endif

   /* Poll the console keyboard */
#if 0
   #if ! defined( HB_GUI )
      hb_inkeyPoll();
   #endif
#endif

   pSym = hb_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   HB_VM_FUNCUNREF( pSym );
   if( HB_VM_ISFUNC( pSym ) )
   {
      HB_TRACE_PRG( ( "Calling: %s", pSym->szName ) );

#ifndef HB_NO_PROFILER
      if( bProfiler && pSym->pDynSym )
         pSym->pDynSym->ulRecurse++;
#endif

      HB_VM_EXECUTE( pSym );

#ifndef HB_NO_PROFILER
      if( bProfiler && pSym->pDynSym )
      {
         pSym->pDynSym->ulCalls++;                   /* profiler support */
         /* Time spent has to be added only inside topmost call of a recursive function */
         if( --pSym->pDynSym->ulRecurse == 0 )
            pSym->pDynSym->ulTime += clock() - ulClock;  /* profiler support */
      }
#endif
   }
   else
      hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, HB_ERR_ARGS_BASEPARAMS );

#ifndef HB_NO_DEBUG
   if( sStackState.fDebugging )
      hb_vmDebuggerEndProc();
#endif

   hb_stackOldFrame( &sStackState );
}

void hb_vmDo( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD
   HB_STACK_STATE sStackState;
   PHB_SYMB pSym;
   PHB_ITEM pSelf;

#ifndef HB_NO_PROFILER
   HB_ULONG ulClock = 0;
   HB_BOOL bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDo(%hu)", uiParams ) );

   HB_TASK_SHEDULER();

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( HB_ULONG ) clock();
#endif

   /* Poll the console keyboard */
#if 0
   #if ! defined( HB_GUI )
      hb_inkeyPoll();
   #endif
#endif

   pSym = hb_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   pSelf = hb_stackSelfItem();   /* NIL, OBJECT or BLOCK */

   if( ! HB_IS_NIL( pSelf ) )  /* are we sending a message ? */
   {
      PHB_SYMB pExecSym;

      pExecSym = hb_objGetMethod( pSelf, pSym, &sStackState );
      if( pExecSym )
         HB_VM_FUNCUNREF( pExecSym );
      if( pExecSym && HB_VM_ISFUNC( pExecSym ) )
      {
         HB_TRACE_PRG( ( "Calling: %s:%s", hb_objGetClsName( pSelf ), pSym->szName ) );

         HB_VM_EXECUTE( pExecSym );

#ifndef HB_NO_PROFILER
         if( bProfiler )
            hb_mthAddTime( clock() - ulClock );
#endif
      }
      else if( pSym->szName[ 0 ] == '_' )
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS );
      else
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, HB_ERR_ARGS_SELFPARAMS );
   }
   else /* it is a function */
   {
      HB_VM_FUNCUNREF( pSym );
      if( HB_VM_ISFUNC( pSym ) )
      {
         HB_TRACE_PRG( ( "Calling: %s", pSym->szName ) );

#ifndef HB_NO_PROFILER
         if( bProfiler && pSym->pDynSym )
            pSym->pDynSym->ulRecurse++;
#endif

         HB_VM_EXECUTE( pSym );

#ifndef HB_NO_PROFILER
         if( bProfiler && pSym->pDynSym )
         {
            pSym->pDynSym->ulCalls++;                   /* profiler support */
            /* Time spent has to be added only inside topmost call of a recursive function */
            if( --pSym->pDynSym->ulRecurse == 0 )
               pSym->pDynSym->ulTime += clock() - ulClock;  /* profiler support */
         }
#endif
      }
      else
         hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, HB_ERR_ARGS_BASEPARAMS );
   }

#ifndef HB_NO_DEBUG
   if( sStackState.fDebugging )
      hb_vmDebuggerEndProc();
#endif

   hb_stackOldFrame( &sStackState );
}

void hb_vmSend( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD
   HB_STACK_STATE sStackState;
   PHB_SYMB pSym;
   PHB_SYMB pExecSym;
   PHB_ITEM pSelf;

#ifndef HB_NO_PROFILER
   HB_ULONG ulClock = 0;
   HB_BOOL bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

   HB_TASK_SHEDULER();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSend(%hu)", uiParams ) );

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( HB_ULONG ) clock();
#endif

   /* Poll the console keyboard */
#if 0
   #if ! defined( HB_GUI )
      hb_inkeyPoll();
   #endif
#endif

   pSym = hb_stackNewFrame( &sStackState, uiParams )->item.asSymbol.value;
   pSelf = hb_stackSelfItem();   /* NIL, OBJECT or BLOCK */

   pExecSym = hb_objGetMethod( pSelf, pSym, &sStackState );
   if( pExecSym )
      HB_VM_FUNCUNREF( pExecSym );
   if( pExecSym && HB_VM_ISFUNC( pExecSym ) )
   {
      HB_TRACE_PRG( ( "Calling: %s:%s", hb_objGetClsName( pSelf ), pSym->szName ) );

      HB_VM_EXECUTE( pExecSym );

#ifndef HB_NO_PROFILER
      if( bProfiler )
         hb_mthAddTime( clock() - ulClock );
#endif
   }
   else if( pSym->szName[ 0 ] == '_' )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, HB_ERR_ARGS_SELFPARAMS );
   else
      hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, HB_ERR_ARGS_SELFPARAMS );

#ifndef HB_NO_DEBUG
   if( sStackState.fDebugging )
      hb_vmDebuggerEndProc();
#endif

   hb_stackOldFrame( &sStackState );
}

static void hb_vmPushObjectVarRef( void )
{
   HB_STACK_TLS_PRELOAD
   HB_STACK_STATE sStackState;
   PHB_ITEM pItem;
   PHB_SYMB pSym;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushObjectVarRef()" ) );

   pItem = hb_stackNewFrame( &sStackState, 0 );   /* procedure name */
   pSym = pItem->item.asSymbol.value;

   if( ! hb_objGetVarRef( hb_stackSelfItem(), pSym, &sStackState ) &&
       hb_vmRequestQuery() == 0 )
      hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + ( pSym->szName[ 0 ] == '_' ? 1 : 0 ), 1, hb_stackSelfItem() );

   hb_stackOldFrame( &sStackState );

   hb_stackPushReturn();
}

void hb_vmEval( HB_USHORT uiParams )
{
   HB_STACK_STATE sStackState;

#ifndef HB_NO_PROFILER
   HB_ULONG ulClock = 0;
   HB_BOOL bProfiler = hb_bProfiler; /* because profiler state may change */
#endif

   HB_TASK_SHEDULER();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEval(%hu)", uiParams ) );

#ifndef HB_NO_PROFILER
   if( bProfiler )
      ulClock = ( HB_ULONG ) clock();
#endif

   hb_stackNewFrame( &sStackState, uiParams );

   hb_vmDoBlock();

#ifndef HB_NO_PROFILER
   if( bProfiler )
      hb_mthAddTime( clock() - ulClock );
#endif

#ifndef HB_NO_DEBUG
   if( sStackState.fDebugging )
      hb_vmDebuggerEndProc();
#endif

   hb_stackOldFrame( &sStackState );
}

static HARBOUR hb_vmDoBlock( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pBlock, pBase;
   int iParam;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoBlock()" ) );

   pBlock = hb_stackSelfItem();
   if( ! HB_IS_BLOCK( pBlock ) )
      hb_errInternal( HB_EI_VMNOTCBLOCK, NULL, "hb_vmDoBlock()", NULL );

   pBase = hb_stackBaseItem();

   /* set number of declared parameters */
   pBase->item.asSymbol.paramdeclcnt = pBlock->item.asBlock.paramcnt;
   /* set the current line number to a line where the codeblock was defined */
   pBase->item.asSymbol.stackstate->uiLineNo = pBlock->item.asBlock.lineno;
   /* set execution context for OOP scope */
   pBase->item.asSymbol.stackstate->uiClass  = pBlock->item.asBlock.hclass;
   pBase->item.asSymbol.stackstate->uiMethod = pBlock->item.asBlock.method;
   /* add missing parameters */
   iParam = pBlock->item.asBlock.paramcnt - pBase->item.asSymbol.paramcnt;
   while( --iParam >= 0 )
      hb_stackAllocItem()->type = HB_IT_NIL;
   /* set static base offset */
   hb_stackSetStaticsBase( pBlock->item.asBlock.value->pStatics );

   hb_vmExecute( pBlock->item.asBlock.value->pCode,
                 pBlock->item.asBlock.value->pSymbols );
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
 */
PHB_ITEM hb_vmEvalBlock( PHB_ITEM pBlock )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlock(%p)", pBlock ) );

   hb_vmPushEvalSym();
   hb_vmPush( pBlock );
   hb_vmSend( 0 );
   return hb_stackReturnItem();
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PHB_ITEM
 *
 * for example:
 *  retVal = hb_vmEvalBlockV( pBlock, 2, pParam1, pParam2 );
 */
PHB_ITEM hb_vmEvalBlockV( PHB_ITEM pBlock, HB_ULONG ulArgCount, ... )
{
   HB_STACK_TLS_PRELOAD
   va_list va;
   HB_ULONG i;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlockV(%p, %lu, ...)", pBlock, ulArgCount ) );

   hb_vmPushEvalSym();
   hb_vmPush( pBlock );

   va_start( va, ulArgCount );
   for( i = 1; i <= ulArgCount; i++ )
      hb_vmPush( va_arg( va, PHB_ITEM ) );
   va_end( va );

   /* take care here, possible loss of data long to short ... */
   /* added an explicit casting here for VC++ JFL */
   hb_vmSend( ( HB_USHORT ) ulArgCount );

   return hb_stackReturnItem();
}

/* Evaluates a passed codeblock item or macro pointer item
 */
PHB_ITEM hb_vmEvalBlockOrMacro( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmEvalBlockOrMacro(%p)", pItem ) );

   if( HB_IS_BLOCK( pItem ) )
   {
      hb_vmPushEvalSym();
      hb_vmPush( pItem );
      hb_vmEval( 0 );
   }
   else
   {
      PHB_MACRO pMacro = ( PHB_MACRO ) hb_itemGetPtr( pItem );
      if( pMacro )
      {
         hb_macroRun( pMacro );
         hb_stackPopReturn();
      }
      else
         hb_itemSetNil( hb_stackReturnItem() );
   }
   return hb_stackReturnItem();
}

/*
 * destroy codeblock or macro in given item
 */
void hb_vmDestroyBlockOrMacro( PHB_ITEM pItem )
{
   if( HB_IS_POINTER( pItem ) )
   {
      PHB_MACRO pMacro = ( PHB_MACRO ) hb_itemGetPtr( pItem );
      if( pMacro )
         hb_macroDelete( pMacro );
   }
   hb_itemRelease( pItem );
}



void hb_vmFunction( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFunction(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmDo( uiParams );
}


#ifndef HB_NO_DEBUG
static void hb_vmDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PHB_ITEM pFrame )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebugEntry" ) );

   switch( nMode )
   {
      case HB_DBG_MODULENAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_MODULENAME );
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmProc( 2 );
         break;

      case HB_DBG_LOCALNAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_LOCALNAME );
         hb_vmPushInteger( nIndex );
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmProc( 3 );
         break;

      case HB_DBG_STATICNAME:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_STATICNAME );
         hb_vmPush( pFrame );                   /* current static frame */
         hb_vmPushInteger( nIndex );            /* variable index */
         hb_vmPushString( szName, strlen( szName ) );
         hb_vmProc( 4 );
         break;

      case HB_DBG_SHOWLINE:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_SHOWLINE );
         hb_vmPushInteger( nLine );
         hb_vmProc( 2 );
         break;

      case HB_DBG_ENDPROC:
         hb_stackPushReturn();      /* saves the previous returned value */
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_ENDPROC );
         hb_vmProc( 1 );
         hb_stackPopReturn();       /* restores the previous returned value */
         break;

      case HB_DBG_GETENTRY:
         /* Try to get C dbgEntry() function pointer */
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_GETENTRY );
         hb_vmProc( 1 );
         break;

      case HB_DBG_VMQUIT:
         hb_vmPushDynSym( s_pDynsDbgEntry );
         hb_vmPushNil();
         hb_vmPushInteger( HB_DBG_VMQUIT );
         hb_vmPushInteger( nIndex );
         hb_vmProc( 2 );
         break;
   }
}

static void hb_vmDummyDebugEntry( int nMode, int nLine, const char * szName, int nIndex, PHB_ITEM pFrame )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDummyDebugEntry" ) );

   HB_SYMBOL_UNUSED( nMode );
   HB_SYMBOL_UNUSED( nLine );
   HB_SYMBOL_UNUSED( szName );
   HB_SYMBOL_UNUSED( nIndex );
   HB_SYMBOL_UNUSED( pFrame );
}

static void hb_vmDebuggerExit( HB_BOOL fRemove )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerExit(%d)", fRemove ) );

   /* is debugger linked ? */
   if( s_pFunDbgEntry )
   {
      /* inform debugger that we are quitting now */
      s_pFunDbgEntry( HB_DBG_VMQUIT, 0, NULL, fRemove ? 1 : 0, NULL );
      /* set dummy debugger function to avoid debugger activation in .prg
       *       destructors if any */
      if( fRemove )
         s_pFunDbgEntry = hb_vmDummyDebugEntry;
   }
}

static void hb_vmDebuggerEndProc( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerEndProc()" ) );

   s_pFunDbgEntry( HB_DBG_ENDPROC, 0, NULL, 0, NULL );
}

static void hb_vmDebuggerShowLine( HB_USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDebuggerShowLine(%hu)", uiLine ) );

   s_pFunDbgEntry( HB_DBG_SHOWLINE, uiLine, NULL, 0, NULL );
}

static void hb_vmLocalName( HB_USHORT uiLocal, const char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmLocalName(%hu, %s)", uiLocal, szLocalName ) );

   if( hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      s_pFunDbgEntry( HB_DBG_LOCALNAME, 0, szLocalName, uiLocal, NULL );
}

static void hb_vmStaticName( HB_BYTE bIsGlobal, HB_USHORT uiStatic, const char * szStaticName ) /* statics vars information for the debugger */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStaticName(%hu, %s)", uiStatic, szStaticName ) );

   HB_SYMBOL_UNUSED( bIsGlobal );

   if( hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      s_pFunDbgEntry( HB_DBG_STATICNAME, 0, szStaticName, uiStatic, ( PHB_ITEM ) hb_stackGetStaticsBase() );
}

static void hb_vmModuleName( const char * szModuleName ) /* PRG and function name information for the debugger */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmModuleName(%s)", szModuleName ) );

   if( s_pFunDbgEntry )
   {
      HB_STACK_TLS_PRELOAD
      s_pFunDbgEntry( HB_DBG_MODULENAME, 0, szModuleName, 0, NULL );
      hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging = HB_TRUE;
   }
}
#endif

static void hb_vmFrame( HB_USHORT usLocals, unsigned char ucParams )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pBase;
   int iTotal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFrame(%d, %d)", ( int ) usLocals, ( int ) ucParams ) );

   pBase = hb_stackBaseItem();

#if 0
   /* This old code which clears additional parameters to make space for
    * local variables without updating pBase->item.asSymbol.paramdeclcnt
    */
   iTotal = pBase->item.asSymbol.paramcnt - ucParams;
   if( iTotal > 0 )
   {
      pBase->item.asSymbol.paramcnt = ucParams;
      do
      {
         hb_itemClear( hb_stackItemFromTop( -iTotal ) );
      }
      while( --iTotal > 0 );
   }

   iTotal = usLocals + ucParams;
   if( iTotal )
   {
      iTotal -= pBase->item.asSymbol.paramcnt;
      while( --iTotal >= 0 )
         hb_vmPushNil();
   }
#else
   pBase->item.asSymbol.paramdeclcnt = ucParams;

   iTotal = ucParams - pBase->item.asSymbol.paramcnt;
   if( iTotal < 0 )
      iTotal = 0;
   iTotal += usLocals;

   if( iTotal )
   {
      do
      {
         hb_vmPushNil();
      }
      while( --iTotal > 0 );
   }
#endif
}

static void hb_vmVFrame( HB_USHORT usLocals, unsigned char ucParams )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pBase;
   int iTotal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmVFrame(%d, %d)", ( int ) usLocals, ( int ) ucParams ) );

   pBase = hb_stackBaseItem();

   pBase->item.asSymbol.paramdeclcnt = ucParams;

   iTotal = ucParams - pBase->item.asSymbol.paramcnt;
   if( iTotal < 0 )
      iTotal = 0;
   iTotal += usLocals;

   if( iTotal )
   {
      do
      {
         hb_vmPushNil();
      }
      while( --iTotal > 0 );
   }
}

static void hb_vmSFrame( PHB_SYMB pSym )      /* sets the statics frame for a function */
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSFrame(%p)", pSym ) );

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   hb_stackSetStaticsBase( pSym->value.pStaticsBase );
}

static void hb_vmStatics( PHB_SYMB pSym, HB_USHORT uiStatics ) /* initializes the global aStatics array or redimensionates it */
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmStatics(%p, %hu)", pSym, uiStatics ) );

   /* statics frame for this PRG */
   pSym->value.pStaticsBase = ( void * ) hb_itemArrayNew( uiStatics );
   pSym->scope.value |= HB_FS_FRAME;
}

#if defined( HB_MT_VM )
/*
 * extended thread static variable reference structure
 */
typedef struct
{
   HB_ITEM source;
   HB_TSD  threadData;
} HB_TSVREF, * PHB_TSVREF;

/*
 * extended thread static variable reference functions
 */
static PHB_ITEM hb_vmTSVRefRead( PHB_ITEM pRefer )
{
   PHB_TSVREF pTSVRef = ( PHB_TSVREF ) pRefer->item.asExtRef.value;
   PHB_ITEM pItem = ( PHB_ITEM ) hb_stackTestTSD( &pTSVRef->threadData );

   if( ! pItem )
   {
      pItem = ( PHB_ITEM ) hb_stackGetTSD( &pTSVRef->threadData );
      hb_itemCloneTo( pItem, &pTSVRef->source );
   }
   return pItem;
}

static PHB_ITEM hb_vmTSVRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   PHB_TSVREF pTSVRef = ( PHB_TSVREF ) pRefer->item.asExtRef.value;

   HB_SYMBOL_UNUSED( pSource );
   return ( PHB_ITEM ) hb_stackGetTSD( &pTSVRef->threadData );
}

static void hb_vmTSVRefCopy( PHB_ITEM pDest )
{
   hb_xRefInc( pDest->item.asExtRef.value );
}

static void hb_vmTSVRefClear( void * value )
{
   if( hb_xRefDec( value ) )
   {
      PHB_ITEM pItem;

      if( HB_IS_COMPLEX( &( ( PHB_TSVREF ) value )->source ) )
         hb_itemClear( &( ( PHB_TSVREF ) value )->source );

      pItem = ( PHB_ITEM ) hb_stackTestTSD( &( ( PHB_TSVREF ) value )->threadData );
      if( pItem && HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );

      hb_xfree( value );
   }
}

static void hb_vmTSVRefMark( void * value )
{
   PHB_ITEM pItem;

   if( HB_IS_GCITEM( &( ( PHB_TSVREF ) value )->source ) )
      hb_gcItemRef( &( ( PHB_TSVREF ) value )->source );

   pItem = ( PHB_ITEM ) hb_stackTestTSD( &( ( PHB_TSVREF ) value )->threadData );
   if( pItem && HB_IS_GCITEM( pItem ) )
      hb_gcItemRef( pItem );
}

/* destructor for terminated threads */
static void hb_vmTSVarClean( void * pThreadItem )
{
   if( HB_IS_COMPLEX( ( PHB_ITEM ) pThreadItem ) )
      hb_itemClear( ( PHB_ITEM ) pThreadItem );
}

/*
 * create extended thread static variable reference
 */
static void hb_vmTSVReference( PHB_ITEM pStatic )
{
   static const HB_EXTREF s_TSVExtRef = {
      hb_vmTSVRefRead,
      hb_vmTSVRefWrite,
      hb_vmTSVRefCopy,
      hb_vmTSVRefClear,
      hb_vmTSVRefMark
   };

   HB_STACK_TLS_PRELOAD
   PHB_TSVREF pTSVRef;
   PHB_ITEM pRefer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmTSVReference(%p)", pStatic ) );

   pTSVRef = ( PHB_TSVREF ) hb_xgrab( sizeof( HB_TSVREF ) );

   pTSVRef->source.type = HB_IT_NIL;
   HB_TSD_INIT( &pTSVRef->threadData, sizeof( HB_ITEM ), NULL, hb_vmTSVarClean );

   /* Use hb_stackReturnItem() as temporary item holder */
   pRefer = hb_stackReturnItem();
   if( HB_IS_COMPLEX( pRefer ) )
      hb_itemClear( pRefer );
   pRefer->type = HB_IT_BYREF | HB_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pTSVRef;
   pRefer->item.asExtRef.func = &s_TSVExtRef;

   hb_itemMove( &pTSVRef->source, pStatic );
   hb_itemMove( pStatic, pRefer );
}

static void hb_vmInitThreadStatics( HB_USHORT uiCount, const HB_BYTE * pCode )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInitThreadStatics(%hu,%p)", uiCount, pCode ) );

   while( uiCount-- )
   {
      HB_USHORT uiStatic = HB_PCODE_MKUSHORT( pCode );
      PHB_ITEM pStatic = ( ( PHB_ITEM ) hb_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
      hb_vmTSVReference( pStatic );
      pCode += 2;
   }
}
#else
static void hb_vmInitThreadStatics( HB_USHORT uiCount, const HB_BYTE * pCode )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInitThreadStatics(%hu,%p)", uiCount, pCode ) );

   /* single thread VM - do nothing, use normal static variables */

   HB_SYMBOL_UNUSED( uiCount );
   HB_SYMBOL_UNUSED( pCode );
}
#endif /* HB_MT_VM */

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void hb_vmPush( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPush(%p)", pItem ) );

   hb_itemCopy( hb_stackAllocItem(), pItem );
}

void hb_vmPushNil( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushNil()" ) );

   hb_stackAllocItem()->type = HB_IT_NIL;
}

void hb_vmPushLogical( HB_BOOL bValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLogical(%d)", ( int ) bValue ) );

   pItem = hb_stackAllocItem();
   pItem->type = HB_IT_LOGICAL;
   pItem->item.asLogical.value = bValue;
}

/* not used by HVM code */
void hb_vmPushNumber( double dNumber, int iDec )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushNumber(%lf, %d)", dNumber, iDec ) );

   if( iDec )
      hb_vmPushDouble( dNumber, iDec );

   else if( HB_DBL_LIM_INT( dNumber ) )
      hb_vmPushInteger( ( int ) dNumber );

   else if( HB_DBL_LIM_LONG( dNumber ) )
      hb_vmPushHBLong( ( HB_MAXINT ) dNumber );

   else
      hb_vmPushDouble( dNumber, hb_stackSetStruct()->HB_SET_DECIMALS );
}

static int hb_vmCalcIntWidth( HB_MAXINT nNumber )
{
   int iWidth;

   if( nNumber <= -1000000000L )
   {
      iWidth = 20;
   }
   else
   {
      iWidth = 10;
      while( nNumber >= 1000000000L )
      {
         iWidth++;
         nNumber /= 10;
      }
   }
   return iWidth;
}

void hb_vmPushInteger( int iNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushInteger(%d)", iNumber ) );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = HB_INT_LENGTH( iNumber );
}

#if HB_VMINT_MAX >= INT32_MAX
static void hb_vmPushIntegerConst( int iNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushIntegerConst(%d)", iNumber ) );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = iNumber;
   pItem->item.asInteger.length = ( HB_USHORT ) hb_vmCalcIntWidth( iNumber );
}
#else
static void hb_vmPushLongConst( long lNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLongConst(%ld)", lNumber ) );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_MAXINT ) lNumber;
   pItem->item.asLong.length = ( HB_USHORT ) hb_vmCalcIntWidth( lNumber );
}
#endif


void hb_vmPushLong( long lNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLong(%ld)", lNumber ) );

   HB_ITEM_PUT_LONGRAW( pItem, lNumber );
}

void hb_vmPushSize( HB_ISIZ nNumber )
{
#if HB_SIZE_MAX <= HB_VMUINT_MAX
   hb_vmPushInteger( ( int ) nNumber );
#else
   if( HB_LIM_INT( nNumber ) )
      hb_vmPushInteger( ( int ) nNumber );
   else
      hb_vmPushHBLong( nNumber );
#endif
}

static void hb_vmPushHBLong( HB_MAXINT nNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushHBLong(%" PFHL "d)", nNumber ) );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = nNumber;
   pItem->item.asLong.length = HB_LONG_LENGTH( nNumber );
}

#if ! defined( HB_LONG_LONG_OFF )
static void hb_vmPushLongLongConst( HB_LONGLONG llNumber )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLongLongConst(%" PFLL "d)", llNumber ) );

   pItem->type = HB_IT_LONG;
   pItem->item.asLong.value = ( HB_MAXINT ) llNumber;
   pItem->item.asLong.length = ( HB_USHORT ) hb_vmCalcIntWidth( llNumber );
}
#endif

void hb_vmPushNumInt( HB_MAXINT nNumber )
{
   if( HB_LIM_INT( nNumber ) )
      hb_vmPushInteger( ( int ) nNumber );
   else
      hb_vmPushHBLong( nNumber );
}

void hb_vmPushDouble( double dNumber, int iDec )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDouble(%lf, %d)", dNumber, iDec ) );

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.value = dNumber;
   pItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   if( iDec == HB_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = ( HB_USHORT ) hb_stackSetStruct()->HB_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = ( HB_USHORT ) iDec;
}

static void hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   pItem->type = HB_IT_DOUBLE;
   pItem->item.asDouble.value = dNumber;

   if( iDec == HB_DEFAULT_DECIMALS )
      pItem->item.asDouble.decimal = ( HB_USHORT ) hb_stackSetStruct()->HB_SET_DECIMALS;
   else
      pItem->item.asDouble.decimal = ( HB_USHORT ) iDec;

   if( iWidth == HB_DEFAULT_WIDTH )
      pItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   else
      pItem->item.asDouble.length = ( HB_USHORT ) iWidth;
}

void hb_vmPushDate( long lDate )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDate(%ld)", lDate ) );

   pItem->type = HB_IT_DATE;
   pItem->item.asDateTime.julian = lDate;
   pItem->item.asDateTime.time = 0;
}

void hb_vmPushTimeStamp( long lJulian, long lMilliSec )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushTimeStamp(%ld, %ld)", lJulian, lMilliSec ) );

   pItem->type = HB_IT_TIMESTAMP;
   pItem->item.asDateTime.julian = lJulian;
   pItem->item.asDateTime.time = lMilliSec;
}

void hb_vmPushPointer( void * pPointer )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushPointer(%p)", pPointer ) );

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pPointer;
   pItem->item.asPointer.collect =
   pItem->item.asPointer.single = HB_FALSE;
}

void hb_vmPushPointerGC( void * pPointer )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushPointerGC(%p)", pPointer ) );

   pItem->type = HB_IT_POINTER;
   pItem->item.asPointer.value = pPointer;
   pItem->item.asPointer.collect = HB_TRUE;
   pItem->item.asPointer.single = HB_FALSE;

   hb_gcAttach( pPointer );
}

void hb_vmPushString( const char * szText, HB_SIZE nLength )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushString(%s, %" HB_PFS "u)", szText, nLength ) );

   hb_itemPutCL( hb_stackAllocItem(), szText, nLength );
}

void hb_vmPushStringPcode( const char * szText, HB_SIZE nLength )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushStringPcode(%s, %" HB_PFS "u)", szText, nLength ) );

   pItem->type = HB_IT_STRING;
   pItem->item.asString.length    = nLength;
   pItem->item.asString.allocated = 0;
   pItem->item.asString.value     = ( char * ) szText;
}

void hb_vmPushSymbol( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushSymbol(%p)", pSym ) );

   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value = pSym;
   pItem->item.asSymbol.stackstate = NULL;
}

void hb_vmPushDynSym( PHB_DYNS pDynSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushDynSym(%p)", pDynSym ) );

   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value = pDynSym->pSymbol;
   pItem->item.asSymbol.stackstate = NULL;
}

void hb_vmPushEvalSym( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushEvalSym()" ) );

   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value = &hb_symEval;
   pItem->item.asSymbol.stackstate = NULL;
}

/* -3    -> HB_P_PUSHBLOCK
 * -2 -1 -> size of codeblock
 *  0 +1 -> number of expected parameters
 * +2 +3 -> number of referenced local variables
 * +4    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlock( const HB_BYTE * pCode, PHB_SYMB pSymbols, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiLocals;
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushBlock(%p,%p,%" HB_PFS "u)", pCode, pSymbols, nLen ) );

   uiLocals = HB_PCODE_MKUSHORT( &pCode[ 2 ] );

   if( nLen )
      nLen -= uiLocals << 1;

   pItem->item.asBlock.value =
      hb_codeblockNew( pCode + 4 + ( uiLocals << 1 ),   /* pcode buffer         */
                       uiLocals,                        /* number of referenced local variables */
                       pCode + 4,                       /* table with referenced local variables */
                       pSymbols,
                       nLen );

   pItem->type = HB_IT_BLOCK;
   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = HB_PCODE_MKUSHORT( pCode );
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -2    -> HB_P_PUSHBLOCKSHORT
 * -1    -> size of codeblock
 *  0    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlockShort( const HB_BYTE * pCode, PHB_SYMB pSymbols, HB_SIZE nLen )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushBlockShort(%p,%p,%" HB_PFS "u)", pCode, pSymbols, nLen ) );

   pItem->item.asBlock.value =
      hb_codeblockNew( pCode,                       /* pcode buffer         */
                       0,                           /* number of referenced local variables */
                       NULL,                        /* table with referenced local variables */
                       pSymbols,
                       nLen );

   pItem->type = HB_IT_BLOCK;

   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = 0;
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* -(5|6)     -> HB_P_MPUSHBLOCK[LARGE]
 * [-5] -4 -3 -> size of codeblock
 * -2 -1      -> number of expected parameters
 * +0         -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void hb_vmPushMacroBlock( const HB_BYTE * pCode, HB_SIZE nSize, HB_USHORT usParams )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushMacroBlock(%p,%" HB_PFS "u,%hu)", pCode, nSize, usParams ) );

   pItem->item.asBlock.value = hb_codeblockMacroNew( pCode, nSize );
   pItem->type = HB_IT_BLOCK;

   /* store the number of expected parameters
    */
   pItem->item.asBlock.paramcnt = usParams;
   /* store the line number where the codeblock was defined
    */
   pItem->item.asBlock.lineno = hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo;
   pItem->item.asBlock.hclass = hb_stackBaseItem()->item.asSymbol.stackstate->uiClass;
   pItem->item.asBlock.method = hb_stackBaseItem()->item.asSymbol.stackstate->uiMethod;
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem = hb_stackAllocItem();

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAlias()" ) );

   pItem->type = HB_IT_INTEGER;
   pItem->item.asInteger.value = hb_rddGetCurrentWorkAreaNumber();
   pItem->item.asInteger.length = 10;
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void hb_vmPushAliasedField( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pAlias;
   int iCurrArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAliasedField(%p)", pSym ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   pAlias = hb_stackItemFromTop( -1 );

   /*
    * NOTE: hb_vmSelecWorkarea clears passed item
    */
   if( hb_vmSelectWorkarea( pAlias, pSym ) == HB_SUCCESS )
      hb_rddGetFieldValue( pAlias, pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of either a field or a memvar based on alias value
 * (for performance reason it replaces alias value with field value)
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPushAliasedVar( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushAliasedVar(%p)", pSym ) );

   if( HB_IS_STRING( pAlias ) )
   {
      const char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( pAlias->item.asString.length == 1 || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarGetValue( pAlias, pSym );
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( hb_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                       pAlias->item.asString.length ) == 0 ||
                 hb_strnicmp( szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                       pAlias->item.asString.length ) == 0 ) )
      {
         hb_rddGetFieldValue( pAlias, pSym );
         return;
      }
   }
   hb_vmPushAliasedField( pSym );
}

static void hb_vmPushLocal( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLocal(%d)", iLocal ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetRef( hb_stackSelfItem()->item.asBlock.value, iLocal );
   }

   hb_itemCopy( hb_stackAllocItem(),
                HB_IS_BYREF( pLocal ) ? hb_itemUnRef( pLocal ) : pLocal );
}

static void hb_vmPushLocalByRef( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pTop;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushLocalByRef(%d)", iLocal ) );

   pTop = hb_stackAllocItem();
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   if( iLocal >= 0 )
   {
      PHB_ITEM pLocal = hb_stackLocalVariableAt( &iLocal );
      if( HB_IS_BYREF( pLocal ) && ! HB_IS_ENUM( pLocal ) )
      {
         hb_itemCopy( pTop, pLocal );
         return;
      }
      pTop->item.asRefer.BasePtr.itemsbasePtr = hb_stackItemBasePtr();
   }
   else
   {
      /* store direct codeblock address because an item where a codeblock
       * is stored can be no longer placed on the eval stack at the time
       * of a codeblock evaluation or variable access
       */
      pTop->item.asRefer.BasePtr.block = hb_stackSelfItem()->item.asBlock.value;
   }
   pTop->type = HB_IT_BYREF;
   pTop->item.asRefer.value = iLocal;
   pTop->item.asRefer.offset = hb_stackBaseOffset();
}

static void hb_vmPushStatic( HB_USHORT uiStatic )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pStatic;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushStatic(%hu)", uiStatic ) );

   pStatic = ( ( PHB_ITEM ) hb_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
   hb_itemCopy( hb_stackAllocItem(),
                HB_IS_BYREF( pStatic ) ? hb_itemUnRef( pStatic ) : pStatic );
}

static void hb_vmPushStaticByRef( HB_USHORT uiStatic )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pTop, pBase;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushStaticByRef(%hu)", uiStatic ) );

   pTop = hb_stackAllocItem();
   pBase = ( PHB_ITEM ) hb_stackGetStaticsBase();

   if( HB_IS_BYREF( pBase->item.asArray.value->pItems + uiStatic - 1 ) &&
       ! HB_IS_ENUM( pBase->item.asArray.value->pItems + uiStatic - 1 ) )
   {
      hb_itemCopy( pTop, pBase->item.asArray.value->pItems + uiStatic - 1 );
      return;
   }
   pTop->type = HB_IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value = uiStatic - 1;
   pTop->item.asRefer.offset = 0;    /* 0 for static variables */
   pTop->item.asRefer.BasePtr.array = pBase->item.asArray.value;
   hb_gcRefInc( pBase->item.asArray.value );
}

static void hb_vmPushVariable( PHB_SYMB pVarSymb )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_INFO, ( "(hb_vmPushVariable)" ) );

   pItem = hb_stackAllocItem();

   /* First try if passed symbol is a name of field
    * in a current workarea - if it is not a field (HB_FAILURE)
    * then try the memvar variable
    */
   if( hb_rddFieldGet( pItem, pVarSymb ) != HB_SUCCESS &&
       hb_memvarGet( pItem, pVarSymb ) != HB_SUCCESS )
   {
      PHB_ITEM pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                      NULL, pVarSymb->szName,
                                      0, EF_CANRETRY );

      while( hb_errLaunch( pError ) == E_RETRY )
      {
         if( hb_rddFieldGet( pItem, pVarSymb ) == HB_SUCCESS ||
             hb_memvarGet( pItem, pVarSymb ) == HB_SUCCESS )
            break;
      }

      hb_errRelease( pError );
   }
}


static void hb_vmDuplicate( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDuplicate()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_itemCopy( hb_stackAllocItem(), pItem );
}

static void hb_vmDuplUnRef( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDuplUnRef()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_itemCopy( hb_stackAllocItem(), pItem );
   if( HB_IS_BYREF( pItem ) )
      hb_itemCopy( pItem, hb_itemUnRef( pItem ) );
}

static void hb_vmPushUnRef( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushUnRef()" ) );

   pItem = hb_stackItemFromTop( -1 );
   hb_itemCopy( hb_stackAllocItem(),
                HB_IS_BYREF( pItem ) ? hb_itemUnRef( pItem ) : pItem );
}

static void hb_vmSwap( int iCount )
{
   HB_STACK_TLS_PRELOAD
   int i = -1;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmSwap(%d)", iCount ) );

   do
   {
      hb_itemSwap( hb_stackItemFromTop( i ), hb_stackItemFromTop( i - 1 ) );
      --i;
   }
   while( iCount-- );
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

static HB_BOOL hb_vmPopLogical( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopLogical()" ) );

   if( HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
   {
      HB_BOOL fValue = hb_stackItemFromTop( -1 )->item.asLogical.value;

      hb_stackDec();
      return fValue;
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ), 1, hb_stackItemFromTop( -1 ) );
      return HB_FALSE;
   }
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAlias()" ) );

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   hb_stackDec();
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into a given field
 */
static void hb_vmPopAliasedField( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   int iCurrArea;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAliasedField(%p)", pSym ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if( hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), pSym ) == HB_SUCCESS )
      hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
   hb_stackDec();    /* alias - it was cleared in hb_vmSelectWorkarea */
   hb_stackPop();    /* field value */
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into either a field or a memvar based on the alias value
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPopAliasedVar( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopAliasedVar(%p)", pSym ) );

   /*
    * "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
    */
   if( HB_IS_STRING( pAlias ) )
   {
      const char * szAlias = pAlias->item.asString.value;

      if( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' )
      {
         if( pAlias->item.asString.length == 1 || /* M->variable */
             ( pAlias->item.asString.length >= 4 &&
               hb_strnicmp( szAlias, "MEMVAR", /* MEMVAR-> or MEMVA-> or MEMV-> */
                                     pAlias->item.asString.length ) == 0 ) )
         {
            hb_memvarSetValue( pSym, hb_stackItemFromTop( -2 ) );
            hb_stackPop();    /* alias */
            hb_stackPop();    /* value */
            return;
         }
      }
      else if( pAlias->item.asString.length >= 4 &&
               ( hb_strnicmp( szAlias, "FIELD", /* FIELD-> or FIEL-> */
                                     pAlias->item.asString.length ) == 0 ||
                 hb_strnicmp( szAlias, "_FIELD", /* _FIELD-> or _FIE-> */
                                       pAlias->item.asString.length ) == 0 ) )
      {
         hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );
         hb_stackPop();    /* alias */
         hb_stackPop();    /* value */
         return;
      }
   }
   hb_vmPopAliasedField( pSym );
}

static void hb_vmPopLocal( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal, pVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopLocal(%d)", iLocal ) );

   pVal = hb_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetRef( hb_stackSelfItem()->item.asBlock.value, iLocal );
   }

   hb_itemMoveToRef( pLocal, pVal );

   hb_stackDec();
}

static void hb_vmPopStatic( HB_USHORT uiStatic )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pStatic, pVal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPopStatic(%hu)", uiStatic ) );

   pVal = hb_stackItemFromTop( -1 );

   /* Remove MEMOFLAG if exists (assignment from field). */
   pVal->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
   pStatic = ( ( PHB_ITEM ) hb_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;

   hb_itemMoveToRef( pStatic, pVal );
   hb_stackDec();
}

/* ----------------------------------------------- */
/*
 * Functions to manage module symbols
 */

PHB_SYMB hb_vmGetRealFuncSym( PHB_SYMB pSym )
{
   if( pSym && ! ( pSym->scope.value & HB_FS_LOCAL ) )
   {
      pSym = pSym->pDynSym &&
           ( pSym->pDynSym->pSymbol->scope.value & HB_FS_LOCAL ) ?
             pSym->pDynSym->pSymbol : NULL;
   }

   return pSym;
}

HB_BOOL hb_vmLockModuleSymbols( void )
{
#if defined( HB_MT_VM )
   return ! s_pSymbolsMtx || hb_threadMutexLock( s_pSymbolsMtx );
#else
   return HB_TRUE;
#endif /* HB_MT_VM */
}

void hb_vmUnlockModuleSymbols( void )
{
#if defined( HB_MT_VM )
   if( s_pSymbolsMtx )
      hb_threadMutexUnlock( s_pSymbolsMtx );
#endif /* HB_MT_VM */
}

const char * hb_vmFindModuleSymbolName( PHB_SYMB pSym )
{
   if( pSym )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;

      while( pLastSymbols )
      {
         if( pSym >= pLastSymbols->pModuleSymbols &&
             pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols )
         {
            return pLastSymbols->szModuleName;
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }
   return NULL;
}

HB_BOOL hb_vmFindModuleSymbols( PHB_SYMB pSym, PHB_SYMB * pSymbols,
                                HB_USHORT * puiSymbols )
{
   if( pSym )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;

/*
      if( pSym->scope.value & HB_FS_PCODEFUNC )
         * pSymbols = pSym->value.pCodeFunc->pSymbols;
*/

      while( pLastSymbols )
      {
         if( pLastSymbols->fActive &&
             pSym >= pLastSymbols->pModuleSymbols &&
             pSym < pLastSymbols->pModuleSymbols + pLastSymbols->uiModuleSymbols )
         {
            *pSymbols   = pLastSymbols->pModuleSymbols;
            *puiSymbols = pLastSymbols->uiModuleSymbols;
            return HB_TRUE;
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }

   *pSymbols   = NULL;
   *puiSymbols = 0;
   return HB_FALSE;
}

PHB_SYMB hb_vmFindFuncSym( const char * szFuncName, void * hDynLib )
{
   static PHB_SYMB pFuncSym = NULL;

   if( szFuncName )
   {
      PHB_SYMBOLS pSymbols = s_pSymbols;

      while( pSymbols )
      {
         if( pSymbols->fActive && pSymbols->hDynLib == hDynLib )
         {
            HB_USHORT ui;

            for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
            {
               PHB_SYMB pSymbol = &pSymbols->pModuleSymbols[ ui ];

               if( ( pSymbol->scope.value & HB_FS_LOCAL ) != 0 &&
                   hb_stricmp( pSymbol->szName, szFuncName ) == 0 )
               {
                  if( ( pSymbol->scope.value & HB_FS_STATIC ) == 0 )
                     return pSymbol;
                  else if( ! pFuncSym )
                     pFuncSym = pSymbol;
               }
            }
         }
         pSymbols = pSymbols->pNext;
      }
   }

   return pFuncSym;
}

#define HB_SYM_STATICSBASE( p )  \
   ( ( PHB_ITEM ) ( ( ( p )->scope.value & HB_FS_FRAME ) ? \
                    ( p )->value.pStaticsBase : NULL ) )

static void hb_vmStaticsClear( void )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      if( pLastSymbols->uiStaticsOffset )
      {
         PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
         PHB_ITEM pStatics = HB_SYM_STATICSBASE( pSym );
         if( pStatics )
         {
            HB_SIZE nLen = hb_arrayLen( pStatics ), ul;

            for( ul = 1; ul <= nLen; ++ul )
            {
               PHB_ITEM pItem = hb_arrayGetItemPtr( pStatics, ul );
               if( HB_IS_COMPLEX( pItem ) )
                  hb_itemClear( pItem );
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void hb_vmStaticsRelease( void )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   while( pLastSymbols )
   {
      if( pLastSymbols->uiStaticsOffset )
      {
         PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
         PHB_ITEM pStatics = HB_SYM_STATICSBASE( pSym );
         if( pStatics )
         {
            hb_itemRelease( pStatics );
            pSym->value.pStaticsBase = NULL;
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static HB_SIZE hb_vmStaticsCount( void )
{
   HB_SIZE nStatics = 0;

   if( hb_vmLockModuleSymbols() )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;
      while( pLastSymbols )
      {
         if( pLastSymbols->uiStaticsOffset )
         {
            PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
            PHB_ITEM pStatics = HB_SYM_STATICSBASE( pSym );
            if( pStatics )
               nStatics += hb_arrayLen( pStatics );
         }
         pLastSymbols = pLastSymbols->pNext;
      }
      hb_vmUnlockModuleSymbols();
   }

   return nStatics;
}

static PHB_ITEM hb_vmStaticsArray( void )
{
   PHB_ITEM pArray = NULL;

   if( hb_vmLockModuleSymbols() )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;
      HB_SIZE nOffset, nCount;

      nCount = hb_vmStaticsCount();
      pArray = hb_itemArrayNew( nCount );
      nOffset = 0;

      while( pLastSymbols )
      {
         if( pLastSymbols->uiStaticsOffset )
         {
            PHB_SYMB pSym = pLastSymbols->pModuleSymbols + pLastSymbols->uiStaticsOffset;
            PHB_ITEM pStatics = HB_SYM_STATICSBASE( pSym );
            if( pStatics )
            {
               HB_SIZE nLen = hb_arrayLen( pStatics ), n;

               for( n = 1; n <= nLen; ++n )
                  hb_arraySet( pArray, ++nOffset, hb_arrayGetItemPtr( pStatics, n ) );
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
      hb_vmUnlockModuleSymbols();
   }

   return pArray;
}

static PHB_SYMBOLS hb_vmFindFreeModule( PHB_SYMB pSymbols, HB_USHORT uiSymbols,
                                        const char * szModuleName, HB_ULONG ulID )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFindFreeModule(%p,%hu,%s,%lu)", pSymbols, uiSymbols, szModuleName, ulID ) );

   if( s_ulFreeSymbols )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;

      while( pLastSymbols )
      {
         if( ! pLastSymbols->fActive &&
             pLastSymbols->ulID == ulID &&
             pLastSymbols->uiModuleSymbols == uiSymbols &&
             pLastSymbols->szModuleName != NULL &&
             strcmp( pLastSymbols->szModuleName, szModuleName ) == 0 )
         {
            PHB_SYMB pModuleSymbols = pLastSymbols->pModuleSymbols;
            HB_USHORT ui;

            for( ui = 0; ui < uiSymbols; ++ui )
            {
               if( ( ( pSymbols[ ui ].scope.value & ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE | HB_FS_DEFERRED ) ) !=
                     ( pModuleSymbols[ ui ].scope.value & ~HB_FS_DEFERRED ) &&
                     ! ( ui != 0 && ui == pLastSymbols->uiStaticsOffset &&
                         HB_SYM_STATICSBASE( &pModuleSymbols[ ui ] ) ) ) ||
                   strcmp( pSymbols[ ui ].szName, pModuleSymbols[ ui ].szName ) != 0 )
               {
                  break;
               }
            }
            if( ui == uiSymbols )
            {
               --s_ulFreeSymbols;
               return pLastSymbols;
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }

   return NULL;
}

void hb_vmFreeSymbols( PHB_SYMBOLS pSymbols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmFreeSymbols(%p)", pSymbols ) );

   if( pSymbols->fActive && hb_vmLockModuleSymbols() )
   {
      if( pSymbols->fActive )
      {
         HB_USHORT ui;

         for( ui = 0; ui < pSymbols->uiModuleSymbols; ++ui )
         {
            PHB_SYMB pSymbol = &pSymbols->pModuleSymbols[ ui ];

            /* do not overwrite already initialized statics' frame */
            if( ui == 0 || ui != pSymbols->uiStaticsOffset ||
                ! HB_SYM_STATICSBASE( pSymbol ) )
            {
               pSymbol->value.pFunPtr = NULL;
               if( pSymbol->pDynSym && pSymbol->pDynSym->pSymbol != pSymbol &&
                   ( pSymbol->scope.value & HB_FS_LOCAL ) == 0 )
                  pSymbol->scope.value |= HB_FS_DEFERRED;
               pSymbol->scope.value &= ~( HB_FS_PCODEFUNC | HB_FS_DYNCODE );
            }
         }
         pSymbols->hDynLib = NULL;
         pSymbols->fActive = HB_FALSE;
         ++s_ulFreeSymbols;
      }
      hb_vmUnlockModuleSymbols();
   }
}

void hb_vmBeginSymbolGroup( void * hDynLib, HB_BOOL fClone )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmBeginSymbolGroup(%p,%d)", hDynLib, ( int ) fClone ) );

   s_hDynLibID = hDynLib;
   s_fCloneSym = fClone;
}

void hb_vmInitSymbolGroup( void * hNewDynLib, int argc, const char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmInitSymbolGroup(%p,%d,%p)", hNewDynLib, argc, argv ) );

   s_fCloneSym = HB_FALSE;

   if( s_hDynLibID )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;
      void * hDynLib = s_hDynLibID;
      HB_BOOL fFound = HB_FALSE;
      HB_USHORT ui;

      s_hDynLibID = NULL;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = HB_TRUE;

            if( pLastSymbols->fInitStatics && pLastSymbols->fActive )
            {
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                  if( scope == HB_FS_INITEXIT &&
                      ! ( ui != 0 && ui == pLastSymbols->uiStaticsOffset &&
                          HB_SYM_STATICSBASE( pLastSymbols->pModuleSymbols + ui ) ) )
                  {
                     hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                     hb_vmPushNil();
                     hb_vmProc( 0 );
                  }
               }
               pLastSymbols->fInitStatics = HB_FALSE;
            }

            pLastSymbols->hDynLib = hNewDynLib;
         }
         pLastSymbols = pLastSymbols->pNext;
      }

      if( fFound )
      {
         HB_BOOL fClipInit = HB_TRUE;

         do
         {
            pLastSymbols = s_pSymbols;
            while( pLastSymbols && hb_vmRequestQuery() == 0 )
            {
               if( pLastSymbols->hDynLib == hNewDynLib )
               {
                  if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_INIT ) != 0 )
                  {
                     ui = pLastSymbols->uiModuleSymbols;
                     while( ui-- )
                     {
                        HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                        if( scope == HB_FS_INIT &&
                            ( strcmp( ( pLastSymbols->pModuleSymbols + ui )->szName,
                                      "CLIPINIT$" ) == 0 ? fClipInit : ! fClipInit ) )
                        {
                           int i;
                           hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                           hb_vmPushNil();
                           for( i = 0; i < argc; ++i )
                           {
                              hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
                           }
                           hb_vmProc( ( HB_USHORT ) argc );
                           if( hb_vmRequestQuery() != 0 )
                              break;
                        }
                     }
                  }
               }
               pLastSymbols = pLastSymbols->pNext;
            }
            fClipInit = ! fClipInit;
         }
         while( ! fClipInit );
      }
   }
}

void hb_vmExitSymbolGroup( void * hDynLib )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmExitSymbolGroup(%p)", hDynLib ) );

   if( hDynLib )
   {
      PHB_SYMBOLS pLastSymbols = s_pSymbols;
      HB_BOOL fFound = HB_FALSE;

      while( pLastSymbols )
      {
         if( pLastSymbols->hDynLib == hDynLib )
         {
            fFound = HB_TRUE;
            if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_EXIT ) != 0 )
            {
               HB_USHORT ui;
               for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
               {
                  HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

                  if( scope == HB_FS_EXIT )
                  {
                     hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                     hb_vmPushNil();
                     hb_vmProc( 0 );
                  }
               }
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }

      if( fFound )
      {
         pLastSymbols = s_pSymbols;
         while( pLastSymbols )
         {
            if( pLastSymbols->hDynLib == hDynLib )
            {
               hb_vmFreeSymbols( pLastSymbols );
            }
            pLastSymbols = pLastSymbols->pNext;
         }
      }
   }
}

PHB_SYMBOLS hb_vmRegisterSymbols( PHB_SYMB pModuleSymbols, HB_USHORT uiSymbols,
                                  const char * szModuleName, HB_ULONG ulID,
                                  HB_BOOL fDynLib, HB_BOOL fClone )
{
   PHB_SYMBOLS pNewSymbols;
   HB_BOOL fRecycled, fInitStatics = HB_FALSE;
   HB_USHORT ui;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRegisterSymbols(%p,%hu,%s,%lu,%d,%d)", pModuleSymbols, uiSymbols, szModuleName, ulID, ( int ) fDynLib, ( int ) fClone ) );

   pNewSymbols = s_ulFreeSymbols == 0 ? NULL :
                 hb_vmFindFreeModule( pModuleSymbols, uiSymbols, szModuleName, ulID );

   if( pNewSymbols )
   {
      pNewSymbols->fActive = fRecycled = HB_TRUE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
   }
   else
   {
      fRecycled = HB_FALSE;

      if( fClone )
      {
         HB_SIZE nSymSize = uiSymbols * sizeof( HB_SYMB );
         HB_SIZE nSize;
         char * buffer;

         nSize = nSymSize;
         for( ui = 0; ui < uiSymbols; ui++ )
            nSize += strlen( pModuleSymbols[ ui ].szName ) + 1;
         buffer = ( char * ) memcpy( hb_xgrab( nSize ), pModuleSymbols, nSymSize );
         pModuleSymbols = ( PHB_SYMB ) buffer;
         for( ui = 0; ui < uiSymbols; ui++ )
         {
            buffer += nSymSize;
            nSymSize = strlen( pModuleSymbols[ ui ].szName ) + 1;
            memcpy( buffer, pModuleSymbols[ ui ].szName, nSymSize );
            pModuleSymbols[ ui ].szName = buffer;
         }
      }

      pNewSymbols = ( PHB_SYMBOLS ) hb_xgrab( sizeof( HB_SYMBOLS ) );
      pNewSymbols->pModuleSymbols = pModuleSymbols;
      pNewSymbols->uiModuleSymbols = uiSymbols;
      pNewSymbols->uiStaticsOffset = 0;
      pNewSymbols->szModuleName = hb_strdup( szModuleName );
      pNewSymbols->ulID = ulID;
      pNewSymbols->fAllocated = fClone;
      pNewSymbols->fActive = HB_TRUE;
      pNewSymbols->fInitStatics = HB_FALSE;
      pNewSymbols->hDynLib = s_hDynLibID;
      pNewSymbols->hScope = 0;
      pNewSymbols->pNext = NULL;

      if( s_pSymbols == NULL )
      {
         s_pSymbols = pNewSymbols;
      }
      else
      {
         PHB_SYMBOLS pLastSymbols = s_pSymbols;

         while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
            pLastSymbols = pLastSymbols->pNext;
         pLastSymbols->pNext = pNewSymbols;
      }
   }

   for( ui = 0; ui < uiSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      PHB_SYMB pSymbol = pNewSymbols->pModuleSymbols + ui;
      HB_SYMBOLSCOPE hSymScope;
      HB_BOOL fPublic, fStatics;

      fStatics = ( pSymbol->scope.value & HB_FS_INITEXIT ) == HB_FS_INITEXIT ||
                 ( fRecycled && ui != 0 && ui == pNewSymbols->uiStaticsOffset &&
                   HB_SYM_STATICSBASE( pSymbol ) );

      if( fRecycled && ! fStatics )
      {
         pSymbol->value.pFunPtr = ( pModuleSymbols + ui )->value.pFunPtr;
         pSymbol->scope.value = ( pModuleSymbols + ui )->scope.value;
      }
      if( fDynLib )
      {
         pSymbol->scope.value |= HB_FS_DYNCODE;
      }

      hSymScope = pSymbol->scope.value;
      pNewSymbols->hScope |= hSymScope;
      /* fPublic = ( hSymScope & ( HB_FS_PUBLIC | HB_FS_MESSAGE | HB_FS_MEMVAR ) ) != 0; */
      fPublic = ( hSymScope & ( HB_FS_INITEXIT | HB_FS_STATIC | HB_FS_FRAME ) ) == 0;
      if( fStatics )
      {
         if( ! fRecycled && strncmp( pSymbol->szName, "(_INITSTATICS", 13 ) == 0 )
            pNewSymbols->uiStaticsOffset = ui;
         fInitStatics = HB_TRUE;
      }

      if( ( hSymScope & ( HB_FS_PCODEFUNC | HB_FS_LOCAL | HB_FS_FRAME ) ) ==
          ( HB_FS_PCODEFUNC | HB_FS_LOCAL ) && ( fRecycled || fClone ) )
      {
         pSymbol->value.pCodeFunc->pSymbols = pNewSymbols->pModuleSymbols;
      }

      if( ! s_pSymStart && ! fDynLib && ! fStatics &&
          ( hSymScope & HB_FS_FIRST ) != 0 &&
          ( hSymScope & HB_FS_INITEXIT ) == 0 )
      {
         /* first public defined symbol to start execution */
         s_pSymStart = pSymbol;
      }

      /* Enable this code to see static functions which are registered in global dynsym table */
#if 0
      if( fPublic && ( hSymScope & ( HB_FS_INITEXIT | HB_FS_STATIC ) ) != 0 )
      {
         HB_TRACE( HB_TR_DEBUG, ( "Registering: %s:%s scope %04x", szModuleName, pSymbol->szName, hSymScope ) );
      }
#endif

      if( fPublic )
      {
         if( fDynLib && HB_VM_ISFUNC( pSymbol ) )
         {
            PHB_DYNS pDynSym;

            pDynSym = hb_dynsymFind( pSymbol->szName );

            if( pDynSym )
            {
               pSymbol->pDynSym = pDynSym;
               if( pDynSym->pSymbol != pSymbol && HB_VM_ISFUNC( pDynSym->pSymbol ) &&
                   ( pDynSym->pSymbol->value.pFunPtr != pSymbol->value.pFunPtr ||
                     ( pDynSym->pSymbol->scope.value & HB_FS_LOCAL ) != 0 ||
                     ( ( pSymbol->scope.value & ( HB_FS_LOCAL | HB_FS_DYNCODE ) ) !=
                       ( HB_FS_LOCAL | HB_FS_DYNCODE ) ) ) )
               {
                  pSymbol->scope.value =
                     ( pSymbol->scope.value & ~( HB_FS_PCODEFUNC | HB_FS_LOCAL ) ) |
                     ( pDynSym->pSymbol->scope.value & HB_FS_PCODEFUNC );
                  pSymbol->value.pFunPtr = pDynSym->pSymbol->value.pFunPtr;
               }
               else
               {
                  pDynSym->pSymbol = pSymbol;
               }
               continue;
            }
         }

         hb_dynsymNew( pSymbol );
      }
   }

   if( ! fRecycled )
   {
      pNewSymbols->fInitStatics = fInitStatics;
   }

   return pNewSymbols;
}

static void hb_vmVerifyPCodeVersion( const char * szModuleName, HB_USHORT uiPCodeVer )
{
   if( uiPCodeVer != 0 )
   {
      if( uiPCodeVer > HB_PCODE_VER ||    /* the module is compiled with newer compiler version then HVM */
          uiPCodeVer < HB_PCODE_VER_MIN ) /* the module is compiled with old not longer supported by HVM compiler version */
      {
         char szPCode[ 10 ];
         hb_snprintf( szPCode, sizeof( szPCode ), "%i.%i", uiPCodeVer >> 8, uiPCodeVer & 0xff );

         hb_errInternal( HB_EI_ERRUNRECOV, "Module '%s'\n"
                         "was compiled with unsupported PCODE version %s.\n"
                         "Please recompile.", szModuleName, szPCode );
      }
   }

}

/*
 * module symbols initialization with extended information
 */
PHB_SYMB hb_vmProcessSymbols( PHB_SYMB pSymbols, HB_USHORT uiModuleSymbols,
                              const char * szModuleName, HB_ULONG ulID,
                              HB_USHORT uiPCodeVer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmProcessSymbols(%p,%hu,%s,%lu,%hu)", pSymbols, uiModuleSymbols, szModuleName, ulID, uiPCodeVer ) );

   hb_vmVerifyPCodeVersion( szModuleName, uiPCodeVer );
   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModuleName, ulID,
                                s_fCloneSym, s_fCloneSym )->pModuleSymbols;
}

PHB_SYMB hb_vmProcessDynLibSymbols( PHB_SYMB pSymbols, HB_USHORT uiModuleSymbols,
                                    const char * szModuleName, HB_ULONG ulID,
                                    HB_USHORT uiPCodeVer )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmProcessDynLibSymbols(%p,%hu,%s,%lu,%hu)", pSymbols, uiModuleSymbols, szModuleName, ulID, uiPCodeVer ) );

   hb_vmVerifyPCodeVersion( szModuleName, uiPCodeVer );
   return hb_vmRegisterSymbols( pSymbols, uiModuleSymbols, szModuleName, ulID,
                                HB_TRUE, HB_TRUE )->pModuleSymbols;
}

static void hb_vmReleaseLocalSymbols( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmReleaseLocalSymbols()" ) );

   while( s_pSymbols )
   {
      PHB_SYMBOLS pDestroy;

      pDestroy = s_pSymbols;
      s_pSymbols = s_pSymbols->pNext;
      if( pDestroy->szModuleName )
         hb_xfree( pDestroy->szModuleName );
      if( pDestroy->fAllocated )
         hb_xfree( pDestroy->pModuleSymbols );
      hb_xfree( pDestroy );
   }
}

/* This calls all _INITSTATICS functions defined in the application.
 * We are using a special symbol's scope HB_FS_INITEXIT to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void hb_vmDoInitStatics( void )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoInitStatics()" ) );

   while( pLastSymbols )
   {
      if( pLastSymbols->fInitStatics )
      {
         HB_USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

            if( scope == HB_FS_INITEXIT )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmProc( 0 );
            }
         }
         pLastSymbols->fInitStatics = HB_FALSE;
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void hb_vmDoInitFunctions( HB_BOOL fClipInit )
{
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoInitFunctions(%d)", fClipInit ) );

   while( pLastSymbols && hb_vmRequestQuery() == 0 )
   {
      /* only if module contains some INIT functions */
      if( pLastSymbols->fActive && ( pLastSymbols->hScope & HB_FS_INIT ) != 0 )
      {
         HB_USHORT ui = pLastSymbols->uiModuleSymbols;

         while( ui-- )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

            if( scope == HB_FS_INIT &&
                ( strcmp( ( pLastSymbols->pModuleSymbols + ui )->szName,
                          "CLIPINIT$" ) == 0 ? fClipInit : ! fClipInit ) )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmProc( ( HB_USHORT ) hb_cmdargPushArgs() );
               if( hb_vmRequestQuery() != 0 )
                  break;
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   }
}

static void hb_vmDoExitFunctions( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_SYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmDoExitFunctions()" ) );

   /* EXIT procedures should be processed? */
   if( s_fDoExitProc )
   {
      s_fDoExitProc = HB_FALSE;
      hb_stackSetActionRequest( 0 );

      while( pLastSymbols )
      {
         /* only if module contains some EXIT functions */
         if( pLastSymbols->fActive && pLastSymbols->hScope & HB_FS_EXIT )
         {
            HB_USHORT ui;

            for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
            {
               HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->scope.value & HB_FS_INITEXIT;

               if( scope == HB_FS_EXIT )
               {
                  hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
                  hb_vmPushNil();
                  hb_vmProc( 0 );
                  if( hb_stackGetActionRequest() )
                     /* QUIT or BREAK was issued - stop processing
                      */
                     return;
               }
            }
         }
         pLastSymbols = pLastSymbols->pNext;
      }
   }
}

/* ------------------------------- */
/* Extended references             */
/* ------------------------------- */

/*
 * extended item reference functions
 */
static PHB_ITEM hb_vmItemRawRefRead( PHB_ITEM pRefer )
{
   return ( PHB_ITEM ) pRefer->item.asExtRef.value;
}

static PHB_ITEM hb_vmItemRawRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   HB_SYMBOL_UNUSED( pSource );
   return ( PHB_ITEM ) pRefer->item.asExtRef.value;
}

static void hb_vmItemRawRefCopy( PHB_ITEM pDest )
{
   pDest->type = HB_IT_NIL;
   hb_itemCopy( pDest, ( PHB_ITEM ) pDest->item.asExtRef.value );
}

static void hb_vmItemRawRefDummy( void * value )
{
   HB_SYMBOL_UNUSED( value );
}

static const HB_EXTREF s_ItmExtRawRef = {
   hb_vmItemRawRefRead,
   hb_vmItemRawRefWrite,
   hb_vmItemRawRefCopy,
   hb_vmItemRawRefDummy,
   hb_vmItemRawRefDummy
};

typedef struct
{
   HB_ITEM  memvar;
   PHB_ITEM value;
} HB_ITMREF, * PHB_ITMREF;

static PHB_ITEM hb_vmItemRefRead( PHB_ITEM pRefer )
{
   return &( ( PHB_ITMREF ) pRefer->item.asExtRef.value )->memvar;
}

static PHB_ITEM hb_vmItemRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   return hb_itemUnRefWrite( ( ( PHB_ITMREF ) pRefer->item.asExtRef.value )->value, pSource );
}

static void hb_vmItemRefCopy( PHB_ITEM pDest )
{
   pDest->type = HB_IT_NIL;
   hb_itemCopy( pDest, &( ( PHB_ITMREF ) pDest->item.asExtRef.value )->memvar );
}

static void hb_vmItemRefClear( void * value )
{
   PHB_ITMREF pItmRef = ( PHB_ITMREF ) value;

#if 1
   if( ! HB_IS_MEMVAR( &pItmRef->memvar ) ||
       pItmRef->memvar.item.asMemvar.value != pItmRef->value ||
       ! HB_IS_EXTREF( pItmRef->value ) ||
       pItmRef->value->item.asExtRef.func != &s_ItmExtRawRef )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_vmItemRefClear()", NULL, NULL );
#endif

   if( hb_xRefDec( pItmRef->value ) )
      hb_xfree( pItmRef->value );
   else
   {
      pItmRef->memvar.type = HB_IT_NIL;
      hb_itemCopyFromRef( &pItmRef->memvar, pItmRef->value );
      hb_itemMove( pItmRef->value, &pItmRef->memvar );
   }

   hb_xfree( value );
}

static void hb_vmItemRefMark( void * value )
{
   /* the original value should be accessible from initial item so it's
    * not necessary to mark if form this point.
    */
#if 1
   HB_SYMBOL_UNUSED( value );
#else
   hb_gcItemRef( ( ( PHB_ITMREF ) value )->memvar );
   hb_gcItemRef( ( ( PHB_ITMREF ) value )->value );
#endif
}

/*
 * push extended item reference
 */
void hb_vmPushItemRef( PHB_ITEM pItem )
{
   static const HB_EXTREF s_ItmExtRef = {
      hb_vmItemRefRead,
      hb_vmItemRefWrite,
      hb_vmItemRefCopy,
      hb_vmItemRefClear,
      hb_vmItemRefMark
   };

   HB_STACK_TLS_PRELOAD
   PHB_ITMREF pItmRef;
   PHB_ITEM pRefer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmPushItemRef(%p)", pItem ) );

   pItmRef = ( PHB_ITMREF ) hb_xgrab( sizeof( HB_ITMREF ) );

   pItmRef->value = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) );
   pItmRef->value->type = HB_IT_BYREF | HB_IT_EXTREF;
   pItmRef->value->item.asExtRef.value = ( void * ) pItem;
   pItmRef->value->item.asExtRef.func = &s_ItmExtRawRef;

   pItmRef->memvar.type = HB_IT_BYREF | HB_IT_MEMVAR;
   pItmRef->memvar.item.asMemvar.value = pItmRef->value;

   pRefer = hb_stackAllocItem();
   pRefer->type = HB_IT_BYREF | HB_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pItmRef;
   pRefer->item.asExtRef.func = &s_ItmExtRef;
}

/* ------------------------------- */

/*
 * extended message reference structure
 */
typedef struct
{
   PHB_DYNS access;
   PHB_DYNS assign;
   HB_ITEM  object;
   HB_ITEM  value;
} HB_MSGREF, * PHB_MSGREF;

/*
 * extended message reference functions
 */
static PHB_ITEM hb_vmMsgRefRead( PHB_ITEM pRefer )
{
   PHB_MSGREF pMsgRef = ( PHB_MSGREF ) pRefer->item.asExtRef.value;

   if( hb_vmRequestQuery() == 0 )
   {
      HB_STACK_TLS_PRELOAD

      hb_stackPushReturn();
      if( ( pMsgRef->value.type & HB_IT_DEFAULT ) == 0 )
      {
         hb_vmPushDynSym( pMsgRef->assign );
         hb_vmPush( &pMsgRef->object );
         hb_vmPush( &pMsgRef->value );
         hb_vmSend( 1 );
      }
      else
      {
         if( ! pMsgRef->access )
            pMsgRef->access = hb_dynsymGetCase( pMsgRef->assign->pSymbol->szName + 1 );
         hb_vmPushDynSym( pMsgRef->access );
         hb_vmPush( &pMsgRef->object );
         hb_vmSend( 0 );
      }
      hb_itemMove( &pMsgRef->value, hb_stackReturnItem() );
      pMsgRef->value.type |= HB_IT_DEFAULT;
      hb_stackPopReturn();
   }
   return &pMsgRef->value;
}

static PHB_ITEM hb_vmMsgRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   PHB_MSGREF pMsgRef = ( PHB_MSGREF ) pRefer->item.asExtRef.value;

   if( hb_vmRequestQuery() == 0 )
   {
      HB_STACK_TLS_PRELOAD

      hb_stackPushReturn();
      hb_vmPushDynSym( pMsgRef->assign );
      hb_vmPush( &pMsgRef->object );
      hb_vmPush( pSource );
      hb_vmSend( 1 );
      hb_itemCopy( &pMsgRef->value, pSource );
      pMsgRef->value.type |= HB_IT_DEFAULT;
      hb_stackPopReturn();
   }
   return NULL; /*&pMsgIdxRef->value;*/
}

static void hb_vmMsgRefCopy( PHB_ITEM pDest )
{
   PHB_MSGREF pMsgRef = ( PHB_MSGREF ) pDest->item.asExtRef.value;

   hb_xRefInc( pMsgRef );

   if( ( pMsgRef->value.type & HB_IT_DEFAULT ) == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pMsgRef->assign );
         hb_vmPush( &pMsgRef->object );
         hb_vmPush( &pMsgRef->value );
         hb_vmSend( 1 );
         hb_vmRequestRestore();
         pMsgRef->value.type |= HB_IT_DEFAULT;
      }
   }
}

static void hb_vmMsgRefClear( void * value )
{
   PHB_MSGREF pMsgRef = ( PHB_MSGREF ) value;

   /* value were change by C code without calling RefWrite(),
    *  f.e. hb_stor*() function
    */
   if( ( pMsgRef->value.type & HB_IT_DEFAULT ) == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pMsgRef->assign );
         hb_vmPush( &pMsgRef->object );
         hb_vmPush( &pMsgRef->value );
         hb_vmSend( 1 );
         hb_vmRequestRestore();
         pMsgRef->value.type |= HB_IT_DEFAULT;
      }
   }

   if( hb_xRefDec( value ) )
   {
      if( HB_IS_COMPLEX( &pMsgRef->value ) )
         hb_itemClear( &pMsgRef->value );
      if( HB_IS_COMPLEX( &pMsgRef->object ) )
         hb_itemClear( &pMsgRef->object );
      hb_xfree( value );
   }
}

static void hb_vmMsgRefMark( void * value )
{
   if( HB_IS_GCITEM( &( ( PHB_MSGREF ) value )->object ) )
      hb_gcItemRef( &( ( PHB_MSGREF ) value )->object );
   if( HB_IS_GCITEM( &( ( PHB_MSGREF ) value )->value ) )
      hb_gcItemRef( &( ( PHB_MSGREF ) value )->value );
}

/*
 * create extended message reference
 */
HB_BOOL hb_vmMsgReference( PHB_ITEM pObject, PHB_DYNS pMessage, PHB_DYNS pAccMsg )
{
   static const HB_EXTREF s_MsgExtRef = {
      hb_vmMsgRefRead,
      hb_vmMsgRefWrite,
      hb_vmMsgRefCopy,
      hb_vmMsgRefClear,
      hb_vmMsgRefMark
   };

   HB_STACK_TLS_PRELOAD
   PHB_MSGREF pMsgRef;
   PHB_ITEM pRefer;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMsgReference(%p,%p,%p)", pObject, pMessage, pAccMsg ) );

   pMsgRef = ( PHB_MSGREF ) hb_xgrab( sizeof( HB_MSGREF ) );
   pMsgRef->access = pAccMsg;
   pMsgRef->assign = pMessage;
   pMsgRef->value.type = HB_IT_NIL | HB_IT_DEFAULT;
   pMsgRef->object.type = HB_IT_NIL;
   hb_itemMove( &pMsgRef->object, pObject );

   pRefer = hb_stackReturnItem();
   if( HB_IS_COMPLEX( pRefer ) )
      hb_itemClear( pRefer );
   pRefer->type = HB_IT_BYREF | HB_IT_EXTREF;
   pRefer->item.asExtRef.value = ( void * ) pMsgRef;
   pRefer->item.asExtRef.func = &s_MsgExtRef;

   return HB_TRUE;
}

/* ------------------------------- */

/*
 * extended object index reference structure
 */
typedef struct
{
   HB_ITEM object;
   HB_ITEM value;
   HB_ITEM index;
} HB_MSGIDXREF, * PHB_MSGIDXREF;

/*
 * extended object index reference functions
 */
static PHB_ITEM hb_vmMsgIdxRefRead( PHB_ITEM pRefer )
{
   PHB_MSGIDXREF pMsgIdxRef = ( PHB_MSGIDXREF ) pRefer->item.asExtRef.value;

   if( hb_vmRequestQuery() == 0 )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pObject = HB_IS_BYREF( &pMsgIdxRef->object ) ?
                         hb_itemUnRef( &pMsgIdxRef->object ) :
                         &pMsgIdxRef->object;

      hb_stackPushReturn();
      if( ( pMsgIdxRef->value.type & HB_IT_DEFAULT ) == 0 )
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
      else
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, &pMsgIdxRef->value, pObject,
                             &pMsgIdxRef->index, NULL );
      hb_stackPopReturn();
      pMsgIdxRef->value.type |= HB_IT_DEFAULT;
   }
   return &pMsgIdxRef->value;
}

static PHB_ITEM hb_vmMsgIdxRefWrite( PHB_ITEM pRefer, PHB_ITEM pSource )
{
   PHB_MSGIDXREF pMsgIdxRef = ( PHB_MSGIDXREF ) pRefer->item.asExtRef.value;

   if( hb_vmRequestQuery() == 0 )
   {
      HB_STACK_TLS_PRELOAD
      PHB_ITEM pObject = HB_IS_BYREF( &pMsgIdxRef->object ) ?
                         hb_itemUnRef( &pMsgIdxRef->object ) :
                         &pMsgIdxRef->object;
      hb_stackPushReturn();
      hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pObject, pObject,
                          &pMsgIdxRef->index, pSource );
      hb_stackPopReturn();
      pMsgIdxRef->value.type |= HB_IT_DEFAULT;
   }

   return NULL; /*&pMsgIdxRef->value;*/
}

static void hb_vmMsgIdxRefCopy( PHB_ITEM pDest )
{
   PHB_MSGIDXREF pMsgIdxRef = ( PHB_MSGIDXREF ) pDest->item.asExtRef.value;

   hb_xRefInc( pMsgIdxRef );

   /* value were change by C code without calling RefWrite(),
    *  f.e. hb_stor*() function
    */
   if( ( pMsgIdxRef->value.type & HB_IT_DEFAULT ) == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = HB_IS_BYREF( &pMsgIdxRef->object ) ?
                            hb_itemUnRef( &pMsgIdxRef->object ) :
                            &pMsgIdxRef->object;
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
         hb_vmRequestRestore();
      }
   }
}

static void hb_vmMsgIdxRefClear( void * value )
{
   PHB_MSGIDXREF pMsgIdxRef = ( PHB_MSGIDXREF ) value;

   /* value were change by C code without calling RefWrite(),
    *  f.e. hb_stor*() function
    */
   if( ( pMsgIdxRef->value.type & HB_IT_DEFAULT ) == 0 )
   {
      if( hb_vmRequestReenter() )
      {
         PHB_ITEM pObject = HB_IS_BYREF( &pMsgIdxRef->object ) ?
                            hb_itemUnRef( &pMsgIdxRef->object ) :
                            &pMsgIdxRef->object;
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pObject, pObject,
                             &pMsgIdxRef->index, &pMsgIdxRef->value );
         hb_vmRequestRestore();
      }
   }

   if( hb_xRefDec( value ) )
   {
      if( HB_IS_COMPLEX( &pMsgIdxRef->value ) )
         hb_itemClear( &pMsgIdxRef->value );
      if( HB_IS_COMPLEX( &pMsgIdxRef->object ) )
         hb_itemClear( &pMsgIdxRef->object );
      if( HB_IS_COMPLEX( &pMsgIdxRef->index ) )
         hb_itemClear( &pMsgIdxRef->index );
      hb_xfree( value );
   }
}

static void hb_vmMsgIdxRefMark( void * value )
{
   if( HB_IS_GCITEM( &( ( PHB_MSGIDXREF ) value )->object ) )
      hb_gcItemRef( &( ( PHB_MSGIDXREF ) value )->object );
   if( HB_IS_GCITEM( &( ( PHB_MSGIDXREF ) value )->index ) )
      hb_gcItemRef( &( ( PHB_MSGIDXREF ) value )->index );
   if( HB_IS_GCITEM( &( ( PHB_MSGIDXREF ) value )->value ) )
      hb_gcItemRef( &( ( PHB_MSGIDXREF ) value )->value );
}

/*
 * create extended message reference
 */
static void hb_vmMsgIndexReference( PHB_ITEM pRefer, PHB_ITEM pObject, PHB_ITEM pIndex )
{
   static const HB_EXTREF s_MsgIdxExtRef = {
      hb_vmMsgIdxRefRead,
      hb_vmMsgIdxRefWrite,
      hb_vmMsgIdxRefCopy,
      hb_vmMsgIdxRefClear,
      hb_vmMsgIdxRefMark
   };

   PHB_MSGIDXREF pMsgIdxRef;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmMsgIndexReference(%p,%p,%p)", pRefer, pObject, pIndex ) );

   pMsgIdxRef = ( PHB_MSGIDXREF ) hb_xgrab( sizeof( HB_MSGIDXREF ) );
   pMsgIdxRef->value.type = HB_IT_NIL | HB_IT_DEFAULT;
   pMsgIdxRef->object.type = HB_IT_NIL;
   pMsgIdxRef->index.type = HB_IT_NIL;
   hb_itemCopy( &pMsgIdxRef->object, HB_IS_STRING( pObject ) ? pRefer : pObject );
   hb_itemMove( &pMsgIdxRef->index, pIndex );

   pIndex->type = HB_IT_BYREF | HB_IT_EXTREF;
   pIndex->item.asExtRef.value = ( void * ) pMsgIdxRef;
   pIndex->item.asExtRef.func = &s_MsgIdxExtRef;
   hb_itemMove( pRefer, pIndex );
}

/* ------------------------------- */
/* VM exceptions                   */
/* ------------------------------- */

void hb_vmRequestQuit( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestQuit()" ) );

   /* In MT mode EXIT functions are executed only from hb_vmQuit()
    * when all other threads have terminated
    */
#if ! defined( HB_MT_VM )
   hb_vmDoExitFunctions(); /* process defined EXIT functions */
#endif /* HB_MT_VM */
   hb_stackSetActionRequest( HB_QUIT_REQUESTED );
}

void hb_vmRequestEndProc( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestEndProc()" ) );

   hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
}

void hb_vmRequestBreak( PHB_ITEM pItem )
{
   HB_STACK_TLS_PRELOAD
   HB_ISIZ nRecoverBase;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestBreak(%p)", pItem ) );

   nRecoverBase = hb_stackGetRecoverBase();
   while( nRecoverBase && ( hb_stackItem( nRecoverBase +
               HB_RECOVER_STATE )->item.asRecover.flags & HB_SEQ_DOALWAYS ) )
   {
#if defined( _HB_RECOVER_DEBUG )
      if( hb_stackItem( nRecoverBase + HB_RECOVER_STATE )->type != HB_IT_RECOVER )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_vmRequestBreak", NULL, NULL );
#endif
      nRecoverBase = hb_stackItem( nRecoverBase +
                                   HB_RECOVER_STATE )->item.asRecover.base;
   }

   if( nRecoverBase )
   {
#if defined( _HB_RECOVER_DEBUG )
      if( hb_stackItem( nRecoverBase + HB_RECOVER_STATE )->type != HB_IT_RECOVER )
         hb_errInternal( HB_EI_ERRUNRECOV, "hb_vmRequestBreak2", NULL, NULL );
#endif
      if( pItem )
         hb_itemCopy( hb_stackItem( nRecoverBase + HB_RECOVER_VALUE ), pItem );

      hb_stackSetActionRequest( HB_BREAK_REQUESTED );
   }
   else
   {
#ifdef HB_CLP_STRICT
      /*
       * do not execute EXIT procedures to be as close as possible
       * buggy Clipper behavior. [druzus]
       */
      s_fDoExitProc = HB_FALSE;
      hb_stackSetActionRequest( HB_QUIT_REQUESTED );
#else
      /*
       * Clipper has a bug here. Tests shows that it set exception flag
       * and then tries to execute EXIT procedures so the first one is
       * immediately interrupted. Because Clipper does not check the
       * exception flag often enough then it's possible to execute one
       * function from first EXIT PROC. Using small trick with
       * QOUT( TYPE( cPrivateVar ) ) in the EXIT procedure (TYPE() is
       * not normal function) we can also check that it tries to execute
       * EXIT procedures exactly here before leave current function.
       * So to be as close as possible the Clipper intentional behavior
       * we execute hb_vmRequestQuit() here. [druzus]
       */
      hb_vmRequestQuit();
#endif
   }
}

void hb_vmRequestCancel( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestCancel()" ) );

   if( hb_stackSetStruct()->HB_SET_CANCEL )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 + 10 ]; /* additional 10 bytes for line info (%hu) overhead */
      char file[ HB_PATH_MAX ];
      HB_USHORT uiLine;
      int iLevel = 0, l;

      hb_conOutErr( hb_conNewLine(), 0 );
      hb_conOutErr( "Cancelled at: ", 0 );

      while( hb_procinfo( iLevel++, buffer, &uiLine, file ) )
      {
         l = ( int ) strlen( buffer );
         hb_snprintf( buffer + l, sizeof( buffer ) - l, " (%hu)%s%s", uiLine, *file ? HB_I_( " in " ) : "", file );

         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }

      /*
       * Clipper does not execute EXIT procedures when quiting using break key
       */
      s_fDoExitProc = HB_FALSE;
      hb_stackSetActionRequest( HB_QUIT_REQUESTED );
   }
}

HB_USHORT hb_vmRequestQuery( void )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( hb_vmThreadRequest & HB_THREQUEST_QUIT )
   {
      if( ! hb_stackQuitState() )
      {
         hb_stackSetQuitState( HB_TRUE );
         hb_stackSetActionRequest( HB_QUIT_REQUESTED );
      }
   }
#endif

   return hb_stackGetActionRequest();
}

HB_BOOL hb_vmRequestReenter( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;
   int iLocks = 0;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestReenter()" ) );

#if defined( HB_MT_VM )
   if( ! s_fHVMActive || hb_stackId() == NULL )
      return HB_FALSE;
   else
   {
      while( hb_stackLockCount() > 0 )
      {
         hb_vmLock();
         ++iLocks;
      }
   }
#else
   if( ! s_fHVMActive )
      return HB_FALSE;
#endif

   hb_stackPushReturn();

   pItem = hb_stackAllocItem();
   pItem->type = HB_IT_RECOVER;
   pItem->item.asRecover.recover = NULL;
   pItem->item.asRecover.base    = iLocks;
   pItem->item.asRecover.flags   = 0;
   pItem->item.asRecover.request = hb_stackGetActionRequest();

   hb_stackSetActionRequest( 0 );

   return HB_TRUE;
}

void hb_vmRequestRestore( void )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiAction;
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestRestore()" ) );

   pItem = hb_stackItemFromTop( -1 );

   if( pItem->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_vmRequestRestore", NULL, NULL );

   uiAction = pItem->item.asRecover.request | hb_stackGetActionRequest();

#if defined( HB_MT_VM )
   if( uiAction & HB_VMSTACK_REQUESTED )
      hb_vmThreadQuit();
   else
   {
      int iCount = ( int ) pItem->item.asRecover.base;
#else
   {
#endif
      if( uiAction & HB_QUIT_REQUESTED )
         hb_stackSetActionRequest( HB_QUIT_REQUESTED );
      else if( uiAction & HB_BREAK_REQUESTED )
         hb_stackSetActionRequest( HB_BREAK_REQUESTED );
      else if( uiAction & HB_ENDPROC_REQUESTED )
         hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
      else
         hb_stackSetActionRequest( 0 );

      hb_stackDec();
      hb_stackPopReturn();

#if defined( HB_MT_VM )
      while( iCount-- > 0 )
         hb_vmUnlock();
#endif
   }
}

HB_BOOL hb_vmRequestReenterExt( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestReenterExt()" ) );

   if( ! s_fHVMActive )
      return HB_FALSE;
   else
   {
      HB_USHORT uiAction = 0;
      int iLocks = 0;
      PHB_ITEM pItem;

#if defined( HB_MT_VM )
      HB_STACK_TLS_PRELOAD

      if( hb_stackId() == NULL )
      {
         uiAction = HB_VMSTACK_REQUESTED;
         /* TODO: add protection against executing hb_threadStateNew()
          * during GC pass
          */
         hb_vmThreadInit( NULL );
         HB_STACK_TLS_RELOAD
      }
      else
      {
         while( hb_stackLockCount() > 0 )
         {
            hb_vmLock();
            ++iLocks;
         }
         hb_stackPushReturn();
      }
#else
      hb_stackPushReturn();
#endif
      pItem = hb_stackAllocItem();
      pItem->type = HB_IT_RECOVER;
      pItem->item.asRecover.recover = NULL;
      pItem->item.asRecover.base    = iLocks;
      pItem->item.asRecover.flags   = 0;
      pItem->item.asRecover.request = uiAction | hb_stackGetActionRequest();

      hb_stackSetActionRequest( 0 );
   }

   return HB_TRUE;
}

HB_BOOL hb_vmIsActive( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsActive()" ) );

   return s_fHVMActive;
}

HB_BOOL hb_vmIsReady( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsReady()" ) );

#if defined( HB_MT_VM )
   return s_fHVMActive && hb_stackId();
#else
   return s_fHVMActive;
#endif
}

HB_BOOL hb_vmInternalsEnabled( void )
{
   return s_fInternalsEnabled;
}

PHB_CODEPAGE hb_vmCDP( void )
{
   HB_STACK_TLS_PRELOAD

   return ( PHB_CODEPAGE ) hb_stackGetCDP();
}

void hb_vmSetCDP( PHB_CODEPAGE pCDP )
{
   HB_STACK_TLS_PRELOAD

   hb_stackSetCDP( ( void * ) pCDP );
}

PHB_LANG hb_vmLang( void )
{
   HB_STACK_TLS_PRELOAD

   return ( PHB_LANG ) hb_stackGetLang();
}

void hb_vmSetLang( PHB_LANG pLang )
{
   HB_STACK_TLS_PRELOAD

   hb_stackSetLang( ( void * ) pLang );
}

void * hb_vmI18N( void )
{
   HB_STACK_TLS_PRELOAD

   return hb_stackGetI18N();
}

void hb_vmSetI18N( void * pI18N )
{
   HB_STACK_TLS_PRELOAD

   hb_i18n_release( hb_stackGetI18N() );
   hb_stackSetI18N( pI18N );
}

#if defined( HB_MT_VM )
#  define HB_XVM_RETURN \
   { \
      if( hb_vmThreadRequest ) \
         hb_vmRequestTest(); \
      return ( hb_stackGetActionRequest() & \
               ( HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED ) ) != 0; \
   }
#else
#  define HB_XVM_RETURN \
   { \
      return ( hb_stackGetActionRequest() & \
               ( HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED ) ) != 0; \
   }
#endif /* HB_MT_VM */

void hb_xvmExitProc( void )
{
   HB_STACK_TLS_PRELOAD

   if( hb_stackGetActionRequest() & HB_ENDPROC_REQUESTED )
      hb_stackSetActionRequest( 0 );
}

void hb_xvmEndProc( void )
{
   HB_STACK_TLS_PRELOAD

   if( ! ( hb_stackGetActionRequest() & ( HB_QUIT_REQUESTED | HB_BREAK_REQUESTED ) ) )
      hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
}

void hb_xvmSeqBegin( void )
{
   HB_STACK_TLS_PRELOAD

   PHB_ITEM pItem;

   /*
    * Create the SEQUENCE envelope
    * To keep compatibility with pure PCODE evaluation we have
    * use exactly the same SEQUENCE envelope or hb_vmRequestBreak()
    * will not work as expected.
    *
    * [ break return value ]  -2
    * [ recover envelope   ]  -1
    * [                    ] <- new recover base
    */

   /* 1) clear the storage for value returned by BREAK statement */
   hb_stackAllocItem()->type = HB_IT_NIL;
   /* 2) recovery state */
   pItem = hb_stackAllocItem();
   /* mark type as NIL - it's not real item */
   pItem->type = HB_IT_RECOVER;
   /* address of RECOVER or END opcode - not used in C code */
   pItem->item.asRecover.recover = NULL;
   /* store current RECOVER base */
   pItem->item.asRecover.base = hb_stackGetRecoverBase();
   /* store current bCanRecover flag - not used in C code */
   pItem->item.asRecover.flags = 0;
   /* clear new recovery state */
   pItem->item.asRecover.request = 0;

   /* set new recover base */
   hb_stackSetRecoverBase( hb_stackTopOffset() );
}

HB_BOOL hb_xvmSeqEnd( void )
{
   HB_STACK_TLS_PRELOAD

   /*
    * remove all items placed on the stack after BEGIN code
    */
   hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_xvmSeqEnd", NULL, NULL );
#endif
   /*
    * Remove the SEQUENCE envelope
    * This is executed either at the end of sequence or as the
    * response to the break statement if there is no RECOVER clause
    */

   /* 2) Restore previous recovery base address */
   hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
   hb_stackDec();
   /* 1) Discard the value returned by BREAK statement */
   hb_stackPop();

#if defined( HB_MT_VM )
   if( hb_vmThreadRequest )
      hb_vmRequestTest();
#endif /* HB_MT_VM */
   if( hb_stackGetActionRequest() & ( HB_ENDPROC_REQUESTED | HB_QUIT_REQUESTED ) )
      return HB_TRUE;
   else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( 0 );
   return HB_FALSE;
}

HB_BOOL hb_xvmSeqEndTest( void )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( hb_vmThreadRequest )
      hb_vmRequestTest();
#endif /* HB_MT_VM */
   if( ( hb_stackGetActionRequest() &
         ( HB_ENDPROC_REQUESTED | HB_BREAK_REQUESTED | HB_QUIT_REQUESTED ) ) != 0 )
      return HB_TRUE;

   /*
    * remove all items placed on the stack after BEGIN code
    */
   hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_xvmSeqEndTest", NULL, NULL );
#endif
   /*
    * Remove the SEQUENCE envelope
    * This is executed either at the end of sequence or as the
    * response to the break statement if there is no RECOVER clause
    */

   /* 2) Restore previous recovery base address */
   hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
   hb_stackDec();
   /* 1) Discard the value returned by BREAK statement */
   hb_stackPop();
   return HB_FALSE;
}

HB_BOOL hb_xvmSeqRecover( void )
{
   HB_STACK_TLS_PRELOAD

   /*
    * Execute the RECOVER code
    */

   /*
    * remove all items placed on the stack after BEGIN code
    */
   hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_xvmSeqRecover", NULL, NULL );
#endif
   /* 2) Restore previous recovery base address */
   hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
   hb_stackDec();
   /* 1) Leave the value returned from BREAK */

#if defined( HB_MT_VM )
   if( hb_vmThreadRequest )
      hb_vmRequestTest();
#endif /* HB_MT_VM */
   if( hb_stackGetActionRequest() & ( HB_ENDPROC_REQUESTED | HB_QUIT_REQUESTED ) )
      return HB_TRUE;
   else if( hb_stackGetActionRequest() & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( 0 );
   return HB_FALSE;
}

void hb_xvmSeqAlways( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSeqAlways()" ) );

   /* Create the SEQUENCE ALWAYS envelope */
   /* 1) clear the storage for RETURN value */
   hb_stackAllocItem()->type = HB_IT_NIL;
   /* 2) recovery state */
   pItem = hb_stackAllocItem();
   /* mark type as NIL - it's not real item */
   pItem->type = HB_IT_RECOVER;
   /* address of RECOVER or END opcode - not used in C code */
   pItem->item.asRecover.recover = NULL;
   /* store current RECOVER base */
   pItem->item.asRecover.base = hb_stackGetRecoverBase();
   /* store current bCanRecover flag - not used in C code */
   pItem->item.asRecover.flags = 0;
   /* clear new recovery state */
   pItem->item.asRecover.request = 0;
   /* set sequence type */
   pItem->item.asRecover.flags = HB_SEQ_DOALWAYS;
   /* set new recover base */
   hb_stackSetRecoverBase( hb_stackTopOffset() );
}

HB_BOOL hb_xvmAlwaysBegin( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmAlwaysBegin()" ) );

   /* remove all items placed on the stack after BEGIN code */
   hb_stackRemove( hb_stackGetRecoverBase() );
#if defined( _HB_RECOVER_DEBUG )
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_xvmAlwaysBegin", NULL, NULL );
#endif
   /* store and reset action */
   hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request = hb_stackGetActionRequest();
   hb_stackSetActionRequest( 0 );
   /* store RETURN value */
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request & HB_ENDPROC_REQUESTED )
      hb_itemMove( hb_stackItemFromTop( HB_RECOVER_VALUE ), hb_stackReturnItem() );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmAlwaysEnd( void )
{
   HB_STACK_TLS_PRELOAD
   HB_USHORT uiPrevAction, uiCurrAction;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmAlwaysEnd()" ) );

   /* remove all items placed on the stack after ALWAYSBEGIN code */
   hb_stackRemove( hb_stackGetRecoverBase() );

#if defined( _HB_RECOVER_DEBUG )
   if( hb_stackItemFromTop( HB_RECOVER_STATE )->type != HB_IT_RECOVER )
      hb_errInternal( HB_EI_ERRUNRECOV, "hb_xvmAlwaysEnd", NULL, NULL );
#endif
   /* restore previous recovery base address */
   hb_stackSetRecoverBase( hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.base );
   uiCurrAction = hb_stackGetActionRequest();
   uiPrevAction = hb_stackItemFromTop( HB_RECOVER_STATE )->item.asRecover.request;
   /* restore requested action */
   if( ( uiCurrAction | uiPrevAction ) & HB_QUIT_REQUESTED )
      hb_stackSetActionRequest( HB_QUIT_REQUESTED );
   else if( ( uiCurrAction | uiPrevAction ) & HB_BREAK_REQUESTED )
      hb_stackSetActionRequest( HB_BREAK_REQUESTED );
   else if( ( uiCurrAction | uiPrevAction ) & HB_ENDPROC_REQUESTED )
      hb_stackSetActionRequest( HB_ENDPROC_REQUESTED );
   else
      hb_stackSetActionRequest( 0 );
   /* remove the ALWAYS envelope */
   hb_stackDec();
   /* restore RETURN value if not overloaded inside ALWAYS code */
   if( ! ( uiCurrAction & HB_ENDPROC_REQUESTED ) &&
         ( uiPrevAction & HB_ENDPROC_REQUESTED ) )
      hb_stackPopReturn();
   else
      hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmSeqBlock( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSeqBlock()" ) );

   hb_vmSeqBlock();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumStart( int nVars, int nDescend )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumStart(%d,%d)", nVars, nDescend ) );

   hb_vmEnumStart( nVars, nDescend );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumNext( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumNext()" ) );

   hb_vmEnumNext();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmEnumPrev( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumPrev()" ) );

   hb_vmEnumPrev();

   HB_XVM_RETURN
}

void hb_xvmEnumEnd( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEnumEnd()" ) );

   hb_vmEnumEnd();
}

HB_BOOL hb_xvmSwitchGet( PHB_ITEM * pSwitchPtr )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSwitchGet(%p)", pSwitchPtr ) );

   *pSwitchPtr = hb_vmSwitchGet();

   HB_XVM_RETURN
}

void hb_xvmSetLine( HB_USHORT uiLine )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSetLine(%hu)", uiLine ) );

   hb_stackBaseItem()->item.asSymbol.stackstate->uiLineNo = uiLine;
#ifndef HB_NO_DEBUG
   if( hb_stackBaseItem()->item.asSymbol.stackstate->fDebugging )
      hb_vmDebuggerShowLine( uiLine );
#endif
}

void hb_xvmFrame( int iLocals, int iParams )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFrame(%d, %d)", iLocals, iParams ) );

   hb_vmFrame( ( HB_USHORT ) iLocals, ( unsigned char ) iParams );
}

void hb_xvmVFrame( int iLocals, int iParams )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmVFrame(%d, %d)", iLocals, iParams ) );

   hb_vmVFrame( ( HB_USHORT ) iLocals, ( unsigned char ) iParams );
}

void hb_xvmSFrame( PHB_SYMB pSymbol )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSFrame(%p)", pSymbol ) );

   hb_vmSFrame( pSymbol );
}

HB_BOOL hb_xvmDo( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDo(%hu)", uiParams ) );

   hb_vmProc( uiParams );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmFunction( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFunction(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmProc( uiParams );
   hb_stackPushReturn();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmSend( HB_USHORT uiParams )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSend(%hu)", uiParams ) );

   hb_itemSetNil( hb_stackReturnItem() );
   hb_vmSend( uiParams );
   hb_stackPushReturn();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushObjectVarRef( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushObjectVarRef()" ) );

   hb_vmPushObjectVarRef();

   HB_XVM_RETURN
}

void hb_xvmRetValue( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmRetValue()" ) );

   hb_stackPopReturn();
   hb_stackReturnItem()->type &= ~HB_IT_MEMOFLAG;
}

void hb_xvmRetNil( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmRetNil()" ) );

   hb_itemSetNil( hb_stackReturnItem() );
}

void hb_xvmRetInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmRetInt(%ld)", lValue ) );

   hb_itemPutNL( hb_stackReturnItem(), lValue );
}

void hb_xvmStatics( PHB_SYMB pSymbol, HB_USHORT uiStatics )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmStatics(%p,%hu)", pSymbol, uiStatics ) );

   hb_vmStatics( pSymbol, uiStatics );
}

void hb_xvmThreadStatics( HB_USHORT uiStatics, const HB_BYTE * statics )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmThreadStatics(%hu,%p)", uiStatics, statics ) );

   hb_vmInitThreadStatics( uiStatics, statics );
}

void hb_xvmParameter( PHB_SYMB pSymbol, int iParams )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmParameter(%p,%d)", pSymbol, iParams ) );

   hb_memvarNewParameter( pSymbol, hb_stackItemFromBase( iParams ) );
}

void hb_xvmPushLocal( HB_SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLocal(%hd)", iLocal ) );

   hb_vmPushLocal( iLocal );
}

void hb_xvmPushLocalByRef( HB_SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLocalByRef(%hd)", iLocal ) );

   hb_vmPushLocalByRef( iLocal );
}

void hb_xvmPopLocal( HB_SHORT iLocal )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopLocal(%hd)", iLocal ) );

   hb_vmPopLocal( iLocal );
}

static PHB_ITEM hb_xvmLocalPtr( int iLocal )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalPtr(%d)", iLocal ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      return hb_stackLocalVariable( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      return hb_codeblockGetRef( hb_stackSelfItem()->item.asBlock.value, iLocal );
   }
}

void hb_xvmCopyLocals( int iDest, int iSource )
{
   PHB_ITEM pDest;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmCopyLocals(%d,%d)", iDest, iSource ) );

   pDest = hb_xvmLocalPtr( iDest );
   hb_itemCopyToRef( hb_xvmLocalPtr( iSource ),
                     HB_IS_BYREF( pDest ) ? hb_itemUnRef( pDest ) : pDest );
}

void hb_xvmPushStatic( HB_USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushStatic(%hu)", uiStatic ) );

   hb_vmPushStatic( uiStatic );
}

void hb_xvmPushStaticByRef( HB_USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushStaticByRef(%hu)", uiStatic ) );

   hb_vmPushStaticByRef( uiStatic );
}

void hb_xvmPopStatic( HB_USHORT uiStatic )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopStatic(%hu)", uiStatic ) );

   hb_vmPopStatic( uiStatic );
}

HB_BOOL hb_xvmPushVariable( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushVariable(%p)", pSymbol ) );

   hb_vmPushVariable( pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopVariable( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopVariable(%p)", pSymbol ) );

   /* See the note above in HB_P_POPVARIABLE */
#if 0
   if( pSymbol->pDynSym && hb_dynsymGetMemvar( pSymbol->pDynSym ) )
      hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
   else if( hb_rddFieldPut( hb_stackItemFromTop( -1 ), pSymbol ) == HB_FAILURE )
#endif
      hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

void hb_xvmPushBlockShort( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushBlockShort(%p, %p)", pCode, pSymbols ) );

   hb_vmPushBlockShort( pCode, pSymbols, HB_FALSE );
}

void hb_xvmPushBlock( const HB_BYTE * pCode, PHB_SYMB pSymbols )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushBlock(%p, %p)", pCode, pSymbols ) );

   hb_vmPushBlock( pCode, pSymbols, HB_FALSE );
}

void hb_xvmPushSelf( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushSelf()" ) );

   hb_vmPush( hb_stackSelfItem() );
}

void hb_xvmPushFuncSymbol( PHB_SYMB pSym )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushFuncSymbol(%p)", pSym ) );

   pItem = hb_stackAllocItem();
   pItem->type = HB_IT_SYMBOL;
   pItem->item.asSymbol.value = pSym;
   pItem->item.asSymbol.stackstate = NULL;
   hb_stackAllocItem()->type = HB_IT_NIL;
}

HB_BOOL hb_xvmPopLogical( HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopLogical(%p)", pfValue ) );

   *pfValue = hb_vmPopLogical();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAlias( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPopAlias()" ) );

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ), NULL ); /* it clears the passed item */
   hb_stackDec();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmSwapAlias( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSwapAlias()" ) );

   hb_vmSwapAlias();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushField( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushField(%p)", pSymbol ) );

   hb_rddGetFieldValue( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAlias( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushAlias()" ) );

   hb_vmPushAlias();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedField( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushAliasedField(%p)", pSymbol ) );

   hb_vmPushAliasedField( pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedFieldExt( PHB_SYMB pAlias, PHB_SYMB pField )
{
   HB_STACK_TLS_PRELOAD
   int iCurrArea;

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushAliasedFieldExt(%p,%p)", pAlias, pField ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if( hb_rddSelectWorkAreaSymbol( pAlias ) == HB_SUCCESS )
      hb_rddGetFieldValue( hb_stackAllocItem(), pField );
   hb_rddSelectWorkAreaNumber( iCurrArea );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushAliasedVar( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushAliasedVar(%p)", pSymbol ) );

   hb_vmPushAliasedVar( pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopField( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopField(%p)", pSymbol ) );

   hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pSymbol );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushMemvar( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushMemvar(%p)", pSymbol ) );

   hb_memvarGetValue( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPushMemvarByRef( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPushMemvarByRef(%p)", pSymbol ) );

   hb_memvarGetRefer( hb_stackAllocItem(), pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopMemvar( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopMemvar(%p)", pSymbol ) );

   hb_memvarSetValue( pSymbol, hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedField( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopAliasedField(%p)", pSymbol ) );

   hb_vmPopAliasedField( pSymbol );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedFieldExt( PHB_SYMB pAlias, PHB_SYMB pField )
{
   HB_STACK_TLS_PRELOAD
   int iCurrArea;

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopAliasedFieldExt(%p,%p)", pAlias, pField ) );

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if( hb_rddSelectWorkAreaSymbol( pAlias ) == HB_SUCCESS )
   {
      hb_rddPutFieldValue( hb_stackItemFromTop( -1 ), pField );
      hb_stackPop();
   }
   hb_rddSelectWorkAreaNumber( iCurrArea );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPopAliasedVar( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_INFO, ( "hb_xvmPopAliasedVar(%p)", pSymbol ) );

   hb_vmPopAliasedVar( pSymbol );

   HB_XVM_RETURN
}

void hb_xvmLocalSetInt( int iLocal, HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalSetInt(%d, %ld)", iLocal, lValue ) );

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackLocalVariable( iLocal );
      if( HB_IS_BYREF( pLocal ) )
         pLocal = hb_itemUnRef( pLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetVar( hb_stackSelfItem(), iLocal );
   }

   if( HB_IS_OBJECT( pLocal ) && hb_objHasOperator( pLocal, HB_OO_OP_ASSIGN ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_ASSIGN, pLocal, pLocal,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      hb_itemPutNL( pLocal, lValue );
   }
}

HB_BOOL hb_xvmLocalAddInt( int iLocal, HB_LONG lAdd )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalAddInt(%d,%ld)", iLocal, lAdd ) );

   hb_vmAddInt( hb_stackLocalVariable( iLocal ), lAdd );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalInc( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalInc(%d)", iLocal ) );

   pLocal = hb_stackLocalVariable( iLocal );
   hb_vmInc( HB_IS_BYREF( pLocal ) ? hb_itemUnRef( pLocal ) : pLocal );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalDec( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalDec(%d)", iLocal ) );

   pLocal = hb_stackLocalVariable( iLocal );
   hb_vmDec( HB_IS_BYREF( pLocal ) ? hb_itemUnRef( pLocal ) : pLocal );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalIncPush( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalInc(%d)", iLocal ) );

   pLocal = hb_stackLocalVariable( iLocal );
   if( HB_IS_BYREF( pLocal ) )
      pLocal = hb_itemUnRef( pLocal );
   hb_vmInc( pLocal );
   hb_itemCopy( hb_stackAllocItem(), pLocal );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLocalAdd( int iLocal )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pLocal;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalAdd(%d)", iLocal ) );

   pLocal = hb_stackLocalVariable( iLocal );
   if( HB_IS_BYREF( pLocal ) )
      pLocal = hb_itemUnRef( pLocal );
   hb_vmPlus( pLocal, hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmStaticAdd( HB_USHORT uiStatic )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pStatic;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmStaticAdd(%hu)", uiStatic ) );

   pStatic = ( ( PHB_ITEM ) hb_stackGetStaticsBase() )->item.asArray.value->pItems + uiStatic - 1;
   if( HB_IS_BYREF( pStatic ) )
      pStatic = hb_itemUnRef( pStatic );
   hb_vmPlus( pStatic, hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMemvarAdd( PHB_SYMB pSymbol )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pMemVar, pVal1, pVal2;

   HB_TRACE( HB_TR_INFO, ( "hb_xvmMemvarAdd(%p)", pSymbol ) );

   pVal1 = hb_stackItemFromTop( -2 );
   pVal2 = hb_stackItemFromTop( -1 );
   if( HB_IS_STRING( pVal1 ) && HB_IS_STRING( pVal2 ) )
   {
      pMemVar = hb_memvarGetItem( pSymbol );
      if( pMemVar )
      {
         hb_vmPlus( pMemVar, pVal1, pVal2 );
         hb_stackPop();
         hb_stackPop();
         HB_XVM_RETURN
      }
   }

   hb_vmPlus( pVal1, pVal1, pVal2 );
   hb_memvarSetValue( pSymbol, pVal1 );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmAnd( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmAnd()" ) );

   hb_vmAnd();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmOr( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmOr()" ) );

   hb_vmOr();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmNot( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNot()" ) );

   hb_vmNot();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmNegate( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNegate()" ) );

   hb_vmNegate();

   HB_XVM_RETURN
}

void hb_xvmDuplicate( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDuplicate()" ) );

   hb_vmDuplicate();
}

void hb_xvmDuplUnRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDuplUnRef()" ) );

   hb_vmDuplUnRef();
}

void hb_xvmPushUnRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushUnRef()" ) );

   hb_vmPushUnRef();
}

void hb_xvmSwap( int iCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmSwap(%d)", iCount ) );

   hb_vmSwap( iCount );
}

HB_BOOL hb_xvmForTest( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmForTest()" ) );

   hb_vmForTest();

   HB_XVM_RETURN
}

void hb_xvmFuncPtr( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmFuncPtr()" ) );

   hb_vmFuncPtr();
}

HB_BOOL hb_xvmEqual( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEqual()" ) );

   hb_vmEqual();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmExactlyEqual( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmExactlyEqual()" ) );

   hb_vmExactlyEqual();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmEqualInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEqualInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value == lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value == ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value == ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value == ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_NIL( pItem ) )
   {
      pItem->item.asLogical.value = HB_FALSE;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_EQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_EQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmEqualIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmEqualIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value == lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value == ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value == ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value == ( double ) lValue;
      hb_stackDec();
   }
   else if( HB_IS_NIL( pItem ) )
   {
      *pfValue = HB_FALSE;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_EQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_EQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqual( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNotEqual()" ) );

   hb_vmNotEqual();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqualInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNotEqualInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value != lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value != ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value != ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value != ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_NIL( pItem ) )
   {
      pItem->item.asLogical.value = HB_TRUE;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_NOTEQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_NOTEQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmNotEqualIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmNotEqualIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value != lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value != ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value != ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value != ( double ) lValue;
      hb_stackDec();
   }
   else if( HB_IS_NIL( pItem ) )
   {
      *pfValue = HB_TRUE;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_NOTEQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_NOTEQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLess( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLess()" ) );

   hb_vmLess();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLessThenInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessThenInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value < lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value < ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value < ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value < ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_LESS ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_LESS, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLessThenIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessThenIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value < lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value < ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value < ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value < ( double ) lValue;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_LESS ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_LESS, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqual( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessEqual()" ) );

   hb_vmLessEqual();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqualThenInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessEqualThenInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value <= lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value <= ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value <= ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value <= ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_LESSEQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_LESSEQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmLessEqualThenIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLessEqualThenIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value <= lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value <= ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value <= ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value <= ( double ) lValue;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_LESSEQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_LESSEQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreater( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreater()" ) );

   hb_vmGreater();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterThenInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterThenInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value > lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value > ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value > ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value > ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_GREATER ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_GREATER, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterThenIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterThenIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value > lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value > ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value > ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value > ( double ) lValue;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_GREATER ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_GREATER, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqual( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterEqual()" ) );

   hb_vmGreaterEqual();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqualThenInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterEqualThenInt(%ld)", lValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      pItem->item.asLogical.value = ( HB_LONG ) pItem->item.asInteger.value >= lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value >= ( HB_MAXINT ) lValue;
      pItem->item.asLogical.value = f;
#else
      pItem->item.asLogical.value = pItem->item.asLong.value >= ( HB_MAXINT ) lValue;
#endif
      pItem->type = HB_IT_LOGICAL;
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asLogical.value = pItem->item.asDouble.value >= ( double ) lValue;
      pItem->type = HB_IT_LOGICAL;
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_GREATEREQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_GREATEREQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmGreaterEqualThenIntIs( HB_LONG lValue, HB_BOOL * pfValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pItem;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmGreaterEqualThenIntIs(%ld,%p)", lValue, pfValue ) );

   pItem = hb_stackItemFromTop( -1 );
   if( HB_IS_INTEGER( pItem ) )
   {
      *pfValue = ( HB_LONG ) pItem->item.asInteger.value >= lValue;
      hb_stackDec();
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if defined( __DCC__ ) /* NOTE: Workaround for vxworks/diab/x86 5.8.0.0 compiler bug. */
      HB_BOOL f = pItem->item.asLong.value >= ( HB_MAXINT ) lValue;
      *pfValue = f;
#else
      *pfValue = pItem->item.asLong.value >= ( HB_MAXINT ) lValue;
#endif
      hb_stackDec();
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      *pfValue = pItem->item.asDouble.value >= ( double ) lValue;
      hb_stackDec();
   }
   else if( hb_objHasOperator( pItem, HB_OO_OP_GREATEREQUAL ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_GREATEREQUAL, pItem, pItem,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
      return hb_xvmPopLogical( pfValue );
   }
   else
   {
      PHB_ITEM pResult;

      hb_vmPushLong( lValue );
      pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem, hb_stackItemFromTop( -1 ) );

      if( pResult )
      {
         hb_stackPop();
         hb_itemMove( pItem, pResult );
         hb_itemRelease( pResult );
         return hb_xvmPopLogical( pfValue );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmInstring( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmInstring()" ) );

   hb_vmInstring();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmAddInt( HB_LONG lAdd )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmAddInt(%ld)", lAdd ) );

   hb_vmAddInt( hb_stackItemFromTop( -1 ), lAdd );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPlus( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPlus()" ) );

   hb_vmPlus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ),
              hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPlusEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPlusEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmPlus( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPlusEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPlusEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmPlus( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMinus( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMinus()" ) );

   hb_vmMinus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ),
               hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMinusEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMinusEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmMinus( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMinusEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMinusEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmMinus( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMultByInt( HB_LONG lValue )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMultByInt(%ld)", lValue ) );

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      int iDec;
      double dValue = hb_itemGetNDDec( pValue, &iDec );

      hb_itemPutNumType( pValue, dValue * lValue, iDec,
                         HB_ITEM_TYPERAW( pValue ), HB_IT_INTEGER );
   }
   else if( hb_objHasOperator( pValue, HB_OO_OP_MULT ) )
   {
      hb_vmPushLong( lValue );
      hb_objOperatorCall( HB_OO_OP_MULT, pValue, pValue,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pSubst;

      hb_vmPushLong( lValue );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pValue, hb_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         hb_stackPop();
         hb_itemMove( pValue, pSubst );
         hb_itemRelease( pSubst );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMult( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMult()" ) );

   hb_vmMult( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMultEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMultEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmMult( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMultEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMultEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmMult( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDivideByInt( HB_LONG lDivisor )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivideByInt(%ld)", lDivisor ) );

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      if( lDivisor == 0 )
      {
         PHB_ITEM pSubst;

         hb_vmPushLong( lDivisor );
         pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pValue, hb_stackItemFromTop( -1 ) );

         if( pSubst )
         {
            hb_stackPop();
            hb_itemMove( pValue, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else
         hb_itemPutND( pValue, hb_itemGetND( pValue ) / lDivisor );
   }
   else if( hb_objHasOperator( pValue, HB_OO_OP_DIVIDE ) )
   {
      hb_vmPushLong( lDivisor );
      hb_objOperatorCall( HB_OO_OP_DIVIDE, pValue, pValue,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pSubst;

      hb_vmPushLong( lDivisor );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pValue, hb_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         hb_stackPop();
         hb_itemMove( pValue, pSubst );
         hb_itemRelease( pSubst );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmModulusByInt( HB_LONG lDivisor )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModulusByInt(%ld)", lDivisor ) );

   pValue = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pValue ) )
   {
      if( lDivisor == 0 )
      {
         PHB_ITEM pSubst;

         hb_vmPushLong( lDivisor );
         pSubst = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pValue, hb_stackItemFromTop( -1 ) );

         if( pSubst )
         {
            hb_stackPop();
            hb_itemMove( pValue, pSubst );
            hb_itemRelease( pSubst );
         }
      }
      else if( HB_IS_NUMINT( pValue ) )
         hb_itemPutND( pValue, ( double ) ( HB_ITEM_GET_NUMINTRAW( pValue ) % lDivisor ) );
      else
         hb_itemPutND( pValue, fmod( hb_itemGetND( pValue ), lDivisor ) );

   }
   else if( hb_objHasOperator( pValue, HB_OO_OP_MOD ) )
   {
      hb_vmPushLong( lDivisor );
      hb_objOperatorCall( HB_OO_OP_MOD, pValue, pValue,
                          hb_stackItemFromTop( -1 ), NULL );
      hb_stackPop();
   }
   else
   {
      PHB_ITEM pSubst;

      hb_vmPushLong( lDivisor );
      pSubst = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pValue, hb_stackItemFromTop( -1 ) );

      if( pSubst )
      {
         hb_stackPop();
         hb_itemMove( pValue, pSubst );
         hb_itemRelease( pSubst );
      }
   }

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDivide( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivide()" ) );

   hb_vmDivide( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDivEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmDivide( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDivEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDivEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmDivide( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmModulus( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModulus()" ) );

   hb_vmModulus( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmModEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmModulus( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmModEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmModulus( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmPower( void )
{
   HB_STACK_TLS_PRELOAD
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPower()" ) );

   hb_vmPower( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmExpEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmExpEq()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   pValue = hb_stackItemFromTop( -1 );
   hb_vmPower( pResult, pResult, pValue );
   hb_itemCopy( pValue, pResult );
   hb_itemMove( hb_stackItemFromTop( -2 ), pValue );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmExpEqPop( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmExpEqPop()" ) );

   pResult = hb_itemUnRef( hb_stackItemFromTop( -2 ) );
   hb_vmPower( pResult, pResult, hb_stackItemFromTop( -1 ) );
   hb_stackPop();
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmInc( void )
{
   HB_STACK_TLS_PRELOAD
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmInc()" ) );

   hb_vmInc( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmIncEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue, pTemp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmIncEq()" ) );

   pResult = hb_stackItemFromTop( -1 );
   pValue = hb_itemUnRef( pResult );
   hb_vmInc( pValue );
   pTemp = hb_stackAllocItem();
   hb_itemCopy( pTemp, pValue );
   hb_itemMove( pResult, pTemp );
   hb_stackDec();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmIncEqPop( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmIncEqPop()" ) );

   hb_vmInc( hb_itemUnRef( hb_stackItemFromTop( -1 ) ) );
   hb_stackPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDec( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDec()" ) );

   hb_vmDec( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDecEq( void )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pResult, pValue, pTemp;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDecEq()" ) );

   pResult = hb_stackItemFromTop( -1 );
   pValue = hb_itemUnRef( pResult );
   hb_vmDec( pValue );
   pTemp = hb_stackAllocItem();
   hb_itemCopy( pTemp, pValue );
   hb_itemMove( pResult, pTemp );
   hb_stackDec();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmDecEqPop( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmDecEqPop()" ) );

   hb_vmDec( hb_itemUnRef( hb_stackItemFromTop( -1 ) ) );
   hb_stackPop();

   HB_XVM_RETURN
}

void hb_xvmArrayDim( HB_USHORT uiDimensions )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayDim(%hu)", uiDimensions ) );

   hb_vmArrayDim( uiDimensions );
}

void hb_xvmArrayGen( HB_SIZE nElements )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayGen(%" HB_PFS "u)", nElements ) );

   hb_vmArrayGen( nElements );
}

void hb_xvmHashGen( HB_SIZE nElements )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmHashGen(%" HB_PFS "u)", nElements ) );

   hb_vmHashGen( nElements );
}

static void hb_vmArrayItemPush( HB_SIZE nIndex )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pArray;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayItemPush(%" HB_PFS "u)", nIndex ) );

   pArray = hb_stackItemFromTop( -1 );

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_OBJECT( pArray ) && hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
      {
         hb_vmPushNumInt( nIndex );
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                             hb_stackItemFromTop( -1 ), NULL );
         hb_stackPop();
         return;
      }

      if( HB_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         PHB_ITEM pItem = hb_stackAllocItem();

         hb_itemCopy( pItem, pArray->item.asArray.value->pItems + nIndex - 1 );
         hb_itemMove( pArray, pItem );
         hb_stackDec();
      }
      else
      {
         hb_vmPushNumInt( nIndex );
         if( ! HB_IS_OBJECT( pArray ) &&
             hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                                 hb_stackItemFromTop( -1 ), NULL ) )
            hb_stackPop();
         else
#ifdef HB_CLP_STRICT
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 0 );
#else
            hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                           2, pArray, hb_stackItemFromTop( -1 ) );
#endif
      }
   }
   else if( HB_IS_HASH( pArray ) )
   {
      PHB_ITEM pValue, pIndex;

      hb_vmPushNumInt( nIndex );
      pIndex = hb_stackItemFromTop( -1 );
      pValue = hb_hashGetItemPtr( pArray, pIndex, HB_HASH_AUTOADD_ACCESS );

      if( pValue )
      {
         hb_itemCopy( pIndex, pValue );
         hb_itemMove( pArray, pIndex );
         hb_stackDec();
      }
      else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                                   pIndex, NULL ) )
         hb_stackPop();
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else
   {
      hb_vmPushNumInt( nIndex );
      if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                              hb_stackItemFromTop( -1 ), NULL ) )
         hb_stackPop();
      else
         hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, hb_stackItemFromTop( -1 ) );
   }
}

static void hb_vmArrayItemPop( HB_SIZE nIndex )
{
   HB_STACK_TLS_PRELOAD
   PHB_ITEM pValue;
   PHB_ITEM pArray;

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmArrayItemPop(%" HB_PFS "u)", nIndex ) );

   pValue = hb_stackItemFromTop( -2 );
   pArray = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( HB_IS_ARRAY( pArray ) )
   {
      if( HB_IS_OBJECT( pArray ) && hb_objHasOperator( pArray, HB_OO_OP_ARRAYINDEX ) )
      {
         hb_vmPushNumInt( nIndex );
         hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                             hb_stackItemFromTop( -1 ), pValue );
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
         return;
      }

      if( HB_IS_VALID_INDEX( nIndex, pArray->item.asArray.value->nLen ) )
      {
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemMoveRef( pArray->item.asArray.value->pItems + nIndex - 1, pValue );
         hb_stackPop();
         hb_stackDec();    /* value was moved above hb_stackDec() is enough */
      }
      else
      {
         hb_vmPushNumInt( nIndex );
         if( ! HB_IS_OBJECT( pArray ) &&
             hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                                 hb_stackItemFromTop( -1 ), pValue ) )
         {
            hb_stackPop();
            hb_stackPop();
            hb_stackPop();
         }
         else
#ifdef HB_CLP_STRICT
            hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 0 );
#else
            hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ),
                           1, hb_stackItemFromTop( -1 ) );
#endif
      }
   }
   else if( HB_IS_HASH( pArray ) )
   {
      PHB_ITEM pDest;

      hb_vmPushNumInt( nIndex );
      pDest = hb_hashGetItemPtr( pArray, hb_stackItemFromTop( -1 ), HB_HASH_AUTOADD_ASSIGN );

      if( pDest )
      {
         pValue->type &= ~( HB_IT_MEMOFLAG | HB_IT_DEFAULT );
         hb_itemMoveRef( pDest, pValue );
         hb_stackPop();
         hb_stackPop();
         hb_stackDec();    /* value was moved above hb_stackDec() is enough */
      }
      else if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                                   hb_stackItemFromTop( -1 ), pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 3, pArray, hb_stackItemFromTop( -1 ), pValue );
   }
   else
   {
      hb_vmPushNumInt( nIndex );
      if( hb_objOperatorCall( HB_OO_OP_ARRAYINDEX, pArray, pArray,
                              hb_stackItemFromTop( -1 ), pValue ) )
      {
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ),
                        1, hb_stackItemFromTop( -1 ) );
   }
}


HB_BOOL hb_xvmArrayPush( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPush()" ) );

   hb_vmArrayPush();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayPushRef( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPushRef()" ) );

   hb_vmArrayPushRef();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayItemPush( HB_SIZE nIndex )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayItemPush(%" HB_PFS "u)", nIndex ) );

   hb_vmArrayItemPush( nIndex );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayPop( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayPop()" ) );

   hb_vmArrayPop();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmArrayItemPop( HB_SIZE nIndex )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmArrayItemPop(%" HB_PFS "u)", nIndex ) );

   hb_vmArrayItemPop( nIndex );

   HB_XVM_RETURN
}

void hb_xvmPushDouble( double dNumber, int iWidth, int iDec )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushDouble(%lf, %d, %d)", dNumber, iWidth, iDec ) );

   hb_vmPushDoubleConst( dNumber, iWidth, iDec );
}

#ifdef HB_LONG_LONG_OFF
void hb_xvmPushLongLong( double dNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLongLong(%l.0f)", dNumber ) );

   hb_vmPushDoubleConst( dNumber, HB_DEFAULT_WIDTH, 0 );
}
#else
void hb_xvmPushLongLong( HB_LONGLONG llNumber )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushLongLong(%" PFLL "i)", llNumber ) );

   hb_vmPushLongLongConst( llNumber );
}
#endif

void hb_xvmPushStringHidden( int iMethod, const char * szText, HB_SIZE nSize )
{
   HB_STACK_TLS_PRELOAD
   char * szString;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushStringHidden(%d, %s, %" HB_PFS "u)", iMethod, szText, nSize ) );

   szString = hb_compDecodeString( iMethod, szText, &nSize );
   hb_itemPutCLPtr( hb_stackAllocItem(), szString, nSize );
}

void hb_xvmLocalName( HB_USHORT uiLocal, const char * szLocalName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmLocalName(%hu, %s)", uiLocal, szLocalName ) );

#ifndef HB_NO_DEBUG
   hb_vmLocalName( uiLocal, szLocalName );
#else
   HB_SYMBOL_UNUSED( uiLocal );
   HB_SYMBOL_UNUSED( szLocalName );
#endif
}

void hb_xvmStaticName( HB_BYTE bIsGlobal, HB_USHORT uiStatic, const char * szStaticName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmStaticName(%d, %hu, %s)", ( int ) bIsGlobal, uiStatic, szStaticName ) );

#ifndef HB_NO_DEBUG
   hb_vmStaticName( bIsGlobal, uiStatic, szStaticName );
#else
   HB_SYMBOL_UNUSED( bIsGlobal );
   HB_SYMBOL_UNUSED( uiStatic );
   HB_SYMBOL_UNUSED( szStaticName );
#endif
}

void hb_xvmModuleName( const char * szModuleName )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmModuleName(%s)", szModuleName ) );

#ifndef HB_NO_DEBUG
   hb_vmModuleName( szModuleName );
#else
   HB_SYMBOL_UNUSED( szModuleName );
#endif
}

HB_BOOL hb_xvmMacroArrayGen( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroArrayGen(%hu)", uiArgSets ) );

   hb_vmMacroArrayGen( uiArgSets );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroDo( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroDo(%hu)", uiArgSets ) );

   hb_vmMacroDo( uiArgSets );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroFunc( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroFunc(%hu)", uiArgSets ) );

   hb_vmMacroFunc( uiArgSets );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroSend( HB_USHORT uiArgSets )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroSend(%hu)", uiArgSets ) );

   hb_vmMacroSend( uiArgSets );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPush( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPush(%d)", iFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushRef( void )
{
   HB_STACK_TLS_PRELOAD

   PHB_ITEM pMacro;

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushRef()" ) );

   pMacro = hb_stackItemFromTop( -1 );
   hb_macroPushReference( pMacro );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushIndex( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushIndex()" ) );

   hb_vmMacroPushIndex();

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushList( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushList(%d)", iFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushPare( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushPare(%d)", iFlags ) );

   hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPushAliased( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPushAliased(%d)", iFlags ) );

   hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPop( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPop(%d)", iFlags ) );

   hb_macroSetValue( hb_stackItemFromTop( -1 ), iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroPopAliased( int iFlags )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroPopAliased(%d)", iFlags ) );

   hb_macroPopAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), iFlags );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroSymbol( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroSymbol()" ) );

   hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

HB_BOOL hb_xvmMacroText( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmMacroText()" ) );

   hb_macroTextValue( hb_stackItemFromTop( -1 ) );

   HB_XVM_RETURN
}

void hb_xvmPushVParams( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushVParams()" ) );

   hb_vmPushVParams();
}

void hb_xvmPushAParams( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmPushAParams()" ) );

   hb_vmPushAParams();
}

void hb_xvmWithObjectStart( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmWithObjectStart()" ) );

   hb_vmWithObjectStart();
}

void hb_xvmWithObjectEnd( void )
{
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmWithObjectEnd()" ) );

   hb_stackPop();  /* remove with object envelope */
   hb_stackPop();  /* remove implicit object */
}

void hb_xvmWithObjectMessage( PHB_SYMB pSymbol )
{
   PHB_ITEM pWith;

   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_xvmWithObjectMessage(%p)", pSymbol ) );

   if( pSymbol )
      hb_vmPushSymbol( pSymbol );

   pWith = hb_stackWithObjectItem();
   if( pWith )
      hb_vmPush( pWith );
   else
      hb_stackAllocItem()->type = HB_IT_NIL;
}



#undef hb_vmFlagEnabled
HB_U32 hb_vmFlagEnabled( HB_U32 flags )
{
   return s_VMFlags & flags;
}

void hb_vmFlagSet( HB_U32 flags )
{
   s_VMFlags |= flags;
}

void hb_vmFlagClear( HB_U32 flags )
{
   s_VMFlags &= ~flags;
}

/* ------------------------------------------------------------------------ */
/* The debugger support functions */
/* ------------------------------------------------------------------------ */

void hb_vmRequestDebug( void )
{
#ifndef HB_NO_DEBUG
   HB_STACK_TLS_PRELOAD

   HB_TRACE( HB_TR_DEBUG, ( "hb_vmRequestDebug()" ) );

   *( hb_stackDebugRequest() ) = HB_TRUE;
#endif
}

HB_BOOL hb_dbg_InvokeDebug( HB_BOOL bInvoke )
{
#ifndef HB_NO_DEBUG
   HB_STACK_TLS_PRELOAD
   HB_BOOL * pfRequest = hb_stackDebugRequest();
   HB_BOOL bRequest = *pfRequest;
   *pfRequest = bInvoke;
   return bRequest;
#else
   HB_SYMBOL_UNUSED( bInvoke );
   return HB_FALSE;
#endif
}

HB_DBGENTRY_FUNC hb_dbg_SetEntry( HB_DBGENTRY_FUNC pFunDbgEntry )
{
   HB_DBGENTRY_FUNC pPrevFunc;

   HB_TRACE( HB_TR_DEBUG, ( "hb_dbg_SetEntry(%p)", pFunDbgEntry ) );

#ifndef HB_NO_DEBUG
   pPrevFunc = s_pFunDbgEntry;
   s_pFunDbgEntry = pFunDbgEntry;
#else
   HB_SYMBOL_UNUSED( pFunDbgEntry );
   pPrevFunc = NULL;
#endif

   return pPrevFunc;
}

PHB_ITEM hb_dbg_vmVarSGet( PHB_ITEM pStaticsBase, int nOffset )
{
   if( pStaticsBase )
      return hb_arrayGetItemPtr( pStaticsBase, nOffset );
   else
      return NULL;
}

HB_ULONG hb_dbg_ProcLevel( void )
{
   return hb_stackCallDepth();
}

/*
 * check if the debugger activation was requested or request the debugger
 * activation if .T. is passed
 */
HB_FUNC( __DBGINVOKEDEBUG )
{
   HB_STACK_TLS_PRELOAD

   if( hb_vmInternalsEnabled() )
   {
#ifndef HB_NO_DEBUG
      HB_BOOL * pfRequest = hb_stackDebugRequest();

      hb_retl( *pfRequest );
      *pfRequest = hb_parl( 1 );
#else
      hb_retl( HB_FALSE );
#endif
   }
   else
      hb_retl( HB_FALSE );
}

/* Return the statics array. Please AClone() before assignments
 * __dbgVMVarSList() --> <aStat>
 */
HB_FUNC( __DBGVMVARSLIST )
{
   if( hb_vmInternalsEnabled() )
      hb_itemReturnRelease( hb_vmStaticsArray() );
   else
   {
      HB_STACK_TLS_PRELOAD
      hb_reta( 0 );
   }
}

/* Return the statics array length.
 * __dbgVMVarSLen() --> <nStatics>
 */
HB_FUNC( __DBGVMVARSLEN )
{
   HB_STACK_TLS_PRELOAD

   if( hb_vmInternalsEnabled() )
      hb_retnint( hb_vmStaticsCount() );
   else
      hb_retnint( 0 );
}

/* Return a specified statics
 * __dbgVMVarSGet( <nStatic> ) --> <xStat>
 */
HB_FUNC( __DBGVMVARSGET )
{
   if( hb_vmInternalsEnabled() )
      hb_itemReturn( hb_dbg_vmVarSGet( hb_param( 1, HB_IT_ARRAY ), hb_parni( 2 ) ) );
}

/*
 * Sets the value of a specified statics
 * __dbgVMVarSSet( <nStatic>, <uValue> ) --> NIL
 */
HB_FUNC( __DBGVMVARSSET )
{
   if( hb_vmInternalsEnabled() )
   {
      PHB_ITEM pStaticsBase = hb_param( 1, HB_IT_ARRAY );
      PHB_ITEM pItem = hb_param( 3, HB_IT_ANY );

      if( pStaticsBase && pItem )
         hb_arraySet( pStaticsBase, hb_parni( 2 ), pItem );
   }
}

HB_FUNC( __DBGPROCLEVEL )
{
   if( hb_vmInternalsEnabled() )
   {
      HB_STACK_TLS_PRELOAD
      hb_retnl( hb_dbg_ProcLevel() - 1 );   /* Don't count self */
   }
}

/*
 * These functions are for GLOBAL variables - now they are only for
 * compatibility with xHarbour debugger - Harbour does not support
 * GLOBALs
 */
HB_ULONG hb_dbg_vmVarGCount( void )
{
#if 0
   return hb_arrayLen( &s_aGlobals );
#else
   return 0;
#endif
}

PHB_ITEM hb_dbg_vmVarGGet( int nGlobal, int nOffset )
{
#if 0
   return hb_arrayGetItemPtr( &s_aGlobals, nGlobal + nOffset );
#else
   HB_SYMBOL_UNUSED( nGlobal );
   HB_SYMBOL_UNUSED( nOffset );
   return NULL;
#endif
}

/*
 * Return a clone of the globals array.
 * __dbgVMVarGList() --> <aStat>
 */
HB_FUNC( __DBGVMVARGLIST )
{
   if( hb_vmInternalsEnabled() )
   {
#if 0
      PHB_ITEM pGlobals = hb_itemClone( &s_aGlobals );
#else
      PHB_ITEM pGlobals = hb_itemArrayNew( 0 );
#endif

      hb_itemReturnRelease( pGlobals );
   }
   else
   {
      HB_STACK_TLS_PRELOAD
      hb_reta( 0 );
   }
}

HB_FUNC( __DBGVMVARGGET )
{
   if( hb_vmInternalsEnabled() )
      hb_itemReturn( hb_dbg_vmVarGGet( hb_parni( 1 ), hb_parni( 2 ) ) );
}

HB_FUNC( __DBGVMVARGSET )
{
#if 0
   if( hb_vmInternalsEnabled() )
   {
      PHB_ITEM pItem = hb_param( 3, HB_IT_ANY );
      if( pItem )
         hb_arraySet( &s_aGlobals, hb_parni( 1 ) + hb_parni( 2 ), pItem );
   }
#endif
}


/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsStaticRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsStaticRef()" ) );

   /* statics are stored as an item of arrays allocated by hb_itemNew() so
    * they do not need any special GC support
    */
}

void hb_vmIsStackRef( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmIsStackRef()" ) );

#if defined( HB_MT_VM )
   if( s_vmStackLst )
   {
      PHB_THREADSTATE pStack = s_vmStackLst;
      do
      {
         if( pStack->fActive && pStack->pStackId )
            hb_stackIsStackRef( pStack->pStackId, hb_vmTSVarClean );
         pStack = pStack->pNext;
      }
      while( pStack != s_vmStackLst );
   }
#else
   hb_stackIsStackRef( hb_stackId(), NULL );
#endif /* HB_MT_VM */
}

void hb_vmUpdateAllocator( PHB_ALLOCUPDT_FUNC pFunc, int iCount )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmUpdateAllocator(%p, %d)", pFunc, iCount ) );

#if defined( HB_MT_VM )
   if( s_vmStackLst )
   {
      PHB_THREADSTATE pStack = s_vmStackLst;
      do
      {
         if( pStack->pStackId )
            hb_stackUpdateAllocator( pStack->pStackId, pFunc, iCount );
         pStack = pStack->pNext;
      }
      while( pStack != s_vmStackLst );
   }
#else
   hb_stackUpdateAllocator( hb_stackId(), pFunc, iCount );
#endif /* HB_MT_VM */
}

/* ------------------------------------------------------------------------ */

/*
 * Turns on | off the profiler activity
 * __SetProfiler( <lOnOff> ) --> <lOldValue>
 */
HB_FUNC( __SETPROFILER )
{
   HB_STACK_TLS_PRELOAD
#ifdef HB_NO_PROFILER
   hb_retl( HB_FALSE );
#else
   hb_retl( hb_bProfiler );
   if( HB_ISLOG( 1 ) )
      hb_bProfiler = hb_parl( 1 );
#endif
}

HB_FUNC( __OPCOUNT ) /* it returns the total amount of opcodes */
{
   HB_STACK_TLS_PRELOAD
   hb_retnl( HB_P_LAST_PCODE - 1 );
}

HB_FUNC( __OPGETPRF ) /* profiler: It returns an array with an opcode called and
                         consumed times { nTimes, nTime },
                         given the opcode index */
{
   HB_STACK_TLS_PRELOAD
#ifndef HB_NO_PROFILER
   HB_ULONG ulOpcode = hb_parnl( 1 );

   hb_reta( 2 );
   if( ulOpcode < HB_P_LAST_PCODE )
   {
      hb_storvnl( hb_ulOpcodesCalls[ ulOpcode ], -1, 1 );
      hb_storvnl( hb_ulOpcodesTime[ ulOpcode ], -1, 2 );
   }
   else
#else
   hb_reta( 2 );
#endif
   {
      hb_storvnl( 0, -1, 1 );
      hb_storvnl( 0, -1, 2 );
   }
}

/*
 * Turns on | off tracing of PRG-level function and method calls
 * __TracePrgCalls( <lOnOff> ) --> <lOldValue>
 */
HB_FUNC( __TRACEPRGCALLS )
{
   HB_STACK_TLS_PRELOAD
#if defined( HB_PRG_TRACE )
   hb_retl( hb_bTracePrgCalls );
   if( HB_ISLOG( 1 ) )
      hb_bTracePrgCalls = hb_parl( 1 );
#else
   hb_retl( HB_FALSE );
#endif
}

HB_FUNC( __QUITCANCEL )
{
   HB_STACK_TLS_PRELOAD

#if defined( HB_MT_VM )
   if( ! hb_stackQuitState() )
#endif
   {
      HB_ISIZ nRecoverBase = hb_stackGetRecoverBase();

      if( nRecoverBase )
      {
         PHB_ITEM pRecover = hb_stackItem( nRecoverBase + HB_RECOVER_STATE );

#if defined( _HB_RECOVER_DEBUG )
         if( pRecover->type != HB_IT_RECOVER )
            hb_errInternal( HB_EI_ERRUNRECOV, "hb_vmRequestBreak", NULL, NULL );
#endif
         if( pRecover->item.asRecover.flags & HB_SEQ_DOALWAYS )
         {
            pRecover->item.asRecover.flags   &= ~HB_QUIT_REQUESTED;
            pRecover->item.asRecover.request &= ~HB_QUIT_REQUESTED;
         }
      }
   }
}

HB_FUNC( __VMNOINTERNALS )
{
   s_fInternalsEnabled = HB_FALSE;
}

HB_FUNC( __VMITEMID )
{
   HB_STACK_TLS_PRELOAD

   PHB_ITEM pItem = hb_param( 1, HB_IT_ANY );

   if( pItem )
   {
      if( HB_IS_ARRAY( pItem ) )
         hb_retptr( hb_arrayId( pItem ) );
      else if( HB_IS_HASH( pItem ) )
         hb_retptr( hb_hashId( pItem ) );
      else if( HB_IS_BLOCK( pItem ) )
         hb_retptr( hb_codeblockId( pItem ) );
   }
}

HB_FUNC( HB_ARRAYTOPARAMS )
{
   HB_STACK_TLS_PRELOAD

   hb_retni( 0 );
}

HB_FUNC( ERRORLEVEL )
{
   HB_STACK_TLS_PRELOAD

   hb_retni( s_nErrorLevel );

   /* NOTE: This should be HB_ISNUM( 1 ), but it's sort of a Clipper bug that it
            accepts other types also and considers them zero. [vszakats] */

   if( hb_pcount() >= 1 )
      /* Only replace the error level if a parameter was passed */
      s_nErrorLevel = hb_parni( 1 );
}


/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

extern void hb_vmForceLink( void );
void hb_vmForceLink( void )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_vmForceLink()" ) );

   HB_FUNC_EXEC( SYSINIT );
}

/* NOTE: Pass string literals only. */
void hb_vmSetLinkedMain( const char * szMain )
{
   s_vm_pszLinkedMain = szMain;
}

void hb_vmSetDefaultGT( const char * szGtName )
{
   hb_gtSetDefault( szGtName );
}

/* Force linking default language and codepage modules */
HB_CODEPAGE_REQUEST( HB_CODEPAGE_DEFAULT )
HB_LANG_REQUEST( HB_LANG_DEFAULT )

#undef HB_FORCE_LINK_MAIN

#if ! defined( HB_DYNLIB ) && defined( HB_OS_WIN ) && \
   ( defined( __DMC__ ) || defined( __WATCOMC__ ) || defined( __MINGW32__ ) )

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainWin

#elif defined( __WATCOMC__ ) && ( defined( HB_OS_LINUX ) || defined( HB_OS_OS2 ) || defined( HB_OS_WIN ) )

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainStd

#endif

#ifdef HB_FORCE_LINK_MAIN
HB_EXTERN_BEGIN
extern void HB_FORCE_LINK_MAIN( void );
HB_EXTERN_END
void _hb_forceLinkMain()
{
   HB_FORCE_LINK_MAIN();
}
#endif

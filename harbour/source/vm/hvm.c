/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Virtual Machine
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats <viktor.szakats@syenar.hu>
 *    hb_vmPushLongConst()
 *    hb_vmPushDoubleConst()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    __VMVARSGET()
 *    __VMVARSLIST()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include <math.h>
#include <time.h>

#include "hbapi.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbapilng.h"
#include "hbapirdd.h"
#include "hbapigt.h"
#include "hbapicdp.h"
#include "hbvm.h"
#include "hbpcode.h"
#include "hbset.h"
#include "hbinkey.ch"
#include "inkey.ch"
#include "hbdebug.ch"

#include "hbmemory.ch"

#ifdef HB_MACRO_STATEMENTS
   #include "hbpp.h"
#endif

/* DEBUG only*/
/* #include <windows.h> */

typedef struct _SYMBOLS
{
   PHB_SYMB pModuleSymbols;  /* pointer to a one module own symbol table */
   USHORT   uiModuleSymbols; /* number of symbols on that table */
   struct _SYMBOLS * pNext;  /* pointer to the next SYMBOLS structure */
   HB_SYMBOLSCOPE hScope;    /* scope collected from all symbols in module used to speed initialization code */
} SYMBOLS, * PSYMBOLS;       /* structure to keep track of all modules symbol tables */

HB_FUNC_EXTERN( SYSINIT );

/* PCode functions */

/* Operators (mathematical / character / misc) */
static void    hb_vmNegate( void );          /* negates (-) the latest value on the stack */
static void    hb_vmPlus( void );            /* sums the latest two values on the stack, removes them and leaves the result */
static void    hb_vmMinus( void );           /* substracts the latest two values on the stack, removes them and leaves the result */
static void    hb_vmMult( void );            /* multiplies the latest two values on the stack, removes them and leaves the result */
static void    hb_vmDivide( void );          /* divides the latest two values on the stack, removes them and leaves the result */
static void    hb_vmModulus( void );         /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
static void    hb_vmPower( void );           /* power the latest two values on the stack, removes them and leaves the result */
static void    hb_vmInc( void );             /* increment the latest numeric value on the stack */
static void    hb_vmDec( void );             /* decrements the latest numeric value on the stack */
static void    hb_vmFuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the satck */

/* Operators (relational) */
static void    hb_vmEqual( BOOL bExact );    /* checks if the two latest values on the stack are equal, removes both and leaves result */
static void    hb_vmNotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
static void    hb_vmLess( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
static void    hb_vmLessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
static void    hb_vmGreater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
static void    hb_vmGreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
static void    hb_vmInstring( void );        /* check whether string 1 is contained in string 2 */
static void    hb_vmForTest( void );         /* test for end condition of for */

/* Operators (logical) */
static void    hb_vmNot( void );             /* changes the latest logical value on the stack */
static void    hb_vmAnd( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
static void    hb_vmOr( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */

/* Array */
static void    hb_vmArrayPush( void );       /* pushes an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPop( void );        /* pops a value from the stack */
static void    hb_vmArrayDim( USHORT uiDimensions ); /* generates an uiDimensions Array and initialize those dimensions from the stack values */
static void    hb_vmArrayGen( ULONG ulElements ); /* generates an ulElements Array and fills it from the stack values */
static void    hb_vmArrayNew( HB_ITEM_PTR, USHORT ); /* creates array */

/* Object */
static void    hb_vmOperatorCall( PHB_ITEM, PHB_ITEM, char * ); /* call an overloaded operator */
static void    hb_vmOperatorCallUnary( PHB_ITEM, char * ); /* call an overloaded unary operator */

/* Database */
static ERRCODE hb_vmSelectWorkarea( PHB_ITEM );  /* select the workarea using a given item or a substituted value */
static void    hb_vmSwapAlias( void );           /* swaps items on the eval stack and pops the workarea number */

/* Execution */
static HARBOUR hb_vmDoBlock( void );             /* executes a codeblock */
static void    hb_vmLocalName( USHORT uiLocal, char * szLocalName ); /* locals and parameters index and name information for the debugger */
static void    hb_vmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName ); /* statics vars information for the debugger */
static void    hb_vmModuleName( char * szModuleName ); /* PRG and function name information for the debugger */
static void    hb_vmFrame( BYTE bLocals, BYTE bParams ); /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmSFrame( PHB_SYMB pSym );     /* sets the statics frame for a function */
static void    hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ); /* increases the global statics array to hold a PRG statics */
static void    hb_vmEndBlock( void );            /* copies the last codeblock pushed value into the return value */
static void    hb_vmRetValue( void );            /* pops the latest stack value into stack.Return */
static void    hb_vmDebuggerShowLine( USHORT uiLine ); /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void );     /* notifies the debugger for an endproc */

/* Push */
static void    hb_vmPushAlias( void );            /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB ); /* pushes an aliased field on the eval stack */
static void    hb_vmPushAliasedVar( PHB_SYMB );   /* pushes an aliased variable on the eval stack */
static void    hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a codeblock */
static void    hb_vmPushBlockShort( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a codeblock */
static void    hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec ); /* Pushes a double constant (pcode) */
static void    hb_vmPushMacroBlock( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a macro-compiled codeblock */
static void    hb_vmPushLocal( SHORT iLocal );    /* pushes the containts of a local onto the stack */
static void    hb_vmPushLocalByRef( SHORT iLocal );    /* pushes a local by refrence onto the stack */
static void    hb_vmPushLongConst( long lNumber );  /* Pushes a long constant (pcode) */
static void    hb_vmPushHBLong( HB_LONG lNumber ); /* pushes a HB_LONG number onto the stack */
#if !defined( HB_LONG_LONG_OFF )
   static void hb_vmPushLongLongConst( LONGLONG lNumber );  /* Pushes a long long constant (pcode) */
#endif
#if HB_INT_MAX >= INT32_MAX
static void    hb_vmPushIntegerConst( int iNumber );  /* Pushes a int constant (pcode) */
#endif
static void    hb_vmPushNumInt( HB_LONG lNumber );     /* pushes a number on to the stack and decides if it is integer or HB_LONG */
extern void    hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 ); /* pushes a number on to the stack and decides if it is integer, long or double */
static void    hb_vmPushStatic( USHORT uiStatic );     /* pushes the containts of a static onto the stack */
static void    hb_vmPushStaticByRef( USHORT uiStatic ); /* pushes a static by refrence onto the stack */
static void    hb_vmPushVariable( PHB_SYMB pVarSymb ); /* pushes undeclared variable */
static void    hb_vmDuplicate( void );            /* duplicates the latest value on the stack */
static void    hb_vmDuplTwo( void );              /* duplicates the latest two value on the stack */

/* Pop */
static BOOL    hb_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
static long    hb_vmPopDate( void );              /* pops the stack latest value and returns its date value as a long */
static HB_LONG hb_vmPopHBLong( void );            /* pops the stack latest value and returns its HB_LONG value */
static double  hb_vmPopNumber( void );            /* pops the stack latest value and returns its numeric value */
static double  hb_vmPopDouble( int * );           /* pops the stack latest value and returns its double numeric format value */
static void    hb_vmPopAlias( void );             /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );  /* pops an aliased field from the eval stack*/
static void    hb_vmPopAliasedVar( PHB_SYMB );    /* pops an aliased variable from the eval stack*/
static void    hb_vmPopLocal( SHORT iLocal );     /* pops the stack latest value onto a local */
static void    hb_vmPopStatic( USHORT uiStatic ); /* pops the stack latest value onto a static */

/* Take the value from stack top without POP */
static double  hb_vmTopNumber( void );            /* take the stack latest value and returns its numeric value */

/* misc */
static void    hb_vmDoInitStatics( void );        /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( void );      /* executes all defined PRGs INIT functions */
static void    hb_vmDoExitFunctions( void );      /* executes all defined PRGs EXIT functions */
static void    hb_vmReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */

extern void * hb_mthRequested( void ); /* profiler from classes.c */
extern void hb_mthAddTime( void *, ULONG ); /* profiler from classes.c */

BOOL hb_bProfiler = FALSE; /* profiler status is off */
BOOL hb_bTracePrgCalls = FALSE; /* prg tracing is off */
ULONG hb_ulOpcodesCalls[ HB_P_LAST_PCODE ]; /* array to profile opcodes calls */
ULONG hb_ulOpcodesTime[ HB_P_LAST_PCODE ]; /* array to profile opcodes consumed time */

#ifdef HARBOUR_START_PROCEDURE
   char *s_pszLinkedMain = NULL; /* name of starup function set by linker */
#endif

/* virtual machine state */

HB_SYMB  hb_symEval = { "__EVAL", HB_FS_PUBLIC, {hb_vmDoBlock}, NULL }; /* symbol to evaluate codeblocks */

static HB_ITEM  s_aStatics;         /* Harbour array to hold all application statics variables */
static USHORT   s_uiStatics;        /* Number of statics added after processing hb_vmStatics() */
static PHB_SYMB s_pSymStart = NULL; /* start symbol of the application. MAIN() is not required */
static PSYMBOLS s_pSymbols = NULL;  /* to hold a linked list of all different modules symbol tables */
static BYTE     s_byErrorLevel;     /* application exit errorlevel */

static BOOL     s_bDebugging;
static BOOL     s_bDebugRequest;    /* debugger invoked via the VM */
static BOOL     s_bDebugShowLines;  /* update source code line on the debugger display */
static BOOL     s_bDebuggerIsWorking; /* to know when __DBGENTRY is beeing invoked */

/* Various compatibility flags
*/
static ULONG	s_VMFlags = HB_VMFLAG_HARBOUR;
#undef hb_vmFlagEnabled
#define hb_vmFlagEnabled(flag)	(s_VMFlags & (flag))

/* Keycodes to stop virtual machine
*/
static int 		s_VMCancelKey = K_ALT_C;
static int		s_VMCancelKeyEx = HB_K_ALT_C;

/* Stores the position on the stack of current SEQUENCE envelope or 0 if no
 * SEQUENCE is active
 */
static LONG     s_lRecoverBase;
#define  HB_RECOVER_STATE     -1
#define  HB_RECOVER_BASE      -2
#define  HB_RECOVER_ADDRESS   -3
#define  HB_RECOVER_VALUE     -4

/* Stores level of procedures call stack
*/
static ULONG   s_ulProcLevel = 0;

int hb_vm_aiExtraParams[HB_MAX_MACRO_ARGS], hb_vm_iExtraParamsIndex = 0;
PHB_SYMB hb_vm_apExtraParamsSymbol[HB_MAX_MACRO_ARGS];

int hb_vm_aiExtraElements[HB_MAX_MACRO_ARGS], hb_vm_iExtraElementsIndex = 0, hb_vm_iExtraElements = 0;

int hb_vm_iExtraIndex;

/* Request for some action - stop processing of opcodes
 */
static USHORT   s_uiActionRequest;

char *hb_vm_sNull = "";

char *hb_vm_acAscii[256] = { "\x00", "\x01", "\x02", "\x03", "\x04", "\x05", "\x06", "\x07", "\x08", "\x09", "\x0A", "\x0B", "\x0C", "\x0D", "\x0E", "\x0F",
                             "\x10", "\x11", "\x12", "\x13", "\x14", "\x15", "\x16", "\x17", "\x18", "\x19", "\x1A", "\x1B", "\x1C", "\x1D", "\x1E", "\x1F",
                             "\x20", "\x21", "\x22", "\x23", "\x24", "\x25", "\x26", "\x27", "\x28", "\x29", "\x2A", "\x2B", "\x2C", "\x2D", "\x2E", "\x2F",
                             "\x30", "\x31", "\x32", "\x33", "\x34", "\x35", "\x36", "\x37", "\x38", "\x39", "\x3A", "\x3B", "\x3C", "\x3D", "\x3E", "\x3F",
                             "\x40", "\x41", "\x42", "\x43", "\x44", "\x45", "\x46", "\x47", "\x48", "\x49", "\x4A", "\x4B", "\x4C", "\x4D", "\x4E", "\x4F",
                             "\x50", "\x51", "\x52", "\x53", "\x54", "\x55", "\x56", "\x57", "\x58", "\x59", "\x5A", "\x5B", "\x5C", "\x5D", "\x5E", "\x5F",
                             "\x60", "\x61", "\x62", "\x63", "\x64", "\x65", "\x66", "\x67", "\x68", "\x69", "\x6A", "\x6B", "\x6C", "\x6D", "\x6E", "\x6F",
                             "\x70", "\x71", "\x72", "\x73", "\x74", "\x75", "\x76", "\x77", "\x78", "\x79", "\x7A", "\x7B", "\x7C", "\x7D", "\x7E", "\x7F",
                             "\x80", "\x81", "\x82", "\x83", "\x84", "\x85", "\x86", "\x87", "\x88", "\x89", "\x8A", "\x8B", "\x8C", "\x8D", "\x8E", "\x8F",
                             "\x90", "\x91", "\x92", "\x93", "\x94", "\x95", "\x96", "\x97", "\x98", "\x99", "\x9A", "\x9B", "\x9C", "\x9D", "\x9E", "\x9F",
                             "\xA0", "\xA1", "\xA2", "\xA3", "\xA4", "\xA5", "\xA6", "\xA7", "\xA8", "\xA9", "\xAA", "\xAB", "\xAC", "\xAD", "\xAE", "\xAF",
                             "\xB0", "\xB1", "\xB2", "\xB3", "\xB4", "\xB5", "\xB6", "\xB7", "\xB8", "\xB9", "\xBA", "\xBB", "\xBC", "\xBD", "\xBE", "\xBF",
                             "\xC0", "\xC1", "\xC2", "\xC3", "\xC4", "\xC5", "\xC6", "\xC7", "\xC8", "\xC9", "\xCA", "\xCB", "\xCC", "\xCD", "\xCE", "\xCF",
                             "\xD0", "\xD1", "\xD2", "\xD3", "\xD4", "\xD5", "\xD6", "\xD7", "\xD8", "\xD9", "\xDA", "\xDB", "\xDC", "\xDD", "\xDE", "\xDF",
                             "\xE0", "\xE1", "\xE2", "\xE3", "\xE4", "\xE5", "\xE6", "\xE7", "\xE8", "\xE9", "\xEA", "\xEB", "\xEC", "\xED", "\xEE", "\xEF",
                             "\xF0", "\xF1", "\xF2", "\xF3", "\xF4", "\xF5", "\xF6", "\xF7", "\xF8", "\xF9", "\xFA", "\xFB", "\xFC", "\xFD", "\xFE", "\xFF" };

/* 21/10/00 - maurilio.longo@libero.it
   This Exception Handler gets called in case of an abnormal termination of an harbour program and
   displays a full stack trace at the harbour language level */
#if defined(HB_OS_OS2)
ULONG _System OS2TermHandler(PEXCEPTIONREPORTRECORD       p1,
                             PEXCEPTIONREGISTRATIONRECORD p2,
                             PCONTEXTRECORD               p3,
                             PVOID                        pv);
#endif

/* call CLIPINIT function to initialize ErrorBlock() and __SetHelpK() */
static void hb_vmDoInitClip( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "CLIPINIT" );

   if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo(0);
   }
}

/* Initialize linked RDDs */
static void hb_vmDoInitRdd( void )
{
   PHB_DYNS pDynSym;
   int i;
   char * rddName[] = { "DBFDBTINIT",
                        "DBFFPTINIT",
                        "DBFNTXINIT",
                        "DBFCDXINIT",
                        "RDDINIT",
                        NULL };

   for ( i = 0; rddName[i]; i++ )
   {
      pDynSym = hb_dynsymFind( rddName[i] );
      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
      {
         hb_vmPushSymbol( pDynSym->pSymbol );
         hb_vmPushNil();
         hb_vmDo(0);
      }
   }
}

/* application entry point */

void HB_EXPORT hb_vmInit( BOOL bStartMainProc )
{

#if defined(HB_OS_OS2)
   EXCEPTIONREGISTRATIONRECORD RegRec = {0};       /* Exception Registration Record */
   APIRET rc = NO_ERROR;                           /* Return code                   */
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInit()"));

   /* initialize internal data structures */
   s_aStatics.type = HB_IT_NIL;
   s_byErrorLevel = 0;
   s_bDebugging = FALSE;
   s_bDebugShowLines = FALSE;
   s_bDebuggerIsWorking = FALSE;
   s_lRecoverBase = 0;
   s_uiActionRequest = 0;

   hb_stack.pItems = NULL; /* keep this here as it is used by fm.c */
   hb_stack.Return.type = HB_IT_NIL;

   hb_xinit();
   hb_errInit();
   hb_stackInit();
   hb_dynsymNew( &hb_symEval );  /* initialize dynamic symbol for evaluating codeblocks */
   hb_setInitialize();        /* initialize Sets */
   hb_conInit();    /* initialize Console */
   hb_memvarsInit();
   hb_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */

   /* Set the language to the default */

   /* This trick is needed to stringify the macro value */
   #define HB_LANG_SELECT_DEFAULT( id ) HB_LANG_SELECT_DEFAULT_( id )
   #define HB_LANG_SELECT_DEFAULT_( id ) hb_langSelectID( #id )
   HB_LANG_SELECT_DEFAULT( HB_LANG_DEFAULT );

   /* Check for some internal switches */
   s_VMFlags = hb_cmdargProcessVM( &s_VMCancelKey, &s_VMCancelKeyEx );
   hb_inkeySetCancelKeys( s_VMCancelKey, s_VMCancelKeyEx );

   /* Initialize opcodes profiler support arrays */
   {
      ULONG ul;

      for( ul = 0; ul < HB_P_LAST_PCODE; ul++ )
      {
         hb_ulOpcodesCalls[ ul ] = 0;
         hb_ulOpcodesTime[ ul ] = 0;
      }
   }

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables
    */
   hb_vmDoInitStatics();
   /* call CLIPINIT function to initialize ErrorBlock() and __SetHelpK()
    * Because on some platform the execution order of init functions
    * is out of Harbour control then this function has to be called
    * explicitly in VM initialization process before hb_vmDoInitFunctions()
    * and not depends on INIT clause.
    */
   hb_vmDoInitClip();      
   hb_vmDoInitRdd();       /* initialize the Harbour's RDDs */
   hb_vmDoInitFunctions(); /* process defined INIT functions */

   /* This is undocumented CA-Clipper, if there's a function called _APPMAIN
      it will be executed first. [vszakats] */
   {
      PHB_DYNS pDynSym = hb_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
         s_pSymStart = pDynSym->pSymbol;
#ifdef HARBOUR_START_PROCEDURE
      else
      {
         /* if first char is '@' then start procedure were set by
            programmer explicitly and should have the highest priority
            in other case it's the name of first public function in
            first linked moudule which is used if there is no
            HARBOUR_START_PROCEDURE in code */
         if( s_pszLinkedMain && *s_pszLinkedMain == '@' )
            pDynSym = hb_dynsymFind( s_pszLinkedMain + 1 );
         else
         {
            pDynSym = hb_dynsymFind( HARBOUR_START_PROCEDURE );

            if( ! ( pDynSym && pDynSym->pSymbol->value.pFunPtr ) && s_pszLinkedMain )
               pDynSym = hb_dynsymFind( s_pszLinkedMain );
         }

         if( pDynSym && pDynSym->pSymbol->value.pFunPtr )
            s_pSymStart = pDynSym->pSymbol;
         else
            hb_errInternal( HB_EI_VMBADSTARTUP, NULL, HARBOUR_START_PROCEDURE, NULL );
      }
#else
#ifndef HB_C52_STRICT
      else if( bStartMainProc && ! s_pSymStart )
         hb_errInternal( HB_EI_VMNOSTARTUP, NULL, NULL, NULL );
#endif
#endif
   }

#if defined(HB_OS_OS2) /* Add OS2TermHandler to this thread's chain of exception handlers */

   RegRec.ExceptionHandler = (ERR)OS2TermHandler;
   rc = DosSetExceptionHandler( &RegRec );
   if (rc != NO_ERROR) {
      hb_errInternal( HB_EI_ERRUNRECOV, "Unable to setup exception handler (DosSetExceptionHandler())", NULL, NULL );
   }
#endif

   if( bStartMainProc && s_pSymStart )
   {
      int i;
      int iArgCount;

      hb_vmPushSymbol( s_pSymStart ); /* pushes first HB_FS_PUBLIC defined symbol to the stack */
      hb_vmPushNil();                 /* places NIL at self */

      iArgCount = 0;
      for( i = 1; i < hb_cmdargARGC(); i++ )     /* places application parameters on the stack */
      {
         char ** argv = hb_cmdargARGV();

         /* Filter out any parameters beginning with //, like //INFO */
         if( ! hb_cmdargIsInternal( argv[ i ] ) )
         {
            hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
            iArgCount++;
         }
      }

      hb_vmDo( iArgCount ); /* invoke it with number of supplied parameters */
   }

#if defined(HB_OS_OS2)
   /* I don't do any check on return code since harbour is exiting in any case */
   rc = DosUnsetExceptionHandler( &RegRec );
#endif
}

void HB_EXPORT hb_vmQuit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmQuit()"));

   #ifdef HB_MACRO_STATEMENTS
     hb_pp_Free();
   #endif

   s_uiActionRequest = 0;         /* EXIT procedures should be processed */
   hb_vmDoExitFunctions();       /* process defined EXIT functions */

   /* release all known items stored in subsystems */
   hb_rddShutDown();
   hb_idleShutDown();
   hb_errExit();
   hb_clsReleaseAll();
   hb_vmReleaseLocalSymbols();  /* releases the local modules linked list */
   hb_dynsymRelease();          /* releases the dynamic symbol table */
   hb_conRelease();             /* releases Console */
   hb_setRelease();             /* releases Sets */
   hb_cdpReleaseAll();          /* releases codepages */

   /* release all remaining items */
   hb_stackRemove( 0 );
   hb_itemClear( &hb_stack.Return );
   hb_arrayRelease( &s_aStatics );
   hb_memvarsRelease();    /* clear all PUBLIC variables */

   /* release all known garbage */
   /* hb_gcReleaseAll(); */

   /* release all known garbage */
   if ( hb_xquery( HB_MEM_USEDMAX ) ) /* check if fmstat is ON */
      hb_gcCollectAll();
   else
      hb_gcReleaseAll();

   hb_memvarsFree();    /* free memory allocated for memvars table */
   hb_stackFree();
/* hb_dynsymLog(); */
   hb_xexit();

   exit( s_byErrorLevel );
}

void HB_EXPORT hb_vmExecute( const BYTE * pCode, PHB_SYMB pSymbols )
{
   LONG w = 0;
   BOOL bCanRecover = FALSE;
   BYTE curPCode;
   ULONG ulPrivateBase;
   ULONG ulLastOpcode = 0; /* opcodes profiler support */
   ULONG ulPastClock = 0;  /* opcodes profiler support */
#ifndef HB_GUI
   static unsigned short uiPolls = 1;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmExecute(%p, %p)", pCode, pSymbols));

   /* NOTE: if pSymbols == NULL then hb_vmExecute is called from macro
    * evaluation. In this case all PRIVATE variables created during
    * macro evaluation belong to a function/procedure where macro
    * compiler was called.
    */
   /* NOTE: Initialization with 0 is needed to avoid GCC -O2 warning */
   ulPrivateBase = pSymbols ? hb_memvarGetPrivatesBase() : 0;

   if( hb_bProfiler )
      ulPastClock = ( ULONG ) clock();

   while( ( curPCode = pCode[ w ] ) != HB_P_ENDPROC )
   {
      if( hb_bProfiler )
      {
         ULONG ulActualClock = ( ULONG ) clock();

         hb_ulOpcodesTime[ ulLastOpcode ] += ( ulActualClock - ulPastClock );
         ulPastClock = ulActualClock;
         ulLastOpcode = pCode[ w ];
         hb_ulOpcodesCalls[ ulLastOpcode ]++;
      }

#ifndef HB_GUI
      if( ! --uiPolls )
      {
         hb_inkeyPoll();
         uiPolls = 255;
         /* IMHO we should have a _SET_ controlled by user
          * sth like:

         if( hb_set.HB_SET_KEYPOLL )
         {
            hb_inkeyPoll();
            uiPolls = hb_set.HB_SET_KEYPOLL;
         }

         for some GTs which can work in assynchrous mode user may
         set it to 0 (or if he doesn't need any inkey poll) and
         when ALT+C/ALT+D is pressed (or any other platform dependent
         key combination) they should set proper flags in
         s_uiActionRequest so we can serve it in main VM loop without
         performance decrease or ignore depending on
         hb_set.HB_SET_CANCEL, hb_set.HB_SET_DEBUG flags
         */
      }
#endif

      switch( curPCode )
      {
         /* Operators ( mathematical / character / misc ) */

         case HB_P_NEGATE:
            hb_vmNegate();
            w++;
            break;

         case HB_P_PLUS:
            hb_vmPlus();
            w++;
            break;

         case HB_P_MINUS:
            hb_vmMinus();
            w++;
            break;

         case HB_P_MULT:
            hb_vmMult();
            w++;
            break;

         case HB_P_DIVIDE:
            hb_vmDivide();
            w++;
            break;

         case HB_P_MODULUS:
            hb_vmModulus();
            w++;
            break;

         case HB_P_POWER:
            hb_vmPower();
            w++;
            break;

         case HB_P_INC:
            hb_vmInc();
            w++;
            break;

         case HB_P_DEC:
            hb_vmDec();
            w++;
            break;

         case HB_P_FUNCPTR:
            hb_vmFuncPtr();
            w++;
            break;

         /* Operators (relational) */

         case HB_P_EQUAL:
            hb_vmEqual( FALSE );
            w++;
            break;

         case HB_P_EXACTLYEQUAL:
            hb_vmEqual( TRUE );
            w++;
            break;

         case HB_P_NOTEQUAL:
            hb_vmNotEqual();
            w++;
            break;

         case HB_P_LESS:
            hb_vmLess();
            w++;
            break;

         case HB_P_LESSEQUAL:
            hb_vmLessEqual();
            w++;
            break;

         case HB_P_GREATER:
            hb_vmGreater();
            w++;
            break;

         case HB_P_GREATEREQUAL:
            hb_vmGreaterEqual();
            w++;
            break;

         case HB_P_INSTRING:
            hb_vmInstring();
            w++;
            break;

         case HB_P_FORTEST:
            hb_vmForTest();
            w++;
            break;

         /* Operators (logical) */

         case HB_P_NOT:
            hb_vmNot();
            w++;
            break;

         case HB_P_AND:
            hb_vmAnd();
            w++;
            break;

         case HB_P_OR:
            hb_vmOr();
            w++;
            break;

         /* Array */

         case HB_P_ARRAYPUSH:
            hb_vmArrayPush();
            w++;
            break;

         case HB_P_ARRAYPOP:
            hb_vmArrayPop();
            w++;
            break;

         case HB_P_ARRAYDIM:
            hb_vmArrayDim( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_ARRAYGEN:
            hb_vmArrayGen( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) + hb_vm_iExtraElements );
            hb_vm_iExtraElements = 0;
            w += 3;
            break;

         /* Object */

         case HB_P_MESSAGE:
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         /* Database */

         case HB_P_SWAPALIAS:
            hb_vmSwapAlias();
            w++;
            break;

         /* Execution */

         case HB_P_DO:
            hb_vmDo( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DOSHORT:
            hb_vmDo( pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_FUNCTION:
            if( HB_IS_COMPLEX( &hb_stack.Return ) )
               hb_itemClear( &hb_stack.Return );
            else
               hb_stack.Return.type = HB_IT_NIL;

            hb_vmDo( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_itemMove( hb_stackTopItem(), &hb_stack.Return );
            hb_stackPush();
            w += 3;
            break;

         case HB_P_FUNCTIONSHORT:
            if( HB_IS_COMPLEX( &hb_stack.Return ) )
               hb_itemClear( &hb_stack.Return );
            else
               hb_stack.Return.type = HB_IT_NIL;

            hb_vmDo( pCode[ w + 1 ] );
            hb_itemMove( hb_stackTopItem(), &hb_stack.Return );
            hb_stackPush();
            w += 2;
            break;

         case HB_P_SEND:
            if( HB_IS_COMPLEX( &hb_stack.Return ) )
               hb_itemClear( &hb_stack.Return );
            else
               hb_stack.Return.type = HB_IT_NIL;

            hb_vmSend( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;

            /* Is This OK??? */
            if( pCode[ w ] == HB_P_POP )
               w++;
            else
            {
               hb_itemMove( hb_stackTopItem(), &hb_stack.Return );
               hb_stackPush();
            }
            break;

         case HB_P_SENDSHORT:
            if( HB_IS_COMPLEX( &hb_stack.Return ) )
               hb_itemClear( &hb_stack.Return );
            else
               hb_stack.Return.type = HB_IT_NIL;

            hb_vmSend( pCode[ w + 1 ] );
            w += 2;

            if( pCode[ w ] == HB_P_POP )
               w++;
            else
            {
               hb_itemMove( hb_stackTopItem(), &hb_stack.Return );
               hb_stackPush();
            }
            break;

         case HB_P_LINE:

            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINE: %s (%i)", (hb_stackBaseItem())->item.asSymbol.value->szName, (hb_stackBaseItem())->item.asSymbol.lineno));

            (hb_stackBaseItem())->item.asSymbol.lineno = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            if( s_bDebugging && s_bDebugShowLines )
               hb_vmDebuggerShowLine( (hb_stackBaseItem())->item.asSymbol.lineno );
            w += 3;
            break;

         case HB_P_PARAMETER:
            hb_memvarNewParameter( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), hb_stackItemFromBase( pCode[ w + 3 ] ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopParameter)"));
            w += 4;
            break;

         case HB_P_FRAME:
            hb_vmFrame( pCode[ w + 1 ], pCode[ w + 2 ] );
            w += 3;
            break;

         case HB_P_SFRAME:
            hb_vmSFrame( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_STATICS:
            hb_vmStatics( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), HB_PCODE_MKUSHORT( &( pCode[ w + 3 ] ) ) );
            w += 5;
            break;

         case HB_P_RETVALUE:
            hb_vmRetValue();
            w++;
            break;

         case HB_P_LOCALNAME:
            hb_vmLocalName( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ),
                            ( char * ) pCode + w + 3 );
            w += 3;
            while( pCode[ w++ ] );
            break;

         case HB_P_STATICNAME:
            hb_vmStaticName( pCode[ w + 1 ], HB_PCODE_MKUSHORT( &( pCode[ w + 2 ] ) ),
                             ( char * ) pCode + w + 4 );
            w += 4;
            while( pCode[ w++ ] );
            break;

         case HB_P_MODULENAME:
            hb_vmModuleName( ( char * ) pCode + w + 1 );
            while( pCode[ w++ ] );
            break;

         case HB_P_ENDBLOCK:
            HB_TRACE(HB_TR_INFO, ("(EndBlock)"));
            hb_vmEndBlock();
            if( pSymbols )
               hb_memvarSetPrivatesBase( ulPrivateBase );
            return;   /* end of a codeblock - stop evaluation */

         /* BEGIN SEQUENCE/RECOVER/END SEQUENCE */

         case HB_P_SEQBEGIN:
            /*
             * Create the SEQUENCE envelope
             * [ break return value      ]  -4
             * [ address of recover code ]  -3
             * [ previous recover base   ]  -2
             * [ current recovery state  ]  -1
             * [                         ] <- new recover base
             */
            /*
             * 1) clear the storage for value returned by BREAK statement
             */
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            hb_stackPush();
            /*
             * 2) store the address of RECOVER or END opcode
             */
            ( hb_stackTopItem() )->type = HB_IT_LONG;
            ( hb_stackTopItem() )->item.asLong.value = w + HB_PCODE_MKINT24( &pCode[ w + 1 ] );;
            hb_stackPush();
            /*
             * 3) store current RECOVER base
             */
            ( hb_stackTopItem() )->type = HB_IT_LONG;
            ( hb_stackTopItem() )->item.asLong.value = s_lRecoverBase;
            hb_stackPush();
            /*
             * 4) store current bCanRecover flag - in a case of nested sequences
             * in the same procedure/function
             */
            ( hb_stackTopItem() )->type = HB_IT_LOGICAL;
            ( hb_stackTopItem() )->item.asLogical.value = bCanRecover;
            hb_stackPush();
            /*
             * set new recover base
             */
            s_lRecoverBase = hb_stackTopOffset();
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = TRUE;
            w += 4;
            break;

         case HB_P_SEQEND:
            /*
             * Remove the SEQUENCE envelope
             * This is executed either at the end of sequence or as the
             * response to the break statement if there is no RECOVER clause
             */
            /*
             * 4) Restore previous recovery state
             */
            hb_stackDec();
            bCanRecover = ( hb_stackTopItem() )->item.asLogical.value;
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /*
             * 3) Restore previous RECOVER base
             */
            hb_stackDec();
            s_lRecoverBase = ( hb_stackTopItem() )->item.asLong.value;
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /*
             * 2) Remove RECOVER address
             */
            hb_stackDec();
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /* 1) Discard the value returned by BREAK statement - there
             * was no RECOVER clause or there was no BREAK statement
             */
            hb_stackPop();
            /*
             * skip outside of SEQUENCE structure
             */
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_SEQRECOVER:
            /*
             * Execute the RECOVER code
             */
            /*
             * 4) Restore previous recovery state
             */
            hb_stackDec();
            bCanRecover = ( hb_stackTopItem() )->item.asLogical.value;
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /*
             * 3) Restore previous RECOVER base
             */
            hb_stackDec();
            s_lRecoverBase = ( hb_stackTopItem() )->item.asLong.value;
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /*
             * 2) Remove RECOVER address
             */
            hb_stackDec();
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            /*
             * 1) Leave the value returned from BREAK  - it will be popped
             * in next executed opcode
             */
            w++;
            break;

         /* Jumps */

         case HB_P_JUMPNEAR:
            w += (signed char) pCode[ w + 1 ];
            break;

         case HB_P_JUMP:
            w += HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) );;
            break;

         case HB_P_JUMPFAR:
            w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            break;

         case HB_P_JUMPFALSENEAR:
            if( ! hb_vmPopLogical() )
               w += (signed char) pCode[ w + 1 ];
            else
               w += 2;
            break;

         case HB_P_JUMPFALSE:
            if( ! hb_vmPopLogical() )
               w += HB_PCODE_MKSHORT( &pCode[ w + 1 ] );
            else
               w += 3;
            break;

         case HB_P_JUMPFALSEFAR:
            if( ! hb_vmPopLogical() )
               w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            else
               w += 4;
            break;

         case HB_P_JUMPTRUENEAR:
            if( hb_vmPopLogical() )
               w += (signed char) pCode[ w + 1 ];
            else
               w += 2;
            break;

         case HB_P_JUMPTRUE:
            if( hb_vmPopLogical() )
               w += HB_PCODE_MKSHORT( &pCode[ w + 1 ] );
            else
               w += 3;
            break;

         case HB_P_JUMPTRUEFAR:
            if( hb_vmPopLogical() )
               w += HB_PCODE_MKINT24( &pCode[ w + 1 ] );
            else
               w += 4;
            break;

         /* Push */

         case HB_P_TRUE:
            {
               PHB_ITEM pStackTopItem = hb_stackTopItem();

               pStackTopItem->type = HB_IT_LOGICAL;
               pStackTopItem->item.asLogical.value = TRUE;
               hb_stackPush();
               w++;
            }
            break;

         case HB_P_FALSE:
            {
               PHB_ITEM pStackTopItem = hb_stackTopItem();

               pStackTopItem->type = HB_IT_LOGICAL;
               pStackTopItem->item.asLogical.value = FALSE;
               hb_stackPush();
               w++;
            }
            break;

         case HB_P_ONE:
            {
               PHB_ITEM pStackTopItem = hb_stackTopItem();

               pStackTopItem->type = HB_IT_INTEGER;
               pStackTopItem->item.asInteger.value = 1;
               pStackTopItem->item.asInteger.length = 10;
               hb_stackPush();
               HB_TRACE(HB_TR_INFO, ("(HB_P_ONE)"));
               w++;
            }
            break;

         case HB_P_ZERO:
            {
               PHB_ITEM pStackTopItem = hb_stackTopItem();

               pStackTopItem->type = HB_IT_INTEGER;
               pStackTopItem->item.asInteger.value = 0;
               pStackTopItem->item.asInteger.length = 10;
               hb_stackPush();
               HB_TRACE(HB_TR_INFO, ("(HB_P_ZERO)"));
               w++;
            }
            break;

         case HB_P_PUSHNIL:
            ( hb_stackTopItem() )->type = HB_IT_NIL;
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHNIL)"));
            w++;
            break;

         case HB_P_PUSHBYTE:
            {
               PHB_ITEM pStackTopItem = hb_stackTopItem();

               pStackTopItem->type = HB_IT_INTEGER;
               pStackTopItem->item.asInteger.value = ( signed char ) pCode[ w + 1 ];
               pStackTopItem->item.asInteger.length = 10;
               hb_stackPush();
               HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHBYTE)"));
               w += 2;
            }
            break;

         case HB_P_PUSHINT:
            HB_TRACE(HB_TR_INFO, ("(HB_P_PUSHINT)"));
            hb_vmPushInteger( HB_PCODE_MKSHORT( &pCode[ w + 1 ] ) );
            w += 3;
            break;

         case HB_P_PUSHLONG:
            HB_TRACE( HB_TR_DEBUG, ("(HB_P_PUSHLONG)") );
#if HB_INT_MAX >= INT32_MAX
            hb_vmPushIntegerConst( ( int ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushLongConst( ( long ) HB_PCODE_MKLONG( &pCode[ w + 1 ] ) );
#endif
            w += 5;
            break;

         case HB_P_PUSHLONGLONG:
            HB_TRACE( HB_TR_DEBUG, ("(HB_P_PUSHLONGLONG)") );
#if !defined( HB_LONG_LONG_OFF )
            hb_vmPushLongLongConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ) );
#else
            hb_vmPushDoubleConst( HB_PCODE_MKLONGLONG( &pCode[ w + 1 ] ),
                                  HB_DEFAULT_WIDTH, HB_DEFAULT_DECIMALS );
#endif
            w += 9;

            break;

         case HB_P_PUSHDOUBLE:
            hb_vmPushDoubleConst( HB_PCODE_MKDOUBLE( &pCode[ w + 1 ] ),
                                  ( int ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) ],
                                  ( int ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) + sizeof( BYTE ) ] );
            w += 1 + sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE );
            break;

         case HB_P_PUSHSTR:
         {
            USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            hb_vmPushStringPcode( ( char * ) pCode + w + 3, ( ULONG ) uiSize - 1 );
            w += ( 3 + uiSize );
            break;
         }

         case HB_P_PUSHSTRSHORT:
            hb_vmPushStringPcode( ( char * ) pCode + w + 2, ( ULONG ) pCode[ w + 1 ] - 1 );
            w += ( 2 + pCode[ w + 1 ] );
            break;

         case HB_P_PUSHBLOCK:
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            hb_vmPushBlock( ( BYTE * ) ( pCode + w ), pSymbols );
            w += HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            break;

         case HB_P_PUSHBLOCKSHORT:
            /* +0    -> _pushblock
             * +1    -> size of codeblock
             */
            hb_vmPushBlockShort( ( BYTE * ) ( pCode + w ), pSymbols );
            w += pCode[ w + 1 ];
            break;

         case HB_P_PUSHSELF:
            hb_vmPush( hb_stackSelfItem() );
            w++;
            break;

         case HB_P_PUSHSYM:
            hb_vmPushSymbol( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSYMNEAR:
            hb_vmPushSymbol( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIAS:
            hb_vmPushAlias();
            w++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            hb_vmPushAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHALIASEDFIELDNEAR:
            hb_vmPushAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_PUSHALIASEDVAR:
            hb_vmPushAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHFIELD:
            /* It pushes the current value of the given field onto the eval stack
             */
            hb_rddGetFieldValue( ( hb_stackTopItem() ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushField)"));
            w += 3;
            break;

         case HB_P_PUSHLOCAL:
            hb_vmPushLocal( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHLOCALNEAR:
            hb_vmPushLocal( ( signed char ) pCode[ w + 1 ] );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_PUSHLOCALREF:
            hb_vmPushLocalByRef( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATIC:
            hb_vmPushStatic( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHSTATICREF:
            hb_vmPushStaticByRef( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_PUSHMEMVAR:
            hb_memvarGetValue( ( hb_stackTopItem() ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvar)"));
            w += 3;
            break;

         case HB_P_PUSHMEMVARREF:
            hb_memvarGetRefer( ( hb_stackTopItem() ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvarRef)"));
            w += 3;
            break;

         case HB_P_PUSHVARIABLE:
            /* Push a value of variable of unknown type onto the eval stack
             */
            hb_vmPushVariable( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_DUPLICATE:
            hb_vmDuplicate();
            w++;
            break;

         case HB_P_DUPLTWO:
            hb_vmDuplTwo();
            w++;
            break;

         /* Pop */

         case HB_P_POP:
            hb_stackPop();
            w++;
            break;

         case HB_P_POPALIAS:
            hb_vmPopAlias();
            w++;
            break;

         case HB_P_POPALIASEDFIELD:
            hb_vmPopAliasedField( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPALIASEDFIELDNEAR:
            hb_vmPopAliasedField( pSymbols + pCode[ w + 1 ] );
            w += 2;
            break;

         case HB_P_POPALIASEDVAR:
            hb_vmPopAliasedVar( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPFIELD:
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            hb_rddPutFieldValue( ( hb_stackItemFromTop(-1) ), pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            hb_stackDec();
            hb_itemClear( ( hb_stackTopItem() ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopField)"));
            w += 3;
            break;

         case HB_P_POPLOCAL:
            hb_vmPopLocal( HB_PCODE_MKSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPLOCALNEAR:
            hb_vmPopLocal( ( signed char ) pCode[ w + 1 ] );
            w += 2;  /* only first two bytes are used */
            break;

         case HB_P_POPSTATIC:
            hb_vmPopStatic( HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ) );
            w += 3;
            break;

         case HB_P_POPMEMVAR:
            hb_stackDec();
            hb_memvarSetValue( pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) ), ( hb_stackTopItem() ) );
            hb_itemClear( ( hb_stackTopItem() ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopMemvar)"));
            w += 3;
            break;

         case HB_P_POPVARIABLE:
         {
            USHORT uiParams;
            PHB_DYNS pDyn;

            /* Pops a value from the eval stack and uses it to set
             * a new value of a variable of unknown type.
             */
            uiParams = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            /* First try if passed symbol is a name of field
             * in a current workarea - if it is not a field (FAILURE)
             * then try the memvar variable (it will create PRIVATE
             * variable if this variable doesn't exist)
             */

            /* memvars.c 417 */
            pDyn = ( PHB_DYNS ) (pSymbols + uiParams)->pDynSym;
            if( pDyn && pDyn->hMemvar )
            {
               /* If exist a memory symbol with this name use it */
               hb_memvarSetValue( pSymbols + uiParams, ( hb_stackItemFromTop(-1) ) );
            } else {
               /* Try with a field and after create a memvar */
               if( hb_rddFieldPut( ( hb_stackItemFromTop(-1) ), pSymbols + uiParams ) == FAILURE )
                  hb_memvarSetValue( pSymbols + uiParams, ( hb_stackItemFromTop(-1) ) );
            }
            hb_stackDec();
            hb_itemClear( ( hb_stackTopItem() ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopVariable)"));
            w += 3;
            break;
         }

         /* macro creation */

         case HB_P_MACROPOP:
            /* compile and run - pop a value from the stack */
            hb_macroSetValue( hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPOPALIASED:
            /* compile and run - pop an aliased variable from the stack */
            hb_macroPopAliasedValue( hb_stackItemFromTop( - 2  ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSH:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), 0, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHARG:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHARG, pCode[ ++w ] );
            w++;

            if( hb_vm_iExtraParamsIndex && hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] == NULL )
            {
               if( pCode[w] == HB_P_PUSHSYMNEAR )
               {
                  hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] = pSymbols + ( USHORT ) ( pCode[w + 1] );
                  w += 2;
               }
               else if( pCode[w] == HB_P_MPUSHSYM )
               {
                  HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );

                  hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] = pDynSym->pSymbol;
                  w += sizeof( HB_DYNS_PTR ) + 1;
               }
               else
               {
                  hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] = pSymbols + HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
                  w += 3;
               }
            }
            else
            {
               if( pCode[w] == HB_P_PUSHSYMNEAR )
               {
                  w += 2;
               }
               else if( pCode[w] == HB_P_MPUSHSYM )
               {
                  w += sizeof( HB_DYNS_PTR ) + 1;
               }
               else
               {
                  w += 3;
               }
            }
            break;

         case HB_P_MACROLIST:
            hb_vm_aiExtraElements[hb_vm_iExtraElementsIndex++] = 0;
            w++;
            break;

         case HB_P_MACROPUSHLIST:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHLIST, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROLISTEND:
            hb_vm_iExtraElements = hb_vm_aiExtraElements[--hb_vm_iExtraElementsIndex];
            w++;
            break;

         case HB_P_MACROPUSHINDEX:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHINDEX, pCode[ ++w ] );
            if( hb_vm_iExtraIndex )
            {
               HB_ITEM *aExtraItems = (HB_ITEM *) hb_xgrab( sizeof( HB_ITEM ) * hb_vm_iExtraIndex );
               int i;

               /* Storing and removing the extra indexes. */
               for ( i = hb_vm_iExtraIndex - 1; i >= 0; i-- )
               {
                  hb_itemCopy( aExtraItems + i, hb_stackItemFromTop(-1) );
                  hb_stackPop();
               }

               /* First index is still on stack.*/
               hb_vmArrayPush();

               /* Now process each of the additional index including the last one (we will skip the HB_P_ARRAYPUSH which is know to follow . */
               for ( i = 0; i < hb_vm_iExtraIndex; i++ )
               {
                  hb_vmPush( aExtraItems + i );
                  hb_vmArrayPush();
               }

               hb_xfree( aExtraItems );

               w++; /* To force skip the HB_P_ARRAYPUSH (was already processed above). */
            }

            w++;
            break;

         case HB_P_MACROPUSHPARE:
            /* compile and run - leave the result on the stack */
            /* the topmost element on the stack contains a macro
             * string for compilation
             */
            hb_macroGetValue( hb_stackItemFromTop( -1 ), HB_P_MACROPUSHPARE, pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHALIASED:
            /* compile and run - leave an aliased variable on the stack */
            hb_macroPushAliasedValue( hb_stackItemFromTop( -2 ), hb_stackItemFromTop( -1 ), pCode[ ++w ] );
            w++;
            break;

         case HB_P_MACROPUSHREF:
            {
               PHB_ITEM pMacro = hb_stackItemFromTop( -1 );
               PHB_SYMB pSym;
               /* compile into a symbol name (used in function calls) */
               hb_macroPushSymbol( pMacro );
               /* NOTE: pMacro string is replaced with a symbol.
                * Symbol is created if it doesn't exist.
               */
               pSym = pMacro->item.asSymbol.value;
               /* NOTE: pMacro item of symbol type is replaced with 
                *  the reference 
               */
               hb_memvarGetRefer( pMacro, pSym );
               w++;
            }
            break;

         case HB_P_MACROSYMBOL:
            /* compile into a symbol name (used in function calls) */
            hb_macroPushSymbol( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         case HB_P_MACROTEXT:
            /* macro text substitution
             * "text &macro.other text"
             */
            hb_macroTextValue( hb_stackItemFromTop( -1 ) );
            w++;
            break;

         /* macro compiled opcodes - we are using symbol address here */

         case HB_P_MMESSAGE:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPopAliasedField( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPALIASEDVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPopAliasedVar( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            /* Pops a value from the eval stack and uses it to set
            * a new value of the given field
            */
            hb_rddPutFieldValue( ( hb_stackItemFromTop(-1) ), pDynSym->pSymbol );
            hb_stackDec();
            hb_itemClear( ( hb_stackTopItem() ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPopField)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPOPMEMVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_stackDec();
            hb_memvarSetValue( pDynSym->pSymbol, ( hb_stackTopItem() ) );
            hb_itemClear( ( hb_stackTopItem() ) );
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPopMemvar)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushAliasedField( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHALIASEDVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushAliasedVar( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
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
            hb_vmPushMacroBlock( ( BYTE * ) ( pCode + w ), pSymbols );
            w += HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );
            break;
         }

         case HB_P_MPUSHFIELD:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            /* It pushes the current value of the given field onto the eval stack
            */
            hb_rddGetFieldValue( ( hb_stackTopItem() ), pDynSym->pSymbol );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushField)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVAR:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_memvarGetValue( ( hb_stackTopItem() ), pDynSym->pSymbol );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvar)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHMEMVARREF:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_memvarGetRefer( ( hb_stackTopItem() ), pDynSym->pSymbol );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmMPushMemvarRef)"));
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHSYM:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushSymbol( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHVARIABLE:
         {
            HB_DYNS_PTR pDynSym = ( HB_DYNS_PTR ) HB_GET_PTR( pCode + w + 1 );
            hb_vmPushVariable( pDynSym->pSymbol );
            w += sizeof( HB_DYNS_PTR ) + 1;
            break;
         }

         case HB_P_MPUSHSTR:
         {
            USHORT uiSize = HB_PCODE_MKUSHORT( &( pCode[ w + 1 ] ) );

            hb_vmPushString( ( char * ) ( pCode + w + 3 ), ( ULONG )( uiSize - 1 ) );
            w += ( 3 + uiSize );
            break;
         }

         case HB_P_LOCALNEARADDINT:
            HB_TRACE( HB_TR_DEBUG, ("HB_P_LOCALNEARADDINT") );
         {
            PHB_ITEM pLocal = hb_stackItemFromBase( pCode[ w + 1 ] );
            int iAdd = HB_PCODE_MKSHORT( &( pCode[ w + 2 ] ) );
            double dNewVal;

            w += 4;

            if( HB_IS_BYREF( pLocal ) )
            {
               pLocal = hb_itemUnRef( pLocal );
            }

            if( HB_IS_NUMINT( pLocal ) )
            {
               HB_LONG lNewVal, lVal = HB_ITEM_GET_NUMINTRAW( pLocal );

               lNewVal = lVal + iAdd;

               if( iAdd >= 0 ? lNewVal >= lVal : lNewVal <  lVal )
               {
                  HB_ITEM_PUT_NUMINTRAW( pLocal, lNewVal );
                  break;
               }
               else
               {
                  dNewVal = ( double ) lVal + ( double ) iAdd;
               }
            }
            else if( HB_IS_DATE( pLocal ) )
            {
               pLocal->item.asDate.value += iAdd;
               break;
            }
            else if( pLocal->type & HB_IT_DOUBLE )
            {
               dNewVal = pLocal->item.asDouble.value + iAdd;
            }
            else
            {
               PHB_ITEM pAdd = hb_itemPutNI( NULL, ( int ) iAdd );
               PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pLocal, pAdd );

               hb_itemRelease( pAdd );

               if( pResult )
               {
                  hb_itemMove( pLocal, pResult );
               }

               break;
            }

            if( !HB_IS_DOUBLE( pLocal ) )
            {
               pLocal->type = HB_IT_DOUBLE;
               pLocal->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
            }
            pLocal->item.asDouble.value = dNewVal;
            pLocal->item.asDouble.length = HB_DBL_LENGTH( dNewVal );
            break;
         }

         /* misc */

         case HB_P_NOOP:
            /* Intentionally do nothing */
            w++;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            hb_errInternal( HB_EI_VMBADOPCODE, NULL, NULL, NULL );
            break;
      }

      if( s_uiActionRequest )
      {
         if( s_uiActionRequest & HB_BREAK_REQUESTED )
         {
            if( bCanRecover )
            {
               /*
                * There is the BEGIN/END sequence deifined in current
                * procedure/function - use it to continue opcodes execution
                */
               /*
                * remove all items placed on the stack after BEGIN code
                */
               hb_stackRemove( s_lRecoverBase );
               /*
                * reload the address of recovery code
                */
               w = ( hb_stackItem( s_lRecoverBase + HB_RECOVER_ADDRESS ) )->item.asLong.value;
               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */
               s_uiActionRequest = 0;
            }
            else
               break;
         }
         else if( s_uiActionRequest & HB_QUIT_REQUESTED )
            break;
         else if( s_uiActionRequest & HB_ENDPROC_REQUESTED )
         {
            /* request to stop current procedure was issued
             * (from macro evaluation)
             */
            s_uiActionRequest = 0;
            break;
         }
      }
   }

   if( pSymbols )
      hb_memvarSetPrivatesBase( ulPrivateBase );
}

/* ------------------------------- */
/* Operators ( mathematical        */
/*             character / misc )  */
/* ------------------------------- */

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate( void )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNegate()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_INTEGER( pItem ) )
   {
#if -HB_INT_MAX > HB_INT_MIN
      if ( pItem->item.asInteger.value < -HB_INT_MAX )
      {
#if HB_LONG_MAX > HB_INT_MAX
         HB_LONG lValue = ( HB_LONG ) pItem->item.asInteger.value;
         pItem->type = HB_IT_LONG;
         pItem->item.asLong.value = -lValue;
         pItem->item.asLong.length = HB_LONG_LENGTH( -lValue );
#else
         double dValue = ( double ) pItem->item.asInteger.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
#endif
      }
      else
#endif
      {
         pItem->item.asInteger.value = -pItem->item.asInteger.value;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
   }
   else if( HB_IS_LONG( pItem ) )
   {
#if -HB_LONG_MAX > HB_LONG_MIN
      if ( pItem->item.asLong.value < -HB_LONG_MAX )
      {
         double dValue = ( double ) pItem->item.asLong.value;
         pItem->type = HB_IT_DOUBLE;
         pItem->item.asDouble.value = -dValue;
         pItem->item.asDouble.length = HB_DBL_LENGTH( -dValue );
      }
      else
#endif
      {
         pItem->item.asLong.value = -pItem->item.asLong.value;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value = -pItem->item.asDouble.value;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1080, NULL, "-", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmPlus( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPlus()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber1 = HB_ITEM_GET_NUMINTRAW( pItem1 );
      HB_LONG lNumber2 = HB_ITEM_GET_NUMINTRAW( pItem2 );
      HB_LONG lResult = lNumber1 + lNumber2;

      hb_stackDec();
      pItem2->type = HB_IT_NIL;

      if ( lNumber2 >= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
      {
         HB_ITEM_PUT_NUMINTRAW( pItem1, lResult );
      }
      else
      {
         hb_stackDec();
         hb_vmPushDouble( ( double ) lNumber1 + ( double ) lNumber2, 0 );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1, iType2 = pItem2->type, iType1 = pItem2->type;
      double dNumber2 = hb_vmPopDouble( &iDec2 );
      double dNumber1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumType( dNumber1 + dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      ULONG ulLen1 = pItem1->item.asString.length;
      ULONG ulLen2 = pItem2->item.asString.length;

      if( ulLen1 < ULONG_MAX - ulLen2 )
      {
         char * szNewString = ( char * ) hb_xgrab( ulLen1 + ulLen2 + 1 );

         hb_xmemcpy( szNewString, pItem1->item.asString.value, ulLen1 );
         hb_xmemcpy( szNewString + ulLen1, pItem2->item.asString.value, ulLen2 );
         hb_itemPutCPtr( pItem1, szNewString, ulLen1 + ulLen2 );
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+", 2, pItem1, pItem2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();

      /* NOTE: This is not a bug. CA-Cl*pper does exactly that. */
      hb_vmPushDate( lDate1 + lDate2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      long lNumber2 = ( long ) hb_vmPopNumber();
      long lDate1 = hb_vmPopDate();

      hb_vmPushDate( lDate1 + lNumber2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpPlus" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPPLUS" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmMinus( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmMinus()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber1 = HB_ITEM_GET_NUMINTRAW( pItem1 );
      HB_LONG lNumber2 = HB_ITEM_GET_NUMINTRAW( pItem2 );
      HB_LONG lResult = lNumber1 - lNumber2;

      hb_stackDec();
      pItem2->type = HB_IT_NIL;

      if ( lNumber2 <= 0 ? lResult >= lNumber1 : lResult < lNumber1 )
      {
         HB_ITEM_PUT_NUMINTRAW( pItem1, lResult );
      }
      else
      {
         hb_stackDec();
         hb_vmPushDouble( ( double ) lNumber1 - ( double ) lNumber2, 0 );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1, iType2 = pItem2->type, iType1 = pItem1->type;
      double dNumber2 = hb_vmPopDouble( &iDec2 );
      double dNumber1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumType( dNumber1 - dNumber2, ( ( iDec1 > iDec2 ) ? iDec1 : iDec2 ), iType1, iType2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();

      hb_vmPushNumInt( lDate1 - lDate2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      long lNumber2 = ( long ) hb_vmPopNumber();
      long lDate1 = hb_vmPopDate();

      hb_vmPushDate( lDate1 - lNumber2 );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      ULONG ulLen1 = pItem1->item.asString.length;
      ULONG ulLen2 = pItem2->item.asString.length;

      if( ulLen1 < ULONG_MAX - ulLen2 )
      {
         char * szNewString = ( char * ) hb_xgrab( ulLen1 + ulLen2 + 1 );
         ULONG ulNewLen = ulLen1 + ulLen2;

         while( ulLen1 && pItem1->item.asString.value[ ulLen1 - 1 ] == ' ' )
            ulLen1--;

         hb_xmemcpy( szNewString, pItem1->item.asString.value, ulLen1 );
         hb_xmemcpy( szNewString + ulLen1, pItem2->item.asString.value, ulLen2 );
         hb_xmemset( szNewString + ulLen1 + ulLen2, ' ', pItem1->item.asString.length - ulLen1 );
         hb_itemPutCPtr( pItem1, szNewString, ulNewLen );
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-", 2, pItem1, pItem2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpMinus" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPMINUS" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmMult( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmMult()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   /* if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      HB_LONG lResult = lNumber1 * lNumber2;
      if ( lNumber2 == 0 || lResult / lNumber2 == lNumber1 )
         hb_vmPushNumInt( lResult );
      else
         hb_vmPushDouble( ( double ) lNumber1 * ( double ) lNumber2, 0 );
   }
   else */ if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1, iType2 = pItem2->type, iType1 = pItem1->type;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumType( d1 * d2, iDec1 + iDec2, iType1, iType2 );
   }

   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpMult" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPMULT" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmDivide( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDivide()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   /*
    * This code is commented out for Clipper compatibility.
    * See David's note below, Druzus.
    */   
   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if ( lDivisor == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         hb_stackPop(); /* pop divisor from the stack */
         /* commented out see note below
         {
            HB_LONG lDivided = hb_vmPopHBLong();
            if ( hb_set.HB_SET_DECIMALS == 0 && lDivided % lDivisor == 0 )
               hb_vmPushNumInt( lDivided / lDivisor );
            else
               hb_vmPushDouble( ( double ) lDivided / ( double ) lDivisor, hb_set.HB_SET_DECIMALS );
         }
         */
         hb_vmPushDouble( hb_vmPopNumber() / ( double ) lDivisor, hb_set.HB_SET_DECIMALS );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_vmTopNumber();

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         hb_stackPop(); /* pop divisor from the stack */

         /* If all both operand was integer and the result is an integer, too,
            push the number without decimals. Clipper compatible. Actually,
            this is not Clipper compatible. The only time Clipper returns 0
            decimal places is for compiler optimized integer division with an
            integer result. Therefore this code is not needed and has been
            removed - David G. Holm <dholm@jsd-llc.com>
         if( bIntegerOperands && fmod( hb_vmTopNumber() / dDivisor ) == 0.0 )
            hb_vmPushNumber( hb_vmPopNumber() / dDivisor, 0 );
         else
         */
         hb_vmPushDouble( hb_vmPopNumber() / dDivisor, hb_set.HB_SET_DECIMALS );
      }
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpDivide" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPDIVIDE" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmModulus( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmModulus()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lDivisor = HB_ITEM_GET_NUMINTRAW( pItem2 );

      if ( lDivisor == 0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         pItem2->type = HB_IT_NIL;
         hb_stackDec(); /* pop divisor from the stack */
         hb_stackDec();
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         if ( hb_set.HB_SET_DECIMALS == 0 )
            hb_vmPushNumInt( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor );
         else
            hb_vmPushDouble( ( double ) ( HB_ITEM_GET_NUMINTRAW( pItem1 ) % lDivisor ), hb_set.HB_SET_DECIMALS );
      }
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dDivisor = hb_vmTopNumber();

      if( dDivisor == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%", 2, pItem1, pItem2 );

         if( pResult )
         {
            hb_stackPop();
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         hb_stackPop(); /* pop divisor from the stack */

         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         hb_vmPushDouble( fmod( hb_vmPopNumber(), dDivisor ), hb_set.HB_SET_DECIMALS );
      }
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpMod" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPMOD" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmPower( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPower()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double d2 = hb_vmPopNumber();
      double d1 = hb_vmPopNumber();

      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_vmPushDouble( pow( d1, d2 ), hb_set.HB_SET_DECIMALS );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpPower" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPPOWER" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmInc( void )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInc()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_DATE( pItem ) )
      pItem->item.asDate.value++;
   else if( HB_IS_NUMINT( pItem ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value < HB_INT_MAX )
      {
         pItem->item.asInteger.value++;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value < HB_LONG_MAX )
      {
         pItem->item.asLong.value++;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
      else
      {
         int iType = pItem->type, iDec;
         double dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( ++dNumber, 0, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value++;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( HB_IS_OBJECT( pItem ) && hb_objHasMsg( pItem, "__OpInc" ) )
      hb_vmOperatorCallUnary( hb_stackItemFromTop( -1 ), "__OPINC" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmDec( void )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDec()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_DATE( pItem ) )
      pItem->item.asDate.value--;
   else if( HB_IS_NUMINT( pItem ) )
   {
      if( HB_IS_INTEGER( pItem ) && pItem->item.asInteger.value > HB_INT_MIN )
      {
         pItem->item.asInteger.value--;
         pItem->item.asInteger.length = HB_INT_LENGTH( pItem->item.asInteger.value );
      }
      else if( HB_IS_LONG( pItem ) && pItem->item.asLong.value > HB_LONG_MIN )
      {
         pItem->item.asLong.value--;
         pItem->item.asLong.length = HB_LONG_LENGTH( pItem->item.asLong.value );
      }
      else
      {
         int iType = pItem->type, iDec;
         double dNumber = hb_vmPopDouble( &iDec );
         hb_vmPushNumType( --dNumber, 0, iType, iType );
      }
   }
   else if( HB_IS_DOUBLE( pItem ) )
   {
      pItem->item.asDouble.value--;
      pItem->item.asDouble.length = HB_DBL_LENGTH( pItem->item.asDouble.value );
   }
   else if( HB_IS_OBJECT( pItem ) && hb_objHasMsg( pItem, "__OpDec" ) )
      hb_vmOperatorCallUnary( pItem, "__OPDEC" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmFuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the satck */
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFuncPtr()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_SYMBOL( pItem ) )
   {
      hb_stackPop();
      hb_vmPushPointer( ( void* ) pItem->item.asSymbol.value->value.pFunPtr );
   }
   else
      hb_errInternal( HB_EI_VMNOTSYMBOL, NULL, "hb_vmFuncPtr()", NULL );
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmEqual( BOOL bExact )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEqual(%d)", (int) bExact));

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
      hb_stackPop();    /* clear the pItem1 */
      hb_vmPushLogical( FALSE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, bExact );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i == 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
      hb_vmPushLogical( hb_vmPopHBLong() == hb_vmPopHBLong() );
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopNumber() == hb_vmPopNumber() );
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDate() == hb_vmPopDate() );
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() == hb_vmPopLogical() );
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpEqual" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPEQUAL" );
   else if( bExact && HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) )
   {
      BOOL bResult = ( pItem1->item.asArray.value == pItem2->item.asArray.value );

      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( pItem1->type != pItem2->type ||
            ( HB_IS_BLOCK( pItem1 ) && HB_IS_BLOCK( pItem2 ) ) ||
            ( ! bExact && HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult;

      if( bExact )
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==", 2, pItem1, pItem2 );
      else
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
   else
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( FALSE );
   }
}

static void hb_vmNotEqual( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNotEqual()"));

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
      hb_stackPop();    /* clear the pItem1 */
      hb_vmPushLogical( TRUE );
   }
   else if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i != 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
      hb_vmPushLogical( hb_vmPopHBLong() != hb_vmPopHBLong() );
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopNumber() != hb_vmPopNumber() );
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDate() != hb_vmPopDate() );
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() != hb_vmPopLogical() );
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpNotEqual" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPNOTEQUAL" );
   else if( pItem1->type != pItem2->type ||
            ( HB_IS_BLOCK( pItem1 ) && HB_IS_BLOCK( pItem2 ) ) ||
            ( HB_IS_ARRAY( pItem1 ) && HB_IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
   else
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( TRUE );
   }
}

static void hb_vmLess( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmLess()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i < 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      hb_vmPushLogical( lNumber1 < lNumber2 );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 < dNumber2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 < lDate2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 < bLogical2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpLess" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESS" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmLessEqual( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmLessEqual()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i <= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      hb_vmPushLogical( lNumber1 <= lNumber2 );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 <= dNumber2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 <= lDate2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 <= bLogical2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpLessEqual" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPLESSEQUAL" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreater( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreater()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i > 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      hb_vmPushLogical( lNumber1 > lNumber2 );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 > dNumber2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 > lDate2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 > bLogical2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpGreater" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATER" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmGreaterEqual( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreaterEqual()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i >= 0 );
   }
   else if( HB_IS_NUMINT( pItem1 ) && HB_IS_NUMINT( pItem2 ) )
   {
      HB_LONG lNumber2 = hb_vmPopHBLong();
      HB_LONG lNumber1 = hb_vmPopHBLong();
      hb_vmPushLogical( lNumber1 >= lNumber2 );
   }
   else if( HB_IS_NUMERIC( pItem1 ) && HB_IS_NUMERIC( pItem2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 >= dNumber2 );
   }
   else if( HB_IS_DATE( pItem1 ) && HB_IS_DATE( pItem2 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 >= lDate2 );
   }
   else if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 >= bLogical2 );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpGreaterEqual" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPGREATEREQUAL" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmInstring( void )
{
   PHB_ITEM pItem1;
   PHB_ITEM pItem2;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmInstring()"));

   pItem1 = hb_stackItemFromTop( -2 );
   pItem2 = hb_stackItemFromTop( -1 );

   if( HB_IS_STRING( pItem1 ) && HB_IS_STRING( pItem2 ) )
   {
      BOOL bResult = ( hb_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                                 pItem2->item.asString.value, pItem2->item.asString.length ) != 0 );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpInstring" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPINSTRING" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
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
   double dStep;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmForTest()"));

   while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
   {
      PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
      else
         /* NOTE: Return from the inside. */
         return;
   }

   dStep = hb_vmPopNumber();

   while( ( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) ) && ( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) ) )
   {
      PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
      else
         /* NOTE: Return from the inside. */
         return;
   }

   if ( hb_stackItemFromTop( -1 )->type == HB_IT_LOGICAL )
   {
      BOOL lEnd;
      BOOL lCurrent;

      lEnd = hb_vmPopLogical();
      while( ! HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

         if( pResult )
         {
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
         else
            /* NOTE: Return from the inside. */
            return;
      }
      lCurrent = hb_vmPopLogical();
      if( dStep >= 0 )           /* Positive loop. Use LESS */
      {
         hb_vmPushLogical( lCurrent <= lEnd );
      }
      else if( dStep < 0 )      /* Negative loop. Use GREATER */
      {
         hb_vmPushLogical( lCurrent >= lEnd );
      }
   }
   else
   {
      double dEnd;
      double dCurrent;

      dEnd = hb_vmPopNumber();
      while( ! HB_IS_NUMERIC( hb_stackItemFromTop( -1 ) ) )
      {
         PHB_ITEM pItem1 = hb_stackItemFromTop( -1 );
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<", 1, pItem1 );

         if( pResult )
         {
            hb_stackPop();
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
         else
            /* NOTE: Return from the inside. */
            return;
      }
      dCurrent = hb_vmPopNumber();
      if( dStep >= 0 )          /* Positive loop. Use LESS */
      {
         hb_vmPushLogical( dCurrent <= dEnd );
      }
      else if( dStep < 0 )      /* Negative loop. Use GREATER */
      {
         hb_vmPushLogical( dCurrent >= dEnd );
      }
   }
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot( void )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNot()"));

   pItem = hb_stackItemFromTop( -1 );

   if( HB_IS_LOGICAL( pItem ) )
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;
   else if( HB_IS_OBJECT( pItem ) && hb_objHasMsg( pItem, "__OpNot" ) )
      hb_vmOperatorCallUnary( pItem, "__OPNOT" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1077, NULL, ".NOT.", 1, pItem );

      if( pResult )
      {
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmAnd( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmAnd()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpAnd" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPAND" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1078, NULL, ".AND.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmOr( void )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOr()"));

   pItem2 = hb_stackItemFromTop( -1 );
   pItem1 = hb_stackItemFromTop( -2 );

   if( HB_IS_LOGICAL( pItem1 ) && HB_IS_LOGICAL( pItem2 ) )
   {
      pItem1->item.asLogical.value = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
      pItem2->type = HB_IT_NIL;
      hb_stackDec();
   }
   else if( HB_IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "__OpOr" ) )
      hb_vmOperatorCall( pItem1, pItem2, "__OPOR" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1079, NULL, ".OR.", 2, pItem1, pItem2 );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

/* ------------------------------- */
/* Array                           */
/* ------------------------------- */

static void hb_vmArrayPush( void )
{
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   ULONG ulIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPush()"));

   pIndex = hb_stackItemFromTop( -1 );
   pArray = hb_stackItemFromTop( -2 );

   if( HB_IS_INTEGER( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asDouble.value;
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }

      return;
   }

/* #ifndef HB_C52_STRICT */
   if( (hb_vmFlagEnabled(HB_VMFLAG_ARRSTR)) && (HB_IS_STRING( pArray )) )
   {
      BYTE b = 0;
      HB_ITEM item;

      if( ulIndex > 0 && ulIndex <= pArray->item.asString.length )
         b = pArray->item.asString.value[ ulIndex - 1 ];
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                        2, pArray, pIndex );

      hb_itemInit( &item );
      hb_itemPutNI( &item, b );
      hb_stackPop();
      hb_itemCopy( hb_stackItemFromTop( -1 ), &item );
      hb_itemClear( &item );
      return;
   }
/* #endif */

   if( HB_IS_OBJECT( pArray ) && hb_objHasMsg( pArray, "__OpArrayIndex" ) )
   {
      hb_vmOperatorCall( pArray, pIndex, "__OPARRAYINDEX" );
      return;
   }

   if( HB_IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      {
         if( pArray->item.asArray.value->ulHolders > 1 )
         {
            /* this is a temporary copy of an array - we can overwrite
             * it with no problem
            */
            hb_arrayGet( pArray, ulIndex, pArray );
            hb_stackPop();
         }
         else
         {
            /* this is a constant array { 1, 2, 3 } - we cannot use
             * the optimization here
            */
            HB_ITEM item;

            hb_itemInit( &item );
            hb_arrayGet( pArray, ulIndex, &item );
            hb_stackPop();

            hb_itemMove( hb_stackItemFromTop( -1 ), &item );
         }
      }
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
   }
   else
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ), 2, pArray, pIndex );
}

static void hb_vmArrayPop( void )
{
   PHB_ITEM pValue;
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   ULONG ulIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPop()"));

   pValue = hb_stackItemFromTop( -3 );
   pArray = hb_stackItemFromTop( -2 );
   pIndex = hb_stackItemFromTop( -1 );

   if( HB_IS_BYREF( pArray ) )
      pArray = hb_itemUnRef( pArray );

   if( HB_IS_INTEGER( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asInteger.value;
   else if( HB_IS_LONG( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asLong.value;
   else if( HB_IS_DOUBLE( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asDouble.value;
   else
   {
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
      return;
   }

/* #ifndef HB_C52_STRICT */
   if( (hb_vmFlagEnabled(HB_VMFLAG_ARRSTR)) && (HB_IS_STRING( pArray )) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asString.length )
      {
         if( pArray->item.asString.bStatic || *( pArray->item.asString.u.pulHolders ) > 1 )
            hb_itemPutC( pArray, pArray->item.asString.value );

         pArray->item.asString.value[ ulIndex - 1 ] = hb_itemGetNI( pValue );

         hb_stackPop();
         hb_stackPop();
         hb_stackPop();    /* remove the value from the stack just like other POP operations */
      }
      else
         hb_errRT_BASE( EG_BOUND, 1132, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ),
                        2, pArray, pIndex );

      return;
   }
/* #endif */

   if( HB_IS_ARRAY( pArray ) )
   {
      if( ulIndex > 0 && ulIndex <= pArray->item.asArray.value->ulLen )
      {
         pValue->type &= ~HB_IT_MEMOFLAG;
         hb_arraySet( pArray, ulIndex, pValue );
         /* This is no longer needed as we manage the array by reference */
         /* hb_itemCopy( pArray, pValue );  places pValue at pArray position */
         hb_stackPop();
         hb_stackPop();
         hb_stackPop();    /* remove the value from the stack just like other POP operations */
      }
      else
         hb_errRT_BASE( EG_BOUND, 1133, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
   }
   else
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ), 1, pIndex );
}

static void hb_vmArrayDim( USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   HB_ITEM itArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayDim(%hu)", uiDimensions));

   itArray.type = HB_IT_NIL;

   hb_vmArrayNew( &itArray, uiDimensions );

   while( uiDimensions-- )
      hb_stackPop();

   hb_itemMove( ( hb_stackTopItem() ), &itArray );
   hb_stackPush();
}

static void hb_vmArrayGen( ULONG ulElements ) /* generates an ulElements Array and fills it from the stack values */
{
   HB_ITEM itArray;
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayGen(%lu)", ulElements));

   itArray.type = HB_IT_NIL;
   hb_arrayNew( &itArray, ulElements );
   for( ulPos = 0; ulPos < ulElements; ulPos++ )
      hb_itemCopy( itArray.item.asArray.value->pItems + ulPos, hb_stackItemFromTop( ulPos - ulElements ) );

   for( ulPos = 0; ulPos < ulElements; ulPos++ )
      hb_stackPop();

   hb_itemMove( ( hb_stackTopItem() ), &itArray );
   hb_stackPush();
}

/* This function creates an array item using 'uiDimension' as an index
 * to retrieve the number of elements from the stack
 */
static void hb_vmArrayNew( HB_ITEM_PTR pArray, USHORT uiDimension )
{
   ULONG ulElements;
   HB_ITEM_PTR pDim;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayNew(%p, %hu)", pArray, uiDimension));

   pDim = hb_stackItemFromTop( - uiDimension );

   /* use the proper type of number of elements */
   switch( pDim->type & ~HB_IT_BYREF )
   {
      case HB_IT_INTEGER:
         ulElements = ( ULONG ) pDim->item.asInteger.value;
         break;

      case HB_IT_LONG:
         ulElements = pDim->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         ulElements = ( ULONG ) pDim->item.asDouble.value;
         break;

      default:
         /* NOTE: Clipper creates empty array if non-numeric value is
          * specified as dimension and stops further processing.
          * There is no runtime error generated.
          */
         ulElements = 0;
         break;
   }

   /* create an array */
   hb_arrayNew( pArray, ulElements );

   if( --uiDimension )
   {
      /* call self recursively to create next dimensions
       */
      while( ulElements )
         hb_vmArrayNew( hb_arrayGetItemPtr( pArray, ulElements-- ), uiDimension );
   }
}

/* ------------------------------- */
/* Object                          */
/* ------------------------------- */

static void hb_vmOperatorCall( PHB_ITEM pObjItem, PHB_ITEM pMsgItem, char * szSymbol )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if HB_IS_OBJECT() && HAS_METHOD() )
    */
   HB_ITEM ItemMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCall(%p, %p, %s)", pObjItem, pMsgItem, szSymbol));

   ItemMsg.type = HB_IT_SYMBOL;
   ItemMsg.item.asSymbol.value = hb_dynsymFind( szSymbol )->pSymbol;
   ItemMsg.item.asSymbol.stackbase = hb_stackTopOffset();

   hb_itemClear( &hb_stack.Return );       /* clear return value */
   hb_vmPush( &ItemMsg );
   hb_vmPush( pObjItem );                             /* Push object              */
   hb_vmPush( pMsgItem );                             /* Push argument            */

   hb_vmDo( 1 );

   /* pop passed arguments - only one here */
   hb_stackPop();               /* pMsgItem */

   /* Push return value on the stack
    * NOTE: for performance reason we don't pop the second argument.
    * We can replace the second argument with the return value.
    */
   hb_itemCopy( pObjItem, &hb_stack.Return );
}

static void hb_vmOperatorCallUnary( PHB_ITEM pObjItem, char * szSymbol )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if HB_IS_OBJECT() && HAS_METHOD() )
    */
   HB_ITEM ItemMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCallUnary(%p, %s)", pObjItem, szSymbol));

   ItemMsg.type = HB_IT_SYMBOL;
   ItemMsg.item.asSymbol.value = hb_dynsymFind( szSymbol )->pSymbol;
   ItemMsg.item.asSymbol.stackbase = hb_stackTopOffset();

   hb_itemClear( &hb_stack.Return );       /* clear return value */
   hb_vmPush( &ItemMsg );
   hb_vmPush( pObjItem );                             /* Push object */

   hb_vmDo( 0 );

   /* Pop passed argument.
    * NOTE: for performance reason we don't pop it and we don't push the
    * return value. We can replace the last element with the new value.
    */
   hb_itemCopy( pObjItem, &hb_stack.Return );
}

/* ------------------------------- */
/* Database                        */
/* ------------------------------- */

static ERRCODE hb_vmSelectWorkarea( PHB_ITEM pAlias )
{
   ERRCODE bSuccess = SUCCESS;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSelectWorkArea(%p)", pAlias));

   /* NOTE: Clipper doesn't generate an error if an workarea specified
    * as numeric value cannot be selected
    */
   switch( pAlias->type & ~HB_IT_BYREF )
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
         bSuccess = hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
         pAlias->type = HB_IT_NIL;
         break;

      case HB_IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         {
         /* expand '&' operator if exists */
            char *cAlias;
            BOOL bNewString;

            cAlias = hb_macroExpandString( pAlias->item.asString.value, pAlias->item.asString.length, &bNewString );
            bSuccess = hb_rddSelectWorkAreaAlias( cAlias );
            if( bNewString )
               hb_xfree( cAlias );
            hb_itemClear( pAlias );
         }
         break;

      /*
       These types were added for Clipper compatibility
      */
      case HB_IT_ARRAY:
      case HB_IT_BLOCK:
         hb_itemClear( pAlias );
         /* Fall through. */
      case HB_IT_NIL:
      case HB_IT_LOGICAL:
      case HB_IT_DATE:
         hb_rddSelectWorkAreaNumber( -1 );
         pAlias->type = HB_IT_NIL;
         break;

      default:
         {
            PHB_ITEM pSubstVal = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&", 1, pAlias );
            if( pSubstVal )
               bSuccess = hb_vmSelectWorkarea( pSubstVal );
            else
               bSuccess = FAILURE;
            hb_itemClear( pAlias );
         }
         break;
   }
   return bSuccess;
}

/* Swaps two last items on the eval stack - the last item after swaping
 * is popped as current workarea number
 */
static void hb_vmSwapAlias( void )
{
   HB_ITEM_PTR pItem;
   HB_ITEM_PTR pWorkArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSwapAlias()"));

   pItem = hb_stackItemFromTop( -1 );
   pWorkArea = hb_stackItemFromTop( -2 );
   hb_vmSelectWorkarea( pWorkArea );

   memcpy( pWorkArea, pItem, sizeof( HB_ITEM ) );
   pItem->type = HB_IT_NIL;
   hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void HB_EXPORT hb_vmDo( USHORT uiParams )
{
   PHB_ITEM pItem;
   PHB_SYMB pSym;
   HB_STACK_STATE sStackState;
   PHB_ITEM pSelf;
   PHB_FUNC pFunc;
   BOOL bDebugPrevState;
   ULONG ulClock = 0;
   void * pMethod = NULL;
   BOOL bProfiler = hb_bProfiler; /* because profiler state may change */

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDo(%hu)", uiParams));

   /*
   printf( "\VmDo nItems: %i Params: %i Extra %i\n", hb_stack.pPos - hb_stack.pBase, uiParams, hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] );
   */
   s_ulProcLevel++;

   if( hb_vm_iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] + 2 ) ) ) && pItem->item.asSymbol.value == hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] )
   {
      uiParams += hb_vm_aiExtraParams[--hb_vm_iExtraParamsIndex];
   }

   if( bProfiler )
      ulClock = ( ULONG ) clock();

   /* Poll the console keyboard
   #ifndef HB_GUI
      hb_inkeyPoll();
   #endif
   */

   pItem = hb_stackNewFrame( &sStackState, uiParams );
   pSym = pItem->item.asSymbol.value;
   pSelf = hb_stackSelfItem();   /* NIL, OBJECT or BLOCK */
   bDebugPrevState = s_bDebugging;
   s_bDebugging = FALSE;

   if( ! HB_IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      BOOL lPopSuper = FALSE;
      PHB_BASEARRAY pSelfBase = NULL;

      if( pSym == &( hb_symEval ) && HB_IS_BLOCK( pSelf ) )
         pFunc = pSym->value.pFunPtr;                 /* __EVAL method = function */
      else
      {
         pFunc = hb_objGetMethod( pSelf, pSym );
         if( HB_IS_OBJECT( pSelf ) )               /* Object passed            */
         {
            pSelfBase = pSelf->item.asArray.value;
            if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
            {
              PHB_ITEM pRealSelf;
              USHORT nPos;
              USHORT uiClass;

              /*
              printf( "\n VmDo Method: %s \n", pSym->szName );
              */
              uiClass=pSelfBase->uiClass;

              pRealSelf = hb_itemNew( NULL ) ;
              hb_itemCopy(pRealSelf ,pSelf->item.asArray.value->pItems) ;  /* hb_arrayGetItemPtr(pSelf,1) ; */
              /* and take back the good pSelfBase */
              pSelfBase = pRealSelf->item.asArray.value;
              /* Now I should exchnage it with the current stacked value */
              hb_itemSwap( pSelf, pRealSelf );
              hb_itemRelease(pRealSelf) ; /* and release the fake one */

              /* Push current SuperClass handle */
              lPopSuper = TRUE ;

              if ( ! pSelf->item.asArray.value->puiClsTree)
              {
               pSelf->item.asArray.value->puiClsTree   = ( USHORT * ) hb_xgrab( sizeof( USHORT ) );
               pSelf->item.asArray.value->puiClsTree[0]=0;
              }

              nPos=pSelfBase->puiClsTree[0]+1;
              pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * ( nPos + 1 ) );

              pSelfBase->puiClsTree[0] = nPos ;
              pSelfBase->puiClsTree[ nPos ] = uiClass;

            }
         }
      }

      if( pFunc )
      {
         if( bProfiler )
            pMethod = hb_mthRequested();

         if ( hb_bTracePrgCalls )
            HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));

         pFunc();

         if (lPopSuper && pSelfBase->puiClsTree)
         {

           USHORT nPos=pSelfBase->puiClsTree[0]-1;
           /* POP SuperClass handle */

           if (nPos)
            {
             pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * (nPos + 1) );
             pSelfBase->puiClsTree[0]=nPos;
            }
           else
            {
             hb_xfree(pSelfBase->puiClsTree);
             pSelfBase->puiClsTree = NULL ;
            }

         }

         if( bProfiler )
            hb_mthAddTime( pMethod, clock() - ulClock );
      }
      else if( pSym->szName[ 0 ] == '_' )
      {
         PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );
         hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1, 1, pArgsArray );
         hb_itemRelease( pArgsArray );
      }
      else
      {
         PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );
         hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, NULL, pSym->szName, 1, pArgsArray );
         hb_itemRelease( pArgsArray );
      }
   }
   else /* it is a function */
   {
      pFunc = pSym->value.pFunPtr;

      if( pFunc )
      {
            if( bProfiler && pSym->pDynSym ) {
               pSym->pDynSym->ulRecurse++;
            }

            if ( hb_bTracePrgCalls )
               HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));

            pFunc();

            if( bProfiler && pSym->pDynSym )
            {
               pSym->pDynSym->ulCalls++;                   /* profiler support */

               /* Time spent has to be added only inside topmost call of a recursive function */
               if( pSym->pDynSym->ulRecurse == 1 ) {
                  pSym->pDynSym->ulTime += clock() - ulClock; /* profiler support */
               }
            }

            if( bProfiler && pSym->pDynSym ) {
               pSym->pDynSym->ulRecurse--;
            }
      }
      else
      {
         /* Attempt to call an undefined function
          *  - generate unrecoverable runtime error
          */
         PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );
         hb_errRT_BASE_SubstR( EG_NOFUNC, 1001, NULL, pSym->szName, 1, pArgsArray );
         hb_itemRelease( pArgsArray );
      }
   }

   if( s_bDebugging )
      hb_vmDebuggerEndProc();
   hb_stackOldFrame( &sStackState );

   s_bDebugging = bDebugPrevState;
   s_ulProcLevel--;
}

void HB_EXPORT hb_vmSend( USHORT uiParams )
{
   PHB_ITEM pItem;
   PHB_SYMB pSym;
   HB_STACK_STATE sStackState;
   PHB_ITEM pSelf;
   PHB_FUNC pFunc = NULL;
   BOOL bDebugPrevState;
   ULONG ulClock = 0;
   void *pMethod = NULL;
   BOOL bProfiler = hb_bProfiler; /* because profiler state may change */

   HB_TRACE(HB_TR_DEBUG, ("hb_vmSend(%hu)", uiParams));

   /*
   printf( "\n VmSend nItems: %i Params: %i Extra %i\n", hb_stack.pPos - hb_stack.pBase, uiParams, hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] );
   */

   s_ulProcLevel++;
   if( hb_vm_iExtraParamsIndex && HB_IS_SYMBOL( pItem = hb_stackItemFromTop( -( uiParams + hb_vm_aiExtraParams[hb_vm_iExtraParamsIndex - 1] + 2 ) ) ) && pItem->item.asSymbol.value == hb_vm_apExtraParamsSymbol[hb_vm_iExtraParamsIndex - 1] )
   {
      uiParams += hb_vm_aiExtraParams[--hb_vm_iExtraParamsIndex];
   }

   if( bProfiler )
   {
      ulClock = ( ULONG ) clock();
   }

   pItem = hb_stackNewFrame( &sStackState, uiParams );   /* procedure name */
   pSym = pItem->item.asSymbol.value;
   pSelf = hb_stackSelfItem();   /* NIL, OBJECT or BLOCK */
   bDebugPrevState = s_bDebugging;
   s_bDebugging = FALSE;

   /* printf( "Symbol: '%s'\n", pSym->szName ); */

   if( HB_IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      pFunc = pSym->value.pFunPtr;

      if( pFunc )
      {
         if( bProfiler && pSym->pDynSym ) {
            pSym->pDynSym->ulRecurse++;
         }

         if ( hb_bTracePrgCalls )
            HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));

         pFunc();

         if( bProfiler && pSym->pDynSym )
         {
            pSym->pDynSym->ulCalls++;                   /* profiler support */

            /* Time spent has to be added only inside topmost call of a recursive function */
            if ( pSym->pDynSym->ulRecurse == 1 ) {
               pSym->pDynSym->ulTime += clock() - ulClock; /* profiler support */
            }
         }

         if( bProfiler && pSym->pDynSym ) {
            pSym->pDynSym->ulRecurse--;
         }
      }
      else
      {
         /* Attempt to call an undefined function
          *  - generate unrecoverable runtime error
          */
         if( strncmp( pSym->szName, "CLASSNAME", strlen( pSym->szName ) ) == 0 )
         {
            hb_itemPutC( &hb_stack.Return, "NIL" );
         }
         else if( pSym->szName[ 0 ] == '_' )
         {
            PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );
            hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, "Class: NIL has no exported property", pSym->szName + 1, 1, pArgsArray );
            hb_itemRelease( pArgsArray );
         }
         else
         {
            PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );
            hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, "Class: NIL has no exported method", pSym->szName, 1, pArgsArray );
            hb_itemRelease( pArgsArray );
         }
      }
   }
   else
   {
      PHB_BASEARRAY pSelfBase = NULL;
      BOOL lPopSuper = FALSE;

      if( HB_IS_BLOCK( pSelf ) )
      {
         if( pSym == &( hb_symEval ) )
         {
            pFunc = pSym->value.pFunPtr;                 /* __EVAL method = function   */
         }
         else if( strncmp( pSym->szName, "EVAL", 4 ) == 0 )
         {
            pSym = &hb_symEval;
            pFunc = pSym->value.pFunPtr;                 /* __EVAL method = function */
         }
      }
      else if( HB_IS_OBJECT( pSelf ) )               /* Object passed            */
      {
         pFunc     = hb_objGetMethod( pSelf, pSym );
         pSelfBase = pSelf->item.asArray.value;

         if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
         {
            PHB_ITEM pRealSelf;
            USHORT nPos;
            USHORT uiClass;

            /*
            printf( "\n VmSend Method: %s \n", pSym->szName );
            */
            uiClass = pSelfBase->uiClass;

            pRealSelf = hb_itemNew( NULL ) ;
            hb_itemCopy( pRealSelf, pSelf->item.asArray.value->pItems ) ;  /* hb_arrayGetItemPtr(pSelf,1) ; */
            /* and take back the good pSelfBase */
            pSelfBase = pRealSelf->item.asArray.value;
            /* Now I should exchnage it with the current stacked value */
            hb_itemSwap( pSelf, pRealSelf );
            hb_itemRelease( pRealSelf ) ; /* and release the fake one */

            /* Push current SuperClass handle */
            lPopSuper = TRUE;

            if ( ! pSelf->item.asArray.value->puiClsTree )
            {
               pSelf->item.asArray.value->puiClsTree   = ( USHORT * ) hb_xgrab( sizeof( USHORT ) );
               pSelf->item.asArray.value->puiClsTree[0]=0;
            }

            nPos=pSelfBase->puiClsTree[0]+1;
            pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * (nPos+1) ) ;
            pSelfBase->puiClsTree[0] = nPos ;
            pSelfBase->puiClsTree[ nPos ] = uiClass;
         }
      }

      if( pFunc )
      {
         if( bProfiler )
         {
            pMethod = hb_mthRequested();
         }

         if ( hb_bTracePrgCalls )
         {
            HB_TRACE(HB_TR_ALWAYS, ("Calling: %s", pSym->szName));
         }

         pFunc();

         if ( pSym != &hb_symEval && lPopSuper && pSelfBase->puiClsTree )
         {
            USHORT nPos=pSelfBase->puiClsTree[0] - 1;

            /* POP SuperClass handle */
            if (nPos)
            {
               pSelfBase->puiClsTree = ( USHORT * ) hb_xrealloc( pSelfBase->puiClsTree, sizeof( USHORT ) * (nPos + 1) );
               pSelfBase->puiClsTree[0]=nPos;
            }
            else
            {
               hb_xfree(pSelfBase->puiClsTree);
               pSelfBase->puiClsTree = NULL ;
            }
         }

         if( bProfiler )
         {
            hb_mthAddTime( pMethod, clock() - ulClock );
         }
      }
      else
      {
         char *sClass = hb_objGetClsName( pSelf );

         if( strncmp( pSym->szName, "CLASSNAME", strlen( pSym->szName ) < 4 ? 4 : strlen( pSym->szName ) ) == 0 )
         {
            hb_itemPutC( &hb_stack.Return, sClass );
         }
         else if( strncmp( pSym->szName, "CLASSH", 6 ) == 0 )
         {
            hb_itemPutNI( &hb_stack.Return, 0 );
         }
         else
         {
            char sDesc[128];

            if( pSym->szName[ 0 ] == '_' )
            {
               PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );

               sprintf( (char *) sDesc, "Class: '%s' has no property", sClass );
               hb_errRT_BASE_SubstR( EG_NOVARMETHOD, 1005, (char *) sDesc, pSym->szName + 1, 1, pArgsArray );
               hb_itemRelease( pArgsArray );
            }
            else
            {
               PHB_ITEM pArgsArray = hb_arrayFromStack( uiParams );

               sprintf( (char *) sDesc, "Class: '%s' has no exported method", sClass );
               hb_errRT_BASE_SubstR( EG_NOMETHOD, 1004, (char *) sDesc, pSym->szName, 1, pArgsArray );
               hb_itemRelease( pArgsArray );
            }
         }
      }
   }

   if( s_bDebugging )
      hb_vmDebuggerEndProc();
   hb_stackOldFrame( &sStackState );


   s_bDebugging = bDebugPrevState;
   s_ulProcLevel--;
}

static HARBOUR hb_vmDoBlock( void )
{
   PHB_ITEM pBlock;
   USHORT uiLine;
   int iParam;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoBlock()"));

   pBlock = hb_stackSelfItem();

   if( ! HB_IS_BLOCK( pBlock ) )
      hb_errInternal( HB_EI_VMNOTCBLOCK, NULL, "hb_vmDoBlock()", NULL );

   /* Check for valid count of parameters */
   iParam = pBlock->item.asBlock.paramcnt - hb_pcount();

   /* add missing parameters */
   while( iParam-- > 0 )
      hb_vmPushNil();

   /* set the current line number to a line where the codeblock was defined
    */
   uiLine = ( hb_stackBaseItem() )->item.asSymbol.lineno;
   ( hb_stackBaseItem() )->item.asSymbol.lineno = pBlock->item.asBlock.lineno;

   hb_codeblockEvaluate( pBlock );

   /* restore stack pointers */
   ( hb_stackBaseItem() )->item.asSymbol.lineno = uiLine;
}

/* Evaluates a passed codeblock item with no arguments passed to a codeblock
*/
HB_ITEM_PTR hb_vmEvalBlock( HB_ITEM_PTR pBlock )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlock(%p)", pBlock));

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );
   hb_vmDo( 0 );
   return &hb_stack.Return;
}

/* Evaluates a codeblock item using passed additional arguments
 * pBlock = an item of codeblock type to evaluate
 * ulArgCount = number of arguments passed to a codeblock
 * ... = the list of arguments of type PHB_ITEM
 *
 *for example:
 * retVal = hb_vmEvalBlockV( pBlock, 2, pParam1, pParam2 );
*/
HB_ITEM_PTR hb_vmEvalBlockV( HB_ITEM_PTR pBlock, ULONG ulArgCount, ... )
{
   va_list va;
   ULONG i;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEvalBlockV(%p, %hu, ...)", pBlock, ulArgCount));

   hb_vmPushSymbol( &hb_symEval );
   hb_vmPush( pBlock );

   va_start( va, ulArgCount );
   for( i = 1; i <= ulArgCount; i++ )
      hb_vmPush( va_arg( va, PHB_ITEM ) );
   va_end( va );

   /* take care here, possible loss of data long to short ... */
   /* added an explicit casting here for VC++ JFL */
   hb_vmDo( (USHORT) ulArgCount );

   return &hb_stack.Return;
}

void hb_vmFunction( USHORT uiParams )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFunction(%hu)", uiParams));

   hb_itemClear( &hb_stack.Return );
   hb_vmDo( uiParams );
   hb_itemCopy( hb_stackTopItem(), &hb_stack.Return );
   hb_stackPush();
}

static void hb_vmLocalName( USHORT uiLocal, char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLocalName(%hu, %s)", uiLocal, szLocalName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushLongConst( HB_DBG_LOCALNAME );
   hb_vmPushLongConst( uiLocal );
   hb_vmPushString( szLocalName, strlen( szLocalName ) );
   s_bDebuggerIsWorking = TRUE;
   hb_vmDo( 3 );
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmStaticName( BYTE bIsGlobal, USHORT uiStatic, char * szStaticName ) /* statics vars information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStaticName(%hu, %s)", uiStatic, szStaticName));

   HB_SYMBOL_UNUSED( bIsGlobal );

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushLongConst( HB_DBG_STATICNAME );
   hb_vmPushLongConst( hb_stack.iStatics );  /* current static frame */
   hb_vmPushLongConst( uiStatic );  /* variable index */
   hb_vmPushString( szStaticName, strlen( szStaticName ) );
   s_bDebuggerIsWorking = TRUE;
   hb_vmDo( 4 );
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmModuleName( char * szModuleName ) /* PRG and function name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModuleName(%s)", szModuleName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushLongConst( HB_DBG_MODULENAME );
   hb_vmPushString( szModuleName, strlen( szModuleName ) );
   s_bDebuggerIsWorking = TRUE;
   hb_vmDo( 2 );
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

static void hb_vmFrame( BYTE bLocals, BYTE bParams )
{
   int iTotal, iExtra;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFrame(%d, %d)", (int) bLocals, (int) bParams));

   iExtra = hb_pcount() - bParams;

   while( iExtra > 0 )
   {
      hb_itemClear( hb_stackItemFromTop( -iExtra ) );
      iExtra--;
   }

   iTotal = bLocals + bParams;
   if( iTotal )
   {
      int i = iTotal - hb_pcount();
      while( i-- > 0 )
         hb_vmPushNil();
   }
}

static void hb_vmSFrame( PHB_SYMB pSym )      /* sets the statics frame for a function */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSFrame(%p)", pSym));

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   hb_stack.iStatics = pSym->value.iStaticsBase; /* pSym is { "$_INITSTATICS", HB_FS_INIT | HB_FS_EXIT, _INITSTATICS } for each PRG */
}

static void hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ) /* initializes the global aStatics array or redimensionates it */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStatics(%p, %hu)", pSym, uiStatics));

   if( HB_IS_NIL( &s_aStatics ) )
   {
      pSym->value.iStaticsBase = 0;         /* statics frame for this PRG */
      hb_arrayNew( &s_aStatics, uiStatics );
   }
   else
   {
      pSym->value.iStaticsBase = hb_arrayLen( &s_aStatics );
      hb_arraySize( &s_aStatics, hb_arrayLen( &s_aStatics ) + uiStatics );
   }

   s_uiStatics = uiStatics; /* We need s_uiStatics for processing hb_vmStaticName() */
}

static void hb_vmEndBlock( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEndBlock()"));

   hb_stackDec();                               /* make the last item visible */
   hb_itemMove( &hb_stack.Return, hb_stackTopItem() ); /* copy it */
}

static void hb_vmRetValue( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRetValue()"));

   hb_stackDec();                               /* make the last item visible */
   hb_itemMove( &hb_stack.Return, hb_stackTopItem() ); /* copy it */
}

static void hb_vmDebuggerEndProc( void )
{
   HB_ITEM item;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerEndProc()"));

   hb_itemInit( &item );
   hb_itemCopy( &item, &hb_stack.Return ); /* saves the previous returned value */

   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushLongConst( HB_DBG_ENDPROC );
   s_bDebuggerIsWorking = TRUE;
   hb_vmDo( 1 );
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;

   hb_itemMove( &hb_stack.Return, &item ); /* restores the previous returned value */
}

static void hb_vmDebuggerShowLine( USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerShowLine(%hu)", uiLine));

   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushLongConst( HB_DBG_SHOWLINE );
   hb_vmPushInteger( uiLine );
   s_bDebuggerIsWorking = TRUE;
   hb_vmDo( 2 );
   s_bDebuggerIsWorking = FALSE;
   s_bDebugShowLines = TRUE;
}

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void HB_EXPORT hb_vmPush( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPush(%p)", pItem));

   hb_itemCopy( hb_stackTopItem(), pItem );
   hb_stackPush();
}

void HB_EXPORT hb_vmPushNil( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNil()"));

   ( hb_stackTopItem() )->type = HB_IT_NIL;
   hb_stackPush();
}

void HB_EXPORT hb_vmPushLogical( BOOL bValue )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLogical(%d)", (int) bValue));

   pStackTopItem->type = HB_IT_LOGICAL;
   pStackTopItem->item.asLogical.value = bValue;
   hb_stackPush();
}

void HB_EXPORT hb_vmPushNumber( double dNumber, int iDec )
{
   hb_vmPushNumType( dNumber, iDec, 0, 0 );
}

void HB_EXPORT hb_vmPushNumType( double dNumber, int iDec, int iType1, int iType2 )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNumber(%lf, %d)", dNumber, iDec));

   if( iDec || iType1 & HB_IT_DOUBLE || iType2 & HB_IT_DOUBLE )
      hb_vmPushDouble( dNumber, iDec );

   else if ( HB_DBL_LIM_INT( dNumber ) )
      hb_vmPushInteger( ( int ) dNumber );

   else if ( HB_DBL_LIM_LONG( dNumber ) )
      hb_vmPushHBLong( ( HB_LONG ) dNumber );

   else
      hb_vmPushDouble( dNumber, hb_set.HB_SET_DECIMALS );
}

static int hb_vmCalcIntWidth( HB_LONG lNumber )
{
   int iWidth;

   if( lNumber <= -1000000000L )
   {
      iWidth = 20;
   }
   else
   {
      iWidth = 10;
      while ( lNumber >= 1000000000L )
      {
         iWidth++;
         lNumber /= 10;
      }
   }
   return iWidth;
}

static void hb_vmPushNumInt( HB_LONG lNumber )
{
   if( HB_LIM_INT( lNumber ) )
      hb_vmPushInteger( ( int ) lNumber );
   else
      hb_vmPushHBLong( lNumber );
}

void HB_EXPORT hb_vmPushInteger( int iNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushInteger(%d)", iNumber));

   pStackTopItem->type = HB_IT_INTEGER;
   pStackTopItem->item.asInteger.value = iNumber;
   pStackTopItem->item.asInteger.length = HB_INT_LENGTH( iNumber );
   hb_stackPush();
}

#if HB_INT_MAX >= INT32_MAX
static void hb_vmPushIntegerConst( int iNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushIntegerConst(%d)", iNumber));

   pStackTopItem->type = HB_IT_INTEGER;
   pStackTopItem->item.asInteger.value = iNumber;
   pStackTopItem->item.asInteger.length = hb_vmCalcIntWidth( iNumber );
   hb_stackPush();
}
#endif

void HB_EXPORT hb_vmPushLong( long lNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLong(%ld)", lNumber));

#if HB_INT_MAX >= LONG_MAX
   pStackTopItem->type = HB_IT_INTEGER;
   pStackTopItem->item.asInteger.value = ( int ) lNumber;
   pStackTopItem->item.asInteger.length = HB_INT_LENGTH( lNumber );
#else
   pStackTopItem->type = HB_IT_LONG;
   pStackTopItem->item.asLong.value = ( HB_LONG ) lNumber;
   pStackTopItem->item.asLong.length = HB_LONG_LENGTH( lNumber );
#endif
   hb_stackPush();
}

void hb_vmPushLongConst( long lNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongConst(%ld)", lNumber));

#if HB_INT_MAX >= LONG_MAX
   pStackTopItem->type = HB_IT_INTEGER;
   pStackTopItem->item.asInteger.value = ( int ) lNumber;
   pStackTopItem->item.asInteger.length = hb_vmCalcIntWidth( lNumber );
#else
   pStackTopItem->type = HB_IT_LONG;
   pStackTopItem->item.asLong.value = ( HB_LONG ) lNumber;
   pStackTopItem->item.asLong.length = hb_vmCalcIntWidth( lNumber );
#endif
   hb_stackPush();
}

static void hb_vmPushHBLong( HB_LONG lNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushHBLong(%" PFHL "d)", lNumber));

   pStackTopItem->type = HB_IT_LONG;
   pStackTopItem->item.asLong.value = lNumber;
   pStackTopItem->item.asLong.length = HB_LONG_LENGTH( lNumber );

   hb_stackPush();
}

#if !defined( HB_LONG_LONG_OFF )
static void hb_vmPushLongLongConst( LONGLONG llNumber )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLongLongConst(%" PFLL "d)", llNumber));

   pStackTopItem->type = HB_IT_LONG;
   pStackTopItem->item.asLong.value = ( HB_LONG ) llNumber;
   pStackTopItem->item.asLong.length = hb_vmCalcIntWidth( llNumber );

   hb_stackPush();
}
#endif

void HB_EXPORT hb_vmPushDouble( double dNumber, int iDec )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDouble(%lf, %d)", dNumber, iDec));

   pStackTopItem->type = HB_IT_DOUBLE;
   pStackTopItem->item.asDouble.value = dNumber;
   pStackTopItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   if( iDec == HB_DEFAULT_DECIMALS )
      pStackTopItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   else
      pStackTopItem->item.asDouble.decimal = iDec;

   hb_stackPush();
}

static void hb_vmPushDoubleConst( double dNumber, int iWidth, int iDec )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDoubleConst(%lf, %d, %d)", dNumber, iWidth, iDec));

   pStackTopItem->type = HB_IT_DOUBLE;
   pStackTopItem->item.asDouble.value = dNumber;

   if( iDec == HB_DEFAULT_DECIMALS )
      pStackTopItem->item.asDouble.decimal = hb_set.HB_SET_DECIMALS;
   else
      pStackTopItem->item.asDouble.decimal = iDec;

   if( iWidth == HB_DEFAULT_WIDTH )
      pStackTopItem->item.asDouble.length = HB_DBL_LENGTH( dNumber );
   else
      pStackTopItem->item.asDouble.length = iWidth;

   hb_stackPush();
}

void HB_EXPORT hb_vmPushDate( long lDate )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDate(%ld)", lDate));

   pStackTopItem->type = HB_IT_DATE;
   pStackTopItem->item.asDate.value = lDate;
   hb_stackPush();
}

void HB_EXPORT hb_vmPushPointer( void * pPointer )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushPointer(%ld)", pPointer));

   pStackTopItem->type = HB_IT_POINTER;
   pStackTopItem->item.asPointer.value = pPointer;
   hb_stackPush();
}

void HB_EXPORT hb_vmPushString( char * szText, ULONG length )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushString(%s, %lu)", szText, length));

   pStackTopItem->type = HB_IT_STRING;
   pStackTopItem->item.asString.length = length;
   if( length == 0 )
   {
      pStackTopItem->item.asString.length  = 0;
      pStackTopItem->item.asString.value   = hb_vm_sNull;
      pStackTopItem->item.asString.bStatic = -1;
   }
   else if( length == 1 )
   {
      pStackTopItem->item.asString.length  = 1;
      pStackTopItem->item.asString.value   = hb_vm_acAscii[ (unsigned char) ( szText[0] ) ];
      pStackTopItem->item.asString.bStatic = -1;
   }
   else
   {
      char * szTemp = ( char * ) hb_xgrab( length + 1 );
      hb_xmemcpy( szTemp, szText, length );
      szTemp[ length ] = '\0';

      pStackTopItem->item.asString.value  = szTemp;
      pStackTopItem->item.asString.bStatic = 0;
      pStackTopItem->item.asString.u.pulHolders = ( HB_COUNTER * ) hb_xgrab( sizeof( HB_COUNTER ) );
      *( pStackTopItem->item.asString.u.pulHolders ) = 1;
   }
   hb_stackPush();
}

void hb_vmPushStringPcode( char * szText, ULONG length )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStringPcode(%s, %lu)", szText, length));

   pStackTopItem->type = HB_IT_STRING;
   pStackTopItem->item.asString.length = length;
   pStackTopItem->item.asString.value  = szText;
   pStackTopItem->item.asString.bStatic = 1;
   hb_stackPush();
}

void HB_EXPORT hb_vmPushSymbol( PHB_SYMB pSym )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushSymbol(%p)", pSym));

   pStackTopItem->type = HB_IT_SYMBOL;
   pStackTopItem->item.asSymbol.value = pSym;
   pStackTopItem->item.asSymbol.stackbase = hb_stackTopOffset();
   hb_stackPush();
}

/* +0    -> HB_P_PUSHBLOCK
 * +1 +2 -> size of codeblock
 * +3 +4 -> number of expected parameters
 * +5 +6 -> number of referenced local variables
 * +7    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols )
{
   USHORT uiLocals;
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlock(%p, %p)", pCode, pSymbols));

   pStackTopItem->type = HB_IT_BLOCK;

   uiLocals = HB_PCODE_MKUSHORT( &( pCode[ 5 ] ) );
   pStackTopItem->item.asBlock.value =
         hb_codeblockNew( pCode + 7 + uiLocals * 2, /* pcode buffer         */
         uiLocals,                                  /* number of referenced local variables */
         pCode + 7,                                 /* table with referenced local variables */
         pSymbols );

   /* store the statics base of function where the codeblock was defined
    */
   pStackTopItem->item.asBlock.statics = hb_stack.iStatics;
   /* store the number of expected parameters
    */
   pStackTopItem->item.asBlock.paramcnt = HB_PCODE_MKUSHORT( &( pCode[ 3 ] ) );
   /* store the line number where the codeblock was defined
    */
   pStackTopItem->item.asBlock.lineno = ( hb_stackBaseItem() )->item.asSymbol.lineno;
   hb_stackPush();
}

/* +0    -> HB_P_PUSHBLOCKSHORT
 * +1    -> size of codeblock
 * +2    -> start of table with referenced local variables
 *
 * NOTE: pCode points to static memory
 */
static void hb_vmPushBlockShort( BYTE * pCode, PHB_SYMB pSymbols )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlockShort(%p, %p)", pCode, pSymbols));

   pStackTopItem->type = HB_IT_BLOCK;

   pStackTopItem->item.asBlock.value =
         hb_codeblockNew( pCode + 2,                /* pcode buffer         */
         0,                                         /* number of referenced local variables */
         NULL,                                      /* table with referenced local variables */
         pSymbols );

   /* store the statics base of function where the codeblock was defined
    */
   pStackTopItem->item.asBlock.statics = hb_stack.iStatics;
   /* store the number of expected parameters
    */
   pStackTopItem->item.asBlock.paramcnt = 0;
   /* store the line number where the codeblock was defined
    */
   pStackTopItem->item.asBlock.lineno = ( hb_stackBaseItem() )->item.asSymbol.lineno;
   hb_stackPush();
}

/* +0    -> HB_P_MPUSHBLOCK
 * +1 +2 -> size of codeblock
 * +3 +4 -> number of expected parameters
 * +5    -> start of pcode
 *
 * NOTE: pCode points to dynamically allocated memory
 */
static void hb_vmPushMacroBlock( BYTE * pCode, PHB_SYMB pSymbols )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushMacroBlock(%p, %p)", pCode, pSymbols));

   HB_SYMBOL_UNUSED( pSymbols ); /* TODO: remove pSymbols */

   pStackTopItem->type = HB_IT_BLOCK;

   pStackTopItem->item.asBlock.value = hb_codeblockMacroNew( pCode + 5, HB_PCODE_MKUSHORT( &( pCode[ 1 ] ) ) - 5 );

   /* store the statics base of function where the codeblock was defined
    */
   pStackTopItem->item.asBlock.statics = hb_stack.iStatics;
   /* store the number of expected parameters
    */
   pStackTopItem->item.asBlock.paramcnt = HB_PCODE_MKUSHORT( &( pCode[ 3 ] ) );
   /* store the line number where the codeblock was defined
    */
   pStackTopItem->item.asBlock.lineno = ( hb_stackBaseItem() )->item.asSymbol.lineno;
   hb_stackPush();
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   PHB_ITEM pStackTopItem = hb_stackTopItem();

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAlias()"));

   pStackTopItem->type = HB_IT_INTEGER;
   pStackTopItem->item.asInteger.value = hb_rddGetCurrentWorkAreaNumber();
   pStackTopItem->item.asInteger.length = 10;
   hb_stackPush();
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of a given field
 * (for performance reason it replaces alias value with field value)
 */
static void hb_vmPushAliasedField( PHB_SYMB pSym )
{
   PHB_ITEM pAlias;
   int iCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedField(%p)", pSym));

   pAlias = hb_stackItemFromTop( -1 );
   iCurrArea = hb_rddGetCurrentWorkAreaNumber();

   /*
    This was added for Clipper compatibility
   */
   if( ( pAlias->type == HB_IT_ARRAY ) || ( pAlias->type == HB_IT_LOGICAL ) ||
       ( pAlias->type == HB_IT_NIL ) || ( pAlias->type == HB_IT_BLOCK ) ||
       ( pAlias->type == HB_IT_DATE ) )
   {
      PHB_ITEM pFName = hb_itemNew( NULL );

      hb_itemPutC( pFName, pSym->szName );
      hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&", 2, pAlias, pFName );
      hb_itemRelease( pFName );
   }
   else
   {
      /* NOTE: hb_vmSelecWorkarea clears passed item
       */
      if( hb_vmSelectWorkarea( pAlias ) == SUCCESS )
         hb_rddGetFieldValue( pAlias, pSym );

      hb_rddSelectWorkAreaNumber( iCurrArea );
   }
}

/* It pops the last item from the stack to use it to select a workarea
 * and next pushes the value of either a field or a memvar based on alias value
 * (for performance reason it replaces alias value with field value)
 * This is used in the following context:
 * ( any_alias )->variable
 */
static void hb_vmPushAliasedVar( PHB_SYMB pSym )
{
   PHB_ITEM pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAliasedVar(%p)", pSym));

   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( ( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' ) && szAlias[ 1 ] == '\0' )
      {  /* M->variable */
         hb_memvarGetValue( pAlias, pSym );
      }
      else
      {
         int iCmp = hb_strnicmp( szAlias, "MEMVAR", 4 );
         if( iCmp == 0 )
               iCmp = hb_strnicmp( szAlias, "MEMVAR", pAlias->item.asString.length );
         if( iCmp == 0 )
         {  /* MEMVAR-> or MEMVA-> or MEMV-> */
            hb_memvarGetValue( pAlias, pSym );
         }
         else
         {  /* field variable */
            iCmp = hb_strnicmp( szAlias, "FIELD", 4 );
            if( iCmp == 0 )
               iCmp = hb_strnicmp( szAlias, "FIELD", pAlias->item.asString.length );
            if( iCmp == 0 )
            {  /* FIELD-> */
               hb_rddGetFieldValue( pAlias, pSym );
            }
            else
            {  /* database alias */
               hb_vmPushAliasedField( pSym );
            }
         }
      }
   }
   else
      hb_vmPushAliasedField( pSym );
}

static void hb_vmPushLocal( SHORT iLocal )
{
   PHB_ITEM pLocal;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocal(%hd)", iLocal));

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = hb_stackItemFromBase( iLocal );
   }
   else
   {
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      pLocal = hb_codeblockGetVar( hb_stackSelfItem(), ( LONG ) iLocal );
   }

   if( HB_IS_BYREF( pLocal ) )
   {
      hb_itemCopy( ( hb_stackTopItem() ), hb_itemUnRef( pLocal ) );
   }
   else
   {
      hb_itemCopy( ( hb_stackTopItem() ), pLocal );
   }

   hb_stackPush();
}

static void hb_vmPushLocalByRef( SHORT iLocal )
{
   HB_ITEM_PTR pTop = hb_stackTopItem();
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocalByRef(%hd)", iLocal));

   pTop->type = HB_IT_BYREF;
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value = iLocal;
   pTop->item.asRefer.offset = hb_stackBaseOffset();
   if( iLocal >= 0 )
      pTop->item.asRefer.BasePtr.itemsbasePtr = &hb_stack.pItems;
   else
   {
      /* store direct codeblock address because an item where a codeblock
       * is stored can be no longer placed on the eval stack at the time
       * of a codeblock evaluation or variable access
      */
      pTop->item.asRefer.BasePtr.block = (hb_stackSelfItem())->item.asBlock.value;
   }
   hb_stackPush();
}

static void hb_vmPushStatic( USHORT uiStatic )
{
   PHB_ITEM pStatic;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStatic(%hu)", uiStatic));

   pStatic = s_aStatics.item.asArray.value->pItems + hb_stack.iStatics + uiStatic - 1;
   if( HB_IS_BYREF( pStatic ) )
      hb_itemCopy( hb_stackTopItem(), hb_itemUnRef( pStatic ) );
   else
      hb_itemCopy( hb_stackTopItem(), pStatic );
   hb_stackPush();
}

static void hb_vmPushStaticByRef( USHORT uiStatic )
{
   HB_ITEM_PTR pTop = hb_stackTopItem();
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStaticByRef(%hu)", uiStatic));

   pTop->type = HB_IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   pTop->item.asRefer.value = hb_stack.iStatics + uiStatic - 1;
   pTop->item.asRefer.offset = 0;    /* 0 for static variables */
   pTop->item.asRefer.BasePtr.itemsbase = &s_aStatics.item.asArray.value->pItems;
   hb_stackPush();
}

static void hb_vmPushVariable( PHB_SYMB pVarSymb )
{
   USHORT uiAction = E_DEFAULT;

   do
   {
      /* First try if passed symbol is a name of field
         * in a current workarea - if it is not a field (FAILURE)
         * then try the memvar variable
         */
      if( hb_rddFieldGet( ( hb_stackTopItem() ), pVarSymb ) == SUCCESS )
      {
         hb_stackPush();
      }
      else
      {
         if( hb_memvarGet( ( hb_stackTopItem() ), pVarSymb ) == SUCCESS )
         {
            hb_stackPush();
         }
         else
         {
            HB_ITEM_PTR pError;

            pError = hb_errRT_New( ES_ERROR, NULL, EG_NOVAR, 1003,
                                    NULL, pVarSymb->szName,
                                    0, EF_CANRETRY );

            uiAction = hb_errLaunch( pError );
            hb_errRelease( pError );
         }
      }
   }
   while( uiAction == E_RETRY );
   HB_TRACE(HB_TR_INFO, ("(hb_vmPushVariable)"));
}


static void hb_vmDuplicate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplicate()"));

   hb_itemCopy( ( hb_stackTopItem() ), hb_stackItemFromTop( -1 ) );
   hb_stackPush();
}

static void hb_vmDuplTwo( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplTwo()"));

   hb_itemCopy( ( hb_stackTopItem() ), hb_stackItemFromTop( -2 ) );
   hb_stackPush();
   hb_itemCopy( ( hb_stackTopItem() ), hb_stackItemFromTop( -2 ) );
   hb_stackPush();
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

static BOOL hb_vmPopLogical( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLogical()"));

   if( HB_IS_LOGICAL( hb_stackItemFromTop( -1 ) ) )
   {
      hb_stackDec();

      ( hb_stackTopItem() )->type = HB_IT_NIL;
      return ( hb_stackTopItem() )->item.asLogical.value;
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ), 1, hb_stackItemFromTop( -1 ) );
      return FALSE;
   }
}

/* NOTE: Type checking should be done by the caller. */

static long hb_vmPopDate( void )
{
   PHB_ITEM pStackTopItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopDate()"));

   hb_stackDec();
   pStackTopItem = hb_stackTopItem();
   pStackTopItem->type = HB_IT_NIL;
   return pStackTopItem->item.asDate.value;
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopNumber( void )
{
   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopNumber()"));

   pItem = hb_stackItemFromTop( -1 );
   hb_stackDec();

   switch( pItem->type & ~HB_IT_BYREF )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         break;

      default:
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         dNumber = 0;  /* To avoid GCC -O2 warning */
         break;
   }

   ( hb_stackTopItem() )->type = HB_IT_NIL;

   return dNumber;
}

/* NOTE: Type checking should be done by the caller. */

static HB_LONG hb_vmPopHBLong( void )
{
   PHB_ITEM pItem;
   HB_LONG lNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopHBLong()"));

   pItem = hb_stackItemFromTop( -1 );
   hb_stackDec();

   switch( pItem->type & ~HB_IT_BYREF )
   {
      case HB_IT_INTEGER:
         lNumber = ( HB_LONG ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         lNumber = ( HB_LONG ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         lNumber = ( HB_LONG ) pItem->item.asDouble.value;
         break;

      default:
         lNumber = 0;  /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         break;
   }

   ( hb_stackTopItem() )->type = HB_IT_NIL;

   return lNumber;
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopDouble( int * piDec )
{
   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopDouble(%p)", piDec));

   pItem = hb_stackItemFromTop( -1 );
   hb_stackDec();

   switch( pItem->type & ~HB_IT_BYREF )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         *piDec = 0;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         *piDec = 0;
         break;

      case HB_IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         *piDec = pItem->item.asDouble.decimal;
         break;

      default:
         dNumber = 0;  /* To avoid GCC -O2 warning */
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopDouble()", NULL );
         break;
   }

   ( hb_stackTopItem() )->type = HB_IT_NIL;

   return dNumber;
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAlias()"));

   hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ) ); /* it clears the passed item */
   hb_stackDec();
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into a given field
 */
static void hb_vmPopAliasedField( PHB_SYMB pSym )
{
   int iCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedField(%p)", pSym));

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if( hb_vmSelectWorkarea( hb_stackItemFromTop( -1 ) ) == SUCCESS )
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
   HB_ITEM_PTR pAlias = hb_stackItemFromTop( -1 );

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedVar(%p)", pSym));

   /* "M", "MEMV" - "MEMVAR" and "FIEL" - "FIELD" are reserved aliases
    */
   if( HB_IS_STRING( pAlias ) )
   {
      char * szAlias = pAlias->item.asString.value;

      if( ( szAlias[ 0 ] == 'M' || szAlias[ 0 ] == 'm' ) && szAlias[ 1 ] == '\0' )
      {  /* M->variable */
         hb_memvarSetValue( pSym, hb_stackItemFromTop( -2 ) );
         hb_stackPop();    /* alias */
         hb_stackPop();    /* value */
      }
      else
      {
         int iCmp = hb_strnicmp( szAlias, "MEMVAR", 4 );
         if( iCmp == 0 )
            iCmp = hb_strnicmp( szAlias, "MEMVAR", pAlias->item.asString.length );
         if( iCmp == 0 )
         {  /* MEMVAR-> or MEMVA-> or MEMV-> */
            hb_memvarSetValue( pSym, hb_stackItemFromTop( -2 ) );
            hb_stackPop();    /* alias */
            hb_stackPop();    /* value */
         }
         else
         {  /* field variable */
            iCmp = hb_strnicmp( szAlias, "FIELD", 4 );
            if( iCmp == 0 )
               iCmp = hb_strnicmp( szAlias, "FIELD", pAlias->item.asString.length );
            if( iCmp == 0 )
            {  /* FIELD-> */
               hb_rddPutFieldValue( hb_stackItemFromTop( -2 ), pSym );
               hb_stackPop();    /* alias */
               hb_stackPop();    /* value */
            }
            else
            {  /* database alias */
               hb_vmPopAliasedField( pSym );
            }
         }
      }
   }
   else
   {
      hb_vmPopAliasedField( pSym );
   }
}

static void hb_vmPopLocal( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLocal(%hd)", iLocal));

   hb_stackDec();

   hb_stackTopItem()->type &= ~HB_IT_MEMOFLAG;
   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      PHB_ITEM pLocal = hb_stackItemFromBase( iLocal );

      if( HB_IS_OBJECT( pLocal ) && hb_objHasMsg( pLocal, "__OpAssign" ) )
      {
         HB_ITEM item;

         hb_itemInit( &item );
         hb_itemCopy( &item, hb_stackTopItem() );
         hb_vmOperatorCall( pLocal, &item, "__OPASSIGN" );
         hb_itemClear( &item );
         hb_stackPush();
         return;
      }

      if( HB_IS_BYREF( pLocal ) )
         hb_itemMove( hb_itemUnRef( pLocal ), hb_stackTopItem() );
      else
         hb_itemMove( pLocal, hb_stackTopItem() );
   }
   else
      /* local variable referenced in a codeblock
       * hb_stackSelfItem() points to a codeblock that is currently evaluated
       */
      hb_itemMove( hb_codeblockGetVar( hb_stackSelfItem(), iLocal ), hb_stackTopItem() );

}

static void hb_vmPopStatic( USHORT uiStatic )
{
   PHB_ITEM pStatic;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopStatic(%hu)", uiStatic));

   hb_stackDec();
   hb_stackTopItem()->type &= ~HB_IT_MEMOFLAG;
   pStatic = s_aStatics.item.asArray.value->pItems + hb_stack.iStatics + uiStatic - 1;

   if( HB_IS_OBJECT( pStatic ) && hb_objHasMsg( pStatic, "__OpAssign" ) )
   {
      HB_ITEM item;

      hb_itemInit( &item );
      hb_itemCopy( &item, hb_stackTopItem() );
      hb_vmOperatorCall( pStatic, &item, "__OPASSIGN" );
      hb_itemClear( &item );
      hb_stackPush();
      return;
   }

   if( HB_IS_BYREF( pStatic ) )
      hb_itemMove( hb_itemUnRef( pStatic ), ( hb_stackTopItem() ) );
   else
      hb_itemMove( pStatic, ( hb_stackTopItem() ) );

}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmTopNumber( void )
{
   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmTopNumber()"));

   pItem = hb_stackItemFromTop( -1 );

   switch( pItem->type & ~HB_IT_BYREF )
   {
      case HB_IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         break;

      case HB_IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         break;

      case HB_IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         break;

      default:
         hb_errInternal( HB_EI_VMPOPINVITEM, NULL, "hb_vmPopNumber()", NULL );
         dNumber = 0;  /* To avoid GCC -O2 warning */
         break;
   }

   return dNumber;
}


/* ----------------------------------------------- */

void HB_EXPORT hb_vmProcessSymbols( PHB_SYMB pModuleSymbols, USHORT uiModuleSymbols ) /* module symbols initialization */
{
   PSYMBOLS pNewSymbols;
   USHORT ui;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSymbols(%p, %hu)", pModuleSymbols, uiModuleSymbols));

   pNewSymbols = ( PSYMBOLS ) hb_xgrab( sizeof( SYMBOLS ) );
   pNewSymbols->pModuleSymbols = pModuleSymbols;
   pNewSymbols->uiModuleSymbols = uiModuleSymbols;
   pNewSymbols->pNext = NULL;
   pNewSymbols->hScope = 0;

   if( s_pSymbols == NULL )
      s_pSymbols = pNewSymbols;
   else
   {
      PSYMBOLS pLastSymbols;

      pLastSymbols = s_pSymbols;
      while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
         pLastSymbols = pLastSymbols->pNext;

      pLastSymbols->pNext = pNewSymbols;
   }

   for( ui = 0; ui < uiModuleSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      HB_SYMBOLSCOPE hSymScope;

      hSymScope = ( pModuleSymbols + ui )->cScope;
      pNewSymbols->hScope |= hSymScope;
      if( ( ! s_pSymStart ) && ( hSymScope & HB_FS_FIRST && ! (  hSymScope & HB_FS_INITEXIT ) ) )
            s_pSymStart = pModuleSymbols + ui;  /* first public defined symbol to start execution */

      if( hSymScope & ( HB_FS_PUBLIC | HB_FS_MESSAGE | HB_FS_MEMVAR | HB_FS_FIRST ) )
         hb_dynsymNew( pModuleSymbols + ui );
   }
}

static void hb_vmReleaseLocalSymbols( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmReleaseLocalSymbols()"));

   while( s_pSymbols )
   {
      PSYMBOLS pDestroy;

      pDestroy = s_pSymbols;
      s_pSymbols = s_pSymbols->pNext;
      hb_xfree( pDestroy );
   }
}

/* This calls all _INITSTATICS functions defined in the application.
 * We are using a special symbol's scope ( HB_FS_INIT | HB_FS_EXIT ) to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void hb_vmDoInitStatics( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitStatics()"));

   do
   {
      if( ( pLastSymbols->hScope & ( HB_FS_INIT | HB_FS_EXIT ) ) == ( HB_FS_INIT | HB_FS_EXIT ) )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( HB_FS_EXIT | HB_FS_INIT );

            if( scope == ( HB_FS_INIT | HB_FS_EXIT ) )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}

static void hb_vmDoExitFunctions( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoExitFunctions()"));

   do
   {
      /* only if module contains some EXIT functions */
      if( pLastSymbols->hScope & HB_FS_EXIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( HB_FS_EXIT | HB_FS_INIT );

            if( scope == HB_FS_EXIT )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();
               hb_vmDo( 0 );
               if( s_uiActionRequest )
                  /* QUIT or BREAK was issued - stop processing
                  */
                  return;
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}

static void hb_vmDoInitFunctions( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitFunctions()"));

   do
   {
      /* only if module contains some INIT functions */
      if( pLastSymbols->hScope & HB_FS_INIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( HB_FS_EXIT | HB_FS_INIT );

            if( scope == HB_FS_INIT )
            {
               int argc = hb_cmdargARGC();
               char ** argv = hb_cmdargARGV();

               int i;
               int iArgCount;

               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               hb_vmPushNil();

               iArgCount = 0;
               for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
               {
                  /* Filter out any parameters beginning with //, like //INFO */
                  if( ! hb_cmdargIsInternal( argv[ i ] ) )
                  {
                     hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
                     iArgCount++;
                  }
               }

               hb_vmDo( iArgCount );
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;

   } while( pLastSymbols );
}

/* NOTE: We should make sure that these get linked.
         Don't make this function static, because it's not called from
         this file. [vszakats] */

void hb_vmForceLink( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmForceLink()"));

   HB_FUNCNAME( SYSINIT )();
}

/* ----------------------------- */

HB_FUNC( ERRORLEVEL )
{
   hb_retni( s_byErrorLevel );

   /* NOTE: This should be ISNUM( 1 ), but it's sort of a Clipper bug that it
            accepts other types also and considers them zero. [vszakats] */

   if( hb_pcount() >= 1 )
      /* Only replace the error level if a parameter was passed */
      s_byErrorLevel = hb_parni( 1 );
}

void hb_vmRequestQuit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestQuit()"));

   s_uiActionRequest = HB_QUIT_REQUESTED;
}

void hb_vmRequestEndProc( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestEndProc()"));

   s_uiActionRequest = HB_ENDPROC_REQUESTED;
}

void hb_vmRequestBreak( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestBreak(%p)", pItem));

   if( s_lRecoverBase )
   {
      if( pItem )
         hb_itemCopy( hb_stackItem( s_lRecoverBase + HB_RECOVER_VALUE ), pItem );

      s_uiActionRequest = HB_BREAK_REQUESTED;
   }
   else
      s_uiActionRequest = HB_QUIT_REQUESTED;
}

USHORT hb_vmRequestQuery( void )
{
   return s_uiActionRequest;
}

void hb_vmRequestCancel( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestCancel()"));

   if( hb_set.HB_SET_CANCEL )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 2 ];
      int i = 1, i2;
      ULONG ulLine;
      PHB_ITEM * pBase;

      hb_conOutErr( hb_conNewLine(), 0 );
      sprintf( buffer, "Cancelled at: %s (%i)", ( hb_stackBaseItem() )->item.asSymbol.value->szName, ( hb_stackBaseItem() )->item.asSymbol.lineno );
      hb_conOutErr( buffer, 0 );
      hb_conOutErr( hb_conNewLine(), 0 );

      while ( buffer[0] )
      {
         i2 = i;
         hb_procname( i++, buffer, FALSE );

         if( buffer[0] == 0 )
            break;

         pBase = hb_stack.pBase;
         while( ( i2-- > 0 ) && pBase != hb_stack.pItems )
            pBase = hb_stack.pItems + ( *pBase )->item.asSymbol.stackbase;

         if( i2 == -1 )
            ulLine = ( *pBase )->item.asSymbol.lineno;
         else
            ulLine = 0;

         i2 = strlen( (char *) buffer );
         sprintf( buffer + i2, " (%lu)", ulLine );

         hb_conOutErr( buffer, 0 );
         hb_conOutErr( hb_conNewLine(), 0 );
      }

      s_uiActionRequest = HB_QUIT_REQUESTED;
   }
}

#undef hb_vmFlagEnabled
ULONG hb_vmFlagEnabled( ULONG flags )
{
	return s_VMFlags & (flags);
}

/* ------------------------------------------------------------------------ */
/* The debugger support functions */
/* ------------------------------------------------------------------------ */

void hb_vmRequestDebug( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestDebug()"));
   s_bDebugRequest = TRUE;
}

/* check if the debugger activation was requested or request the debugger
activation if .T. is passed
*/
HB_FUNC( HB_DBG_INVOKEDEBUG )
{
   BOOL bRequest = s_bDebugRequest;
   if( hb_pcount() > 0 )
      s_bDebugRequest = hb_parl(1);
   else
      s_bDebugRequest = FALSE;
   hb_retl( bRequest );
}

/* $Doc$
 * $FuncName$     <aStat> __vmVarSList()
 * $Description$  Return the statics array. Please aClone before assignments
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLIST )
{
   PHB_ITEM pStatics = hb_arrayClone( &s_aStatics, NULL );

   hb_itemCopy( &hb_stack.Return, pStatics );
   hb_itemRelease( pStatics );
}

/* $Doc$
 * $FuncName$     <nStatics> __vmVarSLen()
 * $Description$  Return the statics array length.
 * $End$ */
HB_FUNC( HB_DBG_VMVARSLEN )
{
   hb_retnl( s_aStatics.item.asArray.value->ulLen );
}

/* $Doc$
 * $FuncName$     <xStat> __vmVarSGet(<nStatic>)
 * $Description$  Return a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSGET )
{
   hb_itemReturn( s_aStatics.item.asArray.value->pItems + hb_parni(1) + hb_parni(2) - 1 );
}

/* $Doc$
 * $FuncName$     __vmVarSSet(<nStatic>,<uValue>)
 * $Description$  Sets the value of a specified statics
 * $End$ */
HB_FUNC( HB_DBG_VMVARSSET )
{
   hb_itemCopy( s_aStatics.item.asArray.value->pItems + hb_parni(1) + hb_parni(2) - 1,
                hb_itemParamPtr( 3, HB_IT_ANY ) );
}

HB_FUNC( HB_DBG_PROCLEVEL )
{
   hb_retnl( s_ulProcLevel - 1 );   /* Don't count self */
}

/* ------------------------------------------------------------------------ */
/* The garbage collector interface */
/* ------------------------------------------------------------------------ */

/* Mark all statics as used so they will not be released by the
 * garbage collector
 */
void hb_vmIsStaticRef( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmIsStaticRef()"));

   /* statics are stored as an item of array type */
   hb_gcItemRef( &s_aStatics );
}

/* $Doc$
 * $FuncName$     __SETPROFILER( <lOnOff> ) --> <lOldValue>
 * $Description$  Turns on | off the profiler activity
 * $End$ */
HB_FUNC( __SETPROFILER )
{
   BOOL bOldValue = hb_bProfiler;

   hb_bProfiler = hb_parl( 1 );

   hb_retl( bOldValue );
}

/* $Doc$
 * $FuncName$     __TRACEPRGCALLS( <lOnOff> ) --> <lOldValue>
 * $Description$  Turns on | off tracing of PRG-level function and method calls
 * $End$ */
HB_FUNC( __TRACEPRGCALLS )
{
   BOOL bOldValue = hb_bTracePrgCalls;

   hb_bTracePrgCalls = hb_parl( 1 );

   hb_retl( bOldValue );
}

/* hvm support for pcode DLLs */

void HB_EXPORT hb_vmProcessDllSymbols( PHB_SYMB pModuleSymbols, USHORT uiModuleSymbols )
{
   PSYMBOLS pNewSymbols;
   USHORT ui;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessDllSymbols(%p, %hu)", pModuleSymbols, uiModuleSymbols));

   pNewSymbols = ( PSYMBOLS ) hb_xgrab( sizeof( SYMBOLS ) );
   pNewSymbols->pModuleSymbols = pModuleSymbols;
   pNewSymbols->uiModuleSymbols = uiModuleSymbols;
   pNewSymbols->pNext = NULL;
   pNewSymbols->hScope = 0;

   if( s_pSymbols == NULL )
      s_pSymbols = pNewSymbols;
   else
   {
      PSYMBOLS pLastSymbols;

      pLastSymbols = s_pSymbols;
      while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
         pLastSymbols = pLastSymbols->pNext;

      pLastSymbols->pNext = pNewSymbols;
   }

   for( ui = 0; ui < uiModuleSymbols; ui++ ) /* register each public symbol on the dynamic symbol table */
   {
      HB_SYMBOLSCOPE hSymScope;

      hSymScope = ( pModuleSymbols + ui )->cScope;
      pNewSymbols->hScope |= hSymScope;

      if( ( hSymScope == HB_FS_PUBLIC ) || ( hSymScope & ( HB_FS_MESSAGE | HB_FS_MEMVAR | HB_FS_FIRST ) ) )
      {
         PHB_DYNS pDynSym = hb_dynsymFind( ( pModuleSymbols + ui )->szName );

         if( pDynSym && pDynSym->pFunPtr && ( pModuleSymbols + ui )->value.pFunPtr )
            ( pModuleSymbols + ui )->value.pFunPtr = pDynSym->pFunPtr;
         else
             hb_dynsymNew( ( pModuleSymbols + ui ) );
      }
   }
}

HB_FUNC( __OPCOUNT ) /* it returns the total amount of opcodes */
{
   hb_retnl( HB_P_LAST_PCODE - 1 );
}

HB_FUNC( __OPGETPRF ) /* profiler: It returns an array with an opcode called and
                         consumed times { nTimes, nTime },
                         given the opcode index */
{
   ULONG ulOpcode = hb_parnl( 1 );

   hb_reta( 2 );
   hb_stornl( 0, -1, 1 );
   hb_stornl( 0, -1, 2 );

   if( ulOpcode < HB_P_LAST_PCODE )
   {
      hb_stornl( hb_ulOpcodesCalls[ ulOpcode ], -1, 1 );
      hb_stornl( hb_ulOpcodesTime[ ulOpcode ],  -1, 2 );
   }
}


HB_FUNC( __VMVARSLIST )
{
   HB_FUNCNAME(HB_DBG_VMVARSLIST)();
}

HB_FUNC( __VMVARSLEN )
{
   HB_FUNCNAME(HB_DBG_VMVARSLEN)();
}

HB_FUNC( __VMVARSGET )
{
   HB_FUNCNAME(HB_DBG_VMVARSGET)();
}

HB_FUNC( __VMVARSSET )
{
   HB_FUNCNAME(HB_DBG_VMVARSSET)();
}


#undef HB_FORCE_LINK_MAIN

#if defined(HB_OS_WIN_32) && !defined(__EXPORT__) && \
    ( defined(__WATCOMC__) || defined(__MINGW32__) )

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainWin

#elif defined(HB_OS_LINUX) && defined(__WATCOMC__)

#  define HB_FORCE_LINK_MAIN  hb_forceLinkMainStd

#endif

#ifdef HB_FORCE_LINK_MAIN
HB_EXTERN_BEGIN
extern void HB_EXPORT HB_FORCE_LINK_MAIN( void );
HB_EXTERN_END
void _hb_forceLinkMain()
{
   HB_FORCE_LINK_MAIN();
}
#endif

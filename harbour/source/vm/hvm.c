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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 *    HB_WORD()
 *    HB___XHELP()
 *    HB_PROCFILE()
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 *    HB___VMVARSGET()
 *    HB___VMVARSLIST()
 *
 * See doc/license.txt for licensing terms.
 *
 */

#ifndef __MPW__
   #include <malloc.h>
#endif
#include <math.h>

#include "extend.h"
#include "ctoharb.h"
#include "errorapi.h"
#include "itemapi.h"
#include "langapi.h"
#include "rddapi.h"
#include "pcode.h"
#include "set.h"
#include "inkey.h"
#include "hbmemory.ch"

typedef struct _SYMBOLS
{
   PHB_SYMB pModuleSymbols;  /* pointer to a one module own symbol table */
   USHORT   uiModuleSymbols; /* number of symbols on that table */
   struct _SYMBOLS * pNext;  /* pointer to the next SYMBOLS structure */
   HB_SYMBOLSCOPE hScope;    /* scope collected from all symbols in module used to speed initialization code */
} SYMBOLS, * PSYMBOLS;       /* structure to keep track of all modules symbol tables */

extern HARBOUR HB_SYSINIT( void );

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
static void    hb_vmArrayAt( void );         /* pushes an array element to the stack, removing the array and the index from the stack */
static void    hb_vmArrayPut( void );        /* sets an array value and pushes the value on to the stack */
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
static void    hb_vmModuleName( char * szModuleName ); /* PRG and function name information for the debugger */
static void    hb_vmFrame( BYTE bLocals, BYTE bParams ); /* increases the stack pointer for the amount of locals and params suplied */
static void    hb_vmSFrame( PHB_SYMB pSym );     /* sets the statics frame for a function */
static void    hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ); /* increases the the global statics array to hold a PRG statics */
static void    hb_vmEndBlock( void );            /* copies the last codeblock pushed value into the return value */
static void    hb_vmRetValue( void );            /* pops the latest stack value into stack.Return */
static void    hb_vmDebuggerShowLine( USHORT uiLine ); /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void );     /* notifies the debugger for an endproc */

/* Push */
static void    hb_vmPushAlias( void );            /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB ); /* pushes an aliased field on the eval stack */
static void    hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a codeblock */
static void    hb_vmPushLocal( SHORT iLocal );    /* pushes the containts of a local onto the stack */
static void    hb_vmPushLocalByRef( SHORT iLocal );    /* pushes a local by refrence onto the stack */
static void    hb_vmPushStatic( USHORT uiStatic );     /* pushes the containts of a static onto the stack */
static void    hb_vmPushStaticByRef( USHORT uiStatic ); /* pushes a static by refrence onto the stack */
static void    hb_vmDuplicate( void );            /* duplicates the latest value on the stack */
static void    hb_vmDuplTwo( void );              /* duplicates the latest two value on the stack */

/* Pop */
static BOOL    hb_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
static long    hb_vmPopDate( void );              /* pops the stack latest value and returns its date value as a LONG */
static double  hb_vmPopNumber( void );            /* pops the stack latest value and returns its numeric value */
static double  hb_vmPopDouble( int * );           /* pops the stack latest value and returns its double numeric format value */
static void    hb_vmPopAlias( void );             /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );  /* pops an aliased field from the eval stack*/
static void    hb_vmPopLocal( SHORT iLocal );     /* pops the stack latest value onto a local */
static void    hb_vmPopStatic( USHORT uiStatic ); /* pops the stack latest value onto a static */

/* stack management functions */
static void    hb_stackDec( void );        /* pops an item from the stack without clearing it's contents */
static void    hb_stackPop( void );        /* pops an item from the stack */
static void    hb_stackFree( void );       /* releases all memory used by the stack */
static void    hb_stackPush( void );       /* pushes an item on to the stack */
static void    hb_stackInit( void );       /* initializes the stack */
static void    hb_stackDispLocal( void );  /* show the types of the items on the stack for debugging purposes */

#define STACK_INITHB_ITEMS      100
#define STACK_EXPANDHB_ITEMS    20

/* misc */
static void    hb_vmDoInitStatics( void );        /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( void );      /* executes all defined PRGs INIT functions */
static void    hb_vmDoExitFunctions( void );      /* executes all defined PRGs EXIT functions */
static void    hb_vmReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */

#ifdef HARBOUR_OBJ_GENERATION

/* TODO: Remove these (WORD/DWORD) when the compiler is cleaned up from them. */
#if defined(__IBMCPP__)
   #undef WORD                            /* 2 bytes unsigned */
   typedef unsigned short int WORD;
#else
   #if ! defined(HB_DONT_DEFINE_BASIC_TYPES)
      #undef WORD                            /* 2 bytes unsigned */
      typedef unsigned short int WORD;
   #endif
#endif

static void    hb_vmProcessObjSymbols ( void ); /* process Harbour generated OBJ symbols */

typedef struct
{
   WORD     wSymbols;             /* module local symbol table symbols amount */
   PHB_SYMB pSymbols;             /* module local symbol table address */
} OBJSYMBOLS, * POBJSYMBOLS;      /* structure used from Harbour generated OBJs */

#ifdef __cplusplus
extern "C" POBJSYMBOLS HB_FIRSTSYMBOL, HB_LASTSYMBOL;
#else
extern POBJSYMBOLS HB_FIRSTSYMBOL, HB_LASTSYMBOL;
#endif
#endif

/* virtual machine state */

HB_STACK hb_stack;
HB_SYMB  hb_symEval = { "__EVAL", FS_PUBLIC, hb_vmDoBlock, 0 }; /* symbol to evaluate codeblocks */

static HB_ITEM  s_aStatics;         /* Harbour array to hold all application statics variables */
static BOOL     s_bDebugging = FALSE;
static BOOL     s_bDebugShowLines = FALSE; /* update source code line on the debugger display */
static PHB_SYMB s_pSymStart = NULL; /* start symbol of the application. MAIN() is not required */
static PSYMBOLS s_pSymbols = NULL;  /* to hold a linked list of all different modules symbol tables */
static BYTE     s_byErrorLevel = 0; /* application exit errorlevel */

/* Stores the position on the stack of current SEQUENCE envelope or 0 if no
 * SEQUENCE is active
 */
static LONG     s_lRecoverBase = 0;
#define  HB_RECOVER_STATE     -1
#define  HB_RECOVER_BASE      -2
#define  HB_RECOVER_ADDRESS   -3
#define  HB_RECOVER_VALUE     -4

/* Request for some action - stop processing of opcodes
 */
static USHORT s_uiActionRequest = 0;

/* application entry point */

void hb_vmInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInit()"));

   /* initialize internal data structures */
   s_aStatics.type = IT_NIL;
   hb_stack.pItems = NULL; /* keep this here as it is used by fm.c */
   hb_stack.Return.type = IT_NIL;

   hb_xinit();
   hb_errInit();
   hb_stackInit();
   hb_dynsymNew( &hb_symEval );  /* initialize dynamic symbol for evaluating codeblocks */
   hb_setInitialize();        /* initialize Sets */
   hb_consoleInitialize();    /* initialize Console */
   hb_memvarsInit();
#ifdef HARBOUR_OBJ_GENERATION
   hb_vmProcessObjSymbols();  /* initialize Harbour generated OBJs symbols */
#endif
   hb_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */

   /* Check for some internal switches */

   if( hb_cmdargCheck( "INFO" ) )
   {
      char * pszVersion = hb_version( 1 );
      char buffer[ 128 ];

      hb_outerr( pszVersion, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
      sprintf( buffer, "DS avail=%luKB  OS avail=%luKB  EMM avail=%luKB", hb_xquery( HB_MEM_BLOCK ), hb_xquery( HB_MEM_VM ), hb_xquery( HB_MEM_EMS ) );
      hb_outerr( buffer, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );

      hb_xfree( pszVersion );
   }

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables
    */
   hb_vmDoInitStatics();
   hb_vmDoInitFunctions(); /* process defined INIT functions */

   /* This is undocumented CA-Clipper, if there's a function called _APPMAIN
      it will be executed first. */
   {
      PHB_DYNS pDynSym = hb_dynsymFind( "_APPMAIN" );

      if( pDynSym && pDynSym->pSymbol->pFunPtr )
         s_pSymStart = pDynSym->pSymbol;
#ifdef HARBOUR_START_PROCEDURE
      else
      {
         pDynSym = hb_dynsymFind( HARBOUR_START_PROCEDURE );

         if( pDynSym && pDynSym->pSymbol->pFunPtr )
            s_pSymStart = pDynSym->pSymbol;
         else
            hb_errInternal( 9999, "Can\'t locate the starting procedure: \'%s\'", HARBOUR_START_PROCEDURE, NULL );
      }
#else
#ifndef HARBOUR_STRICT_CLIPPER_COMPATIBILITY
      else if( ! s_pSymStart )
         hb_errInternal( 9999, "No starting procedure", NULL, NULL );
#endif
#endif
   }

   if( s_pSymStart )
   {
      int i;
      int iArgCount;

      hb_vmPushSymbol( s_pSymStart ); /* pushes first FS_PUBLIC defined symbol to the stack */
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
}

void hb_vmQuit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmQuit()"));

   s_uiActionRequest = 0;         /* EXIT procedures should be processed */
   hb_vmDoExitFunctions();       /* process defined EXIT functions */

   while( hb_stack.pPos > hb_stack.pItems )
      hb_stackPop();

   hb_itemClear( &hb_stack.Return );
   hb_arrayRelease( &s_aStatics );
   hb_rddShutDown();
   hb_errExit();
   hb_clsReleaseAll();
   hb_vmReleaseLocalSymbols();  /* releases the local modules linked list */
   hb_dynsymRelease();          /* releases the dynamic symbol table */
   hb_consoleRelease();         /* releases Console */
   hb_setRelease();             /* releases Sets */
   hb_memvarsRelease();
   hb_stackFree();
/* hb_dynsymLog(); */
   hb_xexit();

   exit( s_byErrorLevel );
}

void hb_vmExecute( BYTE * pCode, PHB_SYMB pSymbols )
{
   BYTE bCode;
   USHORT w = 0;
   USHORT uiParams;
   BOOL bCanRecover = FALSE;
   ULONG ulPrivateBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmExecute(%p, %p)", pCode, pSymbols));

   ulPrivateBase = hb_memvarGetPrivatesBase();
   while( ( bCode = pCode[ w ] ) != HB_P_ENDPROC )
   {
      switch( bCode )
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

         case HB_P_ARRAYAT:
            hb_vmArrayAt();
            w++;
            break;

         case HB_P_ARRAYPUT:
            hb_vmArrayPut();
            w++;
            break;

         case HB_P_ARRAYDIM:
            hb_vmArrayDim( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_ARRAYGEN:
            hb_vmArrayGen( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         /* Object */

         case HB_P_MESSAGE:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmMessage( pSymbols + uiParams );
            w += 3;
            break;

         /* Database */

         case HB_P_SWAPALIAS:
            hb_vmSwapAlias();
            w++;
            break;

         /* Execution */

         case HB_P_DO:
            hb_inkeyPoll();           /* Poll the console keyboard */
            hb_vmDo( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_FUNCTION:
            hb_vmFunction( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_LINE:

            HB_TRACE(HB_TR_INFO, ("Opcode: HB_P_LINE: %s (%i)", hb_stack.pBase->item.asSymbol.value->szName, hb_stack.pBase->item.asSymbol.lineno));

            hb_stack.pBase->item.asSymbol.lineno = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            if( s_bDebugging && s_bDebugShowLines )
               hb_vmDebuggerShowLine( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PARAMETER:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_memvarNewParameter( pSymbols + uiParams, hb_stack.pBase + 1 + pCode[ w + 3 ] );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopParameter)"));
            w += 4;
            break;

         case HB_P_FRAME:
            hb_vmFrame( pCode[ w + 1 ], pCode[ w + 2 ] );
            w += 3;
            break;

         case HB_P_SFRAME:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmSFrame( pSymbols + uiParams );
            w += 3;
            break;

         case HB_P_STATICS:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmStatics( pSymbols + uiParams, pCode[ w + 3 ] + ( pCode[ w + 4 ] * 256 ) );
            w += 5;
            break;

         case HB_P_RETVALUE:
            hb_vmRetValue();
            w++;
            break;

         case HB_P_LOCALNAME:
            hb_vmLocalName( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ),
                            ( char * ) pCode + w + 3 );
            w += 3;
            while( pCode[ w++ ] );
            break;

         case HB_P_MODULENAME:
            hb_vmModuleName( ( char * ) pCode + w + 1 );
            while( pCode[ w++ ] );
            break;

         case HB_P_ENDBLOCK:
            hb_vmEndBlock();
            HB_TRACE(HB_TR_INFO, ("(EndBlock)"));
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
            hb_stack.pPos->type = IT_NIL;
            hb_stackPush();
            /*
             * 2) store the address of RECOVER or END opcode
             */
            hb_stack.pPos->type = IT_LONG;
            hb_stack.pPos->item.asLong.value = w + pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_stackPush();
            /*
             * 3) store current RECOVER base
             */
            hb_stack.pPos->type = IT_LONG;
            hb_stack.pPos->item.asLong.value = s_lRecoverBase;
            hb_stackPush();
            /*
             * 4) store current bCanRecover flag - in a case of nested sequences
             * in the same procedure/function
             */
            hb_stack.pPos->type = IT_LOGICAL;
            hb_stack.pPos->item.asLogical.value = bCanRecover;
            hb_stackPush();
            /*
             * set new recover base
             */
            s_lRecoverBase = hb_stack.pPos - hb_stack.pItems;
            /*
             * we are now inside a valid SEQUENCE envelope
             */
            bCanRecover = TRUE;
            w += 3;
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
            bCanRecover = hb_stack.pPos->item.asLogical.value;
            hb_stack.pPos->type = IT_NIL;
            /*
             * 3) Restore previous RECOVER base
             */
            hb_stackDec();
            s_lRecoverBase = hb_stack.pPos->item.asLong.value;
            hb_stack.pPos->type = IT_NIL;
            /*
             * 2) Remove RECOVER address
             */
            hb_stackDec();
            hb_stack.pPos->type = IT_NIL;
            /* 1) Discard the value returned by BREAK statement - there
             * was no RECOVER clause or there was no BREAK statement
             */
            hb_stackPop();
            /*
             * skip outside of SEQUENCE structure
             */
            w += pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            break;

         case HB_P_SEQRECOVER:
            /*
             * Execute the RECOVER code
             */
            /*
             * 4) Restore previous recovery state
             */
            hb_stackDec();
            bCanRecover = hb_stack.pPos->item.asLogical.value;
            hb_stack.pPos->type = IT_NIL;
            /*
             * 3) Restore previous RECOVER base
             */
            hb_stackDec();
            s_lRecoverBase = hb_stack.pPos->item.asLong.value;
            hb_stack.pPos->type = IT_NIL;
            /*
             * 2) Remove RECOVER address
             */
            hb_stackDec();
            hb_stack.pPos->type = IT_NIL;
            /*
             * 1) Leave the value returned from BREAK  - it will be popped
             * in next executed opcode
             */
            w++;
            break;

         /* Jumps */

         case HB_P_JUMP:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            if( uiParams )
               w += uiParams;
            else
               w += 3;
            break;

         case HB_P_JUMPFALSE:
            if( ! hb_vmPopLogical() )
               w += pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            else
               w += 3;
            break;

         case HB_P_JUMPTRUE:
            if( hb_vmPopLogical() )
               w += pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            else
               w += 3;
            break;

         /* Push */

         case HB_P_TRUE:
            hb_vmPushLogical( TRUE );
            w++;
            break;

         case HB_P_FALSE:
            hb_vmPushLogical( FALSE );
            w++;
            break;

         case HB_P_ZERO:
            hb_vmPushInteger( 0 );
            w++;
            break;

         case HB_P_PUSHNIL:
            hb_stack.pPos->type = IT_NIL;
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushNil)"));
            w++;
            break;

         case HB_P_PUSHINT:
            hb_vmPushInteger( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PUSHLONG:
            hb_vmPushLong( * ( long * ) ( &pCode[ w + 1 ] ) );
            w += 5;
            break;

         case HB_P_PUSHDOUBLE:
            hb_vmPushDouble( * ( double * ) ( &pCode[ w + 1 ] ), ( int ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) ] );
            w += 1 + sizeof( double ) + 1;
            break;

         case HB_P_PUSHSTR:
            {
               USHORT uiSize = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
               hb_vmPushString( ( char * ) pCode + w + 3, ( ULONG ) uiSize );
               w += ( uiSize + 3 );
            }
            break;

         case HB_P_PUSHBLOCK:
            /* +0    -> _pushblock
             * +1 +2 -> size of codeblock
             * +3 +4 -> number of expected parameters
             * +5 +6 -> number of referenced local variables
             * +7    -> start of table with referenced local variables
             */
            hb_vmPushBlock( pCode + w, pSymbols );
            w += ( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            break;

         case HB_P_PUSHSELF:
            hb_vmPush( hb_stack.pBase + 1 );
            w++;
            break;

         case HB_P_PUSHSYM:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmPushSymbol( pSymbols + uiParams );
            w += 3;
            break;

         case HB_P_PUSHALIAS:
            hb_vmPushAlias();
            w++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmPushAliasedField( pSymbols + uiParams );
            w += 3;
            break;

         case HB_P_PUSHFIELD:
            /* It pushes the current value of the given field onto the eval stack
             */
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_rddGetFieldValue( hb_stack.pPos, pSymbols + uiParams );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushField)"));
            w += 3;
            break;

         case HB_P_PUSHLOCAL:
            hb_vmPushLocal( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PUSHLOCALREF:
            hb_vmPushLocalByRef( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PUSHSTATIC:
            hb_vmPushStatic( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PUSHSTATICREF:
            hb_vmPushStaticByRef( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_PUSHMEMVAR:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_memvarGetValue( hb_stack.pPos, pSymbols + uiParams );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvar)"));
            w += 3;
            break;

         case HB_P_PUSHMEMVARREF:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_memvarGetRefer( hb_stack.pPos, pSymbols + uiParams );
            hb_stackPush();
            HB_TRACE(HB_TR_INFO, ("(hb_vmPushMemvarRef)"));
            w += 3;
            break;

         case HB_P_PUSHVARIABLE:
            /* Push a value of variable of unknown type onto the eval stack
             */
            {
               USHORT uiAction = E_DEFAULT;
               PHB_SYMB pVarSymb = pSymbols + pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );

               do
               {
                  /* First try if passed symbol is a name of field
                   * in a current workarea - if it is not a field (FAILURE)
                   * then try the memvar variable
                   */
                  if( hb_rddFieldGet( hb_stack.pPos, pVarSymb ) == SUCCESS )
                     hb_stackPush();
                  else
                  {
                     if( hb_memvarGet( hb_stack.pPos, pVarSymb ) == SUCCESS )
                        hb_stackPush();
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
               w += 3;
            }
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
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_vmPopAliasedField( pSymbols + uiParams );
            w += 3;
            break;

         case HB_P_POPFIELD:
            /* Pops a value from the eval stack and uses it to set
             * a new value of the given field
             */
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_stackDec();
            hb_rddPutFieldValue( hb_stack.pPos, pSymbols + uiParams );
            hb_itemClear( hb_stack.pPos );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopField)"));
            w += 3;
            break;

         case HB_P_POPLOCAL:
            hb_vmPopLocal( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_POPSTATIC:
            hb_vmPopStatic( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
            w += 3;
            break;

         case HB_P_POPMEMVAR:
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            hb_stackDec();
            hb_memvarSetValue( pSymbols + uiParams, hb_stack.pPos );
            hb_itemClear( hb_stack.pPos );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopMemvar)"));
            w += 3;
            break;

         case HB_P_POPVARIABLE:
            /* Pops a value from the eval stack and uses it to set
             * a new value of a variable of unknown type.
             */
            uiParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
            /* First try if passed symbol is a name of field
             * in a current workarea - if it is not a field (FAILURE)
             * then try the memvar variable (it will create PRIVATE
             * variable if this variable doesn't exist)
             */
            hb_stackDec();
            if( hb_rddFieldPut( hb_stack.pPos, pSymbols + uiParams ) == FAILURE )
               hb_memvarSetValue( pSymbols + uiParams, hb_stack.pPos );
            hb_itemClear( hb_stack.pPos );
            HB_TRACE(HB_TR_INFO, ("(hb_vmPopVariable)"));
            w += 3;
            break;

         /* misc */

         case HB_P_NOOP:
            /* Intentionally do nothing */
            w++;
            break;

         default:
            /* TODO: Include to failing pcode in the error message */
            hb_errInternal( 9999, "Unsupported VM opcode", NULL, NULL );
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
               while( hb_stack.pPos > hb_stack.pItems + s_lRecoverBase )
                  hb_stackPop();
               /*
                * reload the address of recovery code
                */
               w = hb_stack.pItems[ s_lRecoverBase + HB_RECOVER_ADDRESS ].item.asLong.value;
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
      }
   }
   hb_memvarSetPrivatesBase( ulPrivateBase );
}

/* ------------------------------- */
/* Operators ( mathematical        */
/*             character / misc )  */
/* ------------------------------- */

/* NOTE: Clipper is resetting the number width on a negate. */

static void hb_vmNegate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmNegate()"));

   if( IS_INTEGER( hb_stack.pPos - 1 ) )
   {
      ( hb_stack.pPos - 1 )->item.asInteger.value = -( hb_stack.pPos - 1 )->item.asInteger.value;
      ( hb_stack.pPos - 1 )->item.asInteger.length = 10;
   }
   else if( IS_LONG( hb_stack.pPos - 1 ) )
   {
      ( hb_stack.pPos - 1 )->item.asLong.value = -( hb_stack.pPos - 1 )->item.asLong.value;
      ( hb_stack.pPos - 1 )->item.asLong.length = 10;
   }
   else if( IS_DOUBLE( hb_stack.pPos - 1 ) )
   {
      ( hb_stack.pPos - 1 )->item.asDouble.value = -( hb_stack.pPos - 1 )->item.asDouble.value;
      ( hb_stack.pPos - 1 )->item.asDouble.length = ( hb_stack.pPos - 1 )->item.asDouble.value >= 10000000000.0 ? 20 : 10;
   }
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1080, NULL, "-" );

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

   pItem1 = hb_stack.pPos - 2;
   pItem2 = hb_stack.pPos - 1;
   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      if( ( double ) ( ( double ) pItem1->item.asString.length + ( double ) pItem2->item.asString.length ) < ( double ) ULONG_MAX )
      {
         pItem1->item.asString.value = ( char * ) hb_xrealloc( pItem1->item.asString.value, pItem1->item.asString.length + pItem2->item.asString.length + 1 );
         hb_xmemcpy( pItem1->item.asString.value + pItem1->item.asString.length,
                     pItem2->item.asString.value, pItem2->item.asString.length );
         pItem1->item.asString.length += pItem2->item.asString.length;
         pItem1->item.asString.value[ pItem1->item.asString.length ] = '\0';
         if( pItem2->item.asString.value )
         {
            hb_xfree( pItem2->item.asString.value );
            pItem2->item.asString.value = NULL;
         }
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+" );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      int iDec2, iDec1;
      double dNumber1 = hb_vmPopDouble( &iDec2 );
      double dNumber2 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumber( dNumber1 + dNumber2, ( iDec1 > iDec2 ) ? iDec1 : iDec2 );
   }

   else if( IS_DATE( pItem1 ) && IS_DATE( pItem2 ) )
   {
      long lDate1 = hb_vmPopDate();
      long lDate2 = hb_vmPopDate();

      hb_vmPushDate( lDate1 + lDate2 );
   }

   else if( IS_DATE( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      int iDec;
      double dNumber2 = hb_vmPopDouble( &iDec );
      long lDate1 = hb_vmPopDate();

      hb_vmPushDate( lDate1 + dNumber2 );
   }

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "+" ) )
      hb_vmOperatorCall( pItem1, pItem2, "+" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1081, NULL, "+" );

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
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmMinus()"));

   pItem2 = hb_stack.pPos - 1;
   pItem1 = hb_stack.pPos - 2;
   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      int iDec2, iDec1;
      double dNumber2 = hb_vmPopDouble( &iDec2 );
      double dNumber1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumber( dNumber1 - dNumber2, ( iDec1 > iDec2 ) ? iDec1 : iDec2 );
   }
   else if( IS_DATE( pItem2 ) && IS_DATE( pItem1 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();

      hb_vmPushLong( lDate1 - lDate2 );
   }
   else if( IS_NUMERIC( pItem2 ) && IS_DATE( pItem1 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      long lDate1 = hb_vmPopDate();

      hb_vmPushDate( lDate1 - dNumber2 );
   }
   else if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      if( ( double ) ( ( double ) pItem1->item.asString.length + ( double ) pItem2->item.asString.length ) < ( double ) ULONG_MAX )
      {
         ULONG ulLen = pItem1->item.asString.length;

         pItem1->item.asString.value = ( char * ) hb_xrealloc( pItem1->item.asString.value, pItem1->item.asString.length + pItem2->item.asString.length + 1 );
         pItem1->item.asString.length += pItem2->item.asString.length;

         while( ulLen && pItem1->item.asString.value[ ulLen - 1 ] == ' ' )
            ulLen--;

         hb_xmemcpy( pItem1->item.asString.value + ulLen, pItem2->item.asString.value, pItem2->item.asString.length );
         ulLen += pItem2->item.asString.length;
         hb_xmemset( pItem1->item.asString.value + ulLen, ' ', pItem1->item.asString.length - ulLen );
         pItem1->item.asString.value[ pItem1->item.asString.length ] = '\0';

         if( pItem2->item.asString.value )
         {
            hb_xfree( pItem2->item.asString.value );
            pItem2->item.asString.value = NULL;
         }
         hb_stackPop();
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-" );
   }
   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "-" ) )
      hb_vmOperatorCall( pItem1, pItem2, "-" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1082, NULL, "-" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMult()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      int iDec2, iDec1;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );

      hb_vmPushNumber( d1 * d2, iDec1 + iDec2 );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "*" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "*" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1083, NULL, "*" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDivide()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      BOOL bIntegerOperands = !IS_DOUBLE( hb_stack.pPos - 1 ) && !IS_DOUBLE( hb_stack.pPos - 2 );
      int iDec1, iDec2;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );

      if( d2 == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1340, NULL, "/" );

         if( pResult )
         {
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
      {
         /* If all both operand was integer and the result is an integer, too,
            push the number without decimals. Clipper compatible. */
         if( bIntegerOperands && fmod( d1, d2 ) == 0.0 )
            hb_vmPushNumber( d1 / d2, 0 );
         else
            hb_vmPushNumber( d1 / d2, hb_set.HB_SET_DECIMALS );
      }
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "/" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "/" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1084, NULL, "/" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModulus()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      int iDec1, iDec2;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );

      if( d2 == 0.0 )
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ZERODIV, 1341, NULL, "%" );

         if( pResult )
         {
            hb_vmPush( pResult );
            hb_itemRelease( pResult );
         }
      }
      else
         /* NOTE: Clipper always returns the result of modulus
                  with the SET number of decimal places. */
         hb_vmPushNumber( fmod( d1, d2 ), hb_set.HB_SET_DECIMALS );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "%" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "%" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1085, NULL, "%" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPower()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      int iDec1, iDec2;
      double d2 = hb_vmPopDouble( &iDec2 );
      double d1 = hb_vmPopDouble( &iDec1 );

      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_vmPushNumber( pow( d1, d2 ), hb_set.HB_SET_DECIMALS );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "^" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "^" );

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "**" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "**" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1088, NULL, "^" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmInc()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) )
   {
      int iDec;
      double dNumber = hb_vmPopDouble( &iDec );
      hb_vmPushNumber( ++dNumber, iDec );
   }
   else if( IS_DATE( hb_stack.pPos - 1 ) )
      hb_vmPushDate( hb_vmPopDate() + 1 );

   else if( IS_OBJECT( hb_stack.pPos - 1 ) && hb_objHasMsg( hb_stack.pPos - 1, "++" ) )
      hb_vmOperatorCallUnary( hb_stack.pPos - 1, "++" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1086, NULL, "++" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDec()"));

   if( IS_NUMERIC( hb_stack.pPos - 1 ) )
   {
      int iDec;
      double dNumber = hb_vmPopDouble( &iDec );
      hb_vmPushNumber( --dNumber, iDec );
   }
   else if( IS_DATE( hb_stack.pPos - 1 ) )
      hb_vmPushDate( hb_vmPopDate() - 1 );

   else if( IS_OBJECT( hb_stack.pPos - 1 ) && hb_objHasMsg( hb_stack.pPos - 1, "--" ) )
      hb_vmOperatorCallUnary( hb_stack.pPos - 1, "--" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1087, NULL, "--" );

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

   pItem = hb_stack.pPos - 1;
   if( IS_SYMBOL( pItem ) )
   {
      hb_stackPop();
      hb_vmPushLong( ( ULONG ) pItem->item.asSymbol.value->pFunPtr );
   }
   else
      hb_errInternal( 9999, "Symbol item expected from hb_vmFuncPtr()", NULL, NULL );
}

/* ------------------------------- */
/* Operators (relational)          */
/* ------------------------------- */

static void hb_vmEqual( BOOL bExact )
{
   PHB_ITEM pItem2;
   PHB_ITEM pItem1;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmEqual(%d)", (int) bExact));

   pItem2 = hb_stack.pPos - 1;
   pItem1 = hb_stack.pPos - 2;
   if( IS_NIL( pItem1 ) && IS_NIL( pItem2 ) )
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( TRUE );
   }

   else if( IS_NIL( pItem1 ) || IS_NIL( pItem2 ) )
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( FALSE );
   }

   else if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, bExact );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i == 0 );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      int iDec;
      hb_vmPushLogical( hb_vmPopDouble( &iDec ) == hb_vmPopDouble( &iDec ) );
   }

   else if( IS_DATE( pItem1 ) && IS_DATE( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDate() == hb_vmPopDate() );

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() == hb_vmPopLogical() );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "==" ) )
      hb_vmOperatorCall( pItem1, pItem2, "==" );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "=" ) )
      hb_vmOperatorCall( pItem1, pItem2, "=" );

   else if( bExact && IS_ARRAY( pItem1 ) && IS_ARRAY( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asArray.value->pItems && pItem2->item.asArray.value->pItems &&
                     pItem1->item.asArray.value->pItems == pItem2->item.asArray.value->pItems;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else if( pItem1->type != pItem2->type ||
            ( IS_BLOCK( pItem1 ) && IS_BLOCK( pItem2 ) ) ||
            ( ! bExact && IS_ARRAY( pItem1 ) && IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult;

      if( bExact )
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1070, NULL, "==" );
      else
         pResult = hb_errRT_BASE_Subst( EG_ARG, 1071, NULL, "=" );

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

   pItem2 = hb_stack.pPos - 1;
   pItem1 = hb_stack.pPos - 2;
   if( IS_NIL( pItem1 ) && IS_NIL( pItem2 ) )
   {
      hb_stackDec();
      hb_stackDec();
      hb_vmPushLogical( FALSE );
   }

   else if( IS_NIL( pItem1 ) || IS_NIL( pItem2 ) )
   {
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( TRUE );
   }

   else if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      int i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i != 0 );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      int iDec;
      hb_vmPushLogical( hb_vmPopDouble( &iDec ) != hb_vmPopDouble( &iDec ) );
   }

   else if( IS_DATE( pItem1 ) && IS_DATE( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDate() != hb_vmPopDate() );

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() != hb_vmPopLogical() );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "!=" ) )
      hb_vmOperatorCall( pItem1, pItem2, "!=" );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "<>" ) )
      hb_vmOperatorCall( pItem1, pItem2, "<>" );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "#" ) )
      hb_vmOperatorCall( pItem1, pItem2, "#" );

   else if( pItem1->type != pItem2->type ||
            ( IS_BLOCK( pItem1 ) && IS_BLOCK( pItem2 ) ) ||
            ( IS_ARRAY( pItem1 ) && IS_ARRAY( pItem2 ) ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1072, NULL, "<>" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLess()"));

   if( IS_STRING( hb_stack.pPos - 2 ) && IS_STRING( hb_stack.pPos - 1 ) )
   {
      int i = hb_itemStrCmp( hb_stack.pPos - 2, hb_stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i < 0 );
   }

   else if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 < dNumber2 );
   }

   else if( IS_DATE( hb_stack.pPos - 1 ) && IS_DATE( hb_stack.pPos - 2 ) )
   {
      LONG lDate2 = hb_vmPopDate();
      LONG lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 < lDate2 );
   }

   else if( IS_LOGICAL( hb_stack.pPos - 1 ) && IS_LOGICAL( hb_stack.pPos - 2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 < bLogical2 );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "<" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "<" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLessEqual()"));

   if( IS_STRING( hb_stack.pPos - 2 ) && IS_STRING( hb_stack.pPos - 1 ) )
   {
      int i = hb_itemStrCmp( hb_stack.pPos - 2, hb_stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i <= 0 );
   }

   else if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 <= dNumber2 );
   }

   else if( IS_DATE( hb_stack.pPos - 1 ) && IS_DATE( hb_stack.pPos - 2 ) )
   {
      LONG lDate2 = hb_vmPopDate();
      LONG lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 <= lDate2 );
   }

   else if( IS_LOGICAL( hb_stack.pPos - 1 ) && IS_LOGICAL( hb_stack.pPos - 2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 <= bLogical2 );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, "<=" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, "<=" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1074, NULL, "<=" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreater()"));

   if( IS_STRING( hb_stack.pPos - 2 ) && IS_STRING( hb_stack.pPos - 1 ) )
   {
      int i = hb_itemStrCmp( hb_stack.pPos - 2, hb_stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i > 0 );
   }

   else if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 > dNumber2 );
   }

   else if( IS_DATE( hb_stack.pPos - 1 ) && IS_DATE( hb_stack.pPos - 2 ) )
   {
      LONG lDate2 = hb_vmPopDate();
      LONG lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 > lDate2 );
   }

   else if( IS_LOGICAL( hb_stack.pPos - 1 ) && IS_LOGICAL( hb_stack.pPos - 2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 > bLogical2 );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, ">" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, ">" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1075, NULL, ">" );

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
   HB_TRACE(HB_TR_DEBUG, ("hb_vmGreaterEqual()"));

   if( IS_STRING( hb_stack.pPos - 2 ) && IS_STRING( hb_stack.pPos - 1 ) )
   {
      int i = hb_itemStrCmp( hb_stack.pPos - 2, hb_stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i >= 0 );
   }

   else if( IS_NUMERIC( hb_stack.pPos - 1 ) && IS_NUMERIC( hb_stack.pPos - 2 ) )
   {
      double dNumber2 = hb_vmPopNumber();
      double dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 >= dNumber2 );
   }

   else if( IS_DATE( hb_stack.pPos - 1 ) && IS_DATE( hb_stack.pPos - 2 ) )
   {
      LONG lDate2 = hb_vmPopDate();
      LONG lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 >= lDate2 );
   }

   else if( IS_LOGICAL( hb_stack.pPos - 1 ) && IS_LOGICAL( hb_stack.pPos - 2 ) )
   {
      BOOL bLogical2 = hb_vmPopLogical();
      BOOL bLogical1 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 >= bLogical2 );
   }

   else if( IS_OBJECT( hb_stack.pPos - 2 ) && hb_objHasMsg( hb_stack.pPos - 2, ">=" ) )
      hb_vmOperatorCall( hb_stack.pPos - 2, hb_stack.pPos - 1, ">=" );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1076, NULL, ">=" );

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

   pItem1 = hb_stack.pPos - 2;
   pItem2 = hb_stack.pPos - 1;
   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      int iResult = hb_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                              pItem2->item.asString.value, pItem2->item.asString.length );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( iResult == 0 ? FALSE : TRUE );
   }
   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "$" ) )
      hb_vmOperatorCall( pItem1, pItem2, "$" );
   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1109, NULL, "$" );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }
   }
}

static void hb_vmForTest( void )        /* Test to check the end point of the FOR */
{
   int iDec;
   double dStep;
   BOOL bEqual;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmForTest()"));

   while( ! IS_NUMERIC( hb_stack.pPos - 1 ) )
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1073, NULL, "<" );

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

   dStep = hb_vmPopDouble( &iDec );

   /* NOTE: step of zero will cause endless loop, as in Clipper */

   if( dStep > 0 )           /* Positive loop. Use LESS */
      hb_vmLess();
   else if( dStep < 0 )      /* Negative loop. Use GREATER */
      hb_vmGreater();

   bEqual = hb_vmPopLogical();    /* Logical should be on top of stack */
   hb_vmPushNumber( dStep, iDec );   /* Push the step expression back on the stack */
   hb_vmPushLogical( bEqual );
}

/* ------------------------------- */
/* Operators (logical)             */
/* ------------------------------- */

static void hb_vmNot( void )
{
   PHB_ITEM pItem;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmNot()"));

   pItem = hb_stack.pPos - 1;
   if( IS_LOGICAL( pItem ) )
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;

   else if( IS_OBJECT( pItem ) && hb_objHasMsg( pItem, "!" ) )
      hb_vmOperatorCallUnary( pItem, "!" );

   else if( IS_OBJECT( pItem ) && hb_objHasMsg( pItem, ".NOT." ) )
      hb_vmOperatorCallUnary( pItem, ".NOT." );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1077, NULL, ".NOT." );

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

   pItem2 = hb_stack.pPos - 1;
   pItem1 = hb_stack.pPos - 2;
   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, ".AND." ) )
      hb_vmOperatorCall( pItem1, pItem2, ".AND." );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1078, NULL, ".AND." );

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

   pItem2 = hb_stack.pPos - 1;
   pItem1 = hb_stack.pPos - 2;
   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
      hb_stackDec();
      hb_stackDec();
      hb_vmPushLogical( bResult );
   }

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, ".OR." ) )
      hb_vmOperatorCall( pItem1, pItem2, ".OR." );

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1079, NULL, ".OR." );

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

static void hb_vmArrayAt( void )
{
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   ULONG ulIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayAt()"));

   pIndex = hb_stack.pPos - 1;
   pArray = hb_stack.pPos - 2;
   if( IS_INTEGER( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asInteger.value;

   else if( IS_LONG( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asLong.value;

   else if( IS_DOUBLE( pIndex ) )
      ulIndex = ( ULONG ) pIndex->item.asDouble.value;

   else
   {
      PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ) );

      if( pResult )
      {
         hb_stackPop();
         hb_stackPop();
         hb_vmPush( pResult );
         hb_itemRelease( pResult );
      }

      return;
   }

   if( ! hb_arrayError( pArray, ulIndex, FALSE ) )
   {
      HB_ITEM item;

      hb_arrayGet( pArray, ulIndex, &item );
      hb_stackPop();
      hb_stackPop();

      hb_itemCopy( hb_stack.pPos, &item );
      hb_itemClear( &item );
      hb_stackPush();
   }
}

static void hb_vmArrayPut( void )
{
   PHB_ITEM pValue;
   PHB_ITEM pIndex;
   PHB_ITEM pArray;
   ULONG ulIndex;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayPut()"));

   pValue = hb_stack.pPos - 1;
   pIndex = hb_stack.pPos - 2;
   pArray = hb_stack.pPos - 3;

   if( IS_INTEGER( pIndex ) )
      ulIndex = pIndex->item.asInteger.value;

   else if( IS_LONG( pIndex ) )
      ulIndex = pIndex->item.asLong.value;

   else if( IS_DOUBLE( pIndex ) )
      ulIndex = pIndex->item.asDouble.value;

   else
   {
      hb_errRT_BASE( EG_ARG, 1069, NULL, hb_langDGetErrorDesc( EG_ARRASSIGN ) );
      return;
   }

   if( ! hb_arrayError( pArray, ulIndex, TRUE ) )
   {
      hb_arraySet( pArray, ulIndex, pValue );
      hb_itemCopy( pArray, pValue );  /* places pValue at pArray position */
      hb_stackPop();
      hb_stackPop();
   }
}

static void hb_vmArrayDim( USHORT uiDimensions ) /* generates an uiDimensions Array and initialize those dimensions from the stack values */
{
   HB_ITEM itArray;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayDim(%hu)", uiDimensions));

   itArray.type = IT_NIL;
   hb_vmArrayNew( &itArray, uiDimensions );

   while( uiDimensions-- )
      hb_stackPop();

   hb_itemCopy( hb_stack.pPos, &itArray );
   hb_itemClear( &itArray );
   hb_stackPush();
}

static void hb_vmArrayGen( ULONG ulElements ) /* generates an ulElements Array and fills it from the stack values */
{
   HB_ITEM itArray;
   ULONG ulPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmArrayGen(%lu)", ulElements));

   itArray.type = IT_NIL;
   hb_arrayNew( &itArray, ulElements );
   for( ulPos = 0; ulPos < ulElements; ulPos++ )
      hb_itemCopy( itArray.item.asArray.value->pItems + ulPos, hb_stack.pPos - ulElements + ulPos );

   for( ulPos = 0; ulPos < ulElements; ulPos++ )
      hb_stackPop();

   hb_itemCopy( hb_stack.pPos, &itArray );
   hb_itemClear( &itArray );
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

   pDim = hb_stack.pPos - uiDimension;

   /* use the proper type of number of elements */
   switch( pDim->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         ulElements = ( ULONG ) pDim->item.asInteger.value;
         break;

      case IT_LONG:
         ulElements = pDim->item.asLong.value;
         break;

      case IT_DOUBLE:
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

void hb_vmMessage( PHB_SYMB pSymMsg ) /* sends a message to an object */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmMessage(%p, %s)", pSymMsg, pSymMsg->szName));

   hb_itemCopy( hb_stack.pPos, hb_stack.pPos - 1 ); /* moves the object forward */
   hb_itemClear( hb_stack.pPos - 1 );
   ( hb_stack.pPos - 1 )->type = IT_SYMBOL;
   ( hb_stack.pPos - 1 )->item.asSymbol.value = pSymMsg;
   ( hb_stack.pPos - 1 )->item.asSymbol.stackbase = ( hb_stack.pPos - 1 ) - hb_stack.pItems;
   hb_stackPush();
}

static void hb_vmOperatorCall( PHB_ITEM pObjItem, PHB_ITEM pMsgItem, char * szSymbol )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if IS_OBJECT() && HAS_METHOD() )
    */
   HB_ITEM ItemMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCall(%p, %p, %s)", pObjItem, pMsgItem, szSymbol));

   ItemMsg.type = IT_SYMBOL;
   ItemMsg.item.asSymbol.value = hb_dynsymFind( szSymbol )->pSymbol;
   ItemMsg.item.asSymbol.stackbase = hb_stack.pPos - hb_stack.pItems;

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
   hb_itemClear( pObjItem );
   hb_itemCopy( pObjItem, &hb_stack.Return );
}

static void hb_vmOperatorCallUnary( PHB_ITEM pObjItem, char * szSymbol )
{
   /* NOTE: There is no need to test if specified symbol exists. It is checked
    * by the caller (if IS_OBJECT() && HAS_METHOD() )
    */
   HB_ITEM ItemMsg;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmOperatorCallUnary(%p, %s)", pObjItem, szSymbol));

   ItemMsg.type = IT_SYMBOL;
   ItemMsg.item.asSymbol.value = hb_dynsymFind( szSymbol )->pSymbol;
   ItemMsg.item.asSymbol.stackbase = hb_stack.pPos - hb_stack.pItems;

   hb_itemClear( &hb_stack.Return );       /* clear return value */
   hb_vmPush( &ItemMsg );
   hb_vmPush( pObjItem );                             /* Push object */
   hb_vmDo( 0 );

   /* Pop passed argument.
    * NOTE: for performance reason we don't pop it and we don't push the
    * return value. We can replace the last element with the new value.
    */
   hb_itemClear( pObjItem );
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
   switch( pAlias->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         /* Alias was used as integer value, for example: 4->field
          * or it was saved on the stack using hb_vmPushAlias()
          * or was evaluated from an expression, (nWorkArea)->field
          */
         hb_rddSelectWorkAreaNumber( pAlias->item.asInteger.value );
         pAlias->type = IT_NIL;
         break;

      case IT_LONG:
         /* Alias was evaluated from an expression, (nWorkArea)->field
          */
         hb_rddSelectWorkAreaNumber( pAlias->item.asLong.value );
         pAlias->type = IT_NIL;
         break;

      case IT_DOUBLE:
         /* Alias was evaluated from an expression, (nWorkArea)->field
          */
         hb_rddSelectWorkAreaNumber( pAlias->item.asDouble.value );
         pAlias->type = IT_NIL;
         break;

      case IT_SYMBOL:
         /* Alias was specified using alias identifier, for example: al->field
          */
         bSuccess = hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
         pAlias->type = IT_NIL;
         break;

      case IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         bSuccess = hb_rddSelectWorkAreaAlias( pAlias->item.asString.value );
         hb_itemClear( pAlias );
         break;

      default:
         {
            PHB_ITEM pSubstVal = hb_errRT_BASE_Subst( EG_ARG, 1065, NULL, "&" );
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

   pItem = hb_stack.pPos - 1;
   pWorkArea = hb_stack.pPos - 2;
   hb_vmSelectWorkarea( pWorkArea );

   memcpy( pWorkArea, pItem, sizeof( HB_ITEM ) );
   pItem->type = IT_NIL;
   hb_stackDec();
}

/* ------------------------------- */
/* Execution                       */
/* ------------------------------- */

void hb_vmDo( USHORT uiParams )
{
   PHB_ITEM pItem;
   PHB_SYMB pSym;
   LONG wStackBase;
   LONG wItemIndex;
   PHB_ITEM pSelf;
   PHB_BASEARRAY pSelfBase;
   PHB_FUNC pFunc;
   int iStatics;
   BOOL bDebugPrevState;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDo(%hu)", uiParams));

   pItem = hb_stack.pPos - uiParams - 2;   /* procedure name */
   pSym = pItem->item.asSymbol.value;
   wStackBase = hb_stack.pBase - hb_stack.pItems; /* as the stack memory block could change */
   wItemIndex = pItem - hb_stack.pItems;
   pSelf = hb_stack.pPos - uiParams - 1;   /* NIL, OBJECT or BLOCK */
   iStatics = hb_stack.iStatics;              /* Return iStatics position */
   bDebugPrevState = s_bDebugging;
   s_bDebugging = FALSE;

   if( ! IS_SYMBOL( pItem ) )
   {
      /* QUESTION: Is this call needed ? [vszel] */
      hb_stackDispLocal();
      hb_errInternal( 9999, "Symbol item expected as a base from hb_vmDo()", NULL, NULL );
   }

#if 0
   if( ! IS_NIL( pSelf ) )
   {
      /* QUESTION: Is this call needed ? [vszel] */
      hb_stackDispLocal();
      hb_errInternal( 9999, "Invalid symbol type for self from hb_vmDo()", NULL, NULL );
   }
#endif

   pItem->item.asSymbol.lineno = 0;
   pItem->item.asSymbol.paramcnt = uiParams;
   hb_stack.pBase = hb_stack.pItems + pItem->item.asSymbol.stackbase;
   pItem->item.asSymbol.stackbase = wStackBase;

   if( ! IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      if( pSym == &( hb_symEval ) && IS_BLOCK( pSelf ) )
         pFunc = pSym->pFunPtr;                 /* __EVAL method = function */
      else
      {
         pFunc = hb_objGetMethod( pSelf, pSym );
         if( IS_OBJECT( pSelf ) )               /* Object passed            */
         {
            pSelfBase = pSelf->item.asArray.value;
            if( pSelfBase->uiPrevCls ) /* Is is a Super cast ? */
            {
              pSelfBase->uiClass   = pSelfBase->uiPrevCls;
              pSelfBase->uiPrevCls = NULL;
            }
         }
      }

      if( pFunc )
         pFunc();
      else
      {
         PHB_ITEM pResult;

         if( pSym->szName[ 0 ] == '_' )
            pResult = hb_errRT_BASE_Subst( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1 );
         else
            pResult = hb_errRT_BASE_Subst( EG_NOMETHOD, 1004, NULL, pSym->szName );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
   else                     /* it is a function */
   {
      pFunc = pSym->pFunPtr;

      if( pFunc )
         pFunc();
      else
         hb_errInternal( 9999, "Invalid function pointer (%s) from hb_vmDo()", pSym->szName, NULL );
   }

   while( hb_stack.pPos > hb_stack.pItems + wItemIndex )
      hb_stackPop();

   hb_stack.pBase = hb_stack.pItems + wStackBase;
   hb_stack.iStatics = iStatics;

   if( s_bDebugging )
      hb_vmDebuggerEndProc();

   s_bDebugging = bDebugPrevState;
}

static HARBOUR hb_vmDoBlock( void )
{
   PHB_ITEM pBlock = hb_stack.pBase + 1;
   USHORT uiStackBase = hb_stack.pBase - hb_stack.pItems; /* as the stack memory block could change */
   USHORT uiLine;
   int iParam;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoBlock()"));

   pBlock = hb_stack.pBase + 1;
   uiStackBase = hb_stack.pBase - hb_stack.pItems; /* as the stack memory block could change */

   if( ! IS_BLOCK( pBlock ) )
      hb_errInternal( 9999, "Codeblock expected from hb_vmDoBlock()", NULL, NULL );

   /* Check for valid count of parameters */
   iParam = pBlock->item.asBlock.paramcnt - hb_pcount();
   /* add missing parameters */
   while( iParam-- > 0 )
      hb_vmPushNil();

   /* set the current line number to a line where the codeblock was defined
    */
   uiLine = hb_stack.pBase->item.asSymbol.lineno;
   hb_stack.pBase->item.asSymbol.lineno = pBlock->item.asBlock.lineno;

   hb_codeblockEvaluate( pBlock );

   /* restore stack pointers */
   hb_stack.pBase = hb_stack.pItems + uiStackBase;
   hb_stack.pBase->item.asSymbol.lineno = uiLine;
}

void hb_vmFunction( USHORT uiParams )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmFunction(%hu)", uiParams));

   hb_itemClear( &hb_stack.Return );
   hb_vmDo( uiParams );
   hb_itemCopy( hb_stack.pPos, &hb_stack.Return );
   hb_stackPush();
}

static void hb_vmLocalName( USHORT uiLocal, char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmLocalName(%hu, %s)", uiLocal, szLocalName));

   HB_SYMBOL_UNUSED( uiLocal );
   HB_SYMBOL_UNUSED( szLocalName );
}

static void hb_vmModuleName( char * szModuleName ) /* PRG and function name information for the debugger */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmModuleName(%s)", szModuleName));

   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushString( szModuleName, strlen( szModuleName ) );
   hb_vmDo( 1 );
   s_bDebugShowLines = TRUE;
}

static void hb_vmFrame( BYTE bLocals, BYTE bParams )
{
   int iTotal;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmFrame(%d, %d)", (int) bLocals, (int) bParams));

   iTotal = bLocals + bParams;
   if( iTotal )
   {
      int i;

      for( i = 0; i < ( iTotal - hb_stack.pBase->item.asSymbol.paramcnt ); i++ )
         hb_vmPushNil();
   }
}

static void hb_vmSFrame( PHB_SYMB pSym )      /* sets the statics frame for a function */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmSFrame(%p)", pSym));

   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   hb_stack.iStatics = ( int ) pSym->pFunPtr; /* pSym is { "$_INITSTATICS", FS_INIT | FS_EXIT, _INITSTATICS } for each PRG */
}

static void hb_vmStatics( PHB_SYMB pSym, USHORT uiStatics ) /* initializes the global aStatics array or redimensionates it */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmStatics(%p, %hu)", pSym, uiStatics));

   if( IS_NIL( &s_aStatics ) )
   {
      pSym->pFunPtr = NULL;         /* statics frame for this PRG */
      hb_arrayNew( &s_aStatics, uiStatics );
   }
   else
   {
      pSym->pFunPtr = ( PHB_FUNC ) hb_arrayLen( &s_aStatics );
      hb_arraySize( &s_aStatics, hb_arrayLen( &s_aStatics ) + uiStatics );
   }
}

static void hb_vmEndBlock( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmEndBlock()"));

   hb_stackDec();                               /* make the last item visible */
   hb_itemCopy( &hb_stack.Return, hb_stack.pPos ); /* copy it */
   hb_itemClear( hb_stack.pPos );               /* and now clear it */
}

static void hb_vmRetValue( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRetValue()"));

   hb_stackDec();                               /* make the last item visible */
   hb_itemCopy( &hb_stack.Return, hb_stack.pPos ); /* copy it */
   hb_itemClear( hb_stack.pPos );               /* now clear it */
}

static void hb_vmDebuggerEndProc( void )
{
   HB_ITEM item;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerEndProc()"));

   hb_itemCopy( &item, &hb_stack.Return ); /* saves the previous returned value */

   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );
   s_bDebugShowLines = TRUE;

   hb_itemCopy( &hb_stack.Return, &item ); /* restores the previous returned value */
   hb_itemClear( &item );
}

static void hb_vmDebuggerShowLine( USHORT uiLine ) /* makes the debugger shows a specific source code line */
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDebuggerShowLine(%hu)", uiLine));

   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushInteger( uiLine );
   hb_vmDo( 1 );
   s_bDebugShowLines = TRUE;
}

/* ------------------------------- */
/* Push                            */
/* ------------------------------- */

void hb_vmPush( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPush(%p)", pItem));

   hb_itemCopy( hb_stack.pPos, pItem );
   hb_stackPush();
}

void hb_vmPushNil( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNil()"));

   hb_stack.pPos->type = IT_NIL;
   hb_stackPush();
}

void hb_vmPushLogical( BOOL bValue )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLogical(%d)", (int) bValue));

   hb_stack.pPos->type = IT_LOGICAL;
   hb_stack.pPos->item.asLogical.value = bValue;
   hb_stackPush();
}

void hb_vmPushNumber( double dNumber, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushNumber(%lf, %d)", dNumber, iDec));

   if( iDec )
      hb_vmPushDouble( dNumber, iDec );

   else if( SHRT_MIN <= dNumber && dNumber <= SHRT_MAX )
      hb_vmPushInteger( dNumber );

   else if( LONG_MIN <= dNumber && dNumber <= LONG_MAX )
      hb_vmPushLong( dNumber );

   else
      hb_vmPushDouble( dNumber, hb_set.HB_SET_DECIMALS );
}

void hb_vmPushInteger( int iNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushInteger(%d)", iNumber));

   hb_stack.pPos->type = IT_INTEGER;
   hb_stack.pPos->item.asInteger.value = iNumber;
   hb_stack.pPos->item.asInteger.length = 10;
   hb_stackPush();
}

void hb_vmPushLong( long lNumber )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLong(%ld)", lNumber));

   hb_stack.pPos->type = IT_LONG;
   hb_stack.pPos->item.asLong.value = lNumber;
   hb_stack.pPos->item.asLong.length = 10;
   hb_stackPush();
}

void hb_vmPushDouble( double dNumber, int iDec )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDouble(%lf, %d)", dNumber, iDec));

   hb_stack.pPos->type = IT_DOUBLE;
   hb_stack.pPos->item.asDouble.value = dNumber;
   hb_stack.pPos->item.asDouble.length = ( dNumber > 10000000000.0 ) ? 20 : 10;
   hb_stack.pPos->item.asDouble.decimal = ( iDec > 9 ) ? 9 : iDec;
   hb_stackPush();
}

void hb_vmPushDate( LONG lDate )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushDate(%ld)", lDate));

   hb_stack.pPos->type = IT_DATE;
   hb_stack.pPos->item.asDate.value = lDate;
   hb_stackPush();
}

void hb_vmPushString( char * szText, ULONG length )
{
   char * szTemp;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushString(%s, %lu)", szText, length));

   szTemp = ( char * ) hb_xgrab( length + 1 );
   hb_xmemcpy( szTemp, szText, length );
   szTemp[ length ] = '\0';

   hb_stack.pPos->type = IT_STRING;
   hb_stack.pPos->item.asString.length = length;
   hb_stack.pPos->item.asString.value = szTemp;
   hb_stackPush();
}

void hb_vmPushSymbol( PHB_SYMB pSym )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushSymbol(%p)", pSym));

   hb_stack.pPos->type = IT_SYMBOL;
   hb_stack.pPos->item.asSymbol.value = pSym;
   hb_stack.pPos->item.asSymbol.stackbase = hb_stack.pPos - hb_stack.pItems;
   hb_stackPush();
}

/* +0    -> HB_P_PUSHBLOCK
 * +1 +2 -> size of codeblock
 * +3 +4 -> number of expected parameters
 * +5 +6 -> number of referenced local variables
 * +7    -> start of table with referenced local variables
 */
static void hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols )
{
   USHORT uiLocals;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushBlock(%p, %p)", pCode, pSymbols));

   hb_stack.pPos->type = IT_BLOCK;

   uiLocals = pCode[ 5 ] + ( pCode[ 6 ] * 256 );
   hb_stack.pPos->item.asBlock.value =
         hb_codeblockNew( pCode + 7 + uiLocals * 2, /* pcode buffer         */
         uiLocals,                                  /* number of referenced local variables */
         ( USHORT * ) ( pCode + 7 ),                /* table with referenced local variables */
         pSymbols );

   /* store the statics base of function where the codeblock was defined
    */
   hb_stack.pPos->item.asBlock.statics = hb_stack.iStatics;
   /* store the number of expected parameters
    */
   hb_stack.pPos->item.asBlock.paramcnt = pCode[ 3 ] + ( pCode[ 4 ] * 256 );
   /* store the line number where the codeblock was defined
    */
   hb_stack.pPos->item.asBlock.lineno = hb_stack.pBase->item.asSymbol.lineno;
   hb_stackPush();
}

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushAlias()"));

   hb_stack.pPos->type = IT_INTEGER;
   hb_stack.pPos->item.asInteger.value = hb_rddGetCurrentWorkAreaNumber();
   hb_stack.pPos->item.asInteger.length = 10;
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

   pAlias = hb_stack.pPos - 1;
   iCurrArea = hb_rddGetCurrentWorkAreaNumber();

   /* NOTE: hb_vmSelecWorkarea clears passed item
    */
   if( hb_vmSelectWorkarea( pAlias ) == SUCCESS )
      hb_rddGetFieldValue( pAlias, pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
}

static void hb_vmPushLocal( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocal(%hd)", iLocal));

   if( iLocal >= 0 )
   {
      PHB_ITEM pLocal;

      /* local variable or local parameter */
      pLocal = hb_stack.pBase + 1 + iLocal;
      if( IS_BYREF( pLocal ) )
         hb_itemCopy( hb_stack.pPos, hb_itemUnRef( pLocal ) );
      else
         hb_itemCopy( hb_stack.pPos, pLocal );
   }
   else
      /* local variable referenced in a codeblock
       * hb_stack.pBase+1 points to a codeblock that is currently evaluated
       */
      hb_itemCopy( hb_stack.pPos, hb_codeblockGetVar( hb_stack.pBase + 1, ( LONG ) iLocal ) );

   hb_stackPush();
}

static void hb_vmPushLocalByRef( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushLocalByRef(%hd)", iLocal));

   hb_stack.pPos->type = IT_BYREF;
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   hb_stack.pPos->item.asRefer.value = iLocal;
   hb_stack.pPos->item.asRefer.offset = hb_stack.pBase - hb_stack.pItems +1;
   hb_stack.pPos->item.asRefer.itemsbase = &hb_stack.pItems;
   hb_stackPush();
}

static void hb_vmPushStatic( USHORT uiStatic )
{
   PHB_ITEM pStatic;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStatic(%hu)", uiStatic));

   pStatic = s_aStatics.item.asArray.value->pItems + hb_stack.iStatics + uiStatic - 1;
   if( IS_BYREF( pStatic ) )
      hb_itemCopy( hb_stack.pPos, hb_itemUnRef( pStatic ) );
   else
      hb_itemCopy( hb_stack.pPos, pStatic );
   hb_stackPush();
}

static void hb_vmPushStaticByRef( USHORT uiStatic )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPushStaticByRef(%hu)", uiStatic));

   hb_stack.pPos->type = IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   hb_stack.pPos->item.asRefer.value = uiStatic - 1;
   hb_stack.pPos->item.asRefer.offset = hb_stack.iStatics;
   hb_stack.pPos->item.asRefer.itemsbase = &s_aStatics.item.asArray.value->pItems;
   hb_stackPush();
}

static void hb_vmDuplicate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplicate()"));

   hb_itemCopy( hb_stack.pPos, hb_stack.pPos - 1 );
   hb_stackPush();
}

static void hb_vmDuplTwo( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmDuplTwo()"));

   hb_itemCopy( hb_stack.pPos, hb_stack.pPos - 2 );
   hb_stackPush();
   hb_itemCopy( hb_stack.pPos, hb_stack.pPos - 2 );
   hb_stackPush();
}

/* ------------------------------- */
/* Pop                             */
/* ------------------------------- */

static BOOL hb_vmPopLogical( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLogical()"));

   if( IS_LOGICAL( hb_stack.pPos - 1 ) )
   {
      hb_stackDec();

      hb_stack.pPos->type = IT_NIL;
      return hb_stack.pPos->item.asLogical.value;
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ) );
      return FALSE;
   }
}

/* NOTE: Type checking should be done by the caller. */

static long hb_vmPopDate( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopDate()"));

   hb_stackDec();

   hb_stack.pPos->type = IT_NIL;
   return hb_stack.pPos->item.asDate.value;
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopNumber( void )
{
   PHB_ITEM pItem;
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopNumber()"));

   pItem = hb_stack.pPos - 1;
   hb_stackDec();

   switch( pItem->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         dNumber = ( double ) pItem->item.asInteger.value;
         break;

      case IT_LONG:
         dNumber = ( double ) pItem->item.asLong.value;
         break;

      case IT_DOUBLE:
         dNumber = pItem->item.asDouble.value;
         break;

      default:
         hb_errInternal( 9999, "Incorrect item on the stack trying to pop a number", NULL, NULL );
         break;
   }

   hb_stack.pPos->type = IT_NIL;

   return dNumber;
}

/* NOTE: Type checking should be done by the caller. */

static double hb_vmPopDouble( int * piDec )
{
   double dNumber;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopDouble(%p)", piDec));

   hb_stackDec();

   switch( hb_stack.pPos->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         dNumber = ( double ) hb_stack.pPos->item.asInteger.value;
         *piDec = 0;
         break;

      case IT_LONG:
         dNumber = ( double ) hb_stack.pPos->item.asLong.value;
         *piDec = 0;
         break;

      case IT_DOUBLE:
         dNumber = hb_stack.pPos->item.asDouble.value;
         *piDec = hb_stack.pPos->item.asDouble.decimal;
         break;

      default:
         hb_errInternal( 9999, "Incorrect item type trying to Pop a double", NULL, NULL );
         break;
   }

   hb_stack.pPos->type = IT_NIL;

   return dNumber;
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAlias()"));

   hb_stackDec();
   hb_vmSelectWorkarea( hb_stack.pPos );
}

/* Pops the alias to use it to select a workarea and next pops a value
 * into given field
 */
static void hb_vmPopAliasedField( PHB_SYMB pSym )
{
   int iCurrArea;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopAliasedField(%p)", pSym));

   iCurrArea = hb_rddGetCurrentWorkAreaNumber();
   if( hb_vmSelectWorkarea( hb_stack.pPos - 1 ) == SUCCESS )
      hb_rddPutFieldValue( hb_stack.pPos - 2, pSym );

   hb_rddSelectWorkAreaNumber( iCurrArea );
   hb_stackDec();    /* alias - it was cleared in hb_vmSelectWorkarea */
   hb_stackPop();    /* field value */
}

static void hb_vmPopLocal( SHORT iLocal )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopLocal(%hd)", iLocal));

   hb_stackDec();

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      PHB_ITEM pLocal = hb_stack.pBase + 1 + iLocal;

      if( IS_BYREF( pLocal ) )
         hb_itemCopy( hb_itemUnRef( pLocal ), hb_stack.pPos );
      else
         hb_itemCopy( pLocal, hb_stack.pPos );
   }
   else
      /* local variable referenced in a codeblock
       * hb_stack.pBase+1 points to a codeblock that is currently evaluated
       */
      hb_itemCopy( hb_codeblockGetVar( hb_stack.pBase + 1, iLocal ), hb_stack.pPos );

   hb_itemClear( hb_stack.pPos );
}

static void hb_vmPopStatic( USHORT uiStatic )
{
   PHB_ITEM pStatic;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmPopStatic(%hu)", uiStatic));

   hb_stackDec();
   pStatic = s_aStatics.item.asArray.value->pItems + hb_stack.iStatics + uiStatic - 1;

   if( IS_BYREF( pStatic ) )
      hb_itemCopy( hb_itemUnRef( pStatic ), hb_stack.pPos );
   else
      hb_itemCopy( pStatic, hb_stack.pPos );

   hb_itemClear( hb_stack.pPos );
}

/* ------------------------------- */
/* stack management functions      */
/* ------------------------------- */

static void hb_stackPop( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackPop()"));

   if( --hb_stack.pPos < hb_stack.pItems )
      hb_errInternal( 9999, "Stack underflow", NULL, NULL );

   if( hb_stack.pPos->type != IT_NIL )
      hb_itemClear( hb_stack.pPos );
}

static void hb_stackDec( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackDec()"));

   if( --hb_stack.pPos < hb_stack.pItems )
      hb_errInternal( 9999, "Stack underflow", NULL, NULL );
}

static void hb_stackFree( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackFree()"));

   hb_xfree( hb_stack.pItems );
}

static void hb_stackPush( void )
{
   LONG CurrIndex;   /* index of current top item */
   LONG TopIndex;    /* index of the topmost possible item */

   HB_TRACE(HB_TR_DEBUG, ("hb_stackPush()"));

   CurrIndex = hb_stack.pPos - hb_stack.pItems;
   TopIndex  = hb_stack.wItems - 1;

   /* enough room for another item ? */
   if( !( TopIndex > CurrIndex ) )
   {
      LONG BaseIndex;   /* index of stack base */

      BaseIndex = hb_stack.pBase - hb_stack.pItems;

      /* no, make more headroom: */
      /* hb_stackDispLocal(); */
      hb_stack.pItems = ( PHB_ITEM ) hb_xrealloc( hb_stack.pItems, sizeof( HB_ITEM ) *
                                ( hb_stack.wItems + STACK_EXPANDHB_ITEMS ) );

      /* fix possibly invalid pointers: */
      hb_stack.pPos = hb_stack.pItems + CurrIndex;
      hb_stack.pBase = hb_stack.pItems + BaseIndex;
      hb_stack.wItems += STACK_EXPANDHB_ITEMS;
      /* hb_stackDispLocal(); */
   }

   /* now, push it: */
   hb_stack.pPos++;
   hb_stack.pPos->type = IT_NIL;
}

static void hb_stackInit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_stackInit()"));

   hb_stack.pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * STACK_INITHB_ITEMS );
   hb_stack.pBase  = hb_stack.pItems;
   hb_stack.pPos   = hb_stack.pItems;     /* points to the first stack item */
   hb_stack.wItems = STACK_INITHB_ITEMS;
}

/* NOTE: DEBUG function */
static void hb_stackDispLocal( void )
{
   PHB_ITEM pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispLocal()"));

   printf( hb_consoleGetNewLine() );
   printf( "Virtual Machine Stack Dump at %s(%i):", hb_stack.pBase->item.asSymbol.value->szName, hb_stack.pBase->item.asSymbol.lineno );
   printf( hb_consoleGetNewLine() );
   printf( "--------------------------" );

   for( pBase = hb_stack.pBase; pBase <= hb_stack.pPos; pBase++ )
   {
      printf( hb_consoleGetNewLine() );

      switch( hb_itemType( pBase ) )
      {
         case IT_NIL:
            printf( "NIL " );
            break;

         case IT_ARRAY:
            if( hb_arrayIsObject( pBase ) )
               printf( "OBJECT = %s ", hb_objGetClsName( pBase ) );
            else
               printf( "ARRAY " );
            break;

         case IT_BLOCK:
            printf( "BLOCK " );
            break;

         case IT_DATE:
            {
               char szDate[ 9 ];
               printf( "DATE = \"%s\" ", hb_itemGetDS( pBase, szDate ) );
            }
            break;

         case IT_DOUBLE:
            printf( "DOUBLE = %f ", hb_itemGetND( pBase ) );
            break;

         case IT_LOGICAL:
            printf( "LOGICAL = %s ", hb_itemGetL( pBase ) ? ".T." : ".F." );
            break;

         case IT_LONG:
            printf( "LONG = %lu ", hb_itemGetNL( pBase ) );
            break;

         case IT_INTEGER:
            printf( "INTEGER = %i ", hb_itemGetNI( pBase ) );
            break;

         case IT_STRING:
            printf( "STRING = \"%s\" ", hb_itemGetCPtr( pBase ) );
            break;

         case IT_SYMBOL:
            printf( "SYMBOL = %s ", pBase->item.asSymbol.value->szName );
            break;

         default:
            printf( "UNKNOWN = TYPE %i ", hb_itemType( pBase ) );
            break;
      }
   }
}

void hb_stackDispCall( void )
{
   PHB_ITEM pBase = hb_stack.pBase;

   HB_TRACE(HB_TR_DEBUG, ("hb_stackDispCall()"));

   while( pBase != hb_stack.pItems )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 32 ];

      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

      if( ( pBase + 1 )->type == IT_ARRAY )
         sprintf( buffer, "Called from %s:%s(%i)", hb_objGetClsName( pBase + 1 ),
            pBase->item.asSymbol.value->szName,
            pBase->item.asSymbol.lineno );
      else
         sprintf( buffer, "Called from %s(%i)",
            pBase->item.asSymbol.value->szName,
            pBase->item.asSymbol.lineno );

      hb_outerr( buffer, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );
   }
}

/* ----------------------------------------------- */

void hb_vmProcessSymbols( PHB_SYMB pModuleSymbols, USHORT uiModuleSymbols ) /* module symbols initialization */
{
   PSYMBOLS pNewSymbols;
   USHORT ui;

#ifdef HARBOUR_OBJ_GENERATION
   static BOOL bObjChecked = FALSE;
#endif

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessSymbols(%p, %hu)", pModuleSymbols, uiModuleSymbols));

#ifdef HARBOUR_OBJ_GENERATION
   if( ! bObjChecked )
   {
      bObjChecked = TRUE;
      hb_vmProcessObjSymbols();   /* to asure Harbour OBJ symbols are processed first */
   }
#endif

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
      if( ( ! s_pSymStart ) && ( hSymScope == FS_PUBLIC ) )
         s_pSymStart = pModuleSymbols + ui;  /* first public defined symbol to start execution */

      if( ( hSymScope == FS_PUBLIC ) || ( hSymScope & ( FS_MESSAGE | FS_MEMVAR ) ) )
         hb_dynsymNew( pModuleSymbols + ui );
   }
}

#ifdef HARBOUR_OBJ_GENERATION
static void hb_vmProcessObjSymbols( void )
{
   static BOOL bDone = FALSE;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmProcessObjSymbols()"));

   if( ! bDone )
   {
      POBJSYMBOLS pObjSymbols = ( POBJSYMBOLS ) &HB_FIRSTSYMBOL;

      while( pObjSymbols < ( POBJSYMBOLS ) ( &HB_LASTSYMBOL - 1 ) )
      {
         hb_vmProcessSymbols( pObjSymbols->pSymbols, pObjSymbols->wSymbols );
         pObjSymbols++;
      }

      bDone = TRUE;
   }
}
#endif

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
 * We are using a special symbol's scope ( FS_INIT | FS_EXIT ) to mark
 * this function. These two bits cannot be marked at the same
 * time for normal user defined functions.
 */
static void hb_vmDoInitStatics( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;

   HB_TRACE(HB_TR_DEBUG, ("hb_vmDoInitStatics()"));

   do
   {
      if( ( pLastSymbols->hScope & ( FS_INIT | FS_EXIT ) ) == ( FS_INIT | FS_EXIT ) )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( FS_EXIT | FS_INIT );

            if( scope == ( FS_INIT | FS_EXIT ) )
            {
               /* _INITSTATICS procedure cannot call any function and it
               * cannot use any local variable then it is safe to call
               * this procedure directly
               * hb_vmPushSymbol( pLastSymbols->pModuleSymbols + ui );
               * hb_vmPushNil();
               * hb_vmDo( 0 );
               */
               if( ( pLastSymbols->pModuleSymbols + ui )->pFunPtr )
                  ( pLastSymbols->pModuleSymbols + ui )->pFunPtr();
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
      if( pLastSymbols->hScope & FS_EXIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( FS_EXIT | FS_INIT );

            if( scope == FS_EXIT )
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
      if( pLastSymbols->hScope & FS_INIT )
      {
         USHORT ui;

         for( ui = 0; ui < pLastSymbols->uiModuleSymbols; ui++ )
         {
            HB_SYMBOLSCOPE scope = ( pLastSymbols->pModuleSymbols + ui )->cScope & ( FS_EXIT | FS_INIT );

            if( scope == FS_INIT )
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

/* NOTE: We should make sure that these get linked. */
/*       Don't make this function static, because it's not called from this file. */
void hb_vmForceLink( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmForceLink()"));

   HB_SYSINIT();
}

/* ----------------------------- */
/* TODO: Put these to /source/rtl/?.c */

HARBOUR HB_LEN( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   /* NOTE: pItem cannot be NULL here */

   switch( pItem->type )
   {
      case IT_ARRAY:
         hb_retnl( hb_arrayLen( pItem ) );
         break;

      case IT_STRING:
         hb_retnl( hb_itemGetCLen( pItem ) );
         break;

      default:
      {
         PHB_ITEM pResult = hb_errRT_BASE_Subst( EG_ARG, 1111, NULL, "LEN" );

         if( pResult )
         {
            hb_itemReturn( pResult );
            hb_itemRelease( pResult );
         }
      }
   }
}

HARBOUR HB_EMPTY( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   /* NOTE: pItem cannot be NULL here */

   switch( pItem->type & ~IT_BYREF )
   {
      case IT_ARRAY:
         hb_retl( hb_arrayLen( pItem ) == 0 );
         break;

      case IT_STRING:
         hb_retl( hb_strEmpty( hb_itemGetCPtr( pItem ), hb_itemGetCLen( pItem ) ) );
         break;

      case IT_INTEGER:
         hb_retl( hb_itemGetNI( pItem ) == 0 );
         break;

      case IT_LONG:
         hb_retl( hb_itemGetNL( pItem ) == 0 );
         break;

      case IT_DOUBLE:
         hb_retl( hb_itemGetND( pItem ) == 0.0 );
         break;

      case IT_DATE:
         /* NOTE: This is correct ! Get the date as long value. */
         hb_retl( hb_itemGetNL( pItem ) == 0 );
         break;

      case IT_LOGICAL:
         hb_retl( ! hb_itemGetL( pItem ) );
         break;

      case IT_BLOCK:
         hb_retl( FALSE );
         break;

      default:
         hb_retl( TRUE );
         break;
   }
}

HARBOUR HB_VALTYPE( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_ANY );

   /* NOTE: pItem cannot be NULL here */

   switch( pItem->type & ~IT_BYREF )
   {
      case IT_ARRAY:
         hb_retc( hb_arrayIsObject( pItem ) ? "O" : "A" );
         break;

      case IT_BLOCK:
         hb_retc( "B" );
         break;

      case IT_DATE:
         hb_retc( "D" );
         break;

      case IT_LOGICAL:
         hb_retc( "L" );
         break;

      case IT_INTEGER:
      case IT_LONG:
      case IT_DOUBLE:
         hb_retc( "N" );
         break;

      case IT_STRING:
         hb_retc( "C" );
         break;

      case IT_NIL:
      default:
         hb_retc( "U" );
         break;
   }
}

HARBOUR HB_TYPE( void )
{
   /* TODO: Implement this. */
}

/* INCOMPATIBILITY: The Clipper NG states that WORD() will only work when used
                    in CALL commands parameter list, otherwise it will return
                    NIL, in Harbour it will work anywhere. */

HARBOUR HB_WORD( void )
{
   if( ISNUM( 1 ) )
      hb_retni( hb_parni( 1 ) );
   else
      hb_errRT_BASE( EG_ARG, 1091, NULL, "WORD" );
}

HARBOUR HB_PROCNAME( void )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   if( ( iLevel == -1 ) )
   {
      if( ( pBase + 1 )->type == IT_ARRAY )  /* it is a method name */
      {
         char * szProcName;

         szProcName = ( char * ) hb_xgrab( strlen( hb_objGetClsName( pBase + 1 ) ) + 1 +
                                strlen( pBase->item.asSymbol.value->szName ) + 1 );
         strcpy( szProcName, hb_objGetClsName( pBase + 1 ) );
         strcat( szProcName, ":" );
         strcat( szProcName, pBase->item.asSymbol.value->szName );
         hb_retc( szProcName );
         hb_xfree( ( void * ) szProcName );
      }
      else
         hb_retc( pBase->item.asSymbol.value->szName );
   }
   else
      hb_retc( "" );
}

HARBOUR HB_PROCLINE( void )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = hb_stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != hb_stack.pItems )
      pBase = hb_stack.pItems + pBase->item.asSymbol.stackbase;

   if( iLevel == -1 )
      hb_retni( pBase->item.asSymbol.lineno );
   else
      hb_retni( 0 );
}

/* NOTE: Clipper undocumented function, which always returns an empty
         string. */

HARBOUR HB_PROCFILE( void )
{
   hb_retc( "" );
}

HARBOUR HB_ERRORLEVEL( void )
{
   hb_retni( s_byErrorLevel );

   /* NOTE: This should be ISNUM( 1 ), but it's sort of a Clipper bug that it
            accepts other types also and consider them zero. */

   if( hb_pcount() >= 1 )
      /* Only replace the error level if a parameter was passed */
      s_byErrorLevel = hb_parni( 1 );
}

HARBOUR HB_PCOUNT( void )
{
   /* Skip current function */
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;

   hb_retni( pBase->item.asSymbol.paramcnt );
}

HARBOUR HB_PVALUE( void )                               /* PValue( <nArg> )         */
{
   USHORT uiParam = hb_parni( 1 );                  /* Get parameter            */
   PHB_ITEM pBase = hb_stack.pItems + hb_stack.pBase->item.asSymbol.stackbase;
                                                /* Skip function + self     */

   if( uiParam && uiParam <= pBase->item.asSymbol.paramcnt )     /* Valid number             */
      hb_itemReturn( pBase + 1 + uiParam );
   else
      hb_errRT_BASE( EG_ARG, 3011, NULL, "PVALUE" );
}

HARBOUR HB___QUIT( void )
{
   hb_vmRequestQuit();
}

void hb_vmRequestQuit( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestQuit()"));

   s_uiActionRequest = HB_QUIT_REQUESTED;
}

void hb_vmRequestBreak( PHB_ITEM pItem )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestBreak(%p)", pItem));

   if( s_lRecoverBase )
   {
      if( pItem )
         hb_itemCopy( hb_stack.pItems + s_lRecoverBase + HB_RECOVER_VALUE, pItem );

      s_uiActionRequest = HB_BREAK_REQUESTED;
   }
   else
      s_uiActionRequest = HB_QUIT_REQUESTED;
}

USHORT hb_vmRequestQuery( void )
{
   return s_uiActionRequest;
}

/* NOTE: This function should normally have a parameter count check. But
         since in Harbour we cannot distinguish between BREAK() function and
         the BREAK statement, because both generate a BREAK() function
         call on the pcode level, we should drop the checking. */

HARBOUR HB_BREAK( void )
{
   hb_vmRequestBreak( hb_param( 1, IT_ANY ) );
}

void hb_vmRequestCancel( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_vmRequestCancel()"));

   if( hb_set.HB_SET_CANCEL )
   {
      char buffer[ HB_SYMBOL_NAME_LEN + 32 ];

      hb_outerr( hb_consoleGetNewLine(), 0 );
      sprintf( buffer, "Cancelled at: %s (%i)", hb_stack.pBase->item.asSymbol.value->szName, hb_stack.pBase->item.asSymbol.lineno );
      hb_outerr( buffer, 0 );
      hb_outerr( hb_consoleGetNewLine(), 0 );

      s_uiActionRequest = HB_QUIT_REQUESTED;
   }
}

/* NOTE: This is an internal undocumented Clipper function, which will try
         to call the function HELP() if it's defined. This is the default
         SetKey() handler for the F1 key. */

HARBOUR HB___XHELP( void )
{
   PHB_DYNS pDynSym = hb_dynsymFind( "HELP" );

   if( pDynSym )
   {
      hb_vmPushSymbol( pDynSym->pSymbol );
      hb_vmPushNil();
      hb_vmDo( 0 );

      /* NOTE: Leave the return value as it is. */
   }
}

/* $Doc$
 * $FuncName$     <aStat> __vmVarSList()
 * $Description$  Return the statics array
 *
 *                Please aClone before assignments
 * $End$ */
HARBOUR HB___VMVARSLIST( void )
{
   PHB_ITEM pStatics = hb_arrayClone( &s_aStatics );

   hb_itemCopy( &hb_stack.Return, pStatics );
   hb_itemRelease( pStatics );
}

/* $Doc$
 * $FuncName$     <xStat> __vmVarSGet(<nStatic>)
 * $Description$  Return a specified statics
 * $End$ */
HARBOUR HB___VMVARSGET( void )
{
   hb_itemReturn( s_aStatics.item.asArray.value->pItems +
                  hb_stack.iStatics + hb_parni( 1 ) - 1 );
}


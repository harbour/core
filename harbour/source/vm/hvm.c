/*
 * $Id$
 */

/*
 * The Harbour virtual machine
 * Copyright(C) 1999 by Antonio Linares.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * You can contact me at: alinares@fivetech.com
 */

#include <limits.h>
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

typedef struct _SYMBOLS
{
   PHB_SYMB pModuleSymbols; /* pointer to a one module own symbol table */
   WORD     wModuleSymbols; /* number of symbols on that table */
   struct _SYMBOLS * pNext; /* pointer to the next SYMBOLS structure */
   SYMBOLSCOPE hScope;      /* scope collected from all symbols in module used to speed initialization code */
} SYMBOLS, * PSYMBOLS;      /* structure to keep track of all modules symbol tables */

extern HARBOUR HB_ERRORSYS( void );
extern HARBOUR HB_ERRORNEW( void );

static void    hb_vmPopAlias( void );        /* pops the workarea number form the eval stack */
static void    hb_vmPopAliasedField( PHB_SYMB );  /* pops an aliased field from the eval stack*/
static void    hb_vmPopField( PHB_SYMB );      /* pops an unaliased field from the eval stack */
static void    hb_vmPushAlias( void );       /* pushes the current workarea number */
static void    hb_vmPushAliasedField( PHB_SYMB );     /* pushes an aliased field on the eval stack */
static void    hb_vmPushField( PHB_SYMB );     /* pushes an unaliased field on the eval stack */
static void    hb_vmSwapAlias( void );       /* swaps items on the eval stack and pops the workarea number */

static void    hb_vmDoInitStatics( void ); /* executes all _INITSTATICS functions */
static void    hb_vmDoInitFunctions( int argc, char * argv[] ); /* executes all defined PRGs INIT functions */
static void    hb_vmDoExitFunctions( void ); /* executes all defined PRGs EXIT functions */
static void    hb_vmReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */

static void    hb_vmDebuggerShowLine( WORD wLine ); /* makes the debugger shows a specific source code line */
static void    hb_vmDebuggerEndProc( void ); /* notifies the debugger for an endproc */

#ifdef HARBOUR_OBJ_GENERATION
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

STACK    stack;
HB_SYMB  symEval = { "__EVAL", FS_PUBLIC, hb_vmDoBlock, 0 }; /* symbol to evaluate codeblocks */
HB_ITEM  aStatics;         /* Harbour array to hold all application statics variables */

static BOOL     s_bDebugging = FALSE;
static BOOL     s_bDebugShowLines = FALSE; /* update source code line on the debugger display */
static PHB_SYMB s_pSymStart = NULL;        /* start symbol of the application. MAIN() is not required */
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
static WORD s_wActionRequest = 0;

/* uncomment it to trace the virtual machine activity */
/* #define  bHB_DEBUG */

#if defined(bHB_DEBUG)
#define HB_DEBUG( x )         printf( x )
#define HB_DEBUG2( x, y )     printf( x, y )
#else
#define HB_DEBUG( x )
#define HB_DEBUG2( x, y )
#endif

/* application entry point */

int main( int argc, char * argv[] )
{
   int i;

   HB_DEBUG( "main\n" );

   /* initialize internal data structures */
   aStatics.type     = IT_NIL;
   stack.Return.type = IT_NIL;

   hb_xinit();
   hb_errInit();
   hb_stackInit();
   hb_dynsymNew( &symEval );  /* initialize dynamic symbol for evaluating codeblocks */
   hb_setInitialize();        /* initialize Sets */
   hb_consoleInitialize();    /* initialize Console */
   hb_memvarsInit();
#ifdef HARBOUR_OBJ_GENERATION
   hb_vmProcessObjSymbols();  /* initialize Harbour generated OBJs symbols */
#endif
   hb_vmSymbolInit_RT();      /* initialize symbol table with runtime support functions */

   /* Call functions that initializes static variables
    * Static variables have to be initialized before any INIT functions
    * because INIT function can use static variables
    */
   hb_vmDoInitStatics();
   hb_vmDoInitFunctions( argc, argv ); /* process defined INIT functions */

#ifdef HARBOUR_START_PROCEDURE
   {
      PHB_DYNS pDynSym = hb_dynsymFind( HARBOUR_START_PROCEDURE );

      if( pDynSym )
         s_pSymStart = pDynSym->pSymbol;
      else
         hb_errInternal( 9999, "Can\'t locate the starting procedure: \'%s\'", HARBOUR_START_PROCEDURE, NULL );
   }
#endif

   hb_vmPushSymbol( s_pSymStart ); /* pushes first FS_PUBLIC defined symbol to the stack */
   hb_vmPushNil();               /* places NIL at self */
   for( i = 1; i < argc; i++ )   /* places application parameters on the stack */
      hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );
   hb_vmDo( argc - 1 );          /* invoke it with number of supplied parameters */

   hb_vmQuit();

   /* This point is never reached */

   return 0;
}

void hb_vmQuit( void )
{
   s_wActionRequest = 0;         /* EXIT procedures should be processed */
   hb_vmDoExitFunctions();       /* process defined EXIT functions */

   while( stack.pPos > stack.pItems )
      hb_stackPop();

   hb_itemClear( &stack.Return );
   hb_arrayRelease( &aStatics );
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

   HB_DEBUG( "Done!\n" );

   exit( s_byErrorLevel );
}

void hb_vmExecute( BYTE * pCode, PHB_SYMB pSymbols )
{
   BYTE bCode;
   WORD w = 0, wParams, wSize;
   BOOL bCanRecover = FALSE;
   ULONG ulPrivateBase = hb_memvarGetPrivatesBase();

   HB_DEBUG( "hb_vmExecute\n" );

   while( ( bCode = pCode[ w ] ) != HB_P_ENDPROC )
   {
      hb_inkeyPoll();                   /* Poll the console keyboard */

      switch( bCode )
      {
         case HB_P_AND:
              hb_vmAnd();
              w++;
              break;

         case HB_P_ARRAYAT:
              hb_vmArrayAt();
              w++;
              break;

         case HB_P_ARRAYPUT:
              hb_vmArrayPut();
              w++;
              break;

         case HB_P_DEC:
              hb_vmDec();
              w++;
              break;

         case HB_P_DIMARRAY:
              hb_vmDimArray( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_DIVIDE:
              hb_vmDivide();
              w++;
              break;

         case HB_P_DO:
              hb_vmDo( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
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

         case HB_P_ENDBLOCK:
              hb_vmEndBlock();
              HB_DEBUG( "EndBlock\n" );
              return;   /* end of a codeblock - stop evaluation */

         case HB_P_EQUAL:
              hb_vmEqual( FALSE );
              w++;
              break;

         case HB_P_EXACTLYEQUAL:
              hb_vmEqual( TRUE );
              w++;
              break;

         case HB_P_FALSE:
              hb_vmPushLogical( FALSE );
              w++;
              break;

         case HB_P_FORTEST:
              hb_vmForTest();
              w++;
              break;

         case HB_P_FRAME:
              hb_vmFrame( pCode[ w + 1 ], pCode[ w + 2 ] );
              w += 3;
              break;

         case HB_P_FUNCPTR:
              hb_vmFuncPtr();
              w++;
              break;

         case HB_P_FUNCTION:
              hb_vmFunction( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_GENARRAY:
              hb_vmGenArray( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_GREATER:
              hb_vmGreater();
              w++;
              break;

         case HB_P_GREATEREQUAL:
              hb_vmGreaterEqual();
              w++;
              break;

         case HB_P_INC:
              hb_vmInc();
              w++;
              break;

         case HB_P_INSTRING:
              hb_vmInstring();
              w++;
              break;

         case HB_P_JUMP:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              if( wParams )
                 w += wParams;
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

         case HB_P_LESS:
              hb_vmLess();
              w++;
              break;

         case HB_P_LESSEQUAL:
              hb_vmLessEqual();
              w++;
              break;

         case HB_P_LINE:
              stack.pBase->item.asSymbol.lineno = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              if( s_bDebugging && s_bDebugShowLines )
                 hb_vmDebuggerShowLine( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_LOCALNAME:
              hb_vmLocalName( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ),
                              ( char * ) pCode + w + 3 );
              w += 3;
              while( pCode[ w++ ] );
              break;

         case HB_P_MESSAGE:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmMessage( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_MINUS:
              hb_vmMinus();
              w++;
              break;

         case HB_P_MODULENAME:
              hb_vmModuleName( ( char * ) pCode + w + 1 );
              while( pCode[ w++ ] );
              break;

         case HB_P_MODULUS:
              hb_vmModulus();
              w++;
              break;

         case HB_P_MULT:
              hb_vmMult();
              w++;
              break;

         case HB_P_NEGATE:
              hb_vmNegate();
              w++;
              break;

         case HB_P_NOT:
              hb_vmNot();
              w++;
              break;

         case HB_P_NOTEQUAL:
              hb_vmNotEqual();
              w++;
              break;

         case HB_P_OR:
              hb_vmOr();
              w++;
              break;

         case HB_P_PARAMETER:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPopParameter( pSymbols + wParams, pCode[ w+3 ] );
              w += 4;
              break;

         case HB_P_PLUS:
              hb_vmPlus();
              w++;
              break;

         case HB_P_POP:
              hb_stackPop();
              w++;
              break;

         case HB_P_POPALIAS:
              hb_vmPopAlias();
              w++;
              break;

         case HB_P_POPALIASEDFIELD:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPopAliasedField( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_POPFIELD:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPopField( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_POPLOCAL:
              hb_vmPopLocal( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_POPMEMVAR:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPopMemvar( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_POPSTATIC:
              hb_vmPopStatic( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_POWER:
              hb_vmPower();
              w++;
              break;

         case HB_P_PUSHALIAS:
              hb_vmPushAlias();
              w++;
              break;

         case HB_P_PUSHALIASEDFIELD:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushAliasedField( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_PUSHBLOCK:
              /* +0    -> _pushblock
               * +1 +2 -> size of codeblock
               * +3 +4 -> number of expected parameters
               * +5 +6 -> number of referenced local variables
               * +7 -> start of table with referenced local variables
               */
              hb_vmPushBlock( pCode + w, pSymbols );
              w += ( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              break;

         case HB_P_PUSHDOUBLE:
              hb_vmPushDouble( * ( double * ) ( &pCode[ w + 1 ] ), ( WORD ) * ( BYTE * ) &pCode[ w + 1 + sizeof( double ) ] );
              w += 1 + sizeof( double ) + 1;
              break;

         case HB_P_PUSHFIELD:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushField( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_PUSHINT:
              hb_vmPushInteger( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
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

         case HB_P_PUSHLONG:
              hb_vmPushLong( * ( long * ) ( &pCode[ w + 1 ] ) );
              w += 5;
              break;

         case HB_P_PUSHMEMVAR:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushMemvar( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_PUSHMEMVARREF:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushMemvarByRef( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_PUSHNIL:
              hb_vmPushNil();
              w++;
              break;

         case HB_P_PUSHSELF:
              hb_vmPush( stack.pBase + 1 );
              w++;
              break;

         case HB_P_PUSHSTATIC:
              hb_vmPushStatic( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_PUSHSTATICREF:
              hb_vmPushStaticByRef( pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 ) );
              w += 3;
              break;

         case HB_P_PUSHSTR:
              wSize = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushString( ( char * ) pCode + w + 3, wSize );
              w += ( wSize + 3 );
              break;

         case HB_P_PUSHSYM:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmPushSymbol( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_SWAPALIAS:
              hb_vmSwapAlias();
              w++;
              break;

         case HB_P_RETVALUE:
              hb_vmRetValue();
              w++;
              break;

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
               stack.pPos->type = IT_NIL;
               hb_stackPush();
               /*
                * 2) store the address of RECOVER or END opcode
                */
               stack.pPos->type = IT_LONG;
               stack.pPos->item.asLong.value = w + pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
               hb_stackPush();
               /*
                * 3) store current RECOVER base
                */
               stack.pPos->type = IT_LONG;
               stack.pPos->item.asLong.value = s_lRecoverBase;
               hb_stackPush();
               /*
                * 4) store current bCanRecover flag - in a case of nested sequences
                * in the same procedure/function
                */
               stack.pPos->type = IT_LOGICAL;
               stack.pPos->item.asLogical.value = bCanRecover;
               hb_stackPush();
               /*
                * set new recover base
                */
               s_lRecoverBase = stack.pPos - stack.pItems;
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
              bCanRecover = stack.pPos->item.asLogical.value;
              stack.pPos->type = IT_NIL;
              /*
               * 3) Restore previous RECOVER base
               */
              hb_stackDec();
              s_lRecoverBase = stack.pPos->item.asLong.value;
              stack.pPos->type = IT_NIL;
              /*
               * 2) Remove RECOVER address
               */
              hb_stackDec();
              stack.pPos->type = IT_NIL;
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
              bCanRecover = stack.pPos->item.asLogical.value;
              stack.pPos->type = IT_NIL;
              /*
               * 3) Restore previous RECOVER base
               */
              hb_stackDec();
              s_lRecoverBase = stack.pPos->item.asLong.value;
              stack.pPos->type = IT_NIL;
              /*
               * 2) Remove RECOVER address
               */
              hb_stackDec();
              stack.pPos->type = IT_NIL;
              /*
               * 1) Leave the value returned from BREAK  - it will be popped
               * in next executed opcode
               */
              w++;
              break;

         case HB_P_SFRAME:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmSFrame( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_STATICS:
              wParams = pCode[ w + 1 ] + ( pCode[ w + 2 ] * 256 );
              hb_vmStatics( pSymbols + wParams );
              w += 3;
              break;

         case HB_P_TRUE:
              hb_vmPushLogical( TRUE );
              w++;
              break;

         case HB_P_ZERO:
              hb_vmPushInteger( 0 );
              w++;
              break;

         case HB_P_NOOP:
              /* Intentionally do nothing */
              break;

         default:
              /* TODO: Include to failing pcode in the error message */
              hb_errInternal( 9999, "Unsupported VM opcode", NULL, NULL );
              break;
      }

      if( s_wActionRequest )
      {
         if( s_wActionRequest & HB_BREAK_REQUESTED )
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
               while( stack.pPos > stack.pItems + s_lRecoverBase )
                  hb_stackPop();
               /*
                * reload the address of recovery code
                */
               w = stack.pItems[ s_lRecoverBase + HB_RECOVER_ADDRESS ].item.asLong.value;
               /*
                * leave the SEQUENCE envelope on the stack - it will
                * be popped either in RECOVER or END opcode
                */
               s_wActionRequest = 0;
            }
            else
               break;
         }
         else if( s_wActionRequest & HB_QUIT_REQUESTED )
            break;
      }
   }
   hb_memvarSetPrivatesBase( ulPrivateBase );
}

void hb_vmAnd( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   HB_DEBUG( "And\n" );

   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asLogical.value && pItem2->item.asLogical.value;
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( bResult );
   }
   else
      hb_errRT_BASE( EG_ARG, 1078, NULL, ".AND." );
}

void hb_vmArrayAt( void )
{
   PHB_ITEM pIndex = stack.pPos - 1;
   PHB_ITEM pArray = stack.pPos - 2;
   ULONG ulIndex;

   if( IS_INTEGER( pIndex ) )
      ulIndex = pIndex->item.asInteger.value;

   else if( IS_LONG( pIndex ) )
      ulIndex = pIndex->item.asLong.value;

   else if( IS_DOUBLE( pIndex ) )
      ulIndex = pIndex->item.asDouble.value;

   else
   {
      hb_errRT_BASE( EG_ARG, 1068, NULL, hb_langDGetErrorDesc( EG_ARRACCESS ) );
      return;
   }

   if( ! hb_arrayError( pArray, ulIndex, FALSE ) )
   {
      HB_ITEM item;

      hb_arrayGet( pArray, ulIndex, &item );
      hb_stackPop();
      hb_stackPop();

      hb_itemCopy( stack.pPos, &item );
      hb_itemClear( &item );
      hb_stackPush();
   }
}

void hb_vmArrayPut( void )
{
   PHB_ITEM pValue = stack.pPos - 1;
   PHB_ITEM pIndex = stack.pPos - 2;
   PHB_ITEM pArray = stack.pPos - 3;
   ULONG ulIndex;

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

static void hb_vmDebuggerEndProc( void )
{
   HB_ITEM item;

   hb_itemCopy( &item, &stack.Return ); /* saves the previous returned value */

   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmDo( 0 );
   s_bDebugShowLines = TRUE;

   hb_itemCopy( &stack.Return, &item ); /* restores the previous returned value */
   hb_itemClear( &item );
}

static void hb_vmDebuggerShowLine( WORD wLine ) /* makes the debugger shows a specific source code line */
{
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushInteger( wLine );
   hb_vmDo( 1 );
   s_bDebugShowLines = TRUE;
}

void hb_vmDec( void )
{
   double dNumber;
   LONG lDate;
   WORD wDec;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
      dNumber = hb_vmPopDouble( &wDec );
      hb_vmPushNumber( --dNumber, wDec );
   }
   else if( IS_DATE( stack.pPos - 1 ) )
   {
      lDate = hb_vmPopDate();
      hb_vmPushDate( --lDate ); /* TOFIX: Dates should decreased other way */
   }
   else
      hb_errRT_BASE( EG_ARG, 1087, NULL, "--" );
}

void hb_vmDimArray( WORD wDimensions ) /* generates a wDimensions Array and initialize those dimensions from the stack values */
{
   HB_ITEM itArray;
   WORD w; /* , wElements; */

   itArray.type = IT_NIL;
   hb_arrayNew( &itArray, ( stack.pPos - wDimensions )->item.asLong.value );

   if( wDimensions > 1 )
      hb_errInternal( 9999, "HVM.C hb_vmDimArray() does not supports multiple dimensions yet", NULL, NULL );

/*
   for( w = 0; w < wElements; w++ )
     hb_itemCopy( itArray.item.asArray.value->pItems + w,
               stack.pPos - wElements + w );
*/

   for( w = 0; w < wDimensions; w++ )
      hb_stackPop();

   hb_itemCopy( stack.pPos, &itArray );
   hb_itemClear( &itArray );
   hb_stackPush();
}

void hb_vmDivide( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      WORD wDec1, wDec2;
      double d2 = hb_vmPopDouble( &wDec2 );
      double d1 = hb_vmPopDouble( &wDec1 );

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
         hb_vmPushNumber( d1 / d2, hb_set.HB_SET_DECIMALS );
   }
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

void hb_vmDo( WORD wParams )
{
   PHB_ITEM pItem = stack.pPos - wParams - 2;   /* procedure name */
   PHB_SYMB pSym = pItem->item.asSymbol.value;
   LONG wStackBase = stack.pBase - stack.pItems; /* as the stack memory block could change */
   LONG wItemIndex = pItem - stack.pItems;
   PHB_ITEM pSelf = stack.pPos - wParams - 1;   /* NIL, OBJECT or BLOCK */
   PHB_FUNC pFunc;
   int iStatics = stack.iStatics;              /* Return iStatics position */
   BOOL bDebugPrevState = s_bDebugging;

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

   pItem->item.asSymbol.lineno   = 0;
   pItem->item.asSymbol.paramcnt = wParams;
   stack.pBase    = stack.pItems + pItem->item.asSymbol.stackbase;
   pItem->item.asSymbol.stackbase = wStackBase;

   HB_DEBUG2( "Do with %i params\n", wParams );

   if( ! IS_NIL( pSelf ) ) /* are we sending a message ? */
   {
      if( pSym == &( symEval ) && IS_BLOCK( pSelf ) )
         pFunc = pSym->pFunPtr;                 /* __EVAL method = function */
      else
         pFunc = hb_objGetMethod( pSelf, pSym );

      if( pFunc )
         pFunc();
      else
      {
         if( pSym->szName[ 0 ] == '_' )
            hb_errRT_BASE( EG_NOVARMETHOD, 1005, NULL, pSym->szName + 1 );
         else
            hb_errRT_BASE( EG_NOMETHOD, 1004, NULL, pSym->szName );
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

   while( stack.pPos > stack.pItems + wItemIndex )
      hb_stackPop();

   stack.pBase = stack.pItems + wStackBase;
   stack.iStatics = iStatics;

   if( s_bDebugging )
      hb_vmDebuggerEndProc();

   s_bDebugging = bDebugPrevState;
}

HARBOUR hb_vmDoBlock( void )
{
   PHB_ITEM pBlock = stack.pBase + 1;
   WORD wStackBase = stack.pBase - stack.pItems; /* as the stack memory block could change */
   WORD wLine;
   int iParam;

   if( ! IS_BLOCK( pBlock ) )
      hb_errInternal( 9999, "Codeblock expected from hb_vmDoBlock()", NULL, NULL );

   /* Check for valid count of parameters */
   iParam = pBlock->item.asBlock.paramcnt - hb_pcount();
   /* add missing parameters */
   while( iParam-- > 0 )
     hb_vmPushNil();

   /* set the current line number to a line where the codeblock was defined
    */
   wLine =stack.pBase->item.asSymbol.lineno;
   stack.pBase->item.asSymbol.lineno = pBlock->item.asBlock.lineno;

   hb_codeblockEvaluate( pBlock );

   /* restore stack pointers */
   stack.pBase = stack.pItems + wStackBase;
   stack.pBase->item.asSymbol.lineno =wLine;

   HB_DEBUG( "End of DoBlock\n" );
}

void hb_vmDuplicate( void )
{
   hb_itemCopy( stack.pPos, stack.pPos - 1 );
   hb_stackPush();
}

void hb_vmDuplTwo( void )
{
   hb_itemCopy( stack.pPos, stack.pPos - 2 );
   hb_stackPush();
   hb_itemCopy( stack.pPos, stack.pPos - 2 );
   hb_stackPush();
}

HARBOUR HB_EVAL( void )
{
   PHB_ITEM pBlock = hb_param( 1, IT_BLOCK );

   if( pBlock )
   {
      WORD w;

      hb_vmPushSymbol( &symEval );
      hb_vmPush( pBlock );

      for( w = 2; w <= hb_pcount(); w++ )
         hb_vmPush( hb_param( w, IT_ANY ) );

      hb_vmDo( hb_pcount() - 1 );
   }
   else
      hb_errInternal( 9999, "Not a valid codeblock on EVAL", NULL, NULL );
}

void hb_vmEndBlock( void )
{
   hb_stackDec();                               /* make the last item visible */
   hb_itemCopy( &stack.Return, stack.pPos ); /* copy it */
   hb_itemClear( stack.pPos );               /* and now clear it */
   HB_DEBUG( "EndBlock\n" );
}

void hb_vmEqual( BOOL bExact )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;
   int i;
   WORD wDec;

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
      i = hb_itemStrCmp( pItem1, pItem2, bExact );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i == 0 );
   }

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() == hb_vmPopLogical() );

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDouble( &wDec ) == hb_vmPopDouble( &wDec ) );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "==" ) )
      hb_vmOperatorCall( pItem1, pItem2, "==" );

   else if( pItem1->type != pItem2->type )
   {
      if( bExact )
         hb_errRT_BASE( EG_ARG, 1070, NULL, "==" );
      else
         hb_errRT_BASE( EG_ARG, 1071, NULL, "=" );
   }
   else
      hb_vmPushLogical( FALSE );
}

void hb_vmForTest( void )        /* Test to check the end point of the FOR */
{
   double dStep;
   BOOL   bEqual;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
       WORD   wDec;

       dStep = hb_vmPopDouble( &wDec );

       /* NOTE: step of zero will cause endless loop, as in Clipper */

       if( dStep > 0 )           /* Positive loop. Use LESS */
           hb_vmLess();
       else if( dStep < 0 )      /* Negative loop. Use GREATER */
           hb_vmGreater();

       bEqual = hb_vmPopLogical();    /* Logical should be on top of stack */
       hb_vmPushNumber( dStep, wDec );   /* Push the step expression back on the stack */
       hb_vmPushLogical( bEqual );
   }
   else
      hb_errRT_BASE( EG_ARG, 1073, NULL, "<" );
}

void hb_vmFrame( BYTE bLocals, BYTE bParams )
{
   int i, iTotal = bLocals + bParams;

   HB_DEBUG( "Frame\n" );
   if( iTotal )
      for( i = 0; i < ( iTotal - stack.pBase->item.asSymbol.paramcnt ); i++ )
         hb_vmPushNil();
}

void hb_vmFuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the satck */
{
   PHB_ITEM pItem = stack.pPos - 1;

   if( IS_SYMBOL( pItem ) )
   {
      hb_stackPop();
      hb_vmPushLong( ( ULONG ) pItem->item.asSymbol.value->pFunPtr );
   }
   else
      hb_errInternal( 9999, "Symbol item expected from hb_vmFuncPtr()", NULL, NULL );
}

void hb_vmFunction( WORD wParams )
{
   hb_itemClear( &stack.Return );
   hb_vmDo( wParams );
   hb_itemCopy( stack.pPos, &stack.Return );
   hb_stackPush();
}

void hb_vmGenArray( WORD wElements ) /* generates a wElements Array and fills it from the stack values */
{
   HB_ITEM itArray;
   WORD w;

   itArray.type = IT_NIL;
   hb_arrayNew( &itArray, wElements );
   for( w = 0; w < wElements; w++ )
      hb_itemCopy( itArray.item.asArray.value->pItems + w,
                stack.pPos - wElements + w );

   for( w = 0; w < wElements; w++ )
      hb_stackPop();

   hb_itemCopy( stack.pPos, &itArray );
   hb_itemClear( &itArray );
   hb_stackPush();
}

void hb_vmGreater( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i > 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = hb_vmPopNumber();
      dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 > dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = hb_vmPopDate();
      lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 > lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      BOOL bLogical1 = hb_vmPopLogical();
      BOOL bLogical2 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 > bLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_objHasMsg( stack.pPos - 2, ">" ) )
      hb_vmOperatorCall( stack.pPos - 2, stack.pPos - 1, ">" );

   else if( ( stack.pPos - 2 )->type != ( stack.pPos - 1 )->type )
      hb_errRT_BASE( EG_ARG, 1075, NULL, ">" );
}

void hb_vmGreaterEqual( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i >= 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = hb_vmPopNumber();
      dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 >= dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = hb_vmPopDate();
      lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 >= lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      BOOL bLogical1 = hb_vmPopLogical();
      BOOL bLogical2 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 >= bLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_objHasMsg( stack.pPos - 2, ">=" ) )
      hb_vmOperatorCall( stack.pPos - 2, stack.pPos - 1, ">=" );

   else if( ( stack.pPos - 2 )->type != ( stack.pPos - 1 )->type )
      hb_errRT_BASE( EG_ARG, 1076, NULL, ">=" );
}

void hb_vmInc( void )
{
   double dNumber;
   LONG lDate;
   WORD wDec;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
      dNumber = hb_vmPopDouble( &wDec );
      hb_vmPushNumber( ++dNumber, wDec );
   }
   else if( IS_DATE( stack.pPos - 1 ) )
   {
      lDate = hb_vmPopDate();
      hb_vmPushDate( ++lDate );
   }
   else
      hb_errRT_BASE( EG_ARG, 1086, NULL, "++" );
}

void hb_vmInstring( void )
{
   PHB_ITEM pItem1 = stack.pPos - 2;
   PHB_ITEM pItem2 = stack.pPos - 1;

   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      int iResult = hb_strAt( pItem1->item.asString.value, pItem1->item.asString.length,
                              pItem2->item.asString.value, pItem2->item.asString.length );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( iResult == 0 ? FALSE : TRUE );
   }
   else
      hb_errRT_BASE( EG_ARG, 1109, NULL, "$" );
}

void hb_vmLess( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i < 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = hb_vmPopNumber();
      dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 < dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = hb_vmPopDate();
      lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 < lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      BOOL bLogical1 = hb_vmPopLogical();
      BOOL bLogical2 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 < bLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_objHasMsg( stack.pPos - 2, "<" ) )
      hb_vmOperatorCall( stack.pPos - 2, stack.pPos - 1, "<" );

   else if( ( stack.pPos - 2 )->type != ( stack.pPos - 1 )->type )
      hb_errRT_BASE( EG_ARG, 1073, NULL, "<" );
}

void hb_vmLessEqual( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i <= 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = hb_vmPopNumber();
      dNumber1 = hb_vmPopNumber();
      hb_vmPushLogical( dNumber1 <= dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = hb_vmPopDate();
      lDate1 = hb_vmPopDate();
      hb_vmPushLogical( lDate1 <= lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      BOOL bLogical1 = hb_vmPopLogical();
      BOOL bLogical2 = hb_vmPopLogical();
      hb_vmPushLogical( bLogical1 <= bLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_objHasMsg( stack.pPos - 2, "<=" ) )
      hb_vmOperatorCall( stack.pPos - 2, stack.pPos - 1, "<=" );

   else if( ( stack.pPos - 2 )->type != ( stack.pPos - 1 )->type )
      hb_errRT_BASE( EG_ARG, 1074, NULL, "<=" );
}

void hb_vmLocalName( WORD wLocal, char * szLocalName ) /* locals and parameters index and name information for the debugger */
{
   HB_SYMBOL_UNUSED( wLocal );
   HB_SYMBOL_UNUSED( szLocalName );
}

void hb_vmMessage( PHB_SYMB pSymMsg ) /* sends a message to an object */
{
   hb_itemCopy( stack.pPos, stack.pPos - 1 ); /* moves the object forward */
   hb_itemClear( stack.pPos - 1 );
   ( stack.pPos - 1 )->type = IT_SYMBOL;
   ( stack.pPos - 1 )->item.asSymbol.value = pSymMsg;
   ( stack.pPos - 1 )->item.asSymbol.stackbase = ( stack.pPos - 1 ) - stack.pItems;
   hb_stackPush();
   HB_DEBUG2( "Message: %s\n", pSymMsg->szName );
}

void hb_vmNegate( void )
{
   if( IS_INTEGER( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->item.asInteger.value = -( stack.pPos - 1 )->item.asInteger.value;

   else if( IS_LONG( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->item.asLong.value = -( stack.pPos - 1 )->item.asLong.value;

   else if( IS_DOUBLE( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->item.asDouble.value = -( stack.pPos - 1 )->item.asDouble.value;

   else
      hb_errRT_BASE( EG_ARG, 1080, NULL, "-" );
}

void hb_vmNot( void )
{
   PHB_ITEM pItem = stack.pPos - 1;

   if( IS_LOGICAL( pItem ) )
      pItem->item.asLogical.value = ! pItem->item.asLogical.value;
   else
      hb_errRT_BASE( EG_ARG, 1077, NULL, ".NOT." );
}

void hb_vmNotEqual( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;
   int i;
   WORD wDec;

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
      i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      hb_stackPop();
      hb_stackPop();
      hb_vmPushLogical( i != 0 );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
      hb_vmPushLogical( hb_vmPopDouble( &wDec ) != hb_vmPopDouble( &wDec ) );

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      hb_vmPushLogical( hb_vmPopLogical() != hb_vmPopLogical() );

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem1, "!=" ) )
      hb_vmOperatorCall( pItem1, pItem2, "!=" );

   else if( pItem1->type != pItem2->type )
      hb_errRT_BASE( EG_ARG, 1072, NULL, "<>" );

   else
      hb_vmPushLogical( TRUE );
}

void hb_vmMinus( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      WORD wDec2, wDec1;
      double dNumber2 = hb_vmPopDouble( &wDec2 );
      double dNumber1 = hb_vmPopDouble( &wDec1 );

      hb_vmPushNumber( dNumber1 - dNumber2, ( wDec1 > wDec2 ) ? wDec1 : wDec2 );
   }
   else if( IS_DATE( pItem2 ) && IS_DATE( pItem1 ) )
   {
      long lDate2 = hb_vmPopDate();
      long lDate1 = hb_vmPopDate();

      hb_vmPushNumber( lDate1 - lDate2, hb_set.HB_SET_DECIMALS );
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

         memcpy( pItem1->item.asString.value + ulLen, pItem2->item.asString.value, pItem2->item.asString.length );
         ulLen += pItem2->item.asString.length;
         memset( pItem1->item.asString.value + ulLen, ' ', pItem1->item.asString.length - ulLen );
         pItem1->item.asString.value[ pItem1->item.asString.length ] = '\0';

         if( pItem2->item.asString.value )
         {
            hb_xfree( pItem2->item.asString.value );
            pItem2->item.asString.value = NULL;
         }
         hb_stackPop();
         return;
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1210, NULL, "-" );
   }
   else if( IS_OBJECT( stack.pPos - 2 ) && hb_objHasMsg( stack.pPos - 2, "-" ) )
      hb_vmOperatorCall( stack.pPos - 2, stack.pPos - 1, "-" );
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

void hb_vmModuleName( char * szModuleName ) /* PRG and function name information for the debugger */
{
   s_bDebugging = TRUE;
   s_bDebugShowLines = FALSE;
   hb_vmPushSymbol( hb_dynsymFind( "__DBGENTRY" )->pSymbol );
   hb_vmPushNil();
   hb_vmPushString( szModuleName, strlen( szModuleName ) );
   hb_vmDo( 1 );
   s_bDebugShowLines = TRUE;
}

void hb_vmModulus( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      WORD wDec1, wDec2;
      double d2 = hb_vmPopDouble( &wDec2 );
      double d1 = hb_vmPopDouble( &wDec1 );

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

void hb_vmMult( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      WORD wDec2, wDec1;
      double d2 = hb_vmPopDouble( &wDec2 );
      double d1 = hb_vmPopDouble( &wDec1 );

      hb_vmPushNumber( d1 * d2, wDec1 + wDec2 );
   }
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

void hb_vmOperatorCall( PHB_ITEM pItem1, PHB_ITEM pItem2, char *szSymbol )
{
   hb_vmPush( pItem1 );                             /* Push object              */
   hb_vmMessage( hb_dynsymGet( szSymbol )->pSymbol );  /* Push operation           */
   hb_vmPush( pItem2 );                             /* Push argument            */
   hb_vmFunction( 1 );
}

void hb_vmOr( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      BOOL bResult = pItem1->item.asLogical.value || pItem2->item.asLogical.value;
      hb_stackDec(); stack.pPos->type = IT_NIL;
      hb_stackDec();
      hb_vmPushLogical( bResult );
   }
   else
      hb_errRT_BASE( EG_ARG, 1079, NULL, ".OR." );
}

void hb_vmPlus( void )
{
   PHB_ITEM pItem1 = stack.pPos - 2;
   PHB_ITEM pItem2 = stack.pPos - 1;

   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      if( ( double ) ( ( double ) pItem1->item.asString.length + ( double ) pItem2->item.asString.length ) < ( double ) ULONG_MAX )
      {
         pItem1->item.asString.value = ( char * ) hb_xrealloc( pItem1->item.asString.value, pItem1->item.asString.length + pItem2->item.asString.length + 1 );
         memcpy( pItem1->item.asString.value+ pItem1->item.asString.length,
                 pItem2->item.asString.value, pItem2->item.asString.length );
         pItem1->item.asString.length += pItem2->item.asString.length;
         pItem1->item.asString.value[ pItem1->item.asString.length ] = '\0';
         if( pItem2->item.asString.value )
         {
            hb_xfree( pItem2->item.asString.value );
            pItem2->item.asString.value = NULL;
         }
         hb_stackPop();
         return;
      }
      else
         hb_errRT_BASE( EG_STROVERFLOW, 1209, NULL, "+" );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      WORD wDec2, wDec1;
      double dNumber1 = hb_vmPopDouble( &wDec2 );
      double dNumber2 = hb_vmPopDouble( &wDec1 );

      hb_vmPushNumber( dNumber1 + dNumber2, ( wDec1 > wDec2 ) ? wDec1 : wDec2 );
   }

   else if( IS_DATE( pItem1 ) && IS_DATE( pItem2 ) )
   {
      long lDate1 = hb_vmPopDate();
      long lDate2 = hb_vmPopDate();

      hb_vmPushDate( lDate1 + lDate2 );
   }

   else if( IS_DATE( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      WORD wDec;
      double dNumber2 = hb_vmPopDouble( &wDec );
      long lDate1 = hb_vmPopDate();

      hb_vmPushDate( lDate1 + dNumber2 );
   }

   else if( IS_OBJECT( pItem1 ) && hb_objHasMsg( pItem2, "+" ) )
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

   HB_DEBUG( "Plus\n" );
}

long hb_vmPopDate( void )
{
   hb_stackDec();

   if( IS_DATE( stack.pPos ) )
   {
      stack.pPos->type = IT_NIL;
      return stack.pPos->item.asDate.value;
   }
   else
   {
      hb_errInternal( 9999, "Incorrect item value trying to Pop a date value", NULL, NULL );
      return 0; /* To suppress compiler warning */
   }
}

/* Pops the item from the eval stack and uses it to select the current
 * workarea
 */
static void hb_vmPopAlias( void )
{
   PHB_ITEM pItem;

   hb_stackDec();
   pItem = stack.pPos;
   switch( pItem->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         /* Alias was used as integer value, for example: 4->field
          * or it was saved on the stack using hb_vmPushAlias()
          * or was evaluated from an expression, (nWorkArea)->field
          */
         hb_rddSelectWorkAreaNumber( pItem->item.asInteger.value );
         pItem->type = IT_NIL;
         break;

      case IT_SYMBOL:
         /* Alias was specified using alias identifier, for example: al->field
          */
         hb_rddSelectWorkAreaSymbol( pItem->item.asSymbol.value );
         pItem->type = IT_NIL;
         break;

      case IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         /* TODO: synchronize it with RDD API
         hb_SelectWorkAreaAlias( pItem->item.asString.value );
         */
         hb_itemClear( pItem );
         break;

      default:
         hb_itemClear( pItem );
         hb_errRT_BASE( EG_BADALIAS, 9999, NULL, NULL );
         break;
   }

   HB_DEBUG( "hb_vmPopAlias\n" );
}

static void hb_vmPopAliasedField( PHB_SYMB pSym )
{
   PHB_ITEM pAlias = stack.pPos - 1;
   int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

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

      case IT_SYMBOL:
         /* Alias was specified using alias identifier, for example: al->field
          */
         hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
         pAlias->type = IT_NIL;
         break;

      case IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         /* TODO: synchronize it with RDD API
         hb_SelectWorkAreaAlias( pAlias->item.asString.value );
         */
         hb_itemClear( pAlias );
         break;

      default:
         hb_itemClear( pAlias );
         hb_errRT_BASE( EG_BADALIAS, 9999, NULL, NULL );
         return;
   }

   hb_rddPutFieldValue( stack.pPos - 2, pSym );
   hb_rddSelectWorkAreaNumber( iCurrArea );
   hb_stackPop();    /* field */
   hb_stackPop();    /* alias */
   HB_DEBUG( "hb_vmPopAliasedField\n" );
}

double hb_vmPopDouble( WORD *pwDec )
{
   double dNumber;

   hb_stackDec();

   switch( stack.pPos->type & ~IT_BYREF )
   {
      case IT_INTEGER:
           dNumber = ( double ) stack.pPos->item.asInteger.value;
           *pwDec = 0;
           break;

      case IT_LONG:
           dNumber = ( double ) stack.pPos->item.asLong.value;
           *pwDec = 0;
           break;

      case IT_DOUBLE:
           dNumber = stack.pPos->item.asDouble.value;
           *pwDec = stack.pPos->item.asDouble.decimal;
           break;

      default:
           hb_errInternal( 9999, "Incorrect item type trying to Pop a double", NULL, NULL );
           break;
   }

   stack.pPos->type = IT_NIL;

   HB_DEBUG( "hb_vmPopDouble\n" );

   return dNumber;
}

static void hb_vmPopField( PHB_SYMB pSym )
{
   hb_rddPutFieldValue( stack.pPos - 1, pSym );
   hb_stackPop();
   HB_DEBUG( "hb_vmPopField\n" );
}

void hb_vmPopLocal( SHORT iLocal )
{
   PHB_ITEM pLocal;

   hb_stackDec();

   if( iLocal >= 0 )
    {
      /* local variable or local parameter */
      pLocal = stack.pBase + 1 + iLocal;
      if( IS_BYREF( pLocal ) )
         hb_itemCopy( hb_itemUnRef( pLocal ), stack.pPos );
      else
         hb_itemCopy( pLocal, stack.pPos );
    }
   else
      /* local variable referenced in a codeblock
       * stack.pBase+1 points to a codeblock that is currently evaluated
       */
      hb_itemCopy( hb_codeblockGetVar( stack.pBase + 1, iLocal ), stack.pPos );

   hb_itemClear( stack.pPos );
   HB_DEBUG( "hb_vmPopLocal\n" );
}

BOOL hb_vmPopLogical( void )
{
   if( IS_LOGICAL( stack.pPos - 1 ) )
   {
      hb_stackDec();

      stack.pPos->type = IT_NIL;
      return stack.pPos->item.asLogical.value;
   }
   else
   {
      hb_errRT_BASE( EG_ARG, 1066, NULL, hb_langDGetErrorDesc( EG_CONDITION ) );
      return FALSE;
   }
}

void hb_vmPopMemvar( PHB_SYMB pSym )
{
   hb_stackDec();
   hb_memvarSetValue( pSym, stack.pPos );
   hb_itemClear( stack.pPos );
   HB_DEBUG( "hb_vmPopMemvar\n" );
}

double hb_vmPopNumber( void )
{
   PHB_ITEM pItem = stack.pPos - 1;
   double dNumber;

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

   stack.pPos->type = IT_NIL;

   HB_DEBUG( "hb_vmPopNumber\n" );

   return dNumber;
}

void hb_vmPopParameter( PHB_SYMB pSym, BYTE bParam )
{
   hb_memvarSetValue( pSym, stack.pBase +1 +bParam );
   HB_DEBUG( "hb_vmPopParameter\n" );
}

void hb_vmPopStatic( WORD wStatic )
{
   PHB_ITEM pStatic;

   hb_stackDec();
   pStatic = aStatics.item.asArray.value->pItems + stack.iStatics + wStatic - 1;

   if( IS_BYREF( pStatic ) )
      hb_itemCopy( hb_itemUnRef( pStatic ), stack.pPos );
   else
      hb_itemCopy( pStatic, stack.pPos );

   hb_itemClear( stack.pPos );
   HB_DEBUG( "hb_vmPopStatic\n" );
}

void hb_vmPower( void )
{
   PHB_ITEM pItem2 = stack.pPos - 1;
   PHB_ITEM pItem1 = stack.pPos - 2;

   if( IS_NUMERIC( pItem2 ) && IS_NUMERIC( pItem1 ) )
   {
      WORD wDec1, wDec2;
      double d2 = hb_vmPopDouble( &wDec2 );
      double d1 = hb_vmPopDouble( &wDec1 );

      /* NOTE: Clipper always returns the result of power
               with the SET number of decimal places. */
      hb_vmPushNumber( pow( d1, d2 ), hb_set.HB_SET_DECIMALS );
   }
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

/* pushes current workarea number on the eval stack
 */
static void hb_vmPushAlias( void )
{
   stack.pPos->type = IT_INTEGER;
   stack.pPos->item.asInteger.value   = hb_rddGetCurrentWorkAreaNumber();
   stack.pPos->item.asInteger.length  = 10;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushAlias\n" );
}

static void hb_vmPushAliasedField( PHB_SYMB pSym )
{
   PHB_ITEM pAlias = stack.pPos - 1;
   int iCurrArea = hb_rddGetCurrentWorkAreaNumber();

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

      case IT_SYMBOL:
         /* Alias was specified using alias identifier, for example: al->field
          */
         hb_rddSelectWorkAreaSymbol( pAlias->item.asSymbol.value );
         pAlias->type = IT_NIL;
         break;

      case IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         /* TODO: synchronize it with RDD API
         hb_SelectWorkAreaAlias( pAlias->item.asString.value );
         */
         hb_itemClear( pAlias );
         break;

      default:
         hb_itemClear( pAlias );
         hb_errRT_BASE( EG_BADALIAS, 9999, NULL, NULL );
         return;
   }

   hb_rddGetFieldValue( pAlias, pSym );
   hb_rddSelectWorkAreaNumber( iCurrArea );
   HB_DEBUG( "hb_vmPushAliasedField\n" );
}

void hb_vmPushLogical( BOOL bValue )
{
   stack.pPos->type = IT_LOGICAL;
   stack.pPos->item.asLogical.value = bValue;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushLogical\n" );
}

static void hb_vmPushField( PHB_SYMB pSym )
{
   hb_rddGetFieldValue( stack.pPos, pSym );
   hb_stackPush();
   HB_DEBUG( "hb_vmPushField\n" );
}

void hb_vmPushLocal( SHORT iLocal )
{
   if( iLocal >= 0 )
   {
      PHB_ITEM pLocal;

      /* local variable or local parameter */
      pLocal = stack.pBase + 1 + iLocal;
      if( IS_BYREF( pLocal ) )
         hb_itemCopy( stack.pPos, hb_itemUnRef( pLocal ) );
      else
         hb_itemCopy( stack.pPos, pLocal );
   }
   else
      /* local variable referenced in a codeblock
       * stack.pBase+1 points to a codeblock that is currently evaluated
       */
     hb_itemCopy( stack.pPos, hb_codeblockGetVar( stack.pBase + 1, ( LONG ) iLocal ) );

   hb_stackPush();
   HB_DEBUG2( "hb_vmPushLocal %i\n", iLocal );
}

void hb_vmPushLocalByRef( SHORT iLocal )
{
   stack.pPos->type = IT_BYREF;
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   stack.pPos->item.asRefer.value = iLocal;
   stack.pPos->item.asRefer.offset = stack.pBase - stack.pItems +1;
   stack.pPos->item.asRefer.itemsbase = &stack.pItems;

   hb_stackPush();
   HB_DEBUG2( "hb_vmPushLocalByRef %i\n", iLocal );
}

void hb_vmPushMemvar( PHB_SYMB pSym )
{
   hb_memvarGetValue( stack.pPos, pSym );
   hb_stackPush();
   HB_DEBUG( "hb_vmPushMemvar\n" );
}

void hb_vmPushMemvarByRef( PHB_SYMB pSym )
{
   hb_memvarGetRefer( stack.pPos, pSym );
   hb_stackPush();
   HB_DEBUG( "hb_vmPushMemvar\n" );
}

void hb_vmPushNil( void )
{
   stack.pPos->type = IT_NIL;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushNil\n" );
}

void hb_vmPushNumber( double dNumber, WORD wDec )
{
   if( wDec )
      hb_vmPushDouble( dNumber, wDec );

   else if( SHRT_MIN <= dNumber && dNumber <= SHRT_MAX )
      hb_vmPushInteger( dNumber );

   else if( LONG_MIN <= dNumber && dNumber <= LONG_MAX )
      hb_vmPushLong( dNumber );

   else
      hb_vmPushDouble( dNumber, hb_set.HB_SET_DECIMALS );
}

void hb_vmPushStatic( WORD wStatic )
{
   PHB_ITEM pStatic;

   pStatic = aStatics.item.asArray.value->pItems + stack.iStatics + wStatic - 1;
   if( IS_BYREF( pStatic ) )
      hb_itemCopy( stack.pPos, hb_itemUnRef( pStatic ) );
   else
      hb_itemCopy( stack.pPos, pStatic );
   hb_stackPush();
   HB_DEBUG2( "hb_vmPushStatic %i\n", wStatic );
}

void hb_vmPushStaticByRef( WORD wStatic )
{
   stack.pPos->type = IT_BYREF;
   /* we store the offset instead of a pointer to support a dynamic stack */
   stack.pPos->item.asRefer.value = wStatic -1;
   stack.pPos->item.asRefer.offset = stack.iStatics;
   stack.pPos->item.asRefer.itemsbase = &aStatics.item.asArray.value->pItems;

   hb_stackPush();
   HB_DEBUG2( "hb_vmPushStaticByRef %i\n", wStatic );
}

void hb_vmPushString( char * szText, ULONG length )
{
   char * szTemp = ( char * ) hb_xgrab( length + 1 );

   memcpy( szTemp, szText, length );
   szTemp[ length ] = '\0';

   stack.pPos->type                 = IT_STRING;
   stack.pPos->item.asString.length = length;
   stack.pPos->item.asString.value  = szTemp;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushString\n" );
}

void hb_vmPushSymbol( PHB_SYMB pSym )
{
   stack.pPos->type   = IT_SYMBOL;
   stack.pPos->item.asSymbol.value = pSym;
   stack.pPos->item.asSymbol.stackbase   = stack.pPos - stack.pItems;
   hb_stackPush();
   HB_DEBUG2( "hb_vmPushSymbol: %s\n", pSym->szName );
}

void hb_vmPush( PHB_ITEM pItem )
{
   hb_itemCopy( stack.pPos, pItem );
   hb_stackPush();
   HB_DEBUG( "hb_vmPush\n" );
}

/* +0   -> HB_P_PUSHBLOCK
* +1 +2 -> size of codeblock
* +3 +4 -> number of expected parameters
* +5 +6 -> number of referenced local variables
* +7 -> start of table with referenced local variables
*/
void hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols )
{
   WORD wLocals;

   stack.pPos->type   = IT_BLOCK;

   wLocals = pCode[ 5 ] + ( pCode[ 6 ] * 256 );
   stack.pPos->item.asBlock.value =
         hb_codeblockNew( pCode + 7 + wLocals*2, /* pcode buffer         */
         wLocals,                                /* number of referenced local variables */
         ( WORD * )( pCode + 7 ),                  /* table with referenced local variables */
         pSymbols );

   /* store the statics base of function where the codeblock was defined
    */
   stack.pPos->item.asBlock.statics = stack.iStatics;
   /* store the number of expected parameters
    */
   stack.pPos->item.asBlock.paramcnt = pCode[ 3 ] + ( pCode[ 4 ] * 256 );
   /* store the line number where the codeblock was defined
    */
   stack.pPos->item.asBlock.lineno = stack.pBase->item.asSymbol.lineno;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushBlock\n" );
}

void hb_vmPushDate( LONG lDate )
{
   stack.pPos->type   = IT_DATE;
   stack.pPos->item.asDate.value = lDate;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushDate\n" );
}

void hb_vmPushDouble( double dNumber, WORD wDec )
{
   stack.pPos->type   = IT_DOUBLE;
   stack.pPos->item.asDouble.value = dNumber;
   if( dNumber >= 10000000000.0 ) stack.pPos->item.asDouble.length = 20;
   else stack.pPos->item.asDouble.length = 10;
   stack.pPos->item.asDouble.decimal = ( wDec > 9 ) ? 9 : wDec;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushDouble\n" );
}

void hb_vmPushInteger( int iNumber )
{
   stack.pPos->type = IT_INTEGER;
   stack.pPos->item.asInteger.value   = iNumber;
   stack.pPos->item.asInteger.length  = 10;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushInteger\n" );
}

void hb_vmPushLong( long lNumber )
{
   stack.pPos->type   = IT_LONG;
   stack.pPos->item.asLong.value   = lNumber;
   stack.pPos->item.asLong.length  = 10;
   hb_stackPush();
   HB_DEBUG( "hb_vmPushLong\n" );
}

void hb_vmRetValue( void )
{
   hb_stackDec();                               /* make the last item visible */
   hb_itemCopy( &stack.Return, stack.pPos ); /* copy it */
   hb_itemClear( stack.pPos );               /* now clear it */
   HB_DEBUG( "RetValue\n" );
}

void hb_stackPop( void )
{
   if( --stack.pPos < stack.pItems )
      hb_errInternal( 9999, "Stack underflow", NULL, NULL );

   if( stack.pPos->type != IT_NIL )
      hb_itemClear( stack.pPos );
}

void hb_stackDec( void )
{
   if( --stack.pPos < stack.pItems )
      hb_errInternal( 9999, "Stack underflow", NULL, NULL );
}

void hb_stackFree( void )
{
   hb_xfree( stack.pItems );
   HB_DEBUG( "hb_stackFree\n" );
}

void hb_stackPush( void )
{
   LONG CurrIndex;   /* index of current top item */
   LONG TopIndex;    /* index of the topmost possible item */

   CurrIndex = stack.pPos - stack.pItems;
   TopIndex  = stack.wItems - 1;

   /* enough room for another item ? */
   if( !( TopIndex > CurrIndex ) )
   {
      LONG BaseIndex;   /* index of stack base */

      BaseIndex = stack.pBase - stack.pItems;

      /* no, make more headroom: */
      /* hb_stackDispLocal(); */
      stack.pItems = ( PHB_ITEM ) hb_xrealloc( stack.pItems, sizeof( HB_ITEM ) *
                                ( stack.wItems + STACK_EXPANDHB_ITEMS ) );

      /* fix possibly invalid pointers: */
      stack.pPos = stack.pItems + CurrIndex;
      stack.pBase = stack.pItems + BaseIndex;
      stack.wItems += STACK_EXPANDHB_ITEMS;
      /* hb_stackDispLocal(); */
   }

   /* now, push it: */
   stack.pPos++;
   stack.pPos->type = IT_NIL;
   return;
}

void hb_stackInit( void )
{
   stack.pItems = ( PHB_ITEM ) hb_xgrab( sizeof( HB_ITEM ) * STACK_INITHB_ITEMS );
   stack.pBase  = stack.pItems;
   stack.pPos   = stack.pItems;     /* points to the first stack item */
   stack.wItems = STACK_INITHB_ITEMS;
   HB_DEBUG( "hb_stackInit\n" );
}

void hb_stackDispLocal( void )
{
   PHB_ITEM pBase;

   for( pBase = stack.pBase; pBase <= stack.pPos; pBase++ )
   {
      switch( pBase->type )
      {
         case IT_NIL:
              printf( "NIL " );
              break;

         case IT_ARRAY:
              if( pBase->item.asArray.value->wClass )
                 printf( "OBJECT " );
              else
                 printf( "ARRAY " );
              break;

         case IT_BLOCK:
              printf( "BLOCK " );
              break;

         case IT_DATE:
              printf( "DATE " );
              break;

         case IT_DOUBLE:
              printf( "DOUBLE " );
              break;

         case IT_LOGICAL:
              printf( "LOGICAL[%c] ", pBase->item.asLogical.value ? 'T' : 'F' );
              break;

         case IT_LONG:
              printf( "LONG" );
              break;

         case IT_INTEGER:
              printf( "INTEGER[%i] ", pBase->item.asInteger.value );
              break;

         case IT_STRING:
              printf( "STRING " );
              break;

         case IT_SYMBOL:
              printf( "SYMBOL(%s) ", pBase->item.asSymbol.value->szName );
              break;

         default:
              printf( "UNKNOWN[%i] ", pBase->type );
              break;
      }
   }

   printf( hb_consoleGetNewLine() );
}

void hb_stackDispCall( void )
{
   PHB_ITEM pBase = stack.pBase;

   while( pBase != stack.pItems )
   {
      pBase = stack.pItems + pBase->item.asSymbol.stackbase;

      if( ( pBase + 1 )->type == IT_ARRAY )
         printf( "Called from %s:%s(%i)", hb_objGetClsName( pBase + 1 ),
                 pBase->item.asSymbol.value->szName,
                 pBase->item.asSymbol.lineno );
      else
         printf( "Called from %s(%i)",
                 pBase->item.asSymbol.value->szName,
                 pBase->item.asSymbol.lineno );

      printf( hb_consoleGetNewLine() );
   }
}

void hb_vmSFrame( PHB_SYMB pSym )      /* sets the statics frame for a function */
{
   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   stack.iStatics = ( int ) pSym->pFunPtr; /* pSym is { "$_INITSTATICS", FS_INIT | FS_EXIT, _INITSTATICS } for each PRG */
   HB_DEBUG( "SFrame\n" );
}

void hb_vmStatics( PHB_SYMB pSym ) /* initializes the global aStatics array or redimensionates it */
{
   WORD wStatics = hb_vmPopNumber();

   if( IS_NIL( &aStatics ) )
   {
      pSym->pFunPtr = NULL;         /* statics frame for this PRG */
      hb_arrayNew( &aStatics, wStatics );
   }
   else
   {
      pSym->pFunPtr = ( PHB_FUNC )hb_arrayLen( &aStatics );
      hb_arraySize( &aStatics, hb_arrayLen( &aStatics ) + wStatics );
   }

   HB_DEBUG2( "Statics %li\n", hb_arrayLen( &aStatics ) );
}

/* Swaps two last items on the eval stack - the last item after swaping
 * is popped as current workarea number
 */
static void hb_vmSwapAlias( void )
{
   HB_ITEM_PTR pItem = stack.pPos -1;
   HB_ITEM_PTR pWorkArea = stack.pPos -2;

   switch( pWorkArea->type & ~IT_BYREF )
   {
      case IT_INTEGER:
         /* Alias was used as integer value, for example: 4->field
          * or it was saved on the stack using hb_vmPushAlias()
          * or was evaluated from an expression, (nWorkArea)->field
          */
         hb_rddSelectWorkAreaNumber( pWorkArea->item.asInteger.value );
         break;

      case IT_SYMBOL:
         /* Alias was specified using alias identifier, for example: al->field
          */
         hb_rddSelectWorkAreaSymbol( pItem->item.asSymbol.value );
         break;

      case IT_STRING:
         /* Alias was evaluated from an expression, for example: (cVar)->field
          */
         /* TODO: synchronize it with RDD API
         hb_rddSelectWorkAreaAlias( pWorkArea->item.asString.value );
         */
         hb_itemClear( pWorkArea );
         break;

      default:
         hb_itemClear( pWorkArea );
         hb_errRT_BASE( EG_BADALIAS, 9999, NULL, NULL );
         return;
   }

   memcpy( pWorkArea, pItem, sizeof( HB_ITEM ) );
   pItem->type = IT_NIL;
   hb_stackDec();

   HB_DEBUG( "hb_vmSwapAlias\n" );
}

void hb_vmProcessSymbols( PHB_SYMB pModuleSymbols, WORD wModuleSymbols ) /* module symbols initialization */
{
   PSYMBOLS pNewSymbols, pLastSymbols;
   WORD w;
   SYMBOLSCOPE hSymScope;

#ifdef HARBOUR_OBJ_GENERATION
   static BOOL bObjChecked = FALSE;

   if( ! bObjChecked )
   {
      bObjChecked = TRUE;
      hb_vmProcessObjSymbols();   /* to asure Harbour OBJ symbols are processed first */
   }
#endif

   pNewSymbols = ( PSYMBOLS ) hb_xgrab( sizeof( SYMBOLS ) );
   pNewSymbols->pModuleSymbols = pModuleSymbols;
   pNewSymbols->wModuleSymbols = wModuleSymbols;
   pNewSymbols->pNext = NULL;
   pNewSymbols->hScope = 0;

   if( s_pSymbols == NULL )
      s_pSymbols = pNewSymbols;
   else
   {
      pLastSymbols = s_pSymbols;
      while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
         pLastSymbols = pLastSymbols->pNext;
      pLastSymbols->pNext = pNewSymbols;
   }

   for( w = 0; w < wModuleSymbols; w++ ) /* register each public symbol on the dynamic symbol table */
   {
      hSymScope = ( pModuleSymbols + w )->cScope;
      pNewSymbols->hScope |= hSymScope;
      if( ( ! s_pSymStart ) && ( hSymScope == FS_PUBLIC ) )
         s_pSymStart = pModuleSymbols + w;  /* first public defined symbol to start execution */

      if( ( hSymScope == FS_PUBLIC ) || ( hSymScope & ( FS_MESSAGE | FS_MEMVAR ) ) )
         hb_dynsymNew( pModuleSymbols + w );
   }
}

#ifdef HARBOUR_OBJ_GENERATION
static void hb_vmProcessObjSymbols( void )
{
   POBJSYMBOLS pObjSymbols = ( POBJSYMBOLS ) &HB_FIRSTSYMBOL;

   static BOOL bDone = FALSE;

   if( ! bDone )
   {
      bDone = TRUE;
      while( pObjSymbols < ( POBJSYMBOLS ) ( &HB_LASTSYMBOL - 1 ) )
      {
         hb_vmProcessSymbols( pObjSymbols->pSymbols, pObjSymbols->wSymbols );
         pObjSymbols++;
      }
   }
}
#endif

static void hb_vmReleaseLocalSymbols( void )
{
   PSYMBOLS pDestroy;

   while( s_pSymbols )
   {
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
   WORD w;
   SYMBOLSCOPE scope;

   do
   {
      if( ( pLastSymbols->hScope & ( FS_INIT | FS_EXIT ) ) == ( FS_INIT | FS_EXIT ) )
      {
         for( w = 0; w < pLastSymbols->wModuleSymbols; w++ )
         {
            scope = ( pLastSymbols->pModuleSymbols + w )->cScope & ( FS_EXIT | FS_INIT );
            if( scope == ( FS_INIT | FS_EXIT ) )
            {
               /* _INITSTATICS procedure cannot call any function and it
               * cannot use any local variable then it is safe to call
               * this procedure directly
               * hb_vmPushSymbol( pLastSymbols->pModuleSymbols + w );
               * hb_vmPushNil();
               * hb_vmDo( 0 );
               */
               if( ( pLastSymbols->pModuleSymbols + w )->pFunPtr )
                  ( pLastSymbols->pModuleSymbols + w )->pFunPtr();
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   } while( pLastSymbols );
}

static void hb_vmDoExitFunctions( void )
{
   PSYMBOLS pLastSymbols = s_pSymbols;
   WORD w;
   SYMBOLSCOPE scope;

   do
   {
      if( pLastSymbols->hScope & FS_EXIT )
      {  /* only if module contains some EXIT functions */
         for( w = 0; w < pLastSymbols->wModuleSymbols; w++ )
         {
            scope = ( pLastSymbols->pModuleSymbols + w )->cScope & ( FS_EXIT | FS_INIT );
            if( scope == FS_EXIT )
            {
               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + w );
               hb_vmPushNil();
               hb_vmDo( 0 );
               if( s_wActionRequest )
                  /* QUIT or BREAK was issued - stop processing
                  */
                  return;
            }
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   } while( pLastSymbols );
}

static void hb_vmDoInitFunctions( int argc, char * argv[] )
{
   PSYMBOLS pLastSymbols = s_pSymbols;
   WORD w;
   SYMBOLSCOPE scope;

   do
   {
      if( pLastSymbols->hScope & FS_INIT )
      {  /* only if module contains some INIT functions */
         for( w = 0; w < pLastSymbols->wModuleSymbols; w++ )
         {
            scope = ( pLastSymbols->pModuleSymbols + w )->cScope & ( FS_EXIT | FS_INIT );
            if( scope == FS_INIT )
            {
               int i;

               hb_vmPushSymbol( pLastSymbols->pModuleSymbols + w );
               hb_vmPushNil();

               for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
                  hb_vmPushString( argv[ i ], strlen( argv[ i ] ) );

               hb_vmDo( argc - 1 );
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
   HB_ERRORSYS();
   HB_ERRORNEW();
}

/* ----------------------------- */
/* TODO: Put these to /source/rtl/?.c */

HARBOUR HB_LEN( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param( 1, IT_ANY );

      /* NOTE: pItem cannot be NULL here */

      switch( pItem->type )
      {
         case IT_ARRAY:
              hb_retnl( pItem->item.asArray.value->ulLen );
              break;

         case IT_STRING:
              hb_retnl( pItem->item.asString.length );
              break;

         default:
              hb_errRT_BASE( EG_ARG, 1111, NULL, "LEN" );
              break;
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "LEN" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_EMPTY( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param( 1, IT_ANY );

      /* NOTE: pItem cannot be NULL here */

      switch( pItem->type & ~IT_BYREF )
      {
         case IT_ARRAY:
              hb_retl( pItem->item.asArray.value->ulLen == 0 );
              break;

         case IT_STRING:
              hb_retl( hb_strEmpty( hb_parc( 1 ), hb_parclen( 1 ) ) );
              break;

         case IT_INTEGER:
              hb_retl( ! hb_parni( 1 ) );
              break;

         case IT_LONG:
              hb_retl( ! hb_parnl( 1 ) );
              break;

         case IT_DOUBLE:
              hb_retl( ! hb_parnd( 1 ) );
              break;

         case IT_DATE:
              hb_retl( atol( hb_pards( 1 ) ) == 0 );  /* Convert to long */
              break;

         case IT_LOGICAL:
              hb_retl( ! hb_parl( 1 ) );
              break;

         case IT_BLOCK:
              hb_retl( FALSE );
              break;

         default:
              hb_retl( TRUE );
              break;
      }
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "EMPTY" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_VALTYPE( void )
{
   if( hb_pcount() == 1 )
   {
      PHB_ITEM pItem = hb_param( 1, IT_ANY );

      /* NOTE: pItem cannot be NULL here */

      switch( pItem->type & ~IT_BYREF )
      {
         case IT_ARRAY:
              if( pItem->item.asArray.value->wClass )
                 hb_retc( "O" );  /* it is an object */
              else
                 hb_retc( "A" );
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
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "VALTYPE" ); /* NOTE: Clipper catches this at compile time! */
}

/* INCOMPATIBILITY: The Clipper NG states that WORD() will only work when used
                    in CALL commands parameter list, otherwise it will return
                    NIL, in Harbour it will work anywhere. */

HARBOUR HB_WORD( void )
{
   if( hb_pcount() == 1 )
   {
      if( ISNUM( 1 ) )
         hb_retni( hb_parni( 1 ) );
      else
         hb_errRT_BASE( EG_ARG, 1091, NULL, "WORD" );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "WORD" );

}

HARBOUR HB_PROCNAME( void )
{
   int iLevel = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = stack.pBase;
   char * szProcName;

   while( ( iLevel-- > 0 ) && pBase != stack.pItems )
      pBase = stack.pItems + pBase->item.asSymbol.stackbase;

   if( ( iLevel == -1 ) )
   {
      if( ( pBase + 1 )->type == IT_ARRAY )  /* it is a method name */
      {
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
   int iLevel  = hb_parni( 1 ) + 1;  /* we are already inside ProcName() */
   PHB_ITEM pBase = stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != stack.pItems )
      pBase = stack.pItems + pBase->item.asSymbol.stackbase;

   if( iLevel == -1 )
      hb_retni( pBase->item.asSymbol.lineno );
   else
      hb_retni( 0 );
}

void hb_vmRequestQuit( void )
{
   s_wActionRequest = HB_QUIT_REQUESTED;
}

HARBOUR HB___QUIT( void )
{
   hb_vmRequestQuit();
}

HARBOUR HB_ERRORLEVEL( void )
{
   BYTE byPrevValue = s_byErrorLevel;

   /* NOTE: This should be ISNUM( 1 ), but it's sort of a Clipper bug that it
            accepts other types also and consider them zero. */

   if( hb_pcount() > 0 )
      /* Only replace the error level if a parameter was passed */
      s_byErrorLevel = hb_parni( 1 );

   hb_retni( byPrevValue );
}

HARBOUR HB_PCOUNT( void )
{
   if( hb_pcount() == 0 )
   {
      PHB_ITEM pBase = stack.pItems + stack.pBase->item.asSymbol.stackbase;
      WORD  wRet  = pBase->item.asSymbol.paramcnt;                /* Skip current function     */

      hb_retni( wRet );
   }
   else
      hb_errRT_BASE( EG_ARGCOUNT, 3000, NULL, "PCOUNT" ); /* NOTE: Clipper catches this at compile time! */
}

HARBOUR HB_PVALUE( void )                               /* PValue( <nArg> )         */
{
   WORD  wParam = hb_parni( 1 );                  /* Get parameter            */
   PHB_ITEM pBase = stack.pItems + stack.pBase->item.asSymbol.stackbase;
                                                /* Skip function + self     */

   if( wParam && wParam <= pBase->item.asSymbol.paramcnt )     /* Valid number             */
      hb_itemReturn( pBase + 1 + wParam );
   else
      hb_errRT_BASE( EG_ARG, 3011, NULL, "PVALUE" );
}

void hb_vmRequestBreak( PHB_ITEM pItem )
{
   if( s_lRecoverBase )
   {
      if( pItem )
         hb_itemCopy( stack.pItems + s_lRecoverBase + HB_RECOVER_VALUE, pItem );
      s_wActionRequest = HB_BREAK_REQUESTED;
   }
   else
      s_wActionRequest = HB_QUIT_REQUESTED;
}

WORD hb_vmRequestQuery( void )
{
   return s_wActionRequest;
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
   if( hb_set.HB_SET_CANCEL )
   {
      printf( "\nCancelled at: %s (%i)\n", stack.pBase->item.asSymbol.value->szName, stack.pBase->item.asSymbol.lineno );
      s_wActionRequest = HB_QUIT_REQUESTED;
   }
}

/* $Id$
 *
 * The Harbour virtual machine
 */

/* Please note the following comments we may use everywhere
   TODO: something should be added here
   TOFIX: something needs to be fixed
   OBSOLETE: something could be removed from here
   QUESTION: I had some questions at this point but I could not get an answer
   OPT: something is commented out to improve performance
   As an example: */

/* TODO: Add all the TODO comments. */

#include <limits.h>
#include <malloc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hbsetup.h"    /* main configuration file */
#include <extend.h>
#include <pcode.h>
#include <set.h>

HARBOUR ERRORSYS( void );
HARBOUR ERRORNEW( void );
HARBOUR EVAL( void );         /* Evaluates a codeblock from Harbour */
HARBOUR MAIN( void );         /* fixed entry point by now */
HARBOUR VALTYPE( void );      /* returns a string description of a value */

extern void InitializeConsole(void); /* This prototype is needed by C++ compilers */
extern void InitSymbolTable(void);   /* This prototype is needed by C++ compilers */

/* currently supported virtual machine actions */
void And( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
void ArrayAt( void );         /* pushes an array element to the stack, removing the array and the index from the stack */
void ArrayPut( void );        /* sets an array value and pushes the value on to the stack */
void Dec( void );             /* decrements the latest numeric value on the stack */
void Div( void );             /* divides the latest two values on the stack, removes them and leaves the result */
void Do( WORD WParams );      /* invoke the virtual machine */
HARBOUR DoBlock( void );      /* executes a codeblock */
void Duplicate( void );       /* duplicates the latest value on the stack */
void EndBlock( void );        /* copies the last codeblock pushed value into the return value */
void Equal( BOOL bExact );    /* checks if the two latest values on the stack are equal, removes both and leaves result */
void ForTest( void );         /* test for end condition of for */
void Frame( BYTE bLocals, BYTE bParams );  /* increases the stack pointer for the amount of locals and params suplied */
void FuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the satck */
void Function( WORD wParams ); /* executes a function saving its result */
void GenArray( WORD wElements ); /* generates a wElements Array and fills it from the stack values */
void Greater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
void GreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
void Inc( void );             /* increment the latest numeric value on the stack */
void Instring( void );        /* check whether string 1 is contained in string 2 */
void ItemCopy( PITEM pDest, PITEM pSource ); /* copies an item to one place to another respecting its containts */
void Less( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
void LessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
void Line( WORD wLine );      /* keeps track of the currently processed PRG line */
void Message( PSYMBOL pSymMsg ); /* sends a message to an object */
void Minus( void );           /* substracts the latest two values on the stack, removes them and leaves the result */
void Modulus( void );         /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
void Mult( void );            /* multiplies the latest two values on the stack, removes them and leaves the result */
void Negate( void );          /* negates (-) the latest value on the stack */
void Not( void );             /* changes the latest logical value on the stack */
void NotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
void OperatorCall( PITEM, PITEM, char *); /* call an overloaded operator */
void Or( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */
void Plus( void );            /* sums the latest two values on the stack, removes them and leaves the result */
long PopDate( void );         /* pops the stack latest value and returns its date value as a LONG */
void PopDefStat( WORD wStatic ); /* pops the stack latest value onto a static as default init */
double PopDouble( void );     /* pops the stack latest value and returns its double numeric format value */
void PopLocal( SHORT wLocal );      /* pops the stack latest value onto a local */
int  PopLogical( void );           /* pops the stack latest value and returns its logical value */
double PopNumber( void );          /* pops the stack latest value and returns its numeric value */
void PopStatic( WORD wStatic );    /* pops the stack latest value onto a static */
void Power( void );            /* power the latest two values on the stack, removes them and leaves the result */
void Push( PITEM pItem );     /* pushes a generic item onto the stack */
void PushBlock( BYTE * pCode, WORD wSize, WORD wParam, PSYMBOL pSymbols ); /* creates a codeblock */
void PushDate( LONG lDate );   /* pushes a long date onto the stack */
void PushDouble( double lNumber, WORD wDec ); /* pushes a double number onto the stack */
void PushLocal( SHORT iLocal );     /* pushes the containts of a local onto the stack */
void PushLocalByRef( SHORT iLocal ); /* pushes a local by refrence onto the stack */
void PushLogical( int iTrueFalse ); /* pushes a logical value onto the stack */
void PushLong( long lNumber ); /* pushes a long number onto the stack */
void PushNil( void );            /* in this case it places nil at self */
void PushNumber( double dNumber, WORD wDec ); /* pushes a number on to the stack and decides if it is integer, long or double */
void PushStatic( WORD wStatic );   /* pushes the containts of a static onto the stack */
void PushString( char * szText, WORD wLength );  /* pushes a string on to the stack */
void PushSymbol( PSYMBOL pSym ); /* pushes a function pointer onto the stack */
void PushInteger( int iNumber ); /* pushes a integer number onto the stack */
void RetValue( void );           /* pops the latest stack value into stack.Return */
void SFrame( PSYMBOL pSym );     /* sets the statics frame for a function */
void Statics( PSYMBOL pSym );    /* increases the the global statics array to hold a PRG statics */

typedef struct _SYMBOLS
{
   PSYMBOL pModuleSymbols; /* pointer to a one module own symbol table */
   WORD    wModuleSymbols; /* number of symbols on that table */
   struct _SYMBOLS * pNext;/* pointer to the next SYMBOLS structure */
} SYMBOLS, * PSYMBOLS;     /* structure to keep track of all modules symbol tables */

void ProcessSymbols( PSYMBOL pSymbols, WORD wSymbols ); /* statics symbols initialization */
void DoInitFunctions( int argc, char * argv[] ); /* executes all defined PRGs INIT functions */
void DoExitFunctions( void ); /* executes all defined PRGs EXIT functions */
void LogSymbols( void );         /* displays all dynamic symbols */
void ReleaseClasses( void );       /* releases all defined classes */
void ReleaseLocalSymbols( void );  /* releases the memory of the local symbols linked list */
void ReleaseDynamicSymbols( void ); /* releases the memory of the dynamic symbol table */
void ReleaseSets( void ); /* releases Sets consumed memory */

/* stack management functions */
void StackPop( void );        /* pops an item from the stack */
void StackFree( void );       /* releases all memory used by the stack */
void StackPush( void );       /* pushes an item on to the stack */
void StackInit( void );       /* initializes the stack */
void StackShow( void );       /* show the types of the items on the stack for debugging purposes */

PCODEBLOCK CodeblockNew( BYTE *, WORD, PSYMBOL, int, WORD );
void CodeblockDelete( PCODEBLOCK );
PITEM CodeblockGetVar( PITEM, SHORT );
void CodeblockEvaluate( PCODEBLOCK );
void CodeblockCopy( PITEM, PITEM );
void CodeblockDetach( PCODEBLOCK );

void InitSymbolTable( void );   /* initialization of runtime support symbols */

static void ForceLink( void );

ULONG hb_isMessage( PITEM, char * );
ULONG hb_strAt( char *, long, char *, long );
PITEM hb_itemReturn( PITEM );

#define STACK_INITITEMS   100
#define STACK_EXPANDITEMS  20

extern ULONG ulMemoryBlocks;      /* memory blocks used */
extern ULONG ulMemoryMaxBlocks;   /* maximum number of used memory blocks */
extern ULONG ulMemoryConsumed;    /* memory size consumed */
extern ULONG ulMemoryMaxConsumed; /* memory max size consumed */

#ifdef HARBOUR_OBJ_GENERATION
void ProcessObjSymbols ( void ); /* process Harbour generated OBJ symbols */

typedef struct
{
   WORD wSymbols;                 /* module local symbol table symbols amount */
   PSYMBOL pSymbols;              /* module local symbol table address */
} OBJSYMBOLS, * POBJSYMBOLS;      /* structure used from Harbour generated OBJs */

#ifdef __cplusplus
extern "C" POBJSYMBOLS HB_FIRSTSYMBOL, HB_LASTSYMBOL;
#else
extern POBJSYMBOLS HB_FIRSTSYMBOL, HB_LASTSYMBOL;
#endif
#endif

STACK stack;
int iHBDEBUG = 0;      /* if 1 traces the virtual machine activity */
SYMBOL symEval = { "__EVAL", FS_PUBLIC, DoBlock, 0 }; /* symbol to evaluate codeblocks */
PSYMBOL pSymStart;     /* start symbol of the application. MAIN() is not required */
ITEM aStatics;         /* Harbour array to hold all application statics variables */
ITEM errorBlock;       /* errorblock */
PSYMBOLS pSymbols = 0; /* to hold a linked list of all different modules symbol tables */
BOOL bQuit = FALSE;    /* inmediately exit the application */
BYTE bErrorLevel = 0;  /* application exit errorlevel */

#define HBDEBUG( x )     if( iHBDEBUG ) printf( x )
#define HBDEBUG2( x, y ) if( iHBDEBUG ) printf( x, y )

void PCode_AND( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__ARRAYAT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__ARRAYPUT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__EQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__ENDBLOCK( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__ENDPROC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__EXACTLYEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__FALSE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__FORTEST( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__FUNCTION( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__FRAME( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__FUNCPTR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__GENARRAY( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__GREATER( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__GREATEREQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__DEC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__DIMARRAY( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__DIVIDE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__DO( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__DUPLICATE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__INC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__INSTRING( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__JUMP( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__JUMPFALSE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__JUMPTRUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__LESSEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__LESS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__LINE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__MESSAGE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__MINUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__MODULUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__MULT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__NEGATE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__NOT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__NOTEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode_OR_( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PLUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POP( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POPDEFSTAT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POPLOCAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POPMEMVAR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POPSTATIC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__POWER( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHBLOCK( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHINT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHLOCAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHLOCALREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHLONG( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHMEMVAR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHMEMVARREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHNIL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHDOUBLE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHSELF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHSTATIC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHSTATICREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHSTR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHSYM( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__PUSHWORD( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__RETVALUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__SFRAME( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__STATICS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__TRUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );
void PCode__ZERO( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize );

void  (*pPCode_Handlers[])( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize ) = { &PCode_AND,
                                                                                                                  &PCode__ARRAYAT,
                                                                                                                  &PCode__ARRAYPUT,
                                                                                                                  &PCode__EQUAL,
                                                                                                                  &PCode__ENDBLOCK,
                                                                                                                  &PCode__ENDPROC,
                                                                                                                  &PCode__EXACTLYEQUAL,
                                                                                                                  &PCode__FALSE,
                                                                                                                  &PCode__FORTEST,
                                                                                                                  &PCode__FUNCTION,
                                                                                                                  &PCode__FRAME,
                                                                                                                  &PCode__FUNCPTR,
                                                                                                                  &PCode__GENARRAY,
                                                                                                                  &PCode__GREATER,
                                                                                                                  &PCode__GREATEREQUAL,
                                                                                                                  &PCode__DEC,
                                                                                                                  &PCode__DIMARRAY,
                                                                                                                  &PCode__DIVIDE,
                                                                                                                  &PCode__DO,
                                                                                                                  &PCode__DUPLICATE,
                                                                                                                  &PCode__INC,
                                                                                                                  &PCode__INSTRING,
                                                                                                                  &PCode__JUMP,
                                                                                                                  &PCode__JUMPFALSE,
                                                                                                                  &PCode__JUMPTRUE,
                                                                                                                  &PCode__LESSEQUAL,
                                                                                                                  &PCode__LESS,
                                                                                                                  &PCode__LINE,
                                                                                                                  &PCode__MESSAGE,
                                                                                                                  &PCode__MINUS,
                                                                                                                  &PCode__MODULUS,
                                                                                                                  &PCode__MULT,
                                                                                                                  &PCode__NEGATE,
                                                                                                                  &PCode__NOT,
                                                                                                                  &PCode__NOTEQUAL,
                                                                                                                  &PCode_OR_,
                                                                                                                  &PCode__PLUS,
                                                                                                                  &PCode__POP,
                                                                                                                  &PCode__POPDEFSTAT,
                                                                                                                  &PCode__POPLOCAL,
                                                                                                                  &PCode__POPMEMVAR,
                                                                                                                  &PCode__POPSTATIC,
                                                                                                                  &PCode__POWER,
                                                                                                                  &PCode__PUSHBLOCK,
                                                                                                                  &PCode__PUSHINT,
                                                                                                                  &PCode__PUSHLOCAL,
                                                                                                                  &PCode__PUSHLOCALREF,
                                                                                                                  &PCode__PUSHLONG,
                                                                                                                  &PCode__PUSHMEMVAR,
                                                                                                                  &PCode__PUSHMEMVARREF,
                                                                                                                  &PCode__PUSHNIL,
                                                                                                                  &PCode__PUSHDOUBLE,
                                                                                                                  &PCode__PUSHSELF,
                                                                                                                  &PCode__PUSHSTATIC,
                                                                                                                  &PCode__PUSHSTATICREF,
                                                                                                                  &PCode__PUSHSTR,
                                                                                                                  &PCode__PUSHSYM,
                                                                                                                  &PCode__PUSHWORD,
                                                                                                                  &PCode__RETVALUE,
                                                                                                                  &PCode__SFRAME,
                                                                                                                  &PCode__STATICS,
                                                                                                                  &PCode__TRUE,
                                                                                                                  &PCode__ZERO };

/* application entry point */

#ifdef WINDOWS
   int __stdcall WinMain( long hIns, long hPrev, char * szCmds, int iCmdShow )
   {
      int argc = 1;
      char * argv[] = { "Test" };
#else
   int main( int argc, char * argv[] )
   {
#endif
   int i;
   void ( * DontDiscardForceLink )( void ) = &ForceLink;

   if( ! DontDiscardForceLink )  /* just to avoid warnings from the C compiler */
      iHBDEBUG += ( int ) DontDiscardForceLink; /* just to avoid warnings from the C compiler */

   HBDEBUG( "main\n" );
   aStatics.wType     = IT_NIL;
   errorBlock.wType   = IT_NIL;
   stack.Return.wType = IT_NIL;
   StackInit();
   NewDynSym( &symEval );  /* initialize dynamic symbol for evaluating codeblocks */
   InitializeSets();       /* initialize Sets */
   InitializeConsole();    /* initialize Console */
#ifdef HARBOUR_OBJ_GENERATION
   ProcessObjSymbols(); /* initialize Harbour generated OBJs symbols */
#endif

   /* Initialize symbol table with runtime support functions */
   InitSymbolTable();

   DoInitFunctions( argc, argv ); /* process defined INIT functions */

#ifdef HARBOUR_START_PROCEDURE
   {
     PDYNSYM pDynSym =FindDynSym( HARBOUR_START_PROCEDURE );
     if( pDynSym )
       pSymStart =pDynSym->pSymbol;
     else
     {
       printf( "Can\'t locate the starting procedure: \'%s\'", HARBOUR_START_PROCEDURE );
       exit(1);
    }
   }
#endif

   PushSymbol( pSymStart ); /* pushes first FS_PUBLIC defined symbol to the stack */

   PushNil();               /* places NIL at self */

   for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
      PushString( argv[ i ], strlen( argv[ i ] ) );

   Do( argc - 1 );         /* invoke it with number of supplied parameters */

   DoExitFunctions();      /* process defined EXIT functions */

   ItemRelease( &stack.Return );
   hb_arrayRelease( &aStatics );
   ItemRelease( &errorBlock );
   ReleaseClasses();
   ReleaseLocalSymbols();       /* releases the local modules linked list */
   ReleaseDynamicSymbols();     /* releases the dynamic symbol table */
   ReleaseSets();               /* releases Sets */
   StackFree();
   /* LogSymbols(); */
   HBDEBUG( "Done!\n" );

   if( ulMemoryBlocks )
   {
      printf( "\n\ntotal memory blocks allocated: %lu\n", ulMemoryMaxBlocks );
      printf( "memory maximum size consumed: %ld\n", ulMemoryMaxConsumed );
      printf( "memory blocks not released: %ld\n", ulMemoryBlocks );
      printf( "memory size not released: %ld\n", ulMemoryConsumed );
   }

   return bErrorLevel;
}

void VirtualMachine( PBYTE pCode, PSYMBOL pSymbols )
{
   BYTE bCode;
   WORD w = 0, wParams, wSize;

   HBDEBUG( "VirtualMachine\n" );

   while( ( bCode = pCode[ w ] ) != _ENDPROC && ! bQuit )
   {
      pPCode_Handlers[bCode]( pCode, pSymbols, &bCode, &w, &wParams, &wSize  );
   }
   HBDEBUG( "EndProc\n" );
}

void And( void )
{
   PITEM pItem2 = stack.pPos - 1;
   PITEM pItem1 = stack.pPos - 2;
   PITEM pError;
   int   iResult;

   HBDEBUG( "And\n" );

   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      iResult = pItem1->value.iLogical && pItem2->value.iLogical;
      StackPop();
      StackPop();
      PushLogical( iResult );
   }
   else
   {
      pError = _errNew();
      _errPutDescription( pError, "Argument error: conditional" );
      _errLaunch( pError );
      _errRelease( pError );
   }
}

void ArrayAt( void )
{
   double dIndex = PopNumber();
   PITEM pArray  = stack.pPos - 1;
   ITEM item;

   hb_arrayGet( pArray, dIndex, &item );
   StackPop();

   ItemCopy( stack.pPos, &item );
   ItemRelease( &item );
   StackPush();
}

void ArrayPut( void )
{
   PITEM pValue = stack.pPos - 1;
   PITEM pIndex = stack.pPos - 2;
   PITEM pArray = stack.pPos - 3;
   ULONG ulIndex;

   if( IS_INTEGER( pIndex ) )
      ulIndex = pIndex->value.iNumber;

   else if( IS_LONG( pIndex ) )
      ulIndex = pIndex->value.lNumber;

   else if( IS_DOUBLE( pIndex ) )
      ulIndex = pIndex->value.dNumber;

   else ;
      /* QUESTION: Should we raise an error here ? */

   hb_arraySet( pArray, ulIndex, pValue );
   ItemCopy( pArray, pValue );  /* places pValue at pArray position */
   StackPop();
   StackPop();
}

void Dec( void )
{
   double dNumber;
   LONG lDate;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
      dNumber = PopDouble();
      PushNumber( --dNumber, stack.pPos->wDec );
   }
   else if( IS_DATE( stack.pPos - 1 ) )
   {
      lDate = PopDate();
      PushDate( --lDate ); /* TOFIX: Dates should decreased other way */
   }
   /* TODO: Should we check other types here and issue an error ? */
}

void Div( void )
{
   double d2 = PopDouble();
   double d1 = PopDouble();

   PushNumber( d1 / d2, hb_set.HB_SET_DECIMALS );
}

void Do( WORD wParams )
{
   PITEM pItem = stack.pPos - wParams - 2;
   PSYMBOL pSym = pItem->value.pSymbol;
   WORD wStackBase = stack.pBase - stack.pItems; /* as the stack memory block could change */
   WORD wItemIndex = pItem - stack.pItems;
   PITEM pSelf = stack.pPos - wParams - 1;
   HARBOURFUNC pFunc;
   int iStatics = stack.iStatics;              /* Return iStatics position */

   if( ! IS_SYMBOL( pItem ) )
   {
      StackShow();
      printf( "symbol item expected as a base from Do()\n" );
      exit( 1 );
   }

   if( ! ( ( IS_NIL( pSelf ) ) || ( IS_BLOCK( pSelf ) ) || ( IS_ARRAY( pSelf ) ) ) )
   {
      StackShow();
      printf( "invalid symbol type for self from Do()\n" );
      exit( 1 );
   }

   pItem->wLine   = 0;
   pItem->wParams = wParams;
   stack.pBase    = stack.pItems + pItem->wBase;
   pItem->wBase   = wStackBase;

   HBDEBUG2( "Do with %i params\n", wParams );

   if( IS_OBJECT( pSelf ) ) /* are we sending a message to an object ? */
   {
      pFunc = GetMethod( pSelf, pSym );
      if( ! pFunc )
      {
         printf( "error: message %s not implemented for class %s\n", pSym->szName,
                 _GetClassName( pSelf ) );
         exit( 1 );
      }
      pFunc();
   }
   else                     /* it is a function */
   {
      pFunc = pSym->pFunPtr;
      if( ! pFunc )
      {
         printf( "error: invalid function pointer (%s) from Do()\n", pSym->szName );
         exit( 1 );
      }
      pFunc();
   }

   while( stack.pPos > stack.pItems + wItemIndex )
      StackPop();

   stack.pBase = stack.pItems + wStackBase;
   stack.iStatics = iStatics;
}

HARBOUR DoBlock( void )
{
   PITEM pBlock = stack.pBase + 1;
   WORD wStackBase = stack.pBase - stack.pItems; /* as the stack memory block could change */
   int iParam;

   if( ! IS_BLOCK( pBlock ) )
   {
      printf( "error: codeblock expected from DoBlock()\n" );
      exit( 1 );
   }

   /* Check for valid count of parameters */
   iParam =pBlock->wParams -_pcount();
   /* add missing parameters */
   while( iParam-- > 0 )
     PushNil();

   /* set pBaseCB to point to local variables of a function where
    * the codeblock was defined
    */
   stack.pBase->wLine =pBlock->wLine;

   CodeblockEvaluate( (PCODEBLOCK)pBlock->value.pCodeblock );

   /* restore stack pointers */
   stack.pBase = stack.pItems + wStackBase;

   HBDEBUG( "End of DoBlock\n" );
}

void Duplicate( void )
{
   ItemCopy( stack.pPos, stack.pPos - 1 );
   StackPush();
}

HARBOUR EVAL( void )
{
   PITEM pBlock = _param( 1, IT_BLOCK );

   if( pBlock )
   {
      WORD w;

      PushSymbol( &symEval );
      Push( pBlock );

      for( w = 2; w <= _pcount(); w++ )
         Push( _param( w, IT_ANY ) );

      Do( _pcount() - 1 );
   }
   else
   {
      printf( "Not a valid codeblock on eval\n" );
      exit( 1 );
   }
}

void EndBlock( void )
{
   StackPop();
   ItemCopy( &stack.Return, stack.pPos );
   HBDEBUG( "EndBlock\n" );
}

void Equal( BOOL bExact )
{
   PITEM pItem2 = stack.pPos - 1;
   PITEM pItem1 = stack.pPos - 2;
   int i;

   if( IS_NIL( pItem1 ) && IS_NIL( pItem2 ) )
   {
      StackPop();
      StackPop();
      PushLogical( 1 );
   }

   else if ( IS_NIL( pItem1 ) || IS_NIL( pItem2 ) )
   {
      StackPop();
      StackPop();
      PushLogical( 0 );
   }

   else if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      i = hb_itemStrCmp( pItem1, pItem2, bExact );
      StackPop();
      StackPop();
      PushLogical( i == 0 );
   }

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      PushLogical( PopLogical() == PopLogical() );

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
      PushLogical( PopDouble() == PopDouble() );

   else if( IS_OBJECT( pItem1 ) && hb_isMessage( pItem1, "==" ) )
      OperatorCall( pItem1, pItem2, "==" );

   else if( pItem1->wType != pItem2->wType )
   {
      printf( "types not match on equal operation\n" );
      exit( 1 );
   }

   else
      PushLogical( 0 );
}

static void ForceLink( void )  /* To force the link of some functions */
{
   ERRORSYS();
   ERRORNEW();
}

void ForTest( void )        /* Test to check the end point of the FOR */
{
   double dStep;
   int    iEqual;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
       dStep = PopNumber();
       if( dStep > 0 )           /* Positive loop. Use LESS */
           Less();
       else if( dStep < 0 )      /* Negative loop. Use GREATER */
           Greater();
       else
           printf( "step of zero will cause endless loop" );
                                 /* Add some break code or so... */
       iEqual = PopLogical();    /* Logical should be on top of stack */
       PushNumber( dStep, stack.pPos->wDec );   /* Push the step expression back on the stack */
       PushLogical( iEqual );
   }
   else
       printf( "step expression should be numerical" );
}

void Frame( BYTE bLocals, BYTE bParams )
{
   int i, iTotal = bLocals + bParams;

   HBDEBUG( "Frame\n" );
   if( iTotal )
      for( i = 0; i < ( iTotal - stack.pBase->wParams ); i++ )
         PushNil();
}

void FuncPtr( void )  /* pushes a function address pointer. Removes the symbol from the satck */
{
   PITEM pItem = stack.pPos - 1;

   if( IS_SYMBOL( pItem ) )
   {
      StackPop();
      PushLong( ( ULONG ) pItem->value.pSymbol->pFunPtr );
   }
   else
   {
      printf( "symbol item expected from FuncPtr()\n" );
      exit( 1 );
   }
}

void Function( WORD wParams )
{
   Do( wParams );
   ItemCopy( stack.pPos, &stack.Return );
   StackPush();
}

void GenArray( WORD wElements ) /* generates a wElements Array and fills it from the stack values */
{
   ITEM itArray;
   WORD w;

   itArray.wType = IT_NIL;
   hb_arrayNew( &itArray, wElements );
   for( w = 0; w < wElements; w++ )
      ItemCopy( ( ( PBASEARRAY ) itArray.value.pBaseArray )->pItems + w,
                stack.pPos - wElements + w );

   for( w = 0; w < wElements; w++ )
      StackPop();

   ItemCopy( stack.pPos, &itArray );
   ItemRelease( &itArray );
   StackPush();
}

void Greater( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;
   int iLogical1, iLogical2;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      StackPop();
      StackPop();
      PushLogical( i > 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = PopNumber();
      dNumber1 = PopNumber();
      PushLogical( dNumber1 > dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushLogical( lDate1 > lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      iLogical1 = PopLogical();
      iLogical2 = PopLogical();
      PushLogical( iLogical1 > iLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_isMessage( stack.pPos - 2, ">" ) )
      OperatorCall( stack.pPos - 2, stack.pPos - 1, ">" );

   else if( ( stack.pPos - 2 )->wType != ( stack.pPos - 1 )->wType )
   {
      printf( "types not match on greater operation\n" );
      exit( 1 );
   }
}

void GreaterEqual( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;
   int iLogical1, iLogical2;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      StackPop();
      StackPop();
      PushLogical( i >= 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = PopNumber();
      dNumber1 = PopNumber();
      PushLogical( dNumber1 >= dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushLogical( lDate1 >= lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      iLogical1 = PopLogical();
      iLogical2 = PopLogical();
      PushLogical( iLogical1 >= iLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_isMessage( stack.pPos - 2, ">=" ) )
      OperatorCall( stack.pPos - 2, stack.pPos - 1, ">=" );

   else if( ( stack.pPos - 2 )->wType != ( stack.pPos - 1 )->wType )
   {
      printf( "types not match on greaterequal operation\n" );
      exit( 1 );
   }
}

void Inc( void )
{
   double dNumber;
   LONG lDate;
   PITEM pError;

   if( IS_NUMERIC( stack.pPos - 1 ) )
   {
      dNumber = PopDouble();
      PushNumber( ++dNumber, stack.pPos->wDec );
   }
   else if( IS_DATE( stack.pPos - 1 ) )
   {
      lDate = PopDate();
      PushDate( ++lDate );
   }
   else
   {
      pError = _errNew();
      _errPutDescription( pError, "Error BASE/1086  Argument error: ++" );
      _errLaunch( pError );
   }
}

void ItemRelease( PITEM pItem )
{
   if( IS_STRING( pItem ) )
   {
      if( pItem->value.szText )
      {
         _xfree( pItem->value.szText );
         pItem->value.szText = 0;
      }
      pItem->wLength = 0;
   }
   else if( IS_ARRAY( pItem ) && pItem->value.pBaseArray )
   {
      if( --( ( PBASEARRAY ) pItem->value.pBaseArray )->wHolders == 0 )
         hb_arrayRelease( pItem );
   }
   else if( IS_BLOCK( pItem ) )
   {
      CodeblockDelete( ( PCODEBLOCK ) pItem->value.pCodeblock );
   }
   pItem->wType = IT_NIL;
}

void Instring( void )
{
   PITEM pItem1 = stack.pPos - 2;
   PITEM pItem2 = stack.pPos - 1;
   int   iResult;
   ULONG ul;

   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      for( iResult = 0, ul = 0; !iResult && ul < (ULONG) pItem1->wLength; ul++ )
         iResult = hb_strAt( pItem1->value.szText + ul, 1,
                             pItem2->value.szText, pItem2->wLength );
      StackPop();
      StackPop();
      PushLogical( iResult == 0 ? 0 : 1 );
   }
   else
   {
      PITEM pError = _errNew();

      _errPutDescription( pError, "Error BASE/1109  Argument error: $" );
      _errLaunch( pError );
   }
}

void ItemCopy( PITEM pDest, PITEM pSource )
{
   ItemRelease( pDest );

   if( pDest == pSource )
   {
      printf( "an item was going to be copied to itself from ItemCopy()\n" );
      exit( 1 );
   }

   memcpy( pDest, pSource, sizeof( ITEM ) );

   if( IS_STRING( pSource ) )
   {
      pDest->value.szText = ( char * ) _xgrab( pSource->wLength + 1 );
      memcpy( pDest->value.szText, pSource->value.szText, pSource->wLength );
      pDest->value.szText[ pSource->wLength ] = 0;
   }

   else if( IS_ARRAY( pSource ) )
      ( ( PBASEARRAY ) pSource->value.pBaseArray )->wHolders++;

   else if( IS_BLOCK( pSource ) )
   {
      CodeblockCopy( pDest, pSource );
   }
}

void Less( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;
   int iLogical1, iLogical2;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      StackPop();
      StackPop();
      PushLogical( i < 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = PopNumber();
      dNumber1 = PopNumber();
      PushLogical( dNumber1 < dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushLogical( lDate1 < lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      iLogical1 = PopLogical();
      iLogical2 = PopLogical();
      PushLogical( iLogical1 < iLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_isMessage( stack.pPos - 2, "<" ) )
      OperatorCall( stack.pPos - 2, stack.pPos - 1, "<" );

   else if( ( stack.pPos - 2 )->wType != ( stack.pPos - 1 )->wType )
   {
      printf( "types not match on less operation\n" );
      exit( 1 );
   }
}

void LessEqual( void )
{
   double dNumber1, dNumber2;
   LONG lDate1, lDate2;
   int i;
   int iLogical1, iLogical2;

   if( IS_STRING( stack.pPos - 2 ) && IS_STRING( stack.pPos - 1 ) )
   {
      i = hb_itemStrCmp( stack.pPos - 2, stack.pPos - 1, FALSE );
      StackPop();
      StackPop();
      PushLogical( i <= 0 );
   }

   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      dNumber2 = PopNumber();
      dNumber1 = PopNumber();
      PushLogical( dNumber1 <= dNumber2 );
   }

   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushLogical( lDate1 <= lDate2 );
   }

   else if( IS_LOGICAL( stack.pPos - 1 ) && IS_LOGICAL( stack.pPos -2 ) )
   {
      iLogical1 = PopLogical();
      iLogical2 = PopLogical();
      PushLogical( iLogical1 <= iLogical2 );
   }

   else if( IS_OBJECT( stack.pPos - 2 ) &&
            hb_isMessage( stack.pPos - 2, "<=" ) )
      OperatorCall( stack.pPos - 2, stack.pPos - 1, "<=" );

   else if( ( stack.pPos - 2 )->wType != ( stack.pPos - 1 )->wType )
   {
      printf( "types not match on lessequal operation\n" );
      exit( 1 );
   }
}

void Message( PSYMBOL pSymMsg ) /* sends a message to an object */
{
   ItemCopy( stack.pPos, stack.pPos - 1 ); /* moves the object forward */
   ItemRelease( stack.pPos - 1 );
   ( stack.pPos - 1 )->wType = IT_SYMBOL;
   ( stack.pPos - 1 )->value.pSymbol = pSymMsg;
   ( stack.pPos - 1 )->wBase = ( stack.pPos - 1 ) - stack.pItems;
   StackPush();
   HBDEBUG2( "Message: %s\n", pSymMsg->szName );
}

void Line( WORD wLine )
{
   stack.pBase->wLine = wLine;
   HBDEBUG( "line\n" );
}

void Negate( void )
{
   if( IS_INTEGER( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->value.iNumber = -( stack.pPos - 1 )->value.iNumber;

   else if( IS_LONG( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->value.lNumber = -( stack.pPos - 1 )->value.lNumber;

   else if( IS_DOUBLE( stack.pPos - 1 ) )
      ( stack.pPos - 1 )->value.dNumber = -( stack.pPos - 1 )->value.dNumber;
}

void Not( void )
{
   PITEM pItem = stack.pPos - 1;

   if( IS_LOGICAL( pItem ) )
      pItem->value.iLogical = ! pItem->value.iLogical;
   else
      ; /* TODO: Raise an error here ? */
}

void NotEqual( void )
{
   PITEM pItem2 = stack.pPos - 1;
   PITEM pItem1 = stack.pPos - 2;
   int i;

   if( IS_NIL( pItem1 ) && IS_NIL( pItem2 ) )
   {
      StackPop();
      StackPop();
      PushLogical( 0 );
   }

   else if ( IS_NIL( pItem1 ) || IS_NIL( pItem2 ) )
   {
      StackPop();
      StackPop();
      PushLogical( 1 );  /* TOFIX: Is this correct ? */
   }

   else if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      i = hb_itemStrCmp( pItem1, pItem2, FALSE );
      StackPop();
      StackPop();
      PushLogical( i != 0 );
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
      PushLogical( PopDouble() != PopDouble() );

   else if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
      PushLogical( PopLogical() != PopLogical() );

   else if( IS_OBJECT( pItem1 ) && hb_isMessage( pItem1, "!=" ) )
      OperatorCall( pItem1, pItem2, "!=" );

   else if( pItem1->wType != pItem2->wType )
   {
      printf( "types not match on equal operation\n" );
      exit( 1 );
   }

   else
      PushLogical( 1 );
}

void Minus( void )
{
   double dNumber1, dNumber2;
   long lDate1, lDate2;

   if( IS_NUMERIC( stack.pPos - 1 ) && IS_NUMERIC( stack.pPos - 2 ) )
   {
      WORD wDec2, wDec1;
      dNumber2 = PopNumber();
      wDec2 = stack.pPos->wDec;
      dNumber1 = PopNumber();
      wDec1 = stack.pPos->wDec;
      PushNumber( dNumber1 - dNumber2, (wDec1 > wDec2) ? wDec1 : wDec2 );
   }
   else if( IS_DATE( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushNumber( lDate1 - lDate2, hb_set.HB_SET_DECIMALS );
   }
   else if( IS_NUMERIC( stack.pPos - 1 ) && IS_DATE( stack.pPos - 2 ) )
   {
      dNumber2 = PopNumber();
      lDate1 = PopDate();
      PushDate( lDate1 - dNumber2 );
   }
   else if( IS_OBJECT( stack.pPos - 2 ) && hb_isMessage( stack.pPos - 2, "-" ) )
      OperatorCall( stack.pPos - 2, stack.pPos - 1, "-" );

   /* TODO: We should substract strings also ? and generate an error it types
      don't match */
}

void Modulus( void )
{
   double d2 = PopDouble();
   double d1 = PopDouble();

   PushNumber( ( long ) d1 % ( long ) d2, hb_set.HB_SET_DECIMALS );
}

void Mult( void )
{
   WORD wDec2, wDec1;
   double d1, d2 = PopDouble();
   wDec2 = stack.pPos->wDec;
   d1 = PopDouble();
   wDec1 = stack.pPos->wDec;

   PushNumber( d1 * d2, wDec1 + wDec2 );
}

void OperatorCall( PITEM pItem1, PITEM pItem2, char *szSymbol )
{
   Push( pItem1 );                             /* Push object              */
   Message( GetDynSym( szSymbol )->pSymbol );  /* Push operation           */
   Push( pItem2 );                             /* Push argument            */
   Function( 1 );
}

void Or( void )
{
   PITEM pItem2 = stack.pPos - 1;
   PITEM pItem1 = stack.pPos - 2;
   PITEM pError;
   int   iResult;

   if( IS_LOGICAL( pItem1 ) && IS_LOGICAL( pItem2 ) )
   {
      iResult = pItem1->value.iLogical || pItem2->value.iLogical;
      StackPop();
      StackPop();
      PushLogical( iResult );
   }
   else
   {
      pError = _errNew();
      _errPutDescription( pError, "Argument error: conditional" );
      _errLaunch( pError );
      _errRelease( pError );
   }
}

void Plus( void )
{
   PITEM pItem1 = stack.pPos - 2;
   PITEM pItem2 = stack.pPos - 1;
   double dNumber1, dNumber2;
   long lDate1, lDate2;

   if( IS_STRING( pItem1 ) && IS_STRING( pItem2 ) )
   {
      pItem1->value.szText = (char*)_xrealloc( pItem1->value.szText, pItem1->wLength + pItem2->wLength + 1 );
      memcpy( pItem1->value.szText + pItem1->wLength,
              pItem2->value.szText, pItem2->wLength );
      pItem1->wLength += pItem2->wLength;
      pItem1->value.szText[ pItem1->wLength ] = 0;
      if( pItem2->value.szText )
      {
         _xfree( pItem2->value.szText );
         pItem2->value.szText = 0;
      }
      StackPop();
      return;
   }

   else if( IS_NUMERIC( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      WORD wDec2, wDec1;
      dNumber2 = PopDouble();
      wDec2 = stack.pPos->wDec;
      dNumber1 = PopDouble();
      wDec1 = stack.pPos->wDec;

      PushNumber( dNumber1 + dNumber2, (wDec1 > wDec2) ? wDec1 : wDec2 );
   }

   else if( IS_DATE( pItem1 ) && IS_DATE( pItem2 ) )
   {
      lDate2 = PopDate();
      lDate1 = PopDate();
      PushDate( lDate1 + lDate2 );
   }

   else if( IS_DATE( pItem1 ) && IS_NUMERIC( pItem2 ) )
   {
      dNumber2 = PopDouble();
      lDate1 = PopDate();
      PushDate( lDate1 + dNumber2 );
   }

   else if( IS_OBJECT( pItem1 ) && hb_isMessage( pItem2, "+" ) )
      OperatorCall( pItem1, pItem2, "+" );

   /* TODO: Generate an error if types don't match */
   HBDEBUG( "Plus\n" );
}

long PopDate( void )
{
   StackPop();

   if( IS_DATE( stack.pPos ) )
      return stack.pPos->value.lDate;
   else
   {
      printf( "incorrect item value trying to Pop a date value\n" );
      exit( 1 );
      return 0;
   }
}

void PopDefStat( WORD wStatic )     /* Pops a default value to a STATIC */
{
   PITEM pStatic;

   StackPop();
   pStatic = ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems + stack.iStatics +
             wStatic - 1;

   if( IS_BYREF( pStatic ) )
   {
      if( ( stack.pItems + pStatic->value.wItem )->wType == IT_NIL )
                                    /* Only initialize when NIL */
         ItemCopy( stack.pItems + pStatic->value.wItem, stack.pPos );
   }
   else
      if( pStatic->wType == IT_NIL ) /* Only initialize when NIL */
         ItemCopy( pStatic, stack.pPos );

   ItemRelease( stack.pPos );
   HBDEBUG( "PopDefStat\n" );
}

double PopDouble( void )
{
   double d;

   StackPop();

   switch( stack.pPos->wType & ~IT_BYREF )
   {
      case IT_INTEGER:
           d = stack.pPos->value.iNumber;
           break;

      case IT_LONG:
           d = stack.pPos->value.lNumber;
           break;

      case IT_DOUBLE:
           d = stack.pPos->value.dNumber;
           break;

      default:
           printf( "Incorrect item type trying to Pop a double\n" );
           exit( 1 );
           d = 0;
   }
   HBDEBUG( "PopDouble\n" );
   return d;
}

void PopLocal( SHORT iLocal )
{
   PITEM pLocal;

   StackPop();

   if( iLocal >= 0 )
    {
      /* local variable or local parameter */
      pLocal = stack.pBase + 1 + iLocal;
      if( IS_BYREF( pLocal ) )
          ItemCopy( stack.pItems + pLocal->value.wItem, stack.pPos );
      else
          ItemCopy( pLocal, stack.pPos );
    }
   else
      /* local variable referenced in a codeblock */
      ItemCopy( CodeblockGetVar( stack.pBase + 1, iLocal ), stack.pPos );

   ItemRelease( stack.pPos );
   HBDEBUG( "PopLocal\n" );
}

int PopLogical( void )
{
   PITEM pError;

   StackPop();

   if( IS_LOGICAL( stack.pPos ) )
      return stack.pPos->value.iLogical;
   else
   {
      pError = _errNew();
      _errPutDescription( pError, "Argument error: conditional" );
      _errLaunch( pError );
      _errRelease( pError );
      return 0;
   }
}

double PopNumber( void )
{
   PITEM pItem = stack.pPos - 1;
   double dNumber;

   StackPop();

   switch( pItem->wType & ~IT_BYREF )
   {
      case IT_INTEGER:
           dNumber = ( double ) pItem->value.iNumber;
           break;

      case IT_LONG:
           dNumber = ( double ) pItem->value.lNumber;
           break;

      case IT_DOUBLE:
           dNumber = pItem->value.dNumber;
           break;

      default:
           printf( "Incorrect item on the stack trying to pop a number\n" );
           exit( 1 );
           break;
   }
   return dNumber;
}

void PopStatic( WORD wStatic )
{
   PITEM pStatic;

   StackPop();
   pStatic = ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems + stack.iStatics +
             wStatic - 1;

   if( IS_BYREF( pStatic ) )
      ItemCopy( stack.pItems + pStatic->value.wItem, stack.pPos );
   else
      ItemCopy( pStatic, stack.pPos );

   ItemRelease( stack.pPos );
   HBDEBUG( "PopStatic\n" );
}

void Power( void )
{
   double d2 = PopDouble();
   double d1 = PopDouble();

   PushNumber( pow( d1, d2 ), hb_set.HB_SET_DECIMALS );
}

void PushLogical( int iTrueFalse )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType    = IT_LOGICAL;
   stack.pPos->value.iLogical = iTrueFalse;
   StackPush();
   HBDEBUG( "PushLogical\n" );
}

void PushLocal( SHORT iLocal )
{
   PITEM pLocal;

   if( iLocal >= 0 )
   {
      /* local variable or local parameter */
      pLocal = stack.pBase + 1 + iLocal;
      if( IS_BYREF( pLocal ) )
         ItemCopy( stack.pPos, stack.pItems + pLocal->value.wItem );
      else
         ItemCopy( stack.pPos, pLocal );
   }
   else
      /* local variable referenced in a codeblock */
     ItemCopy( stack.pPos, CodeblockGetVar( stack.pBase + 1, iLocal ) );
   StackPush();
   HBDEBUG2( "PushLocal %i\n", iLocal );
}

void PushLocalByRef( SHORT iLocal )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType = IT_BYREF;
   /* we store its stack offset instead of a pointer to support a dynamic stack */
   if( iLocal >= 0 )
      /* local variable or local parameter */
      stack.pPos->value.wItem = stack.pBase + 1 + iLocal - stack.pItems;
   else
      /* local variable referenced in a codeblock */
      stack.pPos->value.wItem = iLocal;

   StackPush();
   HBDEBUG2( "PushLocalByRef %i\n", iLocal );
}

void PushNil( void )
{
   ItemRelease( stack.pPos );
   StackPush();
   HBDEBUG( "PushNil\n" );
}

void PushNumber( double dNumber, WORD wDec )
{
   if( wDec )
      PushDouble( dNumber, wDec );

   else if( SHRT_MIN <= dNumber && dNumber <= SHRT_MAX )
      PushInteger( dNumber );

   else if( LONG_MIN <= dNumber && dNumber <= LONG_MAX )
      PushLong( dNumber );

   else
      PushDouble( dNumber, hb_set.HB_SET_DECIMALS );
}

void PushStatic( WORD wStatic )
{
   ItemCopy( stack.pPos, ( ( PBASEARRAY ) aStatics.value.pBaseArray )->pItems +
             stack.iStatics + wStatic - 1 );
   StackPush();
   HBDEBUG2( "PushStatic %i\n", wStatic );
}

void PushString( char * szText, WORD wLength )
{
   char * szTemp = ( char * ) _xgrab( wLength + 1 );

   memcpy (szTemp, szText, wLength);
   szTemp[ wLength ] = 0;

   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_STRING;
   stack.pPos->wLength = wLength;
   stack.pPos->value.szText  = szTemp;
   StackPush();
   HBDEBUG( "PushString\n" );
}

void PushSymbol( PSYMBOL pSym )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_SYMBOL;
   stack.pPos->value.pSymbol = pSym;
   stack.pPos->wBase   = stack.pPos - stack.pItems;
   StackPush();
   HBDEBUG2( "PushSymbol: %s\n", pSym->szName );
}

void Push( PITEM pItem )
{
   ItemCopy( stack.pPos, pItem );
   StackPush();
   HBDEBUG( "Push\n" );
}

void PushBlock( BYTE * pCode, WORD wSize, WORD wParam, PSYMBOL pSymbols )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_BLOCK;
   stack.pPos->value.pCodeblock = CodeblockNew( pCode, wSize, pSymbols,
      stack.iStatics, stack.pBase - stack.pItems );
   /* store the stack base of function where the codeblock was defined */
   stack.pPos->wBase   = stack.pBase - stack.pItems;
   /* store the number of expected parameters */
   stack.pPos->wParams = wParam;
   /* store the line number where the codeblock was defined */
   stack.pPos->wLine   = stack.pBase->wLine;
   StackPush();
   HBDEBUG( "PushBlock\n" );
}

void PushDate( LONG lDate )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_DATE;
   stack.pPos->value.lDate = lDate;
   StackPush();
   HBDEBUG( "PushDate\n" );
}

void PushDouble( double dNumber, WORD wDec )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_DOUBLE;
   stack.pPos->value.dNumber = dNumber;
   if( dNumber >= 10000000000.0 ) stack.pPos->wLength = 20;
   else stack.pPos->wLength = 10;
   stack.pPos->wDec = (wDec > 9) ? 9 : wDec;
   StackPush();
   HBDEBUG( "PushDouble\n" );
}

void PushInteger( int iNumber )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType = IT_INTEGER;
   stack.pPos->value.iNumber = iNumber;
   stack.pPos->wLength = 10;
   stack.pPos->wDec    = 0;
   StackPush();
   HBDEBUG( "PushInteger\n" );
}

void PushLong( long lNumber )
{
   ItemRelease( stack.pPos );
   stack.pPos->wType   = IT_LONG;
   stack.pPos->value.lNumber = lNumber;
   stack.pPos->wLength = 10;
   stack.pPos->wDec    = 0;
   StackPush();
   HBDEBUG( "PushLong\n" );
}

void RetValue( void )
{
   StackPop();
   ItemCopy( &stack.Return, stack.pPos );
   if( stack.Return.wType == IT_BLOCK )
      CodeblockDetach( (PCODEBLOCK)stack.Return.value.pCodeblock );
   HBDEBUG( "RetValue\n" );
}

void StackPop( void )
{
   ItemRelease( stack.pPos );

   if( --stack.pPos < stack.pItems )
   {
      printf( "runtime error: stack underflow\n" );
      exit( 1 );
   }
}

void StackFree( void )
{
   _xfree( stack.pItems );
   HBDEBUG( "StackFree\n" );
}

void StackPush( void )
{
   LONG CurrIndex,   /* index of current top item */
        TopIndex;    /* index of the topmost possible item */

   CurrIndex = stack.pPos - stack.pItems;
   TopIndex  = stack.wItems - 1;

   /* enough room for another item ? */
   if( !( TopIndex > CurrIndex ) )
   {
      LONG BaseIndex;   /* index of stack base */

      BaseIndex = stack.pBase - stack.pItems;

      /* no, make more headroom: */
      /* StackShow(); */
      stack.pItems = (PITEM)_xrealloc( stack.pItems, sizeof( ITEM ) *
                                ( stack.wItems + STACK_EXPANDITEMS ) );

      /* fix possibly invalid pointers: */
      stack.pPos = stack.pItems + CurrIndex;
      stack.pBase = stack.pItems + BaseIndex;
      stack.wItems += STACK_EXPANDITEMS;
      /* StackShow(); */
   }

   /* now, push it: */
   stack.pPos++;
   stack.pPos->wType = IT_NIL;
   return;
}

void StackInit( void )
{
   stack.pItems = ( PITEM ) _xgrab( sizeof( ITEM ) * STACK_INITITEMS );
   stack.pBase  = stack.pItems;
   stack.pPos   = stack.pItems;     /* points to the first stack item */
   stack.wItems = STACK_INITITEMS;
   HBDEBUG( "StackInit\n" );
}

 void StackShow( void )
{
   PITEM p;

   for( p = stack.pBase; p <= stack.pPos; p++ )
   {
      switch( p->wType )
      {
         case IT_NIL:
              printf( "NIL " );
              break;

         case IT_ARRAY:
              if( ( ( PBASEARRAY ) p->value.pBaseArray )->wClass )
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
              printf( "LOGICAL[%i] ", p->value.iLogical );
              break;

         case IT_LONG:
              break;

         case IT_INTEGER:
              printf( "INTEGER[%i] ", p->value.iNumber );
              break;

         case IT_STRING:
              printf( "STRING " );
              break;

         case IT_SYMBOL:
              printf( "SYMBOL(%s) ", p->value.pSymbol->szName );
              break;

         default:
              printf( "DUNNO[%i] ", p->wType );
              break;
      }
   }
   printf( "\n" );
}

void SFrame( PSYMBOL pSym )      /* sets the statics frame for a function */
{
   /* _INITSTATICS is now the statics frame. Statics() changed it! */
   stack.iStatics = ( int ) pSym->pFunPtr; /* pSym is { "_INITSTATICS", FS_INIT, _INITSTATICS } for each PRG */
   HBDEBUG( "SFrame\n" );
}

void Statics( PSYMBOL pSym ) /* initializes the global aStatics array or redimensionates it */
{
   WORD wStatics = PopNumber();

   if( IS_NIL( &aStatics ) )
   {
      pSym->pFunPtr = 0;         /* statics frame for this PRG */
      hb_arrayNew( &aStatics, wStatics );
   }
   else
   {
      pSym->pFunPtr = ( HARBOURFUNC )hb_arrayLen( &aStatics );
      hb_arraySize( &aStatics, hb_arrayLen( &aStatics ) + wStatics );
   }

   HBDEBUG2( "Statics %li\n", hb_arrayLen( &aStatics ) );
}

void ProcessSymbols( PSYMBOL pModuleSymbols, WORD wModuleSymbols ) /* module symbols initialization */
{
   PSYMBOLS pNewSymbols, pLastSymbols;
   WORD w;
#ifdef HARBOUR_OBJ_GENERATION
   static int iObjChecked = 0;

   if( ! iObjChecked )
   {
      iObjChecked = 1;
      ProcessObjSymbols();   /* to asure Harbour OBJ symbols are processed first */
   }
#endif

   pNewSymbols = ( PSYMBOLS ) _xgrab( sizeof( SYMBOLS ) );
   pNewSymbols->pModuleSymbols = pModuleSymbols;
   pNewSymbols->wModuleSymbols = wModuleSymbols;
   pNewSymbols->pNext = 0;

   if( ! pSymbols )
      pSymbols = pNewSymbols;
   else
   {
      pLastSymbols = pSymbols;
      while( pLastSymbols->pNext ) /* locates the latest processed group of symbols */
         pLastSymbols = pLastSymbols->pNext;
      pLastSymbols->pNext = pNewSymbols;
   }

   for( w = 0; w < wModuleSymbols; w++ ) /* register each public symbol on the dynamic symbol table */
   {
      if( ( ! pSymStart ) && ( ( pModuleSymbols + w )->cScope == FS_PUBLIC ) )
         pSymStart = pModuleSymbols + w;  /* first public defined symbol to start execution */

      if( ( ( pModuleSymbols + w )->cScope == FS_PUBLIC ) ||
          ( ( pModuleSymbols + w )->cScope & FS_MESSAGE ) )
         NewDynSym( pModuleSymbols + w );
   }
}

#ifdef HARBOUR_OBJ_GENERATION
void ProcessObjSymbols( void )
{
   POBJSYMBOLS pObjSymbols = ( POBJSYMBOLS ) &HB_FIRSTSYMBOL;

   static int iDone = 0;

   if( ! iDone )
   {
      iDone = 1;
      while( pObjSymbols < ( POBJSYMBOLS ) ( &HB_LASTSYMBOL - 1 ) )
      {
         ProcessSymbols( pObjSymbols->pSymbols, pObjSymbols->wSymbols );
         pObjSymbols++;
      }
   }
}
#endif

void ReleaseLocalSymbols( void )
{
   PSYMBOLS pDestroy;

   while( pSymbols )
   {
      pDestroy = pSymbols;
      pSymbols = pSymbols->pNext;
      _xfree( pDestroy );
   }
}

void DoExitFunctions( void )
{
   PSYMBOLS pLastSymbols = pSymbols;
   WORD w;

   do {
      for( w = 0; w < pLastSymbols->wModuleSymbols; w++ )
      {
         if( ( pLastSymbols->pModuleSymbols + w )->cScope & FS_EXIT )
         {
            PushSymbol( pLastSymbols->pModuleSymbols + w );
            PushNil();
            Do( 0 );
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   } while( pLastSymbols );
}

void DoInitFunctions( int argc, char * argv[] )
{
   PSYMBOLS pLastSymbols = pSymbols;
   WORD w;

   do {
      for( w = 0; w < pLastSymbols->wModuleSymbols; w++ )
      {
         if( ( pLastSymbols->pModuleSymbols + w )->cScope & FS_INIT )
         {
            int i;

            PushSymbol( pLastSymbols->pModuleSymbols + w );
            PushNil();

            for( i = 1; i < argc; i++ ) /* places application parameters on the stack */
               PushString( argv[ i ], strlen( argv[ i ] ) );

            Do( argc - 1 );
         }
      }
      pLastSymbols = pLastSymbols->pNext;
   } while( pLastSymbols );
}

HARBOUR LEN( void )
{
   PITEM pItem;

   if( _pcount() )
   {
      pItem = _param( 1, IT_ANY );

      switch( pItem->wType )
      {
         case IT_ARRAY:
              _retnl( ( ( PBASEARRAY ) pItem->value.pBaseArray )->ulLen );
              break;

         case IT_STRING:
              _retnl( pItem->wLength );
              break;

         default:
              _retni( 0 );  /* QUESTION: Should we raise an error here ? */
              break;
      }
   }
   else
      _retni( 0 );  /* QUESTION: Should we raise an error here ? */
}

HARBOUR EMPTY()
{
   PITEM pItem = _param( 1, IT_ANY );

   if( pItem )
   {
      switch( pItem->wType & ~IT_BYREF )
      {
         case IT_ARRAY:
              _retl( ( ( PBASEARRAY ) pItem->value.pBaseArray )->ulLen == 0 );
              break;

         case IT_STRING:
              _retl( hb_strempty( _parc( 1 ), _parclen( 1 ) ) );
              break;

         case IT_INTEGER:
              _retl( ! _parni( 1 ) );
              break;

         case IT_LONG:
              _retl( ! _parnl( 1 ) );
              break;

         case IT_DOUBLE:
              _retl( ! _parnd( 1 ) );
              break;

         case IT_DATE:
             _retl( atol( _pards( 1 ) ) == 0 );  /* Convert to long */
              break;

         case IT_LOGICAL:
              _retl( ! _parl( 1 ) );
              break;

         case IT_BLOCK:
              _retl( FALSE );
              break;

         default:
              _retl( TRUE );
              break;
      }
   }
   else
      _retl( TRUE );
}


HARBOUR VALTYPE( void )
{
   PITEM pItem;

   if( _pcount() )
   {
      pItem = _param( 1, IT_ANY );

      switch( pItem->wType & ~IT_BYREF )
      {
         case IT_ARRAY:
              if( ( ( PBASEARRAY ) pItem->value.pBaseArray )->wClass )
                 _retc( "O" );  /* it is an object */
              else
                 _retc( "A" );
              break;

         case IT_BLOCK:
              _retc( "B" );
              break;

         case IT_DATE:
              _retc( "D" );
              break;

         case IT_LOGICAL:
              _retc( "L" );
              break;

         case IT_INTEGER:
         case IT_LONG:
         case IT_DOUBLE:
              _retc( "N" );
              break;

         case IT_STRING:
              _retc( "C" );
              break;

         case IT_NIL:
         default:
              _retc( "U" );
              break;
      }
   }
   else
      _retc( "U" );
}

HARBOUR ERRORBLOCK()
{
   ITEM oldError;
   PITEM pNewErrorBlock = _param( 1, IT_BLOCK );

   oldError.wType = IT_NIL;
   ItemCopy( &oldError, &errorBlock );

   if( pNewErrorBlock )
      ItemCopy( &errorBlock, pNewErrorBlock );

   ItemCopy( &stack.Return, &oldError );
   ItemRelease( &oldError );
}

HARBOUR PROCNAME()
{
   int iLevel = _parni( 1 ) + 1;  /* we are already inside ProcName() */
   PITEM pBase = stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != stack.pItems )
      pBase = stack.pItems + pBase->wBase;

   if( ( iLevel == -1 ) )
      _retc( pBase->value.pSymbol->szName );
   else
      _retc( "" );
}

HARBOUR PROCLINE()
{
   int iLevel  = _parni( 1 ) + 1;  /* we are already inside ProcName() */
   PITEM pBase = stack.pBase;

   while( ( iLevel-- > 0 ) && pBase != stack.pItems )
      pBase = stack.pItems + pBase->wBase;

   if( iLevel == -1 )
      _retni( pBase->wLine );
   else
      _retni( 0 );
}

HARBOUR __QUIT()
{
   bQuit = TRUE;
}

HARBOUR ERRORLEVEL()
{
   BYTE bPrevValue = bErrorLevel;

   bErrorLevel = _parni( 1 );
   _retni( bPrevValue );
}

HARBOUR PCOUNT()
{
   PITEM pBase = stack.pItems + stack.pBase->wBase;
   WORD  wRet  = pBase->wParams;                /* Skip current function     */

   _retni( wRet );
}

HARBOUR PVALUE()                                /* PValue( <nArg> )         */
{
   WORD  wParam = _parni( 1 );                  /* Get parameter            */
   PITEM pBase = stack.pItems + stack.pBase->wBase;
                                                /* Skip function + self     */

   if( wParam && wParam <= pBase->wParams )     /* Valid number             */
      hb_itemReturn( pBase + 1 + wParam );
   else
   {
      PITEM pError = _errNew();

      _errPutDescription(pError, "Argument error: PVALUE");
      _errLaunch(pError);
      _errRelease(pError);
      _ret();
   }
}

void PCode_AND( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   And();
   (*w)++;
}

void PCode__ARRAYAT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   ArrayAt();
   (*w)++;
}

void PCode__ARRAYPUT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   ArrayPut();
   (*w)++;
}

void PCode__EQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Equal( FALSE );
   (*w)++;
}

void PCode__ENDBLOCK( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   EndBlock();
   HBDEBUG( "EndProc\n" );
}

void PCode__ENDPROC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__EXACTLYEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Equal( TRUE );
   (*w)++;
}

void PCode__FALSE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushLogical( 0 );
   (*w)++;
}

void PCode__FORTEST( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   ForTest();
   (*w)++;
}

void PCode__FUNCTION( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Function( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__FRAME( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Frame( pCode[ (*w) + 1 ], pCode[ (*w) + 2 ] );
   (*w) += 3;
}

void PCode__FUNCPTR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   FuncPtr();
   (*w)++;
}

void PCode__GENARRAY( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   GenArray( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__GREATER( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Greater();
   (*w)++;
}

void PCode__GREATEREQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   GreaterEqual();
   (*w)++;
}

void PCode__DEC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Dec();
   (*w)++;
}

void PCode__DIMARRAY( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__DIVIDE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Div();
   (*w)++;
}

void PCode__DO( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Do( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__DUPLICATE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Duplicate();
   (*w)++;
}

void PCode__INC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Inc();
   (*w)++;
}

void PCode__INSTRING( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Instring();
   (*w)++;
}

void PCode__JUMP( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wParams) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   if( (*wParams) )
      (*w) += (*wParams);
   else
      (*w) += 3;
}

void PCode__JUMPFALSE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   if( ! PopLogical() )
      (*w) += pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   else
      (*w) += 3;
}

void PCode__JUMPTRUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   if( PopLogical() )
      (*w) += pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   else
      (*w) += 3;
}

void PCode__LESSEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   LessEqual();
   (*w)++;
}

void PCode__LESS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Less();
   (*w)++;
}

void PCode__LINE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Line( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__MESSAGE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wParams) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   Message( pSymbols + (*wParams) );
   (*w) += 3;
}

void PCode__MINUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Minus();
   (*w)++;
}

void PCode__MODULUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Modulus();
   (*w)++;
}

void PCode__MULT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Mult();
   (*w)++;
}

void PCode__NEGATE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Negate();
   (*w)++;
}

void PCode__NOT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Not();
   (*w)++;
}

void PCode__NOTEQUAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   NotEqual();
   (*w)++;
}

void PCode_OR_( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Or();
   (*w)++;
}

void PCode__PLUS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Plus();
   (*w)++;
}

void PCode__POP( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   StackPop();
   (*w)++;
}

void PCode__POPDEFSTAT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PopDefStat( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__POPLOCAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PopLocal( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__POPMEMVAR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__POPSTATIC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PopStatic( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__POWER( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Power();
   (*w)++;
}

void PCode__PUSHBLOCK( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   /* +0    -> _pushblock
    * +1 +2 -> size of codeblock
    * +3 +4 -> number of expected parameters
    * +5 +6 -> number of referenced local variables
    * +7 -> start of table with referenced local variables
    */
   (*wSize) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   PushBlock( pCode + (*w) + 5,
              (*wSize) - 5,
              pCode[ (*w) + 3 ] + ( pCode[ (*w) + 4 ] * 256 ),
              pSymbols );
   (*w) += (*wSize);
}

void PCode__PUSHINT( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushInteger( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__PUSHLOCAL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushLocal( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__PUSHLOCALREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushLocalByRef( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__PUSHLONG( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushLong( * ( long * ) ( &pCode[ (*w) + 1 ] ) );
   (*w) += 5;
}

void PCode__PUSHMEMVAR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__PUSHMEMVARREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__PUSHNIL( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushNil();
   (*w)++;
}

void PCode__PUSHDOUBLE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushDouble( * ( double * ) ( &pCode[ (*w) + 1 ] ), ( WORD ) * ( BYTE * ) &pCode[ (*w) + 1 + sizeof( double ) ] );
   (*w) += 1 + sizeof( double ) + 1;
}

void PCode__PUSHSELF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   Push( stack.pBase + 1 );
   (*w)++;
}

void PCode__PUSHSTATIC( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushStatic( pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 ) );
   (*w) += 3;
}

void PCode__PUSHSTATICREF( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__PUSHSTR( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wSize) =*( (WORD *) &( pCode[ (*w) + 1 ] ) );
   PushString( (char*)pCode + (*w) + 3, (*wSize) );
   (*w) += ( (*wSize) + 3 );
}

void PCode__PUSHSYM( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wParams) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   PushSymbol( pSymbols + (*wParams) );
   (*w) += 3;
}

void PCode__PUSHWORD( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

}

void PCode__RETVALUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   RetValue();
   (*w)++;
}

void PCode__SFRAME( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wParams) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   SFrame( pSymbols + (*wParams) );
   (*w) += 3;
}

void PCode__STATICS( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   (*wParams) = pCode[ (*w) + 1 ] + ( pCode[ (*w) + 2 ] * 256 );
   Statics( pSymbols + (*wParams) );
   (*w) += 3;
}

void PCode__TRUE( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushLogical( 1 );
   (*w)++;
}

void PCode__ZERO( PBYTE pCode, PSYMBOL pSymbols, BYTE *bCode, WORD *w, WORD *wParams, WORD *wSize )
{
   /*-----------------6/2/99 8:38PM--------------------
    * To avoide possible warning about non used parameters
    * --------------------------------------------------*/
   if( ! pCode ){ if( pSymbols ){}; if( (*bCode) ){}; if( w ){}; if( (*wParams) ){}; if( (*wSize) ){}; }

   PushInteger( 0 );
   (*w)++;
}


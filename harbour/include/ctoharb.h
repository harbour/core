/*
 * $Id$
 */

/*
   Copyright(C) 1999 by Antonio Linares.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR
   PURPOSE.  See the GNU General Public License for more details.

   You should have received a copy of the GNU General Public
   License along with this program; if not, write to:

   The Free Software Foundation, Inc.,
   675 Mass Ave, Cambridge, MA 02139, USA.

   You can contact me at: alinares@fivetech.com
 */

#ifndef HB_CTOHARB_H_
#define HB_CTOHARB_H_

#include "extend.h"

/* Harbour virtual machine functions */
extern void    hb_vmExecute( BYTE * pCode, PHB_SYMB pSymbols );  /* invokes the virtual machine */
extern void    hb_vmProcessSymbols( PHB_SYMB pSymbols, WORD wSymbols ); /* statics symbols initialization */

/* PCode functions */
extern void    hb_vmAnd( void );             /* performs the logical AND on the latest two values, removes them and leaves result on the stack */
extern void    hb_vmArrayAt( void );         /* pushes an array element to the stack, removing the array and the index from the stack */
extern void    hb_vmArrayPut( void );        /* sets an array value and pushes the value on to the stack */
extern void    hb_vmDec( void );             /* decrements the latest numeric value on the stack */
extern void    hb_vmDimArray( WORD wDimensions ); /* generates a wDimensions Array and initialize those dimensions from the stack values */
extern void    hb_vmDivide( void );          /* divides the latest two values on the stack, removes them and leaves the result */
extern void    hb_vmDo( WORD WParams );      /* invoke the virtual machine */
extern HARBOUR hb_vmDoBlock( void );         /* executes a codeblock */
extern void    hb_vmDuplicate( void );       /* duplicates the latest value on the stack */
extern void    hb_vmDuplTwo( void );         /* duplicates the latest two value on the stack */
extern void    hb_vmEndBlock( void );        /* copies the last codeblock pushed value into the return value */
extern void    hb_vmEqual( BOOL bExact );    /* checks if the two latest values on the stack are equal, removes both and leaves result */
extern void    hb_vmForTest( void );         /* test for end condition of for */
extern void    hb_vmFrame( BYTE bLocals, BYTE bParams );  /* increases the stack pointer for the amount of locals and params suplied */
extern void    hb_vmFuncPtr( void );         /* pushes a function address pointer. Removes the symbol from the satck */
extern void    hb_vmFunction( WORD wParams ); /* executes a function saving its result */
extern void    hb_vmGenArray( WORD wElements ); /* generates a wElements Array and fills it from the stack values */
extern void    hb_vmGreater( void );         /* checks if the latest - 1 value is greater than the latest, removes both and leaves result */
extern void    hb_vmGreaterEqual( void );    /* checks if the latest - 1 value is greater than or equal the latest, removes both and leaves result */
extern void    hb_vmInc( void );             /* increment the latest numeric value on the stack */
extern void    hb_vmInstring( void );        /* check whether string 1 is contained in string 2 */
extern void    hb_vmLess( void );            /* checks if the latest - 1 value is less than the latest, removes both and leaves result */
extern void    hb_vmLessEqual( void );       /* checks if the latest - 1 value is less than or equal the latest, removes both and leaves result */
extern void    hb_vmLocalName( WORD wLocal, char * szLocalName ); /* locals and parameters index and name information for the debugger */
extern void    hb_vmMessage( PHB_SYMB pSymMsg ); /* sends a message to an object */
extern void    hb_vmMinus( void );           /* substracts the latest two values on the stack, removes them and leaves the result */
extern void    hb_vmModuleName( char * szModuleName ); /* PRG and function name information for the debugger */
extern void    hb_vmModulus( void );         /* calculates the modulus of latest two values on the stack, removes them and leaves the result */
extern void    hb_vmMult( void );            /* multiplies the latest two values on the stack, removes them and leaves the result */
extern void    hb_vmNegate( void );          /* negates (-) the latest value on the stack */
extern void    hb_vmNot( void );             /* changes the latest logical value on the stack */
extern void    hb_vmNotEqual( void );        /* checks if the two latest values on the stack are not equal, removes both and leaves result */
extern void    hb_vmOperatorCall( PHB_ITEM, PHB_ITEM, char *); /* call an overloaded operator */
extern void    hb_vmOr( void );              /* performs the logical OR on the latest two values, removes them and leaves result on the stack */
extern void    hb_vmPlus( void );            /* sums the latest two values on the stack, removes them and leaves the result */
extern long    hb_vmPopDate( void );         /* pops the stack latest value and returns its date value as a LONG */
extern void    hb_vmPopDefStat( WORD wStatic ); /* pops the stack latest value onto a static as default init */
extern double  hb_vmPopDouble( WORD * );   /* pops the stack latest value and returns its double numeric format value */
extern void    hb_vmPopLocal( SHORT wLocal );      /* pops the stack latest value onto a local */
extern BOOL    hb_vmPopLogical( void );           /* pops the stack latest value and returns its logical value */
extern void    hb_vmPopMemvar( PHB_SYMB );      /* pops a value of memvar variable */
extern double  hb_vmPopNumber( void );          /* pops the stack latest value and returns its numeric value */
extern void    hb_vmPopParameter( PHB_SYMB, BYTE );  /* creates a PRIVATE variable and sets it with parameter's value */
extern void    hb_vmPopStatic( WORD wStatic );    /* pops the stack latest value onto a static */
extern void    hb_vmPower( void );            /* power the latest two values on the stack, removes them and leaves the result */
extern void    hb_vmPush( PHB_ITEM pItem );     /* pushes a generic item onto the stack */
extern void    hb_vmPushBlock( BYTE * pCode, PHB_SYMB pSymbols ); /* creates a codeblock */
extern void    hb_vmPushDate( LONG lDate );   /* pushes a long date onto the stack */
extern void    hb_vmPushDouble( double lNumber, WORD wDec ); /* pushes a double number onto the stack */
extern void    hb_vmPushLocal( SHORT iLocal );     /* pushes the containts of a local onto the stack */
extern void    hb_vmPushLocalByRef( SHORT iLocal ); /* pushes a local by refrence onto the stack */
extern void    hb_vmPushLogical( BOOL bValue );    /* pushes a logical value onto the stack */
extern void    hb_vmPushLong( long lNumber ); /* pushes a long number onto the stack */
extern void    hb_vmPushMemvar( PHB_SYMB );     /* pushes a value of memvar variable */
extern void    hb_vmPushMemvarByRef( PHB_SYMB ); /* pushes a reference to a memvar variable */
extern void    hb_vmPushNil( void );            /* in this case it places nil at self */
extern void    hb_vmPushNumber( double dNumber, WORD wDec ); /* pushes a number on to the stack and decides if it is integer, long or double */
extern void    hb_vmPushStatic( WORD wStatic );   /* pushes the containts of a static onto the stack */
extern void    hb_vmPushStaticByRef( WORD iLocal ); /* pushes a static by refrence onto the stack */
extern void    hb_vmPushString( char * szText, ULONG length );  /* pushes a string on to the stack */
extern void    hb_vmPushSymbol( PHB_SYMB pSym ); /* pushes a function pointer onto the stack */
extern void    hb_vmPushInteger( int iNumber ); /* pushes a integer number onto the stack */
extern void    hb_vmRetValue( void );           /* pops the latest stack value into stack.Return */
extern void    hb_vmSFrame( PHB_SYMB pSym );     /* sets the statics frame for a function */
extern void    hb_vmStatics( PHB_SYMB pSym );    /* increases the the global statics array to hold a PRG statics */

/* stack management functions */
extern void    hb_stackDec( void );        /* pops an item from the stack without clearing it's contents */
extern void    hb_stackPop( void );        /* pops an item from the stack */
extern void    hb_stackFree( void );       /* releases all memory used by the stack */
extern void    hb_stackPush( void );       /* pushes an item on to the stack */
extern void    hb_stackInit( void );       /* initializes the stack */
extern void    hb_stackShow( void );       /* show the types of the items on the stack for debugging purposes */
extern void    hb_callStackShow( void );   /* show the procedure names of the call stack for internal errors use */

#define STACK_INITHB_ITEMS   100
#define STACK_EXPANDHB_ITEMS  20

#endif /* HB_CTOHARB_H_ */

/*
 * $Id$

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

#ifndef HB_PCODE_H_
#define HB_PCODE_H_

#include <extend.h>

typedef enum
{
   HB_P_AND,            /* peforms the logical AND of two latest stack values, removes them and places result */
   HB_P_ARRAYAT,        /* places on the virtual machine stack an array element */
   HB_P_ARRAYPUT,       /* sets array element, the array and the index are both on the stack */
   HB_P_EQUAL,          /* check if the latest two values on the stack are equal, removing them and leaving there the result */
   HB_P_ENDBLOCK,       /* end of a codeblock definition */
   HB_P_ENDPROC,        /* instructs the virtual machine to end execution */
   HB_P_EXACTLYEQUAL,   /* check if the latest two values on the stack are exactly equal, removing them and leaving there the result */
   HB_P_FALSE,          /* pushes false on the virtual machine stack */
   HB_P_FORTEST,        /* For STEP. If step > 1 less. If step < 1 greater. */
   HB_P_FUNCTION,       /* instructs the virtual machine to execute a function saving its result */
   HB_P_FRAME,          /* instructs the virtual machine about how many parameters and locals a function uses */
   HB_P_FUNCPTR,        /* returns a function address pointer */
   HB_P_GENARRAY,       /* instructs the virtual machine to build an array and load elemnst from the stack */
   HB_P_GREATER,        /* checks if the second latest value on the stack is greater that the lastest one */
   HB_P_GREATEREQUAL,   /* checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
   HB_P_DEC,            /* decrements the latest value on the virtual machine stack */
   HB_P_DIMARRAY,       /* instructs the virtual machine to build an array with some specific dimensions */
   HB_P_DIVIDE,         /* divides the latest two values on the stack, removing them and leaving there the result */
   HB_P_DO,             /* instructs the virtual machine to execute a function discarding its result */
   HB_P_DUPLICATE,      /* places a copy of the latest virtual machine stack value on to the stack */
   HB_P_DUPLTWO,        /* places a copy of the latest two virtual machine stack value on to the stack */
   HB_P_INC,            /* increments the latest value on the virtual machine stack */
   HB_P_INSTRING,       /* checks if the second latest value on the stack is a substring of the latest one */
   HB_P_JUMP,           /* jumps to a relative offset */
   HB_P_JUMPFALSE,      /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUE,       /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_LESSEQUAL,      /* checks if the second latest value on the stack is less equal that the latest one, leaves the result only */
   HB_P_LESS,           /* checks if the second latest value on the stack is less that the lastest one */
   HB_P_LINE,           /* currently compiled source code line number */
   HB_P_MESSAGE,        /* sends a message to an object */
   HB_P_MINUS,          /* subs the latest two values on the stack, removing them and leaving there the result */
   HB_P_MODULUS,        /* calculates the modulus of the two values on the stack, removing them and leaving there the result */
   HB_P_MULT,           /* multiplies the latest two values on the stack, removing them and leaving there the result */
   HB_P_NEGATE,         /* numerically negates the latest value on the stack */
   HB_P_NOOP,           /* no operation */
   HB_P_NOT,            /* logically negates the latest value on the stack */
   HB_P_NOTEQUAL,       /* checks if the latest two stack values are equal, leaves just the result */
   HB_P_OR,             /* peforms the logical OR of two latest stack values, removes them and places result */
   HB_P_PARAMETER,      /* creates PRIVATE variables and assigns values to functions paramaters */
   HB_P_PLUS,           /* adds the latest two values on the stack, removing them and leaving there the result */
   HB_P_POP,            /* removes the latest value from the stack */
   HB_P_POPLOCAL,       /* pops the contains of the virtual machine stack onto a local variable */
   HB_P_POPMEMVAR,      /* pops the contains of a memvar variable to the virtual machine stack */
   HB_P_POPSTATIC,      /* pops the contains of the virtual machine stack onto a static variable */
   HB_P_POWER,          /* calculates the power of the two values on the stack, removing them and leaving there the result */
   HB_P_PUSHBLOCK,      /* start of a codeblock definition */
   HB_P_PUSHINT,        /* places an integer number on the virtual machine stack */
   HB_P_PUSHLOCAL,      /* pushes the contains of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALREF,   /* pushes a local variable by reference to the virtual machine stack */
   HB_P_PUSHLONG,       /* places an integer number on the virtual machine stack */
   HB_P_PUSHMEMVAR,     /* pushes the contains of a memvar variable to the virtual machine stack */
   HB_P_PUSHMEMVARREF,  /* pushes the a memvar variable by reference to the virtual machine stack */
   HB_P_PUSHNIL,        /* places a nil on the virtual machine stack */
   HB_P_PUSHDOUBLE,     /* places a double number on the virtual machine stack */
   HB_P_PUSHSELF,       /* pushes Self for the current processed method */
   HB_P_PUSHSTATIC,     /* pushes the contains of a static variable to the virtual machine stack */
   HB_P_PUSHSTATICREF,  /* pushes the a static variable by reference to the virtual machine stack */
   HB_P_PUSHSTR,        /* places a string on the virtual machine stack */
   HB_P_PUSHSYM,        /* places a symbol on the virtual machine stack */
   HB_P_PUSHWORD,       /* places a two bytes number on the virtual machine stack */
   HB_P_RETVALUE,       /* instructs the virtual machine to return the latest stack value */
   HB_P_SFRAME,         /* sets the statics frame for a function */
   HB_P_STATICS,        /* defines the number of statics variables for a PRG */
   HB_P_TRUE,           /* pushes true on the virtual machine stack */
   HB_P_ZERO            /* places a zero on the virtual machine stack */
} HB_PCODE;

#endif /* HB_PCODE_H_ */

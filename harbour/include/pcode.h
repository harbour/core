/*
 * $Id$
 */

#ifndef _PCODE_H
#define _PCODE_H

#include <extend.h>

typedef enum
{
   AND_,         /* peforms the logical AND of two latest stack values, removes them and places result */
   _ARRAYAT,     /* places on the virtual machine stack an array element */
   _ARRAYPUT,    /* sets array element, the array and the index are both on the stack */
   _EQUAL,       /* check if the latest two values on the stack are equal, removing them and leaving there the result */
   _ENDBLOCK,    /* end of a codeblock definition */
   _ENDPROC,     /* instructs the virtual machine to end execution */
   _FALSE,       /* pushes false on the virtual machine stack */
   _FORTEST,     /* For STEP. If step > 1 less. If step < 1 greater. */
   _FUNCTION,    /* instructs the virtual machine to execute a function saving its result */
   _FRAME,       /* instructs the virtual machine about how many parameters and locals a function uses */
   _FUNCPTR,     /* returns a function address pointer */
   _GENARRAY,    /* instructs the virtual machine to build an array and load elemnst from the stack */
   _GREATER,     /* checks if the second latest value on the stack is greater that the lastest one */
   _GREATEREQUAL,/* checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
   _DEC,         /* decrements the latest value on the virtual machine stack */
   _DIMARRAY,    /* instructs the virtual machine to build an array with some specific dimensions */
   _DIVIDE,      /* divides the latest two values on the stack, removing them and leaving there the result */
   _DO,          /* instructs the virtual machine to execute a function discarding its result */
   _DUPLICATE,   /* places a copy of the latest virtual machine stack value on to the stack */
   _INC,         /* increments the latest value on the virtual machine stack */
   _INSTRING,    /* checks if the second latest value on the stack is a substring of the latest one */
   _JUMP,        /* jumps to a relative offset */
   _JUMPFALSE,   /* checks a logic expression of the stack and jumps to a relative offset */
   _JUMPTRUE,    /* checks a logic expression of the stack and jumps to a relative offset */
   _LESSEQUAL,   /* checks if the second latest value on the stack is less equal that the latest one, leaves the result only */
   _LESS,        /* checks if the second latest value on the stack is less that the lastest one */
   _LINE,        /* currently compiled source code line number */
   _MESSAGE,     /* sends a message to an object */
   _MINUS,       /* subs the latest two values on the stack, removing them and leaving there the result */
   _MODULUS,     /* calculates the modulus of the two values on the stack, removing them and leaving there the result */
   _MULT,        /* multiplies the latest two values on the stack, removing them and leaving there the result */
   _NEGATE,      /* numerically negates the latest value on the stack */
   _NOT,         /* logically negates the latest value on the stack */
   _NOTEQUAL,    /* checks if the latest two stack values are equal, leaves just the result */
   OR_,          /* peforms the logical OR of two latest stack values, removes them and places result */
   _PLUS,        /* adds the latest two values on the stack, removing them and leaving there the result */
   _POP,         /* removes the latest value from the stack */
   _POPDEFSTAT,  /* pops a default value to a static variable */
   _POPLOCAL,    /* pops the contains of the virtual machine stack onto a local variable */
   _POPMEMVAR,   /* pops the contains of a memvar variable to the virtual machine stack */
   _POPSTATIC,   /* pops the contains of the virtual machine stack onto a static variable */
   _POWER,       /* calculates the power of the two values on the stack, removing them and leaving there the result */
   _PUSHBLOCK,   /* start of a codeblock definition */
   _PUSHINT,     /* places an integer number on the virtual machine stack */
   _PUSHLOCAL,   /* pushes the contains of a local variable to the virtual machine stack */
   _PUSHLOCALREF, /* pushes a local variable by reference to the virtual machine stack */
   _PUSHLONG,    /* places an integer number on the virtual machine stack */
   _PUSHMEMVAR,  /* pushes the contains of a memvar variable to the virtual machine stack */
   _PUSHMEMVARREF, /* pushes the a memvar variable by reference to the virtual machine stack */
   _PUSHNIL,     /* places a nil on the virtual machine stack */
   _PUSHDOUBLE,  /* places a double number on the virtual machine stack */
   _PUSHSELF,    /* pushes Self for the current processed method */
   _PUSHSTATIC,  /* pushes the contains of a static variable to the virtual machine stack */
   _PUSHSTATICREF, /* pushes the a static variable by reference to the virtual machine stack */
   _PUSHSTR,     /* places a string on the virtual machine stack */
   _PUSHSYM,     /* places a symbol on the virtual machine stack */
   _PUSHWORD,    /* places a two bytes number on the virtual machine stack */
   _RETVALUE,    /* instructs the virtual machine to return the latest stack value */
   _SFRAME,      /* sets the statics frame for a function */
   _STATICS,     /* defines the number of statics variables for a PRG */
   _TRUE,        /* pushes true on the virtual machine stack */
   _ZERO         /* places a zero on the virtual machine stack */
} PCODE;

#endif

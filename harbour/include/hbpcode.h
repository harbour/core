/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the PCODE declarations
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

#ifndef HB_PCODE_H_
#define HB_PCODE_H_

/* NOTE:
 * Please update any opcode lookup tables present in
 *  genc.c
 *  harbour.c
 * when new opcode is added
 */
typedef enum
{
   HB_P_AND,                   /* peforms the logical AND of two latest stack values, removes them and places result */
   HB_P_ARRAYPUSH,             /* places on the virtual machine stack an array element */
   HB_P_ARRAYPOP,              /* pops a value from the eval stack into an array element */
   HB_P_ARRAYDIM,              /* instructs the virtual machine to build an array with some specific dimensions */
   HB_P_ARRAYGEN,              /* instructs the virtual machine to build an array and load elemnst from the stack */
   HB_P_EQUAL,                 /* check if the latest two values on the stack are equal, removing them and leaving there the result */
   HB_P_ENDBLOCK,              /* end of a codeblock definition */
   HB_P_ENDPROC,               /* instructs the virtual machine to end execution */
   HB_P_EXACTLYEQUAL,          /* check if the latest two values on the stack are exactly equal, removing them and leaving there the result */
   HB_P_FALSE,                 /* pushes false on the virtual machine stack */
   HB_P_FORTEST,               /* For STEP. If step > 1 less. If step < 1 greater. */
   HB_P_FUNCTION,              /* instructs the virtual machine to execute a function saving its result */
   HB_P_FUNCTIONSHORT,         /* instructs the virtual machine to execute a function saving its result */
   HB_P_FRAME,                 /* instructs the virtual machine about how many parameters and locals a function uses */
   HB_P_FUNCPTR,               /* returns a function address pointer */
   HB_P_GREATER,               /* checks if the second latest value on the stack is greater that the lastest one */
   HB_P_GREATEREQUAL,          /* checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
   HB_P_DEC,                   /* decrements the latest value on the virtual machine stack */
   HB_P_DIVIDE,                /* divides the latest two values on the stack, removing them and leaving there the result */
   HB_P_DO,                    /* instructs the virtual machine to execute a function discarding its result */
   HB_P_DOSHORT,               /* instructs the virtual machine to execute a function discarding its result */
   HB_P_DUPLICATE,             /* places a copy of the latest virtual machine stack value on to the stack */
   HB_P_DUPLTWO,               /* places a copy of the latest two virtual machine stack value on to the stack */
   HB_P_INC,                   /* increments the latest value on the virtual machine stack */
   HB_P_INSTRING,              /* checks if the second latest value on the stack is a substring of the latest one */
   HB_P_JUMPNEAR,              /* jumps to a relative offset 1 Byte */
   HB_P_JUMP,                  /* jumps to a relative offset 2 Bytes */
   HB_P_JUMPFAR,               /* jumps to a relative offset 3 Bytes */
   HB_P_JUMPFALSENEAR,         /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPFALSE,             /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPFALSEFAR,          /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUENEAR,          /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUE,              /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUEFAR,           /* checks a logic expression of the stack and jumps to a relative offset */
   HB_P_LESSEQUAL,             /* checks if the second latest value on the stack is less equal that the latest one, leaves the result only */
   HB_P_LESS,                  /* checks if the second latest value on the stack is less that the lastest one */
   HB_P_LINE,                  /* currently compiled source code line number */
   HB_P_LOCALNAME,             /* sets the name of local variable */
   HB_P_MACROPOP,              /* compile and run - pop a value from the stack */
   HB_P_MACROPOPALIASED,       /* compile and run - pop a field value from the stack */
   HB_P_MACROPUSH,             /* compile and run - leave the result on the stack */
   HB_P_MACROPUSHALIASED,      /* compile and run - leave the field value on the stack */
   HB_P_MACROSYMBOL,           /* compile into a symbol name (used in function calls) */
   HB_P_MACROTEXT,             /* macro text substitution */
   HB_P_MESSAGE,               /* sends a message to an object */
   HB_P_MINUS,                 /* subs the latest two values on the stack, removing them and leaving there the result */
   HB_P_MODULUS,               /* calculates the modulus of the two values on the stack, removing them and leaving there the result */
   HB_P_MODULENAME,            /* sets the name of debugged module */
/* start: pcodes generated by the macro compiler - the symbol address is used */
   HB_P_MMESSAGE,
   HB_P_MPOPALIASEDFIELD,
   HB_P_MPOPALIASEDVAR,
   HB_P_MPOPFIELD,
   HB_P_MPOPMEMVAR,
   HB_P_MPUSHALIASEDFIELD,
   HB_P_MPUSHALIASEDVAR,
   HB_P_MPUSHBLOCK,
   HB_P_MPUSHFIELD,
   HB_P_MPUSHMEMVAR,
   HB_P_MPUSHMEMVARREF,
   HB_P_MPUSHSYM,
   HB_P_MPUSHVARIABLE,
/* end: */
   HB_P_MULT,                  /* multiplies the latest two values on the stack, removing them and leaving there the result */
   HB_P_NEGATE,                /* numerically negates the latest value on the stack */
   HB_P_NOOP,                  /* no operation */
   HB_P_NOT,                   /* logically negates the latest value on the stack */
   HB_P_NOTEQUAL,              /* checks if the latest two stack values are equal, leaves just the result */
   HB_P_OR,                    /* peforms the logical OR of two latest stack values, removes them and places result */
   HB_P_PARAMETER,             /* creates PRIVATE variables and assigns values to functions paramaters */
   HB_P_PLUS,                  /* adds the latest two values on the stack, removing them and leaving there the result */
   HB_P_POP,                   /* removes the latest value from the stack */
   HB_P_POPALIAS,              /* pops the item from the eval stack and selects the current workarea */
   HB_P_POPALIASEDFIELD,       /* pops aliased field */
   HB_P_POPALIASEDFIELDNEAR,   /* pops aliased field */
   HB_P_POPALIASEDVAR,         /* pops aliased variable (either a field or a memvar) */
   HB_P_POPFIELD,              /* pops unaliased field */
   HB_P_POPLOCAL,              /* pops the contains of the virtual machine stack onto a local variable */
   HB_P_POPLOCALNEAR,          /* pops the contains of the virtual machine stack onto a local variable */
   HB_P_POPMEMVAR,             /* pops the contains of a memvar variable to the virtual machine stack */
   HB_P_POPSTATIC,             /* pops the contains of the virtual machine stack onto a static variable */
   HB_P_POPVARIABLE,           /* pops the contains of an undeclared variable from the virtual machine stack */
   HB_P_POWER,                 /* calculates the power of the two values on the stack, removing them and leaving there the result */
   HB_P_PUSHALIAS,             /* saves the current workarea number on the eval stack */
   HB_P_PUSHALIASEDFIELD,      /* pushes aliased field */
   HB_P_PUSHALIASEDFIELDNEAR,  /* pushes aliased field */
   HB_P_PUSHALIASEDVAR,        /* pushes aliased variable (either a field or a memvar) */
   HB_P_PUSHBLOCK,             /* start of a codeblock definition */
   HB_P_PUSHBLOCKSHORT,        /* start of a codeblock definition */
   HB_P_PUSHFIELD,             /* pushes unaliased field */
   HB_P_PUSHBYTE,              /* places a 1 byte integer number on the virtual machine stack */
   HB_P_PUSHINT,               /* places an integer number on the virtual machine stack */
   HB_P_PUSHLOCAL,             /* pushes the contains of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALNEAR,         /* pushes the contains of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALREF,          /* pushes a local variable by reference to the virtual machine stack */
   HB_P_PUSHLONG,              /* places an integer number on the virtual machine stack */
   HB_P_PUSHMEMVAR,            /* pushes the contains of a memvar variable to the virtual machine stack */
   HB_P_PUSHMEMVARREF,         /* pushes the a memvar variable by reference to the virtual machine stack */
   HB_P_PUSHNIL,               /* places a nil on the virtual machine stack */
   HB_P_PUSHDOUBLE,            /* places a double number on the virtual machine stack */
   HB_P_PUSHSELF,              /* pushes Self for the current processed method */
   HB_P_PUSHSTATIC,            /* pushes the contains of a static variable to the virtual machine stack */
   HB_P_PUSHSTATICREF,         /* pushes the a static variable by reference to the virtual machine stack */
   HB_P_PUSHSTR,               /* places a string on the virtual machine stack */
   HB_P_PUSHSTRSHORT,          /* places a string on the virtual machine stack */
   HB_P_PUSHSYM,               /* places a symbol on the virtual machine stack */
   HB_P_PUSHSYMNEAR,           /* places a symbol on the virtual machine stack */
   HB_P_PUSHVARIABLE,          /* pushes the contains of an undeclared variable to the virtual machine stack */
   HB_P_RETVALUE,              /* instructs the virtual machine to return the latest stack value */
   HB_P_SEND,                  /* send operator */
   HB_P_SENDSHORT,             /* send operator */
   HB_P_SEQBEGIN,              /* BEGIN SEQUENCE */
   HB_P_SEQEND,                /* END SEQUENCE */
   HB_P_SEQRECOVER,            /* RECOVER statement */
   HB_P_SFRAME,                /* sets the statics frame for a function */
   HB_P_STATICS,               /* defines the number of statics variables for a PRG */
   HB_P_STATICNAME,            /* sets the name of static variable */
   HB_P_SWAPALIAS,             /* restores the current workarea number from the eval stack */
   HB_P_TRUE,                  /* pushes true on the virtual machine stack */
   HB_P_ZERO,                  /* places a ZERO on the virtual machine stack */
   HB_P_ONE,                   /* places a ONE on the virtual machine stack */
/* NOTE: This have to be the last definition */
   HB_P_LAST_PCODE             /* this defines the number of defined pcodes */
} HB_PCODE;

#endif /* HB_PCODE_H_ */
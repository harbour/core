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
   HB_P_AND,                   /*   0 peforms the logical AND of two latest stack values, removes them and places result */
   HB_P_ARRAYPUSH,             /*   1 places on the virtual machine stack an array element */
   HB_P_ARRAYPOP,              /*   2 pops a value from the eval stack into an array element */
   HB_P_ARRAYDIM,              /*   3 instructs the virtual machine to build an array with some specific dimensions */
   HB_P_ARRAYGEN,              /*   4 instructs the virtual machine to build an array and load elemnst from the stack */
   HB_P_EQUAL,                 /*   5 check if the latest two values on the stack are equal, removing them and leaving there the result */
   HB_P_ENDBLOCK,              /*   6 end of a codeblock definition */
   HB_P_ENDPROC,               /*   7 instructs the virtual machine to end execution */
   HB_P_EXACTLYEQUAL,          /*   8 check if the latest two values on the stack are exactly equal, removing them and leaving there the result */
   HB_P_FALSE,                 /*   9 pushes false on the virtual machine stack */
   HB_P_FORTEST,               /*  10 For STEP. If step > 1 less. If step < 1 greater. */
   HB_P_FUNCTION,              /*  11 instructs the virtual machine to execute a function saving its result */
   HB_P_FUNCTIONSHORT,         /*  12 instructs the virtual machine to execute a function saving its result */
   HB_P_FRAME,                 /*  13 instructs the virtual machine about how many parameters and locals a function uses */
   HB_P_FUNCPTR,               /*  14 returns a function address pointer */
   HB_P_GREATER,               /*  15 checks if the second latest value on the stack is greater that the lastest one */
   HB_P_GREATEREQUAL,          /*  16 checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
   HB_P_DEC,                   /*  17 decrements the latest value on the virtual machine stack */
   HB_P_DIVIDE,                /*  18 divides the latest two values on the stack, removing them and leaving there the result */
   HB_P_DO,                    /*  19 instructs the virtual machine to execute a function discarding its result */
   HB_P_DOSHORT,               /*  20 instructs the virtual machine to execute a function discarding its result */
   HB_P_DUPLICATE,             /*  21 places a copy of the latest virtual machine stack value on to the stack */
   HB_P_DUPLTWO,               /*  22 places a copy of the latest two virtual machine stack value on to the stack */
   HB_P_INC,                   /*  23 increments the latest value on the virtual machine stack */
   HB_P_INSTRING,              /*  24 checks if the second latest value on the stack is a substring of the latest one */
   HB_P_JUMPNEAR,              /*  25 jumps to a relative offset 1 Byte */
   HB_P_JUMP,                  /*  26 jumps to a relative offset 2 Bytes */
   HB_P_JUMPFAR,               /*  27 jumps to a relative offset 3 Bytes */
   HB_P_JUMPFALSENEAR,         /*  28 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPFALSE,             /*  29 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPFALSEFAR,          /*  30 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUENEAR,          /*  31 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUE,              /*  32 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_JUMPTRUEFAR,           /*  33 checks a logic expression of the stack and jumps to a relative offset */
   HB_P_LESSEQUAL,             /*  34 checks if the second latest value on the stack is less equal that the latest one, leaves the result only */
   HB_P_LESS,                  /*  35 checks if the second latest value on the stack is less that the lastest one */
   HB_P_LINE,                  /*  36 currently compiled source code line number */
   HB_P_LOCALNAME,             /*  37 sets the name of local variable */
   HB_P_MACROPOP,              /*  38 compile and run - pop a value from the stack */
   HB_P_MACROPOPALIASED,       /*  39 compile and run - pop a field value from the stack */
   HB_P_MACROPUSH,             /*  40 compile and run - leave the result on the stack */
   HB_P_MACROPUSHARG,          /*  41 compile and run - leave the result on the stack */
   HB_P_MACROPUSHALIASED,      /*  42 compile and run - leave the field value on the stack */
   HB_P_MACROSYMBOL,           /*  43 compile into a symbol name (used in function calls) */
   HB_P_MACROTEXT,             /*  44 macro text substitution */
   HB_P_MESSAGE,               /*  45 sends a message to an object */
   HB_P_MINUS,                 /*  46 subs the latest two values on the stack, removing them and leaving there the result */
   HB_P_MODULUS,               /*  47 calculates the modulus of the two values on the stack, removing them and leaving there the result */
   HB_P_MODULENAME,            /*  48 sets the name of debugged module */
/* start: pcodes generated by the macro compiler - the symbol address is used */
   HB_P_MMESSAGE,              /*  49 */
   HB_P_MPOPALIASEDFIELD,      /*  50 */
   HB_P_MPOPALIASEDVAR,        /*  51 */
   HB_P_MPOPFIELD,             /*  52 */
   HB_P_MPOPMEMVAR,            /*  53 */
   HB_P_MPUSHALIASEDFIELD,     /*  54 */
   HB_P_MPUSHALIASEDVAR,       /*  55 */
   HB_P_MPUSHBLOCK,            /*  56 */
   HB_P_MPUSHFIELD,            /*  57 */
   HB_P_MPUSHMEMVAR,           /*  58 */
   HB_P_MPUSHMEMVARREF,        /*  59 */
   HB_P_MPUSHSYM,              /*  60 */
   HB_P_MPUSHVARIABLE,         /*  61 */
/* end: */
   HB_P_MULT,                  /*  62 multiplies the latest two values on the stack, removing them and leaving there the result */
   HB_P_NEGATE,                /*  63 numerically negates the latest value on the stack */
   HB_P_NOOP,                  /*  64 no operation */
   HB_P_NOT,                   /*  65 logically negates the latest value on the stack */
   HB_P_NOTEQUAL,              /*  66 checks if the latest two stack values are equal, leaves just the result */
   HB_P_OR,                    /*  67 peforms the logical OR of two latest stack values, removes them and places result */
   HB_P_PARAMETER,             /*  68 creates PRIVATE variables and assigns values to functions paramaters */
   HB_P_PLUS,                  /*  69 adds the latest two values on the stack, removing them and leaving there the result */
   HB_P_POP,                   /*  70 removes the latest value from the stack */
   HB_P_POPALIAS,              /*  71 pops the item from the eval stack and selects the current workarea */
   HB_P_POPALIASEDFIELD,       /*  72 pops aliased field */
   HB_P_POPALIASEDFIELDNEAR,   /*  73 pops aliased field */
   HB_P_POPALIASEDVAR,         /*  74 pops aliased variable (either a field or a memvar) */
   HB_P_POPFIELD,              /*  75 pops unaliased field */
   HB_P_POPLOCAL,              /*  76 pops the contains of the virtual machine stack onto a local variable */
   HB_P_POPLOCALNEAR,          /*  77 pops the contains of the virtual machine stack onto a local variable */
   HB_P_POPMEMVAR,             /*  78 pops the contains of a memvar variable to the virtual machine stack */
   HB_P_POPSTATIC,             /*  79 pops the contains of the virtual machine stack onto a static variable */
   HB_P_POPVARIABLE,           /*  80 pops the contains of an undeclared variable from the virtual machine stack */
   HB_P_POWER,                 /*  81 calculates the power of the two values on the stack, removing them and leaving there the result */
   HB_P_PUSHALIAS,             /*  82 saves the current workarea number on the eval stack */
   HB_P_PUSHALIASEDFIELD,      /*  83 pushes aliased field */
   HB_P_PUSHALIASEDFIELDNEAR,  /*  84 pushes aliased field */
   HB_P_PUSHALIASEDVAR,        /*  85 pushes aliased variable (either a field or a memvar) */
   HB_P_PUSHBLOCK,             /*  86 start of a codeblock definition */
   HB_P_PUSHBLOCKSHORT,        /*  87 start of a codeblock definition */
   HB_P_PUSHFIELD,             /*  88 pushes unaliased field */
   HB_P_PUSHBYTE,              /*  89 places a 1 byte integer number on the virtual machine stack */
   HB_P_PUSHINT,               /*  90 places an integer number on the virtual machine stack */
   HB_P_PUSHLOCAL,             /*  91 pushes the contains of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALNEAR,         /*  92 pushes the contains of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALREF,          /*  93 pushes a local variable by reference to the virtual machine stack */
   HB_P_PUSHLONG,              /*  94 places an integer number on the virtual machine stack */
   HB_P_PUSHMEMVAR,            /*  95 pushes the contains of a memvar variable to the virtual machine stack */
   HB_P_PUSHMEMVARREF,         /*  96 pushes the a memvar variable by reference to the virtual machine stack */
   HB_P_PUSHNIL,               /*  97 places a nil on the virtual machine stack */
   HB_P_PUSHDOUBLE,            /*  98 places a double number on the virtual machine stack */
   HB_P_PUSHSELF,              /*  99 pushes Self for the current processed method */
   HB_P_PUSHSTATIC,            /* 100 pushes the contains of a static variable to the virtual machine stack */
   HB_P_PUSHSTATICREF,         /* 101 pushes the a static variable by reference to the virtual machine stack */
   HB_P_PUSHSTR,               /* 102 places a string on the virtual machine stack */
   HB_P_PUSHSTRSHORT,          /* 103 places a string on the virtual machine stack */
   HB_P_PUSHSYM,               /* 104 places a symbol on the virtual machine stack */
   HB_P_PUSHSYMNEAR,           /* 105 places a symbol on the virtual machine stack */
   HB_P_PUSHVARIABLE,          /* 106 pushes the contains of an undeclared variable to the virtual machine stack */
   HB_P_RETVALUE,              /* 107 instructs the virtual machine to return the latest stack value */
   HB_P_SEND,                  /* 108 send operator */
   HB_P_SENDSHORT,             /* 109 send operator */
   HB_P_SEQBEGIN,              /* 110 BEGIN SEQUENCE */
   HB_P_SEQEND,                /* 111 END SEQUENCE */
   HB_P_SEQRECOVER,            /* 112 RECOVER statement */
   HB_P_SFRAME,                /* 113 sets the statics frame for a function */
   HB_P_STATICS,               /* 114 defines the number of statics variables for a PRG */
   HB_P_STATICNAME,            /* 115 sets the name of static variable */
   HB_P_SWAPALIAS,             /* 116 restores the current workarea number from the eval stack */
   HB_P_TRUE,                  /* 117 pushes true on the virtual machine stack */
   HB_P_ZERO,                  /* 118 places a ZERO on the virtual machine stack */
   HB_P_ONE,                   /* 119 places a ONE on the virtual machine stack */
/* NOTE: This have to be the last definition */
   HB_P_LAST_PCODE             /* 120 this defines the number of defined pcodes */
} HB_PCODE;

#endif /* HB_PCODE_H_ */

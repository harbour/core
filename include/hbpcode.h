/*
 * Harbour Project source code:
 * Header file for the PCODE declarations
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

#ifndef HB_PCODE_H_
#define HB_PCODE_H_

/* NOTE:
 *
 * IMPORTANT:
 *   Always add new pcodes to the end of the list to maintain compatibility.
 *
 */

/* NOTE:
 * Please update any opcode lookup tables present in
 *  genc.c
 *  harbour.c
 * when new opcode is added
 */
typedef enum
{
   HB_P_AND = 0,               /*   0 performs the logical AND of two latest stack values, removes them and places result */
   HB_P_ARRAYPUSH,             /*   1 places on the virtual machine stack an array element */
   HB_P_ARRAYPOP,              /*   2 pops a value from the eval stack into an array element */
   HB_P_ARRAYDIM,              /*   3 instructs the virtual machine to build an array with some specific dimensions */
   HB_P_ARRAYGEN,              /*   4 instructs the virtual machine to build an array and load element from the stack */
   HB_P_EQUAL,                 /*   5 check if the latest two values on the stack are equal, removing them and leaving the result */
   HB_P_ENDBLOCK,              /*   6 end of a codeblock definition */
   HB_P_ENDPROC,               /*   7 instructs the virtual machine to end execution */
   HB_P_EXACTLYEQUAL,          /*   8 check if the latest two values on the stack are exactly equal, removing them and leaving the result */
   HB_P_FALSE,                 /*   9 pushes false on the virtual machine stack */
   HB_P_FORTEST,               /*  10 For STEP. If step > 1 less. If step < 1 greater. */
   HB_P_FUNCTION,              /*  11 instructs the virtual machine to execute a function saving its result */
   HB_P_FUNCTIONSHORT,         /*  12 instructs the virtual machine to execute a function saving its result */
   HB_P_FRAME,                 /*  13 instructs the virtual machine about how many parameters and locals a function uses */
   HB_P_FUNCPTR,               /*  14 returns a function address pointer */
   HB_P_GREATER,               /*  15 checks if the second latest value on the stack is greater that the lastest one */
   HB_P_GREATEREQUAL,          /*  16 checks if the second latest value on the stack is greater equal that the latest one, leaves the result only */
   HB_P_DEC,                   /*  17 decrements the latest value on the virtual machine stack */
   HB_P_DIVIDE,                /*  18 divides the latest two values on the stack, removing them and leaving the result */
   HB_P_DO,                    /*  19 instructs the virtual machine to execute a function discarding its result */
   HB_P_DOSHORT,               /*  20 instructs the virtual machine to execute a function discarding its result */
   HB_P_DUPLICATE,             /*  21 places a copy of the latest virtual machine stack value on to the stack */
   HB_P_PUSHTIMESTAMP,         /*  22 places a timestamp constant value on the virtual machine stack */
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
   HB_P_MACROARRAYGEN,         /*  41 generate array from arguments set(s) on HVM stack { &var } */
   HB_P_MACROPUSHLIST,         /*  42 compile and run - leave the result on the stack */
   HB_P_MACROPUSHINDEX,        /*  43 push array items using macro array index */
   HB_P_MACROPUSHPARE,         /*  44 compile and run - leave the result on the stack */
   HB_P_MACROPUSHALIASED,      /*  45 compile and run - leave the field value on the stack */
   HB_P_MACROSYMBOL,           /*  46 compile into a symbol name (used in function calls) */
   HB_P_MACROTEXT,             /*  47 macro text substitution */
   HB_P_MESSAGE,               /*  48 sends a message to an object */
   HB_P_MINUS,                 /*  49 subs the latest two values on the stack, removing them and leaving the result */
   HB_P_MODULUS,               /*  50 calculates the modulus of the two values on the stack, removing them and leaving the result */
   HB_P_MODULENAME,            /*  51 sets the name of debugged module */
/* start: pcodes generated by the macro compiler - the symbol address is used */
   HB_P_MMESSAGE,              /*  52 */
   HB_P_MPOPALIASEDFIELD,      /*  53 */
   HB_P_MPOPALIASEDVAR,        /*  54 */
   HB_P_MPOPFIELD,             /*  55 */
   HB_P_MPOPMEMVAR,            /*  56 */
   HB_P_MPUSHALIASEDFIELD,     /*  57 */
   HB_P_MPUSHALIASEDVAR,       /*  58 */
   HB_P_MPUSHBLOCK,            /*  59 */
   HB_P_MPUSHFIELD,            /*  60 */
   HB_P_MPUSHMEMVAR,           /*  61 */
   HB_P_MPUSHMEMVARREF,        /*  62 */
   HB_P_MPUSHSYM,              /*  63 */
   HB_P_MPUSHVARIABLE,         /*  64 */
/* end: */
   HB_P_MULT,                  /*  65 multiplies the latest two values on the stack, removing them and leaving the result */
   HB_P_NEGATE,                /*  66 numerically negates the latest value on the stack */
   HB_P_NOOP,                  /*  67 no operation */
   HB_P_NOT,                   /*  68 logically negates the latest value on the stack */
   HB_P_NOTEQUAL,              /*  69 checks if the latest two stack values are equal, leaves just the result */
   HB_P_OR,                    /*  70 performs the logical OR of two latest stack values, removes them and places result */
   HB_P_PARAMETER,             /*  71 creates PRIVATE variables and assigns values to functions paramaters */
   HB_P_PLUS,                  /*  72 adds the latest two values on the stack, removing them and leaving the result */
   HB_P_POP,                   /*  73 removes the latest value from the stack */
   HB_P_POPALIAS,              /*  74 pops the item from the eval stack and selects the current workarea */
   HB_P_POPALIASEDFIELD,       /*  75 pops aliased field */
   HB_P_POPALIASEDFIELDNEAR,   /*  76 pops aliased field */
   HB_P_POPALIASEDVAR,         /*  77 pops aliased variable (either a field or a memvar) */
   HB_P_POPFIELD,              /*  78 pops unaliased field */
   HB_P_POPLOCAL,              /*  79 pops the contents of the virtual machine stack onto a local variable */
   HB_P_POPLOCALNEAR,          /*  80 pops the contents of the virtual machine stack onto a local variable */
   HB_P_POPMEMVAR,             /*  81 pops the contents of a memvar variable to the virtual machine stack */
   HB_P_POPSTATIC,             /*  82 pops the contents of the virtual machine stack onto a static variable */
   HB_P_POPVARIABLE,           /*  83 pops the contents of an undeclared variable from the virtual machine stack */
   HB_P_POWER,                 /*  84 calculates the power of the two values on the stack, removing them and leaving the result */
   HB_P_PUSHALIAS,             /*  85 saves the current workarea number on the eval stack */
   HB_P_PUSHALIASEDFIELD,      /*  86 pushes aliased field */
   HB_P_PUSHALIASEDFIELDNEAR,  /*  87 pushes aliased field */
   HB_P_PUSHALIASEDVAR,        /*  88 pushes aliased variable (either a field or a memvar) */
   HB_P_PUSHBLOCK,             /*  89 start of a codeblock definition */
   HB_P_PUSHBLOCKSHORT,        /*  90 start of a codeblock definition */
   HB_P_PUSHFIELD,             /*  91 pushes an unaliased field */
   HB_P_PUSHBYTE,              /*  92 places a 1 byte integer number on the virtual machine stack */
   HB_P_PUSHINT,               /*  93 places an integer number on the virtual machine stack */
   HB_P_PUSHLOCAL,             /*  94 pushes the contents of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALNEAR,         /*  95 pushes the contents of a local variable to the virtual machine stack */
   HB_P_PUSHLOCALREF,          /*  96 pushes a local variable by reference to the virtual machine stack */
   HB_P_PUSHLONG,              /*  97 places an integer number on the virtual machine stack */
   HB_P_PUSHMEMVAR,            /*  98 pushes the contents of a memvar variable to the virtual machine stack */
   HB_P_PUSHMEMVARREF,         /*  99 pushes the a memvar variable by reference to the virtual machine stack */
   HB_P_PUSHNIL,               /* 100 places a nil on the virtual machine stack */
   HB_P_PUSHDOUBLE,            /* 101 places a double number on the virtual machine stack */
   HB_P_PUSHSELF,              /* 102 pushes Self for the current processed method */
   HB_P_PUSHSTATIC,            /* 103 pushes the contents of a static variable to the virtual machine stack */
   HB_P_PUSHSTATICREF,         /* 104 pushes the a static variable by reference to the virtual machine stack */
   HB_P_PUSHSTR,               /* 105 places a string on the virtual machine stack */
   HB_P_PUSHSTRSHORT,          /* 106 places a string on the virtual machine stack */
   HB_P_PUSHSYM,               /* 107 places a symbol on the virtual machine stack */
   HB_P_PUSHSYMNEAR,           /* 108 places a symbol on the virtual machine stack */
   HB_P_PUSHVARIABLE,          /* 109 pushes the contents of an undeclared variable to the virtual machine stack */
   HB_P_RETVALUE,              /* 110 instructs the virtual machine to return the latest stack value */
   HB_P_SEND,                  /* 111 send operator */
   HB_P_SENDSHORT,             /* 112 send operator */
   HB_P_SEQBEGIN,              /* 113 BEGIN SEQUENCE */
   HB_P_SEQEND,                /* 114 END SEQUENCE */
   HB_P_SEQRECOVER,            /* 115 RECOVER statement */
   HB_P_SFRAME,                /* 116 sets the statics frame for a function */
   HB_P_STATICS,               /* 117 defines the number of statics variables for a PRG */
   HB_P_STATICNAME,            /* 118 sets the name of static variable */
   HB_P_SWAPALIAS,             /* 119 restores the current workarea number from the eval stack */
   HB_P_TRUE,                  /* 120 pushes true on the virtual machine stack */
   HB_P_ZERO,                  /* 121 places a ZERO on the virtual machine stack */
   HB_P_ONE,                   /* 122 places a ONE on the virtual machine stack */
   HB_P_MACROFUNC,             /* 123 execute a function saving its result */
   HB_P_MACRODO,               /* 124 execute a function discarding its result */
   HB_P_MPUSHSTR,              /* 125 Macro compiled pushed string */
   HB_P_LOCALNEARADDINT,       /* 126 Add/Subtract specified int into specified local without using the stack. */
   HB_P_MACROPUSHREF,          /* 127 Reference to macro variable @&mvar */
   HB_P_PUSHLONGLONG,          /* 128 places an integer number on the virtual machine stack */
   HB_P_ENUMSTART,             /* 129 Start of FOR EACH loop */
   HB_P_ENUMNEXT,              /* 130 Next item of FOR EACH loop  */
   HB_P_ENUMPREV,              /* 131 Previous item of FOR EACH loop  */
   HB_P_ENUMEND,               /* 132 End of FOR EACH loop */
   HB_P_SWITCH,                /* 133 SWITCH using long values */
   HB_P_PUSHDATE,              /* 134 places a data constant value on the virtual machine stack */
/* optimalization of inlined math operations */
   HB_P_PLUSEQPOP,             /* 135 adds a value to the variable reference */
   HB_P_MINUSEQPOP,            /* 136 subs a value from the variable reference */
   HB_P_MULTEQPOP,             /* 137 multiplies a variable reference by a value */
   HB_P_DIVEQPOP,              /* 138 divides the var reference by a value */
   HB_P_PLUSEQ,                /* 139 adds a value to the variable reference, leave result on the stack */
   HB_P_MINUSEQ,               /* 140 subs a value from the variable reference, leave result on the stack */
   HB_P_MULTEQ,                /* 141 multiplies a variable reference by a value, leave result on the stack */
   HB_P_DIVEQ,                 /* 142 divides the var reference by a value, leave result on the stack */
   HB_P_WITHOBJECTSTART,       /* 143 start WITH OBJECT code */
   HB_P_WITHOBJECTMESSAGE,     /* 144 push message for WITH OBJECT */
   HB_P_WITHOBJECTEND,         /* 145 end WITH OBJECT code */
   HB_P_MACROSEND,             /* 146 send operator with macrlist params */
   HB_P_PUSHOVARREF,           /* 147 pushes reference to object variable */
   HB_P_ARRAYPUSHREF,          /* 148 pushes reference to array element */
   HB_P_VFRAME,                /* 149 frame with variable number of parameters */
   HB_P_LARGEFRAME,            /* 150 frame with more then 255 locals */
   HB_P_LARGEVFRAME,           /* 151 frame with variable number of parameters and more then 255 locals */
   HB_P_PUSHSTRHIDDEN,         /* 152 places a "hidden" string on the virtual machine stack */
   HB_P_LOCALADDINT,           /* 153 Add/Subtract specified int into specified local without using the stack. */
   HB_P_MODEQPOP,              /* 154 calculates the modulus of var reference and a value */
   HB_P_EXPEQPOP,              /* 155 calculates the power of var reference and a value */
   HB_P_MODEQ,                 /* 156 calculates the modulus of var reference and a value, leave result on the stack */
   HB_P_EXPEQ,                 /* 157 calculates the power of var reference and a value, leave result on the stack */
   HB_P_DUPLUNREF,             /* 158 places a copy of the latest virtual machine stack value on to the stack and unreference the source one */
   HB_P_MPUSHBLOCKLARGE,       /* 159 code block generated by the macro compiler larger then 64kb */
   HB_P_MPUSHSTRLARGE,         /* 160 Macro compiled pushed string */
   HB_P_PUSHBLOCKLARGE,        /* 161 start of a codeblock definition */
   HB_P_PUSHSTRLARGE,          /* 162 places a string on the virtual machine stack */
   HB_P_SWAP,                  /* 163 swap n+1 times two items starting from the most top one on the virtual machine stack */
   HB_P_PUSHVPARAMS,           /* 164 push variable function/method parameters on HVM stack */
   HB_P_PUSHUNREF,             /* 165 push unreferenced top item on HVM stack */
   HB_P_SEQALWAYS,             /* 166 set BEGIN SEQUENCE/ALWAYS section */
   HB_P_ALWAYSBEGIN,           /* 167 start ALWAYS section */
   HB_P_ALWAYSEND,             /* 168 finish ALWAYS section */
   HB_P_DECEQPOP,              /* 169 decrements the var reference */
   HB_P_INCEQPOP,              /* 170 increments the var reference */
   HB_P_DECEQ,                 /* 171 decrements the var reference, leave result on the stack */
   HB_P_INCEQ,                 /* 172 increments the var reference, leave result on the stack */
   HB_P_LOCALDEC,              /* 173 decrements the local variable */
   HB_P_LOCALINC,              /* 174 increments the local variable */
   HB_P_LOCALINCPUSH,          /* 175 increments the local variable, push result on the stack */
   HB_P_PUSHFUNCSYM,           /* 176 places a symbol on the virtual machine stack */
   HB_P_HASHGEN,               /* 177 instructs the virtual machine to build a hash and load element from the stack */
   HB_P_SEQBLOCK,              /* 178 set BEQIN SEQUENCE WITH block */
   HB_P_THREADSTATICS,         /* 179 mark thread static variables */
   HB_P_PUSHAPARAMS,           /* 180 push array items on HVM stack */
   HB_P_LAST_PCODE             /* 181 this defines the number of defined pcodes */
} HB_PCODE;

#endif /* HB_PCODE_H_ */

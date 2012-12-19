/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler PCode generation functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
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


#include "hbcomp.h"
#include "hbassert.h"

#define HB_PSIZE_FUNC( func )  HB_PCODE_FUNC( func, PHB_VOID )

/*
 * functions for variable size PCODE tracing
 */
static HB_PSIZE_FUNC( hb_p_pushstrshort )
{
   HB_SYMBOL_UNUSED( cargo );
   return 2 + pFunc->pCode[ nPCodePos + 1 ];
}

static HB_PSIZE_FUNC( hb_p_pushstr )
{
   HB_SYMBOL_UNUSED( cargo );
   return 3 + HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
}

static HB_PSIZE_FUNC( hb_p_pushstrlarge )
{
   HB_SYMBOL_UNUSED( cargo );
   return 4 + HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
}

static HB_PSIZE_FUNC( hb_p_pushstrhidden )
{
   HB_SYMBOL_UNUSED( cargo );
   return 4 + HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
}

static HB_PSIZE_FUNC( hb_p_pushblock )
{
   HB_SYMBOL_UNUSED( cargo );
   return HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
}

static HB_PSIZE_FUNC( hb_p_pushblockshort )
{
   HB_SYMBOL_UNUSED( cargo );
   return pFunc->pCode[ nPCodePos + 1 ];
}

static HB_PSIZE_FUNC( hb_p_pushblocklarge )
{
   HB_SYMBOL_UNUSED( cargo );
   return HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
}

static HB_PSIZE_FUNC( hb_p_localname )
{
   HB_SIZE nStart = nPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   nPCodePos += 3;
   while( pFunc->pCode[ nPCodePos++ ] )
      ;

   return nPCodePos - nStart;
}

static HB_PSIZE_FUNC( hb_p_modulename )
{
   HB_SIZE nStart = nPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   nPCodePos++;
   while( pFunc->pCode[ nPCodePos++ ])
      ;

   return nPCodePos - nStart;
}

static HB_PSIZE_FUNC( hb_p_staticname )
{
   HB_SIZE nStart = nPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   nPCodePos += 4;
   while( pFunc->pCode[ nPCodePos++ ] )
      ;

   return nPCodePos - nStart;
}

static HB_PSIZE_FUNC( hb_p_threadstatics )
{
   HB_SYMBOL_UNUSED( cargo );
   return 3 + ( ( HB_SIZE ) HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) << 1 );
}

const HB_BYTE hb_comp_pcode_len[] = {
   1,        /* HB_P_AND,                  */
   1,        /* HB_P_ARRAYPUSH,            */
   1,        /* HB_P_ARRAYPOP,             */
   3,        /* HB_P_ARRAYDIM,             */
   3,        /* HB_P_ARRAYGEN,             */
   1,        /* HB_P_EQUAL,                */
   1,        /* HB_P_ENDBLOCK,             */
   1,        /* HB_P_ENDPROC,              */
   1,        /* HB_P_EXACTLYEQUAL,         */
   1,        /* HB_P_FALSE,                */
   1,        /* HB_P_FORTEST,              */
   3,        /* HB_P_FUNCTION,             */
   2,        /* HB_P_FUNCTIONSHORT,        */
   3,        /* HB_P_FRAME,                */
   1,        /* HB_P_FUNCPTR,              */
   1,        /* HB_P_GREATER,              */
   1,        /* HB_P_GREATEREQUAL,         */
   1,        /* HB_P_DEC,                  */
   1,        /* HB_P_DIVIDE,               */
   3,        /* HB_P_DO,                   */
   2,        /* HB_P_DOSHORT,              */
   1,        /* HB_P_DUPLICATE,            */
   9,        /* HB_P_PUSHTIMESTAMP,        */
   1,        /* HB_P_INC,                  */
   1,        /* HB_P_INSTRING,             */
   2,        /* HB_P_JUMPNEAR,             */
   3,        /* HB_P_JUMP,                 */
   4,        /* HB_P_JUMPFAR,              */
   2,        /* HB_P_JUMPFALSENEAR,        */
   3,        /* HB_P_JUMPFALSE,            */
   4,        /* HB_P_JUMPFALSEFAR,         */
   2,        /* HB_P_JUMPTRUENEAR,         */
   3,        /* HB_P_JUMPTRUE,             */
   4,        /* HB_P_JUMPTRUEFAR,          */
   1,        /* HB_P_LESSEQUAL,            */
   1,        /* HB_P_LESS,                 */
   3,        /* HB_P_LINE,                 */
   0,        /* HB_P_LOCALNAME,            */
   2,        /* HB_P_MACROPOP,             */
   2,        /* HB_P_MACROPOPALIASED,      */
   2,        /* HB_P_MACROPUSH,            */
   3,        /* HB_P_MACROARRAYGEN,        */
   2,        /* HB_P_MACROPUSHLIST,        */
   1,        /* HB_P_MACROPUSHINDEX,       */
   2,        /* HB_P_MACROPUSHPARE,        */
   2,        /* HB_P_MACROPUSHALIASED,     */
   1,        /* HB_P_MACROSYMBOL,          */
   1,        /* HB_P_MACROTEXT,            */
   3,        /* HB_P_MESSAGE,              */
   1,        /* HB_P_MINUS,                */
   1,        /* HB_P_MODULUS,              */
   0,        /* HB_P_MODULENAME,           */
             /* start: pcodes generated by macro compiler */
   3,        /* HB_P_MMESSAGE,             */
   3,        /* HB_P_MPOPALIASEDFIELD,     */
   3,        /* HB_P_MPOPALIASEDVAR,       */
   3,        /* HB_P_MPOPFIELD,            */
   3,        /* HB_P_MPOPMEMVAR,           */
   3,        /* HB_P_MPUSHALIASEDFIELD,    */
   3,        /* HB_P_MPUSHALIASEDVAR,      */
   0,        /* HB_P_MPUSHBLOCK,           */
   3,        /* HB_P_MPUSHFIELD,           */
   3,        /* HB_P_MPUSHMEMVAR,          */
   3,        /* HB_P_MPUSHMEMVARREF,       */
   3,        /* HB_P_MPUSHSYM,             */
   3,        /* HB_P_MPUSHVARIABLE,        */
             /* end: */
   1,        /* HB_P_MULT,                 */
   1,        /* HB_P_NEGATE,               */
   1,        /* HB_P_NOOP,                 */
   1,        /* HB_P_NOT,                  */
   1,        /* HB_P_NOTEQUAL,             */
   1,        /* HB_P_OR,                   */
   4,        /* HB_P_PARAMETER,            */
   1,        /* HB_P_PLUS,                 */
   1,        /* HB_P_POP,                  */
   1,        /* HB_P_POPALIAS,             */
   3,        /* HB_P_POPALIASEDFIELD,      */
   2,        /* HB_P_POPALIASEDFIELDNEAR,  */
   3,        /* HB_P_POPALIASEDVAR,        */
   3,        /* HB_P_POPFIELD,             */
   3,        /* HB_P_POPLOCAL,             */
   2,        /* HB_P_POPLOCALNEAR,         */
   3,        /* HB_P_POPMEMVAR,            */
   3,        /* HB_P_POPSTATIC,            */
   3,        /* HB_P_POPVARIABLE,          */
   1,        /* HB_P_POWER,                */
   1,        /* HB_P_PUSHALIAS,            */
   3,        /* HB_P_PUSHALIASEDFIELD,     */
   2,        /* HB_P_PUSHALIASEDFIELDNEAR, */
   3,        /* HB_P_PUSHALIASEDVAR,       */
   0,        /* HB_P_PUSHBLOCK,            */
   0,        /* HB_P_PUSHBLOCKSHORT,       */
   3,        /* HB_P_PUSHFIELD,            */
   2,        /* HB_P_PUSHBYTE,             */
   3,        /* HB_P_PUSHINT,              */
   3,        /* HB_P_PUSHLOCAL,            */
   2,        /* HB_P_PUSHLOCALNEAR,        */
   3,        /* HB_P_PUSHLOCALREF,         */
   5,        /* HB_P_PUSHLONG,             */
   3,        /* HB_P_PUSHMEMVAR,           */
   3,        /* HB_P_PUSHMEMVARREF,        */
   1,        /* HB_P_PUSHNIL,              */
   1 + sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ),        /* HB_P_PUSHDOUBLE,           */
   1,        /* HB_P_PUSHSELF,             */
   3,        /* HB_P_PUSHSTATIC,           */
   3,        /* HB_P_PUSHSTATICREF,        */
   0,        /* HB_P_PUSHSTR,              */
   0,        /* HB_P_PUSHSTRSHORT,         */
   3,        /* HB_P_PUSHSYM,              */
   2,        /* HB_P_PUSHSYMNEAR,          */
   3,        /* HB_P_PUSHVARIABLE,         */
   1,        /* HB_P_RETVALUE,             */
   3,        /* HB_P_SEND,                 */
   2,        /* HB_P_SENDSHORT,            */
   4,        /* HB_P_SEQBEGIN,             */
   4,        /* HB_P_SEQEND,               */
   1,        /* HB_P_SEQRECOVER,           */
   3,        /* HB_P_SFRAME,               */
   5,        /* HB_P_STATICS,              */
   0,        /* HB_P_STATICNAME,           */
   1,        /* HB_P_SWAPALIAS,            */
   1,        /* HB_P_TRUE,                 */
   1,        /* HB_P_ZERO,                 */
   1,        /* HB_P_ONE,                  */
   3,        /* HB_P_MACROFUNC,            */
   3,        /* HB_P_MACRODO,              */
   0,        /* HB_P_MPUSHSTR              */
   4,        /* HB_P_LOCALNEARADDINT,      */
   1,        /* HB_P_MACROPUSHREF          */
   9,        /* HB_P_PUSHLONGLONG          */
   3,        /* HB_P_ENUMSTART             */
   1,        /* HB_P_ENUMNEXT              */
   1,        /* HB_P_ENUMPREV              */
   1,        /* HB_P_ENUMEND               */
   3,        /* HB_P_SWITCH                */
   5,        /* HB_P_PUSHDATE,             */
             /* optimalization of inlined math operations */
   1,        /* HB_P_PLUSEQPOP,            */
   1,        /* HB_P_MINUSEQPOP,           */
   1,        /* HB_P_MULTEQPOP,            */
   1,        /* HB_P_DIVEQPOP,             */
   1,        /* HB_P_PLUSEQ,               */
   1,        /* HB_P_MINUSEQ,              */
   1,        /* HB_P_MULTEQ,               */
   1,        /* HB_P_DIVEQ,                */
   1,        /* HB_P_WITHOBJECTSTART,      */
   3,        /* HB_P_WITHOBJECTMESSAGE,    */
   1,        /* HB_P_WITHOBJECTEND,        */
   3,        /* HB_P_MACROSEND,            */
   1,        /* HB_P_PUSHOVARREF,          */
   1,        /* HB_P_ARRAYPUSHREF          */
   3,        /* HB_P_VFRAME                */
   4,        /* HB_P_LARGEFRAME            */
   4,        /* HB_P_LARGEVFRAME           */
   0,        /* HB_P_PUSHSTRHIDDEN         */
   5,        /* HB_P_LOCALADDINT           */
   1,        /* HB_P_MODEQPOP              */
   1,        /* HB_P_EXPEQPOP              */
   1,        /* HB_P_MODEQ                 */
   1,        /* HB_P_EXPEQ                 */
   1,        /* HB_P_DUPLUNREF             */
   0,        /* HB_P_MPUSHBLOCKLARGE,      */
   0,        /* HB_P_MPUSHSTRLARGE         */
   0,        /* HB_P_PUSHBLOCKLAREG,       */
   0,        /* HB_P_PUSHSTRLARGE          */
   2,        /* HB_P_SWAP                  */
   1,        /* HB_P_PUSHVPARAMS           */
   1,        /* HB_P_PUSHUNREF             */
   4,        /* HB_P_SEQALWAYS             */
   4,        /* HB_P_ALWAYSBEGIN           */
   1,        /* HB_P_ALWAYSEND             */
   1,        /* HB_P_DECEQPOP              */
   1,        /* HB_P_INCEQPOP              */
   1,        /* HB_P_DECEQ                 */
   1,        /* HB_P_INCEQ                 */
   3,        /* HB_P_LOCALDEC              */
   3,        /* HB_P_LOCALINC              */
   3,        /* HB_P_LOCALINCPUSH          */
   3,        /* HB_P_PUSHFUNCSYM           */
   3,        /* HB_P_HASHGEN               */
   1,        /* HB_P_SEQBLOCK              */
   0,        /* HB_P_THREADSTATICS         */
   1         /* HB_P_PUSHAPARAMS           */
};

/*
 * this table has pointers to functions which count
 * real size of variable size PCODEs
 */
static PHB_PCODE_FUNC s_psize_table[] =
{
   NULL,                       /* HB_P_AND,                  */
   NULL,                       /* HB_P_ARRAYPUSH,            */
   NULL,                       /* HB_P_ARRAYPOP,             */
   NULL,                       /* HB_P_ARRAYDIM,             */
   NULL,                       /* HB_P_ARRAYGEN,             */
   NULL,                       /* HB_P_EQUAL,                */
   NULL,                       /* HB_P_ENDBLOCK,             */
   NULL,                       /* HB_P_ENDPROC,              */
   NULL,                       /* HB_P_EXACTLYEQUAL,         */
   NULL,                       /* HB_P_FALSE,                */
   NULL,                       /* HB_P_FORTEST,              */
   NULL,                       /* HB_P_FUNCTION,             */
   NULL,                       /* HB_P_FUNCTIONSHORT,        */
   NULL,                       /* HB_P_FRAME,                */
   NULL,                       /* HB_P_FUNCPTR,              */
   NULL,                       /* HB_P_GREATER,              */
   NULL,                       /* HB_P_GREATEREQUAL,         */
   NULL,                       /* HB_P_DEC,                  */
   NULL,                       /* HB_P_DIVIDE,               */
   NULL,                       /* HB_P_DO,                   */
   NULL,                       /* HB_P_DOSHORT,              */
   NULL,                       /* HB_P_DUPLICATE,            */
   NULL,                       /* HB_P_PUSHTIMESTAMP,        */
   NULL,                       /* HB_P_INC,                  */
   NULL,                       /* HB_P_INSTRING,             */
   NULL,                       /* HB_P_JUMPNEAR,             */
   NULL,                       /* HB_P_JUMP,                 */
   NULL,                       /* HB_P_JUMPFAR,              */
   NULL,                       /* HB_P_JUMPFALSENEAR,        */
   NULL,                       /* HB_P_JUMPFALSE,            */
   NULL,                       /* HB_P_JUMPFALSEFAR,         */
   NULL,                       /* HB_P_JUMPTRUENEAR,         */
   NULL,                       /* HB_P_JUMPTRUE,             */
   NULL,                       /* HB_P_JUMPTRUEFAR,          */
   NULL,                       /* HB_P_LESSEQUAL,            */
   NULL,                       /* HB_P_LESS,                 */
   NULL,                       /* HB_P_LINE,                 */
   hb_p_localname,             /* HB_P_LOCALNAME,            */
   NULL,                       /* HB_P_MACROPOP,             */
   NULL,                       /* HB_P_MACROPOPALIASED,      */
   NULL,                       /* HB_P_MACROPUSH,            */
   NULL,                       /* HB_P_MACROARRAYGEN,        */
   NULL,                       /* HB_P_MACROPUSHLIST,        */
   NULL,                       /* HB_P_MACROPUSHINDEX,       */
   NULL,                       /* HB_P_MACROPUSHPARE,        */
   NULL,                       /* HB_P_MACROPUSHALIASED,     */
   NULL,                       /* HB_P_MACROSYMBOL,          */
   NULL,                       /* HB_P_MACROTEXT,            */
   NULL,                       /* HB_P_MESSAGE,              */
   NULL,                       /* HB_P_MINUS,                */
   NULL,                       /* HB_P_MODULUS,              */
   hb_p_modulename,            /* HB_P_MODULENAME,           */
                               /* start: pcodes generated by macro compiler */
   NULL,                       /* HB_P_MMESSAGE,             */
   NULL,                       /* HB_P_MPOPALIASEDFIELD,     */
   NULL,                       /* HB_P_MPOPALIASEDVAR,       */
   NULL,                       /* HB_P_MPOPFIELD,            */
   NULL,                       /* HB_P_MPOPMEMVAR,           */
   NULL,                       /* HB_P_MPUSHALIASEDFIELD,    */
   NULL,                       /* HB_P_MPUSHALIASEDVAR,      */
   NULL,                       /* HB_P_MPUSHBLOCK,           */
   NULL,                       /* HB_P_MPUSHFIELD,           */
   NULL,                       /* HB_P_MPUSHMEMVAR,          */
   NULL,                       /* HB_P_MPUSHMEMVARREF,       */
   NULL,                       /* HB_P_MPUSHSYM,             */
   NULL,                       /* HB_P_MPUSHVARIABLE,        */
                               /* end: */
   NULL,                       /* HB_P_MULT,                 */
   NULL,                       /* HB_P_NEGATE,               */
   NULL,                       /* HB_P_NOOP,                 */
   NULL,                       /* HB_P_NOT,                  */
   NULL,                       /* HB_P_NOTEQUAL,             */
   NULL,                       /* HB_P_OR,                   */
   NULL,                       /* HB_P_PARAMETER,            */
   NULL,                       /* HB_P_PLUS,                 */
   NULL,                       /* HB_P_POP,                  */
   NULL,                       /* HB_P_POPALIAS,             */
   NULL,                       /* HB_P_POPALIASEDFIELD,      */
   NULL,                       /* HB_P_POPALIASEDFIELDNEAR,  */
   NULL,                       /* HB_P_POPALIASEDVAR,        */
   NULL,                       /* HB_P_POPFIELD,             */
   NULL,                       /* HB_P_POPLOCAL,             */
   NULL,                       /* HB_P_POPLOCALNEAR,         */
   NULL,                       /* HB_P_POPMEMVAR,            */
   NULL,                       /* HB_P_POPSTATIC,            */
   NULL,                       /* HB_P_POPVARIABLE,          */
   NULL,                       /* HB_P_POWER,                */
   NULL,                       /* HB_P_PUSHALIAS,            */
   NULL,                       /* HB_P_PUSHALIASEDFIELD,     */
   NULL,                       /* HB_P_PUSHALIASEDFIELDNEAR, */
   NULL,                       /* HB_P_PUSHALIASEDVAR,       */
   hb_p_pushblock,             /* HB_P_PUSHBLOCK,            */
   hb_p_pushblockshort,        /* HB_P_PUSHBLOCKSHORT,       */
   NULL,                       /* HB_P_PUSHFIELD,            */
   NULL,                       /* HB_P_PUSHBYTE,             */
   NULL,                       /* HB_P_PUSHINT,              */
   NULL,                       /* HB_P_PUSHLOCAL,            */
   NULL,                       /* HB_P_PUSHLOCALNEAR,        */
   NULL,                       /* HB_P_PUSHLOCALREF,         */
   NULL,                       /* HB_P_PUSHLONG,             */
   NULL,                       /* HB_P_PUSHMEMVAR,           */
   NULL,                       /* HB_P_PUSHMEMVARREF,        */
   NULL,                       /* HB_P_PUSHNIL,              */
   NULL,                       /* HB_P_PUSHDOUBLE,           */
   NULL,                       /* HB_P_PUSHSELF,             */
   NULL,                       /* HB_P_PUSHSTATIC,           */
   NULL,                       /* HB_P_PUSHSTATICREF,        */
   hb_p_pushstr,               /* HB_P_PUSHSTR,              */
   hb_p_pushstrshort,          /* HB_P_PUSHSTRSHORT,         */
   NULL,                       /* HB_P_PUSHSYM,              */
   NULL,                       /* HB_P_PUSHSYMNEAR,          */
   NULL,                       /* HB_P_PUSHVARIABLE,         */
   NULL,                       /* HB_P_RETVALUE,             */
   NULL,                       /* HB_P_SEND,                 */
   NULL,                       /* HB_P_SENDSHORT,            */
   NULL,                       /* HB_P_SEQBEGIN,             */
   NULL,                       /* HB_P_SEQEND,               */
   NULL,                       /* HB_P_SEQRECOVER,           */
   NULL,                       /* HB_P_SFRAME,               */
   NULL,                       /* HB_P_STATICS,              */
   hb_p_staticname,            /* HB_P_STATICNAME,           */
   NULL,                       /* HB_P_SWAPALIAS,            */
   NULL,                       /* HB_P_TRUE,                 */
   NULL,                       /* HB_P_ZERO,                 */
   NULL,                       /* HB_P_ONE,                  */
   NULL,                       /* HB_P_MACROFUNC,            */
   NULL,                       /* HB_P_MACRODO,              */
   NULL,                       /* HB_P_MPUSHSTR,             */
   NULL,                       /* HB_P_LOCALNEARADDINT,      */
   NULL,                       /* HB_P_MACROPUSHREF          */
   NULL,                       /* HB_P_PUSHLONGLONG          */
   NULL,                       /* HB_P_ENUMSTART             */
   NULL,                       /* HB_P_ENUMNEXT              */
   NULL,                       /* HB_P_ENUMPREV              */
   NULL,                       /* HB_P_ENUMEND               */
   NULL,                       /* HB_P_SWITCH                */
   NULL,                       /* HB_P_PUSHDATE              */
                               /* optimalization of inlined math operations */
   NULL,                       /* HB_P_PLUSEQPOP             */
   NULL,                       /* HB_P_MINUSEQPOP            */
   NULL,                       /* HB_P_MULTEQPOP             */
   NULL,                       /* HB_P_DIVEQPOP              */
   NULL,                       /* HB_P_PLUSEQ                */
   NULL,                       /* HB_P_MINUSEQ               */
   NULL,                       /* HB_P_MULTEQ                */
   NULL,                       /* HB_P_DIVEQ                 */
   NULL,                       /* HB_P_WITHOBJECTSTART       */
   NULL,                       /* HB_P_WITHOBJECTMESSAGE     */
   NULL,                       /* HB_P_WITHOBJECTEND         */
   NULL,                       /* HB_P_MACROSEND             */
   NULL,                       /* HB_P_PUSHOVARREF           */
   NULL,                       /* HB_P_ARRAYPUSHREF          */
   NULL,                       /* HB_P_VFRAME                */
   NULL,                       /* HB_P_LARGEFRAME            */
   NULL,                       /* HB_P_LARGEVFRAME           */
   hb_p_pushstrhidden,         /* HB_P_PUSHSTRHIDDEN         */
   NULL,                       /* HB_P_LOCALADDINT           */
   NULL,                       /* HB_P_MODEQPOP              */
   NULL,                       /* HB_P_EXPEQPOP              */
   NULL,                       /* HB_P_MODEQ                 */
   NULL,                       /* HB_P_EXPEQ                 */
   NULL,                       /* HB_P_DUPLUNREF             */
   NULL,                       /* HB_P_MPUSHBLOCKLARGE       */
   NULL,                       /* HB_P_MPUSHSTRLARGE         */
   hb_p_pushblocklarge,        /* HB_P_PUSHBLOCKLARGE,       */
   hb_p_pushstrlarge,          /* HB_P_PUSHSTRLARGE          */
   NULL,                       /* HB_P_SWAP                  */
   NULL,                       /* HB_P_PUSHVPARAMS           */
   NULL,                       /* HB_P_PUSHUNREF             */
   NULL,                       /* HB_P_SEQALWAYS             */
   NULL,                       /* HB_P_ALWAYSBEGIN           */
   NULL,                       /* HB_P_ALWAYSEND             */
   NULL,                       /* HB_P_DECEQPOP              */
   NULL,                       /* HB_P_INCEQPOP              */
   NULL,                       /* HB_P_DECEQ                 */
   NULL,                       /* HB_P_INCEQ                 */
   NULL,                       /* HB_P_LOCALDEC              */
   NULL,                       /* HB_P_LOCALINC              */
   NULL,                       /* HB_P_LOCALINCPUSH          */
   NULL,                       /* HB_P_PUSHFUNCSYM           */
   NULL,                       /* HB_P_HASHGEN               */
   NULL,                       /* HB_P_SEQBLOCK              */
   hb_p_threadstatics,         /* HB_P_THREADSTATICS         */
   NULL                        /* HB_P_PUSHAPARAMS           */
};

HB_ISIZ hb_compPCodeSize( PHB_HFUNC pFunc, HB_SIZE nOffset )
{
   HB_ISIZ nSize = 0;
   HB_BYTE opcode = pFunc->pCode[ nOffset ];

   if( opcode < HB_P_LAST_PCODE )
   {
      nSize = hb_comp_pcode_len[ opcode ];

      if( nSize == 0 )
      {
         PHB_PCODE_FUNC pCall = s_psize_table[ opcode ];

         if( pCall != NULL )
            nSize = pCall( pFunc, nOffset, NULL );
      }
   }
   return nSize;
}

void hb_compPCodeEval( PHB_HFUNC pFunc, const PHB_PCODE_FUNC * pFunctions, void * cargo )
{
   HB_SIZE nPos = 0;
   HB_SIZE nSkip;
   HB_BYTE opcode;

   /* Make sure that table is correct */
   assert( sizeof( hb_comp_pcode_len ) == HB_P_LAST_PCODE );
   assert( sizeof( s_psize_table ) / sizeof( PHB_PCODE_FUNC ) == HB_P_LAST_PCODE );

   while( nPos < pFunc->nPCodePos )
   {
      opcode = pFunc->pCode[ nPos ];
      if( opcode < HB_P_LAST_PCODE )
      {
         PHB_PCODE_FUNC pCall = pFunctions[ opcode ];
         nSkip = pCall ? pCall( pFunc, nPos, cargo ) : 0;
         if( nSkip == 0 )
         {
            nSkip = hb_comp_pcode_len[ opcode ];
            if( nSkip == 0 )
            {
               pCall = s_psize_table[ opcode ];
               if( pCall != NULL )
                  nSkip = pCall( pFunc, nPos, NULL );
            }
         }

         if( nSkip == 0 )
         {
            char szOpcode[ 16 ];
            ++nPos;
            hb_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
            hb_errInternal( HB_EI_COMPBADOPSIZE, "Invalid (zero) opcode %s size in hb_compPCodeEval()", szOpcode, NULL );
         }
#if 0
         /*
          * Test code to validate return values by PCODE eval functions,
          * in some cases the eval functions can return intentionally differ
          * values so it's not enabled by default. [druzus]
          */
         if( hb_comp_pcode_len[ opcode ] != 0 && hb_comp_pcode_len[ opcode ] != nSkip )
         {
            char szMsg[ 100 ];
            hb_snprintf( szMsg, sizeof( szMsg ), "Wrong PCODE (%d) size (%ld!=%d)", opcode, nSkip, hb_comp_pcode_len[ opcode ] );
            hb_errInternal( HB_EI_COMPBADOPSIZE, szMsg, NULL, NULL );
         }
#endif
         nPos += nSkip;
      }
      else
      {
         char szOpcode[ 16 ];
         ++nPos;
         hb_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
         hb_errInternal( HB_EI_COMPBADOPCODE, "Invalid opcode: %s in hb_compPCodeEval()", szOpcode, NULL );
      }
   }
}

void hb_compPCodeTrace( PHB_HFUNC pFunc, const PHB_PCODE_FUNC * pFunctions, void * cargo )
{
   HB_SIZE nPos = 0;

   /* Make sure that table is correct */
   assert( sizeof( hb_comp_pcode_len ) == HB_P_LAST_PCODE );

   while( nPos < pFunc->nPCodePos )
   {
      HB_BYTE opcode = pFunc->pCode[ nPos ];
      if( opcode < HB_P_LAST_PCODE )
      {
         PHB_PCODE_FUNC pCall = pFunctions[ opcode ];
         if( pCall )
            nPos = pCall( pFunc, nPos, cargo );
         else
            nPos += hb_comp_pcode_len[ opcode ];
      }
      else
      {
         char szOpcode[ 16 ];
         ++nPos;
         hb_snprintf( szOpcode, sizeof( szOpcode ), "%i", opcode );
         hb_errInternal( HB_EI_COMPBADOPCODE, "Invalid opcode: %s in hb_compPCodeTrace()", szOpcode, NULL );
      }
   }
}

void hb_compGenPCode1( HB_BYTE byte, HB_COMP_DECL )
{
   PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( HB_BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->nPCodeSize = HB_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 1 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte;
}

void hb_compGenPCode2( HB_BYTE byte1, HB_BYTE byte2, HB_COMP_DECL )
{
   PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( HB_BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->nPCodeSize = HB_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 2 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
}

void hb_compGenPCode3( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_COMP_DECL )
{
   PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( HB_BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->nPCodeSize = HB_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 3 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
}

void hb_compGenPCode4( HB_BYTE byte1, HB_BYTE byte2, HB_BYTE byte3, HB_BYTE byte4, HB_COMP_DECL )
{
   PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->pCode      = ( HB_BYTE * ) hb_xgrab( HB_PCODE_CHUNK );
      pFunc->nPCodeSize = HB_PCODE_CHUNK;
      pFunc->nPCodePos  = 0;
   }
   else if( ( pFunc->nPCodeSize - pFunc->nPCodePos ) < 4 )
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize += HB_PCODE_CHUNK );

   pFunc->pCode[ pFunc->nPCodePos++ ] = byte1;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte2;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte3;
   pFunc->pCode[ pFunc->nPCodePos++ ] = byte4;
}

void hb_compGenPCodeN( const HB_BYTE * pBuffer, HB_SIZE nSize, HB_COMP_DECL )
{
   PHB_HFUNC pFunc = HB_COMP_PARAM->functions.pLast;   /* get the currently defined Clipper function */

   if( ! pFunc->pCode )                              /* has been created the memory block to hold the pcode ? */
   {
      pFunc->nPCodeSize = ( ( nSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK;
      pFunc->pCode      = ( HB_BYTE * ) hb_xgrab( pFunc->nPCodeSize );
      pFunc->nPCodePos  = 0;
   }
   else if( pFunc->nPCodePos + nSize > pFunc->nPCodeSize )
   {
      /* not enough free space in pcode buffer - increase it */
      pFunc->nPCodeSize += ( ( ( nSize / HB_PCODE_CHUNK ) + 1 ) * HB_PCODE_CHUNK );
      pFunc->pCode = ( HB_BYTE * ) hb_xrealloc( pFunc->pCode, pFunc->nPCodeSize );
   }

   memcpy( pFunc->pCode + pFunc->nPCodePos, pBuffer, nSize );
   pFunc->nPCodePos += nSize;
}

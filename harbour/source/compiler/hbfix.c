/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler main file
 *
 * Copyright 2000 Ryszard Glab <rglab@imid.med.pl>
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
#include <assert.h>

#include "hbcomp.h"


/* helper structure to pass information */
typedef struct HB_stru_fix_info
{
   USHORT iNestedCodeblock;
} HB_FIX_INFO, * HB_FIX_INFO_PTR;

#define HB_FIX_FUNC( func ) HB_PCODE_FUNC( func, HB_FIX_INFO_PTR )
typedef HB_FIX_FUNC( HB_FIX_FUNC_ );
typedef HB_FIX_FUNC_ * HB_FIX_FUNC_PTR;


static HB_FIX_FUNC( hb_p_pushstr )
{
   HB_SYMBOL_UNUSED( cargo );
   return 3 + pFunc->pCode[ lPCodePos + 1 ] +
              pFunc->pCode[ lPCodePos + 2 ] * 256;
}

static HB_FIX_FUNC( hb_p_pushstrshort )
{
   HB_SYMBOL_UNUSED( cargo );
   return 2 + pFunc->pCode[ lPCodePos + 1 ];
}

static HB_FIX_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   --cargo->iNestedCodeblock;
   return 1;
}

static HB_FIX_FUNC( hb_p_pushblock )
{
   USHORT wVar;
   USHORT * pLocal;
   ULONG ulStart = lPCodePos;

   ++cargo->iNestedCodeblock;

   wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );

   /* opcode + codeblock size + number of parameters + number of local variables */
   lPCodePos += 7;
   /* fix local variable's reference */
   while( wVar-- )
   {

      pLocal = ( USHORT * ) &( pFunc->pCode [ lPCodePos ] );
      *pLocal += pFunc->wParamCount;
      lPCodePos +=2;
   }
   return (USHORT) (lPCodePos - ulStart);
}

static HB_FIX_FUNC( hb_p_pushblockshort )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   ++cargo->iNestedCodeblock;

   return 2;
}

static HB_FIX_FUNC( hb_p_localname )
{
   ULONG ulStart = lPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
      ++lPCodePos;

   return (USHORT) (lPCodePos - ulStart + 1) ;
}

static HB_FIX_FUNC( hb_p_modulename )
{
   ULONG ulStart = lPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
      ++lPCodePos;

   return (USHORT) (lPCodePos - ulStart + 1);
}

static HB_FIX_FUNC( hb_p_poplocal )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      /* only local variables used outside of a codeblock need fixing
       */
      SHORT * pVar = ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ];

      *pVar += pFunc->wParamCount;
   }

   return (USHORT) 3;
}

static HB_FIX_FUNC( hb_p_pushlocal )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      /* only local variables used outside of a codeblock need fixing
       */
      SHORT * pVar = ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ];

      *pVar += pFunc->wParamCount;
   }

   return (USHORT) 3;
}

static HB_FIX_FUNC( hb_p_pushlocalref )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      /* only local variables used outside of a codeblock need fixing
       */
      SHORT * pVar = ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ];

      *pVar += pFunc->wParamCount;
   }

   return (USHORT) 3;
}

static HB_FIX_FUNC( hb_p_poplocalnear )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      /* only local variables used outside of a codeblock need fixing
       */
      SHORT * pVar = ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ];

      *pVar += pFunc->wParamCount;
      if( *pVar >= -128 && *pVar <= 127 )
      {
         pFunc->pCode[ lPCodePos + 2 ] = HB_P_NOOP;
         hb_compNOOPadd( pFunc, lPCodePos + 2 );
      }
      else
      {
         /* After fixing this variable cannot be accessed using near code
          */
         pFunc->pCode[ lPCodePos ] = HB_P_POPLOCAL;
      }
   }

   return (USHORT) 3;
}

static HB_FIX_FUNC( hb_p_pushlocalnear )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      /* only local variables used outside of a codeblock need fixing
       */
      SHORT * pVar = ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ];

      *pVar += pFunc->wParamCount;
      if( *pVar >= -128 && *pVar <= 127 )
      {
         pFunc->pCode[ lPCodePos + 2 ] = HB_P_NOOP;
         hb_compNOOPadd( pFunc, lPCodePos + 2 );
      }
      else
      {
         /* After fixing this variable cannot be accessed using near code
          */
         pFunc->pCode[ lPCodePos ] = HB_P_PUSHLOCAL;
      }
   }

   return (USHORT) 3;
}

static HB_FIX_FUNC( hb_p_staticname )
{
   ULONG ulStart = lPCodePos;

   HB_SYMBOL_UNUSED( cargo );
   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
      ++lPCodePos;

   return (USHORT) (lPCodePos - ulStart + 1) ;
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static HB_FIX_FUNC_PTR s_fixlocals_table[] =
{
   NULL,                       /* HB_P_AND,                  */
   NULL,                       /* HB_P_ARRAYPUSH,            */
   NULL,                       /* HB_P_ARRAYPOP,             */
   NULL,                       /* HB_P_ARRAYDIM,             */
   NULL,                       /* HB_P_ARRAYGEN,             */
   NULL,                       /* HB_P_EQUAL,                */
   hb_p_endblock,              /* HB_P_ENDBLOCK,             */
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
   NULL,                       /* HB_P_DUPLTWO,              */
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
   NULL,                       /* HB_P_MACROPUSHARG,         */
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
   hb_p_poplocal,              /* HB_P_POPLOCAL,             */
   hb_p_poplocalnear,          /* HB_P_POPLOCALNEAR,         */
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
   hb_p_pushlocal,             /* HB_P_PUSHLOCAL,            */
   hb_p_pushlocalnear,         /* HB_P_PUSHLOCALNEAR,        */
   hb_p_pushlocalref,          /* HB_P_PUSHLOCALREF,         */
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
   NULL                        /* HB_P_ONE,                  */
};

void hb_compFixFuncPCode( PFUNCTION pFunc )
{
   HB_FIX_INFO fix_info;

   fix_info.iNestedCodeblock = 0;

   assert( HB_P_LAST_PCODE == sizeof( s_fixlocals_table ) / sizeof( HB_FIX_FUNC_PTR ) );

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_fixlocals_table, ( void * ) &fix_info );
}

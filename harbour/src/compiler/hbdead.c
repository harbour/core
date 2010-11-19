/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * dead (unaccessible) PCODE eliminator
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

#include "hbcomp.h"
#include "hbassert.h"

/* helper structure to pass information */
typedef struct _HB_CODETRACE_INFO
{
   HB_BYTE * pCodeMark;
   HB_SIZE * pnJumps;
   HB_SIZE   nJumpPos;
   HB_SIZE   nJumpSize;
   HB_SIZE   nJumpCount;
   HB_SIZE   nPCodeSize;
   HB_BOOL   fFinished;
} HB_CODETRACE_INFO, * PHB_CODETRACE_INFO;

#define HB_CODETRACE_FUNC( func ) HB_PCODE_FUNC( func, PHB_CODETRACE_INFO )
typedef HB_CODETRACE_FUNC( HB_CODETRACE_FUNC_ );
typedef HB_CODETRACE_FUNC_ * PHB_CODETRACE_FUNC;

#define HB_JUMPADDR_ALLOC     64


static void hb_compCodeTraceAddJump( PHB_CODETRACE_INFO pInfo, HB_SIZE nPCodePos )
{
   /* Checking for nPCodePos < pInfo->nPCodeSize disabled intentionally
    * for easier detecting bugs in generated PCODE
    */
   /*
   if( nPCodePos < pInfo->nPCodeSize && pInfo->pCodeMark[ nPCodePos ] == 0 )
   */
   if( pInfo->pCodeMark[ nPCodePos ] == 0 )
   {
      if( pInfo->nJumpSize == 0 )
      {
         pInfo->nJumpSize = HB_JUMPADDR_ALLOC;
         pInfo->pnJumps = ( HB_SIZE * ) hb_xgrab( pInfo->nJumpSize *
                                                  sizeof( HB_SIZE ) );
      }
      else if( pInfo->nJumpSize == pInfo->nJumpCount )
      {
         pInfo->nJumpSize += HB_JUMPADDR_ALLOC;
         pInfo->pnJumps = ( HB_SIZE * ) hb_xrealloc( pInfo->pnJumps,
                                                     pInfo->nJumpSize * sizeof( HB_SIZE ) );
      }
      pInfo->pnJumps[ pInfo->nJumpCount++ ] = nPCodePos;
      pInfo->pCodeMark[ nPCodePos ] = 1;
   }
}

static HB_SIZE hb_compCodeTraceNextPos( PHB_CODETRACE_INFO pInfo, HB_SIZE nPCodePos )
{
   if( nPCodePos < pInfo->nPCodeSize && pInfo->pCodeMark[ nPCodePos ] == 0 )
      return nPCodePos;

   while( pInfo->nJumpPos < pInfo->nJumpCount )
   {
      nPCodePos = pInfo->pnJumps[ pInfo->nJumpPos++ ];
      if( pInfo->pCodeMark[ nPCodePos ] == 1 )
         return nPCodePos;
   }

   pInfo->fFinished = HB_TRUE;
   return pInfo->nPCodeSize;
}

static void hb_compCodeTraceMark( PHB_CODETRACE_INFO pInfo, HB_SIZE nPCodePos, HB_SIZE nSize )
{
   memset( &pInfo->pCodeMark[ nPCodePos ], 2, nSize );
}

/*
 * PCODE trace functions
 */

static HB_CODETRACE_FUNC( hb_p_default )
{
   HB_SIZE nSize = hb_compPCodeSize( pFunc, nPCodePos );

   hb_compCodeTraceMark( cargo, nPCodePos, nSize );
   return hb_compCodeTraceNextPos( cargo, nPCodePos + nSize );
}

static HB_CODETRACE_FUNC( hb_p_jumpnear )
{
   HB_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   hb_compCodeTraceMark( cargo, nPCodePos, 2 );
   return hb_compCodeTraceNextPos( cargo, nNewPos );
}

static HB_CODETRACE_FUNC( hb_p_jump )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 3 );
   return hb_compCodeTraceNextPos( cargo, nNewPos );
}

static HB_CODETRACE_FUNC( hb_p_jumpfar )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   return hb_compCodeTraceNextPos( cargo, nNewPos );
}

static HB_CODETRACE_FUNC( hb_p_jumpfalsenear )
{
   HB_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   hb_compCodeTraceMark( cargo, nPCodePos, 2 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 2 );
}

static HB_CODETRACE_FUNC( hb_p_jumpfalse )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 3 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 3 );
}

static HB_CODETRACE_FUNC( hb_p_jumpfalsefar )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static HB_CODETRACE_FUNC( hb_p_jumptruenear )
{
   HB_SIZE nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];

   hb_compCodeTraceMark( cargo, nPCodePos, 2 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 2 );
}

static HB_CODETRACE_FUNC( hb_p_jumptrue )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKSHORT( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 3 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 3 );
}

static HB_CODETRACE_FUNC( hb_p_jumptruefar )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   hb_compCodeTraceAddJump( cargo, nNewPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static HB_CODETRACE_FUNC( hb_p_seqalways )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nAlwaysPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   hb_compCodeTraceAddJump( cargo, nAlwaysPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static HB_CODETRACE_FUNC( hb_p_alwaysbegin )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nAlwaysEndPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   hb_compCodeTraceAddJump( cargo, nAlwaysEndPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static HB_CODETRACE_FUNC( hb_p_seqbegin )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nRecoverPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   /* this is a hack for -gc3 output - it's not really necessary
    * for pure PCODE evaluation
    */
   if( pFunc->pCode[ nRecoverPos ] != HB_P_SEQEND &&
       pFunc->pCode[ nRecoverPos - 4 ] == HB_P_SEQEND )
   {
      hb_compCodeTraceAddJump( cargo, nRecoverPos - 4 );
   }

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );
   hb_compCodeTraceAddJump( cargo, nRecoverPos );

   return hb_compCodeTraceNextPos( cargo, nPCodePos + 4 );
}

static HB_CODETRACE_FUNC( hb_p_seqend )
{
   HB_BYTE * pAddr = &pFunc->pCode[ nPCodePos + 1 ];
   HB_SIZE nNewPos = nPCodePos + HB_PCODE_MKINT24( pAddr );

   hb_compCodeTraceMark( cargo, nPCodePos, 4 );

   return hb_compCodeTraceNextPos( cargo, nNewPos );
}


static HB_CODETRACE_FUNC( hb_p_switch )
{
   HB_USHORT usCases = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), us;
   HB_SIZE nStart = nPCodePos, nNewPos;

   nPCodePos += 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_PUSHBYTE:
            nPCodePos += 2;
            break;
         case HB_P_PUSHINT:
            nPCodePos += 3;
            break;
         case HB_P_PUSHLONG:
         case HB_P_PUSHDATE:
            nPCodePos += 5;
            break;
         case HB_P_PUSHLONGLONG:
            nPCodePos += 9;
            break;
         case HB_P_PUSHSTRSHORT:
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case HB_P_PUSHSTR:
            nPCodePos += 3 + HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
            break;
         case HB_P_PUSHSTRLARGE:
            nPCodePos += 4 + HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] );
            break;
         case HB_P_PUSHNIL:
            /* default clause */
            us = usCases;
            nPCodePos++;
            break;
      }
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_JUMPNEAR:
            nNewPos = nPCodePos + ( signed char ) pFunc->pCode[ nPCodePos + 1 ];
            nPCodePos += 2;
            break;
         case HB_P_JUMP:
            nNewPos = nPCodePos + HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
            nPCodePos += 3;
            break;
         /*case HB_P_JUMPFAR:*/
         default:
            nNewPos = nPCodePos + HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );
            nPCodePos += 4;
            break;
      }
      hb_compCodeTraceAddJump( cargo, nNewPos );
   }
   hb_compCodeTraceMark( cargo, nStart, nPCodePos - nStart );

   return hb_compCodeTraceNextPos( cargo, us > usCases ?
                                   cargo->nPCodeSize : nPCodePos );
}

static HB_CODETRACE_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   hb_compCodeTraceMark( cargo, nPCodePos, 1 );
   return hb_compCodeTraceNextPos( cargo, cargo->nPCodeSize );
}

static HB_CODETRACE_FUNC( hb_p_endproc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );

   hb_compCodeTraceMark( cargo, nPCodePos, 1 );
   return hb_compCodeTraceNextPos( cargo, cargo->nPCodeSize );
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const PHB_CODETRACE_FUNC s_codeTraceFuncTable[] =
{
   hb_p_default,               /* HB_P_AND,                  */
   hb_p_default,               /* HB_P_ARRAYPUSH,            */
   hb_p_default,               /* HB_P_ARRAYPOP,             */
   hb_p_default,               /* HB_P_ARRAYDIM,             */
   hb_p_default,               /* HB_P_ARRAYGEN,             */
   hb_p_default,               /* HB_P_EQUAL,                */
   hb_p_endblock,              /* HB_P_ENDBLOCK,             */
   hb_p_endproc,               /* HB_P_ENDPROC,              */
   hb_p_default,               /* HB_P_EXACTLYEQUAL,         */
   hb_p_default,               /* HB_P_FALSE,                */
   hb_p_default,               /* HB_P_FORTEST,              */
   hb_p_default,               /* HB_P_FUNCTION,             */
   hb_p_default,               /* HB_P_FUNCTIONSHORT,        */
   hb_p_default,               /* HB_P_FRAME,                */
   hb_p_default,               /* HB_P_FUNCPTR,              */
   hb_p_default,               /* HB_P_GREATER,              */
   hb_p_default,               /* HB_P_GREATEREQUAL,         */
   hb_p_default,               /* HB_P_DEC,                  */
   hb_p_default,               /* HB_P_DIVIDE,               */
   hb_p_default,               /* HB_P_DO,                   */
   hb_p_default,               /* HB_P_DOSHORT,              */
   hb_p_default,               /* HB_P_DUPLICATE,            */
   hb_p_default,               /* HB_P_PUSHTIMESTAMP,        */
   hb_p_default,               /* HB_P_INC,                  */
   hb_p_default,               /* HB_P_INSTRING,             */
   hb_p_jumpnear,              /* HB_P_JUMPNEAR,             */
   hb_p_jump,                  /* HB_P_JUMP,                 */
   hb_p_jumpfar,               /* HB_P_JUMPFAR,              */
   hb_p_jumpfalsenear,         /* HB_P_JUMPFALSENEAR,        */
   hb_p_jumpfalse,             /* HB_P_JUMPFALSE,            */
   hb_p_jumpfalsefar,          /* HB_P_JUMPFALSEFAR,         */
   hb_p_jumptruenear,          /* HB_P_JUMPTRUENEAR,         */
   hb_p_jumptrue,              /* HB_P_JUMPTRUE,             */
   hb_p_jumptruefar,           /* HB_P_JUMPTRUEFAR,          */
   hb_p_default,               /* HB_P_LESSEQUAL,            */
   hb_p_default,               /* HB_P_LESS,                 */
   hb_p_default,               /* HB_P_LINE,                 */
   hb_p_default,               /* HB_P_LOCALNAME,            */
   hb_p_default,               /* HB_P_MACROPOP,             */
   hb_p_default,               /* HB_P_MACROPOPALIASED,      */
   hb_p_default,               /* HB_P_MACROPUSH,            */
   hb_p_default,               /* HB_P_MACROARRAYGEN,        */
   hb_p_default,               /* HB_P_MACROPUSHLIST,        */
   hb_p_default,               /* HB_P_MACROPUSHINDEX,       */
   hb_p_default,               /* HB_P_MACROPUSHPARE,        */
   hb_p_default,               /* HB_P_MACROPUSHALIASED,     */
   hb_p_default,               /* HB_P_MACROSYMBOL,          */
   hb_p_default,               /* HB_P_MACROTEXT,            */
   hb_p_default,               /* HB_P_MESSAGE,              */
   hb_p_default,               /* HB_P_MINUS,                */
   hb_p_default,               /* HB_P_MODULUS,              */
   hb_p_default,               /* HB_P_MODULENAME,           */
                               /* start: pcodes generated by macro compiler */
   hb_p_default,               /* HB_P_MMESSAGE,             */
   hb_p_default,               /* HB_P_MPOPALIASEDFIELD,     */
   hb_p_default,               /* HB_P_MPOPALIASEDVAR,       */
   hb_p_default,               /* HB_P_MPOPFIELD,            */
   hb_p_default,               /* HB_P_MPOPMEMVAR,           */
   hb_p_default,               /* HB_P_MPUSHALIASEDFIELD,    */
   hb_p_default,               /* HB_P_MPUSHALIASEDVAR,      */
   hb_p_default,               /* HB_P_MPUSHBLOCK,           */
   hb_p_default,               /* HB_P_MPUSHFIELD,           */
   hb_p_default,               /* HB_P_MPUSHMEMVAR,          */
   hb_p_default,               /* HB_P_MPUSHMEMVARREF,       */
   hb_p_default,               /* HB_P_MPUSHSYM,             */
   hb_p_default,               /* HB_P_MPUSHVARIABLE,        */
                               /* end: */
   hb_p_default,               /* HB_P_MULT,                 */
   hb_p_default,               /* HB_P_NEGATE,               */
   hb_p_default,               /* HB_P_NOOP,                 */
   hb_p_default,               /* HB_P_NOT,                  */
   hb_p_default,               /* HB_P_NOTEQUAL,             */
   hb_p_default,               /* HB_P_OR,                   */
   hb_p_default,               /* HB_P_PARAMETER,            */
   hb_p_default,               /* HB_P_PLUS,                 */
   hb_p_default,               /* HB_P_POP,                  */
   hb_p_default,               /* HB_P_POPALIAS,             */
   hb_p_default,               /* HB_P_POPALIASEDFIELD,      */
   hb_p_default,               /* HB_P_POPALIASEDFIELDNEAR,  */
   hb_p_default,               /* HB_P_POPALIASEDVAR,        */
   hb_p_default,               /* HB_P_POPFIELD,             */
   hb_p_default,               /* HB_P_POPLOCAL,             */
   hb_p_default,               /* HB_P_POPLOCALNEAR,         */
   hb_p_default,               /* HB_P_POPMEMVAR,            */
   hb_p_default,               /* HB_P_POPSTATIC,            */
   hb_p_default,               /* HB_P_POPVARIABLE,          */
   hb_p_default,               /* HB_P_POWER,                */
   hb_p_default,               /* HB_P_PUSHALIAS,            */
   hb_p_default,               /* HB_P_PUSHALIASEDFIELD,     */
   hb_p_default,               /* HB_P_PUSHALIASEDFIELDNEAR, */
   hb_p_default,               /* HB_P_PUSHALIASEDVAR,       */
   hb_p_default,               /* HB_P_PUSHBLOCK,            */
   hb_p_default,               /* HB_P_PUSHBLOCKSHORT,       */
   hb_p_default,               /* HB_P_PUSHFIELD,            */
   hb_p_default,               /* HB_P_PUSHBYTE,             */
   hb_p_default,               /* HB_P_PUSHINT,              */
   hb_p_default,               /* HB_P_PUSHLOCAL,            */
   hb_p_default,               /* HB_P_PUSHLOCALNEAR,        */
   hb_p_default,               /* HB_P_PUSHLOCALREF,         */
   hb_p_default,               /* HB_P_PUSHLONG,             */
   hb_p_default,               /* HB_P_PUSHMEMVAR,           */
   hb_p_default,               /* HB_P_PUSHMEMVARREF,        */
   hb_p_default,               /* HB_P_PUSHNIL,              */
   hb_p_default,               /* HB_P_PUSHDOUBLE,           */
   hb_p_default,               /* HB_P_PUSHSELF,             */
   hb_p_default,               /* HB_P_PUSHSTATIC,           */
   hb_p_default,               /* HB_P_PUSHSTATICREF,        */
   hb_p_default,               /* HB_P_PUSHSTR,              */
   hb_p_default,               /* HB_P_PUSHSTRSHORT,         */
   hb_p_default,               /* HB_P_PUSHSYM,              */
   hb_p_default,               /* HB_P_PUSHSYMNEAR,          */
   hb_p_default,               /* HB_P_PUSHVARIABLE,         */
   hb_p_default,               /* HB_P_RETVALUE,             */
   hb_p_default,               /* HB_P_SEND,                 */
   hb_p_default,               /* HB_P_SENDSHORT,            */
   hb_p_seqbegin,              /* HB_P_SEQBEGIN,             */
   hb_p_seqend,                /* HB_P_SEQEND,               */
   hb_p_default,               /* HB_P_SEQRECOVER,           */
   hb_p_default,               /* HB_P_SFRAME,               */
   hb_p_default,               /* HB_P_STATICS,              */
   hb_p_default,               /* HB_P_STATICNAME,           */
   hb_p_default,               /* HB_P_SWAPALIAS,            */
   hb_p_default,               /* HB_P_TRUE,                 */
   hb_p_default,               /* HB_P_ZERO,                 */
   hb_p_default,               /* HB_P_ONE,                  */
   hb_p_default,               /* HB_P_MACROFUNC,            */
   hb_p_default,               /* HB_P_MACRODO,              */
   hb_p_default,               /* HB_P_MPUSHSTR,             */
   hb_p_default,               /* HB_P_LOCALNEARADDINT,      */
   hb_p_default,               /* HB_P_MACROPUSHREF          */
   hb_p_default,               /* HB_P_PUSHLONGLONG          */
   hb_p_default,               /* HB_P_ENUMSTART             */
   hb_p_default,               /* HB_P_ENUMNEXT              */
   hb_p_default,               /* HB_P_ENUMPREV              */
   hb_p_default,               /* HB_P_ENUMEND               */
   hb_p_switch,                /* HB_P_SWITCH                */
   hb_p_default,               /* HB_P_PUSHDATE              */
                               /* optimalization of inlined math operations */
   hb_p_default,               /* HB_P_PLUSEQPOP             */
   hb_p_default,               /* HB_P_MINUSEQPOP            */
   hb_p_default,               /* HB_P_MULTEQPOP             */
   hb_p_default,               /* HB_P_DIVEQPOP              */
   hb_p_default,               /* HB_P_PLUSEQ                */
   hb_p_default,               /* HB_P_MINUSEQ               */
   hb_p_default,               /* HB_P_MULTEQ                */
   hb_p_default,               /* HB_P_DIVEQ                 */
   hb_p_default,               /* HB_P_WITHOBJECTSTART       */
   hb_p_default,               /* HB_P_WITHOBJECTMESSAGE     */
   hb_p_default,               /* HB_P_WITHOBJECTEND         */
   hb_p_default,               /* HB_P_MACROSEND             */
   hb_p_default,               /* HB_P_PUSHOVARREF           */
   hb_p_default,               /* HB_P_ARRAYPUSHREF          */
   hb_p_default,               /* HB_P_VFRAME                */
   hb_p_default,               /* HB_P_LARGEFRAME            */
   hb_p_default,               /* HB_P_LARGEVFRAME           */
   hb_p_default,               /* HB_P_PUSHSTRHIDDEN         */
   hb_p_default,               /* HB_P_LOCALADDINT           */
   hb_p_default,               /* HB_P_MODEQPOP              */
   hb_p_default,               /* HB_P_EXPEQPOP              */
   hb_p_default,               /* HB_P_MODEQ                 */
   hb_p_default,               /* HB_P_EXPEQ                 */
   hb_p_default,               /* HB_P_DUPLUNREF             */
   hb_p_default,               /* HB_P_MPUSHBLOCKLARGE       */
   hb_p_default,               /* HB_P_MPUSHSTRLARGE         */
   hb_p_default,               /* HB_P_PUSHBLOCKLARGE        */
   hb_p_default,               /* HB_P_PUSHSTRLARGE          */
   hb_p_default,               /* HB_P_SWAP                  */
   hb_p_default,               /* HB_P_PUSHVPARAMS           */
   hb_p_default,               /* HB_P_PUSHUNREF             */
   hb_p_seqalways,             /* HB_P_SEQALWAYS             */
   hb_p_alwaysbegin,           /* HB_P_ALWAYSBEGIN           */
   hb_p_default,               /* HB_P_ALWAYSEND             */
   hb_p_default,               /* HB_P_DECEQPOP              */
   hb_p_default,               /* HB_P_INCEQPOP              */
   hb_p_default,               /* HB_P_DECEQ                 */
   hb_p_default,               /* HB_P_INCEQ                 */
   hb_p_default,               /* HB_P_LOCALDEC              */
   hb_p_default,               /* HB_P_LOCALINC              */
   hb_p_default,               /* HB_P_LOCALINCPUSH          */
   hb_p_default,               /* HB_P_PUSHFUNCSYM           */
   hb_p_default,               /* HB_P_HASHGEN               */
   hb_p_default,               /* HB_P_SEQBLOCK              */
   hb_p_default,               /* HB_P_THREADSTATICS         */
   hb_p_default                /* HB_P_PUSHAPARAMS           */
};

void hb_compCodeTraceMarkDead( HB_COMP_DECL, PFUNCTION pFunc )
{
   const PHB_CODETRACE_FUNC * pFuncTable = s_codeTraceFuncTable;
   HB_CODETRACE_INFO code_info;

   if( ! HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) || pFunc->nPCodePos < 2 )
      return;

   assert( HB_P_LAST_PCODE == sizeof( s_codeTraceFuncTable ) / sizeof( PHB_CODETRACE_FUNC ) );

   code_info.pnJumps = NULL;
   code_info.nJumpPos = 0;
   code_info.nJumpSize = 0;
   code_info.nJumpCount = 0;
   code_info.nPCodeSize = pFunc->nPCodePos;
   code_info.fFinished = HB_FALSE;

   code_info.pCodeMark = ( HB_BYTE * ) hb_xgrab( code_info.nPCodeSize );
   memset( code_info.pCodeMark, 0, code_info.nPCodeSize );

   hb_compPCodeTrace( pFunc, ( const HB_PCODE_FUNC_PTR * ) pFuncTable, ( void * ) &code_info );

   if( code_info.fFinished )
   {
      HB_SIZE nPos = 0, nCount = 0;
      HB_BYTE bLastCode = HB_P_LAST_PCODE;

      do
      {
         if( code_info.pCodeMark[ nPos ] == 0 )
            ++nCount;
         else
         {
            bLastCode = pFunc->pCode[ nPos ];
            if( nCount )
            {
               hb_compNOOPfill( pFunc, nPos - nCount, nCount, HB_FALSE, HB_TRUE );
               nCount = 0;
            }
         }
      }
      while( ++nPos < code_info.nPCodeSize );

      /* do not strip the last HB_P_ENDBLOCK / HB_P_ENDPROC marker */
      if( nCount > 0 && bLastCode != ( pFunc->szName ? HB_P_ENDPROC : HB_P_ENDBLOCK ) )
      {
         --nPos;
         --nCount;
      }

      if( nCount > 0 )
      {
         /*
          * We cannot simply decrease size of the generated PCODE here
          * because jumps or noops tables may point to the this area
          * and we will have to update also the jump table, [druzus]
          */
         /*
         pFunc->pCode[ nPos - nCount ] = pFunc->pCode[ nPos - 1 ];
         pFunc->nPCodePos = pFunc->nPCodeSize = nPos - nCount + 1;
         */
         hb_compNOOPfill( pFunc, nPos - nCount, nCount, HB_FALSE, HB_TRUE );
      }
   }

   hb_xfree( code_info.pCodeMark );
   if( code_info.pnJumps )
      hb_xfree( code_info.pnJumps );
}

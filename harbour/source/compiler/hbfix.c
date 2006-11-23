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
   HB_COMP_DECL;
   USHORT iNestedCodeblock;
} HB_FIX_INFO, * HB_FIX_INFO_PTR;

#define HB_FIX_FUNC( func ) HB_PCODE_FUNC( func, HB_FIX_INFO_PTR )
typedef HB_FIX_FUNC( HB_FIX_FUNC_ );
typedef HB_FIX_FUNC_ * HB_FIX_FUNC_PTR;


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
   ULONG ulStart = lPCodePos;

   ++cargo->iNestedCodeblock;

   wVar = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 5 ] ) );

   /* opcode + codeblock size + number of parameters + number of local variables */
   lPCodePos += 7;
   if( pFunc->wParamCount == 0 )
   {
      lPCodePos += wVar << 1;
   }
   else
   {
      /* fix local variable's reference */
      while( wVar-- )
      {
         BYTE * pLocal = &( pFunc->pCode[ lPCodePos ] );
         USHORT wLocal = HB_PCODE_MKUSHORT( pLocal );

         wLocal += pFunc->wParamCount;
         pLocal[ 0 ] = HB_LOBYTE( wLocal );
         pLocal[ 1 ] = HB_HIBYTE( wLocal );
         lPCodePos += 2;
      }
   }
   return (lPCodePos - ulStart);
}

static HB_FIX_FUNC( hb_p_pushblockshort )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   ++cargo->iNestedCodeblock;

   return 2;
}

static HB_FIX_FUNC( hb_p_poplocal )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 )
   {
      HB_COMP_DECL = cargo->HB_COMP_PARAM;

      BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
      SHORT iVar = HB_PCODE_MKSHORT( pVar );

      iVar += pFunc->wParamCount;
      pVar[ 0 ] = HB_LOBYTE( iVar );
      pVar[ 1 ] = HB_HIBYTE( iVar );
      if( HB_LIM_INT8( iVar ) && HB_COMP_ISSUPPORTED( HB_COMPFLAG_OPTJUMP ) )
      {
         pFunc->pCode[ lPCodePos ] = HB_P_POPLOCALNEAR;
         hb_compNOOPfill( pFunc, lPCodePos + 2, 1, FALSE, FALSE );
      }
   }

   return 3;
}

static HB_FIX_FUNC( hb_p_pushlocal )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 )
   {
      HB_COMP_DECL = cargo->HB_COMP_PARAM;
      BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
      SHORT iVar = HB_PCODE_MKSHORT( pVar );

      iVar += pFunc->wParamCount;
      pVar[ 0 ] = HB_LOBYTE( iVar );
      pVar[ 1 ] = HB_HIBYTE( iVar );
      if( HB_LIM_INT8( iVar ) && HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) )
      {
         pFunc->pCode[ lPCodePos ] = HB_P_PUSHLOCALNEAR;
         hb_compNOOPfill( pFunc, lPCodePos + 2, 1, FALSE, FALSE );
      }
   }

   return 3;
}

static HB_FIX_FUNC( hb_p_pushlocalref )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 && pFunc->wParamCount != 0 )
   {
      BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
      SHORT iVar = HB_PCODE_MKSHORT( pVar );

      iVar += pFunc->wParamCount;
      pVar[ 0 ] = HB_LOBYTE( iVar );
      pVar[ 1 ] = HB_HIBYTE( iVar );
   }

   return 3;
}

static HB_FIX_FUNC( hb_p_poplocalnear )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 && pFunc->wParamCount != 0 )
   {
      /*
       * this code should never be executed because compiler should
       * generate only non size optimized HB_P_POPLOCAL pcodes
       * for function body
       */
      SHORT iVar = ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      iVar += pFunc->wParamCount;
      pFunc->pCode[ lPCodePos + 1 ] = ( BYTE ) iVar;
      if( !HB_LIM_INT8( iVar ) )
      {
         char sTemp[16];
         char sTemp2[16];

         sprintf( sTemp, "%i", pFunc->wParamCount );
         sprintf( sTemp2, "%i", iVar );
         hb_compGenError( cargo->HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_OPTIMIZEDLOCAL_OUT_OF_RANGE, sTemp2, sTemp );
      }
   }

   return 2;
}

static HB_FIX_FUNC( hb_p_pushlocalnear )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 && pFunc->wParamCount != 0 )
   {
      /*
       * this code should never be executed because compiler should
       * generate only non size optimized HB_P_POPLOCAL pcodes
       * for function body
       */
      SHORT iVar = ( signed char ) pFunc->pCode[ lPCodePos + 1 ];

      iVar += pFunc->wParamCount;
      pFunc->pCode[ lPCodePos + 1 ] = ( BYTE ) iVar;
      if( !HB_LIM_INT8( iVar ) )
      {
         char sTemp[16];
         char sTemp2[16];

         sprintf( sTemp, "%i", pFunc->wParamCount );
         sprintf( sTemp2, "%i", iVar );
         hb_compGenError( cargo->HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_OPTIMIZEDLOCAL_OUT_OF_RANGE, sTemp2, sTemp );
      }
   }

   return 2;
}

static HB_FIX_FUNC( hb_p_localnearaddint )
{
   /* only local variables used outside of a codeblock need fixing
    */
   if( cargo->iNestedCodeblock == 0 && pFunc->wParamCount )
   {
      USHORT uiVar = pFunc->pCode[ lPCodePos + 1 ];

      uiVar += pFunc->wParamCount;
      if( uiVar < 256 )
      {
         pFunc->pCode[ lPCodePos + 1 ] = ( BYTE ) uiVar;
      }
      else
      {
         /* After fixing this variable cannot be accessed using near code */
         char sTemp[16];
         char sTemp2[16];

         sprintf( sTemp, "%i", pFunc->wParamCount );
         sprintf( sTemp2, "%i", uiVar );
         hb_compGenError( cargo->HB_COMP_PARAM, hb_comp_szErrors, 'F', HB_COMP_ERR_OPTIMIZEDLOCAL_OUT_OF_RANGE, sTemp2, sTemp );
      }
   }

   return 4;
}

static HB_FIX_FUNC( hb_p_false )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      switch( pFunc->pCode[ lPCodePos + 1 ] )
      {
         case HB_P_POP:
         case HB_P_JUMPFALSENEAR:
         case HB_P_JUMPFALSE:
         case HB_P_JUMPFALSEFAR:
         case HB_P_JUMPTRUENEAR:
         case HB_P_JUMPTRUE:
         case HB_P_JUMPTRUEFAR:
            if( ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) )
            {
               int iCount = 1;

               switch( pFunc->pCode[ lPCodePos + 1 ] )
               {
                  case HB_P_JUMPFALSENEAR:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMPNEAR;
                     break;
                  case HB_P_JUMPFALSE:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMP;
                     break;
                  case HB_P_JUMPFALSEFAR:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMPFAR;
                     break;
                  case HB_P_POP:
                     iCount = 2;
                     break;
                  case HB_P_JUMPTRUENEAR:
                     iCount = 3;
                     break;
                  case HB_P_JUMPTRUE:
                     iCount = 4;
                     break;
                  case HB_P_JUMPTRUEFAR:
                     iCount = 5;
                     break;
               }
               hb_compNOOPfill( pFunc, lPCodePos, iCount, FALSE, FALSE );
            }
            break;
      }
   }

   return 1;
}

static HB_FIX_FUNC( hb_p_true )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      switch( pFunc->pCode[ lPCodePos + 1 ] )
      {
         case HB_P_POP:
         case HB_P_JUMPTRUENEAR:
         case HB_P_JUMPTRUE:
         case HB_P_JUMPTRUEFAR:
         case HB_P_JUMPFALSENEAR:
         case HB_P_JUMPFALSE:
         case HB_P_JUMPFALSEFAR:
            if( ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) )
            {
               int iCount = 1;
   
               switch( pFunc->pCode[ lPCodePos + 1 ] )
               {
                  case HB_P_JUMPTRUENEAR:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMPNEAR;
                     break;
                  case HB_P_JUMPTRUE:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMP;
                     break;
                  case HB_P_JUMPTRUEFAR:
                     pFunc->pCode[ lPCodePos + 1 ] = HB_P_JUMPFAR;
                     break;
                  case HB_P_POP:
                     iCount = 2;
                     break;
                  case HB_P_JUMPFALSENEAR:
                     iCount = 3;
                     break;
                  case HB_P_JUMPFALSE:
                     iCount = 4;
                     break;
                  case HB_P_JUMPFALSEFAR:
                     iCount = 5;
                     break;
               }
               hb_compNOOPfill( pFunc, lPCodePos, iCount, FALSE, FALSE );
            }
            break;
      }
   }
   return 1;
}

static HB_FIX_FUNC( hb_p_not )
{
   if( cargo->iNestedCodeblock == 0 )
   {
      BYTE opcode;

      switch( pFunc->pCode[ lPCodePos + 1 ] )
      {
         case HB_P_NOT:
            opcode = HB_P_NOOP;
            break;
         case HB_P_JUMPTRUENEAR:
            opcode = HB_P_JUMPFALSENEAR;
            break;
         case HB_P_JUMPTRUE:
            opcode = HB_P_JUMPFALSE;
            break;
         case HB_P_JUMPTRUEFAR:
            opcode = HB_P_JUMPFALSEFAR;
            break;
         case HB_P_JUMPFALSENEAR:
            opcode = HB_P_JUMPTRUENEAR;
            break;
         case HB_P_JUMPFALSE:
            opcode = HB_P_JUMPTRUE;
            break;
         case HB_P_JUMPFALSEFAR:
            opcode = HB_P_JUMPTRUEFAR;
            break;
         default:
            opcode = HB_P_LAST_PCODE;
            break;
      }

      if( opcode < HB_P_LAST_PCODE &&
          ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) )
      {
         hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
         if( opcode == HB_P_NOOP )
            hb_compNOOPfill( pFunc, lPCodePos + 1, 1, FALSE, FALSE );
         else
            pFunc->pCode[ lPCodePos + 1 ] = opcode;
      }
   }
   return 1;
}

static HB_FIX_FUNC( hb_p_jumpfar )
{
   HB_COMP_DECL = cargo->HB_COMP_PARAM;

   if( cargo->iNestedCodeblock == 0 && HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) )
   {
      BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
      LONG lOffset = HB_PCODE_MKINT24( pAddr );
      ULONG ulNewPos = lPCodePos + lOffset;

      switch( pFunc->pCode[ ulNewPos ] )
      {
         case HB_P_JUMPFAR:
            lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
            HB_PUT_LE_UINT24( pAddr, lOffset );
            break;

         case HB_P_JUMPFALSEFAR:
            ulNewPos += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
            if( ulNewPos == lPCodePos + 4 )
            {
               pFunc->pCode[ lPCodePos ] = HB_P_JUMPTRUEFAR;
               HB_PUT_LE_UINT24( pAddr, lOffset + 4 );
            }
            break;

         case HB_P_JUMPTRUEFAR:
            ulNewPos += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
            if( ulNewPos == lPCodePos + 4 )
            {
               pFunc->pCode[ lPCodePos ] = HB_P_JUMPFALSEFAR;
               HB_PUT_LE_UINT24( pAddr, lOffset + 4 );
            }
            break;
      }
   }
   return 4;
}

static HB_FIX_FUNC( hb_p_jumpfalsefar )
{
   HB_COMP_DECL = cargo->HB_COMP_PARAM;

   if( cargo->iNestedCodeblock == 0 && HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) )
   {
      BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
      LONG lOffset = HB_PCODE_MKINT24( pAddr );
      ULONG ulNewPos = lPCodePos + lOffset;

      switch( pFunc->pCode[ ulNewPos ] )
      {
         case HB_P_JUMPFAR:
            lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
            HB_PUT_LE_UINT24( pAddr, lOffset );
            break;
      }
   }
   return 4;
}

static HB_FIX_FUNC( hb_p_jumptruefar )
{
   HB_COMP_DECL = cargo->HB_COMP_PARAM;

   if( cargo->iNestedCodeblock == 0 && HB_COMP_ISSUPPORTED(HB_COMPFLAG_OPTJUMP) )
   {
      BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
      LONG lOffset = HB_PCODE_MKINT24( pAddr );
      ULONG ulNewPos = lPCodePos + lOffset;

      switch( pFunc->pCode[ ulNewPos ] )
      {
         case HB_P_JUMPFAR:
            lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
            HB_PUT_LE_UINT24( pAddr, lOffset );
            break;
      }
   }
   return 4;
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
   hb_p_false,                 /* HB_P_FALSE,                */
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
   hb_p_jumpfar,               /* HB_P_JUMPFAR,              */
   NULL,                       /* HB_P_JUMPFALSENEAR,        */
   NULL,                       /* HB_P_JUMPFALSE,            */
   hb_p_jumpfalsefar,          /* HB_P_JUMPFALSEFAR,         */
   NULL,                       /* HB_P_JUMPTRUENEAR,         */
   NULL,                       /* HB_P_JUMPTRUE,             */
   hb_p_jumptruefar,           /* HB_P_JUMPTRUEFAR,          */
   NULL,                       /* HB_P_LESSEQUAL,            */
   NULL,                       /* HB_P_LESS,                 */
   NULL,                       /* HB_P_LINE,                 */
   NULL,                       /* HB_P_LOCALNAME,            */
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
   NULL,                       /* HB_P_MODULENAME,           */
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
   hb_p_not,                   /* HB_P_NOT,                  */
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
   NULL,                       /* HB_P_PUSHSTR,              */
   NULL,                       /* HB_P_PUSHSTRSHORT,         */
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
   NULL,                       /* HB_P_STATICNAME,           */
   NULL,                       /* HB_P_SWAPALIAS,            */
   hb_p_true,                  /* HB_P_TRUE,                 */
   NULL,                       /* HB_P_ZERO,                 */
   NULL,                       /* HB_P_ONE,                  */
   NULL,                       /* HB_P_MACROFUNC,            */
   NULL,                       /* HB_P_MACRODO,              */
   NULL,                       /* HB_P_MPUSHSTR,             */
   hb_p_localnearaddint,       /* HB_P_LOCALNEARADDINT,      */
   NULL,                       /* HB_P_MACROPUSHREF          */
   NULL,                       /* HB_P_PUSHLONGLONG          */
   NULL,                       /* HB_P_ENUMSTART             */
   NULL,                       /* HB_P_ENUMNEXT              */
   NULL,                       /* HB_P_ENUMPREV              */
   NULL,                       /* HB_P_ENUMEND               */
   NULL,                       /* HB_P_SWITCH                */
   NULL,                       /* HB_P_PUSHDATE              */
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
   NULL                        /* HB_P_VFRAME                */
};

void hb_compFixFuncPCode( HB_COMP_DECL, PFUNCTION pFunc )
{
   HB_FIX_INFO fix_info;

   fix_info.iNestedCodeblock = 0;
   fix_info.HB_COMP_PARAM = HB_COMP_PARAM;

   assert( HB_P_LAST_PCODE == sizeof( s_fixlocals_table ) / sizeof( HB_FIX_FUNC_PTR ) );

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_fixlocals_table, ( void * ) &fix_info );
}

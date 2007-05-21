/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Compiler PCODE optimizer
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

typedef struct HB_stru_opt_info
{
   HB_COMP_DECL;
} HB_OPT_INFO, * HB_OPT_INFO_PTR;

#define HB_OPT_FUNC( func ) HB_PCODE_FUNC( func, HB_OPT_INFO_PTR )
typedef HB_OPT_FUNC( HB_OPT_FUNC_ );
typedef HB_OPT_FUNC_ * HB_OPT_FUNC_PTR;


static HB_OPT_FUNC( hb_p_poplocal )
{
   BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
   SHORT iVar = HB_PCODE_MKSHORT( pVar );

   HB_SYMBOL_UNUSED( cargo );

   if( HB_LIM_INT8( iVar ) )
   {
      pFunc->pCode[ lPCodePos ] = HB_P_POPLOCALNEAR;
      hb_compNOOPfill( pFunc, lPCodePos + 2, 1, FALSE, FALSE );
   }

   return 3;
}

static HB_OPT_FUNC( hb_p_pushlocal )
{
   BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
   SHORT iVar = HB_PCODE_MKSHORT( pVar );

   if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POPLOCAL &&
      HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 4 ] ) == iVar &&
      ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 6, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POPLOCALNEAR &&
            ( SCHAR ) pFunc->pCode[ lPCodePos + 4 ] == iVar &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 5, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POP &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
   }
   else if( HB_LIM_INT8( iVar ) )
   {
      pFunc->pCode[ lPCodePos ] = HB_P_PUSHLOCALNEAR;
      hb_compNOOPfill( pFunc, lPCodePos + 2, 1, FALSE, FALSE );
   }

   return 3;
}

static HB_OPT_FUNC( hb_p_pushlocalnear )
{
   if( pFunc->pCode[ lPCodePos + 2 ] == HB_P_POPLOCAL &&
      ( SCHAR ) pFunc->pCode[ lPCodePos + 1 ] ==
      HB_PCODE_MKSHORT( &pFunc->pCode[ lPCodePos + 3 ] ) &&
      ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 2 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 5, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 2 ] == HB_P_POPLOCALNEAR &&
            pFunc->pCode[ lPCodePos + 1 ] == pFunc->pCode[ lPCodePos + 3 ] &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 2 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 2 ] == HB_P_POP &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 2 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 3, FALSE, FALSE );
   }

   return 2;
}

static HB_OPT_FUNC( hb_p_localaddint )
{
   BYTE * pVar = &pFunc->pCode[ lPCodePos + 1 ];
   SHORT iVar = HB_PCODE_MKSHORT( pVar );

   HB_SYMBOL_UNUSED( cargo );

   if( HB_LIM_INT8( iVar ) )
   {
      pVar[ 0 ] = HB_P_LOCALNEARADDINT;
      pVar[ 1 ] = HB_LOBYTE( iVar );
      hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
   }

   return 5;
}

static HB_OPT_FUNC( hb_p_pushstatic )
{
   if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POPSTATIC &&
       HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) ==
       HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 4 ] ) &&
       ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 6, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POP &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
   }

   return 3;
}

static HB_OPT_FUNC( hb_p_pushmemvar )
{
   if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POPMEMVAR &&
       HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 1 ] ) ==
       HB_PCODE_MKUSHORT( &pFunc->pCode[ lPCodePos + 4 ] ) &&
       ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 6, FALSE, FALSE );
   }
   else if( pFunc->pCode[ lPCodePos + 3 ] == HB_P_POP &&
            ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 3 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
   }

   return 3;
}

static HB_OPT_FUNC( hb_p_pushnil )
{
   if( pFunc->pCode[ lPCodePos + 1 ] == HB_P_POP &&
       ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 2, FALSE, FALSE );
   }

   return 1;
}

static HB_OPT_FUNC( hb_p_false )
{
   switch( pFunc->pCode[ lPCodePos + 1 ] )
   {
      case HB_P_POP:
      case HB_P_NOT:
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
               case HB_P_NOT:
                  pFunc->pCode[ lPCodePos + 1 ] = HB_P_TRUE;
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
   return 1;
}

static HB_OPT_FUNC( hb_p_true )
{
   switch( pFunc->pCode[ lPCodePos + 1 ] )
   {
      case HB_P_POP:
      case HB_P_NOT:
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
               case HB_P_NOT:
                  pFunc->pCode[ lPCodePos + 1 ] = HB_P_FALSE;
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
   return 1;
}

static HB_OPT_FUNC( hb_p_duplicate )
{
   switch( pFunc->pCode[ lPCodePos + 1 ] )
   {
      case HB_P_JUMPTRUEFAR:
      case HB_P_JUMPFALSEFAR:
         if( pFunc->pCode[ lPCodePos + 5 ] == HB_P_POP )
         {
            BYTE * pAddr = &pFunc->pCode[ lPCodePos + 2 ];
            LONG lOffset = HB_PCODE_MKINT24( pAddr ), lLastOffset = 0;
            ULONG ulNewPos = lPCodePos + 1 + lOffset;
            BOOL fNot = FALSE, fRepeat = TRUE;

            do
            {
               if( pFunc->pCode[ ulNewPos ] == HB_P_DUPLICATE )
               {
                  if( lOffset > 0 )
                     hb_p_duplicate( pFunc, ulNewPos, cargo );
               }

               if( pFunc->pCode[ ulNewPos ] == HB_P_NOOP )
               {
                  ulNewPos++;
                  lOffset++;
               }
               else if( pFunc->pCode[ ulNewPos ] == HB_P_NOT )
               {
                  ulNewPos++;
                  lOffset++;
                  fNot = !fNot;
               }
               else if( pFunc->pCode[ ulNewPos ] == HB_P_DUPLICATE &&
                        ( pFunc->pCode[ ulNewPos + 1 ] == HB_P_JUMPTRUEFAR ||
                          pFunc->pCode[ ulNewPos + 1 ] == HB_P_JUMPFALSEFAR ) )
               {
                  LONG lJump;
                  if( pFunc->pCode[ ulNewPos + 1 ] != pFunc->pCode[ lPCodePos + 1 ] )
                     fNot = !fNot;
                  lJump = fNot ? 4 : HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 2 ] );
                  lOffset += lJump + 1;
                  ulNewPos = lPCodePos + 1 + lOffset;
                  fRepeat = lJump > 0;
               }
               else
                  fRepeat = FALSE;

               if( !fNot )
                  lLastOffset = lOffset;
            }
            while( fRepeat );

            if( ( pFunc->pCode[ ulNewPos ] == HB_P_JUMPTRUEFAR ||
                  pFunc->pCode[ ulNewPos ] == HB_P_JUMPFALSEFAR ) &&
                !hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) &&
                !hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 5 ) )
            {
               if( pFunc->pCode[ ulNewPos ] != pFunc->pCode[ lPCodePos + 1 ] )
                  fNot = !fNot;
               if( fNot )
                  lOffset += 4;
               else
                  lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );

               HB_PUT_LE_UINT24( pAddr, lOffset );
               hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
               hb_compNOOPfill( pFunc, lPCodePos + 5, 1, FALSE, FALSE );
            }
            else if( lLastOffset )
            {
               HB_PUT_LE_UINT24( pAddr, lLastOffset );
            }
         }
         break;
   }
   return 1;
}

static HB_OPT_FUNC( hb_p_not )
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
/* This optimization will be enabled in the future in a little bit differ form */
#if 0
      case HB_P_DUPLICATE:
         if( ( pFunc->pCode[ lPCodePos + 2 ] == HB_P_JUMPTRUEFAR ||
               pFunc->pCode[ lPCodePos + 2 ] == HB_P_JUMPFALSEFAR ) &&
             pFunc->pCode[ lPCodePos + 6 ] == HB_P_POP )
         {
            BYTE * pAddr = &pFunc->pCode[ lPCodePos + 3 ];
            LONG lOffset = HB_PCODE_MKINT24( pAddr );

            if( lOffset > 0 )
            {
               hb_p_duplicate( pFunc, lPCodePos + 1, cargo );
               lOffset = HB_PCODE_MKINT24( pAddr );
            }

            if( ( pFunc->pCode[ lPCodePos + 1 ] == HB_P_NOT ||
                  ( pFunc->pCode[ lPCodePos + 1 ] == HB_P_DUPLICATE &&
                    pFunc->pCode[ lPCodePos + lOffset + 2 ] == HB_P_NOT ) ) &&
                ! hb_compIsJump( cargo->HB_COMP_PARAM, pFunc, lPCodePos + 1 ) )
            {
               hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
               if( pFunc->pCode[ lPCodePos + 2 ] == HB_P_JUMPTRUEFAR )
                  pFunc->pCode[ lPCodePos + 2 ] = HB_P_JUMPFALSEFAR;
               else
                  pFunc->pCode[ lPCodePos + 2 ] = HB_P_JUMPTRUEFAR;
               if( pFunc->pCode[ lPCodePos + 1 ] == HB_P_DUPLICATE )
               {
                  ++lOffset;
                  HB_PUT_LE_UINT24( pAddr, lOffset );
               }
            }
         }
         /* no break; */
#endif
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
   return 1;
}

static HB_OPT_FUNC( hb_p_jumpfar )
{
   BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
   LONG lOffset = HB_PCODE_MKINT24( pAddr );
   ULONG ulNewPos = lPCodePos + lOffset;

   HB_SYMBOL_UNUSED( cargo );

   if( lOffset == 4 )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
   }
   else switch( pFunc->pCode[ ulNewPos ] )
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
   return 4;
}

static HB_OPT_FUNC( hb_p_jumpfalsefar )
{
   BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
   LONG lOffset = HB_PCODE_MKINT24( pAddr );
   ULONG ulNewPos = lPCodePos + lOffset;

   HB_SYMBOL_UNUSED( cargo );

   if( lOffset == 8 && pFunc->pCode[ lPCodePos + 4 ] == HB_P_JUMPFAR )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
      pFunc->pCode[ lPCodePos + 4 ] = HB_P_JUMPTRUEFAR;
   }
   else if( pFunc->pCode[ ulNewPos ] == HB_P_JUMPFAR )
   {
      lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
      HB_PUT_LE_UINT24( pAddr, lOffset );
   }
   return 4;
}

static HB_OPT_FUNC( hb_p_jumptruefar )
{
   BYTE * pAddr = &pFunc->pCode[ lPCodePos + 1 ];
   LONG lOffset = HB_PCODE_MKINT24( pAddr );
   ULONG ulNewPos = lPCodePos + lOffset;

   HB_SYMBOL_UNUSED( cargo );

   if( lOffset == 8 && pFunc->pCode[ lPCodePos + 4 ] == HB_P_JUMPFAR )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 4, FALSE, FALSE );
      pFunc->pCode[ lPCodePos + 4 ] = HB_P_JUMPFALSEFAR;
   }
   else if( pFunc->pCode[ ulNewPos ] == HB_P_JUMPFAR )
   {
      lOffset += HB_PCODE_MKINT24( &pFunc->pCode[ ulNewPos + 1 ] );
      HB_PUT_LE_UINT24( pAddr, lOffset );
   }
   return 4;
}

static HB_OPT_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( cargo );

   if( lPCodePos + 1 < pFunc->lPCodePos &&
       pFunc->pCode[ lPCodePos + 1 ] == HB_P_ENDBLOCK )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
   }
   return 1;
}

static HB_OPT_FUNC( hb_p_endproc )
{
   HB_SYMBOL_UNUSED( cargo );

   if( lPCodePos + 1 < pFunc->lPCodePos &&
       pFunc->pCode[ lPCodePos + 1 ] == HB_P_ENDPROC )
   {
      hb_compNOOPfill( pFunc, lPCodePos, 1, FALSE, FALSE );
   }
   return 1;
}

/* NOTE: The  order of functions have to match the order of opcodes mnemonics
 */
static const HB_OPT_FUNC_PTR s_opt_table[] =
{
   NULL,                       /* HB_P_AND,                  */
   NULL,                       /* HB_P_ARRAYPUSH,            */
   NULL,                       /* HB_P_ARRAYPOP,             */
   NULL,                       /* HB_P_ARRAYDIM,             */
   NULL,                       /* HB_P_ARRAYGEN,             */
   NULL,                       /* HB_P_EQUAL,                */
   hb_p_endblock,              /* HB_P_ENDBLOCK,             */
   hb_p_endproc,               /* HB_P_ENDPROC,              */
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
   hb_p_duplicate,             /* HB_P_DUPLICATE,            */
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
   NULL,                       /* HB_P_POPLOCALNEAR,         */
   NULL,                       /* HB_P_POPMEMVAR,            */
   NULL,                       /* HB_P_POPSTATIC,            */
   NULL,                       /* HB_P_POPVARIABLE,          */
   NULL,                       /* HB_P_POWER,                */
   NULL,                       /* HB_P_PUSHALIAS,            */
   NULL,                       /* HB_P_PUSHALIASEDFIELD,     */
   NULL,                       /* HB_P_PUSHALIASEDFIELDNEAR, */
   NULL,                       /* HB_P_PUSHALIASEDVAR,       */
   NULL,                       /* HB_P_PUSHBLOCK,            */
   NULL,                       /* HB_P_PUSHBLOCKSHORT,       */
   NULL,                       /* HB_P_PUSHFIELD,            */
   NULL,                       /* HB_P_PUSHBYTE,             */
   NULL,                       /* HB_P_PUSHINT,              */
   hb_p_pushlocal,             /* HB_P_PUSHLOCAL,            */
   hb_p_pushlocalnear,         /* HB_P_PUSHLOCALNEAR,        */
   NULL,                       /* HB_P_PUSHLOCALREF,         */
   NULL,                       /* HB_P_PUSHLONG,             */
   hb_p_pushmemvar,            /* HB_P_PUSHMEMVAR,           */
   NULL,                       /* HB_P_PUSHMEMVARREF,        */
   hb_p_pushnil,               /* HB_P_PUSHNIL,              */
   NULL,                       /* HB_P_PUSHDOUBLE,           */
   NULL,                       /* HB_P_PUSHSELF,             */
   hb_p_pushstatic,            /* HB_P_PUSHSTATIC,           */
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
   NULL,                       /* HB_P_LOCALNEARADDINT,      */
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
   NULL,                       /* HB_P_VFRAME                */
   NULL,                       /* HB_P_LARGEFRAME            */
   NULL,                       /* HB_P_LARGEVFRAME           */
   NULL,                       /* HB_P_PUSHSTRHIDDEN         */
   hb_p_localaddint,           /* HB_P_LOCALADDINT           */
   NULL,                       /* HB_P_MODEQPOP              */
   NULL,                       /* HB_P_EXPEQPOP              */
   NULL,                       /* HB_P_MODEQ                 */
   NULL,                       /* HB_P_EXPEQ                 */
   NULL,                       /* HB_P_DUPLUNREF             */
   NULL,                       /* HB_P_MPUSHBLOCKLARGE       */
   NULL,                       /* HB_P_MPUSHSTRLARGE         */
   NULL,                       /* HB_P_PUSHBLOCKLARGE        */
   NULL,                       /* HB_P_PUSHSTRLARGE          */
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
   NULL                        /* HB_P_HASHGEN               */
};

void hb_compOptimizePCode( HB_COMP_DECL, PFUNCTION pFunc )
{
   const HB_OPT_FUNC_PTR * pFuncTable = s_opt_table;
   HB_OPT_INFO opt_info;

   opt_info.HB_COMP_PARAM = HB_COMP_PARAM;
   assert( HB_P_LAST_PCODE == sizeof( s_opt_table ) / sizeof( HB_OPT_FUNC_PTR ) );

   hb_compPCodeEval( pFunc, ( const HB_PCODE_FUNC_PTR * ) pFuncTable, ( void * ) &opt_info );
}

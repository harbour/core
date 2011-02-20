/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source with real code generation
 *
 * Copyright 2006-2009 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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
#include "hbdate.h"
#include "hbassert.h"

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, PHB_LABEL_INFO )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

#define HB_GENC_GETLABEL(l)   ( (l) < pFunc->nPCodePos ? cargo->pnLabels[ (l) ] : 0 )

#define HB_GENC_LABEL()       do { \
                                 HB_SIZE nLab = HB_GENC_GETLABEL( nPCodePos ); \
                                 if( nLab != 0 ) \
                                    fprintf( cargo->yyc, "lab%05" HB_PFS "d: ;\n", nLab ); \
                              } while( 0 )

#define HB_GENC_ERROR(s)       do { \
                                 fprintf( cargo->yyc, "\t#error: \"" s "\"\n" ); \
                              } while( 0 )

void hb_compGenCString( FILE * yyc, const HB_BYTE * pText, HB_SIZE nLen )
{
   HB_SIZE nPos;

   fputc( '"', yyc );
   for( nPos = 0; nPos < nLen; nPos++ )
   {
      HB_BYTE uchr = ( HB_BYTE ) pText[ nPos ];
      /*
       * NOTE: After optimization some CHR(n) can be converted
       *       into a string containing nonprintable characters.
       *
       * ? is escaped to avoid conflicts with trigraph sequences which
       * are part of ANSI C standard
       */
      if( uchr == '"' || uchr == '\\' || uchr == '?' )
         fprintf( yyc, "\\%c", uchr );
      else if( uchr < ( HB_BYTE ) ' ' || uchr >= 127 )
      {
         HB_BYTE uchrnext = nPos < nLen - 1 ? pText[ nPos + 1 ] : 0;

         fprintf( yyc, "\\x%02X%s", uchr,
                  ( uchrnext >= ( HB_BYTE ) '0' && uchrnext <= ( HB_BYTE ) '9' ) ||
                  ( uchrnext >= ( HB_BYTE ) 'a' && uchrnext <= ( HB_BYTE ) 'z' ) ||
                  ( uchrnext >= ( HB_BYTE ) 'A' && uchrnext <= ( HB_BYTE ) 'Z' ) ? "\" \"" : "" );
      }
      else
         fprintf( yyc, "%c", uchr );
   }
   fputc( '"', yyc );
}

static void hb_compGenCStrData( FILE * yyc, const HB_BYTE * pText, HB_SIZE nLen,
                                int iMethod )
{
#ifdef __HB_CSTRING_SIZE_MAX
   #if __HB_CSTRING_SIZE_MAX -0 < 1
      #undef __HB_CSTRING_SIZE_MAX
      #define __HB_CSTRING_SIZE_MAX   4096
   #endif
   if( nLen > __HB_CSTRING_SIZE_MAX )
   {
      HB_SIZE nPos;

      fprintf( yyc, "\t{\tconst unsigned char str[ %" HB_PFS "u ] = {", nLen + 1 );
      for( nPos = 0; nPos < nLen; nPos++ )
      {
         if( ( nPos & 0x0F ) == 0 )
            fprintf( yyc, "\n\t\t" );
         fprintf( yyc, "%d,", ( int ) pText[ nPos ] );
      }
      fprintf( yyc, "0 };\n\t\thb_xvmPushString" );
      if( iMethod < 0 )
         fprintf( yyc, "Const( " );
      else
         fprintf( yyc, "Hidden( %d, ", iMethod );
      fprintf( yyc, "( const char * ) str,  %" HB_PFS "u );\n\t}\n", nLen );
   }
   else
#endif
   {
      fprintf( yyc, "\thb_xvmPushString" );
      if( iMethod < 0 )
         fprintf( yyc, "Const( " );
      else
         fprintf( yyc, "Hidden( %d, ", iMethod );
      hb_compGenCString( yyc, pText, nLen );
      fprintf( yyc, ", %" HB_PFS "u );\n", nLen );
   }
}

static void hb_gencc_copyLocals( FILE * yyc, int iLocal1, int iLocal2 )
{
   if( iLocal1 != iLocal2 )
      fprintf( yyc, "\thb_xvmCopyLocals( %d, %d );\n", iLocal1, iLocal2 );
}

static int hb_gencc_checkJumpCondAhead( HB_LONG lValue, PFUNCTION pFunc, HB_SIZE nPCodePos, PHB_LABEL_INFO cargo,
                                        const char * szFunc )
{
   if( HB_GENC_GETLABEL( nPCodePos + 1 ) == 0 )
   {
      HB_ISIZ nOffset = 0;
      HB_BOOL fNot = HB_FALSE;
      int iSize = 0;

      switch( pFunc->pCode[ nPCodePos + 1 ] )
      {
         case HB_P_JUMPFALSENEAR:
            nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 2 ] );
            fNot = HB_TRUE;
            iSize = 3;
            break;

         case HB_P_JUMPFALSE:
            nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
            fNot = HB_TRUE;
            iSize = 4;
            break;

         case HB_P_JUMPFALSEFAR:
            nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 2 ] );
            fNot = HB_TRUE;
            iSize = 5;
            break;

         case HB_P_JUMPTRUENEAR:
            nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 3;
            break;

         case HB_P_JUMPTRUE:
            nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 4;
            break;

         case HB_P_JUMPTRUEFAR:
            nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 2 ] );
            iSize = 5;
            break;
      }

      if( iSize )
      {
         fprintf( cargo->yyc, "\tif( hb_xvm%sIntIs( %ld, &fValue ) ) break;\n",
                  szFunc, lValue );
         fprintf( cargo->yyc, "\tif( %sfValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
                  fNot ? "!" : "", HB_GENC_GETLABEL( nPCodePos + 1 + nOffset ) );
         return iSize;
      }
   }
   fprintf( cargo->yyc, "\tif( hb_xvm%sInt( %ld ) ) break;\n",
            szFunc, lValue );
   return 1;
}

static int hb_gencc_checkNumAhead( HB_LONG lValue, PFUNCTION pFunc, HB_SIZE nPCodePos, PHB_LABEL_INFO cargo )
{
   if( HB_GENC_GETLABEL( nPCodePos ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_POPLOCAL:
            fprintf( cargo->yyc, "\thb_xvmLocalSetInt( %d, %ld );\n",
                     HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     lValue );
            return 3;

         case HB_P_POPLOCALNEAR:
            fprintf( cargo->yyc, "\thb_xvmLocalSetInt( %d, %ld );\n",
                     ( signed char ) pFunc->pCode[ nPCodePos + 1 ], lValue );
            return 2;

         case HB_P_EQUAL:
         case HB_P_EXACTLYEQUAL:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "Equal" );

         case HB_P_NOTEQUAL:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "NotEqual" );

         case HB_P_GREATER:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "GreaterThen" );

         case HB_P_GREATEREQUAL:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "GreaterEqualThen" );

         case HB_P_LESS:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "LessThen" );

         case HB_P_LESSEQUAL:
            return hb_gencc_checkJumpCondAhead( lValue, pFunc, nPCodePos, cargo, "LessEqualThen" );

         case HB_P_ARRAYPUSH:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( hb_xvmArrayItemPush( %ld ) ) break;\n", lValue );
               return 1;
            }
            break;

         case HB_P_ARRAYPOP:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( hb_xvmArrayItemPop( %ld ) ) break;\n", lValue );
               return 1;
            }
            break;

         case HB_P_MULT:
            fprintf( cargo->yyc, "\tif( hb_xvmMultByInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_DIVIDE:
            fprintf( cargo->yyc, "\tif( hb_xvmDivideByInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_MODULUS:
            fprintf( cargo->yyc, "\tif( hb_xvmModulusByInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_MINUS:
            if( lValue > 0 )
            {
               fprintf( cargo->yyc, "\tif( hb_xvmAddInt( -%ld ) ) break;\n", lValue );
               return 1;
            }
#if -LONG_MAX > LONG_MIN
            else if( lValue < -LONG_MAX )
               break;
#endif
            lValue = -lValue;
            /* no break */

         case HB_P_PLUS:
            fprintf( cargo->yyc, "\tif( hb_xvmAddInt( %ld ) ) break;\n", lValue );
            return 1;

         case HB_P_RETVALUE:
            fprintf( cargo->yyc, "\thb_xvmRetInt( %ld );\n", lValue );
            return 1;
      }
   }
   return 0;
}

static int hb_gencc_checkPlusAhead( PFUNCTION pFunc, HB_SIZE nPCodePos, PHB_LABEL_INFO cargo )
{
   if( HB_GENC_GETLABEL( nPCodePos ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_POPLOCALNEAR:
            fprintf( cargo->yyc, "\thb_xvmLocalAdd( %d );\n",
                     ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
            return 2;

         case HB_P_POPLOCAL:
            fprintf( cargo->yyc, "\thb_xvmLocalAdd( %d );\n",
                     HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;

         case HB_P_POPSTATIC:
            fprintf( cargo->yyc, "\thb_xvmStaticAdd( %hu );\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;

         case HB_P_POPMEMVAR:
            fprintf( cargo->yyc, "\thb_xvmMemvarAdd( symbols + %hu );\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 3;
      }
   }
   return 0;
}

static HB_GENC_FUNC( hb_p_and )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmAnd() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPush() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypushref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPushRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmArrayPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDec() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmArrayDim( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDivide() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDo( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDo( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDuplicate();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_duplunref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmDuplUnRef();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushunref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushUnRef();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_swap )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSwap(%d);\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmExactlyEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_GENC_LABEL();

   HB_GENC_ERROR( "HB_P_ENDBLOCK" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\t/* *** END PROC *** */\n" );
   if( nPCodePos < pFunc->nPCodePos - 1 )
   {
      if( cargo->iNestedBlock )
      {
         cargo->fEndRequest = HB_TRUE;
         fprintf( cargo->yyc, "\thb_xvmEndProc();\n" );
      }
      fprintf( cargo->yyc, "\tbreak;\n" );
   }
   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLogical( HB_FALSE );\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmForTest() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFrame( %u, %u );\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFuncPtr();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmFunction( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmFunction( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmArrayGen( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_hashgen )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmHashGen( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmGreater() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmGreaterEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmInc() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmInstring() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   HB_ISIZ nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   HB_ISIZ nOffset = HB_PCODE_MKSHORT( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   HB_ISIZ nOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( !fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   HB_ISIZ nOffset = ( signed char ) ( pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   HB_ISIZ nOffset = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   HB_ISIZ nOffset = HB_PCODE_MKINT24( &( pFunc->pCode[ nPCodePos + 1 ] ) );

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopLogical( &fValue ) ) break;\n\tif( fValue )\n\t\tgoto lab%05" HB_PFS "d;\n",
            HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLess() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLessEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSetLine( %d );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   HB_USHORT usLen;

   HB_GENC_LABEL();

   usLen = ( HB_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 3 ] );
   fprintf( cargo->yyc, "\thb_xvmLocalName( %hu, ",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   hb_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 4;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPop( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPopAliased( %d ) ) break;\n", pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPush( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrodo )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroDo( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_macrofunc )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroFunc( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_macrosend )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroSend( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_macroarraygen )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroArrayGen( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushList( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushIndex() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushPare( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroPushAliased( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroSymbol() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMacroText() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMinus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   HB_USHORT usLen;

   HB_GENC_LABEL();

   usLen = ( HB_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 1 ] );
   fprintf( cargo->yyc, "\thb_xvmModuleName( " );
   hb_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 1 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 2;
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmModulus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMult() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNegate() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNot() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmNotEqual() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmOr() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmParameter( symbols + %hu, %d );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   int iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkPlusAhead( pFunc, nPCodePos + 1, cargo );

   if( iSkip != 0 )
      return 1 + iSkip;

   fprintf( cargo->yyc, "\tif( hb_xvmPlus() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_stackPop();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedField( symbols + %u ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopAliasedVar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopLocal( %hd );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopLocal( %d );\n",
            ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopMemvar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPopStatic( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPopVariable( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPower() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedField( symbols + %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushAliasedVar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   HB_USHORT usSize, us;

   HB_GENC_LABEL();

   usSize = pFunc->pCode[ nPCodePos + 1 ] - 2;
   nPCodePos += 2;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const HB_BYTE codeblock[ %hd ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ nPCodePos + us ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ nPCodePos + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlockShort( codeblock, symbols );\n\t}\n" );

   return 2 + usSize;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   HB_USHORT usSize, us;

   HB_GENC_LABEL();

   usSize = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) - 3;
   nPCodePos += 3;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const HB_BYTE codeblock[ %hd ] = {", usSize );

   for( us = 0; us < usSize; ++us )
   {
      if( ( us & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( us == usSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ nPCodePos + us ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ nPCodePos + us ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlock( codeblock, symbols );\n\t}\n" );

   return 3 + usSize;
}

static HB_GENC_FUNC( hb_p_pushblocklarge )
{
   HB_SIZE nSize, ul;

   HB_GENC_LABEL();

   nSize = HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) - 4;
   nPCodePos += 4;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const HB_BYTE codeblock[ %" HB_PFS "u ] = {", nSize );

   for( ul = 0; ul < nSize; ++ul )
   {
      if( ( ul & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( ul == nSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ nPCodePos + ul ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ nPCodePos + ul ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmPushBlock( codeblock, symbols );\n\t}\n" );

   return 4 + nSize;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   HB_GENC_LABEL();

#if 0
   fprintf( cargo->yyc, "\thb_xvmPushDouble( %.*f, %d, %d );\n",
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) + sizeof( HB_BYTE ) ] + 1,
            HB_PCODE_MKDOUBLE( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) ],
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) + sizeof( HB_BYTE ) ] );
#else
   /*
    * This version keeps double calculation compatible with RT FL functions
    */
   fprintf( cargo->yyc, "\thb_xvmPushDouble( * ( double * ) " );
   {
      double d = HB_PCODE_MKDOUBLE( &pFunc->pCode[ nPCodePos + 1 ] );
      hb_compGenCString( cargo->yyc, ( const HB_BYTE * ) &d, sizeof( double ) );
   }
   fprintf( cargo->yyc, ", %d, %d );\n",
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) ],
            pFunc->pCode[ nPCodePos + 1 + sizeof( double ) + sizeof( HB_BYTE ) ] );
#endif
   return sizeof( double ) + sizeof( HB_BYTE ) + sizeof( HB_BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushField( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   int iVal = ( signed char ) pFunc->pCode[ nPCodePos + 1 ], iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( iVal, pFunc, nPCodePos + 2, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", iVal );
   return 2 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   int iVal = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( iVal, pFunc, nPCodePos + 3, cargo );

   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", iVal );
   return 3 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   HB_GENC_LABEL();

   if( HB_GENC_GETLABEL( nPCodePos + 3 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 3 ] )
      {
         case HB_P_POPLOCALNEAR:
            hb_gencc_copyLocals( cargo->yyc,
                        HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                        ( signed char ) pFunc->pCode[ nPCodePos + 4 ] );
            return 5;

         case HB_P_POPLOCAL:
            hb_gencc_copyLocals( cargo->yyc,
                        HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                        HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
      }
   }

   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   HB_GENC_LABEL();

   if( HB_GENC_GETLABEL( nPCodePos + 2 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 2 ] )
      {
         case HB_P_POPLOCALNEAR:
            hb_gencc_copyLocals( cargo->yyc,
                        ( signed char ) pFunc->pCode[ nPCodePos + 1 ],
                        ( signed char ) pFunc->pCode[ nPCodePos + 3 ] );
            return 4;

         case HB_P_POPLOCAL:
            hb_gencc_copyLocals( cargo->yyc,
                        ( signed char ) pFunc->pCode[ nPCodePos + 1 ],
                        HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
      }
   }

   fprintf( cargo->yyc, "\thb_xvmPushLocal( %d );\n",
            ( signed char ) pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLocalByRef( %d );\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   HB_LONG lVal = HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( lVal, pFunc, nPCodePos + 5, cargo );

   if( iSkip == 0 )
   {
#if INT_MAX >= INT32_MAX
      fprintf( cargo->yyc, "\thb_xvmPushInteger( %d );\n", ( int ) lVal );
#else
      fprintf( cargo->yyc, "\thb_xvmPushLong( %ldL );\n", lVal );
#endif
   }
   return 5 + iSkip;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
#ifdef HB_LONG_LONG_OFF
   HB_GENC_LABEL();
   fprintf( cargo->yyc, "\thb_xvmPushLongLong( %.1f );\n", HB_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 9;
#elif LONG_MAX == LONGLONG_MAX
   HB_LONGLONG llVal = HB_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ), iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( llVal, pFunc, nPCodePos + 9, cargo );

   if( iSkip == 0 )
   {
      fprintf( cargo->yyc, "\thb_xvmPushLong( %ldL );\n", ( long ) llVal );
   }
   return 9 + iSkip;
#else
   char szBuf[ 24 ];
   HB_GENC_LABEL();
   fprintf( cargo->yyc, "\thb_xvmPushLongLong( HB_LL( %s ) );\n",
            hb_numToStr( szBuf, sizeof( szBuf ),
                         HB_PCODE_MKLONGLONG( &pFunc->pCode[ nPCodePos + 1 ] ) ) );
   return 9;
#endif
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushMemvar( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushMemvarByRef( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_GENC_LABEL();

   if( pFunc->pCode[ nPCodePos + 1 ] == HB_P_RETVALUE &&
       HB_GENC_GETLABEL( nPCodePos + 1 ) == 0 )
   {
      fprintf( cargo->yyc, "\thb_xvmRetNil();\n" );
      return 2;
   }
   else
   {
      fprintf( cargo->yyc, "\thb_xvmPushNil();\n" );
      return 1;
   }
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushSelf();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStatic( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStaticByRef( %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   HB_USHORT usLen = pFunc->pCode[ nPCodePos + 1 ] - 1;

   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushStringConst( " );
   hb_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ], usLen );
   fprintf( cargo->yyc, ", %hu );\n", usLen );

   return 3 + usLen;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   HB_SIZE nLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) - 1;

   HB_GENC_LABEL();

   hb_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 3 ], nLen, -1 );

   return 4 + nLen;
}

static HB_GENC_FUNC( hb_p_pushstrlarge )
{
   HB_SIZE nLen = HB_PCODE_MKUINT24( &pFunc->pCode[ nPCodePos + 1 ] ) - 1;

   HB_GENC_LABEL();

   hb_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen, -1 );

   return 5 + nLen;
}

static HB_GENC_FUNC( hb_p_pushstrhidden )
{
   HB_SIZE nLen = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] );

   HB_GENC_LABEL();

   hb_compGenCStrData( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], nLen,
                       pFunc->pCode[ nPCodePos + 1 ] );

   return 4 + nLen;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   HB_GENC_LABEL();

   if( HB_GENC_GETLABEL( nPCodePos + 3 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 3 ] )
      {
         case HB_P_PUSHNIL:
            fprintf( cargo->yyc, "\thb_xvmPushFuncSymbol( symbols + %hu );\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
            return 4;
         case HB_P_PUSHALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     pFunc->pCode[ nPCodePos + 4 ] );
            return 5;
         case HB_P_PUSHALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
         case HB_P_POPALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     pFunc->pCode[ nPCodePos + 4 ] );
            return 5;
         case HB_P_POPALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) );
            return 6;
      }
   }

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   HB_GENC_LABEL();

   if( HB_GENC_GETLABEL( nPCodePos + 2 ) == 0 )
   {
      switch( pFunc->pCode[ nPCodePos + 2 ] )
      {
         case HB_P_PUSHNIL:
            fprintf( cargo->yyc, "\thb_xvmPushFuncSymbol( symbols + %u );\n",
                     pFunc->pCode[ nPCodePos + 1 ] );
            return 3;
         case HB_P_PUSHALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     pFunc->pCode[ nPCodePos + 3 ] );
            return 4;
         case HB_P_PUSHALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPushAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
         case HB_P_POPALIASEDFIELDNEAR:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     pFunc->pCode[ nPCodePos + 3 ] );
            return 4;
         case HB_P_POPALIASEDFIELD:
            fprintf( cargo->yyc,
                     "\tif( hb_xvmPopAliasedFieldExt( symbols + %u, symbols + %u ) ) break;\n",
                     pFunc->pCode[ nPCodePos + 1 ],
                     HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
            return 5;
      }
   }

   fprintf( cargo->yyc, "\thb_xvmPushSymbol( symbols + %d );\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushfuncsym )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushFuncSymbol( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushVariable( symbols + %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmRetValue();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSend( %hu ) ) break;\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSend( %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushovarref )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPushObjectVarRef() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqalways )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSeqAlways();\n\tdo {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static HB_GENC_FUNC( hb_p_alwaysbegin )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\t} while( 0 );\n\tif( hb_xvmAlwaysBegin() ) break;\n\tdo {\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_alwaysend )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\t} while( 0 );\n\tif( hb_xvmAlwaysEnd() ) break;\n" );
   cargo->iNestedBlock--;
   return 1;
}

static HB_GENC_FUNC( hb_p_seqblock )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSeqBlock() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSeqBegin();\n\tdo {\n" );
   cargo->iNestedBlock++;
   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   HB_ISIZ nOffset = HB_PCODE_MKINT24( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   if( nOffset == 4 ) /* no RECOVER clasue */
      fprintf( cargo->yyc, "\t} while( 0 );\n\tif( hb_xvmSeqEnd() ) break;\n" );
   else /* RECOVER exists */
      fprintf( cargo->yyc, "\tif( hb_xvmSeqEndTest() ) break;\n\tgoto lab%05" HB_PFS "d;\n\t} while( 0 );\n",
               HB_GENC_GETLABEL( nPCodePos + nOffset ) );
   cargo->iNestedBlock--;
   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSeqRecover() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmSFrame( symbols + %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmStatics( symbols + %hu, %hu );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   HB_USHORT usLen;

   HB_GENC_LABEL();

   usLen = ( HB_USHORT ) strlen( ( char * ) &pFunc->pCode[ nPCodePos + 4 ] );
   fprintf( cargo->yyc, "\thb_xvmStaticName( %hu, %hu, ",
            ( HB_USHORT ) pFunc->pCode[ nPCodePos + 1 ],
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   hb_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 4 ], usLen );
   fprintf( cargo->yyc, " );\n" );
   return usLen + 5;
}

static HB_GENC_FUNC( hb_p_threadstatics )
{
   HB_USHORT w;
   HB_SIZE nSize, ul;

   HB_GENC_LABEL();

   w = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );
   nSize = ( HB_SIZE ) w << 1;

   fprintf( cargo->yyc, "\t{\n\t\tstatic const HB_BYTE statics[ %" HB_PFS "u ] = {", nSize );

   for( ul = 0; ul < nSize; ++ul )
   {
      if( ( ul & 0x0f ) == 0 )
         fprintf( cargo->yyc, "\n\t\t\t" );
      if( ul == nSize - 1 )
         fprintf( cargo->yyc, "%d", pFunc->pCode[ nPCodePos + ul + 3 ] );
      else
         fprintf( cargo->yyc, "%d, ", pFunc->pCode[ nPCodePos + ul + 3 ] );
   }
   fprintf( cargo->yyc, " };\n\t\thb_xvmThreadStatics( %hu, statics );\n\t}\n", w );

   return 3 + nSize;
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmSwapAlias() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushLogical( HB_TRUE );\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   int iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( 1, pFunc, nPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( 1 );\n" );
   return 1 + iSkip;
}

static HB_GENC_FUNC( hb_p_zero )
{
   int iSkip;

   HB_GENC_LABEL();

   iSkip = hb_gencc_checkNumAhead( 0, pFunc, nPCodePos + 1, cargo );
   if( iSkip == 0 )
      fprintf( cargo->yyc, "\thb_xvmPushInteger( 0 );\n" );
   return 1 + iSkip;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_GENC_LABEL();

   return 1;
}

static HB_GENC_FUNC( hb_p_dummy )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( nPCodePos );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumstart )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEnumStart( %d, %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_enumnext )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEnumNext() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumprev )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmEnumPrev() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumend )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmEnumEnd();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_switch )
{
   HB_USHORT usCases = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ), us;
   HB_SIZE nStart = nPCodePos, nNewPos;
   HB_BOOL fNum = HB_FALSE, fStr = HB_FALSE, fDefault = HB_FALSE;

   HB_GENC_LABEL();

   nPCodePos += 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_PUSHLONG:
            fNum = HB_TRUE;
            nPCodePos += 5;
            break;
         case HB_P_PUSHSTRSHORT:
            fStr = HB_TRUE;
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case HB_P_PUSHNIL:
            /* default clause */
            fDefault = HB_TRUE;
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
   }

   if( fStr || fNum )
   {
      fprintf( cargo->yyc, "\t{\n\t\tPHB_ITEM pSwitch;\n\t\tHB_TYPE type;\n" );
      if( fStr )
         fprintf( cargo->yyc, "\t\tconst char * pszText;\n\t\tHB_SIZE nLen;\n" );
      if( fNum )
         fprintf( cargo->yyc, "\t\tlong lVal;\n" );
      fprintf( cargo->yyc, "\t\tif( hb_xvmSwitchGet( &pSwitch ) ) break;\n"
                           "\t\ttype = hb_itemType( pSwitch );\n" );
      if( fStr )
      {
         fprintf( cargo->yyc, "\t\tpszText = ( type & HB_IT_STRING ) ? hb_itemGetCPtr( pSwitch ) : NULL;\n" );
         fprintf( cargo->yyc, "\t\tnLen = pszText ? hb_itemGetCLen( pSwitch ) : 0;\n" );
      }
      if( fNum )
         fprintf( cargo->yyc, "\t\tlVal = ( type & HB_IT_NUMINT ) ? hb_itemGetNL( pSwitch ) : 0;\n\n" );
   }

   nPCodePos = nStart + 3;
   for( us = 0; us < usCases; ++us )
   {
      switch( pFunc->pCode[ nPCodePos ] )
      {
         case HB_P_PUSHLONG:
            fprintf( cargo->yyc, "\t\tif( ( type & HB_IT_NUMINT ) != 0 && lVal == %ldL )\n",
                     HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
            nPCodePos += 5;
            break;
         case HB_P_PUSHSTRSHORT:
            fprintf( cargo->yyc, "\t\tif( pszText && nLen == %d && !memcmp( pszText, ",
                     pFunc->pCode[ nPCodePos + 1 ] - 1 );
            hb_compGenCString( cargo->yyc, &pFunc->pCode[ nPCodePos + 2 ],
                               pFunc->pCode[ nPCodePos + 1 ] - 1 );
            fprintf( cargo->yyc, ", %d ) )\n", pFunc->pCode[ nPCodePos + 1 ] - 1 );
            nPCodePos += 2 + pFunc->pCode[ nPCodePos + 1 ];
            break;
         case HB_P_PUSHNIL:
            /* default clause */
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
      fprintf( cargo->yyc, "\t\t{\n\t\t\thb_stackPop();\n\t\t\tgoto lab%05" HB_PFS "d;\n\t\t}\n",
               HB_GENC_GETLABEL( nNewPos ) );
   }
   if( !fDefault )
      fprintf( cargo->yyc, "\t\thb_stackPop();\n" );
   if( fStr || fNum )
      fprintf( cargo->yyc, "\t}\n" );

   return nPCodePos - nStart;
}

static HB_GENC_FUNC( hb_p_pushdate )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushDate( %ldL );\n",
            ( long ) HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_pushtimestamp )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushTimeStamp( %ldL, %ldL );\n",
            ( long ) HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 1 ] ),
            ( long ) HB_PCODE_MKLONG( &pFunc->pCode[ nPCodePos + 5 ] ) );
   return 9;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalAddInt( %d, %d ) ) break;\n",
            pFunc->pCode[ nPCodePos + 1 ],
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 2 ] ) );
   return 4;
}

static HB_GENC_FUNC( hb_p_localaddint )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalAddInt( %d, %d ) ) break;\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 3 ] ) );
   return 5;
}

static HB_GENC_FUNC( hb_p_localinc )
{
   int iLocal = HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   if( HB_GENC_GETLABEL( nPCodePos + 3 ) == 0 &&
       ( ( pFunc->pCode[ nPCodePos + 3 ] == HB_P_PUSHLOCAL &&
           iLocal == HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 4 ] ) ) ||
         ( pFunc->pCode[ nPCodePos + 3 ] == HB_P_PUSHLOCALNEAR &&
           iLocal == pFunc->pCode[ nPCodePos + 4 ] ) ) )
   {
      fprintf( cargo->yyc, "\tif( hb_xvmLocalIncPush( %d ) ) break;\n", iLocal );
      return ( pFunc->pCode[ nPCodePos + 3 ] == HB_P_PUSHLOCAL ) ? 6 : 5;
   }

   fprintf( cargo->yyc, "\tif( hb_xvmLocalInc( %d ) ) break;\n", iLocal );
   return 3;
}

static HB_GENC_FUNC( hb_p_localdec )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalDec( %d ) ) break;\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );
   return 3;
}

static HB_GENC_FUNC( hb_p_localincpush )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmLocalIncPush( %d ) ) break;\n",
            HB_PCODE_MKSHORT( &pFunc->pCode[ nPCodePos + 1 ] ) );

   return 3;
}

static HB_GENC_FUNC( hb_p_pluseqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPlusEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_minuseqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMinusEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_multeqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMultEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_diveqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDivEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modeqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmModEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_expeqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmExpEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_deceqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDecEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inceqpop )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmIncEqPop() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pluseq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmPlusEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_minuseq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMinusEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_multeq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmMultEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_diveq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDivEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modeq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmModEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_expeq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmExpEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_deceq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmDecEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inceq )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\tif( hb_xvmIncEq() ) break;\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_withobjectstart )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmWithObjectStart();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_withobjectend )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmWithObjectEnd();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_withobjectmessage )
{
   HB_USHORT usSym = HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] );

   HB_GENC_LABEL();

   if( usSym == 0xFFFF )
      fprintf( cargo->yyc, "\thb_xvmWithObjectMessage( NULL );\n" );
   else
      fprintf( cargo->yyc, "\thb_xvmWithObjectMessage( symbols + %hu );\n",
               usSym );

   return 3;
}

static HB_GENC_FUNC( hb_p_vframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmVFrame( %u, %u );\n",
            pFunc->pCode[ nPCodePos + 1 ], pFunc->pCode[ nPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_largeframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmFrame( %u, %u );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static HB_GENC_FUNC( hb_p_largevframe )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmVFrame( %u, %u );\n",
            HB_PCODE_MKUSHORT( &pFunc->pCode[ nPCodePos + 1 ] ),
            pFunc->pCode[ nPCodePos + 3 ] );
   return 4;
}

static HB_GENC_FUNC( hb_p_pushvparams )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushVParams();\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaparams )
{
   HB_GENC_LABEL();

   fprintf( cargo->yyc, "\thb_xvmPushAParams();\n" );
   return 1;
}


/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static const HB_GENC_FUNC_PTR s_verbose_table[] = {
   hb_p_and,
   hb_p_arraypush,
   hb_p_arraypop,
   hb_p_arraydim,
   hb_p_arraygen,
   hb_p_equal,
   hb_p_endblock,
   hb_p_endproc,
   hb_p_exactlyequal,
   hb_p_false,
   hb_p_fortest,
   hb_p_function,
   hb_p_functionshort,
   hb_p_frame,
   hb_p_funcptr,
   hb_p_greater,
   hb_p_greaterequal,
   hb_p_dec,
   hb_p_divide,
   hb_p_do,
   hb_p_doshort,
   hb_p_duplicate,
   hb_p_pushtimestamp,
   hb_p_inc,
   hb_p_instring,
   hb_p_jumpnear,
   hb_p_jump,
   hb_p_jumpfar,
   hb_p_jumpfalsenear,
   hb_p_jumpfalse,
   hb_p_jumpfalsefar,
   hb_p_jumptruenear,
   hb_p_jumptrue,
   hb_p_jumptruefar,
   hb_p_lessequal,
   hb_p_less,
   hb_p_line,
   hb_p_localname,
   hb_p_macropop,
   hb_p_macropopaliased,
   hb_p_macropush,
   hb_p_macroarraygen,
   hb_p_macropushlist,
   hb_p_macropushindex,
   hb_p_macropushpare,
   hb_p_macropushaliased,
   hb_p_macrosymbol,
   hb_p_macrotext,
   hb_p_message,
   hb_p_minus,
   hb_p_modulus,
   hb_p_modulename,
   /* start: pcodes generated by macro compiler */
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_dummy,
   /* end: */
   hb_p_mult,
   hb_p_negate,
   hb_p_noop,
   hb_p_not,
   hb_p_notequal,
   hb_p_or,
   hb_p_parameter,
   hb_p_plus,
   hb_p_pop,
   hb_p_popalias,
   hb_p_popaliasedfield,
   hb_p_popaliasedfieldnear,
   hb_p_popaliasedvar,
   hb_p_popfield,
   hb_p_poplocal,
   hb_p_poplocalnear,
   hb_p_popmemvar,
   hb_p_popstatic,
   hb_p_popvariable,
   hb_p_power,
   hb_p_pushalias,
   hb_p_pushaliasedfield,
   hb_p_pushaliasedfieldnear,
   hb_p_pushaliasedvar,
   hb_p_pushblock,
   hb_p_pushblockshort,
   hb_p_pushfield,
   hb_p_pushbyte,
   hb_p_pushint,
   hb_p_pushlocal,
   hb_p_pushlocalnear,
   hb_p_pushlocalref,
   hb_p_pushlong,
   hb_p_pushmemvar,
   hb_p_pushmemvarref,
   hb_p_pushnil,
   hb_p_pushdouble,
   hb_p_pushself,
   hb_p_pushstatic,
   hb_p_pushstaticref,
   hb_p_pushstr,
   hb_p_pushstrshort,
   hb_p_pushsym,
   hb_p_pushsymnear,
   hb_p_pushvariable,
   hb_p_retvalue,
   hb_p_send,
   hb_p_sendshort,
   hb_p_seqbegin,
   hb_p_seqend,
   hb_p_seqrecover,
   hb_p_sframe,
   hb_p_statics,
   hb_p_staticname,
   hb_p_swapalias,
   hb_p_true,
   hb_p_zero,
   hb_p_one,
   hb_p_macrofunc,
   hb_p_macrodo,
   /* start: more pcodes generated by macro compiler */
   hb_p_dummy,
   /* end: */
   hb_p_localnearaddint,
   hb_p_macropushref,
   hb_p_pushlonglong,
   hb_p_enumstart,
   hb_p_enumnext,
   hb_p_enumprev,
   hb_p_enumend,
   hb_p_switch,
   hb_p_pushdate,
   /* optimalization of inlined math operations (+=, -= */
   hb_p_pluseqpop,
   hb_p_minuseqpop,
   hb_p_multeqpop,
   hb_p_diveqpop,
   hb_p_pluseq,
   hb_p_minuseq,
   hb_p_multeq,
   hb_p_diveq,
   hb_p_withobjectstart,
   hb_p_withobjectmessage,
   hb_p_withobjectend,
   hb_p_macrosend,
   hb_p_pushovarref,
   hb_p_arraypushref,
   hb_p_vframe,
   hb_p_largeframe,
   hb_p_largevframe,
   hb_p_pushstrhidden,
   hb_p_localaddint,
   hb_p_modeqpop,
   hb_p_expeqpop,
   hb_p_modeq,
   hb_p_expeq,
   hb_p_duplunref,
   hb_p_dummy,
   hb_p_dummy,
   hb_p_pushblocklarge,
   hb_p_pushstrlarge,
   hb_p_swap,
   hb_p_pushvparams,
   hb_p_pushunref,
   hb_p_seqalways,
   hb_p_alwaysbegin,
   hb_p_alwaysend,
   hb_p_deceqpop,
   hb_p_inceqpop,
   hb_p_deceq,
   hb_p_inceq,
   hb_p_localdec,
   hb_p_localinc,
   hb_p_localincpush,
   hb_p_pushfuncsym,
   hb_p_hashgen,
   hb_p_seqblock,
   hb_p_threadstatics,
   hb_p_pushaparams
};

void hb_compGenCRealCode( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc )
{
   const HB_GENC_FUNC_PTR * pFuncTable = s_verbose_table;
   HB_LABEL_INFO label_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   label_info.yyc = yyc;
   label_info.fVerbose = ( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_VERBOSE );
   label_info.fSetSeqBegin = HB_FALSE;
   label_info.fCondJump = HB_FALSE;
   label_info.fEndRequest = HB_FALSE;
   label_info.iNestedBlock = 0;
   if( pFunc->nPCodePos == 0 )
      label_info.pnLabels = NULL;
   else
   {
      label_info.pnLabels = ( HB_SIZE * ) hb_xgrab( pFunc->nPCodePos * sizeof( HB_SIZE ) );
      memset( label_info.pnLabels, 0, pFunc->nPCodePos * sizeof( HB_SIZE ) );
      hb_compGenLabelTable( pFunc, &label_info );
   }

   fprintf( yyc, "{\n" );
   if( label_info.fCondJump )
      fprintf( yyc, "   HB_BOOL fValue;\n" );
   fprintf( yyc, "   do {\n" );

   hb_compPCodeEval( pFunc, ( const HB_PCODE_FUNC_PTR * ) pFuncTable, ( void * ) &label_info );

   fprintf( yyc, "   } while( 0 );\n" );
   if( label_info.fEndRequest )
      fprintf( yyc, "   hb_xvmExitProc();\n" );
   fprintf( yyc, "}\n" );

   if( label_info.pnLabels )
      hb_xfree( label_info.pnLabels );
}

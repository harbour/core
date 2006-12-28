/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler .NET .il source generation
 *
 * Copyright 2003 Antonio Linares <alinares@fivetechsoft.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

#include <assert.h>

#include "hbcomp.h"

static void hb_compGenCReadable( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );
static void hb_genNetFunctions( FILE * yyc );

/* helper structure to pass information */
typedef struct HB_stru_genc_info
{
   HB_COMP_DECL;
   FILE * yyc;
   BOOL bVerbose;
   USHORT iNestedCodeblock;
} HB_GENC_INFO, * HB_GENC_INFO_PTR;

typedef struct HB_stru_funcalls
{
   char * szName;
   BOOL bFirstParam;
   void * pNext;
} HB_FUNCALLS, * HB_FUNCALLS_PTR;

static HB_FUNCALLS_PTR pFunCalls = NULL;

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, HB_GENC_INFO_PTR )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

void hb_compGenILCode( HB_COMP_DECL, PHB_FNAME pFileName )  /* generates the IL output */
{
   char szFileName[ _POSIX_PATH_MAX + 1 ], * szVer;
   PFUNCTION    pFunc = HB_COMP_PARAM->functions.pFirst;
   PCOMSYMBOL   pSym = HB_COMP_PARAM->symbols.pFirst;
   PINLINE      pInline;
   PCOMDECLARED pDeclared;
   PCOMCLASS    pClass;
   FILE * yyc; /* file handle for IL output */
   BOOL bIsPublicFunction ;
   BOOL bIsInitFunction   ;
   BOOL bIsExitFunction   ;
   BOOL bIsStaticVariable ;
   BOOL bIsFirstFunction = TRUE;

   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( pSym );

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".il";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! HB_COMP_PARAM->fQuiet )
   {
      printf( "Generating IL source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   szVer = hb_verHarbour();
   fprintf( yyc, "// %s\n", szVer );
   hb_xfree( szVer );
   fprintf( yyc, "// Generated .NET IL source code\n\n" );

   if( HB_COMP_PARAM->iFunctionCnt )
   {
      fprintf( yyc, ".assembly extern mscorlib{}\n" );
      fprintf( yyc, ".assembly " );
      fprintf( yyc, hb_strupr( pFileName->szName ) );
      fprintf( yyc, "_PRG{}\n" );

      /* Generate functions data
       */
      pFunc = HB_COMP_PARAM->functions.pFirst;

      if( ! HB_COMP_PARAM->fStartProc )
         pFunc = pFunc->pNext; /* No implicit starting procedure */

      while( pFunc )
      {
         bIsInitFunction   = ( pFunc->cScope & HB_FS_INIT ) ;
         bIsExitFunction   = ( pFunc->cScope & HB_FS_EXIT ) ;
         bIsStaticVariable = ( pFunc == HB_COMP_PARAM->pInitFunc ) ;
         bIsPublicFunction = ( pFunc->cScope == HB_FS_PUBLIC ) ;

         /* Is it a PUBLIC FUNCTION/PROCEDURE */
         if ( bIsPublicFunction )
            fprintf( yyc, "\n.method public static void %s()", pFunc->szName );
         /* Is it STATICS$ */
         else if( bIsStaticVariable )
            fprintf( yyc, "static HARBOUR hb_INITSTATICS( void )" ); /* NOTE: hb_ intentionally in lower case */
         /* Is it an INIT FUNCTION/PROCEDURE */
         else if ( bIsInitFunction )
            fprintf( yyc, "HB_FUNC_INIT( %s )", pFunc->szName );
         /* Is it an EXIT FUNCTION/PROCEDURE */
         else if ( bIsExitFunction )
            fprintf( yyc, "HB_FUNC_EXIT( %s )", pFunc->szName );
         /* Then it must be a STATIC FUNCTION/PROCEDURE */
         else
            fprintf( yyc, "HB_FUNC_STATIC( %s )", pFunc->szName );

         fprintf( yyc, "\n{\n" );

         if( bIsFirstFunction )
         {
            fprintf( yyc, "  .entrypoint\n" );
            bIsFirstFunction = FALSE;
         }

         if( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_COMPACT )
            hb_compGenCCompact( pFunc, yyc );
         else
            hb_compGenCReadable( HB_COMP_PARAM, pFunc, yyc );

         fprintf( yyc, "   ret\n}\n" );

         pFunc = pFunc->pNext;
      }

      /* Generate codeblocks data
       */
      pInline = HB_COMP_PARAM->inlines.pFirst;
      while( pInline )
      {
         fprintf( yyc, "#line %i \"%s\"\n", pInline->iLine, pInline->szFileName );

         if( pInline->szName )
         {
            fprintf( yyc, "HB_FUNC_STATIC( %s )\n", pInline->szName );
         }
         fprintf( yyc, "%s", pInline->pCode );
         pInline = pInline->pNext;
      }
   }
   else
   {
      fprintf( yyc, "/* Empty source file */\n\n" );
   }

   /* Generate .NET support functions */
   hb_genNetFunctions( yyc );

   fclose( yyc );

   pFunc = HB_COMP_PARAM->functions.pFirst;
   while( pFunc )
      pFunc = hb_compFunctionKill( HB_COMP_PARAM, pFunc );

   pFunc = HB_COMP_PARAM->funcalls.pFirst;
   while( pFunc )
   {
      HB_COMP_PARAM->funcalls.pFirst = pFunc->pNext;
      hb_xfree( ( void * ) pFunc );  /* NOTE: szName will be released by hb_compSymbolKill() */
      pFunc = HB_COMP_PARAM->funcalls.pFirst;
   }

   if ( HB_COMP_PARAM->iWarnings >= 3 )
   {
      pDeclared = HB_COMP_PARAM->pReleaseDeclared->pNext;
      while( pDeclared )
      {
         HB_COMP_PARAM->pFirstDeclared = pDeclared->pNext;
         hb_xfree( ( void * ) pDeclared );
         pDeclared = HB_COMP_PARAM->pFirstDeclared;
      }

      pClass = HB_COMP_PARAM->pReleaseClass->pNext;
      while( pClass )
      {
         HB_COMP_PARAM->pFirstClass = pClass->pNext;

         pDeclared = pClass->pMethod;
         while ( pDeclared )
         {
            HB_COMP_PARAM->pFirstDeclared = pDeclared->pNext;
            hb_xfree( ( void * ) pDeclared );
            pDeclared = HB_COMP_PARAM->pFirstDeclared;
         }

         hb_xfree( ( void * ) pClass );
         pClass = HB_COMP_PARAM->pFirstClass;
      }
   }

   pSym = HB_COMP_PARAM->symbols.pFirst;
   while( pSym )
      pSym = hb_compSymbolKill( pSym );

   if( ! HB_COMP_PARAM->fQuiet )
      printf( "Done.\n" );
}

static HB_GENC_FUNC( hb_p_and )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_AND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraypush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call object ObjArrayPush( object, object )\n" );

   /* fprintf( cargo->yyc, "\tHB_P_ARRAYPUSH,\n" ); */

   return 1;
}

static HB_GENC_FUNC( hb_p_arraypop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ARRAYPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dec )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DEC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraydim )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYDIM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */", HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_divide )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DIVIDE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_do )
{
   fprintf( cargo->yyc, "\tHB_P_DO, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_doshort )
{
   HB_FUNCALLS_PTR pTemp = pFunCalls, pPrev = NULL;
   char * szFunName;

   HB_SYMBOL_UNUSED( pFunc );

   while( pTemp->pNext != NULL )
   {
      pPrev = pTemp;
      pTemp = ( HB_FUNCALLS_PTR ) pTemp->pNext;
   }

   szFunName = pTemp->szName;
   pTemp->bFirstParam = FALSE;
   hb_xfree( pTemp );

   if( pTemp == pFunCalls )
      pFunCalls = NULL;
   else if( pPrev != NULL )
      pPrev->pNext = NULL;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call object %s( object )\n", szFunName );
   fprintf( cargo->yyc, "            pop\n" );

   /* fprintf( cargo->yyc, "\tHB_P_DOSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] ); */

   return 2;
}

static HB_GENC_FUNC( hb_p_duplicate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLICATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dupltwo )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_DUPLTWO,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_equal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_EQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_exactlyequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call bool ObjExactlyEqual( object, object )\n" );

   /* fprintf( cargo->yyc, "\tHB_P_EXACTLYEQUAL,\n" ); */
   return 1;
}

static HB_GENC_FUNC( hb_p_endblock )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   --cargo->iNestedCodeblock;
   fprintf( cargo->yyc, "\tHB_P_ENDBLOCK,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_endproc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( lPCodePos );

   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldc.i4.0\n" );
   fprintf( cargo->yyc, "            box [mscorlib]System.Boolean\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call bool ObjForTest( object, object, object )\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   int i;

   fprintf( cargo->yyc, "  .locals init (" );

   for( i = 0; i < pFunc->pCode[ lPCodePos + 1 ]; i++ )
   {
      if( i > 0 )
         fprintf( cargo->yyc, "," );

      fprintf( cargo->yyc, " object V_%i", i );
   }

   fprintf( cargo->yyc, " )\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_funcptr )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCPTR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_function )
{
   fprintf( cargo->yyc, "\tHB_P_FUNCTION, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_functionshort )
{
   HB_FUNCALLS_PTR pTemp = pFunCalls, pPrev = NULL;
   char * szFunName;

   HB_SYMBOL_UNUSED( pFunc );

   while( pTemp->pNext != NULL )
   {
      pPrev = pTemp;
      pTemp = ( HB_FUNCALLS_PTR ) pTemp->pNext;
   }

   szFunName = pTemp->szName;
   pTemp->bFirstParam = FALSE;
   hb_xfree( pTemp );

   if( pTemp == pFunCalls )
      pFunCalls = NULL;
   else if( pPrev != NULL )
      pPrev->pNext = NULL;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call object %s( object )\n", szFunName );

   return 2;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   int i, n = HB_PCODE_MKUSHORT( &( pFunc->pCode[ lPCodePos + 1 ] ) );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call vararg object ObjArrayGen( ..." );

   for( i = 0; i < n; i++ )
      fprintf( cargo->yyc, ",object" );

   fprintf( cargo->yyc, ")\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_greater )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_greaterequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_GREATEREQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_inc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_instring )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_INSTRING,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_jumpnear )
{
   LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

   if( lOffset > 127 )
      lOffset -= 256;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "br.s" );
   fprintf( cargo->yyc, "  IL_%04lX\n", ( LONG ) ( lPCodePos + lOffset ) );

   return 2;
}

static HB_GENC_FUNC( hb_p_jump )
{
   LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256 );

   if( lOffset > SHRT_MAX )
      lOffset -= 65536;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "br.s" );
   fprintf( cargo->yyc, "  IL_%04lX\n", ( LONG ) ( lPCodePos + lOffset ) );

   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
      if( lOffset > 8388607L )
         lOffset -= 16777216;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumpfalsenear )
{
   LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

   if( lOffset > 127 )
      lOffset -= 256;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "brfalse.s" );
   fprintf( cargo->yyc, "  IL_%04lX\n", ( LONG ) ( lPCodePos + lOffset ) );

   return 2;
}

static HB_GENC_FUNC( hb_p_jumpfalse )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );

      if( lOffset > SHRT_MAX )
         lOffset -= 65536;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumpfalsefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
      if( lOffset > 8388607L )
         lOffset -= 16777216;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_jumptruenear )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUENEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );
      if( lOffset > 127 )
         lOffset -= 256;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_jumptrue )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
      if( lOffset > SHRT_MAX )
         lOffset -= 65536;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_jumptruefar )
{
   fprintf( cargo->yyc, "\tHB_P_JUMPTRUEFAR, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
      if( lOffset > 8388607L )
         lOffset -= 16777216;

      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_less )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_LESS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_lessequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call bool ObjLessEqual( object, object )\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   HB_SYMBOL_UNUSED( pFunc );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "nop  // HB_P_LINE\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_localname )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_LOCALNAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 3 );
   fprintf( cargo->yyc, "\n" );
   lPCodePos += 3;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return ( lPCodePos - ulStart + 1 );
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOP, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOPALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSH, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushref )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHREF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrodo )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACRODO, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macrofunc )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROFUNC, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macroarraygen )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROARRAYGEN, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_macropushlist )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHLIST, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushindex )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHINDEX,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropushpare )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHPARE, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHALIASED, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_macrosymbol )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROSYMBOL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macrotext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROTEXT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_message )
{
   fprintf( cargo->yyc, "\tHB_P_MESSAGE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_minus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MINUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_modulename )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_MODULENAME," );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 1 );
   fprintf( cargo->yyc, "\n" );
   lPCodePos++;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return ( lPCodePos - ulStart + 1 );
}

static HB_GENC_FUNC( hb_p_modulus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MODULUS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_mult )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MULT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_negate )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NEGATE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_not )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "call object ObjNot( object )\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_notequal )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOTEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_or )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_OR,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_parameter )
{
   fprintf( cargo->yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "   call object ObjAdd( object, object )\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_pop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_popaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocal )
{
   fprintf( cargo->yyc, "\tHB_P_POPLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_poplocalnear )
{
   /* important: check the below code for codeblock locals management */
   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   /* warning: IL requires zero based locals index */
   fprintf( cargo->yyc, "stloc.%i\n", pFunc->pCode[ lPCodePos + 1 ] - 1 );

   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   fprintf( cargo->yyc, "\tHB_P_POPSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   fprintf( cargo->yyc, "\tHB_P_POPVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_power )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_POWER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushaliasedfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushblock )
{
   USHORT wVar, w;
   ULONG ulStart = lPCodePos;

   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */",
               pFunc->pCode[ lPCodePos + 1 ] +
               pFunc->pCode[ lPCodePos + 2 ] * 256 );
   fprintf( cargo->yyc, "\n" );

   w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 3 ] ) );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* number of local parameters (%i) */", w );
   fprintf( cargo->yyc, "\n" );

   wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
   fprintf( cargo->yyc, "\t%i, %i,",
            pFunc->pCode[ lPCodePos + 5 ],
            pFunc->pCode[ lPCodePos + 6 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* number of local variables (%i) */", wVar );
   fprintf( cargo->yyc, "\n" );

   lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
   /* create the table of referenced local variables */
   while( wVar-- )
   {
      w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos ] ) );
      fprintf( cargo->yyc, "\t%i, %i,",
               pFunc->pCode[ lPCodePos ],
               pFunc->pCode[ lPCodePos + 1 ] );
      /* NOTE:
         * When a codeblock is used to initialize a static variable
         * the names of local variables cannot be determined
         * because at the time of C code generation we don't know
         * in which function was defined this local variable
         */
      if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) != ( HB_FS_INIT | HB_FS_EXIT ) )
         if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, w )->szName );
      fprintf( cargo->yyc, "\n" );
      lPCodePos +=2;
   }
   return ( lPCodePos - ulStart );
}

static HB_GENC_FUNC( hb_p_pushblockshort )
{
   ++cargo->iNestedCodeblock;

   fprintf( cargo->yyc, "\tHB_P_PUSHBLOCKSHORT, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */",
               pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushdouble )
{
   int i;

   fprintf( cargo->yyc, "\tHB_P_PUSHDOUBLE," );
   ++lPCodePos;
   for( i = 0; i < (int) ( sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) ); ++i )
      fprintf( cargo->yyc, " %i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %.*f, %d, %d */",
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
      *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
      *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) + sizeof( BYTE ) ] ) ) );
   fprintf( cargo->yyc, "\n" );

   return sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ) + 1;
}

static HB_GENC_FUNC( hb_p_pushfield )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   /* load constant numeric onto the stack */
   fprintf( cargo->yyc, "ldc.i4.s   %i\n", pFunc->pCode[ lPCodePos + 1 ] );
   /* turn the stack value into an object */
   fprintf( cargo->yyc, "            box [mscorlib]System.Int32\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushint )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHINT, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */",
            pFunc->pCode[ lPCodePos + 1 ] +
            pFunc->pCode[ lPCodePos + 2 ] * 256 );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocal )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlocalnear )
{
   /* Important: check the below code for codeblocks locals */
   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   /* Warning: IL uses zero based locals indexes */
   fprintf( cargo->yyc, "ldloc.%i\n", pFunc->pCode[ lPCodePos + 1 ] - 1 );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushlocalref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
      /* Variable with negative order are local variables
       * referenced in a codeblock -handle it with care
       */

      if( cargo->iNestedCodeblock )
      {
         /* we are accesing variables within a codeblock */
         /* the names of codeblock variable are lost     */
         if( wVar < 0 )
            fprintf( cargo->yyc, "\t/* localvar%i */", -wVar );
         else
            fprintf( cargo->yyc, "\t/* codeblockvar%i */", wVar );
      }
      else
         fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, wVar )->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushlong )
{
   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldc.i4 %li\n",
            *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "            box [mscorlib]System.Int32\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_pushlonglong )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "LONG LONG unsupported\n" );

   return 9;
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_FUNCALLS_PTR pTemp = pFunCalls;

   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( lPCodePos );

   if( pTemp )
   {
      while( pTemp->pNext )
         pTemp = ( HB_FUNCALLS_PTR ) pTemp->pNext;

      if( pTemp->bFirstParam )
         pTemp->bFirstParam = FALSE;
      else
      {
/*
         fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
         fprintf( cargo->yyc, "ldnull\n" );
*/
      }
   }
   else
   {
/*
      fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
      fprintf( cargo->yyc, "ldnull\n" );
*/
   }

   return 1;
}

static HB_GENC_FUNC( hb_p_pushself )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHSELF,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_pushstatic )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );

   return 3;
}

static HB_GENC_FUNC( hb_p_pushstr )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = pFunc->pCode[ lPCodePos + 1 ] +
                 pFunc->pCode[ lPCodePos + 2 ] * 256;

   fprintf( cargo->yyc, "\tHB_P_PUSHSTR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );

   if( cargo->bVerbose )
         fprintf( cargo->yyc, "\t/* %i */", wLen );

   lPCodePos += 3;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\n\t" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
          * NOTE: After optimization some CHR(n) can be converted
          *    into a string containing nonprintable characters.
          *
          * TODO: add switch to use hexadecimal format "%#04x"
          */
         if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else if( strchr( "\'\\\"", uchr ) )
            fprintf( cargo->yyc, "%i, ", uchr );
         else
            fprintf( cargo->yyc, "\'%c\', ", uchr );
      }
   }
   fprintf( cargo->yyc, "\n" );

   return ( lPCodePos - ulStart );
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = pFunc->pCode[ lPCodePos + 1 ];

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldstr " );

   lPCodePos += 2;
   if( wLen > 0 )
   {
      fprintf( cargo->yyc, "\"" );
      while( wLen-- )
      {
         BYTE uchr = ( BYTE ) pFunc->pCode[ lPCodePos++ ];
         /*
            * NOTE: After optimization some CHR(n) can be converted
            *    into a string containing nonprintable characters.
            *
            * TODO: add switch to use hexadecimal format "%#04x"
            */
         if( uchr > 0 )
         {
            if( ( uchr < ( BYTE ) ' ' ) || ( uchr >= 127 ) )
               fprintf( cargo->yyc, "%i, ", uchr );
            else if( strchr( "\'\\\"", uchr ) )
               fprintf( cargo->yyc, "%i, ", uchr );
            else
               fprintf( cargo->yyc, "%c", uchr );
         }
      }
   }
   fprintf( cargo->yyc, "\"\n" );

   return ( lPCodePos - ulStart );
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   HB_FUNCALLS_PTR pTemp = pFunCalls;

   if( pTemp != NULL )
   {
      while( pTemp->pNext != NULL )
         pTemp = ( HB_FUNCALLS_PTR ) pTemp->pNext;

      pTemp->pNext = ( HB_FUNCALLS_PTR ) hb_xgrab( sizeof( HB_FUNCALLS ) );
      pTemp = ( HB_FUNCALLS_PTR ) pTemp->pNext;
   }
   else
   {
      pFunCalls = ( HB_FUNCALLS_PTR ) hb_xgrab( sizeof( HB_FUNCALLS ) );
      pTemp = pFunCalls;
   }

   pTemp->szName = hb_compSymbolGetPos( cargo->HB_COMP_PARAM, pFunc->pCode[ lPCodePos + 1 ] )->szName;
   pTemp->bFirstParam = TRUE;
   pTemp->pNext  = NULL;

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "nop\n" );

   return 2;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "   // HB_P_RETVALUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_send )
{
   fprintf( cargo->yyc, "\tHB_P_SEND, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_sendshort )
{
   fprintf( cargo->yyc, "\tHB_P_SENDSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 2;
}

static HB_GENC_FUNC( hb_p_seqbegin )
{
   fprintf( cargo->yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqend )
{
   if( cargo->bVerbose ) fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   else fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "HB_P_SEQEND, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose )
   {
      LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
      fprintf( cargo->yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
   }
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_seqrecover )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SEQRECOVER,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_sframe )
{
   fprintf( cargo->yyc, "\tHB_P_SFRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS) */" );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_statics )
{
   fprintf( cargo->yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* symbol (_INITSTATICS), %i statics */", pFunc->pCode[ lPCodePos + 3 ] + pFunc->pCode[ lPCodePos + 4 ] * 256 );
   fprintf( cargo->yyc, "\n" );

   return 5;
}

static HB_GENC_FUNC( hb_p_staticname )
{
   ULONG ulStart = lPCodePos;

   fprintf( cargo->yyc, "\tHB_P_STATICNAME, %i, %i, %i,", 
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 4 );
   fprintf( cargo->yyc, "\n" );
   lPCodePos += 4;
   while( pFunc->pCode[ lPCodePos ] )
   {
      char chr = pFunc->pCode[ lPCodePos++ ];
      if( chr == '\'' || chr == '\\')
         fprintf( cargo->yyc, " \'\\%c\',", chr );
      else
         fprintf( cargo->yyc, " \'%c\',", chr );
   }
   fprintf( cargo->yyc, " 0,\n" );

   return ( lPCodePos - ulStart + 1 );
}

static HB_GENC_FUNC( hb_p_swapalias )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_SWAPALIAS,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_true )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldc.i4.1\n" );
   fprintf( cargo->yyc, "            box [mscorlib]System.Boolean\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldc.i4.1\n" );
   /* turn the stack value into an object */
   fprintf( cargo->yyc, "            box [mscorlib]System.Int32\n" );

   return 1;
}

static HB_GENC_FUNC( hb_p_zero )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ZERO,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_noop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_NOOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_dummy )
{
   HB_SYMBOL_UNUSED( cargo );
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumstart )
{
   fprintf( cargo->yyc, "\tHB_P_ENUMSTART, %i, %i,\n",
            pFunc->pCode[ lPCodePos + 1 ], pFunc->pCode[ lPCodePos + 2 ] );
   return 3;
}

static HB_GENC_FUNC( hb_p_enumnext )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMNEXT,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumprev )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMPREV,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_enumend )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );
   fprintf( cargo->yyc, "\tHB_P_ENUMEND,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_localnearaddint )
{
   fprintf( cargo->yyc, "  IL_%04lX:  ", lPCodePos );
   fprintf( cargo->yyc, "ldloc.%i\n", pFunc->pCode[ lPCodePos + 1 ] - 1 );
   fprintf( cargo->yyc, "            unbox [mscorlib]System.Int32\n" );
   fprintf( cargo->yyc, "            ldind.i4\n" );
   fprintf( cargo->yyc, "            ldc.i4.s %i\n", pFunc->pCode[ lPCodePos + 2 ] +
                                          (  pFunc->pCode[ lPCodePos + 3 ] * 256 ) );
   fprintf( cargo->yyc, "            add\n" );
   fprintf( cargo->yyc, "            box [mscorlib]System.Int32\n" );
   fprintf( cargo->yyc, "            stloc.%i\n", pFunc->pCode[ lPCodePos + 1 ] - 1 );

   return 4;
}

/* NOTE: The  order of functions have to match the order of opcodes
 *       mnemonics
 */
static HB_GENC_FUNC_PTR s_verbose_table[] = {
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
   hb_p_dupltwo,
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
   hb_p_enumend
};

static void hb_compGenCReadable( HB_COMP_DECL, PFUNCTION pFunc, FILE * yyc )
{
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   genc_info.HB_COMP_PARAM = HB_COMP_PARAM;
   genc_info.iNestedCodeblock = 0;
   genc_info.bVerbose = ( HB_COMP_PARAM->iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_verbose_table, ( void * ) &genc_info );
}

static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc )
{
   ULONG lPCodePos = 0;
   int nChar;

   fprintf( yyc, "\t" );

   nChar = 0;

   while( lPCodePos < pFunc->lPCodePos )
   {
      ++nChar;

      if( nChar > 1 )
         fprintf( yyc, ", " );

      if( nChar == 15 )
      {
         fprintf( yyc, "\n\t" );
         nChar = 1;
      }

      /* Displaying as decimal is more compact than hex */
      fprintf( yyc, "%d", ( int ) pFunc->pCode[ lPCodePos++ ] );
   }

   if( nChar != 0)
      fprintf( yyc, "\n" );
}

static void hb_genNetFunctions( FILE * yyc )
{
   int i;

/*
   // generated IL code for C# source code:
   // public static object ObjAdd( object a, object b )
   // {
   //    if( a.GetType() == typeof( int ) && b.GetType() == typeof( int ) )
   //       return ( int ) a + ( int ) b;
   //
   //    if( a.GetType() == typeof( string ) && b.GetType() == typeof( string ) )
   //       return a.ToString() + b.ToString();
   //
   //    return null;
   // }
   // VERY IMPORTANT: As ObjAdd() is a public method, not specific to a Class,
   // then arguments have to be decreased, as on a normal method, argument 0 is Self.
*/

   char * ObjAdd[] = {
"\n.method public static object ObjAdd(object a, object b)",
"{",
"  .maxstack  2",
"  .locals init (object V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0006:  ldtoken    [mscorlib]System.Int32",
"  IL_000b:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0010:  bne.un.s   IL_003b",
"  IL_0012:  ldarg.1",
"  IL_0013:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0018:  ldtoken    [mscorlib]System.Int32",
"  IL_001d:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0022:  bne.un.s   IL_003b",
"  IL_0024:  ldarg.0",
"  IL_0025:  unbox      [mscorlib]System.Int32",
"  IL_002a:  ldind.i4",
"  IL_002b:  ldarg.1",
"  IL_002c:  unbox      [mscorlib]System.Int32",
"  IL_0031:  ldind.i4",
"  IL_0032:  add",
"  IL_0033:  box        [mscorlib]System.Int32",
"  IL_0038:  stloc.0",
"  IL_0039:  br.s       IL_0077",
"  IL_003b:  ldarg.0",
"  IL_003c:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0041:  ldtoken    [mscorlib]System.String",
"  IL_0046:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_004b:  bne.un.s   IL_0073",
"  IL_004d:  ldarg.1",
"  IL_004e:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0053:  ldtoken    [mscorlib]System.String",
"  IL_0058:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_005d:  bne.un.s   IL_0073",
"  IL_005f:  ldarg.0",
"  IL_0060:  callvirt   instance string [mscorlib]System.Object::ToString()",
"  IL_0065:  ldarg.1",
"  IL_0066:  callvirt   instance string [mscorlib]System.Object::ToString()",
"  IL_006b:  call       string [mscorlib]System.String::Concat(string,string)",
"  IL_0070:  stloc.0",
"  IL_0071:  br.s       IL_0077",
"  IL_0073:  ldnull",
"  IL_0074:  stloc.0",
"  IL_0075:  br.s       IL_0077",
"  IL_0077:  ldloc.0",
"  IL_0078:  ret",
"}", 0 };

/*
   // public static object ObjArrayGen( __arglist )
   // {
   //    ArrayList a    = new ArrayList();
   //    ArgIterator ai = new ArgIterator( __arglist );
   //
   //    while( ai.GetRemainingCount() > 0 )
   //       a.Add( __refvalue( ai.GetNextArg(), object ) );
   //
   //    return a;
   // }
*/
   char * ObjArrayGen[] = {
"\n.method public static vararg object ObjArrayGen()",
"{",
"  .maxstack  2",
"  .locals init (class [mscorlib]System.Collections.ArrayList V_0,",
"                valuetype [mscorlib]System.ArgIterator V_1,",
"                object V_2)",
"  IL_0000:  newobj     instance void [mscorlib]System.Collections.ArrayList::.ctor()",
"  IL_0005:  stloc.0",
"  IL_0006:  ldloca.s   V_1",
"  IL_0008:  arglist",
"  IL_000a:  call       instance void [mscorlib]System.ArgIterator::.ctor(valuetype [mscorlib]System.RuntimeArgumentHandle)",
"  IL_000f:  br.s       IL_0025",
"  IL_0011:  ldloc.0",
"  IL_0012:  ldloca.s   V_1",
"  IL_0014:  call       instance typedref [mscorlib]System.ArgIterator::GetNextArg()",
"  IL_0019:  refanyval  [mscorlib]System.Object",
"  IL_001e:  ldind.ref",
"  IL_001f:  callvirt   instance int32 [mscorlib]System.Collections.ArrayList::Add(object)",
"  IL_0024:  pop",
"  IL_0025:  ldloca.s   V_1",
"  IL_0027:  call       instance int32 [mscorlib]System.ArgIterator::GetRemainingCount()",
"  IL_002c:  ldc.i4.0",
"  IL_002d:  bgt.s      IL_0011",
"  IL_002f:  ldloc.0",
"  IL_0030:  stloc.2",
"  IL_0031:  br.s       IL_0033",
"  IL_0033:  ldloc.2",
"  IL_0034:  ret",
"}", 0 };

/*
   // public static object ObjArrayPush( object array, object index )
   // {
   //    return ( ( ArrayList ) array )[ ( ( int ) index ) - 1 ];
   // }
*/

   char * ObjArrayPush[] = {
"\n.method public static object ObjArrayPush(object 'array', object index)",
"{",
"  .maxstack  3",
"  .locals init (object V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  castclass  [mscorlib]System.Collections.ArrayList",
"  IL_0006:  ldarg.1",
"  IL_0007:  unbox      [mscorlib]System.Int32",
"  IL_000c:  ldind.i4",
"  IL_000d:  ldc.i4.1",
"  IL_000e:  sub",
"  IL_000f:  callvirt   instance object [mscorlib]System.Collections.ArrayList::get_Item(int32)",
"  IL_0014:  stloc.0",
"  IL_0015:  br.s       IL_0017",
"  IL_0017:  ldloc.0",
"  IL_0018:  ret",
"}", 0 };

/*
   // public static bool ObjLessEqual( object a, object b )
   // {
   //    if( a.GetType() == typeof( int ) && b.GetType() == typeof( int ) )
   //       return ( int ) a <= ( int ) b;
   //
   //    return false;
   // }
*/

   char * ObjLessEqual[] = {
"\n.method public static bool ObjLessEqual( object a, object b )",
"{",
"  .maxstack  2",
"  .locals init (bool V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0006:  ldtoken    [mscorlib]System.Int32",
"  IL_000b:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0010:  bne.un.s   IL_003a",
"  IL_0012:  ldarg.1",
"  IL_0013:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0018:  ldtoken    [mscorlib]System.Int32",
"  IL_001d:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0022:  bne.un.s   IL_003a",
"  IL_0024:  ldarg.0",
"  IL_0025:  unbox      [mscorlib]System.Int32",
"  IL_002a:  ldind.i4",
"  IL_002b:  ldarg.1",
"  IL_002c:  unbox      [mscorlib]System.Int32",
"  IL_0031:  ldind.i4",
"  IL_0032:  cgt",
"  IL_0034:  ldc.i4.0",
"  IL_0035:  ceq",
"  IL_0037:  stloc.0",
"  IL_0038:  br.s       IL_003e",
"  IL_003a:  ldc.i4.0",
"  IL_003b:  stloc.0",
"  IL_003c:  br.s       IL_003e",
"  IL_003e:  ldloc.0",
"  IL_003f:  ret",
"}", 0 };

/*
   // public static bool ObjForTest( object current, object end, object step )
   // {
   //    if( ( int ) step >= 0 )
   //       return ( int ) current <= ( int ) end;
   //    else
   //       return ( int ) current >= ( int ) end;
   // }
*/

   char * ObjForTest[] = {
"\n.method public static bool ObjForTest(object current,object end,object step)",
"{",
"  .maxstack  2",
"  .locals init (bool V_0)",
"  IL_0000:  ldarg.2",
"  IL_0001:  unbox      [mscorlib]System.Int32",
"  IL_0006:  ldind.i4",
"  IL_0007:  ldc.i4.0",
"  IL_0008:  blt.s      IL_0020",
"  IL_000a:  ldarg.0",
"  IL_000b:  unbox      [mscorlib]System.Int32",
"  IL_0010:  ldind.i4",
"  IL_0011:  ldarg.1",
"  IL_0012:  unbox      [mscorlib]System.Int32",
"  IL_0017:  ldind.i4",
"  IL_0018:  cgt",
"  IL_001a:  ldc.i4.0",
"  IL_001b:  ceq",
"  IL_001d:  stloc.0",
"  IL_001e:  br.s       IL_0036",
"  IL_0020:  ldarg.0",
"  IL_0021:  unbox      [mscorlib]System.Int32",
"  IL_0026:  ldind.i4",
"  IL_0027:  ldarg.1",
"  IL_0028:  unbox      [mscorlib]System.Int32",
"  IL_002d:  ldind.i4",
"  IL_002e:  clt",
"  IL_0030:  ldc.i4.0",
"  IL_0031:  ceq",
"  IL_0033:  stloc.0",
"  IL_0034:  br.s       IL_0036",
"  IL_0036:  ldloc.0",
"  IL_0037:  ret",
"}", 0 };

/*
   // public static bool ObjExactlyEqual( object a, object b )
   // {
   //    if( a.GetType() == typeof( int ) && b.GetType() == typeof( int ) )
   //       return ( int ) a == ( int ) b;
   //
   //    if( a.GetType() == typeof( string ) && b.GetType() == typeof( string ) )
   //       return a.ToString() == b.ToString();
   //
   //    return false;
   // }
*/

   char * ObjExactlyEqual[] = {
"\n.method public static bool ObjExactlyEqual(object a, object b)",
"{",
"  .maxstack  2",
"  .locals init (bool V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0006:  ldtoken    [mscorlib]System.Int32",
"  IL_000b:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0010:  bne.un.s   IL_0037",
"  IL_0012:  ldarg.1",
"  IL_0013:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0018:  ldtoken    [mscorlib]System.Int32",
"  IL_001d:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0022:  bne.un.s   IL_0037",
"  IL_0024:  ldarg.0",
"  IL_0025:  unbox      [mscorlib]System.Int32",
"  IL_002a:  ldind.i4",
"  IL_002b:  ldarg.1",
"  IL_002c:  unbox      [mscorlib]System.Int32",
"  IL_0031:  ldind.i4",
"  IL_0032:  ceq",
"  IL_0034:  stloc.0",
"  IL_0035:  br.s       IL_0073",
"  IL_0037:  ldarg.0",
"  IL_0038:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_003d:  ldtoken    [mscorlib]System.String",
"  IL_0042:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0047:  bne.un.s   IL_006f",
"  IL_0049:  ldarg.1",
"  IL_004a:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_004f:  ldtoken    [mscorlib]System.String",
"  IL_0054:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0059:  bne.un.s   IL_006f",
"  IL_005b:  ldarg.0",
"  IL_005c:  callvirt   instance string [mscorlib]System.Object::ToString()",
"  IL_0061:  ldarg.1",
"  IL_0062:  callvirt   instance string [mscorlib]System.Object::ToString()",
"  IL_0067:  call       bool [mscorlib]System.String::op_Equality(string,string)",
"  IL_006c:  stloc.0",
"  IL_006d:  br.s       IL_0073",
"  IL_006f:  ldc.i4.0",
"  IL_0070:  stloc.0",
"  IL_0071:  br.s       IL_0073",
"  IL_0073:  ldloc.0",
"  IL_0074:  ret",
"}", 0 };

/*
   // public static object ObjNot( object o )
   // {
   //    return ! ( bool ) o;
   // }
*/

   char * ObjNot[] = {
"\n.method public static object ObjNot(object o)",
"{",
"  .maxstack  2",
"  .locals init (object V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  unbox      [mscorlib]System.Boolean",
"  IL_0006:  ldind.i1",
"  IL_0007:  ldc.i4.0",
"  IL_0008:  ceq",
"  IL_000a:  box        [mscorlib]System.Boolean",
"  IL_000f:  stloc.0",
"  IL_0010:  br.s       IL_0012",
"  IL_0012:  ldloc.0",
"  IL_0013:  ret",
"}", 0 };

/*
   // public static object LEN( object o )
   // {
   //    return ( ( ArrayList ) o ).Count;
   // }
*/

   char * LEN[] = {
"\n.method public static object LEN(object o)",
"{",
"  .maxstack  2",
"  .locals init (object V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  castclass  [mscorlib]System.Collections.ArrayList",
"  IL_0006:  callvirt   instance int32 [mscorlib]System.Collections.ArrayList::get_Count()",
"  IL_000b:  box        [mscorlib]System.Int32",
"  IL_0010:  stloc.0",
"  IL_0011:  br.s       IL_0013",
"  IL_0013:  ldloc.0",
"  IL_0014:  ret",
"}", 0 };

/*
   // public static object QOUT( object o )
   // {
   //    if( o == null )
   //       Console.WriteLine( "NIL" );
   //
   //    if( o.GetType() == typeof( bool ) )
   //       Console.WriteLine( ( bool ) o ? ".T.": ".F." );
   //
   //    else
   //       Console.WriteLine( o );
   //
   //    return null;
   // }
*/

   char * QOUT[] = {
"\n.method public static object QOUT(object o)",
"{",
"  .maxstack  2",
"  .locals init (object V_0)",
"  IL_0000:  ldarg.0",
"  IL_0001:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0006:  ldtoken    [mscorlib]System.Boolean",
"  IL_000b:  call       class [mscorlib]System.Type [mscorlib]System.Type::GetTypeFromHandle(valuetype [mscorlib]System.RuntimeTypeHandle)",
"  IL_0010:  bne.un.s   IL_002e",
"  IL_0012:  ldarg.0",
"  IL_0013:  unbox      [mscorlib]System.Boolean",
"  IL_0018:  ldind.i1",
"  IL_0019:  brtrue.s   IL_0022",
"  IL_001b:  ldstr      \".F.\"",
"  IL_0020:  br.s       IL_0027",
"  IL_0022:  ldstr      \".T.\"",
"  IL_0027:  call       void [mscorlib]System.Console::WriteLine(string)",
"  IL_002c:  br.s       IL_0048",
"  IL_002e:  ldarg.0",
"  IL_002f:  callvirt   instance class [mscorlib]System.Type [mscorlib]System.Object::GetType()",
"  IL_0034:  brtrue.s   IL_0042",
"  IL_0036:  ldstr      \"nil\"",
"  IL_003b:  call       void [mscorlib]System.Console::WriteLine(string)",
"  IL_0040:  br.s       IL_0048",
"  IL_0042:  ldarg.0",
"  IL_0043:  call       void [mscorlib]System.Console::WriteLine(object)",
"  IL_0048:  ldnull",
"  IL_0049:  stloc.0",
"  IL_004a:  br.s       IL_004c",
"  IL_004c:  ldloc.0",
"  IL_004d:  ret",
"}", 0 };

   i = 0;
   while( ObjAdd[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjAdd[ i++ ] );

   i = 0;
   while( ObjArrayGen[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjArrayGen[ i++ ] );

   i = 0;
   while( ObjArrayPush[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjArrayPush[ i++ ] );

   i = 0;
   while( ObjLessEqual[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjLessEqual[ i++ ] );

   i = 0;
   while( ObjForTest[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjForTest[ i++ ] );

   i = 0;
   while( ObjExactlyEqual[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjExactlyEqual[ i++ ] );

   i = 0;
   while( ObjNot[ i ] != 0 )
      fprintf( yyc, "%s\n", ObjNot[ i++ ] );

   i = 0;
   while( LEN[ i ] != 0 )
      fprintf( yyc, "%s\n", LEN[ i++ ] );

   i = 0;
   while( QOUT[ i ] != 0 )
      fprintf( yyc, "%s\n", QOUT[ i++ ] );
}

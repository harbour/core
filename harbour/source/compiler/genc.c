/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

#include <assert.h>

#include "hbcomp.h"

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc );
static void hb_compGenCCompact( PFUNCTION pFunc, FILE * yyc );

/* helper structure to pass information */
typedef struct HB_stru_genc_info
{
   FILE * yyc;
   BOOL bVerbose;
   USHORT iNestedCodeblock;
} HB_GENC_INFO, * HB_GENC_INFO_PTR;

#define HB_GENC_FUNC( func ) HB_PCODE_FUNC( func, HB_GENC_INFO_PTR )
typedef HB_GENC_FUNC( HB_GENC_FUNC_ );
typedef HB_GENC_FUNC_ * HB_GENC_FUNC_PTR;

void hb_compGenCCode( PHB_FNAME pFileName )       /* generates the C language output */
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc = hb_comp_functions.pFirst, pFTemp;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   PCOMDECLARED pDeclared;
   FILE * yyc; /* file handle for C output */
   PINLINE pInline = hb_comp_inlines.pFirst;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating C source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   fprintf( yyc, "/*\n * Harbour Compiler, %d.%d%s (Build %d) (%04d.%02d.%02d) (%s)\n",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY, HB_VER_LEX );
   fprintf( yyc, " * Generated C source code\n */\n\n" );

   if( hb_comp_iFunctionCnt )
   {
      fprintf( yyc, "#include \"hbvmpub.h\"\n" );
      if( hb_comp_iGenCOutput != HB_COMPGENC_COMPACT )
         fprintf( yyc, "#include \"hbpcode.h\"\n" );
      fprintf( yyc, "#include \"hbinit.h\"\n\n\n" );

      if( ! hb_comp_bStartProc )
         pFunc = pFunc->pNext; /* No implicit starting procedure */

      /* write functions prototypes for PRG defined functions */
      while( pFunc )
      {
         if( pFunc->cScope & HB_FS_STATIC || pFunc->cScope & HB_FS_INIT || pFunc->cScope & HB_FS_EXIT )
            fprintf( yyc, "static " );
         else
            fprintf( yyc, "       " );

         if( pFunc == hb_comp_pInitFunc )
            fprintf( yyc, "HARBOUR hb_INITSTATICS( void );\n" ); /* NOTE: hb_ intentionally in lower case */
         else
            fprintf( yyc, "HB_FUNC( %s );\n", pFunc->szName );
         pFunc = pFunc->pNext;
      }

      /* write functions prototypes for inline blocks */
      while( pInline )
      {
         fprintf( yyc, "static HB_FUNC( %s );\n", pInline->szName );
         pInline = pInline->pNext;
      }

      /* write functions prototypes for called functions outside this PRG */
      pFunc = hb_comp_funcalls.pFirst;
      while( pFunc )
      {
         pFTemp = hb_compFunctionFind( pFunc->szName );
         if( ! pFTemp || pFTemp == hb_comp_functions.pFirst )
         {
            if( pFTemp == NULL && hb_compInlineFind( pFunc->szName ) == NULL )
            {
               fprintf( yyc, "extern HB_FUNC( %s );\n", pFunc->szName );
            }
         }

         pFunc = pFunc->pNext;
      }

      /* writes the symbol table */
      /* Generate the wrapper that will initialize local symbol table
       */
      hb_strupr( pFileName->szName );
      fprintf( yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", hb_comp_szPrefix, pFileName->szName );

      while( pSym )
      {
         if( pSym->szName[ 0 ] == '(' )
         {
            /* Since the normal function cannot be INIT and EXIT at the same time
            * we are using these two bits to mark the special function used to
            * initialize static variables
            */
            fprintf( yyc, "{ \"(_INITSTATICS)\", HB_FS_INIT | HB_FS_EXIT, hb_INITSTATICS, NULL }" ); /* NOTE: hb_ intentionally in lower case */
         }
         else
         {
            fprintf( yyc, "{ \"%s\", ", pSym->szName );

            if( pSym->cScope & HB_FS_STATIC )
               fprintf( yyc, "HB_FS_STATIC" );

            else if( pSym->cScope & HB_FS_INIT )
               fprintf( yyc, "HB_FS_INIT" );

            else if( pSym->cScope & HB_FS_EXIT )
               fprintf( yyc, "HB_FS_EXIT" );

            else
               fprintf( yyc, "HB_FS_PUBLIC" );

            if( pSym->cScope & VS_MEMVAR )
               fprintf( yyc, " | HB_FS_MEMVAR" );

            if( ( pSym->cScope != HB_FS_MESSAGE ) && ( pSym->cScope & HB_FS_MESSAGE ) ) /* only for non public symbols */
               fprintf( yyc, " | HB_FS_MESSAGE" );

            if( pSym->cScope & HB_FS_FIRST )
               fprintf( yyc, " | HB_FS_FIRST" );

            /* specify the function address if it is a defined function or an
               external called function */
            if( hb_compFunctionFind( pSym->szName ) ) /* is it a function defined in this module */
               fprintf( yyc, ", HB_FUNCNAME( %s ), NULL }", pSym->szName );
            else if( hb_compFunCallFind( pSym->szName ) ) /* is it a function called from this module */
               fprintf( yyc, ", HB_FUNCNAME( %s ), NULL }", pSym->szName );
            else
               fprintf( yyc, ", NULL, NULL }" );   /* memvar */
         }

         if( pSym != hb_comp_symbols.pLast )
            fprintf( yyc, ",\n" );

         pSym = pSym->pNext;
      }

      fprintf( yyc, "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n"
                    "#if defined(_MSC_VER)\n"
                    "   #if _MSC_VER >= 1010\n"
                    /* [pt] First version of MSC I have that supports this */
                    /* is msvc4.1 (which is msc 10.10) */
                    "      #pragma data_seg( \".CRT$XIY\" )\n"
                    "      #pragma comment( linker, \"/Merge:.CRT=.data\" )\n"
                    "   #else\n"
                    "      #pragma data_seg( \"XIY\" )\n"
                    "   #endif\n"
                    "   static HB_$INITSYM hb_vm_auto_SymbolInit_%s%s = hb_vm_SymbolInit_%s%s;\n"
                    "   #pragma data_seg()\n"
                    "#elif ! defined(__GNUC__)\n"
                    "   #pragma startup hb_vm_SymbolInit_%s%s\n"
                    "#endif\n\n",
                    hb_comp_szPrefix, pFileName->szName,
                    hb_comp_szPrefix, pFileName->szName,
                    hb_comp_szPrefix, pFileName->szName,
                    hb_comp_szPrefix, pFileName->szName );

      /* Generate functions data
       */
      pFunc = hb_comp_functions.pFirst;

      if( ! hb_comp_bStartProc )
         pFunc = pFunc->pNext; /* No implicit starting procedure */

      while( pFunc )
      {
         if( pFunc->cScope != HB_FS_PUBLIC )
            fprintf( yyc, "static " );

         if( pFunc == hb_comp_pInitFunc )        /* Is it STATICS$ */
            fprintf( yyc, "HARBOUR hb_INITSTATICS( void )" ); /* NOTE: hb_ intentionally in lower case */
         else
            fprintf( yyc, "HB_FUNC( %s )", pFunc->szName );

         fprintf( yyc, "\n{\n   static const BYTE pcode[] =\n   {\n" );

         if( hb_comp_iGenCOutput == HB_COMPGENC_COMPACT )
            hb_compGenCCompact( pFunc, yyc );
         else
            hb_compGenCReadable( pFunc, yyc );

         fprintf( yyc, "   };\n\n" );
         fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n}\n\n" );
         pFunc = pFunc->pNext;
      }

      /* Generate codeblocks data
       */
      if( hb_comp_inlines.iCount )
      {
         fprintf( yyc, "#include \"hbapi.h\"\n" );
         pInline = hb_comp_inlines.pFirst;
         while( pInline )
         {
            fprintf( yyc, "#line %i \"%s\"\n", pInline->iLine, pInline->szFileName );
            fprintf( yyc, "static HB_FUNC( %s )\n", pInline->szName );
            fprintf( yyc, "%s", pInline->pCode );
            pInline = pInline->pNext;
         }
      }
   }
   else
      fprintf( yyc, "/* Empty source file */\n\n" );

   fclose( yyc );

   pFunc = hb_comp_functions.pFirst;
   while( pFunc )
      pFunc = hb_compFunctionKill( pFunc );

   pFunc = hb_comp_funcalls.pFirst;
   while( pFunc )
   {
      hb_comp_funcalls.pFirst = pFunc->pNext;
      hb_xfree( ( void * ) pFunc );  /* NOTE: szName will be released by hb_compSymbolKill() */
      pFunc = hb_comp_funcalls.pFirst;
   }

   pInline = hb_comp_inlines.pFirst;
   while( pInline )
   {
      hb_comp_inlines.pFirst = pInline->pNext;
      hb_xfree( ( void * ) pInline->pCode );
      hb_xfree( ( void * ) pInline->szFileName );
      hb_xfree( ( void * ) pInline );  /* NOTE: szName will be released by hb_compSymbolKill() */
      pInline = hb_comp_inlines.pFirst;
   }

   pDeclared = hb_comp_pFirstDeclared;
   while( pDeclared )
   {
      hb_comp_pFirstDeclared = pDeclared->pNext;
      hb_xfree( ( void * ) pDeclared );
      pDeclared = hb_comp_pFirstDeclared;
   }

   pSym = hb_comp_symbols.pFirst;
   while( pSym )
      pSym = hb_compSymbolKill( pSym );

   if( ! hb_comp_bQuiet )
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

   fprintf( cargo->yyc, "\tHB_P_ARRAYPUSH,\n" );
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */", pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
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
   fprintf( cargo->yyc, "\tHB_P_DOSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
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

   fprintf( cargo->yyc, "\tHB_P_EXACTLYEQUAL,\n" );
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
   if( (lPCodePos+1) == pFunc->lPCodePos )
      fprintf( cargo->yyc, "\tHB_P_ENDPROC\n" );
   else
      fprintf( cargo->yyc, "\tHB_P_ENDPROC,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_false )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FALSE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_fortest )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FORTEST,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_frame )
{
   fprintf( cargo->yyc, "\tHB_P_FRAME, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* locals, params */" );
   fprintf( cargo->yyc, "\n" );
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
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_FUNCTIONSHORT, %i,\n", pFunc->pCode[ lPCodePos + 1 ] );
   return 1;
}

static HB_GENC_FUNC( hb_p_arraygen )
{
   fprintf( cargo->yyc, "\tHB_P_ARRAYGEN, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */", pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
   fprintf( cargo->yyc, "\n" );
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
   fprintf( cargo->yyc, "\tHB_P_JUMPNEAR, %i,",
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

static HB_GENC_FUNC( hb_p_jump )
{
   fprintf( cargo->yyc, "\tHB_P_JUMP, %i, %i,",
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
   fprintf( cargo->yyc, "\tHB_P_JUMPFALSENEAR, %i,",
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

   fprintf( cargo->yyc, "\tHB_P_LESSEQUAL,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_line )
{
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "/* %05li */ ", lPCodePos );
   else
      fprintf( cargo->yyc, "\t" );
   fprintf( cargo->yyc, "HB_P_LINE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %i */", pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
   fprintf( cargo->yyc, "\n" );
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

   return lPCodePos - ulStart + 1;
}

static HB_GENC_FUNC( hb_p_macropop )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOP,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropopaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPOPALIASED,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropush )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSH,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_macropushaliased )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_MACROPUSHALIASED,\n" );
   return 1;
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
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

   return lPCodePos - ulStart + 1;
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

   fprintf( cargo->yyc, "\tHB_P_NOT,\n" );
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 4;
}

static HB_GENC_FUNC( hb_p_plus )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PLUS,\n" );
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_popaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popfield )
{
   fprintf( cargo->yyc, "\tHB_P_POPFIELD, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
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
   fprintf( cargo->yyc, "\tHB_P_POPLOCALNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      char wVar = ( char ) pFunc->pCode[ lPCodePos + 1 ];
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
   return 2;
}

static HB_GENC_FUNC( hb_p_popmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_POPMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popstatic )
{
   fprintf( cargo->yyc, "\tHB_P_POPSTATIC, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;
      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );

      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_popvariable )
{
   fprintf( cargo->yyc, "\tHB_P_POPVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName  );
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushaliasedfieldnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDFIELDNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushaliasedvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
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
         * the the names of local variables cannot be determined
         * because at the time of C code generation we don't know
         * in which function was defined this local variable
         */
      if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) != ( HB_FS_INIT | HB_FS_EXIT ) )
         if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compLocalVariableFind( pFunc, w )->szName );
      fprintf( cargo->yyc, "\n" );
      lPCodePos +=2;
   }
   return lPCodePos - ulStart;
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
   for( i = 0; i < sizeof( double ) + sizeof( BYTE ) + sizeof( BYTE ); ++i )
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
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushbyte )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHBYTE, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %i */",
            pFunc->pCode[ lPCodePos + 1 ] );
   fprintf( cargo->yyc, "\n" );
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
   fprintf( cargo->yyc, "\tHB_P_PUSHLOCALNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
   {
      signed char wVar = ( signed char ) pFunc->pCode[ lPCodePos + 1 ];
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
   fprintf( cargo->yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ],
            pFunc->pCode[ lPCodePos + 3 ],
            pFunc->pCode[ lPCodePos + 4 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %li */", *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
   fprintf( cargo->yyc, "\n" );

   return 1 + sizeof( long );
}

static HB_GENC_FUNC( hb_p_pushmemvar )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushmemvarref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushnil )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_PUSHNIL,\n" );
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
   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;
      USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;
      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushstaticref )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
   {
      PVAR pVar;
      PFUNCTION pTmp = hb_comp_functions.pFirst;

      USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
      while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
         pTmp = pTmp->pNext;
      pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
      fprintf( cargo->yyc, "\t/* %s */", pVar->szName );
   }
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

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushstrshort )
{
   ULONG ulStart = lPCodePos;
   USHORT wLen = pFunc->pCode[ lPCodePos + 1 ];

   fprintf( cargo->yyc, "\tHB_P_PUSHSTRSHORT, %i,", pFunc->pCode[ lPCodePos + 1 ] );

   if( cargo->bVerbose )
         fprintf( cargo->yyc, "\t/* %i */", wLen );

   lPCodePos += 2;
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

   return lPCodePos - ulStart;
}

static HB_GENC_FUNC( hb_p_pushsym )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYM, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_pushsymnear )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHSYMNEAR, %i,",
            pFunc->pCode[ lPCodePos + 1 ] );
   if( cargo->bVerbose )
      fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] )->szName );
   fprintf( cargo->yyc, "\n" );
   return 2;
}

static HB_GENC_FUNC( hb_p_pushvariable )
{
   fprintf( cargo->yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
            pFunc->pCode[ lPCodePos + 1 ],
            pFunc->pCode[ lPCodePos + 2 ] );
   if( cargo->bVerbose ) fprintf( cargo->yyc, "\t/* %s */", hb_compSymbolGetPos( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 )->szName );
   fprintf( cargo->yyc, "\n" );
   return 3;
}

static HB_GENC_FUNC( hb_p_retvalue )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_RETVALUE,\n" );
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

   fprintf( cargo->yyc, "\tHB_P_TRUE,\n" );
   return 1;
}

static HB_GENC_FUNC( hb_p_one )
{
   HB_SYMBOL_UNUSED( pFunc );
   HB_SYMBOL_UNUSED( lPCodePos );

   fprintf( cargo->yyc, "\tHB_P_ONE,\n" );
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
   hb_p_swapalias,
   hb_p_true,
   hb_p_zero,
   hb_p_one
};

static void hb_compGenCReadable( PFUNCTION pFunc, FILE * yyc )
{
   HB_GENC_INFO genc_info;

   /* Make sure that table is correct */
   assert( HB_P_LAST_PCODE == sizeof( s_verbose_table ) / sizeof( HB_GENC_FUNC_PTR ) );

   genc_info.iNestedCodeblock = 0;
   genc_info.bVerbose = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );
   genc_info.yyc = yyc;

   hb_compPCodeEval( pFunc, ( HB_PCODE_FUNC_PTR * ) s_verbose_table, ( void * ) &genc_info );

   if( genc_info.bVerbose )
      fprintf( yyc, "/* %05li */\n", pFunc->lPCodePos );
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


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
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
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

static void hb_compGenCReadable( PFUNCTION pFunc );
static void hb_compGenCCompact( PFUNCTION pFunc );
static void hb_fputc( BYTE b );

static FILE * s_yyc; /* file handle for C output */
static int s_nChar;

void hb_compGenCCode( PHB_FNAME pFileName )       /* generates the C language output */
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc = hb_comp_functions.pFirst, pFTemp;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".c";
   hb_fsFNameMerge( szFileName, pFileName );

   s_yyc = fopen( szFileName, "wb" );
   if( ! s_yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating C source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   fprintf( s_yyc, "/*\n * Harbour Compiler, %d.%d%s (Build %d) (%04d.%02d.%02d)\n",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY );
   fprintf( s_yyc, " * Generated C source code\n */\n\n" );

   fprintf( s_yyc, "#include \"hbvmpub.h\"\n" );
   fprintf( s_yyc, "#include \"hbpcode.h\"\n" );
   fprintf( s_yyc, "#include \"hbinit.h\"\n\n\n" );

   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   /* write functions prototypes for PRG defined functions */
   while( pFunc )
   {
      if( pFunc->cScope & HB_FS_STATIC || pFunc->cScope & HB_FS_INIT || pFunc->cScope & HB_FS_EXIT )
         fprintf( s_yyc, "static " );
      else
         fprintf( s_yyc, "       " );

      if( pFunc == hb_comp_pInitFunc )
         fprintf( s_yyc, "HARBOUR hb_INITSTATICS( void );\n" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( s_yyc, "HB_FUNC( %s );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }
   /* write functions prototypes for called functions outside this PRG */
   pFunc = hb_comp_funcalls.pFirst;
   while( pFunc )
   {
      pFTemp = hb_compFunctionFind( pFunc->szName );
      if( ! pFTemp || pFTemp == hb_comp_functions.pFirst )
         fprintf( s_yyc, "extern HB_FUNC( %s );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }

   /* writes the symbol table */
   /* Generate the wrapper that will initialize local symbol table
    */
   hb_strupr( pFileName->szName );
   fprintf( s_yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", hb_comp_szPrefix, pFileName->szName );

   while( pSym )
   {
      if( pSym->szName[ 0 ] == '(' )
      {
         /* Since the normal function cannot be INIT and EXIT at the same time
         * we are using these two bits to mark the special function used to
         * initialize static variables
         */
         fprintf( s_yyc, "{ \"(_INITSTATICS)\", HB_FS_INIT | HB_FS_EXIT, hb_INITSTATICS, NULL }" ); /* NOTE: hb_ intentionally in lower case */
      }
      else
      {
         fprintf( s_yyc, "{ \"%s\", ", pSym->szName );

         if( pSym->cScope & HB_FS_STATIC )
            fprintf( s_yyc, "HB_FS_STATIC" );

         else if( pSym->cScope & HB_FS_INIT )
            fprintf( s_yyc, "HB_FS_INIT" );

         else if( pSym->cScope & HB_FS_EXIT )
            fprintf( s_yyc, "HB_FS_EXIT" );

         else
            fprintf( s_yyc, "HB_FS_PUBLIC" );

         if( pSym->cScope & VS_MEMVAR )
            fprintf( s_yyc, " | HB_FS_MEMVAR" );

         if( ( pSym->cScope != HB_FS_MESSAGE ) && ( pSym->cScope & HB_FS_MESSAGE ) ) /* only for non public symbols */
            fprintf( s_yyc, " | HB_FS_MESSAGE" );

         /* specify the function address if it is a defined function or an
            external called function */
         if( hb_compFunctionFind( pSym->szName ) ) /* is it a function defined in this module */
            fprintf( s_yyc, ", HB_FUNCNAME( %s ), NULL }", pSym->szName );
         else if( hb_compFunCallFind( pSym->szName ) ) /* is it a function called from this module */
            fprintf( s_yyc, ", HB_FUNCNAME( %s ), NULL }", pSym->szName );
         else
            fprintf( s_yyc, ", NULL, NULL }" );   /* memvar */
      }

      if( pSym != hb_comp_symbols.pLast )
         fprintf( s_yyc, ",\n" );

      pSym = pSym->pNext;
   }

   fprintf( s_yyc, "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n"
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
         fprintf( s_yyc, "static " );

      if( pFunc == hb_comp_pInitFunc )        /* Is it STATICS$ */
         fprintf( s_yyc, "HARBOUR hb_INITSTATICS( void )" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( s_yyc, "HB_FUNC( %s )", pFunc->szName );

      fprintf( s_yyc, "\n{\n   static BYTE pcode[] =\n   {\n" );

      if( hb_comp_iGenCOutput == HB_COMPGENC_COMPACT )
         hb_compGenCCompact( pFunc );
      else
         hb_compGenCReadable( pFunc );

      fprintf( s_yyc, "   };\n\n" );
      fprintf( s_yyc, "   hb_vmExecute( pcode, symbols );\n}\n\n" );
      pFunc = pFunc->pNext;
   }

   fclose( s_yyc );

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

   pSym = hb_comp_symbols.pFirst;
   while( pSym )
      pSym = hb_compSymbolKill( pSym );

   if( ! hb_comp_bQuiet )
      printf( "Done.\n" );
}

static void hb_compGenCReadable( PFUNCTION pFunc )
{
   USHORT iNestedCodeblock = 0;
   ULONG lPCodePos = 0;
   BOOL bVerbose = ( hb_comp_iGenCOutput == HB_COMPGENC_VERBOSE );

   while( lPCodePos < pFunc->lPCodePos )
   {
      switch( pFunc->pCode[ lPCodePos ] )
      {
         case HB_P_AND:
            fprintf( s_yyc, "\tHB_P_AND,\n" );
            lPCodePos++;
            break;

         case HB_P_ARRAYPUSH:
            fprintf( s_yyc, "\tHB_P_ARRAYPUSH,\n" );
            lPCodePos++;
            break;

         case HB_P_ARRAYPOP:
            fprintf( s_yyc, "\tHB_P_ARRAYPOP,\n" );
            lPCodePos++;
            break;

         case HB_P_DEC:
            fprintf( s_yyc, "\tHB_P_DEC,\n" );
            lPCodePos++;
            break;

         case HB_P_ARRAYDIM:
            {
               USHORT w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_ARRAYDIM, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %i */", w );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_DIVIDE:
            fprintf( s_yyc, "\tHB_P_DIVIDE,\n" );
            lPCodePos++;
            break;

         case HB_P_DO:
            fprintf( s_yyc, "\tHB_P_DO, %i, %i,\n",
                     pFunc->pCode[ lPCodePos + 1 ],
                     pFunc->pCode[ lPCodePos + 2 ] );
            lPCodePos += 3;
            break;

         case HB_P_DUPLICATE:
            fprintf( s_yyc, "\tHB_P_DUPLICATE,\n" );
            lPCodePos++;
            break;

         case HB_P_DUPLTWO:
            fprintf( s_yyc, "\tHB_P_DUPLTWO,\n" );
            lPCodePos++;
            break;

         case HB_P_EQUAL:
            fprintf( s_yyc, "\tHB_P_EQUAL,\n" );
            lPCodePos++;
            break;

         case HB_P_EXACTLYEQUAL:
            fprintf( s_yyc, "\tHB_P_EXACTLYEQUAL,\n" );
            lPCodePos++;
            break;

         case HB_P_ENDBLOCK:
            --iNestedCodeblock;
            fprintf( s_yyc, "\tHB_P_ENDBLOCK,\n" );
            lPCodePos++;
            break;

         case HB_P_ENDPROC:
            lPCodePos++;
            if( lPCodePos == pFunc->lPCodePos )
               fprintf( s_yyc, "\tHB_P_ENDPROC\n" );
            else
               fprintf( s_yyc, "\tHB_P_ENDPROC,\n" );
            break;

         case HB_P_FALSE:
            fprintf( s_yyc, "\tHB_P_FALSE,\n" );
            lPCodePos++;
            break;

         case HB_P_FORTEST:          /* ER For tests. Step > 0 LESS */
                                     /* Step < 0 GREATER */
            fprintf( s_yyc, "\tHB_P_FORTEST,\n" );
            lPCodePos++;
            break;

         case HB_P_FRAME:
            {
               PVAR pLocal  = pFunc->pLocals;
               BYTE bLocals = 0;

               while( pLocal )
               {
                  pLocal = pLocal->pNext;
                  bLocals++;
               }

               if( bLocals || pFunc->wParamCount )
               {
                  fprintf( s_yyc, "\tHB_P_FRAME, %i, %i,",
                           bLocals - pFunc->wParamCount,
                           pFunc->wParamCount );
                  if( bVerbose ) fprintf( s_yyc, "\t/* locals, params */" );
                  fprintf( s_yyc, "\n" );
               }
            }
            lPCodePos += 3;
            break;

         case HB_P_FUNCPTR:
            fprintf( s_yyc, "\tHB_P_FUNCPTR,\n" );
            lPCodePos++;
            break;

         case HB_P_FUNCTION:
            fprintf( s_yyc, "\tHB_P_FUNCTION, %i, %i,\n",
                     pFunc->pCode[ lPCodePos + 1 ],
                     pFunc->pCode[ lPCodePos + 2 ] );
            lPCodePos += 3;
            break;

         case HB_P_ARRAYGEN:
            {
               USHORT w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_ARRAYGEN, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %i */", w );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_GREATER:
            fprintf( s_yyc, "\tHB_P_GREATER,\n" );
            lPCodePos++;
            break;

         case HB_P_GREATEREQUAL:
            fprintf( s_yyc, "\tHB_P_GREATEREQUAL,\n" );
            lPCodePos++;
            break;

         case HB_P_INC:
            fprintf( s_yyc, "\tHB_P_INC,\n" );
            lPCodePos++;
            break;

         case HB_P_INSTRING:
            fprintf( s_yyc, "\tHB_P_INSTRING,\n" );
            lPCodePos++;
            break;

         case HB_P_JUMPSHORT:
         /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

               if ( lOffset > 127 )
                  lOffset -= 256;

               fprintf( s_yyc, "\tHB_P_JUMPSHORT, %i,",
                         pFunc->pCode[ lPCodePos + 1 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 2;
            break;

         case HB_P_JUMP:
         /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );

               if ( lOffset > SHRT_MAX )
                  lOffset -= 65536;

               fprintf( s_yyc, "\tHB_P_JUMP, %i, %i,",
                         pFunc->pCode[ lPCodePos + 1 ],
                         pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 3;
            break;

         case HB_P_JUMPFAR:
         /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
               if ( lOffset > 8388607L )
                  lOffset -= 16777216;

               fprintf( s_yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
                         pFunc->pCode[ lPCodePos + 1 ],
                         pFunc->pCode[ lPCodePos + 2 ],
                         pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 4;
            break;

         case HB_P_JUMPSHORTFALSE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );

               if ( lOffset > 127 )
                  lOffset -= 256;
               
               fprintf( s_yyc, "\tHB_P_JUMPSHORTFALSE, %i,",
                        pFunc->pCode[ lPCodePos + 1 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 2;
            break;

         case HB_P_JUMPFALSE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
            
               if ( lOffset > SHRT_MAX )
                  lOffset -= 65536;
            
               fprintf( s_yyc, "\tHB_P_JUMPFALSE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 3;
            break;

         case HB_P_JUMPFARFALSE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
               if ( lOffset > 8388607L )
                  lOffset -= 16777216;

               fprintf( s_yyc, "\tHB_P_JUMPFARFALSE, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 4;
            break;

         case HB_P_JUMPSHORTTRUE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] );
               if ( lOffset > 127 )
                  lOffset -= 256;
               
               fprintf( s_yyc, "\tHB_P_JUMPSHORTTRUE, %i,",
                        pFunc->pCode[ lPCodePos + 1 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 2;
            break;

         case HB_P_JUMPTRUE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 );
               if ( lOffset > SHRT_MAX )
                  lOffset -= 65536;
               
               fprintf( s_yyc, "\tHB_P_JUMPTRUE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 3;
            break;

         case HB_P_JUMPFARTRUE:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
               if ( lOffset > 8388607L )
                  lOffset -= 16777216;

               fprintf( s_yyc, "\tHB_P_JUMPFARTRUE, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, ( LONG ) ( lPCodePos + lOffset ) );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 4;
            break;

         case HB_P_LESS:
            fprintf( s_yyc, "\tHB_P_LESS,\n" );
            lPCodePos++;
            break;

         case HB_P_LESSEQUAL:
            fprintf( s_yyc, "\tHB_P_LESSEQUAL,\n" );
            lPCodePos++;
            break;

         case HB_P_LINE:
            {
               USHORT w;
               if( bVerbose ) fprintf( s_yyc, "/* %05li */ ", lPCodePos );
               else fprintf( s_yyc, "\t" );
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "HB_P_LINE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %i */", w );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_LOCALNAME:
            fprintf( s_yyc, "\tHB_P_LOCALNAME, %i, %i,",
                     pFunc->pCode[ lPCodePos + 1 ],
                     pFunc->pCode[ lPCodePos + 2 ] );
            if( bVerbose ) fprintf( s_yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 3 );
            fprintf( s_yyc, "\n" );
            lPCodePos += 3;
            while( pFunc->pCode[ lPCodePos ] )
            {
               char chr = pFunc->pCode[ lPCodePos++ ];
               if( chr == '\'' || chr == '\\')
                  fprintf( s_yyc, " \'\\%c\',", chr );
               else
                  fprintf( s_yyc, " \'%c\',", chr );
            }
            fprintf( s_yyc, " 0,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROPOP:
            fprintf( s_yyc, "\tHB_P_MACROPOP,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROPOPALIASED:
            fprintf( s_yyc, "\tHB_P_MACROPOPALIASED,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROPUSH:
            fprintf( s_yyc, "\tHB_P_MACROPUSH,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROPUSHALIASED:
            fprintf( s_yyc, "\tHB_P_MACROPUSHALIASED,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROSYMBOL:
            fprintf( s_yyc, "\tHB_P_MACROSYMBOL,\n" );
            lPCodePos++;
            break;

         case HB_P_MACROTEXT:
            fprintf( s_yyc, "\tHB_P_MACROTEXT,\n" );
            lPCodePos++;
            break;

         case HB_P_MESSAGE:
            {
               USHORT wSym = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_MESSAGE, %i, %i,",
                        HB_LOBYTE( wSym ),
                        HB_HIBYTE( wSym ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wSym )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_MINUS:
            fprintf( s_yyc, "\tHB_P_MINUS,\n" );
            lPCodePos++;
            break;

         case HB_P_MODULENAME:
            fprintf( s_yyc, "\tHB_P_MODULENAME," );
            if( bVerbose )
               fprintf( s_yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 1 );
            fprintf( s_yyc, "\n" );
            lPCodePos++;
            while( pFunc->pCode[ lPCodePos ] )
            {
               char chr = pFunc->pCode[ lPCodePos++ ];
               if( chr == '\'' || chr == '\\')
                  fprintf( s_yyc, " \'\\%c\',", chr );
               else
                  fprintf( s_yyc, " \'%c\',", chr );
            }
            fprintf( s_yyc, " 0,\n" );
            lPCodePos++;
            break;

         case HB_P_MODULUS:
            fprintf( s_yyc, "\tHB_P_MODULUS,\n" );
            lPCodePos++;
            break;

         case HB_P_MULT:
            fprintf( s_yyc, "\tHB_P_MULT,\n" );
            lPCodePos++;
            break;

         case HB_P_NEGATE:
            fprintf( s_yyc, "\tHB_P_NEGATE,\n" );
            lPCodePos++;
            break;

         case HB_P_NOT:
            fprintf( s_yyc, "\tHB_P_NOT,\n" );
            lPCodePos++;
            break;

         case HB_P_NOTEQUAL:
            fprintf( s_yyc, "\tHB_P_NOTEQUAL,\n" );
            lPCodePos++;
            break;

         case HB_P_OR:
            fprintf( s_yyc, "\tHB_P_OR,\n" );
            lPCodePos++;
            break;

         case HB_P_PARAMETER:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ),
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 4;
            }
            break;

         case HB_P_PLUS:
            fprintf( s_yyc, "\tHB_P_PLUS,\n" );
            lPCodePos++;
            break;

         case HB_P_POP:
            fprintf( s_yyc, "\tHB_P_POP,\n" );
            lPCodePos++;
            break;

         case HB_P_POPALIAS:
            fprintf( s_yyc, "\tHB_P_POPALIAS,\n" );
            lPCodePos++;
            break;

         case HB_P_POPALIASEDFIELD:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POPALIASEDVAR:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POPFIELD:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_POPFIELD, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POPLOCAL:
            {
               SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
               /* Variable with negative order are local variables
                * referenced in a codeblock -handle it with care
                */
               if( iNestedCodeblock )
               {
                  /* we are accesing variables within a codeblock */
                  /* the names of codeblock variable are lost     */
                  if( wVar < 0 )
                  {
                     fprintf( s_yyc, "\tHB_P_POPLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* localvar%i */", -wVar );
                     fprintf( s_yyc, "\n" );
                  }
                  else
                  {
                     fprintf( s_yyc, "\tHB_P_POPLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* codeblockvar%i */", wVar );
                     fprintf( s_yyc, "\n" );
                  }
               }
               else
               {
                  fprintf( s_yyc, "\tHB_P_POPLOCAL, %i, %i,",
                           pFunc->pCode[ lPCodePos + 1 ],
                           pFunc->pCode[ lPCodePos + 2 ] );
                  if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                  fprintf( s_yyc, "\n" );
               }
               lPCodePos += 3;
            }
            break;

         case HB_P_POPMEMVAR:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_POPMEMVAR, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POPSTATIC:
            {
               PVAR pVar;
               PFUNCTION pTmp = hb_comp_functions.pFirst;

               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                   pTmp = pTmp->pNext;
               pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
               fprintf( s_yyc, "\tHB_P_POPSTATIC, %i, %i,",
                         pFunc->pCode[ lPCodePos + 1 ],
                         pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", pVar->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POPVARIABLE:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_POPVARIABLE, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName  );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_POWER:
            fprintf( s_yyc, "\tHB_P_POWER,\n" );
            lPCodePos++;
            break;

         case HB_P_PUSHALIAS:
            fprintf( s_yyc, "\tHB_P_PUSHALIAS,\n" );
            lPCodePos++;
            break;

         case HB_P_PUSHALIASEDFIELD:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHALIASEDVAR:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHBLOCK:
            {
               USHORT wVar, w;

               ++iNestedCodeblock;
               
               fprintf( s_yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %i */",
                        pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256 );
               fprintf( s_yyc, "\n" );
               
               w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 3 ] ) );
               fprintf( s_yyc, "\t%i, %i,",
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* number of local parameters (%i) */", w );
               fprintf( s_yyc, "\n" );
               
               wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
               fprintf( s_yyc, "\t%i, %i,",
                        pFunc->pCode[ lPCodePos + 5 ],
                        pFunc->pCode[ lPCodePos + 6 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* number of local variables (%i) */", wVar );
               fprintf( s_yyc, "\n" );
               
               lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
               /* create the table of referenced local variables */
               while( wVar-- )
               {
                  w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos ] ) );
                  fprintf( s_yyc, "\t%i, %i,",
                           pFunc->pCode[ lPCodePos ],
                           pFunc->pCode[ lPCodePos + 1 ] );
                  /* NOTE:
                   * When a codeblock is used to initialize a static variable
                   * the the names of local variables cannot be determined
                   * because at the time of C code generation we don't know
                   * in which function was defined this local variable
                   */
                  if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) != ( HB_FS_INIT | HB_FS_EXIT ) )
                     if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, w )->szName );
                  fprintf( s_yyc, "\n" );
                  lPCodePos +=2;
               }
            }
            break;

         case HB_P_PUSHDOUBLE:
            {
               int i;
               ++lPCodePos;
               fprintf( s_yyc, "\tHB_P_PUSHDOUBLE, " );
               for( i = 0; i < sizeof( double ) + sizeof( BYTE ); ++i )
                  fprintf( s_yyc, "%i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %.*f, %d */",
                  *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
                  *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
                  *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ) );
               fprintf( s_yyc, "\n" );
               lPCodePos += sizeof( double ) + sizeof( BYTE );
            }
            break;

         case HB_P_PUSHFIELD:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHFIELD, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHINT:
            fprintf( s_yyc, "\tHB_P_PUSHINT, %i, %i,",
                     pFunc->pCode[ lPCodePos + 1 ],
                     pFunc->pCode[ lPCodePos + 2 ] );
            if( bVerbose ) fprintf( s_yyc, "\t/* %i */",
                     pFunc->pCode[ lPCodePos + 1 ] +
                     pFunc->pCode[ lPCodePos + 2 ] * 256 );
            fprintf( s_yyc, "\n" );
            lPCodePos += 3;
            break;

         case HB_P_PUSHLOCAL:
            {
               SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
               /* Variable with negative order are local variables
                * referenced in a codeblock -handle it with care
                */
               if( iNestedCodeblock )
               {
                  /* we are accesing variables within a codeblock */
                  /* the names of codeblock variable are lost     */
                  if( wVar < 0 )
                  {
                     fprintf( s_yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* localvar%i */", -wVar );
                     fprintf( s_yyc, "\n" );
                  }
                  else
                  {
                     fprintf( s_yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* codeblockvar%i */", wVar );
                     fprintf( s_yyc, "\n" );
                  }
               }
               else
               {
                  fprintf( s_yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                           pFunc->pCode[ lPCodePos + 1 ],
                           pFunc->pCode[ lPCodePos + 2 ] );
                  if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                  fprintf( s_yyc, "\n" );
               }
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHLOCALREF:
            {
               SHORT wVar = * ( ( SHORT * ) &( pFunc->pCode )[ lPCodePos + 1 ] );
               /* Variable with negative order are local variables
                * referenced in a codeblock -handle it with care
                */
               if( iNestedCodeblock )
               {
                  /* we are accesing variables within a codeblock */
                  /* the names of codeblock variable are lost     */
                  if( wVar < 0 )
                  {
                     fprintf( s_yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* localvar%i */", -wVar );
                     fprintf( s_yyc, "\n" );
                  }
                  else
                  {
                     fprintf( s_yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( bVerbose ) fprintf( s_yyc, "\t/* codeblockvar%i */", wVar );
                     fprintf( s_yyc, "\n" );
                  }
               }
               else
               {
                  fprintf( s_yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                           pFunc->pCode[ lPCodePos + 1 ],
                           pFunc->pCode[ lPCodePos + 2 ] );
                  if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                  fprintf( s_yyc, "\n" );
               }
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHLONG:
            fprintf( s_yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i,",
                     pFunc->pCode[ lPCodePos + 1 ],
                     pFunc->pCode[ lPCodePos + 2 ],
                     pFunc->pCode[ lPCodePos + 3 ],
                     pFunc->pCode[ lPCodePos + 4 ] );
            if( bVerbose ) fprintf( s_yyc, "\t/* %li */", *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
            fprintf( s_yyc, "\n" );
            lPCodePos += ( 1 + sizeof( long ) );
            break;

         case HB_P_PUSHMEMVAR:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHMEMVARREF:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHNIL:
            fprintf( s_yyc, "\tHB_P_PUSHNIL,\n" );
            lPCodePos++;
            break;

         case HB_P_PUSHSELF:
            fprintf( s_yyc, "\tHB_P_PUSHSELF,\n" );
            lPCodePos++;
            break;

         case HB_P_PUSHSTATIC:
            {
               PVAR pVar;
               PFUNCTION pTmp = hb_comp_functions.pFirst;

               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                   pTmp = pTmp->pNext;
               pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
               fprintf( s_yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
                         pFunc->pCode[ lPCodePos + 1 ],
                         pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", pVar->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHSTATICREF:
            {
               PVAR pVar;
               PFUNCTION pTmp = hb_comp_functions.pFirst;

               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                   pTmp = pTmp->pNext;
               pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
               fprintf( s_yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
                         pFunc->pCode[ lPCodePos + 1 ],
                         pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", pVar->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHSTR:
            {
               USHORT wLen = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHSTR, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %i */", wLen );
               lPCodePos +=3;
               if( wLen > 0 )
               {
                  unsigned char uchr;
               
                  fprintf( s_yyc, "\n\t" );
                  while( wLen-- )
                  {
                     uchr = ( unsigned char ) ( pFunc->pCode[ lPCodePos++ ] );
                     /*
                      * NOTE: After optimization some CHR(n) can be converted
                      *    into a string containing nonprintable characters.
                      *
                      * TODO: add switch to use hexadecimal format "%#04x"
                      */
                     if( ( uchr < (unsigned char) ' ' ) || ( uchr >= 127 ) )
                        fprintf( s_yyc, "%i, ", uchr );
                     else if( strchr( "\'\\\"", uchr ) )
                        fprintf( s_yyc, "%i, ", uchr );
                     else
                        fprintf( s_yyc, "\'%c\', ", uchr );
                  }
               }
               fprintf( s_yyc, "\n" );
            }
            break;

         case HB_P_PUSHSYM:
            {
               USHORT wSym = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHSYM, %i, %i,",
                        HB_LOBYTE( wSym ),
                        HB_HIBYTE( wSym ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wSym )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_PUSHVARIABLE:
            {
               USHORT wVar = pFunc->pCode[ lPCodePos + 1 ] +
                             pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( s_yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
                        HB_LOBYTE( wVar ),
                        HB_HIBYTE( wVar ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
               fprintf( s_yyc, "\n" );
               lPCodePos += 3;
            }
            break;

         case HB_P_RETVALUE:
            fprintf( s_yyc, "\tHB_P_RETVALUE,\n" );
            lPCodePos++;
            break;

         case HB_P_SEQBEGIN:
            {
               LONG lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
               fprintf( s_yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
               fprintf( s_yyc, "\n" );
               lPCodePos += 4;
            }
            break;

         case HB_P_SEQEND:
            {
               LONG lOffset;
               if( bVerbose ) fprintf( s_yyc, "/* %05li */ ", lPCodePos );
               else fprintf( s_yyc, "\t" );
               lOffset = ( LONG ) ( pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536 );
               fprintf( s_yyc, "HB_P_SEQEND, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* %li (abs: %05li) */", lOffset, lPCodePos + lOffset );
               fprintf( s_yyc, "\n" );
               lPCodePos += 4;
            }
            break;

         case HB_P_SEQRECOVER:
            fprintf( s_yyc, "\tHB_P_SEQRECOVER,\n" );
            lPCodePos++;
            break;

         case HB_P_SFRAME:
            /* we only generate it if there are statics used in this function */
            if( pFunc->bFlags & FUN_USES_STATICS )
            {
               USHORT w;
               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               fprintf( s_yyc, "\tHB_P_SFRAME, %i, %i,",
                        HB_LOBYTE( w ), HB_HIBYTE( w ) );
               if( bVerbose ) fprintf( s_yyc, "\t/* symbol (_INITSTATICS) */" );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 3;
            break;

         case HB_P_STATICS:
            {
               USHORT w;
               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               fprintf( s_yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
                        HB_LOBYTE( w ),
                        HB_HIBYTE( w ),
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ] );
               if( bVerbose ) fprintf( s_yyc, "\t/* symbol (_INITSTATICS), %i statics */", pFunc->pCode[ lPCodePos + 3 ] + pFunc->pCode[ lPCodePos + 4 ] * 256 );
               fprintf( s_yyc, "\n" );
            }
            lPCodePos += 5;
            break;

         case HB_P_SWAPALIAS:
            fprintf( s_yyc, "\tHB_P_SWAPALIAS,\n" );
            lPCodePos++;
            break;

         case HB_P_TRUE:
            fprintf( s_yyc, "\tHB_P_TRUE,\n" );
            lPCodePos++;
            break;

         case HB_P_ZERO:
            fprintf( s_yyc, "\tHB_P_ZERO,\n" );
            lPCodePos++;
            break;

         case HB_P_NOOP:
            fprintf( s_yyc, "\tHB_P_NOOP,\n" );
            lPCodePos++;
            break;

         default:
            fprintf( s_yyc, "\t%u, /* Incorrect pcode value: %u */\n", pFunc->pCode[ lPCodePos ], pFunc->pCode[ lPCodePos ] );
            printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
            lPCodePos = pFunc->lPCodePos;
            break;
      }
   }

   if( bVerbose )
      fprintf( s_yyc, "/* %05li */\n", lPCodePos );
}

static void hb_compGenCCompact( PFUNCTION pFunc )
{
   ULONG lPCodePos = 0;

   fprintf( s_yyc, "\t" );

   s_nChar = 0;

   while( lPCodePos < pFunc->lPCodePos )
   {
      switch( pFunc->pCode[ lPCodePos ] )
      {
         case HB_P_AND:
         case HB_P_ARRAYPUSH:
         case HB_P_ARRAYPOP:
         case HB_P_DEC:
         case HB_P_DIVIDE:
         case HB_P_DUPLICATE:
         case HB_P_DUPLTWO:
         case HB_P_ENDBLOCK:
         case HB_P_ENDPROC:
         case HB_P_EQUAL:
         case HB_P_EXACTLYEQUAL:
         case HB_P_FALSE:
         case HB_P_FORTEST:
         case HB_P_FUNCPTR:
         case HB_P_GREATER:
         case HB_P_GREATEREQUAL:
         case HB_P_INC:
         case HB_P_INSTRING:
         case HB_P_LESS:
         case HB_P_LESSEQUAL:
         case HB_P_MACROPOP:
         case HB_P_MACROPOPALIASED:
         case HB_P_MACROPUSH:
         case HB_P_MACROPUSHALIASED:
         case HB_P_MACROSYMBOL:
         case HB_P_MACROTEXT:
         case HB_P_MINUS:
         case HB_P_MODULUS:
         case HB_P_MULT:
         case HB_P_NEGATE:
         case HB_P_NOT:
         case HB_P_NOTEQUAL:
         case HB_P_OR:
         case HB_P_PLUS:
         case HB_P_POP:
         case HB_P_POPALIAS:
         case HB_P_POWER:
         case HB_P_PUSHALIAS:
         case HB_P_PUSHNIL:
         case HB_P_PUSHSELF:
         case HB_P_RETVALUE:
         case HB_P_SWAPALIAS:
         case HB_P_SEQRECOVER:
         case HB_P_TRUE:
         case HB_P_ZERO:
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            break;

         case HB_P_JUMPSHORT:
         case HB_P_JUMPSHORTFALSE:
         case HB_P_JUMPSHORTTRUE:
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            break;

         case HB_P_ARRAYDIM:
         case HB_P_DO:
         case HB_P_FUNCTION:
         case HB_P_ARRAYGEN:
         case HB_P_JUMP:
         case HB_P_JUMPFALSE:
         case HB_P_JUMPTRUE:
         case HB_P_LINE:
         case HB_P_POPLOCAL:
         case HB_P_POPSTATIC:
         case HB_P_PUSHINT:
         case HB_P_PUSHLOCAL:
         case HB_P_PUSHLOCALREF:
         case HB_P_PUSHSTATIC:
         case HB_P_PUSHSTATICREF:
         case HB_P_PUSHSYM:
         case HB_P_MESSAGE:
         case HB_P_POPMEMVAR:
         case HB_P_PUSHMEMVAR:
         case HB_P_PUSHMEMVARREF:
         case HB_P_POPVARIABLE:
         case HB_P_PUSHVARIABLE:
         case HB_P_POPFIELD:
         case HB_P_PUSHFIELD:
         case HB_P_POPALIASEDFIELD:
         case HB_P_PUSHALIASEDFIELD:
         case HB_P_POPALIASEDVAR:
         case HB_P_PUSHALIASEDVAR:
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            break;

         case HB_P_JUMPFAR:
         case HB_P_JUMPFARFALSE:
         case HB_P_JUMPFARTRUE:
         case HB_P_PARAMETER:
         case HB_P_SEQBEGIN:
         case HB_P_SEQEND:
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            break;

         case HB_P_FRAME:
            /* update the number of local variables */
            {
               PVAR pLocal  = pFunc->pLocals;
               BYTE bLocals = 0;

               while( pLocal )
               {
                  pLocal = pLocal->pNext;
                  bLocals++;
               }

               if( bLocals || pFunc->wParamCount )
               {
                  hb_fputc( pFunc->pCode[ lPCodePos++ ] );
                  hb_fputc( ( BYTE )( bLocals - pFunc->wParamCount ) );
                  hb_fputc( ( BYTE )( pFunc->wParamCount ) );
                  lPCodePos += 2;
               }
               else
                  lPCodePos += 3;
            }
            break;

         case HB_P_PUSHBLOCK:
            {
               USHORT wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               /* create the table of referenced local variables */
               while( wVar-- )
               {
                  hb_fputc( pFunc->pCode[ lPCodePos++ ] );
                  hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               }
            }
            break;

         case HB_P_PUSHDOUBLE:
            {
               int i;
               hb_fputc( pFunc->pCode[ lPCodePos++ ] );
               for( i = 0; i < sizeof( double ); ++i )
                  hb_fputc( ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
               hb_fputc( pFunc->pCode[ lPCodePos + sizeof( double ) ] );
               lPCodePos += sizeof( double ) + 1;
            }
            break;

         case HB_P_PUSHLONG:
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            break;

         case HB_P_PUSHSTR:
            {
               USHORT wLen = pFunc->pCode[ lPCodePos + 1 ] +
                      pFunc->pCode[ lPCodePos + 2 ] * 256;
               hb_fputc( pFunc->pCode[ lPCodePos     ] );
               hb_fputc( pFunc->pCode[ lPCodePos + 1 ] );
               hb_fputc( pFunc->pCode[ lPCodePos + 2 ] );
               lPCodePos += 3;
               while( wLen-- )
                  hb_fputc( pFunc->pCode[ lPCodePos++ ] );
            }
            break;

         case HB_P_SFRAME:
            /* we only generate it if there are statics used in this function */
            if( pFunc->bFlags & FUN_USES_STATICS )
            {
               USHORT w;
               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               hb_fputc( pFunc->pCode[ lPCodePos ] );
               hb_fputc( HB_LOBYTE( w ) );
               hb_fputc( HB_HIBYTE( w ) );
            }
            lPCodePos += 3;
            break;

         case HB_P_STATICS:
            {
               USHORT w;
               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               hb_fputc( pFunc->pCode[ lPCodePos ] );
               hb_fputc( HB_LOBYTE( w ) );
               hb_fputc( HB_HIBYTE( w ) );
               hb_fputc( pFunc->pCode[ lPCodePos + 3 ] );
               hb_fputc( pFunc->pCode[ lPCodePos + 4 ] );
               lPCodePos += 5;
            }
            break;

         default:
            printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
            lPCodePos = pFunc->lPCodePos;
            break;
      }
   }

   if( s_nChar != 0)
      fprintf( s_yyc, "\n" );
}

static void hb_fputc( BYTE b )
{
   ++s_nChar;

   if( s_nChar > 1 )
      fprintf( s_yyc, ", " );

   if( s_nChar == 9 )
   {
      fprintf( s_yyc, "\n\t" );
      s_nChar = 1;
   }

   /* Displaying as decimal is more compact than hex */
   fprintf( s_yyc, "%d", ( int ) b );
}


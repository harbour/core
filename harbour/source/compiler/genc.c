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

void hb_compGenCCode( PHB_FNAME pFileName )       /* generates the C language output */
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc = hb_comp_functions.pFirst, pFTemp;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT w, wLen, wSym, wVar;
   USHORT iNestedCodeblock = 0;
   ULONG lPCodePos;
   char chr;
   BOOL bEndProcRequired;

   FILE * yyc;             /* file handle for C output */

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

   fprintf( yyc, "/*\n * Harbour Compiler, %d.%d%s (Build %d) (%04d.%02d.%02d)\n",
      HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY );
   fprintf( yyc, " * Generated C source code\n */\n\n" );

   fprintf( yyc, "#include \"hbvmpub.h\"\n" );
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
   /* write functions prototypes for called functions outside this PRG */
   pFunc = hb_comp_funcalls.pFirst;
   while( pFunc )
   {
      pFTemp = hb_compFunctionFind( pFunc->szName );
      if( ! pFTemp || pFTemp == hb_comp_functions.pFirst )
         fprintf( yyc, "extern HB_FUNC( %s );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }

   /* writes the symbol table */
   /* Generate the wrapper that will initialize local symbol table
    */
   hb_strupr( pFileName->szName );
   fprintf( yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", hb_comp_szPrefix, pFileName->szName );

   if( ! hb_comp_bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

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

      fprintf( yyc, "\n{\n   static BYTE pcode[] =\n   {\n" );

      bEndProcRequired = TRUE;
      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case HB_P_AND:
               fprintf( yyc, "\tHB_P_AND,\n" );
               lPCodePos++;
               break;

            case HB_P_ARRAYPUSH:
               fprintf( yyc, "\tHB_P_ARRAYPUSH,\n" );
               lPCodePos++;
               break;

            case HB_P_ARRAYPOP:
               fprintf( yyc, "\tHB_P_ARRAYPOP,\n" );
               lPCodePos++;
               break;

            case HB_P_DEC:
               fprintf( yyc, "\tHB_P_DEC,\n" );
               lPCodePos++;
               break;

            case HB_P_ARRAYDIM:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\tHB_P_ARRAYDIM, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */", w );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               break;

            case HB_P_DIVIDE:
               fprintf( yyc, "\tHB_P_DIVIDE,\n" );
               lPCodePos++;
               break;

            case HB_P_DO:
               fprintf( yyc, "\tHB_P_DO, %i, %i,\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               lPCodePos += 3;
               break;

            case HB_P_DUPLICATE:
               fprintf( yyc, "\tHB_P_DUPLICATE,\n" );
               lPCodePos++;
               break;

            case HB_P_DUPLTWO:
               fprintf( yyc, "\tHB_P_DUPLTWO,\n" );
               lPCodePos++;
               break;

            case HB_P_EQUAL:
               fprintf( yyc, "\tHB_P_EQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_EXACTLYEQUAL:
               fprintf( yyc, "\tHB_P_EXACTLYEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_ENDBLOCK:
               --iNestedCodeblock;
               fprintf( yyc, "\tHB_P_ENDBLOCK,\n" );
               lPCodePos++;
               break;

            case HB_P_ENDPROC:
               lPCodePos++;
               if( lPCodePos == pFunc->lPCodePos )
               {
                  bEndProcRequired = FALSE;
                  fprintf( yyc, "\tHB_P_ENDPROC\n" );
               }
               else
                  fprintf( yyc, "\tHB_P_ENDPROC,\n" );
               break;

            case HB_P_FALSE:
               fprintf( yyc, "\tHB_P_FALSE,\n" );
               lPCodePos++;
               break;

            case HB_P_FORTEST:          /* ER For tests. Step > 0 LESS */
                                        /* Step < 0 GREATER */
               fprintf( yyc, "\tHB_P_FORTEST,\n" );
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
                     fprintf( yyc, "\tHB_P_FRAME, %i, %i,",
                              bLocals - pFunc->wParamCount,
                              pFunc->wParamCount );
                     if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* locals, params */" );
                     fprintf( yyc, "\n" );
                  }
               }
               lPCodePos += 3;
               break;

            case HB_P_FUNCPTR:
               fprintf( yyc, "\tHB_P_FUNCPTR,\n" );
               lPCodePos++;
               break;

            case HB_P_FUNCTION:
               fprintf( yyc, "\tHB_P_FUNCTION, %i, %i,\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               lPCodePos += 3;
               break;

            case HB_P_ARRAYGEN:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\tHB_P_ARRAYGEN, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */", w );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               break;

            case HB_P_GREATER:
               fprintf( yyc, "\tHB_P_GREATER,\n" );
               lPCodePos++;
               break;

            case HB_P_GREATEREQUAL:
               fprintf( yyc, "\tHB_P_GREATEREQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_INC:
               fprintf( yyc, "\tHB_P_INC,\n" );
               lPCodePos++;
               break;

            case HB_P_INSTRING:
               fprintf( yyc, "\tHB_P_INSTRING,\n" );
               lPCodePos++;
               break;

            case HB_P_JUMP:
            /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
               {
                  w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  fprintf( yyc, "\tHB_P_JUMP, %i, %i,",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i (abs: %05li) */", w, lPCodePos + ( w ? w : 3 ) );
                  fprintf( yyc, "\n" );
               }
               lPCodePos += 3;
               break;

            case HB_P_JUMPFALSE:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\tHB_P_JUMPFALSE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i (abs: %05li) */", w, lPCodePos + ( w ? w : 3 ) );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               break;

            case HB_P_JUMPTRUE:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\tHB_P_JUMPTRUE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i (abs: %05li) */", w, lPCodePos + ( w ? w : 3 ) );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               break;

            case HB_P_JUMPFAR:
            /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
               {
                  LONG lPos = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536;
                  fprintf( yyc, "\tHB_P_JUMPFAR, %i, %i, %i,",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ],
                            pFunc->pCode[ lPCodePos + 3 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %li (abs: %05li) */", lPos, lPCodePos + ( lPos ? lPos : 4 ) );
                  fprintf( yyc, "\n" );
               }
               lPCodePos += 4;
               break;

            case HB_P_JUMPFARFALSE:
               {
                  LONG lPos = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536;
                  fprintf( yyc, "\tHB_P_JUMPFARFALSE, %i, %i, %i,",
                           pFunc->pCode[ lPCodePos + 1 ],
                           pFunc->pCode[ lPCodePos + 2 ],
                           pFunc->pCode[ lPCodePos + 3 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %li (abs: %05li) */", lPos, lPCodePos + ( lPos ? lPos : 4 ) );
                  fprintf( yyc, "\n" );
               }
               lPCodePos += 4;
               break;

            case HB_P_JUMPFARTRUE:
               {
                  LONG lPos = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536;
                  fprintf( yyc, "\tHB_P_JUMPFARTRUE, %i, %i, %i,",
                           pFunc->pCode[ lPCodePos + 1 ],
                           pFunc->pCode[ lPCodePos + 2 ],
                           pFunc->pCode[ lPCodePos + 3 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %li (abs: %05li) */", lPos, lPCodePos + ( lPos ? lPos : 4 ) );
                  fprintf( yyc, "\n" );
               }
               lPCodePos += 4;
               break;

            case HB_P_LESS:
               fprintf( yyc, "\tHB_P_LESS,\n" );
               lPCodePos++;
               break;

            case HB_P_LESSEQUAL:
               fprintf( yyc, "\tHB_P_LESSEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_LINE:
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "/* %05li */ ", lPCodePos );
               else fprintf( yyc, "\t" );
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "HB_P_LINE, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */", w );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               break;

            case HB_P_LOCALNAME:
               fprintf( yyc, "\tHB_P_LOCALNAME, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 3 );
               fprintf( yyc, "\n" );
               lPCodePos += 3;
               while( pFunc->pCode[ lPCodePos ] )
               {
                  chr = pFunc->pCode[ lPCodePos++ ];
                  if( chr == '\'' || chr == '\\')
                     fprintf( yyc, " \'\\%c\',", chr );
                  else
                     fprintf( yyc, " \'%c\',", chr );
               }
               fprintf( yyc, " 0,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROPOP:
               fprintf( yyc, "\tHB_P_MACROPOP,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROPOPALIASED:
               fprintf( yyc, "\tHB_P_MACROPOPALIASED,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROPUSH:
               fprintf( yyc, "\tHB_P_MACROPUSH,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROPUSHALIASED:
               fprintf( yyc, "\tHB_P_MACROPUSHALIASED,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROSYMBOL:
               fprintf( yyc, "\tHB_P_MACROSYMBOL,\n" );
               lPCodePos++;
               break;

            case HB_P_MACROTEXT:
               fprintf( yyc, "\tHB_P_MACROTEXT,\n" );
               lPCodePos++;
               break;

            case HB_P_MESSAGE:
               {
                  USHORT wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wSym );
                  fprintf( yyc, "\tHB_P_MESSAGE, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wSym )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_MINUS:
               fprintf( yyc, "\tHB_P_MINUS,\n" );
               lPCodePos++;
               break;

            case HB_P_MODULENAME:
               fprintf( yyc, "\tHB_P_MODULENAME," );
               if( hb_comp_bGenCVerbose )
                  fprintf( yyc, "\t/* %s */", ( char * ) pFunc->pCode + lPCodePos + 1 );
               fprintf( yyc, "\n" );
               lPCodePos++;
               while( pFunc->pCode[ lPCodePos ] )
               {
                  chr = pFunc->pCode[ lPCodePos++ ];
                  if( chr == '\'' || chr == '\\')
                     fprintf( yyc, " \'\\%c\',", chr );
                  else
                     fprintf( yyc, " \'%c\',", chr );
               }
               fprintf( yyc, " 0,\n" );
               lPCodePos++;
               break;

            case HB_P_MODULUS:
               fprintf( yyc, "\tHB_P_MODULUS,\n" );
               lPCodePos++;
               break;

            case HB_P_MULT:
               fprintf( yyc, "\tHB_P_MULT,\n" );
               lPCodePos++;
               break;

            case HB_P_NEGATE:
               fprintf( yyc, "\tHB_P_NEGATE,\n" );
               lPCodePos++;
               break;

            case HB_P_NOT:
               fprintf( yyc, "\tHB_P_NOT,\n" );
               lPCodePos++;
               break;

            case HB_P_NOTEQUAL:
               fprintf( yyc, "\tHB_P_NOTEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_OR:
               fprintf( yyc, "\tHB_P_OR,\n" );
               lPCodePos++;
               break;

            case HB_P_PARAMETER:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PARAMETER, %i, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ),
                           pFunc->pCode[ lPCodePos + 3 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 4;
               }
               break;

            case HB_P_PLUS:
               fprintf( yyc, "\tHB_P_PLUS,\n" );
               lPCodePos++;
               break;

            case HB_P_POP:
               fprintf( yyc, "\tHB_P_POP,\n" );
               lPCodePos++;
               break;

            case HB_P_POPALIAS:
               fprintf( yyc, "\tHB_P_POPALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_POPALIASEDFIELD:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_POPALIASEDFIELD, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPALIASEDVAR:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_POPALIASEDVAR, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPFIELD:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_POPFIELD, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
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
                        fprintf( yyc, "\tHB_P_POPLOCAL, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* localvar%i */", -wVar );
                        fprintf( yyc, "\n" );
                     }
                     else
                     {
                        fprintf( yyc, "\tHB_P_POPLOCAL, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* codeblockvar%i */", wVar );
                        fprintf( yyc, "\n" );
                     }
                  }
                  else
                  {
                     fprintf( yyc, "\tHB_P_POPLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                     fprintf( yyc, "\n" );
                  }
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPMEMVAR:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_POPMEMVAR, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPSTATIC:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = hb_comp_functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\tHB_P_POPSTATIC, %i, %i,",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", pVar->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPVARIABLE:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_POPVARIABLE, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName  );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POWER:
               fprintf( yyc, "\tHB_P_POWER,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHALIAS:
               fprintf( yyc, "\tHB_P_PUSHALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHALIASEDFIELD:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHALIASEDFIELD, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHALIASEDVAR:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHALIASEDVAR, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHBLOCK:
               ++iNestedCodeblock;

               fprintf( yyc, "\tHB_P_PUSHBLOCK, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */",
                        pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256 );
               fprintf( yyc, "\n" );

               w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 3 ] ) );
               fprintf( yyc, "\t%i, %i,",
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* number of local parameters (%i) */", w );
               fprintf( yyc, "\n" );

               wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
               fprintf( yyc, "\t%i, %i,",
                        pFunc->pCode[ lPCodePos + 5 ],
                        pFunc->pCode[ lPCodePos + 6 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* number of local variables (%i) */", wVar );
               fprintf( yyc, "\n" );

               lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
               /* create the table of referenced local variables */
               while( wVar-- )
               {
                  w = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos ] ) );
                  fprintf( yyc, "\t%i, %i,",
                           pFunc->pCode[ lPCodePos ],
                           pFunc->pCode[ lPCodePos + 1 ] );
                  /* NOTE:
                   * When a codeblock is used to initialize a static variable
                   * the the names of local variables cannot be determined
                   * because at the time of C code generation we don't know
                   * in which function was defined this local variable
                   */
                  if( ( pFunc->cScope & ( HB_FS_INIT | HB_FS_EXIT ) ) != ( HB_FS_INIT | HB_FS_EXIT ) )
                     if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, w )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos +=2;
               }
               break;

            case HB_P_PUSHDOUBLE:
               {
                  int i;
                  ++lPCodePos;
                  fprintf( yyc, "\tHB_P_PUSHDOUBLE, " );
                  for( i = 0; i < sizeof( double ) + sizeof( BYTE ); ++i )
                     fprintf( yyc, "%i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %.*f, %d */",
                     *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
                     *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
                     *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ) );
                  fprintf( yyc, "\n" );
                  lPCodePos += sizeof( double ) + sizeof( BYTE );
               }
               break;

            case HB_P_PUSHFIELD:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHFIELD, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHINT:
               fprintf( yyc, "\tHB_P_PUSHINT, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */",
                        pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256 );
               fprintf( yyc, "\n" );
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
                        fprintf( yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* localvar%i */", -wVar );
                        fprintf( yyc, "\n" );
                     }
                     else
                     {
                        fprintf( yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* codeblockvar%i */", wVar );
                        fprintf( yyc, "\n" );
                     }
                  }
                  else
                  {
                     fprintf( yyc, "\tHB_P_PUSHLOCAL, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                     fprintf( yyc, "\n" );
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
                        fprintf( yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* localvar%i */", -wVar );
                        fprintf( yyc, "\n" );
                     }
                     else
                     {
                        fprintf( yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ] );
                        if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* codeblockvar%i */", wVar );
                        fprintf( yyc, "\n" );
                     }
                  }
                  else
                  {
                     fprintf( yyc, "\tHB_P_PUSHLOCALREF, %i, %i,",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ] );
                     if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compVariableFind( pFunc->pLocals, wVar )->szName );
                     fprintf( yyc, "\n" );
                  }
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHLONG:
               fprintf( yyc, "\tHB_P_PUSHLONG, %i, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %li */", *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
               fprintf( yyc, "\n" );
               lPCodePos += ( 1 + sizeof( long ) );
               break;

            case HB_P_PUSHMEMVAR:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHMEMVAR, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHMEMVARREF:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHMEMVARREF, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHNIL:
               fprintf( yyc, "\tHB_P_PUSHNIL,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHSELF:
               fprintf( yyc, "\tHB_P_PUSHSELF,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHSTATIC:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = hb_comp_functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\tHB_P_PUSHSTATIC, %i, %i,",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", pVar->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHSTATICREF:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = hb_comp_functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = hb_compVariableFind( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\tHB_P_PUSHSTATICREF, %i, %i,",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ] );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", pVar->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHSTR:
               wLen = pFunc->pCode[ lPCodePos + 1 ] +
                      pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\tHB_P_PUSHSTR, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i */", wLen );
               lPCodePos +=3;
               if( wLen > 0 )
               {
                  unsigned char uchr;

                  fprintf( yyc, "\n\t" );
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
                        fprintf( yyc, "%i, ", uchr );
                     else if( strchr( "\'\\\"", uchr ) )
                        fprintf( yyc, "%i, ", uchr );
                     else
                        fprintf( yyc, "\'%c\', ", uchr );
                  }
               }
               fprintf( yyc, "\n" );
               break;

            case HB_P_PUSHSYM:
               {
                  USHORT wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wSym );
                  fprintf( yyc, "\tHB_P_PUSHSYM, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wSym )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHVARIABLE:
               {
                  USHORT wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = hb_compSymbolFixPos( wVar );
                  fprintf( yyc, "\tHB_P_PUSHVARIABLE, %i, %i,",
                           HB_LOBYTE( wFixPos ),
                           HB_HIBYTE( wFixPos ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %s */", hb_compSymbolGetPos( wVar )->szName );
                  fprintf( yyc, "\n" );
                  lPCodePos += 3;
               }
               break;

            case HB_P_RETVALUE:
               fprintf( yyc, "\tHB_P_RETVALUE,\n" );
               lPCodePos++;
               break;

            case HB_P_SEQBEGIN:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536;
               fprintf( yyc, "\tHB_P_SEQBEGIN, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i (abs: %05li) */", w, lPCodePos + ( w ? w : 4 ) );
               fprintf( yyc, "\n" );
               lPCodePos += 4;
               break;

            case HB_P_SEQEND:
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "/* %05li */ ", lPCodePos );
               else fprintf( yyc, "\t" );
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256 + pFunc->pCode[ lPCodePos + 3 ] * 65536;
               fprintf( yyc, "HB_P_SEQEND, %i, %i, %i,",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* %i (abs: %05li) */", w, lPCodePos + ( w ? w : 4 ) );
               fprintf( yyc, "\n" );
               lPCodePos += 4;
               break;

            case HB_P_SEQRECOVER:
               fprintf( yyc, "\tHB_P_SEQRECOVER,\n" );
               lPCodePos++;
               break;

            case HB_P_SFRAME:
               /* we only generate it if there are statics used in this function */
               if( pFunc->bFlags & FUN_USES_STATICS )
               {
                  hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
                  w = hb_compSymbolFixPos( w );
                  fprintf( yyc, "\tHB_P_SFRAME, %i, %i,",
                           HB_LOBYTE( w ), HB_HIBYTE( w ) );
                  if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* symbol (_INITSTATICS) */" );
                  fprintf( yyc, "\n" );
               }
               lPCodePos += 3;
               break;

            case HB_P_STATICS:

               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               w = hb_compSymbolFixPos( w );
               fprintf( yyc, "\tHB_P_STATICS, %i, %i, %i, %i,",
                        HB_LOBYTE( w ),
                        HB_HIBYTE( w ),
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ] );
               if( hb_comp_bGenCVerbose ) fprintf( yyc, "\t/* symbol (_INITSTATICS), %i statics */", pFunc->pCode[ lPCodePos + 3 ] + pFunc->pCode[ lPCodePos + 4 ] * 256 );
               fprintf( yyc, "\n" );
               lPCodePos += 5;
               break;

            case HB_P_SWAPALIAS:
               fprintf( yyc, "\tHB_P_SWAPALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_TRUE:
               fprintf( yyc, "\tHB_P_TRUE,\n" );
               lPCodePos++;
               break;

            case HB_P_ZERO:
               fprintf( yyc, "\tHB_P_ZERO,\n" );
               lPCodePos++;
               break;

            case HB_P_NOOP:
               fprintf( yyc, "\tHB_P_NOOP,\n" );
               lPCodePos++;
               break;

            default:
               fprintf( yyc, "\t%u, /* Incorrect pcode value: %u */\n", pFunc->pCode[ lPCodePos ], pFunc->pCode[ lPCodePos ] );
               printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
               lPCodePos = pFunc->lPCodePos;
               break;
         }
      }

      if( bEndProcRequired )
      {
         if( hb_comp_bGenCVerbose ) fprintf( yyc, "/* %05li */ ", lPCodePos );
         else fprintf( yyc, "\t" );
         fprintf( yyc, "HB_P_ENDPROC\n" );
      }
      else
      {
         if( hb_comp_bGenCVerbose ) fprintf( yyc, "/* %05li */\n", lPCodePos );
      }

      fprintf( yyc, "   };\n\n" );
      fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n}\n\n" );
      pFunc = pFunc->pNext;
   }

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

   pSym = hb_comp_symbols.pFirst;
   while( pSym )
      pSym = hb_compSymbolKill( pSym );

   if( ! hb_comp_bQuiet )
      printf( "Done.\n" );
}


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

#include "extend.h"
#include "compiler.h"
#include "pcode.h"
#include "hberrors.h"

void GenCCode( PHB_FNAME pFileName )       /* generates the C language output */
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc = functions.pFirst, pFTemp;
   PCOMSYMBOL pSym = symbols.pFirst;
   WORD w, wLen, wSym, wVar;
   WORD iNestedCodeblock = 0;
   ULONG lPCodePos;
   char chr;
   BOOL bEndProcRequired;

   FILE * yyc;             /* file handle for C output */

   if( ! pFileName->szExtension )
      pFileName->szExtension =".c";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
      GenError( _szCErrors, 'E', ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! _bQuiet )
   {
      printf( "\nGenerating C source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   fprintf( yyc, "/* Harbour compiler generated code */\n\n" );
   fprintf( yyc, "#include \"hb_vmpub.h\"\n" );
   fprintf( yyc, "#include \"init.h\"\n\n\n" );

   if( ! _bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   /* write functions prototypes for PRG defined functions */
   while( pFunc )
   {
      if( pFunc->cScope & FS_STATIC || pFunc->cScope & FS_INIT || pFunc->cScope & FS_EXIT )
         fprintf( yyc, "static " );

      if( pFunc == _pInitFunc )
         fprintf( yyc, "HARBOUR hb_INITSTATICS( void );\n" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( yyc, "HARBOUR HB_%s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }
   /* write functions prototypes for called functions outside this PRG */
   pFunc = funcalls.pFirst;
   while( pFunc )
   {
      pFTemp = GetFunction( pFunc->szName );
      if( ! pFTemp || pFTemp == functions.pFirst )
         fprintf( yyc, "extern HARBOUR HB_%s( void );\n", pFunc->szName );
      pFunc = pFunc->pNext;
   }

   /* writes the symbol table */
   /* Generate the wrapper that will initialize local symbol table
    */
   yy_strupr( pFileName->szName );
   fprintf( yyc, "\n\nHB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_%s%s )\n", _szPrefix, pFileName->szName );

   if( ! _bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   wSym = 0; /* symbols counter */
   while( pSym )
   {
      if( pSym->szName[ 0 ] == '(' )
      {
         /* Since the normal function cannot be INIT and EXIT at the same time
         * we are using these two bits to mark the special function used to
         * initialize static variables
         */
         fprintf( yyc, "{ \"(_INITSTATICS)\", FS_INIT | FS_EXIT, hb_INITSTATICS, 0}" ); /* NOTE: hb_ intentionally in lower case */
      }
      else
      {
         fprintf( yyc, "{ \"%s\", ", pSym->szName );

         if( pSym->cScope & FS_STATIC )
            fprintf( yyc, "FS_STATIC" );

         else if( pSym->cScope & FS_INIT )
            fprintf( yyc, "FS_INIT" );

         else if( pSym->cScope & FS_EXIT )
            fprintf( yyc, "FS_EXIT" );

         else
            fprintf( yyc, "FS_PUBLIC" );

         if( pSym->cScope & VS_MEMVAR )
            fprintf( yyc, " | FS_MEMVAR" );

         if( ( pSym->cScope != FS_MESSAGE ) && ( pSym->cScope & FS_MESSAGE ) ) /* only for non public symbols */
            fprintf( yyc, " | FS_MESSAGE" );

         /* specify the function address if it is a defined function or an
            external called function */
         if( GetFunction( pSym->szName ) ) /* is it a function defined in this module */
            fprintf( yyc, ", HB_%s, 0 }", pSym->szName );
         else if( GetFuncall( pSym->szName ) ) /* is it a function called from this module */
            fprintf( yyc, ", HB_%s, 0 }", pSym->szName );
         else
            fprintf( yyc, ", 0, 0 }" );   /* memvar */
      }
      ++wSym;

      if( pSym != symbols.pLast )
         fprintf( yyc, ",\n" );

      pSym = pSym->pNext;
   }
   fprintf( yyc, "\nHB_INIT_SYMBOLS_END( hb_vm_SymbolInit_%s%s )\n", _szPrefix, pFileName->szName );
   fprintf( yyc, "#if ! defined(__GNUC__)\n   #pragma startup hb_vm_SymbolInit_%s%s\n#endif\n\n\n", _szPrefix, pFileName->szName );

   /* Generate functions data
    */
   pFunc = functions.pFirst;
   if( ! _bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */
   while( pFunc )
   {
      if( pFunc->cScope != FS_PUBLIC )
         fprintf( yyc, "static " );

      if( pFunc == _pInitFunc )        /* Is it (_INITSTATICS) */
         fprintf( yyc, "HARBOUR hb_INITSTATICS( void )\n{\n   static BYTE pcode[] = { \n" ); /* NOTE: hb_ intentionally in lower case */
      else
         fprintf( yyc, "HARBOUR HB_%s( void )\n{\n   static BYTE pcode[] = { \n", pFunc->szName );

      bEndProcRequired = TRUE;
      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
      {
         switch( pFunc->pCode[ lPCodePos ] )
         {
            case HB_P_AND:
               fprintf( yyc, "\t\tHB_P_AND,\n" );
               lPCodePos++;
               break;

            case HB_P_ARRAYAT:
               fprintf( yyc, "\t\tHB_P_ARRAYAT,\n" );
               lPCodePos++;
               break;

            case HB_P_ARRAYPUT:
               fprintf( yyc, "\t\tHB_P_ARRAYPUT,\n" );
               lPCodePos++;
               break;

            case HB_P_DEC:
               fprintf( yyc, "\t\tHB_P_DEC,\n" );
               lPCodePos++;
               break;

            case HB_P_DIMARRAY:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_DIMARRAY, %i, %i,\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w );
               lPCodePos += 3;
               break;

            case HB_P_DIVIDE:
               fprintf( yyc, "\t\tHB_P_DIVIDE,\n" );
               lPCodePos++;
               break;

            case HB_P_DO:
               fprintf( yyc, "\t\tHB_P_DO, %i, %i,\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               lPCodePos += 3;
               break;

            case HB_P_DUPLICATE:
               fprintf( yyc, "\t\tHB_P_DUPLICATE,\n" );
               lPCodePos++;
               break;

            case HB_P_DUPLTWO:
               fprintf( yyc, "\t\tHB_P_DUPLTWO,\n" );
               lPCodePos++;
               break;

            case HB_P_EQUAL:
               fprintf( yyc, "\t\tHB_P_EQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_EXACTLYEQUAL:
               fprintf( yyc, "\t\tHB_P_EXACTLYEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_ENDBLOCK:
               --iNestedCodeblock;
               fprintf( yyc, "\t\tHB_P_ENDBLOCK,\n" );
               lPCodePos++;
               break;

            case HB_P_ENDPROC:
               lPCodePos++;
               if( lPCodePos == pFunc->lPCodePos )
               {
                  bEndProcRequired = FALSE;
                  fprintf( yyc, "\t\tHB_P_ENDPROC\n" );
               }
               else
                  fprintf( yyc, "\t\tHB_P_ENDPROC,\n" );
               break;

            case HB_P_FALSE:
               fprintf( yyc, "\t\tHB_P_FALSE,\n" );
               lPCodePos++;
               break;

            case HB_P_FORTEST:          /* ER For tests. Step > 0 LESS */
                                        /* Step < 0 GREATER */
               fprintf( yyc, "\t\tHB_P_FORTEST,\n" );
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
                     fprintf( yyc, "\t\tHB_P_FRAME, %i, %i,\t/* locals, params */\n",
                              bLocals - pFunc->wParamCount,
                              pFunc->wParamCount );
                  lPCodePos += 3;
               }
               break;

            case HB_P_FUNCPTR:
               fprintf( yyc, "\t\tHB_P_FUNCPTR,\n" );
               lPCodePos++;
               break;

            case HB_P_FUNCTION:
               fprintf( yyc, "\t\tHB_P_FUNCTION, %i, %i,\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ] );
               lPCodePos += 3;
               break;

            case HB_P_GENARRAY:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_GENARRAY, %i, %i,\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w );
               lPCodePos += 3;
               break;

            case HB_P_GREATER:
               fprintf( yyc, "\t\tHB_P_GREATER,\n" );
               lPCodePos++;
               break;

            case HB_P_GREATEREQUAL:
               fprintf( yyc, "\t\tHB_P_GREATEREQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_INC:
               fprintf( yyc, "\t\tHB_P_INC,\n" );
               lPCodePos++;
               break;

            case HB_P_INSTRING:
               fprintf( yyc, "\t\tHB_P_INSTRING,\n" );
               lPCodePos++;
               break;

            case HB_P_JUMP:
            /* if( 1 ) ( lPCodePos + 3 ) < pFunc->lPCodePos ) */
               {
                  w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  fprintf( yyc, "\t\tHB_P_JUMP, %i, %i,\t/* %i (abs: %05li) */\n",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
               }
               lPCodePos += 3;
               break;

            case HB_P_JUMPFALSE:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_JUMPFALSE, %i, %i,\t/* %i (abs: %05li) */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
               lPCodePos += 3;
               break;

            case HB_P_JUMPTRUE:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_JUMPTRUE, %i, %i,\t/* %i (abs: %05li) */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
               lPCodePos += 3;
               break;

            case HB_P_LESS:
               fprintf( yyc, "\t\tHB_P_LESS,\n" );
               lPCodePos++;
               break;

            case HB_P_LESSEQUAL:
               fprintf( yyc, "\t\tHB_P_LESSEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_LINE:
               fprintf( yyc, "/* %05li */", lPCodePos );
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "  HB_P_LINE, %i, %i,\t\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w );
               lPCodePos += 3;
               break;

            case HB_P_LOCALNAME:
               fprintf( yyc, "\t\tHB_P_LOCALNAME, %i, %i,\t/* %s */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        ( char * ) pFunc->pCode + lPCodePos + 3 );
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

            case HB_P_MESSAGE:
               {
                  WORD wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wSym );
                  fprintf( yyc, "\t\tHB_P_MESSAGE, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wSym )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_MINUS:
               fprintf( yyc, "\t\tHB_P_MINUS,\n" );
               lPCodePos++;
               break;

            case HB_P_MODULENAME:
               fprintf( yyc, "\t\tHB_P_MODULENAME,\t/* %s */\n",
                        ( char * ) pFunc->pCode + lPCodePos++ + 1 );
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
               fprintf( yyc, "\t\tHB_P_MODULUS,\n" );
               lPCodePos++;
               break;

            case HB_P_MULT:
               fprintf( yyc, "\t\tHB_P_MULT,\n" );
               lPCodePos++;
               break;

            case HB_P_NEGATE:
               fprintf( yyc, "\t\tHB_P_NEGATE,\n" );
               lPCodePos++;
               break;

            case HB_P_NOT:
               fprintf( yyc, "\t\tHB_P_NOT,\n" );
               lPCodePos++;
               break;

            case HB_P_NOTEQUAL:
               fprintf( yyc, "\t\tHB_P_NOTEQUAL,\n" );
               lPCodePos++;
               break;

            case HB_P_OR:
               fprintf( yyc, "\t\tHB_P_OR,\n" );
               lPCodePos++;
               break;

            case HB_P_PARAMETER:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_PARAMETER, %i, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           pFunc->pCode[ lPCodePos + 3 ],
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 4;
               }
               break;

            case HB_P_PLUS:
               fprintf( yyc, "\t\tHB_P_PLUS,\n" );
               lPCodePos++;
               break;

            case HB_P_POP:
               fprintf( yyc, "\t\tHB_P_POP,\n" );
               lPCodePos++;
               break;

            case HB_P_POPALIAS:
               fprintf( yyc, "\t\tHB_P_POPALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_POPALIASEDFIELD:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_POPALIASEDFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPFIELD:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_POPFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
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
                        fprintf( yyc, "\t\tHB_P_POPLOCAL, %i, %i,\t/* localvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 -wVar );
                     else
                        fprintf( yyc, "\t\tHB_P_POPLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 wVar );
                  }
                  else
                     fprintf( yyc, "\t\tHB_P_POPLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPMEMVAR:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_POPMEMVAR, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POPSTATIC:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = GetVar( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\t\tHB_P_POPSTATIC, %i, %i,\t/* %s */\n",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ],
                            pVar->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_POWER:
               fprintf( yyc, "\t\tHB_P_POWER,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHALIAS:
               fprintf( yyc, "\t\tHB_P_PUSHALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHALIASEDFIELD:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_PUSHALIASEDFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHBLOCK:
               ++iNestedCodeblock;
               fprintf( yyc, "\t\tHB_P_PUSHBLOCK, %i, %i,\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256 );
               w = * ( ( WORD * ) &( pFunc->pCode [ lPCodePos + 3 ] ) );
               fprintf( yyc, "\t\t%i, %i,\t/* number of local parameters (%i) */\n",
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ], w );
               wVar = * ( ( WORD * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
               fprintf( yyc, "\t\t%i, %i,\t/* number of local variables (%i) */\n",
                        pFunc->pCode[ lPCodePos + 5 ],
                        pFunc->pCode[ lPCodePos + 6 ], wVar );
               lPCodePos += 7;  /* codeblock size + number of parameters + number of local variables */
               /* create the table of referenced local variables */
               while( wVar-- )
               {
                  w = * ( ( WORD * ) &( pFunc->pCode [ lPCodePos ] ) );
                  fprintf( yyc, "\t\t%i, %i,\t/* %s */\n",
                           pFunc->pCode[ lPCodePos ],
                           pFunc->pCode[ lPCodePos + 1 ],
                           GetVar( pFunc->pLocals, w )->szName );
                  lPCodePos +=2;
               }
               break;

            case HB_P_PUSHDOUBLE:
               {
                  int i;
                  ++lPCodePos;
                  fprintf( yyc, "\t\tHB_P_PUSHDOUBLE, " );
                  for( i = 0; i < sizeof( double ) + sizeof( BYTE ); ++i )
                     fprintf( yyc, "%i,", ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ] );
                  fprintf( yyc, "\t/* %.*f, %d */\n",
                  *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ),
                  *( ( double * ) &( pFunc->pCode[ lPCodePos ] ) ),
                  *( ( BYTE * ) &( pFunc->pCode[ lPCodePos + sizeof( double ) ] ) ) );
                  lPCodePos += sizeof( double ) + sizeof( BYTE );
               }
               break;

            case HB_P_PUSHFIELD:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_PUSHFIELD, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHINT:
               fprintf( yyc, "\t\tHB_P_PUSHINT, %i, %i,\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 1 ] +
                        pFunc->pCode[ lPCodePos + 2 ] * 256 );
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
                        fprintf( yyc, "\t\tHB_P_PUSHLOCAL, %i, %i,\t/* localvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 -wVar );
                     else
                        fprintf( yyc, "\t\tHB_P_PUSHLOCAL, %i, %i,\t/* codeblockvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 wVar );
                  }
                  else
                     fprintf( yyc, "\t\tHB_P_PUSHLOCAL, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
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
                        fprintf( yyc, "\t\tHB_P_PUSHLOCALREF, %i, %i,\t/* localvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 -wVar );
                     else
                        fprintf( yyc, "\t\tHB_P_PUSHLOCALREF, %i, %i,\t/* codeblockvar%i */\n",
                                 pFunc->pCode[ lPCodePos + 1 ],
                                 pFunc->pCode[ lPCodePos + 2 ],
                                 wVar );
                  }
                  else
                     fprintf( yyc, "\t\tHB_P_PUSHLOCALREF, %i, %i,\t/* %s */\n",
                              pFunc->pCode[ lPCodePos + 1 ],
                              pFunc->pCode[ lPCodePos + 2 ],
                              GetVar( pFunc->pLocals, wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHLONG:
               fprintf( yyc, "\t\tHB_P_PUSHLONG, %i, %i, %i, %i,\t/* %li */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ],
                        pFunc->pCode[ lPCodePos + 3 ],
                        pFunc->pCode[ lPCodePos + 4 ],
                        *( ( long * ) &( pFunc->pCode[ lPCodePos + 1 ] ) ) );
               lPCodePos += ( 1 + sizeof( long ) );
               break;

            case HB_P_PUSHMEMVAR:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_PUSHMEMVAR, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHMEMVARREF:
               {
                  WORD wFixPos;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] +
                         pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wVar );
                  fprintf( yyc, "\t\tHB_P_PUSHMEMVARREF, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wVar )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHNIL:
               fprintf( yyc, "\t\tHB_P_PUSHNIL,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHSELF:
               fprintf( yyc, "\t\tHB_P_PUSHSELF,\n" );
               lPCodePos++;
               break;

            case HB_P_PUSHSTATIC:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = GetVar( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\t\tHB_P_PUSHSTATIC, %i, %i,\t/* %s */\n",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ],
                            pVar->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHSTATICREF:
               {
                  PVAR pVar;
                  PFUNCTION pTmp = functions.pFirst;

                  wVar = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
                  while( pTmp->pNext && pTmp->pNext->iStaticsBase < wVar )
                      pTmp = pTmp->pNext;
                  pVar = GetVar( pTmp->pStatics, wVar - pTmp->iStaticsBase );
                  fprintf( yyc, "\t\tHB_P_PUSHSTATICREF, %i, %i,\t/* %s */\n",
                            pFunc->pCode[ lPCodePos + 1 ],
                            pFunc->pCode[ lPCodePos + 2 ],
                            pVar->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_PUSHSTR:
               wLen = pFunc->pCode[ lPCodePos + 1 ] +
                      pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_PUSHSTR, %i, %i,\t/* %i */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], wLen );
               lPCodePos +=3;
               while( wLen-- )
               {
                  chr = pFunc->pCode[ lPCodePos++ ];
                  if( chr == '\'' || chr == '\\')
                     fprintf( yyc, " \'\\%c\',", chr );
                  else
                     fprintf( yyc, " \'%c\',", chr );
               }
               fprintf( yyc, "\n" );
               break;

            case HB_P_PUSHSYM:
               {
                  WORD wFixPos;

                  wSym = pFunc->pCode[ lPCodePos + 1 ] +
                          pFunc->pCode[ lPCodePos + 2 ] * 256;
                  wFixPos = FixSymbolPos( wSym );
                  fprintf( yyc, "\t\tHB_P_PUSHSYM, %i, %i,\t/* %s */\n",
                           LOBYTE( wFixPos ),
                           HIBYTE( wFixPos ),
                           GetSymbolOrd( wSym )->szName );
                  lPCodePos += 3;
               }
               break;

            case HB_P_RETVALUE:
               fprintf( yyc, "\t\tHB_P_RETVALUE,\n" );
               lPCodePos++;
               break;

            case HB_P_SEQBEGIN:
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "\t\tHB_P_SEQBEGIN, %i, %i,\t/* %i (abs: %05li) */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
               lPCodePos += 3;
               break;

            case HB_P_SEQEND:
               fprintf( yyc, "/* %05li */", lPCodePos );
               w = pFunc->pCode[ lPCodePos + 1 ] + pFunc->pCode[ lPCodePos + 2 ] * 256;
               fprintf( yyc, "     HB_P_SEQEND, %i, %i,\t/* %i (abs: %05li) */\n",
                        pFunc->pCode[ lPCodePos + 1 ],
                        pFunc->pCode[ lPCodePos + 2 ], w, lPCodePos + ( w ? w: 3 ) );
               lPCodePos += 3;
               break;

            case HB_P_SEQRECOVER:
               fprintf( yyc, "\t\tHB_P_SEQRECOVER,\n" );
               lPCodePos++;
               break;

            case HB_P_SFRAME:
               /* we only generate it if there are statics used in this function */
               if( pFunc->bFlags & FUN_USES_STATICS )
               {
                  GetSymbol( _pInitFunc->szName, &w );
                  w = FixSymbolPos( w );
                  fprintf( yyc, "\t\tHB_P_SFRAME, %i, %i,\t/* symbol (_INITSTATICS) */\n",
                           LOBYTE( w ), HIBYTE( w ) );
               }
               lPCodePos += 3;
               break;

            case HB_P_STATICS:
               {
                  GetSymbol( _pInitFunc->szName, &w );
                  w = FixSymbolPos( w );
                  fprintf( yyc, "\t\tHB_P_STATICS, %i, %i,\t/* symbol (_INITSTATICS) */\n",
                           LOBYTE( w ), HIBYTE( w ) );
                  lPCodePos += 3;
               }
               break;

            case HB_P_SWAPALIAS:
               fprintf( yyc, "\t\tHB_P_SWAPALIAS,\n" );
               lPCodePos++;
               break;

            case HB_P_TRUE:
               fprintf( yyc, "\t\tHB_P_TRUE,\n" );
               lPCodePos++;
               break;

            case HB_P_ZERO:
               fprintf( yyc, "\t\tHB_P_ZERO,\n" );
               lPCodePos++;
               break;

            default:
               printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
               lPCodePos = pFunc->lPCodePos;
               break;
         }
      }

      fprintf( yyc, "/* %05li */", lPCodePos );
      if( bEndProcRequired )
         fprintf( yyc, "  HB_P_ENDPROC };\n\n" );
      else
         fprintf( yyc, "  };\n\n" );
      fprintf( yyc, "   hb_vmExecute( pcode, symbols );\n}\n\n" );
      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   pFunc = functions.pFirst;
   while( pFunc )
      pFunc = KillFunction( pFunc );

   pFunc = funcalls.pFirst;
   while( pFunc )
   {
      funcalls.pFirst = pFunc->pNext;
      hb_xfree( ( void * ) pFunc );  /* NOTE: szName will be released by KillSymbol() */
      pFunc = funcalls.pFirst;
   }

   pSym = symbols.pFirst;
   while( pSym )
      pSym = KillSymbol( pSym );

   if( ! _bQuiet )
      printf( "Done.\n" );
}

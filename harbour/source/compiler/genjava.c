/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Java source generation
 *
 * Copyright 1999 Matteo Baccan <baccan@isanet.it>
 *                Based on a work of Eddie Runia <eddie@runia.com>
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

#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

static void hb_fputc( BYTE b, FILE * yyc );
static void hb_fputs( char * szName, FILE * yyc );

static int s_nChar = 0;

void hb_compGenJava( PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc /*= hb_comp_functions.pFirst */;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   USHORT w, wLen, wVar;
   ULONG lPCodePos;
   LONG lPad;
   LONG lSymbols;
   BOOL bEndProcReq;
   ULONG ulCodeLength;
   FILE * yyc;             /* file handle for C output */

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".java";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating Java source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   s_nChar = 0;

   fprintf( yyc, "/*\n * Harbour Compiler, Build %i%s (%04d.%02d.%02d)\n",
         HB_VER_BUILD, HB_VER_REVISION, HB_VER_YEAR, HB_VER_MONTH, HB_VER_DAY );
   fprintf( yyc, " * Generated JAVA source code\n */\n\n" );

   fprintf( yyc, "public class %s\n", pFileName->szName );
   fprintf( yyc, "{\n" );
   fprintf( yyc, "   public static int[] pCode =\n" );
   fprintf( yyc, "   {\n" );
   fprintf( yyc, "      " );

   /* writes the symbol table */

   if( ! hb_comp_bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   lSymbols = 0;                /* Count number of symbols */
   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }
   hb_fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   hb_fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
   hb_fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   hb_fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   pSym = hb_comp_symbols.pFirst;
   if( ! hb_comp_bStartProc )
      pSym = pSym->pNext; /* starting procedure is always the first symbol */

   while( pSym )
   {
      hb_fputs( pSym->szName, yyc );
      hb_fputc( 0, yyc );
      if( pSym->cScope != _HB_FS_MESSAGE )
         hb_fputc( pSym->cScope, yyc );
      else
         hb_fputc( 0, yyc );

      /* specify the function address if it is a defined function or a
         external called function */
      if( hb_compFunctionFind( pSym->szName ) ) /* is it a defined function ? */
      {
         hb_fputc( SYM_FUNC, yyc );
      }
      else
      {
         if( hb_compFunCallFind( pSym->szName ) )
         {
            hb_fputc( SYM_EXTERN, yyc );
         }
         else
         {
            hb_fputc( SYM_NOLINK, yyc );
         }
      }
      pSym = pSym->pNext;
   }

   pFunc = hb_comp_functions.pFirst;
   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext;

   lSymbols = 0;                /* Count number of symbols */
   while( pFunc )
   {
      lSymbols++;
      pFunc = pFunc->pNext;
   }
   hb_fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   hb_fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
   hb_fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   hb_fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   /* Generate functions data
    */
   pFunc = hb_comp_functions.pFirst;
   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   while( pFunc )
   {
      hb_fputs( pFunc->szName, yyc );
      hb_fputc( 0, yyc );
      /* We will have to add HB_P_ENDPROC in cases when RETURN statement
       * was not used in a function/procedure - this is why we have to reserve
       * one additional byte
       */
      ulCodeLength = pFunc->lPCodePos + 1;
      hb_fputc( ( BYTE ) ( ( ulCodeLength       ) & 255 ), yyc ); /* Write size */
      hb_fputc( ( BYTE ) ( ( ulCodeLength >> 8  ) & 255 ), yyc );
      hb_fputc( ( BYTE ) ( ( ulCodeLength >> 16 ) & 255 ), yyc );
      hb_fputc( ( BYTE ) ( ( ulCodeLength >> 24 ) & 255 ), yyc );

/*      printf( "Creating output for %s\n", pFunc->szName ); */

      lPCodePos = 0;
      lPad = 0;                         /* Number of bytes optimized */
      bEndProcReq = TRUE;
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
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
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
            case HB_P_SEQBEGIN:
            case HB_P_SEQEND:
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               break;

            case HB_P_ENDPROC:
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               if( lPCodePos == pFunc->lPCodePos )
                  bEndProcReq = FALSE;
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
                     hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                     hb_fputc( ( BYTE )( bLocals - pFunc->wParamCount ), yyc );
                     hb_fputc( ( BYTE )( pFunc->wParamCount ), yyc );
                     lPCodePos += 2;
                  }
                  else
                  {
                     lPad += 3;
                     lPCodePos += 3;
                  }
               }
               break;

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
               hb_fputc( pFunc->pCode[ lPCodePos ], yyc );
               wVar = hb_compSymbolFixPos( pFunc->pCode[ lPCodePos + 1 ] + 256 * pFunc->pCode[ lPCodePos + 2 ] );
               hb_fputc( HB_LOBYTE( wVar ), yyc );
               hb_fputc( HB_HIBYTE( wVar ), yyc );
               lPCodePos += 3;
               break;

            case HB_P_PARAMETER:
               hb_fputc( pFunc->pCode[ lPCodePos ], yyc );
               wVar = hb_compSymbolFixPos( pFunc->pCode[ lPCodePos + 1 ] + 256 * pFunc->pCode[ lPCodePos + 2 ] );
               hb_fputc( HB_LOBYTE( wVar ), yyc );
               hb_fputc( HB_HIBYTE( wVar ), yyc );
               hb_fputc( pFunc->pCode[ lPCodePos + 3 ], yyc );
               lPCodePos +=4;
               break;

            case HB_P_PUSHBLOCK:
               wVar = * ( ( USHORT * ) &( pFunc->pCode [ lPCodePos + 5 ] ) );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               /* create the table of referenced local variables */
               while( wVar-- )
               {
                  hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
                  hb_fputc(   pFunc->pCode[ lPCodePos++ ], yyc );
               }
               break;

            case HB_P_PUSHDOUBLE:
               {
                  int i;
                  hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
                  for( i = 0; i < sizeof( double ); ++i )
                     hb_fputc( ( ( BYTE * ) pFunc->pCode )[ lPCodePos + i ], yyc );
                  hb_fputc( pFunc->pCode[ lPCodePos + sizeof( double ) ], yyc );
                  lPCodePos += sizeof( double ) + 1;
               }
               break;

            case HB_P_PUSHLONG:
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               break;

            case HB_P_PUSHSTR:
               wLen = pFunc->pCode[ lPCodePos + 1 ] +
                      pFunc->pCode[ lPCodePos + 2 ] * 256;
               hb_fputc( pFunc->pCode[ lPCodePos     ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos + 1 ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos + 2 ], yyc );
               lPCodePos += 3;
               while( wLen-- )
                  hb_fputc( pFunc->pCode[ lPCodePos++ ], yyc );
               break;

            case HB_P_SFRAME:
               /* we only generate it if there are statics used in this function */
               if( pFunc->bFlags & FUN_USES_STATICS )
               {
                  hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
                  w = hb_compSymbolFixPos( w );
                  hb_fputc( pFunc->pCode[ lPCodePos ], yyc );
                  hb_fputc( HB_LOBYTE( w ), yyc );
                  hb_fputc( HB_HIBYTE( w ), yyc );
               }
               else
                  lPad += 3;
               lPCodePos += 3;
               break;

            case HB_P_STATICS:
               hb_compSymbolFind( hb_comp_pInitFunc->szName, &w );
               w = hb_compSymbolFixPos( w );
               hb_fputc( pFunc->pCode[ lPCodePos ], yyc );
               hb_fputc( HB_LOBYTE( w ), yyc );
               hb_fputc( HB_HIBYTE( w ), yyc );
               hb_fputc( pFunc->pCode[ lPCodePos + 3 ], yyc );
               hb_fputc( pFunc->pCode[ lPCodePos + 4 ], yyc );
               lPCodePos += 5;
               break;

            default:
               printf( "Incorrect pcode value: %u\n", pFunc->pCode[ lPCodePos ] );
               lPCodePos = pFunc->lPCodePos;
               break;
         }
      }

      if( bEndProcReq )
         hb_fputc( HB_P_ENDPROC, yyc );
      else
      {
         /* HB_P_ENDPROC was the last opcode: we have to fill the byte
          * reserved earlier
          */
         lPad++;
      }
      for( ; lPad; lPad-- )
      {
         /* write additional bytes to agree with stored earlier
          * function/procedure size
          */
         hb_fputc( 0, yyc );
      }
      pFunc = pFunc->pNext;
   }

   fprintf( yyc, "\n   };\n\n" );
   fprintf( yyc, "   static public void main( String argv[] )\n" );
   fprintf( yyc, "   {\n" );
   fprintf( yyc, "      Harbour.Run( %s.pCode ); \n", pFileName->szName );
   fprintf( yyc, "   }\n\n" );
   fprintf( yyc, "}\n" );

   fclose( yyc );

   if( ! hb_comp_bQuiet )
      printf( "Done.\n" );
}

static void hb_fputc( BYTE b, FILE * yyc )
{
   int x = b;

   s_nChar++;

   if( s_nChar > 1 )
      fprintf( yyc, ", " );

   if( s_nChar == 9 )
   {
      fprintf( yyc, "\n      " );
      s_nChar = 1;
   }

   fprintf( yyc, "0x%02X", x );
}

static void hb_fputs( char * szName, FILE * yyc )
{
   unsigned int nPos = 0;
   while( nPos < strlen( szName ) )
      hb_fputc( szName[ nPos++ ], yyc );
}

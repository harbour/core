/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Harbour Portable Object (.HRB) generation
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
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

void hb_compGenPortObj( PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX ];
   PFUNCTION pFunc /*= hb_comp_functions.pFirst */;
   PCOMSYMBOL pSym = hb_comp_symbols.pFirst;
   ULONG lPCodePos;
   LONG lSymbols;
   ULONG ulCodeLength;
   FILE * yyc;             /* file handle for C output */

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".hrb";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = fopen( szFileName, "wb" );
   if( ! yyc )
   {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! hb_comp_bQuiet )
   {
      printf( "Generating Harbour Portable Object output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   /* writes the symbol table */

   lSymbols = 0;                /* Count number of symbols */
   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }
   fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   pSym = hb_comp_symbols.pFirst;
   while( pSym )
   {
      fputs( pSym->szName, yyc );
      fputc( 0, yyc );
      if( pSym->cScope != HB_FS_MESSAGE )
         fputc( pSym->cScope, yyc );
      else
         fputc( 0, yyc );

      /* specify the function address if it is a defined function or a
         external called function */
      if( hb_compFunctionFind( pSym->szName ) ) /* is it a defined function ? */
      {
         fputc( SYM_FUNC, yyc );
      }
      else
      {
         if( hb_compFunCallFind( pSym->szName ) )
         {
            fputc( SYM_EXTERN, yyc );
         }
         else
         {
            fputc( SYM_NOLINK, yyc );
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
   fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), yyc ); /* Write number symbols */
   fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), yyc );
   fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), yyc );

   /* Generate functions data
    */
   pFunc = hb_comp_functions.pFirst;
   if( ! hb_comp_bStartProc )
      pFunc = pFunc->pNext; /* No implicit starting procedure */

   while( pFunc )
   {
      fputs( pFunc->szName, yyc );
      fputc( 0, yyc );
      ulCodeLength = pFunc->lPCodePos;
      fputc( ( BYTE ) ( ( ulCodeLength       ) & 255 ), yyc ); /* Write size */
      fputc( ( BYTE ) ( ( ulCodeLength >> 8  ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 16 ) & 255 ), yyc );
      fputc( ( BYTE ) ( ( ulCodeLength >> 24 ) & 255 ), yyc );

/*      printf( "Creating output for %s\n", pFunc->szName ); */

      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
         fputc( pFunc->pCode[ lPCodePos++ ], yyc );

      pFunc = pFunc->pNext;
   }

   fclose( yyc );

   if( ! hb_comp_bQuiet )
      printf( "Done.\n" );
}


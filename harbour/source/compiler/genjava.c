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

#include "hbcomp.h"

#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

static int hb_fputc( BYTE b, FILE * fOut, int nChar )
{
   if( ++nChar > 1 )
      fprintf( fOut, ", " );

   if( nChar == 9 )
   {
      fprintf( fOut, "\n      " );
      nChar = 1;
   }

   fprintf( fOut, "0x%02X", ( int ) b );

   return nChar;
}

static int hb_fputs( char * szName, FILE * fOut, int nChar )
{
   unsigned int nPos = 0;

   while( nPos < strlen( szName ) )
      nChar = hb_fputc( szName[ nPos++ ], fOut, nChar );

   return nChar;
}

void hb_compGenJava( HB_COMP_DECL, PHB_FNAME pFileName )
{
   char szFileName[ _POSIX_PATH_MAX + 1 ], * szVer;
   PFUNCTION pFunc /*= HB_COMP_PARAM->functions.pFirst */;
   PCOMSYMBOL pSym = HB_COMP_PARAM->symbols.pFirst;
   ULONG lPCodePos;
   LONG lSymbols;
   ULONG ulCodeLength;
   FILE * fOut;
   int nChar;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".java";
   hb_fsFNameMerge( szFileName, pFileName );

   fOut = fopen( szFileName, "wb" );
   if( ! fOut )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! HB_COMP_PARAM->fQuiet )
   {
      printf( "Generating Java source output to \'%s\'... ", szFileName );
      fflush( stdout );
   }

   nChar = 0;
   szVer = hb_verHarbour();
   fprintf( fOut, "/*\n * %s\n * Generated JAVA source code\n */\n\n", szVer );
   hb_xfree( szVer );

   fprintf( fOut, "public class %s\n", pFileName->szName );
   fprintf( fOut, "{\n" );
   fprintf( fOut, "   public static int[] pCode =\n" );
   fprintf( fOut, "   {\n" );
   fprintf( fOut, "      " );

   /* writes the symbol table */

   lSymbols = 0;                /* Count number of symbols */
   while( pSym )
   {
      lSymbols++;
      pSym = pSym->pNext;
   }
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), fOut, nChar ); /* Write number symbols */
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), fOut, nChar );
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), fOut, nChar );
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), fOut, nChar );

   pSym = HB_COMP_PARAM->symbols.pFirst;
   while( pSym )
   {
      nChar = hb_fputs( pSym->szName, fOut, nChar );
      nChar = hb_fputc( 0, fOut, nChar );
      nChar = hb_fputc( pSym->cScope, fOut, nChar );

      /* specify the function address if it is a defined function or a
         external called function */
      if( hb_compFunctionFind( HB_COMP_PARAM, pSym->szName ) ) /* is it a defined function ? */
         nChar = hb_fputc( SYM_FUNC, fOut, nChar );
      else
      {
         if( hb_compFunCallFind( HB_COMP_PARAM, pSym->szName ) )
            nChar = hb_fputc( SYM_EXTERN, fOut, nChar );
         else
            nChar = hb_fputc( SYM_NOLINK, fOut, nChar );
      }
      pSym = pSym->pNext;
   }

   pFunc = HB_COMP_PARAM->functions.pFirst;

   lSymbols = 0;                /* Count number of symbols */
   while( pFunc )
   {
      lSymbols++;
      pFunc = pFunc->pNext;
   }
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols       ) & 255 ), fOut, nChar ); /* Write number symbols */
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 8  ) & 255 ), fOut, nChar );
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 16 ) & 255 ), fOut, nChar );
   nChar = hb_fputc( ( BYTE ) ( ( lSymbols >> 24 ) & 255 ), fOut, nChar );

   /* Generate functions data
    */
   pFunc = HB_COMP_PARAM->functions.pFirst;
   if( ! HB_COMP_PARAM->fStartProc )
      pFunc = pFunc->pNext;

   while( pFunc )
   {
      nChar = hb_fputs( pFunc->szName, fOut, nChar );
      nChar = hb_fputc( 0, fOut, nChar );
      ulCodeLength = pFunc->lPCodePos;
      nChar = hb_fputc( ( BYTE ) ( ( ulCodeLength       ) & 255 ), fOut, nChar ); /* Write size */
      nChar = hb_fputc( ( BYTE ) ( ( ulCodeLength >> 8  ) & 255 ), fOut, nChar );
      nChar = hb_fputc( ( BYTE ) ( ( ulCodeLength >> 16 ) & 255 ), fOut, nChar );
      nChar = hb_fputc( ( BYTE ) ( ( ulCodeLength >> 24 ) & 255 ), fOut, nChar );

      lPCodePos = 0;
      while( lPCodePos < pFunc->lPCodePos )
         nChar = hb_fputc( pFunc->pCode[ lPCodePos++ ], fOut, nChar );

      pFunc = pFunc->pNext;
   }

   fprintf( fOut, "\n   };\n\n" );
   fprintf( fOut, "   static public void main( String argv[] )\n" );
   fprintf( fOut, "   {\n" );
   fprintf( fOut, "      Harbour.Run( %s.pCode ); \n", pFileName->szName );
   fprintf( fOut, "   }\n\n" );
   fprintf( fOut, "}\n" );

   fclose( fOut );

   if( ! HB_COMP_PARAM->fQuiet )
      printf( "Done.\n" );
}

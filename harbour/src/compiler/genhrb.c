/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Harbour Portable Object (.hrb) generation
 *
 * Copyright 1999 Eddie Runia <eddie@runia.com>
 * www - http://harbour-project.org
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    rewritten to work on memory buffers and with new compiler code
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

#define SYM_NOLINK    0             /* symbol does not have to be linked */
#define SYM_FUNC      1             /* function defined in this module   */
#define SYM_EXTERN    2             /* function defined in other module  */
#define SYM_DEFERRED  3             /* lately bound function             */

static HB_SIZE hb_compHrbSize( HB_COMP_DECL, HB_ULONG * pulSymbols, HB_ULONG * pulFunctions )
{
   PFUNCTION pFunc;
   PCOMSYMBOL pSym;
   HB_SIZE nSize;

   *pulSymbols = *pulFunctions = 0;

   /* count total size */
   nSize = 10;  /* signature[4] + version[2] + symbols_number[4] */
   pSym = HB_COMP_PARAM->symbols.pFirst;
   while( pSym )
   {
      ( *pulSymbols )++;
      nSize += strlen( pSym->szName ) + 3; /* \0 + symscope[1] + symtype[1] */
      pSym = pSym->pNext;
   }
   nSize += 4; /* functions_number[4] */
   /* Generate functions data */
   pFunc = HB_COMP_PARAM->functions.pFirst;
   while( pFunc )
   {
      if( ( pFunc->funFlags & FUN_FILE_DECL ) == 0 )
      {
         ( *pulFunctions )++;
         nSize += strlen( pFunc->szName ) + 5 + pFunc->nPCodePos; /* \0 + func_size[4] + function_body */
      }
      pFunc = pFunc->pNext;
   }

   return nSize;
}

void hb_compGenBufPortObj( HB_COMP_DECL, HB_BYTE ** pBufPtr, HB_SIZE * pnSize )
{
   PFUNCTION pFunc;
   PCOMSYMBOL pSym;
   HB_ULONG ulSymbols, ulFunctions;
   HB_SIZE nLen;
   HB_BYTE * ptr;

   *pnSize = hb_compHrbSize( HB_COMP_PARAM, &ulSymbols, &ulFunctions );
   /* additional 0 byte is for passing buffer directly as string item */
   ptr = *pBufPtr = ( HB_BYTE * ) hb_xgrab( *pnSize + 1 );

   /* signature */
   *ptr++ = 0xC0;
   *ptr++ = 'H';
   *ptr++ = 'R';
   *ptr++ = 'B';
   HB_PUT_LE_UINT16( ptr, 2 );   /* version number */
   ptr += 2;

   HB_PUT_LE_UINT32( ptr, ulSymbols ); /* number of symbols */
   ptr += 4;
   /* generate the symbol table */
   pSym = HB_COMP_PARAM->symbols.pFirst;
   while( pSym )
   {
      nLen = strlen( pSym->szName ) + 1;
      memcpy( ptr, pSym->szName, nLen );
      ptr += nLen;
      /* TOFIX: this conversion strips upper byte from symbol scope
       *        Now we added workaround for it by using some strict
       *        bit order and restoring some others at runtime when
       *        .hrb file is loaded but we should create new format
       *        for .hrb files in which this field will have at least
       *        16bit [druzus]
       */
      *ptr++ = ( HB_BYTE ) pSym->cScope;
      /* symbol type */
      if( pSym->cScope & HB_FS_LOCAL )
         *ptr++ = SYM_FUNC;      /* function defined in this module */
      else if( pSym->cScope & HB_FS_DEFERRED )
         *ptr++ = SYM_DEFERRED;  /* lately bound function */
      else if( pSym->iFunc )
         *ptr++ = SYM_EXTERN;    /* external function */
      else
         *ptr++ = SYM_NOLINK;    /* other symbol */
      pSym = pSym->pNext;
   }

   HB_PUT_LE_UINT32( ptr, ulFunctions );  /* number of functions */
   ptr += 4;
   /* generate functions data */
   pFunc = HB_COMP_PARAM->functions.pFirst;
   while( pFunc )
   {
      if( ( pFunc->funFlags & FUN_FILE_DECL ) == 0 )
      {
         nLen = strlen( pFunc->szName ) + 1;
         memcpy( ptr, pFunc->szName, nLen );
         ptr += nLen;
         HB_PUT_LE_UINT32( ptr, pFunc->nPCodePos );      /* function size */
         ptr += 4;
         memcpy( ptr, pFunc->pCode, pFunc->nPCodePos );  /* function body */
         ptr += pFunc->nPCodePos;
      }
      pFunc = pFunc->pNext;
   }
}

void hb_compGenPortObj( HB_COMP_DECL, PHB_FNAME pFileName )
{
   char szFileName[ HB_PATH_MAX ];
   HB_SIZE nSize;
   HB_BYTE * pHrbBody;
   FILE * yyc;

   if( ! pFileName->szExtension )
      pFileName->szExtension = ".hrb";
   hb_fsFNameMerge( szFileName, pFileName );

   yyc = hb_fopen( szFileName, "wb" );
   if( ! yyc )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
   }

   if( ! HB_COMP_PARAM->fQuiet )
   {
      char buffer[ 80 + HB_PATH_MAX - 1 ];
      hb_snprintf( buffer, sizeof( buffer ),
                   "Generating Harbour Portable Object output to \'%s\'... ", szFileName );
      hb_compOutStd( HB_COMP_PARAM, buffer );
   }

   hb_compGenBufPortObj( HB_COMP_PARAM, &pHrbBody, &nSize );

   if( fwrite( pHrbBody, nSize, 1, yyc ) != 1 )
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_FILE_WRITE, szFileName, NULL );

   hb_xfree( pHrbBody );

   fclose( yyc );

   if( ! HB_COMP_PARAM->fQuiet )
      hb_compOutStd( HB_COMP_PARAM, "Done.\n" );
}

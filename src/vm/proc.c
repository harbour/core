/*
 * Harbour Project source code:
 * ProcName(), ProcLine() and ProcFile() functions
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 1999-2001 Viktor Szakats (vszakats.net/harbour)
 *    ProcFile()
 *
 * Copyright 2001 JFL (Mafact) <jfl@mafact.com>
 *    Adding the MethodName() just calling ProcName()
 *    Special treatment in case of Object and Eval (only for methodname)
 *    skipping block and adding (b) before the method name
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbvmint.h"
#include "hbapi.h"
#include "hbapicls.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"

HB_FUNC( HB_METHODNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procname( hb_parni( 1 ) + 1, szName, HB_TRUE ) );
}

HB_FUNC( PROCNAME )
{
   char szName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];

   hb_retc( hb_procname( hb_parni( 1 ) + 1, szName, HB_FALSE ) );
}

HB_FUNC( PROCLINE )
{
   HB_ISIZ nOffset = hb_stackBaseProcOffset( hb_parni( 1 ) + 1 );

   if( nOffset > 0 )
      hb_retni( hb_stackItem( nOffset )->item.asSymbol.stackstate->uiLineNo );
   else
      hb_retni( 0 );
}

#ifdef HB_CLP_UNDOC

/* NOTE: Clipper undocumented function, which always returns an empty
         string. [vszakats] */

HB_FUNC( PROCFILE )
{
#ifndef HB_CLP_STRICT
   PHB_SYMB pSym = NULL;

   if( HB_ISSYMBOL( 1 ) )
   {
      pSym = hb_itemGetSymbol( hb_param( 1, HB_IT_SYMBOL ) );
   }
   else if( HB_ISCHAR( 1 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymFindName( hb_parc( 1 ) );

      if( pDynSym )
         pSym = pDynSym->pSymbol;
   }
   else
   {
      HB_ISIZ nOffset = hb_stackBaseProcOffset( hb_parni( 1 ) + 1 );

      if( nOffset > 0 )
      {
         PHB_ITEM pBase = hb_stackItem( nOffset );

         pSym = pBase->item.asSymbol.value;
         if( pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym )
         {
            PHB_ITEM pSelf = hb_stackItem( nOffset + 1 );

            if( HB_IS_BLOCK( pSelf ) )
               pSym = pSelf->item.asBlock.value->pDefSymb;
            else if( pBase->item.asSymbol.stackstate->uiClass )
               pSym = hb_clsMethodSym( pBase );
         }
         else if( pBase->item.asSymbol.stackstate->uiClass )
            pSym = hb_clsMethodSym( pBase );
      }
   }
   hb_retc( hb_vmFindModuleSymbolName( hb_vmGetRealFuncSym( pSym ) ) );
#else
   hb_retc_null();
#endif
}

#endif

/* NOTE: szName size must be an at least:
         HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 [vszakats] */
#define HB_PROCBUF_LEN  ( HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 4 )
char * hb_procname( int iLevel, char * szName, HB_BOOL fMethodName )
{
   HB_ISIZ nOffset = hb_stackBaseProcOffset( iLevel );

   szName[ 0 ] = '\0';
   if( nOffset > 0 )
   {
      PHB_ITEM pBase, pSelf;

      pBase = hb_stackItem( nOffset );
      pSelf = hb_stackItem( nOffset + 1 );

      if( fMethodName && nOffset > 0 &&
          pBase->item.asSymbol.value == &hb_symEval &&
          pBase->item.asSymbol.stackstate->uiClass )
      {
         HB_ISIZ nPrevOffset = hb_stackItem( nOffset )->item.asSymbol.stackstate->nBaseItem;

         if( hb_stackItem( nPrevOffset )->item.asSymbol.stackstate->uiClass ==
             pBase->item.asSymbol.stackstate->uiClass &&
             hb_stackItem( nPrevOffset )->item.asSymbol.stackstate->uiMethod ==
             pBase->item.asSymbol.stackstate->uiMethod )
         {
            pBase = hb_stackItem( nPrevOffset );
            pSelf = hb_stackItem( nPrevOffset + 1 );
         }
      }

      if( pBase->item.asSymbol.value == &hb_symEval ||
          pBase->item.asSymbol.value->pDynSym == hb_symEval.pDynSym )
      {
         hb_strncat( szName, "(b)", HB_PROCBUF_LEN );
         /* it is a method name? */
         if( fMethodName && pBase->item.asSymbol.stackstate->uiClass )
         {
            hb_strncat( szName, hb_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                        HB_PROCBUF_LEN );
            hb_strncat( szName, ":", HB_PROCBUF_LEN );
            hb_strncat( szName, hb_clsMethodName( pBase->item.asSymbol.stackstate->uiClass,
                                                  pBase->item.asSymbol.stackstate->uiMethod ), HB_PROCBUF_LEN );
         }
         else if( HB_IS_BLOCK( pSelf ) )
            hb_strncat( szName, pSelf->item.asBlock.value->pDefSymb->szName,
                        HB_PROCBUF_LEN );
         else if( HB_IS_SYMBOL( pSelf ) )
            hb_strncpy( szName, pSelf->item.asSymbol.value->szName, HB_PROCBUF_LEN );
         else
            hb_strncat( szName, pBase->item.asSymbol.value->szName, HB_PROCBUF_LEN );
      }
      else
      {
         /* it is a method name? */
         if( pBase->item.asSymbol.stackstate->uiClass )
         {
            hb_strncat( szName, hb_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                        HB_PROCBUF_LEN );
            hb_strncat( szName, ":", HB_PROCBUF_LEN );
         }
         hb_strncat( szName, pBase->item.asSymbol.value->szName, HB_PROCBUF_LEN );
      }
   }

   return szName;
}

/* NOTE: szName size must be an at least:
 *          HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5
 *       szFile szie must be an at least:
 *          HB_PATH_MAX
 */
HB_BOOL hb_procinfo( int iLevel, char * szName, HB_USHORT * puiLine, char * szFile )
{
   HB_ISIZ nOffset = hb_stackBaseProcOffset( iLevel );

   if( nOffset > 0 )
   {
      PHB_ITEM pBase, pSelf;
      PHB_SYMB pSym;

      pBase = hb_stackItem( nOffset );
      pSelf = hb_stackItem( nOffset + 1 );

      pSym = pBase->item.asSymbol.value;

      if( szName )
      {
         szName[ 0 ] = '\0';
         if( pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym )
         {
            hb_strncat( szName, "(b)", HB_PROCBUF_LEN );

            if( HB_IS_BLOCK( pSelf ) )
               hb_strncat( szName, pSelf->item.asBlock.value->pDefSymb->szName,
                           HB_PROCBUF_LEN );
            else
               hb_strncat( szName, pSym->szName, HB_PROCBUF_LEN );
         }
         else
         {
            if( pBase->item.asSymbol.stackstate->uiClass ) /* it is a method name */
            {
               hb_strncat( szName, hb_clsName( pBase->item.asSymbol.stackstate->uiClass ),
                           HB_PROCBUF_LEN );
               hb_strncat( szName, ":", HB_PROCBUF_LEN );
            }
            hb_strncat( szName, pSym->szName, HB_PROCBUF_LEN );
         }
      }

      if( puiLine )
         *puiLine = pBase->item.asSymbol.stackstate->uiLineNo;

      if( szFile )
      {
         const char * szModule;

         if( HB_IS_BLOCK( pSelf ) &&
             ( pSym == &hb_symEval || pSym->pDynSym == hb_symEval.pDynSym ) )
            pSym = pSelf->item.asBlock.value->pDefSymb;
         else if( pBase->item.asSymbol.stackstate->uiClass )
            pSym = hb_clsMethodSym( pBase );

         szModule = hb_vmFindModuleSymbolName( hb_vmGetRealFuncSym( pSym ) );

         if( szModule )
            hb_strncpy( szFile, szModule, HB_PATH_MAX - 1 );
         else
            szFile[ 0 ] = '\0';
      }

      return HB_TRUE;
   }

   if( szName )
      szName[ 0 ] = '\0';
   if( puiLine )
      *puiLine = 0;
   if( szFile )
      szFile[ 0 ] = '\0';

   return HB_FALSE;
}

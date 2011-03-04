/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    backward compatible old .prg interface to preprocessor
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbinit.h"

#ifdef HB_LEGACY_LEVEL3

HB_FUNC_EXTERN( __PP_INIT );
HB_FUNC_EXTERN( __PP_ADDRULE );
HB_FUNC_EXTERN( __PP_PROCESS );
HB_FUNC_EXTERN( __PP_STDRULES );

HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_PPLIB3 )
{ "__PP_INIT",     {HB_FS_PUBLIC}, {HB_FUNCNAME( __PP_INIT )},     NULL },
{ "__PP_ADDRULE",  {HB_FS_PUBLIC}, {HB_FUNCNAME( __PP_ADDRULE )},  NULL },
{ "__PP_PROCESS",  {HB_FS_PUBLIC}, {HB_FUNCNAME( __PP_PROCESS )},  NULL },
{ "__PP_STDRULES", {HB_FS_PUBLIC}, {HB_FUNCNAME( __PP_STDRULES )}, NULL },
HB_INIT_SYMBOLS_EX_END( hb_vm_SymbolInit_PPLIB3, "pplib3.c", 0, 0 )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup hb_vm_SymbolInit_PPLIB3
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( hb_vm_SymbolInit_PPLIB3 )
   #include "hbiniseg.h"
#endif


static PHB_ITEM s_pp = NULL;

static PHB_ITEM hb_pp_Get( void )
{
   static PHB_DYNS s_pDynSym = NULL;

   if( s_pp == NULL )
   {
      if( s_pDynSym == NULL )
         s_pDynSym = hb_dynsymFind( "__PP_INIT" );
      if( s_pDynSym )
      {
         hb_vmPushDynSym( s_pDynSym );
         hb_vmPushNil();
         hb_vmProc( 0 );
      }
      if( hb_param( -1, HB_IT_POINTER ) )
         s_pp = hb_itemNew( hb_param( -1, HB_IT_POINTER ) );
   }

   return s_pp;
}

HB_FUNC( __PPADDRULE )
{
   static PHB_DYNS s_pDynSym = NULL;
   PHB_ITEM pp = hb_pp_Get(), pLine = hb_param( 1, HB_IT_ANY );

   if( pp )
   {
      if( s_pDynSym == NULL )
         s_pDynSym = hb_dynsymFind( "__PP_ADDRULE" );
      if( s_pDynSym )
      {
         hb_vmPushDynSym( s_pDynSym );
         hb_vmPushNil();
         hb_vmPush( pp );
         if( pLine )
            hb_vmPush( pLine );
         else
            hb_vmPushNil();
         hb_vmProc( 2 );
      }
   }
}

HB_FUNC( __PREPROCESS )
{
   static PHB_DYNS s_pDynSym = NULL;
   PHB_ITEM pp = hb_pp_Get(), pLine = hb_param( 1, HB_IT_ANY );

   if( pp )
   {
      if( s_pDynSym == NULL )
         s_pDynSym = hb_dynsymFind( "__PP_PROCESS" );
      if( s_pDynSym )
      {
         hb_vmPushDynSym( s_pDynSym );
         hb_vmPushNil();
         hb_vmPush( pp );
         if( pLine )
            hb_vmPush( pLine );
         else
            hb_vmPushNil();
         hb_vmProc( 2 );
      }
   }
}

HB_FUNC( __PP_FREE )
{
   if( s_pp )
   {
      hb_itemRelease( s_pp );
      s_pp = NULL;
   }
}

#endif /* HB_LEGACY_LEVEL3 */

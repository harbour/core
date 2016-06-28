/*
 * LIBXDIFF functions wrapper
 *
 * Copyright 2010 Petr Chornyj <myorg63@mail.ru>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbinit.h"
#include "hbstack.h"
#include "hbvm.h"

#include "xdiff.h"

#define HB_MMF_SIGN                         8000001

#define HB_ERR_MEMSTRU_NOT_MEM_BLOCK        4001
#define HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK  4002
#define HB_ERR_MEMSTRU_DESTROYED            4003

#define XDLT_STD_BLKSIZE                    ( 1024 * 8 )
#define XDLT_MAX_LINE_SIZE                  80

static PHB_ITEM hb_mmf_itemPut( PHB_ITEM pItem, void * pMemAddr, int iType );
static void *   hb_mmf_itemGet( PHB_ITEM pItem, int iType, HB_BOOL fError );
static void     hb_mmf_ret( void * pMemAddr, int iType );
static void *   hb_mmf_param( int iParam, int iType, HB_BOOL fError );

static void xdiff_init( void );

typedef struct
{
   mmfile_t * mmf;
} HB_MMF, * PHB_MMF;

typedef struct
{
   int      type;
   HB_MMF * hb_mmf;
} HB_MMF_HOLDER, * PHB_MMF_HOLDER;

static HB_GARBAGE_FUNC( hb_mmf_destructor )
{
   PHB_MMF_HOLDER pStructHolder = ( PHB_MMF_HOLDER ) Cargo;

   if( pStructHolder->hb_mmf )
   {
      if( pStructHolder->hb_mmf->mmf )
      {
         xdl_free_mmfile( pStructHolder->hb_mmf->mmf );
         hb_xfree( pStructHolder->hb_mmf->mmf );

         pStructHolder->hb_mmf->mmf = NULL;
      }
      hb_xfree( pStructHolder->hb_mmf );
      pStructHolder->hb_mmf = NULL;
   }
}

static const HB_GC_FUNCS s_gc_xdiffFuncs =
{
   hb_mmf_destructor,
   hb_gcDummyMark
};

static PHB_ITEM hb_mmf_itemPut( PHB_ITEM pItem, void * pMemAddr, int iType )
{
   PHB_MMF_HOLDER pStructHolder;

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( pItem );

   pStructHolder = ( PHB_MMF_HOLDER ) hb_gcAllocate( sizeof( HB_MMF_HOLDER ),
                                                     &s_gc_xdiffFuncs );
   pStructHolder->hb_mmf = ( HB_MMF * ) pMemAddr;
   pStructHolder->type   = iType;

   return hb_itemPutPtrGC( pItem, pStructHolder );
}

static void * hb_mmf_itemGet( PHB_ITEM pItem, int iType, HB_BOOL fError )
{
   PHB_MMF_HOLDER pStructHolder = ( PHB_MMF_HOLDER ) hb_itemGetPtrGC( pItem,
                                                                      &s_gc_xdiffFuncs );
   int iError = 0;

   HB_SYMBOL_UNUSED( iError );

   if( ! pStructHolder )
      iError = HB_ERR_MEMSTRU_NOT_MEM_BLOCK;
   else if( pStructHolder->type != iType )
      iError = HB_ERR_MEMSTRU_WRONG_MEMSTRU_BLOCK;
   else if( ! pStructHolder->hb_mmf )
      iError = HB_ERR_MEMSTRU_DESTROYED;
   else
      return pStructHolder->hb_mmf;

   if( fError )
      hb_errRT_BASE_SubstR( EG_ARG, iError, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   return NULL;
}

static void hb_mmf_ret( void * pMemAddr, int iType )
{
   hb_mmf_itemPut( hb_stackReturnItem(), pMemAddr, iType );
}

static void * hb_mmf_param( int iParam, int iType, HB_BOOL fError )
{
   return hb_mmf_itemGet( hb_param( iParam, HB_IT_POINTER ), iType, fError );
}

/* int xdl_init_mmfile(mmfile_t *mmf, long bsize, unsigned long flags) */

HB_FUNC( XDL_INIT_MMFILE )
{
   mmfile_t * mmf = ( mmfile_t * ) hb_xgrab( sizeof( mmfile_t ) );

   if( xdl_init_mmfile( mmf,
                        hb_parnldef( 1, XDLT_STD_BLKSIZE ), ( unsigned long ) hb_parnl( 3 ) ) == 0 )
   {
      HB_MMF * phb_mmf;

      phb_mmf = ( HB_MMF * ) hb_xgrabz( sizeof( HB_MMF ) );
      phb_mmf->mmf = mmf;
      hb_mmf_ret( phb_mmf, HB_MMF_SIGN );
   }
   else
      hb_xfree( mmf );
}

#if 0
HB_FUNC( XDL_FREE_MMFILE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
   {
      xdl_free_mmfile( phb_mmf->mmf );
      phb_mmf->mmf = NULL;
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}
#endif

/* int xdl_mmfile_iscompact(mmfile_t *mmf) */

HB_FUNC( XDL_MMFILE_ISCOMPACT )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
      hb_retl( xdl_mmfile_iscompact( phb_mmf->mmf ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int xdl_seek_mmfile(mmfile_t *mmf, long off) */

HB_FUNC( XDL_SEEK_MMFILE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
      hb_retni( xdl_seek_mmfile( phb_mmf->mmf, hb_parnldef( 2, 0 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* long xdl_read_mmfile(mmfile_t *mmf, void *data, long size) */

HB_FUNC( XDL_READ_MMFILE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
   {
      PHB_ITEM pData = HB_ISBYREF( 2 ) ? hb_param( 2, HB_IT_STRING ) : NULL;
      char *   data;
      HB_SIZE  size;

      if( pData )
      {
         if( ! hb_itemGetWriteCL( pData, &data, &size ) )
            data = NULL;
      }
      else
      {
         size = ( HB_ISNUM( 3 ) && hb_parns( 3 ) >= 0 ) ?
                hb_parns( 3 ) : xdl_mmfile_size( phb_mmf->mmf );

         data = ( char * ) hb_xalloc( size + 1 );
      }

      if( data && size )
      {
         long lResult = xdl_read_mmfile( phb_mmf->mmf, data, ( long ) size );

         if( lResult == -1 )
         {
            hb_retc_null();
            hb_stornl( -1, 4 );
         }
         else
         {
            hb_stornl( lResult, 4 );

            if( pData )
               hb_retclen( data, lResult );
            else
               hb_retclen_buffer( data, lResult );
         }
      }
      else
      {
         hb_retc_null();
         hb_stornl( -1, 4 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* long xdl_write_mmfile(mmfile_t *mmf, void const *data, long size) */

HB_FUNC( XDL_WRITE_MMFILE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
   {
      if( HB_ISCHAR( 2 ) )
      {
         long lSize = ( long ) hb_parclen( 2 );

         if( hb_pcount() > 2 )
            lSize = hb_parnldef( 3, lSize );

         hb_retnl( xdl_write_mmfile( phb_mmf->mmf, hb_parcx( 2 ), lSize ) );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   long xdl_writem_mmfile(mmfile_t *mmf, mmbuffer_t *mb, int nbuf);
   void *xdl_mmfile_writeallocate(mmfile_t *mmf, long size);
   long xdl_mmfile_ptradd(mmfile_t *mmf, char *ptr, long size, unsigned long flags);
   void *xdl_mmfile_first(mmfile_t *mmf, long *size);
   void *xdl_mmfile_next(mmfile_t *mmf, long *size);
 */

/* long xdl_mmfile_size(mmfile_t *mmf) */

HB_FUNC( XDL_MMFILE_SIZE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
      hb_retnl( xdl_mmfile_size( phb_mmf->mmf ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int xdl_mmfile_cmp(mmfile_t *mmf1, mmfile_t *mmf2) */

HB_FUNC( XDL_MMFILE_CMP )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
      hb_retl( xdl_mmfile_cmp( phb_mmf1->mmf, phb_mmf2->mmf ) == 0 );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   int xdl_mmfile_compact(mmfile_t *mmfo, mmfile_t *mmfc, long bsize, unsigned long flags);
 */

HB_FUNC( XDL_MMFILE_COMPACT )
{
   HB_MMF * phb_mmfo = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmfo )
   {
      mmfile_t * mmfc = ( mmfile_t * ) hb_xgrab( sizeof( mmfile_t ) );

      if( xdl_mmfile_compact( phb_mmfo->mmf, mmfc, hb_parnldef( 1, XDLT_STD_BLKSIZE ),
                              ( unsigned long ) hb_parnl( 3 ) ) == 0 )
      {
         HB_MMF * phb_mmf;

         phb_mmf = ( HB_MMF * ) hb_xgrabz( sizeof( HB_MMF ) );
         phb_mmf->mmf = mmfc;
         hb_mmf_ret( phb_mmf, HB_MMF_SIGN );

         hb_stornl( 0, 4 );
      }
      else
      {
         hb_xfree( mmfc );
         hb_stornl( -1, 4 );
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* callbacks */

#define hb_parHandlePtr( n )  hb_fileFromHandle( hb_numToHandle( hb_parnint( n ) ) )

static int xdlt_outfile( void * priv, mmbuffer_t * mb, int nbuf )
{
   int i;

   for( i = 0; i < nbuf; i++ )
   {
      hb_fileWrite( ( PHB_FILE ) priv, mb[ i ].ptr, ( HB_SIZE ) mb[ i ].size, -1 );

      if( hb_fsError() != 0 )
         return -1;
   }
   return 0;
}

static int xdlt_outb( void * priv, mmbuffer_t * mb, int nbuf )
{
   PHB_ITEM pCallback = ( PHB_ITEM ) priv;

   if( pCallback && hb_vmRequestReenter() )
   {
      int iResult;
      int i;

      hb_vmPushEvalSym();
      hb_vmPush( pCallback );

      for( i = 0; i < nbuf; i++ )
         hb_vmPushString( ( const char * ) mb[ i ].ptr, mb[ i ].size );

      hb_vmSend( ( HB_USHORT ) nbuf );
      iResult = hb_parnidef( -1, 0 );

      hb_vmRequestRestore();

      return iResult;
   }
   else
      return -1;
}

/* int xdl_diff(mmfile_t *mmf1, mmfile_t *mmf2, xpparam_t const *xpp, xdemitconf_t const *xecfg, xdemitcb_t *ecb) */

HB_FUNC( XDL_DIFF )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xpparam_t    xpp;
      xdemitconf_t xecfg;
      xdemitcb_t   ecb;

      xpp.flags    = ( unsigned long ) hb_parnldef( 3, 0 );
      xecfg.ctxlen = hb_parnldef( 4, 3 );

      if( HB_ISNUM( 5 ) )
      {
         ecb.priv = hb_parHandlePtr( 5 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_diff( phb_mmf1->mmf, phb_mmf2->mmf, &xpp, &xecfg, &ecb ) );

         hb_fileDetach( ecb.priv );
      }
      else if( hb_fileParamGet( 5 ) )
      {
         ecb.priv = hb_fileParamGet( 5 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_diff( phb_mmf1->mmf, phb_mmf2->mmf, &xpp, &xecfg, &ecb ) );
      }
      else if( HB_ISEVALITEM( 5 ) )
      {
         PHB_ITEM pCallback = hb_param( 5, HB_IT_EVALITEM );

         ecb.priv = ( void * ) pCallback;
         ecb.outf = xdlt_outb;

         hb_retni( xdl_diff( phb_mmf1->mmf, phb_mmf2->mmf, &xpp, &xecfg, &ecb ) );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int xdl_patch(mmfile_t *mmf, mmfile_t *mmfp, int mode, xdemitcb_t *ecb, xdemitcb_t *rjecb) */

HB_FUNC( XDL_PATCH )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      if( ( HB_ISNUM( 4 ) || hb_fileParamGet( 4 ) ) &&
          ( HB_ISNUM( 5 ) || hb_fileParamGet( 5 ) ) )
      {
         int mode = hb_parnidef( 3, XDL_PATCH_NORMAL );

         xdemitcb_t ecb;
         xdemitcb_t rjecb;

         HB_BOOL lDetach_ecb = HB_FALSE;
         HB_BOOL lDetach_rjecb = HB_FALSE;

         ecb.outf = xdlt_outfile;
         rjecb.outf = xdlt_outfile;

         if( HB_ISNUM( 4 ) )
         {
            ecb.priv = hb_parHandlePtr( 4 );
            lDetach_ecb = HB_TRUE;
         }
         else
            ecb.priv = hb_fileParamGet( 4 );

         if( HB_ISNUM( 5 ) )
         {
            rjecb.priv = hb_parHandlePtr( 5 );
            lDetach_rjecb = HB_TRUE;
         }
         else
            rjecb.priv = hb_fileParamGet( 5 );

         hb_retni( xdl_patch( phb_mmf1->mmf, phb_mmf2->mmf, mode, &ecb, &rjecb ) );

         if( lDetach_ecb )
            hb_fileDetach( ecb.priv );
         if( lDetach_rjecb )
            hb_fileDetach( rjecb.priv );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   int xdl_merge3(mmfile_t *mmfo, mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb, xdemitcb_t *rjecb);
   int xdl_bdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, bdiffparam_t const *bdp, xdemitcb_t *ecb);
 */

/* int xdl_bdiff(mmfile_t *mmf1, mmfile_t *mmf2, bdiffparam_t const *bdp, xdemitcb_t *ecb) */

HB_FUNC( XDL_BDIFF )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      bdiffparam_t bdp;
      xdemitcb_t   ecb;

      bdp.bsize = hb_parnldef( 3, 32 ); /* from 16 to 64 */

      if( HB_ISNUM( 4 ) )
      {
         ecb.priv = hb_parHandlePtr( 4 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_bdiff( phb_mmf1->mmf, phb_mmf2->mmf, &bdp, &ecb ) );

         hb_fileDetach( ecb.priv );
      }
      else if( hb_fileParamGet( 4 ) )
      {
         ecb.priv = hb_fileParamGet( 4 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_bdiff( phb_mmf1->mmf, phb_mmf2->mmf, &bdp, &ecb ) );
      }
      else if( HB_ISEVALITEM( 4 ) )
      {
         PHB_ITEM pCallback = hb_param( 4, HB_IT_EVALITEM );

         ecb.priv = ( void * ) pCallback;
         ecb.outf = xdlt_outb;

         hb_retni( xdl_bdiff( phb_mmf1->mmf, phb_mmf2->mmf, &bdp, &ecb ) );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   int xdl_rabdiff_mb(mmbuffer_t *mmb1, mmbuffer_t *mmb2, xdemitcb_t *ecb);
 */

/* int xdl_rabdiff(mmfile_t *mmf1, mmfile_t *mmf2, xdemitcb_t *ecb) */

HB_FUNC( XDL_RABDIFF )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xdemitcb_t ecb;

      if( HB_ISNUM( 3 ) )
      {
         ecb.priv = hb_parHandlePtr( 3 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_rabdiff( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );

         hb_fileDetach( ecb.priv );
      }
      else if( hb_fileParam( 3 ) )
      {
         ecb.priv = hb_fileParam( 3 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_rabdiff( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );
      }
      else if( HB_ISEVALITEM( 3 ) )
      {
         PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

         ecb.priv = ( void * ) pCallback;
         ecb.outf = xdlt_outb;

         hb_retni( xdl_rabdiff( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int xdl_bpatch(mmfile_t *mmf, mmfile_t *mmfp, xdemitcb_t *ecb) */

HB_FUNC( XDL_BPATCH )
{
   HB_MMF * phb_mmf1 = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );
   HB_MMF * phb_mmf2 = ( HB_MMF * ) hb_mmf_param( 2, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf1 && phb_mmf1->mmf && phb_mmf2 && phb_mmf2->mmf )
   {
      xdemitcb_t ecb;

      if( HB_ISNUM( 3 ) )
      {
         ecb.priv = hb_parHandlePtr( 3 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_bpatch( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );

         hb_fileDetach( ecb.priv );
      }
      else if( hb_fileParam( 3 ) )
      {
         ecb.priv = hb_fileParam( 3 );
         ecb.outf = xdlt_outfile;

         hb_retni( xdl_bpatch( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );
      }
      else if( HB_ISEVALITEM( 3 ) )
      {
         PHB_ITEM pCallback = hb_param( 3, HB_IT_EVALITEM );

         ecb.priv = ( void * ) pCallback;
         ecb.outf = xdlt_outb;

         hb_retni( xdl_bpatch( phb_mmf1->mmf, phb_mmf2->mmf, &ecb ) );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* long xdl_bdiff_tgsize(mmfile_t *mmfp) */

HB_FUNC( XDL_BDIFF_TGSIZE )
{
   HB_MMF * phb_mmf = ( HB_MMF * ) hb_mmf_param( 1, HB_MMF_SIGN, HB_TRUE );

   if( phb_mmf && phb_mmf->mmf )
      hb_retnl( xdl_bdiff_tgsize( phb_mmf->mmf ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

static void * wf_malloc( void * priv, unsigned int size )
{
   HB_SYMBOL_UNUSED( priv );

   return size > 0 ? hb_xgrab( size ) : NULL;
}

static void wf_free( void * priv, void * ptr )
{
   HB_SYMBOL_UNUSED( priv );

   if( ptr )
      hb_xfree( ptr );
}

static void * wf_realloc( void * priv, void * ptr, unsigned int size )
{
   HB_SYMBOL_UNUSED( priv );

   return size > 0 ? ( ptr ? hb_xrealloc( ptr, size ) : hb_xgrab( size ) ) : NULL;
}

static void xdiff_init( void )
{
   memallocator_t malt;

   malt.priv    = NULL;
   malt.malloc  = wf_malloc;
   malt.free    = wf_free;
   malt.realloc = wf_realloc;
   xdl_set_allocator( &malt );
}

HB_CALL_ON_STARTUP_BEGIN( _xdiff_init_ )
xdiff_init();
HB_CALL_ON_STARTUP_END( _xdiff_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _xdiff_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY  HB_DATASEG_FUNC( _xdiff_init_ )
   #include "hbiniseg.h"
#endif

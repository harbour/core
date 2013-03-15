/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Cairo library: core
 *
 * Copyright 2009 Mindaugas Kavaliauskas <dbtopas at dbtopas.lt>
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

#include "hbcairo.h"
#include "hbapiitm.h"
#include "hbapierr.h"


/* ============ cairo_t * support ============ */
static HB_GARBAGE_FUNC( hb_cairo_destructor )
{
   cairo_t ** ppCairo = ( cairo_t ** ) Cargo;

   if( *ppCairo )
   {
      cairo_destroy( *ppCairo );
      *ppCairo = NULL;
   }
}

static const HB_GC_FUNCS s_gcCairoFuncs =
{
   hb_cairo_destructor,
   hb_gcDummyMark
};


cairo_t * hb_cairoItemGet( PHB_ITEM pItem )
{
   cairo_t ** ppCairo = ( cairo_t ** ) hb_itemGetPtrGC( pItem, &s_gcCairoFuncs );

   return ppCairo ? *ppCairo : NULL;
}


PHB_ITEM hb_cairoItemPut( PHB_ITEM pItem, cairo_t * pCairo )
{
   cairo_t ** ppCairo = ( cairo_t ** ) hb_gcAllocate( sizeof( cairo_t * ), &s_gcCairoFuncs );

   *ppCairo = pCairo;
   return hb_itemPutPtrGC( pItem, ppCairo );
}


cairo_t * hb_cairo_param( int iParam )
{
   cairo_t ** ppCairo = ( cairo_t ** ) hb_parptrGC( &s_gcCairoFuncs, iParam );

   if( ppCairo && *ppCairo )
      return *ppCairo;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}


void hb_cairo_ret( cairo_t * pCairo )
{
   hb_cairoItemPut( hb_stackReturnItem(), pCairo );
}


HB_FUNC( CAIRO_DESTROY )
{
   cairo_t ** ppCairo = ( cairo_t ** ) hb_parptrGC( &s_gcCairoFuncs, 1 );

   if( ppCairo && *ppCairo )
   {
      cairo_destroy( *ppCairo );
      *ppCairo = NULL;
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* ============ cairo_surface_t * support ============ */
static HB_GARBAGE_FUNC( hb_cairo_surface_destructor )
{
   cairo_surface_t ** ppSurface = ( cairo_surface_t ** ) Cargo;

   if( *ppSurface )
   {
      cairo_surface_destroy( *ppSurface );
      *ppSurface = NULL;
   }
}

static const HB_GC_FUNCS s_gcSurfaceFuncs =
{
   hb_cairo_surface_destructor,
   hb_gcDummyMark
};


cairo_surface_t * hb_cairoSurfaceItemGet( PHB_ITEM pItem )
{
   cairo_surface_t ** ppSurface = ( cairo_surface_t ** ) hb_itemGetPtrGC( pItem, &s_gcSurfaceFuncs );

   return ppSurface ? *ppSurface : NULL;
}


PHB_ITEM hb_cairoSurfaceItemPut( PHB_ITEM pItem, cairo_surface_t * pSurface )
{
   cairo_surface_t ** ppSurface = ( cairo_surface_t ** ) hb_gcAllocate( sizeof( cairo_surface_t * ), &s_gcSurfaceFuncs );

   *ppSurface = pSurface;
   return hb_itemPutPtrGC( pItem, ppSurface );
}


cairo_surface_t * hb_cairo_surface_param( int iParam )
{
   cairo_surface_t ** ppSurface = ( cairo_surface_t ** ) hb_parptrGC( &s_gcSurfaceFuncs, iParam );

   if( ppSurface && *ppSurface )
      return *ppSurface;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}


void hb_cairo_surface_ret( cairo_surface_t * pSurface )
{
   hb_cairoSurfaceItemPut( hb_stackReturnItem(), pSurface );
}


HB_FUNC( CAIRO_SURFACE_DESTROY )
{
   cairo_surface_t ** ppSurface = ( cairo_surface_t ** ) hb_parptrGC( &s_gcSurfaceFuncs, 1 );

   if( ppSurface && *ppSurface )
   {
      cairo_surface_destroy( *ppSurface );
      *ppSurface = NULL;
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* ============ cairo_path_t * support ============ */
static HB_GARBAGE_FUNC( hb_cairo_path_destructor )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) Cargo;

   if( *ppPath )
   {
      cairo_path_destroy( *ppPath );
      *ppPath = NULL;
   }
}

static const HB_GC_FUNCS s_gcPathFuncs =
{
   hb_cairo_path_destructor,
   hb_gcDummyMark
};


cairo_path_t * hb_cairoPathItemGet( PHB_ITEM pItem )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) hb_itemGetPtrGC( pItem, &s_gcPathFuncs );

   return ppPath ? *ppPath : NULL;
}


PHB_ITEM hb_cairoPathItemPut( PHB_ITEM pItem, cairo_path_t * pPath )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) hb_gcAllocate( sizeof( cairo_path_t * ), &s_gcPathFuncs );

   *ppPath = pPath;
   return hb_itemPutPtrGC( pItem, ppPath );
}


cairo_path_t * hb_cairo_path_param( int iParam )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) hb_parptrGC( &s_gcPathFuncs, iParam );

   if( ppPath && *ppPath )
      return *ppPath;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}


void hb_cairo_path_ret( cairo_path_t * pPath )
{
   hb_cairoPathItemPut( hb_stackReturnItem(), pPath );
}


HB_FUNC( CAIRO_PATH_DESTROY )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) hb_parptrGC( &s_gcPathFuncs, 1 );

   if( ppPath && *ppPath )
   {
      cairo_path_destroy( *ppPath );
      *ppPath = NULL;
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


/* ============ cairo_path_t * iterator support ============ */

/*
 * NOTE: Path iterator functions are is not cairo functions.
 *       This is only a way to pass path data to .prg level
 */

typedef struct
{
   cairo_path_t ** ppPath;
   int iPos;
} HB_CAIRO_PATH_ITERATOR, * PHB_CAIRO_PATH_ITERATOR;


static HB_GARBAGE_FUNC( hb_cairo_path_iterator_destructor )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) Cargo;

   if( pIterator->ppPath )
   {
      hb_gcRefFree( pIterator->ppPath );
      pIterator->ppPath = NULL;
   }
}


static HB_GARBAGE_FUNC( hb_cairo_path_iterator_mark )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) Cargo;

   if( pIterator->ppPath )
      hb_gcMark( pIterator->ppPath );
}


static const HB_GC_FUNCS s_gcIteratorFuncs =
{
   hb_cairo_path_iterator_destructor,
   hb_cairo_path_iterator_mark
};


HB_FUNC( CAIRO_PATH_ITERATOR_CREATE )
{
   cairo_path_t ** ppPath = ( cairo_path_t ** ) hb_parptrGC( &s_gcPathFuncs, 1 );

   if( ppPath && *ppPath )
   {
      PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) hb_gcAllocate( sizeof( HB_CAIRO_PATH_ITERATOR ), &s_gcIteratorFuncs );
      pIterator->ppPath = ppPath;
      hb_gcRefInc( ppPath );
      pIterator->iPos = -1;
      hb_itemPutPtrGC( hb_stackReturnItem(), pIterator );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( CAIRO_PATH_ITERATOR_DESTROY )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) hb_parptrGC( &s_gcIteratorFuncs, 1 );

   if( pIterator && pIterator->ppPath )
   {
      hb_gcRefFree( pIterator->ppPath );
      pIterator->ppPath = NULL;
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( CAIRO_PATH_ITERATOR_NEXT )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) hb_parptrGC( &s_gcIteratorFuncs, 1 );
   cairo_path_t *          pPath;

   if( pIterator && pIterator->ppPath && ( pPath = *( pIterator->ppPath ) ) != NULL )
   {
      /* Skip */
      if( pIterator->iPos == -1 )
         pIterator->iPos = 0;
      else if( pIterator->iPos < pPath->num_data )
         pIterator->iPos += pPath->data[ pIterator->iPos ].header.length;

      /* return type */
      if( pIterator->iPos < pPath->num_data )
         hb_retni( pPath->data[ pIterator->iPos ].header.type );
      else
         hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( CAIRO_PATH_ITERATOR_GET_POINTS )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) hb_parptrGC( &s_gcIteratorFuncs, 1 );
   cairo_path_t *          pPath;

   if( pIterator && pIterator->ppPath && ( pPath = *( pIterator->ppPath ) ) != NULL )
   {
      cairo_path_data_t * pData;

      if( pIterator->iPos < pPath->num_data && pIterator->iPos != -1 )
      {
         PHB_ITEM pItem, pArray;
         int      i;

         pData  = pPath->data + pIterator->iPos;
         pArray = hb_itemArrayNew( pData->header.length - 1 );
         for( i = 1; i < pData->header.length; i++ )
         {
            hb_arrayNew( pItem = hb_arrayGetItemPtr( pArray, i ), 2 );
            hb_arraySetND( pItem, 1, pData[ i ].point.x );
            hb_arraySetND( pItem, 2, pData[ i ].point.y );
         }
         hb_itemReturnRelease( pArray );
      }
      else
         hb_ret();
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}


HB_FUNC( CAIRO_PATH_ITERATOR_SET_POINTS )
{
   PHB_CAIRO_PATH_ITERATOR pIterator = ( PHB_CAIRO_PATH_ITERATOR ) hb_parptrGC( &s_gcIteratorFuncs, 1 );
   PHB_ITEM pArray = hb_param( 2, HB_IT_ARRAY );
   cairo_path_t * pPath;

   if( pIterator && pIterator->ppPath && ( pPath = *( pIterator->ppPath ) ) != NULL && pArray )
   {
      cairo_path_data_t * pData;
      HB_SIZE nLen;

      nLen = hb_arrayLen( pArray );
      if( pIterator->iPos < pPath->num_data && pIterator->iPos != -1 &&
          ( HB_SIZE ) pPath->data[ pIterator->iPos ].header.length == nLen + 1 )
      {
         PHB_ITEM pItem;
         int      i;

         pData = pPath->data + pIterator->iPos;
         for( i = 1; i < pData->header.length; i++ )
         {
            pItem = hb_arrayGetItemPtr( pArray, i );
            if( hb_arrayLen( pItem ) == 2 )
            {
               pData[ i ].point.x = hb_arrayGetND( pItem, 1 );
               pData[ i ].point.y = hb_arrayGetND( pItem, 2 );
            }
            else
            {
               hb_retl( HB_FALSE );
               return;
            }
         }
         hb_retl( HB_TRUE );
      }
      else
         hb_retl( HB_FALSE );
   }
   else
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    MINIXML functions wrapper
 *
 * Copyright 2010 Petr Chornyj <myorg63@mail.ru>
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
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbstack.h"
#include "hbvm.h"

#include "mxml.h"
#include "config.h"

#define BUFFER_SIZE 8192

/*  cb's funcs support */
typedef struct
{
   PHB_DYNS type_cb;
} HB_TYPE_CB_VAR;

typedef struct
{
   PHB_DYNS save_cb;
} HB_SAVE_CB_VAR;

typedef struct
{
   PHB_DYNS error_cb;
} HB_ERROR_CB_VAR;


static void hb_type_cb_var_init( void * cargo )
{
   HB_TYPE_CB_VAR * pType_cb = ( HB_TYPE_CB_VAR * ) cargo;

   pType_cb->type_cb = NULL;
}

static void hb_save_cb_var_init( void * cargo )
{
   HB_SAVE_CB_VAR * pSave_cb = ( HB_SAVE_CB_VAR * ) cargo;

   pSave_cb->save_cb = NULL;
}

static void hb_error_cb_var_init( void * cargo )
{
   HB_ERROR_CB_VAR * pError_cb = ( HB_ERROR_CB_VAR * ) cargo;

   pError_cb->error_cb = NULL;
}

static HB_TSD_NEW( s_type_cb_var, sizeof( HB_TYPE_CB_VAR ), hb_type_cb_var_init, NULL );
static HB_TSD_NEW( s_save_cb_var, sizeof( HB_SAVE_CB_VAR ), hb_save_cb_var_init, NULL );
static HB_TSD_NEW( s_error_cb_var, sizeof( HB_ERROR_CB_VAR ), hb_error_cb_var_init, NULL );

/*  mxml_node_t * support */
static HB_GARBAGE_FUNC( hb_mxml_nodeDestructor )
{
   mxml_node_t ** ppMxml_node = ( mxml_node_t ** ) Cargo;

   if( *ppMxml_node )
   {
      mxmlRelease( *ppMxml_node );
      *ppMxml_node = NULL;
   }
}

static const HB_GC_FUNCS s_gc_mxml_nodeFuncs =
{
   hb_mxml_nodeDestructor,
   hb_gcDummyMark
};

mxml_node_t * hb_mxml_node_ItemGet( PHB_ITEM pItem )
{
   mxml_node_t ** ppMxml_node = ( mxml_node_t ** ) hb_itemGetPtrGC( pItem,
                                                                    &s_gc_mxml_nodeFuncs );

   return ppMxml_node ? *ppMxml_node : NULL;
}

PHB_ITEM hb_mxml_node_ItemPut( PHB_ITEM pItem, mxml_node_t * pMxml_node )
{
   mxml_node_t ** ppMxml_node = ( mxml_node_t ** ) hb_gcAllocate( sizeof( mxml_node_t * ),
                                                                  &s_gc_mxml_nodeFuncs );

   *ppMxml_node = pMxml_node;
   mxmlRetain( *ppMxml_node );

   return hb_itemPutPtrGC( pItem, ppMxml_node );
}

mxml_node_t * hb_mxml_node_param( int iParam )
{
   mxml_node_t ** ppMxml_node = ( mxml_node_t ** ) hb_parptrGC( &s_gc_mxml_nodeFuncs,
                                                                iParam );

   if( ppMxml_node && *ppMxml_node )
      return *ppMxml_node;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

void hb_mxml_node_ret( mxml_node_t * pMxml_node )
{
   hb_mxml_node_ItemPut( hb_stackReturnItem(), pMxml_node );
}

/*  mxml_index_t * support */
static HB_GARBAGE_FUNC( hb_mxml_indexDestructor )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) Cargo;

   if( *ppMxml_index )
   {
      mxmlIndexDelete( *ppMxml_index );
      *ppMxml_index = NULL;
   }
}

static const HB_GC_FUNCS s_gc_mxml_indexFuncs =
{
   hb_mxml_indexDestructor,
   hb_gcDummyMark
};

mxml_index_t * hb_mxml_index_ItemGet( PHB_ITEM pItem )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_itemGetPtrGC( pItem,
                                                                       &s_gc_mxml_indexFuncs );

   return ppMxml_index ? *ppMxml_index : NULL;
}

PHB_ITEM hb_mxml_index_ItemPut( PHB_ITEM pItem, mxml_index_t * pMxml_index )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_gcAllocate( sizeof( mxml_index_t * ),
                                                                     &s_gc_mxml_indexFuncs );

   *ppMxml_index = pMxml_index;
   return hb_itemPutPtrGC( pItem, ppMxml_index );
}

mxml_index_t * hb_mxml_index_param( int iParam )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_parptrGC( &s_gc_mxml_indexFuncs,
                                                                   iParam );

   if( ppMxml_index && *ppMxml_index )
      return *ppMxml_index;

   hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   return NULL;
}

void hb_mxml_index_ret( mxml_index_t * pMxml_index )
{
   hb_mxml_index_ItemPut( hb_stackReturnItem(), pMxml_index );
}

/* non-wrap HB_... funcs */

HB_FUNC( HB_MXMLVERSION )
{
   hb_retc( MXML_VERSION );
}

/* MXML... */

/* mxml_type_t mxmlGetType( mxml_node_t * node) */

HB_FUNC( MXMLGETTYPE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      hb_retni( ( mxml_type_t ) mxmlGetType( node ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlGetParent( mxml_node_t * node ) */

HB_FUNC( MXMLGETPARENT )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * parent = mxmlGetParent( node );

      if( parent )
         hb_mxml_node_ret( parent );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlGetFirstChild( mxml_node_t * node ) */

HB_FUNC( MXMLGETFIRSTCHILD )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * first = mxmlGetFirstChild( node );

      if( first )
         hb_mxml_node_ret( first );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlGetLastChild( mxml_node_t * node ) */

HB_FUNC( MXMLGETLASTCHILD )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * last = mxmlGetLastChild( node );

      if( last )
         hb_mxml_node_ret( last );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlGetNextSibling( mxml_node_t * node ) */

HB_FUNC( MXMLGETNEXTSIBLING )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * next = mxmlGetNextSibling( node );

      if( next )
         hb_mxml_node_ret( next );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlGetPrevSibling( mxml_node_t *node ) */

HB_FUNC( MXMLGETPREVSIBLING )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * previous = mxmlGetPrevSibling( node );

      if( previous )
         hb_mxml_node_ret( previous );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* char * mxmlGetElement( mxml_node_t * node ) */

HB_FUNC( MXMLGETELEMENT )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retc( mxmlGetElement( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlGetInteger( mxml_node_t * node ) */

HB_FUNC( MXMLGETINTEGER )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlGetInteger( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* char * mxmlGetOpaque( mxml_node_t * node ) */

HB_FUNC( MXMLGETOPAQUE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retc( mxmlGetOpaque( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* double mxmlGetReal( mxml_node_t * node ) */

HB_FUNC( MXMLGETREAL )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retnd( mxmlGetReal( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* char * mxmlGetText( mxml_node_t * node, int * whitespace ) */

HB_FUNC( MXMLGETTEXT )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      int whitespace = hb_parni( 2 ) ;

      hb_retc( mxmlGetText( node, &whitespace ) );
      hb_storni( whitespace, 2 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* char * mxmlGetCDATA( mxml_node_t * node ) */

HB_FUNC( MXMLGETCDATA )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retc( mxmlGetCDATA( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void * mxmlGetCustom( mxml_node_t * node ) */

/* void * mxmlGetUserData( mxml_node_t * node ) */

HB_FUNC( MXMLGETUSERDATA )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      PHB_ITEM pItem = ( PHB_ITEM ) mxmlGetUserData( node );

      if( pItem )
         hb_itemCopy( hb_stackReturnItem(), pItem );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetUserData(mxml_node_t * node, void * data ) */

HB_FUNC( MXMLSETUSERDATA )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      PHB_ITEM pItem = ( PHB_ITEM ) hb_param( 2, HB_IT_ANY );

      if( pItem )
         hb_retni( mxmlSetUserData( node, pItem ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlIndexGetCount( mxml_index_t *ind ) */

HB_FUNC( MXMLINDEXGETCOUNT )
{
   mxml_index_t * index = ( mxml_index_t * ) hb_mxml_index_param( 1 );

   if( index )
      hb_retni( mxmlIndexGetCount( index ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlAdd( mxml_node_t * parent, int where, mxml_node_t * child, mxml_node_t * node ) */

HB_FUNC( MXMLADD )
{
   mxml_node_t *  parent = ( mxml_node_t * ) ( HB_ISNIL( 1 ) ? NULL : hb_mxml_node_param( 1 ) );
   int            where = hb_parni( 2 );
   mxml_node_t *  child = ( mxml_node_t * ) ( HB_ISNIL( 3 ) ? NULL : hb_mxml_node_param( 3 ) );
   mxml_node_t *  node = ( mxml_node_t * ) hb_mxml_node_param( 4 );

   if( node )
      mxmlAdd( parent, where, child, node );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlDelete( mxml_node_t * node ) */

HB_FUNC( MXMLDELETE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      mxmlDelete( node );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlElementDeleteAttr( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLELEMENTDELETEATTR )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      mxmlElementDeleteAttr( node, hb_parc( 2 ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* const char * mxmlElementGetAttr( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLELEMENTGETATTR )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retc( mxmlElementGetAttr( node, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlElementSetAttr( mxml_node_t * node, const char * name, const char * value ) */

HB_FUNC( MXMLELEMENTSETATTR )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      mxmlElementSetAttr( node, hb_parc( 2 ), hb_parc( 3 ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* const char * mxmlEntityGetName( int val ) */

HB_FUNC( MXMLENTITYGETNAME )
{
   hb_retc( mxmlEntityGetName( hb_parni( 1 ) - 1 ) );
}

/* int mxmlEntityGetValue( const char * name ) */

HB_FUNC( MXMLENTITYGETVALUE )
{
   int i = mxmlEntityGetValue( hb_parc( 1 ) );

   hb_retni( i < 0 ? -1 : i + 1 );
}

/*
   mxml_node_t * mxmlFindElement( mxml_node_t * node, mxml_node_t * top,
                                  const char * name, const char * attr,
                                  const char * value, int descend );
 */

HB_FUNC( MXMLFINDELEMENT )
{
   mxml_node_t *  node = ( mxml_node_t * ) hb_mxml_node_param( 1 );
   mxml_node_t *  top = ( mxml_node_t * ) hb_mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * nodef = mxmlFindElement( node, top,
                                             hb_parc( 3 ), hb_parc( 4 ),
                                             hb_parc( 5 ), hb_parni( 6 ) );
      if( nodef )
         hb_mxml_node_ret( nodef );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlFindPath( mxml_node_t * node, const char * path ) */

HB_FUNC( MXMLFINDPATH )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * nodef = mxmlFindPath( node, hb_parc( 2 ) );

      if( nodef )
         hb_mxml_node_ret( nodef );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlIndexDelete( mxml_index_t * ind ) */

/*
   HB_FUNC( MXMLINDEXDELETE )
   {
   mxml_index_t * index = ( mxml_index_t * ) hb_mxml_index_param( 1 );

   if( index )
      mxmlIndexDelete( index );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
 */

/* mxml_node_t * mxmlIndexEnum( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXENUM )
{
   mxml_index_t * index = ( mxml_index_t * ) hb_mxml_index_param( 1 );

   if( index )
      hb_mxml_node_ret( mxmlIndexEnum( index ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlIndexFind( mxml_index_t * ind, const char * element, const char * value ) */

HB_FUNC( MXMLINDEXFIND )
{
   mxml_index_t * index = ( mxml_index_t * ) hb_mxml_index_param( 1 );

   if( index )
   {
      mxml_node_t * nodef = mxmlIndexFind( index, hb_parc( 2 ), hb_parc( 3 ) );

      if( nodef )
         hb_mxml_node_ret( nodef );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_index_t * mxmlIndexNew( mxml_node_t * node, const char * element, const char * attr ) */

HB_FUNC( MXMLINDEXNEW )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_index_t * index = mxmlIndexNew( node, hb_parc( 2 ), hb_parc( 3 ) );

      if( index )
         hb_mxml_index_ret( index );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* mxml_node_t * mxmlIndexReset( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXRESET )
{
   mxml_index_t * index = ( mxml_index_t * ) hb_mxml_index_param( 1 );

   if( index )
   {
      mxml_node_t * node = mxmlIndexReset( index );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ================ mxml_type_t type_cb( mxml_node_t * node ) =================== */

static mxml_type_t type_cb( mxml_node_t * node )
{
   HB_TYPE_CB_VAR * pType_cb = ( HB_TYPE_CB_VAR * ) hb_stackGetTSD( &s_type_cb_var );

   if( pType_cb != NULL )
   {
      PHB_DYNS pSym = pType_cb->type_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         int      iResult;
         PHB_ITEM pNode = hb_itemNew( NULL );

         mxmlRetain( node );
         hb_mxml_node_ItemPut( pNode, node );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );

         hb_vmFunction( 1 );
         iResult = hb_parnidef( -1, MXML_TEXT );

         hb_itemRelease( pNode );

         hb_vmRequestRestore();
         return ( mxml_type_t ) iResult;
      }
      else
         return MXML_TEXT;
   }
   else
      return MXML_TEXT;
}

/*
   mxml_node_t * mxmlLoadString(mxml_node_t * top, const char * s,
                               mxml_type_t (*cb)(mxml_node_t *))
 */

HB_FUNC( MXMLLOADSTRING )
{
   mxml_node_t *     node_top;
   mxml_node_t *     node;
   mxml_load_cb_t    cb = MXML_NO_CALLBACK;
   HB_TYPE_CB_VAR *  pType_cb = ( HB_TYPE_CB_VAR * ) hb_stackGetTSD( &s_type_cb_var );


   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( ! node_top )
      {
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         return;
      }
   }

   if( HB_ISSYMBOL( 3 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pType_cb->type_cb = pDynSym;
         cb = type_cb;
      }
   }
   else if( HB_ISNUM( 3 ) )
   {
      switch( hb_parni( 3 ) )
      {
      case 0:  cb = MXML_NO_CALLBACK;       break;
      case 1:  cb = MXML_INTEGER_CALLBACK;  break;
      case 2:  cb = MXML_OPAQUE_CALLBACK;   break;
      case 3:  cb = MXML_REAL_CALLBACK;     break;
      case 4:  cb = MXML_TEXT_CALLBACK;     break;
      case 5:  cb = MXML_IGNORE_CALLBACK;   break;
      default: cb = MXML_NO_CALLBACK;
      }
   }
   node = mxmlLoadString( node_top, hb_parc( 2 ), cb );
   pType_cb->type_cb = NULL;

   if( node )
      hb_mxml_node_ret( node );
}

/* mxml_node_t * mxmlNewCDATA( mxml_node_t * parent, const char * string ) */

HB_FUNC( MXMLNEWCDATA )
{
   mxml_node_t * node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewCDATA( MXML_NO_PARENT, hb_parc( 2 ) );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewCDATA( node_parent, hb_parc( 2 ) );

         if( node )
            hb_mxml_node_ret( node );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewElement( mxml_node_t * parent, const char * name ) */

HB_FUNC( MXMLNEWELEMENT )
{
   mxml_node_t * node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewElement( MXML_NO_PARENT, hb_parc( 2 ) );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewElement( node_parent, hb_parc( 2 ) );

         if( node )
            hb_mxml_node_ret( node );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewInteger( mxml_node_t * parent, int integer ) */

HB_FUNC( MXMLNEWINTEGER )
{
   mxml_node_t * node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewInteger( MXML_NO_PARENT, hb_parni( 2 ) );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewInteger( node_parent, hb_parni( 2 ) );

         if( node )
            hb_mxml_node_ret( node );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewOpaque( mxml_node_t *parent, const char *opaque ) */

HB_FUNC( MXMLNEWOPAQUE )
{
   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
      hb_mxml_node_ret( mxmlNewOpaque( MXML_NO_PARENT, hb_parc( 2 ) ) );
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
         hb_mxml_node_ret( mxmlNewOpaque( node_parent, hb_parc( 2 ) ) );
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewReal( mxml_node_t * parent, double real ) */

HB_FUNC( MXMLNEWREAL )
{
   mxml_node_t * node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewReal( MXML_NO_PARENT, hb_parnd( 2 ) );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewReal( node_parent, hb_parnd( 2 ) );

         if( node )
            hb_mxml_node_ret( node );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewText( mxml_node_t * parent, int whitespace, const char * string ) */

HB_FUNC( MXMLNEWTEXT )
{
   mxml_node_t * node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewText( MXML_NO_PARENT, hb_parnidef( 2, 0 ), hb_parc( 3 ) );

      if( node )
         hb_mxml_node_ret( node );
   }
   else
   {
      mxml_node_t * node_parent = ( mxml_node_t * ) hb_mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewText( node_parent, hb_parnidef( 2, 0 ),
                             hb_parc( 3 ) );
         if( node )
            hb_mxml_node_ret( node );
      }
      else
         hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
   }
}

/* mxml_node_t * mxmlNewXML( const char * version ) */

HB_FUNC( MXMLNEWXML )
{
   mxml_node_t * node;

   node = mxmlNewXML( HB_ISCHAR( 1 ) ? hb_parc( 1 ) : "1.0" );

   if( node )
      hb_mxml_node_ret( node );
}

/* int mxmlRelease( mxml_node_t * node ) */

HB_FUNC( MXMLRELEASE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlRelease( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlRemove( mxml_node_t * node ) */

HB_FUNC( MXMLREMOVE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      mxmlRemove( node );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlRetain( mxml_node_t * node ) */

HB_FUNC( MXMLRETAIN )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlRetain( node ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ============ const char *(*mxml_save_cb_t)(mxml_node_t *, int) =============== */

static const char * save_cb( mxml_node_t * node, int where )
{
   HB_SAVE_CB_VAR * pSave_cb = ( HB_SAVE_CB_VAR * ) hb_stackGetTSD( &s_save_cb_var );

   if( node != NULL && pSave_cb != NULL )
   {
      PHB_DYNS pSym = pSave_cb->save_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         const char *   pszResult;
         PHB_ITEM       pNode = hb_itemNew( NULL );

         mxmlRetain( node );
         hb_mxml_node_ItemPut( pNode, node );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );
         hb_vmPushInteger( where );

         hb_vmFunction( 2 );
         pszResult = HB_ISCHAR( -1 ) ? hb_parc( -1 ) : NULL;

         hb_itemRelease( pNode );

         hb_vmRequestRestore();
         return pszResult;
      }
      else
         return NULL;
   }
   else
      return NULL;
}

/* char * mxmlSaveAllocString( mxml_node_t *node, mxml_save_cb_t cb ) */

HB_FUNC( MXMLSAVEALLOCSTRING )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
   {
      mxml_save_cb_t    cb = MXML_NO_CALLBACK;
      HB_SAVE_CB_VAR *  pSave_cb = ( HB_SAVE_CB_VAR * ) hb_stackGetTSD( &s_save_cb_var );

      char              buffer[ BUFFER_SIZE ];
      int               bytes;

      if( HB_ISSYMBOL( 2 ) )
      {
         PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 2, HB_IT_SYMBOL ) ) );

         if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
         {
            pSave_cb->save_cb = pDynSym;
            cb = save_cb;
         }
      }

      bytes = mxmlSaveString( node, buffer, BUFFER_SIZE, cb );
      pSave_cb->save_cb = NULL;

      if( bytes <= 0 )
         hb_retc_null();
      else if( bytes < ( int ) sizeof( buffer ) )
         hb_retclen_buffer( hb_strdup( buffer ), bytes - 1 );  /* Without EoL */
      else
      {
         char * s = ( char * ) hb_xalloc( bytes + 1 );

         if( s == NULL )
            hb_retc_null();
         else
         {
            mxmlSaveString( node, s, bytes + 1, cb );
            hb_retclen_buffer( s, bytes - 1 );
         }
      }
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSaveString( mxml_node_t * node, char * buffer, int bufsize, mxml_save_cb_t cb ) */

HB_FUNC( MXMLSAVESTRING )
{
   mxml_node_t *  node = ( mxml_node_t * ) hb_mxml_node_param( 1 );
   PHB_ITEM       pBuffer = hb_param( 2, HB_IT_STRING );

   if( node )
   {
      mxml_save_cb_t    cb = MXML_NO_CALLBACK;
      HB_SAVE_CB_VAR *  pSave_cb = ( HB_SAVE_CB_VAR * ) hb_stackGetTSD( &s_save_cb_var );

      if( pBuffer && HB_ISBYREF( 2 ) )
      {
         char *   buffer;
         HB_SIZE  buffer_size;

         if( HB_ISSYMBOL( 3 ) )
         {
            PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

            if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
            {
               pSave_cb->save_cb = pDynSym;
               cb = save_cb;
            }
         }

         if( hb_itemGetWriteCL( pBuffer, &buffer, &buffer_size ) )
         {
            int bytes = mxmlSaveString( node, buffer, ( int ) buffer_size, cb );
            pSave_cb->save_cb = NULL;

            if( bytes <= 0 )
               hb_retni( -1 );
            else if( bytes <= ( int ) ( buffer_size - 1 ) )
            {
               hb_storclen( buffer, ( int ) bytes - 1, 2 ); /* Without EoL */
               hb_retni( bytes );
            }
            else
               hb_retni( -( bytes + 1 ) );
         }
         else
            hb_retni( -2 );
      }
      else
         hb_retni( -3 );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetCDATA( mxml_node_t * node, const char * data ) */

HB_FUNC( MXMLSETCDATA )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetCDATA( node, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetElement( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLSETELEMENT )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetElement( node, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetInteger( mxml_node_t * node, int integer ) */

HB_FUNC( MXMLSETINTEGER )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetInteger( node, hb_parnidef( 2, 0 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetOpaque( mxml_node_t * node, const char * opaque ) */

HB_FUNC( MXMLSETOPAQUE )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetOpaque( node, hb_parc( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetReal( mxml_node_t * node, double real ) */

HB_FUNC( MXMLSETREAL )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetReal( node, hb_parnd( 2 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSetText( mxml_node_t * node, int whitespace, const char * string ) */

HB_FUNC( MXMLSETTEXT )
{
   mxml_node_t * node = ( mxml_node_t * ) hb_mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetText( node, ( int ) hb_parldef( 2, HB_FALSE ), hb_parc( 3 ) ) );
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* void mxmlSetWrapMargin( int column ) */

HB_FUNC( MXMLSETWRAPMARGIN )
{
   mxmlSetWrapMargin( hb_parni( 1 ) );
}

/* mxml_node_t * mxmlWalkNext( mxml_node_t * node, mxml_node_t * top, int descend ) */

HB_FUNC( MXMLWALKNEXT )
{
   mxml_node_t *  node = ( mxml_node_t * ) hb_mxml_node_param( 1 );
   mxml_node_t *  top = ( mxml_node_t * ) hb_mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * node_next = mxmlWalkNext( node, top, hb_parnidef( 3, MXML_NO_DESCEND ) );

      if( node_next )
         hb_mxml_node_ret( node_next );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/*
   mxml_node_t * mxmlWalkPrev(mxml_node_t *node, mxml_node_t *top,
                              int descend)
 */

HB_FUNC( MXMLWALKPREV )
{
   mxml_node_t *  node = ( mxml_node_t * ) hb_mxml_node_param( 1 );
   mxml_node_t *  top = ( mxml_node_t * ) hb_mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * node_prev = mxmlWalkPrev( node, top, hb_parnidef( 3, MXML_NO_DESCEND ) );

      if( node_prev )
         hb_mxml_node_ret( node_prev );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* ================ void ( *mxml_error_cb_t )( const char * ) =================== */

static void error_cb( const char * pszErrorMsg )
{
   HB_ERROR_CB_VAR * pError_cb = ( HB_ERROR_CB_VAR * ) hb_stackGetTSD( &s_error_cb_var );

   if( pError_cb != NULL )
   {
      PHB_DYNS pSym = pError_cb->error_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_itemPutCConst( hb_stackAllocItem(), pszErrorMsg );

         hb_vmProc( 1 );
         hb_vmRequestRestore();
      }
   }
}

HB_FUNC( MXMLSETERRORCALLBACK )
{
   if( HB_ISSYMBOL( 1 ) )
   {
      HB_ERROR_CB_VAR * pError_cb = ( HB_ERROR_CB_VAR * ) hb_stackGetTSD( &s_error_cb_var );
      PHB_DYNS          pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 1, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pError_cb->error_cb = pDynSym;

         mxmlSetErrorCallback( error_cb );
      }
      else
      {
         pError_cb->error_cb = NULL;

         mxmlSetErrorCallback( NULL );
      }
   }
}

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    MINIXML functions wrapper
 *
 * Copyright 2010-2011 Petr Chornyj <myorg63@mail.ru>
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
 */

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbapistr.h"
#include "hbstack.h"
#include "hbvm.h"

#include "mxml.h"
#include "config.h"

#define BUFFER_SIZE     8192

#define MXML_ERR_ARGS   ( hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, \
                                                HB_ERR_ARGS_BASEPARAMS ) )

typedef struct /* placeholder for mxml_node_t */
{
   mxml_node_t * node;
   unsigned int flags;
} HBMXML_NODE;

/* =========================== cb's funcs support =============================== */

typedef struct
{
   PHB_DYNS type_cb;
   PHB_DYNS save_cb;
   PHB_DYNS sax_cb;
} HB_CBS_VAR;

typedef struct
{
   PHB_DYNS load_cb;
   PHB_DYNS save_cb;
} HB_CUSTOM_CBS_VAR;

typedef struct
{
   PHB_DYNS error_cb;
} HB_ERROR_CB_VAR;

static void hb_cbs_var_init( void * cargo )
{
   HB_CBS_VAR * pCbs = ( HB_CBS_VAR * ) cargo;

   pCbs->type_cb = NULL;
   pCbs->save_cb = NULL;
   pCbs->sax_cb = NULL;
}

static void hb_custom_cbs_var_init( void * cargo )
{
   HB_CUSTOM_CBS_VAR * pCCbs = ( HB_CUSTOM_CBS_VAR * ) cargo;

   pCCbs->load_cb = NULL;
   pCCbs->save_cb = NULL;
}

static void hb_error_cb_var_init( void * cargo )
{
   HB_ERROR_CB_VAR * pError_cb = ( HB_ERROR_CB_VAR * ) cargo;

   pError_cb->error_cb = NULL;
}

static HB_TSD_NEW( s_cbs_var, sizeof( HB_CBS_VAR ), hb_cbs_var_init, NULL );
static HB_TSD_NEW( s_custom_cbs_var, sizeof( HB_CUSTOM_CBS_VAR ), hb_custom_cbs_var_init, NULL );
static HB_TSD_NEW( s_error_cb_var, sizeof( HB_ERROR_CB_VAR ), hb_error_cb_var_init, NULL );

/* ========================= mxml_node_t * support ============================== */

static HB_GARBAGE_FUNC( hbmxml_nodeDestructor )
{
   HBMXML_NODE * pHbnode = ( HBMXML_NODE * ) Cargo;

   if( pHbnode && pHbnode->node )
   {
      if( mxmlGetUserData( pHbnode->node ) != NULL )
      {
         hb_itemRelease( pHbnode->node->user_data );
         pHbnode->node->user_data = NULL;
      }

      if( pHbnode->flags == 1 )
      {
         mxmlDelete( pHbnode->node );

         pHbnode->node = NULL;
      }
   }
}

static const HB_GC_FUNCS s_gc_mxml_nodeFuncs =
{
   hbmxml_nodeDestructor,
   hb_gcDummyMark
};

mxml_node_t * mxml_node_param( int iParam )
{
   HBMXML_NODE * pHbnode = ( HBMXML_NODE * ) hb_parptrGC( &s_gc_mxml_nodeFuncs,
                                                          iParam );

   return ( pHbnode && pHbnode->node ) ? pHbnode->node : NULL;
}

PHB_ITEM hbmxml_node_ItemPut( PHB_ITEM pItem, mxml_node_t * pMxml_node, unsigned int uiFlags )
{
   HBMXML_NODE * pHbnode = ( HBMXML_NODE * ) hb_gcAllocate( sizeof( HBMXML_NODE ),
                                                            &s_gc_mxml_nodeFuncs );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( pItem );

   pHbnode->node = pMxml_node;
   pHbnode->flags = uiFlags;

   return hb_itemPutPtrGC( pItem, pHbnode );
}

void mxml_node_ret( mxml_node_t * pMxml_node, unsigned int uiFlags )
{
   hbmxml_node_ItemPut( hb_stackReturnItem(), pMxml_node, uiFlags );
}

/* ========================= mxml_index_t * support ============================= */

static HB_GARBAGE_FUNC( hbmxml_indexDestructor )
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
   hbmxml_indexDestructor,
   hb_gcDummyMark
};

mxml_index_t * hbmxml_index_ItemGet( PHB_ITEM pItem )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_itemGetPtrGC( pItem,
                                                                       &s_gc_mxml_indexFuncs );

   return ppMxml_index ? *ppMxml_index : NULL;
}

PHB_ITEM hbmxml_index_ItemPut( PHB_ITEM pItem, mxml_index_t * pMxml_index )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_gcAllocate( sizeof( mxml_index_t * ),
                                                                     &s_gc_mxml_indexFuncs );

   if( pItem )
   {
      if( HB_IS_COMPLEX( pItem ) )
         hb_itemClear( pItem );
   }
   else
      pItem = hb_itemNew( pItem );

   *ppMxml_index = pMxml_index;
   return hb_itemPutPtrGC( pItem, ppMxml_index );
}

mxml_index_t * mxml_index_param( int iParam )
{
   mxml_index_t ** ppMxml_index = ( mxml_index_t ** ) hb_parptrGC( &s_gc_mxml_indexFuncs,
                                                                   iParam );

   return ( ppMxml_index && *ppMxml_index ) ? *ppMxml_index : NULL;
}

void mxml_index_ret( mxml_index_t * pMxml_index )
{
   hbmxml_index_ItemPut( hb_stackReturnItem(), pMxml_index );
}

/* ========================= non-wrap HB_... funcs ============================== */

HB_FUNC( HB_MXMLVERSION )
{
   hb_retc( MXML_VERSION );
}

/* ======================== MXML_... wrapper funcs ============================== */

/*
 * - mxmlEntityAddCallback
 * - mxmlEntityRemoveCallback
 * - mxmlLoadFd
 * - mxmlSAXLoadFd
 * - mxmlSaveFd
 */

/* void mxmlAdd( mxml_node_t * parent, int where, mxml_node_t * child, mxml_node_t * node ) */

HB_FUNC( MXMLADD )
{
   mxml_node_t *  parent = mxml_node_param( 1 );
   int            where = hb_parnidef( 2, MXML_ADD_BEFORE );
   mxml_node_t *  child = mxml_node_param( 3 );
   mxml_node_t *  node = mxml_node_param( 4 );

   where = ( ( where == MXML_ADD_BEFORE ) ? MXML_ADD_BEFORE : MXML_ADD_AFTER );

   if( parent && node )
   {
      int TODO;

      mxmlAdd( parent, where, ( child != NULL ) ? child : MXML_ADD_TO_PARENT, node );
   }
   else
      MXML_ERR_ARGS;
}

/* void mxmlDelete( mxml_node_t * node ) */

HB_FUNC( MXMLDELETE )
{
   HBMXML_NODE *  pHbnode = ( HBMXML_NODE * ) hb_parptrGC( &s_gc_mxml_nodeFuncs, 1 );
   int            TODO;

   if( pHbnode && pHbnode->node )
   {
      if( mxmlGetUserData( pHbnode->node ) != NULL )
      {
         hb_itemRelease( pHbnode->node->user_data );
         pHbnode->node->user_data = NULL;
      }
      mxmlDelete( pHbnode->node );

      pHbnode->node = NULL;
   }
   else
      MXML_ERR_ARGS;
}

/* void mxmlElementDeleteAttr( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLELEMENTDELETEATTR )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hName;

      mxmlElementDeleteAttr( node, hb_parstr_utf8( 2, &hName, NULL ) );

      hb_strfree( hName );
   }
   else
      MXML_ERR_ARGS;
}

/* const char * mxmlElementGetAttr( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLELEMENTGETATTR )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hName;

      hb_retstr_utf8( mxmlElementGetAttr( node, hb_parstr_utf8( 2, &hName, NULL ) ) );

      hb_strfree( hName );
   }
   else
      MXML_ERR_ARGS;
}

/* void mxmlElementSetAttr( mxml_node_t * node, const char * name, const char * value ) */

HB_FUNC( MXMLELEMENTSETATTR )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void *   hName;
      void *   hValue;

      mxmlElementSetAttr( node, hb_parstr_utf8( 2, &hName, NULL ), hb_parstr_utf8( 3, &hValue, NULL ) );

      hb_strfree( hName );
      hb_strfree( hValue );
   }
   else
      MXML_ERR_ARGS;
}

/* const char * mxmlEntityGetName( int val ) */

HB_FUNC( MXMLENTITYGETNAME )
{
   hb_retstr_utf8( mxmlEntityGetName( hb_parni( 1 ) - 1 ) ); /* ?? */
}

/* int mxmlEntityGetValue( const char * name ) */

HB_FUNC( MXMLENTITYGETVALUE )
{
   void *   hName;
   int      i = mxmlEntityGetValue( hb_parstr_utf8( 1, &hName, NULL ) );

   hb_retni( i < 0 ? -1 : i + 1 ); /* ?? */

   hb_strfree( hName );
}

/*
   mxml_node_t * mxmlFindElement( mxml_node_t * node, mxml_node_t * top,
                                  const char * name, const char * attr,
                                  const char * value, int descend );
 */

HB_FUNC( MXMLFINDELEMENT )
{
   void *         hName;
   void *         hAttr;
   void *         hValue;
   mxml_node_t *  node = mxml_node_param( 1 );
   mxml_node_t *  top = mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * nodef = mxmlFindElement( node, top,
                                             hb_parstr_utf8( 3, &hName, NULL ),
                                             hb_parstr_utf8( 4, &hAttr, NULL ),
                                             hb_parstr_utf8( 5, &hValue, NULL ),
                                             hb_parni( 6 ) );

      hb_strfree( hName );
      hb_strfree( hAttr );
      hb_strfree( hValue );

      if( nodef )
         mxml_node_ret( nodef, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlFindPath( mxml_node_t * node, const char * path ) */

HB_FUNC( MXMLFINDPATH )
{
   void *         hPath;
   mxml_node_t *  node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * nodef = mxmlFindPath( node, hb_parstr_utf8( 2, &hPath, NULL ) );

      if( nodef )
         mxml_node_ret( nodef, 0 );

      hb_strfree( hPath );
   }
   else
      MXML_ERR_ARGS;
}

/* char * mxmlGetCDATA( mxml_node_t * node ) */

HB_FUNC( MXMLGETCDATA )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retstr_utf8( mxmlGetCDATA( node ) );
   else
      MXML_ERR_ARGS;
}

/* char * mxmlGetElement( mxml_node_t * node ) */

HB_FUNC( MXMLGETELEMENT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retstr_utf8( mxmlGetElement( node ) );
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlGetFirstChild( mxml_node_t * node ) */

HB_FUNC( MXMLGETFIRSTCHILD )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * first = mxmlGetFirstChild( node );

      if( first )
         mxml_node_ret( first, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* int mxmlGetInteger( mxml_node_t * node ) */

HB_FUNC( MXMLGETINTEGER )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlGetInteger( node ) );
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlGetLastChild( mxml_node_t * node ) */

HB_FUNC( MXMLGETLASTCHILD )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * last = mxmlGetLastChild( node );

      if( last )
         mxml_node_ret( last, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlGetNextSibling( mxml_node_t * node ) */

HB_FUNC( MXMLGETNEXTSIBLING )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * next = mxmlGetNextSibling( node );

      if( next )
         mxml_node_ret( next, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* char * mxmlGetOpaque( mxml_node_t * node ) */

HB_FUNC( MXMLGETOPAQUE )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retstr_utf8( mxmlGetOpaque( node ) );
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlGetParent( mxml_node_t * node ) */

HB_FUNC( MXMLGETPARENT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * parent = mxmlGetParent( node );

      if( parent )
         mxml_node_ret( parent, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlGetPrevSibling( mxml_node_t *node ) */

HB_FUNC( MXMLGETPREVSIBLING )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_node_t * previous = mxmlGetPrevSibling( node );

      if( previous )
         mxml_node_ret( previous, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* double mxmlGetReal( mxml_node_t * node ) */

HB_FUNC( MXMLGETREAL )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retnd( mxmlGetReal( node ) );
   else
      MXML_ERR_ARGS;
}

/* int mxmlGetRefCount( mxml_node_t * node ) */

HB_FUNC( MXMLGETREFCOUNT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlGetRefCount( node ) );
   else
      MXML_ERR_ARGS;
}

/* char * mxmlGetText( mxml_node_t * node, int * whitespace ) */

HB_FUNC( MXMLGETTEXT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      int whitespace = hb_parni( 2 );

      hb_retstr_utf8( mxmlGetText( node, &whitespace ) );
      hb_storni( whitespace, 2 );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_type_t mxmlGetType( mxml_node_t * node) */

HB_FUNC( MXMLGETTYPE )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( ( int ) mxmlGetType( node ) );
   else
      MXML_ERR_ARGS;
}

/* void * mxmlGetUserData( mxml_node_t * node ) */

HB_FUNC( MXMLGETUSERDATA )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      PHB_ITEM pItem = ( PHB_ITEM ) mxmlGetUserData( node );

      if( pItem )
         hb_itemCopy( hb_stackReturnItem(), pItem );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlIndexEnum( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXENUM )
{
   mxml_index_t * index = mxml_index_param( 1 );

   if( index )
      mxml_node_ret( mxmlIndexEnum( index ), 0 );
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlIndexFind( mxml_index_t * ind, const char * element, const char * value ) */

HB_FUNC( MXMLINDEXFIND )
{
   mxml_index_t * index = mxml_index_param( 1 );

   if( index )
   {
      void *         hElement;
      void *         hValue;
      mxml_node_t *  nodef = mxmlIndexFind( index,
                                            hb_parstr_utf8( 2, &hElement, NULL ),
                                            hb_parstr_utf8( 3, &hValue, NULL ) );

      hb_strfree( hElement );
      hb_strfree( hValue );

      if( nodef )
         mxml_node_ret( nodef, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* void mxmlIndexDelete( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXDELETE )
{
   mxml_index_t * index = mxml_index_param( 1 );

   if( index )
      mxmlIndexDelete( index );
   else
      MXML_ERR_ARGS;
}

/* int mxmlIndexGetCount( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXGETCOUNT )
{
   mxml_index_t * index = mxml_index_param( 1 );

   if( index )
      hb_retni( mxmlIndexGetCount( index ) );
   else
      MXML_ERR_ARGS;
}

/* mxml_index_t * mxmlIndexNew( mxml_node_t * node, const char * element, const char * attr ) */

HB_FUNC( MXMLINDEXNEW )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void *         hElement;
      void *         hAttribute;
      mxml_index_t * index = mxmlIndexNew( node,
                                           hb_parstr_utf8( 2, &hElement, NULL ),
                                           hb_parstr_utf8( 3, &hAttribute, NULL ) );

      hb_strfree( hElement );
      hb_strfree( hAttribute );

      if( index )
         mxml_index_ret( index );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlIndexReset( mxml_index_t * ind ) */

HB_FUNC( MXMLINDEXRESET )
{
   mxml_index_t * index = mxml_index_param( 1 );

   if( index )
   {
      mxml_node_t * node = mxmlIndexReset( index );

      if( node )
         mxml_node_ret( node, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* ================ mxml_type_t type_cb( mxml_node_t * node ) =================== */

static mxml_type_t type_cb( mxml_node_t * node )
{
   HB_CBS_VAR * pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );

   if( pCbs != NULL )
   {
      PHB_DYNS pSym = pCbs->type_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         int      iResult;
         PHB_ITEM pNode = hb_itemNew( NULL );

         hbmxml_node_ItemPut( pNode, node, 0 );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );

         hb_vmFunction( 1 );
         iResult = hb_parnidef( -1, MXML_TEXT );

         hb_itemRelease( pNode );

         hb_vmRequestRestore();
         return ( mxml_type_t ) iResult;
      }
   }
   return MXML_TEXT;
}

HB_FUNC( MXMLLOADFILE )
{
   void *         hFree;
   mxml_node_t *  node_top;
   mxml_node_t *  node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );
   FILE *         file;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param( 1 );

      if( ! node_top )
      {
         MXML_ERR_ARGS;
         return;
      }
   }

   if( HB_ISSYMBOL( 3 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->type_cb = pDynSym;
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

   file = hb_fopen( hb_parstr_utf8( 2, &hFree, NULL ), "rb" );
   if( file )
   {
      node = mxmlLoadFile( node_top, file, cb );

      if( node )
         mxml_node_ret( node, ( node_top == MXML_NO_PARENT ) ? 1 : 0 );
   }

   pCbs->type_cb = NULL;
   hb_strfree( hFree );
}

/*
   mxml_node_t * mxmlLoadString( mxml_node_t * top, const char * s,
                                 mxml_type_t (*cb)(mxml_node_t *) )
 */

HB_FUNC( MXMLLOADSTRING )
{
   void *         hFree;
   mxml_node_t *  node_top;
   mxml_node_t *  node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );


   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param( 1 );

      if( ! node_top )
      {
         MXML_ERR_ARGS;
         return;
      }
   }

   if( HB_ISSYMBOL( 3 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->type_cb = pDynSym;
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

   node = mxmlLoadString( node_top, hb_parstr_utf8( 2, &hFree, NULL ), cb );
   pCbs->type_cb = NULL;

   if( node )
      mxml_node_ret( node, ( node_top == MXML_NO_PARENT ) ? 1 : 0 );

   hb_strfree( hFree );
}

/* mxml_node_t * mxmlNewCDATA( mxml_node_t * parent, const char * string ) */

HB_FUNC( MXMLNEWCDATA )
{
   void *         hString;
   mxml_node_t *  node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewCDATA( MXML_NO_PARENT, hb_parstr_utf8( 2, &hString, NULL ) );

      if( node )
         mxml_node_ret( node, 1 );

      hb_strfree( hString );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewCDATA( node_parent, hb_parstr_utf8( 2, &hString, NULL ) );

         if( node )
            mxml_node_ret( node, 0 );

         hb_strfree( hString );
      }
      else
         MXML_ERR_ARGS;
   }
}

/* mxml_node_t * mxmlNewElement( mxml_node_t * parent, const char * name ) */

HB_FUNC( MXMLNEWELEMENT )
{
   void *         hName;
   mxml_node_t *  node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewElement( MXML_NO_PARENT, hb_parstr_utf8( 2, &hName, NULL ) );

      if( node )
         mxml_node_ret( node, 1 );

      hb_strfree( hName );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewElement( node_parent, hb_parstr_utf8( 2, &hName, NULL ) );

         if( node )
            mxml_node_ret( node, 0 );

         hb_strfree( hName );
      }
      else
         MXML_ERR_ARGS;
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
         mxml_node_ret( node, 1 );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewInteger( node_parent, hb_parni( 2 ) );

         if( node )
            mxml_node_ret( node, 0 );
      }
      else
         MXML_ERR_ARGS;
   }
}

/* mxml_node_t * mxmlNewOpaque( mxml_node_t *parent, const char * opaque ) */

HB_FUNC( MXMLNEWOPAQUE )
{
   void *         hOpaque;
   mxml_node_t *  node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewOpaque( MXML_NO_PARENT, hb_parstr_utf8( 2, &hOpaque, NULL ) );

      if( node )
         mxml_node_ret( node, 1 );

      hb_strfree( hOpaque );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewOpaque( node_parent, hb_parstr_utf8( 2, &hOpaque, NULL ) );

         if( node )
            mxml_node_ret( node, 0 );

         hb_strfree( hOpaque );
      }
      else
         MXML_ERR_ARGS;
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
         mxml_node_ret( node, 1 );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewReal( node_parent, hb_parnd( 2 ) );

         if( node )
            mxml_node_ret( node, 0 );
      }
      else
         MXML_ERR_ARGS;
   }
}

/* mxml_node_t * mxmlNewText( mxml_node_t * parent, int whitespace, const char * string ) */

HB_FUNC( MXMLNEWTEXT )
{
   void *         hString;
   mxml_node_t *  node;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node = mxmlNewText( MXML_NO_PARENT, hb_parnidef( 2, 0 ),
                          hb_parstr_utf8( 3, &hString, NULL ) );

      if( node )
         mxml_node_ret( node, 1 );

      hb_strfree( hString );
   }
   else
   {
      mxml_node_t * node_parent = mxml_node_param( 1 );

      if( node_parent )
      {
         node = mxmlNewText( node_parent, hb_parnidef( 2, 0 ),
                             hb_parstr_utf8( 3, &hString, NULL ) );

         if( node )
            mxml_node_ret( node, 0 );

         hb_strfree( hString );
      }
      else
         MXML_ERR_ARGS;
   }
}

/* mxml_node_t * mxmlNewXML( const char * version ) */

HB_FUNC( MXMLNEWXML )
{
   void *         hVersion;
   mxml_node_t *  node;

   node = mxmlNewXML( HB_ISCHAR( 1 ) ? hb_parstr_utf8( 1, &hVersion, NULL ) : NULL );

   if( node )
      mxml_node_ret( node, 1 );

   if( HB_ISCHAR( 1 ) )
      hb_strfree( hVersion );
}

/* int mxmlRelease( mxml_node_t * node ) */

HB_FUNC( MXMLRELEASE )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlRelease( node ) );
   else
      MXML_ERR_ARGS;
}

/* void mxmlRemove( mxml_node_t * node ) */

HB_FUNC( MXMLREMOVE )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      int TODO;

      mxmlRemove( node );
   }
   else
      MXML_ERR_ARGS;
}

/* int mxmlRetain( mxml_node_t * node ) */

HB_FUNC( MXMLRETAIN )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlRetain( node ) );
   else
      MXML_ERR_ARGS;
}

/* ======= void (*mxml_sax_cb_t)(mxml_node_t *, mxml_sax_event_t, void *)======== */

static void sax_cb( mxml_node_t * node, mxml_sax_event_t event, void * data )
{
   HB_CBS_VAR * pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );

   if( node != NULL && pCbs != NULL )
   {
      PHB_DYNS pSym = pCbs->sax_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         PHB_ITEM    pNode = hb_itemNew( NULL );
         HB_USHORT   uPCount = 2;

         hbmxml_node_ItemPut( pNode, node, 0 );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );
         hb_vmPushInteger( ( int ) ( event + 1 ) );

         if( data != NULL )
         {
            hb_vmPush( ( PHB_ITEM ) data );
            uPCount += 1;
         }
         hb_vmProc( uPCount );

         hb_itemRelease( pNode );
         hb_vmRequestRestore();
      }
   }
}

/*
   mxml_node_t * mxmlSAXLoadFile( mxml_node_t * top,
                                  FILE * fp,
                                  mxml_load_cb_t cb,
                                  mxml_sax_cb_t sax_cb,
                                  void * sax_data )
 */

HB_FUNC( MXMLSAXLOADFILE )
{
   void *         hFree;
   mxml_node_t *  node_top;
   mxml_node_t *  node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   mxml_sax_cb_t  cb_sax = MXML_NO_CALLBACK;
   PHB_ITEM       pData = ( hb_pcount() > 4 ) ? hb_param( 5, HB_IT_ANY ) : NULL;
   HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );
   FILE *         file;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param( 1 );

      if( ! node_top )
      {
         MXML_ERR_ARGS;
         return;
      }
   }

   if( HB_ISSYMBOL( 3 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->type_cb = pDynSym;
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

   if( HB_ISSYMBOL( 4 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 4, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->sax_cb = pDynSym;
         cb_sax = sax_cb;
      }
   }

   file = hb_fopen( hb_parstr_utf8( 2, &hFree, NULL ), "rb" );
   if( file )
   {
      node = mxmlSAXLoadFile( node_top, file, cb, cb_sax, pData );

      if( node != NULL )
         mxml_node_ret( node, ( node_top != NULL ) ? 1 : 0 );

      fclose( file );
   }

   pCbs->type_cb = NULL;
   pCbs->sax_cb = NULL;

   hb_strfree( hFree );
}

/*
   mxml_node_t * mxmlSAXLoadString( mxml_node_t * top,
                                    const char * s,
                                    mxml_load_cb_t cb,
                                    mxml_sax_cb_t sax_cb,
                                    void * sax_data )
 */

HB_FUNC( MXMLSAXLOADSTRING )
{
   void *         hFree;
   mxml_node_t *  node_top;
   mxml_node_t *  node;
   mxml_load_cb_t cb = MXML_NO_CALLBACK;
   mxml_sax_cb_t  cb_sax = MXML_NO_CALLBACK;
   PHB_ITEM       pData = ( hb_pcount() > 4 ) ? hb_param( 5, HB_IT_ANY ) : NULL;
   HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );
   const char *   s;

   if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
   {
      node_top = MXML_NO_PARENT;
   }
   else
   {
      node_top = mxml_node_param( 1 );

      if( ! node_top )
      {
         MXML_ERR_ARGS;
         return;
      }
   }

   if( HB_ISSYMBOL( 3 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->type_cb = pDynSym;
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

   if( HB_ISSYMBOL( 4 ) )
   {
      PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 4, HB_IT_SYMBOL ) ) );

      if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
      {
         pCbs->sax_cb = pDynSym;
         cb_sax = sax_cb;
      }
   }

   s = hb_parstr_utf8( 2, &hFree, NULL );
   if( s )
   {
      node = mxmlSAXLoadString( node_top, s, cb, cb_sax, pData );

      if( node != NULL )
         mxml_node_ret( node, ( node_top != NULL ) ? 1 : 0 );
   }

   pCbs->type_cb = NULL;
   pCbs->sax_cb = NULL;

   hb_strfree( hFree );
}

/* ============ const char *(*mxml_save_cb_t)(mxml_node_t *, int) =============== */

static const char * save_cb( mxml_node_t * node, int where )
{
   HB_CBS_VAR * pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );

   if( node != NULL && pCbs != NULL )
   {
      PHB_DYNS pSym = pCbs->save_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         PHB_ITEM       pNode = hb_itemNew( NULL );
         PHB_ITEM       pResult;

         const char *   pszResult;

         hbmxml_node_ItemPut( pNode, node, 0 );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );
         hb_vmPushInteger( where );
         hb_vmFunction( 2 );

         pResult = hb_param( -1, HB_IT_ANY );
         if( hb_itemType( pResult ) == HB_IT_STRING )
         {
            void *         hText;
            HB_SIZE        nText;
            const char *   pszText = hb_itemGetStrUTF8( pResult, &hText, &nText );

            hb_strfree( hText );
            pszResult = pszText;
         }
         else
            pszResult = NULL;

         hb_itemRelease( pNode );
         hb_vmRequestRestore();

         return pszResult;
      }
   }
   return NULL;
}

/* char * mxmlSaveAllocString( mxml_node_t *node, mxml_save_cb_t cb ) */

HB_FUNC( MXMLSAVEALLOCSTRING )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );
      char           buffer[ BUFFER_SIZE ];
      int            bytes;

      if( HB_ISSYMBOL( 2 ) )
      {
         PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 2, HB_IT_SYMBOL ) ) );

         if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
         {
            pCbs->save_cb = pDynSym;
            cb = save_cb;
         }
      }

      bytes = mxmlSaveString( node, buffer, BUFFER_SIZE, cb );
      pCbs->save_cb = NULL;

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
      MXML_ERR_ARGS;
}

HB_FUNC( MXMLSAVEFILE )
{
   mxml_node_t *  node = mxml_node_param( 1 );
   FILE *         file;
   void *         hFree;

   if( node && HB_ISCHAR( 2 ) )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );

      if( HB_ISSYMBOL( 3 ) )
      {
         PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

         if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
         {
            pCbs->save_cb = pDynSym;
            cb = save_cb;
         }
      }

      file = hb_fopen( hb_parstr_utf8( 2, &hFree, NULL ), "wb" );
      if( file )
      {
         hb_retni( mxmlSaveFile( node, file, cb ) );
         fclose( file );
      }

      pCbs->save_cb = NULL;
      hb_strfree( hFree );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

/* int mxmlSaveString( mxml_node_t * node, char * buffer, int bufsize, mxml_save_cb_t cb ) */

HB_FUNC( MXMLSAVESTRING )
{
   mxml_node_t *  node = mxml_node_param( 1 );
   PHB_ITEM       pBuffer = hb_param( 2, HB_IT_STRING );

   if( node )
   {
      mxml_save_cb_t cb = MXML_NO_CALLBACK;
      HB_CBS_VAR *   pCbs = ( HB_CBS_VAR * ) hb_stackGetTSD( &s_cbs_var );

      if( pBuffer && HB_ISBYREF( 2 ) )
      {
         char *   buffer;
         HB_SIZE  buffer_size;

         if( HB_ISSYMBOL( 3 ) )
         {
            PHB_DYNS pDynSym = hb_dynsymNew( hb_itemGetSymbol( hb_param( 3, HB_IT_SYMBOL ) ) );

            if( pDynSym && hb_dynsymIsFunction( pDynSym ) )
            {
               pCbs->save_cb = pDynSym;
               cb = save_cb;
            }
         }

         if( hb_itemGetWriteCL( pBuffer, &buffer, &buffer_size ) )
         {
            int bytes = mxmlSaveString( node, buffer, ( int ) buffer_size, cb );
            pCbs->save_cb = NULL;

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
      MXML_ERR_ARGS;
}

/* int mxmlSetCDATA( mxml_node_t * node, const char * data ) */

HB_FUNC( MXMLSETCDATA )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hData;

      hb_retni( mxmlSetCDATA( node, hb_parstr_utf8( 2, &hData, NULL ) ) );

      hb_strfree( hData );
   }
   else
      MXML_ERR_ARGS;
}

/* int mxmlSetElement( mxml_node_t * node, const char * name ) */

HB_FUNC( MXMLSETELEMENT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hFree;

      hb_retni( mxmlSetElement( node, hb_parstr_utf8( 2, &hFree, NULL ) ) );

      hb_strfree( hFree );
   }
   else
      MXML_ERR_ARGS;
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

/* int mxmlSetInteger( mxml_node_t * node, int integer ) */

HB_FUNC( MXMLSETINTEGER )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetInteger( node, hb_parnidef( 2, 0 ) ) );
   else
      MXML_ERR_ARGS;
}

/* int mxmlSetOpaque( mxml_node_t * node, const char * opaque ) */

HB_FUNC( MXMLSETOPAQUE )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hOpaque;

      hb_retni( mxmlSetOpaque( node, hb_parstr_utf8( 2, &hOpaque, NULL ) ) );

      hb_strfree( hOpaque );
   }
   else
      MXML_ERR_ARGS;
}

/* int mxmlSetReal( mxml_node_t * node, double real ) */

HB_FUNC( MXMLSETREAL )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
      hb_retni( mxmlSetReal( node, hb_parnd( 2 ) ) );
   else
      MXML_ERR_ARGS;
}

/* int mxmlSetText( mxml_node_t * node, int whitespace, const char * string ) */

HB_FUNC( MXMLSETTEXT )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      void * hString;

      hb_retni( mxmlSetText( node, hb_parnidef( 2, 1 ), hb_parstr_utf8( 3, &hString, NULL ) ) );

      hb_strfree( hString );
   }
   else
      MXML_ERR_ARGS;
}

/* int mxmlSetUserData(mxml_node_t * node, void * data ) */

HB_FUNC( MXMLSETUSERDATA )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      PHB_ITEM pItem = hb_itemClone( hb_param( 2, HB_IT_ANY ) );

      if( pItem )
         hb_retni( mxmlSetUserData( node, pItem ) );
   }
   else
      MXML_ERR_ARGS;
}

/* void mxmlSetWrapMargin( int column ) */

HB_FUNC( MXMLSETWRAPMARGIN )
{
   mxmlSetWrapMargin( hb_parni( 1 ) );
}

/* mxml_node_t * mxmlWalkNext( mxml_node_t * node, mxml_node_t * top, int descend ) */

HB_FUNC( MXMLWALKNEXT )
{
   mxml_node_t *  node = mxml_node_param( 1 );
   mxml_node_t *  top = mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * node_next = mxmlWalkNext( node, top, hb_parnidef( 3, MXML_NO_DESCEND ) );

      if( node_next )
         mxml_node_ret( node_next, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* mxml_node_t * mxmlWalkPrev( mxml_node_t * node, mxml_node_t * top, int descend ) */

HB_FUNC( MXMLWALKPREV )
{
   mxml_node_t *  node = mxml_node_param( 1 );
   mxml_node_t *  top = mxml_node_param( 2 );

   if( node && top )
   {
      mxml_node_t * node_prev = mxmlWalkPrev( node, top, hb_parnidef( 3, MXML_NO_DESCEND ) );

      if( node_prev )
         mxml_node_ret( node_prev, 0 );
   }
   else
      MXML_ERR_ARGS;
}

/* const void * mxmlGetCustom( mxml_node_t * node ) */

HB_FUNC( MXMLGETCUSTOM )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node )
   {
      PHB_ITEM pItem = ( PHB_ITEM ) mxmlGetCustom( node );

      if( pItem )
         hb_itemReturn( pItem );
   }
   else
      MXML_ERR_ARGS;
}

/* =============== void ( *mxml_custom_destroy_cb_t )( void * ) ================= */

static void custom_destroy_cb( void * Cargo )
{
   PHB_ITEM pItem = ( PHB_ITEM ) Cargo;

   if( pItem != NULL )
      hb_itemRelease( pItem );
}

/*
   mxml_node_t * mxmlNewCustom( mxml_node_t * parent,
                                void * data, mxml_custom_destroy_cb_t destroy )
 */

HB_FUNC( MXMLNEWCUSTOM )
{
   mxml_node_t * node = MXML_NO_PARENT;

   if( hb_pcount() > 1 )
   {
      if( HB_ISNIL( 1 ) || ( HB_ISNUM( 1 ) && hb_parni( 1 ) == MXML_NO_PARENT ) )
      {
         PHB_ITEM pItem = hb_itemClone( hb_param( 2, HB_IT_ANY ) );
         node           = mxmlNewCustom( MXML_NO_PARENT, pItem, custom_destroy_cb );

         if( node )
            mxml_node_ret( node, 1 );
         else
            hb_itemRelease( pItem );
      }
      else
      {
         mxml_node_t * node_parent = mxml_node_param( 1 );

         if( node_parent )
         {
            PHB_ITEM pItem = hb_itemClone( hb_param( 2, HB_IT_ANY ) );
            node           = mxmlNewCustom( node_parent, pItem, custom_destroy_cb );

            if( node )
               mxml_node_ret( node, 0 );
            else
               hb_itemRelease( pItem );
         }
         else
            MXML_ERR_ARGS;
      }
   }
   else
      MXML_ERR_ARGS;

}

/*
   int mxmlSetCustom ( mxml_node_t * node,
                       void * data, mxml_custom_destroy_cb_t destroy )
 */

HB_FUNC( MXMLSETCUSTOM )
{
   mxml_node_t * node = mxml_node_param( 1 );

   if( node && hb_pcount() > 1 )
   {
      PHB_ITEM pItem = hb_itemClone( hb_param( 2, HB_IT_ANY ) );
      int           iResult;

      if( pItem )
      {
         iResult = mxmlSetCustom( node, pItem, custom_destroy_cb );

         hb_retni( iResult );

         if( iResult < 0 )
            hb_itemRelease( pItem );
      }
   }
   else
      MXML_ERR_ARGS;
}

/* ======= int ( *mxml_custom_load_cb_t )( mxml_node_t *, const char * ) ======== */

static int custom_load_cb( mxml_node_t * node, const char * data )
{
   HB_CUSTOM_CBS_VAR * pCCbs = ( HB_CUSTOM_CBS_VAR * ) hb_stackGetTSD( &s_custom_cbs_var );

   if( node != NULL && pCCbs != NULL && data != NULL )
   {
      PHB_DYNS pSym = pCCbs->load_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         PHB_ITEM pNode = hb_itemNew( NULL );
         int      iResult;

         hbmxml_node_ItemPut( pNode, node, 0 );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );
         hb_itemPutCConst( hb_stackAllocItem(), data );

         hb_vmFunction( 2 );
         iResult = hb_parnidef( -1, 1 );

         hb_itemRelease( pNode );

         hb_vmRequestRestore();
         return iResult;
      }
   }
   return 1;
}

/* ============ char * ( *mxml_custom_save_cb_t )( mxml_node_t  * ) ============= */

static char * custom_save_cb( mxml_node_t * node )
{
   HB_CUSTOM_CBS_VAR * pCCbs = ( HB_CUSTOM_CBS_VAR * ) hb_stackGetTSD( &s_custom_cbs_var );

   if( node != NULL && pCCbs != NULL )
   {
      PHB_DYNS pSym = pCCbs->save_cb;

      if( pSym && hb_vmRequestReenter() )
      {
         PHB_ITEM pNode = hb_itemNew( NULL );
         char *   pszResult = NULL;

         hbmxml_node_ItemPut( pNode, node, 0 );

         hb_vmPushDynSym( pSym );
         hb_vmPushNil();
         hb_vmPushItemRef( pNode );

         hb_vmFunction( 1 );

         if( HB_ISCHAR( -1 ) )
            pszResult = strdup( hb_parc( -1 ) );

         hb_itemRelease( pNode );

         hb_vmRequestRestore();
         return pszResult;
      }
   }
   return NULL;
}

/* void mxmlSetCustomHandlers( mxml_custom_load_cb_t load, mxml_custom_save_cb_t save ) */

HB_FUNC( MXMLSETCUSTOMHANDLERS )
{
   mxml_custom_load_cb_t   load = NULL;
   mxml_custom_save_cb_t   save = NULL;

   if( HB_ISSYMBOL( 1 ) && HB_ISSYMBOL( 2 ) )
   {
      HB_CUSTOM_CBS_VAR *  pCCbs = ( HB_CUSTOM_CBS_VAR * ) hb_stackGetTSD( &s_custom_cbs_var );
      PHB_DYNS             pDynLoad = hb_dynsymNew( hb_itemGetSymbol( hb_param( 1, HB_IT_SYMBOL ) ) );
      PHB_DYNS             pDynSave = hb_dynsymNew( hb_itemGetSymbol( hb_param( 2, HB_IT_SYMBOL ) ) );

      if( pDynLoad && hb_dynsymIsFunction( pDynLoad ) && pDynSave && hb_dynsymIsFunction( pDynSave ) )
      {
         pCCbs->load_cb = pDynLoad;
         pCCbs->save_cb = pDynSave;
         load = custom_load_cb;
         save = custom_save_cb;
      }
      else
      {
         pCCbs->load_cb = NULL;
         pCCbs->save_cb = NULL;
      }
   }
   mxmlSetCustomHandlers( load, save );
}

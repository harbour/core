/*
 * Low-level Windows object handling functions
 *
 * Copyright 2008-2015 Viktor Szakats (vszakats.net/harbour)
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

#include "hbwapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"

static HB_GARBAGE_FUNC( s_gc_HDC_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      DeleteDC( ( HDC ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HDC_funcs =
{
   s_gc_HDC_release,
   hb_gcDummyMark
};

void hbwapi_ret_HDC( HDC p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HDC ), &s_gc_HDC_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

HB_BOOL hbwapi_is_HDC( int iParam )
{
   return hb_parptrGC( &s_gc_HDC_funcs, iParam ) != NULL;
}

HDC hbwapi_par_HDC( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HDC_funcs, iParam );

   return ph ? ( HDC ) *ph : ( HDC ) __hbwapi_par_handle( iParam );
}

static HB_GARBAGE_FUNC( s_gc_HPEN_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      DeleteObject( ( HPEN ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HPEN_funcs =
{
   s_gc_HPEN_release,
   hb_gcDummyMark
};

void hbwapi_ret_HPEN( HPEN p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HPEN ), &s_gc_HPEN_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

HB_BOOL hbwapi_is_HPEN( int iParam )
{
   return hb_parptrGC( &s_gc_HPEN_funcs, iParam ) != NULL;
}

HPEN hbwapi_par_HPEN( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HPEN_funcs, iParam );

   return ph ? ( HPEN ) *ph : ( HPEN ) __hbwapi_par_handle( iParam );
}

static HB_GARBAGE_FUNC( s_gc_HBRUSH_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      DeleteObject( ( HBRUSH ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HBRUSH_funcs =
{
   s_gc_HBRUSH_release,
   hb_gcDummyMark
};

void hbwapi_ret_HBRUSH( HBRUSH p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HBRUSH ), &s_gc_HBRUSH_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

HB_BOOL hbwapi_is_HBRUSH( int iParam )
{
   return hb_parptrGC( &s_gc_HBRUSH_funcs, iParam ) != NULL;
}

HBRUSH hbwapi_par_HBRUSH( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HBRUSH_funcs, iParam );

   return ph ? ( HBRUSH ) *ph : ( HBRUSH ) __hbwapi_par_handle( iParam );
}

static HB_GARBAGE_FUNC( s_gc_HFONT_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      DeleteObject( ( HFONT ) *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_HFONT_funcs =
{
   s_gc_HFONT_release,
   hb_gcDummyMark
};

void hbwapi_ret_HFONT( HFONT p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( HFONT ), &s_gc_HFONT_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

HB_BOOL hbwapi_is_HFONT( int iParam )
{
   return hb_parptrGC( &s_gc_HFONT_funcs, iParam ) != NULL;
}

HFONT hbwapi_par_HFONT( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_HFONT_funcs, iParam );

   return ph ? ( HFONT ) *ph : ( HFONT ) __hbwapi_par_handle( iParam );
}

static HB_GARBAGE_FUNC( s_gc_PDEVMODE_release )
{
   void ** ph = ( void ** ) Cargo;

   /* Check if pointer is not NULL to avoid multiple freeing */
   if( ph && *ph )
   {
      /* Destroy the object */
      hb_xfree( *ph );

      /* set pointer to NULL to avoid multiple freeing */
      *ph = NULL;
   }
}

static const HB_GC_FUNCS s_gc_PDEVMODE_funcs =
{
   s_gc_PDEVMODE_release,
   hb_gcDummyMark
};

void hbwapi_ret_PDEVMODE( PDEVMODE p )
{
   if( p )
   {
      void ** ph = ( void ** ) hb_gcAllocate( sizeof( PDEVMODE ), &s_gc_PDEVMODE_funcs );

      *ph = p;

      hb_retptrGC( ph );
   }
   else
      hb_retptr( NULL );
}

HB_BOOL hbwapi_is_PDEVMODE( int iParam )
{
   return hb_parptrGC( &s_gc_PDEVMODE_funcs, iParam ) != NULL;
}

PDEVMODE hbwapi_par_PDEVMODE( int iParam )
{
   void ** ph = ( void ** ) hb_parptrGC( &s_gc_PDEVMODE_funcs, iParam );

   return ph ? ( PDEVMODE ) *ph : NULL;
}

HB_FUNC( __WAPI_TYPE )
{
   if(      hbwapi_is_HDC( 1 ) )      hb_retc_const( "HDC" );
   else if( hbwapi_is_HPEN( 1 ) )     hb_retc_const( "HPEN" );
   else if( hbwapi_is_HBRUSH( 1 ) )   hb_retc_const( "HBRUSH" );
   else if( hbwapi_is_HFONT( 1 ) )    hb_retc_const( "HFONT" );
   else if( hbwapi_is_PDEVMODE( 1 ) ) hb_retc_const( "PDEVMODE" );
   else if( HB_ISPOINTER( 1 ) )       hb_retc_const( "HANDLE" );
   else                               hb_retc_null();
}

#if defined( __HBWIN_WITH_UNSAFE_HANDLES )
static int s_iDbgUnsafeMode = 2;  /* 0 = disallow, 1 = trace, 2 = trace + RTE, other = allow */

HB_FUNC( __WAPI_DBGUNSAFEHANDLES )
{
   hb_retni( s_iDbgUnsafeMode );

   if( HB_ISNUM( 1 ) )
      s_iDbgUnsafeMode = hb_parni( 1 );
}

/* The goal is to minimize numeric pointers circulating
   on .prg level, so make them visible. */
static HB_BOOL s_handle_trace( int n )
{
   if( hb_vmInternalsEnabled() )
   {
      switch( s_iDbgUnsafeMode )
      {
         case 0:
            return HB_FALSE;
         case 1:
         case 2:
         {
            char procname[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
            char file[ HB_PATH_MAX ];
            HB_USHORT line;

            hb_procinfo( 0, procname, &line, file );

            HB_TRACE( HB_TR_ALWAYS, ( "%s:%s:%i: __hbwapi_par*_handle(%d)", file, procname, line, n ) );

            if( s_iDbgUnsafeMode == 2 )
               hb_errRT_BASE( EG_ARG, 19000, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
         }
      }

      return HB_TRUE;
   }
   else
      return HB_FALSE;
}
#else
HB_FUNC( __WAPI_DBGUNSAFEHANDLES ) { ; }
#endif

void * __hbwapi_par_handle( int n )
{
#if defined( __HBWIN_WITH_UNSAFE_HANDLES )
   if( HB_ISNUM( n ) )
      return s_handle_trace( n ) ? ( void * ) ( HB_PTRUINT ) hb_parnint( n ) : NULL;
   else
#endif
      return hb_parptr( n );
}

void * __hbwapi_parv_handle( int n, int i )
{
#if defined( __HBWIN_WITH_UNSAFE_HANDLES )
   if( HB_ISNUM( n ) )
      return s_handle_trace( n ) ? ( void * ) ( HB_PTRUINT ) hb_parvnint( n, i ) : NULL;
   else
#endif
      return hb_parvptr( n, i );
}

void * hbwapi_itemGet_HANDLE( PHB_ITEM pItem )
{
#if defined( __HBWIN_WITH_UNSAFE_HANDLES )
   if( pItem && HB_IS_NUMERIC( pItem ) )
      return ( void * ) ( HB_PTRUINT ) hb_itemGetNInt( pItem );
   else
#endif
      return hb_itemGetPtr( pItem );
}

/* pArray must not be NULL */
void * hbwapi_arrayGet_HANDLE( PHB_ITEM pArray, HB_SIZE nIndex )
{
   return hbwapi_itemGet_HANDLE( hb_arrayGetItemPtr( pArray, nIndex ) );
}

HB_BOOL hbwapi_is_HANDLE( int iParam )
{
#if defined( __HBWIN_WITH_UNSAFE_HANDLES )
   return hb_param( iParam, HB_IT_POINTER | HB_IT_NUMERIC ) != NULL;
#else
   return hb_param( iParam, HB_IT_POINTER ) != NULL;
#endif
}
